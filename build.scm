;; =============================================================================
;; PURE ACADEMIC STATIC SITE COMPILER (100% R5RS / Gauche Compatible)
;; Focus: Low-Level Character Parsing, Streams, and Functional States
;; =============================================================================

;; -----------------------------------------------------------------------------
;; 1. TEXT MATCHING & INDENTED INJECTION ENGINE (Finite State Machine)
;; -----------------------------------------------------------------------------

;; Prepends a captured whitespace indentation string to every line of a snippet
(define (indent-snippet snippet-str spaces-str)
  (let ((chars (string->list snippet-str)))
    (let loop ((lst chars) (result '()))
      (cond ((null? lst) 
             (string-append-list (reverse result)))
            ;; When we hit a newline character inside the injected code block, 
            ;; immediately paste the parent line's trailing layout indent right after it.
            ((char=? (car lst) #\newline)
             (loop (cdr lst) (cons spaces-str (cons "\n" result))))
            (else
             (loop (cdr lst) (cons (string (car lst)) result)))))))

;; Main parser: tracks leading space, targets tags, and matches nested templates
(define (replace-template-tag template-str tag-name replacement-str)
  (let* ((template-list (string->list template-str))
         (tag-target    (string-append "{{" tag-name "}}"))
         (target-len    (string-length tag-target)))

    ;; 'current-indent' tracks consecutive spaces or tabs on the current row
    (let loop ((chars template-list) (current-indent '()) (result '()))
      (cond ((null? chars) 
             (string-append-list (reverse result)))
            
            ;; Reset indentation collection memory on every line break
            ((char=? (car chars) #\newline)
             (loop (cdr chars) '() (cons "\n" result)))
            
            ;; If the character is horizontal whitespace, save it to the track buffer
            ((or (char=? (car chars) #\space) (char=? (car chars) #\tab))
             (loop (cdr chars) (cons (car chars) current-indent) (cons (string (car chars)) result)))
            
            ;; Target Tag Hit: Format the snippet using our captured whitespace row padding!
            ((and (>= (length chars) target-len)
                  (string=? (list->string (take chars target-len)) tag-target))
             (let* ((spaces-prefix (list->string (reverse current-indent)))
                    (indented-block (indent-snippet replacement-str spaces-prefix)))
               (loop (drop chars target-len) '() (cons indented-block result))))
            
            ;; Standard Character: Clear indentation memory and copy the letter forward
            (else 
             (loop (cdr chars) '() (cons (string (car chars)) result)))))))

;; Pure structural helper functions for our scanning machine needles
(define (take lst n) (if (or (= n 0) (null? lst)) '() (cons (car lst) (take (cdr lst) (- n 1)))))
(define (drop lst n) (if (or (= n 0) (null? lst)) lst (drop (cdr lst) (- n 1))))
(define (string-append-list lst) (if (null? lst) "" (string-append (car lst) (string-append-list (cdr lst)))))

;; -----------------------------------------------------------------------------
;; 2. LOW-LEVEL MANIFEST PARSER (Char-by-Char Line Splitter)
;; -----------------------------------------------------------------------------
(define (read-manifest-lines path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((char (read-char port)) (current-word '()) (lines '()))
        (cond ((eof-object? char)
               (let ((final-lines (if (null? current-word) lines (cons (list->string (reverse current-word)) lines))))
                 (reverse final-lines)))
              ((char=? char #\newline)
               (if (null? current-word)
                   (loop (read-char port) '() lines)
                   (loop (read-char port) '() (cons (list->string (reverse current-word)) lines))))
              ((char=? char #\return)
               (loop (read-char port) current-word lines))
              (else
               (loop (read-char port) (cons char current-word) lines)))))))

;; -----------------------------------------------------------------------------
;; 3. LOW-LEVEL FILENAME FORMATTER (Char-by-Char Title Transformer)
;; -----------------------------------------------------------------------------
(define (strip-html-extension str)
  (let ((len (string-length str)))
    (if (and (>= len 5) (string=? (substring str (- len 5) len) ".html")) (substring str 0 (- len 5)) str)))

(define (filename->title filename)
  (let* ((clean-name (strip-html-extension filename))
         (char-list  (string->list clean-name)))
    (let loop ((chars char-list) (capitalize? #t) (result '()))
      (cond ((null? chars) (list->string (reverse result)))
            ((or (char=? (car chars) #\-) (char=? (car chars) #\_)) (loop (cdr chars) #t (cons #\space result)))
            (capitalize? (loop (cdr chars) #f (cons (char-upcase (car chars)) result)))
            (else (loop (cdr chars) #f (cons (car chars) result)))))))

;; -----------------------------------------------------------------------------
;; 4. DATA FILTERS & NAV LINK GENERATOR
;; -----------------------------------------------------------------------------
(define (generate-navbar pages-list)
  (cond ((null? pages-list) "")
        ((string=? (car pages-list) "index.html")
         (generate-navbar (cdr pages-list)))
        (else
         (let* ((page (car pages-list))
                (title (filename->title page))
                (link-element (string-append "<li><a href=\"" page "\">" title "</a></li>\n")))
           (string-append link-element (generate-navbar (cdr pages-list)))))))

;; -----------------------------------------------------------------------------
;; 5. NATIVE MONADIC FILE I/O & BYTE DUPLICATORS
;; -----------------------------------------------------------------------------
(define (read-file-to-string path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((char (read-char port)) (chars '()))
        (if (eof-object? char) (list->string (reverse chars)) (loop (read-char port) (cons char chars)))))))

(define (write-string-to-file path content)
  (call-with-output-file path (lambda (port) (display content port))))

;; Low-level pipeline asset duplication engine
(define (copy-binary-file source-path destination-path)
  (call-with-input-file source-path
    (lambda (input-port)
      (call-with-output-file destination-path
        (lambda (output-port)
          (let copy-loop ((char (read-char input-port)))
            (if (eof-object? char)
                #t
                (begin
                  (write-char char output-port)
                  (copy-loop (read-char input-port))))))))))

;; -----------------------------------------------------------------------------
;; 6. MAIN COMPILER EXECUTION LOOP
;; -----------------------------------------------------------------------------
(define (compile-site)
  (let* ((pages       (read-manifest-lines "pages/manifest.txt"))
         (layout      (read-file-to-string "templates/layout.html"))
         (navbar-raw  (read-file-to-string "templates/navbar.html"))
         
         (nav-links   (generate-navbar pages))
         (navbar-full (replace-template-tag navbar-raw "nav_links" nav-links)))

    ;; Step A: Iterate over and compile individual pages
    (let compile-loop ((remaining-pages pages))
      (if (not (null? remaining-pages))
          (let* ((current-page (car remaining-pages))
                 (page-body    (read-file-to-string (string-append "pages/" current-page)))
                 
                 (with-navbar  (replace-template-tag layout "navbar" navbar-full))
                 (final-html   (replace-template-tag with-navbar "content" page-body))
                 (output-path  (string-append "docs/" current-page)))
            
            (write-string-to-file output-path final-html)
            (display "Compiled Page: ") (display output-path) (newline)
            
            (compile-loop (cdr remaining-pages)))))
            
    ;; Step B: Deploy project core static assets via our custom byte duplicator
    (copy-binary-file "assets/style.css" "docs/assets/style.css")
    (display "Deployed Asset: docs/assets/style.css") (newline)
    
    (copy-binary-file "assets/main.js" "docs/assets/main.js")
    (display "Deployed Asset: docs/assets/main.js") (newline)))

(compile-site)
