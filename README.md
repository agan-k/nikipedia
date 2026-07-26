nikipedia/
├── assets
    ├── main.js
    ├── style.css
├── dist(docs)   
├── pages
    ├── index.html 
    ├── about.html
    ├── ...
    ├── manifest.txt
├── templates
    ├── layout.html
    ├── navbar.html
build.scm
- To create new page:
1. Add html snippet containing page content (look at the example of pages in
the nikipedia/pages directory). For example:

    nikipedia/pages/animals.html:
        <h1>Animals</h1>
        <h2>Animal</h2>
        <img src="" width="200" />
        <p>About this!</p>

2. Add name of the new page file in the nikipedia/pages/manifest.txt
nikipedia/pages/manifest.txt contains names of all pages so that the build script can dynamically create
nav-links without having to rely on external libraries to scan the hardware directory (bypassing the OS completely).
3. run build command:
with included build file: ./build 
or full stand-alone command: rm -rf docs && mkdir -p docs/assets && gosh build.scm
