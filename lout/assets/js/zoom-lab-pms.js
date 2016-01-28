var f = window.location.pathname.split('/');
switch(f[f.length-1]) {
case 'lab1.html':
case 'lab2.html':
case 'lab3.html':
    var e = document.createElement('style');
    e.textContent  = '#main-content {font-size: 12pt;}';
    e.textContent += '#main-content p {font-size: 12pt;}';
    e.textContent += '#main-content li {font-size: 12pt; margin-bottom: 0.25em;}';
    e.textContent += '#main-content ul {margin-bottom: 0.5em;}';
    e.textContent += '#main-content h1 {font-size: 18pt; font-weight: bold; margin-top: 2em;}';
    e.textContent += '#main-content h2 {font-size: 14pt; font-weight: bold; margin-top: 1.5em;}';
    e.textContent += '#main-content h3 {font-weight: bold; margin-top: 1.5em;}';
    e.textContent += '#main-content pre {padding: 0.5em; font-size: 12pt; margin-top: 1.5em; margin-bottom: 1.5em}';
    document.getElementsByTagName('HEAD')[0].appendChild(e);
}
