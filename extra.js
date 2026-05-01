// Per-page language toggle for the EN <-> KR navbar link.
// Detects the current page's language and rewrites the toggle's href to the
// equivalent page on the other language site. Falls back to the language root
// if no equivalent exists (e.g., reference pages, which are English-only).

document.addEventListener('DOMContentLoaded', function () {
  var enRoot = '/lossratio/';
  var koRoot = '/lossratio/ko/';

  // Locate the toggle: a navbar link whose text is exactly "EN" or "KR".
  var links = document.querySelectorAll('.navbar a.nav-link');
  var toggle = null;
  for (var i = 0; i < links.length; i++) {
    var t = links[i].textContent.trim();
    if (t === 'EN' || t === 'KR') {
      toggle = links[i];
      break;
    }
  }
  if (!toggle) return;

  var path = window.location.pathname;

  if (path.indexOf(koRoot) === 0) {
    // Korean -> English
    var rest = path.slice(koRoot.length);
    rest = rest.replace(/-ko\.html$/, '.html');
    toggle.href = enRoot + rest;
  } else if (path.indexOf(enRoot) === 0) {
    // English -> Korean
    var rest = path.slice(enRoot.length);
    if (/^articles\/[^/]+\.html$/.test(rest) && !/articles\/index\.html$/.test(rest)) {
      // Vignette page: add -ko suffix
      toggle.href = koRoot + rest.replace(/\.html$/, '-ko.html');
    } else if (/^reference\//.test(rest)) {
      // Reference is English-only; fall back to Korean root.
      toggle.href = koRoot;
    } else {
      // Home, articles index, news index, etc.: same path on the Korean side.
      toggle.href = koRoot + rest;
    }
  }
});
