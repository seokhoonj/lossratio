// Open external links (different host) in a new tab.
document.addEventListener("DOMContentLoaded", function () {
  var here = window.location.hostname;
  document.querySelectorAll('a[href^="http"]').forEach(function (a) {
    try {
      if (new URL(a.href).hostname !== here) {
        a.target = "_blank";
        a.rel = "noopener noreferrer";
      }
    } catch (e) {}
  });
});
