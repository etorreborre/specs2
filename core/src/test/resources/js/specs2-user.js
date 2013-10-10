function initUserScript(document) {
  var js = document.createElement("script");
  js.type = "text/javascript";
  js.src = "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML";
  document.head.appendChild(js);
  MathJax.Hub.Config({
    tex2jax: {inlineMath: [['\\(','\\)']]}
  });	
}
