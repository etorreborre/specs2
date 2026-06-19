function toggleImage(image) {
  if (image.style.listStyleImage.match('expanded')) {
    image.style.listStyleImage = "url(images/collapsed.gif)";
  } else {
    image.style.listStyleImage = "url(images/expanded.gif)";
  }
};
function showHide(id) {
  element = document.getElementById(id);
  element.style.display = (element.style.display == 'block')? 'none' : 'block';
};
function showHideByClass(name) {
  var elements = document.getElementsByClassName(name);
  for (i = 0; i < elements.length; i++) {
    elements[i].style.display = (elements[i].style.display == 'none') ? elements[i].style.display = '': 'none';
  }
};
function showByClass(name) {
  var elements = document.getElementsByClassName(name);
  for (i = 0; i < elements.length; i++) {
    elements[i].style.display = '';
  }
};
function showAll() {
  showByTag('text');
  showByTag('example');
  showByTag('step');
};
function showByTag(name) {
  var elements = document.getElementsByTagName(name);
  for (i = 0; i < elements.length; i++) {
    elements[i].style.display = 'block';
  }
};
function hideByClass(name) {
  var elements = document.getElementsByClassName(name);
  for (i = 0; i < elements.length; i++) {
    elements[i].style.display = 'none';
  }
};
function showById(id) {
  document.getElementById(id).style.display = ''
};
function hideById(id) {
  document.getElementById(id).style.display = 'none'
};