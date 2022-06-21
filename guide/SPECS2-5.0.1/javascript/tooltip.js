var xPos;
var yPos;

function showToolTipWithIcon(title,icon,msg,evt){

   var toolTip = document.getElementById("toolTip");
   toolTip.innerHTML = "<h1>"+title+"</h1><p><img src='"+icon+"'/>"+msg+"</p>";
   toolTip.style.visibility = "visible";

   if (document.all) evt = event;
   var st = Math.max(document.body.scrollTop, document.documentElement.scrollTop);

   if (navigator.userAgent.toLowerCase().indexOf('safari')>=0)st=0; 
   var leftPos = evt.clientX-2;
   if (leftPos<0) leftPos = 0;

   toolTip.style.left = leftPos + 'px';
   toolTip.style.width = (msg.length * 2) + 'px'
   toolTip.style.top = evt.clientY-toolTip.offsetHeight-18+st+ 'px';
}
function showToolTip(title,msg,evt){

   var toolTip = document.getElementById("toolTip");
   toolTip.innerHTML = "<h1>"+title+"</h1><p>"+msg+"</p>";
   toolTip.style.visibility = "visible";

    if (document.all) evt = event;
    var st = Math.max(document.body.scrollTop, document.documentElement.scrollTop);

    if (navigator.userAgent.toLowerCase().indexOf('safari')>=0)st=0; 
    var leftPos = evt.clientX-2;
    if(leftPos<0)leftPos = 0;

    toolTip.style.left = leftPos + 'px';
    toolTip.style.width = (msg.length * 2) + 'px'
    toolTip.style.top = evt.clientY-toolTip.offsetHeight-18+st+ 'px';
}

function hideToolTip(){
   var toolTip = document.getElementById("toolTip");
   toolTip.style.visibility = "hidden";
}