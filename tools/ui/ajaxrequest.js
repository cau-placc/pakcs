/*
    @author Christof Kluﬂ
    @version September 2008

    This Library handles Ajax requests and DOM modifications
    for the Curry-Library UI2HTML
*/

// images, that are shown, when an Ajax request is (in)active
var busyImage  = "http://www.informatik.uni-kiel.de/~pakcs/UI/Throbber.gif";
var readyImage = "http://www.informatik.uni-kiel.de/~pakcs/UI/Throbber.png";

// Handler function that creates an Ajax request
function ajaxRequest(e,w,id) {
  var nvs = []

  getFormNameAndValues(w,nvs);
  if (opener) {
    getFormNameAndValues(opener,nvs);
  }

  nvs[id] = ""; // Event Id (EVENT_...)

  var url = document.location.href;
  var callback = getCallBack;
  var postData = "";  

  for (var name in nvs) {  
    if (name.substr(0,5) == "FIELD" || name.substr(0,5) == "EVENT") {
      var value = nvs[name];
      postData += escape(name) + "=" + encodeURIComponent(value) + "&";
    }
  }

  sendRequest(url,callback,postData,w);
}

// Handler function that creates an Ajax request
function handleKeypress(e,w,id) {
  if (e.which == 13) { // RETURN http://www.danshort.com/ASCIImap/
    e.preventDefault();
    e.stopPropagation();
    ajaxRequest(e,w,id);
  }
  return false;
}


// get an element by id, search in popups too 
function getElement(w,id) {
  var elem = document.getElementById(id);

  if (!elem) {
    elem = document.getElementsByName(id)[0];
  }

  if (!elem && opener) {
    elem = opener.document.getElementById(id);
    if (!elem) {
       elem = opener.document.getElementsByName(id)[0];     
    }
  }

  if (!elem) {
    elem = w.document.getElementById(id);
    if (!elem) {
       elem = w.document.getElementsByName(id)[0];     
    }
  }

  return elem;
}

// handle Ajax response
function getCallBack(response,w) {
  //rfc4627 http://www.ietf.org/rfc/rfc4627.txt?number=4627 
  // it is maybe better to use http://www.json.org/json2.js
  
  var resp = eval('(' + response + ')');
  var content = resp.content;

  var closeWindow = false;  

  for (var i=0; i < content.length; i++) {
    var changeInfo = content[i];

    if (changeInfo.id) {
      if (changeInfo.id == "close") {
        closeWindow = true;
        continue;
      }

      var elem = getElement(w,changeInfo.id);      
      var changes = changeInfo.changes;

      if (changes.list) { 
        // remove all list entries
        elem.options.length = 0;
        for (var k = 0; k < changes.list.length; k++) {
          var opt = changes.list[k];
          opt = opt.replace(" ","\u00A0","g");
          elem.options[k] = new Option(opt,k);
        }
      }

      if (changes.value && elem) {
        if (elem.tagName.toLowerCase() == "span" 
            || elem.tagName.toLowerCase() == "div") {
          elem.innerHTML = changes.value;

          //while (elem.hasChildNodes()) 
          //  elem.removeChild(elem.firstChild);
          //var text = document.createTextNode(changes.value);
          //elem.appendChild(text);

        } else if (elem.tagName.toLowerCase() == "canvas") {

          var canvasItems = changes.value.split(";");
          for (var k = 0; k < canvasItems.length; k++) {
            var item = canvasItems[k];

            if (item.substr(0,5) == "CLine") {
              item = item.substr(5);

              // [(150,150),(183,150)]
              var nums = item.match(/\[\((\d+),(\d+)\),\((\d+),(\d+)\)\]/);

              var ctx = elem.getContext('2d');
              ctx.beginPath();
              ctx.moveTo(nums[1],nums[2]);
              ctx.lineTo(nums[3],nums[4]);
              ctx.closePath(); ctx.stroke();

              //console.log(nums[1] + " " + nums[2] + " " + nums[3] + " " + nums[4]);
            }
          }
        } else if (elem.tagName.toLowerCase() == "select") {
          elem.selectedIndex = parseInt(changes.value);
        } else if (elem.type == "checkbox") {
          elem.checked = (changes.value == "0") ? false : true;
        } else if (elem.type == "radio") {
          radios = document.forms[0][elem.name];	
          for (var k = 0; k < radios.length; k++) {
            if (radios[k].value == changes.value) {
              radios[k].checked = true;
              break;
            }
          }
        } else {
          elem.value = changes.value;
        }
      }

      if (typeof (changes.disabled) != "undefined") {
        elem.disabled = changes.disabled;  	
      }

      if (typeof (changes.visible) != "undefined") {
        elem.style.display = changes.visible ? "inline" : "none"; 
      }

      if (changes.style) {
        var styles = changes.style.split(";");

        for (var k = 0; k < styles.length; k++) {
          if (styles[k].indexOf(":") != -1) {
            var stylenv = styles[k].split(":");
            var sname = stripWhiteSpaces(stylenv[0]);
            var svalue = stripWhiteSpaces(stylenv[1]);

            elem.style[camelize(sname)]= svalue;
          }
        }
      }

      if (typeof (changes.errorbg) != "undefined") {
        elem.className = changes.errorbg ? "error" : "";
      }

      if (changes.pos) {
        var line = changes.pos[0];
        var column = changes.pos[1];
        // not implemented, should scroll a textarea
      }
    }
  }

  if (resp.popups) {
    ps = resp.popups;

    for (var i = 0; i < ps.length; i++) {
      p = ps[i];

      if (p.type == "popup") {
        var html = ps[i].html;
        html = html.replace("ajaxRequest","opener.ajaxRequest","g");

        var newWindow =
         window.open("","",'width=400,height=300,scrollbars=yes,resizable=yes');
        newWindow.document.open();
        newWindow.document.write(html);
        newWindow.document.close();
        newWindow.document.title = p.title; 
      } else if (p.type == "changeevent") {
        var elem = getElement(w,p.id);
        var str = p.eventid;

        if (p.event != "default") {
          elem[p.event] = function (event) { ajaxRequest(event,window,str); }
        } else { // choose default event based on tag name
          switch (elem.tagName) {
            case 'select': 
              elem.onclick = 
                function (event) { ajaxRequest(event,window,str); };
              break;
            case 'input':
              if (elem.type == 'text') {
                elem.onkeypress = 
                  function (event) { ajaxRequest(event,window,str); };
              } else {
                elem.onclick =
                  function (event) { ajaxRequest(event,window,str); };
              }; 
              break;
            default:
              elem.onclick = 
                function (event) { ajaxRequest(event,window,str); };	
          }
        }
      } else if (p.type == "htmlsite") {
         document.title = p.title;
         document.getElementsByTagName("form")[0].innerHTML = p.html; 
      } else if (p.type == "changeChilds") {
        getElement(w,p.ref).innerHTML = p.html;
      }
    } 
  }

  if (closeWindow) {
    w.close();
  }
}


// collects the content of the widgets in an ui 
// as name/value pairs from the HTML page
function getFormNameAndValues(w,nvs) {    
  // add Name/Value infos from input fields
  var inputs = w.document.getElementsByTagName("input");
  for (var i = 0; i < inputs.length; i++) {    
    var input = inputs[i];    
    var name = input.name;
    var value = input.value;

    if (input.type == "radio") {
      if (!input.checked) {
        continue;
      }
    }
 
    if (!(value.substr(0,5) == "EVENT") && !(name.substr(0,5) == "EVENT")) {
      if (input.type == "checkbox") {
         value = (input.checked) ? 1 : 0;	 
      }
      nvs[name] = value;
    }
  }

  addToNVs(w,nvs,"textarea");
  addToNVs(w,nvs,"select");

  var spans = w.document.getElementsByTagName("span");
  for (var i = 0; i < spans.length; i++) {
    var span = spans[i];

    if (span.getAttribute("id")) {
      var childs = span.childNodes;
      var elementNodes = false;

      for (var j = 0; j < childs.length; j++) {
        if (childs[j].nodeType == 1) {
          elementNodes = true;
          break; 
        }
      };

      // contains no element nodes
      if (!elementNodes) { 
        var name = span.getAttribute("id");
        var value = span.innerHTML;
        nvs[name] = value;
      }
    }
  }

  var divs = w.document.getElementsByTagName("div");
  for (var i = 0; i < divs.length; i++) {
    var div = divs[i];

    if (div.getAttribute("id")) {
      var childs = div.childNodes;
      var elementNodes = false;

      for (var j = 0; j < childs.length; j++) {
        if (childs[j].nodeType == 1) {
          elementNodes = true;
          break; 
        }
      };

      // contains no element nodes
      if (!elementNodes) { 
        var name = div.getAttribute("id");
        var value = div.innerHTML;
        nvs[name] = value;
      }
    }
  } 
}


function addToNVs(w,nvs,tagname) {
  var tags = w.document.getElementsByTagName(tagname);
  for (var i = 0; i < tags.length; i++) {
    var tag = tags[i];
    var name = tag.name;
    var value = tag.value;

    if (!(value.substr(0,5) == "EVENT") 
        && !(name.substr(0,5) == "EVENT")) {
      nvs[name] = value;
    }
  }
}


// creates a new XMLHttpRequest Object for Ajax requests
function getRequestObject() {
  var req = null;
  if (window.XMLHttpRequest) {
    req = new XMLHttpRequest();
  } else if (typeof ActiveXObject != "undefined") {
    req = new ActiveXObject("Microsoft.XMLHTTP");
  }
  return req;
}

// counts requests, if 0 no request is active
var requests = 0;

function sendRequest(url,callback,postData, w) {
  requests++;
  showBusy(requests);

  var req = getRequestObject();

  function handleResponse() {
    if (req.readyState == 4 && req.status == 200) {    
      callback(req.responseText,w);
      requests--;
      showBusy(requests);
    }
  }

  req.onreadystatechange = handleResponse;
  req.open("POST",url,true);
  req.setRequestHeader('Content-type','application/x-www-form-urlencoded');
  req.send(postData);
}



// Return input string without prefix and suffix white spaces
// from PAKCS WUI
function stripWhiteSpaces(s) {
  var len = s.length;
  var start = 0;
  var stop  = len;
  while (start < len && s.charAt(start) == ' ') { start++ ; }
  while (stop > start && s.charAt(stop-1) == ' ') { stop-- ; }
  return s.substring(start,stop);
}

// Converts a css style name separated by dashes into 
// JavaScript style name. 
// i.e.'backgroud-color' is converted to 'backgroundColor'.
function camelize(val) {
   return val.replace(/-(.)/g, function(m, l){return l.toUpperCase()});
}


// state informations
var stateimg = false;

window.addEventListener('load',initState,false);

function initState() {
  stateimg = document.createElement('img');
  document.body.appendChild(stateimg);
  showBusy(0);   
  if (stateimg) {
    stateimg.style.position = "fixed";
    stateimg.style.bottom= "2px";
    stateimg.style.right="2px";  
  }
}

function showBusy(requests) {
  if (stateimg) {  	  
    if (requests > 0) {
       stateimg.src = busyImage;
    } else {
       stateimg.src = readyImage;
    }       
  }
}

//http://developer.mozilla.org/en/docs/DOM:element.addEventListener
//target.addEventListener(type, listener, useCapture);

