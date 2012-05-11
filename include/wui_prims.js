//----------------------
// Prelude for inclusion into WUI scripts:

var AllowSubmission = true;

// check whether submission is possible:
function submissionAllowed() {
  var allowed = AllowSubmission;
  AllowSubmission = true;
  if (!allowed) {
    alert("Submission not possible due to errors in input fields!");
  }
  return allowed;
}

// Boolean conjunction (similar to && but evaluate both arguments
// due to strictness)
function And(b1,b2) { return b1 && b2 }

// Get integer value of a string element with a given name:
function intValueOf(ElemName) {
  return parseInt(document.getElementsByName(ElemName)[0].value) ;
}

// Get string value of an element with a given name:
function stringValueOf(ElemName) {
  if (LazyStringConversion) { // defined by Curry2JS translator
    return document.getElementsByName(ElemName)[0].value ;
  } else {
    return string2characterlist(document.getElementsByName(ElemName)[0].value) ;
  }
}

// Get integer value of a selection element with a given name:
function selectValueOf(ElemName,ValueField) {
  return ValueField[parseInt(document.getElementsByName(ElemName)[0].value)] ;
}

// Get Boolean value of a check box element with a given name:
function checkBoxValueOf(ElemName) {
 return document.getElementsByName(ElemName)[0].checked ;
}


// Show or hide the error message in a WUI element by modifying the CSS
// style class depending on the Boolean value
// of the second argument, and return the Boolean value:
function setErrorClassName(ElemName,b) {
  var field = document.getElementById(ElemName);
  var errmsgfield = document.getElementById("MSG_"+ElemName);
  if (b) {
    if (errmsgfield) { errmsgfield.className = "wuihide"; }
    if (field) { field.className = "wuipassiveerrmsg"; }
  } else {
    if (errmsgfield) { errmsgfield.className = "wuinohide"; }
    if (field) { field.className = "wuiactiveerrmsg"; }
  }
  return b;
}

// Hide the error message in a WUI element and return true:
function unsetErrorClassName(ElemName) {
  var field = document.getElementById(ElemName);
  var errmsgfield = document.getElementById("MSG_"+ElemName);
  if (errmsgfield) { errmsgfield.className = "wuihide"; }
  if (field) { field.className = "wuipassiveerrmsg"; }
  return true;
}

// Return input string without prefix and suffix white spaces
function stripWhiteSpaces(s) {
  var len = s.length;
  var start = 0;
  var stop  = len;
  while (start < len && s.charAt(start) == ' ') { start++ ; }
  while (stop > start && s.charAt(stop-1) == ' ') { stop-- ; }
  return s.substring(start,stop);
}

// Contains an element an integer string?
function parseIntCheck(ElemName) {
  var elem = document.getElementsByName(ElemName)[0];
  var elemstr = stripWhiteSpaces(elem.value);
  var i = 0;
  var c;
  if (elemstr == "") { return false; }
  if (elemstr.charAt(0) == "-") { i++; }
  while (i < elemstr.length) {
    c = elemstr.charCodeAt(i);
    if (c<48 || c>57) { return false; }
    i++;
  }
  return true;
}

// Is a string not empty?
function notEmpty(s) { return (s != "") ; }


// Transform an array into a list representation:
function array2list(a) { return array2listi(a,0); }

function array2listi(a,i) {
  if (i > a.length) { alert("Internal error in array2list"); }
  if (i == a.length) {
    return new Array("[]");
  } else {
    return new Array(":",a[i],array2listi(a,i+1));
  }
}

//----------------------
