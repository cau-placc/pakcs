//----------------------
// Primitives for JavaScript compilation:

// higher-order application (in case of explicitApply):
function apply(f,e) {
  var missing = f[1];
  if (missing>1) {
    r = new Array();
    r[0] = f[0];
    r[1] = missing-1;
    for (var i = 2; i < f.length; i++) r[i] = f[i];
    r[i] = e;
    return r;
  } else {
    return fullapply(f,e);
  }
}

// applying a partial constructor application to its last argument
// (in case of explicitApply):
function fullconsapply(f,e) {
  r = new Array();
  r[0] = f[0];
  for (var i = 2; i < f.length; i++) r[i-1] = f[i];
  r[i-1] = e;
  return r;
}

// boolean equality:
function boolEq(x1,x2) {
  switch (typeof(x1)) {
  case "number" : return (x1==x2);
  case "boolean" : return (x1==x2);
  case "string" : if (typeof(x2)=="string") { return (x1==x2); }
		      else { return boolEq(string2characterlist(x1),x2); }
  case "object" :
    if (typeof(x2)=="string") { x2 = string2characterlist(x2); }
    var eq = true;
    for (var i =0 ; eq && i < x1.length ; i++) eq = boolEq(x1[i],x2[i]) ;
    return eq;
  default : alert("Internal error: Unhandled type "+typeof(x1)+" in boolEq!");
  }
}

// Implementation of Prelude.div
function preludeDiv(x,y) { return (x - (x % y)) / y; }

// Implementation of Prelude.ord
function preludeOrd(s) { return s.charCodeAt(0); }

// Implementation of Prelude.char
function preludeChr(n) { return String.fromCharCode(n); }

// failed (should not occur in correctly translated programs!)
function alertFailed() {
  alert("Internal error: Failure occurred!");
}

// Transform a standard string into a (lazy) list-of-character representation,
// i.e., only the first cons of the list is built
function string2charlist(s) {
  if (s == "") {
    return new Array("[]");
  } else {
    return new Array(":",s.charAt(0),s.substring(1));
  }
}

// Transform a standard string into a list-of-character representation
function string2characterlist(s) {
  var len = s.length;
  var i = s.length;
  var tail = new Array("[]");
  while (i>0) {
    i-- ;
    tail = new Array(":",s.charAt(i),tail);
  }
  return tail;
}

//----------------------
