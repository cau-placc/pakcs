var Action = {

 // convenience constructors for primitive actions
 pure    : function(x)       { return { kind: 'pure' , value: x           }; },
 apply   : function(af,a)    { return { kind: 'apply', fun: af , arg: a   }; },
 seq     : function()        { return { kind: 'seq'  , actions: arguments }; },
 prim    : function(n,fun)   { return { kind: 'prim' , arity: n, fun: fun }; },
 event   : function()        { return { kind: 'event'                     }; },
 observe : function(o,r,e,a) {
    return { kind: 'observe', once: o, elems: [r], event: e, action: a };
  },

 // interpreter for actions
 perform: function(event,action) {
    //console.log(action.kind);
    
    switch (action.kind) {

    case "pure"   : return action.value;
    case "window" : return window;
    case "event"  : return event;
    case "elems"  : return $A(action.elems);

    case "apply" :
    return Action.perform(event,action.fun)(Action.perform(event,action.arg));

    case "select" :
    var obj = Action.perform(event,action.obj);
    return action.bind ? obj[action.prop].bind(obj) : obj[action.prop];

    case "curryfy" :
    return curryfy(action.arity,Action.perform(event,action.fun));

    case "seq" :
    results = $A(action.actions).collect(Action.perform.curry(event));
    if (action.first) {
      return results[0];
    } else {
      return results[results.length-1];
    }

    case "observe" :
    //console.log(action.elems);
    
    
    handler = function(event) {
      if (action.once) {
        action.elems.each(function(elem){
            Event.stopObserving(elem,action.event,handler);
          });
      }
      Action.perform(event,action.action);
    }
    action.elems.each(function(elem) {
        
        Event.observe(elem,action.event,handler);
      });
    return null;

    default :
      throw('Action.perform: unknown action\n\n' + Object.toJSON(action));
    }

  },

 // register action to be performed after the page is loaded
 performOnLoad: function(action) {
   //console.log(action);
   
   //Event.observe(window, 'load', function(event) {
   var event = "";
       Action.perform(event,action);
   //  });
  }
};

curryfy = function(arity,fun) {
  if (arity<=1) { return fun; }
  else { return function(arg) { return curryfy(arity-1,fun.curry(arg)); }; }
};


// primitive functions
var Prim = {
 eventSource : function(event) { 
    return [Event.element(event)];
  },

 display : function(ids) {
    return ids.each(function(id) { return $(id).show(); });
  },

 hide : function(ids) {
    return ids.each(function(id) { return $(id).hide(); });
  },

 addClassName : function(ids,cname) {
    return ids.each(function(id) { return $(id).addClassName(cname); });
  }
}
