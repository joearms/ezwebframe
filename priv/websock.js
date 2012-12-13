function connect_to_erlang(host, port, mod){
    // console.log('connect', [host,port,mod]);
    make_live_buttons();
    make_live_inputs();
    var ws = 'ws://' + host + ':' + port + '/websocket/' + mod;
    start_session(ws);
}

function onClose(evt) {
    // change the color of the display when the socket closes
    // so we can see it closed
    // console.log('closed');
    document.body.style.backgroundColor='#ffb2b2';
    alert("Socket closed - your erlang probably crashed");
}  
  
function onMessage(evt) {
    var json = JSON.parse(evt.data);
    do_cmds(json);
}
  
function onError(evt) { 
    // if we get an error change the color of the display so we 
    // can see we got an error
    document.body.style.backgroundColor='orange';
}  
  
function send(msg) {
    websocket.send(msg);
}
  
function start_session(wsUri){
    // console.log('start_session', wsUri);
    websocket           = new WebSocket(wsUri); 
    websocket.onopen    = onOpen;
    websocket.onclose   = onClose;
    websocket.onmessage = onMessage; 
    websocket.onerror   = onError;
    return(false);
}  
    
function onOpen(evt) { 
    // console.log("connected");
}

// START:do 
function do_cmds(objs){
    // console.log('do_cmds', objs);
    for(var i = 0; i < objs.length; i++){
	var o = objs[i];
	// as a safety measure we only evaluate js that is loaded
	if(eval("typeof("+o.cmd+")") == "function"){
	    eval(o.cmd + "(o)");
	} else {
	    // console.log('bad_cmd', o);
	    alert("bad_command:"+o.cmd);
	};
    };
}
// END:do

function make_live_buttons(){
    $(".live_button").each(
	function(){
	    var b=$(this);
	    var txt = b.text();
	    b.click(function(){
		// console.log('clicked',txt);
		send_json({clicked:txt});
	    });
	});
}

function send_json(x){
    // console.log('send',x);
    send(JSON.stringify(x));
}

// We want the inputs to send a message when we hit CR in the input

function make_live_inputs(){
    $(".live_input").each(
	function(){
	    var e=$(this);
	    var id = e.attr('id');
            // console.log("entry",[e,id]);
	    e.keyup(function(ev){
			if(ev.keyCode==13){
			    read_entry(e, id);
			};
		    });
	    
	});
}
	
function read_entry(x, id){
    var val = x.val();
    x.val(" ");
    send_json({'entry':id, txt:val});
}
    
// browser commands

function append_div(o){
    var x = $("#"+o.id);
    x.append(o.txt);
    x.animate({scrollTop: x.prop("scrollHeight") }, 1000);
}

function fill_div(o){
    $('#'+o.id).html(o.txt);
}

