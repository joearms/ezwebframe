/*
 * This file defines a single function SVG to use the module
 * var x = new SVG();
 * x.mk_canvas({width:200, id:1, ht:100, color:blue});
 * x.mk_circle({parent:1, cx:10, cy:20, r:15, color:"green"});
 * x.render([cmd]) interprets a list of commands
 *  
 */

var SVG = {};

SVG.svg_ns = 'http://www.w3.org/2000/svg';
SVG.xhtml_ns = 'http://www.w3.org/1999/xhtml';
SVG.xlinkns = 'http://www.w3.org/1999/xlink';

// drag variables
SVG.xstart = 0;
SVG.ystart = 0;
SVG.dragging = false;
SVG.dragobj = null;
SVG.canvas = null;


SVG.init1 = function(o){
    // console.log('***** INIT ***** ', [SVG,o.indiv]);
    var canvas = SVG.add_canvas(o);
}
    
SVG.id = function(tag) {
    return document.getElementById(tag);
};

SVG.D = function(x, y) {
    return x == undefined ? y : x;
};

SVG.save = function(key, val){
    if(key !== undefined){
	if(V.hasOwnProperty(key)){
	    alert('Error SVG created object with duplicate ID = ' + key);
	} else {
	    V[key] = val;
	}
    }
}

SVG.add_canvas = function(o) {
    var canvas = document.createElementNS(SVG.svg_ns, 'svg');
    canvas.setAttribute("width", SVG.D(o.width, 200));
    canvas.setAttribute("height", SVG.D(o.ht, 100));
    canvas.setAttribute("style","background-color:" + 
			SVG.D(o.color, "#eeffbb"));
    canvas.setAttribute("id", o.id);
    canvas.addEventListener("mousemove", SVG.mouse_move, false);
    canvas.addEventListener("mousedown", SVG.mouse_down, false);
    canvas.addEventListener("mouseup", SVG.mouse_up, false);
    SVG.id(o.parent).appendChild(canvas);
    // add a marker that be used to add the tip to the end of the arrows
    var marker = SVG.make_arrow_marker();
    canvas.appendChild(marker);
    SVG.save(o.id, canvas);
    return canvas;
};

// Jarc_arc code from osé M. Vidal
// http://jmvidal.cse.sc.edu/talks/canvassvg/

SVG.add_arc = function(o)
{
    var path = document.createElementNS(SVG.svg_ns, "path");
    var startangle = o.startangle*Math.PI/180;
    var angle = o.angle*Math.PI/180;
    var endangle = startangle + angle;
    
    // Compute the two points where our wedge intersects the circle
    // These formulas are chosen so that an angle of 0 is at 12 o'clock
    // and positive angles increase clockwise.
    var x1 = o.cx + o.r * Math.sin(startangle);
    var y1 = o.cy - o.r * Math.cos(startangle);
    var x2 = o.cx + o.r * Math.sin(endangle);
    var y2 = o.cy - o.r * Math.cos(endangle);
    
    // This is a flag for angles larger than than a half circle
    var big = 0;
    if (endangle - startangle > Math.PI) big = 1;
    
    var d = "M " + o.cx + "," + o.cy +  // Start at circle center
    " L " + x1 + "," + y1 +             // Draw line to (x1,y1)
    " A " + o.r + "," + o.r +           // Draw an arc of radius r
    " 0 " + big + " 1 " +               // Arc details...
    x2 + "," + y2 +                     // Arc goes to to (x2,y2)
    " Z";                               // Close path back to (cx,cy)
    path.setAttribute("d", d);          // Set this path 
    path.setAttribute("fill", SVG.D(o.fill, "blue")); // Set wedge color
    path.setAttribute("stroke", "black");   // Outline wedge in black
    path.setAttribute("stroke-width", "2"); // 2 units thick
    V[o.parent].appendChild(path);
    SVG.save(o.id, path);
};

SVG.add_bezier = function(o)
{
    var path = document.createElementNS(SVG.svg_ns, "path");
    path.setAttribute("d", o.path);                         // Set this path 
    path.setAttribute("fill", SVG.D(o.fill, "blue"));       // Set wedge color
    path.setAttribute("stroke", "black");                   // Outline in black
    path.setAttribute("stroke-width", SVG.D(o.width, "2")); // 2 units thick
    V[o.parent].appendChild(path);
    SVG.save(o.id, path);
};  

SVG.add_path = function(o)
{
    var group = document.createElementNS(SVG.svg_ns, 'g');
    group.setAttribute('transform', o.transform);
    V[o.parent].appendChild(group);
    var path = document.createElementNS(SVG.svg_ns, "path");
    path.setAttribute("d", o.d);                      // Set this path 
    path.setAttribute("fill", SVG.D(o.fill, "blue")); // Set wedge color
    path.setAttribute("stroke", "black");             // Outline wedge in black
    path.setAttribute("stroke-width", SVG.D(o.width, "2")); // 2 units thick
    group.appendChild(path);
};  

SVG.add_circle = function(o)
{
    var obj = document.createElementNS(SVG.svg_ns, 'circle');
    obj.setAttribute('cx', o.x);
    obj.setAttribute('cy', o.y);
    obj.setAttribute('r', SVG.D(o.r, 10));
    obj.setAttribute('fill', SVG.D(o.fill,"red"));
    obj.setAttribute('stroke', SVG.D(o.stroke,
				      SVG.D(o.outer, "blue")));
    obj.setAttribute('stroke-width', SVG.D(o.stroke,2));
    V[o.parent].appendChild(obj);
    SVG.save(o.id, obj);
};

SVG.add_ellipse = function(o)
{
    var obj = document.createElementNS(SVG.svg_ns, "ellipse");
    obj.setAttribute("cx", o.cx);
    obj.setAttribute("cy", o.cy);
    obj.setAttribute("rx", o.rx);
    obj.setAttribute("ry", o.ry);
    obj.setAttribute("fill", SVG.D(o.fill,"black"));
    V[o.parent].appendChild(obj);
    SVG.save(o.id, obj);
};

SVG.add_group = function(o)
{
    var group = document.createElementNS(SVG.svg_ns, 'g');
    if(SVG.D(o.draggable, false)){
	group.type="draggable";
	group.id = o.id;
    };
    var x = SVG.D(o.x,10);
    var y = SVG.D(o.y,10);
    var t = "translate("+x+","+y+")";
    group.setAttribute('transform', t);
    group.setAttribute('x',x);
    group.setAttribute('y',y);
    V[o.parent].appendChild(group);
    SVG.save(o.id, group);
    return group;
};

SVG.add_image = function(o)
{
    var img = document.createElementNS(SVG.svg_ns, "image");  
    img.setAttribute("x", SVG.D(o.x,0));
    img.setAttribute("y", SVG.D(o.y,0));
    img.setAttribute("width", SVG.D(o.width, 20));
    img.setAttribute("height", SVG.D(o.ht, 20));
    img.setAttributeNS(SVG.xlinkns, "href", o.img);
    V[o.parent].appendChild(img);
    SVG.save(o.id, img);
};

SVG.add_line = function(o)
{
    // console.log('add_line',o);
    var obj= document.createElementNS(SVG.svg_ns,"line");
    obj.setAttribute("x1", o.x1);
    obj.setAttribute("y1", o.y1);
    obj.setAttribute("x2", o.x2);
    obj.setAttribute("y2", o.y2);
    obj.setAttribute("stroke","black");
    var width = SVG.D(o.width,1);
    obj.setAttribute("stroke-width",width+"px");
    obj.setAttribute("fill",SVG.D(o.fill,"black"));
    if (o.arrow == true)
	obj.setAttribute("marker-end","url(#myArrowTip)");
    obj.setAttribute("id", o.id);
    V[o.parent].appendChild(obj);
    SVG.save(o.id, obj);
};
 
SVG.add_rect = function(o)
{
    var obj = document.createElementNS(SVG.svg_ns, 'rect');
    obj.setAttribute('x', o.x);
    obj.setAttribute('y', o.y);
    obj.setAttribute('width', SVG.D(o.width,80));
    obj.setAttribute('height', SVG.D(o.ht, 20));
    obj.setAttribute('fill', SVG.D(o.color,"#aaaaaa"));
    obj.setAttribute('stroke', SVG.D(o.stroke,SVG.
				      D(o.fill, "aaaaaa")));
    obj.setAttribute('stroke-width', SVG.D(o.thickness,1));
    obj.setAttribute("rx", SVG.D(o.rx, 3));
    obj.setAttribute("ry", SVG.D(o.ry, 3));
    obj.setAttribute("id", o.id);
    if(o.dash != undefined) 
	obj.setAttribute("style", "stroke-dasharray: "+ o.dash);
    V[o.parent].appendChild(obj);
    SVG.save(o.id,obj);
    
    return obj;
};

SVG.add_text = function(o) 
{
    var text = document.createElementNS(SVG.svg_ns, "text");
    text.setAttribute("fill", SVG.D(o.fill,"black"));
    var size = SVG.D(o.size, 1);
    text.setAttribute("font-size", size+"em");
    var font = SVG.D(o.font,"Arial");
    text.setAttribute("font-family", font);
    text.setAttribute("x", SVG.D(o.x,10));
    text.setAttribute("y", SVG.D(o.y,10));
    // start middle end
    text.setAttribute("text-anchor", SVG.D(o.anchor,"start"));
    var str = SVG.D(o.str,"** missing str in text **");
    var data = document.createTextNode(o.str);
    text.setAttribute("id", o.id);
    text.appendChild(data);
    V[o.parent].appendChild(text);
    SVG.save(o.id, text);
};

SVG.mouse_up = function(evt)
{
    // can get mouse up from a click on a button
    if(SVG.dragging){
	SVG.dragging = false;
	var t = evt.target.parentNode;
	var x  = t.getAttribute("x");
	var y  = t.getAttribute("y");
	var z  = t.getAttribute("id");
	// console.log('stop dragging', [t,z]);
	// SVG.log("stop dragging x=" + x +" y=" + y);
	send_json({cmd:'stop_drag',x:x,y:y,id:z});
	if(t.ondrop){
	    t.ondrop(x, y);
	}
    }
};
    
SVG.mouse_move = function(evt)
{ 
    if (SVG.dragging) 
	{
	    var t = evt.target;
	    var x = evt.clientX + window.scrollX;
	    var y = evt.clientY + window.scrollY;
	    // Move drag element by the same amount the cursor has moved.
	    var x1 = (x-SVG.xstart);
	    var y1 = (y-SVG.ystart);
	    if (SVG.dragobj.constrain == "h"){
		// if we want to constrain the dragging we 
		// freeze y1
		y1 = SVG.dragobj.getAttribute("y");
	    };
	    if (SVG.dragobj.constrain == "v"){
		// if we want to constrain the dragging we 
		// freeze y1
		x1 = SVG.dragobj.getAttribute("x");
	    };
	    SVG.dragobj.setAttribute("x", x1);
	    SVG.dragobj.setAttribute("y", y1);
	    var t = "translate("+x1+","+y1+")";
	    SVG.dragobj.setAttribute('transform', t);
	    SVG.update_draggables(SVG.dragobj);	    
	}
};
   
SVG.update_draggables = function(x){
    // console.log('update_draggables', [x.elastic, x.id]);
    var lines= x.elastic;
    for(var i=0; i < lines.length; i++){
	var o   = V[lines[i]];
	var g1  = V[o.id1];
	var x1  = parseInt(g1.getAttribute("x"));
	var y1  = parseInt(g1.getAttribute("y"));
	var r1  = o.r1;
	var g2  = V[o.id2];
	var x2  = parseInt(g2.getAttribute("x"));
	var y2  = parseInt(g2.getAttribute("y"));
	var r2  = o.r2;
	var obj = o.obj;
	var r1  = o.r1;
	var r2  = o.r2;
	var f   = SVG.endpoints(x1,y1,r1,x2,y2,r2);
	obj.setAttribute("x1",f.x1);
	obj.setAttribute("y1",f.y1);
	obj.setAttribute("x2",f.x2);
	obj.setAttribute("y2",f.y2);
	// console.log('here', endpoints);
    }
}

SVG.mouse_down = function(evt)
{
    var t = evt.target.parentNode;
    // console.log("clicked on type=", [t,t.type]);
    if(t.type == "button"){
	// fire the button method
	t.obj.clicked();
    } else if(t.type == "draggable") {
	SVG.dragging = true;
	SVG.dragobj = t;
	var x = evt.clientX + window.scrollX;
	var y = evt.clientY + window.scrollY;
	var cxstart = t.getAttribute("x");
	var cystart = t.getAttribute("y");
	SVG.xstart = x - cxstart;
	SVG.ystart = y - cystart;
	// console.log('start drag',[t.id,t.type,t]);
	send_json({cmd:'start_drag',x:cxstart,y:cystart,id:t.id});
	// SVG.log("start dragging t.type="+t.type+
	// " x=" +cxstart+" y="+cystart);
    }
};

SVG.make_arrow_marker = function()
{
    var marker = document.createElementNS(SVG.svg_ns, "marker");
    marker.setAttribute("id", "myArrowTip"); // <-- the name in url(#...)
    marker.setAttribute("viewBox","0 0 10 10");
    marker.setAttribute("refX",1);
    marker.setAttribute("refY",5);
    marker.setAttribute("markerUnits", 8);
    marker.setAttribute("orient","auto");
    marker.setAttribute("markerWidth",8);
    marker.setAttribute("markerHeight",6);
    var path = document.createElementNS(SVG.svg_ns, "polyline");
    path.setAttribute("points","0,0 10,5 0,10 1,5");
    path.setAttribute("fill","darkblue");
    marker.appendChild(path);
    return marker;
};

SVG.log = function(x){
    SVG.id("log").innerHTML += x + "<br>";
};

SVG.add_elastic_arrow = function(o){
    // console.log('add_elastic', o);
    var g1 = V[o.id1];
    var x1 = parseInt(g1.getAttribute("x"));
    var y1 = parseInt(g1.getAttribute("y"));
    var r1 = o.r1;
    var g2 = V[o.id2];
    var x2 = parseInt(g2.getAttribute("x"));
    var y2 = parseInt(g2.getAttribute("y"));
    var r2 = o.r2;
    var f  = SVG.endpoints(x1,y1,r1,x2,y2,r2);
    SVG.add_line({parent:o.parent,id:o.id,x1:f.x1,y1:f.y1,x2:f.x2,y2:f.y2,
		  arrow:true});
    // need to tell the drag blogs that they elastic endpoints
    // make a new_id
    var newId = o.id +'e';
    SVG.save(newId, {id1:o.id1, id2:o.id2, r1:r1, r2:r2, obj:V[o.id]});
    g1.elastic.push(newId);
    g2.elastic.push(newId);
}    

SVG.endpoints = function(x1,y1,r1,x2,y2,r2)
{
   // line between two circles
    var m,a,b,x3,y3,x4,y4;
    m = 5; // marker offset
    d = Math.sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));
    b = x1;
    a = (x2 - b)/d;
    x3 = r1*a + b;
    x4 = (d-r2-m)*a + b;
    b = y1;
    a = (y2 - b)/d;
    y3 = (r1)*a + b;
    y4 = (d-r2-m)*a + b;
    return {x1:x3, y1:y3, x2:x4, y2:y4} 
}

SVG.add_drag_rect = function(o){
    var g1 = SVG.add_group({parent:o.parent, draggable:true,
			    id:o.id, x:o.x,y:o.y});
    //  console.log('g1', [g1,o.id]);
    g1.id = o.id;
    g1.constrain = 'h';
    SVG.add_rect({parent:o.id, x:0,y:0,width:o.width,color:'pink',
		  ht:o.ht, rx:2,ry:2});
};
    


SVG.add_dragblob = function(o)
{
    // red circle - black border
    var g1 = SVG.add_group({parent:o.parent, draggable:true,
			    id:o.id, x:o.x,y:o.y});
    //  console.log('g1', [g1,o.id]);
    g1.id = o.id;
    g1.elastic = new Array();
    SVG.add_circle({parent:o.id, x:0,y:0,r:o.r,stroke:2});
};
    

/* 
 * Now for some objects that are built using SVG
 * Button
 *
 */

SVG.add_button = function(o){
    new SVG.Button(o);
}

SVG.Button = function(o)
{
    var g = SVG.add_group({parent:o.parent,id:o.id,x:o.x,y:o.y});
    this.parentId = o.id;
    this.group = g;
    g.type = "button";
    g.obj = this;
    this.width = o.width;
    this.rect = SVG.add_rect({parent:o.id,
			      id:o.id+'r',
			      x:0, y:0, str:o.str, stroke:"black", 
			      rx:1, ry:1, width:o.width, ht:25, thickness:1,
			      color:"#dddddd"});
    this.text = SVG.add_text({parent:o.id,
			      id:o.id+'t',
			      x:(o.width/2),y:18, str:o.str, 
			      anchor:"middle"});
};

SVG.Button.prototype.clicked = function(){
    // r is the dotted line rectange that appears
    // this does not need an id
    var r = SVG.add_rect({parent:this.parentId,
			  x:3,y:3,color:"none",
			  stroke:"black",
			  width: this.width - 6,
			  ht:19,
			  thickness:1, dash:"1,1"});
    var oldcolor = this.rect.getAttribute('fill');
    var g = this.group;
    var f = function(){g.removeChild(r);};
    send_json({cmd:'clicked',id:this.parentId});
    setTimeout(f, 500);
};

// make a design grid -- easier to see the coordinates

SVG.add_grid = function(o){
    var width = o.width;
    var ht = o.ht;
    var step = o.step;
    var parent = o.parent;
    // draw horizontal grid
    var i = 0;
    while(i < ht){
	SVG.add_line({parent:parent, x1:0,y1:i,x2:width,y2:i});
	SVG.add_text({parent:parent, x:5,y:i+15,str:i});
	i += step;
    };
    // draw vertical grid
    i = 0;
    while(i < width){
	SVG.add_line({parent:o.parent, x1:i,y1:0,x2:i,y2:ht});
	SVG.add_text({parent:parent, x:i+5,y:15,str:i});
	i += step;
    };
}

function send_json(o){
    send(JSON.stringify(o));
}

SVG.mk_generic = function(o){
    var obj = document.createElementNS(SVG.svg_ns, o.type);
    for(key in o){
	var val = o[key];
	obj.setAttributeNS(null, key, val);
    };
    V[o.parent].appendChild(obj);
    SVG.save(o.id, obj);
}
