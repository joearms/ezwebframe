-module(svg_pad4).
-compile(export_all).

start(Ws) -> 
    process_flag(trap_exit, true),
    put(free, 1),
 
    %% INIT MUST be the first command
    make_svg_object(Ws, init1, 
	    [{id, svg}, {parent, here}, 
	     {width,800}, {ht,400}, {color,'#ddebdd'}]),
    make_svg_object(Ws, add_grid,
     		    [{parent,svg}, {step,50},{width,800}, {ht,400}]),

    make_svg_object(Ws, add_drag_rect,
		    [{parent,svg},
		     {id,3456},
		     {x,300},{y,350},{width,100},{ht,25}]),
    text(Ws, 300, 390, "Hozontally constrained draggagle"),
    add_standard_objects(Ws),
    add_generic_objects(Ws),

    make_svg_object(Ws, add_button,
		    [{parent,svg},{id,23},
		     {x,200},{y,300},{width,100},{str,<<"click me">>}
		    ]),

    make_svg_object(Ws, add_button,
	    [{parent,svg},{id,223},
	     {x,50},{y,300},{width,120},{str,<<"click 12">>}
	     ]),


    %% To make an elastic line we make two drag blobs

    make_svg_object(Ws, add_dragblob,
		    [{id,100},{parent,svg},{x,20},{y,350},
		     {r,10}]),
    
    make_svg_object(Ws, add_dragblob,
		    [{id,101},{parent,svg},{x,100},{y,350},
		     {r,10}]),

    make_svg_object(Ws, add_dragblob,
		    [{id,103},{parent,svg},{x,20},{y,250},
		     {r,10}]),
    
    make_svg_object(Ws, add_elastic_arrow, 
		    [{parent,svg},{id,268}, 
		     {id1,100},
		     {r1,10},{r2,10},
		     {id2,101}]), 
    make_svg_object(Ws, add_elastic_arrow, 
		    [{parent,svg},{id,269}, 
		     {id1,101},
		     {r1,10},{r2,10},
		     {id2,103}]), 
    make_dashed_container(Ws, rect_container()),
    loop(Ws, true).

add_generic_objects(Ws) ->
    Scale = 0.03,
    Y=230,
    make_generic_object(Ws, rect,	 
			[{x,50},{y,50},{rx,5},{ry,5},
			 {parent,svg},
			 {width,50},{height,20},
			 {fill,<<"#bacdff">>}
			]),
   
    text(Ws, 55, 65,"rect1"),
   
    make_generic_object(Ws,rect,
			[{x,50},
			 {y,100},
			 {rx,5},
			 {ry,5},{width,50},{height,30},
			 {'stroke-dasharray', <<"2,2">>},
			 {'stroke', blue},
			 {'stroke-width',3},
			 {fill,yellow},
			 {parent,svg}]),
    text(Ws, 55,120,"rect2"),   
 
    make_generic_object(Ws,circle,
	  [{r,35},
	   {cx,150},{cy, 100},
	   {'stroke-dasharray', <<"2,2">>},
	   {'stroke', blue},
	   {'stroke-width',3},
	   {fill,orange},
	   {parent,svg}]),
    text(Ws, 120,110,"circle1"),
 
    make_generic_object(Ws, circle,
			[{r,35},
			 {cx,150},{cy, 200},
			 {fill,green},
			 {parent,svg}]),
    
    text(Ws, 130,210,"circle2"),
    
    make_generic_object(Ws, path,
			[{parent,svg},
			 transform(250,100,Scale),
			 {d,wow()}]),
    text(Ws, 250, 140, "Music!"),

    %% Here X=100 Y=270
    make_generic_object(Ws, path,
			[
			 {parent,svg},
			 {style,<<"stroke:black">>},
			 %% H is horizional line to
			 %% h is in relative units (easier)
			 {d,<<"M100 280 H 250 M 100 290 h 150">>}]
		       ),
      text(Ws, 100, 270, "horizontal lines").
    

rect_container() ->
    {550,50,200,300}.


make_dashed_container(Ws, {X, Y, W, H}) ->
    make_svg_object(Ws, add_rect,[{x,X},{y,Y},{ht,H},{width,W},
			  {parent,svg},
			  {color,none},
			  {thickness,6}, 
			  {dash,<<"4,4">>}]).

add_standard_objects(Ws) ->
    make_svg_object(Ws, add_rect,[{x,200},{y,50},{ht,200},
				  {width,150},
				  {parent,svg},
				  {color,none},
				  {thickness,4}, 
				  {dash,<<"4,4">>}]),
    
    make_svg_object(Ws, add_line,[{x1,350},{y1,350},{x2,475},{y2,325},
				  {width,2},
			  {parent,svg}]),

    make_svg_object(Ws, add_line,[{parent,svg},
				  {x1,350},{y1,340},{x2,475},{y2,315},
				  {width,2}, {arrow,true}]),

    make_svg_object(Ws, add_image,[{img,<<"./p5.jpg">>}, 
				   {x,200}, 
				   {y,350}, {width,100}, {ht,50},
				   {parent,svg}]),

    make_svg_object(Ws, add_text,
		    [{parent,svg},{x,350},
		     {y,250}, {str,<<"Drag the Red blob below">>}]),

    make_svg_object(Ws, add_dragblob,[{id,12}, {parent,svg},{x,450},{y,280}]),
    
    
    %% green ellipse
    make_svg_object(Ws, add_ellipse, [{cx,300},{cy,200},{rx,45},{ry,25},
				      {fill,pink},{parent,svg}]),
    text(Ws,270,200,"ellipse1"),
    
    %%  an arcs

    make_svg_object(Ws, add_arc,[{parent,svg},
				 {fill,orange},
				 {startangle,20}, {angle,65}, 
				 {cx,300}, {cy,100}, {r,45}]), 
    text(Ws,315,75,"arc"),
    
    %% groups can be made draggable

    make_svg_object(Ws, add_group,
		    [{parent,svg},{id,99},{x,400},
		     {draggable,true},
		     {y,50}]),
    make_svg_object(Ws, add_rect,[{parent,99}, {x,0},{y,0}, 
				  {width,100},{ht,150},
				  {color,<<"#fedcba">>}]),
    make_svg_object(Ws, add_text,
		    [{parent,99},{y,20},{str,<<"Drag Me">>}]),

    make_svg_object(Ws, add_arc,[{parent,99}, {startangle,10}, {angle,320}, 
	      {cx,50}, {cy,60}, {r,35}, {fill, red}]).
    
make_generic_object(Ws, Type, L) ->
    L1 = [{cmd,'SVG.mk_generic'},{type,Type}|L],
    Ws ! {send, list_to_binary(ezwebframe_mochijson2:encode([{struct,L1}]))}.

make_svg_object(Ws, X, L) ->
    Cmd= list_to_binary(atom_to_list(X)),
    L1 = [{cmd,<<"SVG.",Cmd/binary>>}|L],
    Ws ! {send, list_to_binary(ezwebframe_mochijson2:encode([{struct,L1}]))}.


new_index() ->
    N = get(free),
    put(free, N+1),
    N.

enc(L) ->
    L1 = [{struct,J} || J <- L],
    C = ezwebframe_mochijson2:encode(L),
    list_to_binary(C).

transform(X, Y, Scale) ->
    {transform,f2b("translate(~p,~p) scale(~p,~p)", [X,Y,Scale,-Scale])}.

text(Ws, X, Y, Str) ->
    make_svg_object(Ws, add_text, 
		    [{parent,svg},{x,X},{y,Y},{str,list_to_binary(Str)}]).


random_rect(Ws) ->
    {X0,Y0,Width,Ht} = rect_container(),
    Id = new_index(),
    X = X0 + pos_ran(Width),
    Y = Y0 + pos_ran(Ht),
    W = pos_ran(Width+X0-X) - 5,
    H = pos_ran(Ht+Y0 -Y) - 5,
    if
	H < 0; W < 0 ->
	    random_rect(Ws);
	true ->
	    make_generic_object(Ws,rect,
				[{rx,3},
				 {ry,3},
				 {parent,svg},
				 {x,X},{y,Y},{width,W},{height,H},
				 {fill,ran_color()}
				])
    end.

pos_ran(N) when N =< 0 ->
    1;
pos_ran(N) ->
    random:uniform(N).

ran_color() ->
    B1 = unsigned_byte_to_hex_string(random:uniform(255)),
    B2 = unsigned_byte_to_hex_string(random:uniform(255)),
    B3 = unsigned_byte_to_hex_string(random:uniform(255)),
    list_to_binary([$#,B1,B2,B3]).

wow() ->
    <<"M643 2c0 -102 -65 -214 -190 -248c0 -13 1 -27 1 -40c0 -46 -1 -92 -4 -138c-7 -119 -92 -227 -214 -227c-111 0 -202 92 -202 205c0 58 54 104 113 104c54 0 95 -48 95 -104c0 -52 -43 -95 -95 -95c-13 0 -27 4 -39 10c26 -47 74 -80 130 -80c100 0 166 94 172 193c3 44 4 89 4 133v31c-31 -5 -63 -6 -79 -6c-189 0 -333 173 -333 372c0 181 134 314 254 451c-37 129 -54 211 -54 379c0 197 147 308 159 308c25 0 151 -219 151 -388c0 -150 -90 -267 -190 -380c22 -73 42 -147 61 -221h6c154 0 254 -127 254 -259zM452 -207c66 20 124 84 124 170c0 90 -64 178 -168 192c27 -129 40 -239 44 -362zM338 -220c7 0 45 1 75 5c-4 127 -19 241 -47 372c-87 -5 -136 -62 -136 -124c0 -45 26 -92 83 -125c4 -4 7 -9 7 -14c0 -11 -9 -21 -20 -21c-15 0 -125 63 -125 186c0 90 61 178 168 198c-16 64 -35 127 -53 190c-110 -124 -220 -249 -220 -414c0 -149 144 -253 268 -253zM409 1108c-100 -55 -162 -159 -162 -273c0 -94 27 -190 40 -236c86 102 158 209 158 342c0 77 -11 110 -36 167z">>.
    

f2b(F, D) ->
    list_to_binary(lists:flatten(io_lib:format(F, D))).


loop(Ws, B) ->
    receive
	{msg,Json} ->
	    io:format("Received:~p~n",[Json]),
	    loop(Ws, B);
	X ->
	    io:format("handler got unexpected::~p~n",[X]),
	    loop(Ws, B)
    after 1000 ->
	    case B of
		true ->
		    random_rect(Ws),
		    loop(Ws, B);
		false ->
		    loop(Ws, B)
	    end
    end.

-spec unsigned_byte_to_hex_string(integer()) -> [byte()].

%% N is in -128 .. 127    

unsigned_byte_to_hex_string(N) when N >= 0, N < 256 ->
    [nibble_to_hex_char(N bsr 4),nibble_to_hex_char(N band 15)].

nibble_to_hex_char(X) when X < 10 -> $0 + X; 
nibble_to_hex_char(X) -> $a + X - 10.
