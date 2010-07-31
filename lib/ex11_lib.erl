-module(ex11_lib).

%% Copyright (C) 2003, 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% 2003-12-?? Original version by joe@sics.se
%% 2004-02-15 Added initial support for using multiple screens
%%            Frej Drejhammar <frej@stacken.kth.se>

-define(Vsn, "3.1").

-export([colors/0,
	 mkArc/6,
	 mkPoint/2,
	 mkRectangle/4,
	 eAllocColor/4,
	 eAllocNamedColor/2,
	 eChangeGC/2,
	 eClearArea/6, 
	 eConfigureWindow/2,
	 eConnect/1,
	 eCopyArea/9,
	 eCreateGC/3,
	 eCreateGlyphCursor/11,
	 eCreatePixmap/5,
	 eCreateWindow/11,
	 eDestroyWindow/1,
	 eFillPoly/5,
	 eFreeGC/1,
	 eFreePixmap/1,
	 eGetKeyboardMapping/2,
	 eImageText8/5,
	 eListFonts/2,
	 eMapWindow/1,
	 eOpenFont/2,
	 eParseEvent/2,
	 ePolyArc/3,
	 ePolyFillArc/3,
	 ePolyFillRectangle/3,
	 ePolyRectangle/3,
	 ePolyLine/4,
	 ePolyText8/5,
	 ePutImage/9,
	 eSetCloseDownMode/1,
	 eSetInputFocus/3,
	 eUnmapWindow/1,
	 xSetInputFocus/1,
	 pConnect/2,
	 pError/1,
	 pEvent/1,
	 pReply/2,
	 sleep/1,
	 xEnsureFont/2,
	 xEnsureNamedGC/3,
	 xClearArea/1,
	 xColor/2,
	 xCreateNamedGC/3,
	 xCreateNamedGC/4,
	 xCreateGC/2,
	 xCreateGC/3,
	 xCreateCursor/2,
	 xCreatePixmap/4,
	 xCreateSimpleWindow/7,
	 xCreateSimpleWindow/10,
	 xCreateWindow/10,
	 xDestroyGC/2,
	 xDo/2,
	 xFlush/1,
	 xFreePixmap/2,
	 xGC/2,
	 xGC/3,
	 xPen/3,
	 xPen/4,
	 xSendMeAllEvents/2,
	 xStart/1,
	 cmd/2, 
	 get_display/2,
	 get_root_of_screen/2,
	 new_id/1, 
	 free_id/2,
	 rpc/2,
	 reply/2,
	 xAddHandler/3,
	 xQueryFont/2,
	 xGetVar/2, 
	 xSetVar/3,
	 xMkTmpGC/2,
	 xMkTmpPen/3,
	 xVar/2
	]).


%% these are the widgets

-import(lists, [reverse/1, reverse/2]).

-import(ex11_lib_utils, [i2h/1]).

-import(lists, [map/2, member/2]).

-define(INT16, 16/big-signed-integer).

%% commands

-include("ex11_lib.hrl").

colors() -> ex11_lib_rgb:colors().

xStart(Vsn) ->
    case Vsn > ?Vsn of
	true ->
	    io:format("*** Warning ex11 library version=~p~n"
		      " required version=~p~n", [?Vsn, Vsn]);
	false ->
	    void
    end,
    case ex11_lib_control:start() of
	{ok, Pid, Screen} ->
	    put(ex11Pid, Pid),
	    init(Pid, Screen),
	    {ok, Pid};
	Error ->
	    Error
    end.

new_id(Pid) -> rpc(Pid, create_id).

add_gc_to_win(Pid, X) -> Pid ! {addGCtoWin, X}.
    

cmd(Pid, {cast, X}) -> Pid ! {sendCmd, X};
cmd(Pid, C)         -> rpc(Pid, {cmd, C}).

free_id(Pid, Id) -> rpc(Pid, {free_id, Id}).

get_display(Pid, Key) -> rpc(Pid, {get_display, Key}).

color(Pid, C) -> rpc(Pid, {color, C}).
    
%% xGetVar(Key) -> {ok, Val} | error

xGetVar(Pid, Key) -> rpc(Pid, {xGetVar, Key}).
    
xSetVar(Pid, Key, Val) -> rpc(Pid, {xSetVar, Key, Val}).

xColor(Pid, C) -> color(Pid, C).


xAddHandler(Pid, Win, Fun) ->
    xSetVar(Pid, {handler, Win}, Fun).

xFlush(Pid) -> cast(Pid, flush).

rpc(Pid, Q) ->
    Pid ! {self(), Q},
    receive
	{Pid, Reply} ->
	    %% io:format("Reply=~p~n",[Reply]),
	    Reply
    end.

reply(Pid, R) ->
    Pid ! {self(), R}.

cast(Pid, Q) ->
    Pid ! Q.

init(Pid, Screen) ->
    %% Screen is the screen we were started with
    %% But I'll ignore this ...
    %% Do all initialisation here
    %% first we create a sub-window which is unmapped
    %% this is parent all GC's
    Win = xVar(Pid, defaultWindow),
    xPen(Pid, "black",1,?black),
    xPen(Pid, "white",1,?white),
    Font   = xEnsureFont(Pid, "9x15"),
    xCreateNamedGC(Pid, "text", [{function, copy},
				 {font, Font},
				 {fill_style, solid},
				 {foreground, xColor(Pid, ?DarkBlue)}]),
    %% the dummy GC
    GC =  xCreateNamedGC(Pid, dummyGC, [{function,copy},
					{line_width,1},
					{line_style,solid},
					{foreground, xColor(Pid, ?black)}]),
    true.

xCreateCursor(Display, CursorId) ->    
    case xGetVar(Display, {cursor,CursorId}) of
	{ok, Val} ->
	    Val;
	error ->
	    CursorFont = xEnsureFont(Display, "cursor"),
	    Id = new_id(Display),
	    cmd(Display, eCreateGlyphCursor(Id, 
					    CursorFont, CursorFont,
					    CursorId,
					    CursorId,
					    0,0,0,
					    255,255,255)),
	    xSetVar(Display, {cursor,CursorId}, Id),
	    Id
    end.

xClearArea(Area) ->
    eClearArea(false,Area,0,0,0,0).

xCreateSimpleWindow(Display, X, Y, Width, Ht, Cursor, Bg) ->
    xCreateWindow(Display,top, X, Y, Width, Ht, 1, 0, 0, 
		  [{backgroundPixel, Bg},
		   {eventMask, 
		    ?EVENT_EXPOSURE bor 
		    ?EVENT_STRUCTURE_NOTIFY bor 
		    ?EVENT_BUTTON_PRESS bor 
		    ?EVENT_BUTTON1_MOTION},
		   {backgroundPixmap, 0}, 
		   {cursor, xCreateCursor(Display,Cursor)},
		   {borderPixmap,0}]).

%% Note color is set differently

xCreateSimpleWindow(Display, 
		    Parent, X, Y, Width, Ht, BorderWidth, Cursor, Col, Mask) ->
    Opts = [{eventMask, Mask},
	    {cursor,  xCreateCursor(Display, Cursor)},
	    {backgroundPixel, Col}],
    xCreateWindow(Display, Parent, X, Y, Width, Ht, BorderWidth, 0, 0,
		  Opts).

xCreateWindow(Display, Parent, X, Y, Width, Ht, BorderWidth, Class, Visual,
	      Opts) ->  
    ParentWin = case Parent of
		    top -> xVar(Display, defaultWindow);
		    _   -> Parent
		end,
    Depth = xVar(Display, {depth, ParentWin}),
    WindowId = new_id(Display),
    Display ! {newWindow,self(),WindowId,ParentWin,Depth},
    Cmd = eCreateWindow(Depth, WindowId, ParentWin, X, Y, Width, Ht,
			BorderWidth, Class, Visual, Opts),
    cmd(Display, Cmd),
    WindowId.

xCreatePixmap(Display, Drawable, Width, Ht) ->
    Id = new_id(Display),
    % XXXX Wrong, the depth should be fetched from the parent
    % get_depth(Display),
    % Screen = get_display(Display, default_screen),
    % Depth = get_display(Display, {depth, Screen}),
    Depth = xVar(Display, {depth, Drawable}),
    cmd(Display, eCreatePixmap(Depth, Id, Drawable, Width, Ht)),
    Id.

xEnsureFont(Display, FontNameStr) ->
    case xGetVar(Display, {font,FontNameStr}) of
	{ok, Id} -> 
	    Id;
	error ->
	    Id = new_id(Display),
	    %% io:format("xEnsureFont creating a new font: Id=~p font=~p~n",
	    %% [Id, FontNameStr]),
	    cmd(Display, eOpenFont(Id, FontNameStr)),
	    xSetVar(Display, {font,FontNameStr}, Id),
	    Id
    end.

%% GC's
%% Named GC's
%%   Creating:
%%     xCreateNamedGC(Display, Name, Screen, Opts) -> Id
%%     xCreateNamedGC(Display, Name, Opts)         -> Id  (default screen)
%%   Using:
%%     xGC(Display, {Screen, Name}) -> Id
%%     xGC(Display, Name) -> Id
%% Per-window GC's (these are destroyed when the window is deleted)
%%   Creating:
%%     xCreateGC(Display, Win, Opts) -> Id
%%     xCreateGC(Display, Opts) -> Id (default screen and root window)
%%   Changing:
%%     eChangeGC(Gc, Opts) -> void
%%   Destroying
%%     xDestroyGC(Display, Id)
 
xCreateNamedGC(Display, Name, Opts) ->
    Win    = xVar(Display, defaultWindow),
    Id     = new_id(Display),
    cmd(Display, eCreateGC(Id, Win, Opts)),
    xSetVar(Display, {gc, Name}, Id),
    Id.

xCreateNamedGC(Display, Name, Screen, Opts) ->
    Id  = new_id(Display),
    Win = xVar(Display, {defaultWindow, Screen}),
    cmd(Display, eCreateGC(Id, Win, Opts)),
    xSetVar(Display, {gc, {Screen, Name}}, Id),
    Id.

xGC(Display, Key) -> xVar(Display, {gc, Key}).

xCreateGC(Display, Win, Opts) ->
    Id = new_id(Display),
    add_gc_to_win(Display, {Win, Id}),
    cmd(Display, eCreateGC(Id, Win, Opts)),
    Id.

xCreateGC(Display, Opts) ->
    Id = new_id(Display),
    %% io:format("creating GC Name=~p Opts=~p~n",[{gc,Name},Opts]),
    Win = xVar(Display, defaultWindow),
    cmd(Display, eCreateGC(Id, Win, Opts)),
    Id.

xDestroyGC(Display, Id) ->
    cmd(Display, eFreeGC(Id)),
    free_id(Display, Id).

xEnsureNamedGC(Display, Name, Opts) ->
    case xGetVar(Display, {gc,Name}) of
	{ok, GC} -> GC;
	error    -> xCreateNamedGC(Display, Name, Opts)
    end.

xFreePixmap(Display, Id) ->
    xDo(Display, eFreePixmap(Id)),
    free_id(Display, Id).

xPen(Display, Name, Width, Color) ->
    case xGetVar(Display, {gc, Name}) of
	{ok, GC} ->
	    GC;
	error ->
	    xCreateNamedGC(Display, Name, 
			   [{function,copy},
			    {line_width,Width},
			    {line_style,solid},
			    {foreground, xColor(Display, Color)}])
    end.

xPen(Display, Width, Color) ->
    xCreateGC(Display, [{function,copy},
			{line_width,Width},
			{line_style,solid},
			{foreground, xColor(Display, Color)}]).

xSendMeAllEvents(Display, But) ->
    S = self(),
    xAddHandler(Display, But, fun(X) -> S ! {event, X} end).

%% Turn a string into a Name

%% xGC(Display, Id) when integer(Id) -> Id;
%% xGC(Display, Name)                -> xGC(Display, Name, "black").

xGC(Display, Name, Default) ->
    case xGetVar(Display, {gc,Name}) of
	{ok, GC} ->
	    GC;
	error ->
	    case xGetVar(Display, {gc, Default}) of
		{ok, GC} ->
		    GC;
		error ->
		    io:format("** missing variable {gc,~p}~n",[Name]),
		    io:format("AND missing {gc,~p}~n",[Default])
	    end
    end.


%%---------------------------------------------------------------------
%% Parsing reply routines
%%    The names of these routines always starts with a p


%%----------------------------------------------------------------------
%% pConnect(Bin, Screen) -> {ok, #display} | {error, Why}

pConnect(Bin, Screen) ->    
    case Bin of
	<<0, Emsg/binary>>  -> {error,decode_refused_connection(Emsg)};
	<<1,_,Msg/binary>>  -> decode_accepted_connect(Msg, Screen);
	<<2,_/binary>>      -> {error,"authentication required"}
    end.

decode_refused_connection(<<Len,ProtMaj:16,ProtMin:16,_:16,D/binary>>) ->
    <<ReasonB:Len/binary, _/binary>> = D,
    Reason = binary_to_list(ReasonB),
    {error, {eConnectionRefused, ProtMaj, ProtMin, Reason}}.

decode_accepted_connect(Msg, ScreenNo) ->
    <<_:32, _Len:16, RelNo:32, ResBase:32, ResMask0:32,
     MbufSz:32, VendorLen:16, MaxReqLen:16,
     Screens, Formats,
     ImOrder, BmapOrder, BmapScanU, BmapScanP,
     MinKCode, MaxKCode, _:32,
    B/binary>> = Msg,
    VendorPad = pad_size(VendorLen),
    PmapFormLen = 8*Formats,
    <<VendorB:VendorLen/binary,
     _:VendorPad/binary,
     PmapForm:PmapFormLen/binary,
     PmapScreen/binary>> = B,
    {ResMask,ResShift} = resource_calc(ResMask0),
    Vendor = binary_to_list(VendorB),
    Format = decode_format(Formats,PmapForm),
    Screen = decode_screen(Screens,PmapScreen),
    Display = #display{
      resource_mask=ResMask,
      resource_base=ResBase,
      resource_shift=ResShift,
      release=RelNo,
      motion_buffer=MbufSz,
      max_request_size=MaxReqLen,
      bitmap_bit_order=BmapOrder,
      bitmap_unit=BmapScanU,
      bitmap_pad=BmapScanP,
      byte_order=ImOrder,
      vendor=Vendor,
      min_keycode=MinKCode,
      max_keycode=MaxKCode,
      nscreens=Screens,
      screens=Screen,
      default_screen=ScreenNo,
      nformats=Formats,
      pixmap_formats=Format},
    {ok, Display}.

%% Eh...what a hell am I doing here ? See OpenDis.c l.374
resource_calc(ResMask0) ->
    resource_calc(ResMask0,ResMask0,0).

resource_calc(ResMask,Mask,ResShift) when (Mask bor 1) =/= 0 ->
    {(ResMask bsr ResShift) - 5, % ResMask
     ResShift};
resource_calc(ResMask,Mask,ResShift) ->
    resource_calc(ResMask,Mask bsr 1,ResShift+1).

decode_format(0, _) -> [];
decode_format(N, <<Depth,
		  Bpp,
		  ScanlinePad,
		  _:40,
		  T/binary>>) when N>0 ->
    Format = #format{depth=Depth,
		     bpp=Bpp,
		     scanline_pad=ScanlinePad
		    },
    [Format | decode_format(N-1,T)].

decode_screen(0, _) -> [];
decode_screen(N, <<Window:32,
		  Colormap:32,
		  WhitePixel:32, BlackPixel:32,
		  CinpMask:32,
		  WidthInPixel:16, HeightInPixel:16,
		  WidthInMm:16, HeightInMm:16,
		  MinInstMaps:16, MaxInstMaps:16,
		  RootVisual:32,
		  Bs, Su, Rd, Nd,
		  D/binary>>) when N>0 ->

    {Depths, Rest} = decode_depth(Nd,D),
    Screen = #screen{root=Window,
		     cmap=Colormap,
		     white_pixel=WhitePixel,
		     black_pixel=BlackPixel,
		     root_input_mask=CinpMask,
		     width=WidthInPixel,
		     height=HeightInPixel,
		     mwidth=WidthInMm,
		     mheight=HeightInMm,
		     max_maps=MaxInstMaps,
		     min_maps=MinInstMaps,
		     root_visual=RootVisual,
		     backing_store=Bs,
		     save_unders=Su,
		     depths=Depths,
		     root_depth=Rd},
    [Screen | decode_screen(N-1,Rest)].

decode_depth(Nd,D) -> decode_depth(Nd,D,[]).

decode_depth(0,Rest,Acc) -> {lists:reverse(Acc),Rest};
decode_depth(Nd,<<Depth,_,NoVisuals:16,_:32,T/binary>>,Acc) when Nd>0 ->
    NV = 24*NoVisuals,
    <<Vis:NV/binary,Rest/binary>> = T,
    Visuals = decode_visuals(NoVisuals, Vis),
    D = #depth{depth=Depth, nvisuals=NoVisuals, visuals=Visuals},
    decode_depth(Nd-1, Rest, [D|Acc]).

decode_visuals(0,_) -> [];
decode_visuals(Nv,<<VisualId:32,Class,BpRGB,ColMapEnt:16,
		   RedMask:32, GreenMask:32, BlueMask:32,
		   _:32, Rest/binary>>) when Nv>0 ->
    Visual = #visual{visualid=VisualId, class=Class,
		     bits_per_rgb=BpRGB, map_entries=ColMapEnt,
		     red_mask=RedMask, green_mask=GreenMask,
 blue_mask=BlueMask},
    [Visual | decode_visuals(Nv-1,Rest)].

%%----------------------------------------------------------------------

mkPoint(X, Y) -> <<X:16, Y:16>>.

mkRectangle(X, Y, W, H) -> <<X:16,Y:16,W:16,H:16>>.  

mkArc(X, Y, W, H, A1, A2) -> <<X:16,Y:16,W:16,H:16,A1:16,A2:16>>.  

eConnect([]) ->
    <<?MSB_BYTEORDER:8,      % Erlang byte-order
     0,                      % unused
     11:16,                  % proto-major-ver
     0:16,                   % proto-minor-ver
     0:16,                   % proto-name-len
     0:16,                   % proto-data-len
     0,0>>;                  % unused (pad)
eConnect(Cookie) ->
    Len = length(Cookie),
    Pad = add_pad(Cookie),
    C   = list_to_binary(Cookie),
    <<?MSB_BYTEORDER:8,      % Erlang byte-order
     0,                      % unused
     11:16,                  % proto-major-ver
     0:16,                   % proto-minor-ver
     18:16,                  % proto-name-len
     Len:16,                 % proto-data-len
     0:16,                   % unused
     "MIT-MAGIC-COOKIE-1",   % auth-proto-name
     0:16,                   % pad
     C/binary,               % auth-proto-data
     Pad/binary>>.           % pad

eAllocColor(Cmap, R, G, B) ->
    call(84, <<Cmap:32,R:16,G:16,B:16>>, eAllocColor).

eAllocNamedColor(Cmap, Str) ->
    Len = length(Str), B=list_to_binary(Str),
    call(85, <<Cmap:32,Len:16,0:16,B/binary>>, eAllocNamedColor).

eChangeGC(Cid, Values) -> 
    {ValueMask, Data} = encode_GC_options(Values),
    req(56, <<Cid:32, ValueMask:32, Data/binary>>).	

eClearArea(Bool, Window, X, Y, Width, Ht) ->
    B = eBool(Bool),
    req(61, B, <<Window:32,X:16,Y:16,Width:16,Ht:16>>).

eConfigureWindow(Win, Opts) ->
    {ValueMask, Data} = encode_config_win_options(Opts),
    req(12, <<Win:32, ValueMask:16, 0:16,Data/binary>>).

eCopyArea(SrcDrawable, DestDrawable, GC, X_src, Y_src, X_dest, Y_dest,
	  Width, Height) ->
    req(62, <<SrcDrawable:32, DestDrawable:32, GC:32, 
	     X_src:16, Y_src:16, X_dest:16, Y_dest:16,
	     Width:16, Height:16>>).

eCreateGC(Cid, Drawable, Values) -> 
    {ValueMask, Data} = encode_GC_options(Values),
    req(55, <<Cid:32, Drawable:32, ValueMask:32, Data/binary>>).	

eCreateGlyphCursor(Cursor, SrcFont, MaskFont, SourceChar, MaskChar,
		   FgRed, FgGreen, FgBlue, BgRed, BgGreen, BgBlue) ->
    req(94, <<Cursor:32, SrcFont:32, MaskFont:32, SourceChar:16, 
	     MaskChar:16,
	     FgRed:16, FgGreen:16, FgBlue:16, 
	     BgRed:16, BgGreen:16, BgBlue:16>>).

eCreatePixmap(Depth, PixMap, Drawable, Width, Ht) ->
    req(53, Depth, <<PixMap:32, Drawable:32, Width:16, Ht:16>>).

eCreateWindow(Depth, Wid, Parent, X, Y, Width, Ht, BorderWidth, Class,
	      Visual, Opts) ->
    %% io:format("eCreateWindow:~p~n",
    %% [{Depth, Wid, Parent, X, Y, Width, Ht, BorderWidth, Class,
    %% Visual, Opts}]),
    ClassOp = case Class of
		  0 -> 0;
		  copyFromParent -> 0;
		  inputOutput -> 1;
		  inputOnly -> 2
	      end,
    VisualId = case Visual of
		   copyFromParent -> <<0:32>>;
		   _ -> <<Visual:32>>
	       end,
    {ValueMask, Data} = encode_window_options(Opts),
    req(1, Depth, <<Wid:32, Parent:32, X:16, Y:16, Width:16, Ht:16,
		   BorderWidth:16, ClassOp:16, VisualId/binary,
		   ValueMask:32, Data/binary>>).

eDestroyWindow(Win) ->
    req(4, <<Win:32>>).

eFillPoly(Drawable, GC, Shape, CoordMode, Points) ->
    ShapeOpCode = case Shape of
		      complex -> 0;
		      nonConvex -> 1;
		      convex -> 2
		  end,
    ModeOpCode = case CoordMode of
		     origin    -> 0;
		     previous  -> 1
		 end,
    X = list_to_binary(Points),
    req(69, 0, <<Drawable:32, GC:32, ShapeOpCode:8,
		ModeOpCode:8,0:16,X/binary>>).

eFreeGC(GC) ->
    req(60, <<GC:32>>).

eFreePixmap(Id) ->
    req(54, <<Id:32>>).

xGetAtomName(Display, N) ->
    %% io:format("xGetAtomName:~p~n",[N]),
    xDo(Display, eGetAtomName(N)).

eGetAtomName(N) ->
    call(17,<<N:32>>,eGetAtomName).

eGetKeyboardMapping(First, Last) ->
    Count = Last - First + 1,
    call(101, <<First:8,Count:8,0:16>>, {eGetKeyboardMapping,First}).

eImageText8(Drawable, GC, X, Y, Str) ->
    Len = length(Str),
    BStr = list_to_binary(Str),
    B = <<BStr/binary>>,
    req(76, Len, <<Drawable:32, GC:32, X:16, Y:16, B/binary>>).

eListFonts(Max, Str) ->	
    B = list_to_binary(Str), Len = length(Str),
    call(49, <<Max:16,Len:16,B/binary>>, eListFonts).
	      
eMapWindow(W) ->
    req(8, <<W:32>>).

eOpenFont(Fid, Str) ->
    Len = length(Str),
    B = list_to_binary(Str),
    req(45, <<Fid:32, Len:16,0:16, B/binary>>).

ePolyArc(Drawable, Gc, Arcs) ->
    X = list_to_binary(Arcs),
    req(68, <<Drawable:32, Gc:32, X/binary>>).
    

ePolyFillArc(Drawable, Gc, Arcs) ->
    X = list_to_binary(Arcs),
    req(71, <<Drawable:32, Gc:32, X/binary>>).
    

ePolyFillRectangle(Drawable, Gc, Rects) ->
    X = list_to_binary(Rects),
    req(70, <<Drawable:32, Gc:32, X/binary>>).

ePolyLine(Drawable, GC, CoordMode, Points) ->
    ModeOpCode = case CoordMode of
		     origin    -> 0;
		     previous  -> 1
		 end,
    X = list_to_binary(Points),
    req(65, ModeOpCode, <<Drawable:32, GC:32, X/binary>>).

ePolyRectangle(Drawable, Gc, Rects) ->
    X = list_to_binary(Rects),
    req(67, <<Drawable:32, Gc:32, X/binary>>).

ePolyText8(Drawable, GC, X, Y, Str) ->
    Len = length(Str),
    Delta = 2,
    BStr = list_to_binary(Str),
    B = <<Len:8,Delta:8, BStr/binary>>,
    req(74, <<Drawable:32, GC:32, X:16, Y:16, B/binary>>).


ePutImage(Draw, GC, Width, Ht, X, Y, Pad, Depth, Data) ->
    req(72, 2, <<Draw:32,GC:32,Width:16,Ht:16,X:16,Y:16,Pad:8,Depth:8,
		0:16,Data/binary>>).

eSetCloseDownMode(Mode) ->
    X = case Mode of
	    destroy -> 0;
	    1 -> retainPermanent;
	    2 -> retainTemporary
	end,
    req(112, X, <<>>).

eSetInputFocus(Revert, Window, TimeStamp) ->
    Mode = case Revert of
	       none -> 0;
	       pointerRoot -> 2;
	       parent -> 3
	   end,
    req(42,Mode,<<Window:32,TimeStamp:32>>).

eUnmapWindow(Win) ->
    req(10, <<Win:32>>).
    
%% query the font 
xQueryFont(Display, Str) ->
    Id  = xEnsureFont(Display, Str),
    Cmd = eQueryFont(Id),
    {ok, Val} = xDo(Display, Cmd),
    %% io:format("Font:~p=~p~n",[Str, Val]),
    Props = Val#fontInfo.font_props,
    %% io:format("Props=~p~n",[Props]),
    Props1 = map(fun({Atom,Val1}) ->
			 {ok, Name} = xGetAtomName(Display, Atom),
			 %% io:format("Name=~p~n", [Name]),
			 {Name, Val1}
		 end, Props),
    Val#fontInfo{font_props=Props1}.

eQueryFont(Id) ->
    call(47, <<Id:32>>, eQueryFont).


xSetInputFocus(Window) ->
    eSetInputFocus(none, Window, 0).



%%----------------------------------------------------------------------
%% requests are *allways* a multiple of 4 bytes
%% the first 4 bytes are [Op,Mod,N1,N2]
%% Len = N1*256 ? N2
%% The Len = length of command in 4 byte units

call(Op, Bin, Reply) ->
    {call, pack_request(Op, 0, Bin), Reply}.

req(Op, Bin)       -> {cast, pack_request(Op, 0, Bin)}.

req(Op, Code, Bin) -> {cast, pack_request(Op, Code, Bin)}.

pack_request(Op, Code, Bin) ->
    %% io:format("pack_request Op=~p Code=~p Bin=~p~n",[Op,Code,Bin]),
    Bin1 = pad_bin(Bin),
    Len = (size(Bin1) div 4) + 1,
    <<Op:8,Code:8,Len:16,Bin1/binary>>.

%% pad_bin(Bin) -> Bin'
%%   adds extra bytes to Bin to make the length of Bin a multiple of 4

pad_bin(Bin) ->
    case size(Bin) rem 4 of
	0 -> Bin;
	1 -> <<Bin/binary, 0:24>>;
	2 -> <<Bin/binary, 0:16>>;
	3 -> <<Bin/binary, 0:8>>
   end.
    
%% pad(Bin, Len) ->
%%     io:format("pad ~p ~w~n",[Bin, Len]),
%%    case Len rem 4 of
%%        0 -> Bin;
%%        1 -> <<Bin/binary, 0:8>>;
%%        2 -> <<Bin/binary, 0:16>>;
%%        3 -> <<Bin/binary, 0:24>>
%%    end.

pad_size(E) -> (4 - (E rem 4)) rem 4.

add_pad(Data) -> pad(Data).
    
pad(Data) when is_binary(Data) -> 
    pad_0(pad_size(size(Data)));
pad(Data) when is_list(Data) -> 
    pad_0(pad_size(length(Data))).

pad_0(0) -> <<>>;
pad_0(1) -> <<0>>;
pad_0(2) -> <<0,0>>;
pad_0(3) -> <<0,0,0>>.

pError(<<0:8,Error:8,Seq:16,BadRes:32,Mi:16,Ma, _:168, Rest/binary>>) ->
    {error, Seq, BadRes, {err_type(Error), Ma, Mi, Rest}}.

err_type(1)  -> request;
err_type(2)  -> value;
err_type(3)  -> window;
err_type(4)  -> pixmap;
err_type(5)  -> atom;
err_type(6)  -> cursor;
err_type(7)  -> font;
err_type(8)  -> match;
err_type(9)  -> drawable;
err_type(10) -> access;
err_type(11) -> alloc;
err_type(12) -> colormap;
err_type(13) -> gcontext;
err_type(14) -> idchoice;
err_type(15) -> name;
err_type(16) -> length;
err_type(17) -> implementation;
err_type(_)  -> undefined.

pEvent(<<N,_/binary>> = B) ->
    {event, event_name(N), B}.

event_name(X) ->
    case X of
	2 -> keyPress;
	3 -> keyRelease;
	4 -> buttonPress;
	5 -> buttonRelease;
	6 -> motionNotify;
	7 -> enterNotify;
	8 -> leaveNotify;
	9 -> focusIn;
	10 -> focusOut;
	11 -> keymapNnotify;
	12 -> expose;
	13 -> graphicsExposure;
	14 -> noExposure;
	15 -> visibilityNotify;
	16 -> createNotify;
	17 -> destroyNotify;
	18 -> unmapNotify;
        19 -> mapNotify;
	20 -> mapRequest;
	21 -> reparentNotify;
	22 -> configureNotify;
	23 -> configureRequest;
	24 -> gravityNotify;
	25 -> resizeRequest;
	26 -> circulateNotify;
	27 -> circulateRequest;
	28 -> propertyNotify;
	29 -> selectionClear;
	30 -> selectionRequest;
	31 -> selectionNotify;
	32 -> colormapNotify;
	33 -> client_message;
	34 -> mappingNotify;
	N -> {unknown, N}
    end.


%%----------------------------------------------------------------------
encode_config_win_options(L) ->
    encode([x,y,width,ht,borderWidth,sibling,stackMode],
	   L, 0, 16#40, []).

encode_window_options(L) ->
    encode([backgroundPixmap,
	    backgroundPixel,
	    borderPixmap,
	    borderPixel,
	    bitGravity,
	    winGravity,
	    backingStore,
	    backingPlanes,
	    backingPixel,
	    overrideRedirect,
	    saveUnder,
	    eventMask,
	    doNotPropogateMask,
	    colorMap,	
	    cursor], L, 0,  16#4000, []).

encode_GC_options(L) ->
    encode([function,
	    plane_mask,
	    foreground,
	    background,
	    line_width,
	    line_style,
	    cap_style,
	    join_style,
	    fill_style,
	    fill_rule,
	    tile,
	    stipple,
	    tile_stipple_x_origin,
	    tile_stipple_y_origin,
	    font,
	    subwindow_mode,
	    graphics_exposures,
	    clip_x_origin,
	    clip_y_origin,
	    clip_mask,
	    dash_offset,
	    dashes,
	    arc_mode], L, 0, 16#400000, []).

encode_value(backgroundPixmap, none) -> 0;
encode_value(backgroundPixmap, parentRelative) -> 1;
encode_value(backgroundPixmap, N) -> N;
encode_value(backgroundPixel, N) -> N;
encode_value(borderPixmap, N) -> N;
encode_value(borderPixel, N) -> N;
encode_value(bitGravity, N) -> eBitGravity(N);
encode_value(winGravity, N) -> eWinGravity(N);
encode_value(backingStore, notUseful) -> 0;
encode_value(backingStore, whenMapped) -> 1;
encode_value(backingStore, always) -> 2;
encode_value(backingPlanes, N) -> N;
encode_value(backingPixel, N) -> N;
encode_value(overrideRedirect, B) -> eBool(B);
encode_value(saveUnder, B) -> eBool(B);
encode_value(eventMask, N) -> N;
encode_value(doNotPropogateMask, N) -> N;
encode_value(colorMap, N) -> N;
encode_value(cursor, N) -> N;
encode_value(function, X)   -> eFunction(X);
encode_value(plane_mask, N) -> N;
encode_value(foreground, N) -> N;
encode_value(background, N) -> N;
encode_value(line_width, N) -> N;
encode_value(line_style, N) -> eLineStyle(N);
encode_value(cap_style, N)  -> eCapStyle(N);
encode_value(join_style, N) -> eJoinStyle(N);
encode_value(fill_style, N) -> eFillStyle(N);
encode_value(fill_rule, N)  -> eFillRule(N);
encode_value(tile, N)       -> N;
encode_value(tile_stipple_x_origin, N) -> N;
encode_value(tile_stipple_y_origin, N) -> N;
encode_value(font, N)       -> N;
encode_value(subwindow_mode, N) -> eSubWindowMode(N), N;
encode_value(graphics_exposures, B) -> eBool(B);
encode_value(clip_x_origin, N) -> N;
encode_value(clip_y_origin, N) -> N;
encode_value(clip_mask, N)     -> N;
encode_value(dash_offset, N)   -> N;
encode_value(dashes, N)        -> N;
encode_value(arc_mode, N)      -> eArcMode(N);
encode_value(x, N)             -> N;
encode_value(y, N)             -> N;
encode_value(width, N)         -> N;
encode_value(ht, N)            -> N;
encode_value(borderWidth,N)    -> N;
encode_value(sibling, N)       -> N;
encode_value(stackMode, N)     -> eStackMode(N).

eBool(true)  -> 1;
eBool(false) -> 0.

eBitGravity(X) ->
    case X of
	forget -> 0;
	northwest -> 1;
	north -> 2;
	northeast -> 3;
	west -> 4;
	center -> 5;
	east -> 6;
	southwest -> 7;
	south -> 8;
	southeast -> 9;
	static -> 10
    end.

eWinGravity(X) ->
    case X of
	unmap -> 0;
	northwest -> 1;
	north -> 2;
	northeast -> 3;
	west -> 4;
	center -> 5;
	east -> 6;
	southwest -> 7;
	south -> 8;
	southeast -> 9;
	static -> 10
    end.


eFunction(X) ->
    case X of
	clear -> 0;
	'and' -> 1;
	andReverse -> 2;
	copy -> 3;
	andInverted -> 4;
	noOp -> 5;
	'xor' -> 6;
	'or' -> 7;
	'nor' -> 8;
	equiv -> 9;
	invert -> 10;
	orReverse -> 11;
	copyInverted -> 12;
	orInverted -> 13;
	'nand' -> 14;
	set -> 15
    end.


eLineStyle(solid) -> 0;
eLineStyle(onOffDash) -> 1;
eLineStyle(doubleDash) -> 2.

eCapStyle(notLast) -> 0;
eCapStyle(butt) -> 1;
eCapStyle(round) -> 2;
eCapStyle(projecting) -> 3.

eJoinStyle(miter) -> 0;
eJoinStyle(round) -> 1;
eJoinStyle(bevel) -> 2.

eFillStyle(solid)   -> 0;
eFillStyle(tiled) -> 1;
eFillStyle(stippled) -> 2;
eFillStyle(opaqueStippled) -> 3.

eFillRule(evenOdd) -> 0;
eFillRule(winding) -> 1.

eSubWindowMode(clipByChildren) -> 0;
eSubWindowMode(includeInferiors) -> 1.

eArcMode(chord)    -> 0;
eArcMode(pieSlice) -> 1.

eStackMode(above) -> 0;
eStackMode(below) -> 1;
eStackMode(topIf) -> 2;
eStackMode(bottomIf) -> 3;
eStackMode(opposite) -> 4.


%%----------------------------------------------------------------------

encode([H|T], L, V, Mask, Bin) ->
    case contains(H, L, []) of
	{yes, Val, L1} ->
	    %% io:format("encodeing:~p~n",[{H,Val}]),
	    B1 = encode_value(H, Val),
	    V1 = (V bsr 1) bor Mask,
	    encode(T, L1, V1, Mask, [<<B1:32>>|Bin]);
	no ->
	    V1 = V bsr 1,
	    encode(T, L, V1, Mask, Bin)
    end;
encode([], [], V, _, Bin) ->
    R = reverse(Bin),
    %% io:format("V=~p bin=~p~n",[V, R]),
    {V, list_to_binary(R)};
encode([], L, V, _, Bin) ->
    exit({badOptionIn,eCreateWindow,L}).

contains(Key,[{Key,Val}|L1], L2) -> {yes, Val, reverse(L2, L1)};
contains(Key,[H|L1], L2)         -> contains(Key, L1, [H|L2]);
contains(Key, [], _)             -> no.

%%----------------------------------------------------------------------

pReply(Type, Bin) ->
    case (catch pReply1(Type, Bin)) of
	{'EXIT', Why} ->
	    io:format("This is an error:~p -- error parsing reply:~p~n",
		      [Why, Type]),
	    {error, {parse, Type, Why}};
	Other ->
	    Other
    end.

pReply1(eAllocColor, <<_:48,R:16,G:16,B:16,_:16,Pixel:32,_/binary>>) ->
    {R,G,B,Pixel};
pReply1(eAllocNamedColor, <<_:48,Pixel:32,ER:16,EG:16,EB:16,VR:16,VG:16,VB:16,
			   _/binary>>) ->
    {Pixel, ER,EG,EB,VR,VG,VB};
pReply1(eGetAtomName, <<_:64,Need:16,_:22/binary,S1/binary>>) ->
    case size(S1) of
	Got when Got >= Need -> 
	    {B1,_} = split_binary(S1, Need),
	    binary_to_list(B1);
	_ ->
	    binary_to_list(S1)
    end;
pReply1({eGetKeyboardMapping,First}, <<_:8,KeySymsPerKeycode:8,_:16,ReplyLen:32,_:192,
			      Stuff/binary>>) ->
    Is = pKeySymbs(Stuff),
    io:format("KeySymsPerKeycode=~p~n",[KeySymsPerKeycode]),
    Parse = gather_keysyms(Is, First, KeySymsPerKeycode, []),
    {keys, Parse};
pReply1(eListFonts, <<_:48,NumberOfFonts:16,_:176, B/binary>> = B1) ->
    %% 176 = 22 bytes of unused stuff ...
    %% io:format("Nfonts=~p~n", [NumberOfFonts]),
    %% Note Number of fonts usually is wrong so I don't retain it
    Fonts = pFontList(B, []);
pReply1(eQueryFont, <<_:64,
		     MinBounds:12/binary,       % note the units of binary
						% are inbytes
		     _:32,
		     MaxBounds:12/binary,
		     _:32,
		     MinByte2:16,
		     MaxByte2:16,
		     DefaultChar:16,
		     NFontProps:16,
		     DrawDirection:8,
		     MinByte1:8,
		     MaxByte1:8,
		     AllCharsExist:8,
		     FontAscent:?INT16, 
		     FontDescent:?INT16,
		     NCharInfos:32,
		     Rest/binary>>) ->
    %% io:format("Here size(Rest)=~p~n",[size(Rest)]),
    Cmin = pCharInfo(MinBounds),
    Cmax = pCharInfo(MaxBounds),
    {FontProps, T} = pFontProps(NFontProps, Rest, []),
    {CharInfos, _} = pCharInfos(NCharInfos, T, []),
    NumberOfChars = MaxByte1 - MinByte1 + 1,
    Found = length(CharInfos),
    %% io:format("N Chars = ~p (~p..~p) found ~p descriptions~n",
    %% [NumberOfChars, MinByte1, MaxByte1, Found]),
    %% io:format("MinByte2=~p MaxByte2=~p~n",[MinByte2, MaxByte2]),
    #fontInfo{min_bounds=Cmin, max_bounds=Cmax,
	      min_byte2=MinByte2, max_byte2=MaxByte2,
	      default_char=DefaultChar, draw_direction=DrawDirection,
	      min_byte1=MinByte1,max_byte1=MaxByte1,
	      all_chars_exist=AllCharsExist,font_ascent=FontAscent,
	      font_descent=FontDescent,
	      font_props=FontProps,
	      char_infos=CharInfos}.

pKeySymbs(<<>>) -> [];
pKeySymbs(<<I:32,Rest/binary>>) -> [I|pKeySymbs(Rest)].
    
gather_keysyms([], _, _, L) ->
    reverse(L);
gather_keysyms(Is, First, N, L) ->
    {Keys,Rest} = take(N, Is, []),
    gather_keysyms(Rest, First+1, N, [{First,Keys}|L]).

take(0, T, L)     -> {reverse(L), T};
take(N, [H|T], L) -> take(N-1, T, [H|L]);
take(N, [], L)    -> {reverse(L), []}.

pFontProps(0, B, L) -> 
    {reverse(L), B};
pFontProps(N, <<Atom:32,Value:32,R/binary>>, L) ->
    pFontProps(N-1, R, [{Atom,Value}|L]).

pCharInfos(0, B, L) -> 
    {reverse(L), B};
pCharInfos(N, <<B1:12/binary,R/binary>>, L) ->
    pCharInfos(N-1, R, [pCharInfo(B1)|L]).

pCharInfo(<<Left:?INT16,Right:?INT16,Width:?INT16,
	   Ascent:?INT16,Descent:?INT16,Attr:?INT16>>) ->
    #charInfo{left_side_bearing=Left,
	      right_side_bearing=Right,
	      width=Width,
	      ascent=Ascent,
	      descent=Descent,
	      attributes=Attr};
pCharInfo(B) ->
    io:format("Size Here=~p~n",[size(B)]).

%% Parsing a font list is strange - sometimes the list
%% appears to be truncated - this may indictae a bug or it may
%% be that the documentation just doesn't work
%% also the number of matching fonts seems wrong

pFontList(<<N:16,B/binary>>, L) ->
    get_str(B, []).

get_str(<<>>, L) ->
    reverse(L);
get_str(<<0,B/binary>>, L) ->
    get_str(B, L);
get_str(<<N:8,B/binary>>, L) ->
    case size(B) of
	Len when Len >= N ->
	    {B1, B2} = split_binary(B, N),
	    get_str(B2, [binary_to_list(B1)|L]);
	_ ->
	    %% this case is puzzeling the buffer seems to
	    %% be truncated
	    reverse(L)
    end.

%%----------------------------------------------------------------------
%% Event parsing routines

%% eParseEvent(Type, Bin) -> {Win, Args}.

eParseEvent(expose, <<12:8,_:24,Win:32,_/binary>>) ->
    %% pg 136
    {Win, expose};
eParseEvent(keyPress, <<2:8,KeyCode:8,_:80,Win:32,_:96,State:16,_/binary>>) ->
    %% pg 192
    %% Note the keypess numbers are the same as occur in xev
    %% have to add my own conversion routines
    %% Key is the phyiscal key number which is totally unrelated to
    %% The asci collating number
    %% Consequative keys on the keyboard are consequative numbers in this 
    %% collating sequence ...
    {Win, {KeyCode,State}};
eParseEvent(buttonPress,<<4:8,Button:8,
	    _Seq:16,Time:32,_:32,Event:32,_:32,
	    RootX:?INT16,RootY:?INT16,
	    EventX:?INT16,EventY:?INT16,_/binary>>) ->
    %% pg 71
    {Event, {Button, EventX, EventY,RootX,RootY}};
eParseEvent(configureNotify, <<22:8,_:8,_Seq:16,_:32,Win:32,_:32,
			      _X:16,_Y:16,Width:16,Ht:16,_/binary>>) ->
    %% pg 94
    {Win, {Width,Ht}};
eParseEvent(enterNotify, <<7:8,_:8,_Seq:16,_:64,Win:32,_/binary>>) ->
    %% pg 134
    {Win, enter};
eParseEvent(leaveNotify, <<8:8,_:8,_Seq:16,_:64,Win:32,_/binary>>) ->
    %% pg 134
    {Win, leave}; 
eParseEvent(motionNotify, <<6:8,_:88,Event:32,_:32,
			   RootX:?INT16,RootY:?INT16,
			   EventX:?INT16,EventY:?INT16,State:16,
			   _/binary>>) ->
    {Event, {State, EventX, EventY, RootX, RootY}}.


xDo(Display, L) when is_list(L) -> map(fun(I) -> cmd(Display, I) end, L);
xDo(Display, L)              -> cmd(Display, L).

%% Make a temporary GC in the correct graphics context

xMkTmpGC(Display, Opts) ->		      
    {ok, Id} = xGetVar(Display, dummyGC),
    xDo(Display, eChangeGC(Id, Opts)),
    Id.

%% temporary pen

xMkTmpPen(Display, Width, Color) ->
    xMkTmpGC(Display,[{function,copy},
		      {line_width,Width},
		      {line_style,solid},
		      {foreground, xColor(Display, Color)}]).

xVar(Display, Key) ->
    case xGetVar(Display, Key) of
	{ok, Val} ->
	    Val;
	error ->
	    io:format("*** xVar No variable:~p~n",[Key]),
	    exit(1)
    end.

sleep(T) ->
    receive
    after T ->
	    true
    end.

get_root_of_screen(Pid, Screen) -> rpc(Pid, {get_root_of_screen, Screen}).

    
