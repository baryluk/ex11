%% top-level objects

%%---------------------------------------------------
%% Client byte order.
%% (This Erlang implementation of X11 is always running
%%  as a MSB client. The bit manipulating primitives in
%%  Erlang will isolate us from the underlying machine.)

-define(MSB_BYTEORDER, $B).
-define(LSB_BYTEORDER, $l).

-record(depth,
        {depth,           % this depth (Z) of the depth 
	 nvisuals,        % number of Visual types at this depth 
	 visuals          % list of visuals possible at this depth
	}).

-define(PRINT_DEPTH(D),
	begin
	io:format("DEPTH:~n"
		  "  depth    = ~p~n  nvisuals = ~p~n",
		  [D#depth.depth, D#depth.nvisuals]),
	    lists:foreach(
	      fun(V) -> ?PRINT_VISUAL(V) end,
	      D#depth.visuals)
	end).

-record(display,
        {fd,                  % Network socket. 
	 resource_id = 0,
	 resource_max = 0,
	 resource_mask = 0,
	 resource_shift = 0,
	 resource_base = 0,
	 proto_major_version, % major version of server's X protocol
	 proto_minor_version, % minor version of servers X protocol 
	 vendor,              % vendor of the server hardware 
	 byte_order,          % screen byte order, LSBFirst, MSBFirst 
	 bitmap_unit,         % padding and data requirements 
	 bitmap_pad,          % padding requirements on bitmaps 
	 bitmap_bit_order,    % LeastSignificant or MostSignificant
	 nformats,            % number of pixmap formats in list 
	 pixmap_formats,      % pixmap format list
	 release,             % release of the server
	 last_request_read,   % seq number of last event read 
	 request,             % sequence number of last request.
	 max_request_size,    % maximum number 32 bit words in request
	 display_name,        % "host:display" string used on this connect
	 default_screen,      % default screen for operations 
	 nscreens,            % number of screens on this server
         screens,             % list of screens 
	 motion_buffer,       % size of motion buffer
	 min_keycode,         % minimum defined keycode
	 max_keycode          % maximum defined keycode 
	}).

-define(IS_DISPLAY(D), is_record(D,display)).

%% Access macros
-define(ROOT_DEPTH(D, S),
	((lists:nth(S + 1, D#display.screens))#screen.root_depth) ).
-define(ROOT_ID(D, S),
	((lists:nth(S + 1, D#display.screens))#screen.root) ).
-define(WHITE_PIXEL(D, S),
	((lists:nth(S + 1, D#display.screens))#screen.white_pixel) ).
-define(BLACK_PIXEL(D, S),
	((lists:nth(S + 1, D#display.screens))#screen.black_pixel) ).
-define(COLOR_MAP(D, S),
	((lists:nth(S + 1, D#display.screens))#screen.cmap) ).
-define(DEFAULT_VISUAL(D, S),
	((lists:nth(S + 1, D#display.screens))#screen.root_visual) ).

-define(PRINT_DISPLAY(D), 
	begin
	io:format("DISPLAY:~n"
		  "  fd                  = ~p~n  resource_id         = ~p~n"
		  "  resource_max        = ~p~n  resource_mask       = ~p~n"
		  "  resource_shift      = ~p~n  resource_base       = ~p~n"
		  "  proto_major_version = ~p~n  proto_minor_version = ~p~n"
		  "  vendor              = ~p~n  byte_order          = ~p~n"
		  "  bitmap_unit         = ~p~n  bitmap_pad          = ~p~n"
		  "  bitmap_bit_order    = ~p~n  nformats            = ~p~n"
		  "  pixmap_formats      = ~p~n  release             = ~p~n"
		  "  last_request_read   = ~p~n  request             = ~p~n"
		  "  max_request_size    = ~p~n  display_name        = ~p~n"
		  "  default_screen      = ~p~n  nscreens            = ~p~n"
		  "  motion_buffer       = ~p~n  min_keycode         = ~p~n"
		  "  max_keycode         = ~p~n",
		  [D#display.fd, D#display.resource_id,
		   D#display.resource_max, D#display.resource_mask,
		   D#display.resource_shift, D#display.resource_base,
		   D#display.proto_major_version,D#display.proto_minor_version,
		   D#display.vendor, D#display.byte_order,
		   D#display.bitmap_unit, D#display.bitmap_pad,
		   D#display.bitmap_bit_order, D#display.nformats,
		   D#display.pixmap_formats, D#display.release,
		   D#display.last_request_read, D#display.request,
		   D#display.max_request_size, D#display.display_name,
		   D#display.default_screen, D#display.nscreens,
		   D#display.motion_buffer, D#display.min_keycode,
		   D#display.max_keycode]),
	    lists:foreach(
	      fun(S) -> ?PRINT_SCREEN(S) end,
	      D#display.screens)
	end).

-record(format,
	{depth,
	 bpp,
	 scanline_pad}).

-record(screen,
	{root,            % Root window id. 
	 width, height,   % width and height of screen 
	 mwidth, mheight, % width and height of  in millimeters 
	 ndepths,         % number of depths possible 
	 white_pixel,     % White and Black pixel values 
	 black_pixel,      
	 root_depth,      % bits per pixel 
	 cmap,            % default color map 
         backing_store,   % Never, WhenMapped, Always 
         save_unders,
	 max_maps,        % max and min color maps 
	 min_maps, 
         root_input_mask, % initial root input mask
	 depths,          % list of allowable depths on the screen 
	 root_visual,     % root visual 
         default_gc       % GC for the root root visual 
	}).

-define(PRINT_SCREEN(S),
	begin
	io:format("SCREEN:~n"
		  "  root            = ~p~n  width           = ~p~n"
		  "  height          = ~p~n  mwidth          = ~p~n"
		  "  mheight         = ~p~n  ndepths         = ~p~n"
		  "  white_pixel     = ~p~n  black_pixel     = ~p~n"
		  "  root_depth      = ~p~n  cmap            = ~p~n"
		  "  backing_store   = ~p~n  save_unders     = ~p~n"
		  "  max_maps        = ~p~n  min_maps        = ~p~n"
		  "  root_input_mask = ~p~n  root_visual     = ~p~n"
		  "  default_gc      = ~p~n  Ndepths         = ~p~n", 
		  [S#screen.root, S#screen.width, 
		   S#screen.height, S#screen.mwidth, 
		   S#screen.mheight, S#screen.ndepths,        
		   S#screen.white_pixel, S#screen.black_pixel,      
		   S#screen.root_depth, S#screen.cmap,           
		   S#screen.backing_store, S#screen.save_unders,
		   S#screen.max_maps, S#screen.min_maps, 
		   S#screen.root_input_mask, S#screen.root_visual,  
		   S#screen.default_gc, length(S#screen.depths)]),
	    lists:foreach(
	      fun(D) -> ?PRINT_DEPTH(D) end,
	      S#screen.depths)
	end).

-record(visual,
	{visualid,        % visual id of this visual 
	 class,           % class of screen (monochrome, etc.) 
	 red_mask,        % mask values
	 green_mask, 
	 blue_mask,  
         bits_per_rgb,    % log base 2 of distinct color values 
         map_entries      % color map entries 
	}).

-define(PRINT_VISUAL(V),
	io:format("VISUAL:~n"
		  "  visualid = ~p~n  class = ~p~n"
		  "  red_mask = ~p~n  green_mask = ~p~n"
		  "  blue_mask = ~p~n  bits_per_rgb = ~p~n"
		  "  map_entries = ~p~n",
		  [V#visual.visualid, V#visual.class,
		   V#visual.red_mask, V#visual.green_mask,
		   V#visual.blue_mask, V#visual.bits_per_rgb,
		   V#visual.map_entries])).



%% --------------
%% Set of Events

-define(EVENT_KEY_PRESS,             16#00000001).
-define(EVENT_KEY_RELEASE,           16#00000002).
-define(EVENT_BUTTON_PRESS,          16#00000004).
-define(EVENT_BUTTON_RELEASE,        16#00000008).
-define(EVENT_ENTER_WINDOW,          16#00000010).
-define(EVENT_LEAVE_WINDOW,          16#00000020).
-define(EVENT_POINTER_MOTION,        16#00000040).
-define(EVENT_POINTER_MOTION_HINT,   16#00000080).
-define(EVENT_BUTTON1_MOTION,        16#00000100).
-define(EVENT_BUTTON2_MOTION,        16#00000200).
-define(EVENT_BUTTON3_MOTION,        16#00000400).
-define(EVENT_BUTTON4_MOTION,        16#00000800).
-define(EVENT_BUTTON5_MOTION,        16#00001000).
-define(EVENT_BUTTON_MOTION,         16#00002000).
-define(EVENT_KEYMAP_STATE,          16#00004000).
-define(EVENT_EXPOSURE,              16#00008000).
-define(EVENT_VISIBILITY_CHANGE,     16#00010000).
-define(EVENT_STRUCTURE_NOTIFY,      16#00020000).
-define(EVENT_RESIZE_REDIRECT,       16#00040000).
-define(EVENT_SUBSTRUCTURE_NOTIFY,   16#00080000).
-define(EVENT_SUBSTRUCTURE_REDIRECT, 16#00100000).
-define(EVENT_FOCUS_CHANGE,          16#00200000).
-define(EVENT_PROPERTY_CHANGE,       16#00400000).
-define(EVENT_COLORMAP_CHANGE,       16#00800000).
-define(EVENT_OWNER_GRAB_BUTTON,     16#01000000).

%% font information

-record(fontInfo,
	{min_bounds,       % Min bounds (#charInfo)
	 max_bounds,       % Max bounds (#charInfo)
	 min_byte2,        % min char or byte2
	 max_byte2,        % max char or byte2
	 default_char,     %
	 draw_direction,   %
	 min_byte1,        % min byte1
	 max_byte1,        % max byte1
	 all_chars_exist,  % bool
	 font_ascent,      % int
	 font_descent,     % int
	 font_props,       % {Atom,Property}
	 char_infos        % [#charInfo]
	}).

-record(charInfo,
	{left_side_bearing,
	 right_side_bearing,
	 width,
	 ascent,
	 descent,
	 attributes
	}).

%% $Xorg: cursorfont.h,v 1.4 2001/02/09 02:03:39 xorgcvs Exp $ */
%%  
%%  opyright 1987, 1998  The Open Group
%% 
%% Permission to use, copy, modify, distribute, and sell this software and its
%% documentation for any purpose is hereby granted without fee, provided that
%% the above copyright notice appear in all copies and that both that
%% copyright notice and this permission notice appear in supporting
%% documentation.
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%% IN NO EVENT SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR
%% OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
%% ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% 
%% Except as contained in this notice, the name of The Open Group shall
%% not be used in advertising or otherwise to promote the sale, use or
%% other dealings in this Software without prior written authorization
%% from The Open Group.


-define(XC_num_glyphs,154).
-define(XC_X_cursor,0).
-define(XC_arrow,2).
-define(XC_based_arrow_down,4).
-define(XC_based_arrow_up,6).
-define(XC_boat,8).
-define(XC_bogosity,10).
-define(XC_bottom_left_corner,12).
-define(XC_bottom_right_corner,14).
-define(XC_bottom_side,16).
-define(XC_bottom_tee,18).
-define(XC_box_spiral,20).
-define(XC_center_ptr,22).
-define(XC_circle,24).
-define(XC_clock,26).
-define(XC_coffee_mug,28).
-define(XC_cross,30).
-define(XC_cross_reverse,32).
-define(XC_crosshair,34).
-define(XC_diamond_cross,36).
-define(XC_dot,38).
-define(XC_dotbox,40).
-define(XC_double_arrow,42).
-define(XC_draft_large,44).
-define(XC_draft_small,46).
-define(XC_draped_box,48).
-define(XC_exchange,50).
-define(XC_fleur,52).
-define(XC_gobbler,54).
-define(XC_gumby,56).
-define(XC_hand1,58).
-define(XC_hand2,60).
-define(XC_heart,62).
-define(XC_icon,64).
-define(XC_iron_cross,66).
-define(XC_left_ptr,68).
-define(XC_left_side,70).
-define(XC_left_tee,72).
-define(XC_leftbutton,74).
-define(XC_ll_angle,76).
-define(XC_lr_angle,78).
-define(XC_man,80).
-define(XC_middlebutton,82).
-define(XC_mouse,84).
-define(XC_pencil,86).
-define(XC_pirate,88).
-define(XC_plus,90).
-define(XC_question_arrow,92).
-define(XC_right_ptr,94).
-define(XC_right_side,96).
-define(XC_right_tee,98).
-define(XC_rightbutton,100).
-define(XC_rtl_logo,102).
-define(XC_sailboat,104).
-define(XC_sb_down_arrow,106).
-define(XC_sb_h_double_arrow,108).
-define(XC_sb_left_arrow,110).
-define(XC_sb_right_arrow,112).
-define(XC_sb_up_arrow,114).
-define(XC_sb_v_double_arrow,116).
-define(XC_shuttle,118).
-define(XC_sizing,120).
-define(XC_spider,122).
-define(XC_spraycan,124).
-define(XC_star,126).
-define(XC_target,128).
-define(XC_tcross,130).
-define(XC_top_left_arrow,132).
-define(XC_top_left_corner,134).
-define(XC_top_right_corner,136).
-define(XC_top_side,138).
-define(XC_top_tee,140).
-define(XC_trek,142).
-define(XC_ul_angle,144).
-define(XC_umbrella,146).
-define(XC_ur_angle,148).
-define(XC_watch,150).
-define(XC_xterm,152).

%% derived from /usr/X11R6/lib/X11/rgb.txt
-define(snow,16#FFFAFA).
-define(GhostWhite,16#F8F8FF).
-define(WhiteSmoke,16#F5F5F5).
-define(gainsboro,16#DCDCDC).
-define(FloralWhite,16#FFFAF0).
-define(OldLace,16#FDF5E6).
-define(linen,16#FAF0E6).
-define(AntiqueWhite,16#FAEBD7).
-define(PapayaWhip,16#FFEFD5).
-define(BlanchedAlmond,16#FFEBCD).
-define(bisque,16#FFE4C4).
-define(PeachPuff,16#FFDAB9).
-define(NavajoWhite,16#FFDEAD).
-define(moccasin,16#FFE4B5).
-define(cornsilk,16#FFF8DC).
-define(ivory,16#FFFFF0).
-define(LemonChiffon,16#FFFACD).
-define(seashell,16#FFF5EE).
-define(honeydew,16#F0FFF0).
-define(MintCream,16#F5FFFA).
-define(azure,16#F0FFFF).
-define(AliceBlue,16#F0F8FF).
-define(lavender,16#E6E6FA).
-define(LavenderBlush,16#FFF0F5).
-define(MistyRose,16#FFE4E1).
-define(white,16#FFFFFF).
-define(black,16#0).
-define(DarkSlateGray,16#2F4F4F).
-define(DarkSlateGrey,16#2F4F4F).
-define(DimGray,16#696969).
-define(DimGrey,16#696969).
-define(SlateGray,16#708090).
-define(SlateGrey,16#708090).
-define(LightSlateGray,16#778899).
-define(LightSlateGrey,16#778899).
-define(gray,16#BEBEBE).
-define(grey,16#BEBEBE).
-define(LightGrey,16#D3D3D3).
-define(LightGray,16#D3D3D3).
-define(MidnightBlue,16#191970).
-define(navy,16#80).
-define(NavyBlue,16#80).
-define(CornflowerBlue,16#6495ED).
-define(DarkSlateBlue,16#483D8B).
-define(SlateBlue,16#6A5ACD).
-define(MediumSlateBlue,16#7B68EE).
-define(LightSlateBlue,16#8470FF).
-define(MediumBlue,16#CD).
-define(RoyalBlue,16#4169E1).
-define(blue,16#FF).
-define(DodgerBlue,16#1E90FF).
-define(DeepSkyBlue,16#BFFF).
-define(SkyBlue,16#87CEEB).
-define(LightSkyBlue,16#87CEFA).
-define(SteelBlue,16#4682B4).
-define(LightSteelBlue,16#B0C4DE).
-define(LightBlue,16#ADD8E6).
-define(PowderBlue,16#B0E0E6).
-define(PaleTurquoise,16#AFEEEE).
-define(DarkTurquoise,16#CED1).
-define(MediumTurquoise,16#48D1CC).
-define(turquoise,16#40E0D0).
-define(cyan,16#FFFF).
-define(LightCyan,16#E0FFFF).
-define(CadetBlue,16#5F9EA0).
-define(MediumAquamarine,16#66CDAA).
-define(aquamarine,16#7FFFD4).
-define(DarkGreen,16#6400).
-define(DarkOliveGreen,16#556B2F).
-define(DarkSeaGreen,16#8FBC8F).
-define(SeaGreen,16#2E8B57).
-define(MediumSeaGreen,16#3CB371).
-define(LightSeaGreen,16#20B2AA).
-define(PaleGreen,16#98FB98).
-define(SpringGreen,16#FF7F).
-define(LawnGreen,16#7CFC00).
-define(green,16#FF00).
-define(chartreuse,16#7FFF00).
-define(MediumSpringGreen,16#FA9A).
-define(GreenYellow,16#ADFF2F).
-define(LimeGreen,16#32CD32).
-define(YellowGreen,16#9ACD32).
-define(ForestGreen,16#228B22).
-define(OliveDrab,16#6B8E23).
-define(DarkKhaki,16#BDB76B).
-define(khaki,16#F0E68C).
-define(PaleGoldenrod,16#EEE8AA).
-define(LightGoldenrodYellow,16#FAFAD2).
-define(LightYellow,16#FFFFE0).
-define(yellow,16#FFFF00).
-define(gold,16#FFD700).
-define(LightGoldenrod,16#EEDD82).
-define(goldenrod,16#DAA520).
-define(DarkGoldenrod,16#B8860B).
-define(RosyBrown,16#BC8F8F).
-define(IndianRed,16#CD5C5C).
-define(SaddleBrown,16#8B4513).
-define(sienna,16#A0522D).
-define(peru,16#CD853F).
-define(burlywood,16#DEB887).
-define(beige,16#F5F5DC).
-define(wheat,16#F5DEB3).
-define(SandyBrown,16#F4A460).
-define(tan,16#D2B48C).
-define(chocolate,16#D2691E).
-define(firebrick,16#B22222).
-define(brown,16#A52A2A).
-define(DarkSalmon,16#E9967A).
-define(salmon,16#FA8072).
-define(LightSalmon,16#FFA07A).
-define(orange,16#FFA500).
-define(DarkOrange,16#FF8C00).
-define(coral,16#FF7F50).
-define(LightCoral,16#F08080).
-define(tomato,16#FF6347).
-define(OrangeRed,16#FF4500).
-define(red,16#FF0000).
-define(HotPink,16#FF69B4).
-define(DeepPink,16#FF1493).
-define(pink,16#FFC0CB).
-define(LightPink,16#FFB6C1).
-define(PaleVioletRed,16#DB7093).
-define(maroon,16#B03060).
-define(MediumVioletRed,16#C71585).
-define(VioletRed,16#D02090).
-define(magenta,16#FF00FF).
-define(violet,16#EE82EE).
-define(plum,16#DDA0DD).
-define(orchid,16#DA70D6).
-define(MediumOrchid,16#BA55D3).
-define(DarkOrchid,16#9932CC).
-define(DarkViolet,16#9400D3).
-define(BlueViolet,16#8A2BE2).
-define(purple,16#A020F0).
-define(MediumPurple,16#9370DB).
-define(thistle,16#D8BFD8).
-define(snow1,16#FFFAFA).
-define(snow2,16#EEE9E9).
-define(snow3,16#CDC9C9).
-define(snow4,16#8B8989).
-define(seashell1,16#FFF5EE).
-define(seashell2,16#EEE5DE).
-define(seashell3,16#CDC5BF).
-define(seashell4,16#8B8682).
-define(AntiqueWhite1,16#FFEFDB).
-define(AntiqueWhite2,16#EEDFCC).
-define(AntiqueWhite3,16#CDC0B0).
-define(AntiqueWhite4,16#8B8378).
-define(bisque1,16#FFE4C4).
-define(bisque2,16#EED5B7).
-define(bisque3,16#CDB79E).
-define(bisque4,16#8B7D6B).
-define(PeachPuff1,16#FFDAB9).
-define(PeachPuff2,16#EECBAD).
-define(PeachPuff3,16#CDAF95).
-define(PeachPuff4,16#8B7765).
-define(NavajoWhite1,16#FFDEAD).
-define(NavajoWhite2,16#EECFA1).
-define(NavajoWhite3,16#CDB38B).
-define(NavajoWhite4,16#8B795E).
-define(LemonChiffon1,16#FFFACD).
-define(LemonChiffon2,16#EEE9BF).
-define(LemonChiffon3,16#CDC9A5).
-define(LemonChiffon4,16#8B8970).
-define(cornsilk1,16#FFF8DC).
-define(cornsilk2,16#EEE8CD).
-define(cornsilk3,16#CDC8B1).
-define(cornsilk4,16#8B8878).
-define(ivory1,16#FFFFF0).
-define(ivory2,16#EEEEE0).
-define(ivory3,16#CDCDC1).
-define(ivory4,16#8B8B83).
-define(honeydew1,16#F0FFF0).
-define(honeydew2,16#E0EEE0).
-define(honeydew3,16#C1CDC1).
-define(honeydew4,16#838B83).
-define(LavenderBlush1,16#FFF0F5).
-define(LavenderBlush2,16#EEE0E5).
-define(LavenderBlush3,16#CDC1C5).
-define(LavenderBlush4,16#8B8386).
-define(MistyRose1,16#FFE4E1).
-define(MistyRose2,16#EED5D2).
-define(MistyRose3,16#CDB7B5).
-define(MistyRose4,16#8B7D7B).
-define(azure1,16#F0FFFF).
-define(azure2,16#E0EEEE).
-define(azure3,16#C1CDCD).
-define(azure4,16#838B8B).
-define(SlateBlue1,16#836FFF).
-define(SlateBlue2,16#7A67EE).
-define(SlateBlue3,16#6959CD).
-define(SlateBlue4,16#473C8B).
-define(RoyalBlue1,16#4876FF).
-define(RoyalBlue2,16#436EEE).
-define(RoyalBlue3,16#3A5FCD).
-define(RoyalBlue4,16#27408B).
-define(blue1,16#FF).
-define(blue2,16#EE).
-define(blue3,16#CD).
-define(blue4,16#8B).
-define(DodgerBlue1,16#1E90FF).
-define(DodgerBlue2,16#1C86EE).
-define(DodgerBlue3,16#1874CD).
-define(DodgerBlue4,16#104E8B).
-define(SteelBlue1,16#63B8FF).
-define(SteelBlue2,16#5CACEE).
-define(SteelBlue3,16#4F94CD).
-define(SteelBlue4,16#36648B).
-define(DeepSkyBlue1,16#BFFF).
-define(DeepSkyBlue2,16#B2EE).
-define(DeepSkyBlue3,16#9ACD).
-define(DeepSkyBlue4,16#688B).
-define(SkyBlue1,16#87CEFF).
-define(SkyBlue2,16#7EC0EE).
-define(SkyBlue3,16#6CA6CD).
-define(SkyBlue4,16#4A708B).
-define(LightSkyBlue1,16#B0E2FF).
-define(LightSkyBlue2,16#A4D3EE).
-define(LightSkyBlue3,16#8DB6CD).
-define(LightSkyBlue4,16#607B8B).
-define(SlateGray1,16#C6E2FF).
-define(SlateGray2,16#B9D3EE).
-define(SlateGray3,16#9FB6CD).
-define(SlateGray4,16#6C7B8B).
-define(LightSteelBlue1,16#CAE1FF).
-define(LightSteelBlue2,16#BCD2EE).
-define(LightSteelBlue3,16#A2B5CD).
-define(LightSteelBlue4,16#6E7B8B).
-define(LightBlue1,16#BFEFFF).
-define(LightBlue2,16#B2DFEE).
-define(LightBlue3,16#9AC0CD).
-define(LightBlue4,16#68838B).
-define(LightCyan1,16#E0FFFF).
-define(LightCyan2,16#D1EEEE).
-define(LightCyan3,16#B4CDCD).
-define(LightCyan4,16#7A8B8B).
-define(PaleTurquoise1,16#BBFFFF).
-define(PaleTurquoise2,16#AEEEEE).
-define(PaleTurquoise3,16#96CDCD).
-define(PaleTurquoise4,16#668B8B).
-define(CadetBlue1,16#98F5FF).
-define(CadetBlue2,16#8EE5EE).
-define(CadetBlue3,16#7AC5CD).
-define(CadetBlue4,16#53868B).
-define(turquoise1,16#F5FF).
-define(turquoise2,16#E5EE).
-define(turquoise3,16#C5CD).
-define(turquoise4,16#868B).
-define(cyan1,16#FFFF).
-define(cyan2,16#EEEE).
-define(cyan3,16#CDCD).
-define(cyan4,16#8B8B).
-define(DarkSlateGray1,16#97FFFF).
-define(DarkSlateGray2,16#8DEEEE).
-define(DarkSlateGray3,16#79CDCD).
-define(DarkSlateGray4,16#528B8B).
-define(aquamarine1,16#7FFFD4).
-define(aquamarine2,16#76EEC6).
-define(aquamarine3,16#66CDAA).
-define(aquamarine4,16#458B74).
-define(DarkSeaGreen1,16#C1FFC1).
-define(DarkSeaGreen2,16#B4EEB4).
-define(DarkSeaGreen3,16#9BCD9B).
-define(DarkSeaGreen4,16#698B69).
-define(SeaGreen1,16#54FF9F).
-define(SeaGreen2,16#4EEE94).
-define(SeaGreen3,16#43CD80).
-define(SeaGreen4,16#2E8B57).
-define(PaleGreen1,16#9AFF9A).
-define(PaleGreen2,16#90EE90).
-define(PaleGreen3,16#7CCD7C).
-define(PaleGreen4,16#548B54).
-define(SpringGreen1,16#FF7F).
-define(SpringGreen2,16#EE76).
-define(SpringGreen3,16#CD66).
-define(SpringGreen4,16#8B45).
-define(green1,16#FF00).
-define(green2,16#EE00).
-define(green3,16#CD00).
-define(green4,16#8B00).
-define(chartreuse1,16#7FFF00).
-define(chartreuse2,16#76EE00).
-define(chartreuse3,16#66CD00).
-define(chartreuse4,16#458B00).
-define(OliveDrab1,16#C0FF3E).
-define(OliveDrab2,16#B3EE3A).
-define(OliveDrab3,16#9ACD32).
-define(OliveDrab4,16#698B22).
-define(DarkOliveGreen1,16#CAFF70).
-define(DarkOliveGreen2,16#BCEE68).
-define(DarkOliveGreen3,16#A2CD5A).
-define(DarkOliveGreen4,16#6E8B3D).
-define(khaki1,16#FFF68F).
-define(khaki2,16#EEE685).
-define(khaki3,16#CDC673).
-define(khaki4,16#8B864E).
-define(LightGoldenrod1,16#FFEC8B).
-define(LightGoldenrod2,16#EEDC82).
-define(LightGoldenrod3,16#CDBE70).
-define(LightGoldenrod4,16#8B814C).
-define(LightYellow1,16#FFFFE0).
-define(LightYellow2,16#EEEED1).
-define(LightYellow3,16#CDCDB4).
-define(LightYellow4,16#8B8B7A).
-define(yellow1,16#FFFF00).
-define(yellow2,16#EEEE00).
-define(yellow3,16#CDCD00).
-define(yellow4,16#8B8B00).
-define(gold1,16#FFD700).
-define(gold2,16#EEC900).
-define(gold3,16#CDAD00).
-define(gold4,16#8B7500).
-define(goldenrod1,16#FFC125).
-define(goldenrod2,16#EEB422).
-define(goldenrod3,16#CD9B1D).
-define(goldenrod4,16#8B6914).
-define(DarkGoldenrod1,16#FFB90F).
-define(DarkGoldenrod2,16#EEAD0E).
-define(DarkGoldenrod3,16#CD950C).
-define(DarkGoldenrod4,16#8B6508).
-define(RosyBrown1,16#FFC1C1).
-define(RosyBrown2,16#EEB4B4).
-define(RosyBrown3,16#CD9B9B).
-define(RosyBrown4,16#8B6969).
-define(IndianRed1,16#FF6A6A).
-define(IndianRed2,16#EE6363).
-define(IndianRed3,16#CD5555).
-define(IndianRed4,16#8B3A3A).
-define(sienna1,16#FF8247).
-define(sienna2,16#EE7942).
-define(sienna3,16#CD6839).
-define(sienna4,16#8B4726).
-define(burlywood1,16#FFD39B).
-define(burlywood2,16#EEC591).
-define(burlywood3,16#CDAA7D).
-define(burlywood4,16#8B7355).
-define(wheat1,16#FFE7BA).
-define(wheat2,16#EED8AE).
-define(wheat3,16#CDBA96).
-define(wheat4,16#8B7E66).
-define(tan1,16#FFA54F).
-define(tan2,16#EE9A49).
-define(tan3,16#CD853F).
-define(tan4,16#8B5A2B).
-define(chocolate1,16#FF7F24).
-define(chocolate2,16#EE7621).
-define(chocolate3,16#CD661D).
-define(chocolate4,16#8B4513).
-define(firebrick1,16#FF3030).
-define(firebrick2,16#EE2C2C).
-define(firebrick3,16#CD2626).
-define(firebrick4,16#8B1A1A).
-define(brown1,16#FF4040).
-define(brown2,16#EE3B3B).
-define(brown3,16#CD3333).
-define(brown4,16#8B2323).
-define(salmon1,16#FF8C69).
-define(salmon2,16#EE8262).
-define(salmon3,16#CD7054).
-define(salmon4,16#8B4C39).
-define(LightSalmon1,16#FFA07A).
-define(LightSalmon2,16#EE9572).
-define(LightSalmon3,16#CD8162).
-define(LightSalmon4,16#8B5742).
-define(orange1,16#FFA500).
-define(orange2,16#EE9A00).
-define(orange3,16#CD8500).
-define(orange4,16#8B5A00).
-define(DarkOrange1,16#FF7F00).
-define(DarkOrange2,16#EE7600).
-define(DarkOrange3,16#CD6600).
-define(DarkOrange4,16#8B4500).
-define(coral1,16#FF7256).
-define(coral2,16#EE6A50).
-define(coral3,16#CD5B45).
-define(coral4,16#8B3E2F).
-define(tomato1,16#FF6347).
-define(tomato2,16#EE5C42).
-define(tomato3,16#CD4F39).
-define(tomato4,16#8B3626).
-define(OrangeRed1,16#FF4500).
-define(OrangeRed2,16#EE4000).
-define(OrangeRed3,16#CD3700).
-define(OrangeRed4,16#8B2500).
-define(red1,16#FF0000).
-define(red2,16#EE0000).
-define(red3,16#CD0000).
-define(red4,16#8B0000).
-define(DeepPink1,16#FF1493).
-define(DeepPink2,16#EE1289).
-define(DeepPink3,16#CD1076).
-define(DeepPink4,16#8B0A50).
-define(HotPink1,16#FF6EB4).
-define(HotPink2,16#EE6AA7).
-define(HotPink3,16#CD6090).
-define(HotPink4,16#8B3A62).
-define(pink1,16#FFB5C5).
-define(pink2,16#EEA9B8).
-define(pink3,16#CD919E).
-define(pink4,16#8B636C).
-define(LightPink1,16#FFAEB9).
-define(LightPink2,16#EEA2AD).
-define(LightPink3,16#CD8C95).
-define(LightPink4,16#8B5F65).
-define(PaleVioletRed1,16#FF82AB).
-define(PaleVioletRed2,16#EE799F).
-define(PaleVioletRed3,16#CD6889).
-define(PaleVioletRed4,16#8B475D).
-define(maroon1,16#FF34B3).
-define(maroon2,16#EE30A7).
-define(maroon3,16#CD2990).
-define(maroon4,16#8B1C62).
-define(VioletRed1,16#FF3E96).
-define(VioletRed2,16#EE3A8C).
-define(VioletRed3,16#CD3278).
-define(VioletRed4,16#8B2252).
-define(magenta1,16#FF00FF).
-define(magenta2,16#EE00EE).
-define(magenta3,16#CD00CD).
-define(magenta4,16#8B008B).
-define(orchid1,16#FF83FA).
-define(orchid2,16#EE7AE9).
-define(orchid3,16#CD69C9).
-define(orchid4,16#8B4789).
-define(plum1,16#FFBBFF).
-define(plum2,16#EEAEEE).
-define(plum3,16#CD96CD).
-define(plum4,16#8B668B).
-define(MediumOrchid1,16#E066FF).
-define(MediumOrchid2,16#D15FEE).
-define(MediumOrchid3,16#B452CD).
-define(MediumOrchid4,16#7A378B).
-define(DarkOrchid1,16#BF3EFF).
-define(DarkOrchid2,16#B23AEE).
-define(DarkOrchid3,16#9A32CD).
-define(DarkOrchid4,16#68228B).
-define(purple1,16#9B30FF).
-define(purple2,16#912CEE).
-define(purple3,16#7D26CD).
-define(purple4,16#551A8B).
-define(MediumPurple1,16#AB82FF).
-define(MediumPurple2,16#9F79EE).
-define(MediumPurple3,16#8968CD).
-define(MediumPurple4,16#5D478B).
-define(thistle1,16#FFE1FF).
-define(thistle2,16#EED2EE).
-define(thistle3,16#CDB5CD).
-define(thistle4,16#8B7B8B).
-define(gray0,16#0).
-define(grey0,16#0).
-define(gray1,16#30303).
-define(grey1,16#30303).
-define(gray2,16#50505).
-define(grey2,16#50505).
-define(gray3,16#80808).
-define(grey3,16#80808).
-define(gray4,16#A0A0A).
-define(grey4,16#A0A0A).
-define(gray5,16#D0D0D).
-define(grey5,16#D0D0D).
-define(gray6,16#F0F0F).
-define(grey6,16#F0F0F).
-define(gray7,16#121212).
-define(grey7,16#121212).
-define(gray8,16#141414).
-define(grey8,16#141414).
-define(gray9,16#171717).
-define(grey9,16#171717).
-define(gray10,16#1A1A1A).
-define(grey10,16#1A1A1A).
-define(gray11,16#1C1C1C).
-define(grey11,16#1C1C1C).
-define(gray12,16#1F1F1F).
-define(grey12,16#1F1F1F).
-define(gray13,16#212121).
-define(grey13,16#212121).
-define(gray14,16#242424).
-define(grey14,16#242424).
-define(gray15,16#262626).
-define(grey15,16#262626).
-define(gray16,16#292929).
-define(grey16,16#292929).
-define(gray17,16#2B2B2B).
-define(grey17,16#2B2B2B).
-define(gray18,16#2E2E2E).
-define(grey18,16#2E2E2E).
-define(gray19,16#303030).
-define(grey19,16#303030).
-define(gray20,16#333333).
-define(grey20,16#333333).
-define(gray21,16#363636).
-define(grey21,16#363636).
-define(gray22,16#383838).
-define(grey22,16#383838).
-define(gray23,16#3B3B3B).
-define(grey23,16#3B3B3B).
-define(gray24,16#3D3D3D).
-define(grey24,16#3D3D3D).
-define(gray25,16#404040).
-define(grey25,16#404040).
-define(gray26,16#424242).
-define(grey26,16#424242).
-define(gray27,16#454545).
-define(grey27,16#454545).
-define(gray28,16#474747).
-define(grey28,16#474747).
-define(gray29,16#4A4A4A).
-define(grey29,16#4A4A4A).
-define(gray30,16#4D4D4D).
-define(grey30,16#4D4D4D).
-define(gray31,16#4F4F4F).
-define(grey31,16#4F4F4F).
-define(gray32,16#525252).
-define(grey32,16#525252).
-define(gray33,16#545454).
-define(grey33,16#545454).
-define(gray34,16#575757).
-define(grey34,16#575757).
-define(gray35,16#595959).
-define(grey35,16#595959).
-define(gray36,16#5C5C5C).
-define(grey36,16#5C5C5C).
-define(gray37,16#5E5E5E).
-define(grey37,16#5E5E5E).
-define(gray38,16#616161).
-define(grey38,16#616161).
-define(gray39,16#636363).
-define(grey39,16#636363).
-define(gray40,16#666666).
-define(grey40,16#666666).
-define(gray41,16#696969).
-define(grey41,16#696969).
-define(gray42,16#6B6B6B).
-define(grey42,16#6B6B6B).
-define(gray43,16#6E6E6E).
-define(grey43,16#6E6E6E).
-define(gray44,16#707070).
-define(grey44,16#707070).
-define(gray45,16#737373).
-define(grey45,16#737373).
-define(gray46,16#757575).
-define(grey46,16#757575).
-define(gray47,16#787878).
-define(grey47,16#787878).
-define(gray48,16#7A7A7A).
-define(grey48,16#7A7A7A).
-define(gray49,16#7D7D7D).
-define(grey49,16#7D7D7D).
-define(gray50,16#7F7F7F).
-define(grey50,16#7F7F7F).
-define(gray51,16#828282).
-define(grey51,16#828282).
-define(gray52,16#858585).
-define(grey52,16#858585).
-define(gray53,16#878787).
-define(grey53,16#878787).
-define(gray54,16#8A8A8A).
-define(grey54,16#8A8A8A).
-define(gray55,16#8C8C8C).
-define(grey55,16#8C8C8C).
-define(gray56,16#8F8F8F).
-define(grey56,16#8F8F8F).
-define(gray57,16#919191).
-define(grey57,16#919191).
-define(gray58,16#949494).
-define(grey58,16#949494).
-define(gray59,16#969696).
-define(grey59,16#969696).
-define(gray60,16#999999).
-define(grey60,16#999999).
-define(gray61,16#9C9C9C).
-define(grey61,16#9C9C9C).
-define(gray62,16#9E9E9E).
-define(grey62,16#9E9E9E).
-define(gray63,16#A1A1A1).
-define(grey63,16#A1A1A1).
-define(gray64,16#A3A3A3).
-define(grey64,16#A3A3A3).
-define(gray65,16#A6A6A6).
-define(grey65,16#A6A6A6).
-define(gray66,16#A8A8A8).
-define(grey66,16#A8A8A8).
-define(gray67,16#ABABAB).
-define(grey67,16#ABABAB).
-define(gray68,16#ADADAD).
-define(grey68,16#ADADAD).
-define(gray69,16#B0B0B0).
-define(grey69,16#B0B0B0).
-define(gray70,16#B3B3B3).
-define(grey70,16#B3B3B3).
-define(gray71,16#B5B5B5).
-define(grey71,16#B5B5B5).
-define(gray72,16#B8B8B8).
-define(grey72,16#B8B8B8).
-define(gray73,16#BABABA).
-define(grey73,16#BABABA).
-define(gray74,16#BDBDBD).
-define(grey74,16#BDBDBD).
-define(gray75,16#BFBFBF).
-define(grey75,16#BFBFBF).
-define(gray76,16#C2C2C2).
-define(grey76,16#C2C2C2).
-define(gray77,16#C4C4C4).
-define(grey77,16#C4C4C4).
-define(gray78,16#C7C7C7).
-define(grey78,16#C7C7C7).
-define(gray79,16#C9C9C9).
-define(grey79,16#C9C9C9).
-define(gray80,16#CCCCCC).
-define(grey80,16#CCCCCC).
-define(gray81,16#CFCFCF).
-define(grey81,16#CFCFCF).
-define(gray82,16#D1D1D1).
-define(grey82,16#D1D1D1).
-define(gray83,16#D4D4D4).
-define(grey83,16#D4D4D4).
-define(gray84,16#D6D6D6).
-define(grey84,16#D6D6D6).
-define(gray85,16#D9D9D9).
-define(grey85,16#D9D9D9).
-define(gray86,16#DBDBDB).
-define(grey86,16#DBDBDB).
-define(gray87,16#DEDEDE).
-define(grey87,16#DEDEDE).
-define(gray88,16#E0E0E0).
-define(grey88,16#E0E0E0).
-define(gray89,16#E3E3E3).
-define(grey89,16#E3E3E3).
-define(gray90,16#E5E5E5).
-define(grey90,16#E5E5E5).
-define(gray91,16#E8E8E8).
-define(grey91,16#E8E8E8).
-define(gray92,16#EBEBEB).
-define(grey92,16#EBEBEB).
-define(gray93,16#EDEDED).
-define(grey93,16#EDEDED).
-define(gray94,16#F0F0F0).
-define(grey94,16#F0F0F0).
-define(gray95,16#F2F2F2).
-define(grey95,16#F2F2F2).
-define(gray96,16#F5F5F5).
-define(grey96,16#F5F5F5).
-define(gray97,16#F7F7F7).
-define(grey97,16#F7F7F7).
-define(gray98,16#FAFAFA).
-define(grey98,16#FAFAFA).
-define(gray99,16#FCFCFC).
-define(grey99,16#FCFCFC).
-define(gray100,16#FFFFFF).
-define(grey100,16#FFFFFF).
-define(DarkGrey,16#A9A9A9).
-define(DarkGray,16#A9A9A9).
-define(DarkBlue,16#8B).
-define(DarkCyan,16#8B8B).
-define(DarkMagenta,16#8B008B).
-define(DarkRed,16#8B0000).
-define(LightGreen,16#90EE90).
