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

%% pointer information

-record(pointerInfo,
        {same_screen,
         root_win,
         child_win,
         root_x,
         root_y,
         win_x,
         win_y,
         mask}).


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

-include("ex11_lib_colors.hrl").
