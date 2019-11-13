-ifndef(_SW_HRL).
-define(_SW_HRL, true).


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

-include("ex11_lib_colors.hrl").


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
%% IN NOEVEN T SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR
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

-record(win, {
	  x=10,
	  y=10,
	  width=200,
	  ht=25,
	  border=1,
	  color= ?gold,
	  cursor=?XC_arrow,
	  mask=0,
	  map=true,
	  win=0,
	  type=unset,
	  parent=undefined,
	  screen=undefined
	 }).

-record(s,
	{
	  handlers,
	  default,    % Fun(X, Display,State) -> State
	  wargs,
	  data,
	  win
	 }).

-endif.
