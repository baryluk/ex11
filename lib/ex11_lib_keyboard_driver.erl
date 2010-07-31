-module(ex11_lib_keyboard_driver).

%% Started 2004-01-20 by joe@sics.se (Joe Armstrong)

%% This is done as a global process so that several windows
%% can share the same keyboard driver.

%% ex11_lib_keyboard_driver:ensure_started(Display) is called
%%   whenever a new X11 session is started

-export([ensure_started/1, analyse/1]).
-import(ex11_lib, [xDo/2, eGetKeyboardMapping/2]).
-import(lists, [map/2]).

analyse(X) ->
    ex11_lib_keyboard_driver ! {self(), X},
    receive
	{ex11_lib_keyboard_driver, Val} ->
	    Val
    end.

ensure_started(Display) ->
    (catch ensure_started1(Display)).

ensure_started1(Display) ->
    case whereis(ex11_lib_keyboard_driver) of
	undefined ->
	    register(ex11_lib_keyboard_driver, 
		     spawn(fun() -> init(Display) end));
	Pid ->
	    true
    end.

init(Display) ->
    %% Ask the display for the Min and Max keycodes
    {First,Last} = K = ex11_lib:get_display(Display, keycodes),
    io:format("Min,max keycodes=~p,~p~n",[First,Last]),
    %% gte the keycodes
    {ok, {keys, Keys}} = xDo(Display, eGetKeyboardMapping(First,Last)),
    Table = map(fun({I,Ks}) ->
			Ks1 = map(fun key/1, Ks),
			list_to_tuple(Ks1)
	    end, Keys),
    loop(list_to_tuple(Table), First, Last).

loop(Table, First, Last) ->
    receive
	{From, {Key, State}} when Key >= First, Key =< Last ->
	    Index = Key - First + 1,
	    S = element(Index, Table),
	    {Type, Element} = classify(State),
	    Val = if
		      Element > size(S) ->
			  internalError;
		      true ->
			  element(Element, S)
		  end,
	    From ! {ex11_lib_keyboard_driver, {State,Key,Type,Val}},
	    loop(Table, First, Last);
	{From, {Key,State}} ->
	    From ! {ex11_lib_keyboard_driver, {State,Key,error,error}},
	    loop(Table, First, Last);
	Other ->
	    io:format("ex11_lib_keyboard_driver internal error:~p~n",[Other]),
	    loop(Table, First, Last)
    end.


prt(0) -> "0";
prt(I) when I < 256 ->
    [I];
prt(I) ->
    ex11_lib_utils:int2hex(I).



%% Bit 1 = least significant bit
%% bit 1 set = shifted
%% bit 2 caps lock
%% bit 3 Ctrl
%% bit 4 = Alt  80
%% 32 or 128 = Alt graphics

%% is_alt_gr(32) -> true;
%% is_alt_gr(128)  -> true;
%% is_alt_gr(_) -> false.
    
is_shift(X) -> (X band 1) == 1.

is_ctrl(X) -> (X band 4) == 4.

is_alt(X) -> (X band 8) == 8.

%% There are three  modifiers
%%  Ctrl, Alt, Shift
%%  CtrlAlt CtrlShift AltShift
%%  CtrlAltShift

classify(State) ->
    classify(is_ctrl(State), is_alt(State), is_shift(State)).

classify(true, false, false)  -> {ctrl, 1};
classify(true, true, false)   -> {ctrlAlt, 1};
classify(true, true, true)    -> {ctrlAltShift, 2};
classify(true, false, true)   -> {ctrlShift, 2};
classify(false, true, false)  -> {alt, 1};
classify(false, true, true)   -> {altShift, 2};
classify(false, false, true)  -> {shift, 2};
classify(false, false, false) -> {none, 1}.

key(I) when I =< 255 -> {char, I};

key(16#aa1) -> {cmd, k_emspace};
key(16#aa2) -> {cmd, k_enspace};
key(16#aa3) -> {cmd, k_em3space};
key(16#aa4) -> {cmd, k_em4space};
key(16#aa5) -> {cmd, k_digitspace};
key(16#aa6) -> {cmd, k_punctspace};
key(16#aa7) -> {cmd, k_thinspace};
key(16#aa8) -> {cmd, k_hairspace};
key(16#aa9) -> {cmd, k_emdash};
key(16#aaa) -> {cmd, k_endash};
key(16#aac) -> {cmd, k_signifblank};
key(16#aae) -> {cmd, k_ellipsis};
key(16#aaf) -> {cmd, k_doubbaselinedot};
key(16#ab0) -> {cmd, k_onethird};
key(16#ab1) -> {cmd, k_twothirds};
key(16#ab2) -> {cmd, k_onefifth};
key(16#ab3) -> {cmd, k_twofifths};
key(16#ab4) -> {cmd, k_threefifths};
key(16#ab5) -> {cmd, k_fourfifths};
key(16#ab6) -> {cmd, k_onesixth};
key(16#ab7) -> {cmd, k_fivesixths};
key(16#ab8) -> {cmd, k_careof};
key(16#abb) -> {cmd, k_figdash};
key(16#abc) -> {cmd, k_leftanglebracket};
key(16#abd) -> {cmd, k_decimalpoint};
key(16#abe) -> {cmd, k_rightanglebracket};
key(16#abf) -> {cmd, k_marker};
key(16#ac3) -> {cmd, k_oneeighth};
key(16#ac4) -> {cmd, k_threeeighths};
key(16#ac5) -> {cmd, k_fiveeighths};
key(16#ac6) -> {cmd, k_seveneighths};
key(16#ac9) -> {cmd, k_trademark};
key(16#aca) -> {cmd, k_signaturemark};
key(16#acb) -> {cmd, k_trademarkincircle};
key(16#acc) -> {cmd, k_leftopentriangle};
key(16#acd) -> {cmd, k_rightopentriangle};
key(16#ace) -> {cmd, k_emopencircle};
key(16#acf) -> {cmd, k_emopenrectangle};
key(16#ad0) -> {cmd, k_leftsinglequotemark};
key(16#ad1) -> {cmd, k_rightsinglequotemark};
key(16#ad2) -> {cmd, k_leftdoublequotemark};
key(16#ad3) -> {cmd, k_rightdoublequotemark};
key(16#ad4) -> {cmd, k_prescription};
key(16#ad6) -> {cmd, k_minutes};
key(16#ad7) -> {cmd, k_seconds};
key(16#ad9) -> {cmd, k_latincross};
key(16#ada) -> {cmd, k_hexagram};
key(16#adb) -> {cmd, k_filledrectbullet};
key(16#adc) -> {cmd, k_filledlefttribullet};
key(16#add) -> {cmd, k_filledrighttribullet};
key(16#ade) -> {cmd, k_emfilledcircle};
key(16#adf) -> {cmd, k_emfilledrect};
key(16#ae0) -> {cmd, k_enopencircbullet};
key(16#ae1) -> {cmd, k_enopensquarebullet};
key(16#ae2) -> {cmd, k_openrectbullet};
key(16#ae3) -> {cmd, k_opentribulletup};
key(16#ae4) -> {cmd, k_opentribulletdown};
key(16#ae5) -> {cmd, k_openstar};
key(16#ae6) -> {cmd, k_enfilledcircbullet};
key(16#ae7) -> {cmd, k_enfilledsqbullet};
key(16#ae8) -> {cmd, k_filledtribulletup};
key(16#ae9) -> {cmd, k_filledtribulletdown};
key(16#aea) -> {cmd, k_leftpointer};
key(16#aeb) -> {cmd, k_rightpointer};
key(16#aec) -> {cmd, k_club};
key(16#aed) -> {cmd, k_diamond};
key(16#aee) -> {cmd, k_heart};
key(16#af0) -> {cmd, k_maltesecross};
key(16#af1) -> {cmd, k_dagger};
key(16#af2) -> {cmd, k_doubledagger};
key(16#af3) -> {cmd, k_checkmark};
key(16#af4) -> {cmd, k_ballotcross};
key(16#af5) -> {cmd, k_musicalsharp};
key(16#af6) -> {cmd, k_musicalflat};
key(16#af7) -> {cmd, k_malesymbol};
key(16#af8) -> {cmd, k_femalesymbol};
key(16#af9) -> {cmd, k_telephone};
key(16#afa) -> {cmd, k_telephonerecorder};
key(16#afb) -> {cmd, k_phonographcopyright};
key(16#afc) -> {cmd, k_caret};
key(16#afd) -> {cmd, k_singlelowquotemark};
key(16#afe) -> {cmd, k_doublelowquotemark};
key(16#aff) -> {cmd, k_cursor};

key(16#ff08) -> {char, 16#08}; % backspace
key(16#ff09) -> {char, 16#09}; % tab
key(16#ff0a) -> {char, 16#0a}; % linefeed
key(16#ff0b) -> {char, 16#0b}; % clear
key(16#ff0d) -> {char, 16#0d}; % return, enter

key(16#ff13) -> {char, 16#13}; % pause , hold
key(16#ff14) -> {char, 16#14}; % scroll lock
key(16#ff15) -> {char, 16#15}; % sys request
key(16#ff1b) -> {char, 16#1b}; % esc
key(16#ffff) -> {char, 16#ff}; % delete rubout


key(16#FE01) -> {cmd, k_ISO_Lock};
key(16#FE02) -> {cmd, k_ISO_Level2_Latch};
key(16#FE03) -> {cmd, k_ISO_Level3_Shift};
key(16#FE04) -> {cmd, k_ISO_Level3_Latch};
key(16#FE05) -> {cmd, k_ISO_Level3_Lock};
key(16#FF7E) -> {cmd, k_ISO_Group_Shift};
key(16#FE06) -> {cmd, k_ISO_Group_Latch};
key(16#FE07) -> {cmd, k_ISO_Group_Lock};
key(16#FE08) -> {cmd, k_ISO_Next_Group};
key(16#FE09) -> {cmd, k_ISO_Next_Group_Lock};
key(16#FE0A) -> {cmd, k_ISO_Prev_Group};
key(16#FE0B) -> {cmd, k_ISO_Prev_Group_Lock};
key(16#FE0C) -> {cmd, k_ISO_First_Group};
key(16#FE0D) -> {cmd, k_ISO_First_Group_Lock};
key(16#FE0E) -> {cmd, k_ISO_Last_Group};
key(16#FE0F) -> {cmd, k_ISO_Last_Group_Lock};
key(16#FE20) -> {cmd, k_ISO_Left_Tab };
key(16#FE21) -> {cmd, k_ISO_Move_Line_Up};
key(16#FE22) -> {cmd, k_ISO_Move_Line_Down};
key(16#FE23) -> {cmd, k_ISO_Partial_Line_Up};
key(16#FE24) -> {cmd, k_ISO_Partial_Line_Down};
key(16#FE25) -> {cmd, k_ISO_Partial_Space_Left};
key(16#FE26) -> {cmd, k_ISO_Partial_Space_Right};
key(16#FE27) -> {cmd, k_ISO_Set_Margin_Left};
key(16#FE28) -> {cmd, k_ISO_Set_Margin_Right};
key(16#FE29) -> {cmd, k_ISO_Release_Margin_Left};
key(16#FE2A) -> {cmd, k_ISO_Release_Margin_Right};
key(16#FE2B) -> {cmd, k_ISO_Release_Both_Margins};
key(16#FE2C) -> {cmd, k_ISO_Fast_Cursor_Left};
key(16#FE2D) -> {cmd, k_ISO_Fast_Cursor_Right};
key(16#FE2E) -> {cmd, k_ISO_Fast_Cursor_Up};
key(16#FE2F) -> {cmd, k_ISO_Fast_Cursor_Down};
key(16#FE30) -> {cmd, k_ISO_Continuous_Underline};
key(16#FE31) -> {cmd, k_ISO_Discontinuous_Underline};
key(16#FE32) -> {cmd, k_ISO_Emphasize};
key(16#FE33) -> {cmd, k_ISO_Center_Object};
key(16#FE34) -> {cmd, k_ISO_Enter };
key(16#FE50) -> {cmd, k_dead_grave };
key(16#FE51) -> {cmd, k_dead_acute };
key(16#FE52) -> {cmd, k_dead_circumflex};
key(16#FE53) -> {cmd, k_dead_tilde };
key(16#FE54) -> {cmd, k_dead_macron };
key(16#FE55) -> {cmd, k_dead_breve };
key(16#FE56) -> {cmd, k_dead_abovedot};
key(16#FE57) -> {cmd, k_dead_diaeresis};
key(16#FE58) -> {cmd, k_dead_abovering};
key(16#FE59) -> {cmd, k_dead_doubleacute};
key(16#FE5A) -> {cmd, k_dead_caron };
key(16#FE5B) -> {cmd, k_dead_cedilla };
key(16#FE5C) -> {cmd, k_dead_ogonek };
key(16#FE5D) -> {cmd, k_dead_iota };
key(16#FE5E) -> {cmd, k_dead_voiced_sound};
key(16#FE5F) -> {cmd, k_dead_semivoiced_sound};
key(16#FE60) -> {cmd, k_dead_belowdot};
key(16#FE61) -> {cmd, k_dead_hook };
key(16#FE62) -> {cmd, k_dead_horn };
key(16#FED0) -> {cmd, k_First_Virtual_Screen};
key(16#FED1) -> {cmd, k_Prev_Virtual_Screen};
key(16#FED2) -> {cmd, k_Next_Virtual_Screen};
key(16#FED4) -> {cmd, k_Last_Virtual_Screen};
key(16#FED5) -> {cmd, k_Terminate_Server};
key(16#FE70) -> {cmd, k_AccessX_Enable};
key(16#FE71) -> {cmd, k_AccessX_Feedback_Enable};
key(16#FE72) -> {cmd, k_RepeatKeys_Enable};
key(16#FE73) -> {cmd, k_SlowKeys_Enable};
key(16#FE74) -> {cmd, k_BounceKeys_Enable};
key(16#FE75) -> {cmd, k_StickyKeys_Enable};
key(16#FE76) -> {cmd, k_MouseKeys_Enable};
key(16#FE77) -> {cmd, k_MouseKeys_Accel_Enable};
key(16#FE78) -> {cmd, k_Overlay1_Enable};
key(16#FE79) -> {cmd, k_Overlay2_Enable};
key(16#FE7A) -> {cmd, k_AudibleBell_Enable};
key(16#FEE0) -> {cmd, k_Pointer_Left };
key(16#FEE1) -> {cmd, k_Pointer_Right};
key(16#FEE2) -> {cmd, k_Pointer_Up };
key(16#FEE3) -> {cmd, k_Pointer_Down };
key(16#FEE4) -> {cmd, k_Pointer_UpLeft};
key(16#FEE5) -> {cmd, k_Pointer_UpRight};
key(16#FEE6) -> {cmd, k_Pointer_DownLeft};
key(16#FEE7) -> {cmd, k_Pointer_DownRight};
key(16#FEE8) -> {cmd, k_Pointer_Button_Dflt};
key(16#FEE9) -> {cmd, k_Pointer_Button1};
key(16#FEEA) -> {cmd, k_Pointer_Button2};
key(16#FEEB) -> {cmd, k_Pointer_Button3};
key(16#FEEC) -> {cmd, k_Pointer_Button4};
key(16#FEED) -> {cmd, k_Pointer_Button5};
key(16#FEEE) -> {cmd, k_Pointer_DblClick_Dflt};
key(16#FEEF) -> {cmd, k_Pointer_DblClick1};
key(16#FEF0) -> {cmd, k_Pointer_DblClick2};
key(16#FEF1) -> {cmd, k_Pointer_DblClick3};
key(16#FEF2) -> {cmd, k_Pointer_DblClick4};
key(16#FEF3) -> {cmd, k_Pointer_DblClick5};
key(16#FEF4) -> {cmd, k_Pointer_Drag_Dflt};
key(16#FEF5) -> {cmd, k_Pointer_Drag1};
key(16#FEF6) -> {cmd, k_Pointer_Drag2};
key(16#FEF7) -> {cmd, k_Pointer_Drag3};
key(16#FEF8) -> {cmd, k_Pointer_Drag4};
key(16#FEFD) -> {cmd, k_Pointer_Drag5};
key(16#FEF9) -> {cmd, k_Pointer_EnableKeys};
key(16#FEFA) -> {cmd, k_Pointer_Accelerate};
key(16#FEFB) -> {cmd, k_Pointer_DfltBtnNext};
key(16#FEFC) -> {cmd, k_Pointer_DfltBtnPrev};

key(16#FF50) -> {cmd, home};
key(16#FF51) -> {cmd, left};
key(16#FF52) -> {cmd, up};
key(16#FF53) -> {cmd, right};
key(16#FF54) -> {cmd, down};
key(16#FF55) -> {cmd, prior};
key(16#FF55) -> {cmd, pageUp};
key(16#FF56) -> {cmd, next};
key(16#FF56) -> {cmd, pageDown};
key(16#FF57) -> {cmd, 'end'};
key(16#FF58) -> {cmd, 'begin'};

key(16#FF60) -> {cmd, select};
key(16#FF61) -> {cmd, print};  
key(16#FF62) -> {cmd, execute};
key(16#FF63) -> {cmd, insert};
key(16#FF65) -> {cmd, undo};  
key(16#FF66) -> {cmd, redo}; 
key(16#FF67) -> {cmd,  menu};
key(16#FF68) -> {cmd,  find};
key(16#FF69) -> {cmd,  cancel};
key(16#FF6A) -> {cmd,  help};
key(16#FF6B) -> {cmd,  break};

key(16#FF7E) -> {cmd, modeSwitch};
key(16#FF7E) -> {cmd, scriptSwitch};
key(16#FF7F) -> {cmd, numLock}; 	

key(16#FF80) -> {keypad,space};
key(16#FF89) -> {keypad,tab};
key(16#FF8D) -> {keypad,enter};
key(16#FF91) -> {keypad,f1};
key(16#FF92) -> {keypad,f2};
key(16#FF93) -> {keypad,f3};
key(16#FF94) -> {keypad,f4};
key(16#FF95) -> {keypad,home};
key(16#FF96) -> {keypad,left};
key(16#FF97) -> {keypad,up};
key(16#FF98) -> {keypad,right};
key(16#FF99) -> {keypad,down};
key(16#FF9A) -> {keypad,prior};
key(16#FF9A) -> {keypad,pageUp};
key(16#FF9B) -> {keypad,next};
key(16#FF9B) -> {keypad,pageDown};
key(16#FF9C) -> {keypad,'end'};
key(16#FF9D) -> {keypad,'begin'};
key(16#FF9E) -> {keypad,insert};
key(16#FF9F) -> {keypad,delete};
key(16#FFBD) -> {keypad,equal};
key(16#FFAA) -> {keypad,multiply};
key(16#FFAB) -> {keypad,add};
key(16#FFAC) -> {keypad,separator};
key(16#FFAD) -> {keypad,subtract};
key(16#FFAE) -> {keypad,decimal};
key(16#FFAF) -> {keypad,divide};

key(16#FFB0) -> {keypad, $0};
key(16#FFB1) -> {keypad, $1};
key(16#FFB2) -> {keypad, $2};
key(16#FFB3) -> {keypad, $3};
key(16#FFB4) -> {keypad, $4};
key(16#FFB5) -> {keypad, $5};
key(16#FFB6) -> {keypad, $6};
key(16#FFB7) -> {keypad, $7};
key(16#FFB8) -> {keypad, $8};
key(16#FFB9) -> {keypad, $9};

key(16#FFBE) -> {f, 1};
key(16#FFBF) -> {f, 2};
key(16#FFC0) -> {f, 3};
key(16#FFC1) -> {f, 4};
key(16#FFC2) -> {f, 5};
key(16#FFC3) -> {f, 6};
key(16#FFC4) -> {f, 7};
key(16#FFC5) -> {f, 8};
key(16#FFC6) -> {f, 9};
key(16#FFC7) -> {f, 10};
key(16#FFC8) -> {f, 11};
key(16#FFC9) -> {f, 12};
key(16#FFCA) -> {f, 13};
key(16#FFCB) -> {f, 14};
key(16#FFCC) -> {f, 15};
key(16#FFCD) -> {f, 16};
key(16#FFCE) -> {f, 17};
key(16#FFCF) -> {f, 18};
key(16#FFD0) -> {f, 19};
key(16#FFD1) -> {f, 20};
key(16#FFD2) -> {f, 21};
key(16#FFD3) -> {f, 22};
key(16#FFD4) -> {f, 23};
key(16#FFD5) -> {f, 24};
key(16#FFD6) -> {f, 25};
key(16#FFD7) -> {f, 26};
key(16#FFD8) -> {f, 27};
key(16#FFD9) -> {f, 28};
key(16#FFDA) -> {f, 29};
key(16#FFDB) -> {f, 30};
key(16#FFDC) -> {f, 31};
key(16#FFDD) -> {f, 32};
key(16#FFDE) -> {f, 33};
key(16#FFDF) -> {f, 34};
key(16#FFE0) -> {f, 35};

key(16#FFE1) -> {cmd, shift_L};
key(16#FFE2) -> {cmd, shift_R};
key(16#FFE3) -> {cmd, control_L};
key(16#FFE4) -> {cmd, control_R};
key(16#FFE5) -> {cmd, caps_Lock};
key(16#FFE6) -> {cmd, shift_Lock};
key(16#FFE7) -> {cmd, meta_L};
key(16#FFE8) -> {cmd, meta_R};
key(16#FFE9) -> {cmd, alt_L};
key(16#FFEA) -> {cmd, alt_R};
key(16#FFEB) -> {cmd, super_L};
key(16#FFEC) -> {cmd, super_R};
key(16#FFED) -> {cmd, hyper_L};
key(16#FFEE) -> {cmd, hyper_R};

key(X) ->
    io:format("Unknown Key=~s~n",[prt(X)]),
    {unknown, X}.

		

				   
	    
