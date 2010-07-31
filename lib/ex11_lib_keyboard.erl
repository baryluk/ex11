-module(ex11_lib_keyboard).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([analyse/1]).

analyse({KeyCode, State}) ->
    Key = map(KeyCode, State),
    {classify(State), Key}.

map(KeyCode, State) ->
    case map(KeyCode) of
	undefined ->
	    {unknown,KeyCode};
	N when integer(N) ->
	    %% lowercase letter 
	    case is_shift(State) of
		true ->
		    {char, N + $A - $a};
		
		false ->
		    {char, N}
	    end;
	A = {arrow, _} ->
	    A;
	{X, Y} ->
	    case is_shift(State) of
		true  -> {char, Y};
		false -> {char, X}
	    end;
	{X, Y, Z} ->
	    case is_shift(State) of
		true -> {char, Y};
		false ->
		    case is_alt_gr(State) of
			true -> {char, Z};
			false -> {char, X}
		    end
	    end;
	Atom when atom(Atom) ->
	    Atom
    end.

%% Bit 1 = least significant bit
%% bit 1 set = shifted
%% bit 2 caps lock
%% bit 3 Ctrl
%% bit 4 = Alt  80
%% 32 or 128 = Alt graphics

is_alt_gr(32) -> true;
is_alt_gr(128)  -> true;
is_alt_gr(_) -> false.
    
is_shift(X) -> (X band 1) == 1.

is_ctrl(X) -> (X band 4) == 4.

is_alt(X) -> (X band 8) == 8.

%% There are three  modifiers
%%  Ctrl, Alt, Shift
%%  CtrlAlt CtrlShift AltShift
%%  CtrlAltShift

classify(State) ->
    classify(is_ctrl(State), is_alt(State), is_shift(State)).

classify(true, false, false)  -> ctrl;
classify(true, true, false)   -> ctrlAlt;
classify(true, true, true)    -> ctrlAltShift;
classify(true, false, true)   -> ctrlShift;
classify(false, true, false)  -> alt;
classify(false, true, true)   -> altShift;
classify(false, false, true)  -> shift;
classify(false, false, false) -> none.
    

%% the character map table
%% If there is one character which is an atom then 
%%    this is the value.
%% If there is one character which is an integer then this
%%    is a lower case letter. If Shift is pressed this
%%    will be an upper case letter
%% A tuple of arity two means the values for an unshifted then a shifted
%%    character
%% A tuple of arity thress means the values for an unshifted then a shifted
%%    then an Alt Gr modified character

    
map(9)  -> esc;
map(10) -> {$1,$!};
map(11) -> {$2,$",$@};
map(12) -> {$3,$#,$£};
map(13) -> {$4,$¤,$$};
map(14) -> {$5,$%};
map(15) -> {$6,$&};
map(16) -> {$7,$/,${};
map(17) -> {$8,$(,$[};
map(18) -> {$9,$),$]};
map(19) -> {$0,$=,$}};
map(20) -> {$+,$?,$\\};
map(21) -> {$',$`};
map(22) -> backSpace;
map(23) -> tab;
map(24) -> $q;
map(25) -> $w;
map(26) -> $e;
map(27) -> $r;
map(28) -> $t;
map(29) -> $y;
map(30) -> $u;
map(31) -> $i;
map(32) -> $o;
map(33) -> $p;
map(34) -> $å;
map(35) -> {$^,$^,$~};
map(36) -> ret;
map(37) -> crtl;
map(38) -> $a;
map(39) -> $s;
map(40) -> $d;
map(41) -> $f;
map(42) -> $g;
map(43) -> $h;
map(44) -> $j;
map(45) -> $k;
map(46) -> $l;
map(47) -> $ö;
map(48) -> $ä;
map(49) -> {$§,$½};
map(50) -> shift;
map(51) -> {$',$*};
map(94) -> {$<,$>,$|};
map(52) -> $z;
map(53) -> $x;
map(54) -> $c;
map(55) -> $v;
map(56) -> $b;
map(57) -> $n;
map(58) -> $m;
map(59) -> {$,,$;};
map(60) -> {$.,$:};
map(61) -> {$-,$_};
map(64) -> alt;
map(65) -> $ ;
map(66) -> capsLock;
map(67) -> f1;
map(68) -> f2;
map(69) -> f3;
map(70) -> f4;
map(71) -> f5;
map(72) -> f6;
map(73) -> f7;
map(74) -> f8;
map(75) -> f9;
map(76) -> f10;
map(95) -> f11;
map(96) -> f12;
map(98)  -> {arrow,up};
map(100) -> {arrow, left};
map(102) -> {arrow, right};
map(104) -> {arrow, down};
map(109) -> ctrl;
map(113) -> altGR;
map(X)  -> undefined.

    
    

    

%% Buttons

%% ESC F1 .. F12 
%%  9  67..96

%% §   1  2   <-
%% 49  10 11  22
%% Tab q w w er    ret
%% 23  24          36 






