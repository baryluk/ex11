
  +------------------------------------------------------+
  |                                                      |
  |  ex11 -- the Erlang X11 binding			 |
  |							 |
  +------------------------------------------------------+


  Congratulations  - you  have succeeded  in starting  the  X11 Erlang
graphics system. ex11 is an Erlang binding to the X11 protocol.

  Using  the ex11  binding users  can easily  write  advanced graphics
packages which are directly callable from Erlang.

  ex11 is  written almost  entirely in Erlang  (the only  exception to
this  is  a small  interface  program  which  decodes jpeg  images)  -
otherwise everything is written in Erlang.

  ex11  implements a  useful subset  of  the X11  protocol. While  not
complete  this  subset can  be  used for  writing  a  large number  of
different graphics programs.

  In addition to the X11  protocol binding the distribution contains a
tutorial on graphics programming and a simple widget library.

  The widget library contains  code for buttons, sliders, entries etc.
The  ex11  widget  set  is   easy  to  build  using  the  routines  in
ex11_lib.erl.  The tutorial describes in  detail how to write a simple
"hello  world"   application,  and   follows  this  with   a  detailed
description of how the button widget works.

  ex11 is  not in any sense  complete. To date  the following problems
have been successfully addressed:

     - Font matching and font metric querying
     - Displays in a fixed width font
     - Colors 
     - Mouse and mouse movement event handling
     - Graphic contexts
     - Line graphics
     - Display of JPG images
     - Cursor management
     - Parsing keyboard events

  And the following simple widgets have been programmed

  - text widget (this widget) 
  - slider widgets 
  - entry widget 
  - button widgets 
  - JPG display widget
     
  In addition arbitrary nested window structures can be created, and
text and graphics can be displayed in any window.

  The fact that the *entire* package is written in Erlang has a number
of beneficial  side-effects.  To  start with *all*  low-level graphics
primitives  are  immediately  available  *without*  changing  language
level. If a widget does not behave  exactly as we had hoped then it is
very easy to change the code *without* changing to (say) C.

  Surprisingly the widget code is a lot shorter than the corresponding
C code, and  amazingly is is very fast. I suspect  the reason for this
is that many abstraction layers have been removed.

  The "traditional"  way of  building graphics is  Erlang has  been to
interface the language to some graphics package, like this:

    +----------------------------------+
    |  Erlang                          |
    +----------------------------------+
    | Erlang graphics library routines |
    +----------------------------------+
    | Erlang port program              |
    +----------------------------------+
    | C port decoding routines         |
    +----------------------------------+
    | C widget library                 |
    +----------------------------------+
    | X11 library                      |
    +----------------------------------+
    | X11 marshalling routines         |
    +----------------------------------+
    | X11 server                       |
    +----------------------------------+

  The X  windows system  has a traditional  client/server architecture
and is  designed so that clients  and servers can  reside on different
machine communication  through a socket -  this makes it  very easy to
entirely dispense  with the  C interface routines  and let  the Erlang
programs communicate directly with the x server socket, something like
this:

				
    +----------------------------------+
    | Erlang                           |
    +----------------------------------+
    | Erlang graphics library routines |
    +----------------------------------+
    | Erlang X11 marshalling routines  |
    +----------------------------------+
    | X11 server                       |
    +----------------------------------+

  Have fun 

  /Joe Armstrong






