#!/bin/sh
 # \
 exec wish "$0" -- "$@"

 set i {}

 proc color {type} {
    global i
    set color [tk_chooseColor -initialcolor [$i.text cget -$type]]
    if {$color != ""} {$i.text configure -$type $color}
 }

 proc fontupdate {type args} {
    global i font
    switch $type {
        scale {
            font configure font -size $args
            font configure fontb -size $args -weight bold
        }
        font {
            font configure font -family $font
            font configure fontb -family $font -weight bold
        }
    }
 }

 if {![winfo exists .$i]} {toplevel .$i}
 .$i configure -bd 2 -relief raised
 wm title .$i "Font Viewer"
 wm geometry .$i 350x200
 frame $i.bottom
 text $i.text -background white
 frame $i.bottom.1 -relief groove -bd 2
 frame $i.bottom.2 -relief groove -bd 2
 scale $i.bottom.1.size -from 6 -to 22 -orient h -command [list      fontupdate scale]
 button $i.bottom.2.fg -text Foreground -command "color foreground"
 button $i.bottom.2.bg -text Background -command "color background"
 set menu [tk_optionMenu $i.bottom.1.font font "Choose a font"]

 pack $i.bottom -side bottom -fill both
 pack $i.text -fill both
 pack $i.bottom.1.font -pady 3
 pack $i.bottom.1.size

 grid $i.bottom.1 $i.bottom.2 -pady 5 -padx 5 -ipady 5 -ipadx 5 -sticky ns
 grid columnconfigure $i.bottom {0 1} -weight 1
 grid $i.bottom.2.fg -sticky nesw -pady 3
 grid $i.bottom.2.bg -sticky nesw -pady 3

 font create font -family fixed
 font create fontb -family fixed -weight bold
 $i.text configure -font font
 $i.text tag configure bold -font fontb
 $i.text tag configure underline -underline 1
 $i.text insert end {normal } {} {bold} bold { } {} {underline} underline { } {} {both} {bold underline}
 $i.text configure -state disabled

 $menu delete 0
 foreach x [lsort -dictionary [font families]] {
    $menu add radiobutton -label $x -variable font -command [list fontupdate font]
 }

 $i.bottom.1.size set 14
