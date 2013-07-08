proc init_orb {} {
        global State plot
 
        set orb $State(hypo_source)
 
        set fname [format "/tmp/orbwish%d" [pid]]
        set fdb [open $fname w]
        puts $fdb rt1.0
        puts $fdb ""
        close $fdb
 
        set mysource $plot(waveform_sourcecode)
 
        set dbcallback [dblookup [dbopen $fname r] 0 origin 0 0]
 
        exec unlink $fname
 
        orb $orb $orb r
         
        $orb orbdbcallback $dbcallback wormwatch_dbcallback
 
        orbchannel $orb.$mysource
 
        rtd $plot(rtdframe).myrtd -filter "BW 0.01 1 0 0" \
                -source $State(hypo_source).$plot(waveform_sourcecode) \
                -twin 600.0 \
                -width 750 -height $plot(rtd_height) \
                -abottom -2500.0 -atop 2500.0
        pack $plot(rtdframe).myrtd -side bottom -fill x
 
        set mytext "$plot(waveform_label):"
        label $plot(rtdframe).myrtd_label -text $mytext
        pack $plot(rtdframe).myrtd_label -side bottom -anchor w
 
        $orb select "(/db/origin|$mysource)"
 
        $orb start
}
