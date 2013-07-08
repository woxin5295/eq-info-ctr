#
#       Startstop (Solaris Version) Configuration File
#
#    <nRing> is the number of transport rings to create.
#    <Ring> specifies the name of a ring followed by it's size
#    in kilobytes, eg        Ring    WAVE_RING 1024
#    The maximum size of a ring is 1024 kilobytes.
#    Ring names are listed in file earthworm.h.
#
 nRing               4
 Ring   AD_RING   1024
 Ring   WAVE_RING 1024
 Ring   PICK_RING 1024
 Ring   HYPO_RING 1024
#
 MyModuleId    MOD_STARTSTOP  # Module Id for this program
 HeartbeatInt  50             # Heartbeat interval in seconds
 MyClassName   RT             # For this program
 MyPriority     4             # For this program
 LogFile        1             # 1=write a log file to disk, 0=don't
#
#    Class must be RT or TS
#    RT priorities from 0 to 59
#    TS priorities le 0
#
#    If the command string required to start a process contains
#       embedded blanks, it must be enclosed in double-quotes.
#    Processes may be disabled by commenting them out.
#    To comment out a line, preceed the line by #.
#
# Process           "tankplayer tankplayer.d"
# Class/Priority     TS 0
#
 Process          "vdl -v parms.vdl -n 18 -s 40 -f 50 -j 100 -t 1 -i VPE -k 0 -e vdl_ew.d"
 Class/Priority    RT 10
#
#
 Process           "ad_demux ad_demux_a.d"
 Class/Priority     TS 0
#
# Process           "ad_demux ad_demux_b.d"
# Class/Priority     TS 0
#
# Process           "getwave"
# Class/Priority     TS 0
#
# Process           "getad"
# Class/Priority     TS 0
#
# Process           "pick_ew pick_ew.d"
# Class/Priority     TS 0
#
 Process          "coaxtoring coaxtoring.d"
 Class/Priority    RT 10
#
# Lardass picker (picker A)
# ************************
# Process          "picker picker_a.d"
# Class/Priority    RT 5
#
# Honker picker (picker B)
# ***********************
# Process          "picker picker_b.d"
# Class/Priority    RT 5
#
# Process          "binder binder.d"
# Class/Priority    TS 0
#
# Process          "eqproc eqproc.d"
# Class/Priority    TS 0
#
# Process          "diskmgr diskmgr.d"
# Class/Priority    TS 0
#
# Process          "pagerfeeder pagerfeeder.d"
# Class/Priority    TS 0
#
# Process          "statmgr statmgr.d"
# Class/Priority    TS 0
#
# Process          "eqalarm_ew eqalarm_ew.d"
# Class/Priority    TS 0
#
# Process          "copystatus WAVE_RING HYPO_RING"
# Class/Priority    RT 5
#
# Process          "copystatus PICK_RING HYPO_RING"
# Class/Priority    RT 5
#
# Process          "menlo_report menlo_report.d"
# Class/Priority    TS 0
#
# Process          "putter ../data/lp512.pik"
# Class/Priority    TS 0
#
# Process          "wave_server wave_server.d"
# Class/Priority    TS 0
#
# Process          "export_generic export_generic.d"
# Class/Priority    RT 0
