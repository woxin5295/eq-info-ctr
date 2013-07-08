#
# vdl_ew configuration file
# VDL under Earthworm is started by startstop, and given the commandline found in startstop.d
# That's where vdl get's its cusomary command line arguments. The file here is read
# by the "feedme" routines, which determine vdl's identity to earthworm, and what to feed to vdl.
#
 MyModuleId     MOD_VDL_EW   # module id for this export,
 RingName       WAVE_RING      # transport ring to use for input/output,
 HeartBeatInt   10             # Heartbeat interval in seconds (Earthworm internal)
		# feedme() will consider beating the heart each time it's called, but will
		# not beat faster than the interval above
 LogFile        1              # If 0, don't write logfile at all,
 MaxMsgSize	4096	# length of largest message we'll ever handle - in bytes
 MaxMessages	1000	 # message buffer size (#channels * seconds to buffer)

# List the message logos to grab from transport ring for us to choose from
#  - as to which we will forward to VDL
#              Installation       Module       Message Type
 GetMsgLogo    INST_UTAH      MOD_WILDCARD     TYPE_TRACEBUF

# UUSS	#pass through #TCP_PORT 2003 #TCP_HOST uussr1.seis.utah.edu
TCP_PORT 2003
TCP_HOST nsn4.cr.usgs.gov

# List of pin numbers to send to VDL, and the corresponding VDL-USNSN designators
#		Earthworm pin number	USNSN designator
SendPin  83   0		# ARUT SHZ
SendPin  24   1		# DAU SHZ
SendPin  68   2		# PTI SHZ
SendPin 153   3		# MSU SHZ
SendPin  85   5		# TMI SHZ
SendPin 274   6     # SRU BHZ
SendPin 276   7     # SRU BHN
SendPin 275   8     # SRU BHE
SendPin 256   9		# NOQ BHZ
SendPin 258  10		# NOQ BHN
SendPin 257  11		# NOQ BHE
SendPin 262  12		# CTU BHZ
SendPin 264  13		# CTU BHN
SendPin 263  14		# CTU BHE
SendPin 268  21         # HVU BHZ
SendPin 270  22         # HVU BHN
SendPin 269  23         # HVU BHE
SendPin 280  30         # MPU BHZ
SendPin 282  31         # MPU BHN
SendPin 281  32         # MPU BHE
SendPin 286  30         # YMR BHZ
SendPin 288  31         # YMR BHN
SendPin 287  32         # YMR BHE

