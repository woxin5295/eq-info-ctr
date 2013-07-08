#
# This is local_mag's parameter file

#  Basic Earthworm setup:
#
MyModuleId         MOD_LOCAL_MAG  # module id for this instance of local_mag 
RingName           PICK_RING   # shared memory ring for input/output
LogFile            1           # 0 to completely turn off disk log file
Verbose            1           # Whether to report lots of messages
HeartBeatInterval  15          # seconds between heartbeats
WaveServer         137.229.32.103:16022 # IP Address and Port for wave-server

# List the message logos to grab from transport ring
#              Installation       Module          Message Types
GetRequestsFrom  INST_FAIRBANKS    MOD_CSS_REPORT	  TYPE_ML_REQUEST
MagNetwork         mag         # Name of network of stations to use for
                               # magnitude calculations
WoodAndersonResponseFile /usr/local/Iceworm/Development/src/bin/local_mag/Wood_Anderson
BandpassFilterOrder  4        /* order of bandpass filter for local mag */
BandpassUpperCutoff  10      /* upper cutoff (Hz) of bandpass filter */
BandpassLowerCutoff  0.5      /* lower cutoff (Hz) of bandpass filter */

MaxVal             2048        # Maximum amplitude of seismic trace
ClipThresh         0.9         # Threshold (Fraction of MaxVal) above which
			       #  seismic trace is declared clipped
Spreading          1.11        # Geometrical spreading constant for local-mag
Atten              0.00189     # Attenuation constant per km
Refdist            100.        # Reference distance for magnitude calculation
Refmag             3.0         # Magnitude at reference distance
