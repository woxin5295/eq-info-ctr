/* Copyright (c) 2000 Boulder Real Time Technologies, Inc. */
/* All rights reserved */

/* This software module is wholly owned by Boulder Real Time 
   Technologies, Inc. Any use of this software module without
   express written permission from Boulder Real Time Technologies,
   Inc. is prohibited. */

#include "stock.h"

/* K2 event file codes */
#define	K2EVT_MAX_K2_CHANNELS		12
#define	K2EVT_MAX_MW_CHANNELS		18
#define	K2EVT_STN_ID_LENGTH		5
#define	K2EVT_CHANNEL_ID_LENGTH		5
#define	K2EVT_COMMENT_LENGTH		33
#define	K2EVT_STREAM_K2_MAX_VOTERS	(K2EVT_MAX_K2_CHANNELS+3)
#define	K2EVT_STREAM_MW_MAX_VOTERS	(K2EVT_MAX_MW_CHANNELS+3)
#define K2EVT_MODEM_INITCMD_LENGTH	64
#define K2EVT_MODEM_DIALPREFIX_LENGTH	16
#define K2EVT_MODEM_DIALSUFFIX_LENGTH	16
#define K2EVT_MODEM_HANGUPCMD_LENGTH	16
#define K2EVT_MODEM_AUTOANSWERON_LENGTH	16
#define K2EVT_MODEM_AUTOANSWEROFF_LENGTH	16
#define K2EVT_MODEM_PHONES		4
#define K2EVT_MODEM_PHONENUMBER_LENGTH	24
#define K2EVT_TIMECODE_BYTES		13
#define K2EVT_TAG_SIZE			16
#define K2EVT_FILEHDR_SIZE_K2		2040
#define K2EVT_FILEHDR_SIZE_MW		2736

/* g to nm/s**2 physical constant */
#define	G2NMPS2				(9.80665e9)

/* K2 Tag Frame Header */
typedef struct k2tag_ {
	unsigned char sync;		/* Sync character 'K' */
	unsigned char byteOrder;	/* = 0 for LSB first (INTEL)
					   = 1 for MSB first (MOTOROLA) */
	unsigned char version;		/* File format version (K2_KFF_VERSION) */
	unsigned char instrumentType;	/* Instrument type */
	unsigned int type;		/* structure type */
	unsigned short length;		/* structure size in bytes
					   (no. bytes in file header or frame header) */
	unsigned short dataLength;	/* no. of data bytes following the structure
					   (00 if file header, else no. of data
					   multiplexed data bytes) */
	unsigned short id;		/* unique instrument id (e.g. serial number)
					   (used for multi-instrument files) */
	unsigned short checksum;	/* 16 bit checksum of structure + data */
} K2EvtTag;

/* K2 File Header */
typedef struct k2filehdr_ {
	char id[4];			/* = 'KMI' to denote a Kinemetrics file 
					   (NOTE: Added extra charafcter to hold NULL) */
	unsigned char instrumentCode;	/* = '9' for K2 */
	unsigned short headerVersion;	/* header version * 100 */
	unsigned short headerBytes;	/* size of header */
	struct misc_ro_params_ {
		unsigned char a2dBits;		/* A/D bits per sample */
		unsigned char sampleBytes;	/* bytes per sample (= 3) */
		unsigned char restartSource;	/* = 0 - unknown
						   = 1 - power switch
						   = 2 - user command
						   = 3 - software watchdog
						   = 4 - DSP failure
						   = 5 - battery failure
						   = 7 - memory error */
		char bytePad[3];		/* for expansion */
		unsigned short installedChan;	/* number of data streams */
		unsigned short maxChannels;	/* physical number of channels */
		unsigned short sysBlkVersion;	/* sys block version * 100 */
		unsigned short bootBlkVersion;	/* boot block version * 100 */
		unsigned short appBlkVersion;	/* application block version * 100 */
		unsigned short dspBlkVersion;	/* DSP version * 100 */
		short int batteryVoltage;	/* voltage * 10 - negative value indicates charging */
		unsigned short crc;		/* 16-bit CRC of entire file with this word
						   set to 0xffff
						   NOTE: Not used at present */
		unsigned short flags;		/* bit 0 = 1 if DSP system error */
		short int temperature;		/* degrees C X 10 */
		short int wordpad[3];		/* for expansion */
		int dwordpad[4];		/* for expansion */
	} ro_misc;
	struct timing_ro_params_ {
		unsigned char clockSource;	/* 0 = RTC from cold start
						   1 = keyboard
						   2 = Sync with external reference pulse
						   3 = internal GPS */
		unsigned char gpsStatus;	/* Bit 0 set = currently checking for presence
							       of GPS board
						   Bit 1 set = GPS board present
						   Bit 2 set = error communication with GPS
						   Bit 3 set = failed to lock within allotted
							       time (gpsMaxTurnOnTime)
						   Bit 4 set = not locked
						   Bit 5 set = GPS power is on */
		unsigned char gpsSOH;		/* same as Acutime SOH code */
		unsigned char bytepad[5];	/* for expansion */
		unsigned short gpsLockFailCount;/* no. of time GPS failed to lock within
						   gpsMaxTurnOnTime */
		unsigned short gpsUpdateRTCCount;/* no. of time GPS actually updated the RTC */
		short int acqDelay;		/* time in msec between actual A/D conversion
						   and DSP output */
		short int gpsLatitude;		/* latitude x 100 , degrees North */
		short int gpsLongitude;		/* longitude x 100 , degrees East */
		short int gpsAltitude;		/* altitude in meters */
		unsigned short dacCount;	/* dac counts */
		short int wordpad;		/* for expansion */
		short int gpsLastDrift[2];	/* in msec (e.g. 5 = RTC was 5 msec faster
						   than GPS */
		unsigned int gpsLastTurnOnTime[2];/* time when GPS was last turned on */
		unsigned int gpsLastUpdateTime[2];/* time of last RTC update */
		unsigned int gpsLastLockTime[2];/* time of last GPS lock */
		int dwordpad[4];		/* for expansion */
	} ro_timing;
	struct channel_ro_params_ {
		int maxPeak;			/* in raw sample counts */
		unsigned int maxPeakOffset;	/* offset from start of file */
		int minPeak;
		unsigned int minPeakOffset;
		int mean;			/* in raw sample counts */
		int aqOffset;
		int dwordpad[3];		/* for expansion */
	} ro_channel[K2EVT_MAX_MW_CHANNELS];
	struct stream_ro_params_ {
		unsigned int startTime;		/* first sample time, includes PEM */
		unsigned int triggerTime;
		unsigned int duration;		/* in no. of frames
						   NOTE: frames may have different sizes */
		unsigned short errors;
		unsigned short flags;		/* Bit 0 set = funtional test
						   Bit 1 set = sensor response test (SRT)
						   Bit 2 set = recorded data = trigger data */
		unsigned short startTimeMsec;
		unsigned short triggerTimeMsec;
		unsigned int nscans;		/* no. of scans in the event */
		unsigned int triggerBitMap;	/* indicates first channel to trigger */
		unsigned int pad;		/* for expansion */
	} ro_stream;
	struct misc_rw_params_ {
		unsigned short serialNumber;
		unsigned short nchannels;	/* no. of channels used */
		char stnID[K2EVT_STN_ID_LENGTH+1];
		char comment[K2EVT_COMMENT_LENGTH+1];
		short int elevation;		/* meters above sea level */
		float latitude;			/* degrees north */
		float longitude;		/* degrees east */
		short int userCodes[4];		/* 60 bytes to here */
		unsigned char cutlerCode;	/* 0 = Cutler off
						   1 = 4800 baud
						   2 = 9600 baud
						   3 = 19200 baud
						   4 = 38400 baud
						   5 = 57600 baud */
		unsigned char minBatteryVoltage;/* minimum alarm battery voltage x 10 */
		unsigned char cutler_decimation;/* Cutler grabber decimation factor
						   0 = 1:1 (raw)
						   1 = 1:2
						   2 = 1:4
						   3 = 1:5
						   4 = 1:10
						   5 = 1:20 */
		unsigned char cutler_irig_type;	/* 0 = B
						   1 = E (default)
						   2 = H */
		unsigned int cutler_bitmap;	/* Digital field station bitmap
						   (channels to output) */
		unsigned int channel_bitmap;	/* channels selected for acq storage */
		unsigned char cutler_protocol;	/* 0 = CRLF - USGS
						   1 = KMI/Agbabian */
		char siteID[17+1];
		unsigned char externalTrigger;	/* 0 = off, 1 = on */
		unsigned char networkFlag;	/* Bit 0 set = master
						   Bit 0 unset = slave */
	} rw_misc;
	struct timing_rw_params_ {
		unsigned char gpsTurnOnInterval;/* minutes between GPS update checking */
		unsigned char gpsMaxTurnOnTime;	/* max time in minutes GPS tries to
						   lock before giving up */
		unsigned char bytepad[6];
		short int localOffset;		/* hours ahead of UTC */
		short int wordpad[3];
		int dwordpad[4];
	} rw_timing;
	struct channel_rw_params_ {
		char id[K2EVT_CHANNEL_ID_LENGTH+1];
		char bytepad;			/* for expansion */
		unsigned short sensorSerialNumberExt;/* high word of serial no. */
		short int north;		/* displacement */
		short int east;			/* displacement */
		short int up;			/* displacement */
		short int altitude;		/* displacement */
		short int azimuth;		/* displacement */
		unsigned short sensorType;
		unsigned short sensorSerialNumber;/* low word of serial no. */
		unsigned short gain;		/* only 1 defined as gain */
		unsigned char triggerType;	/* 0 = threshold (default)
						   1 = STA/LTA */
		unsigned char iirTriggerFilter;	/* 0 = A
						   1 = B (default 0.1Hz to 12.5Hz @ 200sps)
						   2 = C */
		unsigned char StaSeconds;	/* STA seconds x 10 (default = 1.0 seconds)  */
		unsigned char LtaSeconds;	/* LTA seconds  (default = 60 seconds) */
		unsigned short StaLtaRatio;	/* STA/LTA trigger ratio * 10 (default = 4) */
		unsigned char StaLtaPercent;	/* STA/LTA detrigger percent of trigger ratio
						   0 = 10%
						   1 = 15%
						   2 = 20%
						   3 = 40% (default)
						   4 = 60%
						   5 = 100% */
		char bytepada;
		float fullscale;		/* volts */
		float sensitivity;		/* in volts per unit (e.g., g's) */
		float damping;			/* fraction of critical */
		float naturalFrequency;		/* hz */
		float triggerThreshold;		/* fraction of fullscale */
		float detriggerThreshold;	/* fraction of fullscale */
		float alarmTriggerThreshold;	/* fraction of fullscale */
		char bytepad2[16];
	} rw_channel[K2EVT_MAX_MW_CHANNELS];
	struct stream_rw_params_ {
		unsigned char filterFlag;	/* Bit 0 set = filtered data
						   Bit 1 set = auto FT after event
						   Bit 2 set = compressed */
		unsigned char primaryStorage;	/* = 0 - drive A:, etc. */
		unsigned char secondaryStorage;	/* = 1 - drive B:, etc. */
		unsigned char bytepad[3];	/* for expansion */
		unsigned short CheckinTime;	/* minutes since midnight, -1 = no checkin */
		unsigned short eventNumber;	/* NOT USED */
		unsigned short sps;		/* sampling rate */
		unsigned short apw;		/* array propagation windoe in seconds */
		unsigned short preEvent;	/* in seconds */
		unsigned short postEvent;	/* in seconds */
		unsigned short minRunTime;	/* in seconds */
		short int VotesToTrigger;
		short int VotesToDetrigger;
		char bytepada;
		unsigned char FilterType;	/* 0 = regular, 1 = causal */
		unsigned char DataFmt;		/* for streaming data - 0 = uncompressed, 1 = compressed */
		char Reserved;
		short int Timeout;		/* for streaming data - com mode */
		unsigned short TxBlkSize;
		unsigned short BufferSize;
		unsigned short SampleRate;	/* for streaming data - must be 0 or 100 */
		unsigned int TxChanMap;		/* for streaming data - channel bitmap */
		int dwordpad[2];		/* for expansion */
		struct voter_info_ {
			unsigned char type;		/* voter typr code */
			unsigned char number;		/* channel number, stream number, etc. */
			short int weight;		/* voting weight, range is -100 to 100 */
		} voterInfo[K2EVT_STREAM_MW_MAX_VOTERS];
	} rw_stream;
	struct modem_rw_params_ {
		char initCmd[K2EVT_MODEM_INITCMD_LENGTH+1];/* initialization string */
		char dialingPrefix[K2EVT_MODEM_DIALPREFIX_LENGTH+1];/* dialing prefix */
		char dialingSuffix[K2EVT_MODEM_DIALSUFFIX_LENGTH+1];/* dialing suffix */
		char hangupCmd[K2EVT_MODEM_HANGUPCMD_LENGTH+1];
		char autoAnswerOnCmd[K2EVT_MODEM_AUTOANSWERON_LENGTH+1];
		char autoAnswerOffCmd[K2EVT_MODEM_AUTOANSWEROFF_LENGTH+1];
		char phoneNumber[K2EVT_MODEM_PHONES][K2EVT_MODEM_PHONENUMBER_LENGTH+1];
		unsigned char waitForConnection;	/* seconds */
		unsigned char pauseBetweenCalls;	/* seconds */
		unsigned char maxDialAttempts;
		char cellShare;				/* 0 = 1 Hz output
							   1 = cell phone */
		char cellOnTime;			/* minutes */
		unsigned char cellWarmupTime;		/* seconds */
		short int cellStartTime[5];		/* minutes since midnight */
		char bytepad[4];
		unsigned short flags;			/* Bit 0 set = enable auto call out
							   Bit 1 set = call out on batteru < 12 V
							   Bit 2 set = call out on battery charge failed
							   Bit 3 set = call out on extreme temperature
							   Bit 4 set = call out on event
							   Bit 5 set = call out on GPS lock failure */
		char calloutMsg[46+1];
	} rw_modem;
} K2EvtFile;

/* K2 Frame Header */
typedef struct k2framehdr_ {
	unsigned char frameType;	/* like a version no. = K2EVT_XX_FRAME_HEADER_TYPE */
	unsigned char instrumentCode;	/* instrument code */
	unsigned short recorderID;
	unsigned short frameSize;	/* no. of frame bytes including 32 byte frame header */
	unsigned int blockTime;		/* block time */
	unsigned short channelBitMap;	/* 1 bit for each channel in use, Channel 1 is bit 0 */
	unsigned short streamPar;	/* Bits 00-11 = Stream sampling rate, 1..4095
					   Bits 12-15 = Stream number 0..15 */
	unsigned char frameStatus;	/* Bits 00-03 = frame sequence number 0..9
					   Bit 4      = stream triggered flag
					   Bit 5      = compressed flag
					   Bits 06-07 = Sample size 1 = 16 bits
								    2 = 24 bits
								    3 = 32 bits */
	unsigned char frameStatus2;	/* Bit 0 set  = issued the ADD SCAN command */
	unsigned short msec;
	unsigned char channelBitMap1;	/* extended channel bitmap (channels 17-24) */
	unsigned char timeCode[K2EVT_TIMECODE_BYTES];
					/* Time code sampled every millisecond
					   Bits 04-07 of timeCode[ 0] = time code type
					   Bit 3      of timeCode[ 0] = time code bit sampled
									at first msec of frame (0=low,1=high)
					   Bit 2      of timeCode[ 0] = time code bit at 2nd msec
					   Bit 0      of timeCode[12] = last (100th msec) bit of time code */
} K2EvtFrame;

/* K2 Event Info */
typedef struct k2evtinfo_ {
	double time;			/* Time of first sample */
	double triggertime;		/* Time of trigger */
	double endtime;			/* Time of last sample */
	double samprate;		/* Sample rate */
	int nsamp;			/* Number of samples */
	int nchan;			/* Number of channels */
	int jdate;			/* Julian year-day of first sample */
	int ft;				/* set to 1 if FT */
	int channel_bitmap;	
	struct {			/* channels structure */
		double time_maxpeak;
		double time_minpeak;
		double percent_maxpeak;
		double percent_minpeak;
		double v_maxpeak;
		double v_minpeak;
		double g_maxpeak;
		double g_minpeak;
		double calib;
		int maxpeak;
		int minpeak;
	} chans[K2EVT_MAX_MW_CHANNELS];
} K2EvtInfo;

int evt_readtag (FILE *file, unsigned char **buf_ptr, K2EvtTag *tag, int *evt_bigendian, int *size, int *datasize, unsigned short *chk);
int evt_readk2filehdr (FILE *file, unsigned char **buf_ptr, K2EvtFile *fhdr, int expected_size, int evt_bigendian, int ismw, unsigned short *computed_checksum, int *nframes);
int evt_readframehdr (FILE *file, unsigned char **buf_ptr, K2EvtFrame *fhdr, int expected_size, int evt_bigendian, unsigned short *computed_checksum);
int evt_encodek2filehdr (K2EvtFile *fhdr, unsigned char *buffer, int evt_bigendian, int ismw);
int evt_demuxdata (FILE *file, unsigned char **buf_ptr, K2EvtFile *fhdr, int *nsamps, int *nchans, int **dataout, int *dataout_size);
int evt_parseparams (K2EvtFile *hdr, int ismw, Pf *params, char *error);
int evt2info (K2EvtFile *fhdr, K2EvtInfo *info);
unsigned short evt_checksum (unsigned char *ptr, int size);

