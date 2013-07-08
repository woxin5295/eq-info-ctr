#include <limits.h>
#include <stdio.h>
#include <math.h>
#include <unistd.h>

#include "tr.h"
#include "k2evt.h"
#include "swapbytes.h"
#include "brttutil.h"


unsigned short chanbitmap(int nchan);
double epoch2k2time (double epoch) ;
void local22k2 (unsigned char **k2_pointer, void *data_pointer) ;
void local42k2 (unsigned char **k2_pointer, void *data_pointer) ;
void localstring2k2 (unsigned char **k2_pointer, char *data_pointer, int size) ;
void localbyte2k2 (unsigned char **k2_pointer, void *data_pointer) ;
void clearstring (unsigned char **ptr, int nbytes) ;
unsigned short k2_checksum (unsigned char *ptr, int size) ;
int encodek2Frame (unsigned char *buf_ptr, K2EvtFrame *fh) ;
int encodek2tag (unsigned char *buf_ptr, K2EvtTag *tag) ;
int encodek2header (unsigned char *buf_ptr, K2EvtFile *hdr) ;
void newFileHeader(K2EvtFile **inhdr, Pf *pf,char *sta );

unsigned short chanbitmap(int nchan) {
	unsigned short s=0;
	int f=1;
	for (int j=0;j < nchan;j++) {
		s+=f; 
		f*=2;
	}
	return (s);
}
double epoch2k2time (double epoch) {
	double k2time;

	k2time=epoch - 315532800.000; /*10 years between 
									1980 and 1970, 
									conveniently  epoch(19080) */
	return(k2time);

}

void local22k2 (unsigned char **k2_pointer, void *data_pointer) {
	H2N2 (*k2_pointer, data_pointer, 1);
	*k2_pointer += 2;
}

void local42k2 (unsigned char **k2_pointer, void *data_pointer) {
	H2N4 (*k2_pointer, data_pointer, 1);
	*k2_pointer += 4;
}

void localstring2k2 (unsigned char **k2_pointer, char *data_pointer, int size) {
	memcpy (*k2_pointer, data_pointer, size);
	
	*k2_pointer += size;
}

void localbyte2k2 (unsigned char **k2_pointer, void *data_pointer) {
	memcpy (*k2_pointer, data_pointer, 1);
	*k2_pointer += 1;
}

void clearstring (unsigned char **ptr, int nbytes) {
	memset (*ptr, 0, nbytes);
	*ptr += nbytes;
}

unsigned short k2_checksum (unsigned char *ptr, int size) {
	unsigned int checksum = 0;
	int i;

	for (i=0; i<size; i++,ptr++) checksum += *ptr;

	return (checksum);
}



int encodek2Frame (unsigned char *buf_ptr, K2EvtFrame *fh) {
	unsigned char *ptr;
	ptr = buf_ptr;

	localbyte2k2 (&ptr, &(fh->frameType));
	localbyte2k2 (&ptr, &(fh->instrumentCode));
	local22k2 (&ptr, &(fh->recorderID));
	local22k2 (&ptr, &(fh->frameSize));
	local42k2 (&ptr, &(fh->blockTime));
	local22k2 (&ptr, &(fh->channelBitMap));
	local22k2 (&ptr, &(fh->streamPar));
	localbyte2k2 (&ptr, &(fh->frameStatus));
	localbyte2k2 (&ptr, &(fh->frameStatus2));
	local22k2 (&ptr, &(fh->msec));
	localbyte2k2 (&ptr, &(fh->channelBitMap1));
	localstring2k2 (&ptr, &(fh->timeCode),K2EVT_TIMECODE_BYTES);

	return(0);
}
int encodek2tag (unsigned char *buf_ptr, K2EvtTag *tag) {
	unsigned char *ptr;
	ptr = buf_ptr;
	
	localbyte2k2 (&ptr, &(tag->sync));
	localbyte2k2 (&ptr, &(tag->byteOrder));
	localbyte2k2 (&ptr, &(tag->version));
	localbyte2k2 (&ptr, &(tag->instrumentType));
	local42k2 (&ptr, &(tag->type));
	local22k2 (&ptr, &(tag->length));
	local22k2 (&ptr, &(tag->dataLength));
	local22k2 (&ptr, &(tag->id));
	local22k2 (&ptr, &(tag->checksum));
	return(0);

}
int encodek2header (unsigned char *buf_ptr, K2EvtFile *hdr) {
	unsigned char *ptr;
	int i, nch, nvt;

	ptr = buf_ptr;

	localstring2k2 (&ptr, hdr->id, 3);
	localbyte2k2 (&ptr, &(hdr->instrumentCode));
	local22k2 (&ptr, &(hdr->headerVersion));
	local22k2 (&ptr, &(hdr->headerBytes));
	localbyte2k2 (&ptr, &(hdr->ro_misc.a2dBits));
	localbyte2k2 (&ptr, &(hdr->ro_misc.sampleBytes));
	localbyte2k2 (&ptr, &(hdr->ro_misc.restartSource));
	clearstring (&ptr, 3);

	local22k2 (&ptr, &(hdr->ro_misc.installedChan));
	local22k2 (&ptr, &(hdr->ro_misc.maxChannels));
	local22k2 (&ptr, &(hdr->ro_misc.sysBlkVersion));
	local22k2 (&ptr, &(hdr->ro_misc.bootBlkVersion));
	local22k2 (&ptr, &(hdr->ro_misc.appBlkVersion));
	local22k2 (&ptr, &(hdr->ro_misc.dspBlkVersion));
	local22k2 (&ptr, &(hdr->ro_misc.batteryVoltage));
	local22k2 (&ptr, &(hdr->ro_misc.crc));
	local22k2 (&ptr, &(hdr->ro_misc.flags));
	local22k2 (&ptr, &(hdr->ro_misc.temperature));
	clearstring (&ptr, 22);

	localbyte2k2 (&ptr, &(hdr->ro_timing.clockSource));
	localbyte2k2 (&ptr, &(hdr->ro_timing.gpsStatus));
	localbyte2k2 (&ptr, &(hdr->ro_timing.gpsSOH));

	clearstring (&ptr, 5);

	local22k2 (&ptr, &(hdr->ro_timing.gpsLockFailCount));
	local22k2 (&ptr, &(hdr->ro_timing.gpsUpdateRTCCount));
	local22k2 (&ptr, &(hdr->ro_timing.acqDelay));
	local22k2 (&ptr, &(hdr->ro_timing.gpsLatitude));
	local22k2 (&ptr, &(hdr->ro_timing.gpsLongitude));
	local22k2 (&ptr, &(hdr->ro_timing.gpsAltitude));
	local22k2 (&ptr, &(hdr->ro_timing.dacCount));
	clearstring (&ptr, 2);
	local22k2 (&ptr, &(hdr->ro_timing.gpsLastDrift[0]));
	local22k2 (&ptr, &(hdr->ro_timing.gpsLastDrift[1]));
	local42k2 (&ptr, &(hdr->ro_timing.gpsLastTurnOnTime[0]));
	local42k2 (&ptr, &(hdr->ro_timing.gpsLastTurnOnTime[1]));
	local42k2 (&ptr, &(hdr->ro_timing.gpsLastUpdateTime[0]));
	local42k2 (&ptr, &(hdr->ro_timing.gpsLastUpdateTime[1]));
	local42k2 (&ptr, &(hdr->ro_timing.gpsLastLockTime[0]));
	local42k2 (&ptr, &(hdr->ro_timing.gpsLastLockTime[1]));
	clearstring (&ptr, 16);

	nch = K2EVT_MAX_K2_CHANNELS;
	nvt = K2EVT_STREAM_K2_MAX_VOTERS;
	for (i=0; i<nch; i++) {
		local42k2 (&ptr, &(hdr->ro_channel[i].maxPeak));
		local42k2 (&ptr, &(hdr->ro_channel[i].maxPeakOffset));
		local42k2 (&ptr, &(hdr->ro_channel[i].minPeak));
		local42k2 (&ptr, &(hdr->ro_channel[i].minPeakOffset));
		local42k2 (&ptr, &(hdr->ro_channel[i].mean));
		local42k2 (&ptr, &(hdr->ro_channel[i].aqOffset));
		clearstring (&ptr, 12);
	}

	local42k2 (&ptr, &(hdr->ro_stream.startTime));
	local42k2 (&ptr, &(hdr->ro_stream.triggerTime));
	local42k2 (&ptr, &(hdr->ro_stream.duration));
	local22k2 (&ptr, &(hdr->ro_stream.errors));
	local22k2 (&ptr, &(hdr->ro_stream.flags));
	local22k2 (&ptr, &(hdr->ro_stream.startTimeMsec));
	local22k2 (&ptr, &(hdr->ro_stream.triggerTimeMsec));
	local42k2 (&ptr, &(hdr->ro_stream.nscans));
	local42k2 (&ptr, &(hdr->ro_stream.triggerBitMap));
	clearstring (&ptr, 4);

	local22k2 (&ptr, &(hdr->rw_misc.serialNumber));
	local22k2 (&ptr, &(hdr->rw_misc.nchannels));
	localstring2k2 (&ptr, hdr->rw_misc.stnID, K2EVT_STN_ID_LENGTH);
	localstring2k2 (&ptr, hdr->rw_misc.comment, K2EVT_COMMENT_LENGTH);
	local22k2 (&ptr, &(hdr->rw_misc.elevation));
	local42k2 (&ptr, &(hdr->rw_misc.latitude));
	local42k2 (&ptr, &(hdr->rw_misc.longitude));
	local22k2 (&ptr, &(hdr->rw_misc.userCodes[0]));
	local22k2 (&ptr, &(hdr->rw_misc.userCodes[1]));
	local22k2 (&ptr, &(hdr->rw_misc.userCodes[2]));
	local22k2 (&ptr, &(hdr->rw_misc.userCodes[3]));
	localbyte2k2 (&ptr, &(hdr->rw_misc.cutlerCode));
	localbyte2k2 (&ptr, &(hdr->rw_misc.minBatteryVoltage));
	localbyte2k2 (&ptr, &(hdr->rw_misc.cutler_decimation));
	localbyte2k2 (&ptr, &(hdr->rw_misc.cutler_irig_type));
	local42k2 (&ptr, &(hdr->rw_misc.cutler_bitmap));
	local42k2 (&ptr, &(hdr->rw_misc.channel_bitmap));
	localbyte2k2 (&ptr, &(hdr->rw_misc.cutler_protocol));
	localstring2k2 (&ptr, hdr->rw_misc.siteID, 17);
	localbyte2k2 (&ptr, &(hdr->rw_misc.externalTrigger));
	localbyte2k2 (&ptr, &(hdr->rw_misc.networkFlag));

	localbyte2k2 (&ptr, &(hdr->rw_timing.gpsTurnOnInterval));
	localbyte2k2 (&ptr, &(hdr->rw_timing.gpsMaxTurnOnTime));
	clearstring (&ptr, 6);
	local22k2 (&ptr, &(hdr->rw_timing.localOffset));
	clearstring (&ptr, 2 + 2 + 2);
	clearstring (&ptr, 4 + 4 + 4 + 4);

	for (i=0; i<nch; i++) {
		localstring2k2 (&ptr, hdr->rw_channel[i].id, K2EVT_CHANNEL_ID_LENGTH);
		clearstring (&ptr, 1);
		local22k2 (&ptr, &(hdr->rw_channel[i].sensorSerialNumberExt));
		local22k2 (&ptr, &(hdr->rw_channel[i].north));
		local22k2 (&ptr, &(hdr->rw_channel[i].east));
		local22k2 (&ptr, &(hdr->rw_channel[i].up));
		local22k2 (&ptr, &(hdr->rw_channel[i].altitude));
		local22k2 (&ptr, &(hdr->rw_channel[i].azimuth));
		local22k2 (&ptr, &(hdr->rw_channel[i].sensorType));
		local22k2 (&ptr, &(hdr->rw_channel[i].sensorSerialNumber));
		local22k2 (&ptr, &(hdr->rw_channel[i].gain));
		localbyte2k2 (&ptr, &(hdr->rw_channel[i].triggerType));
		localbyte2k2 (&ptr, &(hdr->rw_channel[i].iirTriggerFilter));
		localbyte2k2 (&ptr, &(hdr->rw_channel[i].StaSeconds));
		localbyte2k2 (&ptr, &(hdr->rw_channel[i].LtaSeconds));
		local22k2 (&ptr, &(hdr->rw_channel[i].StaLtaRatio));
		localbyte2k2 (&ptr, &(hdr->rw_channel[i].StaLtaPercent));
		clearstring (&ptr, 1);
		local42k2 (&ptr, &(hdr->rw_channel[i].fullscale));
		local42k2 (&ptr, &(hdr->rw_channel[i].sensitivity));
		local42k2 (&ptr, &(hdr->rw_channel[i].damping));
		local42k2 (&ptr, &(hdr->rw_channel[i].naturalFrequency));
		local42k2 (&ptr, &(hdr->rw_channel[i].triggerThreshold));
		local42k2 (&ptr, &(hdr->rw_channel[i].detriggerThreshold));
		local42k2 (&ptr, &(hdr->rw_channel[i].alarmTriggerThreshold));
		clearstring (&ptr, 16);
	}

	localbyte2k2 (&ptr, &(hdr->rw_stream.filterFlag));
	localbyte2k2 (&ptr, &(hdr->rw_stream.primaryStorage));
	localbyte2k2 (&ptr, &(hdr->rw_stream.secondaryStorage));
	clearstring (&ptr, 3);
	local22k2 (&ptr, &(hdr->rw_stream.CheckinTime));
	local22k2 (&ptr, &(hdr->rw_stream.eventNumber));
	local22k2 (&ptr, &(hdr->rw_stream.sps));
	local22k2 (&ptr, &(hdr->rw_stream.apw));
	local22k2 (&ptr, &(hdr->rw_stream.preEvent));
	local22k2 (&ptr, &(hdr->rw_stream.postEvent));
	local22k2 (&ptr, &(hdr->rw_stream.minRunTime));
	local22k2 (&ptr, &(hdr->rw_stream.VotesToTrigger));
	local22k2 (&ptr, &(hdr->rw_stream.VotesToDetrigger));
	clearstring (&ptr, 1);
	localbyte2k2 (&ptr, &(hdr->rw_stream.FilterType));
	localbyte2k2 (&ptr, &(hdr->rw_stream.DataFmt));
	clearstring (&ptr, 1);
	local22k2 (&ptr, &(hdr->rw_stream.Timeout));
	local22k2 (&ptr, &(hdr->rw_stream.TxBlkSize));
	local22k2 (&ptr, &(hdr->rw_stream.BufferSize));
	local22k2 (&ptr, &(hdr->rw_stream.SampleRate));
	local42k2 (&ptr, &(hdr->rw_stream.TxChanMap));
	clearstring (&ptr, 4 + 4);
	for (i=0; i<nvt; i++) {
		localbyte2k2 (&ptr, &(hdr->rw_stream.voterInfo[i].type));
		localbyte2k2 (&ptr, &(hdr->rw_stream.voterInfo[i].number));
		local22k2 (&ptr, &(hdr->rw_stream.voterInfo[i].weight));
	}

	localstring2k2 (&ptr, hdr->rw_modem.initCmd, K2EVT_MODEM_INITCMD_LENGTH);
	localstring2k2 (&ptr, hdr->rw_modem.dialingPrefix, K2EVT_MODEM_DIALPREFIX_LENGTH);
	localstring2k2 (&ptr, hdr->rw_modem.dialingSuffix, K2EVT_MODEM_DIALSUFFIX_LENGTH);
	localstring2k2 (&ptr, hdr->rw_modem.hangupCmd, K2EVT_MODEM_HANGUPCMD_LENGTH);
	localstring2k2 (&ptr, hdr->rw_modem.autoAnswerOnCmd, K2EVT_MODEM_AUTOANSWERON_LENGTH);
	localstring2k2 (&ptr, hdr->rw_modem.autoAnswerOffCmd, K2EVT_MODEM_AUTOANSWEROFF_LENGTH);
	for (i=0; i<K2EVT_MODEM_PHONES; i++) {
		localstring2k2 (&ptr, hdr->rw_modem.phoneNumber[i], K2EVT_MODEM_PHONENUMBER_LENGTH);
	}
	localbyte2k2 (&ptr, &(hdr->rw_modem.waitForConnection));
	localbyte2k2 (&ptr, &(hdr->rw_modem.pauseBetweenCalls));
	localbyte2k2 (&ptr, &(hdr->rw_modem.maxDialAttempts));
	localbyte2k2 (&ptr, &(hdr->rw_modem.cellShare));
	localbyte2k2 (&ptr, &(hdr->rw_modem.cellOnTime));
	localbyte2k2 (&ptr, &(hdr->rw_modem.cellWarmupTime));
	for (i=0; i<5; i++) {
		local22k2 (&ptr, &(hdr->rw_modem.cellStartTime[i]));
	}
	clearstring (&ptr, 4);
	local22k2 (&ptr, &(hdr->rw_modem.flags));
	localstring2k2 (&ptr, hdr->rw_modem.calloutMsg, 46);

	return (0);
}

int newFrameHeader(K2EvtFrame **inframe,
		
	unsigned short recorderID,
	unsigned short frameSize,
	unsigned int blockTime ) {
	K2EvtFrame *frame;
	frame=*inframe;

	frame->frameType=3;
	frame->instrumentCode=9;
	frame->recorderID=recorderID;
	frame->frameSize=frameSize;
	frame->blockTime=blockTime;
	frame->channelBitMap=0;
	frame->streamPar=0;
	frame->frameStatus=0;
	frame->frameStatus2=0;
	frame->msec=0;
	frame->channelBitMap1=0;

	return(0);

}	

void newTag(K2EvtTag **intag, 
		unsigned char datatag,
		unsigned short numbytes, 
		unsigned short datalength, 
		unsigned short serialno, 
		unsigned short crc );
typedef struct Flags { 
    unsigned int verbose   : 4 ; 
	int display : 1 ;
} Flags ;

static void
usage ()
{
    cbanner ( "0.99", 
	     "[-p pfname] [-s subset] [-t triggertime] db file start end",
		 "Nikolaus Horn",
		 "Vienna / Austria",
	     "nikolaus.horn@zamg.ac.at");
    exit (1);
}


void newTag(K2EvtTag **intag, 
		unsigned char datatag,
		unsigned short numbytes, 
		unsigned short datalength, 
		unsigned short serialno, 
		unsigned short crc ) {
	K2EvtTag	*tag;
	tag=*intag;
	tag->sync='K';
	tag->byteOrder=1;	/* = 0 for LSB first (INTEL)
								   = 1 for MSB first (MOTOROLA) */
	tag->version=1;		/* File format version (K2_KFF_VERSION) */
	tag->instrumentType=24;	/* Instrument type */
	if (datatag == 1) {
		tag->type= 2 ;		/* data type */
	} else {
		tag->type= 1 ;		/* header type */
	}
	tag->length= numbytes;		/* structure size in bytes
					   (no. bytes in file header or frame header) */
	tag->dataLength=datalength ;	/* no. of data bytes following the structure
					   (00 if file header, else no. of data
					   multiplexed data bytes) */
	tag->id=serialno;		/* unique instrument id (e.g. serial number)
					   (used for multi-instrument files) */
	tag->checksum=crc;	/* 16 bit checksum of structure + data */
}




void newFileHeader(K2EvtFile **inhdr, Pf *pf,char *sta ) {
	int numchan=12;
	K2EvtFile *hdr;
	hdr=*inhdr;
	memcpy(hdr->id,"KMI",3);			/* = 'KMI' to denote a Kinemetrics file 
					   (NOTE: Added extra charafcter to hold NULL) */
	hdr->instrumentCode='9';	/* = '9' for K2 */
	hdr->headerVersion =100;	/* header version * 100 */
	hdr->headerBytes=2040;	/* size of header */
	hdr->ro_misc.a2dBits=24;		/* A/D bits per sample */
	hdr->ro_misc.sampleBytes=3;	/* bytes per sample (= 3) */
	hdr->ro_misc.restartSource=0;	/* = 0 - unknown
						   = 1 - power switch
						   = 2 - user command
						   = 3 - software watchdog
						   = 4 - DSP failure
						   = 5 - battery failure
						   = 7 - memory error */
		//hdr->ro_misc. bytePad[3];		/* for expansion */
	hdr->ro_misc.installedChan;	/* number of data streams */
	hdr->ro_misc.maxChannels;	/* physical number of channels */
	hdr->ro_misc.sysBlkVersion;	/* sys block version * 100 */
	hdr->ro_misc.bootBlkVersion;	/* boot block version * 100 */
	hdr->ro_misc.appBlkVersion;	/* application block version * 100 */
	hdr->ro_misc.dspBlkVersion;	/* DSP version * 100 */
	hdr->ro_misc.batteryVoltage;	/* voltage * 10 - negative value indicates charging */
	hdr->ro_misc.crc=0xffff;		/* 16-bit CRC of entire file with this word
			   set to 0xffff
			   NOTE: Not used at present */
	hdr->ro_misc.flags=0;		/* bit 0 = 1 if DSP system error */
	hdr->ro_misc.temperature=240;		/* degrees C X 10 */
	//hdr->ro_misc.wordpad[3];		/* for expansion */
	//hdr->ro_misc.dwordpad[4];		/* for expansion */
	hdr->ro_timing.clockSource;	/* 0 = RTC from cold start
				   1 = keyboard
				   2 = Sync with external reference pulse
						   3 = internal GPS */
	//hdr->ro_timing.gpsStatus;	
		 				/* Bit 0 set = currently checking for presence
									   of GPS board
						   Bit 1 set = GPS board present
						   Bit 2 set = error communication with GPS
						   Bit 3 set = failed to lock within allotted
									   time (gpsMaxTurnOnTime)
						   Bit 4 set = not locked
						   Bit 5 set = GPS power is on */
	//hdr->ro_timing.gpsSOH;		/* same as Acutime SOH code */
	//hdr->ro_timing.bytepad[5];	/* for expansion */
	//hdr->ro_timing.gpsLockFailCount;
					/* no. of time GPS failed to lock within
						gpsMaxTurnOnTime */
	//hdr->ro_timing.gpsUpdateRTCCount;/* no. of time GPS actually updated the RTC */
	//hdr->ro_timing.acqDelay;		
					/* time in msec between actual A/D conversion
						and DSP output */
	//hdr->ro_timing.gpsLatitude;		/* latitude x 100 , degrees North */
	//hdr->ro_timing.gpsLongitude;		/* longitude x 100 , degrees East */
	//hdr->ro_timing.gpsAltitude;		/* altitude in meters */
	// hdr->ro_timing.dacCount;	/* dac counts */
	//hdr->ro_timing.wordpad;		/* for expansion */
	//hdr->ro_timing.gpsLastDrift[2];	
					/* in msec (e.g. 5 = RTC was 5 msec faster
						than GPS */
	//hdr->ro_timing.gpsLastTurnOnTime[2];/* time when GPS was last turned on */
	//hdr->ro_timing.gpsLastUpdateTime[2];/* time of last RTC update */
	//hdr->ro_timing.gpsLastLockTime[2];/* time of last GPS lock */
	//hdr->ro_timing.dwordpad[4];		/* for expansion */
	for (int channo=0;channo<numchan;channo++) {
		hdr->ro_channel[channo].maxPeak=0;
		hdr->ro_channel[channo].maxPeakOffset=0;
		hdr->ro_channel[channo].minPeak=0;
		hdr->ro_channel[channo].minPeakOffset=0;
		hdr->ro_channel[channo].mean=0;
		hdr->ro_channel[channo].aqOffset=0;
	} 
	hdr->ro_stream.startTime=0.0;		/* first sample time, includes PEM */
	hdr->ro_stream.triggerTime=0.0;
	hdr->ro_stream.duration=0;		/* in no. of frames NOTE: frames may have different sizes */
	hdr->ro_stream.errors=0;
	hdr->ro_stream.flags;		
				/* Bit 0 set = funtional test
				   Bit 1 set = sensor response test (SRT)
				   Bit 2 set = recorded data = trigger data */
	hdr->ro_stream.startTimeMsec=0;
	hdr->ro_stream.triggerTimeMsec=0;
	hdr->ro_stream.nscans=0;		/* no. of scans in the event */
	hdr->ro_stream.triggerBitMap=0;	/* indicates first channel to trigger */
	hdr->ro_stream.pad=0;		/* for expansion */
	hdr->rw_misc.serialNumber=123;
	//attention, fix later
	hdr->rw_misc.nchannels=3;	/* no. of channels used */
	memcpy(hdr->rw_misc.stnID,sta,K2EVT_STN_ID_LENGTH);
	//hdr->rw_misc.comment[K2EVT_COMMENT_LENGTH+1];
	hdr->rw_misc.elevation=0;		/* meters above sea level */
	hdr->rw_misc.latitude=0;			/* degrees north */
	hdr->rw_misc.longitude=0;		/* degrees east */
	//hdr->rw_misc.userCodes= {'\0','\0','\0','\0'};		/* 60 bytes to here */
	hdr->rw_misc.cutlerCode=0;	
				/* 0 = Cutler off
				   1 = 4800 baud
				   2 = 9600 baud
				   3 = 19200 baud
				   4 = 38400 baud
				   5 = 57600 baud */
	hdr->rw_misc.minBatteryVoltage=115;/* minimum alarm battery voltage x 10 */
	hdr->rw_misc.cutler_decimation=0;/* Cutler grabber decimation factor
				   0 = 1:1 (raw)
				   1 = 1:2
				   2 = 1:4
				   3 = 1:5
				   4 = 1:10
				   5 = 1:20 */
	hdr->rw_misc.cutler_irig_type=1;	/* 0 = B
				   1 = E (default)
				   2 = H */
	hdr->rw_misc.cutler_bitmap=0;	/* Digital field station bitmap (channels to output) */
	hdr->rw_misc.channel_bitmap=0;	/* channels selected for acq storage */
	hdr->rw_misc.cutler_protocol=0;	/* 0 = CRLF - USGS 1 = KMI/Agbabian */
	//hdr->rw_misc.siteID[17+1];
	copystrip(hdr->rw_misc.siteID,sta,17);
	hdr->rw_misc.externalTrigger=0;	/* 0 = off, 1 = on */
	hdr->rw_misc.networkFlag;	/* Bit 0 set = master Bit 0 unset = slave */
	hdr->rw_timing.gpsTurnOnInterval;/* minutes between GPS update checking */
	hdr->rw_timing.gpsMaxTurnOnTime;	/* max time in minutes GPS tries to
				   lock before giving up */
	hdr->rw_timing.bytepad[6];
	hdr->rw_timing.localOffset;		/* hours ahead of UTC */
	hdr->rw_timing.wordpad[3];
	for (int channo=0;channo< K2EVT_MAX_MW_CHANNELS;channo++) {
		hdr->rw_channel[channo].id[K2EVT_CHANNEL_ID_LENGTH+1];
		hdr->rw_channel[channo]. bytepad;			/* for expansion */
		hdr->rw_channel[channo]. sensorSerialNumberExt;/* high word of serial no. */
		hdr->rw_channel[channo].north;		/* displacement */
		hdr->rw_channel[channo].east;			/* displacement */
		hdr->rw_channel[channo].up;			/* displacement */
		hdr->rw_channel[channo].altitude;		/* displacement */
		hdr->rw_channel[channo].azimuth;		/* displacement */
		hdr->rw_channel[channo].sensorType=32; /*EpiSensor*/
		hdr->rw_channel[channo].sensorSerialNumber;/* low word of serial no. */
		hdr->rw_channel[channo].gain=1;		/* only 1 defined as gain */
		hdr->rw_channel[channo].triggerType=0;	
				/* 0 = threshold (default)
				   1 = STA/LTA */
		hdr->rw_channel[channo].iirTriggerFilter;	
				/* 0 = A
				   1 = B (default 0.1Hz to 12.5Hz @ 200sps)
						   2 = C */
		 hdr->rw_channel[channo].StaSeconds;	/* STA seconds x 10 (default = 1.0 seconds)  */
		 hdr->rw_channel[channo].LtaSeconds;	/* LTA seconds  (default = 60 seconds) */
		 hdr->rw_channel[channo].StaLtaRatio;	/* STA/LTA trigger ratio * 10 (default = 4) */
		 hdr->rw_channel[channo].StaLtaPercent;	
						/* STA/LTA detrigger percent of trigger ratio
						   0 = 10%
						   1 = 15%
						   2 = 20%
						   3 = 40% (default)
						   4 = 60%
						   5 = 100% */
		hdr->rw_channel[channo].bytepada;
		hdr->rw_channel[channo].fullscale=20;		/* volts */
		hdr->rw_channel[channo].sensitivity=20;		/* in volts per unit (e.g., g's) */
		hdr->rw_channel[channo].damping;			/* fraction of critical */
		hdr->rw_channel[channo].naturalFrequency;		/* hz */
		hdr->rw_channel[channo].triggerThreshold;		/* fraction of fullscale */
		hdr->rw_channel[channo].detriggerThreshold;	/* fraction of fullscale */
		hdr->rw_channel[channo].alarmTriggerThreshold;	/* fraction of fullscale */
		hdr->rw_channel[channo].bytepad2[16];
	} 
	hdr->rw_stream.filterFlag=0;	/* Bit 0 set = filtered data
			   Bit 1 set = auto FT after event
			   Bit 2 set = compressed */
	hdr->rw_stream.primaryStorage=0;	/* = 0 - drive A:, etc. */
	hdr->rw_stream.secondaryStorage=0;	/* = 1 - drive B:, etc. */
	//hdr->rw_stream.bytepad[3];	/* for expansion */
	hdr->rw_stream.CheckinTime=-1;	/* minutes since midnight, -1 = no checkin */
	hdr->rw_stream.eventNumber;	/* NOT USED */
	hdr->rw_stream.sps;		/* sampling rate */
	hdr->rw_stream.apw;		/* array propagation windoe in seconds */
	hdr->rw_stream.preEvent;	/* in seconds */
	hdr->rw_stream.postEvent;	/* in seconds */
	hdr->rw_stream.minRunTime;	/* in seconds */
	hdr->rw_stream.VotesToTrigger;
	hdr->rw_stream.VotesToDetrigger;
	hdr->rw_stream.bytepada;
	hdr->rw_stream.FilterType;	/* 0 = regular, 1 = causal */
	hdr->rw_stream.DataFmt;		/* for streaming data - 0 = uncompressed, 1 = compressed */
	hdr->rw_stream.Reserved;
	hdr->rw_stream.Timeout;		/* for streaming data - com mode */
	hdr->rw_stream.TxBlkSize;
	hdr->rw_stream.BufferSize;
	hdr->rw_stream.SampleRate;	/* for streaming data - must be 0 or 100 */
	hdr->rw_stream.TxChanMap;		/* for streaming data - channel bitmap */
	//hdr->rw_stream.dwordpad[2];		/* for expansion */
	for (int voter=0;voter< K2EVT_STREAM_MW_MAX_VOTERS;voter++) {
		hdr->rw_stream.voterInfo[voter].type;		/* voter typr code */
		hdr->rw_stream.voterInfo[voter].number;		/* channel number, stream number, etc. */
		hdr->rw_stream.voterInfo[voter].weight;		/* voting weight, range is -100 to 100 */
	} 
	hdr->rw_modem.initCmd[K2EVT_MODEM_INITCMD_LENGTH+1];/* initialization string */
	hdr->rw_modem.dialingPrefix[K2EVT_MODEM_DIALPREFIX_LENGTH+1];/* dialing prefix */
	hdr->rw_modem.dialingSuffix[K2EVT_MODEM_DIALSUFFIX_LENGTH+1];/* dialing suffix */
	hdr->rw_modem.hangupCmd[K2EVT_MODEM_HANGUPCMD_LENGTH+1];
	hdr->rw_modem.autoAnswerOnCmd[K2EVT_MODEM_AUTOANSWERON_LENGTH+1];
	hdr->rw_modem.autoAnswerOffCmd[K2EVT_MODEM_AUTOANSWEROFF_LENGTH+1];
	hdr->rw_modem.phoneNumber[K2EVT_MODEM_PHONES][K2EVT_MODEM_PHONENUMBER_LENGTH+1];
	hdr->rw_modem.waitForConnection;	/* seconds */
	hdr->rw_modem.pauseBetweenCalls;	/* seconds */
	hdr->rw_modem.maxDialAttempts;
	hdr->rw_modem.cellShare;	/* 0 = 1 Hz output 1 = cell phone */
	hdr->rw_modem.cellOnTime;			/* minutes */
	hdr->rw_modem.cellWarmupTime;		/* seconds */
	hdr->rw_modem.cellStartTime[5];		/* minutes since midnight */
	hdr->rw_modem.bytepad[4];
	hdr->rw_modem.flags;			
					/* Bit 0 set = enable auto call out
					   Bit 1 set = call out on batteru < 12 V
					   Bit 2 set = call out on battery charge failed
					   Bit 3 set = call out on extreme temperature
					   Bit 4 set = call out on event
					   Bit 5 set = call out on GPS lock failure */
	hdr->rw_modem.calloutMsg[46+1];
} 



int main (int argc, char **argv) {
	int             c,
                    errflg = 0;
    Dbptr      db, tr;
    char       *database,*outfile, *pfname=0;
    Flags	   flags ;
    Pf          *pf,*pf_def,*pf_sta=NULL,*pf_stas;
    Tbl		   *input;
    char	   *start, *stop, *triggertimestring=NULL ;
    char	   *subset=0 ;
    long	    nrecords,nsamp1 ;
	int 		duration,nframes,sampsperframe;
	double		tstart, tstop, triggertime=0;
	char 		sta[STA_LEN],sta1[STA_LEN];
	char 		chan[STA_LEN],chan1[STA_LEN];
	double 		samprate,samprate1,t1,t2;
	FILE 			*fout;
	size_t			nw;
	unsigned char *hdrbuf=malloc(2040);
	unsigned char *framebuf=malloc(32);
	unsigned char *tagbuf=malloc(16);
	unsigned short cksum;
	unsigned char wfarr[3 * 12 * 100]; /*1000sps * 0.1 sec * 12 chan * 3bytes*/
	int sampindex=0;

	K2EvtFile *myhdr=malloc(2040);
	K2EvtTag *mytag=malloc(sizeof(K2EvtTag));
	K2EvtFrame *myframe=malloc(sizeof(K2EvtFrame));
	float *data[12] ;
	unsigned short channelBitmap=0;
	int	pf_serialno,pf_sensortype;
	double pf_sensitivity, pf_fullscale, pf_lat, pf_lon, pf_elev;

    memset ( &flags, 0, sizeof(flags)); 
    elog_init ( argc, argv ) ; 


    while ((c = getopt (argc, argv, "do:p:s:t:v")) != -1) {
		switch (c) {

			case 'd':
				flags.display = 1 ; 
				break ; 

			case 'p':
				pfname= optarg ;
				break;

			case 's':
				subset = optarg ; 
				break ;

			case 't':
				triggertimestring = optarg ; 
				break ;

			case 'v':
				flags.verbose++ ;
				break;

			default:
				errflg++;
				break ;
		}
    }

    if (errflg || argc-optind != 4)
		usage ();

	if (pfname==NULL) {
		pfname=Program_Name;
	}
    if (pfread (pfname, &pf) != 0)
		die (0, "Can't read parameter file %s\n",pfname);

	database = argv[optind++];
	outfile = argv[optind++];
	start = argv[optind++];
	stop = argv[optind++];
	tstart=str2epoch(start);
	tstop=str2epoch(stop);
	tstart=ceil(tstart);	/* make sure we have second boundaries, 
							   this also makes sure we have a integer number of frames */
	tstop=floor(tstop);
	duration=tstop - tstart;
    if ( dbopen(database, "r", &db) ) { 
        die ( 0, "Can't open database %s", database ) ;
    }

	input = pfget_tbl (pf, "view" ) ;
  	db = dbprocess ( db, input, 0 ) ;

	if (subset) {
		db=dbsubset(db,subset,0);
	}

    tr = dbinvalid() ;
    if ( trload_css ( db, start, stop, &tr, 0, 0 ) ) { 
		die ( 1, "trload_css failed" ) ; 
    }

    if ( trsplit(tr, 0, 0) ) { 
		complain ( 0, "trsplit failed" ) ; 
    }

    if ( trsplice(tr, 0, 0, 0) ) { 
		complain ( 0, "trsplice failed" ) ; 
    }

    if ( flags.display ) { 
		trdisp (tr, "will try to convert this stuff to evt") ;
    }
	tr.record=0;
	dbgetv(tr,0,"time",&t1,"endtime",&t2,"sta",sta1,"chan",chan1,"samprate",&samprate1,"nsamp",&nsamp1,NULL);
	samprate1=round(samprate1);
	

	dbquery ( tr, dbRECORD_COUNT, &nrecords ) ; 
	if (nrecords > 12) {
		printf("will only use the first 12 channels, consider modifying the subset...");
		nrecords=12;
	}
	for ( tr.record = 0 ; tr.record < nrecords ; tr.record++ ) { 
		double ts,te;
		dbgetv(tr,0,"time",&ts,"endtime",&te,"samprate",&samprate,"sta",sta,"chan",chan,NULL);
		samprate=round(samprate);
		if (t1 != ts || t2 != te) {
			die ( 0, "this simplistic version only works with 101% correct times and/or subsets, sorry..." ) ;
		}	
		if (strcmp(sta,sta1) !=0) {
			die ( 0, "%s <=> %s this simplistic version only works with ONE station, but the subset left more",sta,sta1 ) ;
		}
		if (samprate != samprate1) {
			die ( 0, "all channels in a K2-EVT File MUST have the same samplerate!" );
		}
	}

	newFileHeader(&myhdr, pf, sta);

	if (parse_param (pf, "sta_defaults", P_ARRPF, 1, &pf_def) < 0) {
		elog_die (0, "error parsing array sta_defaults.\n");
	}
	if (parse_param (pf, "sta_params", P_ARRPF, 1, &pf_stas) < 0) {
		elog_die (0, "error parsing array sta_params.\n");
	}
	if (parse_param (pf_stas, sta, P_ARRPF, 0, &pf_sta) < 0) {
		elog_die (0, "error parsing sta_params{%s}\n.",sta);
	}

	if (parse_param(pf_def,"sensortype",P_LINT,1,&pf_sensortype)< 0 ) {
		        elog_die (0, "error parsing default sensortype.\n");
	}
	if (parse_param(pf_def,"serialno",P_LINT,1,&pf_serialno)< 0 ) {
		        elog_die (0, "error parsing default serialno.\n");
	}
	if (parse_param(pf_def,"sensitivity",P_DBL,1,&pf_sensitivity)< 0 ) {
		        elog_die (0, "error parsing default sensitivity.\n");
	}
	if (parse_param(pf_def,"fullscale",P_DBL,1,&pf_fullscale)< 0 ) {
		        elog_die (0, "error parsing default fullscale.\n");
	}
	if (parse_param(pf_def,"lat",P_DBL,1,&pf_lat)< 0 ) {
		        elog_die (0, "error parsing default lat.\n");
	}
	if (parse_param(pf_def,"lon",P_DBL,1,&pf_lon)< 0 ) {
		        elog_die (0, "error parsing default lon.\n");
	}
	if (parse_param(pf_def,"elev",P_DBL,1,&pf_elev)< 0 ) {
		        elog_die (0, "error parsing default elev.\n");
	}
	if (pf_sta==NULL) {
		elog_notify (0, "can't parse array sta_params{%s}, will use defaults\n.",sta);
		pf_sta=pf_def;

	} else {
		if (parse_param(pf_def,"sensortype",P_LINT,0,&pf_sensortype)< 0 ) {
					elog_die (0, "error parsing sensortype.\n");
		}
		if (parse_param(pf_sta,"serialno",P_LINT,0,&pf_serialno)< 0 ) {
					elog_die (0, "error parsing serialno.\n");
		}
		if (parse_param(pf_sta,"sensitivity",P_DBL,0,&pf_sensitivity)< 0 ) {
					elog_die (0, "error parsing sensitivity.\n");
		}
		if (parse_param(pf_sta,"fullscale",P_DBL,0,&pf_fullscale)< 0 ) {
					elog_die (0, "error parsing fullscale.\n");
		}
		if (parse_param(pf_sta,"lat",P_DBL,0,&pf_lat)< 0 ) {
					elog_die (0, "error parsing lat.\n");
		}
		if (parse_param(pf_sta,"lon",P_DBL,0,&pf_lon)< 0 ) {
					elog_die (0, "error parsing lon.\n");
		}
		if (parse_param(pf_sta,"elev",P_DBL,0,&pf_elev)< 0 ) {
					elog_die (0, "error parsing elev.\n");
		}
	}
	myhdr->rw_stream.sps=samprate;		/* sampling rate */
	myhdr->rw_stream.preEvent=1;	/* in seconds */
	myhdr->rw_stream.postEvent=5;	/* in seconds */
	myhdr->rw_stream.minRunTime=6;	/* in seconds */
	myhdr->ro_stream.duration=duration;
	myhdr->ro_stream.nscans=duration* 10;
	myhdr->ro_stream.startTime=epoch2k2time(t1);
	myhdr->ro_stream.startTimeMsec=0;
	int ttms=0;
	if (triggertimestring) {
		triggertime=str2epoch(triggertimestring);
		ttms=remainder(triggertime,1.0 ) * 1000.0; 
		triggertime=epoch2k2time(triggertime);
	} else {
		triggertime=epoch2k2time(t1)+5;
		ttms=0;
	}
	myhdr->ro_stream.triggerTime=triggertime;
	myhdr->ro_stream.triggerTimeMsec=ttms;
	myhdr->rw_misc.nchannels=nrecords;
	for (int channo=0;channo< nrecords;channo++) {
		myhdr->rw_channel[channo].sensitivity=pf_sensitivity;	
		myhdr->rw_channel[channo].sensitivity=pf_fullscale;	
	}
	myhdr->rw_misc.elevation=pf_elev;		/* meters above sea level */
	myhdr->rw_misc.latitude=pf_lat;			/* degrees north */
	myhdr->rw_misc.longitude=pf_lon;		/* degrees east */

	nframes=duration * 10;
	myhdr->ro_stream.duration=nframes;
	myhdr->ro_stream.nscans=duration*samprate;

	channelBitmap=chanbitmap(nrecords);

	for ( tr.record = 0 ; tr.record < nrecords ; tr.record++ ) { 
		unsigned long i ; 
		int nsamp;
		char chan[7];
		unsigned long minoffset=0;
		unsigned long maxoffset=0;
		int	max=INT_MIN;
		int min=INT_MAX;
	    dbgetv(tr, 0, "chan",chan,"data", &data[tr.record], "nsamp", &nsamp, NULL ) ; 
	    for ( i=0 ; i<nsamp ; i++ ) { 
			if (data[tr.record][i] > max) {
				max=data[tr.record][i];
				maxoffset=i;
			}
			if (data[tr.record][i] < min) {
				min=data[tr.record][i];
				minoffset=i;
			}
			
	    }
		myhdr->ro_channel[tr.record].maxPeak=max;
		myhdr->ro_channel[tr.record].maxPeakOffset=maxoffset;
		myhdr->ro_channel[tr.record].minPeak=min;
		myhdr->ro_channel[tr.record].minPeakOffset=minoffset;
		memcpy(myhdr->rw_channel[tr.record].id,chan,K2EVT_CHANNEL_ID_LENGTH);
	}
	for ( tr.record = 0 ; tr.record < nrecords ; tr.record++ ) { 

	}

	newTag(&mytag,0, 2040, 0, 128,1234);
	encodek2header(hdrbuf,myhdr);
	cksum=k2_checksum(hdrbuf,2040);
	
	mytag->length=2040;/*header size*/
	mytag->dataLength=0;
	mytag->checksum=cksum;
	encodek2tag(tagbuf,mytag);
	fout=fopen(outfile,"w+");
	nw=fwrite(tagbuf,16,1,fout);
	nw=fwrite(hdrbuf,2040,1,fout);

	
	double t=t1 ;
	double k2t=round(epoch2k2time(t));
	newFrameHeader(&myframe,128,(sampsperframe*3*nrecords)+32, k2t);
	sampsperframe=TIME2SAMP(0,samprate,0.1);/*samples per frame*/
	
	for (int fn=0;fn < nframes;fn++) {
		cksum=0;
		sampindex=0;
		for (int s=0;s<sampsperframe;s++) {
			long val;
			unsigned char buf[4];
			for ( int channo = 0 ; channo < nrecords ; channo++ ) { 
				int index=((fn*sampsperframe) + s);
				val=round(data[channo][index]);
				/*debugging...*/
				//val=66051;/*0x010203*/
				memcpy(&buf,&val,4);
				val <<=8;
#ifdef WORDS_BIGENDIAN
				cksum+=buf[1];
				cksum+=buf[2];
				cksum+=buf[3];
				wfarr[sampindex++]=buf[1];
				wfarr[sampindex++]=buf[2];
				wfarr[sampindex++]=buf[3];
#else
				cksum+=buf[2];
				cksum+=buf[1];
				cksum+=buf[0];
				wfarr[sampindex++]=buf[2];
				wfarr[sampindex++]=buf[1];
				wfarr[sampindex++]=buf[0];
#endif
			}
		}

		mytag->checksum=cksum;
		mytag->length=32;/*header size*/
		mytag->dataLength=sampsperframe * nrecords * 3;
		myframe->frameSize=(sampsperframe*3*nrecords)+32;
		myframe->blockTime=round(k2t);
		myframe->msec=fmod(k2t,1.0) * 1000;
		myframe->channelBitMap=channelBitmap;
		
		encodek2Frame(framebuf,myframe);
		mytag->checksum+=k2_checksum(framebuf,32);
		encodek2tag(tagbuf,mytag);
		k2t+=0.1;
		//k2t=round((k2t+0.1)*10.0)/10.0;
		nw=fwrite(tagbuf,1,16,fout);
		if (nw!= 16) {
			die(0,"could not write file tag, wrote %d bytes instead of 16",nw);
		}
		nw=fwrite(framebuf,32,1,fout);
		nw=fwrite(&wfarr, 1,sampsperframe * 3 * nrecords, fout);
	}
	
	
	fclose(fout);

    tr.table = dbALL ;
    trfree(tr) ;
    pffree(pf) ; 
    return 0;
}

