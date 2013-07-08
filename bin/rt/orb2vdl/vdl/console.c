/*********************** CONSOLE *********************************
*
*	calling Sequence :	console()		No arguments
*
*	Programmed by : D.C. Ketchum
*	Created :		Apr 1991
*	Date Modified Description of Mod
*	20-apr-92		Base Functionality
**********************************************************************/
void console()
{
/*
		Handle command that are typed in by user.  This are one character
		commands preceded by a validating tab
*/
	extern int gbldoy;
	extern short detect_seq;
	extern int expbias;
	extern int trigfreq[16];
	extern double sntrig;
	extern struct chan_desc *ch;
	extern int station,net,node,leap,leapnext;
	extern int trigforce;
	extern int showdet;
	extern sequence;
	extern ch_lowtrg,ch_hightrg;		/* Trigger chan and range */
	extern recenter_count;					/* The recenter count */
	int yr,day,hr,mn,sec,ms;				/* temps for time code */
	int next,last,err,i,j,ip;
	int calmask;
	static int armed;
	long min,max;
	unsigned char ttbuf[100];
	unsigned char cmdchr;
	extern unsigned char cmdchrsave;	/* place to save cmdchr */
	extern int dbgmem,dbgmem2;			/* flags for dbg output from mem stuff*/
	extern int linepnt,linemode;		/* the line buffer pointer /mode flgs*/
	extern unsigned char linebuf[];		/* connect to line buffer */
	extern int pathmass;				/* used by Mass recenter command */
	extern int ranflg;
	extern double ranmag,ranfreq;
	extern int derive_lp,continuous,tcp_comm,bb_delay,lp_delay;
	int mask;
	cmdchr=0;
	err=read(0,&cmdchr,1);				/* get the character */
#ifdef SOLARIS2
	if(err == 0) return;
#endif
	if(err == -1 && errno == EWOULDBLOCK) return;		/* nothing to input */
	if(err !=1) {
		fprintf(logout,"Its not one err=%d cmdchr=%d errno=%d\n",err,cmdchr,errno);
		perror("Its not one ");
		return;
	}
	fprintf(logout,"Input char =%x %c\n",cmdchr,cmdchr);
	cmdchr=toupper((char) cmdchr);		/* convert to upper case */
	if(linemode && cmdchr > 0) {		/* are we in line mode */
		if((cmdchr == 127 || cmdchr == 8) && linepnt > 0) {	/* delete char */
			linepnt--;
			linebuf[linepnt]=0;			/* indicate end of string*/
			return;
		}
		if(cmdchr == 21) {				/* control U */
			linepnt=0;					/* no chars in string */
			linebuf[0]=0;
			fprintf(logout,"^U - Line is purged\n");
			return;
		}
		if(cmdchr == 20) {				/* control t output to this point */
			linebuf[linepnt]=0;
			if(linepnt > 0) fprintf(logout,"%s\n",linebuf);
			return;
		}
		if(cmdchr == 13) {				/* carriage return */
			cmdchr=cmdchrsave;			/* restore to command */
			fprintf(logout,"%s\n",linebuf);
			armed=1;
			linepnt=0;					/* indicate new line comming */
		} else {
			linebuf[linepnt]=cmdchr;	/* store the character */
			linepnt++;					/* another one is in */
			linebuf[linepnt]=0;			/* store zero to terminate string*/
			return;
		}
	}
	if(cmdchr == 36 || cmdchr == 9) { /* Is it escape to arm us */
		armed=1;					/* yes, arm us and leave*/
		return;
	}
	else {							/* No, If we are not armed leave */
		if(armed == 0) 	return;		/* not armed, discard character */
	}
	switch (cmdchr)
	{
		case '?':
fprintf(logout,"@,A = Print Ring, Trig, Compression variables for chans\n");
fprintf(logout,"M = Mass Recenter\nC = Calibrate 1 character ('H'ighgain or 'L'owgain)\nU = Kill Triggering\n");
fprintf(logout,"W = Force New Warmup Cycle\nS = Status List Misc variables\n");
fprintf(logout,"T = Summary of Last Second's data (min,max,diff, 7 points)\n");
fprintf(logout,"D = Toggle Debug Flag\nV = Call CLKXFER\nX = ReXmit Detections\n");
fprintf(logout,"P = Toggle POW display Flag\nH = Force High frequency Trigger\n");
fprintf(logout,"F = Force Normal Trigger\nL = Force SM Trigger\nB = Force out of Sequence\n");
fprintf(logout,"E = Print Event information\nQ = Tick Memory debug flgs\n");
fprintf(logout,"^c = stop\n+ = Tweek time +10 counts\n");
			break;
		case 3:
			fprintf(logout,"Control c received ...\n");
			exit(-1);
			break;
		case 'A':
			fprintf(logout,"  ID  NS  Trigdown  Trgdead  Trgtot  CN sch seq doy cmptot cmpstr prt\n");
			for (i=0; i<=MAX_CH; i++) 
				fprintf(logout,"%4s %2d %10d %8d %8d %2d %3d %3d %3d %8d %4d %2d\n",
				ch[i].txt,ch[i].nsamp_qd,
				ch[i].trigdown,ch[i].trigdead,
				ch[i].trigtotal,ch[i].continuous,ch[i].stat_chan,ch[i].seq,
				ch[i].doy,ch[i].cmptotal,ch[i].cmpstart,ch[i].partial);
			break;
		case 'D':
			dbg=(dbg+1)%2;
			if(dbg == 1) fprintf(logout,"Debug on\n");
			else fprintf(logout,"Debug off\n");
			break;
		case 'F':
			fprintf(logout,"Trigger force next time ...\n");
			trigforce=1;
			break;
		case 'P':
			showdet= (showdet+1) % 2;
			if(showdet) fprintf(logout,"POW debug on...\n");
			else fprintf(logout,"POW debug off...\n");
			break;
		case 'Q':
			if(dbgmem) {
				if(dbgmem2) {
					dbgmem=0; dbgmem2=0;}
				else dbgmem2=1;
			} else {
				if(dbgmem2) {
					dbgmem2=0; dbgmem=1;
				} else 	dbgmem2=1;
			}
			fprintf(logout,"DBGMEM=%d, DBGMEM2=%d\n",dbgmem,dbgmem2);
			break;
		case 'S':						/* General status print out */
			fprintf(logout,"Bias=%d\n",expbias);
			fprintf(logout,"Trig ncoin=%d nfreq=%d warmup=%d sntrig=%6.2f trgrd=%d \n",
				ncoin,nfreq,ch[0].warmup,sntrig,triggered);
			fprintf(logout,"Wind ");
			for (i=0; i<16; i++) fprintf(logout,"%3d ",trigfreq[i]);
			fprintf(logout,"\nTrglow=%d Trghi=%d\n",
				ch_lowtrg,ch_hightrg);
			fprintf(logout,"Detect seq=%d Station=%d Net=%d Node=%d leap=%d leapnext=%d\n",
				detect_seq,station,net,node,leap,leapnext);
			fprintf(logout,"LP derive=%d continuous=%d tcp=%d BB delay=%d LP delay=%d\n",
				derive_lp,continuous,tcp_comm,bb_delay,lp_delay);
			break;
		case 'T':
			for (i=0;i<6; i++) {
				fprintf(logout,"ch=%d %s\n",i,ch[i].txt);
				for(j=0; j<20; j++) {
					fprintf(logout,"%7d ",*(ch[i].ring+j));
					if( (j % 10) == 9) fprintf(logout,"\n");
				}
			}
			break;
		case 'U':
			for(j=0; j<MAX_CH; j++) if(ch[j].trig_chan > 0) ch[j].warmup=10000;
			fprintf(logout,"Triggering killed ...\n");
			break;
		case 'W':						/* rewarmup */
			for(j=0; j<MAX_CH; j++) if(ch[j].trig_chan > 0) ch[j].warmup=11;
			fprintf(logout,"Force Rewarmup Executed...\n");
			init_trig();			/* force channels to starting state*/
			break;
		case '>':
			ranfreq=ranfreq*2.;
			fprintf(logout,"Freq= %8.2f\n",ranfreq);			
			break;
		case '<':
			ranfreq=ranfreq/2.;
			fprintf(logout,"Freq=%8.2f\n",ranfreq);			
			break;
		case '~':
			ranflg++;
			ranflg=ranflg % 2;
			fprintf(logout,"ranflg=%d\n",ranflg);
			break;
		case '^':
			ranmag*=10.;
			fprintf(logout,"Mag=%8.2f\n",ranmag);
			break;
		case 'V':
			ranmag/=10.;
			fprintf(logout,"Mag=%8.2f\n",ranmag);
			break;
		default:
			if(dbg) fprintf(logout,"Cmd unknown=%c",cmdchr);
			break;
	}
	armed=0;					/* mark Not armed */
}								/* End of console handler */
