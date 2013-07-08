# 1 "dbmaprelease_mac.F"
      real*4 xlat1, xlon1, xlat2, xlon2, xmb, ymb
      real*8 evtime
      character*80 dbnam, sta, arg, auth(20), sym(20)
      character*80 tstart, tend, atext, title, custom_title
      character*80 lblfile(20),bp_lbl_file, temp
      character*512 filename_in, special_files
      real*4 symhue(20), symlit(20), symsat(20)
      parameter  (MAXEVENTS = 100000)
      integer*8 ievs(MAXEVENTS)
      integer*4 dep1,dep2,dep3
      character*1 ans
      external setans
      common /setanss/ ans
c
# 1 "/opt/antelope/5.2-64/include/EV_f.i" 1 

	integer	EV_NULL
        parameter (EV_NULL = (0))
	integer	EV_NAME
        parameter (EV_NAME = (1))
	integer	EV_NHYPOS
        parameter (EV_NHYPOS = (2))
	integer	EV_PREFHYPO
        parameter (EV_PREFHYPO = (3))
	integer	EV_HYPO_TIME
        parameter (EV_HYPO_TIME = (4))
	integer	EV_HYPO_LAT
        parameter (EV_HYPO_LAT = (5))
	integer	EV_HYPO_LON
        parameter (EV_HYPO_LON = (6))
	integer	EV_HYPO_DEPTH
        parameter (EV_HYPO_DEPTH = (7))
	integer	EV_HYPO_MB
        parameter (EV_HYPO_MB = (8))
	integer	EV_HYPO_MS
        parameter (EV_HYPO_MS = (9))
	integer	EV_HYPO_ML
        parameter (EV_HYPO_ML = (10))
	integer	EV_HYPO_AUTH
        parameter (EV_HYPO_AUTH = (11))
	integer	EV_HYPO_NSTAS
        parameter (EV_HYPO_NSTAS = (12))
	integer	EV_HYPO_ASSOCFLAG
        parameter (EV_HYPO_ASSOCFLAG = (13))
	integer	EV_HYPO_STA_STA
        parameter (EV_HYPO_STA_STA = (14))
	integer	EV_HYPO_STA_LAT
        parameter (EV_HYPO_STA_LAT = (15))
	integer	EV_HYPO_STA_LON
        parameter (EV_HYPO_STA_LON = (16))
	integer	EV_HYPO_STA_ELEV
        parameter (EV_HYPO_STA_ELEV = (17))
	integer	EV_HYPO_STA_NASSOCS
        parameter (EV_HYPO_STA_NASSOCS = (50))
	integer	EV_HYPO_STA_ASSOC_CHAN
        parameter (EV_HYPO_STA_ASSOC_CHAN = (18))
	integer	EV_HYPO_STA_ASSOC_PHASE
        parameter (EV_HYPO_STA_ASSOC_PHASE = (19))
	integer	EV_HYPO_STA_ASSOC_IPHASE
        parameter (EV_HYPO_STA_ASSOC_IPHASE = (20))
	integer	EV_HYPO_STA_ASSOC_TIME
        parameter (EV_HYPO_STA_ASSOC_TIME = (21))
	integer	EV_HYPO_STA_ASSOC_AZIMUTH
        parameter (EV_HYPO_STA_ASSOC_AZIMUTH = (22))
	integer	EV_HYPO_STA_ASSOC_INC
        parameter (EV_HYPO_STA_ASSOC_INC = (23))
	integer	EV_HYPO_STA_ASSOC_RECT
        parameter (EV_HYPO_STA_ASSOC_RECT = (24))
	integer	EV_HYPO_STA_ASSOC_DELTIME
        parameter (EV_HYPO_STA_ASSOC_DELTIME = (25))
	integer	EV_HYPO_STA_ASSOC_ASSOCFLAG
        parameter (EV_HYPO_STA_ASSOC_ASSOCFLAG = (26))
	integer	EV_HYPO_STA_ASSOC_DISTANCE
        parameter (EV_HYPO_STA_ASSOC_DISTANCE = (27))
	integer	EV_HYPO_STA_ASSOC_TIMERES
        parameter (EV_HYPO_STA_ASSOC_TIMERES = (28))
	integer	EV_HYPO_STA_ASSOC_SHAZ
        parameter (EV_HYPO_STA_ASSOC_SHAZ = (29))
	integer	EV_HYPO_STA_ASSOC_HSAZ
        parameter (EV_HYPO_STA_ASSOC_HSAZ = (30))
	integer	EV_HYPO_STA_ASSOC_AZRES
        parameter (EV_HYPO_STA_ASSOC_AZRES = (31))
	integer	EV_HYPO_STA_ASSOC_TIMEWGT
        parameter (EV_HYPO_STA_ASSOC_TIMEWGT = (32))
	integer	EV_HYPO_STA_ASSOC_AZWGT
        parameter (EV_HYPO_STA_ASSOC_AZWGT = (33))
	integer	EV_DBL
        parameter (EV_DBL = (34))
	integer	EV_TUPLE
        parameter (EV_TUPLE = (35))
	integer	EV_EVID
        parameter (EV_EVID = (36))
	integer	EV_HYPO_TUPLE
        parameter (EV_HYPO_TUPLE = (37))
	integer	EV_HYPO_ORID
        parameter (EV_HYPO_ORID = (38))
	integer	EV_HYPO_STA_ASSOC_ARID
        parameter (EV_HYPO_STA_ASSOC_ARID = (39))
	integer	EV_HYPO_STA_ASSOC_ASTUPLE
        parameter (EV_HYPO_STA_ASSOC_ASTUPLE = (40))
	integer	EV_HYPO_STA_ASSOC_ARTUPLE
        parameter (EV_HYPO_STA_ASSOC_ARTUPLE = (41))

	integer	EVSV_NULL
        parameter (EVSV_NULL = (0))
	integer	EVSV_HYPO_TIME
        parameter (EVSV_HYPO_TIME = (4))
	integer	EVSV_HYPO_LAT
        parameter (EVSV_HYPO_LAT = (5))
	integer	EVSV_HYPO_LON
        parameter (EVSV_HYPO_LON = (6))
	integer	EVSV_HYPO_DEPTH
        parameter (EVSV_HYPO_DEPTH = (7))
	integer	EVSV_HYPO_MB
        parameter (EVSV_HYPO_MB = (8))
	integer	EVSV_HYPO_MS
        parameter (EVSV_HYPO_MS = (9))
	integer	EVSV_HYPO_ML
        parameter (EVSV_HYPO_ML = (10))
	integer	EVSV_HYPO_AUTH
        parameter (EVSV_HYPO_AUTH = (11))
	integer	EVSV_HYPO_NSTAS
        parameter (EVSV_HYPO_NSTAS = (12))
	integer	EVSV_HYPO_ASSOCFLAG
        parameter (EVSV_HYPO_ASSOCFLAG = (13))
	integer	EVSV_HYPO_PREF
        parameter (EVSV_HYPO_PREF = (42))
	integer	EVSV_STA_STA
        parameter (EVSV_STA_STA = (14))
	integer	EVSV_STA_LAT
        parameter (EVSV_STA_LAT = (15))
	integer	EVSV_STA_LON
        parameter (EVSV_STA_LON = (16))
	integer	EVSV_STA_ELEV
        parameter (EVSV_STA_ELEV = (17))
        integer EVSV_STA_NSCVS
        parameter (EVSV_STA_NSCVS = (60))
        integer EVSV_STA_SCVS
        parameter (EVSV_STA_SCVS = (61))
	integer	EVSV_STA_NASSOCS
        parameter (EVSV_STA_NASSOCS = (50))
	integer	EVSV_STA_ASSOC_CHAN
        parameter (EVSV_STA_ASSOC_CHAN = (18))
	integer	EVSV_STA_ASSOC_PHASE
        parameter (EVSV_STA_ASSOC_PHASE = (19))
	integer	EVSV_STA_ASSOC_IPHASE
        parameter (EVSV_STA_ASSOC_IPHASE = (20))
	integer	EVSV_STA_ASSOC_TIME
        parameter (EVSV_STA_ASSOC_TIME = (21))
	integer	EVSV_STA_ASSOC_AZIMUTH
        parameter (EVSV_STA_ASSOC_AZIMUTH = (22))
	integer	EVSV_STA_ASSOC_INC
        parameter (EVSV_STA_ASSOC_INC = (23))
	integer	EVSV_STA_ASSOC_RECT
        parameter (EVSV_STA_ASSOC_RECT = (24))
	integer	EVSV_STA_ASSOC_DELTIME
        parameter (EVSV_STA_ASSOC_DELTIME = (25))
	integer	EVSV_STA_ASSOC_ASSOCFLAG
        parameter (EVSV_STA_ASSOC_ASSOCFLAG = (26))
	integer	EVSV_STA_ASSOC_DISTANCE
        parameter (EVSV_STA_ASSOC_DISTANCE = (27))
	integer	EVSV_STA_ASSOC_TIMERES
        parameter (EVSV_STA_ASSOC_TIMERES = (28))
	integer	EVSV_STA_ASSOC_SHAZ
        parameter (EVSV_STA_ASSOC_SHAZ = (29))
	integer	EVSV_STA_ASSOC_HSAZ
        parameter (EVSV_STA_ASSOC_HSAZ = (30))
	integer	EVSV_STA_ASSOC_AZRES
        parameter (EVSV_STA_ASSOC_AZRES = (31))
	integer	EVSV_STA_ASSOC_TIMEWGT
        parameter (EVSV_STA_ASSOC_TIMEWGT = (32))
	integer	EVSV_STA_ASSOC_AZWGT
        parameter (EVSV_STA_ASSOC_AZWGT = (33))
	integer	EVSV_DBL
        parameter (EVSV_DBL = (34))
	integer	EVSV_HYPO_TUPLE
        parameter (EVSV_HYPO_TUPLE = (37))
	integer	EVSV_HYPO_ORID
        parameter (EVSV_HYPO_ORID = (38))
	integer	EVSV_STA_ASSOC_ARID
        parameter (EVSV_STA_ASSOC_ARID = (39))
	integer	EVSV_STA_ASSOC_ASTUPLE
        parameter (EVSV_STA_ASSOC_ASTUPLE = (40))
	integer	EVSV_STA_ASSOC_ARTUPLE
        parameter (EVSV_STA_ASSOC_ARTUPLE = (41))


c $Id: dbmaprelease.f,v 1.11 2010-10-26 17:02:56 mitch Exp $ 
# 16 "dbmaprelease_mac.F" 2 
# 1 "/opt/antelope/5.2-64/include/dbl2.i" 1 


# 1 "/opt/antelope/5.2-64/include/db.i" 1 

      integer dbINVALID
      parameter (dbINVALID = -102)
      integer dbCOUNT
      parameter (dbCOUNT = -301)
      integer dbDATABASE_COUNT
      parameter (dbDATABASE_COUNT = -302	       )
      integer dbTABLE_COUNT
      parameter (dbTABLE_COUNT = -303	      )
      integer dbFIELD_COUNT
      parameter (dbFIELD_COUNT = -304	       )
      integer dbRECORD_COUNT
      parameter (dbRECORD_COUNT = -305	      )
      integer dbDESCRIPTION
      parameter (dbDESCRIPTION = -306)
      integer dbSCHEMA_DESCRIPTION
      parameter (dbSCHEMA_DESCRIPTION = -307    )
      integer dbDATABASE_DESCRIPTION
      parameter (dbDATABASE_DESCRIPTION = -308 )
      integer dbTABLE_DESCRIPTION
      parameter (dbTABLE_DESCRIPTION = -309   )
      integer dbFIELD_DESCRIPTION
      parameter (dbFIELD_DESCRIPTION = -310  )
      integer dbDETAIL
      parameter (dbDETAIL = -311)
      integer dbSCHEMA_DETAIL
      parameter (dbSCHEMA_DETAIL = -312	       )
      integer dbDATABASE_DETAIL
      parameter (dbDATABASE_DETAIL = -313	      )
      integer dbTABLE_DETAIL
      parameter (dbTABLE_DETAIL = -314	     )
      integer dbFIELD_DETAIL
      parameter (dbFIELD_DETAIL = -315	    )
      integer dbNAME
      parameter (dbNAME = -316)
      integer dbSCHEMA_NAME
      parameter (dbSCHEMA_NAME = -317	       )
      integer dbDATABASE_NAME
      parameter (dbDATABASE_NAME = -318	      )
      integer dbTABLE_NAME
      parameter (dbTABLE_NAME = -319	     )
      integer dbFIELD_NAME
      parameter (dbFIELD_NAME = -320	    )
      integer dbTABLE_PRESENT
      parameter (dbTABLE_PRESENT = -321)
      integer dbSIZE
      parameter (dbSIZE = -322)
      integer dbTABLE_SIZE
      parameter (dbTABLE_SIZE = -323	   )
      integer dbFIELD_SIZE
      parameter (dbFIELD_SIZE = -324	  )
      integer dbFIELD_TYPE
      parameter (dbFIELD_TYPE = -328	  )
      integer dbTABLE_FILENAME
      parameter (dbTABLE_FILENAME = -329)
      integer dbDBPATH
      parameter (dbDBPATH = -330)
      integer dbTABLE_DIRNAME
      parameter (dbTABLE_DIRNAME = -331)
      integer dbPRIMARY_KEY
      parameter (dbPRIMARY_KEY = -332)
      integer dbALTERNATE_KEY
      parameter (dbALTERNATE_KEY = -333)
      integer dbFOREIGN_KEYS
      parameter (dbFOREIGN_KEYS = -334)
      integer dbUNIQUE_ID
      parameter (dbUNIQUE_ID = -335)
      integer dbUNIQUE_ID_NAME
      parameter (dbUNIQUE_ID_NAME = -336)
      integer dbSINGLE
      parameter (dbSINGLE = -337)
      integer dbSCHEMA_DEFAULT
      parameter (dbSCHEMA_DEFAULT = -338)
      integer dbDBPATH_DEFAULT
      parameter (dbDBPATH_DEFAULT = -339)
      integer dbLOAD_DATE
      parameter (dbLOAD_DATE = -340)
      integer dbTYPE
      parameter (dbTYPE = -325)
      integer dbFORMAT
      parameter (dbFORMAT = -326)
      integer dbUNITS
      parameter (dbUNITS = -327)
      integer dbDATABASE
      parameter (dbDATABASE = -401)
      integer dbVIEW
      parameter (dbVIEW = -402)
      integer dbTABLE
      parameter (dbTABLE = -403)
      integer dbFIELD
      parameter (dbFIELD = -404)
      integer dbRECORD
      parameter (dbRECORD = -405)
      integer dbMERGE
      parameter (dbMERGE = -406)
      integer dbALL
      parameter (dbALL = -501)
      integer dbPRIMARY
      parameter (dbPRIMARY = -502)
      integer dbALTERNATE
      parameter (dbALTERNATE = -503)
      integer dbSCRATCH
      parameter (dbSCRATCH = -504)
      integer dbNULL
      parameter (dbNULL = -505)
      integer dbBOOLEAN
      parameter (dbBOOLEAN = 1)
      integer dbINTEGER
      parameter (dbINTEGER = 2)
      integer dbREAL
      parameter (dbREAL = 3)
      integer dbTIME
      parameter (dbTIME = 4)
      integer dbYEARDAY
      parameter (dbYEARDAY = 5)
      integer dbSTRING
      parameter (dbSTRING = 6)
      integer dbDATE
      parameter (dbDATE = 7)
      integer dbWAVEFORM
      parameter (dbWAVEFORM = 136)
      integer dbRESPONSE
      parameter (dbRESPONSE = 137)
      integer dbBFLOAT
      parameter (dbBFLOAT = 138)
      integer dbBDOUBLE
      parameter (dbBDOUBLE = 139)
      integer dbBSHORT
      parameter (dbBSHORT = 140)
      integer dbBINT
      parameter (dbBINT = 141)
      integer dbDBPTR
      parameter (dbDBPTR = 142)
      integer dbUNIQUE
      parameter (dbUNIQUE = 1)
      integer dbOUTER_JOIN
      parameter (dbOUTER_JOIN = 2)
      integer dbadd
      integer dbaddnull
      integer dbadd_remark
      integer dbaddv
      integer dbcompile
      integer dbcreate
      integer dbcrunch
      integer dbdelete
      integer dbdestroy
      integer dbex_compile
      integer dbex_eval
      integer dbex_free
      integer dbextfile
      integer dbfilename
      integer dbfind_join_keys
      integer dbget
      integer dbget_remark
      integer dbgetv
      integer dbis_expression
      integer dbmark
      integer dbnextid
      integer dbopen
      integer dbopen_database
      integer dbopen_table
      integer dbput
      integer dbputv
      integer dbputx
      integer dbread_view
      integer dbsave_view
      integer dbset
      integer dbtruncate
      integer dbunjoin
      integer dbwrite_view
# 4 "/opt/antelope/5.2-64/include/dbl2.i" 2 
      character *(*) ATTRID_ADATE
      character *(*) ATTRID_ALGID
      character *(*) ATTRID_ALGORITHM
      character *(*) ATTRID_AMP
      character *(*) ATTRID_ARID
      character *(*) ATTRID_ARRAY
      character *(*) ATTRID_ATTRIB
      character *(*) ATTRID_ATYPE
      character *(*) ATTRID_AUTH
      character *(*) ATTRID_AZDEF
      character *(*) ATTRID_AZIMUTH
      character *(*) ATTRID_AZRES
      character *(*) ATTRID_BAND
      character *(*) ATTRID_BANDW
      character *(*) ATTRID_BAZIM
      character *(*) ATTRID_BEAMID
      character *(*) ATTRID_BELIEF
      character *(*) ATTRID_BESTDC
      character *(*) ATTRID_BLKFAC
      character *(*) ATTRID_BMTYP
      character *(*) ATTRID_BSLOW
      character *(*) ATTRID_CALIB
      character *(*) ATTRID_CALPER
      character *(*) ATTRID_CALRATIO
      character *(*) ATTRID_CDATE
      character *(*) ATTRID_CDPERR
      character *(*) ATTRID_CFREQ
      character *(*) ATTRID_CHAN
      character *(*) ATTRID_CHANA
      character *(*) ATTRID_CHANID
      character *(*) ATTRID_CHANO
      character *(*) ATTRID_CHID
      character *(*) ATTRID_CLAERR
      character *(*) ATTRID_CLIP
      character *(*) ATTRID_CLOERR
      character *(*) ATTRID_CMPX
      character *(*) ATTRID_CODA
      character *(*) ATTRID_CODDES
      character *(*) ATTRID_CODE
      character *(*) ATTRID_COLDEP
      character *(*) ATTRID_COLDIA
      character *(*) ATTRID_COLINT
      character *(*) ATTRID_COLVOL
      character *(*) ATTRID_COMM
      character *(*) ATTRID_COMMID
      character *(*) ATTRID_COMPA
      character *(*) ATTRID_CONF
      character *(*) ATTRID_COTERR
      character *(*) ATTRID_CTYPE
      character *(*) ATTRID_CUREV
      character *(*) ATTRID_CUROR
      character *(*) ATTRID_DATATYPE
      character *(*) ATTRID_DATE
      character *(*) ATTRID_DATSW
      character *(*) ATTRID_DATTYP
      character *(*) ATTRID_DAY
      character *(*) ATTRID_DEAST
      character *(*) ATTRID_DELAZ
      character *(*) ATTRID_DELSLO
      character *(*) ATTRID_DELTA
      character *(*) ATTRID_DELTIM
      character *(*) ATTRID_DEPDP
      character *(*) ATTRID_DEPTH
      character *(*) ATTRID_DESCR
      character *(*) ATTRID_DESCRIP
      character *(*) ATTRID_DFILE
      character *(*) ATTRID_DIG
      character *(*) ATTRID_DIGITAL
      character *(*) ATTRID_DIP1
      character *(*) ATTRID_DIP2
      character *(*) ATTRID_DIR
      character *(*) ATTRID_DIST
      character *(*) ATTRID_DLID
      character *(*) ATTRID_DNORTH
      character *(*) ATTRID_DOY
      character *(*) ATTRID_DTYPE
      character *(*) ATTRID_DURAT
      character *(*) ATTRID_DUSED
      character *(*) ATTRID_EDATE
      character *(*) ATTRID_EDEPTH
      character *(*) ATTRID_ELEV
      character *(*) ATTRID_EMA
      character *(*) ATTRID_EMARES
      character *(*) ATTRID_ENDTIME
      character *(*) ATTRID_ESAZ
      character *(*) ATTRID_ETYPE
      character *(*) ATTRID_EVID
      character *(*) ATTRID_EVNAME
      character *(*) ATTRID_EXLAT
      character *(*) ATTRID_EXLON
      character *(*) ATTRID_EXPTYP
      character *(*) ATTRID_FILE
      character *(*) ATTRID_FILES
      character *(*) ATTRID_FILTID
      character *(*) ATTRID_FILTYP
      character *(*) ATTRID_FKID
      character *(*) ATTRID_FKQUAL
      character *(*) ATTRID_FKRID
      character *(*) ATTRID_FKTYP
      character *(*) ATTRID_FM
      character *(*) ATTRID_FNORM
      character *(*) ATTRID_FOFF
      character *(*) ATTRID_FSID
      character *(*) ATTRID_FSRID
      character *(*) ATTRID_FSTAT
      character *(*) ATTRID_FSTYP
      character *(*) ATTRID_FTID
      character *(*) ATTRID_GNORM
      character *(*) ATTRID_GRN
      character *(*) ATTRID_GRNAME
      character *(*) ATTRID_HANG
      character *(*) ATTRID_HICUT
      character *(*) ATTRID_HSLOPE
      character *(*) ATTRID_IDTYPE
      character *(*) ATTRID_IDVALUE
      character *(*) ATTRID_IMB
      character *(*) ATTRID_IML
      character *(*) ATTRID_IMS
      character *(*) ATTRID_INID
      character *(*) ATTRID_INSNAME
      character *(*) ATTRID_INSTANT
      character *(*) ATTRID_INSTYP
      character *(*) ATTRID_INSTYPE
      character *(*) ATTRID_INTSCL
      character *(*) ATTRID_IPHASE
      character *(*) ATTRID_JDATE
      character *(*) ATTRID_LAT
      character *(*) ATTRID_LDATE
      character *(*) ATTRID_LDDATE
      character *(*) ATTRID_LEAP
      character *(*) ATTRID_LINENO
      character *(*) ATTRID_LOCATION
      character *(*) ATTRID_LOCUT
      character *(*) ATTRID_LOGAT
      character *(*) ATTRID_LON
      character *(*) ATTRID_LSLOPE
      character *(*) ATTRID_LTYPE
      character *(*) ATTRID_MAG
      character *(*) ATTRID_MAGB
      character *(*) ATTRID_MAGID
      character *(*) ATTRID_MAGLR
      character *(*) ATTRID_MAGNITUDE
      character *(*) ATTRID_MAGSH
      character *(*) ATTRID_MAGTYPE
      character *(*) ATTRID_MAXBLK
      character *(*) ATTRID_MAXF
      character *(*) ATTRID_MAXINT
      character *(*) ATTRID_MAXKX
      character *(*) ATTRID_MAXKY
      character *(*) ATTRID_MAXSX
      character *(*) ATTRID_MAXSY
      character *(*) ATTRID_MB
      character *(*) ATTRID_MBID
      character *(*) ATTRID_MEDIUM
      character *(*) ATTRID_MEXPON
      character *(*) ATTRID_MFF
      character *(*) ATTRID_MFFERR
      character *(*) ATTRID_MINBLK
      character *(*) ATTRID_ML
      character *(*) ATTRID_MLID
      character *(*) ATTRID_MNAME
      character *(*) ATTRID_MO
      character *(*) ATTRID_MOAUTH
      character *(*) ATTRID_MOIST
      character *(*) ATTRID_MON
      character *(*) ATTRID_MRF
      character *(*) ATTRID_MRFERR
      character *(*) ATTRID_MRR
      character *(*) ATTRID_MRRERR
      character *(*) ATTRID_MRT
      character *(*) ATTRID_MRTERR
      character *(*) ATTRID_MS
      character *(*) ATTRID_MSID
      character *(*) ATTRID_MTF
      character *(*) ATTRID_MTFERR
      character *(*) ATTRID_MTT
      character *(*) ATTRID_MTTERR
      character *(*) ATTRID_NAME
      character *(*) ATTRID_NASS
      character *(*) ATTRID_NAXAZM
      character *(*) ATTRID_NAXPLG
      character *(*) ATTRID_NAXVAL
      character *(*) ATTRID_NBYTE
      character *(*) ATTRID_NCALIB
      character *(*) ATTRID_NCALPER
      character *(*) ATTRID_NDEF
      character *(*) ATTRID_NDLID
      character *(*) ATTRID_NDP
      character *(*) ATTRID_NET
      character *(*) ATTRID_NETNAME
      character *(*) ATTRID_NETTYPE
      character *(*) ATTRID_NETWRK
      character *(*) ATTRID_NF
      character *(*) ATTRID_NMB
      character *(*) ATTRID_NMO
      character *(*) ATTRID_NMS
      character *(*) ATTRID_NORID
      character *(*) ATTRID_NOWFT
      character *(*) ATTRID_NRLPB
      character *(*) ATTRID_NRMW
      character *(*) ATTRID_NSAMP
      character *(*) ATTRID_NSLPB
      character *(*) ATTRID_NSMW
      character *(*) ATTRID_NSTA
      character *(*) ATTRID_NX
      character *(*) ATTRID_NXALG
      character *(*) ATTRID_NXARID
      character *(*) ATTRID_NXCHID
      character *(*) ATTRID_NXCOMM
      character *(*) ATTRID_NXDLID
      character *(*) ATTRID_NXEVID
      character *(*) ATTRID_NXFILT
      character *(*) ATTRID_NXFK
      character *(*) ATTRID_NXFKR
      character *(*) ATTRID_NXFS
      character *(*) ATTRID_NXFSR
      character *(*) ATTRID_NXFTID
      character *(*) ATTRID_NXINID
      character *(*) ATTRID_NXORID
      character *(*) ATTRID_NXSENS
      character *(*) ATTRID_NXSITE
      character *(*) ATTRID_NXSPRO
      character *(*) ATTRID_NXWFID
      character *(*) ATTRID_NY
      character *(*) ATTRID_OFFDAT
      character *(*) ATTRID_OFFDATE
      character *(*) ATTRID_ONDATE
      character *(*) ATTRID_OPSW
      character *(*) ATTRID_OPTYP
      character *(*) ATTRID_ORID
      character *(*) ATTRID_PALDEP
      character *(*) ATTRID_PAXAZM
      character *(*) ATTRID_PAXPLG
      character *(*) ATTRID_PAXVAL
      character *(*) ATTRID_PCHID
      character *(*) ATTRID_PDLID
      character *(*) ATTRID_PER
      character *(*) ATTRID_PHASE
      character *(*) ATTRID_PLPREF
      character *(*) ATTRID_PNAME
      character *(*) ATTRID_PORID
      character *(*) ATTRID_PREFOR
      character *(*) ATTRID_PVALUE
      character *(*) ATTRID_QUAL
      character *(*) ATTRID_RECT
      character *(*) ATTRID_REELSZ
      character *(*) ATTRID_REFSTA
      character *(*) ATTRID_REGION
      character *(*) ATTRID_REL
      character *(*) ATTRID_REMARK
      character *(*) ATTRID_RESID
      character *(*) ATTRID_RIPPLE
      character *(*) ATTRID_RSPTYP
      character *(*) ATTRID_RSPTYPE
      character *(*) ATTRID_SAMPRATE
      character *(*) ATTRID_SDDP
      character *(*) ATTRID_SDEPTH
      character *(*) ATTRID_SDMB
      character *(*) ATTRID_SDMO
      character *(*) ATTRID_SDMS
      character *(*) ATTRID_SDOBS
      character *(*) ATTRID_SDZDP
      character *(*) ATTRID_SEAZ
      character *(*) ATTRID_SEGTYP
      character *(*) ATTRID_SEGTYPE
      character *(*) ATTRID_SENSID
      character *(*) ATTRID_SITEID
      character *(*) ATTRID_SLIP1
      character *(*) ATTRID_SLIP2
      character *(*) ATTRID_SLODEF
      character *(*) ATTRID_SLORES
      character *(*) ATTRID_SLOW
      character *(*) ATTRID_SMAJAX
      character *(*) ATTRID_SMINAX
      character *(*) ATTRID_SMPRAT
      character *(*) ATTRID_SNAME
      character *(*) ATTRID_SNR
      character *(*) ATTRID_SPAUTH
      character *(*) ATTRID_SPMM
      character *(*) ATTRID_SPROID
      character *(*) ATTRID_SPRT
      character *(*) ATTRID_SPVT
      character *(*) ATTRID_SRN
      character *(*) ATTRID_SRNAME
      character *(*) ATTRID_STA
      character *(*) ATTRID_STAA
      character *(*) ATTRID_STANAM
      character *(*) ATTRID_STANAME
      character *(*) ATTRID_STAO
      character *(*) ATTRID_STASSID
      character *(*) ATTRID_STATYPE
      character *(*) ATTRID_STAV
      character *(*) ATTRID_STID
      character *(*) ATTRID_STIME
      character *(*) ATTRID_STR1
      character *(*) ATTRID_STR2
      character *(*) ATTRID_STRIKE
      character *(*) ATTRID_STRING
      character *(*) ATTRID_STT
      character *(*) ATTRID_STX
      character *(*) ATTRID_STY
      character *(*) ATTRID_STYPE
      character *(*) ATTRID_STZ
      character *(*) ATTRID_SXX
      character *(*) ATTRID_SXY
      character *(*) ATTRID_SXZ
      character *(*) ATTRID_SYY
      character *(*) ATTRID_SYZ
      character *(*) ATTRID_SZZ
      character *(*) ATTRID_TAGID
      character *(*) ATTRID_TAGNAME
      character *(*) ATTRID_TAPEBLOCK
      character *(*) ATTRID_TAPEFILE
      character *(*) ATTRID_TARNAM
      character *(*) ATTRID_TAXAZM
      character *(*) ATTRID_TAXPLG
      character *(*) ATTRID_TAXVAL
      character *(*) ATTRID_TCALIB
      character *(*) ATTRID_TDENSE
      character *(*) ATTRID_TEXT
      character *(*) ATTRID_TFILE
      character *(*) ATTRID_TIME
      character *(*) ATTRID_TIMEDEF
      character *(*) ATTRID_TIMERES
      character *(*) ATTRID_TLEN
      character *(*) ATTRID_TMFC
      character *(*) ATTRID_TMFI
      character *(*) ATTRID_TMNLPB
      character *(*) ATTRID_TMNMW
      character *(*) ATTRID_TPBLCK
      character *(*) ATTRID_TPFILE
      character *(*) ATTRID_TPTYPE
      character *(*) ATTRID_TRATBL
      character *(*) ATTRID_TSHIFT
      character *(*) ATTRID_TSITE
      character *(*) ATTRID_TUPID
      character *(*) ATTRID_UNCERTAINTY
      character *(*) ATTRID_USEDFT
      character *(*) ATTRID_VANG
      character *(*) ATTRID_VELID
      character *(*) ATTRID_VMODEL
      character *(*) ATTRID_VOLNAM
      character *(*) ATTRID_VOLNAME
      character *(*) ATTRID_WATDEP
      character *(*) ATTRID_WFID
      character *(*) ATTRID_WGT
      character *(*) ATTRID_YEAR
      character *(*) ATTRID_YIELD
      character *(*) ATTRID_YLDMAX
      character *(*) RELID_AFFILIATION
      character *(*) RELID_ALIAS
      character *(*) RELID_ARRIVAL
      character *(*) RELID_ASSOC
      character *(*) RELID_BEAM
      character *(*) RELID_CENTRYD
      character *(*) RELID_CHANNEL
      character *(*) RELID_CHOPER
      character *(*) RELID_CODE
      character *(*) RELID_COMMENT
      character *(*) RELID_COUNTER
      character *(*) RELID_DATE
      character *(*) RELID_DAY
      character *(*) RELID_DETECTION
      character *(*) RELID_DETLOC
      character *(*) RELID_EVENT
      character *(*) RELID_EVWF
      character *(*) RELID_EXPLO
      character *(*) RELID_EXTRA
      character *(*) RELID_FEATURE
      character *(*) RELID_FILTER
      character *(*) RELID_FKDISC
      character *(*) RELID_FKREC
      character *(*) RELID_FPLANE
      character *(*) RELID_FSDISC
      character *(*) RELID_FSREC
      character *(*) RELID_GREGION
      character *(*) RELID_INSTRUMENT
      character *(*) RELID_LASTID
      character *(*) RELID_LOC
      character *(*) RELID_MOMENT
      character *(*) RELID_NETMAG
      character *(*) RELID_NETWORK
      character *(*) RELID_ORIGERR
      character *(*) RELID_ORIGIN
      character *(*) RELID_REMARK
      character *(*) RELID_SENSOR
      character *(*) RELID_SIGPRO
      character *(*) RELID_SITE
      character *(*) RELID_SITECHAN
      character *(*) RELID_SREGION
      character *(*) RELID_STALOG
      character *(*) RELID_STAMAG
      character *(*) RELID_STASSOC
      character *(*) RELID_STATION
      character *(*) RELID_TAPE
      character *(*) RELID_WFDISC
      character *(*) RELID_WFTAG
      character *(*) RELID_WFTAPE
      character *(*) RELID_WFTAR
      character *(*) RELID_XPARAM
      integer DBL_ASCII
      integer DBL_BINARY
      integer DBL_DBL
      integer DBL_FLT
      integer DBL_INT
      integer DBL_STR
      integer NUMBER_RELS30
      integer NUMBER_ATTRS
      integer NUMBER_ATTRS30
      parameter (ATTRID_ADATE = "adate"		)
      parameter (ATTRID_ALGID = "algid"		)
      parameter (ATTRID_ALGORITHM = "algorithm"	)
      parameter (ATTRID_AMP = "amp"		)
      parameter (ATTRID_ARID = "arid"		)
      parameter (ATTRID_ARRAY = "array"		)
      parameter (ATTRID_ATTRIB = "attrib"	)
      parameter (ATTRID_ATYPE = "atype"		)
      parameter (ATTRID_AUTH = "auth"		)
      parameter (ATTRID_AZDEF = "azdef"		)
      parameter (ATTRID_AZIMUTH = "azimuth"	)
      parameter (ATTRID_AZRES = "azres"		)
      parameter (ATTRID_BAND = "band"		)
      parameter (ATTRID_BANDW = "bandw"		)
      parameter (ATTRID_BAZIM = "bazim"		)
      parameter (ATTRID_BEAMID = "beamid"	)
      parameter (ATTRID_BELIEF = "belief"	)
      parameter (ATTRID_BESTDC = "bestdc"	)
      parameter (ATTRID_BLKFAC = "blkfac"	)
      parameter (ATTRID_BMTYP = "bmtyp"		)
      parameter (ATTRID_BSLOW = "bslow"		)
      parameter (ATTRID_CALIB = "calib"		)
      parameter (ATTRID_CALPER = "calper"	)
      parameter (ATTRID_CALRATIO = "calratio"	)
      parameter (ATTRID_CDATE = "cdate"		)
      parameter (ATTRID_CDPERR = "cdperr"	)
      parameter (ATTRID_CFREQ = "cfreq"		)
      parameter (ATTRID_CHAN = "chan"		)
      parameter (ATTRID_CHANA = "chana"		)
      parameter (ATTRID_CHANID = "chanid"	)
      parameter (ATTRID_CHANO = "chano"		)
      parameter (ATTRID_CHID = "chid"		)
      parameter (ATTRID_CLAERR = "claerr"	)
      parameter (ATTRID_CLIP = "clip"		)
      parameter (ATTRID_CLOERR = "cloerr"	)
      parameter (ATTRID_CMPX = "cmpx"		)
      parameter (ATTRID_CODA = "coda"		)
      parameter (ATTRID_CODDES = "coddes"	)
      parameter (ATTRID_CODE = "code"		)
      parameter (ATTRID_COLDEP = "coldep"	)
      parameter (ATTRID_COLDIA = "coldia"	)
      parameter (ATTRID_COLINT = "colint"	)
      parameter (ATTRID_COLVOL = "colvol"	)
      parameter (ATTRID_COMM = "comm"		)
      parameter (ATTRID_COMMID = "commid"	)
      parameter (ATTRID_COMPA = "compa"		)
      parameter (ATTRID_CONF = "conf"		)
      parameter (ATTRID_COTERR = "coterr"	)
      parameter (ATTRID_CTYPE = "ctype"		)
      parameter (ATTRID_CUREV = "curev"		)
      parameter (ATTRID_CUROR = "curor"		)
      parameter (ATTRID_DATATYPE = "datatype"	)
      parameter (ATTRID_DATE = "date"		)
      parameter (ATTRID_DATSW = "datsw"		)
      parameter (ATTRID_DATTYP = "dattyp"	)
      parameter (ATTRID_DAY = "day"		)
      parameter (ATTRID_DEAST = "deast"		)
      parameter (ATTRID_DELAZ = "delaz"		)
      parameter (ATTRID_DELSLO = "delslo"	)
      parameter (ATTRID_DELTA = "delta"		)
      parameter (ATTRID_DELTIM = "deltim"	)
      parameter (ATTRID_DEPDP = "depdp"		)
      parameter (ATTRID_DEPTH = "depth"		)
      parameter (ATTRID_DESCR = "descr"		)
      parameter (ATTRID_DESCRIP = "descrip"	)
      parameter (ATTRID_DFILE = "dfile"		)
      parameter (ATTRID_DIG = "dig"		)
      parameter (ATTRID_DIGITAL = "digital"	)
      parameter (ATTRID_DIP1 = "dip1"		)
      parameter (ATTRID_DIP2 = "dip2"		)
      parameter (ATTRID_DIR = "dir"		)
      parameter (ATTRID_DIST = "dist"		)
      parameter (ATTRID_DLID = "dlid"		)
      parameter (ATTRID_DNORTH = "dnorth"	)
      parameter (ATTRID_DOY = "doy"		)
      parameter (ATTRID_DTYPE = "dtype"		)
      parameter (ATTRID_DURAT = "durat"		)
      parameter (ATTRID_DUSED = "dused"		)
      parameter (ATTRID_EDATE = "edate"		)
      parameter (ATTRID_EDEPTH = "edepth"	)
      parameter (ATTRID_ELEV = "elev"		)
      parameter (ATTRID_EMA = "ema"		)
      parameter (ATTRID_EMARES = "emares"	)
      parameter (ATTRID_ENDTIME = "endtime"	)
      parameter (ATTRID_ESAZ = "esaz"		)
      parameter (ATTRID_ETYPE = "etype"		)
      parameter (ATTRID_EVID = "evid"		)
      parameter (ATTRID_EVNAME = "evname"	)
      parameter (ATTRID_EXLAT = "exlat"		)
      parameter (ATTRID_EXLON = "exlon"		)
      parameter (ATTRID_EXPTYP = "exptyp"	)
      parameter (ATTRID_FILE = "file"		)
      parameter (ATTRID_FILES = "files"		)
      parameter (ATTRID_FILTID = "filtid"	)
      parameter (ATTRID_FILTYP = "filtyp"	)
      parameter (ATTRID_FKID = "fkid"		)
      parameter (ATTRID_FKQUAL = "fkqual"	)
      parameter (ATTRID_FKRID = "fkrid"		)
      parameter (ATTRID_FKTYP = "fktyp"		)
      parameter (ATTRID_FM = "fm"		)
      parameter (ATTRID_FNORM = "fnorm"		)
      parameter (ATTRID_FOFF = "foff"		)
      parameter (ATTRID_FSID = "fsid"		)
      parameter (ATTRID_FSRID = "fsrid"		)
      parameter (ATTRID_FSTAT = "fstat"		)
      parameter (ATTRID_FSTYP = "fstyp"		)
      parameter (ATTRID_FTID = "ftid"		)
      parameter (ATTRID_GNORM = "gnorm"		)
      parameter (ATTRID_GRN = "grn"		)
      parameter (ATTRID_GRNAME = "grname"	)
      parameter (ATTRID_HANG = "hang"		)
      parameter (ATTRID_HICUT = "hicut"		)
      parameter (ATTRID_HSLOPE = "hslope"	)
      parameter (ATTRID_IDTYPE = "idtype"	)
      parameter (ATTRID_IDVALUE = "idvalue"	)
      parameter (ATTRID_IMB = "imb"		)
      parameter (ATTRID_IML = "iml"		)
      parameter (ATTRID_IMS = "ims"		)
      parameter (ATTRID_INID = "inid"		)
      parameter (ATTRID_INSNAME = "insname"	)
      parameter (ATTRID_INSTANT = "instant"	)
      parameter (ATTRID_INSTYP = "instyp"	)
      parameter (ATTRID_INSTYPE = "instype"	)
      parameter (ATTRID_INTSCL = "intscl"	)
      parameter (ATTRID_IPHASE = "iphase"	)
      parameter (ATTRID_JDATE = "jdate"		)
      parameter (ATTRID_LAT = "lat"		)
      parameter (ATTRID_LDATE = "ldate"		)
      parameter (ATTRID_LDDATE = "lddate"	)
      parameter (ATTRID_LEAP = "leap"		)
      parameter (ATTRID_LINENO = "lineno"	)
      parameter (ATTRID_LOCATION = "location"	)
      parameter (ATTRID_LOCUT = "locut"		)
      parameter (ATTRID_LOGAT = "logat"		)
      parameter (ATTRID_LON = "lon"		)
      parameter (ATTRID_LSLOPE = "lslope"	)
      parameter (ATTRID_LTYPE = "ltype"		)
      parameter (ATTRID_MAG = "mag"		)
      parameter (ATTRID_MAGB = "magb"		)
      parameter (ATTRID_MAGID = "magid"		)
      parameter (ATTRID_MAGLR = "maglr"		)
      parameter (ATTRID_MAGNITUDE = "magnitude"	)
      parameter (ATTRID_MAGSH = "magsh"		)
      parameter (ATTRID_MAGTYPE = "magtype"	)
      parameter (ATTRID_MAXBLK = "maxblk"	)
      parameter (ATTRID_MAXF = "maxf"		)
      parameter (ATTRID_MAXINT = "maxint"	)
      parameter (ATTRID_MAXKX = "maxkx"		)
      parameter (ATTRID_MAXKY = "maxky"		)
      parameter (ATTRID_MAXSX = "maxsx"		)
      parameter (ATTRID_MAXSY = "maxsy"		)
      parameter (ATTRID_MB = "mb"		)
      parameter (ATTRID_MBID = "mbid"		)
      parameter (ATTRID_MEDIUM = "medium"	)
      parameter (ATTRID_MEXPON = "mexpon"	)
      parameter (ATTRID_MFF = "mff"		)
      parameter (ATTRID_MFFERR = "mfferr"	)
      parameter (ATTRID_MINBLK = "minblk"	)
      parameter (ATTRID_ML = "ml"		)
      parameter (ATTRID_MLID = "mlid"		)
      parameter (ATTRID_MNAME = "mname"		)
      parameter (ATTRID_MO = "mo"		)
      parameter (ATTRID_MOAUTH = "moauth"	)
      parameter (ATTRID_MOIST = "moist"		)
      parameter (ATTRID_MON = "mon"		)
      parameter (ATTRID_MRF = "mrf"		)
      parameter (ATTRID_MRFERR = "mrferr"	)
      parameter (ATTRID_MRR = "mrr"		)
      parameter (ATTRID_MRRERR = "mrrerr"	)
      parameter (ATTRID_MRT = "mrt"		)
      parameter (ATTRID_MRTERR = "mrterr"	)
      parameter (ATTRID_MS = "ms"		)
      parameter (ATTRID_MSID = "msid"		)
      parameter (ATTRID_MTF = "mtf"		)
      parameter (ATTRID_MTFERR = "mtferr"	)
      parameter (ATTRID_MTT = "mtt"		)
      parameter (ATTRID_MTTERR = "mtterr"	)
      parameter (ATTRID_NAME = "name"		)
      parameter (ATTRID_NASS = "nass"		)
      parameter (ATTRID_NAXAZM = "naxazm"	)
      parameter (ATTRID_NAXPLG = "naxplg"	)
      parameter (ATTRID_NAXVAL = "naxval"	)
      parameter (ATTRID_NBYTE = "nbyte"		)
      parameter (ATTRID_NCALIB = "ncalib"	)
      parameter (ATTRID_NCALPER = "ncalper"	)
      parameter (ATTRID_NDEF = "ndef"		)
      parameter (ATTRID_NDLID = "ndlid"		)
      parameter (ATTRID_NDP = "ndp"		)
      parameter (ATTRID_NET = "net"		)
      parameter (ATTRID_NETNAME = "netname"	)
      parameter (ATTRID_NETTYPE = "nettype"	)
      parameter (ATTRID_NETWRK = "netwrk"	)
      parameter (ATTRID_NF = "nf"		)
      parameter (ATTRID_NMB = "nmb"		)
      parameter (ATTRID_NMO = "nmo"		)
      parameter (ATTRID_NMS = "nms"		)
      parameter (ATTRID_NORID = "norid"		)
      parameter (ATTRID_NOWFT = "nowft"		)
      parameter (ATTRID_NRLPB = "nrlpb"		)
      parameter (ATTRID_NRMW = "nrmw"		)
      parameter (ATTRID_NSAMP = "nsamp"		)
      parameter (ATTRID_NSLPB = "nslpb"		)
      parameter (ATTRID_NSMW = "nsmw"		)
      parameter (ATTRID_NSTA = "nsta"		)
      parameter (ATTRID_NX = "nx"		)
      parameter (ATTRID_NXALG = "nxalg"		)
      parameter (ATTRID_NXARID = "nxarid"	)
      parameter (ATTRID_NXCHID = "nxchid"	)
      parameter (ATTRID_NXCOMM = "nxcomm"	)
      parameter (ATTRID_NXDLID = "nxdlid"	)
      parameter (ATTRID_NXEVID = "nxevid"	)
      parameter (ATTRID_NXFILT = "nxfilt"	)
      parameter (ATTRID_NXFK = "nxfk"		)
      parameter (ATTRID_NXFKR = "nxfkr"		)
      parameter (ATTRID_NXFS = "nxfs"		)
      parameter (ATTRID_NXFSR = "nxfsr"		)
      parameter (ATTRID_NXFTID = "nxftid"	)
      parameter (ATTRID_NXINID = "nxinid"	)
      parameter (ATTRID_NXORID = "nxorid"	)
      parameter (ATTRID_NXSENS = "nxsens"	)
      parameter (ATTRID_NXSITE = "nxsite"	)
      parameter (ATTRID_NXSPRO = "nxspro"	)
      parameter (ATTRID_NXWFID = "nxwfid"	)
      parameter (ATTRID_NY = "ny"		)
      parameter (ATTRID_OFFDAT = "offdat"	)
      parameter (ATTRID_OFFDATE = "offdate"	)
      parameter (ATTRID_ONDATE = "ondate"	)
      parameter (ATTRID_OPSW = "opsw"		)
      parameter (ATTRID_OPTYP = "optyp"		)
      parameter (ATTRID_ORID = "orid"		)
      parameter (ATTRID_PALDEP = "paldep"	)
      parameter (ATTRID_PAXAZM = "paxazm"	)
      parameter (ATTRID_PAXPLG = "paxplg"	)
      parameter (ATTRID_PAXVAL = "paxval"	)
      parameter (ATTRID_PCHID = "pchid"		)
      parameter (ATTRID_PDLID = "pdlid"		)
      parameter (ATTRID_PER = "per"		)
      parameter (ATTRID_PHASE = "phase"		)
      parameter (ATTRID_PLPREF = "plpref"	)
      parameter (ATTRID_PNAME = "pname"		)
      parameter (ATTRID_PORID = "porid"		)
      parameter (ATTRID_PREFOR = "prefor"	)
      parameter (ATTRID_PVALUE = "pvalue"	)
      parameter (ATTRID_QUAL = "qual"		)
      parameter (ATTRID_RECT = "rect"		)
      parameter (ATTRID_REELSZ = "reelsz"	)
      parameter (ATTRID_REFSTA = "refsta"	)
      parameter (ATTRID_REGION = "region"	)
      parameter (ATTRID_REL = "rel"		)
      parameter (ATTRID_REMARK = "remark"	)
      parameter (ATTRID_RESID = "resid"		)
      parameter (ATTRID_RIPPLE = "ripple"	)
      parameter (ATTRID_RSPTYP = "rsptyp"	)
      parameter (ATTRID_RSPTYPE = "rsptype"	)
      parameter (ATTRID_SAMPRATE = "samprate"	)
      parameter (ATTRID_SDDP = "sddp"		)
      parameter (ATTRID_SDEPTH = "sdepth"	)
      parameter (ATTRID_SDMB = "sdmb"		)
      parameter (ATTRID_SDMO = "sdmo"		)
      parameter (ATTRID_SDMS = "sdms"		)
      parameter (ATTRID_SDOBS = "sdobs"		)
      parameter (ATTRID_SDZDP = "sdzdp"		)
      parameter (ATTRID_SEAZ = "seaz"		)
      parameter (ATTRID_SEGTYP = "segtyp"	)
      parameter (ATTRID_SEGTYPE = "segtype"	)
      parameter (ATTRID_SENSID = "sensid"	)
      parameter (ATTRID_SITEID = "siteid"	)
      parameter (ATTRID_SLIP1 = "slip1"		)
      parameter (ATTRID_SLIP2 = "slip2"		)
      parameter (ATTRID_SLODEF = "slodef"	)
      parameter (ATTRID_SLORES = "slores"	)
      parameter (ATTRID_SLOW = "slow"		)
      parameter (ATTRID_SMAJAX = "smajax"	)
      parameter (ATTRID_SMINAX = "sminax"	)
      parameter (ATTRID_SMPRAT = "smprat"	)
      parameter (ATTRID_SNAME = "sname"		)
      parameter (ATTRID_SNR = "snr"		)
      parameter (ATTRID_SPAUTH = "spauth"	)
      parameter (ATTRID_SPMM = "spmm"		)
      parameter (ATTRID_SPROID = "sproid"	)
      parameter (ATTRID_SPRT = "sprt"		)
      parameter (ATTRID_SPVT = "spvt"		)
      parameter (ATTRID_SRN = "srn"		)
      parameter (ATTRID_SRNAME = "srname"	)
      parameter (ATTRID_STA = "sta"		)
      parameter (ATTRID_STAA = "staa"		)
      parameter (ATTRID_STANAM = "stanam"	)
      parameter (ATTRID_STANAME = "staname"	)
      parameter (ATTRID_STAO = "stao"		)
      parameter (ATTRID_STASSID = "stassid"	)
      parameter (ATTRID_STATYPE = "statype"	)
      parameter (ATTRID_STAV = "stav"		)
      parameter (ATTRID_STID = "stid"		)
      parameter (ATTRID_STIME = "stime"		)
      parameter (ATTRID_STR1 = "str1"		)
      parameter (ATTRID_STR2 = "str2"		)
      parameter (ATTRID_STRIKE = "strike"	)
      parameter (ATTRID_STRING = "string"	)
      parameter (ATTRID_STT = "stt"		)
      parameter (ATTRID_STX = "stx"		)
      parameter (ATTRID_STY = "sty"		)
      parameter (ATTRID_STYPE = "stype"		)
      parameter (ATTRID_STZ = "stz"		)
      parameter (ATTRID_SXX = "sxx"		)
      parameter (ATTRID_SXY = "sxy"		)
      parameter (ATTRID_SXZ = "sxz"		)
      parameter (ATTRID_SYY = "syy"		)
      parameter (ATTRID_SYZ = "syz"		)
      parameter (ATTRID_SZZ = "szz"		)
      parameter (ATTRID_TAGID = "tagid"		)
      parameter (ATTRID_TAGNAME = "tagname"	)
      parameter (ATTRID_TAPEBLOCK = "tapeblock"	)
      parameter (ATTRID_TAPEFILE = "tapefile"	)
      parameter (ATTRID_TARNAM = "tarnam"	)
      parameter (ATTRID_TAXAZM = "taxazm"	)
      parameter (ATTRID_TAXPLG = "taxplg"	)
      parameter (ATTRID_TAXVAL = "taxval"	)
      parameter (ATTRID_TCALIB = "tcalib"	)
      parameter (ATTRID_TDENSE = "tdense"	)
      parameter (ATTRID_TEXT = "text"		)
      parameter (ATTRID_TFILE = "tfile"		)
      parameter (ATTRID_TIME = "time"		)
      parameter (ATTRID_TIMEDEF = "timedef"	)
      parameter (ATTRID_TIMERES = "timeres"	)
      parameter (ATTRID_TLEN = "tlen"		)
      parameter (ATTRID_TMFC = "tmfc"		)
      parameter (ATTRID_TMFI = "tmfi"		)
      parameter (ATTRID_TMNLPB = "tmnlpb"	)
      parameter (ATTRID_TMNMW = "tmnmw"		)
      parameter (ATTRID_TPBLCK = "tpblck"	)
      parameter (ATTRID_TPFILE = "tpfile"	)
      parameter (ATTRID_TPTYPE = "tptype"	)
      parameter (ATTRID_TRATBL = "tratbl"	)
      parameter (ATTRID_TSHIFT = "tshift"	)
      parameter (ATTRID_TSITE = "tsite"		)
      parameter (ATTRID_TUPID = "tupid"		)
      parameter (ATTRID_UNCERTAINTY = "uncertainty"	)
      parameter (ATTRID_USEDFT = "usedft"	)
      parameter (ATTRID_VANG = "vang"		)
      parameter (ATTRID_VELID = "velid"		)
      parameter (ATTRID_VMODEL = "vmodel"	)
      parameter (ATTRID_VOLNAM = "volnam"	)
      parameter (ATTRID_VOLNAME = "volname"	)
      parameter (ATTRID_WATDEP = "watdep"	)
      parameter (ATTRID_WFID = "wfid"		)
      parameter (ATTRID_WGT = "wgt"		)
      parameter (ATTRID_YEAR = "year"		)
      parameter (ATTRID_YIELD = "yield"		)
      parameter (ATTRID_YLDMAX = "yldmax"	)
      parameter (DBL_ASCII = 0	)
      parameter (DBL_BINARY = 1	)
      parameter (DBL_DBL = 2	)
      parameter (DBL_FLT = 1	)
      parameter (DBL_INT = 0	)
      parameter (DBL_STR = 3	)
      parameter (NUMBER_ATTRS = 282		)
      parameter (NUMBER_ATTRS30 = 152		)
      parameter (NUMBER_RELS30 = 23	)
      parameter (RELID_AFFILIATION = "affiliation"	)
      parameter (RELID_ALIAS = "alias"	)
      parameter (RELID_ARRIVAL = "arrival"	)
      parameter (RELID_ASSOC = "assoc"	)
      parameter (RELID_BEAM = "beam"		)
      parameter (RELID_CENTRYD = "centryd"	)
      parameter (RELID_CHANNEL = "channel"	)
      parameter (RELID_CHOPER = "choper"	)
      parameter (RELID_CODE = "code"		)
      parameter (RELID_COMMENT = "comment"	)
      parameter (RELID_COUNTER = "counter"	)
      parameter (RELID_DATE = "date"		)
      parameter (RELID_DAY = "day"		)
      parameter (RELID_DETECTION = "detection"	)
      parameter (RELID_DETLOC = "detloc"	)
      parameter (RELID_EVENT = "event"	)
      parameter (RELID_EVWF = "evwf"		)
      parameter (RELID_EXPLO = "explo"		)
      parameter (RELID_EXTRA = "extra"		)
      parameter (RELID_FEATURE = "feature"	)
      parameter (RELID_FILTER = "filter"	)
      parameter (RELID_FKDISC = "fkdisc"	)
      parameter (RELID_FKREC = "fkrec"		)
      parameter (RELID_FPLANE = "fplane"	)
      parameter (RELID_FSDISC = "fsdisc"	)
      parameter (RELID_FSREC = "fsrec"		)
      parameter (RELID_GREGION = "gregion"	)
      parameter (RELID_INSTRUMENT = "instrument"	)
      parameter (RELID_LASTID = "lastid"	)
      parameter (RELID_LOC = "loc"		)
      parameter (RELID_MOMENT = "moment"	)
      parameter (RELID_NETMAG = "netmag"	)
      parameter (RELID_NETWORK = "network"	)
      parameter (RELID_ORIGERR = "origerr"	)
      parameter (RELID_ORIGIN = "origin"	)
      parameter (RELID_REMARK = "remark"	)
      parameter (RELID_SENSOR = "sensor"	)
      parameter (RELID_SIGPRO = "sigpro"	)
      parameter (RELID_SITE = "site"	)
      parameter (RELID_SITECHAN = "sitechan"	)
      parameter (RELID_SREGION = "sregion"	)
      parameter (RELID_STALOG = "stalog"	)
      parameter (RELID_STAMAG = "stamag"	)
      parameter (RELID_STASSOC = "stassoc"	)
      parameter (RELID_STATION = "station"	)
      parameter (RELID_TAPE = "tape"		)
      parameter (RELID_WFDISC = "wfdisc"	)
      parameter (RELID_WFTAG = "wftag"	)
      parameter (RELID_WFTAPE = "wftape"	)
      parameter (RELID_WFTAR = "wftar"	)
      parameter (RELID_XPARAM = "xparam"	)

c $Id: dbmaprelease.f,v 1.11 2010-10-26 17:02:56 mitch Exp $ 
# 17 "dbmaprelease_mac.F" 2 
c
      data  pi / 3.14159265358979323846d0 /
      data  tstart / ' ' /
      data  tend / ' ' /
c
      dep1 = 30
      dep2 = 75
      dep3 = 125
c
c     kint = signal (2,setans,-1)
      if (iargc() .lt. 3) then
	print *,
     +'usage: dbmapevents dbname {sta | orid | lat:lon} range ',
     *'                   [-c] [-l] [auth1 sym1 hue1 lit1 sat1 ...]'
	stop
      end if
c     call set_ieeehandlers
c
c    Parse command line
c
      call getarg (1, dbnam)
      call getarg (2, sta)
      do 1  i = 1, len(sta)
	if (sta(i:i) .eq. ':') go to 2
    1 continue
      xlat1 = -100.0d0
      xlon1 = -100.0d0
      go to 3
    2 continue
      read (sta(1:i-1), *, err=58) xlat1
      read (sta(i+1:ilen(sta)), *, err=58) xlon1
      if (xlon1 .gt. 180.d0) xlon1 = xlon1 - 360.0d0
      if (xlon1 .lt. -180.d0) xlon1 = xlon1 + 360.0d0
    3 continue
      call getarg (3, arg)
      do 5  i = 1, len(arg)
	if (arg(i:i) .eq. ':') go to 6
    5 continue
      read (arg, *) range
      xmax = range
      xmin = -range
      ymax = range
      ymin = -range
      go to 7
    6 continue
      read (arg(1:i-1), *, err=58) xlat2
      read (arg(i+1:ilen(arg)), *, err=58) xlon2
      if (xlon2 .gt. 180.d0) xlon2 = xlon2 - 360.0d0
      if (xlon2 .lt. -180.d0) xlon2 = xlon2 + 360.0d0
      if (xlat1 .gt. -100.0d0) then
        xmax =  0.5d0*(xlon2-xlon1)
        xmin = -0.5d0*(xlon2-xlon1)
        ymax =  0.5d0*(xlat2-xlat1)
        ymin = -0.5d0*(xlat2-xlat1)
        xlat1 = 0.5d0*(xlat2+xlat1)
        xlon1 = 0.5d0*(xlon2+xlon1)
      end if
    7 continue
      n = iargc()
      nauth = 0
      i = 4
      inter = 0
      itran = 0
   66 continue
	if (i .gt. n) go to 67
          call getarg (i, arg)
	  if (arg .eq. '-c') then
	    inter = 1
	    i = i + 1
	    go to 66
	  end if
          call getarg (i, arg)
	  if (arg .eq. '-l') then
	    itran = 1
            xmax = range*9.8/6.0
            xmin = -range*9.8/6.0
	    i = i + 1
	    go to 66
	  end if
	if (n-i .lt. 4) then
	  print *,'dbmapevents: Too few arguments'
	  stop
	end if
	nauth = nauth + 1
	call getarg (i, auth(nauth))
	call getarg (i+1, sym(nauth))
	call getarg (i+2, arg)
	read (arg, *) symhue(nauth)
	call getarg (i+3, arg)
	read (arg, *) symlit(nauth)
	call getarg (i+4, arg)
	read (arg, *) symsat(nauth)
	i = i + 5
      go to 66
   67 continue
c
c    Open database
c
      call db30create (dbnam, idb)
      if (idb .eq. 0) then
	print *,'dbmapevents: db30create error'
	stop
      end if
c
c    Read in site data and find reference station
c
      jorid = -1
      call dbgetntuples (idb, RELID_SITE, ntuples)
      if (ntuples .lt. 1) then
	go to 64
      end if
      do 55  i = 1, ntuples
	call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_LON, xlon,
     +                   ATTRID_LAT, xlat,
     +                   ATTRID_STA, arg,
     +			 0)
	if (sta .eq. arg) then
	  xlat1 = xlat
	  xlon1 = xlon
	  if (xlon1 .gt. 180.d0) xlon1 = xlon1 - 360.0d0
	  if (xlon1 .lt. -180.d0) xlon1 = xlon1 + 360.0d0
	  go to 56
	end if
   55 continue
   64 do 65  i = 1, len(sta)
	if (sta(i:i) .eq. ':') go to 68
   65 continue
      read (sta, *) iorid
      call dbgetntuples (idb, RELID_ORIGIN, ntuples)
      if (ntuples .lt. 1) then
	print *,'dbmapevents: No origin tuples'
	stop
      end if
      do 57  i = 1, ntuples
	call dbgetattrs (idb, RELID_ORIGIN, i,
     +                   ATTRID_LON, xlon,
     +                   ATTRID_LAT, xlat,
     +                   ATTRID_ORID, jorid,
     +                   ATTRID_TIME, evtime,
     +			 0)
	if (jorid .eq. iorid) then
	  write (sta, '(a,i4)') 'orid = ',iorid
	  xlat1 = xlat
	  xlon1 = xlon
	  if (xlon1 .gt. 180.d0) xlon1 = xlon1 - 360.0d0
	  if (xlon1 .lt. -180.d0) xlon1 = xlon1 + 360.0d0
	  go to 56
	end if
   57 continue
   58 print *,'dbmapevents: reference station or origin not found'
      stop
   68 continue
   56 continue
c
c    Set up plotting
c
c     print*,'About to read in parameters with get_params'
c
c    Read in parameters
c
      filename_in = 'dbmap_release'
      call get_params(filename_in,custom_title,special_files,nsf,
     &             istaplt,istnam,ipdeplt,ipdepth,iporid,icities,ipipe,
     &             iblue,ipmag,idcirc,ititl,ipumps,iflt,icol,itran)
c     print*,'nsf: ',nsf
c     print*,'special files: ',special_files
      if(ititl.eq.1) then
        write(title,'(a)') custom_title
      endif
      if(itran.eq.1) then
        xmax = range*9.8/6.0
        xmin = -range*9.8/6.0
        xdim = 9.8
        ydim = 6.0
        xlow = 0.1
        ylow = 1.3
      else
        xdim = 3.3
        ydim = 3.3
        xlow = 4.1
        ylow = 3.7
      endif
      nevs = 0
      x = 0.5*(xmin+xmax)
      y = 0.5*(ymin+ymax)
      inset = 0
      j1=1
      do i=1,nsf
        do j = j1,512
          if(special_files(j:j).eq.' ') then
            j2 = j-1
            lblfile(i) = special_files(j1:j2)
            j1 = j+1
            go to 303
          endif
        enddo
 303    continue
      enddo
c     nsf = 2
c     lblfile(1) = 'fort.54'
c     lblfile(2) = 'junk.bp'
c
c    Make plot
c
  100 call plotit (itran, xdim, ydim,  xlow, ylow,
     +                   xmax, xmin, ymax, ymin, dbnam,
     +                   sta, jorid, xlat1, xlon1, evtime,
     +                   nauth, auth, sym, symhue, symlit, symsat,
     +                   idb, nevs, ievs, iporid, ipdepth,
     +                   istaplt, istnam, ipdeplt,
     +                   ipmag, idcirc, icities, ipipe, ipaths,
     +                   iblue, icol, ititl, iflt, inset, title, 
     +                   tstart, tend, dep1, dep2, dep3)
      if (nsf.gt.0) then
        ybp = 0.
        do i=1,nsf
        temp = ' '
        temp = lblfile(i)
        call get_data_path(temp,bp_lbl_file)
        open(33,file=bp_lbl_file,err=237)
        read(33,'(a)',end=237)atext
        if(atext(1:5).eq.'label') then
  238 read(33,'(5f10.4,i4,i2,a80)',end=237)alat,alon,alat1,alon1,size,
     +                               ifnt,ipos,atext
	  call latlon2xydel (xlat1, xlon1, alat, alon,
     +                     xplt1, yplt1)
          if(alon1.lt.-900.0) then
            angle = alat1
          else
	    call latlon2xydel (xlat1, xlon1, alat1, alon1,
     +                       x2, y2)
            angle = (180./3.14159)*atan2((y2-yplt1),(x2-xplt1))
          endif
          if(ifnt.ne.0) call cfont(ifnt)
          call chrsiz(size,1.0,0.0)
          call text (xplt1, yplt1, angle, ipos, atext, 0)
          if(ifnt.ne.0) call cfont(132)
          go to 238
        else
          call chrsiz(0.1,1.0,0.0)
          call setscl(0.,xdim,0.,ydim)
  241     read(33,'(a80)',end=239)atext
          ybp = ybp - .20
          call text (xdim/2.0, ybp, 0.0, 4, atext, 1)
          go to 241
  239     call setscl(xmin,xmax,ymin,ymax)
        endif
  237   continue
        close (33)
        enddo
  240   continue
      endif
      akmpinch = 111.195*2.0*ymax/ydim
      if(akmpinch.lt.1) then
        alen = 1./akmpinch
        ntics = 10
        atext = ' '
        atext = '1'
      else if(akmpinch.lt.10) then
        alen = 10./akmpinch
        ntics = 10
        atext = ' '
        atext = '10'
      else if(akmpinch.lt.50) then
        alen = 50./akmpinch
        ntics = 5
        atext = ' '
        atext = '50'
      else if(akmpinch.lt.100) then
        alen = 100./akmpinch
        ntics = 10
        atext = ' '
        atext = '100'
      else if(akmpinch.lt.200) then
        alen = 200./akmpinch
        ntics = 2
        atext = ' '
        atext = '200'
      else if(akmpinch.lt.300) then
        alen = 300./akmpinch
        ntics = 3
        atext = ' '
        atext = '300'
      else if(akmpinch.lt.500) then
        alen = 500./akmpinch
        ntics = 5
        atext = ' '
        atext = '500'
      else if(akmpinch.lt.1000) then
        alen = 1000./akmpinch
        ntics = 10
        atext = ' '
        atext = '1000'
      else if(akmpinch.lt.10000) then
        alen = 10000./akmpinch
        ntics = 10
        atext = ' '
        atext = '10000'
      endif
      x1 = xdim-alen-0.5
      x2 = xdim-0.5
      y1 = 0.35
      y2 = 0.35
      dtic = alen/ntics
      call chrsiz(0.08,1.0,0.0)
      call setscl(0.,xdim,0.,ydim)
      call line(x1,y1,x2,y2,0.01,0,0)
      call line(x1,y1,x1,y1+.12,0.01,0,0)
      call line(x2,y2,x2,y2+.12,0.01,0,0)
      call text (x1, y1+.12, 0.0, 3, '0', 1)
      call text (x2, y2+.12, 0.0, 3, atext, 1)
      call text (x1+alen/2.0, y2-.05, 0.0, 5, 'kilometers', 1)
      do i=1,ntics-1
        call line(x1+dtic*i,y1,x1+dtic*i,y1+.06,0.01,0,0)
      enddo
      call setscl(xmin,xmax,ymin,ymax)
c
c    Check for cursor entries
c
      if (inter .eq. 0) then
        call finitt
	call hdkild
        stop
      end if
  110 if(ans.ne.'S') call cursor (x,y,ans)
      if (ans .eq. 'q') then
        call finitt
	stop
      else if (ans .eq. 'Q') then
        call finitt
	call hdkild
	stop
      else if (ans .eq. '?') then
	write (6, '(a)') 'Key  Action'
	write (6, '(a)') ' ?   help'
	write (6, '(a)') ' R   Redraw'
	write (6, '(a)') ' d   Select depth intervals'
        write (6, '(a)') ' l   Draw a line from current position'
        write (6, '(a)') ' G   Geographic Lat Lon of cursor position'
        write (6, '(a)') ' N   Set in an inset'
        write (6, '(a)') ' S   Sleep'
	write (6, '(a)') ' F   Draw faults'
	write (6, '(a)') ' T   draw text at lower left corner'
	write (6, '(a)') ' Z   draw text at lower left corner and save'
	write (6, '(a)') ' L   Toggle default title'
	write (6, '(a)') ' b   Toggle blue of ocean'
	write (6, '(a)') ' g   Plot great circle paths'
	write (6, '(a)') ' c   Plot cities'
	write (6, '(a)') ' C   Toggle circle and hexagon symbols'
	write (6, '(a)') ' P   Plot pipeline'
	write (6, '(a)') ' q   quit and keep window'
	write (6, '(a)') ' Q   quit and destroy window'
	write (6, '(a)') ' i   small zoom in'
	write (6, '(a)') ' I   big zoom in'
	write (6, '(a)') ' o   small zoom out'
	write (6, '(a)') ' O   big zoom out'
	write (6, '(a)') ' p   pan'
	write (6, '(a)') ' E   great circle to closest event'
	write (6, '(a)') ' e   identify closest event'
	write (6, '(a)') ' s   identify closest station'
	write (6, '(a)') ' t   enter time range'
	write (6, '(a)') ' B   Black and White depth coloring on/off'
	write (6, '(a)') ' 1   orid labels on/off'
	write (6, '(a)') ' 2   depth coloring on/off'
	write (6, '(a)') ' 3   shallow depth coloring on/off'
	write (6, '(a)') ' 4   shallow depth diff coloring on/off'
	write (6, '(a)') ' 5   sta symbols on/off'
	write (6, '(a)') ' 6   mark pdevs on/off'
	write (6, '(a)') ' 7   mag size on/off'
        write (6, '(a)') ' 8   distance circles on/off'
        write (6, '(a)') ' 9   sta names on/off'
	go to 110
      else if (ans .eq. 'R') then
	go to 100
      else if (ans .eq. 'G') then
        call xydel2latlon (xlat1, xlon1, x, y,xlat0, xlon0)
        print*,'Latitude = ',xlat0,' Longitude = ',xlon0
	go to 110
      else if (ans .eq. 'S') then
        call sleep (20)
        nevs = 0
	go to 100
      else if (ans .eq. 'N') then
        if (inset.eq.0) then
          nevs = 0
          inset = 1
          call cursor(x2,y2,ans)
          if (x2.gt.x) then
            xl = x
            xr = x2
          else
            xl = x2
            xr = x
          endif
          if (y2.gt.y) then
            yb = y
            yt = y2
          else
            yb = y2
            yt = y
          endif
          ylow = ylow + (yb-ymin)*ydim/(ymax - ymin)
          xlow = xlow + (xl-xmin)*xdim/(xmax - xmin)
          xdim = (xr - xl) * xdim/(xmax - xmin)
          ydim = (yt - yb) * ydim/(ymax - ymin)
c         xdim = 5.3
c         ydim = 1.3
c         if(itran.eq.0) then
c           xlow = 2.0
c           ylow = 2.1
c         else
c           xlow = 4.0
c           ylow = 1.4
c         endif
c         rangei = 9.0
c         xmax = rangei
c         xmin = -rangei
c         ymax = rangei*1.3/5.3
c         ymin = -rangei*1.3/5.3
          xlatorg = xlat1
          xlonorg = xlon1
          print*,'Enter range (deg), and center lat and lon:'
          read(*,*)rangei,xlat1,xlon1
          if (xdim.gt.ydim) then
            ymin = -rangei
            ymax = rangei
            xmin = -rangei*xdim/ydim
            xmax = rangei*xdim/ydim
          else
            ymin = -rangei
            ymax = rangei
            xmin = -rangei*ydim/xdim
            xmax = rangei*ydim/xdim
          endif
c         xlat1 = 53.0
c         xlon1 = -178.0
          ititl = 1
          title = ' '
        else
          nevs = 0
          inset = 0
          xdim = 7.3
          ydim = 7.3
          xlow = 0.1
          ylow = 2.0
          xmax = range
          xmin = -range
          ymax = range
          ymin = -range
          if(itran.eq.1) then
            xmax = range*9.8/6.0
            xmin = -range*9.8/6.0
            xdim = 9.8
            ydim = 6.0
            xlow = 0.1
            ylow = 1.3
          else
            xdim = 7.3
            ydim = 7.3
            xlow = 0.1
            ylow = 2.0
          endif
          xlat1 = xlatorg
          xlon1 = xlonorg
          ititl = 0
        endif
        go to 100
      else if (ans .eq. 'd') then
	  write (6, '(a)') 'Enter three depth values:'
          read(5,*)dep1,dep2,dep3
          print*,'Using depth ranges - ',dep1,dep2,dep3
	go to 110
      else if (ans .eq. 'L') then
	if (ititl .eq. 1) then
	  write (6, '(a)') 'Turning default title on'
	  ititl = 0
	else
	  write (6, '(a)') 'Enter custom title for plot'
          read(5,'(a)')title
          print*,'Using title - ',title
	  ititl = 1
	end if
	go to 110
      else if (ans .eq. 'F') then
	if (iflt .eq. 1) then
	  write (6, '(a)') 'Turning fault plotting off'
	  iflt = 0
	else
	  write (6, '(a)') 'Turning fault plotting on'
	  iflt = 1
	end if
	go to 110
      else if (ans .eq. 'C') then
	if (icol .eq. 1) then
	  write (6, '(a)') 'Turning circle symbol plotting on'
	  icol = 0
	else
	  write (6, '(a)') 'Turning box symbol plotting on'
	  icol = 1
	end if
	go to 110
      else if (ans .eq. 'b') then
	if (iblue .eq. 1) then
	  write (6, '(a)') 'Turning blue ocean plotting off'
	  iblue = 0
	else
	  write (6, '(a)') 'Turning blue ocean plotting on'
	  iblue = 1
	end if
	go to 110
      else if (ans .eq. 'T') then
        Print*,'Enter text to draw:'
        read(5,'(a)')atext
        call text (x, y, 0.0, 1, atext, 0)
	go to 110
      else if (ans .eq. 'Z') then
        Print*,'Enter text to draw:'
        read(5,'(a)')atext
        Print*,'Enter size of text (inches):'
        read(5,*)size
        Print*,'Enter font number (0 for default):'
        read(5,*)ifont
        print*,'Choose angle with second cursor (h=0.0,v=90.0):'
        x1 = x
        y1 = y
        call cursor(x1,y1,ans)
        if(ans.eq.'h') then
          angle = 0.0
          x1 = x + 2.
        else if(ans.eq.'v') then
          angle = 90.0
          y1 = y + 2.
        else
          angle = (180./3.14159)*atan2((y1-y),(x1-x))
        endif
        call xydel2latlon (xlat1, xlon1, x, y,xlat0, xlon0)
        ipos = 0
        if(ifont.ne.0) call cfont(ifont)
        call chrsiz(size,1.0,0.0)
        call text (x, y, angle, ipos, atext, 0)
        call chrsiz(0.08,1.0,0.0)
        if(ifont.ne.0) call cfont(132)
        call xydel2latlon (xlat1, xlon1, x1, y1,xlat01, xlon01)
        if(ans.eq.'h'.or.ans.eq.'v') then
        anum = -999.0
        write(54,'(5f10.4,i4,i2,a80)')xlat0,xlon0,angle,anum,size,
     +                             ifont,ipos,atext
        else
        write(54,'(5f10.4,i4,i2,a80)')xlat0,xlon0,xlat01,xlon01,size,
     +                             ifont,ipos,atext
        endif
	go to 110
      else if (ans .eq. 'g') then
	if (ipaths .eq. 1) then
	  write (6, '(a)') 'Turning great circle plotting off'
	  ipaths = 0
	else
	  write (6, '(a)') 'Turning great circle plotting on'
	  ipaths = 1
	end if
	go to 110
      else if (ans .eq. 'c') then
	if (icities .eq. 1) then
	  write (6, '(a)') 'Turning city plotting off'
	  icities = 0
	else
	  write (6, '(a)') 'Turning city plotting on'
	  icities = 1
	end if
	go to 110
      else if (ans .eq. 'P') then
	if (ipipe .eq. 1) then
	  write (6, '(a)') 'Turning pipeline plotting off'
	  ipipe = 0
	else
	  write (6, '(a)') 'Turning pipeline plotting on'
	  ipipe = 1
	end if
	go to 110
      else if (ans .eq. 'i') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 0.8*xx
	xmin = x - 0.8*xx
	ymax = y + 0.8*yy
	ymin = y - 0.8*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'I') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 0.2*xx
	xmin = x - 0.2*xx
	ymax = y + 0.2*yy
	ymin = y - 0.2*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'o') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 1.25*xx
	xmin = x - 1.25*xx
	ymax = y + 1.25*yy
	ymin = y - 1.25*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'O') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 5.0*xx
	xmin = x - 5.0*xx
	ymax = y + 5.0*yy
	ymin = y - 5.0*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'p') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + xx
	xmin = x - xx
	ymax = y + yy
	ymin = y - yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'l') then
        print*,'select second point with cursor:'
        call cursor (x1 ,y1 ,ans)
        call line (x1, y1, x, y, 0.0, 0, 0)
        go to 110
      else if (ans .eq. 'E') then
        write (6, '(a,$)') 'Enter offset azimuth: '
        read (5, *) az
        az = az*3.14159/180.0
	call idevent (xlat1, xlon1, x, y, nevs, ievs, 
     +                nauth, auth)
        r = sqrt(x*x+y*y)
        az = az + atan2(x,y)
        x = r*sin(az)
        y = r*cos(az)
        call line (0.0, 0.0, x, y, 0.0, 0, 0)
        go to 110
      else if (ans .eq. 'e') then
	call idevent (xlat1, xlon1, x, y, nevs, ievs, 
     +                nauth, auth)
	go to 110
      else if (ans .eq. 's') then
	call idstation (xlat1, xlon1, x, y, idb,
     +                nauth, auth)
	go to 110
      else if (ans .eq. 't') then
        write (6, '(a,$)') 'Enter start time: '
        read (5, '(a)') tstart
        write (6, '(a,$)') 'Enter end time: '
        read (5, '(a)') tend
        nevs = 0
	go to 110
      else if (ans .eq. '1') then
	if (iporid .eq. 1) then
	  write (6, '(a)') 'Turning orid labeling off'
	  iporid = 0
	else
	  write (6, '(a)') 'Turning orid labeling on'
	  iporid = 1
	end if
	go to 110
      else if (ans .eq. 'B') then
	if (ipdepth .gt. 0) then
	  write (6, '(a)') 'Turning B & W depth coloring off'
	  ipdepth = 0
	else
	  write (6, '(a)') 'Turning B & W depth coloring on'
	  ipdepth = 4
	end if
	go to 110
      else if (ans .eq. '2') then
	if (ipdepth .gt. 0) then
	  write (6, '(a)') 'Turning depth coloring off'
	  ipdepth = 0
	else
	  write (6, '(a)') 'Turning depth coloring on'
	  ipdepth = 1
	end if
	go to 110
      else if (ans .eq. '3') then
	if (ipdepth .gt. 0) then
	  write (6, '(a)') 'Turning shallow depth coloring off'
	  ipdepth = 0
	else
	  write (6, '(a)') 'Turning shallow depth coloring on'
	  ipdepth = 2
	end if
	go to 110
      else if (ans .eq. '4') then
	if (ipdepth .gt. 0) then
	  write (6, '(a)') 'Turning shallow depth diff coloring off'
	  ipdepth = 0
	else
	  write (6, '(a)') 'Turning shallow depth diff coloring on'
	  ipdepth = 3
	end if
	go to 110
      else if (ans .eq. '5') then
	if (istaplt .gt. 0) then
	  write (6, '(a)') 
     +           'Turning unassociated station symbol plotting off'
	  istaplt = 0
	else
	  write (6, '(a)') 
     +           'Turning unassociated station symbol plotting on'
	  istaplt = 1
	end if
	go to 110
      else if (ans .eq. '6') then
	if (ipdeplt .gt. 0) then
	  write (6, '(a)') 
     +           'Not marking PDE events'
	  ipdeplt = 0
	else
	  write (6, '(a)') 
     +           'Marking PDE events'
	  ipdeplt = 1
	end if
	go to 110
      else if (ans .eq. '7') then
	if (ipmag .gt. 0) then
	  write (6, '(a)') 
     +           'Not marking magnitudes'
	  ipmag = 0
	else
	  write (6, '(a)') 
     +           'Marking magnitudes'
	  ipmag = 1
	end if
	go to 110
      else if (ans .eq. '8') then
        if (idcirc .gt. 0) then
          write (6, '(a)')
     +           'Not plotting distance circles'
          idcirc = 0
        else
          write (6, '(a)')
     +           'Plotting distance circles'
          idcirc = 1
        end if
        go to 110
      else if (ans .eq. '9') then
        if (istnam .gt. 0) then
          write (6, '(a)')
     +           'Not plotting station names'
          istnam = 0
        else
          write (6, '(a)')
     +           'Plotting station names'
          istnam = 1
        end if
        go to 110
      end if
c
      go to 110
      end
      subroutine plotit (itran, xdim, ydim,  xlow, ylow,
     +                   xmax, xmin, ymax, ymin, dbnam,
     +                   sta, jorid, xlatc, xlonc, evtime,
     +                   nauth, auth, sym, symhue, symlit, symsat,
     +                   idb, nevs, ievs, iporid, ipdepth,
     +                   istaplt, istnam, ipdeplt,
     +                   ipmag, idcirc, icities, ipipe, ipaths, iblue,
     +                   icol, ititl, iflt, inset, title, tstrt, tnd, 
     +                   dep1, dep2, dep3)
c
      parameter  (MAXEVENTS = 100000)
      parameter  (MAXPOINTS = 200000)
      real*4 xlat1, xlon1, xlat2, xlon2, pi, xlatc, xlonc
      real*4 ppltx(10000),pplty(10000)
      real*8 depth, deptho, depthc, evtime, elat, elon, slat,slon
      character*80 dbnam, sta, arg, auth(20), sym(20), title
      character*80 region, type, loc, tstart, tend, symbl, string
      real*4 symhue(20), symlit(20), symsat(20)
      real*8 slatmndp, slatmxdp, slonmndp, slonmxdp
      integer*8 ievs(MAXEVENTS)
      character*(*) tstrt, tnd
      character*30  city
      character*30  charevnts
      character*30  text1
      character*30  text2
      character*30  text3
      character*30  text4
      character*10  frmat
      integer*4 dep1,dep2,dep3
c
# 1 "/opt/antelope/5.2-64/include/EV_f.i" 1 

	integer	EV_NULL
        parameter (EV_NULL = (0))
	integer	EV_NAME
        parameter (EV_NAME = (1))
	integer	EV_NHYPOS
        parameter (EV_NHYPOS = (2))
	integer	EV_PREFHYPO
        parameter (EV_PREFHYPO = (3))
	integer	EV_HYPO_TIME
        parameter (EV_HYPO_TIME = (4))
	integer	EV_HYPO_LAT
        parameter (EV_HYPO_LAT = (5))
	integer	EV_HYPO_LON
        parameter (EV_HYPO_LON = (6))
	integer	EV_HYPO_DEPTH
        parameter (EV_HYPO_DEPTH = (7))
	integer	EV_HYPO_MB
        parameter (EV_HYPO_MB = (8))
	integer	EV_HYPO_MS
        parameter (EV_HYPO_MS = (9))
	integer	EV_HYPO_ML
        parameter (EV_HYPO_ML = (10))
	integer	EV_HYPO_AUTH
        parameter (EV_HYPO_AUTH = (11))
	integer	EV_HYPO_NSTAS
        parameter (EV_HYPO_NSTAS = (12))
	integer	EV_HYPO_ASSOCFLAG
        parameter (EV_HYPO_ASSOCFLAG = (13))
	integer	EV_HYPO_STA_STA
        parameter (EV_HYPO_STA_STA = (14))
	integer	EV_HYPO_STA_LAT
        parameter (EV_HYPO_STA_LAT = (15))
	integer	EV_HYPO_STA_LON
        parameter (EV_HYPO_STA_LON = (16))
	integer	EV_HYPO_STA_ELEV
        parameter (EV_HYPO_STA_ELEV = (17))
	integer	EV_HYPO_STA_NASSOCS
        parameter (EV_HYPO_STA_NASSOCS = (50))
	integer	EV_HYPO_STA_ASSOC_CHAN
        parameter (EV_HYPO_STA_ASSOC_CHAN = (18))
	integer	EV_HYPO_STA_ASSOC_PHASE
        parameter (EV_HYPO_STA_ASSOC_PHASE = (19))
	integer	EV_HYPO_STA_ASSOC_IPHASE
        parameter (EV_HYPO_STA_ASSOC_IPHASE = (20))
	integer	EV_HYPO_STA_ASSOC_TIME
        parameter (EV_HYPO_STA_ASSOC_TIME = (21))
	integer	EV_HYPO_STA_ASSOC_AZIMUTH
        parameter (EV_HYPO_STA_ASSOC_AZIMUTH = (22))
	integer	EV_HYPO_STA_ASSOC_INC
        parameter (EV_HYPO_STA_ASSOC_INC = (23))
	integer	EV_HYPO_STA_ASSOC_RECT
        parameter (EV_HYPO_STA_ASSOC_RECT = (24))
	integer	EV_HYPO_STA_ASSOC_DELTIME
        parameter (EV_HYPO_STA_ASSOC_DELTIME = (25))
	integer	EV_HYPO_STA_ASSOC_ASSOCFLAG
        parameter (EV_HYPO_STA_ASSOC_ASSOCFLAG = (26))
	integer	EV_HYPO_STA_ASSOC_DISTANCE
        parameter (EV_HYPO_STA_ASSOC_DISTANCE = (27))
	integer	EV_HYPO_STA_ASSOC_TIMERES
        parameter (EV_HYPO_STA_ASSOC_TIMERES = (28))
	integer	EV_HYPO_STA_ASSOC_SHAZ
        parameter (EV_HYPO_STA_ASSOC_SHAZ = (29))
	integer	EV_HYPO_STA_ASSOC_HSAZ
        parameter (EV_HYPO_STA_ASSOC_HSAZ = (30))
	integer	EV_HYPO_STA_ASSOC_AZRES
        parameter (EV_HYPO_STA_ASSOC_AZRES = (31))
	integer	EV_HYPO_STA_ASSOC_TIMEWGT
        parameter (EV_HYPO_STA_ASSOC_TIMEWGT = (32))
	integer	EV_HYPO_STA_ASSOC_AZWGT
        parameter (EV_HYPO_STA_ASSOC_AZWGT = (33))
	integer	EV_DBL
        parameter (EV_DBL = (34))
	integer	EV_TUPLE
        parameter (EV_TUPLE = (35))
	integer	EV_EVID
        parameter (EV_EVID = (36))
	integer	EV_HYPO_TUPLE
        parameter (EV_HYPO_TUPLE = (37))
	integer	EV_HYPO_ORID
        parameter (EV_HYPO_ORID = (38))
	integer	EV_HYPO_STA_ASSOC_ARID
        parameter (EV_HYPO_STA_ASSOC_ARID = (39))
	integer	EV_HYPO_STA_ASSOC_ASTUPLE
        parameter (EV_HYPO_STA_ASSOC_ASTUPLE = (40))
	integer	EV_HYPO_STA_ASSOC_ARTUPLE
        parameter (EV_HYPO_STA_ASSOC_ARTUPLE = (41))

	integer	EVSV_NULL
        parameter (EVSV_NULL = (0))
	integer	EVSV_HYPO_TIME
        parameter (EVSV_HYPO_TIME = (4))
	integer	EVSV_HYPO_LAT
        parameter (EVSV_HYPO_LAT = (5))
	integer	EVSV_HYPO_LON
        parameter (EVSV_HYPO_LON = (6))
	integer	EVSV_HYPO_DEPTH
        parameter (EVSV_HYPO_DEPTH = (7))
	integer	EVSV_HYPO_MB
        parameter (EVSV_HYPO_MB = (8))
	integer	EVSV_HYPO_MS
        parameter (EVSV_HYPO_MS = (9))
	integer	EVSV_HYPO_ML
        parameter (EVSV_HYPO_ML = (10))
	integer	EVSV_HYPO_AUTH
        parameter (EVSV_HYPO_AUTH = (11))
	integer	EVSV_HYPO_NSTAS
        parameter (EVSV_HYPO_NSTAS = (12))
	integer	EVSV_HYPO_ASSOCFLAG
        parameter (EVSV_HYPO_ASSOCFLAG = (13))
	integer	EVSV_HYPO_PREF
        parameter (EVSV_HYPO_PREF = (42))
	integer	EVSV_STA_STA
        parameter (EVSV_STA_STA = (14))
	integer	EVSV_STA_LAT
        parameter (EVSV_STA_LAT = (15))
	integer	EVSV_STA_LON
        parameter (EVSV_STA_LON = (16))
	integer	EVSV_STA_ELEV
        parameter (EVSV_STA_ELEV = (17))
        integer EVSV_STA_NSCVS
        parameter (EVSV_STA_NSCVS = (60))
        integer EVSV_STA_SCVS
        parameter (EVSV_STA_SCVS = (61))
	integer	EVSV_STA_NASSOCS
        parameter (EVSV_STA_NASSOCS = (50))
	integer	EVSV_STA_ASSOC_CHAN
        parameter (EVSV_STA_ASSOC_CHAN = (18))
	integer	EVSV_STA_ASSOC_PHASE
        parameter (EVSV_STA_ASSOC_PHASE = (19))
	integer	EVSV_STA_ASSOC_IPHASE
        parameter (EVSV_STA_ASSOC_IPHASE = (20))
	integer	EVSV_STA_ASSOC_TIME
        parameter (EVSV_STA_ASSOC_TIME = (21))
	integer	EVSV_STA_ASSOC_AZIMUTH
        parameter (EVSV_STA_ASSOC_AZIMUTH = (22))
	integer	EVSV_STA_ASSOC_INC
        parameter (EVSV_STA_ASSOC_INC = (23))
	integer	EVSV_STA_ASSOC_RECT
        parameter (EVSV_STA_ASSOC_RECT = (24))
	integer	EVSV_STA_ASSOC_DELTIME
        parameter (EVSV_STA_ASSOC_DELTIME = (25))
	integer	EVSV_STA_ASSOC_ASSOCFLAG
        parameter (EVSV_STA_ASSOC_ASSOCFLAG = (26))
	integer	EVSV_STA_ASSOC_DISTANCE
        parameter (EVSV_STA_ASSOC_DISTANCE = (27))
	integer	EVSV_STA_ASSOC_TIMERES
        parameter (EVSV_STA_ASSOC_TIMERES = (28))
	integer	EVSV_STA_ASSOC_SHAZ
        parameter (EVSV_STA_ASSOC_SHAZ = (29))
	integer	EVSV_STA_ASSOC_HSAZ
        parameter (EVSV_STA_ASSOC_HSAZ = (30))
	integer	EVSV_STA_ASSOC_AZRES
        parameter (EVSV_STA_ASSOC_AZRES = (31))
	integer	EVSV_STA_ASSOC_TIMEWGT
        parameter (EVSV_STA_ASSOC_TIMEWGT = (32))
	integer	EVSV_STA_ASSOC_AZWGT
        parameter (EVSV_STA_ASSOC_AZWGT = (33))
	integer	EVSV_DBL
        parameter (EVSV_DBL = (34))
	integer	EVSV_HYPO_TUPLE
        parameter (EVSV_HYPO_TUPLE = (37))
	integer	EVSV_HYPO_ORID
        parameter (EVSV_HYPO_ORID = (38))
	integer	EVSV_STA_ASSOC_ARID
        parameter (EVSV_STA_ASSOC_ARID = (39))
	integer	EVSV_STA_ASSOC_ASTUPLE
        parameter (EVSV_STA_ASSOC_ASTUPLE = (40))
	integer	EVSV_STA_ASSOC_ARTUPLE
        parameter (EVSV_STA_ASSOC_ARTUPLE = (41))


c $Id: dbmaprelease.f,v 1.11 2010-10-26 17:02:56 mitch Exp $ 
# 813 "dbmaprelease_mac.F" 2 
# 1 "/opt/antelope/5.2-64/include/dbl2.i" 1 


# 1 "/opt/antelope/5.2-64/include/db.i" 1 

      integer dbINVALID
      parameter (dbINVALID = -102)
      integer dbCOUNT
      parameter (dbCOUNT = -301)
      integer dbDATABASE_COUNT
      parameter (dbDATABASE_COUNT = -302	       )
      integer dbTABLE_COUNT
      parameter (dbTABLE_COUNT = -303	      )
      integer dbFIELD_COUNT
      parameter (dbFIELD_COUNT = -304	       )
      integer dbRECORD_COUNT
      parameter (dbRECORD_COUNT = -305	      )
      integer dbDESCRIPTION
      parameter (dbDESCRIPTION = -306)
      integer dbSCHEMA_DESCRIPTION
      parameter (dbSCHEMA_DESCRIPTION = -307    )
      integer dbDATABASE_DESCRIPTION
      parameter (dbDATABASE_DESCRIPTION = -308 )
      integer dbTABLE_DESCRIPTION
      parameter (dbTABLE_DESCRIPTION = -309   )
      integer dbFIELD_DESCRIPTION
      parameter (dbFIELD_DESCRIPTION = -310  )
      integer dbDETAIL
      parameter (dbDETAIL = -311)
      integer dbSCHEMA_DETAIL
      parameter (dbSCHEMA_DETAIL = -312	       )
      integer dbDATABASE_DETAIL
      parameter (dbDATABASE_DETAIL = -313	      )
      integer dbTABLE_DETAIL
      parameter (dbTABLE_DETAIL = -314	     )
      integer dbFIELD_DETAIL
      parameter (dbFIELD_DETAIL = -315	    )
      integer dbNAME
      parameter (dbNAME = -316)
      integer dbSCHEMA_NAME
      parameter (dbSCHEMA_NAME = -317	       )
      integer dbDATABASE_NAME
      parameter (dbDATABASE_NAME = -318	      )
      integer dbTABLE_NAME
      parameter (dbTABLE_NAME = -319	     )
      integer dbFIELD_NAME
      parameter (dbFIELD_NAME = -320	    )
      integer dbTABLE_PRESENT
      parameter (dbTABLE_PRESENT = -321)
      integer dbSIZE
      parameter (dbSIZE = -322)
      integer dbTABLE_SIZE
      parameter (dbTABLE_SIZE = -323	   )
      integer dbFIELD_SIZE
      parameter (dbFIELD_SIZE = -324	  )
      integer dbFIELD_TYPE
      parameter (dbFIELD_TYPE = -328	  )
      integer dbTABLE_FILENAME
      parameter (dbTABLE_FILENAME = -329)
      integer dbDBPATH
      parameter (dbDBPATH = -330)
      integer dbTABLE_DIRNAME
      parameter (dbTABLE_DIRNAME = -331)
      integer dbPRIMARY_KEY
      parameter (dbPRIMARY_KEY = -332)
      integer dbALTERNATE_KEY
      parameter (dbALTERNATE_KEY = -333)
      integer dbFOREIGN_KEYS
      parameter (dbFOREIGN_KEYS = -334)
      integer dbUNIQUE_ID
      parameter (dbUNIQUE_ID = -335)
      integer dbUNIQUE_ID_NAME
      parameter (dbUNIQUE_ID_NAME = -336)
      integer dbSINGLE
      parameter (dbSINGLE = -337)
      integer dbSCHEMA_DEFAULT
      parameter (dbSCHEMA_DEFAULT = -338)
      integer dbDBPATH_DEFAULT
      parameter (dbDBPATH_DEFAULT = -339)
      integer dbLOAD_DATE
      parameter (dbLOAD_DATE = -340)
      integer dbTYPE
      parameter (dbTYPE = -325)
      integer dbFORMAT
      parameter (dbFORMAT = -326)
      integer dbUNITS
      parameter (dbUNITS = -327)
      integer dbDATABASE
      parameter (dbDATABASE = -401)
      integer dbVIEW
      parameter (dbVIEW = -402)
      integer dbTABLE
      parameter (dbTABLE = -403)
      integer dbFIELD
      parameter (dbFIELD = -404)
      integer dbRECORD
      parameter (dbRECORD = -405)
      integer dbMERGE
      parameter (dbMERGE = -406)
      integer dbALL
      parameter (dbALL = -501)
      integer dbPRIMARY
      parameter (dbPRIMARY = -502)
      integer dbALTERNATE
      parameter (dbALTERNATE = -503)
      integer dbSCRATCH
      parameter (dbSCRATCH = -504)
      integer dbNULL
      parameter (dbNULL = -505)
      integer dbBOOLEAN
      parameter (dbBOOLEAN = 1)
      integer dbINTEGER
      parameter (dbINTEGER = 2)
      integer dbREAL
      parameter (dbREAL = 3)
      integer dbTIME
      parameter (dbTIME = 4)
      integer dbYEARDAY
      parameter (dbYEARDAY = 5)
      integer dbSTRING
      parameter (dbSTRING = 6)
      integer dbDATE
      parameter (dbDATE = 7)
      integer dbWAVEFORM
      parameter (dbWAVEFORM = 136)
      integer dbRESPONSE
      parameter (dbRESPONSE = 137)
      integer dbBFLOAT
      parameter (dbBFLOAT = 138)
      integer dbBDOUBLE
      parameter (dbBDOUBLE = 139)
      integer dbBSHORT
      parameter (dbBSHORT = 140)
      integer dbBINT
      parameter (dbBINT = 141)
      integer dbDBPTR
      parameter (dbDBPTR = 142)
      integer dbUNIQUE
      parameter (dbUNIQUE = 1)
      integer dbOUTER_JOIN
      parameter (dbOUTER_JOIN = 2)
      integer dbadd
      integer dbaddnull
      integer dbadd_remark
      integer dbaddv
      integer dbcompile
      integer dbcreate
      integer dbcrunch
      integer dbdelete
      integer dbdestroy
      integer dbex_compile
      integer dbex_eval
      integer dbex_free
      integer dbextfile
      integer dbfilename
      integer dbfind_join_keys
      integer dbget
      integer dbget_remark
      integer dbgetv
      integer dbis_expression
      integer dbmark
      integer dbnextid
      integer dbopen
      integer dbopen_database
      integer dbopen_table
      integer dbput
      integer dbputv
      integer dbputx
      integer dbread_view
      integer dbsave_view
      integer dbset
      integer dbtruncate
      integer dbunjoin
      integer dbwrite_view
# 4 "/opt/antelope/5.2-64/include/dbl2.i" 2 
      character *(*) ATTRID_ADATE
      character *(*) ATTRID_ALGID
      character *(*) ATTRID_ALGORITHM
      character *(*) ATTRID_AMP
      character *(*) ATTRID_ARID
      character *(*) ATTRID_ARRAY
      character *(*) ATTRID_ATTRIB
      character *(*) ATTRID_ATYPE
      character *(*) ATTRID_AUTH
      character *(*) ATTRID_AZDEF
      character *(*) ATTRID_AZIMUTH
      character *(*) ATTRID_AZRES
      character *(*) ATTRID_BAND
      character *(*) ATTRID_BANDW
      character *(*) ATTRID_BAZIM
      character *(*) ATTRID_BEAMID
      character *(*) ATTRID_BELIEF
      character *(*) ATTRID_BESTDC
      character *(*) ATTRID_BLKFAC
      character *(*) ATTRID_BMTYP
      character *(*) ATTRID_BSLOW
      character *(*) ATTRID_CALIB
      character *(*) ATTRID_CALPER
      character *(*) ATTRID_CALRATIO
      character *(*) ATTRID_CDATE
      character *(*) ATTRID_CDPERR
      character *(*) ATTRID_CFREQ
      character *(*) ATTRID_CHAN
      character *(*) ATTRID_CHANA
      character *(*) ATTRID_CHANID
      character *(*) ATTRID_CHANO
      character *(*) ATTRID_CHID
      character *(*) ATTRID_CLAERR
      character *(*) ATTRID_CLIP
      character *(*) ATTRID_CLOERR
      character *(*) ATTRID_CMPX
      character *(*) ATTRID_CODA
      character *(*) ATTRID_CODDES
      character *(*) ATTRID_CODE
      character *(*) ATTRID_COLDEP
      character *(*) ATTRID_COLDIA
      character *(*) ATTRID_COLINT
      character *(*) ATTRID_COLVOL
      character *(*) ATTRID_COMM
      character *(*) ATTRID_COMMID
      character *(*) ATTRID_COMPA
      character *(*) ATTRID_CONF
      character *(*) ATTRID_COTERR
      character *(*) ATTRID_CTYPE
      character *(*) ATTRID_CUREV
      character *(*) ATTRID_CUROR
      character *(*) ATTRID_DATATYPE
      character *(*) ATTRID_DATE
      character *(*) ATTRID_DATSW
      character *(*) ATTRID_DATTYP
      character *(*) ATTRID_DAY
      character *(*) ATTRID_DEAST
      character *(*) ATTRID_DELAZ
      character *(*) ATTRID_DELSLO
      character *(*) ATTRID_DELTA
      character *(*) ATTRID_DELTIM
      character *(*) ATTRID_DEPDP
      character *(*) ATTRID_DEPTH
      character *(*) ATTRID_DESCR
      character *(*) ATTRID_DESCRIP
      character *(*) ATTRID_DFILE
      character *(*) ATTRID_DIG
      character *(*) ATTRID_DIGITAL
      character *(*) ATTRID_DIP1
      character *(*) ATTRID_DIP2
      character *(*) ATTRID_DIR
      character *(*) ATTRID_DIST
      character *(*) ATTRID_DLID
      character *(*) ATTRID_DNORTH
      character *(*) ATTRID_DOY
      character *(*) ATTRID_DTYPE
      character *(*) ATTRID_DURAT
      character *(*) ATTRID_DUSED
      character *(*) ATTRID_EDATE
      character *(*) ATTRID_EDEPTH
      character *(*) ATTRID_ELEV
      character *(*) ATTRID_EMA
      character *(*) ATTRID_EMARES
      character *(*) ATTRID_ENDTIME
      character *(*) ATTRID_ESAZ
      character *(*) ATTRID_ETYPE
      character *(*) ATTRID_EVID
      character *(*) ATTRID_EVNAME
      character *(*) ATTRID_EXLAT
      character *(*) ATTRID_EXLON
      character *(*) ATTRID_EXPTYP
      character *(*) ATTRID_FILE
      character *(*) ATTRID_FILES
      character *(*) ATTRID_FILTID
      character *(*) ATTRID_FILTYP
      character *(*) ATTRID_FKID
      character *(*) ATTRID_FKQUAL
      character *(*) ATTRID_FKRID
      character *(*) ATTRID_FKTYP
      character *(*) ATTRID_FM
      character *(*) ATTRID_FNORM
      character *(*) ATTRID_FOFF
      character *(*) ATTRID_FSID
      character *(*) ATTRID_FSRID
      character *(*) ATTRID_FSTAT
      character *(*) ATTRID_FSTYP
      character *(*) ATTRID_FTID
      character *(*) ATTRID_GNORM
      character *(*) ATTRID_GRN
      character *(*) ATTRID_GRNAME
      character *(*) ATTRID_HANG
      character *(*) ATTRID_HICUT
      character *(*) ATTRID_HSLOPE
      character *(*) ATTRID_IDTYPE
      character *(*) ATTRID_IDVALUE
      character *(*) ATTRID_IMB
      character *(*) ATTRID_IML
      character *(*) ATTRID_IMS
      character *(*) ATTRID_INID
      character *(*) ATTRID_INSNAME
      character *(*) ATTRID_INSTANT
      character *(*) ATTRID_INSTYP
      character *(*) ATTRID_INSTYPE
      character *(*) ATTRID_INTSCL
      character *(*) ATTRID_IPHASE
      character *(*) ATTRID_JDATE
      character *(*) ATTRID_LAT
      character *(*) ATTRID_LDATE
      character *(*) ATTRID_LDDATE
      character *(*) ATTRID_LEAP
      character *(*) ATTRID_LINENO
      character *(*) ATTRID_LOCATION
      character *(*) ATTRID_LOCUT
      character *(*) ATTRID_LOGAT
      character *(*) ATTRID_LON
      character *(*) ATTRID_LSLOPE
      character *(*) ATTRID_LTYPE
      character *(*) ATTRID_MAG
      character *(*) ATTRID_MAGB
      character *(*) ATTRID_MAGID
      character *(*) ATTRID_MAGLR
      character *(*) ATTRID_MAGNITUDE
      character *(*) ATTRID_MAGSH
      character *(*) ATTRID_MAGTYPE
      character *(*) ATTRID_MAXBLK
      character *(*) ATTRID_MAXF
      character *(*) ATTRID_MAXINT
      character *(*) ATTRID_MAXKX
      character *(*) ATTRID_MAXKY
      character *(*) ATTRID_MAXSX
      character *(*) ATTRID_MAXSY
      character *(*) ATTRID_MB
      character *(*) ATTRID_MBID
      character *(*) ATTRID_MEDIUM
      character *(*) ATTRID_MEXPON
      character *(*) ATTRID_MFF
      character *(*) ATTRID_MFFERR
      character *(*) ATTRID_MINBLK
      character *(*) ATTRID_ML
      character *(*) ATTRID_MLID
      character *(*) ATTRID_MNAME
      character *(*) ATTRID_MO
      character *(*) ATTRID_MOAUTH
      character *(*) ATTRID_MOIST
      character *(*) ATTRID_MON
      character *(*) ATTRID_MRF
      character *(*) ATTRID_MRFERR
      character *(*) ATTRID_MRR
      character *(*) ATTRID_MRRERR
      character *(*) ATTRID_MRT
      character *(*) ATTRID_MRTERR
      character *(*) ATTRID_MS
      character *(*) ATTRID_MSID
      character *(*) ATTRID_MTF
      character *(*) ATTRID_MTFERR
      character *(*) ATTRID_MTT
      character *(*) ATTRID_MTTERR
      character *(*) ATTRID_NAME
      character *(*) ATTRID_NASS
      character *(*) ATTRID_NAXAZM
      character *(*) ATTRID_NAXPLG
      character *(*) ATTRID_NAXVAL
      character *(*) ATTRID_NBYTE
      character *(*) ATTRID_NCALIB
      character *(*) ATTRID_NCALPER
      character *(*) ATTRID_NDEF
      character *(*) ATTRID_NDLID
      character *(*) ATTRID_NDP
      character *(*) ATTRID_NET
      character *(*) ATTRID_NETNAME
      character *(*) ATTRID_NETTYPE
      character *(*) ATTRID_NETWRK
      character *(*) ATTRID_NF
      character *(*) ATTRID_NMB
      character *(*) ATTRID_NMO
      character *(*) ATTRID_NMS
      character *(*) ATTRID_NORID
      character *(*) ATTRID_NOWFT
      character *(*) ATTRID_NRLPB
      character *(*) ATTRID_NRMW
      character *(*) ATTRID_NSAMP
      character *(*) ATTRID_NSLPB
      character *(*) ATTRID_NSMW
      character *(*) ATTRID_NSTA
      character *(*) ATTRID_NX
      character *(*) ATTRID_NXALG
      character *(*) ATTRID_NXARID
      character *(*) ATTRID_NXCHID
      character *(*) ATTRID_NXCOMM
      character *(*) ATTRID_NXDLID
      character *(*) ATTRID_NXEVID
      character *(*) ATTRID_NXFILT
      character *(*) ATTRID_NXFK
      character *(*) ATTRID_NXFKR
      character *(*) ATTRID_NXFS
      character *(*) ATTRID_NXFSR
      character *(*) ATTRID_NXFTID
      character *(*) ATTRID_NXINID
      character *(*) ATTRID_NXORID
      character *(*) ATTRID_NXSENS
      character *(*) ATTRID_NXSITE
      character *(*) ATTRID_NXSPRO
      character *(*) ATTRID_NXWFID
      character *(*) ATTRID_NY
      character *(*) ATTRID_OFFDAT
      character *(*) ATTRID_OFFDATE
      character *(*) ATTRID_ONDATE
      character *(*) ATTRID_OPSW
      character *(*) ATTRID_OPTYP
      character *(*) ATTRID_ORID
      character *(*) ATTRID_PALDEP
      character *(*) ATTRID_PAXAZM
      character *(*) ATTRID_PAXPLG
      character *(*) ATTRID_PAXVAL
      character *(*) ATTRID_PCHID
      character *(*) ATTRID_PDLID
      character *(*) ATTRID_PER
      character *(*) ATTRID_PHASE
      character *(*) ATTRID_PLPREF
      character *(*) ATTRID_PNAME
      character *(*) ATTRID_PORID
      character *(*) ATTRID_PREFOR
      character *(*) ATTRID_PVALUE
      character *(*) ATTRID_QUAL
      character *(*) ATTRID_RECT
      character *(*) ATTRID_REELSZ
      character *(*) ATTRID_REFSTA
      character *(*) ATTRID_REGION
      character *(*) ATTRID_REL
      character *(*) ATTRID_REMARK
      character *(*) ATTRID_RESID
      character *(*) ATTRID_RIPPLE
      character *(*) ATTRID_RSPTYP
      character *(*) ATTRID_RSPTYPE
      character *(*) ATTRID_SAMPRATE
      character *(*) ATTRID_SDDP
      character *(*) ATTRID_SDEPTH
      character *(*) ATTRID_SDMB
      character *(*) ATTRID_SDMO
      character *(*) ATTRID_SDMS
      character *(*) ATTRID_SDOBS
      character *(*) ATTRID_SDZDP
      character *(*) ATTRID_SEAZ
      character *(*) ATTRID_SEGTYP
      character *(*) ATTRID_SEGTYPE
      character *(*) ATTRID_SENSID
      character *(*) ATTRID_SITEID
      character *(*) ATTRID_SLIP1
      character *(*) ATTRID_SLIP2
      character *(*) ATTRID_SLODEF
      character *(*) ATTRID_SLORES
      character *(*) ATTRID_SLOW
      character *(*) ATTRID_SMAJAX
      character *(*) ATTRID_SMINAX
      character *(*) ATTRID_SMPRAT
      character *(*) ATTRID_SNAME
      character *(*) ATTRID_SNR
      character *(*) ATTRID_SPAUTH
      character *(*) ATTRID_SPMM
      character *(*) ATTRID_SPROID
      character *(*) ATTRID_SPRT
      character *(*) ATTRID_SPVT
      character *(*) ATTRID_SRN
      character *(*) ATTRID_SRNAME
      character *(*) ATTRID_STA
      character *(*) ATTRID_STAA
      character *(*) ATTRID_STANAM
      character *(*) ATTRID_STANAME
      character *(*) ATTRID_STAO
      character *(*) ATTRID_STASSID
      character *(*) ATTRID_STATYPE
      character *(*) ATTRID_STAV
      character *(*) ATTRID_STID
      character *(*) ATTRID_STIME
      character *(*) ATTRID_STR1
      character *(*) ATTRID_STR2
      character *(*) ATTRID_STRIKE
      character *(*) ATTRID_STRING
      character *(*) ATTRID_STT
      character *(*) ATTRID_STX
      character *(*) ATTRID_STY
      character *(*) ATTRID_STYPE
      character *(*) ATTRID_STZ
      character *(*) ATTRID_SXX
      character *(*) ATTRID_SXY
      character *(*) ATTRID_SXZ
      character *(*) ATTRID_SYY
      character *(*) ATTRID_SYZ
      character *(*) ATTRID_SZZ
      character *(*) ATTRID_TAGID
      character *(*) ATTRID_TAGNAME
      character *(*) ATTRID_TAPEBLOCK
      character *(*) ATTRID_TAPEFILE
      character *(*) ATTRID_TARNAM
      character *(*) ATTRID_TAXAZM
      character *(*) ATTRID_TAXPLG
      character *(*) ATTRID_TAXVAL
      character *(*) ATTRID_TCALIB
      character *(*) ATTRID_TDENSE
      character *(*) ATTRID_TEXT
      character *(*) ATTRID_TFILE
      character *(*) ATTRID_TIME
      character *(*) ATTRID_TIMEDEF
      character *(*) ATTRID_TIMERES
      character *(*) ATTRID_TLEN
      character *(*) ATTRID_TMFC
      character *(*) ATTRID_TMFI
      character *(*) ATTRID_TMNLPB
      character *(*) ATTRID_TMNMW
      character *(*) ATTRID_TPBLCK
      character *(*) ATTRID_TPFILE
      character *(*) ATTRID_TPTYPE
      character *(*) ATTRID_TRATBL
      character *(*) ATTRID_TSHIFT
      character *(*) ATTRID_TSITE
      character *(*) ATTRID_TUPID
      character *(*) ATTRID_UNCERTAINTY
      character *(*) ATTRID_USEDFT
      character *(*) ATTRID_VANG
      character *(*) ATTRID_VELID
      character *(*) ATTRID_VMODEL
      character *(*) ATTRID_VOLNAM
      character *(*) ATTRID_VOLNAME
      character *(*) ATTRID_WATDEP
      character *(*) ATTRID_WFID
      character *(*) ATTRID_WGT
      character *(*) ATTRID_YEAR
      character *(*) ATTRID_YIELD
      character *(*) ATTRID_YLDMAX
      character *(*) RELID_AFFILIATION
      character *(*) RELID_ALIAS
      character *(*) RELID_ARRIVAL
      character *(*) RELID_ASSOC
      character *(*) RELID_BEAM
      character *(*) RELID_CENTRYD
      character *(*) RELID_CHANNEL
      character *(*) RELID_CHOPER
      character *(*) RELID_CODE
      character *(*) RELID_COMMENT
      character *(*) RELID_COUNTER
      character *(*) RELID_DATE
      character *(*) RELID_DAY
      character *(*) RELID_DETECTION
      character *(*) RELID_DETLOC
      character *(*) RELID_EVENT
      character *(*) RELID_EVWF
      character *(*) RELID_EXPLO
      character *(*) RELID_EXTRA
      character *(*) RELID_FEATURE
      character *(*) RELID_FILTER
      character *(*) RELID_FKDISC
      character *(*) RELID_FKREC
      character *(*) RELID_FPLANE
      character *(*) RELID_FSDISC
      character *(*) RELID_FSREC
      character *(*) RELID_GREGION
      character *(*) RELID_INSTRUMENT
      character *(*) RELID_LASTID
      character *(*) RELID_LOC
      character *(*) RELID_MOMENT
      character *(*) RELID_NETMAG
      character *(*) RELID_NETWORK
      character *(*) RELID_ORIGERR
      character *(*) RELID_ORIGIN
      character *(*) RELID_REMARK
      character *(*) RELID_SENSOR
      character *(*) RELID_SIGPRO
      character *(*) RELID_SITE
      character *(*) RELID_SITECHAN
      character *(*) RELID_SREGION
      character *(*) RELID_STALOG
      character *(*) RELID_STAMAG
      character *(*) RELID_STASSOC
      character *(*) RELID_STATION
      character *(*) RELID_TAPE
      character *(*) RELID_WFDISC
      character *(*) RELID_WFTAG
      character *(*) RELID_WFTAPE
      character *(*) RELID_WFTAR
      character *(*) RELID_XPARAM
      integer DBL_ASCII
      integer DBL_BINARY
      integer DBL_DBL
      integer DBL_FLT
      integer DBL_INT
      integer DBL_STR
      integer NUMBER_RELS30
      integer NUMBER_ATTRS
      integer NUMBER_ATTRS30
      parameter (ATTRID_ADATE = "adate"		)
      parameter (ATTRID_ALGID = "algid"		)
      parameter (ATTRID_ALGORITHM = "algorithm"	)
      parameter (ATTRID_AMP = "amp"		)
      parameter (ATTRID_ARID = "arid"		)
      parameter (ATTRID_ARRAY = "array"		)
      parameter (ATTRID_ATTRIB = "attrib"	)
      parameter (ATTRID_ATYPE = "atype"		)
      parameter (ATTRID_AUTH = "auth"		)
      parameter (ATTRID_AZDEF = "azdef"		)
      parameter (ATTRID_AZIMUTH = "azimuth"	)
      parameter (ATTRID_AZRES = "azres"		)
      parameter (ATTRID_BAND = "band"		)
      parameter (ATTRID_BANDW = "bandw"		)
      parameter (ATTRID_BAZIM = "bazim"		)
      parameter (ATTRID_BEAMID = "beamid"	)
      parameter (ATTRID_BELIEF = "belief"	)
      parameter (ATTRID_BESTDC = "bestdc"	)
      parameter (ATTRID_BLKFAC = "blkfac"	)
      parameter (ATTRID_BMTYP = "bmtyp"		)
      parameter (ATTRID_BSLOW = "bslow"		)
      parameter (ATTRID_CALIB = "calib"		)
      parameter (ATTRID_CALPER = "calper"	)
      parameter (ATTRID_CALRATIO = "calratio"	)
      parameter (ATTRID_CDATE = "cdate"		)
      parameter (ATTRID_CDPERR = "cdperr"	)
      parameter (ATTRID_CFREQ = "cfreq"		)
      parameter (ATTRID_CHAN = "chan"		)
      parameter (ATTRID_CHANA = "chana"		)
      parameter (ATTRID_CHANID = "chanid"	)
      parameter (ATTRID_CHANO = "chano"		)
      parameter (ATTRID_CHID = "chid"		)
      parameter (ATTRID_CLAERR = "claerr"	)
      parameter (ATTRID_CLIP = "clip"		)
      parameter (ATTRID_CLOERR = "cloerr"	)
      parameter (ATTRID_CMPX = "cmpx"		)
      parameter (ATTRID_CODA = "coda"		)
      parameter (ATTRID_CODDES = "coddes"	)
      parameter (ATTRID_CODE = "code"		)
      parameter (ATTRID_COLDEP = "coldep"	)
      parameter (ATTRID_COLDIA = "coldia"	)
      parameter (ATTRID_COLINT = "colint"	)
      parameter (ATTRID_COLVOL = "colvol"	)
      parameter (ATTRID_COMM = "comm"		)
      parameter (ATTRID_COMMID = "commid"	)
      parameter (ATTRID_COMPA = "compa"		)
      parameter (ATTRID_CONF = "conf"		)
      parameter (ATTRID_COTERR = "coterr"	)
      parameter (ATTRID_CTYPE = "ctype"		)
      parameter (ATTRID_CUREV = "curev"		)
      parameter (ATTRID_CUROR = "curor"		)
      parameter (ATTRID_DATATYPE = "datatype"	)
      parameter (ATTRID_DATE = "date"		)
      parameter (ATTRID_DATSW = "datsw"		)
      parameter (ATTRID_DATTYP = "dattyp"	)
      parameter (ATTRID_DAY = "day"		)
      parameter (ATTRID_DEAST = "deast"		)
      parameter (ATTRID_DELAZ = "delaz"		)
      parameter (ATTRID_DELSLO = "delslo"	)
      parameter (ATTRID_DELTA = "delta"		)
      parameter (ATTRID_DELTIM = "deltim"	)
      parameter (ATTRID_DEPDP = "depdp"		)
      parameter (ATTRID_DEPTH = "depth"		)
      parameter (ATTRID_DESCR = "descr"		)
      parameter (ATTRID_DESCRIP = "descrip"	)
      parameter (ATTRID_DFILE = "dfile"		)
      parameter (ATTRID_DIG = "dig"		)
      parameter (ATTRID_DIGITAL = "digital"	)
      parameter (ATTRID_DIP1 = "dip1"		)
      parameter (ATTRID_DIP2 = "dip2"		)
      parameter (ATTRID_DIR = "dir"		)
      parameter (ATTRID_DIST = "dist"		)
      parameter (ATTRID_DLID = "dlid"		)
      parameter (ATTRID_DNORTH = "dnorth"	)
      parameter (ATTRID_DOY = "doy"		)
      parameter (ATTRID_DTYPE = "dtype"		)
      parameter (ATTRID_DURAT = "durat"		)
      parameter (ATTRID_DUSED = "dused"		)
      parameter (ATTRID_EDATE = "edate"		)
      parameter (ATTRID_EDEPTH = "edepth"	)
      parameter (ATTRID_ELEV = "elev"		)
      parameter (ATTRID_EMA = "ema"		)
      parameter (ATTRID_EMARES = "emares"	)
      parameter (ATTRID_ENDTIME = "endtime"	)
      parameter (ATTRID_ESAZ = "esaz"		)
      parameter (ATTRID_ETYPE = "etype"		)
      parameter (ATTRID_EVID = "evid"		)
      parameter (ATTRID_EVNAME = "evname"	)
      parameter (ATTRID_EXLAT = "exlat"		)
      parameter (ATTRID_EXLON = "exlon"		)
      parameter (ATTRID_EXPTYP = "exptyp"	)
      parameter (ATTRID_FILE = "file"		)
      parameter (ATTRID_FILES = "files"		)
      parameter (ATTRID_FILTID = "filtid"	)
      parameter (ATTRID_FILTYP = "filtyp"	)
      parameter (ATTRID_FKID = "fkid"		)
      parameter (ATTRID_FKQUAL = "fkqual"	)
      parameter (ATTRID_FKRID = "fkrid"		)
      parameter (ATTRID_FKTYP = "fktyp"		)
      parameter (ATTRID_FM = "fm"		)
      parameter (ATTRID_FNORM = "fnorm"		)
      parameter (ATTRID_FOFF = "foff"		)
      parameter (ATTRID_FSID = "fsid"		)
      parameter (ATTRID_FSRID = "fsrid"		)
      parameter (ATTRID_FSTAT = "fstat"		)
      parameter (ATTRID_FSTYP = "fstyp"		)
      parameter (ATTRID_FTID = "ftid"		)
      parameter (ATTRID_GNORM = "gnorm"		)
      parameter (ATTRID_GRN = "grn"		)
      parameter (ATTRID_GRNAME = "grname"	)
      parameter (ATTRID_HANG = "hang"		)
      parameter (ATTRID_HICUT = "hicut"		)
      parameter (ATTRID_HSLOPE = "hslope"	)
      parameter (ATTRID_IDTYPE = "idtype"	)
      parameter (ATTRID_IDVALUE = "idvalue"	)
      parameter (ATTRID_IMB = "imb"		)
      parameter (ATTRID_IML = "iml"		)
      parameter (ATTRID_IMS = "ims"		)
      parameter (ATTRID_INID = "inid"		)
      parameter (ATTRID_INSNAME = "insname"	)
      parameter (ATTRID_INSTANT = "instant"	)
      parameter (ATTRID_INSTYP = "instyp"	)
      parameter (ATTRID_INSTYPE = "instype"	)
      parameter (ATTRID_INTSCL = "intscl"	)
      parameter (ATTRID_IPHASE = "iphase"	)
      parameter (ATTRID_JDATE = "jdate"		)
      parameter (ATTRID_LAT = "lat"		)
      parameter (ATTRID_LDATE = "ldate"		)
      parameter (ATTRID_LDDATE = "lddate"	)
      parameter (ATTRID_LEAP = "leap"		)
      parameter (ATTRID_LINENO = "lineno"	)
      parameter (ATTRID_LOCATION = "location"	)
      parameter (ATTRID_LOCUT = "locut"		)
      parameter (ATTRID_LOGAT = "logat"		)
      parameter (ATTRID_LON = "lon"		)
      parameter (ATTRID_LSLOPE = "lslope"	)
      parameter (ATTRID_LTYPE = "ltype"		)
      parameter (ATTRID_MAG = "mag"		)
      parameter (ATTRID_MAGB = "magb"		)
      parameter (ATTRID_MAGID = "magid"		)
      parameter (ATTRID_MAGLR = "maglr"		)
      parameter (ATTRID_MAGNITUDE = "magnitude"	)
      parameter (ATTRID_MAGSH = "magsh"		)
      parameter (ATTRID_MAGTYPE = "magtype"	)
      parameter (ATTRID_MAXBLK = "maxblk"	)
      parameter (ATTRID_MAXF = "maxf"		)
      parameter (ATTRID_MAXINT = "maxint"	)
      parameter (ATTRID_MAXKX = "maxkx"		)
      parameter (ATTRID_MAXKY = "maxky"		)
      parameter (ATTRID_MAXSX = "maxsx"		)
      parameter (ATTRID_MAXSY = "maxsy"		)
      parameter (ATTRID_MB = "mb"		)
      parameter (ATTRID_MBID = "mbid"		)
      parameter (ATTRID_MEDIUM = "medium"	)
      parameter (ATTRID_MEXPON = "mexpon"	)
      parameter (ATTRID_MFF = "mff"		)
      parameter (ATTRID_MFFERR = "mfferr"	)
      parameter (ATTRID_MINBLK = "minblk"	)
      parameter (ATTRID_ML = "ml"		)
      parameter (ATTRID_MLID = "mlid"		)
      parameter (ATTRID_MNAME = "mname"		)
      parameter (ATTRID_MO = "mo"		)
      parameter (ATTRID_MOAUTH = "moauth"	)
      parameter (ATTRID_MOIST = "moist"		)
      parameter (ATTRID_MON = "mon"		)
      parameter (ATTRID_MRF = "mrf"		)
      parameter (ATTRID_MRFERR = "mrferr"	)
      parameter (ATTRID_MRR = "mrr"		)
      parameter (ATTRID_MRRERR = "mrrerr"	)
      parameter (ATTRID_MRT = "mrt"		)
      parameter (ATTRID_MRTERR = "mrterr"	)
      parameter (ATTRID_MS = "ms"		)
      parameter (ATTRID_MSID = "msid"		)
      parameter (ATTRID_MTF = "mtf"		)
      parameter (ATTRID_MTFERR = "mtferr"	)
      parameter (ATTRID_MTT = "mtt"		)
      parameter (ATTRID_MTTERR = "mtterr"	)
      parameter (ATTRID_NAME = "name"		)
      parameter (ATTRID_NASS = "nass"		)
      parameter (ATTRID_NAXAZM = "naxazm"	)
      parameter (ATTRID_NAXPLG = "naxplg"	)
      parameter (ATTRID_NAXVAL = "naxval"	)
      parameter (ATTRID_NBYTE = "nbyte"		)
      parameter (ATTRID_NCALIB = "ncalib"	)
      parameter (ATTRID_NCALPER = "ncalper"	)
      parameter (ATTRID_NDEF = "ndef"		)
      parameter (ATTRID_NDLID = "ndlid"		)
      parameter (ATTRID_NDP = "ndp"		)
      parameter (ATTRID_NET = "net"		)
      parameter (ATTRID_NETNAME = "netname"	)
      parameter (ATTRID_NETTYPE = "nettype"	)
      parameter (ATTRID_NETWRK = "netwrk"	)
      parameter (ATTRID_NF = "nf"		)
      parameter (ATTRID_NMB = "nmb"		)
      parameter (ATTRID_NMO = "nmo"		)
      parameter (ATTRID_NMS = "nms"		)
      parameter (ATTRID_NORID = "norid"		)
      parameter (ATTRID_NOWFT = "nowft"		)
      parameter (ATTRID_NRLPB = "nrlpb"		)
      parameter (ATTRID_NRMW = "nrmw"		)
      parameter (ATTRID_NSAMP = "nsamp"		)
      parameter (ATTRID_NSLPB = "nslpb"		)
      parameter (ATTRID_NSMW = "nsmw"		)
      parameter (ATTRID_NSTA = "nsta"		)
      parameter (ATTRID_NX = "nx"		)
      parameter (ATTRID_NXALG = "nxalg"		)
      parameter (ATTRID_NXARID = "nxarid"	)
      parameter (ATTRID_NXCHID = "nxchid"	)
      parameter (ATTRID_NXCOMM = "nxcomm"	)
      parameter (ATTRID_NXDLID = "nxdlid"	)
      parameter (ATTRID_NXEVID = "nxevid"	)
      parameter (ATTRID_NXFILT = "nxfilt"	)
      parameter (ATTRID_NXFK = "nxfk"		)
      parameter (ATTRID_NXFKR = "nxfkr"		)
      parameter (ATTRID_NXFS = "nxfs"		)
      parameter (ATTRID_NXFSR = "nxfsr"		)
      parameter (ATTRID_NXFTID = "nxftid"	)
      parameter (ATTRID_NXINID = "nxinid"	)
      parameter (ATTRID_NXORID = "nxorid"	)
      parameter (ATTRID_NXSENS = "nxsens"	)
      parameter (ATTRID_NXSITE = "nxsite"	)
      parameter (ATTRID_NXSPRO = "nxspro"	)
      parameter (ATTRID_NXWFID = "nxwfid"	)
      parameter (ATTRID_NY = "ny"		)
      parameter (ATTRID_OFFDAT = "offdat"	)
      parameter (ATTRID_OFFDATE = "offdate"	)
      parameter (ATTRID_ONDATE = "ondate"	)
      parameter (ATTRID_OPSW = "opsw"		)
      parameter (ATTRID_OPTYP = "optyp"		)
      parameter (ATTRID_ORID = "orid"		)
      parameter (ATTRID_PALDEP = "paldep"	)
      parameter (ATTRID_PAXAZM = "paxazm"	)
      parameter (ATTRID_PAXPLG = "paxplg"	)
      parameter (ATTRID_PAXVAL = "paxval"	)
      parameter (ATTRID_PCHID = "pchid"		)
      parameter (ATTRID_PDLID = "pdlid"		)
      parameter (ATTRID_PER = "per"		)
      parameter (ATTRID_PHASE = "phase"		)
      parameter (ATTRID_PLPREF = "plpref"	)
      parameter (ATTRID_PNAME = "pname"		)
      parameter (ATTRID_PORID = "porid"		)
      parameter (ATTRID_PREFOR = "prefor"	)
      parameter (ATTRID_PVALUE = "pvalue"	)
      parameter (ATTRID_QUAL = "qual"		)
      parameter (ATTRID_RECT = "rect"		)
      parameter (ATTRID_REELSZ = "reelsz"	)
      parameter (ATTRID_REFSTA = "refsta"	)
      parameter (ATTRID_REGION = "region"	)
      parameter (ATTRID_REL = "rel"		)
      parameter (ATTRID_REMARK = "remark"	)
      parameter (ATTRID_RESID = "resid"		)
      parameter (ATTRID_RIPPLE = "ripple"	)
      parameter (ATTRID_RSPTYP = "rsptyp"	)
      parameter (ATTRID_RSPTYPE = "rsptype"	)
      parameter (ATTRID_SAMPRATE = "samprate"	)
      parameter (ATTRID_SDDP = "sddp"		)
      parameter (ATTRID_SDEPTH = "sdepth"	)
      parameter (ATTRID_SDMB = "sdmb"		)
      parameter (ATTRID_SDMO = "sdmo"		)
      parameter (ATTRID_SDMS = "sdms"		)
      parameter (ATTRID_SDOBS = "sdobs"		)
      parameter (ATTRID_SDZDP = "sdzdp"		)
      parameter (ATTRID_SEAZ = "seaz"		)
      parameter (ATTRID_SEGTYP = "segtyp"	)
      parameter (ATTRID_SEGTYPE = "segtype"	)
      parameter (ATTRID_SENSID = "sensid"	)
      parameter (ATTRID_SITEID = "siteid"	)
      parameter (ATTRID_SLIP1 = "slip1"		)
      parameter (ATTRID_SLIP2 = "slip2"		)
      parameter (ATTRID_SLODEF = "slodef"	)
      parameter (ATTRID_SLORES = "slores"	)
      parameter (ATTRID_SLOW = "slow"		)
      parameter (ATTRID_SMAJAX = "smajax"	)
      parameter (ATTRID_SMINAX = "sminax"	)
      parameter (ATTRID_SMPRAT = "smprat"	)
      parameter (ATTRID_SNAME = "sname"		)
      parameter (ATTRID_SNR = "snr"		)
      parameter (ATTRID_SPAUTH = "spauth"	)
      parameter (ATTRID_SPMM = "spmm"		)
      parameter (ATTRID_SPROID = "sproid"	)
      parameter (ATTRID_SPRT = "sprt"		)
      parameter (ATTRID_SPVT = "spvt"		)
      parameter (ATTRID_SRN = "srn"		)
      parameter (ATTRID_SRNAME = "srname"	)
      parameter (ATTRID_STA = "sta"		)
      parameter (ATTRID_STAA = "staa"		)
      parameter (ATTRID_STANAM = "stanam"	)
      parameter (ATTRID_STANAME = "staname"	)
      parameter (ATTRID_STAO = "stao"		)
      parameter (ATTRID_STASSID = "stassid"	)
      parameter (ATTRID_STATYPE = "statype"	)
      parameter (ATTRID_STAV = "stav"		)
      parameter (ATTRID_STID = "stid"		)
      parameter (ATTRID_STIME = "stime"		)
      parameter (ATTRID_STR1 = "str1"		)
      parameter (ATTRID_STR2 = "str2"		)
      parameter (ATTRID_STRIKE = "strike"	)
      parameter (ATTRID_STRING = "string"	)
      parameter (ATTRID_STT = "stt"		)
      parameter (ATTRID_STX = "stx"		)
      parameter (ATTRID_STY = "sty"		)
      parameter (ATTRID_STYPE = "stype"		)
      parameter (ATTRID_STZ = "stz"		)
      parameter (ATTRID_SXX = "sxx"		)
      parameter (ATTRID_SXY = "sxy"		)
      parameter (ATTRID_SXZ = "sxz"		)
      parameter (ATTRID_SYY = "syy"		)
      parameter (ATTRID_SYZ = "syz"		)
      parameter (ATTRID_SZZ = "szz"		)
      parameter (ATTRID_TAGID = "tagid"		)
      parameter (ATTRID_TAGNAME = "tagname"	)
      parameter (ATTRID_TAPEBLOCK = "tapeblock"	)
      parameter (ATTRID_TAPEFILE = "tapefile"	)
      parameter (ATTRID_TARNAM = "tarnam"	)
      parameter (ATTRID_TAXAZM = "taxazm"	)
      parameter (ATTRID_TAXPLG = "taxplg"	)
      parameter (ATTRID_TAXVAL = "taxval"	)
      parameter (ATTRID_TCALIB = "tcalib"	)
      parameter (ATTRID_TDENSE = "tdense"	)
      parameter (ATTRID_TEXT = "text"		)
      parameter (ATTRID_TFILE = "tfile"		)
      parameter (ATTRID_TIME = "time"		)
      parameter (ATTRID_TIMEDEF = "timedef"	)
      parameter (ATTRID_TIMERES = "timeres"	)
      parameter (ATTRID_TLEN = "tlen"		)
      parameter (ATTRID_TMFC = "tmfc"		)
      parameter (ATTRID_TMFI = "tmfi"		)
      parameter (ATTRID_TMNLPB = "tmnlpb"	)
      parameter (ATTRID_TMNMW = "tmnmw"		)
      parameter (ATTRID_TPBLCK = "tpblck"	)
      parameter (ATTRID_TPFILE = "tpfile"	)
      parameter (ATTRID_TPTYPE = "tptype"	)
      parameter (ATTRID_TRATBL = "tratbl"	)
      parameter (ATTRID_TSHIFT = "tshift"	)
      parameter (ATTRID_TSITE = "tsite"		)
      parameter (ATTRID_TUPID = "tupid"		)
      parameter (ATTRID_UNCERTAINTY = "uncertainty"	)
      parameter (ATTRID_USEDFT = "usedft"	)
      parameter (ATTRID_VANG = "vang"		)
      parameter (ATTRID_VELID = "velid"		)
      parameter (ATTRID_VMODEL = "vmodel"	)
      parameter (ATTRID_VOLNAM = "volnam"	)
      parameter (ATTRID_VOLNAME = "volname"	)
      parameter (ATTRID_WATDEP = "watdep"	)
      parameter (ATTRID_WFID = "wfid"		)
      parameter (ATTRID_WGT = "wgt"		)
      parameter (ATTRID_YEAR = "year"		)
      parameter (ATTRID_YIELD = "yield"		)
      parameter (ATTRID_YLDMAX = "yldmax"	)
      parameter (DBL_ASCII = 0	)
      parameter (DBL_BINARY = 1	)
      parameter (DBL_DBL = 2	)
      parameter (DBL_FLT = 1	)
      parameter (DBL_INT = 0	)
      parameter (DBL_STR = 3	)
      parameter (NUMBER_ATTRS = 282		)
      parameter (NUMBER_ATTRS30 = 152		)
      parameter (NUMBER_RELS30 = 23	)
      parameter (RELID_AFFILIATION = "affiliation"	)
      parameter (RELID_ALIAS = "alias"	)
      parameter (RELID_ARRIVAL = "arrival"	)
      parameter (RELID_ASSOC = "assoc"	)
      parameter (RELID_BEAM = "beam"		)
      parameter (RELID_CENTRYD = "centryd"	)
      parameter (RELID_CHANNEL = "channel"	)
      parameter (RELID_CHOPER = "choper"	)
      parameter (RELID_CODE = "code"		)
      parameter (RELID_COMMENT = "comment"	)
      parameter (RELID_COUNTER = "counter"	)
      parameter (RELID_DATE = "date"		)
      parameter (RELID_DAY = "day"		)
      parameter (RELID_DETECTION = "detection"	)
      parameter (RELID_DETLOC = "detloc"	)
      parameter (RELID_EVENT = "event"	)
      parameter (RELID_EVWF = "evwf"		)
      parameter (RELID_EXPLO = "explo"		)
      parameter (RELID_EXTRA = "extra"		)
      parameter (RELID_FEATURE = "feature"	)
      parameter (RELID_FILTER = "filter"	)
      parameter (RELID_FKDISC = "fkdisc"	)
      parameter (RELID_FKREC = "fkrec"		)
      parameter (RELID_FPLANE = "fplane"	)
      parameter (RELID_FSDISC = "fsdisc"	)
      parameter (RELID_FSREC = "fsrec"		)
      parameter (RELID_GREGION = "gregion"	)
      parameter (RELID_INSTRUMENT = "instrument"	)
      parameter (RELID_LASTID = "lastid"	)
      parameter (RELID_LOC = "loc"		)
      parameter (RELID_MOMENT = "moment"	)
      parameter (RELID_NETMAG = "netmag"	)
      parameter (RELID_NETWORK = "network"	)
      parameter (RELID_ORIGERR = "origerr"	)
      parameter (RELID_ORIGIN = "origin"	)
      parameter (RELID_REMARK = "remark"	)
      parameter (RELID_SENSOR = "sensor"	)
      parameter (RELID_SIGPRO = "sigpro"	)
      parameter (RELID_SITE = "site"	)
      parameter (RELID_SITECHAN = "sitechan"	)
      parameter (RELID_SREGION = "sregion"	)
      parameter (RELID_STALOG = "stalog"	)
      parameter (RELID_STAMAG = "stamag"	)
      parameter (RELID_STASSOC = "stassoc"	)
      parameter (RELID_STATION = "station"	)
      parameter (RELID_TAPE = "tape"		)
      parameter (RELID_WFDISC = "wfdisc"	)
      parameter (RELID_WFTAG = "wftag"	)
      parameter (RELID_WFTAPE = "wftape"	)
      parameter (RELID_WFTAR = "wftar"	)
      parameter (RELID_XPARAM = "xparam"	)

c $Id: dbmaprelease.f,v 1.11 2010-10-26 17:02:56 mitch Exp $ 
# 814 "dbmaprelease_mac.F" 2 
c
      data  pi / 3.14159265358979323846d0 /
      data symsiz / 0.05 /
      data charevnts / ' ' /
c
c
c    Initialize plot
c
      if(inset.eq.0) then
      call initt (itran, 'dbmapevents.ps', '', 
     +                   'dbmapevents', 0.9, 0.0, 0.0)
      endif
      if (itran .eq. 0) then
        call setdim (7.5, 10.0, 0.0, 0.0)
      else
        call setdim (10.0, 7.5, 0.0, 0.0)
      end if
      call setscl (0.,1.,0.,1.)
c     call box (0.,1.,0.,1.,0.,0,0)
      if(ititl.eq.0) then
      title = 'Earthquakes in Alaska in ' // dbnam(1:ilen(dbnam))
crah +        // ', centered at ' // sta
      endif
c
c    Plot map
c
      if (idcirc .eq. 1) then
        dcircle = 0.0
      else
        dcircle = -1.0
      end if
      if (iblue.eq.1) call setbac (200.0, 0.95, 1.0)
      call edprojmap (xdim, ydim, xlow, ylow,
     +                      xlatc, xlonc, xmin, xmax,
     +                      ymin, ymax, dcircle, title)

c
c    Open event view
c
      call delbox2llbox (xlatc, xlonc, xmin, xmax, 
     +                         ymin, ymax,
     +                         xlatmn, xlatmx, xlonmn, xlonmx)
      slatmndp = xlatmn - 5.0
      slatmxdp = xlatmx + 5.0
      slonmndp = xlonmn - 5.0
      slonmxdp = xlonmx + 5.0
      if(abs(slonmndp).gt.180.0.or.abs(slonmxdp).gt.180.0) then
        slonmndp = -180.0
        slonmxdp =  180.0
      endif
      if (jorid .lt. 0) then
        if (nevs .eq. 0) then
          nevs = MAXEVENTS
          tstart = tstrt
          tend = tnd
          call evcreate ('css3.0', 1, dbnam, tstart, tend, 
     +       slatmndp, slatmxdp, slonmndp, slonmxdp, nevs, ievs)
          if (nevs .lt. 0) then
	    print *,'dbmapevents: Error return from evcreate'
          end if
          if (nevs .eq. 0) then
	    print *,'dbmapevents: No events within range'
          end if
	  if (nevs .eq. MAXEVENTS) then
	    print *,'dbmapevents: Attempt to exceed events dimension'
	  end if
        end if
      else
        if (nevs .eq. 0) then
          nevs = MAXEVENTS
	  write (tstart, '(f17.5)') evtime-0.001
	  write (tend, '(f17.5)') evtime+0.001
          slatmndp = xlatc-0.001
          slatmxdp = xlatc+0.001
          slonmndp = xlonc-0.001
          slonmxdp = xlonc+0.001
          call evcreate ('css3.0', 1, dbnam, tstart, tend, 
     +       slatmndp, slatmxdp, slonmndp, slonmxdp, nevs, ievs)
          if (nevs .lt. 0) then
	    print *,'dbmapevents: Error return from evcreate'
          end if
          if (nevs .eq. 0) then
	    print *,'dbmapevents: No events within range'
          end if
	  if (nevs .eq. MAXEVENTS) then
	    print *,'dbmapevents: Attempt to exceed events dimension'
	  end if
        end if
      end if
c
c    Plot symbol legend
c
c       charevents = '                         '
        charevnts = '                         '
        if (nevs.lt.1) then
          charevnts(2:16) = 'Event centered on map'
        elseif (nevs.lt.10) then
          charevnts(2:16) = ' events plotted'
          write(charevnts(1:1),'(i1)')nevs
        elseif (nevs.lt.100) then
          charevnts(3:17) = ' events plotted'
          write(charevnts(1:2),'(i2)')nevs
        elseif (nevs.lt.1000) then
          charevnts(4:18) = ' events plotted'
          write(charevnts(1:3),'(i3)')nevs
        elseif (nevs.lt.10000) then
          charevnts(5:19) = ' events plotted'
          write(charevnts(1:4),'(i4)')nevs
        else
          charevnts(6:20) = ' events plotted'
          write(charevnts(1:5),'(i5)')nevs
        endif
      if (inset.eq.1) then
c       call setscl (0.0, xdim, 0.0, ydim)
c       call text (0.3, 0.2, 0.0, 1, charevnts, 1)
c       call setscl (xmin, xmax, ymin, ymax)
      else
      if (jorid .lt. 0) then
      if (nauth .eq. 0) then
	call setscl (0.0, xdim, 0.0, ydim)
	call setfor (0.0, 0.0, 0.0)
	xplt1 = 0.2
	yplt1 = -0.2
crah    call text (0.5, -0.2, 0.0, 1, 'All events plotted', 1)
	call text (0.2, -0.2, 0.0, 1, charevnts, 1)
	if (ipdeplt .eq. 1) then
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (120.0, 0.5, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, 'Event in the PDE', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (240.0, 0.5, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, 'Event not in the PDE', 1)
	end if
	if (ipmag .eq. 1) then
	    if (xmb .gt. -999.0) then
	      ss = symsiz*(2.0 + 2.0*(xmb-2.0)/4.0)
	      symbl = 'hexagon'
            else
	      ss = symsiz
	      symbl = 'box'
            end if
          if (itran.eq.1) then
	    xplt1 = 8.0
          else
	    xplt1 = 6.0
          endif
	  ypltm = - 0.2
            if(icol.eq.1) then
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (240.0, 0.5, 1.0)
	  call symbol ('box', xplt1, ypltm, symsiz, 0.0, 1, 1)
            else
	      call setfor (240.0, 0.5, 1.0)
	  call symbol ('box', xplt1, ypltm, symsiz, 0.0, 1, 0)
            endif
	  call setfor (0.0, 0.0, 0.0)
          if (itran.eq.1) then
	    xplt1 = 8.3
          else
	    xplt1 = 6.3
          endif
	  call text (xplt1, ypltm, 0.0, 1, 'No M', 1)
	  do 33  i = 2, 5
            if (itran.eq.1) then
	      xplt1 = 8.0
            else
	      xplt1 = 6.0
            endif
	    ypltm = ypltm - 0.2
	    xmb = i
	    ss = symsiz*(2.0 + 2.0*(xmb-2.0)/4.0)
            if(icol.eq.1) then
	      call setbac (240.0, 0.5, 1.0)
	      call symbol ('hexagon', xplt1, ypltm, ss, 0.0, 1, 1)
            else
	      call setfor (240.0, 0.5, 1.0)
crah          sss = ss*3*xdim/(xmax-xmin)
crah          call circle (xplt1, ypltm, sss, 90, 1, 1, 0.2, 0)
	      call symbol ('circle', xplt1, ypltm, ss, 0.0, 1, 0)
            endif
	    call setfor (0.0, 0.0, 0.0)
	    call setbac (0.0, 1.0, 0.0)
            if (itran.eq.1) then
	      xplt1 = 8.3
            else
	      xplt1 = 6.3
            endif
	    write (string, '(a,f3.1)') 'M = ',xmb
	    call text (xplt1, ypltm, 0.0, 1, string, 1)
   33     continue
	end if
        write(text1,'(a9,i2)')'Depth <= ',dep1
        write(text2,'(i2,a12,i3)')dep1,' < Depth <= ',dep2
        write(text3,'(i3,a12,i3)')dep2,' < Depth <= ',dep3
        write(text4,'(a9,i3)')' Depth > ',dep3
	if (ipdepth .eq. 1) then
	  xplt1 = 0.2
c  yplt1 = yplt1 - 0.2
          call setfor (0.0, 0.0, 0.0)
c  call setbac (0.0, 0.8, 0.0)
c  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
c  call setfor (0.0, 0.0, 0.0)
c  xplt1 = 0.5
c  call text (xplt1, yplt1, 0.0, 1, 'Default depth', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (240.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text1, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (120.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text2, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (0.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text3, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (300.0, 0.9, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text4, 1)
	else if (ipdepth .eq. 4) then
	  xplt1 = 0.2
c  yplt1 = yplt1 - 0.2
	  call setfor (0.0, 0.0, 0.0)
c  call setbac (0.0, 0.8, 0.0)
c  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
c  call setfor (0.0, 0.0, 0.0)
c  xplt1 = 0.5
c  call text (xplt1, yplt1, 0.0, 1, 'Default depth', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (240.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text1, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (120.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text2, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (60.0, 0.8, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text3, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (300.0, 1.0, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text4, 1)
	else if (ipdepth .eq. 2) then
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (240.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, 'Depth <= 5', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (180.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, '5 < Depth <= 10', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (120.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, '10 < Depth <= 15', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (30.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, '15 < Depth <= 20', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (0.0, 0.5, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, 'Depth > 20', 1)
	end if
	call setscl (xmin, xmax, ymin, ymax)
      else
	call setscl (0.0, xdim, 0.0, ydim)
	if (ipdepth .eq. 3) then
	  xplt1 = 0.2
	  yplt1 = -0.4
	  call setbac (240.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, 'Delta depth < -1', 1)
	  xplt1 = 0.2
	  yplt1 = -0.6
	  call setbac (180.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1,0.0,1,'-2 < Delta depth < -0.2', 
     +               1)
	  xplt1 = 0.2
	  yplt1 = -0.8
	  call setbac (120.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1,yplt1,0.0,1,'-0.2 < Delta depth < 0.2', 
     +               1)
	  xplt1 = 0.2
	  yplt1 = -1.0
	  call setbac (30.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, '0.2 < Delta depth < 1', 
     +               1)
	  xplt1 = 0.2
	  yplt1 = -1.2
	  call setbac (0.0, 0.5, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, '1 < Delta depth', 1)
	else
	do 210  i = 1, nauth
	  call setbac (symhue(i), symlit(i), symsat(i))
	  xplt1 = 0.2
	  yplt1 = -0.2 - (i-1)*0.20
	  if (sym(i) .eq. 'NONE') then
	  else
	    call symbol (sym(i), xplt1, yplt1, symsiz, 0.0, 1, 1)
	  end if
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, auth(i), 1)
  210   continue
	end if
	call setscl (xmin, xmax, ymin, ymax)
      end if
      end if
      endif
c
c    Read in great circle paths and plot 
c
      if (ipaths .eq. 1) then
	open(23,file='paths.map')
	i=0
 235    read(23,*,end=232)xlat,xlon,xlat2,xlon2
	  i=i+1
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
	  call latlon2xydel (xlatc, xlonc, xlat2, xlon2,
     +                     xplt2, yplt2)
        call setfor (0.0, 0.0, 0.0)
	call line(xplt1,yplt1,xplt2,yplt2,0,0,.01,0)
	  go to 235
 232    continue
	close (23)
      endif
c
c    Read in fault locations and plot
c
      if (iflt .eq. 1) then
        open(23,file='/usr/local/aeic/5.2-64/data/maps/faults.lin')
        read(23,'(a10)')frmat
 245    i=0
 246    read(23,'(2f12.6)',end=247)xlat,xlon
          if(xlat.eq.0.0.and.xlon.eq.0.0) go to 242
          i=i+1
          xlon = -xlon
cRAH      if (xlon.gt.0.0)xlon = -xlon
          call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          ppltx(i) = xplt1
          pplty(i) = yplt1
          go to 246
 242    continue
        if(i.eq.0) go to 245
        npltp = i
        call setfor (40.0, 0.2, 1.0)
        call nplot(npltp,ppltx,pplty,0,0,.01,0,' ')
        go to 245
 247    continue
        close (23)
      endif
c
c    Read in pipeline locations and plot 
c
      if (ipipe .eq. 1) then
	open(23,file='/usr/local/aeic/5.2-64/data/maps/pipeline.lin')
	i=0
 225    read(23,'(f12.6,f13.6)',end=222)xlat,xlon
	  i=i+1
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
	  ppltx(i) = xplt1
	  pplty(i) = yplt1
	  go to 225
 222    continue
	npltp = i
	close (23)
        call setfor (0.0, 0.0, 0.0)
	call nplot(npltp,ppltx,pplty,0,0,.01,0,' ')
        if (ipumps .eq. 1) then
	  open(23,file='/usr/local/aeic/5.2-64/data/maps/pipepumps.dat')
 226      read(23,'(2f12.6,1x,a)',end=227)xlat,xlon,city
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          call setfor (0.0, 0.0, 0.0)
          call chrsiz (0.06, 1.0, 0.0)
          call setbac (0.0, 0.8, 0.0)
	  call symbol ('hexagon', xplt1, yplt1, 2*symsiz, 0.0, 0, 1)
          call setbac (0.0, 1.0, 0.0)
            call text (xplt1, yplt1, 0.0, 1, '  ' // city, 0)
	  go to 226
        endif
 227    continue
	close (23)
      endif
c
c    Read in city locations and plot 
c
      if (icities .eq. 1) then
	open(23,file='/usr/local/aeic/5.2-64/data/maps/cities.dat')
 224    read(23,'(2f12.6,1x,a)',end=223)xlat,xlon,city
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          call setfor (0.0, 0.0, 0.0)
          call chrsiz (0.12, 1.0, 0.0)
          call setbac (0.0, 0.8, 0.0)
	  call symbol ('box', xplt1, yplt1, 2*symsiz, 0.0, 0, 1)
          call setbac (0.0, 1.0, 0.0)
          if (icities .eq. 1) then
            call text (xplt1, yplt1, 0.0, 1, '  ' // city, 0)
          endif
	  go to 224
 223    continue
	close (23)
      endif
c
c    Read in station locations and plot 
c
      if (istaplt .eq. 1) then
        call dbgetntuples (idb, RELID_SITE, ntuples)
        call setfor (0.0, 0.0, 0.0)
        call chrsiz (0.08, 1.0, 0.0)
        do 75  i = 1, ntuples
	  call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_LON, xlon,
     +                   ATTRID_LAT, xlat,
     +                   ATTRID_STA, arg,
     +			 0)
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          call setbac (0.0, 0.9, 0.0)
	  call symbol ('triangle', xplt1, yplt1, 2*symsiz, 0.0, 0, 1)
          call setbac (0.0, 1.0, 0.0)
          if (istnam .eq. 1) then
            call text (xplt1, yplt1, 0.0, 1, '  ' // arg, 0)
          endif
   75   continue
      end if
      if (jorid .ge. 0) then
        do 400  i = 1, nevs
	  call evget (ievs(i), EV_NHYPOS, nhypos,
     +                       EV_PREFHYPO, ipref,
     +                       EV_EVID, ievid,
     +			 0)
c         print *, "Mitch 1 nhypos, ipref, ievid", nhypos, ipref, ievid
	  if (ipref .lt. 1) ipref = 1
	  call evget (ievs(i), 
     +                       EV_HYPO_ORID, ipref, iorid,
     +                       EV_HYPO_NSTAS, ipref, nstas,
     +                       0)
c         print *, "Mitch 2 ipref, iorid, nstas ", ipref, iorid, nstas
c	Mitch added the below line, It is major crap
c	evget does not return value for iorid and exits with Orid not found
          if (iorid .eq. 0 .and. nstas .gt. 0) iorid = jorid 
	  if (iorid .eq. jorid) go to 410
  400   continue
  410   continue
	if (i .gt. nevs) then
	  write (6, '(a)') 'Orid not found.'
	  stop
	end if
	do 475  j = 1, nstas
	  call evget (ievs(i), 
     +                       EV_HYPO_STA_STA, ipref, j, arg,
     +                       EV_HYPO_STA_LAT, ipref, j, slat,
     +                       EV_HYPO_STA_LON, ipref, j, slon,
     +                       EV_HYPO_STA_NASSOCS, ipref, j, nassocs,
     +                       0)
	  jflg = 0
          xlat = slat
          xlon = slon
	  do 476  k = 1, nassocs
	    call evget (ievs(i), 
     +              EV_HYPO_STA_ASSOC_ASSOCFLAG, ipref, j, k, iflg,
     +              0)
	    jflg = jflg + iflg
  476     continue
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
	  if (jflg .eq. 0) then
	    call setbac (0.0, 0.3, 0.0)
            call chrsiz (0.04, 1.0, 0.0)
	    call symbol ('cross', xplt1, yplt1, 0.8*symsiz, 0.0, 0, 1)
	    call setbac (0.0, 1.0, 0.0)
	    call text (xplt1, yplt1, 0.0, 1, '  ' // arg, 0)
	  else
	    call setbac (0.0, 0.0, 0.0)
            call chrsiz (0.06, 1.0, 0.0)
	    call symbol ('triangle', xplt1, yplt1, symsiz, 0.0, 0, 1)
	    call text (xplt1, yplt1, 0.0, 1, '  ' // arg, 0)
	    call setbac (0.0, 1.0, 0.0)
	  end if
  475   continue
      end if
c
c    Read in hypocentral data and plot it
c
      call setfor (0.0, 0.0, 0.0)
      if (jorid .lt. 0) then
      do 500  i = 1, nevs
	call evget (ievs(i), EV_NHYPOS, nhypos,
     +                       EV_PREFHYPO, ipref,
     +                       EV_EVID, ievid,
     +			 0)
        ipde = 0
        xmb = -999.0
        do 555  j = 1, nhypos
          call evget (ievs(i), EV_HYPO_AUTH, j, arg,
     +                       EV_HYPO_MB, j, ymb,
     +                       EV_HYPO_ML, j, yml,
     +                       EV_HYPO_MS, j, yms,
     +                0)
          if (yml .gt. -999.0) then
             xmb = yml
          elseif (yms .gt. -999.0) then
             xmb = yms
          elseif (ymb .gt. -999.0) then
             xmb = ymb
          endif
          if (arg .eq. 'PDE') ipde = 1
  555   continue
	iplt = 1
	if (nauth .eq. 0) then
	  i1 = ipref
	  i2 = ipref
	else
	  i1 = 1
	  i2 = nhypos
	end if
	if (nauth .gt. 1) then
	  do 491  l = 1, nauth
            do 492 k = 1, nhypos
	      call evget (ievs(i), EV_HYPO_AUTH, k, arg,
     +                       0)
              if (arg .eq. auth(l)) go to 491
  492       continue
            go to 500
  491     continue
	end if
	if (nauth .gt. 0) then
	  deptho = 0.0
	  depthc = 0.0
	  do 490 k = i1, i2
	    call evget (ievs(i), EV_HYPO_LAT, k, elat,
     +                       EV_HYPO_LON, k, elon,
     +                       EV_HYPO_DEPTH, k, depth,
     +                       EV_HYPO_AUTH, k, arg,
     +                       EV_HYPO_MB, k, ymb,
     +                       0)
            if (arg .eq. auth(1)) deptho = depth
            if (arg .eq. auth(2)) depthc = depth
  490     continue
	  dep = depthc-deptho
	end if
	do 505  k = i1, i2
	call evget (ievs(i), EV_HYPO_LAT, k, elat,
     +                       EV_HYPO_LON, k, elon,
     +                       EV_HYPO_DEPTH, k, depth,
     +                       EV_HYPO_AUTH, k, arg,
     +                       EV_HYPO_ORID, k, iorid,
     +                       EV_HYPO_MB, k, ymb,
     +                       EV_HYPO_ML, k, yml,
     +                       EV_HYPO_MS, k, yms,
     +                       EV_HYPO_NSTAS, k, nstas,
     +                       0)
          if (yml .gt. -999.0) then
             xmb = yml
          elseif (yms .gt. -999.0) then
             xmb = yms
          elseif (ymb .gt. -999.0) then
             xmb = ymb
          endif
c       xmb = nstas
	xlat = elat
	xlon = elon
	if (nauth .eq. 0) then
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          symbl = 'hexagon'
	  if (ipdepth .eq. 1) then
c    if (depth .eq. 33.0d0) then
c           if(icol.eq.1) then
c      call setbac (0.0, 0.8, 0.0)
c           else
c      call setfor (0.0, 0.8, 0.0)
c           endif
	    if (depth .le. dep1) then
            if(icol.eq.1) then
	      call setbac (240.0, 0.4, 1.0)
            else
	      call setfor (240.0, 0.4, 1.0)
            endif
	    else if (depth .le. dep2) then
            if(icol.eq.1) then
	      call setbac (120.0, 0.4, 1.0)
            else
	      call setfor (120.0, 0.4, 1.0)
            endif
	    else if (depth .le. dep3) then
            if(icol.eq.1) then
	      call setbac (0.0, 0.4, 1.0)
            else
	      call setfor (0.0, 0.4, 1.0)
            endif
	    else
            if(icol.eq.1) then
	      call setbac (300.0, 0.9, 1.0)
            else
	      call setfor (300.0, 0.9, 1.0)
            endif
	    end if
	  else if (ipdepth .eq. 4) then
c    if (depth .eq. 33.0d0) then
c           if(icol.eq.1) then
c      call setbac (0.0, 0.8, 0.0)
c           else
c      call setfor (0.0, 0.8, 0.0)
c           endif
	    if (depth .le. dep1) then
            if(icol.eq.1) then
	      call setbac (240.0, 0.4, 1.0)
            else
	      call setfor (240.0, 0.4, 1.0)
            endif
	    else if (depth .le. dep2) then
            if(icol.eq.1) then
	      call setbac (120.0, 0.4, 1.0)
            else
	      call setfor (120.0, 0.4, 1.0)
            endif
	    else if (depth .le. dep3) then
            if(icol.eq.1) then
	      call setbac (60.0, 0.8, 1.0)
            else
	      call setfor (60.0, 0.8, 1.0)
            endif
	    else
            if(icol.eq.1) then
	      call setbac (300.0, 1.0, 1.0)
            else
	      call setfor (300.0, 1.0, 1.0)
            endif
	    end if
	  else if (ipdepth .eq. 2) then
	    if (depth .le. 5.0d0) then
            if(icol.eq.1) then
	      call setbac (240.0, 0.4, 1.0)
            else
	      call setfor (240.0, 0.4, 1.0)
            endif
	    else if (depth .le. 10.0d0) then
            if(icol.eq.1) then
	      call setbac (180.0, 0.4, 1.0)
            else
	      call setfor (180.0, 0.4, 1.0)
            endif
	    else if (depth .le. 15.0d0) then
            if(icol.eq.1) then
	      call setbac (120.0, 0.4, 1.0)
            else
	      call setfor (120.0, 0.4, 1.0)
            endif
	    else if (depth .le. 20.0d0) then
            if(icol.eq.1) then
	      call setbac (30.0, 0.4, 1.0)
            else
	      call setfor (30.0, 0.4, 1.0)
            endif
	    else
            if(icol.eq.1) then
	      call setbac (0.0, 0.5, 1.0)
            else
	      call setfor (0.0, 0.5, 1.0)
            endif
	    end if
	  else if (ipdeplt .eq. 1) then
	    if (ipde .eq. 1) then
            if(icol.eq.1) then
	      call setbac (120.0, 0.5, 1.0)
            else
	      call setfor (120.0, 0.5, 1.0)
            endif
	    else
            if(icol.eq.1) then
	      call setbac (240.0, 0.5, 1.0)
            else
	      call setfor (240.0, 0.5, 1.0)
            endif
	    end if
	  else
            if(icol.eq.1) then
	    call setbac (240.0, 0.5, 1.0)
            else
	    call setfor (0.0, 0.5, 1.0)
            endif
	  end if
	  if (ipmag .gt. 0) then
	    if (xmb .gt. -999.0) then
	      ss = symsiz*(2.0 + 2.0*(xmb-2.0)/4.0)
	      symbl = 'hexagon'
            else
	      ss = symsiz
	      symbl = 'box'
            end if
	  else
	    ss = symsiz
	    symbl = 'box'
	  end if
          if (icol.eq.1) then
	    call symbol (symbl, xplt1, yplt1, ss, 0.0, 0, 1)
          else
crah        call circle (xplt1, yplt1, 3*ss, 90, 0, 0, 0.2, 0)
	    call symbol ('circle', xplt1, yplt1, ss, 0.0, 0, 0)
          endif
	else
	  do 510  j = 1, nauth
	    if (arg .eq. auth(j)) go to 511
  510     continue
	  go to 500
  511     continue
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
	  if (ipdepth .eq. 3) then
	    if (dep .lt. -1.0) then
	      call setbac (240.0, 0.4, 1.0)
	    else if (dep .le. -0.2) then
	      call setbac (180.0, 0.4, 1.0)
	    else if (dep .le. 0.2) then
	      call setbac (120.0, 0.4, 1.0)
	    else if (dep .le. 1.0) then
	      call setbac (30.0, 0.4, 1.0)
	    else
	      call setbac (0.0, 0.5, 1.0)
	    end if
	  else
	    call setbac (symhue(j), symlit(j), symsat(j))
	  end if
          call symbol (sym(j), xplt1, yplt1, symsiz, 0.0, 0, 1)
          call setbac (0.0, 1.0, 0.0)
	end if
	if (iplt .gt. 1) then
          call line (xplt1, yplt1, xplt2, yplt2, 0.0, 0, 0)
	else
	  if (iporid .eq. 1) then
	    write (title, '(i15)') iorid
	    do 513 l = 1, len(title)
	      if (title(l:l) .ne. ' ') go to 514
  513       continue
  514       l = l - 5
            call chrsiz (0.04, 1.0, 0.0)
	    call text (xplt1, yplt1, 0.0, 1, title(l:len(title)), 
     +               0)
	  end if
	end if
	iplt = iplt + 1
	xplt2 = xplt1
	yplt2 = yplt1
  505 continue
  500 continue
      call setfor (0.0, 0.0, 0.0)
      call setbac (0.0, 1.0, 0.0)
      else
	call setbac (240.0, 0.5, 1.0)
        call symbol ('hexagon', 0.0, 0.0, symsiz*2.5, 0.0, 0, 1)
        call setfor (0.0, 0.0, 0.0)
        call setbac (0.0, 1.0, 0.0)
      end if
c
      return
      end
      subroutine idevent (xlat0, xlon0, x, y, nevs, ievs,
     +                    nauth, auth)
c
      real*4 xlat0, xlon0
      real*4  x, y
      integer nevs
      integer*8 ievs(nevs)
      integer nauth
      character*(*) auth(nauth)
c
# 1 "/opt/antelope/5.2-64/include/EV_f.i" 1 

	integer	EV_NULL
        parameter (EV_NULL = (0))
	integer	EV_NAME
        parameter (EV_NAME = (1))
	integer	EV_NHYPOS
        parameter (EV_NHYPOS = (2))
	integer	EV_PREFHYPO
        parameter (EV_PREFHYPO = (3))
	integer	EV_HYPO_TIME
        parameter (EV_HYPO_TIME = (4))
	integer	EV_HYPO_LAT
        parameter (EV_HYPO_LAT = (5))
	integer	EV_HYPO_LON
        parameter (EV_HYPO_LON = (6))
	integer	EV_HYPO_DEPTH
        parameter (EV_HYPO_DEPTH = (7))
	integer	EV_HYPO_MB
        parameter (EV_HYPO_MB = (8))
	integer	EV_HYPO_MS
        parameter (EV_HYPO_MS = (9))
	integer	EV_HYPO_ML
        parameter (EV_HYPO_ML = (10))
	integer	EV_HYPO_AUTH
        parameter (EV_HYPO_AUTH = (11))
	integer	EV_HYPO_NSTAS
        parameter (EV_HYPO_NSTAS = (12))
	integer	EV_HYPO_ASSOCFLAG
        parameter (EV_HYPO_ASSOCFLAG = (13))
	integer	EV_HYPO_STA_STA
        parameter (EV_HYPO_STA_STA = (14))
	integer	EV_HYPO_STA_LAT
        parameter (EV_HYPO_STA_LAT = (15))
	integer	EV_HYPO_STA_LON
        parameter (EV_HYPO_STA_LON = (16))
	integer	EV_HYPO_STA_ELEV
        parameter (EV_HYPO_STA_ELEV = (17))
	integer	EV_HYPO_STA_NASSOCS
        parameter (EV_HYPO_STA_NASSOCS = (50))
	integer	EV_HYPO_STA_ASSOC_CHAN
        parameter (EV_HYPO_STA_ASSOC_CHAN = (18))
	integer	EV_HYPO_STA_ASSOC_PHASE
        parameter (EV_HYPO_STA_ASSOC_PHASE = (19))
	integer	EV_HYPO_STA_ASSOC_IPHASE
        parameter (EV_HYPO_STA_ASSOC_IPHASE = (20))
	integer	EV_HYPO_STA_ASSOC_TIME
        parameter (EV_HYPO_STA_ASSOC_TIME = (21))
	integer	EV_HYPO_STA_ASSOC_AZIMUTH
        parameter (EV_HYPO_STA_ASSOC_AZIMUTH = (22))
	integer	EV_HYPO_STA_ASSOC_INC
        parameter (EV_HYPO_STA_ASSOC_INC = (23))
	integer	EV_HYPO_STA_ASSOC_RECT
        parameter (EV_HYPO_STA_ASSOC_RECT = (24))
	integer	EV_HYPO_STA_ASSOC_DELTIME
        parameter (EV_HYPO_STA_ASSOC_DELTIME = (25))
	integer	EV_HYPO_STA_ASSOC_ASSOCFLAG
        parameter (EV_HYPO_STA_ASSOC_ASSOCFLAG = (26))
	integer	EV_HYPO_STA_ASSOC_DISTANCE
        parameter (EV_HYPO_STA_ASSOC_DISTANCE = (27))
	integer	EV_HYPO_STA_ASSOC_TIMERES
        parameter (EV_HYPO_STA_ASSOC_TIMERES = (28))
	integer	EV_HYPO_STA_ASSOC_SHAZ
        parameter (EV_HYPO_STA_ASSOC_SHAZ = (29))
	integer	EV_HYPO_STA_ASSOC_HSAZ
        parameter (EV_HYPO_STA_ASSOC_HSAZ = (30))
	integer	EV_HYPO_STA_ASSOC_AZRES
        parameter (EV_HYPO_STA_ASSOC_AZRES = (31))
	integer	EV_HYPO_STA_ASSOC_TIMEWGT
        parameter (EV_HYPO_STA_ASSOC_TIMEWGT = (32))
	integer	EV_HYPO_STA_ASSOC_AZWGT
        parameter (EV_HYPO_STA_ASSOC_AZWGT = (33))
	integer	EV_DBL
        parameter (EV_DBL = (34))
	integer	EV_TUPLE
        parameter (EV_TUPLE = (35))
	integer	EV_EVID
        parameter (EV_EVID = (36))
	integer	EV_HYPO_TUPLE
        parameter (EV_HYPO_TUPLE = (37))
	integer	EV_HYPO_ORID
        parameter (EV_HYPO_ORID = (38))
	integer	EV_HYPO_STA_ASSOC_ARID
        parameter (EV_HYPO_STA_ASSOC_ARID = (39))
	integer	EV_HYPO_STA_ASSOC_ASTUPLE
        parameter (EV_HYPO_STA_ASSOC_ASTUPLE = (40))
	integer	EV_HYPO_STA_ASSOC_ARTUPLE
        parameter (EV_HYPO_STA_ASSOC_ARTUPLE = (41))

	integer	EVSV_NULL
        parameter (EVSV_NULL = (0))
	integer	EVSV_HYPO_TIME
        parameter (EVSV_HYPO_TIME = (4))
	integer	EVSV_HYPO_LAT
        parameter (EVSV_HYPO_LAT = (5))
	integer	EVSV_HYPO_LON
        parameter (EVSV_HYPO_LON = (6))
	integer	EVSV_HYPO_DEPTH
        parameter (EVSV_HYPO_DEPTH = (7))
	integer	EVSV_HYPO_MB
        parameter (EVSV_HYPO_MB = (8))
	integer	EVSV_HYPO_MS
        parameter (EVSV_HYPO_MS = (9))
	integer	EVSV_HYPO_ML
        parameter (EVSV_HYPO_ML = (10))
	integer	EVSV_HYPO_AUTH
        parameter (EVSV_HYPO_AUTH = (11))
	integer	EVSV_HYPO_NSTAS
        parameter (EVSV_HYPO_NSTAS = (12))
	integer	EVSV_HYPO_ASSOCFLAG
        parameter (EVSV_HYPO_ASSOCFLAG = (13))
	integer	EVSV_HYPO_PREF
        parameter (EVSV_HYPO_PREF = (42))
	integer	EVSV_STA_STA
        parameter (EVSV_STA_STA = (14))
	integer	EVSV_STA_LAT
        parameter (EVSV_STA_LAT = (15))
	integer	EVSV_STA_LON
        parameter (EVSV_STA_LON = (16))
	integer	EVSV_STA_ELEV
        parameter (EVSV_STA_ELEV = (17))
        integer EVSV_STA_NSCVS
        parameter (EVSV_STA_NSCVS = (60))
        integer EVSV_STA_SCVS
        parameter (EVSV_STA_SCVS = (61))
	integer	EVSV_STA_NASSOCS
        parameter (EVSV_STA_NASSOCS = (50))
	integer	EVSV_STA_ASSOC_CHAN
        parameter (EVSV_STA_ASSOC_CHAN = (18))
	integer	EVSV_STA_ASSOC_PHASE
        parameter (EVSV_STA_ASSOC_PHASE = (19))
	integer	EVSV_STA_ASSOC_IPHASE
        parameter (EVSV_STA_ASSOC_IPHASE = (20))
	integer	EVSV_STA_ASSOC_TIME
        parameter (EVSV_STA_ASSOC_TIME = (21))
	integer	EVSV_STA_ASSOC_AZIMUTH
        parameter (EVSV_STA_ASSOC_AZIMUTH = (22))
	integer	EVSV_STA_ASSOC_INC
        parameter (EVSV_STA_ASSOC_INC = (23))
	integer	EVSV_STA_ASSOC_RECT
        parameter (EVSV_STA_ASSOC_RECT = (24))
	integer	EVSV_STA_ASSOC_DELTIME
        parameter (EVSV_STA_ASSOC_DELTIME = (25))
	integer	EVSV_STA_ASSOC_ASSOCFLAG
        parameter (EVSV_STA_ASSOC_ASSOCFLAG = (26))
	integer	EVSV_STA_ASSOC_DISTANCE
        parameter (EVSV_STA_ASSOC_DISTANCE = (27))
	integer	EVSV_STA_ASSOC_TIMERES
        parameter (EVSV_STA_ASSOC_TIMERES = (28))
	integer	EVSV_STA_ASSOC_SHAZ
        parameter (EVSV_STA_ASSOC_SHAZ = (29))
	integer	EVSV_STA_ASSOC_HSAZ
        parameter (EVSV_STA_ASSOC_HSAZ = (30))
	integer	EVSV_STA_ASSOC_AZRES
        parameter (EVSV_STA_ASSOC_AZRES = (31))
	integer	EVSV_STA_ASSOC_TIMEWGT
        parameter (EVSV_STA_ASSOC_TIMEWGT = (32))
	integer	EVSV_STA_ASSOC_AZWGT
        parameter (EVSV_STA_ASSOC_AZWGT = (33))
	integer	EVSV_DBL
        parameter (EVSV_DBL = (34))
	integer	EVSV_HYPO_TUPLE
        parameter (EVSV_HYPO_TUPLE = (37))
	integer	EVSV_HYPO_ORID
        parameter (EVSV_HYPO_ORID = (38))
	integer	EVSV_STA_ASSOC_ARID
        parameter (EVSV_STA_ASSOC_ARID = (39))
	integer	EVSV_STA_ASSOC_ASTUPLE
        parameter (EVSV_STA_ASSOC_ASTUPLE = (40))
	integer	EVSV_STA_ASSOC_ARTUPLE
        parameter (EVSV_STA_ASSOC_ARTUPLE = (41))


c $Id: dbmaprelease.f,v 1.11 2010-10-26 17:02:56 mitch Exp $ 
# 1652 "dbmaprelease_mac.F" 2 
      real*8 elat, elon, time, depth, sec
      character*80 author, alg
c
      call xydel2latlon (xlat0, xlon0, x, y,
     +                         xlat2, xlon2)
      amin = 1.e30
      do 100  i = 1, nevs
	call evget (ievs(i), EV_NHYPOS, nhypos,
     +                       EV_PREFHYPO, ipref,
     +                       EV_EVID, ievid,
     +			 0)
	do 200  j = 1, nhypos
	  call evget (ievs(i), EV_HYPO_LAT, j, elat,
     +                       EV_HYPO_LON, j, elon,
     +                       EV_HYPO_AUTH, j, author,
     +                       0)
          xlat = elat
          xlon = elon
	  if (nauth .eq. 0) then
	  else
	    do 510  k = 1, nauth
	      if (author .eq. auth(k)) go to 511
  510       continue
	    go to 200
  511       continue
	  end if
          call latlon2xydel (xlat, xlon, xlat2, xlon2,
     +                         xdel, ydel)
	  dist = sqrt(xdel**2 + ydel**2)
	  if (dist .lt. amin) then
	    amin = dist
	    ihold = i
	    jhold = j
	  end if
  200   continue
  100 continue
      i = ihold
      j = jhold
      call evget (ievs(i), EV_HYPO_LAT, j, elat,
     +                       EV_HYPO_LON, j, elon,
     +                       EV_HYPO_DEPTH, j, depth,
     +                       EV_HYPO_TIME, j, time,
     +                       EV_HYPO_MB, j, xmb,
     +                       EV_HYPO_MS, j, xms,
     +                       EV_HYPO_ML, j, xml,
     +                       EV_HYPO_AUTH, j, author,
     +                       EV_HYPO_ORID, j, iorid,
     +                       0)
      call e2h(time, iyear, iday, ihour, imin, sec)
      write (6, '(a)') ' '
      write (6, '(a,a)')    'AUTH:   ',author(1:ilen(author))
      write (6, '(a,i8)')   'ORID:   ',iorid
      write (6, '(a,i4,i3,1x,i2.2,a,i2.2,a,f5.2)')   
     +                      'TIME:   ',iyear,iday,ihour,':',
     +                   imin,':',sec
      write (6, '(a,f9.4)') 'LAT:    ',elat
      write (6, '(a,f9.4)') 'LON:    ',elon
      write (6, '(a,f9.4)') 'DEPTH:  ',depth
      write (6, '(a,f9.4)') 'MB:     ',xmb
      write (6, '(a,f9.4)') 'MS:     ',xms
      write (6, '(a,f9.4)') 'ML:     ',xml
      xlat2 = elat
      xlon2 = elon
      call latlon2xydel (xlat0, xlon0, xlat2, xlon2,
     +                         x, y)
c    
      return
      end
      subroutine idstation (xlat0, xlon0, x, y, idb,
     +                    nauth, auth)
c
      real*4 xlat0, xlon0
      real*4  x, y
      integer idb
      integer nauth
      character*(*) auth(nauth)
c
# 1 "/opt/antelope/5.2-64/include/EV_f.i" 1 

	integer	EV_NULL
        parameter (EV_NULL = (0))
	integer	EV_NAME
        parameter (EV_NAME = (1))
	integer	EV_NHYPOS
        parameter (EV_NHYPOS = (2))
	integer	EV_PREFHYPO
        parameter (EV_PREFHYPO = (3))
	integer	EV_HYPO_TIME
        parameter (EV_HYPO_TIME = (4))
	integer	EV_HYPO_LAT
        parameter (EV_HYPO_LAT = (5))
	integer	EV_HYPO_LON
        parameter (EV_HYPO_LON = (6))
	integer	EV_HYPO_DEPTH
        parameter (EV_HYPO_DEPTH = (7))
	integer	EV_HYPO_MB
        parameter (EV_HYPO_MB = (8))
	integer	EV_HYPO_MS
        parameter (EV_HYPO_MS = (9))
	integer	EV_HYPO_ML
        parameter (EV_HYPO_ML = (10))
	integer	EV_HYPO_AUTH
        parameter (EV_HYPO_AUTH = (11))
	integer	EV_HYPO_NSTAS
        parameter (EV_HYPO_NSTAS = (12))
	integer	EV_HYPO_ASSOCFLAG
        parameter (EV_HYPO_ASSOCFLAG = (13))
	integer	EV_HYPO_STA_STA
        parameter (EV_HYPO_STA_STA = (14))
	integer	EV_HYPO_STA_LAT
        parameter (EV_HYPO_STA_LAT = (15))
	integer	EV_HYPO_STA_LON
        parameter (EV_HYPO_STA_LON = (16))
	integer	EV_HYPO_STA_ELEV
        parameter (EV_HYPO_STA_ELEV = (17))
	integer	EV_HYPO_STA_NASSOCS
        parameter (EV_HYPO_STA_NASSOCS = (50))
	integer	EV_HYPO_STA_ASSOC_CHAN
        parameter (EV_HYPO_STA_ASSOC_CHAN = (18))
	integer	EV_HYPO_STA_ASSOC_PHASE
        parameter (EV_HYPO_STA_ASSOC_PHASE = (19))
	integer	EV_HYPO_STA_ASSOC_IPHASE
        parameter (EV_HYPO_STA_ASSOC_IPHASE = (20))
	integer	EV_HYPO_STA_ASSOC_TIME
        parameter (EV_HYPO_STA_ASSOC_TIME = (21))
	integer	EV_HYPO_STA_ASSOC_AZIMUTH
        parameter (EV_HYPO_STA_ASSOC_AZIMUTH = (22))
	integer	EV_HYPO_STA_ASSOC_INC
        parameter (EV_HYPO_STA_ASSOC_INC = (23))
	integer	EV_HYPO_STA_ASSOC_RECT
        parameter (EV_HYPO_STA_ASSOC_RECT = (24))
	integer	EV_HYPO_STA_ASSOC_DELTIME
        parameter (EV_HYPO_STA_ASSOC_DELTIME = (25))
	integer	EV_HYPO_STA_ASSOC_ASSOCFLAG
        parameter (EV_HYPO_STA_ASSOC_ASSOCFLAG = (26))
	integer	EV_HYPO_STA_ASSOC_DISTANCE
        parameter (EV_HYPO_STA_ASSOC_DISTANCE = (27))
	integer	EV_HYPO_STA_ASSOC_TIMERES
        parameter (EV_HYPO_STA_ASSOC_TIMERES = (28))
	integer	EV_HYPO_STA_ASSOC_SHAZ
        parameter (EV_HYPO_STA_ASSOC_SHAZ = (29))
	integer	EV_HYPO_STA_ASSOC_HSAZ
        parameter (EV_HYPO_STA_ASSOC_HSAZ = (30))
	integer	EV_HYPO_STA_ASSOC_AZRES
        parameter (EV_HYPO_STA_ASSOC_AZRES = (31))
	integer	EV_HYPO_STA_ASSOC_TIMEWGT
        parameter (EV_HYPO_STA_ASSOC_TIMEWGT = (32))
	integer	EV_HYPO_STA_ASSOC_AZWGT
        parameter (EV_HYPO_STA_ASSOC_AZWGT = (33))
	integer	EV_DBL
        parameter (EV_DBL = (34))
	integer	EV_TUPLE
        parameter (EV_TUPLE = (35))
	integer	EV_EVID
        parameter (EV_EVID = (36))
	integer	EV_HYPO_TUPLE
        parameter (EV_HYPO_TUPLE = (37))
	integer	EV_HYPO_ORID
        parameter (EV_HYPO_ORID = (38))
	integer	EV_HYPO_STA_ASSOC_ARID
        parameter (EV_HYPO_STA_ASSOC_ARID = (39))
	integer	EV_HYPO_STA_ASSOC_ASTUPLE
        parameter (EV_HYPO_STA_ASSOC_ASTUPLE = (40))
	integer	EV_HYPO_STA_ASSOC_ARTUPLE
        parameter (EV_HYPO_STA_ASSOC_ARTUPLE = (41))

	integer	EVSV_NULL
        parameter (EVSV_NULL = (0))
	integer	EVSV_HYPO_TIME
        parameter (EVSV_HYPO_TIME = (4))
	integer	EVSV_HYPO_LAT
        parameter (EVSV_HYPO_LAT = (5))
	integer	EVSV_HYPO_LON
        parameter (EVSV_HYPO_LON = (6))
	integer	EVSV_HYPO_DEPTH
        parameter (EVSV_HYPO_DEPTH = (7))
	integer	EVSV_HYPO_MB
        parameter (EVSV_HYPO_MB = (8))
	integer	EVSV_HYPO_MS
        parameter (EVSV_HYPO_MS = (9))
	integer	EVSV_HYPO_ML
        parameter (EVSV_HYPO_ML = (10))
	integer	EVSV_HYPO_AUTH
        parameter (EVSV_HYPO_AUTH = (11))
	integer	EVSV_HYPO_NSTAS
        parameter (EVSV_HYPO_NSTAS = (12))
	integer	EVSV_HYPO_ASSOCFLAG
        parameter (EVSV_HYPO_ASSOCFLAG = (13))
	integer	EVSV_HYPO_PREF
        parameter (EVSV_HYPO_PREF = (42))
	integer	EVSV_STA_STA
        parameter (EVSV_STA_STA = (14))
	integer	EVSV_STA_LAT
        parameter (EVSV_STA_LAT = (15))
	integer	EVSV_STA_LON
        parameter (EVSV_STA_LON = (16))
	integer	EVSV_STA_ELEV
        parameter (EVSV_STA_ELEV = (17))
        integer EVSV_STA_NSCVS
        parameter (EVSV_STA_NSCVS = (60))
        integer EVSV_STA_SCVS
        parameter (EVSV_STA_SCVS = (61))
	integer	EVSV_STA_NASSOCS
        parameter (EVSV_STA_NASSOCS = (50))
	integer	EVSV_STA_ASSOC_CHAN
        parameter (EVSV_STA_ASSOC_CHAN = (18))
	integer	EVSV_STA_ASSOC_PHASE
        parameter (EVSV_STA_ASSOC_PHASE = (19))
	integer	EVSV_STA_ASSOC_IPHASE
        parameter (EVSV_STA_ASSOC_IPHASE = (20))
	integer	EVSV_STA_ASSOC_TIME
        parameter (EVSV_STA_ASSOC_TIME = (21))
	integer	EVSV_STA_ASSOC_AZIMUTH
        parameter (EVSV_STA_ASSOC_AZIMUTH = (22))
	integer	EVSV_STA_ASSOC_INC
        parameter (EVSV_STA_ASSOC_INC = (23))
	integer	EVSV_STA_ASSOC_RECT
        parameter (EVSV_STA_ASSOC_RECT = (24))
	integer	EVSV_STA_ASSOC_DELTIME
        parameter (EVSV_STA_ASSOC_DELTIME = (25))
	integer	EVSV_STA_ASSOC_ASSOCFLAG
        parameter (EVSV_STA_ASSOC_ASSOCFLAG = (26))
	integer	EVSV_STA_ASSOC_DISTANCE
        parameter (EVSV_STA_ASSOC_DISTANCE = (27))
	integer	EVSV_STA_ASSOC_TIMERES
        parameter (EVSV_STA_ASSOC_TIMERES = (28))
	integer	EVSV_STA_ASSOC_SHAZ
        parameter (EVSV_STA_ASSOC_SHAZ = (29))
	integer	EVSV_STA_ASSOC_HSAZ
        parameter (EVSV_STA_ASSOC_HSAZ = (30))
	integer	EVSV_STA_ASSOC_AZRES
        parameter (EVSV_STA_ASSOC_AZRES = (31))
	integer	EVSV_STA_ASSOC_TIMEWGT
        parameter (EVSV_STA_ASSOC_TIMEWGT = (32))
	integer	EVSV_STA_ASSOC_AZWGT
        parameter (EVSV_STA_ASSOC_AZWGT = (33))
	integer	EVSV_DBL
        parameter (EVSV_DBL = (34))
	integer	EVSV_HYPO_TUPLE
        parameter (EVSV_HYPO_TUPLE = (37))
	integer	EVSV_HYPO_ORID
        parameter (EVSV_HYPO_ORID = (38))
	integer	EVSV_STA_ASSOC_ARID
        parameter (EVSV_STA_ASSOC_ARID = (39))
	integer	EVSV_STA_ASSOC_ASTUPLE
        parameter (EVSV_STA_ASSOC_ASTUPLE = (40))
	integer	EVSV_STA_ASSOC_ARTUPLE
        parameter (EVSV_STA_ASSOC_ARTUPLE = (41))


c $Id: dbmaprelease.f,v 1.11 2010-10-26 17:02:56 mitch Exp $ 
# 1730 "dbmaprelease_mac.F" 2 
# 1 "/opt/antelope/5.2-64/include/dbl2.i" 1 


# 1 "/opt/antelope/5.2-64/include/db.i" 1 

      integer dbINVALID
      parameter (dbINVALID = -102)
      integer dbCOUNT
      parameter (dbCOUNT = -301)
      integer dbDATABASE_COUNT
      parameter (dbDATABASE_COUNT = -302	       )
      integer dbTABLE_COUNT
      parameter (dbTABLE_COUNT = -303	      )
      integer dbFIELD_COUNT
      parameter (dbFIELD_COUNT = -304	       )
      integer dbRECORD_COUNT
      parameter (dbRECORD_COUNT = -305	      )
      integer dbDESCRIPTION
      parameter (dbDESCRIPTION = -306)
      integer dbSCHEMA_DESCRIPTION
      parameter (dbSCHEMA_DESCRIPTION = -307    )
      integer dbDATABASE_DESCRIPTION
      parameter (dbDATABASE_DESCRIPTION = -308 )
      integer dbTABLE_DESCRIPTION
      parameter (dbTABLE_DESCRIPTION = -309   )
      integer dbFIELD_DESCRIPTION
      parameter (dbFIELD_DESCRIPTION = -310  )
      integer dbDETAIL
      parameter (dbDETAIL = -311)
      integer dbSCHEMA_DETAIL
      parameter (dbSCHEMA_DETAIL = -312	       )
      integer dbDATABASE_DETAIL
      parameter (dbDATABASE_DETAIL = -313	      )
      integer dbTABLE_DETAIL
      parameter (dbTABLE_DETAIL = -314	     )
      integer dbFIELD_DETAIL
      parameter (dbFIELD_DETAIL = -315	    )
      integer dbNAME
      parameter (dbNAME = -316)
      integer dbSCHEMA_NAME
      parameter (dbSCHEMA_NAME = -317	       )
      integer dbDATABASE_NAME
      parameter (dbDATABASE_NAME = -318	      )
      integer dbTABLE_NAME
      parameter (dbTABLE_NAME = -319	     )
      integer dbFIELD_NAME
      parameter (dbFIELD_NAME = -320	    )
      integer dbTABLE_PRESENT
      parameter (dbTABLE_PRESENT = -321)
      integer dbSIZE
      parameter (dbSIZE = -322)
      integer dbTABLE_SIZE
      parameter (dbTABLE_SIZE = -323	   )
      integer dbFIELD_SIZE
      parameter (dbFIELD_SIZE = -324	  )
      integer dbFIELD_TYPE
      parameter (dbFIELD_TYPE = -328	  )
      integer dbTABLE_FILENAME
      parameter (dbTABLE_FILENAME = -329)
      integer dbDBPATH
      parameter (dbDBPATH = -330)
      integer dbTABLE_DIRNAME
      parameter (dbTABLE_DIRNAME = -331)
      integer dbPRIMARY_KEY
      parameter (dbPRIMARY_KEY = -332)
      integer dbALTERNATE_KEY
      parameter (dbALTERNATE_KEY = -333)
      integer dbFOREIGN_KEYS
      parameter (dbFOREIGN_KEYS = -334)
      integer dbUNIQUE_ID
      parameter (dbUNIQUE_ID = -335)
      integer dbUNIQUE_ID_NAME
      parameter (dbUNIQUE_ID_NAME = -336)
      integer dbSINGLE
      parameter (dbSINGLE = -337)
      integer dbSCHEMA_DEFAULT
      parameter (dbSCHEMA_DEFAULT = -338)
      integer dbDBPATH_DEFAULT
      parameter (dbDBPATH_DEFAULT = -339)
      integer dbLOAD_DATE
      parameter (dbLOAD_DATE = -340)
      integer dbTYPE
      parameter (dbTYPE = -325)
      integer dbFORMAT
      parameter (dbFORMAT = -326)
      integer dbUNITS
      parameter (dbUNITS = -327)
      integer dbDATABASE
      parameter (dbDATABASE = -401)
      integer dbVIEW
      parameter (dbVIEW = -402)
      integer dbTABLE
      parameter (dbTABLE = -403)
      integer dbFIELD
      parameter (dbFIELD = -404)
      integer dbRECORD
      parameter (dbRECORD = -405)
      integer dbMERGE
      parameter (dbMERGE = -406)
      integer dbALL
      parameter (dbALL = -501)
      integer dbPRIMARY
      parameter (dbPRIMARY = -502)
      integer dbALTERNATE
      parameter (dbALTERNATE = -503)
      integer dbSCRATCH
      parameter (dbSCRATCH = -504)
      integer dbNULL
      parameter (dbNULL = -505)
      integer dbBOOLEAN
      parameter (dbBOOLEAN = 1)
      integer dbINTEGER
      parameter (dbINTEGER = 2)
      integer dbREAL
      parameter (dbREAL = 3)
      integer dbTIME
      parameter (dbTIME = 4)
      integer dbYEARDAY
      parameter (dbYEARDAY = 5)
      integer dbSTRING
      parameter (dbSTRING = 6)
      integer dbDATE
      parameter (dbDATE = 7)
      integer dbWAVEFORM
      parameter (dbWAVEFORM = 136)
      integer dbRESPONSE
      parameter (dbRESPONSE = 137)
      integer dbBFLOAT
      parameter (dbBFLOAT = 138)
      integer dbBDOUBLE
      parameter (dbBDOUBLE = 139)
      integer dbBSHORT
      parameter (dbBSHORT = 140)
      integer dbBINT
      parameter (dbBINT = 141)
      integer dbDBPTR
      parameter (dbDBPTR = 142)
      integer dbUNIQUE
      parameter (dbUNIQUE = 1)
      integer dbOUTER_JOIN
      parameter (dbOUTER_JOIN = 2)
      integer dbadd
      integer dbaddnull
      integer dbadd_remark
      integer dbaddv
      integer dbcompile
      integer dbcreate
      integer dbcrunch
      integer dbdelete
      integer dbdestroy
      integer dbex_compile
      integer dbex_eval
      integer dbex_free
      integer dbextfile
      integer dbfilename
      integer dbfind_join_keys
      integer dbget
      integer dbget_remark
      integer dbgetv
      integer dbis_expression
      integer dbmark
      integer dbnextid
      integer dbopen
      integer dbopen_database
      integer dbopen_table
      integer dbput
      integer dbputv
      integer dbputx
      integer dbread_view
      integer dbsave_view
      integer dbset
      integer dbtruncate
      integer dbunjoin
      integer dbwrite_view
# 4 "/opt/antelope/5.2-64/include/dbl2.i" 2 
      character *(*) ATTRID_ADATE
      character *(*) ATTRID_ALGID
      character *(*) ATTRID_ALGORITHM
      character *(*) ATTRID_AMP
      character *(*) ATTRID_ARID
      character *(*) ATTRID_ARRAY
      character *(*) ATTRID_ATTRIB
      character *(*) ATTRID_ATYPE
      character *(*) ATTRID_AUTH
      character *(*) ATTRID_AZDEF
      character *(*) ATTRID_AZIMUTH
      character *(*) ATTRID_AZRES
      character *(*) ATTRID_BAND
      character *(*) ATTRID_BANDW
      character *(*) ATTRID_BAZIM
      character *(*) ATTRID_BEAMID
      character *(*) ATTRID_BELIEF
      character *(*) ATTRID_BESTDC
      character *(*) ATTRID_BLKFAC
      character *(*) ATTRID_BMTYP
      character *(*) ATTRID_BSLOW
      character *(*) ATTRID_CALIB
      character *(*) ATTRID_CALPER
      character *(*) ATTRID_CALRATIO
      character *(*) ATTRID_CDATE
      character *(*) ATTRID_CDPERR
      character *(*) ATTRID_CFREQ
      character *(*) ATTRID_CHAN
      character *(*) ATTRID_CHANA
      character *(*) ATTRID_CHANID
      character *(*) ATTRID_CHANO
      character *(*) ATTRID_CHID
      character *(*) ATTRID_CLAERR
      character *(*) ATTRID_CLIP
      character *(*) ATTRID_CLOERR
      character *(*) ATTRID_CMPX
      character *(*) ATTRID_CODA
      character *(*) ATTRID_CODDES
      character *(*) ATTRID_CODE
      character *(*) ATTRID_COLDEP
      character *(*) ATTRID_COLDIA
      character *(*) ATTRID_COLINT
      character *(*) ATTRID_COLVOL
      character *(*) ATTRID_COMM
      character *(*) ATTRID_COMMID
      character *(*) ATTRID_COMPA
      character *(*) ATTRID_CONF
      character *(*) ATTRID_COTERR
      character *(*) ATTRID_CTYPE
      character *(*) ATTRID_CUREV
      character *(*) ATTRID_CUROR
      character *(*) ATTRID_DATATYPE
      character *(*) ATTRID_DATE
      character *(*) ATTRID_DATSW
      character *(*) ATTRID_DATTYP
      character *(*) ATTRID_DAY
      character *(*) ATTRID_DEAST
      character *(*) ATTRID_DELAZ
      character *(*) ATTRID_DELSLO
      character *(*) ATTRID_DELTA
      character *(*) ATTRID_DELTIM
      character *(*) ATTRID_DEPDP
      character *(*) ATTRID_DEPTH
      character *(*) ATTRID_DESCR
      character *(*) ATTRID_DESCRIP
      character *(*) ATTRID_DFILE
      character *(*) ATTRID_DIG
      character *(*) ATTRID_DIGITAL
      character *(*) ATTRID_DIP1
      character *(*) ATTRID_DIP2
      character *(*) ATTRID_DIR
      character *(*) ATTRID_DIST
      character *(*) ATTRID_DLID
      character *(*) ATTRID_DNORTH
      character *(*) ATTRID_DOY
      character *(*) ATTRID_DTYPE
      character *(*) ATTRID_DURAT
      character *(*) ATTRID_DUSED
      character *(*) ATTRID_EDATE
      character *(*) ATTRID_EDEPTH
      character *(*) ATTRID_ELEV
      character *(*) ATTRID_EMA
      character *(*) ATTRID_EMARES
      character *(*) ATTRID_ENDTIME
      character *(*) ATTRID_ESAZ
      character *(*) ATTRID_ETYPE
      character *(*) ATTRID_EVID
      character *(*) ATTRID_EVNAME
      character *(*) ATTRID_EXLAT
      character *(*) ATTRID_EXLON
      character *(*) ATTRID_EXPTYP
      character *(*) ATTRID_FILE
      character *(*) ATTRID_FILES
      character *(*) ATTRID_FILTID
      character *(*) ATTRID_FILTYP
      character *(*) ATTRID_FKID
      character *(*) ATTRID_FKQUAL
      character *(*) ATTRID_FKRID
      character *(*) ATTRID_FKTYP
      character *(*) ATTRID_FM
      character *(*) ATTRID_FNORM
      character *(*) ATTRID_FOFF
      character *(*) ATTRID_FSID
      character *(*) ATTRID_FSRID
      character *(*) ATTRID_FSTAT
      character *(*) ATTRID_FSTYP
      character *(*) ATTRID_FTID
      character *(*) ATTRID_GNORM
      character *(*) ATTRID_GRN
      character *(*) ATTRID_GRNAME
      character *(*) ATTRID_HANG
      character *(*) ATTRID_HICUT
      character *(*) ATTRID_HSLOPE
      character *(*) ATTRID_IDTYPE
      character *(*) ATTRID_IDVALUE
      character *(*) ATTRID_IMB
      character *(*) ATTRID_IML
      character *(*) ATTRID_IMS
      character *(*) ATTRID_INID
      character *(*) ATTRID_INSNAME
      character *(*) ATTRID_INSTANT
      character *(*) ATTRID_INSTYP
      character *(*) ATTRID_INSTYPE
      character *(*) ATTRID_INTSCL
      character *(*) ATTRID_IPHASE
      character *(*) ATTRID_JDATE
      character *(*) ATTRID_LAT
      character *(*) ATTRID_LDATE
      character *(*) ATTRID_LDDATE
      character *(*) ATTRID_LEAP
      character *(*) ATTRID_LINENO
      character *(*) ATTRID_LOCATION
      character *(*) ATTRID_LOCUT
      character *(*) ATTRID_LOGAT
      character *(*) ATTRID_LON
      character *(*) ATTRID_LSLOPE
      character *(*) ATTRID_LTYPE
      character *(*) ATTRID_MAG
      character *(*) ATTRID_MAGB
      character *(*) ATTRID_MAGID
      character *(*) ATTRID_MAGLR
      character *(*) ATTRID_MAGNITUDE
      character *(*) ATTRID_MAGSH
      character *(*) ATTRID_MAGTYPE
      character *(*) ATTRID_MAXBLK
      character *(*) ATTRID_MAXF
      character *(*) ATTRID_MAXINT
      character *(*) ATTRID_MAXKX
      character *(*) ATTRID_MAXKY
      character *(*) ATTRID_MAXSX
      character *(*) ATTRID_MAXSY
      character *(*) ATTRID_MB
      character *(*) ATTRID_MBID
      character *(*) ATTRID_MEDIUM
      character *(*) ATTRID_MEXPON
      character *(*) ATTRID_MFF
      character *(*) ATTRID_MFFERR
      character *(*) ATTRID_MINBLK
      character *(*) ATTRID_ML
      character *(*) ATTRID_MLID
      character *(*) ATTRID_MNAME
      character *(*) ATTRID_MO
      character *(*) ATTRID_MOAUTH
      character *(*) ATTRID_MOIST
      character *(*) ATTRID_MON
      character *(*) ATTRID_MRF
      character *(*) ATTRID_MRFERR
      character *(*) ATTRID_MRR
      character *(*) ATTRID_MRRERR
      character *(*) ATTRID_MRT
      character *(*) ATTRID_MRTERR
      character *(*) ATTRID_MS
      character *(*) ATTRID_MSID
      character *(*) ATTRID_MTF
      character *(*) ATTRID_MTFERR
      character *(*) ATTRID_MTT
      character *(*) ATTRID_MTTERR
      character *(*) ATTRID_NAME
      character *(*) ATTRID_NASS
      character *(*) ATTRID_NAXAZM
      character *(*) ATTRID_NAXPLG
      character *(*) ATTRID_NAXVAL
      character *(*) ATTRID_NBYTE
      character *(*) ATTRID_NCALIB
      character *(*) ATTRID_NCALPER
      character *(*) ATTRID_NDEF
      character *(*) ATTRID_NDLID
      character *(*) ATTRID_NDP
      character *(*) ATTRID_NET
      character *(*) ATTRID_NETNAME
      character *(*) ATTRID_NETTYPE
      character *(*) ATTRID_NETWRK
      character *(*) ATTRID_NF
      character *(*) ATTRID_NMB
      character *(*) ATTRID_NMO
      character *(*) ATTRID_NMS
      character *(*) ATTRID_NORID
      character *(*) ATTRID_NOWFT
      character *(*) ATTRID_NRLPB
      character *(*) ATTRID_NRMW
      character *(*) ATTRID_NSAMP
      character *(*) ATTRID_NSLPB
      character *(*) ATTRID_NSMW
      character *(*) ATTRID_NSTA
      character *(*) ATTRID_NX
      character *(*) ATTRID_NXALG
      character *(*) ATTRID_NXARID
      character *(*) ATTRID_NXCHID
      character *(*) ATTRID_NXCOMM
      character *(*) ATTRID_NXDLID
      character *(*) ATTRID_NXEVID
      character *(*) ATTRID_NXFILT
      character *(*) ATTRID_NXFK
      character *(*) ATTRID_NXFKR
      character *(*) ATTRID_NXFS
      character *(*) ATTRID_NXFSR
      character *(*) ATTRID_NXFTID
      character *(*) ATTRID_NXINID
      character *(*) ATTRID_NXORID
      character *(*) ATTRID_NXSENS
      character *(*) ATTRID_NXSITE
      character *(*) ATTRID_NXSPRO
      character *(*) ATTRID_NXWFID
      character *(*) ATTRID_NY
      character *(*) ATTRID_OFFDAT
      character *(*) ATTRID_OFFDATE
      character *(*) ATTRID_ONDATE
      character *(*) ATTRID_OPSW
      character *(*) ATTRID_OPTYP
      character *(*) ATTRID_ORID
      character *(*) ATTRID_PALDEP
      character *(*) ATTRID_PAXAZM
      character *(*) ATTRID_PAXPLG
      character *(*) ATTRID_PAXVAL
      character *(*) ATTRID_PCHID
      character *(*) ATTRID_PDLID
      character *(*) ATTRID_PER
      character *(*) ATTRID_PHASE
      character *(*) ATTRID_PLPREF
      character *(*) ATTRID_PNAME
      character *(*) ATTRID_PORID
      character *(*) ATTRID_PREFOR
      character *(*) ATTRID_PVALUE
      character *(*) ATTRID_QUAL
      character *(*) ATTRID_RECT
      character *(*) ATTRID_REELSZ
      character *(*) ATTRID_REFSTA
      character *(*) ATTRID_REGION
      character *(*) ATTRID_REL
      character *(*) ATTRID_REMARK
      character *(*) ATTRID_RESID
      character *(*) ATTRID_RIPPLE
      character *(*) ATTRID_RSPTYP
      character *(*) ATTRID_RSPTYPE
      character *(*) ATTRID_SAMPRATE
      character *(*) ATTRID_SDDP
      character *(*) ATTRID_SDEPTH
      character *(*) ATTRID_SDMB
      character *(*) ATTRID_SDMO
      character *(*) ATTRID_SDMS
      character *(*) ATTRID_SDOBS
      character *(*) ATTRID_SDZDP
      character *(*) ATTRID_SEAZ
      character *(*) ATTRID_SEGTYP
      character *(*) ATTRID_SEGTYPE
      character *(*) ATTRID_SENSID
      character *(*) ATTRID_SITEID
      character *(*) ATTRID_SLIP1
      character *(*) ATTRID_SLIP2
      character *(*) ATTRID_SLODEF
      character *(*) ATTRID_SLORES
      character *(*) ATTRID_SLOW
      character *(*) ATTRID_SMAJAX
      character *(*) ATTRID_SMINAX
      character *(*) ATTRID_SMPRAT
      character *(*) ATTRID_SNAME
      character *(*) ATTRID_SNR
      character *(*) ATTRID_SPAUTH
      character *(*) ATTRID_SPMM
      character *(*) ATTRID_SPROID
      character *(*) ATTRID_SPRT
      character *(*) ATTRID_SPVT
      character *(*) ATTRID_SRN
      character *(*) ATTRID_SRNAME
      character *(*) ATTRID_STA
      character *(*) ATTRID_STAA
      character *(*) ATTRID_STANAM
      character *(*) ATTRID_STANAME
      character *(*) ATTRID_STAO
      character *(*) ATTRID_STASSID
      character *(*) ATTRID_STATYPE
      character *(*) ATTRID_STAV
      character *(*) ATTRID_STID
      character *(*) ATTRID_STIME
      character *(*) ATTRID_STR1
      character *(*) ATTRID_STR2
      character *(*) ATTRID_STRIKE
      character *(*) ATTRID_STRING
      character *(*) ATTRID_STT
      character *(*) ATTRID_STX
      character *(*) ATTRID_STY
      character *(*) ATTRID_STYPE
      character *(*) ATTRID_STZ
      character *(*) ATTRID_SXX
      character *(*) ATTRID_SXY
      character *(*) ATTRID_SXZ
      character *(*) ATTRID_SYY
      character *(*) ATTRID_SYZ
      character *(*) ATTRID_SZZ
      character *(*) ATTRID_TAGID
      character *(*) ATTRID_TAGNAME
      character *(*) ATTRID_TAPEBLOCK
      character *(*) ATTRID_TAPEFILE
      character *(*) ATTRID_TARNAM
      character *(*) ATTRID_TAXAZM
      character *(*) ATTRID_TAXPLG
      character *(*) ATTRID_TAXVAL
      character *(*) ATTRID_TCALIB
      character *(*) ATTRID_TDENSE
      character *(*) ATTRID_TEXT
      character *(*) ATTRID_TFILE
      character *(*) ATTRID_TIME
      character *(*) ATTRID_TIMEDEF
      character *(*) ATTRID_TIMERES
      character *(*) ATTRID_TLEN
      character *(*) ATTRID_TMFC
      character *(*) ATTRID_TMFI
      character *(*) ATTRID_TMNLPB
      character *(*) ATTRID_TMNMW
      character *(*) ATTRID_TPBLCK
      character *(*) ATTRID_TPFILE
      character *(*) ATTRID_TPTYPE
      character *(*) ATTRID_TRATBL
      character *(*) ATTRID_TSHIFT
      character *(*) ATTRID_TSITE
      character *(*) ATTRID_TUPID
      character *(*) ATTRID_UNCERTAINTY
      character *(*) ATTRID_USEDFT
      character *(*) ATTRID_VANG
      character *(*) ATTRID_VELID
      character *(*) ATTRID_VMODEL
      character *(*) ATTRID_VOLNAM
      character *(*) ATTRID_VOLNAME
      character *(*) ATTRID_WATDEP
      character *(*) ATTRID_WFID
      character *(*) ATTRID_WGT
      character *(*) ATTRID_YEAR
      character *(*) ATTRID_YIELD
      character *(*) ATTRID_YLDMAX
      character *(*) RELID_AFFILIATION
      character *(*) RELID_ALIAS
      character *(*) RELID_ARRIVAL
      character *(*) RELID_ASSOC
      character *(*) RELID_BEAM
      character *(*) RELID_CENTRYD
      character *(*) RELID_CHANNEL
      character *(*) RELID_CHOPER
      character *(*) RELID_CODE
      character *(*) RELID_COMMENT
      character *(*) RELID_COUNTER
      character *(*) RELID_DATE
      character *(*) RELID_DAY
      character *(*) RELID_DETECTION
      character *(*) RELID_DETLOC
      character *(*) RELID_EVENT
      character *(*) RELID_EVWF
      character *(*) RELID_EXPLO
      character *(*) RELID_EXTRA
      character *(*) RELID_FEATURE
      character *(*) RELID_FILTER
      character *(*) RELID_FKDISC
      character *(*) RELID_FKREC
      character *(*) RELID_FPLANE
      character *(*) RELID_FSDISC
      character *(*) RELID_FSREC
      character *(*) RELID_GREGION
      character *(*) RELID_INSTRUMENT
      character *(*) RELID_LASTID
      character *(*) RELID_LOC
      character *(*) RELID_MOMENT
      character *(*) RELID_NETMAG
      character *(*) RELID_NETWORK
      character *(*) RELID_ORIGERR
      character *(*) RELID_ORIGIN
      character *(*) RELID_REMARK
      character *(*) RELID_SENSOR
      character *(*) RELID_SIGPRO
      character *(*) RELID_SITE
      character *(*) RELID_SITECHAN
      character *(*) RELID_SREGION
      character *(*) RELID_STALOG
      character *(*) RELID_STAMAG
      character *(*) RELID_STASSOC
      character *(*) RELID_STATION
      character *(*) RELID_TAPE
      character *(*) RELID_WFDISC
      character *(*) RELID_WFTAG
      character *(*) RELID_WFTAPE
      character *(*) RELID_WFTAR
      character *(*) RELID_XPARAM
      integer DBL_ASCII
      integer DBL_BINARY
      integer DBL_DBL
      integer DBL_FLT
      integer DBL_INT
      integer DBL_STR
      integer NUMBER_RELS30
      integer NUMBER_ATTRS
      integer NUMBER_ATTRS30
      parameter (ATTRID_ADATE = "adate"		)
      parameter (ATTRID_ALGID = "algid"		)
      parameter (ATTRID_ALGORITHM = "algorithm"	)
      parameter (ATTRID_AMP = "amp"		)
      parameter (ATTRID_ARID = "arid"		)
      parameter (ATTRID_ARRAY = "array"		)
      parameter (ATTRID_ATTRIB = "attrib"	)
      parameter (ATTRID_ATYPE = "atype"		)
      parameter (ATTRID_AUTH = "auth"		)
      parameter (ATTRID_AZDEF = "azdef"		)
      parameter (ATTRID_AZIMUTH = "azimuth"	)
      parameter (ATTRID_AZRES = "azres"		)
      parameter (ATTRID_BAND = "band"		)
      parameter (ATTRID_BANDW = "bandw"		)
      parameter (ATTRID_BAZIM = "bazim"		)
      parameter (ATTRID_BEAMID = "beamid"	)
      parameter (ATTRID_BELIEF = "belief"	)
      parameter (ATTRID_BESTDC = "bestdc"	)
      parameter (ATTRID_BLKFAC = "blkfac"	)
      parameter (ATTRID_BMTYP = "bmtyp"		)
      parameter (ATTRID_BSLOW = "bslow"		)
      parameter (ATTRID_CALIB = "calib"		)
      parameter (ATTRID_CALPER = "calper"	)
      parameter (ATTRID_CALRATIO = "calratio"	)
      parameter (ATTRID_CDATE = "cdate"		)
      parameter (ATTRID_CDPERR = "cdperr"	)
      parameter (ATTRID_CFREQ = "cfreq"		)
      parameter (ATTRID_CHAN = "chan"		)
      parameter (ATTRID_CHANA = "chana"		)
      parameter (ATTRID_CHANID = "chanid"	)
      parameter (ATTRID_CHANO = "chano"		)
      parameter (ATTRID_CHID = "chid"		)
      parameter (ATTRID_CLAERR = "claerr"	)
      parameter (ATTRID_CLIP = "clip"		)
      parameter (ATTRID_CLOERR = "cloerr"	)
      parameter (ATTRID_CMPX = "cmpx"		)
      parameter (ATTRID_CODA = "coda"		)
      parameter (ATTRID_CODDES = "coddes"	)
      parameter (ATTRID_CODE = "code"		)
      parameter (ATTRID_COLDEP = "coldep"	)
      parameter (ATTRID_COLDIA = "coldia"	)
      parameter (ATTRID_COLINT = "colint"	)
      parameter (ATTRID_COLVOL = "colvol"	)
      parameter (ATTRID_COMM = "comm"		)
      parameter (ATTRID_COMMID = "commid"	)
      parameter (ATTRID_COMPA = "compa"		)
      parameter (ATTRID_CONF = "conf"		)
      parameter (ATTRID_COTERR = "coterr"	)
      parameter (ATTRID_CTYPE = "ctype"		)
      parameter (ATTRID_CUREV = "curev"		)
      parameter (ATTRID_CUROR = "curor"		)
      parameter (ATTRID_DATATYPE = "datatype"	)
      parameter (ATTRID_DATE = "date"		)
      parameter (ATTRID_DATSW = "datsw"		)
      parameter (ATTRID_DATTYP = "dattyp"	)
      parameter (ATTRID_DAY = "day"		)
      parameter (ATTRID_DEAST = "deast"		)
      parameter (ATTRID_DELAZ = "delaz"		)
      parameter (ATTRID_DELSLO = "delslo"	)
      parameter (ATTRID_DELTA = "delta"		)
      parameter (ATTRID_DELTIM = "deltim"	)
      parameter (ATTRID_DEPDP = "depdp"		)
      parameter (ATTRID_DEPTH = "depth"		)
      parameter (ATTRID_DESCR = "descr"		)
      parameter (ATTRID_DESCRIP = "descrip"	)
      parameter (ATTRID_DFILE = "dfile"		)
      parameter (ATTRID_DIG = "dig"		)
      parameter (ATTRID_DIGITAL = "digital"	)
      parameter (ATTRID_DIP1 = "dip1"		)
      parameter (ATTRID_DIP2 = "dip2"		)
      parameter (ATTRID_DIR = "dir"		)
      parameter (ATTRID_DIST = "dist"		)
      parameter (ATTRID_DLID = "dlid"		)
      parameter (ATTRID_DNORTH = "dnorth"	)
      parameter (ATTRID_DOY = "doy"		)
      parameter (ATTRID_DTYPE = "dtype"		)
      parameter (ATTRID_DURAT = "durat"		)
      parameter (ATTRID_DUSED = "dused"		)
      parameter (ATTRID_EDATE = "edate"		)
      parameter (ATTRID_EDEPTH = "edepth"	)
      parameter (ATTRID_ELEV = "elev"		)
      parameter (ATTRID_EMA = "ema"		)
      parameter (ATTRID_EMARES = "emares"	)
      parameter (ATTRID_ENDTIME = "endtime"	)
      parameter (ATTRID_ESAZ = "esaz"		)
      parameter (ATTRID_ETYPE = "etype"		)
      parameter (ATTRID_EVID = "evid"		)
      parameter (ATTRID_EVNAME = "evname"	)
      parameter (ATTRID_EXLAT = "exlat"		)
      parameter (ATTRID_EXLON = "exlon"		)
      parameter (ATTRID_EXPTYP = "exptyp"	)
      parameter (ATTRID_FILE = "file"		)
      parameter (ATTRID_FILES = "files"		)
      parameter (ATTRID_FILTID = "filtid"	)
      parameter (ATTRID_FILTYP = "filtyp"	)
      parameter (ATTRID_FKID = "fkid"		)
      parameter (ATTRID_FKQUAL = "fkqual"	)
      parameter (ATTRID_FKRID = "fkrid"		)
      parameter (ATTRID_FKTYP = "fktyp"		)
      parameter (ATTRID_FM = "fm"		)
      parameter (ATTRID_FNORM = "fnorm"		)
      parameter (ATTRID_FOFF = "foff"		)
      parameter (ATTRID_FSID = "fsid"		)
      parameter (ATTRID_FSRID = "fsrid"		)
      parameter (ATTRID_FSTAT = "fstat"		)
      parameter (ATTRID_FSTYP = "fstyp"		)
      parameter (ATTRID_FTID = "ftid"		)
      parameter (ATTRID_GNORM = "gnorm"		)
      parameter (ATTRID_GRN = "grn"		)
      parameter (ATTRID_GRNAME = "grname"	)
      parameter (ATTRID_HANG = "hang"		)
      parameter (ATTRID_HICUT = "hicut"		)
      parameter (ATTRID_HSLOPE = "hslope"	)
      parameter (ATTRID_IDTYPE = "idtype"	)
      parameter (ATTRID_IDVALUE = "idvalue"	)
      parameter (ATTRID_IMB = "imb"		)
      parameter (ATTRID_IML = "iml"		)
      parameter (ATTRID_IMS = "ims"		)
      parameter (ATTRID_INID = "inid"		)
      parameter (ATTRID_INSNAME = "insname"	)
      parameter (ATTRID_INSTANT = "instant"	)
      parameter (ATTRID_INSTYP = "instyp"	)
      parameter (ATTRID_INSTYPE = "instype"	)
      parameter (ATTRID_INTSCL = "intscl"	)
      parameter (ATTRID_IPHASE = "iphase"	)
      parameter (ATTRID_JDATE = "jdate"		)
      parameter (ATTRID_LAT = "lat"		)
      parameter (ATTRID_LDATE = "ldate"		)
      parameter (ATTRID_LDDATE = "lddate"	)
      parameter (ATTRID_LEAP = "leap"		)
      parameter (ATTRID_LINENO = "lineno"	)
      parameter (ATTRID_LOCATION = "location"	)
      parameter (ATTRID_LOCUT = "locut"		)
      parameter (ATTRID_LOGAT = "logat"		)
      parameter (ATTRID_LON = "lon"		)
      parameter (ATTRID_LSLOPE = "lslope"	)
      parameter (ATTRID_LTYPE = "ltype"		)
      parameter (ATTRID_MAG = "mag"		)
      parameter (ATTRID_MAGB = "magb"		)
      parameter (ATTRID_MAGID = "magid"		)
      parameter (ATTRID_MAGLR = "maglr"		)
      parameter (ATTRID_MAGNITUDE = "magnitude"	)
      parameter (ATTRID_MAGSH = "magsh"		)
      parameter (ATTRID_MAGTYPE = "magtype"	)
      parameter (ATTRID_MAXBLK = "maxblk"	)
      parameter (ATTRID_MAXF = "maxf"		)
      parameter (ATTRID_MAXINT = "maxint"	)
      parameter (ATTRID_MAXKX = "maxkx"		)
      parameter (ATTRID_MAXKY = "maxky"		)
      parameter (ATTRID_MAXSX = "maxsx"		)
      parameter (ATTRID_MAXSY = "maxsy"		)
      parameter (ATTRID_MB = "mb"		)
      parameter (ATTRID_MBID = "mbid"		)
      parameter (ATTRID_MEDIUM = "medium"	)
      parameter (ATTRID_MEXPON = "mexpon"	)
      parameter (ATTRID_MFF = "mff"		)
      parameter (ATTRID_MFFERR = "mfferr"	)
      parameter (ATTRID_MINBLK = "minblk"	)
      parameter (ATTRID_ML = "ml"		)
      parameter (ATTRID_MLID = "mlid"		)
      parameter (ATTRID_MNAME = "mname"		)
      parameter (ATTRID_MO = "mo"		)
      parameter (ATTRID_MOAUTH = "moauth"	)
      parameter (ATTRID_MOIST = "moist"		)
      parameter (ATTRID_MON = "mon"		)
      parameter (ATTRID_MRF = "mrf"		)
      parameter (ATTRID_MRFERR = "mrferr"	)
      parameter (ATTRID_MRR = "mrr"		)
      parameter (ATTRID_MRRERR = "mrrerr"	)
      parameter (ATTRID_MRT = "mrt"		)
      parameter (ATTRID_MRTERR = "mrterr"	)
      parameter (ATTRID_MS = "ms"		)
      parameter (ATTRID_MSID = "msid"		)
      parameter (ATTRID_MTF = "mtf"		)
      parameter (ATTRID_MTFERR = "mtferr"	)
      parameter (ATTRID_MTT = "mtt"		)
      parameter (ATTRID_MTTERR = "mtterr"	)
      parameter (ATTRID_NAME = "name"		)
      parameter (ATTRID_NASS = "nass"		)
      parameter (ATTRID_NAXAZM = "naxazm"	)
      parameter (ATTRID_NAXPLG = "naxplg"	)
      parameter (ATTRID_NAXVAL = "naxval"	)
      parameter (ATTRID_NBYTE = "nbyte"		)
      parameter (ATTRID_NCALIB = "ncalib"	)
      parameter (ATTRID_NCALPER = "ncalper"	)
      parameter (ATTRID_NDEF = "ndef"		)
      parameter (ATTRID_NDLID = "ndlid"		)
      parameter (ATTRID_NDP = "ndp"		)
      parameter (ATTRID_NET = "net"		)
      parameter (ATTRID_NETNAME = "netname"	)
      parameter (ATTRID_NETTYPE = "nettype"	)
      parameter (ATTRID_NETWRK = "netwrk"	)
      parameter (ATTRID_NF = "nf"		)
      parameter (ATTRID_NMB = "nmb"		)
      parameter (ATTRID_NMO = "nmo"		)
      parameter (ATTRID_NMS = "nms"		)
      parameter (ATTRID_NORID = "norid"		)
      parameter (ATTRID_NOWFT = "nowft"		)
      parameter (ATTRID_NRLPB = "nrlpb"		)
      parameter (ATTRID_NRMW = "nrmw"		)
      parameter (ATTRID_NSAMP = "nsamp"		)
      parameter (ATTRID_NSLPB = "nslpb"		)
      parameter (ATTRID_NSMW = "nsmw"		)
      parameter (ATTRID_NSTA = "nsta"		)
      parameter (ATTRID_NX = "nx"		)
      parameter (ATTRID_NXALG = "nxalg"		)
      parameter (ATTRID_NXARID = "nxarid"	)
      parameter (ATTRID_NXCHID = "nxchid"	)
      parameter (ATTRID_NXCOMM = "nxcomm"	)
      parameter (ATTRID_NXDLID = "nxdlid"	)
      parameter (ATTRID_NXEVID = "nxevid"	)
      parameter (ATTRID_NXFILT = "nxfilt"	)
      parameter (ATTRID_NXFK = "nxfk"		)
      parameter (ATTRID_NXFKR = "nxfkr"		)
      parameter (ATTRID_NXFS = "nxfs"		)
      parameter (ATTRID_NXFSR = "nxfsr"		)
      parameter (ATTRID_NXFTID = "nxftid"	)
      parameter (ATTRID_NXINID = "nxinid"	)
      parameter (ATTRID_NXORID = "nxorid"	)
      parameter (ATTRID_NXSENS = "nxsens"	)
      parameter (ATTRID_NXSITE = "nxsite"	)
      parameter (ATTRID_NXSPRO = "nxspro"	)
      parameter (ATTRID_NXWFID = "nxwfid"	)
      parameter (ATTRID_NY = "ny"		)
      parameter (ATTRID_OFFDAT = "offdat"	)
      parameter (ATTRID_OFFDATE = "offdate"	)
      parameter (ATTRID_ONDATE = "ondate"	)
      parameter (ATTRID_OPSW = "opsw"		)
      parameter (ATTRID_OPTYP = "optyp"		)
      parameter (ATTRID_ORID = "orid"		)
      parameter (ATTRID_PALDEP = "paldep"	)
      parameter (ATTRID_PAXAZM = "paxazm"	)
      parameter (ATTRID_PAXPLG = "paxplg"	)
      parameter (ATTRID_PAXVAL = "paxval"	)
      parameter (ATTRID_PCHID = "pchid"		)
      parameter (ATTRID_PDLID = "pdlid"		)
      parameter (ATTRID_PER = "per"		)
      parameter (ATTRID_PHASE = "phase"		)
      parameter (ATTRID_PLPREF = "plpref"	)
      parameter (ATTRID_PNAME = "pname"		)
      parameter (ATTRID_PORID = "porid"		)
      parameter (ATTRID_PREFOR = "prefor"	)
      parameter (ATTRID_PVALUE = "pvalue"	)
      parameter (ATTRID_QUAL = "qual"		)
      parameter (ATTRID_RECT = "rect"		)
      parameter (ATTRID_REELSZ = "reelsz"	)
      parameter (ATTRID_REFSTA = "refsta"	)
      parameter (ATTRID_REGION = "region"	)
      parameter (ATTRID_REL = "rel"		)
      parameter (ATTRID_REMARK = "remark"	)
      parameter (ATTRID_RESID = "resid"		)
      parameter (ATTRID_RIPPLE = "ripple"	)
      parameter (ATTRID_RSPTYP = "rsptyp"	)
      parameter (ATTRID_RSPTYPE = "rsptype"	)
      parameter (ATTRID_SAMPRATE = "samprate"	)
      parameter (ATTRID_SDDP = "sddp"		)
      parameter (ATTRID_SDEPTH = "sdepth"	)
      parameter (ATTRID_SDMB = "sdmb"		)
      parameter (ATTRID_SDMO = "sdmo"		)
      parameter (ATTRID_SDMS = "sdms"		)
      parameter (ATTRID_SDOBS = "sdobs"		)
      parameter (ATTRID_SDZDP = "sdzdp"		)
      parameter (ATTRID_SEAZ = "seaz"		)
      parameter (ATTRID_SEGTYP = "segtyp"	)
      parameter (ATTRID_SEGTYPE = "segtype"	)
      parameter (ATTRID_SENSID = "sensid"	)
      parameter (ATTRID_SITEID = "siteid"	)
      parameter (ATTRID_SLIP1 = "slip1"		)
      parameter (ATTRID_SLIP2 = "slip2"		)
      parameter (ATTRID_SLODEF = "slodef"	)
      parameter (ATTRID_SLORES = "slores"	)
      parameter (ATTRID_SLOW = "slow"		)
      parameter (ATTRID_SMAJAX = "smajax"	)
      parameter (ATTRID_SMINAX = "sminax"	)
      parameter (ATTRID_SMPRAT = "smprat"	)
      parameter (ATTRID_SNAME = "sname"		)
      parameter (ATTRID_SNR = "snr"		)
      parameter (ATTRID_SPAUTH = "spauth"	)
      parameter (ATTRID_SPMM = "spmm"		)
      parameter (ATTRID_SPROID = "sproid"	)
      parameter (ATTRID_SPRT = "sprt"		)
      parameter (ATTRID_SPVT = "spvt"		)
      parameter (ATTRID_SRN = "srn"		)
      parameter (ATTRID_SRNAME = "srname"	)
      parameter (ATTRID_STA = "sta"		)
      parameter (ATTRID_STAA = "staa"		)
      parameter (ATTRID_STANAM = "stanam"	)
      parameter (ATTRID_STANAME = "staname"	)
      parameter (ATTRID_STAO = "stao"		)
      parameter (ATTRID_STASSID = "stassid"	)
      parameter (ATTRID_STATYPE = "statype"	)
      parameter (ATTRID_STAV = "stav"		)
      parameter (ATTRID_STID = "stid"		)
      parameter (ATTRID_STIME = "stime"		)
      parameter (ATTRID_STR1 = "str1"		)
      parameter (ATTRID_STR2 = "str2"		)
      parameter (ATTRID_STRIKE = "strike"	)
      parameter (ATTRID_STRING = "string"	)
      parameter (ATTRID_STT = "stt"		)
      parameter (ATTRID_STX = "stx"		)
      parameter (ATTRID_STY = "sty"		)
      parameter (ATTRID_STYPE = "stype"		)
      parameter (ATTRID_STZ = "stz"		)
      parameter (ATTRID_SXX = "sxx"		)
      parameter (ATTRID_SXY = "sxy"		)
      parameter (ATTRID_SXZ = "sxz"		)
      parameter (ATTRID_SYY = "syy"		)
      parameter (ATTRID_SYZ = "syz"		)
      parameter (ATTRID_SZZ = "szz"		)
      parameter (ATTRID_TAGID = "tagid"		)
      parameter (ATTRID_TAGNAME = "tagname"	)
      parameter (ATTRID_TAPEBLOCK = "tapeblock"	)
      parameter (ATTRID_TAPEFILE = "tapefile"	)
      parameter (ATTRID_TARNAM = "tarnam"	)
      parameter (ATTRID_TAXAZM = "taxazm"	)
      parameter (ATTRID_TAXPLG = "taxplg"	)
      parameter (ATTRID_TAXVAL = "taxval"	)
      parameter (ATTRID_TCALIB = "tcalib"	)
      parameter (ATTRID_TDENSE = "tdense"	)
      parameter (ATTRID_TEXT = "text"		)
      parameter (ATTRID_TFILE = "tfile"		)
      parameter (ATTRID_TIME = "time"		)
      parameter (ATTRID_TIMEDEF = "timedef"	)
      parameter (ATTRID_TIMERES = "timeres"	)
      parameter (ATTRID_TLEN = "tlen"		)
      parameter (ATTRID_TMFC = "tmfc"		)
      parameter (ATTRID_TMFI = "tmfi"		)
      parameter (ATTRID_TMNLPB = "tmnlpb"	)
      parameter (ATTRID_TMNMW = "tmnmw"		)
      parameter (ATTRID_TPBLCK = "tpblck"	)
      parameter (ATTRID_TPFILE = "tpfile"	)
      parameter (ATTRID_TPTYPE = "tptype"	)
      parameter (ATTRID_TRATBL = "tratbl"	)
      parameter (ATTRID_TSHIFT = "tshift"	)
      parameter (ATTRID_TSITE = "tsite"		)
      parameter (ATTRID_TUPID = "tupid"		)
      parameter (ATTRID_UNCERTAINTY = "uncertainty"	)
      parameter (ATTRID_USEDFT = "usedft"	)
      parameter (ATTRID_VANG = "vang"		)
      parameter (ATTRID_VELID = "velid"		)
      parameter (ATTRID_VMODEL = "vmodel"	)
      parameter (ATTRID_VOLNAM = "volnam"	)
      parameter (ATTRID_VOLNAME = "volname"	)
      parameter (ATTRID_WATDEP = "watdep"	)
      parameter (ATTRID_WFID = "wfid"		)
      parameter (ATTRID_WGT = "wgt"		)
      parameter (ATTRID_YEAR = "year"		)
      parameter (ATTRID_YIELD = "yield"		)
      parameter (ATTRID_YLDMAX = "yldmax"	)
      parameter (DBL_ASCII = 0	)
      parameter (DBL_BINARY = 1	)
      parameter (DBL_DBL = 2	)
      parameter (DBL_FLT = 1	)
      parameter (DBL_INT = 0	)
      parameter (DBL_STR = 3	)
      parameter (NUMBER_ATTRS = 282		)
      parameter (NUMBER_ATTRS30 = 152		)
      parameter (NUMBER_RELS30 = 23	)
      parameter (RELID_AFFILIATION = "affiliation"	)
      parameter (RELID_ALIAS = "alias"	)
      parameter (RELID_ARRIVAL = "arrival"	)
      parameter (RELID_ASSOC = "assoc"	)
      parameter (RELID_BEAM = "beam"		)
      parameter (RELID_CENTRYD = "centryd"	)
      parameter (RELID_CHANNEL = "channel"	)
      parameter (RELID_CHOPER = "choper"	)
      parameter (RELID_CODE = "code"		)
      parameter (RELID_COMMENT = "comment"	)
      parameter (RELID_COUNTER = "counter"	)
      parameter (RELID_DATE = "date"		)
      parameter (RELID_DAY = "day"		)
      parameter (RELID_DETECTION = "detection"	)
      parameter (RELID_DETLOC = "detloc"	)
      parameter (RELID_EVENT = "event"	)
      parameter (RELID_EVWF = "evwf"		)
      parameter (RELID_EXPLO = "explo"		)
      parameter (RELID_EXTRA = "extra"		)
      parameter (RELID_FEATURE = "feature"	)
      parameter (RELID_FILTER = "filter"	)
      parameter (RELID_FKDISC = "fkdisc"	)
      parameter (RELID_FKREC = "fkrec"		)
      parameter (RELID_FPLANE = "fplane"	)
      parameter (RELID_FSDISC = "fsdisc"	)
      parameter (RELID_FSREC = "fsrec"		)
      parameter (RELID_GREGION = "gregion"	)
      parameter (RELID_INSTRUMENT = "instrument"	)
      parameter (RELID_LASTID = "lastid"	)
      parameter (RELID_LOC = "loc"		)
      parameter (RELID_MOMENT = "moment"	)
      parameter (RELID_NETMAG = "netmag"	)
      parameter (RELID_NETWORK = "network"	)
      parameter (RELID_ORIGERR = "origerr"	)
      parameter (RELID_ORIGIN = "origin"	)
      parameter (RELID_REMARK = "remark"	)
      parameter (RELID_SENSOR = "sensor"	)
      parameter (RELID_SIGPRO = "sigpro"	)
      parameter (RELID_SITE = "site"	)
      parameter (RELID_SITECHAN = "sitechan"	)
      parameter (RELID_SREGION = "sregion"	)
      parameter (RELID_STALOG = "stalog"	)
      parameter (RELID_STAMAG = "stamag"	)
      parameter (RELID_STASSOC = "stassoc"	)
      parameter (RELID_STATION = "station"	)
      parameter (RELID_TAPE = "tape"		)
      parameter (RELID_WFDISC = "wfdisc"	)
      parameter (RELID_WFTAG = "wftag"	)
      parameter (RELID_WFTAPE = "wftape"	)
      parameter (RELID_WFTAR = "wftar"	)
      parameter (RELID_XPARAM = "xparam"	)

c $Id: dbmaprelease.f,v 1.11 2010-10-26 17:02:56 mitch Exp $ 
# 1731 "dbmaprelease.F" 2 
      real*8 elat, elon
      character*80 sta, staname
      real*4 xlat4, xlon4, elev
c
      call xydel2latlon (xlat0, xlon0, x, y,
     +                         xlat2, xlon2)
      amin = 1.e30
      call dbgetntuples (idb, RELID_SITE, ntuples)
      do 100  i = 1, ntuples
	call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_LON, xlon4,
     +                   ATTRID_LAT, xlat4,
     +			 0)
        xlat = xlat4
        xlon = xlon4
        call latlon2xydel (xlat, xlon, xlat2, xlon2,
     +                         xdel, ydel)
        dist = sqrt(xdel**2 + ydel**2)
	if (dist .lt. amin) then
	  amin = dist
	  ihold = i
	end if
  100 continue
      i = ihold
      call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_STA, sta,
     +                   ATTRID_LON, xlon4,
     +                   ATTRID_LAT, xlat4,
     +                   ATTRID_ELEV, elev,
     +			 0)
      write (6, '(a)') ' '
      write (6, '(a,a)')    'STA:    ',sta(1:ilen(sta))
      write (6, '(a,f9.4)') 'LAT:    ',xlat4
      write (6, '(a,f9.4)') 'LON:    ',xlon4
      write (6, '(a,f9.4)') 'ELEV:   ',elev
      xlat2 = xlat4
      xlon2 = xlon4
      call latlon2xydel (xlat0, xlon0, xlat2, xlon2,
     +                         x, y)
c    
      return
      end
      subroutine symbol (type, x, y, size, thick, iclip, ifill)
      character*(*)      type
c
c     common /pscl/ xmin,xmax,ymin,ymax,xscale,yscale,xrange,yrange
c
      real*4 xplt(10), yplt(10)
      character*8 xtype, ytype
c
      call nsymbol (type, x, y, size, thick, iclip, ifill)
c
      return
      end
c     subroutine setans
c     character*1 ans
c     common /setanss/ ans
c
c     ans = 'R'
c     return
c     end
