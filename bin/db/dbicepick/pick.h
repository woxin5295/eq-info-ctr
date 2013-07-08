/* @(#)pick.h	1.5 08/13/96 */
/*======================================================================
 *
 * Various defines in support of the phase picking library.
 *
 *====================================================================*/
#ifndef pick_h_included
#define pick_h_included

#include <sys/types.h>

/* Various constants */

#define PICK_IDLE  0
#define PICK_BUSY  1
#define PICK_DONE  2

/* Structure templates */

/* trace data format */

struct pick_trace {
    double tofs;  /* time of first sample  */
    float  sint;  /* sample interval (sec) */
    long nsamp;   /* number of samples     */
    long *data;   /* nsamp words of data   */
};

/* a pick */

struct pick {   
    double time;    /* pick time                  */
    double xpk[3];  /* abs value of first 3 peaks */
    char   fm;      /* first motion flag          */
    int    wt;      /* pick weight                */
};

/* a coda */

struct coda {
    long   aav[6];  /* average abs values */
    float  len;     /* coda length (sec)  */
};

/* Rex Allen picker criteria                     */
/* (identifiers follow original Earthworm picker */

struct pick_rapar {
    int     i5; /* Parameter used to calculate itrm                 */
    int     i6; /* Minimum number of small zero crossings           */
    int     i7; /* Minimum number of big zero crossings             */
    int     i8; /* Minimum size of 1'st three peaks                 */
    int     i9; /* Minimum coda length in seconds                   */
    double  c1; /* Filter parameter for raw data                    */
    double  c2; /* Filter parameter for characteristic function     */
    double  c3; /* Filter parameter for short-term average          */
    double  c4; /* Filter parameter for long-term average           */
    double  c5; /* STA/LTA event threshold                          */
    double  c6; /* Dead station threshold                           */
    double  c7; /* Coda termination threshold                       */
    double  c8; /* Frac of c7 at which alt coda termination used    */
    double  c9; /* Frac of pre-event level for alt coda termination */
    /* Following were hard-coded into original Earthworm picker     */
    double  h1;  /* quiet station constant         (QUIET)          */
    double  h2;  /* used in calculating eabs          (C4)          */
    double  h3;  /* used in calculating crtinc     (EREFS)          */
    double  h4;  /* used in calculating xfrz         (1.6)          */
    int     h5;  /* Max interval between zero crossings, samples    */
    int     h6;  /* used in computing itrm           (150)          */
    int     h7;  /* used in computing itrm (max itrm) (50)          */
    int     h8;  /* used in coda aav (lwindow)       (100)          */
    int     h9;  /* used in coda aav (lwindow)       (200)          */
    double h10;  /* used in computing pick weight    (4.0)          */
    double h11;  /* used in computing pick weight    (6.0)          */
    double h12;  /* used in computing pick weight    (0.5)          */
    double h13;  /* used in computing pick weight  (200.0)          */
    double h14;  /* used in computing pick weight    (3.0)          */
    double h15;  /* used in computing pick weight    (3.0)          */
    double h16;  /* used in computing pick weight  (100.0)          */
    double h17;  /* used in computing pick weight    (2.0)          */
    double h18;  /* used in computing pick weight   (25.0)          */
    int  npwin;  /* number of windows for coda aav calculation      */
    int  *pwin;  /* the windows                                     */
    /* Following are new constants                                  */
    int     n1;  /* number of samples to use to stablize filters    */
};

/* Processing history for Rex Allen picker */

struct pick_rainfo {
    long   dold;      /* Prior value of raw integer data              */
    long   prev_sample;
    int    last_pwin;
    int	   maxcodalen;
    int	   first;
    double rdif;      /* First difference                             */
    double edat;      /* Characteristic function                      */
    double rdat;      /* Filtered data value                          */
    double eabs;      /* Running mean absolute value (aav) of rdat    */
    double esta;      /* STA of characteristic function               */
    double elta;      /* LTA of characteristic function               */
    double eref;      /* STA/LTA reference (trigger) level            */
    double eold;      /* Old value of eref                            */
    double cocrit;    /* Coda measurement threshold                   */
    double crtinc;    /* ecrit zero corssing increment                */
    double ecrit;     /* Event termination level                      */
    int    evlen;     /* Event length in samp                         */
    double famp;      /* Average amplitude                            */
    int    iamp;      /* Sample counter for ave. amp calculation      */
    int    icount;    /* Coda length in number of windows             */
    int    isml;      /* Small zero-crossing counter                  */
    int    k;         /* Index to array of windows to push onto stack */
    int    m;         /* Zero-crossing counter                        */
    int    mint;      /* Interval between zero crossings in samples   */
    int    ndrt;      /* Coda length index within window              */
    int    next;      /* Counter of zero crossings early in P-phase   */
    int    nzero;     /* Big zero-crossing counter                    */
    double rbig;      /* Threshold for big zero crossings             */
    double rlast;     /* Size of last big zero crossing               */
    double rold;      /* Previous value of filtered data              */
    double rsrdat;    /* Running sum of rdat in coda calculation      */
    long   fmdat[10]; /* First 10 points after pick (for first motion)*/
    double tmax;      /* Instantaneous maximum in current half cycle  */
    long   xdot;      /* First difference at pick time                */
    double xfrz;      /* Used in first motion calculation             */
    int    pflag;     /* End-of-pick flag; 1 = pick calculation active*/
    int    cflag;     /* End-of-coda flag; 1 = coda calculation active*/
    struct pick pick; /* Current pick                                 */
    struct coda coda; /* Current coda                                 */
    u_long count;     /* number of samples processed so far           */
    int    save;      /* if non-zero, save intermediate output        */
    /* Following are referenced only if save is set */
    struct {
        struct wfdisc *edat;  /* characteristic function             */
        struct wfdisc *esta;  /* STA of characteristic function      */
        struct wfdisc *elta;  /* LTA of characteristic function      */
        struct wfdisc *eref;  /* STA/LTA reference level             */
        struct wfdisc *ecrit; /* event termination level             */
        struct wfdisc *rdif;  /* First difference                    */
        struct wfdisc *rdat;  /* Filtered data value                 */
        struct wfdisc *eabs;  /* Running mean absolute value of rdat */
        struct wfdisc *isml;  /* Small zero-crossing counter         */
        struct wfdisc *nzero; /* Big zero-crossing counter           */
    } wd;
};

/* function prototypes */

#ifdef __STDC__

/* Public routines */

int Pick_Init(
    int,
    long,
    long
);

void Pick_RASaveInit(
    struct pick_rainfo *,
    struct wfdisc *
);

void Pick_RASaveClose(
    void
);

int Pick_RexAllen(
    struct pick_trace *,
    struct pick_rainfo *,
    struct pick_rapar *,
    struct pick *,
    struct coda *,
    int *,
    int,
    int
);

/* Internal use routines */

int pick_radetect(
    struct pick_rainfo *,
    struct pick_rapar *,
    long,
    long,
    double
);

void pick_rainit(
    struct pick_rainfo *
);

int pick_raprocess(
    struct pick_rainfo *,
    struct pick_rapar *,
    long
);

void pick_rasave(
    struct pick_rainfo *
);

#else
#endif /* __STDC__ */

#endif /* pick_h_included */
