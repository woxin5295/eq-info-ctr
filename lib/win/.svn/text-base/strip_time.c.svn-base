
#include <stdlib.h>
#include "win.h"

unsigned char *
strip_time( unsigned char *block, double *epoch )
{
        char    bcd_timecode[6];

        memcpy( &bcd_timecode[0], block, 6 );
        *epoch = japan_timecode_to_epoch( bcd_timecode );
        return block + 6;
}

