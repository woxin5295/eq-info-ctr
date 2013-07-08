/* .c
 * NAME: g2i.c 
 * PURPOSE:  convert 16 bit gainrang format to 32 bit sun integers.
 * The CT (pc box) gainranges the 24-bit samples from the borehole
 * digitizers.  The gainrange format is a 16 bit word with [MSB] bits 13,14,15 
 * as the gain code (max needed is 6),  bit 12 containing the size code, and
 * bits 0-11 containing the mantissa.  Since the ct originally hacked the 24
 * bit samples to 16 bit, any useful precision was lost at that point (note
 * that numbers greater than 2048 and less than -2048 may have lost
 * precision).  Therefore, by converting the 16 gainranged numbers to and from
 * 32 bit integers (after the CT's first hack) should not affect the precision
 * of the samples.
 * AUTHOR: Mark Richards, Sandia National Labs, Div 9224
 * DATE: 10/11/90
 */

#define TWOtotheELEVENTH 2048
#include <stdio.h>
#include "adsn.h"

long	g2i(pos)
unsigned char	*pos;
{
	extern int	scale[];
	short	gain, sign, short_value;   /*16 bit integers*/
	long	value;
	unsigned char	temp;

	/*reverse order of bytes, because ct produces ibm pc byte order*/
	temp = *pos;
	*pos = *(pos + 1);
	*(pos + 1) = temp;

	gain = *pos;
	gain >>= 5;     /*move gain bits to low order*/
	gain &= 0x0007;  /*zero all but 3 gain bits*/

	/*gives 12 bit 2's comp*/
	short_value = (*pos & 0x0f) * 0x100 + *(pos + 1);
	sign = *pos & 0x10;
	if (sign > 0)
		short_value |= 0xf000;  /*gives 16 bit 2's comp*/

	/*datavalue = (mantissa * 2**11 / scalevalue) */
	value = ((long) short_value * TWOtotheELEVENTH) / scale[gain];

	return(value);
} /* make_long_from_ranged */


