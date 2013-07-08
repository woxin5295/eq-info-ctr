	subroutine aeic_reg (colat, elon, regnam)

c  return a geographic region name for a given lat and lon

c  colat  - colatitude of point (south = 0)
c  elon   - East longitude of point (0-360)
c  nchar  - number of characters in region name
c  regnam - geographic region name

c  this routine was designed specifically for aeic catalog preparation.
c  hence, geographic names in the data file (aeic_regions) are modified
c  from the original flinn-engdahl regions (cf. bssa v. 64, n. 3, pt ii,
c  june 1974).

c  each cell corresponds to a 1x1 deg region in the range 48n-75n, 130w-170e
c  (1680 cells total).  the cells are ordered first by increasing latitude
c  and then by increasing west longitude for a given latitude (e.g. [48n 130w],
c  [48n 131w] ...).  each record contains the latitude of the southern boundary
c  and the longitude of the East boundary (west boundary if the cell is west of
c  180) followed by the region name.

	parameter (latmin = 90 + 48)	!colatitude of 48n
	parameter (latmax = 90 + 75)	!colatitude of 75n
	parameter (lonmin = 170)	!170e
	parameter (lonmax = 360 - 130)	!east longitude of 130w

	parameter (ncells = (latmax-latmin+1)*(lonmax-lonmin))

	parameter (maxlen = 100)

	real*4 colat, elon
	character*(*) regnam

	character*100 cell(ncells)

        data cell(   1) /"48n130wit Vancouver Island region"/
        data cell(   2) /"48n131wit Vancouver Island region"/
        data cell(   3) /"48n132wwest of Vancouver Island"/
        data cell(   4) /"48n133wwest of Vancouver Island"/
        data cell(   5) /"48n134wwest of Vancouver Island"/
        data cell(   6) /"48n135wwest of Vancouver Island"/
        data cell(   7) /"48n136wwest of Vancouver Island"/
        data cell(   8) /"48n137wwest of Vancouver Island"/
        data cell(   9) /"48n138wwest of Vancouver Island"/
        data cell(  10) /"48n139wwest of Vancouver Island"/
        data cell(  11) /"48n140wwest of Vancouver Island"/
        data cell(  12) /"48n141wwest of Vancouver Island"/
        data cell(  13) /"48n142wsouth of Alaska"/
        data cell(  14) /"48n143wsouth of Alaska"/
        data cell(  15) /"48n144wsouth of Alaska"/
        data cell(  16) /"48n145wsouth of Alaska"/
        data cell(  17) /"48n146wsouth of Alaska"/
        data cell(  18) /"48n147wsouth of Alaska"/
        data cell(  19) /"48n148wsouth of Alaska"/
        data cell(  20) /"48n149wsouth of Alaska"/
        data cell(  21) /"48n150wsouth of Alaska"/
        data cell(  22) /"48n151wsouth of Alaska"/
        data cell(  23) /"48n152wsouth of Alaska"/
        data cell(  24) /"48n153wsouth of Alaska"/
        data cell(  25) /"48n154wsouth of Alaska"/
        data cell(  26) /"48n155wsouth of Alaska"/
        data cell(  27) /"48n156wsouth of Alaska"/
        data cell(  28) /"48n157wsouth of Alaska"/
        data cell(  29) /"48n158wsouth of Alaska"/
        data cell(  30) /"48n159wsouth of Alaska"/
        data cell(  31) /"48n160wsouth of Alaska"/
        data cell(  32) /"48n161wsouth of Alaska"/
        data cell(  33) /"48n162wsouth of Alaska"/
        data cell(  34) /"48n163wsouth of Alaska"/
        data cell(  35) /"48n164wsouth of Alaska"/
        data cell(  36) /"48n165wit Aleutian Islands roa"/
        data cell(  37) /"48n166wit Aleutian Islands roa"/
        data cell(  38) /"48n167wit Aleutian Islands roa"/
        data cell(  39) /"48n168wit Aleutian Islands roa"/
        data cell(  40) /"48n169wit Aleutian Islands roa"/
        data cell(  41) /"48n170wit Aleutian Islands roa"/
        data cell(  42) /"48n171wit Aleutian Islands roa"/
        data cell(  43) /"48n172wit Aleutian Islands roa"/
        data cell(  44) /"48n173wit Aleutian Islands roa"/
        data cell(  45) /"48n174wit Aleutian Islands roa"/
        data cell(  46) /"48n175wit Aleutian Islands roa"/
        data cell(  47) /"48n176wit Aleutian Islands roa"/
        data cell(  48) /"48n177wit Aleutian Islands roa"/
        data cell(  49) /"48n178wit Aleutian Islands roa"/
        data cell(  50) /"48n179wit Aleutian Islands roa"/
        data cell(  51) /"48n179eit Aleutian Islands roa"/
        data cell(  52) /"48n178eit Aleutian Islands roa"/
        data cell(  53) /"48n177eit Aleutian Islands roa"/
        data cell(  54) /"48n176eit Aleutian Islands roa"/
        data cell(  55) /"48n175eit Aleutian Islands roa"/
        data cell(  56) /"48n174eit Aleutian Islands roa"/
        data cell(  57) /"48n173eit Aleutian Islands roa"/
        data cell(  58) /"48n172eit Aleutian Islands roa"/
        data cell(  59) /"48n171eit Aleutian Islands roa"/
        data cell(  60) /"48n170eit Aleutian Islands roa"/
        data cell(  61) /"49n130wit Vancouver Island region"/
        data cell(  62) /"49n131wit Vancouver Island region"/
        data cell(  63) /"49n132wwest of Vancouver Island"/
        data cell(  64) /"49n133wwest of Vancouver Island"/
        data cell(  65) /"49n134wwest of Vancouver Island"/
        data cell(  66) /"49n135wwest of Vancouver Island"/
        data cell(  67) /"49n136wwest of Vancouver Island"/
        data cell(  68) /"49n137wwest of Vancouver Island"/
        data cell(  69) /"49n138wwest of Vancouver Island"/
        data cell(  70) /"49n139wwest of Vancouver Island"/
        data cell(  71) /"49n140wwest of Vancouver Island"/
        data cell(  72) /"49n141wwest of Vancouver Island"/
        data cell(  73) /"49n142wsouth of Alaska"/
        data cell(  74) /"49n143wsouth of Alaska"/
        data cell(  75) /"49n144wsouth of Alaska"/
        data cell(  76) /"49n145wsouth of Alaska"/
        data cell(  77) /"49n146wsouth of Alaska"/
        data cell(  78) /"49n147wsouth of Alaska"/
        data cell(  79) /"49n148wsouth of Alaska"/
        data cell(  80) /"49n149wsouth of Alaska"/
        data cell(  81) /"49n150wsouth of Alaska"/
        data cell(  82) /"49n151wsouth of Alaska"/
        data cell(  83) /"49n152wsouth of Alaska"/
        data cell(  84) /"49n153wsouth of Alaska"/
        data cell(  85) /"49n154wsouth of Alaska"/
        data cell(  86) /"49n155wsouth of Alaska"/
        data cell(  87) /"49n156wsouth of Alaska"/
        data cell(  88) /"49n157wsouth of Alaska"/
        data cell(  89) /"49n158wsouth of Alaska"/
        data cell(  90) /"49n159wsouth of Alaska"/
        data cell(  91) /"49n160wsouth of Alaska"/
        data cell(  92) /"49n161wsouth of Alaska"/
        data cell(  93) /"49n162wsouth of Alaska"/
        data cell(  94) /"49n163wsouth of Alaska"/
        data cell(  95) /"49n164wsouth of Alaska"/
        data cell(  96) /"49n165wit Aleutian Islands roa"/
        data cell(  97) /"49n166wit Aleutian Islands roa"/
        data cell(  98) /"49n167wit Aleutian Islands roa"/
        data cell(  99) /"49n168wit Aleutian Islands roa"/
        data cell( 100) /"49n169wit Aleutian Islands roa"/
        data cell( 101) /"49n170wit Aleutian Islands roa"/
        data cell( 102) /"49n171wit Aleutian Islands roa"/
        data cell( 103) /"49n172wit Aleutian Islands roa"/
        data cell( 104) /"49n173wit Aleutian Islands roa"/
        data cell( 105) /"49n174wit Aleutian Islands roa"/
        data cell( 106) /"49n175wit Aleutian Islands roa"/
        data cell( 107) /"49n176wit Aleutian Islands roa"/
        data cell( 108) /"49n177wit Aleutian Islands roa"/
        data cell( 109) /"49n178wit Aleutian Islands roa"/
        data cell( 110) /"49n179wit Aleutian Islands roa"/
        data cell( 111) /"49n179eit Aleutian Islands roa"/
        data cell( 112) /"49n178eit Aleutian Islands roa"/
        data cell( 113) /"49n177eit Aleutian Islands roa"/
        data cell( 114) /"49n176eit Aleutian Islands roa"/
        data cell( 115) /"49n175eit Aleutian Islands roa"/
        data cell( 116) /"49n174eit Aleutian Islands roa"/
        data cell( 117) /"49n173eit Aleutian Islands roa"/
        data cell( 118) /"49n172eit Aleutian Islands roa"/
        data cell( 119) /"49n171eit Aleutian Islands roa"/
        data cell( 120) /"49n170eit Aleutian Islands roa"/
        data cell( 121) /"50n130wit Vancouver Island region"/
        data cell( 122) /"50n131wit Vancouver Island region"/
        data cell( 123) /"50n132wwest of Vancouver Island"/
        data cell( 124) /"50n133wwest of Vancouver Island"/
        data cell( 125) /"50n134wwest of Vancouver Island"/
        data cell( 126) /"50n135wwest of Vancouver Island"/
        data cell( 127) /"50n136wwest of Vancouver Island"/
        data cell( 128) /"50n137wwest of Vancouver Island"/
        data cell( 129) /"50n138wwest of Vancouver Island"/
        data cell( 130) /"50n139wwest of Vancouver Island"/
        data cell( 131) /"50n140wwest of Vancouver Island"/
        data cell( 132) /"50n141wwest of Vancouver Island"/
        data cell( 133) /"50n142wsouth of Alaska"/
        data cell( 134) /"50n143wsouth of Alaska"/
        data cell( 135) /"50n144wsouth of Alaska"/
        data cell( 136) /"50n145wsouth of Alaska"/
        data cell( 137) /"50n146wsouth of Alaska"/
        data cell( 138) /"50n147wsouth of Alaska"/
        data cell( 139) /"50n148wsouth of Alaska"/
        data cell( 140) /"50n149wsouth of Alaska"/
        data cell( 141) /"50n150wsouth of Alaska"/
        data cell( 142) /"50n151wsouth of Alaska"/
        data cell( 143) /"50n152wsouth of Alaska"/
        data cell( 144) /"50n153wsouth of Alaska"/
        data cell( 145) /"50n154wsouth of Alaska"/
        data cell( 146) /"50n155wsouth of Alaska"/
        data cell( 147) /"50n156wsouth of Alaska"/
        data cell( 148) /"50n157wsouth of Alaska"/
        data cell( 149) /"50n158wsouth of Alaska"/
        data cell( 150) /"50n159wsouth of Alaska"/
        data cell( 151) /"50n160wsouth of Alaska"/
        data cell( 152) /"50n161wsouth of Alaska"/
        data cell( 153) /"50n162wsouth of Alaska"/
        data cell( 154) /"50n163wsouth of Alaska"/
        data cell( 155) /"50n164wsouth of Alaska"/
        data cell( 156) /"50n165wit Aleutian Islands roa"/
        data cell( 157) /"50n166wit Aleutian Islands roa"/
        data cell( 158) /"50n167wit Aleutian Islands roa"/
        data cell( 159) /"50n168wit Aleutian Islands roa"/
        data cell( 160) /"50n169wit Aleutian Islands roa"/
        data cell( 161) /"50n170wit Aleutian Islands roa"/
        data cell( 162) /"50n171wit Aleutian Islands roa"/
        data cell( 163) /"50n172wit Andreanof Islands roa"/
        data cell( 164) /"50n173wit Andreanof Islands roa"/
        data cell( 165) /"50n174wit Andreanof Islands roa"/
        data cell( 166) /"50n175wit Andreanof Islands roa"/
        data cell( 167) /"50n176wit Andreanof Islands roa"/
        data cell( 168) /"50n177wit Andreanof Islands roa"/
        data cell( 169) /"50n178wit Andreanof Islands roa"/
        data cell( 170) /"50n179wit Andreanof Islands roa"/
        data cell( 171) /"50n179eit Rat Islands roa"/
        data cell( 172) /"50n178eit Rat Islands roa"/
        data cell( 173) /"50n177eit Rat Islands roa"/
        data cell( 174) /"50n176eit Rat Islands roa"/
        data cell( 175) /"50n175eit Rat Islands roa"/
        data cell( 176) /"50n174eit Aleutian Islands roa"/
        data cell( 177) /"50n173eit Aleutian Islands roa"/
        data cell( 178) /"50n172eit Aleutian Islands roa"/
        data cell( 179) /"50n171eit Aleutian Islands roa"/
        data cell( 180) /"50n170eit Aleutian Islands roa"/
        data cell( 181) /"51n130wit Queen Charlotte Islands region"/
        data cell( 182) /"51n131wit Queen Charlotte Islands region"/
        data cell( 183) /"51n132wit Queen Charlotte Islands region"/
        data cell( 184) /"51n133wit Queen Charlotte Islands region"/
        data cell( 185) /"51n134wit Queen Charlotte Islands region"/
        data cell( 186) /"51n135wwest of Vancouver Island"/
        data cell( 187) /"51n136wwest of Vancouver Island"/
        data cell( 188) /"51n137wwest of Vancouver Island"/
        data cell( 189) /"51n138wwest of Vancouver Island"/
        data cell( 190) /"51n139wwest of Vancouver Island"/
        data cell( 191) /"51n140wwest of Vancouver Island"/
        data cell( 192) /"51n141wwest of Vancouver Island"/
        data cell( 193) /"51n142wsouth of Alaska"/
        data cell( 194) /"51n143wsouth of Alaska"/
        data cell( 195) /"51n144wsouth of Alaska"/
        data cell( 196) /"51n145wsouth of Alaska"/
        data cell( 197) /"51n146wsouth of Alaska"/
        data cell( 198) /"51n147wsouth of Alaska"/
        data cell( 199) /"51n148wsouth of Alaska"/
        data cell( 200) /"51n149wsouth of Alaska"/
        data cell( 201) /"51n150wsouth of Alaska"/
        data cell( 202) /"51n151wsouth of Alaska"/
        data cell( 203) /"51n152wsouth of Alaska"/
        data cell( 204) /"51n153wsouth of Alaska"/
        data cell( 205) /"51n154wsouth of Alaska"/
        data cell( 206) /"51n155wsouth of Alaska"/
        data cell( 207) /"51n156wsouth of Alaska"/
        data cell( 208) /"51n157wsouth of Alaska"/
        data cell( 209) /"51n158wsouth of Alaska"/
        data cell( 210) /"51n159wsouth of Alaska"/
        data cell( 211) /"51n160wsouth of Alaska"/
        data cell( 212) /"51n161wsouth of Alaska"/
        data cell( 213) /"51n162wsouth of Alaska"/
        data cell( 214) /"51n163wsouth of Alaska"/
        data cell( 215) /"51n164wsouth of Alaska"/
        data cell( 216) /"51n165wit Aleutian Islands roa"/
        data cell( 217) /"51n166wit Aleutian Islands roa"/
        data cell( 218) /"51n167wit Fox Islands roa"/
        data cell( 219) /"51n168wit Fox Islands roa"/
        data cell( 220) /"51n169wit Fox Islands roa"/
        data cell( 221) /"51n170wit Fox Islands roa"/
        data cell( 222) /"51n171wit Fox Islands roa"/
        data cell( 223) /"51n172wit Andreanof Islands roa"/
        data cell( 224) /"51n173wit Andreanof Islands roa"/
        data cell( 225) /"51n174wit Andreanof Islands roa"/
        data cell( 226) /"51n175wit Andreanof Islands roa"/
        data cell( 227) /"51n176wit Andreanof Islands roa"/
        data cell( 228) /"51n177wit Andreanof Islands roa"/
        data cell( 229) /"51n178wit Andreanof Islands roa"/
        data cell( 230) /"51n179wit Andreanof Islands roa"/
        data cell( 231) /"51n179eit Rat Islands roa"/
        data cell( 232) /"51n178eit Rat Islands roa"/
        data cell( 233) /"51n177eit Rat Islands roa"/
        data cell( 234) /"51n176eit Rat Islands roa"/
        data cell( 235) /"51n175eit Rat Islands roa"/
        data cell( 236) /"51n174eit Near Islands roa"/
        data cell( 237) /"51n173eit Near Islands roa"/
        data cell( 238) /"51n172eit Near Islands roa"/
        data cell( 239) /"51n171eit Near Islands roa"/
        data cell( 240) /"51n170eit Near Islands roa"/
        data cell( 241) /"52n130wit Queen Charlotte Islands region"/
        data cell( 242) /"52n131wit Queen Charlotte Islands region"/
        data cell( 243) /"52n132wit Queen Charlotte Islands region"/
        data cell( 244) /"52n133wit Queen Charlotte Islands region"/
        data cell( 245) /"52n134wit Queen Charlotte Islands region"/
        data cell( 246) /"52n135wwest of Vancouver Island"/
        data cell( 247) /"52n136wwest of Vancouver Island"/
        data cell( 248) /"52n137wwest of Vancouver Island"/
        data cell( 249) /"52n138wwest of Vancouver Island"/
        data cell( 250) /"52n139wwest of Vancouver Island"/
        data cell( 251) /"52n140wwest of Vancouver Island"/
        data cell( 252) /"52n141wwest of Vancouver Island"/
        data cell( 253) /"52n142wsouth of Alaska"/
        data cell( 254) /"52n143wsouth of Alaska"/
        data cell( 255) /"52n144wsouth of Alaska"/
        data cell( 256) /"52n145wsouth of Alaska"/
        data cell( 257) /"52n146wsouth of Alaska"/
        data cell( 258) /"52n147wsouth of Alaska"/
        data cell( 259) /"52n148wsouth of Alaska"/
        data cell( 260) /"52n149wsouth of Alaska"/
        data cell( 261) /"52n150wsouth of Alaska"/
        data cell( 262) /"52n151wsouth of Alaska"/
        data cell( 263) /"52n152wsouth of Alaska"/
        data cell( 264) /"52n153wsouth of Alaska"/
        data cell( 265) /"52n154wsouth of Alaska"/
        data cell( 266) /"52n155wsouth of Alaska"/
        data cell( 267) /"52n156wsouth of Alaska"/
        data cell( 268) /"52n157wsouth of Alaska"/
        data cell( 269) /"52n158wsouth of Alaska"/
        data cell( 270) /"52n159wsouth of Alaska"/
        data cell( 271) /"52n160wsouth of Alaska"/
        data cell( 272) /"52n161wsouth of Alaska"/
        data cell( 273) /"52n162wsouth of Alaska"/
        data cell( 274) /"52n163wsouth of Alaska"/
        data cell( 275) /"52n164wsouth of Alaska"/
        data cell( 276) /"52n165wit Aleutian Islands roa"/
        data cell( 277) /"52n166wit Fox Islands roa"/
        data cell( 278) /"52n167wit Fox Islands roa"/
        data cell( 279) /"52n168wit Fox Islands roa"/
        data cell( 280) /"52n169wit Fox Islands roa"/
        data cell( 281) /"52n170wit Fox Islands roa"/
        data cell( 282) /"52n171wit Fox Islands roa"/
        data cell( 283) /"52n172wit Andreanof Islands roa"/
        data cell( 284) /"52n173wit Andreanof Islands roa"/
        data cell( 285) /"52n174wit Andreanof Islands roa"/
        data cell( 286) /"52n175wit Andreanof Islands roa"/
        data cell( 287) /"52n176wit Andreanof Islands roa"/
        data cell( 288) /"52n177wit Andreanof Islands roa"/
        data cell( 289) /"52n178wit Andreanof Islands roa"/
        data cell( 290) /"52n179wit Andreanof Islands roa"/
        data cell( 291) /"52n179eit Rat Islands roa"/
        data cell( 292) /"52n178eit Rat Islands roa"/
        data cell( 293) /"52n177eit Rat Islands roa"/
        data cell( 294) /"52n176eit Rat Islands roa"/
        data cell( 295) /"52n175eit Rat Islands roa"/
        data cell( 296) /"52n174eit Near Islands roa"/
        data cell( 297) /"52n173eit Near Islands roa"/
        data cell( 298) /"52n172eit Near Islands roa"/
        data cell( 299) /"52n171eit Near Islands roa"/
        data cell( 300) /"52n170eit Near Islands roa"/
        data cell( 301) /"53n130wit Queen Charlotte Islands region"/
        data cell( 302) /"53n131wit Queen Charlotte Islands region"/
        data cell( 303) /"53n132wit Queen Charlotte Islands region"/
        data cell( 304) /"53n133wit Queen Charlotte Islands region"/
        data cell( 305) /"53n134wit Queen Charlotte Islands region"/
        data cell( 306) /"53n135wwest of Vancouver Island"/
        data cell( 307) /"53n136wwest of Vancouver Island"/
        data cell( 308) /"53n137wwest of Vancouver Island"/
        data cell( 309) /"53n138wwest of Vancouver Island"/
        data cell( 310) /"53n139wwest of Vancouver Island"/
        data cell( 311) /"53n140wwest of Vancouver Island"/
        data cell( 312) /"53n141wwest of Vancouver Island"/
        data cell( 313) /"53n142wsouth of Alaska"/
        data cell( 314) /"53n143wsouth of Alaska"/
        data cell( 315) /"53n144wsouth of Alaska"/
        data cell( 316) /"53n145wsouth of Alaska"/
        data cell( 317) /"53n146wsouth of Alaska"/
        data cell( 318) /"53n147wsouth of Alaska"/
        data cell( 319) /"53n148wsouth of Alaska"/
        data cell( 320) /"53n149wsouth of Alaska"/
        data cell( 321) /"53n150wsouth of Alaska"/
        data cell( 322) /"53n151wsouth of Alaska"/
        data cell( 323) /"53n152wsouth of Alaska"/
        data cell( 324) /"53n153wsouth of Alaska"/
        data cell( 325) /"53n154wsouth of Alaska"/
        data cell( 326) /"53n155wsouth of Alaska"/
        data cell( 327) /"53n156wsouth of Alaska"/
        data cell( 328) /"53n157wsouth of Alaska"/
        data cell( 329) /"53n158wsouth of Alaska"/
        data cell( 330) /"53n159wsouth of Alaska"/
        data cell( 331) /"53n160wsouth of Alaska"/
        data cell( 332) /"53n161wsouth of Alaska"/
        data cell( 333) /"53n162wsouth of Alaska"/
        data cell( 334) /"53n163wit Unimak Island roa"/
        data cell( 335) /"53n164wit Unimak Island roa"/
        data cell( 336) /"53n165wit Fox Islands roa"/
        data cell( 337) /"53n166wit Fox Islands roa"/
        data cell( 338) /"53n167wit Fox Islands roa"/
        data cell( 339) /"53n168wit Fox Islands roa"/
        data cell( 340) /"53n169wit Fox Islands roa"/
        data cell( 341) /"53n170wit Fox Islands roa"/
        data cell( 342) /"53n171wit Fox Islands roa"/
        data cell( 343) /"53n172wit Andreanof Islands roa"/
        data cell( 344) /"53n173wit Andreanof Islands roa"/
        data cell( 345) /"53n174wit Andreanof Islands roa"/
        data cell( 346) /"53n175wit Andreanof Islands roa"/
        data cell( 347) /"53n176wit Andreanof Islands roa"/
        data cell( 348) /"53n177wit Andreanof Islands roa"/
        data cell( 349) /"53n178wit Andreanof Islands roa"/
        data cell( 350) /"53n179wit Andreanof Islands roa"/
        data cell( 351) /"53n179eit Rat Islands roa"/
        data cell( 352) /"53n178eit Rat Islands roa"/
        data cell( 353) /"53n177eit Rat Islands roa"/
        data cell( 354) /"53n176eit Rat Islands roa"/
        data cell( 355) /"53n175eit Rat Islands roa"/
        data cell( 356) /"53n174eit Near Islands roa"/
        data cell( 357) /"53n173eit Near Islands roa"/
        data cell( 358) /"53n172eit Near Islands roa"/
        data cell( 359) /"53n171eit Near Islands roa"/
        data cell( 360) /"53n170eit Near Islands roa"/
        data cell( 361) /"54n130win southeastern Alaska"/
        data cell( 362) /"54n131wit Queen Charlotte Islands region"/
        data cell( 363) /"54n132wit Queen Charlotte Islands region"/
        data cell( 364) /"54n133wit Queen Charlotte Islands region"/
        data cell( 365) /"54n134wit Queen Charlotte Islands region"/
        data cell( 366) /"54n135wwest of Vancouver Island"/
        data cell( 367) /"54n136wwest of Vancouver Island"/
        data cell( 368) /"54n137wwest of Vancouver Island"/
        data cell( 369) /"54n138wwest of Vancouver Island"/
        data cell( 370) /"54n139wwest of Vancouver Island"/
        data cell( 371) /"54n140wwest of Vancouver Island"/
        data cell( 372) /"54n141wwest of Vancouver Island"/
        data cell( 373) /"54n142wsouth of Alaska"/
        data cell( 374) /"54n143wsouth of Alaska"/
        data cell( 375) /"54n144wsouth of Alaska"/
        data cell( 376) /"54n145wsouth of Alaska"/
        data cell( 377) /"54n146wsouth of Alaska"/
        data cell( 378) /"54n147wsouth of Alaska"/
        data cell( 379) /"54n148wsouth of Alaska"/
        data cell( 380) /"54n149wsouth of Alaska"/
        data cell( 381) /"54n150wsouth of Alaska"/
        data cell( 382) /"54n151wsouth of Alaska"/
        data cell( 383) /"54n152wsouth of Alaska"/
        data cell( 384) /"54n153wsouth of Alaska"/
        data cell( 385) /"54n154wsouth of Alaska"/
        data cell( 386) /"54n155wsouth of Alaska"/
        data cell( 387) /"54n156wsouth of Alaska"/
        data cell( 388) /"54n157wsouth of Alaska"/
        data cell( 389) /"54n158wsouth of Alaska"/
        data cell( 390) /"54n159wsouth of Alaska"/
        data cell( 391) /"54n160wit Alaska Peninsula roa"/
        data cell( 392) /"54n161wit Alaska Peninsula roa"/
        data cell( 393) /"54n162wit Alaska Peninsula roa"/
        data cell( 394) /"54n163wit Unimak Island roa"/
        data cell( 395) /"54n164wit Unimak Island roa"/
        data cell( 396) /"54n165wit Fox Islands roa"/
        data cell( 397) /"54n166wit Fox Islands roa"/
        data cell( 398) /"54n167wit Fox Islands roa"/
        data cell( 399) /"54n168wit Fox Islands roa"/
        data cell( 400) /"54n169wit Fox Islands roa"/
        data cell( 401) /"54n170wit Fox Islands roa"/
        data cell( 402) /"54n171wit Fox Islands roa"/
        data cell( 403) /"54n172wit Bering Sea roa"/
        data cell( 404) /"54n173wit Bering Sea roa"/
        data cell( 405) /"54n174wit Bering Sea roa"/
        data cell( 406) /"54n175wit Bering Sea roa"/
        data cell( 407) /"54n176wit Bering Sea roa"/
        data cell( 408) /"54n177wit Bering Sea roa"/
        data cell( 409) /"54n178wit Bering Sea roa"/
        data cell( 410) /"54n179wit Bering Sea roa"/
        data cell( 411) /"54n179eit Bering Sea roa"/
        data cell( 412) /"54n178eit Bering Sea roa"/
        data cell( 413) /"54n177eit Bering Sea roa"/
        data cell( 414) /"54n176eit Bering Sea roa"/
        data cell( 415) /"54n175eit Bering Sea roa"/
        data cell( 416) /"54n174eit Near Islands roa"/
        data cell( 417) /"54n173eit Near Islands roa"/
        data cell( 418) /"54n172eit Near Islands roa"/
        data cell( 419) /"54n171eit Near Islands roa"/
        data cell( 420) /"54n170eit Near Islands roa"/
        data cell( 421) /"55n130win southeastern Alaska"/
        data cell( 422) /"55n131win southeastern Alaska"/
        data cell( 423) /"55n132win southeastern Alaska"/
        data cell( 424) /"55n133win southeastern Alaska"/
        data cell( 425) /"55n134win southeastern Alaska"/
        data cell( 426) /"55n135woff the coast of southeastern Alaska"/
        data cell( 427) /"55n136woff the coast of southeastern Alaska"/
        data cell( 428) /"55n137woff the coast of southeastern Alaska"/
        data cell( 429) /"55n138woff the coast of southeastern Alaska"/
        data cell( 430) /"55n139woff the coast of southeastern Alaska"/
        data cell( 431) /"55n140woff the coast of southeastern Alaska"/
        data cell( 432) /"55n141woff the coast of southeastern Alaska"/
        data cell( 433) /"55n142wbeneath the Gulf of Alaska"/
        data cell( 434) /"55n143wbeneath the Gulf of Alaska"/
        data cell( 435) /"55n144wbeneath the Gulf of Alaska"/
        data cell( 436) /"55n145wbeneath the Gulf of Alaska"/
        data cell( 437) /"55n146wbeneath the Gulf of Alaska"/
        data cell( 438) /"55n147wbeneath the Gulf of Alaska"/
        data cell( 439) /"55n148wbeneath the Gulf of Alaska"/
        data cell( 440) /"55n149wbeneath the Gulf of Alaska"/
        data cell( 441) /"55n150wbeneath the Gulf of Alaska"/
        data cell( 442) /"55n151wsouth of Alaska"/
        data cell( 443) /"55n152wsouth of Alaska"/
        data cell( 444) /"55n153wsouth of Alaska"/
        data cell( 445) /"55n154wsouth of Alaska"/
        data cell( 446) /"55n155wsouth of Alaska"/
        data cell( 447) /"55n156wsouth of Alaska"/
        data cell( 448) /"55n157wit Alaska Peninsula roa"/
        data cell( 449) /"55n158wit Alaska Peninsula roa"/
        data cell( 450) /"55n159wit Alaska Peninsula roa"/
        data cell( 451) /"55n160wit Alaska Peninsula roa"/
        data cell( 452) /"55n161wit Alaska Peninsula roa"/
        data cell( 453) /"55n162wit Alaska Peninsula roa"/
        data cell( 454) /"55n163wit Unimak Island roa"/
        data cell( 455) /"55n164wit Unimak Island roa"/
        data cell( 456) /"55n165wit Fox Islands roa"/
        data cell( 457) /"55n166wit Fox Islands roa"/
        data cell( 458) /"55n167wit Fox Islands roa"/
        data cell( 459) /"55n168wit Fox Islands roa"/
        data cell( 460) /"55n169wit Bering Sea roa"/
        data cell( 461) /"55n170wit Bering Sea roa"/
        data cell( 462) /"55n171wit Bering Sea roa"/
        data cell( 463) /"55n172wit Bering Sea roa"/
        data cell( 464) /"55n173wit Bering Sea roa"/
        data cell( 465) /"55n174wit Bering Sea roa"/
        data cell( 466) /"55n175wit Bering Sea roa"/
        data cell( 467) /"55n176wit Bering Sea roa"/
        data cell( 468) /"55n177wit Bering Sea roa"/
        data cell( 469) /"55n178wit Bering Sea roa"/
        data cell( 470) /"55n179wit Bering Sea roa"/
        data cell( 471) /"55n179eit Bering Sea roa"/
        data cell( 472) /"55n178eit Bering Sea roa"/
        data cell( 473) /"55n177eit Bering Sea roa"/
        data cell( 474) /"55n176eit Bering Sea roa"/
        data cell( 475) /"55n175eit Bering Sea roa"/
        data cell( 476) /"55n174eit Bering Sea roa"/
        data cell( 477) /"55n173eit Bering Sea roa"/
        data cell( 478) /"55n172eit Bering Sea roa"/
        data cell( 479) /"55n171eit Bering Sea roa"/
        data cell( 480) /"55n170eit Bering Sea roa"/
        data cell( 481) /"56n130win southeastern Alaska"/
        data cell( 482) /"56n131win southeastern Alaska"/
        data cell( 483) /"56n132win southeastern Alaska"/
        data cell( 484) /"56n133win southeastern Alaska"/
        data cell( 485) /"56n134win southeastern Alaska"/
        data cell( 486) /"56n135win southeastern Alaska"/
        data cell( 487) /"56n136woff the coast of southeastern Alaska"/
        data cell( 488) /"56n137woff the coast of southeastern Alaska"/
        data cell( 489) /"56n138woff the coast of southeastern Alaska"/
        data cell( 490) /"56n139woff the coast of southeastern Alaska"/
        data cell( 491) /"56n140woff the coast of southeastern Alaska"/
        data cell( 492) /"56n141woff the coast of southeastern Alaska"/
        data cell( 493) /"56n142wbeneath the Gulf of Alaska"/
        data cell( 494) /"56n143wbeneath the Gulf of Alaska"/
        data cell( 495) /"56n144wbeneath the Gulf of Alaska"/
        data cell( 496) /"56n145wbeneath the Gulf of Alaska"/
        data cell( 497) /"56n146wbeneath the Gulf of Alaska"/
        data cell( 498) /"56n147wbeneath the Gulf of Alaska"/
        data cell( 499) /"56n148wbeneath the Gulf of Alaska"/
        data cell( 500) /"56n149wbeneath the Gulf of Alaska"/
        data cell( 501) /"56n150wbeneath the Gulf of Alaska"/
        data cell( 502) /"56n151wit Kodiak Island roa"/
        data cell( 503) /"56n152wit Kodiak Island roa"/
        data cell( 504) /"56n153wit Kodiak Island roa"/
        data cell( 505) /"56n154wit Kodiak Island roa"/
        data cell( 506) /"56n155wit Alaska Peninsula roa"/
        data cell( 507) /"56n156wit Alaska Peninsula roa"/
        data cell( 508) /"56n157wit Alaska Peninsula roa"/
        data cell( 509) /"56n158wit Alaska Peninsula roa"/
        data cell( 510) /"56n159wit Alaska Peninsula roa"/
        data cell( 511) /"56n160wit Alaska Peninsula roa"/
        data cell( 512) /"56n161wit Alaska Peninsula roa"/
        data cell( 513) /"56n162wit Bristol Bay roa"/
        data cell( 514) /"56n163wit Bering Sea roa"/
        data cell( 515) /"56n164wit Bering Sea roa"/
        data cell( 516) /"56n165wit Bering Sea roa"/
        data cell( 517) /"56n166wit Bering Sea roa"/
        data cell( 518) /"56n167wit Pribilof Islands roa"/
        data cell( 519) /"56n168wit Pribilof Islands roa"/
        data cell( 520) /"56n169wit Pribilof Islands roa"/
        data cell( 521) /"56n170wit Pribilof Islands roa"/
        data cell( 522) /"56n171wit Pribilof Islands roa"/
        data cell( 523) /"56n172wit Bering Sea roa"/
        data cell( 524) /"56n173wit Bering Sea roa"/
        data cell( 525) /"56n174wit Bering Sea roa"/
        data cell( 526) /"56n175wit Bering Sea roa"/
        data cell( 527) /"56n176wit Bering Sea roa"/
        data cell( 528) /"56n177wit Bering Sea roa"/
        data cell( 529) /"56n178wit Bering Sea roa"/
        data cell( 530) /"56n179wit Bering Sea roa"/
        data cell( 531) /"56n179eit Bering Sea roa"/
        data cell( 532) /"56n178eit Bering Sea roa"/
        data cell( 533) /"56n177eit Bering Sea roa"/
        data cell( 534) /"56n176eit Bering Sea roa"/
        data cell( 535) /"56n175eit Bering Sea roa"/
        data cell( 536) /"56n174eit Bering Sea roa"/
        data cell( 537) /"56n173eit Bering Sea roa"/
        data cell( 538) /"56n172eit Bering Sea roa"/
        data cell( 539) /"56n171eit Bering Sea roa"/
        data cell( 540) /"56n170eit Bering Sea roa"/
        data cell( 541) /"57n130win British Columbia"/
        data cell( 542) /"57n131win British Columbia"/
        data cell( 543) /"57n132win southeastern Alaska"/
        data cell( 544) /"57n133win southeastern Alaska"/
        data cell( 545) /"57n134win southeastern Alaska"/
        data cell( 546) /"57n135win southeastern Alaska"/
        data cell( 547) /"57n136win southeastern Alaska"/
        data cell( 548) /"57n137woff the coast of southeastern Alaska"/
        data cell( 549) /"57n138woff the coast of southeastern Alaska"/
        data cell( 550) /"57n139woff the coast of southeastern Alaska"/
        data cell( 551) /"57n140woff the coast of southeastern Alaska"/
        data cell( 552) /"57n141woff the coast of southeastern Alaska"/
        data cell( 553) /"57n142wbeneath the Gulf of Alaska"/
        data cell( 554) /"57n143wbeneath the Gulf of Alaska"/
        data cell( 555) /"57n144wbeneath the Gulf of Alaska"/
        data cell( 556) /"57n145wbeneath the Gulf of Alaska"/
        data cell( 557) /"57n146wbeneath the Gulf of Alaska"/
        data cell( 558) /"57n147wbeneath the Gulf of Alaska"/
        data cell( 559) /"57n148wbeneath the Gulf of Alaska"/
        data cell( 560) /"57n149wbeneath the Gulf of Alaska"/
        data cell( 561) /"57n150wbeneath the Gulf of Alaska"/
        data cell( 562) /"57n151wit Kodiak Island roa"/
        data cell( 563) /"57n152wit Kodiak Island roa"/
        data cell( 564) /"57n153wit Kodiak Island roa"/
        data cell( 565) /"57n154wit Kodiak Island roa"/
        data cell( 566) /"57n155wit Alaska Peninsula roa"/
        data cell( 567) /"57n156wit Alaska Peninsula roa"/
        data cell( 568) /"57n157wit Alaska Peninsula roa"/
        data cell( 569) /"57n158wit Alaska Peninsula roa"/
        data cell( 570) /"57n159wit Bristol Bay roa"/
        data cell( 571) /"57n160wit Bristol Bay roa"/
        data cell( 572) /"57n161wit Bristol Bay roa"/
        data cell( 573) /"57n162wit Bristol Bay roa"/
        data cell( 574) /"57n163wit Bering Sea roa"/
        data cell( 575) /"57n164wit Bering Sea roa"/
        data cell( 576) /"57n165wit Bering Sea roa"/
        data cell( 577) /"57n166wit Bering Sea roa"/
        data cell( 578) /"57n167wit Pribilof Islands roa"/
        data cell( 579) /"57n168wit Pribilof Islands roa"/
        data cell( 580) /"57n169wit Pribilof Islands roa"/
        data cell( 581) /"57n170wit Pribilof Islands roa"/
        data cell( 582) /"57n171wit Pribilof Islands roa"/
        data cell( 583) /"57n172wit Bering Sea roa"/
        data cell( 584) /"57n173wit Bering Sea roa"/
        data cell( 585) /"57n174wit Bering Sea roa"/
        data cell( 586) /"57n175wit Bering Sea roa"/
        data cell( 587) /"57n176wit Bering Sea roa"/
        data cell( 588) /"57n177wit Bering Sea roa"/
        data cell( 589) /"57n178wit Bering Sea roa"/
        data cell( 590) /"57n179wit Bering Sea roa"/
        data cell( 591) /"57n179eit Bering Sea roa"/
        data cell( 592) /"57n178eit Bering Sea roa"/
        data cell( 593) /"57n177eit Bering Sea roa"/
        data cell( 594) /"57n176eit Bering Sea roa"/
        data cell( 595) /"57n175eit Bering Sea roa"/
        data cell( 596) /"57n174eit Bering Sea roa"/
        data cell( 597) /"57n173eit Bering Sea roa"/
        data cell( 598) /"57n172eit Bering Sea roa"/
        data cell( 599) /"57n171eit Bering Sea roa"/
        data cell( 600) /"57n170eit Bering Sea roa"/
        data cell( 601) /"58n130win British Columbia"/
        data cell( 602) /"58n131win British Columbia"/
        data cell( 603) /"58n132win British Columbia"/
        data cell( 604) /"58n133win southeastern Alaska"/
        data cell( 605) /"58n134win southeastern Alaska"/
        data cell( 606) /"58n135win southeastern Alaska"/
        data cell( 607) /"58n136win southeastern Alaska"/
        data cell( 608) /"58n137win southeastern Alaska"/
        data cell( 609) /"58n138win southeastern Alaska"/
        data cell( 610) /"58n139woff the coast of southeastern Alaska"/
        data cell( 611) /"58n140woff the coast of southeastern Alaska"/
        data cell( 612) /"58n141woff the coast of southeastern Alaska"/
        data cell( 613) /"58n142wbeneath the Gulf of Alaska"/
        data cell( 614) /"58n143wbeneath the Gulf of Alaska"/
        data cell( 615) /"58n144wbeneath the Gulf of Alaska"/
        data cell( 616) /"58n145wbeneath the Gulf of Alaska"/
        data cell( 617) /"58n146wbeneath the Gulf of Alaska"/
        data cell( 618) /"58n147wbeneath the Gulf of Alaska"/
        data cell( 619) /"58n148wbeneath the Gulf of Alaska"/
        data cell( 620) /"58n149wbeneath the Gulf of Alaska"/
        data cell( 621) /"58n150wbeneath the Gulf of Alaska"/
        data cell( 622) /"58n151wit Kodiak Island roa"/
        data cell( 623) /"58n152wit Kodiak Island roa"/
        data cell( 624) /"58n153wit Kodiak Island roa"/
        data cell( 625) /"58n154wit Alaska Peninsula roa"/
        data cell( 626) /"58n155wit Alaska Peninsula roa"/
        data cell( 627) /"58n156wit Alaska Peninsula roa"/
        data cell( 628) /"58n157wit Alaska Peninsula roa"/
        data cell( 629) /"58n158wit Bristol Bay roa"/
        data cell( 630) /"58n159wit Bristol Bay roa"/
        data cell( 631) /"58n160wit Bristol Bay roa"/
        data cell( 632) /"58n161wit Bristol Bay roa"/
        data cell( 633) /"58n162wit Bristol Bay roa"/
        data cell( 634) /"58n163wit Bering Sea roa"/
        data cell( 635) /"58n164wit Bering Sea roa"/
        data cell( 636) /"58n165wit Bering Sea roa"/
        data cell( 637) /"58n166wit Bering Sea roa"/
        data cell( 638) /"58n167wit Bering Sea roa"/
        data cell( 639) /"58n168wit Bering Sea roa"/
        data cell( 640) /"58n169wit Bering Sea roa"/
        data cell( 641) /"58n170wit Bering Sea roa"/
        data cell( 642) /"58n171wit Bering Sea roa"/
        data cell( 643) /"58n172wit Bering Sea roa"/
        data cell( 644) /"58n173wit Bering Sea roa"/
        data cell( 645) /"58n174wit Bering Sea roa"/
        data cell( 646) /"58n175wit Bering Sea roa"/
        data cell( 647) /"58n176wit Bering Sea roa"/
        data cell( 648) /"58n177wit Bering Sea roa"/
        data cell( 649) /"58n178wit Bering Sea roa"/
        data cell( 650) /"58n179wit Bering Sea roa"/
        data cell( 651) /"58n179eit Bering Sea roa"/
        data cell( 652) /"58n178eit Bering Sea roa"/
        data cell( 653) /"58n177eit Bering Sea roa"/
        data cell( 654) /"58n176eit Bering Sea roa"/
        data cell( 655) /"58n175eit Bering Sea roa"/
        data cell( 656) /"58n174eit Bering Sea roa"/
        data cell( 657) /"58n173eit Bering Sea roa"/
        data cell( 658) /"58n172eit Bering Sea roa"/
        data cell( 659) /"58n171eit Bering Sea roa"/
        data cell( 660) /"58n170eit Bering Sea roa"/
        data cell( 661) /"59n130win British Columbia"/
        data cell( 662) /"59n131win British Columbia"/
        data cell( 663) /"59n132win British Columbia"/
        data cell( 664) /"59n133win British Columbia"/
        data cell( 665) /"59n134win southeastern Alaska"/
        data cell( 666) /"59n135win southeastern Alaska"/
        data cell( 667) /"59n136win southeastern Alaska"/
        data cell( 668) /"59n137win southeastern Alaska"/
        data cell( 669) /"59n138win southeastern Alaska"/
        data cell( 670) /"59n139wit Yakutat Bay roa"/
        data cell( 671) /"59n140wit Yakutat Bay roa"/
        data cell( 672) /"59n141wit Yakutat Bay roa"/
        data cell( 673) /"59n142wbeneath the Gulf of Alaska"/
        data cell( 674) /"59n143wbeneath the Gulf of Alaska"/
        data cell( 675) /"59n144wbeneath the Gulf of Alaska"/
        data cell( 676) /"59n145wbeneath the Gulf of Alaska"/
        data cell( 677) /"59n146wbeneath the Gulf of Alaska"/
        data cell( 678) /"59n147wbeneath the Gulf of Alaska"/
        data cell( 679) /"59n148wit Kenai Peninsula roa"/
        data cell( 680) /"59n149wit Kenai Peninsula roa"/
        data cell( 681) /"59n150wit Kenai Peninsula roa"/
        data cell( 682) /"59n151wit Kenai Peninsula roa"/
        data cell( 683) /"59n152wit Cook Inlet roa"/
        data cell( 684) /"59n153wit Cook Inlet roa"/
        data cell( 685) /"59n154wit Cook Inlet roa"/
        data cell( 686) /"59n155win southwestern Alaska"/
        data cell( 687) /"59n156win southwestern Alaska"/
        data cell( 688) /"59n157win southwestern Alaska"/
        data cell( 689) /"59n158win southwestern Alaska"/
        data cell( 690) /"59n159win southwestern Alaska"/
        data cell( 691) /"59n160win southwestern Alaska"/
        data cell( 692) /"59n161win southwestern Alaska"/
        data cell( 693) /"59n162win southwestern Alaska"/
        data cell( 694) /"59n163win southwestern Alaska"/
        data cell( 695) /"59n164win southwestern Alaska"/
        data cell( 696) /"59n165win southwestern Alaska"/
        data cell( 697) /"59n166win southwestern Alaska"/
        data cell( 698) /"59n167win southwestern Alaska"/
        data cell( 699) /"59n168wit Bering Sea roa"/
        data cell( 700) /"59n169wit Bering Sea roa"/
        data cell( 701) /"59n170wit Bering Sea roa"/
        data cell( 702) /"59n171wit Bering Sea roa"/
        data cell( 703) /"59n172wit Bering Sea roa"/
        data cell( 704) /"59n173wit Bering Sea roa"/
        data cell( 705) /"59n174wit Bering Sea roa"/
        data cell( 706) /"59n175wit Bering Sea roa"/
        data cell( 707) /"59n176wit Bering Sea roa"/
        data cell( 708) /"59n177wit Bering Sea roa"/
        data cell( 709) /"59n178wit Bering Sea roa"/
        data cell( 710) /"59n179wit Bering Sea roa"/
        data cell( 711) /"59n179eit Bering Sea roa"/
        data cell( 712) /"59n178eit Bering Sea roa"/
        data cell( 713) /"59n177eit Bering Sea roa"/
        data cell( 714) /"59n176eit Bering Sea roa"/
        data cell( 715) /"59n175eit Bering Sea roa"/
        data cell( 716) /"59n174eit Bering Sea roa"/
        data cell( 717) /"59n173eit Bering Sea roa"/
        data cell( 718) /"59n172eit Bering Sea roa"/
        data cell( 719) /"59n171eit Bering Sea roa"/
        data cell( 720) /"59n170ein eastern Siberia"/
        data cell( 721) /"60n130wit Yukon Territory"/
        data cell( 722) /"60n131wit Yukon Territory"/
        data cell( 723) /"60n132wit Yukon Territory"/
        data cell( 724) /"60n133wit Yukon Territory"/
        data cell( 725) /"60n134wit Yukon Territory"/
        data cell( 726) /"60n135wit Yukon Territory"/
        data cell( 727) /"60n136wit Yukon Territory"/
        data cell( 728) /"60n137wit Yukon Territory"/
        data cell( 729) /"60n138wit Yukon Territory"/
        data cell( 730) /"60n139wit Yakutat Bay roa"/
        data cell( 731) /"60n140wit Yakutat Bay roa"/
        data cell( 732) /"60n141wit Yakutat Bay roa"/
        data cell( 733) /"60n142wit Cape Yakataga roa"/
        data cell( 734) /"60n143wit Cape Yakataga roa"/
        data cell( 735) /"60n144wit Cape Yakataga roa"/
        data cell( 736) /"60n145wit Prince William Sound roa"/
        data cell( 737) /"60n146wit Prince William Sound roa"/
        data cell( 738) /"60n147wit Prince William Sound roa"/
        data cell( 739) /"60n148wit Kenai Peninsula roa"/
        data cell( 740) /"60n149wit Kenai Peninsula roa"/
        data cell( 741) /"60n150wit Kenai Peninsula roa"/
        data cell( 742) /"60n151wit Kenai Peninsula roa"/
        data cell( 743) /"60n152wit Cook Inlet roa"/
        data cell( 744) /"60n153wit Cook Inlet roa"/
        data cell( 745) /"60n154win southwestern Alaska"/
        data cell( 746) /"60n155win southwestern Alaska"/
        data cell( 747) /"60n156win southwestern Alaska"/
        data cell( 748) /"60n157win southwestern Alaska"/
        data cell( 749) /"60n158win southwestern Alaska"/
        data cell( 750) /"60n159win southwestern Alaska"/
        data cell( 751) /"60n160win southwestern Alaska"/
        data cell( 752) /"60n161win southwestern Alaska"/
        data cell( 753) /"60n162win southwestern Alaska"/
        data cell( 754) /"60n163win southwestern Alaska"/
        data cell( 755) /"60n164win southwestern Alaska"/
        data cell( 756) /"60n165win southwestern Alaska"/
        data cell( 757) /"60n166win southwestern Alaska"/
        data cell( 758) /"60n167win southwestern Alaska"/
        data cell( 759) /"60n168wit Bering Sea roa"/
        data cell( 760) /"60n169wit Bering Sea roa"/
        data cell( 761) /"60n170wit Bering Sea roa"/
        data cell( 762) /"60n171wit Bering Sea roa"/
        data cell( 763) /"60n172wit Bering Sea roa"/
        data cell( 764) /"60n173wit Bering Sea roa"/
        data cell( 765) /"60n174wit Bering Sea roa"/
        data cell( 766) /"60n175wit Bering Sea roa"/
        data cell( 767) /"60n176wit Bering Sea roa"/
        data cell( 768) /"60n177wit Bering Sea roa"/
        data cell( 769) /"60n178wit Bering Sea roa"/
        data cell( 770) /"60n179wit Bering Sea roa"/
        data cell( 771) /"60n179eit Bering Sea roa"/
        data cell( 772) /"60n178eit Bering Sea roa"/
        data cell( 773) /"60n177eit Bering Sea roa"/
        data cell( 774) /"60n176eit Bering Sea roa"/
        data cell( 775) /"60n175eit Bering Sea roa"/
        data cell( 776) /"60n174eit Bering Sea roa"/
        data cell( 777) /"60n173eit Bering Sea roa"/
        data cell( 778) /"60n172ein eastern Siberia"/
        data cell( 779) /"60n171ein eastern Siberia"/
        data cell( 780) /"60n170ein eastern Siberia"/
        data cell( 781) /"61n130wit Yukon Territory"/
        data cell( 782) /"61n131wit Yukon Territory"/
        data cell( 783) /"61n132wit Yukon Territory"/
        data cell( 784) /"61n133wit Yukon Territory"/
        data cell( 785) /"61n134wit Yukon Territory"/
        data cell( 786) /"61n135wit Yukon Territory"/
        data cell( 787) /"61n136wit Yukon Territory"/
        data cell( 788) /"61n137wit Yukon Territory"/
        data cell( 789) /"61n138wit Yukon Territory"/
        data cell( 790) /"61n139wit Yukon Territory"/
        data cell( 791) /"61n140wit Yukon Territory"/
        data cell( 792) /"61n141wit Cape Yakataga roa"/
        data cell( 793) /"61n142wit Cape Yakataga roa"/
        data cell( 794) /"61n143wit Cape Yakataga roa"/
        data cell( 795) /"61n144wit Cape Yakataga roa"/
        data cell( 796) /"61n145wit Prince William Sound roa"/
        data cell( 797) /"61n146wit Prince William Sound roa"/
        data cell( 798) /"61n147wit Prince William Sound roa"/
        data cell( 799) /"61n148wit Prince William Sound roa"/
        data cell( 800) /"61n149wit Cook Inlet roa"/
        data cell( 801) /"61n150wit Cook Inlet roa"/
        data cell( 802) /"61n151wit Cook Inlet roa"/
        data cell( 803) /"61n152wit Cook Inlet roa"/
        data cell( 804) /"61n153wit Cook Inlet roa"/
        data cell( 805) /"61n154win southwestern Alaska"/
        data cell( 806) /"61n155win southwestern Alaska"/
        data cell( 807) /"61n156win southwestern Alaska"/
        data cell( 808) /"61n157win southwestern Alaska"/
        data cell( 809) /"61n158win southwestern Alaska"/
        data cell( 810) /"61n159win southwestern Alaska"/
        data cell( 811) /"61n160win southwestern Alaska"/
        data cell( 812) /"61n161win southwestern Alaska"/
        data cell( 813) /"61n162win southwestern Alaska"/
        data cell( 814) /"61n163win southwestern Alaska"/
        data cell( 815) /"61n164win southwestern Alaska"/
        data cell( 816) /"61n165win southwestern Alaska"/
        data cell( 817) /"61n166win southwestern Alaska"/
        data cell( 818) /"61n167wit Bering Sea roa"/
        data cell( 819) /"61n168wit Bering Sea roa"/
        data cell( 820) /"61n169wit Bering Sea roa"/
        data cell( 821) /"61n170wit Bering Sea roa"/
        data cell( 822) /"61n171wit Bering Sea roa"/
        data cell( 823) /"61n172wit Bering Sea roa"/
        data cell( 824) /"61n173wit Bering Sea roa"/
        data cell( 825) /"61n174wit Bering Sea roa"/
        data cell( 826) /"61n175wit Bering Sea roa"/
        data cell( 827) /"61n176wit Bering Sea roa"/
        data cell( 828) /"61n177wit Bering Sea roa"/
        data cell( 829) /"61n178wit Bering Sea roa"/
        data cell( 830) /"61n179wit Bering Sea roa"/
        data cell( 831) /"61n179eit Bering Sea roa"/
        data cell( 832) /"61n178eit Bering Sea roa"/
        data cell( 833) /"61n177eit Bering Sea roa"/
        data cell( 834) /"61n176eit Bering Sea roa"/
        data cell( 835) /"61n175ein eastern Siberia"/
        data cell( 836) /"61n174ein eastern Siberia"/
        data cell( 837) /"61n173ein eastern Siberia"/
        data cell( 838) /"61n172ein eastern Siberia"/
        data cell( 839) /"61n171ein eastern Siberia"/
        data cell( 840) /"61n170ein eastern Siberia"/
        data cell( 841) /"62n130wit Yukon Territory"/
        data cell( 842) /"62n131wit Yukon Territory"/
        data cell( 843) /"62n132wit Yukon Territory"/
        data cell( 844) /"62n133wit Yukon Territory"/
        data cell( 845) /"62n134wit Yukon Territory"/
        data cell( 846) /"62n135wit Yukon Territory"/
        data cell( 847) /"62n136wit Yukon Territory"/
        data cell( 848) /"62n137wit Yukon Territory"/
        data cell( 849) /"62n138wit Yukon Territory"/
        data cell( 850) /"62n139wit Yukon Territory"/
        data cell( 851) /"62n140wit Yukon Territory"/
        data cell( 852) /"62n141wit east-central roa"/
        data cell( 853) /"62n142wit east-central roa"/
        data cell( 854) /"62n143wit east-central roa"/
        data cell( 855) /"62n144wit east-central roa"/
        data cell( 856) /"62n145wit east-central roa"/
        data cell( 857) /"62n146wit central roa"/
        data cell( 858) /"62n147wit central roa"/
        data cell( 859) /"62n148wit central roa"/
        data cell( 860) /"62n149wit central roa"/
        data cell( 861) /"62n150wit central roa"/
        data cell( 862) /"62n151wit central roa"/
        data cell( 863) /"62n152wit central roa"/
        data cell( 864) /"62n153wit central roa"/
        data cell( 865) /"62n154wit central roa"/
        data cell( 866) /"62n155wit west-central roa"/
        data cell( 867) /"62n156wit west-central roa"/
        data cell( 868) /"62n157wit west-central roa"/
        data cell( 869) /"62n158wit west-central roa"/
        data cell( 870) /"62n159wit west-central roa"/
        data cell( 871) /"62n160wit west-central roa"/
        data cell( 872) /"62n161wit Seward Peninsula roa"/
        data cell( 873) /"62n162wit Seward Peninsula roa"/
        data cell( 874) /"62n163wit Seward Peninsula roa"/
        data cell( 875) /"62n164wit Seward Peninsula roa"/
        data cell( 876) /"62n165wit Seward Peninsula roa"/
        data cell( 877) /"62n166wit Bering Sea roa"/
        data cell( 878) /"62n167wit Bering Sea roa"/
        data cell( 879) /"62n168wit Bering Sea roa"/
        data cell( 880) /"62n169wit St. LawrenceIsland roa"/
        data cell( 881) /"62n170wit Bering Sea roa"/
        data cell( 882) /"62n171wit Bering Sea roa"/
        data cell( 883) /"62n172wit Bering Sea roa"/
        data cell( 884) /"62n173wit Bering Sea roa"/
        data cell( 885) /"62n174wit Bering Sea roa"/
        data cell( 886) /"62n175wit Bering Sea roa"/
        data cell( 887) /"62n176wit Bering Sea roa"/
        data cell( 888) /"62n177wit Bering Sea roa"/
        data cell( 889) /"62n178wit Bering Sea roa"/
        data cell( 890) /"62n179wit Bering Sea roa"/
        data cell( 891) /"62n179ein eastern Siberia"/
        data cell( 892) /"62n178ein eastern Siberia"/
        data cell( 893) /"62n177ein eastern Siberia"/
        data cell( 894) /"62n176ein eastern Siberia"/
        data cell( 895) /"62n175ein eastern Siberia"/
        data cell( 896) /"62n174ein eastern Siberia"/
        data cell( 897) /"62n173ein eastern Siberia"/
        data cell( 898) /"62n172ein eastern Siberia"/
        data cell( 899) /"62n171ein eastern Siberia"/
        data cell( 900) /"62n170ein eastern Siberia"/
        data cell( 901) /"63n130wit Yukon Territory"/
        data cell( 902) /"63n131wit Yukon Territory"/
        data cell( 903) /"63n132wit Yukon Territory"/
        data cell( 904) /"63n133wit Yukon Territory"/
        data cell( 905) /"63n134wit Yukon Territory"/
        data cell( 906) /"63n135wit Yukon Territory"/
        data cell( 907) /"63n136wit Yukon Territory"/
        data cell( 908) /"63n137wit Yukon Territory"/
        data cell( 909) /"63n138wit Yukon Territory"/
        data cell( 910) /"63n139wit Yukon Territory"/
        data cell( 911) /"63n140wit Yukon Territory"/
        data cell( 912) /"63n141wit east-central roa"/
        data cell( 913) /"63n142wit east-central roa"/
        data cell( 914) /"63n143wit east-central roa"/
        data cell( 915) /"63n144wit east-central roa"/
        data cell( 916) /"63n145wit east-central roa"/
        data cell( 917) /"63n146wit central roa"/
        data cell( 918) /"63n147wit central roa"/
        data cell( 919) /"63n148wit central roa"/
        data cell( 920) /"63n149wit central roa"/
        data cell( 921) /"63n150wit central roa"/
        data cell( 922) /"63n151wit central roa"/
        data cell( 923) /"63n152wit central roa"/
        data cell( 924) /"63n153wit central roa"/
        data cell( 925) /"63n154wit central roa"/
        data cell( 926) /"63n155wit west-central roa"/
        data cell( 927) /"63n156wit west-central roa"/
        data cell( 928) /"63n157wit west-central roa"/
        data cell( 929) /"63n158wit west-central roa"/
        data cell( 930) /"63n159wit west-central roa"/
        data cell( 931) /"63n160wit west-central roa"/
        data cell( 932) /"63n161wit Seward Peninsula roa"/
        data cell( 933) /"63n162wit Seward Peninsula roa"/
        data cell( 934) /"63n163wit Seward Peninsula roa"/
        data cell( 935) /"63n164wit Seward Peninsula roa"/
        data cell( 936) /"63n165wit Bering Strait region"/
        data cell( 937) /"63n166wit Bering Strait region"/
        data cell( 938) /"63n167wit Bering Strait region"/
        data cell( 939) /"63n168wit St. LawrenceIsland roa"/
        data cell( 940) /"63n169wit St. LawrenceIsland roa"/
        data cell( 941) /"63n170wit St. LawrenceIsland roa"/
        data cell( 942) /"63n171wit St. LawrenceIsland roa"/
        data cell( 943) /"63n172wit Bering Sea roa"/
        data cell( 944) /"63n173wit Bering Sea roa"/
        data cell( 945) /"63n174wit Bering Sea roa"/
        data cell( 946) /"63n175wit Bering Sea roa"/
        data cell( 947) /"63n176wit Bering Sea roa"/
        data cell( 948) /"63n177wit Bering Sea roa"/
        data cell( 949) /"63n178wit Bering Sea roa"/
        data cell( 950) /"63n179wit Bering Sea roa"/
        data cell( 951) /"63n179ein eastern Siberia"/
        data cell( 952) /"63n178ein eastern Siberia"/
        data cell( 953) /"63n177ein eastern Siberia"/
        data cell( 954) /"63n176ein eastern Siberia"/
        data cell( 955) /"63n175ein eastern Siberia"/
        data cell( 956) /"63n174ein eastern Siberia"/
        data cell( 957) /"63n173ein eastern Siberia"/
        data cell( 958) /"63n172ein eastern Siberia"/
        data cell( 959) /"63n171ein eastern Siberia"/
        data cell( 960) /"63n170ein eastern Siberia"/
        data cell( 961) /"64n130wit Northwest Territory"/
        data cell( 962) /"64n131wit Yukon Territory"/
        data cell( 963) /"64n132wit Yukon Territory"/
        data cell( 964) /"64n133wit Yukon Territory"/
        data cell( 965) /"64n134wit Yukon Territory"/
        data cell( 966) /"64n135wit Yukon Territory"/
        data cell( 967) /"64n136wit Yukon Territory"/
        data cell( 968) /"64n137wit Yukon Territory"/
        data cell( 969) /"64n138wit Yukon Territory"/
        data cell( 970) /"64n139wit Yukon Territory"/
        data cell( 971) /"64n140wit Yukon Territory"/
        data cell( 972) /"64n141wit east-central roa"/
        data cell( 973) /"64n142wit east-central roa"/
        data cell( 974) /"64n143wit east-central roa"/
        data cell( 975) /"64n144wit east-central roa"/
        data cell( 976) /"64n145wit east-central roa"/
        data cell( 977) /"64n146wit central roa"/
        data cell( 978) /"64n147wit central roa"/
        data cell( 979) /"64n148wit central roa"/
        data cell( 980) /"64n149wit central roa"/
        data cell( 981) /"64n150wit central roa"/
        data cell( 982) /"64n151wit central roa"/
        data cell( 983) /"64n152wit central roa"/
        data cell( 984) /"64n153wit central roa"/
        data cell( 985) /"64n154wit central roa"/
        data cell( 986) /"64n155wit west-central roa"/
        data cell( 987) /"64n156wit west-central roa"/
        data cell( 988) /"64n157wit west-central roa"/
        data cell( 989) /"64n158wit west-central roa"/
        data cell( 990) /"64n159wit west-central roa"/
        data cell( 991) /"64n160wit west-central roa"/
        data cell( 992) /"64n161wit Seward Peninsula roa"/
        data cell( 993) /"64n162wit Seward Peninsula roa"/
        data cell( 994) /"64n163wit Seward Peninsula roa"/
        data cell( 995) /"64n164wit Seward Peninsula roa"/
        data cell( 996) /"64n165wit Seward Peninsula roa"/
        data cell( 997) /"64n166wit Seward Peninsula roa"/
        data cell( 998) /"64n167wit Bering Strait region"/
        data cell( 999) /"64n168wit Bering Strait region"/
        data cell(1000) /"64n169wit Bering Strait region"/
        data cell(1001) /"64n170wit Bering Strait region"/
        data cell(1002) /"64n171win eastern Siberia"/
        data cell(1003) /"64n172win eastern Siberia"/
        data cell(1004) /"64n173win eastern Siberia"/
        data cell(1005) /"64n174win eastern Siberia"/
        data cell(1006) /"64n175win eastern Siberia"/
        data cell(1007) /"64n176win eastern Siberia"/
        data cell(1008) /"64n177win eastern Siberia"/
        data cell(1009) /"64n178win eastern Siberia"/
        data cell(1010) /"64n179win eastern Siberia"/
        data cell(1011) /"64n179ein eastern Siberia"/
        data cell(1012) /"64n178ein eastern Siberia"/
        data cell(1013) /"64n177ein eastern Siberia"/
        data cell(1014) /"64n176ein eastern Siberia"/
        data cell(1015) /"64n175ein eastern Siberia"/
        data cell(1016) /"64n174ein eastern Siberia"/
        data cell(1017) /"64n173ein eastern Siberia"/
        data cell(1018) /"64n172ein eastern Siberia"/
        data cell(1019) /"64n171ein eastern Siberia"/
        data cell(1020) /"64n170ein eastern Siberia"/
        data cell(1021) /"65n130wit Northwest Territory"/
        data cell(1022) /"65n131wit Northwest Territory"/
        data cell(1023) /"65n132wit northern Yukon Territory"/
        data cell(1024) /"65n133wit northern Yukon Territory"/
        data cell(1025) /"65n134wit northern Yukon Territory"/
        data cell(1026) /"65n135wit northern Yukon Territory"/
        data cell(1027) /"65n136wit northern Yukon Territory"/
        data cell(1028) /"65n137wit northern Yukon Territory"/
        data cell(1029) /"65n138wit northern Yukon Territory"/
        data cell(1030) /"65n139wit northern Yukon Territory"/
        data cell(1031) /"65n140wit northern Yukon Territory"/
        data cell(1032) /"65n141wit north-central roa"/
        data cell(1033) /"65n142wit north-central roa"/
        data cell(1034) /"65n143wit north-central roa"/
        data cell(1035) /"65n144wit north-central roa"/
        data cell(1036) /"65n145wit north-central roa"/
        data cell(1037) /"65n146wit north-central roa"/
        data cell(1038) /"65n147wit north-central roa"/
        data cell(1039) /"65n148wit north-central roa"/
        data cell(1040) /"65n149wit north-central roa"/
        data cell(1041) /"65n150wit north-central roa"/
        data cell(1042) /"65n151wit north-central roa"/
        data cell(1043) /"65n152wit north-central roa"/
        data cell(1044) /"65n153wit north-central roa"/
        data cell(1045) /"65n154wit north-central roa"/
        data cell(1046) /"65n155wit north-central roa"/
        data cell(1047) /"65n156wit north-central roa"/
        data cell(1048) /"65n157wit north-central roa"/
        data cell(1049) /"65n158wit north-central roa"/
        data cell(1050) /"65n159wit Seward Peninsula roa"/
        data cell(1051) /"65n160wit Seward Peninsula roa"/
        data cell(1052) /"65n161wit Seward Peninsula roa"/
        data cell(1053) /"65n162wit Seward Peninsula roa"/
        data cell(1054) /"65n163wit Seward Peninsula roa"/
        data cell(1055) /"65n164wit Seward Peninsula roa"/
        data cell(1056) /"65n165wit Seward Peninsula roa"/
        data cell(1057) /"65n166wit Seward Peninsula roa"/
        data cell(1058) /"65n167wit Seward Peninsula roa"/
        data cell(1059) /"65n168wit Bering Strait region"/
        data cell(1060) /"65n169win eastern Siberia"/
        data cell(1061) /"65n170win eastern Siberia"/
        data cell(1062) /"65n171win eastern Siberia"/
        data cell(1063) /"65n172win eastern Siberia"/
        data cell(1064) /"65n173win eastern Siberia"/
        data cell(1065) /"65n174win eastern Siberia"/
        data cell(1066) /"65n175win eastern Siberia"/
        data cell(1067) /"65n176win eastern Siberia"/
        data cell(1068) /"65n177win eastern Siberia"/
        data cell(1069) /"65n178win eastern Siberia"/
        data cell(1070) /"65n179win eastern Siberia"/
        data cell(1071) /"65n179ein eastern Siberia"/
        data cell(1072) /"65n178ein eastern Siberia"/
        data cell(1073) /"65n177ein eastern Siberia"/
        data cell(1074) /"65n176ein eastern Siberia"/
        data cell(1075) /"65n175ein eastern Siberia"/
        data cell(1076) /"65n174ein eastern Siberia"/
        data cell(1077) /"65n173ein eastern Siberia"/
        data cell(1078) /"65n172ein eastern Siberia"/
        data cell(1079) /"65n171ein eastern Siberia"/
        data cell(1080) /"65n170ein eastern Siberia"/
        data cell(1081) /"66n130wit Northwest Territory"/
        data cell(1082) /"66n131wit Northwest Territory"/
        data cell(1083) /"66n132wit Northwest Territory"/
        data cell(1084) /"66n133wit Northwest Territory"/
        data cell(1085) /"66n134wit northern Yukon Territory"/
        data cell(1086) /"66n135wit northern Yukon Territory"/
        data cell(1087) /"66n136wit northern Yukon Territory"/
        data cell(1088) /"66n137wit northern Yukon Territory"/
        data cell(1089) /"66n138wit northern Yukon Territory"/
        data cell(1090) /"66n139wit northern Yukon Territory"/
        data cell(1091) /"66n140wit northern Yukon Territory"/
        data cell(1092) /"66n141wit north-central roa"/
        data cell(1093) /"66n142wit north-central roa"/
        data cell(1094) /"66n143wit north-central roa"/
        data cell(1095) /"66n144wit north-central roa"/
        data cell(1096) /"66n145wit north-central roa"/
        data cell(1097) /"66n146wit north-central roa"/
        data cell(1098) /"66n147wit north-central roa"/
        data cell(1099) /"66n148wit north-central roa"/
        data cell(1100) /"66n149wit north-central roa"/
        data cell(1101) /"66n150wit north-central roa"/
        data cell(1102) /"66n151wit north-central roa"/
        data cell(1103) /"66n152wit north-central roa"/
        data cell(1104) /"66n153wit north-central roa"/
        data cell(1105) /"66n154wit north-central roa"/
        data cell(1106) /"66n155wit north-central roa"/
        data cell(1107) /"66n156wit north-central roa"/
        data cell(1108) /"66n157wit north-central roa"/
        data cell(1109) /"66n158wit north-central roa"/
        data cell(1110) /"66n159wit Seward Peninsula roa"/
        data cell(1111) /"66n160wit Seward Peninsula roa"/
        data cell(1112) /"66n161wit Seward Peninsula roa"/
        data cell(1113) /"66n162wit Seward Peninsula roa"/
        data cell(1114) /"66n163wit Seward Peninsula roa"/
        data cell(1115) /"66n164wit Seward Peninsula roa"/
        data cell(1116) /"66n165wit Seward Peninsula roa"/
        data cell(1117) /"66n166wit Seward Peninsula roa"/
        data cell(1118) /"66n167wit Bering Strait region"/
        data cell(1119) /"66n168wit Bering Strait region"/
        data cell(1120) /"66n169win eastern Siberia"/
        data cell(1121) /"66n170win eastern Siberia"/
        data cell(1122) /"66n171win eastern Siberia"/
        data cell(1123) /"66n172win eastern Siberia"/
        data cell(1124) /"66n173win eastern Siberia"/
        data cell(1125) /"66n174win eastern Siberia"/
        data cell(1126) /"66n175win eastern Siberia"/
        data cell(1127) /"66n176win eastern Siberia"/
        data cell(1128) /"66n177win eastern Siberia"/
        data cell(1129) /"66n178win eastern Siberia"/
        data cell(1130) /"66n179win eastern Siberia"/
c* corrected the following 10 records from:
c*	beneath the northern coast of eastern Siberia
c* to:
c*	in eastern Siberia
c* jcl 5/31/96
        data cell(1131) 
     *    /"66n179win eastern Siberia"/
        data cell(1132) 
     *    /"66n179win eastern Siberia"/
        data cell(1133) 
     *    /"66n179win eastern Siberia"/
        data cell(1134) 
     *    /"66n179win eastern Siberia"/
        data cell(1135) 
     *    /"66n179win eastern Siberia"/
        data cell(1136) 
     *    /"66n179win eastern Siberia"/
        data cell(1137) 
     *    /"66n179win eastern Siberia"/
        data cell(1138) 
     *    /"66n179win eastern Siberia"/
        data cell(1139) 
     *    /"66n179win eastern Siberia"/
        data cell(1140) 
     *    /"66n179win eastern Siberia"/
        data cell(1141) /"67n130wit Northwest Territory"/
        data cell(1142) /"67n131wit Northwest Territory"/
        data cell(1143) /"67n132wit Northwest Territory"/
        data cell(1144) /"67n133wit Northwest Territory"/
        data cell(1145) /"67n134wit Northwest Territory"/
        data cell(1146) /"67n135wit Northwest Territory"/
        data cell(1147) /"67n136wit northern Yukon Territory"/
        data cell(1148) /"67n137wit northern Yukon Territory"/
        data cell(1149) /"67n138wit northern Yukon Territory"/
        data cell(1150) /"67n139wit northern Yukon Territory"/
        data cell(1151) /"67n140wit northern Yukon Territory"/
        data cell(1152) /"67n141wit Brooks Range roa"/
        data cell(1153) /"67n142wit Brooks Range roa"/
        data cell(1154) /"67n143wit Brooks Range roa"/
        data cell(1155) /"67n144wit Brooks Range roa"/
        data cell(1156) /"67n145wit Brooks Range roa"/
        data cell(1157) /"67n146wit Brooks Range roa"/
        data cell(1158) /"67n147wit Brooks Range roa"/
        data cell(1159) /"67n148wit Brooks Range roa"/
        data cell(1160) /"67n149wit Brooks Range roa"/
        data cell(1161) /"67n150wit Brooks Range roa"/
        data cell(1162) /"67n151wit Brooks Range roa"/
        data cell(1163) /"67n152wit Brooks Range roa"/
        data cell(1164) /"67n153wit Brooks Range roa"/
        data cell(1165) /"67n154wit Brooks Range roa"/
        data cell(1166) /"67n155wit Brooks Range roa"/
        data cell(1167) /"67n156wit Brooks Range roa"/
        data cell(1168) /"67n157wit Brooks Range roa"/
        data cell(1169) /"67n158wit Brooks Range roa"/
        data cell(1170) /"67n159wit Brooks Range roa"/
        data cell(1171) /"67n160wit Brooks Range roa"/
        data cell(1172) /"67n161wit Brooks Range roa"/
        data cell(1173) /"67n162wit Brooks Range roa"/
        data cell(1174) /"67n163wit Brooks Range roa"/
        data cell(1175) /"67n164wit Brooks Range roa"/
        data cell(1176) /"67n165wit Brooks Range roa"/
        data cell(1177) /"67n166wit Bering Strait region"/
        data cell(1178) /"67n167wit Bering Strait region"/
        data cell(1179) /"67n168wit Bering Strait region"/
        data cell(1180) /"67n169wit Bering Strait region"/
        data cell(1181) /"67n170wit Chukchi Sea region"/
        data cell(1182) /"67n171wit Chukchi Sea region"/
        data cell(1183) 
     *    /"67n172wbeneath the northern coast of eastern Siberia"/
        data cell(1184) 
     *    /"67n173wbeneath the northern coast of eastern Siberia"/
        data cell(1185) 
     *    /"67n174wbeneath the northern coast of eastern Siberia"/
        data cell(1186) 
     *    /"67n175wbeneath the northern coast of eastern Siberia"/
        data cell(1187) 
     *    /"67n176wbeneath the northern coast of eastern Siberia"/
        data cell(1188) 
     *    /"67n177wbeneath the northern coast of eastern Siberia"/
        data cell(1189) 
     *    /"67n178wbeneath the northern coast of eastern Siberia"/
        data cell(1190) 
     *    /"67n179wbeneath the northern coast of eastern Siberia"/
        data cell(1191) 
     *    /"67n179ebeneath the northern coast of eastern Siberia"/
        data cell(1192) 
     *    /"67n178ebeneath the northern coast of eastern Siberia"/
        data cell(1193) /"67n177ein eastern Siberia"/
        data cell(1194) /"67n176ein eastern Siberia"/
        data cell(1195) /"67n175ein eastern Siberia"/
        data cell(1196) /"67n174ein eastern Siberia"/
        data cell(1197) /"67n173ein eastern Siberia"/
        data cell(1198) /"67n172ein eastern Siberia"/
        data cell(1199) /"67n171ein eastern Siberia"/
        data cell(1200) /"67n170ein eastern Siberia"/
        data cell(1201) /"68n130wit Northwest Territory"/
        data cell(1202) /"68n131wit Northwest Territory"/
        data cell(1203) /"68n132wit Northwest Territory"/
        data cell(1204) /"68n133wit Northwest Territory"/
        data cell(1205) /"68n134wit Northwest Territory"/
        data cell(1206) /"68n135wit Northwest Territory"/
        data cell(1207) /"68n136wit northern Yukon Territory"/
        data cell(1208) /"68n137wit northern Yukon Territory"/
        data cell(1209) /"68n138wit northern Yukon Territory"/
        data cell(1210) /"68n139wit northern Yukon Territory"/
        data cell(1211) /"68n140wit northern Yukon Territory"/
        data cell(1212) /"68n141wit Brooks Range roa"/
        data cell(1213) /"68n142wit Brooks Range roa"/
        data cell(1214) /"68n143wit Brooks Range roa"/
        data cell(1215) /"68n144wit Brooks Range roa"/
        data cell(1216) /"68n145wit Brooks Range roa"/
        data cell(1217) /"68n146wit Brooks Range roa"/
        data cell(1218) /"68n147wit Brooks Range roa"/
        data cell(1219) /"68n148wit Brooks Range roa"/
        data cell(1220) /"68n149wit Brooks Range roa"/
        data cell(1221) /"68n150wit Brooks Range roa"/
        data cell(1222) /"68n151wit Brooks Range roa"/
        data cell(1223) /"68n152wit Brooks Range roa"/
        data cell(1224) /"68n153wit Brooks Range roa"/
        data cell(1225) /"68n154wit Brooks Range roa"/
        data cell(1226) /"68n155wit Brooks Range roa"/
        data cell(1227) /"68n156wit Brooks Range roa"/
        data cell(1228) /"68n157wit Brooks Range roa"/
        data cell(1229) /"68n158wit Brooks Range roa"/
        data cell(1230) /"68n159wit Brooks Range roa"/
        data cell(1231) /"68n160wit Brooks Range roa"/
        data cell(1232) /"68n161wit Brooks Range roa"/
        data cell(1233) /"68n162wit Brooks Range roa"/
        data cell(1234) /"68n163wit Brooks Range roa"/
        data cell(1235) /"68n164wit Brooks Range roa"/
        data cell(1236) /"68n165wit Brooks Range roa"/
        data cell(1237) /"68n166wit Brooks Range roa"/
        data cell(1238) /"68n167wit Chukchi Sea region"/
        data cell(1239) /"68n168wit Chukchi Sea region"/
        data cell(1240) /"68n169wit Chukchi Sea region"/
        data cell(1241) /"68n170wit Chukchi Sea region"/
        data cell(1242) /"68n171wit Chukchi Sea region"/
        data cell(1243) /"68n172wit Chukchi Sea region"/
        data cell(1244) /"68n173wit Chukchi Sea region"/
        data cell(1245) /"68n174wit Chukchi Sea region"/
        data cell(1246) /"68n175wit Chukchi Sea region"/
        data cell(1247) 
     *    /"68n176wbeneath the northern coast of eastern Siberia"/
        data cell(1248) 
     *    /"68n177wbeneath the northern coast of eastern Siberia"/
        data cell(1249) 
     *    /"68n178wbeneath the northern coast of eastern Siberia"/
        data cell(1250) 
     *    /"68n179wbeneath the northern coast of eastern Siberia"/
        data cell(1251) 
     *    /"68n179ebeneath the northern coast of eastern Siberia"/
        data cell(1252) 
     *    /"68n178ebeneath the northern coast of eastern Siberia"/
        data cell(1253) 
     *    /"68n177ebeneath the northern coast of eastern Siberia"/
        data cell(1254) 
     *    /"68n176ebeneath the northern coast of eastern Siberia"/
        data cell(1255) 
     *    /"68n175ebeneath the northern coast of eastern Siberia"/
        data cell(1256) 
     *    /"68n174ebeneath the northern coast of eastern Siberia"/
        data cell(1257) 
     *    /"68n173ebeneath the northern coast of eastern Siberia"/
        data cell(1258) 
     *    /"68n172ebeneath the northern coast of eastern Siberia"/
        data cell(1259) 
     *    /"68n171ebeneath the northern coast of eastern Siberia"/
        data cell(1260) 
     *    /"68n170ebeneath the northern coast of eastern Siberia"/
        data cell(1261) /"69n130wit Northwest Territory"/
        data cell(1262) /"69n131wit Northwest Territory"/
        data cell(1263) /"69n132wit Northwest Territory"/
        data cell(1264) /"69n133wit Northwest Territory"/
        data cell(1265) /"69n134wit Northwest Territory"/
        data cell(1266) /"69n135wit Northwest Territory"/
        data cell(1267) /"69n136wit Northwest Territory"/
        data cell(1268) /"69n137wit northern Yukon Territory"/
        data cell(1269) /"69n138wit northern Yukon Territory"/
        data cell(1270) /"69n139wit northern Yukon Territory"/
        data cell(1271) /"69n140wit northern Yukon Territory"/
        data cell(1272) /"69n141wit northeast Brooks Range roa"/
        data cell(1273) /"69n142wit northeast Brooks Range roa"/
        data cell(1274) /"69n143wit northeast Brooks Range roa"/
        data cell(1275) /"69n144wit northeast Brooks Range roa"/
        data cell(1276) /"69n145wit northeast Brooks Range roa"/
        data cell(1277) /"69n146wit North Slope roa"/
        data cell(1278) /"69n147wit North Slope roa"/
        data cell(1279) /"69n148wit North Slope roa"/
        data cell(1280) /"69n149wit North Slope roa"/
        data cell(1281) /"69n150wit North Slope roa"/
        data cell(1282) /"69n151wit North Slope roa"/
        data cell(1283) /"69n152wit North Slope roa"/
        data cell(1284) /"69n153wit North Slope roa"/
        data cell(1285) /"69n154wit North Slope roa"/
        data cell(1286) /"69n155wit North Slope roa"/
        data cell(1287) /"69n156wit North Slope roa"/
        data cell(1288) /"69n157wit North Slope roa"/
        data cell(1289) /"69n158wit North Slope roa"/
        data cell(1290) /"69n159wit North Slope roa"/
        data cell(1291) /"69n160wit North Slope roa"/
        data cell(1292) /"69n161wit North Slope roa"/
        data cell(1293) /"69n162wit North Slope roa"/
        data cell(1294) /"69n163wit North Slope roa"/
        data cell(1295) /"69n164wit Chukchi Sea region"/
        data cell(1296) /"69n165wit Chukchi Sea region"/
        data cell(1297) /"69n166wit Chukchi Sea region"/
        data cell(1298) /"69n167wit Chukchi Sea region"/
        data cell(1299) /"69n168wit Chukchi Sea region"/
        data cell(1300) /"69n169wit Chukchi Sea region"/
        data cell(1301) /"69n170wit Chukchi Sea region"/
        data cell(1302) /"69n171wit Chukchi Sea region"/
        data cell(1303) /"69n172wit Chukchi Sea region"/
        data cell(1304) /"69n173wit Chukchi Sea region"/
        data cell(1305) /"69n174wit Chukchi Sea region"/
        data cell(1306) /"69n175wit Chukchi Sea region"/
        data cell(1307) /"69n176wit Chukchi Sea region"/
        data cell(1308) /"69n177wit Chukchi Sea region"/
        data cell(1309) 
     *    /"69n178wbeneath the northern coast of eastern Siberia"/
        data cell(1310) 
     *    /"69n179wbeneath the northern coast of eastern Siberia"/
        data cell(1311) 
     *    /"69n179ebeneath the northern coast of eastern Siberia"/
        data cell(1312) 
     *    /"69n178ebeneath the northern coast of eastern Siberia"/
        data cell(1313) 
     *    /"69n177ebeneath the northern coast of eastern Siberia"/
        data cell(1314) 
     *    /"69n176ebeneath the northern coast of eastern Siberia"/
        data cell(1315) 
     *    /"69n175ebeneath the northern coast of eastern Siberia"/
        data cell(1316) 
     *    /"69n174ebeneath the northern coast of eastern Siberia"/
        data cell(1317) 
     *    /"69n173ebeneath the northern coast of eastern Siberia"/
        data cell(1318) 
     *    /"69n172ebeneath the northern coast of eastern Siberia"/
        data cell(1319) 
     *    /"69n171ebeneath the northern coast of eastern Siberia"/
        data cell(1320) 
     *    /"69n170ebeneath the northern coast of eastern Siberia"/
        data cell(1321) /"70n130wit Beaufort Sea region"/
        data cell(1322) /"70n131wit Beaufort Sea region"/
        data cell(1323) /"70n132wit Beaufort Sea region"/
        data cell(1324) /"70n133wit Beaufort Sea region"/
        data cell(1325) /"70n134wit Beaufort Sea region"/
        data cell(1326) /"70n135wit Beaufort Sea region"/
        data cell(1327) /"70n136wit Beaufort Sea region"/
        data cell(1328) /"70n137wit Beaufort Sea region"/
        data cell(1329) /"70n138wit Beaufort Sea region"/
        data cell(1330) /"70n139wit Beaufort Sea region"/
        data cell(1331) /"70n140wit Beaufort Sea region"/
        data cell(1332) /"70n141wit northeast Brooks Range roa"/
        data cell(1333) /"70n142wit northeast Brooks Range roa"/
        data cell(1334) /"70n143wit northeast Brooks Range roa"/
        data cell(1335) /"70n144wit northeast Brooks Range roa"/
        data cell(1336) /"70n145wit northeast Brooks Range roa"/
        data cell(1337) /"70n146wit North Slope roa"/
        data cell(1338) /"70n147wit North Slope roa"/
        data cell(1339) /"70n148wit North Slope roa"/
        data cell(1340) /"70n149wit North Slope roa"/
        data cell(1341) /"70n150wit North Slope roa"/
        data cell(1342) /"70n151wit North Slope roa"/
        data cell(1343) /"70n152wit North Slope roa"/
        data cell(1344) /"70n153wit North Slope roa"/
        data cell(1345) /"70n154wit North Slope roa"/
        data cell(1346) /"70n155wit North Slope roa"/
        data cell(1347) /"70n156wit North Slope roa"/
        data cell(1348) /"70n157wit North Slope roa"/
        data cell(1349) /"70n158wit North Slope roa"/
        data cell(1350) /"70n159wit North Slope roa"/
        data cell(1351) /"70n160wit North Slope roa"/
        data cell(1352) /"70n161wit North Slope roa"/
        data cell(1353) /"70n162wit North Slope roa"/
        data cell(1354) /"70n163wit Chukchi Sea region"/
        data cell(1355) /"70n164wit Chukchi Sea region"/
        data cell(1356) /"70n165wit Chukchi Sea region"/
        data cell(1357) /"70n166wit Chukchi Sea region"/
        data cell(1358) /"70n167wit Chukchi Sea region"/
        data cell(1359) /"70n168wit Chukchi Sea region"/
        data cell(1360) /"70n169wit Chukchi Sea region"/
        data cell(1361) /"70n170wit Chukchi Sea region"/
        data cell(1362) /"70n171wit Chukchi Sea region"/
        data cell(1363) /"70n172wit Chukchi Sea region"/
        data cell(1364) /"70n173wit Chukchi Sea region"/
        data cell(1365) /"70n174wit Chukchi Sea region"/
        data cell(1366) /"70n175wit Chukchi Sea region"/
        data cell(1367) /"70n176wit Chukchi Sea region"/
        data cell(1368) 
     *    /"70n177wbeneath the northern coast of eastern Siberia"/
        data cell(1369) 
     *    /"70n178wbeneath the northern coast of eastern Siberia"/
        data cell(1370) 
     *    /"70n179wbeneath the northern coast of eastern Siberia"/
        data cell(1371) 
     *    /"70n179ebeneath the northern coast of eastern Siberia"/
        data cell(1372) 
     *    /"70n178ebeneath the northern coast of eastern Siberia"/
        data cell(1373) /"70n177eit eastern Siberian Sea region"/
        data cell(1374) /"70n176eit eastern Siberian Sea region"/
        data cell(1375) /"70n175eit eastern Siberian Sea region"/
        data cell(1376) /"70n174eit eastern Siberian Sea region"/
        data cell(1377) /"70n173eit eastern Siberian Sea region"/
        data cell(1378) /"70n172eit eastern Siberian Sea region"/
        data cell(1379) 
     *    /"70n171ebeneath the northern coast of eastern Siberia"/
        data cell(1380) 
     *    /"70n170ebeneath the northern coast of eastern Siberia"/
        data cell(1381) /"71n130wit Beaufort Sea region"/
        data cell(1382) /"71n131wit Beaufort Sea region"/
        data cell(1383) /"71n132wit Beaufort Sea region"/
        data cell(1384) /"71n133wit Beaufort Sea region"/
        data cell(1385) /"71n134wit Beaufort Sea region"/
        data cell(1386) /"71n135wit Beaufort Sea region"/
        data cell(1387) /"71n136wit Beaufort Sea region"/
        data cell(1388) /"71n137wit Beaufort Sea region"/
        data cell(1389) /"71n138wit Beaufort Sea region"/
        data cell(1390) /"71n139wit Beaufort Sea region"/
        data cell(1391) /"71n140wit Beaufort Sea region"/
        data cell(1392) /"71n141wit Beaufort Sea region"/
        data cell(1393) /"71n142wit Beaufort Sea region"/
        data cell(1394) /"71n143wit Beaufort Sea region"/
        data cell(1395) /"71n144wit Beaufort Sea region"/
        data cell(1396) /"71n145wit Beaufort Sea region"/
        data cell(1397) /"71n146wit Beaufort Sea region"/
        data cell(1398) /"71n147wit Beaufort Sea region"/
        data cell(1399) /"71n148wit Beaufort Sea region"/
        data cell(1400) /"71n149wit Beaufort Sea region"/
        data cell(1401) /"71n150wit Beaufort Sea region"/
        data cell(1402) /"71n151wit Beaufort Sea region"/
        data cell(1403) /"71n152wit Beaufort Sea region"/
        data cell(1404) /"71n153wit Beaufort Sea region"/
        data cell(1405) /"71n154wit North Slope roa"/
        data cell(1406) /"71n155wit North Slope roa"/
        data cell(1407) /"71n156wit North Slope roa"/
        data cell(1408) /"71n157wit North Slope roa"/
        data cell(1409) /"71n158wit Beaufort Sea region"/
        data cell(1410) /"71n159wit Beaufort Sea region"/
        data cell(1411) /"71n160wit Beaufort Sea region"/
        data cell(1412) /"71n161wit Beaufort Sea region"/
        data cell(1413) /"71n162wit Beaufort Sea region"/
        data cell(1414) /"71n163wit Beaufort Sea region"/
        data cell(1415) /"71n164wit Beaufort Sea region"/
        data cell(1416) /"71n165wit Beaufort Sea region"/
        data cell(1417) /"71n166wit Chukchi Sea region"/
        data cell(1418) /"71n167wit Chukchi Sea region"/
        data cell(1419) /"71n168wit Chukchi Sea region"/
        data cell(1420) /"71n169wit Chukchi Sea region"/
        data cell(1421) /"71n170wit Chukchi Sea region"/
        data cell(1422) /"71n171wit Chukchi Sea region"/
        data cell(1423) /"71n172wit Chukchi Sea region"/
        data cell(1424) /"71n173wit Chukchi Sea region"/
        data cell(1425) /"71n174wit Chukchi Sea region"/
        data cell(1426) /"71n175wit Chukchi Sea region"/
        data cell(1427) /"71n176wit Chukchi Sea region"/
        data cell(1428) 
     *    /"71n177wbeneath the northern coast of eastern Siberia"/
        data cell(1429) 
     *    /"71n178wbeneath the northern coast of eastern Siberia"/
        data cell(1430) 
     *    /"71n179wbeneath the northern coast of eastern Siberia"/
        data cell(1431) 
     *    /"71n179ebeneath the northern coast of eastern Siberia"/
        data cell(1432) 
     *    /"71n178ebeneath the northern coast of eastern Siberia"/
        data cell(1433) /"71n177eit eastern Siberian Sea region"/
        data cell(1434) /"71n176eit eastern Siberian Sea region"/
        data cell(1435) /"71n175eit eastern Siberian Sea region"/
        data cell(1436) /"71n174eit eastern Siberian Sea region"/
        data cell(1437) /"71n173eit eastern Siberian Sea region"/
        data cell(1438) /"71n172eit eastern Siberian Sea region"/
        data cell(1439) /"71n171eit eastern Siberian Sea region"/
        data cell(1440) /"71n170eit eastern Siberian Sea region"/
        data cell(1441) /"72n130wit Beaufort Sea region"/
        data cell(1442) /"72n131wit Beaufort Sea region"/
        data cell(1443) /"72n132wit Beaufort Sea region"/
        data cell(1444) /"72n133wit Beaufort Sea region"/
        data cell(1445) /"72n134wit Beaufort Sea region"/
        data cell(1446) /"72n135wit Beaufort Sea region"/
        data cell(1447) /"72n136wit Beaufort Sea region"/
        data cell(1448) /"72n137wit Beaufort Sea region"/
        data cell(1449) /"72n138wit Beaufort Sea region"/
        data cell(1450) /"72n139wit Beaufort Sea region"/
        data cell(1451) /"72n140wit Beaufort Sea region"/
        data cell(1452) /"72n141wit Beaufort Sea region"/
        data cell(1453) /"72n142wit Beaufort Sea region"/
        data cell(1454) /"72n143wit Beaufort Sea region"/
        data cell(1455) /"72n144wit Beaufort Sea region"/
        data cell(1456) /"72n145wit Beaufort Sea region"/
        data cell(1457) /"72n146wit Beaufort Sea region"/
        data cell(1458) /"72n147wit Beaufort Sea region"/
        data cell(1459) /"72n148wit Beaufort Sea region"/
        data cell(1460) /"72n149wit Beaufort Sea region"/
        data cell(1461) /"72n150wit Beaufort Sea region"/
        data cell(1462) /"72n151wit Beaufort Sea region"/
        data cell(1463) /"72n152wit Beaufort Sea region"/
        data cell(1464) /"72n153wit Beaufort Sea region"/
        data cell(1465) /"72n154wit Beaufort Sea region"/
        data cell(1466) /"72n155wit Beaufort Sea region"/
        data cell(1467) /"72n156wit Beaufort Sea region"/
        data cell(1468) /"72n157wit Beaufort Sea region"/
        data cell(1469) /"72n158wit Beaufort Sea region"/
        data cell(1470) /"72n159wit Beaufort Sea region"/
        data cell(1471) /"72n160wit Beaufort Sea region"/
        data cell(1472) /"72n161wit Beaufort Sea region"/
        data cell(1473) /"72n162wit Beaufort Sea region"/
        data cell(1474) /"72n163wit Beaufort Sea region"/
        data cell(1475) /"72n164wit Beaufort Sea region"/
        data cell(1476) /"72n165wit Beaufort Sea region"/
        data cell(1477) /"72n166wit eastern Siberian Sea region"/
        data cell(1478) /"72n167wit eastern Siberian Sea region"/
        data cell(1479) /"72n168wit eastern Siberian Sea region"/
        data cell(1480) /"72n169wit eastern Siberian Sea region"/
        data cell(1481) /"72n170wit eastern Siberian Sea region"/
        data cell(1482) /"72n171wit eastern Siberian Sea region"/
        data cell(1483) /"72n172wit eastern Siberian Sea region"/
        data cell(1484) /"72n173wit eastern Siberian Sea region"/
        data cell(1485) /"72n174wit eastern Siberian Sea region"/
        data cell(1486) /"72n175wit eastern Siberian Sea region"/
        data cell(1487) /"72n176wit eastern Siberian Sea region"/
        data cell(1488) /"72n177wit eastern Siberian Sea region"/
        data cell(1489) /"72n178wit eastern Siberian Sea region"/
        data cell(1490) /"72n179wit eastern Siberian Sea region"/
        data cell(1491) /"72n179eit eastern Siberian Sea region"/
        data cell(1492) /"72n178eit eastern Siberian Sea region"/
        data cell(1493) /"72n177eit eastern Siberian Sea region"/
        data cell(1494) /"72n176eit eastern Siberian Sea region"/
        data cell(1495) /"72n175eit eastern Siberian Sea region"/
        data cell(1496) /"72n174eit eastern Siberian Sea region"/
        data cell(1497) /"72n173eit eastern Siberian Sea region"/
        data cell(1498) /"72n172eit eastern Siberian Sea region"/
        data cell(1499) /"72n171eit eastern Siberian Sea region"/
        data cell(1500) /"72n170eit eastern Siberian Sea region"/
        data cell(1501) /"73n130wit Beaufort Sea region"/
        data cell(1502) /"73n131wit Beaufort Sea region"/
        data cell(1503) /"73n132wit Beaufort Sea region"/
        data cell(1504) /"73n133wit Beaufort Sea region"/
        data cell(1505) /"73n134wit Beaufort Sea region"/
        data cell(1506) /"73n135wit Beaufort Sea region"/
        data cell(1507) /"73n136wit Beaufort Sea region"/
        data cell(1508) /"73n137wit Beaufort Sea region"/
        data cell(1509) /"73n138wit Beaufort Sea region"/
        data cell(1510) /"73n139wit Beaufort Sea region"/
        data cell(1511) /"73n140wit Beaufort Sea region"/
        data cell(1512) /"73n141wit Beaufort Sea region"/
        data cell(1513) /"73n142wit Beaufort Sea region"/
        data cell(1514) /"73n143wit Beaufort Sea region"/
        data cell(1515) /"73n144wit Beaufort Sea region"/
        data cell(1516) /"73n145wit Beaufort Sea region"/
        data cell(1517) /"73n146wit Beaufort Sea region"/
        data cell(1518) /"73n147wit Beaufort Sea region"/
        data cell(1519) /"73n148wit Beaufort Sea region"/
        data cell(1520) /"73n149wit Beaufort Sea region"/
        data cell(1521) /"73n150wit Beaufort Sea region"/
        data cell(1522) /"73n151wit Beaufort Sea region"/
        data cell(1523) /"73n152wit Beaufort Sea region"/
        data cell(1524) /"73n153wit Beaufort Sea region"/
        data cell(1525) /"73n154wit Beaufort Sea region"/
        data cell(1526) /"73n155wit Beaufort Sea region"/
        data cell(1527) /"73n156wit Beaufort Sea region"/
        data cell(1528) /"73n157wit Beaufort Sea region"/
        data cell(1529) /"73n158wit Beaufort Sea region"/
        data cell(1530) /"73n159wit Beaufort Sea region"/
        data cell(1531) /"73n160wit Beaufort Sea region"/
        data cell(1532) /"73n161wit Beaufort Sea region"/
        data cell(1533) /"73n162wit Beaufort Sea region"/
        data cell(1534) /"73n163wit Beaufort Sea region"/
        data cell(1535) /"73n164wit Beaufort Sea region"/
        data cell(1536) /"73n165wit Beaufort Sea region"/
        data cell(1537) /"73n166wit eastern Siberian Sea region"/
        data cell(1538) /"73n167wit eastern Siberian Sea region"/
        data cell(1539) /"73n168wit eastern Siberian Sea region"/
        data cell(1540) /"73n169wit eastern Siberian Sea region"/
        data cell(1541) /"73n170wit eastern Siberian Sea region"/
        data cell(1542) /"73n171wit eastern Siberian Sea region"/
        data cell(1543) /"73n172wit eastern Siberian Sea region"/
        data cell(1544) /"73n173wit eastern Siberian Sea region"/
        data cell(1545) /"73n174wit eastern Siberian Sea region"/
        data cell(1546) /"73n175wit eastern Siberian Sea region"/
        data cell(1547) /"73n176wit eastern Siberian Sea region"/
        data cell(1548) /"73n177wit eastern Siberian Sea region"/
        data cell(1549) /"73n178wit eastern Siberian Sea region"/
        data cell(1550) /"73n179wit eastern Siberian Sea region"/
        data cell(1551) /"73n179eit eastern Siberian Sea region"/
        data cell(1552) /"73n178eit eastern Siberian Sea region"/
        data cell(1553) /"73n177eit eastern Siberian Sea region"/
        data cell(1554) /"73n176eit eastern Siberian Sea region"/
        data cell(1555) /"73n175eit eastern Siberian Sea region"/
        data cell(1556) /"73n174eit eastern Siberian Sea region"/
        data cell(1557) /"73n173eit eastern Siberian Sea region"/
        data cell(1558) /"73n172eit eastern Siberian Sea region"/
        data cell(1559) /"73n171eit eastern Siberian Sea region"/
        data cell(1560) /"73n170eit eastern Siberian Sea region"/
        data cell(1561) /"74n130wit Beaufort Sea region"/
        data cell(1562) /"74n131wit Beaufort Sea region"/
        data cell(1563) /"74n132wit Beaufort Sea region"/
        data cell(1564) /"74n133wit Beaufort Sea region"/
        data cell(1565) /"74n134wit Beaufort Sea region"/
        data cell(1566) /"74n135wit Beaufort Sea region"/
        data cell(1567) /"74n136wit Beaufort Sea region"/
        data cell(1568) /"74n137wit Beaufort Sea region"/
        data cell(1569) /"74n138wit Beaufort Sea region"/
        data cell(1570) /"74n139wit Beaufort Sea region"/
        data cell(1571) /"74n140wit Beaufort Sea region"/
        data cell(1572) /"74n141wit Beaufort Sea region"/
        data cell(1573) /"74n142wit Beaufort Sea region"/
        data cell(1574) /"74n143wit Beaufort Sea region"/
        data cell(1575) /"74n144wit Beaufort Sea region"/
        data cell(1576) /"74n145wit Beaufort Sea region"/
        data cell(1577) /"74n146wit Beaufort Sea region"/
        data cell(1578) /"74n147wit Beaufort Sea region"/
        data cell(1579) /"74n148wit Beaufort Sea region"/
        data cell(1580) /"74n149wit Beaufort Sea region"/
        data cell(1581) /"74n150wit Beaufort Sea region"/
        data cell(1582) /"74n151wit Beaufort Sea region"/
        data cell(1583) /"74n152wit Beaufort Sea region"/
        data cell(1584) /"74n153wit Beaufort Sea region"/
        data cell(1585) /"74n154wit Beaufort Sea region"/
        data cell(1586) /"74n155wit Beaufort Sea region"/
        data cell(1587) /"74n156wit Beaufort Sea region"/
        data cell(1588) /"74n157wit Beaufort Sea region"/
        data cell(1589) /"74n158wit Beaufort Sea region"/
        data cell(1590) /"74n159wit Beaufort Sea region"/
        data cell(1591) /"74n160wit Beaufort Sea region"/
        data cell(1592) /"74n161wit Beaufort Sea region"/
        data cell(1593) /"74n162wit Beaufort Sea region"/
        data cell(1594) /"74n163wit Beaufort Sea region"/
        data cell(1595) /"74n164wit Beaufort Sea region"/
        data cell(1596) /"74n165wit Beaufort Sea region"/
        data cell(1597) /"74n166wit eastern Siberian Sea region"/
        data cell(1598) /"74n167wit eastern Siberian Sea region"/
        data cell(1599) /"74n168wit eastern Siberian Sea region"/
        data cell(1600) /"74n169wit eastern Siberian Sea region"/
        data cell(1601) /"74n170wit eastern Siberian Sea region"/
        data cell(1602) /"74n171wit eastern Siberian Sea region"/
        data cell(1603) /"74n172wit eastern Siberian Sea region"/
        data cell(1604) /"74n173wit eastern Siberian Sea region"/
        data cell(1605) /"74n174wit eastern Siberian Sea region"/
        data cell(1606) /"74n175wit eastern Siberian Sea region"/
        data cell(1607) /"74n176wit eastern Siberian Sea region"/
        data cell(1608) /"74n177wit eastern Siberian Sea region"/
        data cell(1609) /"74n178wit eastern Siberian Sea region"/
        data cell(1610) /"74n179wit eastern Siberian Sea region"/
        data cell(1611) /"74n179eit eastern Siberian Sea region"/
        data cell(1612) /"74n178eit eastern Siberian Sea region"/
        data cell(1613) /"74n177eit eastern Siberian Sea region"/
        data cell(1614) /"74n176eit eastern Siberian Sea region"/
        data cell(1615) /"74n175eit eastern Siberian Sea region"/
        data cell(1616) /"74n174eit eastern Siberian Sea region"/
        data cell(1617) /"74n173eit eastern Siberian Sea region"/
        data cell(1618) /"74n172eit eastern Siberian Sea region"/
        data cell(1619) /"74n171eit eastern Siberian Sea region"/
        data cell(1620) /"74n170eit eastern Siberian Sea region"/
        data cell(1621) /"75n130wit Beaufort Sea region"/
        data cell(1622) /"75n131wit Beaufort Sea region"/
        data cell(1623) /"75n132wit Beaufort Sea region"/
        data cell(1624) /"75n133wit Beaufort Sea region"/
        data cell(1625) /"75n134wit Beaufort Sea region"/
        data cell(1626) /"75n135wit Beaufort Sea region"/
        data cell(1627) /"75n136wit Beaufort Sea region"/
        data cell(1628) /"75n137wit Beaufort Sea region"/
        data cell(1629) /"75n138wit Beaufort Sea region"/
        data cell(1630) /"75n139wit Beaufort Sea region"/
        data cell(1631) /"75n140wit Beaufort Sea region"/
        data cell(1632) /"75n141wit Beaufort Sea region"/
        data cell(1633) /"75n142wit Beaufort Sea region"/
        data cell(1634) /"75n143wit Beaufort Sea region"/
        data cell(1635) /"75n144wit Beaufort Sea region"/
        data cell(1636) /"75n145wit Beaufort Sea region"/
        data cell(1637) /"75n146wit Beaufort Sea region"/
        data cell(1638) /"75n147wit Beaufort Sea region"/
        data cell(1639) /"75n148wit Beaufort Sea region"/
        data cell(1640) /"75n149wit Beaufort Sea region"/
        data cell(1641) /"75n150wit Beaufort Sea region"/
        data cell(1642) /"75n151wit Beaufort Sea region"/
        data cell(1643) /"75n152wit Beaufort Sea region"/
        data cell(1644) /"75n153wit Beaufort Sea region"/
        data cell(1645) /"75n154wit Beaufort Sea region"/
        data cell(1646) /"75n155wit Beaufort Sea region"/
        data cell(1647) /"75n156wit Beaufort Sea region"/
        data cell(1648) /"75n157wit Beaufort Sea region"/
        data cell(1649) /"75n158wit Beaufort Sea region"/
        data cell(1650) /"75n159wit Beaufort Sea region"/
        data cell(1651) /"75n160wit Beaufort Sea region"/
        data cell(1652) /"75n161wit Beaufort Sea region"/
        data cell(1653) /"75n162wit Beaufort Sea region"/
        data cell(1654) /"75n163wit Beaufort Sea region"/
        data cell(1655) /"75n164wit Beaufort Sea region"/
        data cell(1656) /"75n165wit Beaufort Sea region"/
        data cell(1657) /"75n166wit eastern Siberian Sea region"/
        data cell(1658) /"75n167wit eastern Siberian Sea region"/
        data cell(1659) /"75n168wit eastern Siberian Sea region"/
        data cell(1660) /"75n169wit eastern Siberian Sea region"/
        data cell(1661) /"75n170wit eastern Siberian Sea region"/
        data cell(1662) /"75n171wit eastern Siberian Sea region"/
        data cell(1663) /"75n172wit eastern Siberian Sea region"/
        data cell(1664) /"75n173wit eastern Siberian Sea region"/
        data cell(1665) /"75n174wit eastern Siberian Sea region"/
        data cell(1666) /"75n175wit eastern Siberian Sea region"/
        data cell(1667) /"75n176wit eastern Siberian Sea region"/
        data cell(1668) /"75n177wit eastern Siberian Sea region"/
        data cell(1669) /"75n178wit eastern Siberian Sea region"/
        data cell(1670) /"75n179wit eastern Siberian Sea region"/
        data cell(1671) /"75n179eit eastern Siberian Sea region"/
        data cell(1672) /"75n178eit eastern Siberian Sea region"/
        data cell(1673) /"75n177eit eastern Siberian Sea region"/
        data cell(1674) /"75n176eit eastern Siberian Sea region"/
        data cell(1675) /"75n175eit eastern Siberian Sea region"/
        data cell(1676) /"75n174eit eastern Siberian Sea region"/
        data cell(1677) /"75n173eit eastern Siberian Sea region"/
        data cell(1678) /"75n172eit eastern Siberian Sea region"/
        data cell(1679) /"75n171eit eastern Siberian Sea region"/
        data cell(1680) /"75n170eit eastern Siberian Sea region"/

c  -- begin routine

	if (len(regnam) .lt. maxlen) then
	  print *, 'error (aeic_region): ',
     *			'length of character variable too short'
	  stop
	endif

	regnam = 'an undefined region of Alaska'

	latd = int(colat)
	lond = int(elon)

	if (latd .lt. latmin .or. colat .gt. float(latmax)) return
	if (elon .gt. float(lonmax) .or. lond .lt. lonmin) return

	ilat = latd - latmin

	if (elon .ge. 180.) then
	  ilon = lonmax - lond + 1
	  if (elon .gt. float(lond)) ilon = ilon - 1
	else
	  ilon = lonmax - lond
	endif

	ic = ilat*60 + ilon

	regnam = cell(ic)(8:lentru(cell(ic)))

	return
	end
