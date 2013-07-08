void
latlon2xydel (xlat1, xlon1, xlat2, xlon2,
              xdel, ydel)
 
double        xlat1, xlon1, xlat2, xlon2;
double *      xdel;
double *            ydel;
 
{
      double xlt1, xln1, xlt2, xln2, del, az, pi;
 
      if (xlat1 == xlat2 && xlon1 == xlon2) {
        *xdel = 0.0;
        *ydel = 0.0;
        return;
      } else {
        xlt1 = xlat1 * M_PI / 180.0;
        xln1 = xlon1 * M_PI / 180.0;
        xlt2 = xlat2 * M_PI / 180.0;
        xln2 = xlon2 * M_PI / 180.0;
        dist (xlt1, xln1, xlt2, xln2, &del, &az);
      }
      del = del * 180.0 / M_PI;
      *xdel = del * sin (az);
      *ydel = del * cos (az);
 
      return;
}
 
void
xydel2latlon (xlat1, xlon1, xdel, ydel,
              xlat2, xlon2)
 
double        xlat1, xlon1, xdel, ydel;
double *      xlat2;
double *             xlon2;
 
{
      double xlt1, xln1, xlt2, xln2, del, az;
 
      if (xdel == 0.0 && ydel == 0.0) {
        *xlat2 = xlat1;
        *xlon2 = xlon1;
        return;
      } else {
        xlt1 = xlat1 * M_PI / 180.0;
        xln1 = xlon1 * M_PI / 180.0;
        del = sqrt(xdel*xdel + ydel*ydel);
        del = del * M_PI / 180.0;
        az = atan2 ( xdel, ydel );
        latlon (xlt1, xln1, del, az, &xlt2, &xln2);
      }
      *xlat2 = xlt2 * 180.0 / M_PI;
      *xlon2 = xln2 * 180.0 / M_PI;
 
      return;
}
 
