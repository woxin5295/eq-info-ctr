proc latlon2pix {lat lon} {
        global plot
 
        set dbfake [list 0 0 0 0]
 
        set ctr_lat $plot(proj_center_lat)
        set ctr_lon $plot(proj_center_lon)
 
        set dist [dbeval $dbfake "distance($ctr_lat,$ctr_lon,$lat,$lon)"]
        set az [dbeval $dbfake "azimuth($ctr_lat,$ctr_lon,$lat,$lon)"]
 
        set pixdist [expr $dist * $plot(pix_per_deg)]
        set az_radians [expr $az * 0.0174533]
 
        set x [expr $plot(proj_center_x) + $pixdist * [sin $az_radians]]
        set y [expr $plot(proj_center_y) - $pixdist * [cos $az_radians]]
 
        return [list [int $x] [int $y]]
}
