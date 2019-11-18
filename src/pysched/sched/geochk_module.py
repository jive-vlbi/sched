from . import makescn
from ..catalog import StationCatalog, ScanCatalog, SetupFileCatalog, \
    SourceCatalog
from ..util import f2str, bool2str

import schedlib as s

import numpy as np
from astropy.coordinates import SkyCoord

station_catalog = StationCatalog()
scan_catalog = ScanCatalog()
sf_catalog = SetupFileCatalog()
source_catalog = SourceCatalog()
def geochk(j_scan, scan_index, start_time, end_time):
    ok_geo = np.empty(shape=s.schsou.geosrci.shape, dtype=bool)
    use_geo = np.empty(shape=s.schsou.geosrci.shape, dtype=int)

    scans = scan_catalog.direct_access_entries
    stations = station_catalog.used(use_direct_access=True)
    sources = source_catalog.entries

    seg_elevation = np.empty(dtype=float, shape=(len(stations), s.schsou.ngeo))

    use_time = True
    last_scan_index = np.zeros(shape=(StationCatalog.maxsta,))
    approx_time = (start_time + end_time) / 2
    n_reject = 0

    if len(stations) > 20:
        s.wlog(1, "GEOCHK:  Printing only first 20 stations information")
        ms_print = 20
    else:
        ms_print = len(stations)

    s.wlog(1, "             --------------------- ")
    year, day, time_rad = s.timej(approx_time)
    c_time = f2str(s.tformwrp(time_rad, "T", 0, 2, 2, "::@"))
    s.wlog(1, "Building geodetic segment centered at {} {} {}".format(
        year, day, c_time))
    s.wlog(1, "   Using GEOPRT={:>4}     GEOTRIES={:>4}    GEOBACK={:>3}     "
           "GEOSREP={:>3}".format(s.schsou.geoprt, s.schsou.geotries, 
                                  s.schsou.geoback, s.schsou.geosrep))
    s.wlog(1, "         GEOSLEW={:>6.2f}  GEOSLOW={:>7.1f}  GELOWEL={:>6.2f}  "
           "GEOHIEL={:>6.2f}".format(s.schsou.geoslew, s.schsou.geoslow, 
                                     s.schsou.geolowel, s.schsou.geohiel))
    s.wlog(1, "         See sched.runlog for details of the build process.")

    if s.schsou.geoprt >= 0:
        s.wlog(0, "       Note in fit, SecZ < 4 treated as 4 to avoid favoring "
               "extreme low elevations.")
        s.wlog(0, "Elevations at center for sources considered are: ")
        s.wlog(0, "                  Prio " + "   ".join(
            s.stcode for s in stations[:20]))

    elevation_tolerance = 0
    
    sun_coord = SkyCoord(*s.sunpos(approx_time), unit="rad")
    t_freq = sf_catalog.entries[scans[j_scan - 1].setnum - 1].sffreq[0] / 1000
    required_separation = 60 * (t_freq ** -0.6)

    for geo_index in range(s.schsou.ngeo):
        l_scan = scan_index + 1
        n_good = makescn(last_scan_index, l_scan, j_scan, 
                         s.schsou.geosrci[geo_index],
                         f2str(s.schcsc.geosrc[geo_index]), approx_time, 
                         scans[j_scan - 1].opminel - elevation_tolerance, 
                         use_time)

        for i, station in enumerate(stations):
            seg_elevation[i, geo_index] = (station.el1[l_scan - 1] + 
                                           station.el2[l_scan - 1]) / 2

        ok_geo[geo_index] = (n_good >= scans[j_scan - 1].opmian)

        source = sources[s.schsou.geosrci[geo_index] - 1]
        source_coord = SkyCoord(source.rap, source.decp, unit="rad")
        source_separation = sun_coord.separation(source_coord).deg
        
        ok_geo[geo_index] = ok_geo[geo_index] and \
                            (source_separation > required_separation)

        if not ok_geo[geo_index]:
            n_reject += 1
            use_geo[geo_index] = 9

        if ok_geo[geo_index]:
            use_geo[geo_index] = 5
            for i, station in enumerate(stations):
                if station.stascn[l_scan - 1]:
                    if (seg_elevation[i, geo_index] <= s.schsou.geolowel + 10):
                        use_geo[geo_index] = 4
            for i, station in enumerate(stations):
                if station.stascn[l_scan - 1]:
                    if (seg_elevation[i, geo_index] <= s.schsou.geolowel):
                        use_geo[geo_index] = 3
            
            n_low = 0
            n_high = 0
            for i, station in enumerate(stations):
                if station.stascn[l_scan - 1] and \
                   (seg_elevation[i, geo_index] > 
                    (scans[j_scan - 1].opminel - elevation_tolerance)) and \
                   (seg_elevation[i, geo_index] <= s.schsou.geolowel):
                    n_low += 1
                if station.stascn[l_scan - 1] and \
                   (seg_elevation[i, geo_index] >= s.schsou.geohiel):
                    n_high += 1

            if ((n_low >= 1) and (n_high >= 3)) or (n_low >= 3):
                use_geo[geo_index] = 2
            if (n_low >= 2) and (n_high >= 2):
                use_geo[geo_index] = 1

        if s.schsou.geoprt >= 0:
            msg = "{:>5d} {:<8} {:>5d}".format(
                geo_index + 1, f2str(s.schcsc.geosrc[geo_index]), 
                use_geo[geo_index])
            msg += "".join("{:5.0f}".format(el) 
                           if el > (scans[l_scan - 1].opminel - 
                                    elevation_tolerance)
                           else "   --"
                           for el in seg_elevation[:, geo_index])
            if source_separation <= required_separation:
                msg += "   Too near sun: {:5.0f} deg.".format(source_separation)
            s.wlog(0, msg)
    
    if n_reject == s.schsou.ngeo:
        s.errlog("GEOCHK:  None of the sources specified for a geodetic "
                 "segment are up at OPMINANT antennas.")

    return ok_geo, use_geo, seg_elevation
