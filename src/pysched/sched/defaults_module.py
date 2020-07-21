from ..util import f2str
from ..catalog import StationCatalog, ScanCatalog, SetupCatalog
from . import defset

import schedlib as s

def defaults():
    if s.schcon.debug:
        s.wlog(0, "DEFAULTS starting.")

    # Protect against mixing RDBE and non-RDBE stations and frequency files.
    if (f2str(s.schsco.freqfile).find("RDBE") != -1) != \
       (f2str(s.schcst.stafile ).find("RDBE") != -1):
        s.wlog(1, " ")
        s.wlog(1, "DEFAULTS: **** Did you intend to mix RDBE and non-RDBE "
               "station and frequency files? ****")
        s.wlog(1, " ")

    # Discourage use of MARK5A on the VLBA except for pointing.
    station_catalog = StationCatalog()
    station_catalog.read()
    station_catalog.read_scheduled_attributes()
    if not s.schn1.notape and any(station.station.startswith("VLBA") and 
                                  (station.disk == "MARK5A") 
                                  for station in station_catalog.used()):
        s.wlog(1, "STREAD: ==== WARNING ==== Mark5A specified for a VLBA "
               " station.")
        s.wlog(1, "        Those recorders have been removed from most "
               "VLBA stations.")

    s.srfinish()
    s.pkfinish()
    s.vlascns()

    if not s.schcon.noset:
        defset()

        s.sdopincr()

    s.recctl()

    # Default the grab stuff.  Put after setups defaulted so we
    # have the bit rate.  The GRABGAP assumes that the required
    # bit rate is the total over all channels.
    scan_catalog = ScanCatalog()
    scan_catalog.read()
    setups = SetupCatalog().read()
    used_scans = scan_catalog.used()
    for scan_index, scan in enumerate(used_scans, scan_catalog.scan_offset):
        if (scan.grabto != "NONE") and (scan.datapath == "IN2DISK"):
            if scan.grabtime[0] < 0:
                scan.grabtime[0] = 30
            if scan.grabtime[1] < 0:
                scan.grabtime[1] = 10
            if scan.grabgap == 0:
                for station in station_catalog.used():
                    if station.stascn[scan_index]:
                        bps = setups[station.nsetup[scan_index] - 1].totbps
                        scan.grabgap = max(scan.grabgap,
                                           5 + scan.grabtime[0] * bps / 110.)
    scan_catalog.write(range(scan_catalog.scan_offset,
                             scan_catalog.scan_offset + len(used_scans)))
    
