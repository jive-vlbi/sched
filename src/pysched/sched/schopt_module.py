from . import optnone, addpeak, addgeo
from ..catalog import ScanCatalog, StationCatalog, SourceCatalog, \
    PhaseCenterCatalog, SetupCatalog, PeakCatalog
from ..util import f2str

import schedlib as s

import numpy as np

import itertools

def schopt():
    if s.schcon.debug:
        s.wlog(0, "SCHOPT: Starting.")

    scan_catalog = ScanCatalog()
    scan_catalog.read()
    scans = scan_catalog.entries
    
    station_catalog = StationCatalog()
    station_catalog.read()
    station_catalog.read_scheduled_attributes()
    stations = station_catalog.used()

    source_catalog = SourceCatalog()
    sources = source_catalog.read()

    pc_catalog = PhaseCenterCatalog()
    phase_centers = pc_catalog.read()

    setups = SetupCatalog().read()
    peak_catalog = PeakCatalog() 
    peak_groups = peak_catalog.read()

    # FIX way too crude
    catalogs = [scan_catalog, station_catalog, source_catalog, pc_catalog]
    def call_schedlib(f, *args):
        for catalog in catalogs:
            catalog.write()
        station_catalog.write_scheduled_attributes()
        ret = f(*args)
        scan_catalog.read(slice(scan_catalog.nr_elements))
        for catalog in catalogs[1:]:
            catalog.read()
        station_catalog.read_scheduled_attributes()
        return ret

    for scan in scans:
        scan.origen = 1

    optmode = f2str(s.schsco.optmode)
    if (optmode not in {"NONE", "SCANS"}) or \
       s.schcon.autopeak or s.schsou.anygeo:
        # get the int value of nscans,
        # else (increment) operations affect nscans directly,
        # since it's a dimensionless np.array
        scan_index = int(s.schn1.nscans) 
        s.schn1.scan1 = scan_index + 1
    else:
        scan_index = 0
        s.schn1.scan1 = 1
    
    s.wlog(0, "SCHOPT:  First output scan will be number {}".format(
        s.schn1.scan1))

    done = False
    peak_opt = 0
    geo_opt = 0
    k_scan = 0
    last_scan_index = np.zeros(shape=(station_catalog.maxsta,), dtype=int)
    last_s_scan_index = np.zeros(shape=(station_catalog.maxsta,), dtype=int)
    while not done:
        scan_index += 1
        if scan_index > scan_catalog.maxscan:
            s.errlog("SCHOPT:  Trying to generate too many scans. Max: {}".\
                     format(scan_catalog.maxscan))
        
        scan = scans[scan_index - 1]
        if (peak_opt != 0) or (geo_opt != 0):
            keep = True
        else:
            k_scan += 1
            assert((k_scan == scan_index) or (scan_index >= s.schn1.nscans))
            if optmode == "NONE":
                adjust, keep, done = optnone(k_scan, scan_index)
            elif optmode == "SCANS":
                adjust, keep, done = call_schedlib(
                    s.optskd, last_scan_index, k_scan, scan_index)
            elif optmode == "CELLS":
                adjust, keep, done = call_schedlib(
                    s.optcells, last_scan_index, k_scan, scan_index)
                scan.origen = 2
            elif optmode == "CSUB":
                adjust, keep, done = call_schedlib(
                    s.optcsub, last_scan_index, k_scan, scan_index)
                scan.origen = 2
            elif optmode == "UPTIME":
                last_scan_index, adjust, keep, done = call_schedlib(
                    s.optupt, last_scan_index, k_scan, scan_index)
                scan.origen = 2
            elif optmode == "HAS":
                adjust, keep, done = call_schedlib(
                    s.opthas, last_scan_index, k_scan, scan_index)
                scan.origen = 2
            elif optmode == "HIGHEL":
                k_scan, adjust, keep, done = call_schedlib(
                    s.opthiel, last_scan_index, k_scan, scan_index)
                scan.origen = 2
            else:
                s.errlog("SCHOPT: Invalid OPTMODE: {}".format(optmode))

            scan_catalog.write(range(scan_index - 1, scan_index))
            if keep and (not done):
                s.opttim(last_scan_index, last_s_scan_index, scan_index, 
                    adjust, False, False)
                scan_catalog.read(slice(scan_index - 1, scan_index))
                if (s.schcon.opdur != 0) and \
                   (scan.stopj > 
                    scans[s.schn1.scan1 - 1].startj + s.schcon.opdur) and \
                   (optmode != "HAS"):
                    done = True

                n_good = s.scngeo(last_scan_index, scan_index)
                station_catalog.read_scheduled_attributes()
            
        insert_adjust = adjust
        if keep and (not done):
            if (peak_opt == 0) and ((scan.geolen > 0) or (geo_opt >= 1)):
                insert_adjust = False
                geo_opt, keep = addgeo(last_scan_index, scan_index, geo_opt, 
                                       scans, stations)
                # geo_opt, keep = s.addgeo(last_scan_index, scan_index, geo_opt)
                scan_catalog.read(slice(len(scans)))
                station_catalog.read_scheduled_attributes()

            peak_opt, insert_adjust = addpeak(
                last_scan_index, scans, stations, setups, peak_groups,
                peak_opt, insert_adjust, scan_index)
            peak_catalog.write()
            scan_catalog.write()
            # peak_opt, insert_adjust = s.addpeak(
            #     last_scan_index, scan_index, peak_opt, insert_adjust)
            keep = s.makeptg(last_scan_index, scan_index, keep)
            scan_catalog.read(slice(scan_index - 1, scan_index))
            station_catalog.read()
            station_catalog.read_scheduled_attributes()

        if keep and (not done):
            s.opttim(last_scan_index, last_s_scan_index, scan_index, 
                insert_adjust, False, True)
            n_good = s.scngeo(last_scan_index, scan_index)
            n_good = s.autodown(last_scan_index, scan_index)

            scan_catalog.read(slice(scan_index - 1, scan_index))
            
            station_catalog.read_scheduled_attributes()
            keep = keep and ((n_good >= scan.opmian) or 
                             (optmode in {"CSUB", "HAS"}) or 
                             (scan.point >= 0))
            if not keep:
                for station in stations:
                    station.stascn[scan_index - 1] = False
                station_catalog.write_scheduled_attributes()
            else:
                s.settps(scan_index, last_scan_index)
                
                for station_index, station in enumerate(stations, 1):
                    if station.stascn[scan_index - 1] and s.schn1.vlbitp and \
                       (not s.schcon.noset) and station.usedisk:
                        s.diskpos(scan_index, station_index, last_scan_index)
                station_catalog.read_scheduled_attributes()
                             
                if (optmode not in {"NONE", "UPTIME"}) or s.schsou.anygeo:
                    s.optsch(scan_index)

                source = sources[scan.srcnum - 1]
                source.sused = True
                if not scan.norec:
                    source.usedrec = True
                if scan.idopsrc != 0:
                    sources[scan.idopsrc - 1].sused = True
                if scan.ivlaphs != 0:
                    sources[scan.ivlaphs - 1].sused = True
                if scan.icent != 0:
                    source.usedcent = True
                    for source_index in phase_centers[scan.icent - 1].ctrsrci:
                        sources[source_index - 1].usedphs = True
        
            for station_index, station in enumerate(stations):
                if station.stascn[scan_index - 1]:
                    last_scan_index[station_index] = scan_index
                    if scan.origen < 4:
                        last_s_scan_index[station_index] = scan_index

            s.schn1.scanl = scan_index

    if s.schcon.debug:
        s.wlog(0, "SCHOPT: 9.")

    if s.schn1.scanl == 0:
        s.wlog(1, "SCHOPT: Did not schedule any scans")
        s.errlog(" Abort")

    scan_catalog.write()
    station_catalog.write_scheduled_attributes()
    source_catalog.write()
    s.accsrc(False)
    source_catalog.read()
    gotall = call_schedlib(s.srcflg)

    if not gotall:
        s.errlog("SCHOPT: Not all sources found; programming problem.")

    call_schedlib(s.getpairs)

    year, day1, start = s.timej(scans[s.schn1.scan1 - 1].startj)
    year, day2, stop = s.timej(scans[s.schn1.scanl - 1].stopj)
    
    time1 = f2str(s.tformwrp(start, "T", 0, 2, 2, "::@"))
    time2 = f2str(s.tformwrp(stop, "T", 0, 2, 2, "::@"))
    s.wlog(0, "SCHOPT:  There will be {number} output scans ({first} - {last}) "
           "from {day1}/{time1} to {day2}/{time2}".format(
               number=s.schn1.scanl - s.schn1.scan1 + 1,
               first=s.schn1.scan1, last=s.schn1.scanl,
               day1=day1, day2=day2, time1=time1, time2=time2))

    call_schedlib(s.schtim)
    call_schedlib(s.sch24)

    if s.schcon.debug:
        s.wlog(0, "SCHOPT: Done.")
