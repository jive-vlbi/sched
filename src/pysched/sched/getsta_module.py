from . import parameter, stread
from ..catalog import StationCatalog
from .. import key, util

import schedlib as s

last_station_file = ""

def getsta(stdin, values, index, gotvex, mjd1):
    global last_station_file

    maxsta = StationCatalog.maxcat

    if s.schcon.debug and (index < 2):
        s.wlog(1, "GETSTA: starting.")
    if index == 0:
        last_station_file = ""

    station_file = util.expand_file_name(values["stafile"])
    s.schcst.stafile = util.resize_string(station_file, 
                                          s.schcst.stafile.itemsize, "stafile")
    s.schcst.locafile = util.resize_string(util.expand_file_name(
        values["locfile"]), s.schcst.locafile.itemsize, "locfile")
    if (station_file.upper() != "NONE") and (station_file != last_station_file):
        try:
            f = open(station_file, "r")
        except Exception as e:
            s.putout(str(e))
            s.error("Station catalog: {} not opened".format(station_file))
        else:
            with f:
                input_iterator = key.KeyfileLister(f)
                stread(input_iterator, stdin, mjd1)
        
    last_station_file = station_file

    if (index == 0) and (len(values["stations"]) == 0):
        s.errlog("GETSTA: No stations specified!")
    elif len(values["stations"]) == 0:
        s.schn2a.stascn[index] = s.schn2a.stascn[index-1]
    else:
        s.schn2a.stascn[index] = False
        for station in values["stations"]:
            ksta, ista, doit = s.stano(station)
            if doit:
                if ksta == 0:
                    s.wlog(1, "GETSTA: Station {} not found in catalogs".\
                           format(station))
                    s.errlog("GETSTA: Note -- specify station catalog "
                             "before end of first scan input")

                if ista == 0:
                    s.schn1.nsta += 1
                    if s.schn1.nsta > maxsta:
                        s.errlog("'SCHIN: Too many stations!")
                    ista = s.schn1.nsta
                    s.schc1.staname[ista-1] = s.schcst.station[ksta-1]

                    s.schn1.stanum[ista-1] = ksta
                    s.schsta.ischsta[ksta-1] = ista

                    control = bytes(s.schcst.control[s.schn1.stanum[ista-1]]).\
                       decode().strip()
                    if control == "VEX ":
                        gotvex = True

                    if control == "VSOP":
                        s.schcon.dovsop = True

                    if s.schc1.staname[ista-1][:4] != b"VLBA":
                        s.schn5.allvlba = False
                    else:
                        s.schn1.gotvlba = True

                s.schn2a.stascn[index][ista-1] = True
                            
    return gotvex
