from ..catalog import SetupCatalog, SetupFileCatalog, StationCatalog, \
                    FrequencyCatalog
from . import parameter, setbbc

import schedlib as s

from collections import defaultdict

def setdefs():
    station_catalog = StationCatalog()
    station_catalog.read() 
    station_entries = station_catalog.entries
    
    setup_catalog = SetupCatalog()
    setup_catalog.read()
    setup_entries_map = defaultdict(list)
    for entry in setup_catalog.scheduled():
        setup_entries_map[entry.isetnum].append(entry)
    
    setup_file_catalog = SetupFileCatalog()
    setup_file_catalog.read()
    for index, setup_file_entry in enumerate(setup_file_catalog.scheduled(), 1):
        if index in setup_entries_map:
            setup_file_entry.mschn = max(
                setup_entry.nchan 
                for setup_entry in setup_entries_map[index])
            setup_file_entry.minbbc = min(
                station_entries[setup_entry.isetsta-1].nbbc
                for setup_entry in setup_entries_map[index])
        else:
            setup_file_entry.mschn = 0
            setup_file_entry.minbbc = 99
    setup_file_catalog.write(range(len(setup_file_catalog.scheduled())))

    s.setfirm()

    if not s.schn1.vlaonly:
        for index, entry in enumerate(setup_catalog.scheduled(), 1):
            s.setchan(index)
            entry.needcat = s.setfreq(index)
            s.sethw1(index)

    for index, entry in enumerate(setup_catalog.scheduled(), 1):
        s.setfcat(index, entry.needcat)

    setup_catalog.read()
    for index in range(1, len(setup_catalog.scheduled()) + 1):
        setbbc(index, setup_catalog, FrequencyCatalog().read(), 
               station_entries)

    s.setrec()

    setup_catalog.read()
    for index, entry in enumerate(setup_catalog.scheduled(), 1):
        if (entry.track[0, 0] == 0) and \
           any(entry.format.startswith(f) 
               for f in ("VLBA","MKIV", "VDIF", "MARK5B", "LBA")):
            # transpose the track order as settrk returns it in fortran order
            entry.track = s.settrk(
                entry.nchan, entry.tapemode, entry.format, entry.bits,
                entry.bbc, entry.sidebd, entry.dbe, 
                station_entries[entry.isetsta-1].dar, index, 
                s.schn5.twohead, s.schcon.debug, parameter.ilog).T
    setup_catalog.write(range(len(setup_catalog.scheduled())))
    
    s.setusyn()
    setup_catalog.read()
    for entry in setup_catalog.scheduled():
        entry.vfesyn.fill(0)
    setup_catalog.write(range(len(setup_catalog.scheduled())))
