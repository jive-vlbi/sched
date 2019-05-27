from ..catalog import SetupCatalog, StationCatalog

import schedlib as s

import numpy as np

import copy

def setexpnd():
    if s.schcon.debug:
        s.wlog(0, "SETEXPND: Starting.")

    for i in range(s.setn1.nset):
        s.setstdef(i+1)

    catalog = SetupCatalog()
    catalog.read()
    setup_entries = catalog.scheduled()
    copy_to = len(setup_entries)
    for setup_entry in setup_entries:
        for station in setup_entry.setsta[setup_entry.setsta != ""]:
            if copy_to >= len(catalog.entries):
                s.wlog(1, "SETEXPND: There are too many setup groups after "
                       "creating new ones")
                s.wlog(1, "          for each station in the input setups.")
                s.wlog(1, "          Maximum is {} which is setups times "
                       "stations.".format(len(catalog.entries)))
                s.errlog("SETEXPND: You need fewer setups or a bigger MSET in "
                         "SCHED.")
            dest = catalog.entries[copy_to]
            dest.__dict__.update(copy.deepcopy(setup_entry.__dict__))
            dest.setsta[0] = station
            copy_to += 1

    if copy_to == len(setup_entries):
        s.errlog("SETEXPND: None of the specified setups are used.")

    # move the new entries to the front and invalidate the others
    catalog.entries = catalog.entries[len(setup_entries):] + \
                      catalog.entries[:len(setup_entries)]
    s.setn1.nset = copy_to - len(setup_entries)

    station_catalog = StationCatalog()
    station_catalog.read()
    stations = station_catalog.used()
    for ks, setup_entry in enumerate(catalog.scheduled(), 1):
        setup_entry.listks = ks
        station_index = next(s.schn1.stanum[station.ischsta-1] 
                             for station in stations
                             if station.station == setup_entry.setsta[0])
        if station_index is None:
            s.wlog(1, "SETEXPND: Did not get station number. "
                   "Programming problem.")
            s.wlog(1, "          Station: {}".format(setup_entry.setsta[0]))
            s.errset(ks)
        setup_entry.isetsta = station_index

    catalog.write(range(s.setn1.nset))

    for lsta in range(s.schn1.nsta):
        for iscn in range(s.schn1.nscans):
            s.schn2a.nsetup[iscn, lsta] = s.gnset(iscn+1, lsta+1)


