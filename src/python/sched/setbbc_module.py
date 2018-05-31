from sched import bbcdbbc

import schedlib as s

# SCHED's version only has the ks parameter, 
# add the catalogs to prevent reloading
def setbbc(ks, setup_catalog, frequency_entries, station_entries):
    if s.setn1.sdebug:
        s.wlog(0, "SETBBC: Starting.")

    setup_entry = setup_catalog.entries[ks-1]
    for ich in range(setup_entry.nchan):
        if setup_entry.ifreqnum[ich] >= 1:
            if setup_entry.ifchan[ich] == "":
                setup_entry.ifchan[ich] = \
                    frequency_entries[setup_entry.ifreqnum[ich]-1].\
                    fifnam[setup_entry.ifreqif[ich]-1]
        else:
            if setup_entry.ifchan[ich] == "":
                s.wlog(1, "SETBBC: Cannot set IFCHANs.  First bad channel: "
                       "{} of {} total.".format(ich + 1, setup_entry.nchan))
                s.errset(ks)
    setup_catalog.write(range(ks-1, ks))
    
    station_entry = station_entries[setup_entry.isetsta-1]
    if (station_entry.recorder == "S2") and \
       (station_entry.dar in ("VLBA", "VLBAG", "VLBA4")):
        s.bbcvs2(ks)
    else:
        def handle_dbbc(ks):
            bbcdbbc(ks, setup_entry, station_entry)
            setup_catalog.write(range(ks-1, ks))
        bbc_func = {"VLBA": s.bbcvlba,
                    "RDBE": s.bbcrdbe,
                    "RDBE2": s.bbcrdbe,
                    "WIDAR": s.bbcwidar,
                    "VLBAG": s.bbcgeo,
                    "VLBA4": s.bbcgeo,
                    "MKIV": s.bbcm4,
                    "DBBC": handle_dbbc,
                    "CDAS": s.bbccdas,
                    "R1002": s.bbckvsr,
                    "LBA": s.bbclba}.get(station_entry.dar, None)
        if bbc_func is not None:
            bbc_func(ks)
        elif station_entry.dar != "NONE":
            s.wlog(1, "SETBBC: SCHED does not set default BBCs for format: {}, "
                   "DAR type: {}".format(setup_entry.format, station_entry.dar))
            s.errset(ks)
