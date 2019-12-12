from . import bbcdbbc, bbc_emerlin

import schedlib as s

# SCHED's version only has the ks parameter, 
# add the catalogs to prevent reloading
def setbbc(ks, setup_catalog, frequency_entries, station_entries):
    if s.setn1.sdebug:
        s.wlog(0, "SETBBC: Starting.")

    setup_entry = setup_catalog.entries[ks-1]
    for ich, channel in enumerate(setup_entry.channel):
        if channel.ifreqnum >= 1:
            if channel.ifchan == "":
                channel.ifchan = frequency_entries[channel.ifreqnum-1].\
                                 fifnam[channel.ifreqif-1]
        else:
            if channel.ifchan == "":
                s.wlog(1, "SETBBC: Cannot set IFCHANs.  First bad channel: "
                       "{} of {} total.".format(ich + 1, 
                                                len(setup_entry.channel)))
                s.errset(ks)
    setup_catalog.write(range(ks-1, ks))
    
    station_entry = station_entries[setup_entry.isetsta-1]
    if (station_entry.recorder == "S2") and \
       (station_entry.dar in ("VLBA", "VLBAG", "VLBA4")):
        s.bbcvs2(ks)
    else:
        def handle_func(f):
            def handle(ks):
                f(ks, setup_entry, station_entry)
                setup_catalog.write(range(ks-1, ks))
            return handle
        bbc_func = {"VLBA": s.bbcvlba,
                    "RDBE": s.bbcrdbe,
                    "RDBE2": s.bbcrdbe,
                    "WIDAR": s.bbcwidar,
                    "VLBAG": s.bbcgeo,
                    "VLBA4": s.bbcgeo,
                    "MKIV": s.bbcm4,
                    "DBBC": handle_func(bbcdbbc),
                    "DBBC3": handle_func(bbcdbbc),
                    "CDAS": s.bbccdas,
                    "R1002": s.bbckvsr,
                    "LBA": s.bbclba,
                    "eMERL": handle_func(bbc_emerlin)}.get(
                        station_entry.dar, None)
        if bbc_func is not None:
            bbc_func(ks)
        elif station_entry.dar != "NONE":
            s.wlog(1, "SETBBC: SCHED does not set default BBCs for format: {}, "
                   "DAR type: {}".format(setup_entry.format, station_entry.dar))
            s.errset(ks)
