from ..catalog import StationCatalog, ScanCatalog

import schedlib as s

import copy

station_catalog = StationCatalog()
scan_catalog = ScanCatalog()
copyall_attributes = {"startj", "stopj"}
def scndup(to, from_, copyall, caller, use_direct_access=True):
    if s.schcon.debug:
        s.wlog(0, "SCNDUP: Duplicating scan {} to scan {}.  Called by: {} "
               "copyall = {}".format(from_+1, to+1, caller, copyall))
        s.wlog(0, "SCNDUP: ANNOT: {}".format(f2str(s.schc2a.annot[from_])))
    if to >= ScanCatalog.maxscan:
        s.errlog("SCHDUP: Output scan number {} too big for arrays of "
                 "dimension: {}".format(to+1, ScanCatalog.maxscan))

    # copy the scan
    if use_direct_access:
        for block, items in scan_catalog.block_items.items():
            for attr in items:
                if copyall or (attr not in copyall_attributes):
                    array = getattr(block, attr)
                    array[..., to] = array[..., from_]
        entries = scan_catalog.entries
        for attr in scan_catalog.extended_attributes:
            setattr(entries[to], attr, 
                    copy.deepcopy(getattr(entries[from_], attr)))
    else:
        entries = scan_catalog.entries
        if not copyall:
            original = {k: getattr(entries[to], k) for k in copyall_attributes}
        # copy entry dict so the all references to entry are updated
        entries[to].__dict__ = copy.deepcopy(entries[from_].__dict__)
        if not copyall:
            for k, v in original.items():
                setattr(entries[to], k, v)
    
    s.schn2a.nsetup[to, :] = s.schn2a.nsetup[from_, :]
    s.schn2a.fseti[to, :] = s.schn2a.fseti[from_, :]
    s.schn2a.stascn[to, :] = s.schn2a.stascn[from_, :]
    s.schn2b.dopincr[to, :] = s.schn2b.dopincr[from_, :]
    if not copyall:
        # reset some entries
        if use_direct_access:
            s.schn2a.duronly[to] = 1
            s.schc2a.annot[to] = "".ljust(s.schc2a.annot.itemsize)
        else:
            entries[to].duronly = 1
            entries[to].annot = ""
    else:
        # copy station entries
        s.schn5.tpstart[to, :] = s.schn5.tpstart[from_, :]
        s.schn5.tcorr[to, :] = s.schn5.tcorr[from_, :]
        s.schn5.gbytes[to, :] = s.schn5.gbytes[from_, :]
        s.schn6.lst1[to, :] = s.schn6.lst1[from_, :]
        s.schn6.lst2[to, :] = s.schn6.lst2[from_, :]
        s.schn6.tonsrc[to, :] = s.schn6.tonsrc[from_, :]
        s.schn6.tslew[to, :] = s.schn6.tslew[from_, :]
        s.schn6.el1[to, :] = s.schn6.el1[from_, :]
        s.schn6.az1[to, :] = s.schn6.az1[from_, :]
        s.schn6.ha1[to, :] = s.schn6.ha1[from_, :]
        s.schn6.pa1[to, :] = s.schn6.pa1[from_, :]
        s.schc6.up1[to, :] = s.schc6.up1[from_, :]
        s.schn6.el2[to, :] = s.schn6.el2[from_, :]
        s.schn6.az2[to, :] = s.schn6.az2[from_, :]
        s.schn6.ha2[to, :] = s.schn6.ha2[from_, :]
        s.schn6.pa2[to, :] = s.schn6.pa2[from_, :]
        s.schc6.up2[to, :] = s.schc6.up2[from_, :]

    if not use_direct_access:
        # update the scan related attributes of stations in the catalog
        station_catalog.read_scheduled_attributes_for_scan(to)
