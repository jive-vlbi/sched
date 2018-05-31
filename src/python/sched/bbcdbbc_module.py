from sched import bbcalt, ifdbbc

import schedlib as s

import numpy as np

def bbcdbbc(ks, setup_entry, station_entry):
    """
    ks: Fortran index into setups
    setup_entry: changes will be written to catalog
    station_entty: read only
    """
    if s.schcon.debug:
        s.wlog(1, "BBCDBBC: Starting")

    # 32 MHz requires E mode firmware, for now configure non-E if not required
    # FIX maybe add E mode firmware switch to setup (keyin)?
    e_firmware = setup_entry.samprate > 63
    ifbbc, mif = ifdbbc(station_entry.dbbcver, e_firmware) 
    max_bbc = ifbbc.shape[0]
    if station_entry.nbbc > max_bbc:
        s.wlog(1, "BBCDBBC: Number of VCs at {} Larger than maximum expected: "
             "{}".format(setup_entry.setsta[0], max_bbc))
        s.wlog(1, "   Catalog or programming problem ")
        s.errset(ks)
    ubbc = bbcalt(ks, setup_entry, ifbbc, list("ABCD"), "DBBC", "BBCDBBC")
