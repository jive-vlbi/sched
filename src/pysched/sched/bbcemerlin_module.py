# BBC and IF assignments in the VEX file are currently ignored by eMERLIN 
# out stations, for historical reasons emulate a GEO DBBC assignment

from . import bbcalt, ifdbbc

import schedlib as s

import numpy as np

def bbc_emerlin(ks, setup_entry, station_entry):
    """
    ks: Fortran index into setups
    setup_entry: changes will be written to catalog
    station_entry: read only
    """
    if s.schcon.debug:
        s.wlog(1, "BBCEMERLIN: Starting")

    ifbbc, mif = ifdbbc("GEO", False) 
    ifnam = list("AB")
    max_bbc = ifbbc.shape[0]
    if station_entry.nbbc > max_bbc:
        s.wlog(1, "BBCDBBC: Number of VCs at {} Larger than maximum expected: "
             "{}".format(setup_entry.setsta[0], max_bbc))
        s.wlog(1, "   Catalog or programming problem ")
        s.errset(ks)
    ubbc = bbcalt(ks, setup_entry, ifbbc, ifnam, "eMERL", "BBCEMERLIN")
