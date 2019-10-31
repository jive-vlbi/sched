from . import scndup
from ..catalog import ScanCatalog

import schedlib as s

scan_catalog = ScanCatalog()
def optnone(k_scan, scan_index):
    """
    Copies scan indexed by k_scan to scan indexed by scan_index,
    if the indices differ and not done already yet.
    Return tuple of booleans (adjust, keep, done)
    """
    if s.schcon.debug and (k_scan <= 3):
        s.wlog(0, "OPTNONE: Starting.")

    done = (k_scan > s.schn1.nscans)
    if (not done) and (k_scan != scan_index):
        scndup(scan_index - 1, k_scan - 1, True, "OPTNONE")

    # duronly, 1 means only start times, 4 only stop times specified
    adjust = (s.schn1.dwells and (scan_index > s.schn1.scan1)) and \
             (scan_catalog.direct_access_entries[scan_index - 1].duronly in 
              {1, 4})
    return (adjust, True, done)
        
