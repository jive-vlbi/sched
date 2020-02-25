from ..catalog import ScanCatalog, SourceCatalog, SetupCatalog
from ..util import f2str, resize_string
from .. import vex

import schedlib as s

def vexout():
    if s.schcon.dovex:
        scan_catalog = ScanCatalog()
        scan_catalog.read()
        source_catalog = SourceCatalog()
        source_catalog.read()
        sources = source_catalog.scheduled()
        moving = False
        for scan in scan_catalog.used():
            source = sources[scan.srcnum - 1] # -1: adjust for FORTRAN indexing
            if source.sused and not scan.norec and \
               (source.planet or source.satel or 
                (source.dra >= 0.0001/15) or (source.ddec >= 0.0001)):
                moving = True
                break
        if moving:
            s.wlog(1, "++++ VEXOUT: Cannot pass accurate positions for planets "
                   "or satellites in the VEX file.")
            s.wlog(1, "        For correlation, get the source positions "
                   "elsewhere - like from ephemeris.")
            s.wlog(1, "        For VLBA pointing observations as of 2011, the "
                   "VEX positions")
            s.wlog(1, "        are not used so this is ok.  You may ignore the "
                   "warnings from VXSUDT.")

        setup_catalog = SetupCatalog()
        setup_catalog.read()
        setups = setup_catalog.used()
        all_none = all(setup.format == "NONE" for setup in setups)
        observation_type = f2str(s.schsco.obstyp)
        if (s.schcon.override or (not all_none) or \
            (observation_type == "PTVLBA")) and \
            (observation_type != "CONFIG"):
            # write two VEX (version) files, 
            # only print warnings while making the first one
            with open("{}.vex".format(f2str(s.schc1.expcode).lower()), "w") \
                 as vex_file:
                s.wlog(0, "Writing V E X file {}".format(vex_file.name))
                vex.write(vex_file, vex_version="1.5", print_warnings=True)
                # write VEX file name to common block for v2dout
                s.vex1.vexfile = resize_string(vex_file.name, 
                                               s.vex1.vexfile.itemsize,
                                               "vexfile")
            with open("{}.vex2".format(f2str(s.schc1.expcode).lower()), "w") \
                 as vex_file:
                s.wlog(0, "Writing V E X 2 file {}".format(vex_file.name))
                vex.write(vex_file, vex_version="2.0", print_warnings=False)

            if observation_type != "PTVLBA":
                s.v2dout()
        elif all_none:
            s.errlog("VEXOUT: Cannot use VEX with a schedule in which all "
                     "scans have FORMAT=NONE.")
        elif s.schn1.config:
            s.errlog("VEXOUT: Do not mix VEX with Configuration tests. "
                     "Set DOVEX=-1")
