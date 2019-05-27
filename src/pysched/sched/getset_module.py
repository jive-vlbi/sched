from . import rdset
from ..catalog import SetupFileCatalog, SetupCatalog
from .. import key

import schedlib as s

def getset():
    if s.schcon.debug:
        s.wlog(0, "GETSET: Starting.")

    for index, entry in enumerate(SetupFileCatalog().read()):
        setfilename = entry.setfile
        if setfilename in {"DUMMY", "DEFAULT"}:
            s.errlog(" SETUP file required. ")
        else:
            if setfilename not in \
               (entry.setname for entry in SetupCatalog().read()):
                try:
                    f = open(setfilename, "r")
                except Exception as e:
                    s.wlog(1, str(e))
                    s.errlog("RDSET: Problem opening setup file")
                with f:
                    input_iterator = key.KeyfileLister(f)
                    rdset(setfilename, input_iterator, index+1)

    s.setn2a.fifmin[:, :s.setn1.nset].setfield(0., dtype=s.setn2a.fifmin.dtype)
    s.setn2a.fifmax[:, :s.setn1.nset].setfield(1e15, 
                                                dtype=s.setn2a.fifmax.dtype)
    s.setn2a.corinv[:, :s.setn1.nset].setfield(0., dtype=s.setn2a.corinv.dtype)
