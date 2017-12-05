from sched import rdset

import key

import schedlib as s

def getset():
    if s.schcon.debug:
        s.wlog(0, "GETSET: Starting.")

    for index, setfile in enumerate(s.schssf.setfile[:s.schsf.nsetf]):
        setfilename = bytes(setfile).decode().strip()
        if setfilename in {"DUMMY", "DEFAULT"}:
            s.errlog(" SETUP file required. ")
        else:
            if setfile not in s.setc1.setname[:s.setn1.nset]:
                try:
                    f = open(setfilename, "r")
                except Exception as e:
                    s.wlog(1, str(e))
                    s.errlog("RDSET: Problem opening setup file")
                with f:
                    input_iterator = key.KeyfileIterator(f)
                    rdset(setfilename, input_iterator, index+1)

    s.setn2a.fifmin[:, :s.setn1.nset].setfield(0., dtype=s.setn2a.fifmin.dtype)
    s.setn2a.fifmax[:, :s.setn1.nset].setfield(1e15, 
                                                dtype=s.setn2a.fifmax.dtype)
    s.setn2a.corinv[:, :s.setn1.nset].setfield(0., dtype=s.setn2a.corinv.dtype)
