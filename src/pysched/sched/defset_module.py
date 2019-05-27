from . import setexpnd, setdefs, chkset

import schedlib as s

def defset():
    setexpnd()
    setdefs()
    s.chkfirm()
    for i in range(s.setn1.nset):
        chkset(i + 1)
        
    s.chksfil()
    s.sfinfo()
    s.sbpair()
    s.chkvdifx()
    s.chkjive()
