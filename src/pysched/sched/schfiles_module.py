from . import srread, rdpeak, stread, rfreq, satini, pcread, rdset
from ..catalog import SetupFileCatalog

import schedlib as s

def schfiles(input_iterator, stdin, values, present, gotsat):
    restart = False
    if s.schcon.plot and (input_iterator.input_ is stdin):
        s.errlog("SCHFILES: Must read input from SCHEDULE file when plotting.")

    if "tapeini" in present:
        s.errlog("TAPEINI sections no longer supported in SCHED")
        
    if "satinit" in present:
        if gotsat:
            s.errlog("SCHFILES: Can only read one SATINI section.")
        gotsat = True

    def handle_setinit():
        setinit = values["setinit"]
        index = SetupFileCatalog.extend_with(setinit, 
                                             "SCHFILES: Too many setup files.")
        
        rdset(setinit, input_iterator, index)
            
    # handle inline catalogs, possible ones are:
    inline_catalogs = [
        ("srccat", "Imbedded source catalog.", 
         lambda: srread(input_iterator, stdin, False, 'i')),
        ("peakinit", "Imbedded peak inst.", 
         lambda: rdpeak(input_iterator, stdin)),
        ("stacat", "Imbedded station catalog.", 
         lambda: stread(input_iterator, stdin, mjd1=0.)), # mjd1 is unused
        ("lineinit", "Reading line freqs.", 
         lambda: rfreq(input_iterator)),
        ("satinit", "Reading satellite info.", 
         lambda: satini(input_iterator)),
        ("setinit", "Imbedded setup: {}".format(values["setinit"]),
         handle_setinit),
        ("pcenters", "Phase center lists.", 
         lambda: pcread(input_iterator))
    ]
    todo = [cat for cat in inline_catalogs if cat[0] in present]
    # can do only one at a time
    if len(todo) > 1:
        s.errlog("'SCHFILES: Cannot do 2 in-stream files at the same time.")
    for keyword, text, function in todo:
        if s.schcon.debug:
            s.wlog(1, "SCHFILES: {}".format(text))
        function()
        restart = True

    if "coverlet" in present:
        # copy text verbatim to temporary file 
        # until "endcover" and "/" are on one line
        try:
            temp_file = open("schedcov.tmp", "w")
        except Exception as e:
            s.wlog(1, str(e))
            s.errlog("SCHFILES: Cannot open cover letter scratch file.  "
                     "This should not happen.")
        with temp_file:
            while True:
                line = input_iterator.parser.readline()
                if len(line) == 0:
                    s.errlog("SCHFILES: Cover letter not terminated with "
                             "ENDCOVER /")
                if (line.upper().find("ENDCOVER") != -1) and \
                   (line.find("/") != -1):
                    break
                temp_file.write(line)
        s.schcon.coverlet = True
        restart = True
        
    return gotsat, restart
