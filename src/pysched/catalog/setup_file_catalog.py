from .catalog import Catalog

import schedlib as s

class SetupFileCatalog(Catalog):
    """
    DOUBLE PRECISION SFFREQ(MAXCHN,MAXSET)
                     SFFILT(MAXCHN,MAXSET)
    INTEGER          MSCHN(MAXSET)
    INTEGER          SAMEBBC(MAXCHN,MAXSET)
                     MINBBC(MAXSET)
    REAL             FSPEED(MAXSET)
    LOGICAL          OKXC(MAXSET)
    CHARACTER        SETFILE(MAXSET)*80
                     SFSIDE(MAXCHN,MAXSET)*1
    CHARACTER        SFPOL(MAXCHN,MAXSET)*3
    """
    
    max_setup_files = s.schsf.mschn.shape[0]
    block_items = {
        s.schsf: [
            "sffreq", 
            "sffilt", 
            "mschn", 
            "samebbc", 
            "minbbc", 
            "fspeed", 
            "okxc"
        ],
        s.schssf: [
            "setfile", 
            "sfside", 
            "sfpol"
        ]
    }

    def __init__(self):
        super().__init__(self.max_setup_files, self.block_items)

    def scheduled_slice(self):
        return slice(s.schsf.nsetf)

    @classmethod
    def extend_with(cls, filename, error_message):
        """
        Write a new catalog entry directly into the common blocks
        and return the (Fortran based) index of the entry
        """
        catalog = cls()
        catalog.read()
        # create an entry for the setup file if it is new
        index = next((index for index, entry in enumerate(catalog.scheduled())
                      if entry.setfile == filename), None)
        if (index is None) and (s.schsf.nsetf < catalog.max_setup_files):
            index = int(s.schsf.nsetf)
            catalog.entries[index].setfile = filename
            s.schsf.nsetf += 1
            catalog.write(range(index, int(s.schsf.nsetf)))
        elif (index is None):
            s.errlog(error_message)
        return index + 1
