from .catalog import Catalog

import schedlib as s

class FrequencySetCatalog(Catalog):
    """
    INTEGER           FSETKS(MFSET)
    INTEGER           FSETSCN(MFSET)
    INTEGER           FSETPS(3,MFSET)
    INTEGER           FSSAME(MFSET)  
    CHARACTER         FSPCAL(MFSET)*4
    """

    max_freq_set = s.fsetn.fsetks.shape[0]
    block_items = {
        s.fsetn: [
            "fsetks",
            "fsetscn",
            "fsetps",
            "fssame"
        ],
        s.fsetc: [
            "fspcal"
        ]
    }
        
    def __init__(self):
        super().__init__(self.max_freq_set, self.block_items)

    def scheduled_slice(self):
        return slice(s.fsetn.nfset)
