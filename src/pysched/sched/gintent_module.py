from .. import util

import schedlib as s

import numpy as np

def gintent(intents, entries, scan_index):
    entry = entries[scan_index]
    if len(intents) > 0:
        entry.nscint = len(intents)
        for intent_index, intent in enumerate(intents):
            # find in stored intents
            padded = util.resize_string(intent, s.schc2c.intent.itemsize,
                                        "intents")
            stored_indices = np.argwhere(
                s.schc2c.intent[:s.schn2a.nintent] == padded.encode())
            if len(stored_indices) == 0:
                if s.schn2a.nintent >= len(s.schc2c.intent):
                    s.errlog("GINTENT: Too many distinct intents. "
                             "Max {}  Report need for more.".format(
                                 len(s.schc2c.intent)))
                s.schc2c.intent[s.schn2a.nintent] = padded
                s.schn2a.nintent += 1
                entry.iscint[intent_index] = s.schn2a.nintent
            else:
                entry.iscint[intent_index] = stored_indices[0] + 1

        if "NONE" in (intent.upper() for intent in intents):
            if len(intents) > 1:
                s.errlog("GINTENT:  Please do not mix 'NONE' with "
                         "other INTENTs")
            entry.iscint[0] = 0
            entry.nscint = 0

    else:
        if scan_index + 1 > s.schn1.scan1:
            prev = entries[scan_index-1]
            entry.nscint, entry.iscint = prev.nscint, prev.iscint
