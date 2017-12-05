from sched import parameter

import schedlib as s

def toggle(present, key1, key2, default):
    value1 = (key1 in present)
    value2 = (key2 in present)
    if value1 and value2:
        s.errlog("TOGGLE: You can't set both {} and {}".format(key1, key2))
    elif not (value1 or value2):
        return default
    elif value1:
        return True
    return False
