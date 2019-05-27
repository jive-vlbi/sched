from . import parameter

import schedlib as s

def toggle(values, present, key1, key2, default):
    if (key1 in present) and (key2 in present):
        s.errlog("TOGGLE: You can't set both {} and {}!".format(key1, key2))
    elif (key1 not in present) and (key2 not in present):
        return default
    elif key1 in present:
        return values[key1]
    else:
        return not values[key2]

