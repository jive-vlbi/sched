from . import parameter, scndup

import schedlib as s

group = None
repeat = None

def schrep(values, entries, index, marker, start, stop, day, year):
    global group, repeat
    if s.schcon.debug:
        s.wlog(0, "SCHREP starting.")
    # if we get a repeat request, store it such that we can execute it when
    # the requested number of scan (group) has been parsed
    if marker == -1:
        repeat = int(values["repeat"])
        group = int(values["group"])
        if repeat != 0:
            marker = index
    elif values["repeat"] > 1:
        s.errlog("SCHREP: Cannot do nested or overlapping loops.")

    if s.schcon.debug:
        s.wlog(0, "SCHREP: Scan - {}  Repeat and group: {} {}".format(
            index+1, repeat, group))

    if (marker != -1) and (index + 1 >= group + marker):
        for r in range(repeat-1):
            for g in range(group):
                index += 1
                scndup(index, marker, False, "SCHREP", use_direct_access=False)
                start[index] = parameter.unset
                stop[index] = parameter.unset
                day[index] = day[marker]
                year[index] = year[marker]

                marker += 1

        marker = -1
    
    return index, marker
