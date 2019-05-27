from . import parameter

import schedlib as s

def gettim(values, entries, index, start, stop, day, year, mjd1):
    day[index] = values["day"]
    month = values["month"]
    year[index] = values["year"]
    if year[index] < 100 and year[index] != 0:
        s.errlog("GETTIM:  Don''t use 2 digit year!")

    if (day[index] != 0) and (month > 1):
        year[index], day[index], error = s.sla_calyd(
            year[index], month, day[index])
        if error == 2:
            s.errlog("GETTIM: Bad month ")
        if error == -1:
            s.errlog("GETTIM: Unlikely year ")

    start[index] = values["start"] / 86400. \
                   if values["start"] != parameter.unset else parameter.unset
    stop[index] = values["stop"] / 86400. \
                  if values["stop"] != parameter.unset else parameter.unset

    entry = entries[index]

    if index == 0:
        if day[index] > 366:
            myear, mday, mut = s.lst2ut(0., day[index], 0.)
            mjd1, error = s.sla_cldj(myear, 1, mday)
            if error not in [0, 3]:
                s.errlog("GETTIM: Problem converting day to MJD")
        else:
            mjd1, error = s.sla_cldj(year[index], 1, day[index])
            if error not in [0, 3]:
                s.errlog("GETTIM: Problem getting MJD of first scan before "
                         "station catalog read.")

    if (values["dwell"][0] != parameter.unset) and \
       (values["duration"] != parameter.unset):
        s.errlog("GETTIM: Don''t specify both DUR and DWELL!  Input scan: {}".\
                 format(index+1))
    elif values["dwell"][0] != parameter.unset:
        entry.dwell = True
        entry.dur = values["dwell"][0] / 86400.
        entry.nowait = values["dwell"][1] 
        entry.mindw = values["dwell"][2] / 86400.
    elif values["duration"] != parameter.unset:
        entry.dwell = False
        entry.dur = values["duration"] / 86400.
        entry.nowait, entry.mindw = (0, 0.)
    else:
        if index == 0:
            entry.dwell = False
            entry.dur, entry.nowait, entry.mindw = (0., 0, 0.)
        else:
            prev = entries[index - 1]
            entry.dwell, entry.dur, entry.nowait, entry.mindw = \
                prev.dwell, prev.dur, prev.nowait, prev.mindw

    s.schn1.dwells |= entry.dwell
    return mjd1
