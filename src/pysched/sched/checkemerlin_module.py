import schedlib as s

def check_emerlin(setup, station):
    if setup.samprate != 128:
        s.wlog(1, "Check eMERLIN: Invalid SAMPRATE specified for station {}: "
               "{} for DAR=eMERL. Must be 128 Msamp/s.".format(station.station,
                                                               setup.samprate))
        return True
    return False
