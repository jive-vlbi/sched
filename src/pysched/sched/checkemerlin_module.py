import schedlib as s

def check_emerlin(setup, station):
    bits = max(setup.bits)
    expected_samprate = 256 // bits
    if setup.samprate != expected_samprate:
        s.wlog(1, "Check eMERLIN: Invalid SAMPRATE specified for station {}: "
               "{} for DAR=eMERL. Must be {} Msamp/s for {} bits.".format(
                   station.station, setup.samprate, expected_samprate, bits))
        return True
    return False
