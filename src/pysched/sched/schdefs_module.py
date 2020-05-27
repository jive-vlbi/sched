from ..util import get_catalog_dir

import schedlib as s

import os.path

file_type_defaults = {
    "location": "locations.dat",
    "stations": "stations_RDBE.dat",
    "sources":  "sources.gsfc",
    "frequency": "freq_RDBE.dat",
    "peakcommand": "peak.cmd",
    "refpointing": "sources.peak",
    "messages": "messages.txt"}

def schdefs(file_type):
    if file_type not in file_type_defaults:
        s.putout("SCHFILES: Unrecognized file type: {}".format(file_type))
        s.error("Programming problem")
        return ""
    default = file_type_defaults[file_type]
    return os.path.join(get_catalog_dir(), default)
