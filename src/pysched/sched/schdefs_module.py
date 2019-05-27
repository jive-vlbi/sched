import schedlib as s

import pkg_resources

import os

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

def get_catalog_dir():
    if "SCHED" in os.environ:
        return os.path.join(os.environ["SCHED"], "catalogs")
    else:
        return os.path.join(pkg_resources.resource_filename("pysched", ".."), 
                            "catalogs")
