from ..catalog import StationCatalog
from . import parameter
from .. import util, key

import schedlib as s

import collections
import itertools
import math

def stread(input_iterator, stdin, mjd1):
    
    if input_iterator.input_ is stdin:
        infile = "Program_input"
        locfile = "NOLOC"
    else:
        infile = input_iterator.input_.name
        locfile = util.f2str(s.schcst.locafile)

    # 100 - 25: magic number copied from stread.f
    s.wlog(0, "STREAD:  Reading station catalog: {}".format(infile[-(100-25):]))
    s.wlog(0, "STREAD:  Reading locations file:  {}".format(locfile))

    if locfile.upper not in ("NONE", "NOLOC"):
        try:
            f = open(locfile, "r")
        except Exception as e:
            noloc = True
        else:
            with f:
                location_keyin_data = key.read_keyfile(f)
            noloc = False
    else:
        noloc = True

    mdb = s.rdcatn.dbx.shape[0]
    if (not noloc) and (len(location_keyin_data) > mdb):
        s.error("RDLOC: Too many stations in location file. Max: {}".format(
            mdb))

    station_state_defaults = {
        "version": ["", util.noop],
    }
    station_record_defaults = {
        "station":  ["NONAME",     util.upper],
        "stcode":   ["XX",         util.noop],
        "dbname":   ["",           util.noop],
        "dbcode":   ["",           util.noop],
        "frame":    ["XX",         util.noop],
        "elev":     [0.,           util.noop],
        "lat":      [0.,           util.multiply_by(parameter.raddeg / 3600.)],
        "long":     [0.,           util.multiply_by(parameter.raddeg /3600.)],
        "zalim":    [None,         util.noop],
        "x":        [0.,           util.noop],
        "y":        [0.,           util.noop],
        "z":        [0.,           util.noop],
        "dxdt":     [0.,           util.noop],
        "dydt":     [0.,           util.noop],
        "dzdt":     [0.,           util.noop],
        "epoch":    [0.,           util.noop],
        "descrip":  ["",           util.noop],
        "control":  ["NONE",       util.upper],
        "dar":      ["NONE",       util.noop],
        "recorder": ["NONE",       util.noop],
        "ndrives":  [2.,           util.noop],
        "nheads":   [1.,           util.noop],
        "disk":     ["",           util.noop],
        # two spellings, disk has preference over disc
        "disc":     ["NONE",       util.noop],
        "disk":     ["NONE",       util.noop],
        "mediadef": ["NONE",       util.noop],
        "nbbc":     [8.,           util.noop],
        "dbbcver":  ["ASTRO",      util.noop],
        "hor_az":   [[],           util.noop],
        "hor_el":   [[],           util.noop],
        "ax1lim":   [[-90., 450.], util.noop],
        "ax2lim":   [[2., 90.],    util.noop],
        "ax1rate":  [1000.,        util.noop],
        "ax2rate":  [1000.,        util.noop],
        "ax1acc":   [[1000., 0],   util.noop],
        "ax2acc":   [[1000., 0],   util.noop],
        "mount":    ["ALTAZ",      util.noop],
        "axistype": [0.,           util.noop],
        "axisoff":  [0.,           util.noop],
        "tsettle":  [0.,           util.noop],
        "minsetup": [0.,           util.noop],
        "tscal":    ["GAP",        util.upper],
        "maxsrchr": [1e6,          util.noop],
        "tlevset":  [0.,           util.noop],
        "endcat":   [0.,           util.noop],
    }
    input_iterator.set_defaults(station_record_defaults, station_state_defaults)
    
    location_state_defaults = {
        "version": ["Not known", util.noop],
    }
    
    location_record_defaults = {
        "dbname":   ["",  util.noop],
        "dbcode":   ["",  util.noop],
        "frame":    ["",  util.noop],
        "axistype": ["",  util.noop],
        "axisoff":  [0.,  util.noop],
        "x":        [0.,  util.noop],
        "y":        [0.,  util.noop],
        "z":        [0.,  util.noop],
        "dxdt":     [0.,  util.noop],
        "dydt":     [0.,  util.noop],
        "dzdt":     [0.,  util.noop],
        "epoch":    [0.,  util.noop],
        "begin":    [0.,  util.noop],
        "end":      [1e5, util.noop],
    }
    
    locations = collections.defaultdict(list)
    if not noloc:
        for loc in location_keyin_data:
            values, present = util.merge_record_with_defaults(
                loc, location_record_defaults, location_state_defaults)
            locations[values["dbname"]].append(values)
    
    # warn about location close together with time overlap
    # don't warn about known cases
    compare_locations = [loc for loc in sum(locations.values(), []) 
                         if loc["dbname"] not in ("MIAMI20", "SINTOTU") and 
                         not loc["dbname"].startswith("VLA")]
    for loc_a, loc_b in itertools.combinations(compare_locations, 2):
        separation = math.sqrt(
            sum([(loc_a[d] - loc_b[d]) ** 2 for d in "xyz"]))
        time_overlap = (min(loc_a["end"], loc_b["end"]) >
                        max(loc_a["begin"], loc_b["begin"]))
        if (separation < 10.) and time_overlap:
            s.wlog(0, "RDLOC:  Locations.dat stations {}   and {}   appear to "
                   "be the same (time and position).  Sep (m):{:10.2f}".format(
                       loc_a["dbname"], loc_b["dbname"], separation))
            
    # update the station values with station and location file catalog values
    station_catalog = StationCatalog()

    warnxyz = True
    locwarn = True
    # map from station catalog entry attributes to keyin keywords
    attribute_to_key = dict(zip(station_catalog.attributes, 
                                station_catalog.attributes))
    attribute_to_key.update({
        "long_bn": "long",
        "horaz": "hor_az",
        "horel": "hor_el",
        "xpos": "x",
        "ypos": "y",
        "zpos": "z",
        "dxpos": "dxdt",
        "dypos": "dydt",
        "dzpos": "dzdt",
        "mjdrate": "epoch",
        "axoff": "axisoff",
        "stndriv": "ndrives"})
    
    start = int(s.schsta.msta)
    index = start
    seen = set(entry.station for entry in station_catalog.entries[:index])
    for record in input_iterator:
        station_values, present = util.merge_record_with_defaults(
            record, station_record_defaults, station_state_defaults)
        if "endcat" in present:
            break

        if index >= len(station_catalog.entries):
            s.errlog(" STREAD: Too many stations in catalog, max {} "
                     " Last station: {}".format(
                         len(station_catalog.entries), 
                         station_catalog.entries[-1].station))

        if station_values["station"] in seen:
            s.wlog(0, "STREAD:  Ignoring extra station catalog entry for {}".\
                   format(station_values["station"]))
            continue
        else:
            seen.add(station_values["station"])
            

        # disc/disk confusion cases
        if "disk" not in present:
            station_values["disk"] = station_values["disc"]
        if station_values["mediadef"] == "DISC":
            station_values["mediadef"] = "DISK"

        # copy all values to the common block placeholder
        entry = station_catalog.entries[index]
        entry.set_keyin_values(station_values, attribute_to_key)

        entry.stcodeu = entry.stcode.upper()
        
        # fill in x, y, z from elev, lat, long (or reverse)
        gotxyz = ((entry.xpos, entry.ypos, entry.zpos) != (0., 0., 0.))
        gotllh = ((entry.elev, entry.lat, entry.long_bn) != (0., 0., 0.))
        if gotxyz and gotllh and warnxyz:
            s.putout("RDSTA: Both XYZ and Lat/Long coordinates given for some "
                     "stations.")
            s.putout("       There is nothing to insure they agree.")
            warnxyz = False
        
        conversion_mode = None # 0: llh -> xyz, 1: xyz -> llh
        if gotxyz and (not gotllh or (entry.elev > 1e6)):
            conversion_mode = 1
        elif gotllh and not gotxyz:
            conversion_mode = 0
        elif not gotllh and not gotxyz:
            if noloc or (len(locations) == 0):
                if locwarn:
                    if locfile.upper() == "NOLOC":
                        s.putout("RDSTA: **Cannot use locations catalog with "
                                 "in-line stations catalog.")
                    elif locfile.upper() != "NONE":
                        s.putout("RDSTA:  Could not open locations catalog: "
                                 "{}".format(locfile))
                    locwarn = False
                s.putout("RDSTA:   No coordinates for {}".format(
                    station_values["station"]))
            else:
                # get xyz from locations catalog
                station_locations = locations.get(station_values["dbname"])
                if station_locations is not None:
                    for location in station_locations:
                        if location["begin"] <= mjd1 < location["end"]:
                            for attribute in ["xpos", 
                                              "ypos",
                                              "zpos",
                                              "dxpos",
                                              "dypos",
                                              "dzpos",
                                              "mjdrate",
                                              "axoff"]:
                                setattr(entry, attribute, 
                                        location[attribute_to_key[attribute]])
                            conversion_mode = 1
                            break
                if conversion_mode is None:
                    s.putout("RDSTA: No coordinates in stations or locations "
                             "file for {} dbname: {}".format(
                                 station_values["station"], 
                                 station_values["dbname"]))
                    s.putout("RDSTA: Check date range in addition to names.")
                   
        if conversion_mode is not None:
            (entry.long_bn, entry.lat, entry.elev, 
             entry.xpos, entry.ypos, entry.zpos, ier) = \
                s.geoxyz(conversion_mode, 
                         entry.long_bn, entry.lat, entry.elev, 
                         entry.xpos, entry.ypos, entry.zpos)
            if ier != 0:
                s.putout("RDSTA: Problem with coordinate conversions for {}".\
                         format(station_values["station"]))
        
        if entry.ax1acc[1] == 0:
            entry.ax1acc[1] = entry.ax1acc[0]
        if entry.ax2acc[1] == 0:
            entry.ax2acc[1] = entry.ax2acc[0]

        entry.naxlim = min(len(entry.ax1lim), len(entry.ax2lim)) // 2

        if "zalim" not in present:
            if entry.mount == "ALTAZ":
                entry.zalim = 90. - entry.ax2lim[0]
            else:
                entry.zalim = 90.

        control = station_values["control"]
        entry.vlbadar = ((len(control) >= 5) and (control[4] == 'V'))
        
        dar = station_values["dar"]
        entry.useonsrc = ((dar.startswith("RDBE") and 
                           control.startswith("VLBA")) or 
                          ((dar == "WIDAR" ) and 
                           (control == "VEX")))

        def check(value, allowed, not_supported, station, element_type):
            if value not in allowed:
                s.errlog("STREAD: Invalid {} type {} for {}".format(
                    element_type, value, station))
            if value in not_supported:
                s.errlog("STREAD: {} type {} no longer supported by "
                         "SCHED.  Station: {}".format(
                             element_type[0].upper() + element_type[1:], 
                             value, station))

        check(entry.control, 
              ("VLA", "VLBA", "NRAO", "NRAOV", "SNAP", "VEX", "SN50", "VSOP", 
               "NONE"),
              ("SNAP", "SN50"),
              entry.station,
              "control")
        if entry.control.startswith("VLA"):
            s.errlog("STREAD: Control type VLA (old system card images) no "
                     "longer supported.  Use VEX.")

        check(entry.dar,
              ("VLBA", "RDBE", "RDBE2", "DBBC", "DBBC3", "VLBAG", 
               "MKIV", "MKIII", "S2", "K4", "K5", "VERA", "VSOP", 
               "VLBA4", "LBA", "R1002", "WIDAR", "CDAS", "eMERL", 
               "NONE"),
              ("MKIII", "S2"),
              entry.station,
              "DAR")

        check(entry.recorder,
              ("VLBA", "MKIV", "VLBA4", "MKIII", 
               "S2", "K4", "K5", "VERA", "VSOP", 
               "MARK5A", "MARK5B", "MARK5C", "NONE"),
              ("MKIII", "S2"),
              entry.station,
              "recorder")
            
        check(entry.disk,
              ("MARK5A", "MARK5B", "MARK5C", "LBADR", "NONE"),
              tuple(),
              entry.station,
              "DISK")

        check(entry.mediadef,
              ("TAPE", "DISK", "NONE"),
              tuple(),
              entry.station,
              "MEDIADEF")

        if (entry.xpos, entry.ypos, entry.zpos) == (0., 0., 0.):
            s.errlog("STREAD: Location required for {}: infile={}, locfile={}".\
                     format(entry.station, infile, locfile))

        if entry.tscal not in ("CONT", "GAP"):
            s.errlog("STREAD: Unknown TSCAL for {}:  {}".format(
                entry.station, entry.tscal))
        
        index += 1
              
    s.schsta.msta = index

    s.schcst.stver = util.resize_string(station_state_defaults["version"][0],
                                        s.schcst.stver.itemsize, "version")
    s.schcst.locaver = util.resize_string(location_state_defaults["version"][0],
                                          s.schcst.locaver.itemsize, "version")

    station_catalog.write(range(start, index))

