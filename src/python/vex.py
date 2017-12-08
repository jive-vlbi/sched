from catalog import SetupCatalog, ScanCatalog, StationCatalog, SourceCatalog
from util import f2str
import dbbc_patching

from sched import parameter

import schedlib as s

import numpy as np

import re
from collections import defaultdict, OrderedDict
import itertools
import functools
import copy

vex_version = "2.0" # version of VEX definition
sched_version = "1" # version of SCHED VEX writing routine

class Block:
    def __init__(self, name, defs=[]):
        self.name = name
        self.defs = copy.deepcopy(defs)

class RefDef:
    def __init__(self, name, refs=[]):
        self.name = name
        self.refs = copy.deepcopy(refs)

class ValueDef:
    def __init__(self, name, params=[]):
        self.name = name
        self.params = copy.deepcopy(params)

class Ref:
    def __init__(self, block, name, qualifiers=[]):
        self.block = block
        self.name = name
        self.qualifiers = copy.deepcopy(qualifiers)

class ParamValues:
    def __init__(self, parameter, values):
        self.parameter = parameter
        self.values = values

block_separator = "*------------------------------------------------------"\
                  "------------------------\n"
def write(output):
    output.write(header_block())
    output.write(block_separator)
    output.write(global_block())
    output.write(block_separator)
    output.write(exper_block())
    output.write(block_separator)
    mode_text, scan_mode_name = modes_block()
    output.write(mode_text)
    output.write(block_separator)
    output.write(stations_block())
    output.write(block_separator)
    output.write(procedures_block())
    output.write(block_separator)
    output.write(source_block())
    output.write(block_separator)
    output.write(sched_block(scan_mode_name))

def header_block():
    return """
VEX_rev = {};
*    SCHED vers: {}
*    VEX/SCHED:  {}.{}
*    Other versions: Sched: {:5.1f} Plot: {:4.2f} JPL-ephem: {:4.2f}
""".format(vex_version,
           f2str(s.verc.version),
           vex_version, sched_version,
           float(s.vern.vernum), float(s.plver()), float(s.jplver()))[1:]

def global_block():
    return """
$GLOBAL;
     ref $EXPER = {};
*                                                      +------------+
*                         PI revision number:          |{:10.4f}  |
*                                                      +------------+
*    mode:       {}
""".format(str2def(f2str(s.schc1.expcode)),
           float(s.schco.schver),
           f2str(s.schsco.obsmode))[1:]

def exper_block():
    expcode = f2str(s.schc1.expcode)
    ret  = """
$EXPER;
*
def {};
     exper_name = {};
     exper_description = {};
     PI_name = {};
     PI_email = {};
""".format(str2def(expcode),
           str2val(expcode),
           str2val(f2str(s.schc1.expt)),
           str2val(f2str(s.schsco.piname)),
           str2val(f2str(s.schsco.email)))[1:]

    ret += """
*    address:   {}
*               {}
*               {}
*               {}
""".format(*(f2str(address) for address in s.schsco.address))[1:]

    ret += """
*    phone:     {}
*    during obs:{}
*    fax:       {}
""".format(*(f2str(contact) for contact in [s.schsco.phone,
                                                     s.schsco.obsphone,
                                                     s.schsco.fax]))[1:]

    comment_prefix = "*    "
    ret += text2comments((f2str(note) for note in s.schsco.note), 
                         comment_prefix)

    year, doy, time_ = s.timej(s.schn2c.stopj[0])
    year, month, day, jd, mname, dname = s.tdatecw(year, 1, doy)
    ret += """
*
*    year, doy: {}, {}
*    date     : {} {} {} {}
*    MJD      : {}
""".format(year, doy,
           f2str(dname), day, f2str(mname), year,
           int(jd - 2400000.5 ))[1:]

    ret += "     exper_nominal_start={};\n".format(time2str(s.schn2c.startj[0]))
    ret += "     exper_nominal_stop={};\n".format(time2str(
        s.schn2c.stopj[s.schn1.scanl - 1]))

    if s.schcon.coverlet:
        with open("schedcov.tmp", "r") as f:
            ret += "*\n"
            ret += "*-------------------- cover  letter --------------------\n"
            for line in f.readlines():
                ret += "*" + line
            ret += "*------------------------- end -------------------------\n"


    if s.schsco.correl:
        ret += """
*
     target_correlator = {};
*
""".format(str2val(f2str(s.schsco.correl)))[1:]

    for text, value in (
            ("*    integr_time    : {} s", float(s.schco.coravg)),
            ("*    number_channels: {}",   s.schco.corchan),
            ("*    number_antenna : {}",   s.schco.cornant),
            ("*    cross_polarize : {}",   "Yes" if s.schco.corpol else "No"),
            ("*    weight_func    : {}",   f2str(s.schsco.corwtfn)),
            ("*    distrib_medium : {}",   f2str(s.schsco.cortape)),
            ("*    distrib_format : {}",   f2str(s.schsco.cordfmt)),
            ("*    source_pos_cat : {}",   f2str(s.schsco.corsrcs))):
        if value:
            ret += text.format(value) + "\n"

    if s.schsco.corship[0]:
        ret += "*    distribute_to  :\n"
        for ship in s.schsco.corship:
            if ship:
                ret += "*                     {}\n".format(f2str(ship))

    
    ret += text2comments((f2str(note) for note in s.schsco.cornote), 
                         comment_prefix)
    ret += """
*
enddef;
"""[1:]

    return ret

def modes_block():
    # some common block variables are indexed by scan number, but not part 
    # of the scan catalog, the indices are offset from the catalog entries
    scan_offset = s.schn1.scan1 - 1
    scans = ScanCatalog().read()
    
    setups = SetupCatalog().read()

    stations = StationCatalog().read()
    stations = [stations[i-1] for i in s.schn1.stanum[:s.schn1.nsta]]

    format_map = {"MARKIII": "Mark3A",
                  "MKIV": "Mark4",
                  "VLBA": "VLBA",
                  # 2 valid LBA formats in vex2 LBA_AT and LBA_VSOP, 
                  # no idea which is supposed to be used
                  "LBA": "LBA_AT",
                  "MARK5B": "MARK5B",
                  "VDIF": "VDIF"} 
                  
    phase_cals = defaultdict(lambda: defaultdict(set))
    pcal_map = {"OFF" : 0,
                "1MHZ": 1,
                "5MHZ": 5,
                ""    : 1}
    def cmp_bitstream(x, y):
        (sideband_a, bbc_a, channel_a) = x
        (sideband_b, bbc_b, channel_b) = y
        if sideband_a != sideband_b:
            return 1 if sideband_a == "L" else -1
        if bbc_a != bbc_b:
            return bbc_a - bbc_b
        return channel_a - channel_b

    def do_if(setup, if_pcal):
        if_channel = OrderedDict(
            [(if_, channel) for channel, if_ in enumerate(setup.ifchan)])
        if_ = tuple(("if_def",
                     "&IF_" + if_name,
                     if_name,
                     setup.pol[channel][0], # only take first character (RCP->R)
                     "{:9.2f} MHz".format(setup.firstlo[channel]),
                     setup.side1[channel]) +
                    tuple("{:d} MHz".format(i) for i in if_pcal)
                    for if_name, channel in if_channel.items())
        return if_, if_channel
        
    def do_bbc(setup, if_, if_channel):
        bbc_channel = OrderedDict(
            [(bbc, channel) for channel, bbc in enumerate(setup.bbc)])
        bbc = tuple(("BBC_assign",
                     "&BBC{:02d}".format(bbc_number),
                     bbc_number,
                     # get IF link name
                     if_[tuple(if_channel).index(setup.ifchan[channel])][1])
                    for bbc_number, channel in bbc_channel.items())
        return bbc, bbc_channel

    def do_phasecal_and_freq(setup, scan, tone_interval, bbc, bbc_channel):
        # get LO sum and BBC width from schedule if specified,
        # else from setup
        lo_sum = [(scan.freq[channel_index] + 
                   setup.corinv[channel_index])
                  if scan.freq[channel_index] > 0 else
                  (setup.firstlo[channel_index] + 
                   (1 if setup.side1[channel_index] == "U" else -1) * 
                   setup.bbsyn[channel_index])
                  for channel_index in range(setup.nchan)]
        bbc_width = [scan.bw[channel_index]
                     if scan.bw[channel_index] > 0 else
                     setup.bbfilt[channel_index]
                     for channel_index in range(setup.nchan)]
        
        if tone_interval != 0:
            ntone, tone_channel, ntone_per_def, itone_per_def = \
                s.vxton2(-1, 
                         scan.setnum, 
                         lo_sum, 
                         setup.netside, 
                         bbc_width, 
                         tone_interval)
            phase_cal = tuple(("phase_cal_detect", "&PCD") +
                              tuple(itone_per_def[:ntone_per_def[
                                  phase_def], 
                                                  phase_def])
                              for phase_def in range(ntone))
        else:
            # ``empty'' phase cal and tone channel map
            phase_cal = (("phase_cal_detect", "&NoCal"),)
            tone_channel = [1 for channel_index in range(setup.nchan)]
        
        freq = tuple(("chan_def",
                      "",
                      "{:.6f} MHz".format(lo_sum[channel_index]).rstrip(".0"),
                      setup.netside[channel_index],
                      "{:7.2f} MHz".format(bbc_width[channel_index])
                      if bbc_width[channel_index] >= 1 else
                      "{:7.3f} kHz".format(bbc_width[channel_index] * 1000),
                      "&CH{:02d}".format(channel_index + 1),
                      # BBC link name
                      bbc[tuple(bbc_channel).index(
                          setup.bbc[channel_index])][1],
                      # phase cal link name, tone_channel is fortran
                      # based, so -1 for array indexing
                      phase_cal[tone_channel[channel_index] - 1][1])
                     for channel_index in range(setup.nchan))
        return phase_cal, freq
        
    def do_datastream_type(setup, freq):
        track_format = next((
            track_format for sched_format, track_format in format_map.items()
            if setup.format.startswith(sched_format)),
                            None)
        if track_format is None:
            return None, None, None
        
        channel_order = sorted(
            ((setup.sidebd[channel_index],
              setup.bbc[channel_index],
              channel_index)
             for channel_index in range(setup.nchan)),
            key=functools.cmp_to_key(cmp_bitstream))
        channel_order = ((sideband,
                          bbc,
                          bit,
                          channel_index)
                         for sideband, bbc, channel_index 
                         in channel_order
                         for bit in ["sign", "mag"][
                                 :int(setup.bits[channel_index])])
        if track_format.startswith("VDIF"):
            # hard-coded 1 thread VDIF section, with astro patching
            datastream_link = "&DS1"
            thread_link = "&thread0"
            datastream = (("datastream", datastream_link, "VDIF"),
                          ("thread", 
                           datastream_link, 
                           thread_link, 
                           0, # thread number
                           setup.nchan,
                           "{:7.3f} Ms/sec".format(setup.samprate),
                           setup.bits[0],
                           "real",
                           8000)) + \
                tuple(("channel",
                       datastream_link,
                       thread_link,
                       freq[channel_index][5], # FREQ link name
                       channel_in_thread)
                       for channel_in_thread, 
                       (_, _, bit, channel_index) 
                       in enumerate(channel for channel in channel_order
                                    if channel[2] == "sign"))
            return None, datastream, None

        elif track_format.startswith("MARK5B"):
            try:
                channel_data = [(setup.sidebd[channel_index],
                                 setup.bbc[channel_index],
                                 bit)
                                for channel_index in range(setup.nchan)
                                for bit in ["sign", "mag"][
                                        :int(setup.bits[channel_index])]]
                input_bitstreams = dbbc_patching.get_input_bitstreams(
                    channel_data)
                # on-disk bitstreams are the compressed input bitstreams
                sorted_bitstreams = sorted(input_bitstreams)
                on_disk_bitstreams = (sorted_bitstreams.index(i)
                                      for i in input_bitstreams)
                bitstream_data = zip(input_bitstreams, 
                                     on_disk_bitstreams,
                                     (d[2] for d in channel_data), # sign/mag
                                     range(setup.nchan))
            except dbbc_patching.NoMatchingPatch:
                bitstream_data = ((bitstream, bitstream, bit, channel_index)
                                  for bitstream, (_, _, bit, channel_index) 
                                  in enumerate(channel_order))
            bitstream = (("stream_sample_rate",
                          "{:7.3f} Ms/sec".format(setup.samprate)),) + \
                        tuple(("stream_def",
                               freq[channel_index][5], # FREQ link name
                               bit,
                               input_bitstream,
                               on_disk_bitstream)
                              for (input_bitstream, on_disk_bitstream, 
                                   bit, channel_index) in bitstream_data)
            return bitstream, None, None

        else:
            assert(setup.tapemode == 1)
            fanout = int(setup.fanout)
            if track_format.startswith("LBA"):
                def track_number(base_track, bit_index, fanout_index):
                    return base_track + bit_index
            else:
                def track_number(base_track, bit_index, fanout_index):
                    return base_track + fanout_index * 2 + \
                        bit_index * 2 * fanout

            track = (("track_frame_format", track_format),
                     ("sample_rate", "{:7.3f} Ms/sec".format(setup.samprate)))
            for channel_index in range(setup.nchan):
                base_track_number = setup.track[0, channel_index]
                if s.schn5.twohead and (base_track_number > 33):
                    base_track_number -= 64
                    headstack = 2
                else:
                    headstack = 1

                track += tuple(itertools.chain(
                    [("fanout_def",
                      "",
                      freq[channel_index][5], # FREQ link name
                      bit,
                      headstack) +
                      tuple(track_number(base_track_number, bit_index, 
                                     fanout_index) 
                            for fanout_index in range(fanout))
                     for bit_index, bit in enumerate(["sign", "mag"][
                             :int(setup.bits[channel_index])])]))
            return None, None, track

    blocks = {"IF", "BBC", "PHASE_CAL_DETECT", "FREQ", 
              "TRACKS", "BITSTREAMS", "DATASTREAMS"}
    # mode is mappings from block type to a 
    # <mapping from block def to stations>
    modes = [{block: defaultdict(set) for block in blocks}]
    # mapping from scan index to index in modes list
    scan_mode_index = {}

    def add_to_mode(mode, scan_mode):
        for block, block_defs in scan_mode.items():
            for block_def, block_stations in block_defs.items():
                mode_stations_in_other_defs = set()
                for mode_block_def, mode_stations in mode[block].items():
                    if (mode_block_def != block_def) and \
                       (block_stations & mode_stations):
                        # this mode has stations for the same block type
                        # in another block def, so it's not compatible
                        return False
        # everything is compatible, add the stations to the current mode
        for block, block_defs in scan_mode.items():
            for block_def, block_stations in block_defs.items():
                mode[block][block_def].update(block_stations)
        return True

    for scan_index, scan in enumerate(scans):
        scan_mode = {block: defaultdict(set) for block in blocks}

        for station_index, station in enumerate(stations):
            setup = setups[s.schn2a.nsetup[scan_index+scan_offset, 
                                           station_index] - 1]
            station_code = station.stcode
            if s.schn2a.stascn[scan_index+scan_offset, station_index]:
                # add pcal to IF def if pcal was specified in the schedule
                frequency_setup_index = s.schn2a.fseti[scan_index+scan_offset,
                                                       station_index]
                if_pcal = ()
                if frequency_setup_index > 0:
                    tone_interval = pcal_map[
                        f2str(s.fsetc.fspcal[frequency_setup_index-1]).upper()]
                    if tone_interval != 0:
                        if_pcal = (tone_interval,)
                else:
                    tone_interval = pcal_map[setup.spcal.upper()]
                
                if_, if_channel = do_if(setup, if_pcal)
                scan_mode["IF"][if_].add(station_code)

                bbc, bbc_channel = do_bbc(setup, if_, if_channel)
                scan_mode["BBC"][bbc].add(station_code)

                phase_cal, freq = do_phasecal_and_freq(
                    setup, scan, tone_interval, bbc, bbc_channel)
                scan_mode["PHASE_CAL_DETECT"][phase_cal].add(station_code)
                scan_mode["FREQ"][freq].add(station_code)
                
                bitstream, datastream, track = do_datastream_type(
                    setup, freq)
                if bitstream is not None:
                    scan_mode["BITSTREAMS"][bitstream].add(station_code)
                if datastream is not None:
                    scan_mode["DATASTREAMS"][datastream].add(station_code)
                if track is not None:
                    scan_mode["TRACKS"][track].add(station_code)
        
        mode_index = next((index for index, mode in enumerate(modes) 
                           if add_to_mode(mode, scan_mode)), 
                          None)
        if mode_index is None:
            # current scan mode doesn't fit any of the current modes
            mode_index = len(modes)
            modes.append(scan_mode)
        scan_mode_index[scan_index] = mode_index

    # functions to name block defs
    def make_if_name(block_def):
        pols = set(if_def[3] for if_def in block_def)
        freq_split = block_def[0][4].split() # "4281.1 MHz" -> ["4281.1", "MHz"]
        return "LO@{}{}{}Pol{}".format(
            round(float(freq_split[0])),
            freq_split[1],
            "D" if len(pols) > 1 else pols.pop(),
            "NoTone" if len(block_def[0]) < 7 else "Tone/{}".format(
                block_def[0][6].split()[0]))

    def make_bbc_name(block_def):
        return "{}BBCs".format(len(block_def))

    def make_phasecal_name(block_def):
        return "NoDetect" if len(block_def[0]) < 3 else "Detect"
    
    def make_freq_name(block_def):
        chan_defs = [param_values for param_values in block_def
                     if param_values[0] == "chan_def"]
        sky_freq_split = chan_defs[0][2].split()
        bandwidth_split = chan_defs[0][4].split()
        return "{}{}{}x{}{}".format(
            sky_freq_split[0], sky_freq_split[1], len(chan_defs),
            round(float(bandwidth_split[0])), bandwidth_split[1])
    
    def make_datastreams_name(block_def):
        param_values = defaultdict(list)
        for d in block_def:
            param_values[d[0]].append(d)
        return "{}_{}-{}Thr{}Ch".format(
            param_values["datastream"][0][2], # VDIF/VDIF_legacy
            param_values["thread"][0][8], # bytes/packet
            len(param_values["thread"]),
            len(param_values["channel"]))

    def make_bitstreams_name(block_def):
        channels = set(param_values[1] for param_values in block_def
                       if param_values[0] == "stream_def")
        bits = set(param_values[2] for param_values in block_def
                   if param_values[0] == "stream_def")
        return "BS{}Ch{}Bit".format(len(channels), len(bits))

    def make_tracks_name(block_def):
        track_format = next(param_values[1] for param_values in block_def
                            if param_values[0] == "track_frame_format")
        fanout = next(len(param_values) - 5 for param_values in block_def
                      if param_values[0] == "fanout_def")
        channels = set(param_values[2] for param_values in block_def
                       if param_values[0] == "fanout_def")
        bits = set(param_values[3] for param_values in block_def
                   if param_values[0] == "fanout_def")
        return "{}.{}Ch{}bit1to{}".format(track_format,
                                          len(channels),
                                          len(bits),
                                          fanout)

    block_name_function = {
        "IF": make_if_name, 
        "BBC": make_bbc_name, 
        "PHASE_CAL_DETECT": make_phasecal_name, 
        "FREQ": make_freq_name, 
        "TRACKS": make_tracks_name, 
        "BITSTREAMS": make_bitstreams_name, 
        "DATASTREAMS": make_datastreams_name}

    # {block -> {def -> name}}
    block_def_name = {block: {} for block in blocks} 

    # {block -> {def -> {mode -> stations}}}
    block_def_mode_stations = defaultdict(lambda: defaultdict(
        lambda: defaultdict(set)))

    # {scan index -> mode}
    scan_mode_name = {}
    # modes
    modes_text = "$MODE;\n"
    for mode_index, mode in enumerate(modes):
        mode_name = "Mode{}".format(mode_index)
        scan_mode_name.update(
            {si: mode_name for si, mi in scan_mode_index.items()
             if mode_index == mi})
        
        modes_text += """
*
def {};
     ref $PROCEDURES = Procedure:{};
""".format(mode_name, ":".join(station.stcode for station in stations))[1:]
        
        for block, block_defs in mode.items():
            for block_def, block_stations in block_defs.items():
                block_def_mode_stations[block][block_def][mode_name].update(
                    block_stations)
                block_name = block_def_name[block].get(block_def)
                if block_name is None:
                    block_name = extend_name(
                        block_name_function[block](block_def),
                        set(block_def_name[block].values()))
                    block_def_name[block][block_def] = block_name
                modes_text += """
     ref ${} = {}{};
""".format(block, block_name, 
           "".join(":" + station for station in block_stations))[1:]
    
        modes_text += "enddef;\n"

    
    # blocks referenced in modes
    for block in blocks:
        block_defs = set()
        for mode in modes:
            block_defs.update(mode[block].keys())
        if len(block_defs) > 0:
            modes_text += block_separator
            modes_text += "${};\n".format(block)
            for block_def in sorted(block_defs, key=lambda block_def: 
                                    block_def_name[block][block_def]):
                modes_text += "*\n{}".format(block_def2str(
                    block_def_name[block][block_def], block_def, 
                    mode_stations=block_def_mode_stations[block][block_def]))
    
    return (modes_text, scan_mode_name)

def procedures_block():
    # just a hardcoded procedure
    return """
$PROCEDURES;
*
def Procedure;
     procedure_name_prefix =  "01";
     tape_change =  420 sec;
     headstack_motion =    6 sec;
     new_source_command =    5 sec;
     new_tape_setup =   20 sec;
     setup_always = on :  20 sec;
     parity_check = off : 100 sec;
     tape_prepass = off : 600 sec;
     preob_cal  = on :  10 sec : preob;
     midob_cal  = on :  15 sec : midob;
     postob_cal = on :   0 sec : postob;
enddef;
"""[1:]

def stations_block():
    def do_site(station):
        # force 2 char site code
        if len(station.stcode) == 0:
            site_id =  ("site_ID", "Xx")
        elif len(station.stcode) == 1:
            site_id = ("site_ID", station.stcode + station.stcode.lower(), 
                       station.stcode)
        else:
            site_id = ("site_ID", station.stcode[:2])
        site = (("site_type", "fixed"),
                ("site_name", str2val(station.station)),
                site_id,
                ("site_position",) + tuple(
                    "{:14.5f} m".format(i) for i in 
                    (station.xpos, station.ypos, station.zpos)),
                ("site_velocity",) + tuple(
                    "{:10.6f} m/yr".format(i) for i in
                    (station.dxpos, station.dypos, station.dzpos)),
                ("site_position_epoch", 
                 "{}y{}d".format(*s.timej(station.mjdrate)[:2])))
        if station.nhoriz > 1:
            site += (("horizon_map_az", "{} deg".format(station.horaz[0])) + \
                     tuple(station.horaz[1:]),
                     ("horizon_map_el", "{} deg".format(station.horel[0])) + \
                     tuple(station.horel[1:]))
        return site

    def do_antenna(station):
        mount_map = {"ALTAZ": ("az", "el"),
                     "EQUAT": ("ha", "dec"),
                     "XYNS": ("x", "yns"),
                     "XYEW": ("x", "yew")}
        axes = mount_map.get(station.mount)
        antenna = tuple()
        if axes is not None:
            antenna += (("axis_type",) + axes,
                        ("antenna_motion", axes[0], 
                         "{:6.1f} deg/min".format(station.ax1rate),
                         "{} sec".format(int(station.tsettle))),
                        ("antenna_motion", axes[1], 
                         "{:6.1f} deg/min".format(station.ax2rate),
                         "{} sec".format(int(station.tsettle))))
            if (station.mount == "ALTAZ") and \
               station.station.startswith("VLBA"):
                antenna += pointing_sectors(station)
            
        antenna += (("axis_offset", "{:10.5} m".format(station.axoff)),)
        return antenna

    def do_das(station, usedisk):
        das = tuple()
        recorder = None
        if usedisk and (station.disk in ("MARK5" + abc for abc in "ABC")):
            recorder = "Mark5" + station.disk[5]
            das += (("equip", "recorder", recorder, "&" + recorder),)
                
        dar_map = {"MKIV": "Mark4",
                   "RDBE": "RDBE_DDC",
                   "RDBE2": "RDBE_PFB", # just guessing about RDBE mapping
                   "VLBAG": "VLBAG4",
                   "NONE": "none",
                   "R1002": "Mark4"}
        dar_map.update({k: k for k in ("VLBA", "VLBA4", "K4", "WIDAR", "LBA",
                                       "DBBC")})
        try:
            electronics = dar_map[station.dar]
            das += (("equip", "rack", electronics, "&" + electronics),)
        except KeyError:
            raise RuntimeError("Unknown DAR of type: {}".format(station.dar))

        return das

    def make_das_name(block_def):
        rack = next(param_values[2] for param_values in block_def
                    if param_values[1] == "rack")
        recorder = next((param_values[2] for param_values in block_def
                         if param_values[1] == "recorder"), None)
        if recorder is None:
            return rack
        else:
            return "{}+{}".format(rack, recorder)

    stations = StationCatalog().read()
    stations = [stations[i-1] for i in s.schn1.stanum[:s.schn1.nsta]]
    
    generator = {"SITE": lambda station, index: do_site(station),
                 "ANTENNA": lambda station, index: do_antenna(station),
                 "DAS": lambda station, index: do_das(station, 
                                                      s.schn5.usedisk[index])}
    block_def_name = {block: {} for block in generator.keys()}
    stations_text = "$STATION;\n"
    for index, station in enumerate(stations):
        stations_text += "*\ndef {};\n".format(station.stcode)
        for block, function in generator.items():
            block_def = function(station, index)
            name = block_def_name[block].get(block_def)
            if name is None:
                if block == "DAS":
                    name = make_das_name(block_def)
                else:
                    # SITE/ANTENNA, just use station name
                    name = station.station
                name = extend_name(name, block_def_name[block])
                block_def_name[block][block_def] = name
            stations_text += "     ref ${} = {};\n".format(block, name)
        stations_text += "enddef;\n"

    for block, def_name in block_def_name.items():
        stations_text += block_separator
        stations_text += "${};\n".format(block)
        for def_, name in def_name.items():
            stations_text += "*\n{}".format(block_def2str(name, def_))
            
    return stations_text

def pointing_sectors(station):
    ax1 = station.ax1lim
    ax2 = station.ax2lim
    diff = ax1[1] - ax1[0]
    if diff <= 360:
        if ax2[1] <= 90.0001:
            # name, lowaz, highaz, lowel, highel
            zones = (("n",  ax1[0], ax1[1], ax2[0], ax2[1]),)
        else:
            zones = (("n",  ax1[0], ax1[1], ax2[0], 90.),
                     ("np", 0.,     0.,     90.,    ax2[1]))
            s.wlog(1, "WRAPZONE: {} has over-the-top pointing.".format(
                station.station) )
            s.wlog(1, "          This is not well supported in SCHED.  "
                   "Az and El may be wrong ")
            s.wlog(1, "          and slew calculations may be way off.")
    elif diff <= 720:
        if ax2[1] <= 90.0001:
            zones = (("ccw", ax1[0], ax1[1] - 360, ax2[0], ax2[1]),
                     ("n",   ax1[1] - 360, ax1[0] + 360, ax2[0], ax2[1]),
                     ("cw",  ax1[0] + 360, ax1[0], ax2[0], ax2[1]))
        else:
            zones = (("ccw",  ax1[0], ax1[1] - 360, ax2[0], 90.),
                     ("n",    ax1[1] - 360, ax1[0] + 360, ax2[0], 90.),
                     ("cw",   ax1[0] + 360, ax1[0], ax2[0], 90.),
                     ("ccwp", ax1[0], ax1[1] - 360, 90., ax2[1]),
                     ("ccwp", ax1[1] - 360, ax1[0] + 360, 90., ax2[1]),
                     ("ccwp", ax1[0] + 360, ax1[0], 90., ax2[1]))
    else: # diff > 720
        s.wlog(1, "WRAPZONE: Station {} has an azimuth wrap range over "
               "180 degrees.".format(station.station))
        s.wlog(1, "          SCHED is not set up for that")
        s.wlog(1, "          Will not give wrap zones in VEX file.")
        
    zone_counter = defaultdict(int)
    def name_suffix(zone):
        zone_counter[zone] += 1
        if zone_counter[zone] == 1:
            return ""
        return "#{}".format(zone_counter[zone])
    return tuple(("pointing_sector", "&" + zone[0] + name_suffix(zone), zone[0],
                  "az", zone[1], zone[2],
                  "el", zone[3], zone[4]) for zone in zones)

def scan_sector(station, scan, az1, el1):
    if (station.mount == "ALTAZ") and \
       (station.ax1lim[1] - station.ax1lim[0] <= 720):
        if (az1 >= station.ax1lim[0]) and (az1 < station.ax1lim[1] - 360):
            zone = "ccw"
        elif (az1 >= station.ax1lim[0] + 360) and (az1 < station.ax1lim[1]):
            zone = "cw"
        else:
            zone = "n"

        if (station.ax2lim[1] > 90) and (el1 > 90):
            zone += "p"
        return zone
    return None
    
def source_block():
    sources = [source for source in SourceCatalog().read() if source.sused]
    source_text = "$SOURCE;\n"
    source_defs = 0
    for source in sources:
        aliases = source.source[np.in1d(source.csused, ["+", "*"])]
        source_defs += len(aliases)
        if source_defs > 1000:
            s.wlog(1, "VXWRSU: WARNING: More than 1000 sources in this "
                   "schedule. This VEX will NOT run on the Field System!")
        for alias in aliases:
            source_text += """
def {};
     source_name = {};
     ra = {}; dec = {}; ref_coord_frame = J2000;
enddef;
""".format(str2def(alias), alias, 
           f2str(s.tformwrp(source.ra2000, "T", 0, 2, 10, "hms")),
           f2str(s.tformwrp(source.d2000,  " ", 1, 2, 9,  "d'\"")))[1:]
            
    return source_text

def sched_block(scan_mode):
    scan_offset = s.schn1.scan1 - 1
    scans = ScanCatalog().read()[scan_offset: s.schn1.scanl]
    
    stations = StationCatalog().read()
    stations = [stations[i-1] for i in s.schn1.stanum[:s.schn1.nsta]]

    setups = SetupCatalog().read()[:s.setn1.nset]

    byte_offset = defaultdict(float) # station -> expected bytes recorded

    ret = "$SCHED;\n"
    for scan_index, scan in enumerate(scans):
        found_one = False
        for station_index, station in enumerate(stations):
            setup = setups[s.schn2a.nsetup[scan_index+scan_offset, 
                                           station_index] - 1]
            if s.schn2a.stascn[scan_index+scan_offset, station_index] and \
               (station.station.startswith("VLBA") or setup.format != "NONE"):
                found_one = True
                break
        if not found_one:
            continue

        scan_def = (("start", time2str(scan.startj)),
                    ("mode", scan_mode[scan_index]),
                    ("source", scan.scnsrc))
        scan_name = "No{:04d}".format(scan_index+scan_offset+1)
        for station_index, station in enumerate(stations):
            if s.schn2a.stascn[scan_index+scan_offset, station_index]:
                setup = setups[s.schn2a.nsetup[scan_index+scan_offset, 
                                               station_index] - 1]
                if (scan.grabto == "FILE") and (scan.datapath == "IN2NET"):
                    s.errlog("VXSCH: You have requested a GRABTO (ftp) scan, "
                             "but you are  not recording to disk. You must set "
                             "DATAPATH=IN2DISK")
                
                if scan.grabto == "NET":
                    s.wlog(1, "VXSCH: You have requested GRABTO=NET, but that "
                           "is not supported in VEX and will be ignored. ")

                elif scan.grabto == "FILE":
                    grab_duration, before_stop = scan.grabtime
                    scan_duration = (scan.stopj - scan.startj) * \
                                    parameter.secpday
                    if grab_duration + before_stop > scan_duration:
                        s.errlog("VXSCH:   WARNING: You have scheduled a "
                                 "GRABTO scan, but the GRABTIME is not "
                                 "consistent with the scan length. Please "
                                 "increase the scan length or change the "
                                 "GRABTIME parameters.")
                    # cast from numpy.float to python float so round returns int
                    grab_stop = round(float(scan_duration-before_stop))
                    grab_start = round(float(grab_stop-grab_duration))
                    scan_def += (("data_transfer", 
                                  station.stcode,
                                  "disk2file",
                                  "{}_{}_{}".format(
                                      f2str(s.schc1.expcode).lower(),
                                      station.stcode.lower(),
                                      scan_name.lower()),
                                  "{} sec".format(grab_start),
                                  "{} sec".format(grab_stop)),)
                    if scan_index < len(scans) - 1:
                        # check that enough time to do the disk2file is 
                        # schedule before the next scan
                        bitrate = setup.totbps
                        # assume a 110Mbps disk2file rate and some latency
                        expected_time = 5 + grab_duration * bitrate / 110
                        gap = (scans[scan_index+1].startj - scan.stopj) * \
                              parameter.secpday
                        if expected_time > gap:
                            s.wlog(1, "VXTRAN: You have scheduled an ftp "
                                   "(GRABTO) scan but you have not left a long "
                                   "enough gap to transfer the data before the "
                                   "next scan starts. Try inserting a gap of "
                                   "{} seconds after the ftp scan (#{})".format(
                                       expected_time + 1, scan_index + 1))
                elif scan.datapath == "IN2NET":
                    # cast from numpy.float to python float so round returns int
                    duration = round(float(scan.stopj - scan.startj) * 
                                     parameter.secpday)
                    scan_def += (("data_transfer",
                                  station.stcode,
                                  "in2net",
                                  "",
                                  "0 sec",
                                  "{} sec".format(duration)),)

                if station.station.startswith("VLBA") or \
                   (setup.format != "NONE"):
                    # cast from numpy.float to python float so round returns int
                    data_good = round(max(
                        float(s.schn6.tonsrc[scan_index+scan_offset, 
                                             station_index] - 
                              scan.startj) * parameter.secpday,
                        0))
                    data_stop = round(float(scan.stopj - scan.startj) * 
                                      parameter.secpday)
                    if s.schn5.usedisk[station_index]:
                        media_position = "{:9.3f} GB".format(
                            byte_offset[station.stcode])
                        byte_offset[station.stcode] = s.schn5.gbytes[
                            scan_index+scan_offset, station_index]
                    else:
                        media_position = ""

                    pointing_sector = ""
                    if station.station.startswith("VLBA"):
                        zone = scan_sector(
                            station, scan,
                            s.schn6.az1[scan_index+scan_offset, station_index],
                            s.schn6.el1[scan_index+scan_offset, station_index])
                        # assumption here is that the zone name is the link name
                        # according to comments in sched this is true in the
                        # normal case, otherwise 3 zones have the same name,
                        # no clue which one would be the one to pick
                        # see Sched/wrapzone.f
                        if zone is not None:
                            pointing_sector = "&" + zone
                        
                    scan_def += (("station",
                                  station.stcode,
                                  "{} sec".format(min(data_good, data_stop)),
                                  "{} sec".format(data_stop),
                                  media_position,
                                  pointing_sector,
                                  0 if scan.norec else 1),)

        ret += "*\n{}".format(
            block_def2str(scan_name, scan_def, keyword="scan"))

    return ret
    
def block_def2str(name, block_def, keyword="def", mode_stations={}):
    ret = """
{} {};
""".format(keyword, name)[1:]

    # add a comment in which mode this is referenced by which stations
    for mode, stations in mode_stations.items():
        ret += """
* {}: {}
""".format(mode, ", ".join(sorted(stations)))[1:]
    
    # align columns for the same param value lines
    param_columns = {param_value[0]: defaultdict(int) 
                     for param_value in block_def}
    for param_value in block_def:
        param = param_value[0]
        for column, value in enumerate(param_value[1:]):
            param_columns[param][column] = \
                max(param_columns[param][column], len(str(value)))
    for param_value in block_def:
        param = param_value[0]
        ret += "     {} = {};\n".format(
            param,
            " : ".join("{:>{width}}".format(value, 
                                            width=param_columns[param][column])
                       for column, value in enumerate(param_value[1:])))
    ret += "end{};\n".format(keyword)
    return ret

def str2def(s):
    s = s.strip()
    s = re.sub("\s", "_", s)
    s = re.sub("[;:=]", "X", s)
    return s[:32]

def str2val(s):
    s = re.sub("[;:=&*$]", "X", s)
    if (len(s) == 0) or re.match(".*\s.*", s):
        # pre-undo all standard C-quoting rules
        s = s.replace('\\', '\\\\')
        s = s.replace('"', '\\"')
        return '"{}"'.format(s)
    return s

def text2comments(lines, comment_prefix):
    # comments may only contain 128 characters, including leading *
    # try to split text on word boundary when required
    ret = ""
    max_comment_length = 128 - len(comment_prefix)
    for line in lines:
        if line:
            if len(line) > max_comment_length:
                index = line[: max_comment_length].rfind(" ")
                if index <  len(line) - max_comment_length:
                    # split in the middle of a word
                    index = max_comment_length
                split_lines = [line[:index], line[index:]]
            else:
                split_lines = [line]
            for split_line in split_lines:
                ret += comment_prefix + split_line + "\n"
    return ret

def time2str(t):
    year, doy, time_ = s.timej(t)
    return "{}y{}d{}".format(year, doy, f2str(
        s.tformwrp(time_, "T", 0, 2, 2, "hms")))

def extend_name(base_name, used_names):
    extension = 2
    base_name = str2def(base_name)
    
    name = base_name
    while name in used_names:
        name = base_name + "#{:02d}".format(extension)
        extension += 1
    return name

