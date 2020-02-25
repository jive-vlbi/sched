from .. import util

import schedlib as s

def getcov(values):
    s.schco.schver = values["version"]
    for attribute in [
            "piname",
            "phone",
            "email",
            "fax",
            "obsphone",
            "obsmode"]:
        setattr(s.schsco, attribute, 
                util.resize_string(values[attribute], 
                                   getattr(s.schsco, attribute).itemsize, 
                                   attribute))
    for i in range(4):
        address = "address" + str(i+1)
        s.schsco.address[i] = util.resize_string(
            values[address], s.schsco.address.itemsize, address)
        note = "note" + str(i+1)
        s.schsco.note[i] = util.resize_string(
            values[note], s.schsco.note.itemsize, note)


    if s.schcon.debug:
        s.wlog(0, "GETCOV: Checking cover information.")

    missing = False
    if s.schco.schver == 0.:
        s.wlog(1, "     Schedule version is missing. ")
        missing = True
    if values["piname"] == "":
        s.wlog(1, "     No PINAME given. ")
        missing = True
    if values["address1"] == "":
        s.wlog(1, "     No address specified.")
        missing = True
    if values["phone"] == "":
        s.wlog(1, "     No PI phone number specified.")
        missing = True
    if (values["email"] == "") and (values["fax"] == ""):
        s.wlog(1, "     No email address or fax number specified.")
        missing = True

    if missing:
        s.wlog(1, "GETCOV:  Cover information incomplete or missing.")
        if not s.schn1.notape and not s.schcon.plot:
            s.errlog("GETCOV: Cover information is required for VLBI "
                     "observations.")
        if s.schcon.plot:
            s.wlog(1, "GETCOV: Sched will plot, but not write telescope "
                   "control files.")
    
    text = [
        "Schedule Version: {:10.2f}".format(float(s.schco.schver)),
        "Processed by SCHED version: {:6.2f}  {}".format(
            float(s.vern.vernum), bytes(s.verc.version).decode()),
        "PI:       {}".format(values["piname"]),
        "Address:  {}".format(values["address1"]),
        "          {}".format(values["address2"]),
        "          {}".format(values["address3"]),
        "          {}".format(values["address4"]),
        "Phone:    {}".format(values["phone"]),
        "EMAIL:    {}".format(values["email"]),
        "Fax:      {}".format(values["fax"]),
        "Phone during observation: {}".format(values["obsphone"]),
        "Observing mode: {}".format(values["obsmode"]),
        "Notes:    {}".format(values["note1"]),
        "          {}".format(values["note2"]),
        "          {}".format(values["note3"]),
        "          {}".format(values["note4"])
    ]
    line = next((line for line in text if line.find("!") != -1), None)
    if line is not None:
        s.wlog(1, "GETCOV: Please do not use exclamation marks in the cover "
               "information.")
        s.wlog(1, "        They mess up the parsing of the CRD files at the "
               "VLBA stations.")
        s.wlog(1, "        One was found in the line: ")
        s.wlog(1, line)
        s.errlog("SCHIN: Remove exclamation marks.")

    for index, line in enumerate(text):
        s.schsco.cover[index] = util.resize_string(
            line, s.schsco.cover.itemsize, "cover")

