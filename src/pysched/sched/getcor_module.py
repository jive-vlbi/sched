from .. import util

import schedlib as s

def getcor(values):
    if s.schcon.debug:
        s.wlog(0, "GETCOR starting.")

    miscor = False

    correl = values["correl"]
    s.schsco.correl = util.resize_string(correl, s.schsco.correl.itemsize, 
                                         "correl")
    if not s.schn1.notape and correl not in ("SOCORRO",
                                             "VLBADIFX",
                                             "VLBA",
                                             "HAYSTACK",
                                             "BONN",
                                             "JIVE",
                                             "WASHINGTON",
                                             "USNO",
                                             "JPL",
                                             "BOLOGNA",
                                             "MITAKA",
                                             "PENTICTON",
                                             "LBA",
                                             "FXCORR",
                                             "ASC",
                                             "OTHER"):
        s.wlog(1, " ** WARNING: {} is not a recognized correlator.".format(
            correl))
        s.wlog(1, "     Recognized correlators are: SOCORRO, VLBADIFX, "
               "VLBA, HAYSTACK, BONN, JIVE,")
        s.wlog(1, "       USNO, JPL, BOLOGNA, MITAKA, PENTICTON, LBA, "
               "FXCORR, ASC, and OTHER")
        miscor = True

    s.schco.coravg = values["coravg"][0]
    s.schco.caexact = (values["coravg"][1].upper() == "EXACT")
    s.schco.corav2 = values["coravg2"][0]
    s.schco.caexact2 = (values["coravg2"][1].upper() == "EXACT")

    s.schco.corchan = values["corchan"][0]
    s.schco.corfft = values["corchan"][1] if values["corchan"][1] != 0 \
                     else max(128, s.schco.corchan)
    s.schco.cornant = values["cornant"]
    s.schco.corpol = (values["corpol"] == "ON")
    
    s.schsco.corwtfn = util.resize_string(values["corwtfn"], 
                                          s.schsco.corwtfn.itemsize, "corwtfn")
    s.schsco.cortape = util.resize_string(values["cortape"], 
                                          s.schsco.cortape.itemsize, "cortape")
    s.schsco.cordfmt = util.resize_string(values["cordfmt"], 
                                          s.schsco.cordfmt.itemsize, "cordfmt")
    s.schsco.corsrcs = util.resize_string(values["corsrcs"], 
                                          s.schsco.corsrcs.itemsize, "corsrcs")
    for i in range(4):
        corship = "corship" + str(i+1)
        s.schsco.corship[i] = util.resize_string(
            values[corship], s.schsco.corship.itemsize, corship)
        cornote = "cornote" + str(i+1)
        s.schsco.cornote[i] = util.resize_string(
            values[cornote], s.schsco.cornote.itemsize, cornote)

    if not s.schn1.notape:
        s.chkcor()
        
        if correl in ("SOCORRO",
                      "VLBADIFX",
                      "VLBA",
                      "FXCORR"):
            s.socdef(values["corpol"])
        else:
            s.cordef(values["corpol"])
    
    if correl in ("SOCORRO",
                  "VLBADIFX",
                  "VLBA"):
        correlator_text = ("  Correlator (Required if recording):          {}"
                           "    (VLBA DiFX software correlator)".format(correl))
    elif miscor:
        correlator_text = ("  Correlator (Required if recording):          {}"
                           "    (Unknown correlator)".format(correl))
    else:
        correlator_text = ("  Correlator (Required if recording):          {}".\
                           format(correl))
    
    if len(values["corsrcs"]) <= 13:
        sources_text1 = "  Source positions from:             {}".format(
            values["corsrcs"])
        sources_text2 = ""
    else:
        sources_text1 = "  Source positions from:"
        sources_text2 = "  {}".format(values["corsrcs"])

    for index, line in enumerate((
            "CORRELATION REQUESTS (Defaults in parentheses): ",
            correlator_text,
            "  Correlator average time (2 sec):            {:8.3f} sec.".format(
                float(s.schco.coravg)),
            "    Alternate average time (for spacecraft).  {:8.3f} sec.".format(
                float(s.schco.corav2)),
            "  Output spectral channels per baseband (16):    {:8d}".format(
                int(s.schco.corchan)),
            "     Correlator FFT size (128):                  {:8d}".format(
                int(s.schco.corfft)),
            "  Number of antennas to be correlated:           {:8d}".format(
                int(s.schco.cornant)),
            "  Polarization (ON):                           {}".format(
                values["corpol"]),
            "  Correlator weighting function (UNIFORM):     {}".format(
                values["corwtfn"]),
            "  Distribution tape (DAT):                     {}".format(
                values["cortape"]),
            "  Distribution format (FITS):                  {}".format(
                values["cordfmt"]),
            sources_text1,
            sources_text2,
            "  Shipping address for correlator output: ",
            "      {}".format(values["corship1"]),
            "      {}".format(values["corship2"]),
            "      {}".format(values["corship3"]),
            "      {}".format(values["corship4"]),
            "  Correlator Notes: {}".format(values["cornote1"]),
            "      {}".format(values["cornote2"]),
            "      {}".format(values["cornote3"]),
            "      {}".format(values["cornote4"]),
            " "
    )):
        s.schsco.corstuff[index] = util.resize_string(
            line, s.schsco.corstuff.itemsize, "corstuff")
