C$Procedure      TIMOUT ( Time Output )
 
      SUBROUTINE  TIMOUT ( ET, PICTUR, OUTPUT )
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine converts an input epoch represented in TDB seconds
C     past the TDB epoch of J2000 to a character string formatted to
C     the specifications of a user's format picture.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     TIME
C
C$ Keywords
C
C     TIME
C
C$ Declarations
 
 
      DOUBLE PRECISION      ET
      CHARACTER*(*)         PICTUR
      CHARACTER*(*)         OUTPUT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   An epoch in seconds past the ephemeris epoch J2000
C     PICTUR     I   A format specification for the output string.
C     STRING     O   A string representation of the input epoch.
C
C$ Detailed_Input
C
C     ET         a double precision representation of time in seconds
C                past the ephemeris epoch J2000.
C
C     PICTUR     is a string that specifies how the output should be
C                presented.  The string is made up of various markers
C                that stand for various components associated with
C                a time.
C
C                There are five types of markers that may appear in a
C                format picture.  String Markers, Numeric Markers,
C                Meta markers, Modifier Markers and Literal Markers.
C
C                The PICTUR string is examined and the various markers
C                are identified. The output time string is constructed
C                by replacing each of the identified markers with
C                an appropriate time component.
C
C                The various markers and their meanings are discussed
C                in the Particulars section below.
C
C                Note that leading and trailing blanks in PICTUR are
C                ignored.
C
C
C$ Detailed_Output
C
C     OUTPUT     is a string matching the format of the input string.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     No exceptions are detected by this routine.  However, the user's
C     processing environment must be properly initialized by loading
C     a leapseconds kernel via the routine FURNSH before calling this
C     routine.  If a leapsecond kernel has not been loaded, an error
C     will be signalled by a routines called by TIMOUT.
C
C$ Files
C
C     A leapseconds kernel must be "loaded" via the routine FURNSH
C     prior to calling TIMOUT.
C
C$ Particulars
C
C
C     A format picture is simply a string of letters that lets
C     TIMOUT know where various components of a time representation
C     should be placed during creation of the time string.
C     Here's an example of such a picture:
C
C        MON DD,YYYY  HR:MN:SC.#### (TDB) ::TDB
C
C     Here is a sample of times that would be created by using this
C     format.
C
C        JAN 12,1992  12:28:18.2772 (TDB)
C        FEB 13,1994  23:18:25.2882 (TDB)
C        AUG 21,1995  00:02:00.1881 (TDB)
C
C     As you can see from the samples above, the format picture
C     specifies that every time string created should begin with a
C     three-letter abbreviation for the month, followed by a space and
C     the day of the month. The day of month is followed immediately
C     by a comma and the year. The year component is followed by two
C     spaces. The next outputs are hours represented as a two digit
C     integer, a colon, minutes represented as a two digit integer,
C     another colon, and seconds rounded to 4 decimal places and
C     having a two digit integer part. This is followed by a space and
C     the string (TDB). The special marker `::TDB' in the time picture
C     is an ``invisible'' marker. It is used to specify the time
C     system that should be used in creating the time string (in this
C     case Barycentric Dynamical Time).
C
C     TIMOUT does not recognize all of the parts of the time format
C     picture in the example above. The list of recognized parts and
C     unrecognized parts is shown in the table below.
C
C       Recognized       Unrecognized
C       ----------       ------------
C       'MON'            ' '
C       'DD'             ','
C       'YYYY'           '  '
C       'HR'             ':'
C       'MN'             '(TDB)'
C       'SC'
C       '.####'
C       '::TDB'
C
C     The unrecognized parts are called literal markers.  They are
C     copied exactly as they appear in PICTUR into the output string.
C     The recognized parts of the picture are replaced by a
C     component of time or, as in the case of `::TDB' are used
C     as instructions about the overall properties of the time
C     string.
C
C     The full list of recognized markers, their classification
C     and meaning are given below.
C
C     MARKER       CLASS     MEANING
C     -----------  --------  -----------------------------------------
C     '.##...'     modifier  represents a numeric component that
C                            immediately precedes this in a decimal
C                            format.  Number of decimal places
C                            equals the number of '#'  characters
C     '::GCAL'     meta      dates are reported in Gregorian calendar
C     '::JCAL'     meta      dates are reported in Julian calendar
C     '::MCAL'     meta      dates after 15 October, 1582 are reported
C                            in Gregorian calendar; before that
C                            dates are reported in Julian calendar
C
C     '::RND'      meta      round output to places specified by
C                            least significant component
C
C     '::TDB'      meta      all components should be TDB
C
C     '::TDT'      meta      all components should be TDT
C
C     '::TRNC'     meta      truncate all output components (default)
C     '::UTC'      meta      all components should be UTC (default)
C     '::UTC+h:m'  meta      all components in UTC offset by +h (hours)
C                            and +m (minutes) so as to allow time zones.
C     '::UTC-h:m'  meta      all components in UTC offset by -h (hours)
C                            and -m (minutes) so as to allow time zones.
C     'AMPM'       string    String (either 'A.M.'  or 'P.M.')
C                            indicating whether hours are before
C                            or after noon.
C     'ampm'       string    String (either 'a.m.'  or 'p.m.')
C                            indicating whether hours are before
C                            or after noon.
C     'AP'         numeric   AM/PM equivalents of the hour component
C                            of a time.
C     'DD'         numeric   Day of month
C     'DOY'        numeric   Day of year
C     'ERA'        string    String (either 'B.C.'  or 'A.D.') giving
C                            era associated with an epoch.
C     '?ERA?'      string    String: either ' B.C. ' or ' A.D. ' if the
C                            year is before 1000 A.D.  otherwise a
C                            blank: ' '.
C     'era'        string    String (either 'b.c.'  or 'a.d.') giving
C                            era associated with an epoch.
C     '?era?'        string   String: either ' b.c. ' or ' a.d. ' if the
C                            year is before 1000 A.D. otherwise a
C                            blank: ' '.
C     'HR'         numeric   hour component of time
C     'JULIAND'    numeric   Julian date component of time
C     'MM'         numeric   numeric representation of month component
C     'MN'         numeric   minute component of time
C     'MON'        string    upper case three letter abbreviation for
C                            month
C     'Mon'        string    capitalized three letter abbreviation for
C                            month
C     'mon'        string    lower case three letter abbreviation for
C                            month
C     'MONTH'      string    upper case full name of month
C     'Month'      string    capitalized full name of month
C     'month'      string    lower case full name of month
C     'SC'         numeric   seconds component of time
C     'SP1950'     numeric   seconds past 1950 component of time
C     'SP2000'     numeric   seconds past 2000 component of time
C     'YR'         numeric   last two digits of year component of time
C     'YYYY'       numeric   year component of time
C     'WEEKDAY'    string    upper case day of week
C     'Weekday'    string    capitalized day of week
C     'weekday'    string    lower case day of week
C     'WKD'        string    upper case three letter abbreviation for
C                            day of week.
C     'Wkd'        string    capitalized three letter abbreviation for
C                            day of week.
C     'wkd'        string    lower case three letter abbreviation for
C                            day of week.
C
C     String Markers
C
C        String markers are portions of the format picture that will
C        be replaced with a character string that represents the
C        corresponding component of a time.
C
C     Numeric Markers
C
C        Numeric markers are portions of the format picture that will
C        be replaced with a decimal string that represents the
C        corresponding component of a time.
C
C     Meta Markers
C
C        Meta markers (listed under the class ``meta'' in the
C        table above) are used to indicate `global' properties of
C        your time string. You may specify time scale and how
C        rounding should be performed on the components of time
C        in your output string. Meta markers may be placed anywhere
C        in your format picture. They do not contribute to placement
C        of characters in output time strings. Also there are no
C        restrictions on how many meta markers you may place in
C        the format picture. However, if you supply conflicting
C        `meta' markers (for example ::TDT and ::TDB) in your
C        picture the first marker listed (in left to right order)
C        overrules the conflicting marker that appears later in
C        the picture.
C
C     Default Meta Markers
C
C        If you do not specify a time system, calendar, or time
C        zone through the use of a Meta Marker, TIMOUT uses the
C        values returned by the SPICE routine TIMDEF. The default
C        time system, calendar returned by TIMDEF are UTC and
C        the Gregorian calendar.  The default time zone returned
C        by TIMDEF is a blank indicating that no time zone offset
C        should be used.
C
C        See the header for the routine TIMDEF for a more complete
C        discussion of setting and retrieving default values.
C
C     Modifier Markers
C
C        The numeric markers listed in the table above stand
C        for integers unless they are modified through use of a
C        modifier marker. The strings
C
C           .#
C           .##
C           .###
C           .####
C
C        are used to this end. When a numeric marker is followed
C        immediately by one of these modifiers, the corresponding time
C        component will be written with the number of decimal places
C        indicated by the number of successive occurrences of the
C        character '#'. Any numeric token may be modified.
C
C     Rounding vs. Truncation
C
C        The meta markers ::TRNC and ::RND allow you to control
C        how the output time picture is rounded. If you specify
C        ::TRNC all components of time are simply truncated to
C        the precision specified by the marker and any modifier.
C        If you specify ::RND the output time is rounded to the
C        least significant component of the format picture. The
C        default action is truncation.
C
C        Whether an output time string should be rounded or
C        truncated depends upon what you plan to do with the
C        string. For example suppose you simply want to get the
C        calendar date associated with a time and not the time of
C        day. Then you probably do not want to round your output.
C        Rounding 1992 Dec 31, 13:12:00 to the nearest day
C        produces 1993 Jan 1. Thus in this case rounding is probably
C        not appropriate.
C
C        However, if you are producing output for plotting using
C        Julian Date, seconds past 1950 or seconds past 2000, you will
C        probably want your output rounded so as to produce a smoother
C        plot.
C
C     Time Systems
C
C        TIMOUT can produce output strings for epochs relative to
C        any of the three systems UTC, TDT, or TDB.  If you do not
C        explicitly specify a time system, TIMOUT will produce strings
C        relative to the time system returned by the SPICE routine
C        TIMDEF.  Unless you call TIMDEF and change it, the default time
C        system is UTC.  However, by using one of the Meta Markers
C        ::UTC, ::TDT, or ::TDB you may specify that TIMOUT produce
C        time strings relative to the UTC, TDT, or TDB system
C        respectively.
C
C     Time Zones
C
C        The meta markers ::UTC+h:m  and ::UTC-h:m  allow you to
C        offset UTC times so that you may represent times in a time
C        zone other than GMT.  For example you can output times in
C        Pacific Standard time by placing the meta-marker ::UTC-8 in
C        your format picture.
C
C        For instance, if you use the picture
C
C           YYYY Mon DD, HR:MN:SC ::UTC
C
C        you will get output strings such as:
C
C           1995 Jan 03, 12:00:00
C
C        If you use the picture
C
C
C           YYYY Mon DD, HR:MN:SC ::UTC-8
C
C        you will get output strings such as:
C
C           1995 Jan 03, 04:00:00
C
C        Finally, if you use the picture
C
C           YYYY Mon DD, HR:MN:SC ::UTC-8:15
C
C        you will get output string
C
C           1995 Jan 03, 03:45:00
C
C        Note that the minutes are always added or subtracted based on
C        the sign present in the time zone specifier. In the case of
C        ::UTC+h:m, minutes are added. In the case ::UTC-h:m, minutes
C        are subtracted.
C
C        The unsigned part of the hours component can be no more than
C        12.  The unsigned part of the minutes component can be no
C        more than 59.
C
C     Calendars
C
C        The calendar currently used by western countries is the
C        Gregorian calendar.  This calendar begins on Oct 15, 1582.
C        Prior to Gregorian calendar the Julian calendar was used. The
C        last Julian calendar date prior to the beginning of the
C        Gregorian calendar is Oct 5, 1582.
C
C        The primary difference between the Julian and Gregorian
C        calendars is in the determination of leap years. Nevertheless,
C        both can be formally extended backward and forward in time
C        indefinitely.
C
C        By default TIMOUT uses the default calendar returned by
C        TIMDEF. Under most circumstances this will be the Gregorian
C        calendar (::GCAL).  However you may specify that TIMOUT use a
C        specific calendar through use of one of the calendar Meta
C        Markers. You may specify that TIMOUT use the Julian calendar
C        (::JCAL), the Gregorian calendar (::GCAL)  or a mixture of
C        both (::MCAL).
C
C        If you specify ::MCAL, epochs that occur after the beginning
C        of the Gregorian calendar will be represented using the
C        Gregorian calendar, and epochs prior to the beginning of the
C        Gregorian calendar will be represented using the Julian
C        calendar.
C
C     Getting Software to Construct Pictures for You.
C
C        Although it is not difficult to construct time format
C        pictures, you do need to be aware of the various markers that
C        may appear in a format picture.
C
C        There is an alternative means for getting a format picture.
C        The routine TPICTR constructs format pictures from a sample
C        time string.  For example, suppose you would like your time
C        strings to look like the basic pattern of the string below.
C
C        'Fri Jul 26 12:22:09 PDT 1996'
C
C        You can call TPICTR with this string, and it will create the
C        appropriate PICTUR for use with TIMOUT.
C
C        CALL TPICTR ( 'Fri Jul 26 12:22:09 PDT 1996', PICTUR, OK )
C
C        The result will be:
C
C        'Wkd Mon DD HR:MN:SC (PDT) ::UTC-7'
C
C        Note: not every date that you can read is interpretable by
C        TPICTR.  For example, you might be able to understand that
C        19960212121116 is Feb 2 1996, 12:11:16.  However, TPICTR
C        cannot recognize this string.  Thus it is important to check
C        the logical output OK to make sure that TPICTR was able to
C        understand the time picture you provided.
C
C        Even thought TPICTR can not recognize every time pattern that
C        has been used by various people, it does recognize nearly all
C        patterns that you use when you want to communicate outside
C        your particular circle of colleagues.
C
C$ Examples
C
C     Example 1.
C     ----------
C
C     Suppose you need to create time strings similar to the
C     default time string produced by the UNIX utility "date"
C     (for example a string of the form "Thu Aug 01 09:47:16 PDT 1996")
C
C     Make the following string assignment.
C
C       PICTUR = 'Wkd Mon DD HH:MN:SC PDT YYYY ::UTC-7'
C
C    (Note the meta marker ::UTC-7 is used to adjust the output
C     time system from UTC to PDT.  Also note that the substring PDT
C     is a literal marker.  Without it, the time system would not
C     appear in the output time string.
C
C     Now for each time ET for which an output time string is required
C     make the call to TIMOUT below, and write the time string.
C
C        CALL TIMOUT ( ET, PICTUR, STRING )
C        WRITE (*,*) STRING
C
C     Alternatively, you can let the routine TPICTR create the TIMOUT
C     time picture for you.
C
C       CALL TPICTR ( 'Thu Aug 01 09:47:16 PDT 1996', PICTUR, OK )
C
C       IF ( OK ) THEN
C
C          CALL TIMOUT ( ET, PICTUR, STRING )
C          WRITE (*,*) STRING
C
C       END IF
C
C
C     Example 2.
C     ----------
C
C     Suppose you want to output a string that contains both the
C     calendar representations of the date as well as the Julian
C     date (for example a string of the form:
C     "Thu Aug 01 09:47:16 PDT 1996 (2450297.1994 JDUTC)" )
C
C     Make the following assignment.
C
C       PICTUR = 'Wkd Mon DD HR:MN ::UTC-7 YYYY (JULIAND.#### JDUTC)'
C
C     Now for each time ET for which an output time string is required
C     make the call to TIMOUT below, and write the time string.
C
C        CALL TIMOUT ( ET, PICTUR, STRING )
C        WRITE (*,*) STRING
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.3.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in RMAIND call.  Replaced header references to LDPOOL with
C        references to FURNSH.
C
C-    Spicelib Version 3.2.0, 09-APR-2002 (WLT)
C
C        Added code to bracket the fractional part of a time component
C        so that it cannot become negative due to inability to invert
C        arthmetic operations with double precision arithmetic.
C
C-    Spicelib Version 3.1.0, 21-JUN-2001 (WLT)
C
C        Added the format picture components ?ERA? and ?era? which
C        vanish for years after 999 A.D.
C
C-    Spicelib Version 3.0.2, 10-APR-2000 (WLT)
C
C        Declared SCAN to be external.
C
C-    Spicelib Version 3.0.1, 22-JUN-1998 (WLT)
C
C        A number of typographical and grammatical errors
C        were corrected in the header.
C
C-    SPICELIB Version 3.0.0, 30-DEC-1997 (WLT)
C
C        The previous version of this routine did not output
C        fractional components for epochs prior to 1 A.D.
C
C        In addition, the default time system, calendar and time zone
C        are obtained from TIMDEF.
C
C-    SPICELIB Version 2.0.0, 1-APR-1997  (WLT)
C
C        In the event that the format picture requested 'YR' as
C        the first component of a time string, the previous edition
C        of this routine used the year value corresponding to the
C        last call to this routine (or whatever happened to be in
C        memory on the first call).  This error has been corrected.
C
C-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT)
C
C-&
 
 
 
C$ Index_Entries
C
C     Convert and format d.p. seconds past J2000 as a string
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 3.3.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in RMAIND call.  Replaced header references to LDPOOL with
C        references to FURNSH.
C
C-    Spicelib Version 3.1.0, 21-JUN-2001 (WLT)
C
C        Added the format picture components ?ERA? and ?era? which
C        vanish for years after 999 A.D.
C
C-    Spicelib Version 3.0.2, 10-APR-2000 (WLT)
C
C        Declared SCAN to be external.
C
C-    Spicelib Version 3.0.1, 22-JUN-1998 (WLT)
C
C        A number of typographical and grammatical errors
C        were corrected in the header.
C
C-    SPICELIB Version 3.0.0, 30-DEC-1997 (WLT)
C
C        The previous version of this routine did not output
C        fractional components for epochs prior to 1 A.D.
C
C        This error was due to overuse of the original year
C        component returned from TTRANS.  The original year
C        component is now saved for use in computing the fractional
C        component.  The modified year (used in printing B.C. epochs)
C        is stored in a separate variable.
C
C-    SPICELIB Version 2.0.0, 1-APR-1997  (WLT)
C
C        In the event that the format picture requested 'YR' as
C        the first component of a time string, the previous edition
C        of this routine used the year value corresponding to the
C        last call to this routine (or whatever happened to be in
C        memory on the first call).  This error has been corrected.
C
C
C        The error was fixed by recoding the following IF THEN statement
C
C              IF (       TYPE .EQ. YEAR
C    .               .OR. TYPE .EQ. MONTH
C    .               .OR. TYPE .EQ. MON
C    .               .OR. TYPE .EQ. DAY
C    .               .OR. TYPE .EQ. DOY
C    .               .OR. TYPE .EQ. NOON
C    .               .OR. TYPE .EQ. HOUR
C    .               .OR. TYPE .EQ. ERA
C    .               .OR. TYPE .EQ. AMPM
C    .               .OR. TYPE .EQ. MINUTE
C    .               .OR. TYPE .EQ. SEC   ) THEN
C
C        as
C
C              IF (       TYPE .EQ. YEAR
C    .               .OR. TYPE .EQ. YR
C    .               .OR. TYPE .EQ. MONTH
C    .               .OR. TYPE .EQ. MON
C    .               .OR. TYPE .EQ. DAY
C    .               .OR. TYPE .EQ. DOY
C    .               .OR. TYPE .EQ. NOON
C    .               .OR. TYPE .EQ. HOUR
C    .               .OR. TYPE .EQ. ERA
C    .               .OR. TYPE .EQ. AMPM
C    .               .OR. TYPE .EQ. MINUTE
C    .               .OR. TYPE .EQ. SEC   ) THEN
C
C
C-    Beta Version 2.1.0, 17-MAR-1994 (MJS) (NJB)
C
C        Integer argument to BRCKTD changed from 0 to 0.0D0.
C
C-&
C
 
 
 
C
C     SPICELIB functions
C
 
      INTEGER               BRCKTI
      INTEGER               BSRCHC
      INTEGER               ISRCHI
      INTEGER               RTRIM
      EXTERNAL              SCAN
 
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      UNITIM
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      J1950
 
      LOGICAL               RETURN
 
C
C     Local parameters
C
C
C     The parameters below act essentially as an enumeration
C     of the various kinds of components we will be looking at in the
C     input time string.
C
      INTEGER               CURRNT
      PARAMETER           ( CURRNT = 1 )
 
      INTEGER               BEGIN
      PARAMETER           ( BEGIN  = CURRNT )
 
      INTEGER               NONAME
      PARAMETER           ( NONAME = CURRNT  + 1 )
 
      INTEGER               YEAR
      PARAMETER           ( YEAR   = NONAME + 1 )
 
      INTEGER               YR
      PARAMETER           ( YR     = YEAR   + 1 )
 
      INTEGER               UMON
      PARAMETER           ( UMON   = YR     + 1 )
 
      INTEGER               MMON
      PARAMETER           ( MMON   = UMON   + 1 )
 
      INTEGER               LMON
      PARAMETER           ( LMON   = MMON   + 1 )
 
      INTEGER               UMNTH
      PARAMETER           ( UMNTH  = LMON   + 1 )
 
      INTEGER               MMNTH
      PARAMETER           ( MMNTH  = UMNTH  + 1 )
 
      INTEGER               LMNTH
      PARAMETER           ( LMNTH  = MMNTH  + 1 )
 
      INTEGER               MONTH
      PARAMETER           ( MONTH  = LMNTH  + 1 )
 
      INTEGER               DOY
      PARAMETER           ( DOY    = MONTH  + 1 )
 
      INTEGER               UWKD
      PARAMETER           ( UWKD   = DOY    + 1 )
 
      INTEGER               MWKD
      PARAMETER           ( MWKD   = UWKD   + 1 )
 
      INTEGER               LWKD
      PARAMETER           ( LWKD   = MWKD   + 1 )
 
      INTEGER               UWEEKD
      PARAMETER           ( UWEEKD = LWKD   + 1 )
 
      INTEGER               MWEEKD
      PARAMETER           ( MWEEKD = UWEEKD + 1 )
 
      INTEGER               LWEEKD
      PARAMETER           ( LWEEKD = MWEEKD + 1 )
 
      INTEGER               DAY
      PARAMETER           ( DAY    = LWEEKD + 1 )
 
      INTEGER               MINUTE
      PARAMETER           ( MINUTE = DAY    + 1 )
 
      INTEGER               HOUR
      PARAMETER           ( HOUR   = MINUTE + 1 )
 
      INTEGER               SEC
      PARAMETER           ( SEC    = HOUR   + 1 )
 
      INTEGER               POINT
      PARAMETER           ( POINT  = SEC    + 1 )
 
      INTEGER               PLACE
      PARAMETER           ( PLACE  = POINT  + 1 )
 
      INTEGER               JULIAN
      PARAMETER           ( JULIAN = PLACE  + 1 )
 
      INTEGER               UTC
      PARAMETER           ( UTC    = JULIAN + 1 )
 
      INTEGER               TDB
      PARAMETER           ( TDB    = UTC    + 1 )
 
      INTEGER               TDT
      PARAMETER           ( TDT    = TDB    + 1 )
 
      INTEGER               SP2000
      PARAMETER           ( SP2000 = TDT    + 1 )
 
      INTEGER               SP1950
      PARAMETER           ( SP1950 = SP2000 + 1 )
 
      INTEGER               ROUND
      PARAMETER           ( ROUND  = SP1950 + 1 )
 
      INTEGER               TRUNC
      PARAMETER           ( TRUNC  = ROUND  + 1 )
 
      INTEGER               UERA
      PARAMETER           ( UERA   = TRUNC  + 1 )
 
      INTEGER               LERA
      PARAMETER           ( LERA   = UERA   + 1 )
 
      INTEGER               UERAX
      PARAMETER           ( UERAX  = LERA + 1 )
 
      INTEGER               LERAX
      PARAMETER           ( LERAX  = UERAX + 1 )
 
      INTEGER               UAMPM
      PARAMETER           ( UAMPM  = LERAX   + 1 )
 
      INTEGER               LAMPM
      PARAMETER           ( LAMPM  = UAMPM  + 1 )
 
      INTEGER               UTCP
      PARAMETER           ( UTCP   = LAMPM  + 1 )
 
      INTEGER               UTCM
      PARAMETER           ( UTCM   = UTCP   + 1 )
 
      INTEGER               JCAL
      PARAMETER           ( JCAL   = UTCM   + 1 )
 
      INTEGER               GCAL
      PARAMETER           ( GCAL   = JCAL   + 1 )
 
      INTEGER               MCAL
      PARAMETER           ( MCAL   = GCAL   + 1 )
 
      INTEGER               TIMSYS
      PARAMETER           ( TIMSYS = MCAL   + 1 )
 
      INTEGER               CALNDR
      PARAMETER           ( CALNDR = TIMSYS + 1 )
 
      INTEGER               AMPM
      PARAMETER           ( AMPM   = CALNDR + 1 )
 
      INTEGER               MON
      PARAMETER           ( MON    = AMPM   + 1 )
 
      INTEGER               WKDAY
      PARAMETER           ( WKDAY  = MON    + 1 )
 
      INTEGER               ERA
      PARAMETER           ( ERA    = WKDAY  + 1 )
 
      INTEGER               NOON
      PARAMETER           ( NOON   = ERA    + 1 )
 
      INTEGER               RLYEAR
      PARAMETER           ( RLYEAR = NOON   + 1 )
 
      INTEGER               FINISH
      PARAMETER           ( FINISH = RLYEAR      )
 
C
C     The following parameters serve as an enumeration of the various
C     time formats that are recognized.
C
      INTEGER               YMD
      PARAMETER           ( YMD    = 1 )
 
      INTEGER               CONTIN
      PARAMETER           ( CONTIN = YMD + 1 )
 
C
C     The parameters below are used to declare the space needed for
C     scanning the input format string.
C
      INTEGER               MAXMRK
      PARAMETER           ( MAXMRK = 42  )
 
      INTEGER               ROOM
      PARAMETER           ( ROOM   = 100 )
 
C
C     The length of the local string that we will use for copying
C     the format string.
C
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 256 )
 
      INTEGER               MRKSIZ
      PARAMETER           ( MRKSIZ = 8 )
 
      INTEGER               LOCLEN
      PARAMETER           ( LOCLEN = 32 )
 
C
C     Local variables
C
      LOGICAL               FIRST
 
C
C     The next set of variables holds the marks and auxiliary
C     arrays used for scanning the format string.
C
      CHARACTER*(MRKSIZ)    MARKS  (     MAXMRK )
      INTEGER               CLASS  ( 0 : MAXMRK )
      INTEGER               NMARKS
      INTEGER               MRKLEN (     MAXMRK )
      INTEGER               PNTRS  (     100    )
 
      INTEGER               DUMP   (     10     )
      INTEGER               NDUMP
      INTEGER               TIMFMT
      INTEGER               TYPE
      INTEGER               NUMTYP
 
C
C     The variables below are used to hold, base formats, values of
C     time vector components, adjustments to use when rounding,
C     the lengths of the format pictures and whether or not various
C     components have already been computed.
C
      CHARACTER*(32)        ORIGNL ( BEGIN : FINISH )
      DOUBLE PRECISION      VALUES ( BEGIN : FINISH )
      DOUBLE PRECISION      PAD    ( BEGIN : FINISH )
      INTEGER               LENGTH ( BEGIN : FINISH )
      INTEGER               ID     ( BEGIN : FINISH )
      LOGICAL               HAVE   ( BEGIN : FINISH )
 
C
C     The array below contains the indexes of the various values
C     associated with the three different times of time vectors
C     that we will be using YMD, YD, CONTIN.
C
      INTEGER               COMPNT ( 8, CONTIN )
 
C
C     We will be making a local copy of the input format string
C     and the input time.
C
      CHARACTER*(MAXLEN)    MYSTR
      CHARACTER*(MAXLEN)    SUBSTR
      CHARACTER*(MAXLEN)    STRING
 
      DOUBLE PRECISION      MYET
 
C
C     The integers below are used to mark substring boundaries.
C
      INTEGER               B
      INTEGER               E
      INTEGER               START
      INTEGER               NTOKNS
      INTEGER               IDENT  (  ROOM   )
      INTEGER               BEG    (  ROOM   )
      INTEGER               END    (  ROOM   )
 
      CHARACTER*(LOCLEN)    FMT
      INTEGER               WIDTH
      INTEGER               APPND
 
C
C     Times come in three flavors: TDT, TDB, UTC.  The one for used
C     on this particular invocation of TIMOUT is stored in TIMTYP.
C     The routine TTRANS needs to have input and output time vector
C     types.  The one used based upon the input PICTUR is stored
C     in BASTYP.
C
      INTEGER               TIMTYP
      CHARACTER*(8)         YMDFMT
      CHARACTER*(8)         YWFMT
      CHARACTER*(16)        BASTYP
      CHARACTER*(16)        INTYP
      CHARACTER*(16)        TSYS
      CHARACTER*(16)        CAL
      CHARACTER*(32)        ZON
 
      DOUBLE PRECISION      HOFF
      DOUBLE PRECISION      MOFF
      INTEGER               CALTYP
      INTEGER               LAST
 
      LOGICAL               OK
      LOGICAL               DOERA
      LOGICAL               DOZONE
 
C
C     Loop counters and delimiters
C
      INTEGER               I
      INTEGER               J
      INTEGER               PART
      INTEGER               STOPAT
      INTEGER               TRNCAT
 
C
C     Utility double precision numbers
C
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      FACTOR
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      INCR
      DOUBLE PRECISION      INTMED
      DOUBLE PRECISION      NTVEC  ( 8 )
      DOUBLE PRECISION      PTVEC  ( 8 )
      DOUBLE PRECISION      TEMPD
      DOUBLE PRECISION      TIMPAD
      DOUBLE PRECISION      TVEC   ( 8 )
      DOUBLE PRECISION      VALUE
 
C
C     The array power is used to assist in the truncation of double
C     precision values.
C
      DOUBLE PRECISION      POWER  ( 0 : 14 )
      DOUBLE PRECISION      X
 
C
C     calendar variables.
C
      INTEGER               JYEAR
      INTEGER               JMONTH
      INTEGER               JDAY
      INTEGER               JDOY
      INTEGER               GYEAR
      INTEGER               GMONTH
      INTEGER               GDAY
      INTEGER               GDOY
      LOGICAL               GO2JUL
 
C
C     Character string representations for months and week days.
C
      CHARACTER*(9)         MONTHS ( 12 )
      CHARACTER*(9)         MYMON
      INTEGER               MYLEN
      INTEGER               MONTYP
      INTEGER               MLEN   ( 12 )
      CHARACTER*(9)         WKDAYS (  7 )
      CHARACTER*(9)         MYWKD
      INTEGER               WKLEN  (  7 )
      INTEGER               WKTYP
 
      INTEGER               INDX
 
      LOGICAL               UNKNWN
      LOGICAL               MAKING
      LOGICAL               PUMPUP
      LOGICAL               VANISH
 
C
C     Save everything.
C
      SAVE
 
 
C
C     Initial values
C
 
      DATA                 MONTHS  / 'January',   'February',
     .                               'March',     'April',
     .                               'May',       'June',
     .                               'July',      'August',
     .                               'September', 'October',
     .                               'November',  'December' /
 
      DATA                 MLEN    /  7,           8,
     .                                5,           5,
     .                                3,           4,
     .                                4,           6,
     .                                9,           7,
     .                                8,           8 /
 
      DATA                 WKDAYS  / 'Sunday',    'Monday',
     .                               'Tuesday',   'Wednesday',
     .                               'Thursday',  'Friday',
     .                               'Saturday' /
 
      DATA                 WKLEN   /  6,           6,
     .                                7,           9,
     .                                8,           6,
     .                                8 /
 
      DATA                 FIRST   / .TRUE. /
 
      DATA                 POWER   / 1.0D0,  1.0D1,  1.0D2,  1.0D3,
     .                               1.0D4,  1.0D5,  1.0D6,  1.0D7,
     .                               1.0D8,  1.0D9,  1.0D10, 1.0D11,
     .                               1.0D12, 1.0D13, 1.0D14         /
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'TIMOUT' )
 
C
C     Chapter 1. Initializations.
C     =================================================================
C
C     On the first pass, we need to set up the recognized tokens
C     that will be used for scanning, the classes of these tokens
C     and the array of ID's for time systems.
C
      IF ( FIRST ) THEN
 
         FIRST    = .FALSE.
 
         MARKS (  1 ) = 'YYYY'
         MARKS (  2 ) = 'YR'
         MARKS (  3 ) = 'MON'
         MARKS (  4 ) = 'Mon'
         MARKS (  5 ) = 'mon'
         MARKS (  6 ) = 'MONTH'
         MARKS (  7 ) = 'Month'
         MARKS (  8 ) = 'month'
         MARKS (  9 ) = 'MM'
         MARKS ( 10 ) = 'DOY'
         MARKS ( 11 ) = 'WKD'
         MARKS ( 12 ) = 'Wkd'
         MARKS ( 13 ) = 'wkd'
         MARKS ( 14 ) = 'WEEKDAY'
         MARKS ( 15 ) = 'Weekday'
         MARKS ( 16 ) = 'weekday'
         MARKS ( 17 ) = 'DD'
         MARKS ( 18 ) = 'MN'
         MARKS ( 19 ) = 'HR'
         MARKS ( 20 ) = 'SC'
         MARKS ( 21 ) = '.#'
         MARKS ( 22 ) = '#'
         MARKS ( 23 ) = 'JULIAND'
         MARKS ( 24 ) = '::UTC'
         MARKS ( 25 ) = '::TDB'
         MARKS ( 26 ) = '::TDT'
         MARKS ( 27 ) = 'SP2000'
         MARKS ( 28 ) = 'SP1950'
         MARKS ( 29 ) = '::RND'
         MARKS ( 30 ) = '::TRNC'
         MARKS ( 31 ) = 'ERA'
         MARKS ( 32 ) = 'era'
         MARKS ( 33 ) = 'AMPM'
         MARKS ( 34 ) = 'ampm'
         MARKS ( 35 ) = '::UTC+'
         MARKS ( 36 ) = '::UTC-'
         MARKS ( 37 ) = '::JCAL'
         MARKS ( 38 ) = '::GCAL'
         MARKS ( 39 ) = '::MCAL'
         MARKS ( 40 ) = 'AP'
         MARKS ( 41 ) = '?ERA?'
         MARKS ( 42 ) = '?era?'
 
         NMARKS       =  42
 
         CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTRS )
 
C
C        Now that we've prepared our recognized substrings and
C        auxiliary arrays for scanning, collect the id's of the
C        various marks and classify the various marks.
C        substrings.
C
         ID ( NONAME ) = 0
         ID ( YEAR   ) = BSRCHC ( 'YYYY',    NMARKS, MARKS )
         ID ( YR     ) = BSRCHC ( 'YR',      NMARKS, MARKS )
         ID ( UMON   ) = BSRCHC ( 'MON',     NMARKS, MARKS )
         ID ( MMON   ) = BSRCHC ( 'Mon',     NMARKS, MARKS )
         ID ( LMON   ) = BSRCHC ( 'mon',     NMARKS, MARKS )
         ID ( UMNTH  ) = BSRCHC ( 'MONTH',   NMARKS, MARKS )
         ID ( MMNTH  ) = BSRCHC ( 'Month',   NMARKS, MARKS )
         ID ( LMNTH  ) = BSRCHC ( 'month',   NMARKS, MARKS )
         ID ( MONTH  ) = BSRCHC ( 'MM',      NMARKS, MARKS )
         ID ( DOY    ) = BSRCHC ( 'DOY',     NMARKS, MARKS )
         ID ( UWKD   ) = BSRCHC ( 'WKD',     NMARKS, MARKS )
         ID ( MWKD   ) = BSRCHC ( 'Wkd',     NMARKS, MARKS )
         ID ( LWKD   ) = BSRCHC ( 'wkd',     NMARKS, MARKS )
         ID ( UWEEKD ) = BSRCHC ( 'WEEKDAY', NMARKS, MARKS )
         ID ( MWEEKD ) = BSRCHC ( 'Weekday', NMARKS, MARKS )
         ID ( LWEEKD ) = BSRCHC ( 'weekday', NMARKS, MARKS )
         ID ( DAY    ) = BSRCHC ( 'DD',      NMARKS, MARKS )
         ID ( MINUTE ) = BSRCHC ( 'MN',      NMARKS, MARKS )
         ID ( HOUR   ) = BSRCHC ( 'HR',      NMARKS, MARKS )
         ID ( SEC    ) = BSRCHC ( 'SC',      NMARKS, MARKS )
         ID ( POINT  ) = BSRCHC ( '.#',      NMARKS, MARKS )
         ID ( PLACE  ) = BSRCHC ( '#',       NMARKS, MARKS )
         ID ( JULIAN ) = BSRCHC ( 'JULIAND', NMARKS, MARKS )
         ID ( UTC    ) = BSRCHC ( '::UTC',   NMARKS, MARKS )
         ID ( TDB    ) = BSRCHC ( '::TDB',   NMARKS, MARKS )
         ID ( TDT    ) = BSRCHC ( '::TDT',   NMARKS, MARKS )
         ID ( SP2000 ) = BSRCHC ( 'SP2000',  NMARKS, MARKS )
         ID ( SP1950 ) = BSRCHC ( 'SP1950',  NMARKS, MARKS )
         ID ( ROUND  ) = BSRCHC ( '::RND',   NMARKS, MARKS )
         ID ( TRUNC  ) = BSRCHC ( '::TRNC',  NMARKS, MARKS )
         ID ( UERA   ) = BSRCHC ( 'ERA',     NMARKS, MARKS )
         ID ( LERA   ) = BSRCHC ( 'era',     NMARKS, MARKS )
         ID ( UERAX  ) = BSRCHC ( '?ERA?',   NMARKS, MARKS )
         ID ( LERAX  ) = BSRCHC ( '?era?',   NMARKS, MARKS )
         ID ( UAMPM  ) = BSRCHC ( 'AMPM',    NMARKS, MARKS )
         ID ( LAMPM  ) = BSRCHC ( 'ampm',    NMARKS, MARKS )
         ID ( UTCP   ) = BSRCHC ( '::UTC+',  NMARKS, MARKS )
         ID ( UTCM   ) = BSRCHC ( '::UTC-',  NMARKS, MARKS )
         ID ( JCAL   ) = BSRCHC ( '::JCAL',  NMARKS, MARKS )
         ID ( GCAL   ) = BSRCHC ( '::GCAL',  NMARKS, MARKS )
         ID ( MCAL   ) = BSRCHC ( '::MCAL',  NMARKS, MARKS )
         ID ( AMPM   ) = BSRCHC ( 'AP',      NMARKS, MARKS )
 
         CLASS ( ID(NONAME)  ) =  NONAME
         CLASS ( ID(YEAR)    ) =  YEAR
         CLASS ( ID(YR)      ) =  YR
         CLASS ( ID(UMON)    ) =  MON
         CLASS ( ID(MMON)    ) =  MON
         CLASS ( ID(LMON)    ) =  MON
         CLASS ( ID(UMNTH)   ) =  MON
         CLASS ( ID(MMNTH)   ) =  MON
         CLASS ( ID(LMNTH)   ) =  MON
         CLASS ( ID(MONTH)   ) =  MONTH
         CLASS ( ID(DOY)     ) =  DOY
         CLASS ( ID(UWKD)    ) =  WKDAY
         CLASS ( ID(MWKD)    ) =  WKDAY
         CLASS ( ID(LWKD)    ) =  WKDAY
         CLASS ( ID(UWEEKD)  ) =  WKDAY
         CLASS ( ID(MWEEKD)  ) =  WKDAY
         CLASS ( ID(LWEEKD)  ) =  WKDAY
         CLASS ( ID(DAY)     ) =  DAY
         CLASS ( ID(MINUTE)  ) =  MINUTE
         CLASS ( ID(HOUR)    ) =  HOUR
         CLASS ( ID(SEC)     ) =  SEC
         CLASS ( ID(POINT)   ) =  POINT
         CLASS ( ID(PLACE)   ) =  PLACE
         CLASS ( ID(JULIAN)  ) =  JULIAN
         CLASS ( ID(UTC)     ) =  TIMSYS
         CLASS ( ID(TDB)     ) =  TIMSYS
         CLASS ( ID(TDT)     ) =  TIMSYS
         CLASS ( ID(SP2000)  ) =  SP2000
         CLASS ( ID(SP1950)  ) =  SP1950
         CLASS ( ID(ROUND)   ) =  ROUND
         CLASS ( ID(TRUNC)   ) =  TRUNC
         CLASS ( ID(UERA)    ) =  ERA
         CLASS ( ID(LERA)    ) =  ERA
         CLASS ( ID(UERAX)   ) =  ERA
         CLASS ( ID(LERAX)   ) =  ERA
         CLASS ( ID(UAMPM)   ) =  NOON
         CLASS ( ID(LAMPM)   ) =  NOON
         CLASS ( ID(UTCP)    ) =  TIMSYS
         CLASS ( ID(UTCM)    ) =  TIMSYS
         CLASS ( ID(JCAL)    ) =  CALNDR
         CLASS ( ID(GCAL)    ) =  CALNDR
         CLASS ( ID(MCAL)    ) =  CALNDR
         CLASS ( ID(AMPM)    ) =  AMPM
 
         DO I = BEGIN, FINISH
            PAD (I) = 0.0D0
         END DO
 
         PAD  ( SEC    )     =   0.5D0
         PAD  ( MINUTE )     =  60.0D0 * PAD(SEC)
         PAD  ( HOUR   )     =  60.0D0 * PAD(MINUTE)
         PAD  ( DAY    )     =  24.0D0 * PAD(HOUR)
         PAD  ( MONTH  )     =  30.0D0 * PAD(DAY)
         PAD  ( DOY    )     =           PAD(DAY)
         PAD  ( MON    )     =           PAD(MONTH)
         PAD  ( YEAR   )     = 365.0D0 * PAD(DAY)
         PAD  ( YR     )     = 365.0D0 * PAD(DAY)
         PAD  ( JULIAN )     =           PAD(DAY)
         PAD  ( SP2000 )     =           PAD(SEC)
         PAD  ( SP1950 )     =           PAD(SEC)
         PAD  ( AMPM   )     =           PAD(HOUR)
 
C
C        After we've made the initial scan for tokens and determined
C        the time system requested, we will want to get rid of the
C        time system tokens.
C
         DUMP (  1 ) = ID(UTC)
         DUMP (  2 ) = ID(TDT)
         DUMP (  3 ) = ID(TDB)
         DUMP (  4 ) = ID(ROUND)
         DUMP (  5 ) = ID(TRUNC)
         DUMP (  6 ) = ID(UTCM)
         DUMP (  7 ) = ID(UTCP)
         DUMP (  8 ) = ID(JCAL)
         DUMP (  9 ) = ID(GCAL)
         DUMP ( 10 ) = ID(MCAL)
 
         NDUMP      = 10
 
C
C        Set up the default formats for the various time components
C
         ORIGNL(YEAR)   = 'YYYY'
         LENGTH(YEAR)   =  4
 
         ORIGNL(YR)     = '0Y'
         LENGTH(YR)     =  2
 
         ORIGNL(DOY)    = '0DD'
         LENGTH(DOY)    =  3
 
         ORIGNL(DAY)    = '0D'
         LENGTH(DAY)    =  2
 
         ORIGNL(MONTH)  = '0M'
         LENGTH(MONTH)  =  2
 
         ORIGNL(HOUR)   = '0H'
         LENGTH(HOUR)   =  2
 
         ORIGNL(AMPM)   = '0H'
         LENGTH(AMPM)   =  2
 
         ORIGNL(MINUTE) = '0M'
         LENGTH(MINUTE) =  2
 
         ORIGNL(SEC)    = '0S'
         LENGTH(SEC)    =  2
 
         ORIGNL(JULIAN) = 'XXXXXXX'
         LENGTH(JULIAN) =  7
 
         ORIGNL(SP2000) = 'XXXXXXXXXXX'
         LENGTH(SP2000) =  11
 
         ORIGNL(SP1950) = 'XXXXXXXXXXX'
         LENGTH(SP1950) =  11
 
C
C        Finally set up the component pointers...
C
         COMPNT(1,YMD)    = RLYEAR
         COMPNT(2,YMD)    = MONTH
         COMPNT(3,YMD)    = DAY
         COMPNT(4,YMD)    = HOUR
         COMPNT(5,YMD)    = MINUTE
         COMPNT(6,YMD)    = SEC
 
         COMPNT(1,CONTIN) = CURRNT
 
      END IF
 
C
C     Chapter 2.  Parsing the input picture.
C     ==============================================================
C
C     First let's copy the input picture into local storage
C     (left justified) and get just past the end of the
C     significant portion (this way the loop that constructs the
C     output string will terminate with no unfinished business
C     left to resolve).
C
      MYSTR = ' '
      CALL    LJUST ( PICTUR, MYSTR(1:MAXLEN-1) )
      E     = RTRIM (         MYSTR             ) + 1
      START = 1
 
C
C     Scan the input string.
C
      CALL SCAN ( MYSTR(1:E),
     .            MARKS,      MRKLEN, PNTRS,  ROOM,  START,
     .            NTOKNS,     IDENT,  BEG,    END           )
 
C
C     Locate the time system that will be used.  This must
C     be one of the following: UTC, TDB, TDT
C
      UNKNWN = .TRUE.
      GO2JUL = .FALSE.
      DOZONE = .FALSE.
      I      = 1
      HOFF   = 0.0D0
      MOFF   = 0.0D0
 
C
C     Get the default time type from TIMDEF
C
      CALL TIMDEF ( 'GET', 'SYSTEM', TSYS )
 
      IF ( TSYS .EQ. 'UTC' ) THEN
         TIMTYP = ID(UTC)
      ELSE IF ( TSYS .EQ. 'TDB' ) THEN
         TIMTYP = ID(TDB)
      ELSE IF ( TSYS .EQ. 'TDT' ) THEN
         TIMTYP = ID(TDT)
      ELSE
         TIMTYP = ID(UTCP)
 
         CALL TIMDEF ( 'GET', 'ZONE', ZON )
 
         CALL PREFIX ( '::', 0, ZON )
         CALL ZZUTCPM( ZON, 1, HOFF, MOFF, LAST, OK )
 
         DOZONE = OK
 
C
C        The routine TIMDEF uses ZZUTCPM to determine whether
C        or not a time zone is legitimate before it stores it
C        to be "GOTTEN."  As a result the value of OK should
C        always be TRUE.  However, just in case TIMDEF should
C        someday use something other that ZZUTCPM for checking
C        we put in the unneeded check below.
C
         IF ( .NOT. OK ) THEN
            TIMTYP = ID(UTC)
         END IF
 
      END IF
 
 
      DO WHILE ( UNKNWN .AND. I .LE. NTOKNS )
 
         IF ( CLASS(IDENT(I)) .EQ. TIMSYS ) THEN
            TIMTYP = IDENT(I)
            UNKNWN = .FALSE.
            DOZONE = .FALSE.
 
            IF(      IDENT(I) .EQ. ID(UTCP)
     .          .OR. IDENT(I) .EQ. ID(UTCM) ) THEN
 
C
C              We've got a time zone specification. Parse it and
C              store the offsets from UTC.
C
               CALL ZZUTCPM ( MYSTR, BEG(I), HOFF, MOFF, LAST, OK )
 
               IF ( OK ) THEN
 
                  DOZONE = .TRUE.
                  TIMTYP = ID(UTCP)
 
C
C                 If we ran all the way up to the end of the next
C                 token, we simply reset the identity of the next
C                 token to be a zone type and increment  I.
C
C                 This way we never see the next token in this loop
C                 and it gets removed later when time systems and
C                 other meta markers from  our copy of the time
C                 format string.
C
                  IF ( LAST .EQ. END(I+1) ) THEN
                     IDENT(I+1) = IDENT(I)
                     I          = I + 1
                  ELSE
                     END(I)     = LAST
                     BEG(I+1)   = LAST + 1
                  END IF
 
               END IF
 
            END IF
 
 
         END IF
 
         I = I + 1
 
      END DO
 
C
C     Determine whether we should use the Julian or gregorian (default)
C     calendar
C
      UNKNWN = .TRUE.
      I      = 1
 
C
C     Get the default calendar from TIMDEF.
C
      CALL TIMDEF ( 'GET', 'CALENDAR', CAL )
 
      IF      ( CAL .EQ. 'GREGORIAN' ) THEN
         CALTYP = ID(GCAL)
      ELSE IF ( CAL .EQ. 'JULIAN'    ) THEN
         CALTYP = ID(JCAL)
      ELSE
         CALTYP = ID(MCAL)
      END IF
 
      DO WHILE ( UNKNWN .AND. I .LE. NTOKNS )
 
         IF ( CLASS(IDENT(I)) .EQ. CALNDR ) THEN
            CALTYP = IDENT(I)
            UNKNWN = .FALSE.
         END IF
 
         I = I + 1
      END DO
 
 
C
C     Next determine whether or not we shall be performing rounding
C     on output.
C
      PUMPUP =      ISRCHI ( ID(ROUND), NTOKNS, IDENT ) .NE. 0
 
C
C     Determine if we have an Era specification
C
      DOERA  =      ISRCHI ( ID(LERA),  NTOKNS, IDENT ) .NE. 0
     .         .OR. ISRCHI ( ID(UERA),  NTOKNS, IDENT ) .NE. 0
     .         .OR. ISRCHI ( ID(UERAX), NTOKNS, IDENT ) .NE. 0
     .         .OR. ISRCHI ( ID(LERAX), NTOKNS, IDENT ) .NE. 0
 
C
C     Until we've examined the year, we assume that the era is not
C     supposed to vanish.
C
      VANISH = .FALSE.
C
C     Next remove all of the time system dudes from the list of
C     tokens.
C
      CALL SCANRJ ( DUMP, NDUMP, NTOKNS, IDENT, BEG, END )
 
 
C
C     If the user wants to round the output, we need to pump up ET
C     by the smallest significant part of the input picture.  But
C     in either case we are going to pad the input time.  For now
C     we pad it by zero.
C
      TIMPAD = 0.0D0
 
      IF ( PUMPUP ) THEN
 
C
C        We need to determine the amount to pad ET by.  So we need
C        to look at the string and find the least significant component
C        that has been requested.  Keep in mind that the last token
C        is of type NONAME (its a blank) by construction.
C
         I = 1
 
         DO WHILE ( I .LE. NTOKNS )
 
            TYPE = CLASS ( IDENT(I) )
 
            IF   (      ( TYPE .EQ. NONAME )
     .             .OR. ( TYPE .EQ. POINT  )
     .             .OR. ( TYPE .EQ. PLACE  )
     .             .OR. ( TYPE .EQ. NOON   )
     .             .OR. ( TYPE .EQ. ERA    )
     .             .OR. ( TYPE .EQ. MON    )
     .             .OR. ( TYPE .EQ. WKDAY  ) ) THEN
 
C
C              Don't do anything, just go on to the next token.
C
               I = I + 1
 
            ELSE
 
C
C              Look up the amount we should pad our time by.
C
               FACTOR = 1.0D0
               INCR   = PAD ( TYPE )
 
C
C              Examine the next token.  If it's not a decimal point
C              and marker, we have the least significant part of
C              this component.
C
               I      = I + 1
               TYPE   = CLASS(IDENT(I))
 
               IF ( TYPE .EQ. POINT ) THEN
 
                  FACTOR = FACTOR * 0.1D0
                  I      = I + 1
 
C
C                 Now just look for the end of the string of place
C                 holders
C
                  DO WHILE ( IDENT(I) .EQ. ID(PLACE) )
                     FACTOR = FACTOR * 0.1D0
                     I      = I + 1
                  END DO
 
               END IF
 
C
C              Now compute the time pad for this component of the
C              time string.
C
               INCR = INCR * FACTOR
 
               IF ( TIMPAD .NE. 0.0D0 ) THEN
                  TIMPAD = MIN ( TIMPAD, INCR )
               ELSE
                  TIMPAD = INCR
               END IF
 
            END IF
 
         END DO
 
      END IF
 
C
C     Right now we don't have any components of the time format
C     and we don't need any of them so far.
C
      DO PART = BEGIN, FINISH
         HAVE(PART) = .FALSE.
      END DO
 
 
C
C     Set up the input time format and the output time format that will
C     be used later.
C
C     The input time format is used to convert the basic ET we have now
C     to one of the various time formats that are supported by the
C     routine TTRANS.  If we are going to construct a string in one of
C     the dynamical time systems we will call the input time a formal
C     time in seconds past a formal calendar epoch of J2000.  If on the
C     other hand we are going to construct a UTC based string, we will
C     convert our ET to an earth based epoch (TDT) and use this as our
C     base input system.
C
C
      MYET = ET
 
      IF      ( TIMTYP .EQ. ID(TDB) ) THEN
 
C
C        Since we are likely to need SP2000, SP1950 or JD, we
C        compute them now.
C
         MYET           = MYET + TIMPAD
         VALUES(SP2000) = MYET
         VALUES(JULIAN) = UNITIM( MYET,   'TDB',  'JDTDB'  )
         VALUES(SP1950) = VALUES(SP2000) + SPD()*( J2000() - J1950() )
 
         BASTYP         = 'FORMAL'
         YMDFMT         = 'YMDF'
         YWFMT          = 'YMWDF'
         HAVE  (SP2000) = .TRUE.
         HAVE  (SP1950) = .TRUE.
         HAVE  (JULIAN) = .TRUE.
 
      ELSE IF ( TIMTYP .EQ. ID(TDT) ) THEN
 
         MYET           = UNITIM( MYET,   'TDB',  'TDT'    )  + TIMPAD
         VALUES(SP2000) = MYET
         VALUES(JULIAN) = UNITIM( MYET,   'TDT',  'JDTDT'  )
         VALUES(SP1950) = VALUES(SP2000) + SPD()*( J2000() - J1950() )
 
         BASTYP         = 'FORMAL'
         YMDFMT         = 'YMDF'
         YWFMT          = 'YMWDF'
         HAVE  (SP2000) = .TRUE.
         HAVE  (SP1950) = .TRUE.
         HAVE  (JULIAN) = .TRUE.
 
      ELSE
 
C
C        In this case we convert to an earth based frame for our
C        working epoch.  This rounds properly when it's time to get
C        fractional components.
C
         MYET   =  UNITIM ( MYET, 'TDB', 'TDT' ) + TIMPAD
         BASTYP = 'TDT'
         YMDFMT = 'YMD'
         YWFMT  = 'YMWD'
 
      END IF
 
C
C     Chapter 3.  Building the Output String
C     ==================================================================
C
 
C
C     Now we are ready to go, we need to fetch the tokens
C     and construct the output string.  We will
C     put the next portion of the output at APPND
C
      APPND  =  1
      MAKING = .FALSE.
 
      DO I = 1, NTOKNS
 
         TYPE    = CLASS( IDENT(I) )
         TVEC(1) = MYET
 
C
C        If the next marker is not one we use as a place holder
C        in the fractional part of decimal formats AND we
C        are in the process of building a format, then the format
C        building is done.  We can construct the component and
C        append it to the string we are building.
C
         IF ( TYPE .NE. PLACE  .AND.  MAKING ) THEN
 
C
C           We also need to be sure this isn't a decimal point
C           marker before we add on to the output string.
C
            IF (  TYPE .NE. POINT .OR. HAVE(POINT) ) THEN
 
C
C              We are going to truncate the number to the number of
C              places requested NOT round.
C
               TRNCAT = BRCKTI( WIDTH - LENGTH(NUMTYP) - 1, 0, 14 )
               FRAC   = VALUE - DINT  (VALUE )
 
               IF ( FRAC .LT. 0 ) THEN
                  VALUE = VALUE - 1.0D0
                  FRAC  = FRAC  + 1.0D0
               END IF
 
               INTMED = ( DINT( FRAC * POWER(TRNCAT) ) - 0.125D0 )
     .                                / POWER(TRNCAT)
 
               FRAC   = BRCKTD( INTMED,      0.0D0,      1.0D0 )
 
               VALUE  = DINT(VALUE) + FRAC
 
               CALL DPFMT ( VALUE, FMT, SUBSTR )
 
 
               STRING(APPND:) = SUBSTR
               APPND          = APPND + WIDTH
               HAVE  (POINT)  =.FALSE.
               MAKING         =.FALSE.
 
            END IF
 
         END IF
 
C
C        If the token isn't recognized we can just
C        append it to the string we are constructing and
C        adjust the point at which the next substring is
C        to be appended.
C
         IF ( TYPE .EQ. NONAME ) THEN
 
            STRING(APPND:) = MYSTR ( BEG(I) : END(I) )
            APPND          = APPND - BEG(I) + END(I) + 1
 
C
C        If the token is a place holder, we either just append it
C        or tack it on to a format string we are creating..
C
         ELSE IF ( TYPE .EQ. PLACE ) THEN
 
 
            IF ( MAKING ) THEN
               B           = WIDTH + 1
               E           = B     - BEG(I) + END(I)
               FMT(B:E)    = MYSTR ( BEG(I) : END(I) )
               WIDTH       = WIDTH - BEG(I) + END(I) + 1
            ELSE
               STRING(APPND:APPND) = MYSTR ( BEG(I) : END(I) )
               APPND               = APPND - BEG(I) + END(I) + 1
            END IF
 
C
C        If the token is the decimal point plus place holder
C        AND we are making a format, we append it to the current
C        format and determine the fractional part of the current
C        quantity.
C
         ELSE IF ( TYPE .EQ. POINT ) THEN
 
            IF ( .NOT. MAKING ) THEN
 
               B           =  APPND
               E           =  APPND - BEG(I) + END(I)
               STRING(B:E) =  MYSTR ( BEG(I) : END(I) )
               APPND       =  E + 1
               HAVE(POINT) = .FALSE.
 
            ELSE IF ( TIMFMT .EQ. CONTIN ) THEN
 
               B           = WIDTH + 1
               E           = B     - BEG(I) + END(I)
               FMT(B:E)    = MYSTR ( BEG(I) : END(I) )
               WIDTH       = E
               HAVE(POINT) =.TRUE.
 
            ELSE
 
               B           = WIDTH + 1
               E           = B     - BEG(I) + END(I)
               FMT(B:E)    = MYSTR ( BEG(I) : END(I) )
               WIDTH       = E
               HAVE(POINT) =.TRUE.
 
C
C              Since we obviously are going to be needing
C              the fractional part of this component we fetch it
C              now and add it to whatever the integer part of the
C              current value is.  Here's how we do this.
C              If we truncated the input time to this component
C              we'd have a value on an "integer" portion of the
C              time scale.
C              .
C              .               current
C              .               time
C              .               truncated    .---MYET
C                                   |       |
C                                   v       v
C              time scale: ---------+-------X-----------+-----
C                                                       ^
C                                                       |
C              .                               truncated time
C              .                               plus 1 in this
C              .                               component
C              .
C              Add one to the truncated component to get the
C              next integer component.  Finally we convert these
C              two constructed stings to seconds so that we can
C              get the "fractional part" of the current component.
C              Fortunately, when we computed the integer value
C              for this component we constructed the time
C              vectors we need, so we don't have to go to a lot
C              of trouble now.
C
 
               CALL TTRANS( INTYP, BASTYP, PTVEC  )
               CALL TTRANS( INTYP, BASTYP, NTVEC  )
 
               DELTA = MAX(1.0D0, NTVEC(1) - PTVEC(1) )
               FRAC  = BRCKTD( 0.0D0, 1.0D0, (MYET - PTVEC(1)) / DELTA )
 
               VALUE = VALUE + FRAC
 
            END IF
 
         ELSE
 
C
C        If we get to this point we have an honest time
C        string component to fetch.  We might already have
C        this guy.  If so we can just collect him from the
C        values buffer (although this collection is performed
C        after the next long IF-THEN block that gets the value
C        if we don't already have it).
C
            MAKING      =.TRUE.
            HAVE(POINT) =.FALSE.
            FMT         = ORIGNL( TYPE )
            WIDTH       = LENGTH( TYPE )
            NUMTYP      =         TYPE
 
            IF ( .NOT. HAVE(TYPE) )  THEN
 
               TVEC(1) = MYET
 
C
C              Most components are handled in the next block.
C
               IF (       TYPE .EQ. YEAR
     .               .OR. TYPE .EQ. YR
     .               .OR. TYPE .EQ. MONTH
     .               .OR. TYPE .EQ. MON
     .               .OR. TYPE .EQ. DAY
     .               .OR. TYPE .EQ. DOY
     .               .OR. TYPE .EQ. NOON
     .               .OR. TYPE .EQ. HOUR
     .               .OR. TYPE .EQ. ERA
     .               .OR. TYPE .EQ. AMPM
     .               .OR. TYPE .EQ. MINUTE
     .               .OR. TYPE .EQ. SEC   ) THEN
 
 
                  CALL TTRANS ( BASTYP, YMDFMT, TVEC )
 
C
C                 The seconds component is finished.  Regardless
C                 of any zone or calendar modifications, we just
C                 don't have to deal with this component any more.
C
                  VALUES(SEC)    = TVEC(6)
 
C
C                 If we need to deal with time zones, this is
C                 the time to do it.
C
                  IF ( TIMTYP .EQ. ID(UTCP) ) THEN
 
                     TVEC(4) = TVEC(4) + HOFF
                     TVEC(5) = TVEC(5) + MOFF
                     TVEC(6) = 0.0D0
 
                     CALL TTRANS ( 'YMDF', 'YMDF', TVEC )
 
                  END IF
 
C
C                 One way or the other the hours and minutes components
C                 are finished.  Record their values.
C
                  VALUES(HOUR)   = TVEC(4)
                  VALUES(MINUTE) = TVEC(5)
 
                  IF ( VALUES(HOUR) .EQ. 0.0D0 ) THEN
                     VALUES(AMPM) =  12.0D0
                  ELSE IF ( VALUES(HOUR) .GT. 12.0D0 ) THEN
                     VALUES(AMPM) = VALUES(HOUR) - 12.0D0
                  ELSE
                     VALUES(AMPM) = VALUES(HOUR)
                  END IF
 
C
C                 Finally, if we need to change the calendar to
C                 Julian this is the place to handle it.
C
                  JYEAR  = NINT( TVEC(1) )
                  JMONTH = NINT( TVEC(2) )
                  JDAY   = NINT( TVEC(3) )
 
                  CALL GR2JUL ( JYEAR, JMONTH, JDAY, JDOY )
 
                  GYEAR  = JYEAR
                  GMONTH = JMONTH
                  GDAY   = JDAY
 
                  CALL JUL2GR ( GYEAR, GMONTH, GDAY, GDOY )
 
                  IF      ( CALTYP .EQ. ID(GCAL) ) THEN
 
                     VALUES(YEAR)  = DBLE(GYEAR)
                     VALUES(MONTH) = DBLE(GMONTH)
                     VALUES(DAY)   = DBLE(GDAY)
                     VALUES(DOY)   = DBLE(GDOY)
                     GO2JUL        = .FALSE.
 
                  ELSE IF ( CALTYP .EQ. ID(JCAL) ) THEN
 
                     VALUES(YEAR)  = DBLE(JYEAR)
                     VALUES(MONTH) = DBLE(JMONTH)
                     VALUES(DAY)   = DBLE(JDAY)
                     VALUES(DOY)   = DBLE(JDOY)
                     GO2JUL        = .TRUE.
 
                  ELSE IF ( CALTYP .EQ. ID(MCAL) ) THEN
 
 
                     IF      ( GYEAR .LT. 1582 ) THEN
                        GO2JUL = .TRUE.
                     ELSE IF ( GYEAR .GT. 1582 ) THEN
                        GO2JUL = .FALSE.
                     ELSE IF ( GMONTH .LT. 10 ) THEN
                        GO2JUL = .TRUE.
                     ELSE IF ( GMONTH .GT. 10 ) THEN
                        GO2JUL = .FALSE.
                     ELSE IF ( GDAY   .GE. 15 ) THEN
                        GO2JUL = .FALSE.
                     ELSE
                        GO2JUL = .TRUE.
                     END IF
 
                     IF ( GO2JUL ) THEN
                        VALUES(YEAR)  = DBLE(JYEAR)
                        VALUES(MONTH) = DBLE(JMONTH)
                        VALUES(DAY)   = DBLE(JDAY)
                        VALUES(DOY)   = DBLE(JDOY)
                     ELSE
                        VALUES(YEAR)  = DBLE(GYEAR)
                        VALUES(MONTH) = DBLE(GMONTH)
                        VALUES(DAY)   = DBLE(GDAY)
                        VALUES(DOY)   = DBLE(GDOY)
                     END IF
 
                  END IF
 
C
C                 Determine the era associated with the epoch.  Also
C                 if the year component is negative, we handle  that
C                 now.
C
C                 We store the actual value of the year so that
C                 it can be used when determining rounding of
C                 other components.
C
                  VALUES(RLYEAR) = VALUES(YEAR)
 
                  IF ( DOERA ) THEN
 
                     IF ( VALUES(YEAR) .LT. 1 ) THEN
                        VALUES(YEAR) = 1.0D0 - VALUES(YEAR)
                        VALUES(ERA)  = 1.0D0
                     ELSE
                        VALUES(ERA)  = 2.0D0
                     END IF
 
                     VANISH = VALUES(YEAR) .GE. 1000.0D0
 
                  END IF
 
C
C                 Fetch the last two digits of the year.
C
                  CALL RMAIND ( VALUES(YEAR), 100.0D0, X, TEMPD )
                  VALUES(YR) = TEMPD

                  HAVE  (YEAR)   = .TRUE.
                  HAVE  (YR)     = .TRUE.
                  HAVE  (DOY)    = .TRUE.
                  HAVE  (MONTH)  = .TRUE.
                  HAVE  (MON)    = .TRUE.
                  HAVE  (DAY)    = .TRUE.
                  HAVE  (HOUR)   = .TRUE.
                  HAVE  (MINUTE) = .TRUE.
                  HAVE  (SEC)    = .TRUE.
                  HAVE  (AMPM)   = .TRUE.
                  HAVE  (ERA)    = .TRUE.
 
 
               ELSE IF ( TYPE .EQ. WKDAY ) THEN
 
                  TVEC(1) = MYET
 
                  CALL TTRANS ( BASTYP, YWFMT, TVEC )
 
C
C                 If we need to deal with time zones, this is
C                 the time to do it.
C
 
                  IF ( TIMTYP .EQ. ID(UTCP) ) THEN
 
                     TVEC(5) = TVEC(5) + HOFF
                     TVEC(6) = TVEC(6) + MOFF
                     TVEC(7) = 0.0D0
 
                     CALL TTRANS ( 'YMWDF', 'YMWDF', TVEC )
 
 
                  END IF
 
                  VALUES(WKDAY)  = TVEC(4)
                  HAVE  (WKDAY)  =.TRUE.
 
               ELSE IF (      ( TYPE .EQ. SP1950 )
     .                   .OR. ( TYPE .EQ. SP2000 ) ) THEN
 
C
C                 The only way to get here is if the output time
C                 type is UTC or a time zone (otherwise we'd
C                 already HAVE SP2000 and SP1950).
C
                  TVEC(1) = MYET
 
                  CALL TTRANS ( BASTYP, 'FORMAL', TVEC )
 
                  VALUES(SP2000) = TVEC(1)
                  VALUES(SP1950) = VALUES(SP2000)
     .                           + SPD() * ( J2000() - J1950() )
 
                  HAVE  (SP2000) = .TRUE.
                  HAVE  (SP1950) = .TRUE.
 
               ELSE IF ( TYPE .EQ. JULIAN ) THEN
 
C
C                 The same tale can be told here as in the last
C                 case.  We can only get here if this is UTC
C                 output.
C
                  TVEC(1) = MYET
 
                  CALL TTRANS ( BASTYP, 'JDUTC', TVEC )
 
                  VALUES(JULIAN) = TVEC(1)
                  HAVE  (JULIAN) =.TRUE.
 
               END IF
 
 
            END IF
 
C
C           O.K. whatever thing we are about to construct, we now
C           have it's numeric value.  It's time to construct its
C           string  value.
C
C
C           We need to treat character months, weekdays, eras, a.m.'s
C           and p.m.'s specially.
C
            IF ( TYPE .EQ. MON ) THEN
 
               INDX           = NINT  ( VALUES(MONTH) )
               MYMON          = MONTHS( INDX )
               MONTYP         = IDENT ( I )
 
C
C              There is no ELSE case in the block below because all of
C              the possible MONTYP values are checked explicitly.
C
               IF      ( MONTYP .EQ. ID(UMON)  ) THEN
                  CALL UCASE ( MYMON, MYMON )
                  MYMON(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( MONTYP .EQ. ID(MMON)   ) THEN
                  MYMON(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( MONTYP .EQ. ID(LMON)   ) THEN
                  CALL LCASE ( MYMON, MYMON )
                  MYMON(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( MONTYP .EQ. ID(MMNTH) ) THEN
                  MYLEN     = MLEN(INDX)
               ELSE IF ( MONTYP .EQ. ID(UMNTH) ) THEN
                  CALL UCASE ( MYMON, MYMON )
                  MYLEN     = MLEN(INDX)
               ELSE IF ( MONTYP .EQ. ID(LMNTH) ) THEN
                  CALL LCASE ( MYMON, MYMON )
                  MYLEN     = MLEN(INDX)
               END IF
 
               STRING(APPND:) = MYMON
               APPND          = APPND + MYLEN
               MAKING         =.FALSE.
 
            ELSE IF ( TYPE .EQ. WKDAY ) THEN
 
               INDX           = NINT  ( VALUES(WKDAY) )
               MYWKD          = WKDAYS( INDX )
               WKTYP          = IDENT ( I )
 
C
C              There is no ELSE case in the block below because all of
C              the possible WKTYP values are checked explicitly.
C
               IF      ( WKTYP .EQ. ID(UWKD)   ) THEN
                  CALL UCASE ( MYWKD, MYWKD )
                  MYWKD(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( WKTYP .EQ. ID(MWKD)   ) THEN
                  MYWKD(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( WKTYP .EQ. ID(LWKD)   ) THEN
                  CALL LCASE ( MYWKD, MYWKD )
                  MYWKD(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( WKTYP .EQ. ID(MWEEKD) ) THEN
                  MYLEN     = WKLEN(INDX)
               ELSE IF ( WKTYP .EQ. ID(UWEEKD) ) THEN
                  CALL UCASE ( MYWKD, MYWKD )
                  MYLEN     = WKLEN(INDX)
               ELSE IF ( WKTYP .EQ. ID(LWEEKD) ) THEN
                  CALL LCASE ( MYWKD, MYWKD )
                  MYLEN     = WKLEN(INDX)
               END IF
 
               STRING(APPND:) = MYWKD
               APPND          = APPND + MYLEN
               MAKING         =.FALSE.
 
 
            ELSE IF ( TYPE .EQ. ERA ) THEN
 
               IF      (         VALUES(ERA)    .EQ. 2.0D0
     .                   .AND. (      IDENT(I) .EQ. ID(UERA)
     .                           .OR. IDENT(I) .EQ. ID(UERAX) ) ) THEN
 
                  STRING(APPND:) = ' A.D.'
 
               ELSE IF (         VALUES(ERA)   .EQ. 2.0D0
     .                   .AND. (      IDENT(I) .EQ. ID(LERA)
     .                           .OR. IDENT(I) .EQ. ID(LERAX) ) ) THEN
                   STRING(APPND:) = ' a.d.'
 
               ELSE IF  (         VALUES(ERA)  .EQ. 1.0D0
     .                   .AND. (      IDENT(I) .EQ. ID(UERA)
     .                           .OR. IDENT(I) .EQ. ID(UERAX) ) ) THEN
 
                  STRING(APPND:) = ' B.C.'
 
               ELSE
 
                  STRING(APPND:) = ' b.c.'
 
               END IF
C
C              If we have the vanishing kind of era, and we've
C              determined that it needs to vanish, then blank out the
C              portion of the string we just filled in. and don't
C              increment the place holder.
C
               IF (     IDENT(I) .EQ. ID(UERAX)
     .             .OR. IDENT(I) .EQ. ID(LERAX)) THEN
 
                  IF ( VANISH ) THEN
                     STRING(APPND:) = ' '
                     APPND = APPND + 1
                  ELSE
                     APPND = APPND + 6
                  END IF
 
               ELSE
                  CALL LJUST ( STRING(APPND:), STRING(APPND:) )
                  APPND          = APPND + 4
               END IF
 
               MAKING         =.FALSE.
 
            ELSE IF ( TYPE .EQ. NOON ) THEN
 
 
               IF      (       IDENT(I)   .EQ. ID(UAMPM)
     .                   .AND. VALUES(HOUR) .GE. 12.0D0    ) THEN
 
                  STRING(APPND:) = 'P.M.'
 
               ELSE IF (       IDENT(I)    .EQ. ID(UAMPM)
     .                   .AND. VALUES(HOUR) .LT. 12.0D0    ) THEN
 
                  STRING(APPND:) = 'A.M.'
 
               ELSE IF (       IDENT(I)    .EQ. ID(LAMPM)
     .                   .AND. VALUES(HOUR) .GE. 12.0D0    ) THEN
 
                  STRING(APPND:) = 'p.m.'
 
               ELSE
 
                  STRING(APPND:) = 'a.m.'
 
               END IF
 
               APPND          = APPND + 4
               MAKING         =.FALSE.
 
            ELSE
 
               VALUE = VALUES( TYPE )
 
            END IF
 
C
C           If we are now creating a format string, we should
C           construct the previous time representation and
C           the next for this component (just in case we need it
C           later).
C
            IF ( MAKING ) THEN
 
C
C              We store the value of our current type in the
C              CURRNT slot of the values array.  This value
C              is used by the single numeric types, JD, SP2000,
C              and SP1950.
C
               VALUES(CURRNT) = VALUES(TYPE)
 
C
C              Here's how this works:  We will copy all of
C              the components of the time representation up to
C              the current one.  This is the truncated representation
C              of our epoch.  We then copy these same components into
C              another time vector, but add an increment to the
C              component corresponding to the one we are dealing with
C              now.  We use an increment of 0 for those components that
C              already contain their fractional part. We use an
C              increment of 1 for the components that typically have
C              integer representations.
C
C
C              Zero out the previous and next time vectors so we won't
C              have to do it when we are filling in the truncated
C              portions.
C
               DO J = 1, 7
                  PTVEC(J) = 0.0D0
                  NTVEC(J) = 0.0D0
               END DO
 
 
               IF      ( TYPE .EQ. YEAR  .OR. TYPE .EQ. YR ) THEN
 
                  STOPAT = 1
                  TIMFMT = YMD
                  INTYP  = YMDFMT
                  INCR   = 1.0D0
 
               ELSE IF ( TYPE .EQ. MONTH) THEN
 
                  STOPAT = 2
                  TIMFMT = YMD
                  INTYP  = YMDFMT
                  INCR   = 1.0D0
 
               ELSE IF ( TYPE .EQ. DAY .OR. TYPE .EQ. DOY ) THEN
 
                  STOPAT = 3
                  TIMFMT = YMD
                  INTYP  = YMDFMT
                  INCR   = 1.0D0
 
 
               ELSE IF ( TYPE .EQ. HOUR .OR. TYPE .EQ. AMPM ) THEN
 
C
C                 Note that in this case (and the next 2) that if we
C                 an HOUR component, we had to get it either from
C                 a Day of Year format or from a Year Month Day
C                 format. Thus we have all of the more significant
C                 components for this format.
C
                  STOPAT = 4
                  TIMFMT = YMD
                  INTYP  = YMDFMT
                  INCR   = 1.0D0
 
               ELSE IF ( TYPE .EQ. MINUTE  ) THEN
 
                  STOPAT = 5
                  TIMFMT = YMD
                  INTYP  = YMDFMT
 
                  INCR   = 1.0D0
 
               ELSE IF ( TYPE .EQ. SEC  ) THEN
 
                  STOPAT = 6
                  TIMFMT = YMD
                  INTYP  = YMDFMT
                  INCR   = 0.0D0
 
 
               ELSE IF ( TYPE .EQ. JULIAN ) THEN
 
                  STOPAT = 1
                  TIMFMT = CONTIN
                  INCR   = 0.0D0
 
                  IF      ( TIMTYP .EQ. ID(TDT) ) THEN
                     INTYP = 'JDTDT'
                  ELSE IF ( TIMTYP .EQ. ID(TDB) ) THEN
                     INTYP = 'JDTDB'
                  ELSE IF (      TIMTYP .EQ. ID(UTC)
     .                      .OR. TIMTYP .EQ. ID(UTCP) ) THEN
                     INTYP = 'JDUTC'
                  END IF
 
               ELSE
 
C
C                 The only types left are the continuous (numeric)
C                 types.
C
                  STOPAT = 1
                  TIMFMT = CONTIN
                  INCR   = 0.0D0
                  INTYP  = BASTYP
 
 
               END IF
 
C
C              Ok.  We are now ready to construct the previous
C              and next time vectors.
C
               DO J = 1, STOPAT
                  PTVEC(J) = VALUES(COMPNT(J,TIMFMT))
                  NTVEC(J) = PTVEC (J)
               END DO
 
               NTVEC(STOPAT) = NTVEC(STOPAT) + INCR
 
C
C              If the type is a year or month, then we need to set
C              the month to 1, so that we will be working with
C              beginnings of years not beginning of last months of
C              the previous year.
C
               IF ( TYPE .EQ. YEAR  .OR. TYPE .EQ. YR ) THEN
                  PTVEC(2) = 1.0D0
                  NTVEC(2) = 1.0D0
                  PTVEC(3) = 1.0D0
                  NTVEC(3) = 1.0D0
               ELSE IF ( TYPE .EQ. MONTH ) THEN
                  PTVEC(3) = 1.0D0
                  NTVEC(3) = 1.0D0
               END IF
 
               IF ( GO2JUL .AND. TIMFMT .NE. CONTIN ) THEN
 
C
C                 Convert both PTVEC and NTVEC to the gregorian
C                 calendar
C
                  JYEAR  = NINT(PTVEC(1))
                  JMONTH = NINT(PTVEC(2))
                  JDAY   = NINT(PTVEC(3))
 
                  CALL JUL2GR ( JYEAR, JMONTH, JDAY, JDOY )
 
                  PTVEC(1) = DBLE(JYEAR)
                  PTVEC(2) = DBLE(JMONTH)
                  PTVEC(3) = DBLE(JDAY)
 
                  JYEAR  = NINT(NTVEC(1))
                  JMONTH = NINT(NTVEC(2))
                  JDAY   = NINT(NTVEC(3))
 
                  CALL JUL2GR ( JYEAR, JMONTH, JDAY, JDOY )
 
                  NTVEC(1) = DBLE(JYEAR)
                  NTVEC(2) = DBLE(JMONTH)
                  NTVEC(3) = DBLE(JDAY)
 
               END IF
 
               IF ( DOZONE .AND. TIMFMT .NE. CONTIN ) THEN
 
                  PTVEC(4) = PTVEC(4) - HOFF
                  NTVEC(4) = NTVEC(5) - HOFF
 
                  PTVEC(5) = PTVEC(5) - MOFF
                  NTVEC(5) = NTVEC(5) - MOFF
 
                  PTVEC(6) = 0.0D0
                  NTVEC(6) = 0.0D0
 
                  CALL TTRANS ( 'YMDF', 'YMDF', PTVEC )
                  CALL TTRANS ( 'YMDF', 'YMDF', NTVEC )
 
                  IF ( TYPE .EQ. SEC ) THEN
                     PTVEC(6) = VALUES(SEC)
                     NTVEC(6) = VALUES(SEC)
                  END IF
 
               END IF
 
 
            END IF
 
         END IF
 
      END DO
 
C
C     All that's left to do is to copy the constructed string
C     to the output string.
C
      OUTPUT = STRING
 
 
      CALL CHKOUT ( 'TIMOUT' )
      RETURN
      END
