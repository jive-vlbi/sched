C$Procedure      STR2ET ( String to ET )
 
      SUBROUTINE STR2ET ( STRING, ET )
 
C$ Abstract
C
C     Convert a string representing an epoch to a double precision
C     value representing the number of TDB seconds past the J2000
C     epoch corresponding to the input epoch.
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
C      TIME
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         STRING
      DOUBLE PRECISION      ET
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   A string representing an epoch.
C     ET         O   The equivalent value in seconds past J2000, TDB.
C
C$ Detailed_Input
C
C     STRING     is a string representing an epoch.  Virtually all
C                common calendar representations are allowed. You may
C                specify a time string belonging to any of the
C                systems TDB, TDT, UTC.  Moreover, you may specify a
C                time string relative to a specific UTC based time
C                zone.
C
C                The rules used in the parsing of STRING are spelled
C                out in great detail in the routine TPARTV. The basics
C                are given in the Particulars section below.
C
C$ Detailed_Output
C
C     ET         is the double precision number of TDB seconds past the
C                J2000 epoch that corresponds to the input STRING.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C
C     1) The error SPICE(UNPARSEDTIME) is signaled if the
C        string cannot be recognized as a legitimate time string.
C
C     2) The error SPICE(TIMECONFLICT) is signaled if more than
C        one time system is specified as part of the time string.
C
C     3) The error SPICE(BADTIMESTRING) is signaled if any component
C        of the time string is outside the normal range of usage. For
C        example, the day January 35 is outside the normal range of days
C        in January. The checks applied are spelled out in the routine
C        TCHECK.
C
C     4) If a time zone is specified with hours or minutes components
C        that are outside of the normal range, the error
C        SPICE(TIMEZONEERROR) will be signaled.
C        
C$ Files
C
C     None.
C
C$ Particulars
C
C      This routine computes the ephemeris epoch corresponding to an
C      input string.  The ephemeris epoch is represented as seconds
C      past the J2000 epoch in the time system known as Barycentric
C      Dynamical Time (TDB).  This time system is also referred to as
C      Ephemeris Time (ET) throughout the SPICE Toolkit.
C
C      The variety of ways people have developed for representing
C      times is enormous. It is unlikely that any single subroutine
C      can accommodate the wide variety of custom time formats that
C      have arisen in various computing contexts. However, we
C      believe that this routine will correctly interpret most time
C      formats used throughout the planetary science community.
C      For example this routine supports ISO time formats and UNIX
C      `date` output formats. One obvious omission from the strings
C      recognized by this routine are strings of the form
C
C           93234.1829  or 1993234.1829
C
C      Some readers may recognize this as the epoch that is 0.1829
C      days past the beginning of the 234'th day of 1993.  However,
C      many other readers may regard this interpretation as a bit
C      obscure.
C
C      Below we outline some of the rules used in the interpretation
C      of strings.  A more complete discussion of the interpretation
C      of strings is given in the routine TPARTV.
C
C
C      Default Behavior
C      ----------------
C
C      Consider the string
C
C           1988 June 13, 3:29:48
C
C      There is nothing in this string to indicate what time system
C      the date and time belong to.  Moreover, there is nothing to
C      indicate whether the time is based on a 24-hour clock or
C      twelve hour clock.
C
C      In the absence of such indicators, the default interpretation
C      of this string is to regard the time of day to be a time on
C      a 24-hour clock in the UTC time system.  The date is a date
C      on the Gregorian Calendar (this is the calendar used in nearly
C      all western societies).
C
C      Labels
C      ------
C
C      If you add more information to the string, STR2ET can make a
C      more informed interpretation of the time string. For example:
C
C           1988 June 13, 3:29:48 P.M.
C
C      is still regarded as a UTC epoch.  However, with the addition
C      of the 'P.M.' label it is now interpreted as the same epoch
C      as the unlabeled epoch 1988 June 13, 15:29:48.   Similarly
C
C           1988 June 13, 12:29:48 A.M.
C
C      is interpreted as
C
C            1988 June 13, 00:29:48
C
C      For the record: 12:00 A.M. corresponds to Midnight (00:00 on the
C      24 hour clock.  12:00 P.M. corresponds to Noon. (12:00) on the
C      24 hour clock.
C
C      You may add still further indicators to the string.  For example
C
C          1988 June 13, 3:29:48 P.M. PST
C
C      is interpreted as an epoch in the Pacific Standard Time system.
C      This is equivalent to
C
C          1988 June 13, 07:29:48 UTC
C
C      The following U.S. time zones are recognized.
C
C         EST   --- Eastern Standard Time  ( UTC-5:00 )
C         CST   --- Central Standard Time  ( UTC-6:00 )
C         MST   --- Mountain Standard Time ( UTC-7:00 )
C         PST   --- Pacific Standard Time  ( UTC-8:00 )
C
C         EDT   --- Eastern Daylight Time  ( UTC-4:00 )
C         CDT   --- Central Daylight Time  ( UTC-5:00 )
C         MDT   --- Mountain Daylight Time ( UTC-6:00 )
C         PDT   --- Pacific Daylight Time  ( UTC-7:00 )
C
C      In addition any other time zone may be specified by representing
C      its offset from UTC. This notation starts with the letters 'UTC'
C      followed by a '+' for time zones east of Greenwich and '-' for
C      time zones west of Greenwich.  This is followed by the number of
C      hours to add or subtract from UTC.  This is optionally followed
C      by a colon ':' and the number of minutes to add or subtract to
C      get the local time zone.  Thus to specify the time zone of
C      Calcutta (which is 5 and 1/2 hours ahead of UTC) you would
C      specify the time zone to be UTC+5:30.  To specify the time zone
C      of Newfoundland (which is 3 and 1/2 hours behind UTC) use the
C      offset notation UTC-3:30.
C
C      For the Record:  Leapseconds occur at the same time in all
C      time zones.  In other words, the seconds component of a time
C      string is the same for any time zone as is the seconds
C      component of UTC.  Thus the following are all legitimate
C      ways to represent an epoch of some event that occurred
C      in the leapsecond
C
C           1995 December 31 23:59:60.5  (UTC)
C
C
C           1996 January   1, 05:29:60.5  (UTC+5:30 --- Calcutta Time)
C           1995 December 31, 20:29:60.5  (UTC-3:30 --- Newfoundland)
C           1995 December 31  18:59:60.5  (EST)
C           1995 December 31  17:59:60.5  (CST)
C           1995 December 31  16:59:60.5  (MST)
C           1995 December 31  15:59:60.5  (PST)
C
C
C      In addition to specifying time zones, you may specify that the
C      string be interpreted as a formal  calendar representation in
C      either the Barycentric Dynamical Time system (TDB) or the
C      Terrestrial Dynamical Time system (TDT).  In These systems there
C      are no leapseconds.  Times in TDB are written as
C
C        1988 June 13, 12:29:48 TDB
C
C      TDT times are written as:
C
C        1988 June 13, 12:29:48 TDT
C
C     Finally, you may explicitly state that the time system is UTC
C
C        1988 June 13, 12:29:48 UTC.
C
C
C      Abbreviating Years
C      ------------------
C
C      Although it can lead to confusion, many people are in the
C      habit of abbreviating years when they write them in dates.
C      For example
C
C         99 Jan 13,  12:28:24
C
C      Upon seeing such a string, most of us would regard this
C      as being 1999 January 13, 12:28:24 and not January 13 of
C      the year 99.  This routine interprets years that are less
C      than 100 as belonging either to the 1900's or 2000's.  Years
C      greater than 68 ( 69 - 99 ) are regarded as being an
C      abbreviation with the '19' suppressed (1969 - 1999).  Years
C      smaller than 69 ( 00 - 68 ) are regarded as being an
C      abbreviation with the '20' suppressed (2000 - 2068).
C
C      Note that in general it is usually a good idea to write
C      out the year.  Or if you'd like to save some typing
C      abbreviate 1999 as '99.
C
C      If you need to specify an epoch whose year
C      is less than 1000, we recommend that you specify the era
C      along with the year.  For example if you want to specify
C      the year 13 A.D. write it as
C
C        13 A.D. Jan 12
C
C      When specifying the era it should immediately follow the year.
C      Both the A.D. and B.C. eras are supported.
C
C
C      Changing Default Behavior
C      -------------------------
C
C      As discussed above, if a string is unlabeled, it is regarded
C      as representing a string in the UTC time system on the
C      Gregorian calendar.  In addition abbreviated years are
C      regarded as abbreviations of the years from 1969 to 2068.
C
C      You may modify these defaults through the routines TIMDEF
C      and TSETYR (an entry point of TEXPYR).
C
C      You may:
C
C        Set the calendar to be Gregorian, Julian or a mixture of
C        two via the TIMDEF;
C
C        Set the time system to be UTC, TDB, TDT or any time zone
C        via the routine TIMDEF;
C
C        Set the range of year abbreviations to be any 100 year
C        interval via the routine TSETYR.
C
C     See the routine TEXPYR and TIMDEF for details on changing
C     defaults.
C
C     These alterations affect only the interpretation of unlabeled
C     strings.  If an input string is labeled the specification
C     in the label is used.
C
C
C     If any component of a date or time is out of range, STR2ET
C     regards the string as erroneous.  Below is a list of
C     erroneous strings and why they are regarded as such.
C
C        1997 Jan 32 12:29:29     --- there are only 31 days in January
C
C        '98 Jan 12 13:29:29 A.M. --- Hours must be between 1 and 12
C                                     inclusive when A.M. or P.M. is
C                                     specified.
C
C        1997 Feb 29, 12:29:20.0  --- February has only 29 days in
C                                     1997. This would be ok if the
C                                     year was 1996.
C
C
C        1992 Mar 12 12:62:20     --- Minutes must be between 0 and 59
C                                     inclusive.
C
C        1993 Mar 18 15:29:60.5   --- Seconds is out of range for this
C                                     date.  It would not be out of
C                                     range for Dec 31 23:59:60.5 or
C                                     Jun 30 23:59:60.5 because these
C                                     can be leapseconds (UTC).
C
C     Specifics On Interpretation of the Input String
C     -----------------------------------------------
C
C     The process of examining the string to determine its meaning is
C     called "parsing" the string. The string is parsed by first
C     determining its recognizable substrings (integers, punctuation
C     marks, names of months, names of weekdays, time systems, time
C     zones, etc.)  These recognizable substrings are called the tokens
C     of the input string.  The meaning of some tokens are immediately
C     determined. For example named months, weekdays, time systems have
C     clear meanings.  However, the meanings of numeric components must
C     be deciphered from their magnitudes and location in the string
C     relative to the immediately recognized components of the input
C     string.
C
C     To determine the meaning of the numeric tokens in the input
C     string, a set of "production rules" and transformations are
C     applied to the full set of tokens in the string.  These
C     transformations are repeated until the meaning of every token
C     has been determined, or until further transformations yield
C     no new clues into the meaning of the numeric tokens.
C
C     1)  Unless the substring 'JD' or 'jd' is present, the string is
C         assumed to be a calendar format (day-month-year or year and
C         day of year).  If the substring JD or jd is present, the
C         string is assumed to represent a Julian date.
C
C     2)  If the Julian date specifier is not present, any integer
C         greater than 999 is regarded as being a year specification.
C
C     3)  A dash '-' can represent a minus sign only if it precedes
C         the first digit in the string and the string contains
C         the Julian date specifier (JD).  (No negative years,
C         months, days, etc are allowed).
C
C     4)  Numeric components of a time string must be separated
C         by a character that is not a digit or decimal point.
C         Only one decimal component is allowed.  For example
C         1994219.12819 is sometimes interpreted as the
C         219th day of 1994 + 0.12819 days.  STR2ET does not
C         support such strings.
C
C         No exponential components are allowed.  For example you
C         can't specify the Julian date of J2000 as 2.451545E6.
C
C     5)  The single colon (:) when used to separate numeric
C         components of a string is interpreted as separating
C         Hours, Minutes, and Seconds of time.
C
C     6)  If a double slash (//) or double colon (::) follows
C         a pair of integers, those integers are assumed  to
C         represent the year and day of year.
C
C     7)  A quote followed by an integer less than 100 is regarded
C         as an abbreviated year.  For example: '93 would be regarded
C         as the 93rd year of the reference century.  See TEXPYR
C         for further discussion of abbreviated years.
C
C      8) An integer followed by 'B.C.' or 'A.D.' is regarded as
C         a year in the era associated with that abbreviation.
C
C      9) All dates are regarded as belonging to the extended
C         Gregorian Calendar (the Gregorian calendar is the calendar
C         currently used by western society).  See the routine TIMDEF
C         to modify this behavior.
C
C     10) If the ISO date-time separator (T) is present in the string
C         ISO allowed token patterns are examined for a match
C         with the current token list.  If no match is found the
C         search is abandoned and appropriate diagnostic messages
C         are generated.
C
C     11) If two delimiters are found in succession in the time
C         string, the time string is diagnosed as an erroneous string.
C         (Delimiters are comma, white space, dash, slash, period, or
C         of year mark.  The day of year mark is a pair of forward
C         slashes or a pair of colons.)
C
C         Note the delimiters do not have to be the same. The pair
C         of characters ",-" counts as two successive delimiters.
C
C     12) White space and commas serve only to delimit tokens in the
C         input string.  They do not affect the meaning of any
C         of the tokens.
C
C     13) If an integer is greater than 1000 (and the 'JD' label
C         is not present, the integer is regarded as a year.
C
C     14) When the size of the integer components does not clearly
C         specify a year the following patterns are assumed
C
C         Calendar Format
C
C             Year Month Day
C             Month Day Year
C             Year Day Month
C
C             where Month is the name of a month, not its numeric
C             value.
C
C             When integer components are separated by slashes (/)
C             as in 3/4/5.  Month, Day, Year is assumed (2005 March 4)
C
C          Day of Year Format.
C
C             If a day of year marker (// or ::) is present, the
C             pattern I-I// or I-I:: (where I stands for an integer)
C             is interpreted as Year Day-of-Year. However, I-I/ is
C             regarded as ambiguous.
C
C
C$ Examples
C
C      Below is a sampling of some of the time formats that are
C      acceptable as inputs to STR2ET.  A complete discussion of
C      permissible formats is given in the SPICE routine TPARTV as well
C      as the reference document time.req located in the "doc"
C      directory of the Toolkit.
C
C      ISO (T) Formats.
C
C      String                        Year Mon  DOY DOM  HR Min Sec
C      ----------------------------  ---- ---  --- ---  -- --- ------
C      1996-12-18T12:28:28           1996 Dec   na  18  12  28 28
C      1986-01-18T12                 1986 Jan   na  18  12  00 00
C      1986-01-18T12:19              1986 Jan   na  18  12  19 00
C      1986-01-18T12:19:52.18        1986 Jan   na  18  12  19 52.18
C      1995-08T18:28:12              1995  na  008  na  18  28 12
C      1995-18T                      1995  na  018  na  00  00 00
C
C
C      Calendar Formats.
C
C      String                        Year   Mon DOM  HR Min  Sec
C      ----------------------------  ----   --- ---  -- ---  ------
C      Tue Aug  6 11:10:57  1996     1996   Aug  06  11  10  57
C      1 DEC 1997 12:28:29.192       1997   Dec  01  12  28  29.192
C      2/3/1996 17:18:12.002         1996   Feb  03  17  18  12.002
C      Mar 2 12:18:17.287 1993       1993   Mar  02  12  18  17.287
C      1992 11:18:28  3 Jul          1992   Jul  03  11  18  28
C      June 12, 1989 01:21           1989   Jun  12  01  21  00
C      1978/3/12 23:28:59.29         1978   Mar  12  23  28  59.29
C      17JUN1982 18:28:28            1982   Jun  17  18  28  28
C      13:28:28.128 1992 27 Jun      1992   Jun  27  13  28  28.128
C      1972 27 jun 12:29             1972   Jun  27  12  29  00
C      '93 Jan 23 12:29:47.289       1993*  Jan  23  12  29  47.289
C      27 Jan 3, 19:12:28.182        2027*  Jan  03  19  12  28.182
C      23 A.D. APR 4, 18:28:29.29    0023** Apr  04  18  28  29.29
C      18 B.C. Jun 3, 12:29:28.291   -017** Jun  03  12  29  28.291
C      29 Jun  30 12:29:29.298       2029+  Jun  30  12  29  29.298
C      29 Jun '30 12:29:29.298       2030*  Jun  29  12  29  29.298
C
C      Day of Year Formats
C
C      String                        Year  DOY HR Min Sec
C      ----------------------------  ----  --- -- --- ------
C      1997-162::12:18:28.827        1997  162 12  18 28.827
C      162-1996/12:28:28.287         1996  162 12  28 28.287
C      1993-321/12:28:28.287         1993  231 12  28 28.287
C      1992 183// 12 18 19           1992  183 12  18 19
C      17:28:01.287 1992-272//       1992  272 17  28 01.287
C      17:28:01.282 272-1994//       1994  272 17  28 01.282
C      '92-271/ 12:28:30.291         1992* 271 12  28 30.291
C      92-182/ 18:28:28.281          1992* 182 18  28 28.281
C      182-92/ 12:29:29.192          0182+ 092 12  29 29.192
C      182-'92/ 12:28:29.182         1992  182 12  28 29.182
C
C
C      Julian Date Strings
C
C      jd 28272.291                  Julian Date   28272.291
C      2451515.2981 (JD)             Julian Date 2451515.2981
C      2451515.2981 JD               Julian Date 2451515.2981
C
C                                   Abbreviations Used in Tables
C
C                                      na    --- Not Applicable
C                                      Mon   --- Month
C                                      DOY   --- Day of Year
C                                      DOM   --- Day of Month
C                                      Wkday --- Weekday
C                                      Hr    --- Hour
C                                      Min   --- Minutes
C                                      Sec   --- Seconds
C
C      * The default interpretation of a year that has been abbreviated
C      with a leading quote as in 'xy (such as '92) is to treat the
C      year as 19xy if xy > 68 and to treat it is 20xy otherwise. Thus
C      '69 is interpreted as 1969 and '68 is treated as 2068. However,
C      you may change the "split point" and centuries through use of
C      the SPICE routine TSETYR which is an entry point in the SPICE
C      module TEXPYR.  See that routine for a discussion of how you may
C      reset the split point.
C
C      ** All epochs are regarded as belonging to the Gregorian
C      calendar.  We formally extend the Gregorian calendar backward
C      and forward in time for all epochs.
C
C      +  When a day of year format or calendar format string is input
C         and neither of the integer components of the date is greater
C         than 1000, the first integer is regarded as being the year.
C
C
C      Suppose you would like to determine whether your favorite
C      time representation is supported by STR2ET.  The small
C      program below gives you a simple way to experiment with
C      STR2ET.  (Note that erroneous inputs will be flagged by
C      signaling an error.)
C
C      To run this program you need to:
C
C      1.  copy it to a file,
C      2.  un-comment the obvious lines of code,
C      3.  compile it,
C      4.  link the resulting object file with SPICELIB,
C      5.  and place the leapseconds kernel in your current directory.
C
C      PROGRAM
C
C      CHARACTER*(64)        STRING
C      CHARACTER*(64)        CALDR
C      CHARACTER*(64)        DAYOFY
C      CHARACTER*(127)       FILNAM
C
C      DOUBLE PRECISION      ET
C
C
C      First get the name of a leapseconds kernel, and load it.
C
C      CALL PROMPT ( 'Leapseconds kernel: ', FILNAM )
C      CALL FURNSH ( FILNAM )
C
C      Leave some space on the screen and get the first trial string.
C      If we get a blank input, we quit.
C
C      WRITE (*,*)
C      CALL PROMPT ( 'String to try: ', STRING )
C
C      DO WHILE ( STRING .NE. ' ' )
C
C         Convert the string to ET and then back to UTC calendar
C         and day-of-year formats.
C
C         CALL STR2ET ( STRING, ET )
C         CALL ET2UTC ( ET, 'C', 0, CALDR  )
C         CALL ET2UTC ( ET, 'D', 0, DAYOFY )
C
C         Print the results.
C
C         WRITE (*,*) 'Calendar    Format: ', CALDR
C         WRITE (*,*) 'Day of year Format: ', DAYOFY
C
C         Ask for another string and do it all again.
C
C         WRITE (*,*)
C         CALL PROMPT ( 'String to try: ', STRING )
C
C      END DO
C      END
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
C     C.H. Acton         (JPL)
C     N.J. Bachman       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.1, 02-NOV-2009 (CHA)
C
C        A few minor grammar errors were fixed in the header. 
C        The header sections were reordered.
C
C-    SPICELIB Version 1.3.0, 31-AUG-2006 (NJB) (EDW)
C
C        Bug fix:  routine formerly returned incorrect results
C        in some cases on calls following calls for which a time
C        zone was specified.
C
C        Replaced reference to LDPOOL in header Examples section
C        with reference to FURNSH.
C
C-    SPICELIB Version 1.2.2, 29-JUL-2003 (NJB)
C
C        Various minor header corrections were made
C
C-    SPICELIB Version 1.2.1, 10-FEB-2003 (NJB)
C
C        Corrected header typo.
C
C-    SPICELIB Version 1.2.0, 11-NOV-1997 (WLT)
C
C        The previous versions of this routine did not correctly
C        convert day-of-year strings in the TDB or TDT systems.
C        They treated the day of year as year, month, day giving
C        spectacularly wrong answers.
C
C        In addition, comments concerning the default century for
C        abbreviated years were updated to reflect changes to TEXPYR
C
C-    SPICELIB Version 1.1.0, 10-FEB-1997 (WLT)
C
C        In the case that a time zone could not be parsed,
C        this routine signaled an error and checked out without
C        then returning.  This error has been corrected.
C
C-    SPICELIB Version 1.0.0, 15-NOV-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Convert a string to TDB seconds past the J2000 epoch
C
C-&

C$ Revisions
C
C-    SPICELIB Version 1.3.0, 31-AUG-2006 (NJB)
C
C        Bug fix:  routine formerly returned incorrect results
C        in some cases on calls following calls for which a time
C        zone was specified.
C
C        The problem was caused by the variable ZONED not being
C        properly set when a time system was specified
C        in the input string.  In such cases, ZONED retained the
C        value from the previous call.  
C        
C-&


C
C     SPICELIB Functions.
C
      LOGICAL               RETURN
 
 
C
C     Local (in-line) Functions
C
      INTEGER               SMWDSZ
      PARAMETER           ( SMWDSZ = 16 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               MSGSIZ
      PARAMETER           ( MSGSIZ = 400 )
 
      INTEGER               ERA
      PARAMETER           ( ERA    = 1 )
 
      INTEGER               WKDAY
      PARAMETER           ( WKDAY  = ERA    + 1 )
 
      INTEGER               ZONE
      PARAMETER           ( ZONE   = WKDAY  + 1 )
 
      INTEGER               AMPM
      PARAMETER           ( AMPM   = ZONE   + 1 )
 
      INTEGER               SYSTEM
      PARAMETER           ( SYSTEM = AMPM   + 1 )
 
      CHARACTER*(LNSIZE)    PICTUR
      CHARACTER*(MSGSIZ)    ERROR
 
      CHARACTER*(SMWDSZ)    CALNDR
      CHARACTER*(SMWDSZ)    CHECK
      CHARACTER*(SMWDSZ)    DEFSYS
      CHARACTER*(SMWDSZ)    DEFZON
      CHARACTER*(SMWDSZ)    FORML
      CHARACTER*(SMWDSZ)    JULN
      CHARACTER*(SMWDSZ)    MIXED
      CHARACTER*(SMWDSZ)    GREGRN
      CHARACTER*(SMWDSZ)    MODIFY ( SYSTEM )
      CHARACTER*(SMWDSZ)    TYPE
 
      CHARACTER*(2)         HSTR
      CHARACTER*(2)         MSTR
 
      CHARACTER*(SMWDSZ)    MNAME  ( 12 )
 
      DOUBLE PRECISION      DHOFF
      DOUBLE PRECISION      DMOFF
      DOUBLE PRECISION      EXTRA
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      HOFF
      DOUBLE PRECISION      HOUR
      DOUBLE PRECISION      MDY    ( 2 )
      DOUBLE PRECISION      MINUTE
      DOUBLE PRECISION      MOFF
      DOUBLE PRECISION      MON    ( 2 )
      DOUBLE PRECISION      SECS
      DOUBLE PRECISION      TVEC   ( 8 )
      DOUBLE PRECISION      TVECM  ( 8 )
 
      INTEGER               CYEAR
      INTEGER               GYEAR
 
      INTEGER               DAY
      INTEGER               DOY
      INTEGER               I
      INTEGER               LAST
      INTEGER               MONTH
      INTEGER               NTVEC
      INTEGER               ORGNYR
      INTEGER               YEAR
 
C
C     The following integers are pointers to the
C     locations of various components in a time vector.
C
      INTEGER               DY
      INTEGER               HR
      INTEGER               MM
      INTEGER               MN
      INTEGER               SC
      INTEGER               YR
 
      LOGICAL               ADJUST
      LOGICAL               DOJUL
      LOGICAL               MODS
      LOGICAL               OK
      LOGICAL               OK1
      LOGICAL               OK2
      LOGICAL               SUCCES
      LOGICAL               YABBRV
      LOGICAL               ZONED

C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
      DATA                  DEFZON  / ' '         /
      DATA                  DEFSYS  / 'UTC'       /
      DATA                  MIXED   / 'MIXED'     /
      DATA                  JULN    / 'JULIAN'    /
      DATA                  GREGRN  / 'GREGORIAN' /
      DATA                  DHOFF   /  0.0D0      /
      DATA                  DMOFF   /  0.0D0      /
 
      DATA                  MNAME   /   'January',   'February',
     .                                  'March',     'April',
     .                                  'May',       'June',
     .                                  'July',      'August',
     .                                  'September', 'October',
     .                                  'November',  'December'  /
 
 
 
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'STR2ET' )
 
C
C     Collect the current defaults.
C
      CALL TIMDEF ( 'GET', 'SYSTEM',   DEFSYS )
      CALL TIMDEF ( 'GET', 'ZONE',     DEFZON )
      CALL TIMDEF ( 'GET', 'CALENDAR', CALNDR )
 
      IF ( DEFZON .NE. ' ' ) THEN
         CALL PREFIX  ( '::', 0, DEFZON )
         CALL ZZUTCPM ( DEFZON, 1, DHOFF, DMOFF, LAST, SUCCES )
      ELSE
         DHOFF = 0.0D0
         DMOFF = 0.0D0
      END IF
 
C
C     See if TPARTV can recognize what the user has supplied.
C
      CALL TPARTV ( STRING,
     .              TVEC,   NTVEC, TYPE,
     .              MODIFY, MODS,  YABBRV, SUCCES,
     .              PICTUR, ERROR )
 
 
      IF ( .NOT. SUCCES ) THEN
 
         CALL SETMSG ( ERROR )
         CALL SIGERR ( 'SPICE(UNPARSEDTIME)' )
         CALL CHKOUT ( 'STR2ET' )
         RETURN
 
      END IF
 
C
C     A system and time zone are incompatible components in a
C     time string.
C
      IF (       MODIFY(ZONE)   .NE. ' '
     .     .AND. MODIFY(SYSTEM) .NE. ' ' ) THEN
 
         CALL SETMSG ( 'Both a time system and time zone have '
     .   //            'been specified in the input string (# '
     .   //            'and #). These are inconsistent. A '
     .   //            'time zone is a fixed offset from UTC. ' )
 
         CALL ERRCH  ( '#', MODIFY(SYSTEM)   )
         CALL ERRCH  ( '#', MODIFY(ZONE)     )
         CALL SIGERR ( 'SPICE(TIMECONFLICT)' )
         CALL CHKOUT ( 'STR2ET'              )
         RETURN
 
      END IF
C
C     If both the zone and system are empty, we can replace them
C     with the default zone and system values (only one of which
C     can be non-blank).
C
      ZONED = .FALSE.

      IF ( MODIFY(ZONE) .EQ. ' ' .AND. MODIFY(SYSTEM) .EQ. ' ' ) THEN
 
         MODIFY(ZONE)   = DEFZON
         MODIFY(SYSTEM) = DEFSYS
         HOFF           = DHOFF
         MOFF           = DMOFF
         ZONED          = MODIFY(ZONE) .NE. ' '
 
 
      ELSE IF ( MODIFY(ZONE) .NE. ' ' ) THEN
C
C        Parse the time zone specification.  If we don't succeed
C        in the parsing, signal an error.
C
         ZONED = .TRUE.
         CALL PREFIX  ( '::', 0, MODIFY(ZONE) )
         CALL ZZUTCPM ( MODIFY(ZONE), 1, HOFF, MOFF, LAST, SUCCES )
 
         IF ( .NOT. SUCCES ) THEN
            CALL SETMSG ( '# is not a legitimate time zone '
     .      //            'specification. '       )
            CALL ERRCH  ( '#', MODIFY(ZONE)(3:)   )
            CALL SIGERR ( 'SPICE(TIMEZONEERROR)'  )
            CALL CHKOUT ( 'STR2ET' )
            RETURN
         END IF
 
      END IF
 
C
C     We handle the julian date case now.  It doesn't have the
C     complications associated with it that the calendar strings
C     have.
C
      IF ( TYPE .EQ. 'JD' ) THEN
 
         IF      ( MODIFY(SYSTEM) .EQ. 'UTC' ) THEN
            TYPE = 'JDUTC'
         ELSE IF ( MODIFY(SYSTEM) .EQ. 'TDB' ) THEN
            TYPE = 'JDTDB'
         ELSE IF ( MODIFY(SYSTEM) .EQ. 'TDT' ) THEN
            TYPE = 'JDTDT'
         ELSE
            TYPE = 'JDUTC'
         END IF
 
         CALL TTRANS ( TYPE, 'TDB', TVEC )
         ET = TVEC(1)
 
         CALL CHKOUT ( 'STR2ET' )
         RETURN
 
      END IF
 
C
C     Set the indexes of the hours, minutes, seconds, etc. components
C     of the time vector.
C
      IF ( TYPE .EQ. 'YD' ) THEN
         YR    =  1
         DY    =  2
         HR    =  3
         MN    =  4
         SC    =  5
         FORML = 'YDF'
      ELSE
         YR    =  1
         MM    =  2
         DY    =  3
         HR    =  4
         MN    =  5
         SC    =  6
         FORML = 'YMDF'
      END IF
 
 
C
C     Check the components for reasonableness.
C
      CALL TCHCKD ( CHECK )
      CALL TPARCH ( 'YES' )
 
C
C     If the calendar is NOT gregorian, or if we have a time zone
C     present, we avoid the problem of checking for legitimate
C     leapseconds (at least we avoid this problem for the moment).
C
      ADJUST = .FALSE.
 
      IF ( ZONED .OR.  CALNDR .NE. GREGRN ) THEN
 
         IF (       TVEC(SC) .GE. 60.0D0
     .        .AND. TVEC(SC) .LT. 61.0D0 ) THEN
 
            ADJUST   = .TRUE.
            TVEC(SC) = TVEC(SC) - 1.0D0
 
         END IF
 
      END IF
 
 
 
      IF ( CALNDR .EQ. MIXED  ) THEN
C
C        This is a bit awkward, but here's what's going on.
C        If the input calendar is part of the Julian calendar
C        it might be Feb 29 on a century such as 1500.  These
C        are not legitimate dates on the Gregorian calendar.
C        But they are ok on the Julian calendar.
C
C        However, one of the year numbers YEAR or YEAR + 4 will
C        be a leap year on both the Julian and Gregorian calendar.
C        If we have just a century problem, it will be a problem
C        for only one of the years.  So in the range where we could
C        have a problem we call TCHECK twice and .OR. the results
C        of the checks to see if we have a legitimate time vector.
C
         IF ( TVEC(YR) .LT. 1580.0D0 ) THEN
 
            CALL MOVED  ( TVEC, 6, TVECM )
 
            TVECM(1) = TVECM(1) + 4.0D0
 
            CALL TCHECK ( TVECM, TYPE, MODS, MODIFY, OK1, ERROR )
            CALL TCHECK ( TVEC,  TYPE, MODS, MODIFY, OK2, ERROR )
 
            OK = OK1 .OR. OK2
 
         ELSE
 
            CALL TCHECK ( TVEC,  TYPE, MODS, MODIFY, OK, ERROR )
 
         END IF
 
      ELSE IF ( CALNDR .EQ. JULN  ) THEN
C
C        Basically, this is the same story as before, but there
C        are no bounds in the years where we might be on a century.
C        So we just check twice for each time vector.
C
         CALL MOVED  ( TVEC, 6, TVECM )
 
         TVECM(1) = TVECM(1) + 4.0D0
 
         CALL TCHECK ( TVECM, TYPE, MODS, MODIFY, OK1, ERROR )
         CALL TCHECK ( TVEC,  TYPE, MODS, MODIFY, OK2, ERROR )
 
         OK = OK1 .OR. OK2
 
      ELSE
C
C        TCHECK was designed for the Gregorian Calendar,  So we
C        don't have much to do.
C
         CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
 
      END IF
C
C     Reset the checking status.
C
      CALL TPARCH ( CHECK  )
 
C
C     If we didn't get an OK from the inspection above,
C     say so and signal an error.
C
      IF ( .NOT. OK ) THEN
 
         CALL SETMSG ( ERROR )
         CALL SIGERR ( 'SPICE(BADTIMESTRING)' )
         CALL CHKOUT ( 'STR2ET' )
         RETURN
 
      END IF
C
C     Reset TVEC(SC) if it was adjusted earlier.
C
      IF ( ADJUST ) THEN
         TVEC(SC) = TVEC(SC) + 1.0D0
      END IF
 
C
C     There are no leapseconds in the TDT and TDB time systems
C     This means that the seconds component must be less than 60.
C
      IF (      MODIFY(SYSTEM) .EQ. 'TDT'
     .     .OR. MODIFY(SYSTEM) .EQ. 'TDB' ) THEN
 
         IF ( TVEC(SC) .GE. 60.0D0 ) THEN
 
            CALL SETMSG ( 'The seconds component of time must '
     .      //            'be less than 60 for any '
     .      //            'calendar representation of #. ' )
            CALL ERRCH  ( '#', MODIFY(SYSTEM) )
            CALL SIGERR ( 'SPICE(BADTIMESTRING)' )
            CALL CHKOUT ( 'STR2ET' )
            RETURN
 
         END IF
 
      END IF
 
 
C
C     If a B.C. era  marker is present we can't have a year abbreviation
C
      IF ( MODIFY(ERA) .EQ. 'B.C.' .AND. YABBRV ) THEN
 
         CALL SETMSG ( 'The Year may be abbreviated only if the '
     .   //            'year belongs to the Christian Era (A.D.) ' )
         CALL SIGERR ( 'SPICE(BADTIMESTRING)' )
         CALL CHKOUT ( 'STR2ET' )
         RETURN
 
      END IF
C
C     If the era is B.C. we need to reset the year.
C
      IF ( MODIFY(ERA) .EQ. 'B.C.' ) THEN
         TVEC(YR) = 1.0D0 - TVEC(YR)
      END IF
 
C
C     If there is a A.M. or P.M. time string modifier, we need to adjust
C     the hours component of the time.
C
      IF     ( MODIFY(AMPM) .EQ. 'P.M.'   ) THEN
 
         IF ( TVEC(HR) .LT. 12.0D0 ) THEN
            TVEC(HR) = TVEC(HR) + 12.0D0
         END IF
 
      ELSE IF ( MODIFY(AMPM) .EQ. 'A.M.' ) THEN
 
         IF ( TVEC(HR) .GE. 12.0D0 ) THEN
            TVEC(HR) = TVEC(HR) - 12.0D0
         END IF
 
      END IF
 
C
C     If the year has been abbreviated, we need to convert it
C     to the proper range.  In addition we assume a year less
C     than 100 that is not qualified with the B.C. or A.D. era
C     string is in fact an abbreviated year.
C
      YEAR = NINT( TVEC(YR) )
 
      IF ( YABBRV ) THEN
 
         CALL       TEXPYR ( YEAR )
         TVEC(YR) = DBLE   ( YEAR )
 
      ELSE IF (       YEAR        .LT. 100
     .          .AND. MODIFY(ERA) .EQ. ' ' ) THEN
 
         CALL       TEXPYR ( YEAR )
         TVEC(YR) = DBLE   ( YEAR )
 
      END IF
 
 
C
C     We may need to convert to the Gregorian Calendar, now is
C     the time to do so.
C
      IF ( CALNDR .EQ. MIXED ) THEN
C
C        We need to check the components.
C
         IF ( TYPE .EQ. 'YD' ) THEN
 
            DOJUL =            TVEC(YR) .LT. 1582.0D0
     .             .OR. (      TVEC(YR) .EQ. 1582.0D0
     .                   .AND. TVEC(DY) .LT.  279.0D0 )
 
         ELSE
 
            DOJUL =             TVEC(YR) .LT. 1582.0D0
     .             .OR. (       TVEC(YR) .LE. 1582.0D0
     .                    .AND. TVEC(MM) .LT.   10.0D0 )
     .             .OR. (       TVEC(YR) .LE. 1582.0D0
     .                    .AND. TVEC(MM) .LE.   10.0D0
     .                    .AND. TVEC(DY) .LT.    6.0D0 )
 
         END IF
 
      ELSE IF ( CALNDR .EQ. JULN ) THEN
 
         DOJUL = .TRUE.
 
      ELSE
 
         DOJUL = .FALSE.
 
      END IF
C
C     If the input string is from the julian calendar, we need
C     to convert it to Gregorian.  We also need to save the original
C     year value in the unlikely event it is needed for a later
C     diagnostic message.
C
      IF ( DOJUL ) THEN
 
         IF ( TYPE .EQ. 'YD' ) THEN
 
            YEAR   = DINT( TVEC(YR) )
            MONTH  = 1
            DAY    = DINT( TVEC(DY) )
            FRAC   = TVEC(DY) - DBLE(DAY)
            ORGNYR = YEAR
 
            CALL JUL2GR ( YEAR, MONTH, DAY, DOY )
 
            TVEC(YR) = DBLE(YEAR)
            TVEC(DY) = DBLE(DOY ) + FRAC
 
         ELSE
 
            YEAR   = DINT ( TVEC(YR) )
            MONTH  = DINT ( TVEC(MM) )
            DAY    = DINT ( TVEC(DY) )
            FRAC   = TVEC(DY) - DBLE(DAY)
            ORGNYR = YEAR
 
            CALL JUL2GR ( YEAR, MONTH, DAY, DOY )
 
            TVEC(YR) = DBLE(YEAR)
            TVEC(MM) = DBLE(MONTH)
            TVEC(DY) = DBLE(DAY) + FRAC
 
         END IF
 
      ELSE
 
         ORGNYR = DINT( TVEC(YR) )
 
      END IF
 
C
C     The TDT and TDB calendars don't need to worry about time
C     zone adjustments.
C
      IF      ( MODIFY(SYSTEM) .EQ. 'TDT' ) THEN
 
         CALL TTRANS ( FORML, 'FORMAL', TVEC )
         CALL TTRANS ( 'TDT',  'TDB',    TVEC )
         ET = TVEC(1)
         CALL CHKOUT ( 'STR2ET' )
         RETURN
 
      ELSE IF ( MODIFY(SYSTEM) .EQ. 'TDB' ) THEN
 
         CALL TTRANS ( FORML, 'FORMAL', TVEC(1) )
         ET = TVEC(1)
         CALL CHKOUT ( 'STR2ET' )
         RETURN
 
      END IF
 
 
C
C     If a time zone has been specified, we need to convert
C     from the time zone components to UTC components.
C
      IF ( ZONED  ) THEN
C
C        A time zone was specified explicitly in the input
C        string.  We need to compute the hour and minute offsets
C        associated with the time zone.
C
         TVEC(HR) = TVEC(HR) - HOFF
         TVEC(MN) = TVEC(MN) - MOFF
         SECS     = TVEC(SC)
         TVEC(SC) = 0.0D0
 
         CALL TTRANS ( FORML, FORML, TVEC )
 
         TVEC(SC) = SECS
 
      END IF
C
C     If we decided to forgo the leapseconds check earlier
C     now is the time to do it.  We've now got Gregorian UTC
C     time components.
C
 
      IF ( ADJUST ) THEN
 
         CALL TCHCKD ( CHECK )
         CALL TPARCH ( 'YES' )
 
         MODS         = .FALSE.
         MODIFY(AMPM) = ' '
 
         CALL TCHECK ( TVEC,  TYPE, MODS, MODIFY, OK, ERROR )
 
      ELSE
 
         OK = .TRUE.
 
      END IF
 
 
      IF ( OK ) THEN
C
C        That's it we are ready to rumble.
C
         CALL TTRANS ( TYPE, 'TDB', TVEC )
         ET = TVEC(1)
         CALL CHKOUT ( 'STR2ET' )
         RETURN
 
      END IF
 
C     ===============================================================
C     If you are still here, it is because OK was .FALSE. in the test
C     above.  The only way this can happen is if the seconds were
C     not in the expected range.  The rest of the code is a diagnosis
C     of this problem.  (This is a nuisance case that is
C     unlikely to occur very often.)
C
 
      IF ( ZONED .AND. DOJUL ) THEN
 
         ERROR  = 'The seconds component of ''#'' is out of range. '
     .   //       'On the Julian Calendar '
     .   //       'in the specified time zone  (#) leapseconds '
     .   //       'can occur during the year # only in the '
     .   //       'second that immediately follows the time #:#:59 '
     .   //       'on  # # and # #. '
 
         CALL REPMC ( ERROR,  '#', STRING,           ERROR  )
         CALL REPMC ( ERROR , '#', MODIFY(ZONE)(3:), ERROR  )
 
 
      ELSE IF ( ZONED ) THEN
C
C        If we had a time zone, we want to say what time zone
C        in the output string.
C
         ERROR  = 'The seconds component of ''#'' is out of range. '
     .   //       'In the specified time zone  (#) leapseconds '
     .   //       'can occur during the year # only in the '
     .   //       'second that immediately follows the time #:#:59 '
     .   //       'on  # # and # #.'
 
         CALL REPMC ( ERROR,  '#', STRING,           ERROR  )
         CALL REPMC ( ERROR , '#', MODIFY(ZONE)(3:), ERROR  )
 
      ELSE
C
C        No time zone, this case can only occur if we interpreted
C        the input string as a date on the Julian Calendar
C
         ERROR  = 'The seconds component of ''#'' is out of range. '
     .   //       'Leapseconds can occur during the year # of '
     .   //       'the Julian calendar only in the second that '
     .   //       'immediately follows the time #:#:59  on # # '
     .   //       'and # #.'' '
 
         CALL REPMC ( ERROR,  '#', STRING,           ERROR  )
 
      END IF
 
C
C     First fill in the year portion of the error message.
C
      CALL REPMI ( ERROR , '#', ORGNYR, ERROR  )
 
      MON(1) =  6.0D0
      MON(2) = 12.0D0
 
      MDY(1) = 30.0D0
      MDY(2) = 31.0D0
C
C     Next Fill in the hours and minutes. Recall that leapseconds
C     occur during the last second of the 59'th minute of the 23'rd
C     hour UTC.  So in the new time zone, it occurs in the 59'th + MOFF
C     minute of the 23'rd + HOFF hour of the time zone.  We adjust
C     these to account for hour roll over and day roll over.
C
      MINUTE = 59.0D0 + MOFF
 
      IF ( MINUTE .GT. 59.0D0 ) THEN
         MINUTE = MINUTE - 60.0D0
         EXTRA  = 1.0D0
      ELSE IF ( MINUTE .LT. 0.0D0 ) THEN
         MINUTE = MINUTE + 60.0D0
         EXTRA  = -1.0D0
      ELSE
         EXTRA  = 0.0D0
      END IF
 
 
      HOUR = 23.0D0 + HOFF + EXTRA
 
      IF ( HOUR .GT. 23 ) THEN
         HOUR = HOUR - 24
      END IF
 
C
C     Convert the hours and minutes to strings and place the
C     strings in the message.
C
      CALL DPFMT ( HOUR,   '0x', HSTR )
      CALL DPFMT ( MINUTE, '0x', MSTR )
 
      CALL REPMC ( ERROR , '#', HSTR, ERROR  )
      CALL REPMC ( ERROR , '#', MSTR, ERROR  )
 
C
C     Last step we generate the month and day corresponding
C     to Dec 31, 23:59, and Jun 30, 23:59.  We only want the
C     dates that belong to the original year.  We could
C     probably figure out the right year to use, but with Julian
C     date possibly messing everything up, we just use the
C     current year and the one before.  If you keep in mind that
C     the Julian Year is always less than the Gregorian year and
C     that the offsets can only push you into the next year, you
C     can determine that we want to start with what ever current
C     UTC year we have and work backwards until we have the
C     year corresponding to the original year.  Since the current
C     UTC year was constructed from the input original year, we
C     only have to step back at most 1 year to get all the dates
C     that might have leapseconds in the user specified year
C     of whatever calendar happens to be in use.
C
      CYEAR = DINT( TVEC(YR) )
 
      DO GYEAR = CYEAR, CYEAR-1, -1
 
         DO I = 1, 2
 
            TVEC(1) = DBLE(GYEAR)
            TVEC(2) = MON(I)
            TVEC(3) = MDY(I)
            TVEC(4) = 23.0D0 + HOFF
            TVEC(5) = 59.0D0 + MOFF
            TVEC(6) = 0.0D0
C
C           Normalize the time vector.
C
            CALL TTRANS ( 'YMDF', 'YMDF', TVEC )
 
            YEAR   = NINT(TVEC(1))
            MONTH  = NINT(TVEC(2))
            DAY    = NINT(TVEC(3))
 
            IF ( DOJUL ) THEN
               CALL GR2JUL ( YEAR, MONTH, DAY, DOY )
            END IF
 
 
            IF ( YEAR .EQ. ORGNYR ) THEN
 
               CALL REPMC ( ERROR, '#', MNAME(MONTH), ERROR  )
               CALL REPMI ( ERROR, '#', DAY,          ERROR  )
 
            END IF
 
         END DO
 
      END DO
 
      CALL SETMSG ( ERROR  )
      CALL SIGERR ( 'SPICE(BADTIMESTRING)' )
      CALL CHKOUT ( 'STR2ET' )
      RETURN
 
 
      END
