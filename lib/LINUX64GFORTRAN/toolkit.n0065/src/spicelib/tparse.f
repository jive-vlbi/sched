C$Procedure       TPARSE ( Parse a UTC time string )
 
       SUBROUTINE TPARSE ( STRING, SP2000, ERROR )
       IMPLICIT NONE
 
C$ Abstract
C
C      Parse a time string and return seconds past the J2000 epoch
C      on a formal calendar.
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
C     None.
C
C$ Keywords
C
C      PARSING, TIME
C
C$ Declarations
 
      CHARACTER*(*)      STRING
      DOUBLE PRECISION   SP2000
      CHARACTER*(*)      ERROR
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I   Input time string, UTC.
C      SP2000     O   Equivalent UTC seconds past J2000.
C      ERROR      O   Descriptive error message.
C
C$ Detailed_Input
C
C      STRING      is an input time string, containing a Calendar or
C                  Julian Date.  It may be in several different
C                  formats and can make use of abbreviations.
C                  Several example strings and
C                  the times that they translate to are listed below
C                  in the Examples section.
C
C$ Detailed_Output
C
C      SP2000      is the equivalent of UTC, expressed in UTC
C                  seconds past J2000. If an error occurs, or if
C                  UTC is ambiguous, SP2000 is not changed.
C
C      ERROR       is a descriptive error message, which is blank when
C                  no error occurs.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      The input string is examined and the various components of
C      a date are identified: julian date, year, month, day of year,
C      day of month, hour, minutes, seconds.  These items are
C      assumed to be components on a calendar that contains no
C      leapseconds (i.e. every day is assumed to have exactly 86400
C      seconds).
C
C      TPARSE recognizes a wide range of standard time formats.
C      The examples section contains a list of several common
C      strings that are recognized and their interpretation.
C      TPARSE relies on the lower lever routine TPARTV to
C      interpret the input string.
C
C      Here is a brief summary of some of the basic rules used
C      in the interpretation of strings.
C
C      1)  Unless the substring JD or jd is present the string is
C          assumed to be a calendar format (day-month-year or year and
C          day of year).  If the substring JD or jd is present, the
C          string is assumed to represent a julian date.
C
C      2)  If the julian date specifier is not present, any integer
C          greater than 999 is regarded as being a year specification.
C
C      3)  A dash '-' can represent a minus sign only if it is precedes
C          the first digit in the string and the string contains
C          the julian date specifier (JD).  (No negative years,
C          months, days, etc are allowed).
C
C      4)  Numeric components of a time string must be separated
C          by a character that is not a digit or decimal point.
C          Only one decimal component is allowed.  For example
C          1994219.12819 is sometimes interpreted as the
C          219th day of 1994 + 0.12819 days.  TPARSE does not
C          support such strings.
C
C          No exponential components are allowed.  For example you
C          can't input 1993 Jun 23 23:00:01.202E-4 you have
C          to explicitly list all zeros that follow the decimal
C          point: i.e.  1993 Jun 23 23:00:00.0001202
C
C      5)  The single colon (:) when used to separate numeric
C          components of a string is interpreted as separating
C          Hours, Minutes, and Seconds of time.
C
C      6)  If a double slash (//) or double colon (::) follows
C          a pair of integers, those integers are assumed  to
C          represent the year and day of year.
C
C      7)  A quote followed by an integer less than 100 is regarded
C          as an abbreviated year.  For example: '93 would be regarded
C          as the 93rd year of the reference century.  See TEXPYR
C          for further discussion of abbreviated years.
C
C       8) An integer followed by 'B.C.' or 'A.D.' is regarded as
C          a year in the era associated with that abbreviation.
C
C       9) All dates are regarded as belonging to the extended
C          Gregorian Calendar (the Gregorian calendar is the calendar
C          currently used by western society).  See the routine JUL2GR
C          for  converting from Julian Calendar to the
C          Gregorian Calendar.
C          western society).
C
C      10) When the size of the integer components does not clearly
C          specify a year the following patterns are assumed
C
C          Calendar Format
C
C              Year Month Day
C              Month Day Year
C              Year Day Month
C
C              Where Month is the name of a month, not its numeric
C              value.
C
C              When integer components are separated by slashes (/)
C              as in 3/4/5.  Month, Day, Year is assumed (2005 March 4)
C
C           Day of Year Format.
C
C              If a day of year marker is present (// or ::) the
C              pattern
C
C              I-I// or I-I:: (where I stands for and integer)
C              is interpreted as Year Day-of-Year. However, I-I/ is
C              regarded as ambiguous.
C
C      To understand the complete list of strings that can be understood
C      by TPARSE you need to examine TPARTV and read the appendix to
C      the TIME required reading entitled "Parsing Time Strings"
C
C      TPARSE does not support the specification of time system
C      such as TDT or TDB; AM/PM specifications of time; or time
C      zones (such as PDT, UTC+7:20, etc.).
C
C      If some part of the time string is not recognized or if
C      the meaning of the components are not clear, an error string
C      is constructed that explains the problem with the string.
C
C      Since the routine is works by breaking the input string into
C      a sequence of tokens whose meanings are determined by position
C      and magnitude, you can supply strings such as 1993 FEB 35 and
C      have this correctly interpreted as March 7, 1993.  However,
C      this default action can be modified so that only "proper"
C      calendar dates and times are recognized.  To do this call
C      the routine TPARCH as shown below:
C
C         CALL TPARCH ( 'YES' )
C
C      This will cause the routine to treat dates and times with
C      components outside the normal range as errors.
C
C      To return to the default behavior
C
C         CALL TPARCH ( 'NO' )
C
C$ Examples
C
C      The following are examples of valid inputs to TPARSE:
C
C
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
C                                      Sec   --- Sec
C
C      * The default interpretation of a year that has been abbreviated
C      with a leading quote as in 'xy (such as '92) is to treat
C      the year as 19xy if xy > 68 and to treat it is 20xy otherwise.
C      Thus '70 is interpreted as 1970 and '67 is treated as 2067.
C      However, you may change the "split point" and centuries through
C      use of the SPICE routine TSETYR which is an entry point in
C      the SPICE module TEXPYR.  See that routine for a discussion of
C      how you may reset the split point.
C
C      ** All epochs are regarded as belonging to the Gregorian
C      calendar.  We formally extend the Gregorian calendar backward
C      and forward in time for all epochs.  If you have epochs belonging
C      to the Julian Calendar, consult the routines TPARTV and JUL2GR
C      for a discussion concerning conversions to the Gregorian
C      calendar and ET.
C
C      +  When a day of year format or calendar format string is
C      input and neither of integer components of the date
C      is greater than 1000, the first integer
C      is regarded as being the year.
C
C      Any integer greater than 1000
C      is regarded as a year specification. Thus 1001-1821//12:28:28
C      is interpreted as specifying two years and will be rejected
C      as ambiguous.
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      J.M. Lynch      (JPL)
C      W.M. Owen       (JPL)
C      M.J. Spencer    (JPL)
C      I.M. Underwood  (JPL)
C      W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.0.1, 18-MAY-2010 (BVS)
C
C        Removed "C$" marker from text in the header.
C
C-    SPICELIB Version 5.0.0, 30-DEC-1997 (WLT)
C
C        The routine was modified to compensate for the inability
C        of the Muller-Wimberly formula to handle negative years
C        (that is years prior to 1 AD.
C
C        Comments concerning the default century used for two
C        digit years were upgraded.
C
C-    SPICELIB Version 4.0.0, 8-APR-1996 (WLT)
C
C        All of the token recognition and parsing was moved
C        into the routine TPARTV.  The entry point TPARCH
C        was moved to the routine TCHECK.
C
C        This routine now merely assembles the
C        parsed components to produce SP2000.
C
C        The number of strings now recognized has been greatly
C        increased.  However, the interpretation given to
C        strings such as  31 Jan 32 has been changed.
C
C-    SPICELIB Version 3.0.0, 30-JUL-1993 (WLT)
C
C        The entry point TPARCH was added so that users may
C        restrict the set of input calendar strings to those
C        that are in proper form.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 18-NOV-1991 (MJS)
C
C        TPARSE no longer accepts a blank time string.
C
C-    SPICELIB Version 1.0.1, 26-MAR-1991 (JML)
C
C        In the Detailed_Input section of the header, the
C        description of how default values are assigned to
C        tokens in STRING was clarified.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU)
C
C-&
 
C$ Index_Entries
C
C     parse a utc time string
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 5.0.0, 30-DEC-1997 (WLT)
C
C        The routine was modified to compensate for the inability
C        of the Muller-Wimberly formula to handle negative years
C        (that is years prior to 1 AD.
C
C        Comments concerning the default century used for two
C        digit years were upgraded.
C
C-    SPICELIB Version 4.0.0, 8-APR-1996 (WLT)
C
C        All of the token recognition and parsing was moved
C        into the routine TPARTV.  The entry point TPARCH
C        was moved to the routine TCHECK.
C
C        This routine now merely assembles the
C        the parsed components to produce SP2000.
C
C-    SPICELIB Version 3.0.0, 30-JUL-1993 (WLT)
C
C        The entry point TPARCH was added so that users may
C        restrict the set of input calendar strings to those
C        that are in proper form.
C
C-    SPICELIB Version 2.0.0, 18-NOV-1991 (MJS)
C
C        TPARSE no longer accepts a blank time string. Prior to
C        this fix, TPARSE interpreted a blank time string to be
C        -1577880000.000 UTC seconds (1 JAN 1950 00:00:00).
C
C-    SPICELIB Version 1.0.1, 26-MAR-1991 (JML)
C
C        In the Detailed_Input section of the header, the
C        description of how default values are assigned to
C        tokens in STRING was clarified.
C
C        NAIFers are accustomed to specifying day of year
C        formats of UTC strings in the following form:
C
C           1986-247 // 12:00:00
C
C        This revision to the header states explicitly that
C        the // is a blank token which results in the default
C        value being assigned to the month token.  The previous
C        version of the header implied that tokens could be left
C        out or "missing" from the string, and that default values
C        would automatically be assigned.  This works only for
C        tokens missing from the right end of the string.  For
C        default values to be assigned to tokens missing from the
C        middle of a UTC string, consecutive delimiters such as
C        // or :: must be included.
C
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD
 
C
C     Parameters
C
      INTEGER               ERA
      PARAMETER           ( ERA    =  1 )
 
      INTEGER               WDAY
      PARAMETER           ( WDAY   = ERA    + 1 )
 
      INTEGER               ZONE
      PARAMETER           ( ZONE   = WDAY   + 1 )
 
      INTEGER               AMPM
      PARAMETER           ( AMPM   = ZONE   + 1 )
 
      INTEGER               SYSTEM
      PARAMETER           ( SYSTEM = AMPM   + 1 )
 
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
C
C     Local variables
C
 
      CHARACTER*(5)         TYPE
      CHARACTER*(8)         MODIFY   ( 5 )
 
      CHARACTER*(LNSIZE)    PICTUR
 
      DOUBLE PRECISION      TVEC     ( 10 )
 
      INTEGER               DAY
      INTEGER               MONTH
      INTEGER               NTVEC
      INTEGER               Q
      INTEGER               TEMP
      INTEGER               YEAR
 
      LOGICAL               ADJUST
      LOGICAL               MODS
      LOGICAL               OK
      LOGICAL               SUCCES
      LOGICAL               YABBRV
 
C
C     All the work of taking apart the string is handled
C     by TPARTV.
C
      ERROR  = ' '
      SUCCES = .TRUE.
 
      CALL TPARTV ( STRING,
     .              TVEC,   NTVEC, TYPE,
     .              MODIFY, MODS,  YABBRV, SUCCES,
     .              PICTUR, ERROR )
 
      IF ( .NOT. SUCCES ) THEN
         RETURN
      END IF
 
C
C     We are not going to support all of the various
C     time string modifiers that can be parsed.
C
      IF ( MODS ) THEN
 
         IF ( MODIFY(SYSTEM) .NE. ' ' ) THEN
 
            ERROR = 'TPARSE does not support the specification '
     .      //      'of a time system in a string.  The time '
     .      //      'system # was specified. '
            CALL REPMC ( ERROR, '#', MODIFY(SYSTEM), ERROR )
            RETURN
 
         ELSE IF ( MODIFY(ZONE) .NE. ' ' ) THEN
 
            ERROR = 'TPARSE does not support the specification '
     .      //      'of a time zone in a time string.  The time '
     .      //      'zone ''#'' was specified. '
            CALL REPMC ( ERROR, '#', MODIFY(ZONE), ERROR )
            RETURN
 
         ELSE IF ( MODIFY(AMPM) .NE. ' ' ) THEN
 
            ERROR = 'TPARSE does not support the AM/PM '
     .      //      'conventions for time strings. '
            RETURN
 
         END IF
 
      END IF
 
 
 
      IF ( TYPE .EQ. 'JD' ) THEN
C
C        Nothing to do but convert TVEC(1).
C
 
         SP2000 = (TVEC(1)-J2000()) * SPD()
 
      ELSE IF ( TYPE .EQ. 'YMD' .OR. TYPE .EQ. 'YD' ) THEN
 
 
         CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
 
         IF ( .NOT. OK ) THEN
            RETURN
         END IF
C
C        If we have day of year format, we move it into the
C        month-day of month format.
C
         IF ( TYPE .EQ. 'YD' ) THEN
            TVEC(6) = TVEC(5)
            TVEC(5) = TVEC(4)
            TVEC(4) = TVEC(3)
            TVEC(3) = TVEC(2)
            TVEC(2) = 1.0D0
         END IF
C
C        Get the year month and day as integers.
C
         YEAR   = NINT( TVEC(1) )
         MONTH  = NINT( TVEC(2) )
         DAY    = NINT( TVEC(3) )
C
C        Fix up the year as needed.
C
         IF ( MODIFY(ERA) .EQ. 'B.C.' ) THEN
 
            YEAR = 1 - YEAR
 
         ELSE IF ( MODIFY(ERA) .EQ. 'A.D.' ) THEN
C
C           Do nothing.
C
         ELSE IF ( YEAR .LT. 100 ) THEN
 
            CALL TEXPYR ( YEAR )
 
         END IF
 
C
C        Apply the Muller-Wimberly formula and then tack on
C        the seconds.
C
         IF ( YEAR .LT. 1 ) THEN
C
C           The Muller-Wimberly formula doesn't work for years
C           less than 0.  So we boost the year by an appropriate
C           multiple of 400 and then subtract the appropriate
C           number of days later.
C
            ADJUST = .TRUE.
            TEMP   =  YEAR
 
            CALL RMAINI ( TEMP, 400, Q, YEAR )
 
            YEAR = YEAR  + 400
            Q    = Q-1
 
         ELSE
 
            ADJUST = .FALSE.
 
         END IF
 
 
         DAY    = 367*YEAR - 7*(YEAR+(MONTH+9)/12)/4
     .                     - 3*((YEAR+(MONTH-9)/7)/100+1)/4
     .                     + 275*MONTH/9 + DAY - 730516
 
         IF ( ADJUST ) THEN
C
C           Adjust DAY by the appropriate multiple of 400 years.
C
            DAY = DAY + Q*(400*365 + 97)
 
         END IF
 
         SP2000 =  ( DBLE (DAY) - 0.5D0 ) * SPD()
     .            +  3600.0D0 * TVEC(4)
     .            +    60.0D0 * TVEC(5)
     .            +             TVEC(6)
 
      ELSE
C
C        We've already covered all the bases we are planning to
C        cover in this routine.  Any other case is regarded as an
C        error.
C
 
         ERROR = 'The only type of time strings that are handled '
     .   //      'by TPARSE are ''JD'', ''YMD'' and ''YD'' '
     .   //      '(year day-of-year).  You''ve entered a string '
     .   //      'of the type #. '
 
         CALL REPMC ( ERROR, '#', TYPE, ERROR )
 
 
      END IF
 
 
 
      RETURN
 
      END
