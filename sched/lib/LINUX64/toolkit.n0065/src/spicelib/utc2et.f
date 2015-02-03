C$Procedure      UTC2ET ( UTC to Ephemeris Time )
 
      SUBROUTINE UTC2ET ( UTCSTR, ET )
      IMPLICIT NONE
 
C$ Abstract
C
C      Convert an input time from Calendar or Julian Date format, UTC,
C      to ephemeris seconds past J2000.
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
C      TIME, KERNEL
C
C$ Keywords
C
C      TIME
C
C$ Declarations
 
      CHARACTER*(*)       UTCSTR
      DOUBLE PRECISION    ET
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      UTCSTR     I   Input time string, UTC.
C      ET         O   Output epoch, ephemeris seconds past J2000.
C
C$ Detailed_Input
C
C      UTCSTR      is an input time string, containing a Calendar or
C                  Julian Date, UTC. Any input string acceptable to the
C                  routine TPARTV are acceptable to UTC2ET. The length
C                  of UTCSTR should not exceed 80 characters.
C
C$ Detailed_Output
C
C      ET          is the equivalent of UTCSTR, expressed in ephemeris
C                  seconds past J2000. If an error occurs, or if the
C                  input string is ambiguous, ET is not changed.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      1) If the input time string is ambiguous, the error
C         SPICE(INVALIDTIMESTRING) is signalled.
C
C      2) This routine does not attempt to account for variations
C         in the length of the second that were in effect prior
C         to Jan 1, 1972.  For days prior to that date, we assume
C         there are exactly 86400 ephemeris seconds.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      This routine handles that task of converting strings
C      representing epochs in the UTC system to ephemeris seconds
C      (TDB) past the epoch of the J2000 frame.
C
C      Although this routine is intended primarily for the
C      conversion of epochs during the "space age" it may also
C      be used to convert much earlier epochs.  However, before
C      using this routine to convert epochs prior to 1972
C      you must be sure that the assumptions made by in the
C      implementation are consistent with the accuracy of
C      the input calendar string.
C
C      As noted in the "Exceptions" section above, this routine
C      does not attempt to account for variations in the
C      length of the second that were used prior to Jan 1, 1972.
C      Instead each "UTC" day prior to Jan 1, 1972 is assumed
C      to have exactly 86400 TDT seconds.
C
C  Ancient Epochs
C  --------------
C
C      The calendar used today, the Gregorian calendar, has its
C      initial epoch on 15 October, 1582.  Prior to that epoch the
C      Julian calendar was used for the recording of epochs.
C      October 15, 1582 (Gregorian) corresponds to
C      October 05, 1582 (Julian).  From the point of view of the
C      implementation of this routine, all epochs belong to the
C      Gregorian calendar extended indefinitely backward in time.
C      If you need to obtain ephemeris seconds past the J2000 epoch
C      from Julian Calendar strings, we suggest that
C      you make use of the SPICE routine STR2ET.
C
C$ Examples
C
C      Below is a sampling of some of the time formats that
C      are acceptable as inputs to UTC2ET.  A complete discussion
C      of permissible formats is given in the SPICE routine
C      TPARTV as well as the User's reference file time.req
C      located in the "doc" directory of the toolkit.
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
C      Thus '70 is interpreted as 1970 and '47 is treated as 2047.
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
C
C$ Restrictions
C
C      The conversion between ET and UTC depends on the values in the
C      input kernel pool. The kernel pool should be loaded prior to
C      calling this routine.
C
C      Before using this routine for epochs prior to Jan 1, 1972
C      be sure to check the "Particulars" section to make sure
C      that the assumptions made in this routine are consistent
C      with the accuracy you require for your application.
C
C$ Literature_References
C
C      Jesperson and Fitz-Randolph, From Sundials to Atomic Clocks,
C      Dover Publications, New York, 1977.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C      W.M. Owen       (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-      SPICELIB Version 2.1.0, 05-JAN-1998  (WLT)
C
C          Comments concerning the default century for abbreviated
C          years were updated to reflect changes to TEXPYR.
C
C-      SPICELIB Version 2.0.0, 20-NOV-1996  (WLT)
C
C          About the only thing that is the same in this routine
C          from the previous editions, is that the interface is
C          unchanged.  Nearly everything else has been modified.
C          The routine was modified to make use of TPARTV
C          and TTRANS to handle the task of parsing and
C          computing seconds past 2000 TDB.  This version
C          now handles leap seconds correctly.
C
C-      SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C          Comment section for permuted index source lines was added
C          following the header.
C
C-      SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU)
C
C-&
 
C$ Index_Entries
C
C     utc to ephemeris time
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
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
 
C
C     Local variables
C
 
      INTEGER               LONGLN
      PARAMETER           ( LONGLN = 480 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
 
      CHARACTER*(LNSIZE)    PICTUR
      CHARACTER*(LONGLN)    ERROR
 
      INTEGER               SMWDSZ
      PARAMETER           ( SMWDSZ = 8 )
 
      CHARACTER*(SMWDSZ)    TYPE
      CHARACTER*(SMWDSZ)    MODIFY ( 5 )
 
 
      DOUBLE PRECISION      TVEC   ( 10 )
      INTEGER               NTVEC
      INTEGER               YEAR
 
 
      LOGICAL               SUCCES
      LOGICAL               OK
      LOGICAL               MODS
      LOGICAL               YABBRV
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'UTC2ET' )
C
C     So far we have no errors, the type of input is unknown.
C
      ERROR  = ' '
      TYPE   = ' '
 
C
C     First parse the string and perform the various tests on
C     the validity of its components.
C
      CALL TPARTV ( UTCSTR,
     .              TVEC,   NTVEC, TYPE,
     .              MODIFY, MODS,  YABBRV, SUCCES,
     .              PICTUR, ERROR )
 
      IF ( .NOT. SUCCES ) THEN
         CALL SETMSG ( ERROR )
         CALL SIGERR ( 'SPICE(INVALIDTIMESTRING)' )
         CALL CHKOUT ( 'UTC2ET' )
         RETURN
      END IF
 
C
C     We are not going to allow most of the modifiers in strings.
C
      IF ( MODS ) THEN
 
         IF (       MODIFY(SYSTEM) .NE. ' '
     .        .AND. MODIFY(SYSTEM) .NE. 'UTC' ) THEN
 
            ERROR = 'UTC2ET does not support the specification '
     .      //      'of a time system in a string.  The time '
     .      //      'system # was specified. Try the routine STR2ET.'
            CALL REPMC  ( ERROR, '#', MODIFY(SYSTEM), ERROR )
 
            CALL SETMSG ( ERROR )
            CALL SIGERR ( 'SPICE(INVALIDTIMESTRING)' )
            CALL CHKOUT ( 'UTC2ET' )
            RETURN
 
         ELSE IF ( MODIFY(ZONE) .NE. ' ' ) THEN
 
            ERROR = 'UTC2ET does not support the specification '
     .      //      'of a time zone in a time string.  The time '
     .      //      'zone ''#'' was specified. Try the routine STR2ET.'
            CALL REPMC  ( ERROR, '#', MODIFY(ZONE), ERROR )
            CALL SETMSG ( ERROR )
            CALL SIGERR ( 'SPICE(INVALIDTIMESTRING)' )
            CALL CHKOUT ( 'UTC2ET' )
            RETURN
 
         ELSE IF ( MODIFY(AMPM) .NE. ' ' ) THEN
 
            ERROR = 'UTC2ET does not support the AM/PM '
     .      //      'conventions for time strings. Try the '
     .      //      'routine STR2ET.'
            CALL SETMSG ( ERROR )
            CALL SIGERR ( 'SPICE(INVALIDTIMESTRING)' )
            CALL CHKOUT ( 'UTC2ET' )
            RETURN
 
         END IF
 
      END IF
 
C
C     If parsing the time string went well, we let TTRANS handle
C     the problem of transforming the time vector to TDB.
C
      IF ( TYPE .EQ. 'YMD' .OR. TYPE .EQ. 'YD') THEN
C
C        Check the components of the time vector for reasonableness.
C
         CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
 
         IF ( .NOT. OK ) THEN
            CALL SETMSG ( ERROR )
            CALL SIGERR ( 'SPICE(INVALIDTIMESTRING)' )
         END IF
 
C
C        Fix up the year as needed.
C
         YEAR   = NINT( TVEC(1) )
 
         IF ( MODIFY(ERA) .EQ. 'B.C.' ) THEN
 
            YEAR = 1 - YEAR
 
         ELSE IF ( MODIFY(ERA) .EQ. 'A.D.' ) THEN
C
C           Do nothing.
C
         ELSE IF ( YEAR .LT. 100 ) THEN
 
            CALL TEXPYR ( YEAR )
 
         END IF
 
         TVEC(1) = DBLE(YEAR)
C
C        We are ready for launch, convert the time vector.
C
         CALL TTRANS ( TYPE, 'TDB', TVEC )
         ET = TVEC(1)
 
      ELSE IF ( TYPE .EQ. 'JD' ) THEN
 
         CALL TTRANS ( 'JDUTC', 'TDB', TVEC )
         ET = TVEC(1)
 
      ELSE
C
C        The only way to get here is if we got some unexpected
C        type of time string. Signal an error.
C
         CALL SETMSG ( '# time strings are not handled by '
     .   //            'UTC2ET. ' )
         CALL ERRCH  ( '#', TYPE )
         CALL SIGERR ( 'SPICE(INVALIDTIMESTRING)' )
         CALL CHKOUT ( 'UTC2ET' )
         RETURN
 
      END IF
 
 
      CALL CHKOUT ( 'UTC2ET' )
      RETURN
 
      END
