C$Procedure      ET2UTC ( Ephemeris Time to UTC )
 
      SUBROUTINE ET2UTC ( ET, FORMAT, PREC, UTCSTR )
 
C$ Abstract
C
C      Convert an input time from ephemeris seconds past J2000
C      to Calendar, Day-of-Year, or Julian Date format, UTC.
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
C      TIME
C
C$ Keywords
C
C      TIME
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FORMAT
      INTEGER               PREC
      CHARACTER*(*)         UTCSTR
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ET         I   Epoch, given in ephemeris seconds past J2000.
C      FORMAT     I   Format of output epoch.
C      PREC       I   Digits of precision in fractional seconds or days.
C      UTCSTR     O   Output time string, UTC.
C
C$ Detailed_Input
C
C      ET          is the input epoch, ephemeris seconds past J2000.
C
C      FORMAT      is the format of the output time string. It may be
C                  any of the following:
C
C
C                    'C'      Calendar format, UTC.
C
C                    'D'      Day-of-Year format, UTC.
C
C                    'J'      Julian Date format, UTC.
C
C                    'ISOC'   ISO Calendar format, UTC.
C
C                    'ISOD'   ISO Day-of-Year format, UTC.
C
C      PREC        is the number of digits of precision to which
C                  fractional seconds (for Calendar and Day-of-Year
C                  formats) or days (for Julian Date format) are to
C                  be computed. If PREC is zero or smaller, no decimal
C                  point is appended to the output string. If PREC is
C                  greater than 14, it is treated as 14.
C
C$ Detailed_Output
C
C      UTCSTR      is the output time string equivalent to the input
C                  epoch, in the specified format.  Some examples are
C                  shown below.
C
C                        'C'      '1986 APR 12 16:31:09.814'
C                        'D'      '1986-102 // 16:31:12.814'
C                        'J'      'JD 2446533.18834276'
C                        'ISOC'   '1987-04-12T16:31:12.814'
C                        'ISOD'   '1987-102T16:31:12.814'
C
C                  If an error occurs, UTCSTR is not changed.
C
C                  Fractional seconds, or for Julian dates, fractional
C                  days, are rounded to the precision level specified
C                  by the input argument PREC.
C
C                  UTCSTR should be declared to be at least
C                  20 + PREC characters in length to ensure
C                  sufficient room to hold calendar strings
C                  for modern epochs.  For epochs prior to
C                  1000 A.D. at least 24 + PREC characters in
C                  length are required to hold the output
C                  calendar string.
C
C                  For epochs prior to 1000 A.D. Jan 1 calendar
C                  and day of year formats are returned with the
C                  era (A.D. or B.C.) attached to the year.  For
C                  example
C
C                       '877 A.D. MAR 17 13:29:11.829'
C                       '471 B.C. Jan 01 12:00:00.000'
C                       '471 B.C. 001 // 12:00:00.000'
C
C                  ISO formats do not support the inclusion of an era.
C                  For years prior to 1 A.D. an error will be signaled
C                  if ISO format has been requested.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the format for the output string is not recognized, the
C        error SPICE(INVALIDTIMEFORMAT) is signaled.
C
C     2) If PREC is less than or equal to zero, it is treated as
C        zero.  If PREC is greater than 14, it is treated as 14.
C
C     3) If one of the ISO formats is specified (ISOC or ISOD) but
C        the year corresponding to ET is prior to 1  A.D. on the
C        Gregorian Calendar, the error SPICE(YEAROUTOFRANGE) will
C        be signaled.
C
C     4) Epochs prior to 15 Oct, 1582 on the Gregorian calendar (the
C        calendar commonly used in western societies) are returned in
C        the "extended" Gregorian Calendar.  To convert epochs to the
C        Julian calendar see the entry point GR2JUL in the routine
C        JUL2GR.
C
C     5) This routine does not attempt to account for variations
C        in the length of the second that were in effect prior
C        to Jan 1, 1972.  For days prior to that date, we assume
C        there are exactly 86400 ephemeris seconds. Consequently
C        the UTC Gregorian calendar strings produced for epochs
C        prior to Jan 1, 1972 differ from the corresponding
C        TDB calendar strings by approximately 41.18 seconds.
C        (TDB Gregorian calendar strings are produced by the
C        routine ETCAL).
C
C     6) If a leapseconds kernel has not been loaded prior to calling
C        this routine, an error will be signaled by a routine in the
C        call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine handles the task of converting a double precision
C     representation of an epoch to a character string suitable for
C     human consumption.  The more general routine TIMOUT may also be
C     used to convert ET to time strings.
C 
C$ Examples
C
C     Let the value of ET be -527644192.5403653 ephemeris seconds
C     past J2000. Assuming that the nominal values in the kernel pool
C     have not been altered, the following calls
C
C        CALL ET2UTC ( ET, 'C', 0, UTCSTR )
C        CALL ET2UTC ( ET, 'C', 3, UTCSTR )
C        CALL ET2UTC ( ET, 'D', 5, UTCSTR )
C        CALL ET2UTC ( ET, 'J', 7, UTCSTR )
C
C     produce the following output strings
C
C        '1983 APR 13 12:09:14'
C        '1983 APR 13 12:09:14.274'
C        '1983-103 // 12:09:14.27400'
C        'JD 2445438.0064152'
C
C     respectively.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     Jesperson and Fitz-Randolph, From Sundials to Atomic Clocks,
C     Dover Publications, New York, 1977.
C
C$ Author_and_Institution
C
C     C.H. Acton      (JPL)
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     W.M. Owen       (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.4, 06-APR-2009 (NJB)
C
C        Header was updated to state that fractional
C        seconds or days are rounded in the output
C        string.
C
C-    SPICELIB Version 3.0.3, 28-JAN-2008 (BVS)
C
C        Fixed typo in the ISOC example string in Detailed_Output.
C
C-    SPICELIB Version 3.0.2, 29-JUL-2003 (NJB) (CHA)
C
C        Various header changes were made to improve clarity and 
C        more fully explain the routine's functionality.
C
C-    SPICELIB Version 3.0.1, 14-SEP-2000 (EDW)
C
C        Added FAILED check after TTRANS call during the calendar "C"
C        format processing to catch failure signal from TTRANS. 
C        Lack of this check caused CSPICE based programs to core dump
C        if ET2UTC was called without a leapseconds kernel while
C        error action was set to RETURN.
C
C-    SPICELIB Version 3.0.0, 13-MAR-1996 (WLT)
C
C        The construction of the numerical components of the
C        output string are now handled by the SPICELIB routine
C        TTRANS.
C
C        In addition the routine now supports the ISO formats and
C        the era associated with an epoch (B.C. or A.D.) in non
C        ISO formats.
C
C-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
C
C        Removed some potential compile warnings that could be caused
C        by truncation of double precision values to integers through
C        a direct assignment. The direct assignment has been replaced
C        with a call to the intrinsic function IDINT.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 21-MAR-1991 (NJB) (JML)
C
C        Two bugs involving rounding errors were corrected.  One of
C        the bugs caused conversion errors of magnitude as large as
C        1 second.  See $Revisions for details.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU)
C
C-&
 
C$ Index_Entries
C
C     ephemeris time to utc
C
C-&
 
C$ Revisions
C
C-      SPICELIB Version 3.0.0, 13-MAR-1995 (WLT)
C
C          The construction of the numerical components of the
C          output string are now handled by the SPICELIB routine
C          TTRANS.
C
C          In addition the routine now supports the era associated
C          with an epoch (B.C. or A.D.)
C
C-      SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
C
C          Removed some potential compile warnings that could be caused
C          by truncation of double precision values to integers through
C          a direct assignment. The direct assignment has been replaced
C          with a call to the intrinsic function IDINT.
C
C-      SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C          Comment section for permuted index source lines was added
C          following the header.
C
C-      SPICELIB Version 2.0.0, 21-MAR-1991 (NJB) (JML)
C
C       1) In the previous version of this routine, the algorithm
C          that was used permitted inconsistent conversion of the
C          integer and fractional parts of the UTC value corresponding
C          to the input ET value.
C
C          In the case where rounding the double precision UTC time
C          corresponding to the input ET value to PREC decimal places
C          resulted in a carry (to the integer portion of the UTC
C          value), the integer portion of the UTC value was treated
C          correctly, but the fraction was not always rounded correctly.
C          The specific case where the problem occurred was when the
C          input ET value mapped to a UTC time having a fractional
C          part that rounded up to 1.0 when rounded PREC decimal places,
C          but that did not round up to 1.0 when rounded to the nearest
C          PREC+1 decimal places.  The set of such fractions can be
C          represented as
C
C             { 1 - EPSILON :      EPSILON  <  5 * ( 10 ** -(PREC+1) )
C                                           -
C                             and
C
C                                  EPSILON  >  5 * ( 10 ** -(PREC+2) )
C
C                                                                     }
C
C          For example, if the input ET mapped to the UTC time
C
C             2 JAN 1991 00:34:12.99994,
C
C          then a call to this routine with PREC set to 3 would result
C          in the output
C
C             2 JAN 1991 00:34:13.999
C
C          instead of the correct value
C
C             2 JAN 1991 00:34:13.000
C
C          On the other hand, if the input ET mapped to the UTC time
C
C             2 JAN 1991 00:34:12.99996,
C
C          then a call to this routine with PREC set to 3 would result
C          in the correct output.
C
C
C          This error was apparently difficult to generate:  it has
C          never been reported by any SPICELIB users, and was eventually
C          discovered by NAIF staff.
C
C
C
C       2) The second bug is somewhat less severe, as far as the
C          magnitude of the error is concerned.  However, it's easier
C          to generate this error.  Namely, in some cases, the
C          fractional part of the input ET value is rounded to PREC
C          SIGNIFICANT DIGITS, rather than to PREC decimal places.
C          The effect of this is that the fraction is occasionally
C          truncated rather than rounded.  For example, the ET value
C          equivalent to the UTC string
C
C             1991 JAN 2 00:34:12.0009
C
C          would be converted to
C
C             1991 JAN 2 00:34:12.000
C
C          instead of the correct value
C
C             1991 JAN 2 00:34:12.001
C
C          when the input argument PREC was set equal to 3.
C
C
C
C          The modifications made to solve these problems are as
C          follows:
C
C             1)  The input ET value, after conversion to `UTC seconds
C                 past 2000', is broken up into the sum of a whole
C                 number of seconds and a non-negative, fractional
C                 number of seconds.  The fact that the fractional
C                 part is non-negative simplifies the conversion of the
C                 fraction.
C
C             2)  The fraction is rounded to PREC decimal places---
C                 that is, to the nearest integer multiple of
C                 10**(-PREC).  If the rounding results in a carry,
C                 the whole number portion of the time value is
C                 incremented by 1 second.  After this step, the
C                 whole number of seconds correctly accounts for
C                 any necessary rounding of the fraction.
C
C             3)  The whole number portion of the time value is passed
C                 through the inverse Muller-Wimberly algorithm to
C                 obtain years, months, days, hours, minutes, and
C                 whole seconds.  A small fraction is added to the
C                 whole number to prevent round-off error from occurring
C                 when divisions are performed.
C
C             4)  The fraction is converted to a string using the
C                 SPICELIB routine DPSTRF.  To ensure that DPSTRF
C                 produces an output string containing PREC decimal
C                 places, an integer is added to the fraction value
C                 before supplying it to DPSTRF.  This integer
C                 `anchors' the first significant digit of the input
C                 value in the units place.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      UNITIM
      LOGICAL               FAILED
 
C
C     Local Functions (Statement Functions)
C
      INTEGER               NDIGIT
 
C
C     Local variables
C
 
      CHARACTER*(4)         FMT
      CHARACTER*(80)        ENDSTR
      CHARACTER*(80)        STR
      CHARACTER*(80)        FRACT
 
      DOUBLE PRECISION      TAI
      DOUBLE PRECISION      FRCSEC
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      TVEC   ( 8 )
      DOUBLE PRECISION      WHLSEC
 
      INTEGER               DAY
      INTEGER               HOUR
      INTEGER               MINUTE
      INTEGER               MONTH
      INTEGER               SECOND
      INTEGER               YEAR
 
      INTEGER               MYPREC
 
      INTEGER               BDAY
      INTEGER               BHR
      INTEGER               BMN
      INTEGER               BMONTH
      INTEGER               BSC
      INTEGER               EDAY
      INTEGER               EHR
      INTEGER               EMN
      INTEGER               EMONTH
      INTEGER               ESC
 
      INTEGER               I
 
      CHARACTER*3           MTHNAM   ( 12 )
 
 
C
C     Save everything between calls
C
      SAVE
 
C
C     Initial values
C
      DATA         MTHNAM       / 'JAN', 'FEB', 'MAR', 'APR',
     .                            'MAY', 'JUN', 'JUL', 'AUG',
     .                            'SEP', 'OCT', 'NOV', 'DEC'  /
 
 
 
C
C     The function NDIGIT gives the number of digits required to
C     display a non-negative integer that is less than 10000
C
      NDIGIT(DAY) = MIN(1,DAY/1000) + MIN(1,DAY/100) + MIN(1,DAY/10) + 1
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ET2UTC' )
 
C
C     Convert FORMAT to uppercase for ease of comparison. Make sure it's
C     one of the recognized formats.
C
      CALL UCASE ( FORMAT, FMT )
 
      IF (       FMT .NE. 'J'
     .     .AND. FMT .NE. 'C'
     .     .AND. FMT .NE. 'D'
     .     .AND. FMT .NE. 'ISOD'
     .     .AND. FMT .NE. 'ISOC' ) THEN
 
         CALL SETMSG ( 'ET2UTC: Format specification for output '
     .   //            'time string is not recognized. Valid '
     .   //            'specifications are: ''C'', '
     .   //            '''D'', ''J'', ''ISOC'', or ''ISOD''. '
     .   //            'The supplied format was ''#''. ' )
 
         CALL ERRCH  ( '#', FORMAT )
         CALL SIGERR ( 'SPICE(INVALIDTIMEFORMAT)' )
         CALL CHKOUT ( 'ET2UTC' )
         RETURN
 
      END IF
C
C     Force PREC into an acceptable range
C
      MYPREC = MAX ( 0 , MIN ( 14, PREC ))
 
C
C     If the output is Julian Date, we're ready to go. Remember that
C     the day part of Julian Date already has seven digits built in.
C
      IF ( FMT .EQ. 'J' ) THEN
 
         TVEC(1) = ET
 
         CALL TTRANS ( 'TDB',  'JDUTC',        TVEC )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ET2UTC' )
            RETURN
         END IF
  
         CALL DPSTRF ( TVEC(1), MYPREC+7, 'F', STR  )
         CALL PREFIX ( 'JD', 0,                STR  )
 
         UTCSTR = STR
 
         CALL CHKOUT ( 'ET2UTC' )
         RETURN
 
      END IF
C
C     If we've dropped past the IF-THEN block above, we need
C     to construct a calendar format string. First thing to
C     do is convert from ET to TAI.
C
 
      TAI = UNITIM ( ET, 'TDB', 'TAI' )
 
C
C     We're going to break up TAI into an integer and a
C     fractional part.  The integer will be the greatest
C     integer less than or equal to TAI, and the fraction
C     will be the difference between TAI and the integer
C     part.  The fraction will always be in the interval
C
C        [0, 1)
C
C     After making this decomposition, we'll adjust the integer
C     and fraction to take rounding into account.  The result
C     of the adjustment is that the fraction will be an integer
C     number of time units of length 10**(-MYPREC) seconds, where
C     the integer is in the range [0, (10**MYPREC)-1].  If the
C     fraction rounds up to 1, the fraction will be set to zero,
C     and the whole number portion of TAI will be incremented.
C
C     Since the integers involved may be too large to represent
C     using the INTEGER data type, we'll represent them with
C     double precision numbers.  We'll use the intrinsic ANINT
C     function to keep round-off from creeping into these d.p.
C     numbers representing integers.
C
C     Find the greatest integer less than or equal to TAI.
C     Recall that INT truncates toward the origin.  If TAI
C     is negative and is not already an integer, the result we
C     want is one less than AINT(TAI).
C
      WHLSEC = AINT ( TAI )
 
      IF (        ( TAI .LT. 0.0D0  )
     .      .AND. ( TAI .NE. WHLSEC )  )  THEN
 
         WHLSEC = WHLSEC - 1.0D0
 
      END IF
 
C
C     The fractional part of TAI must be rounded to the
C     nearest multiple of 10**(-MYPREC).  Fractions that are
C     equidistant from two multiples of 10**(-MYPREC) are
C     rounded up.
C
C     To accomplish the rounding, we scale the fraction by
C     10**MYPREC.
C
C
      SCALE  = ANINT (  10.0D0 ** MYPREC )
      FRCSEC = ANINT (  SCALE * ( TAI - WHLSEC )  )
 
C
C     If a carry occurred, the fraction becomes zero, and
C     we must increment WHLSEC.
C
      IF (  FRCSEC  .EQ.  SCALE  ) THEN
 
         WHLSEC = WHLSEC + 1.0D0
         FRCSEC = 0.0D0
 
      END IF
 
      FRCSEC = FRCSEC / SCALE
 
C
C     Now, we let TTRANS handle the transformation to
C     the desired components for output.
C
C     FRCSEC will be converted to a string containing MYPREC digits.
C     This will be done later on when the output string is
C     assembled.
C
      TVEC(1) = WHLSEC
 
      IF ( FMT .EQ. 'C' .OR. FMT .EQ. 'ISOC' ) THEN
 
         CALL TTRANS ( 'TAI', 'YMD', TVEC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ET2UTC' )
            RETURN
         END IF

         YEAR        = NINT( TVEC(1) )
         MONTH       = NINT( TVEC(2) )
         DAY         = NINT( TVEC(3) )
         HOUR        = NINT( TVEC(4) )
         MINUTE      = NINT( TVEC(5) )
         SECOND      = NINT( TVEC(6) )
C
C        The beginning of the string is going to be the year.
C        Depending upon the size of the year, it may or
C        may not have an era label.  However the end of the
C        string has a fixed size.  We set up that portion of the
C        string now.  First fill in the month...
C
         IF ( FMT .EQ. 'C' ) THEN
            ENDSTR      = ' MMM 00 00:00:00'
            ENDSTR(2:4) = MTHNAM(MONTH)
C
C           ... and then fill in the day portion of the string.
C
            EDAY = 7
            BDAY = EDAY - NDIGIT(DAY) + 1
 
 
            CALL INTSTR ( DAY, ENDSTR(BDAY:EDAY) )
            EHR  = 10
            EMN  = 13
            ESC  = 16
 
         ELSE
 
            ENDSTR = '-0M-00T00:00:00'
            EDAY   =  6
            BDAY   =  EDAY - NDIGIT(DAY) + 1
 
            EMONTH = 3
            BMONTH = EMONTH - NDIGIT(MONTH) + 1
            CALL INTSTR ( MONTH, ENDSTR(BMONTH:EMONTH) )
            CALL INTSTR ( DAY,   ENDSTR(BDAY:EDAY)     )
            EHR  =  9
            EMN  = 12
            ESC  = 15
 
         END IF
 
      ELSE
C
C        We must have day of year format.  Convert TAI to that
C        format.
C
 
         CALL TTRANS ( 'TAI', 'YD', TVEC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ET2UTC' )
            RETURN
         END IF
         
         YEAR   = NINT( TVEC(1) )
         MONTH  = 1
         DAY    = NINT( TVEC(2) )
         HOUR   = NINT( TVEC(3) )
         MINUTE = NINT( TVEC(4) )
         SECOND = NINT( TVEC(5) )
C
C        As in the previous case, the end of the output string will
C        have a fixed size.  We fill in the day portion of the string
C        now.  Note that we have set things up so that the hour,
C        minutes and seconds appear in the same location in both
C        day of year and calendar format of strings.
C
         IF ( FMT .EQ. 'D' ) THEN
            ENDSTR = '-000 // 00:00:00'
            EDAY   = 4
            BDAY   = EDAY - NDIGIT(DAY) + 1
 
            CALL INTSTR ( DAY, ENDSTR(BDAY:EDAY) )
            EHR  = 10
            EMN  = 13
            ESC  = 16
         ELSE
            ENDSTR = '-000T00:00:00'
            EDAY   = 4
            BDAY   = EDAY - NDIGIT(DAY) + 1
 
            CALL INTSTR ( DAY, ENDSTR(BDAY:EDAY) )
            EHR  = 7
            EMN  = 10
            ESC  = 13
         END IF
      END IF
C
C     Fill out the hours, minutes and integer portion of
C     seconds in the output string.
C
      BHR  = EHR - NDIGIT(HOUR) + 1
      BMN  = EMN - NDIGIT(MINUTE) + 1
      BSC  = ESC - NDIGIT(SECOND) + 1
 
      CALL INTSTR ( HOUR,   ENDSTR(BHR:EHR) )
      CALL INTSTR ( MINUTE, ENDSTR(BMN:EMN) )
      CALL INTSTR ( SECOND, ENDSTR(BSC:ESC) )
C
C     Append the fractional part of the seconds component.
C
      IF ( MYPREC .GT. 0 ) THEN
 
C
C        DPSTRF gives MYPREC significant digits in the output,
C        not necessarily MYPREC digits to the right of the
C        decimal point.  We will add a one-digit integer to
C        FRCSEC to `anchor' the first significant digit of
C        FRCSEC in a known place.  That way, we can get DPSTRF
C        to give us a known number of digits after the decimal
C        point.
C
C        The integer part of FRCSEC will not affect the output
C        string.
C
         FRCSEC = FRCSEC + 1.0D0
 
         CALL DPSTRF ( FRCSEC, MYPREC+1, 'F', FRACT )
 
         I = INDEX ( FRACT, '.' )
         ENDSTR(ESC+1: ) = FRACT(I:I+MYPREC)
 
      END IF
C
C     The end of the time string is now complete.  We need to
C     construct the year portion of the string. We are going to
C     append an era if the year is before 1000 A.D. Note that
C     we make sure the first character in the ending string
C     is a blank (' ') if the era is to be attached.  Otherwise
C     we'd get confusing day of year formats like
C     999 A.D.-019 // 12:13:18.
C
      IF ( YEAR .GE. 1000 ) THEN
 
         CALL   INTSTR( YEAR, STR )
 
      ELSE IF ( YEAR .GT. 0 ) THEN
 
         CALL INTSTR ( YEAR,      STR )
 
         IF ( FMT .EQ. 'C' .OR. FMT .EQ. 'D' ) THEN
            CALL SUFFIX ( 'A.D.', 1, STR )
            ENDSTR(1:1) = ' '
         END IF
 
      ELSE IF ( YEAR .LE. 0 ) THEN
 
         IF ( FMT .EQ. 'C' .OR. FMT .EQ. 'D' ) THEN
            YEAR = -YEAR + 1
 
            CALL INTSTR ( YEAR,      STR )
            CALL SUFFIX ( 'B.C.', 1, STR )
 
            ENDSTR(1:1) = ' '
         ELSE
 
            YEAR = -YEAR + 1
            CALL SETMSG ( 'The year of the ET epoch supplied is '
     .      //            '# B.C.  Years in this era are not '
     .      //            'supported in ISO format. ' )
            CALL ERRINT ( '#', YEAR )
            CALL SIGERR ( 'SPICE(YEAROUTOFRANGE)'  )
            CALL CHKOUT ( 'ET2UTC' )
            RETURN
 
         END IF
 
      END IF
 
C
C     Finally append the ENDSTR to STR to get the fully formatted
C     string.
C
      CALL SUFFIX ( ENDSTR, 0, STR )
 
      UTCSTR = STR
 
      CALL CHKOUT ( 'ET2UTC' )
      RETURN
 
 
      END
