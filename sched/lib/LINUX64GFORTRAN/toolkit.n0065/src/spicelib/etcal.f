C$Procedure            ETCAL ( Convert ET to Calendar format )
 
      SUBROUTINE ETCAL ( ET, STRING )
 
C$ Abstract
C
C
C     Convert from an ephemeris epoch measured in seconds past
C     the epoch of J2000 to a calendar string format using a
C     formal calendar free of leapseconds.
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
C     TIME
C
C$ Declarations
 
      DOUBLE PRECISION      ET
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris time measured in seconds past J2000.
C     STRING     O   A standard calendar representation of ET.
C
C$ Detailed_Input
C
C     ET       is an epoch measured in ephemeris seconds
C              past the epoch of J2000.
C
C$ Detailed_Output
C
C     STRING   is a calendar string representing the input ephemeris
C              epoch.  This string is based upon extending the
C              Gregorian Calendar backward and forward indefinitely
C              keeping the same rules for determining leap years.
C              Moreover, there is no accounting for leapseconds.
C
C              To be sure that all of the date can be stored in
C              STRING, it should be declared to have length at
C              least 48 characters.
C
C              The string will have the following format
C
C                 year (era) mon day hr:mn:sc.sss
C
C              Where:
C
C                 year --- is the year
C                 era  --- is the chronological era associated with
C                          the date.  For years after 999 A.D.
C                          the era is omitted.  For years
C                          between 1 A.D. and 999 A.D. (inclusive)
C                          era is the string 'A.D.' For epochs
C                          before 1 A.D. Jan 1 00:00:00, era is
C                          given as 'B.C.' and the year is converted
C                          to years before the "Christian Era".
C                          The last B.C. epoch is
C
C                            1 B.C. DEC 31 23:59:59.999
C
C                          The first A.D. epoch (which occurs .001
C                          seconds after the last B.C. epoch) is:
C
C                             1 A.D. JAN 1 00:00:00.000
C
C                          Note: there is no year 0 A.D. or 0 B.C.
C                 mon  --- is a 3-letter abbreviation for the month
C                          in all capital letters.
C                 day  --- is the day of the month
C                 hr   --- is the hour of the day (between 0 and 23)
C                          leading zeros are added to hr if the
C                          numeric value is less than 10.
C                 mn   --- is the minute of the hour (0 to 59)
C                          leading zeros are added to mn if the
C                          numeric value is less than 10.
C                 sc.sss   is the second of the minute to 3 decimal
C                          places ( 0 to 59.999).  Leading zeros
C                          are added if the numeric value is less
C                          than 10.  Seconds are truncated, not
C                          rounded.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the input ET is so large that the corresponding
C        number of days since 1 A.D. Jan 1, 00:00:00 is
C        within 1 of overflowing or underflowing an integer,
C        ET will not be converted to the correct string
C        representation rather, the string returned will
C        state that the epoch was before or after the day
C        that is INTMIN +1 or INTMAX - 1 days after
C        1 A.D. Jan 1, 00:00:00.
C
C     2) If the output string is not sufficiently long to hold
C        the full date, it will be truncated on the right.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is an error free routine for converting ephemeris epochs
C     represented as seconds past the J2000 epoch to formal
C     calendar strings based upon the Gregorian Calendar.  This formal
C     time is often useful when one needs a human recognizable
C     form of an ephemeris epoch.  There is no accounting for leap
C     seconds in the output times produced.
C
C     Note: The calendar epochs produced are not the same as the
C           UTC calendar epochs that correspond to ET. The strings
C           produced by this routine may vary from the corresponding
C           UTC epochs by more than 1 minute.
C
C     This routine can be used in creating error messages or
C     in routines and programs in which one prefers to report
C     times without employing leapseconds to produce exact UTC
C     epochs.
C
C
C$ Examples
C
C     Suppose you wish to  report that no data is
C     available at a particular ephemeris epoch ET.  The following
C     code shows how you might accomplish this task.
C
C     CALL DPSTRF ( ET,  6, 'F', ETSTR  )
C     CALL ETCAL  ( ET,          STRING )
C
C     E1 = RTRIM   (             STRING )
C     E2 = RTRIM   (             ETSTR  )
C
C     WRITE (*,*) 'There is no data available for the body '
C     WRITE (*,*) 'at requested time: '
C     WRITE (*,*) '   ', STRING(1:E1), ' (', ETSTR(1:E2), ')'
C
C
C$ Restrictions
C
C     One must keep in mind when using this routine that
C     ancient times are not based upon the Gregorian
C     calendar.  For example the 0 point of the Julian
C     Date system is 4713 B.C. Jan 1, 12:00:00 on the Julian
C     Calendar.  If one formalized the Gregorian calendar
C     and extended it indefinitely, the zero point of the Julian
C     date system corresponds to 4714 B.C. NOV 24 12:00:00 on
C     the Gregorian calendar.  There are several reasons for this.
C     Leap years in the Julian calendar occur every
C     4 years (including *all* centuries).  Moreover,  the
C     Gregorian calendar "effectively" begins on 15 Oct, 1582 A.D.
C     which is 5 Oct, 1582 A.D. in the Julian Calendar.
C
C     Therefore you must be careful in your interpretation
C     of ancient dates produced by this routine.
C
C$ Literature_References
C
C     1. "From Sundial to Atomic Clocks---Understanding Time and
C         Frequency" by James Jespersen and Jane Fitz-Randolph
C         Dover Publications, Inc. New York (1982).
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-     SPICELIB Version 2.2.0, 05-MAR-1998 (WLT)
C
C         The documentation concerning the appearance of the output
C         time string was corrected so that it does not suggest
C         a comma is inserted after the day of the month.  The
C         comma was removed from the output string in Version 2.0.0
C         (see the note below) but the documentation was not upgraded
C         accordingly.
C
C-     SPICELIB Version 2.1.0, 20-MAY-1996 (WLT)
C
C         Two arrays that were initialized but never used were
C         removed.
C
C-     SPICELIB Version 2.0.0, 16-AUG-1995 (KRG)
C
C         If the day number was less than 10, the spacing was off for
C         the rest of the time by one space, that for the "tens" digit.
C         This has been fixed by using a leading zero when the number of
C         days is < 10.
C
C         Also, the comma that appeared between the month/day/year
C         and the hour:minute:seconds tokens has been removed. This was
C         done in order to make the calendar date format of ETCAL
C         consistent with the calendar date format of ET2UTC.
C
C
C-     SPICELIB Version 1.0.0, 14-DEC-1993 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Convert ephemeris time to a formal calendar date
C
C-&
 
C$ Revisions
C
C-     SPICELIB Version 2.1.0, 20-MAY-1996 (WLT)
C
C         Two arrays that were initialized but never used were
C         removed.
C
C-     SPICELIB Version 2.0.0, 16-AUG-1995 (KRG)
C
C         If the day number was less than 10, the spacing was off for
C         the rest of the time by one space, that for the "tens" digit.
C         This has been fixed byusing a leading zero when the number of
C         days is < 10.
C
C         Also, the comma that appeared between the month/day/year
C         and the hour:minute:seconds tokens has been removed. This was
C         done in order to make the calendar date format of ETCAL
C         consistent with the calendar date format of ET2UTC.
C
C-     SPICELIB Version 1.0.0, 14-DEC-1993 (WLT)
C
C-&
 
C
C     Spicelib Functions.
C
 
 
      DOUBLE PRECISION      SPD
      INTEGER               INTMAX
      INTEGER               INTMIN
      INTEGER               LSTLTI
 
 
 
C
C     We declare the variables that contain the number of days in
C     400 years, 100 years, 4 years and 1 year.
C
      INTEGER               DP400Y
      PARAMETER           ( DP400Y = 365*400 + 97 )
 
      INTEGER               DP100Y
      PARAMETER           ( DP100Y = 365*100 + 24 )
 
      INTEGER               DP4Y
      PARAMETER           ( DP4Y   = 365*4   + 1  )
 
      INTEGER               DP1Y
      PARAMETER           ( DP1Y   = 365 )
 
 
C
C     The following integers give the number of days during the
C     associated month of a non-leap year.
C
      INTEGER               JAN
      PARAMETER           ( JAN = 31 )
 
      INTEGER               FEB
      PARAMETER           ( FEB = 28 )
 
      INTEGER               MAR
      PARAMETER           ( MAR = 31 )
 
      INTEGER               APR
      PARAMETER           ( APR = 30 )
 
      INTEGER               MAY
      PARAMETER           ( MAY = 31 )
 
      INTEGER               JUN
      PARAMETER           ( JUN = 30 )
 
      INTEGER               JUL
      PARAMETER           ( JUL = 31 )
 
      INTEGER               AUG
      PARAMETER           ( AUG = 31 )
 
      INTEGER               SEP
      PARAMETER           ( SEP = 30 )
 
      INTEGER               OCT
      PARAMETER           ( OCT = 31 )
 
      INTEGER               NOV
      PARAMETER           ( NOV = 30 )
 
      INTEGER               DEC
      PARAMETER           ( DEC = 31 )
 
C
C     The integers that follow give the number of days in a normal
C     year that precede the first of the month.
C
      INTEGER               JAN0
      PARAMETER           ( JAN0 = 0  )
 
      INTEGER               FEB0
      PARAMETER           ( FEB0 = JAN + JAN0 )
 
      INTEGER               MAR0
      PARAMETER           ( MAR0 = FEB + FEB0 )
 
      INTEGER               APR0
      PARAMETER           ( APR0 = MAR + MAR0 )
 
      INTEGER               MAY0
      PARAMETER           ( MAY0 = APR + APR0 )
 
      INTEGER               JUN0
      PARAMETER           ( JUN0 = MAY + MAY0 )
 
      INTEGER               JUL0
      PARAMETER           ( JUL0 = JUN + JUN0 )
 
      INTEGER               AUG0
      PARAMETER           ( AUG0 = JUL + JUL0 )
 
      INTEGER               SEP0
      PARAMETER           ( SEP0 = AUG + AUG0 )
 
      INTEGER               OCT0
      PARAMETER           ( OCT0 = SEP + SEP0 )
 
      INTEGER               NOV0
      PARAMETER           ( NOV0 = OCT + OCT0 )
 
      INTEGER               DEC0
      PARAMETER           ( DEC0 = NOV + NOV0 )
 
C
C     The integers that follow give the number of days in a leap
C     year that precede the first of the month.
C
      INTEGER               JANL0
      PARAMETER           ( JANL0 = JAN0  )
 
      INTEGER               FEBL0
      PARAMETER           ( FEBL0 = FEB0 )
 
      INTEGER               MARL0
      PARAMETER           ( MARL0 = MAR0 + 1 )
 
      INTEGER               APRL0
      PARAMETER           ( APRL0 = APR0 + 1 )
 
      INTEGER               MAYL0
      PARAMETER           ( MAYL0 = MAY0 + 1 )
 
      INTEGER               JUNL0
      PARAMETER           ( JUNL0 = JUN0 + 1 )
 
      INTEGER               JULL0
      PARAMETER           ( JULL0 = JUL0 + 1 )
 
      INTEGER               AUGL0
      PARAMETER           ( AUGL0 = AUG0 + 1 )
 
      INTEGER               SEPL0
      PARAMETER           ( SEPL0 = SEP0 + 1 )
 
      INTEGER               OCTL0
      PARAMETER           ( OCTL0 = OCT0 + 1 )
 
      INTEGER               NOVL0
      PARAMETER           ( NOVL0 = NOV0 + 1 )
 
      INTEGER               DECL0
      PARAMETER           ( DECL0 = DEC0 + 1 )
 
      INTEGER               STRSIZ
      PARAMETER           ( STRSIZ = 16 )
 
      INTEGER               LNGSIZ
      PARAMETER           ( LNGSIZ = 180 )
 
 
C
C     The variables below hold the components of the output string
C     before they are put together.
C
      CHARACTER*(STRSIZ)    ERA
      CHARACTER*(STRSIZ)    YSTR
      CHARACTER*(STRSIZ)    DSTR
      CHARACTER*(STRSIZ)    HSTR
      CHARACTER*(STRSIZ)    MSTR
      CHARACTER*(STRSIZ)    SSTR
      CHARACTER*(STRSIZ)    MESSGE
 
C
C     We will construct our string using the local variable DATE
C     and transfer the results to the output STRING when we are
C     done.
C
      CHARACTER*(LNGSIZ)    DATE
 
C
C     MONTHS contains 3-letter abbreviations for the months of the year
C
      CHARACTER*(3)         MONTHS ( 12 )
 
      DOUBLE PRECISION      DMNINT
      DOUBLE PRECISION      DMXINT
      DOUBLE PRECISION      DP2000
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      HALFD
      DOUBLE PRECISION      MYDNOM
      DOUBLE PRECISION      MYNUM
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      REMD
      DOUBLE PRECISION      SECS
      DOUBLE PRECISION      SECSPD
 
      INTEGER               BH
      INTEGER               BM
      INTEGER               DAY
      INTEGER               DAYNUM
      INTEGER               DN2000
      INTEGER               DOFYR
      INTEGER               DOY
      INTEGER               DP0001
      INTEGER               HOURS
      INTEGER               IQ
      INTEGER               LDAYS
      INTEGER               MINS
      INTEGER               MONTH
      INTEGER               OFFSET
      INTEGER               REM
      INTEGER               TSECS
      INTEGER               YDAYS
      INTEGER               YEAR
      INTEGER               YR1
      INTEGER               YR100
      INTEGER               YR4
      INTEGER               YR400
 
      LOGICAL               FIRST
      LOGICAL               ADJUST
 
C
C     The array EXTRA contains the number of additional days that
C     appear before the first of a month during a leap year (as opposed
C     to a non-leap year).
C
 
      INTEGER               EXTRA  ( 12 )
      INTEGER               DPBEGL ( 12 )
 
C
C     DPJAN0(I) gives the number of days that occur before the I'th
C     month of a normal year.
C
      INTEGER               DPJAN0 ( 12 )
 
      SAVE
 
      DATA                  FIRST   / .TRUE. /
 
      DATA                  EXTRA   /  0,     0,     1,     1,
     .                                 1,     1,     1,     1,
     .                                 1,     1,     1,     1    /
 
      DATA                  DPJAN0  /  JAN0,  FEB0,  MAR0,  APR0,
     .                                 MAY0,  JUN0,  JUL0,  AUG0,
     .                                 SEP0,  OCT0,  NOV0,  DEC0 /
 
      DATA                  DPBEGL  /  JANL0, FEBL0, MARL0, APRL0,
     .                                 MAYL0, JUNL0, JULL0, AUGL0,
     .                                 SEPL0, OCTL0, NOVL0, DECL0 /
 
      DATA                  MONTHS  / 'JAN',  'FEB', 'MAR', 'APR',
     .                                'MAY',  'JUN', 'JUL', 'AUG',
     .                                'SEP',  'OCT', 'NOV', 'DEC' /
 
 
 
C
C     Definitions of statement functions.
C
C
C     The number of days elapsed since Jan 1, of year 1 A.D. to
C     Jan 1 of YEAR is given by:
C
      YDAYS(YEAR) = 365*(YEAR-1)
     .              +  ((YEAR-1)/4)
     .              -  ((YEAR-1)/100)
     .              +  ((YEAR-1)/400)
 
C
C     The number of leap days in a year is given by:
C
      LDAYS(YEAR) =   ( (YEAR/4  ) * 4   ) / YEAR
     .              - ( (YEAR/100) * 100 ) / YEAR
     .              + ( (YEAR/400) * 400 ) / YEAR
 
C
C     To compute the day of the year we
C
C        look up the number of days to the beginning of the month,
C
C        add on the number leap days that occurred prior to that
C        time
C
C        add on the number of days into the month
C
      DOY(YEAR,MONTH,DAY) =   DPJAN0 ( MONTH )
     .                      + EXTRA  ( MONTH ) * LDAYS(YEAR)
     .                      + DAY
 
 
C
C     The number of days since 1 Jan 1 A.D. is given by:
C
      DP0001(YEAR,MONTH,DAY) = YDAYS(YEAR)
     .                       + DOY  (YEAR, MONTH, DAY)
     .                       - 1
 
 
 
 
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
         HALFD  = SPD()/2.0D0
         SECSPD = SPD()
         DN2000 = DP0001 ( 2000, 1, 1 )
         DMXINT = INTMAX()
         DMNINT = INTMIN()
 
      END IF
C
C     Now we "in-line" compute the following call.
C
C        call rmaind ( et + halfd, secspd, dp2000, secs )
C
C     because we can't make a call to rmaind.
C
C     The reader may wonder why we use et + halfd.  The value
C     et is seconds past the ephemeris epoch of J2000 which
C     is at 2000 Jan 1, 12:00:00.  We want to compute days past
C     2000 Jan 1, 00:00:00.  The seconds past THAT epoch is et + halfd.
C     We add on 0.0005 seconds so that the string produced will be
C     rounded to the nearest millisecond.
C
      MYDNOM = SECSPD
      MYNUM  = ET + HALFD
 
      Q    = DINT ( MYNUM / MYDNOM )
      REMD = MYNUM -    Q * MYDNOM
 
      IF ( REMD .LT. 0.0D0 ) THEN
         Q    = Q   - 1.0D0
         REMD = REMD + MYDNOM
      END IF
 
      SECS   = REMD
      DP2000 = Q
 
C
C     Do something about the problem when ET is vastly
C     out of range.  (Day number outside MAX and MIN integer).
C
      IF      ( DP2000 + DN2000 .LT. DMNINT + 1) THEN
 
         DP2000 = DMNINT - DN2000 + 1
         MESSGE = 'Epoch before '
         SECS   = 0.0D0
 
      ELSE IF ( DP2000 + DN2000 .GT. DMXINT - 1 ) THEN
 
         DP2000 = DMXINT - DN2000 - 1
         MESSGE = 'Epoch after '
         SECS   = 0.0D0
 
      ELSE
 
         MESSGE = ' '
 
      END IF
 
C
C     Compute the number of days since 1 .A.D. Jan 1, 00:00:00.
C     From the tests in the previous IF-ELSE IF-ELSE block this
C     addition is guaranteed not to overflow.
C
      DAYNUM = INT( DP2000 + DBLE(DN2000) )
 
C
C     If the number of days is negative, we need to do a little
C     work so that we can represent the date in the B.C. era.
C     We add enough multiples of 400 years so that the year will
C     be positive and then we subtract off the appropriate multiple
C     of 400 years later.
C
      IF ( DAYNUM .LT. 0 ) THEN
 
C
C        Since we can't make the call below and remain
C        error free, we compute it ourselves.
C
C        call rmaini ( daynum, dp400y, offset, daynum )
C
         IQ   = DAYNUM / DP400Y
         REM  = DAYNUM - DP400Y*IQ
 
         IF ( REM .LT. 0 ) THEN
            IQ   = IQ   - 1
            REM  = REM  + DP400Y
         END IF
 
         OFFSET = IQ
         DAYNUM = REM
 
         ADJUST = .TRUE.
 
      ELSE
 
         ADJUST = .FALSE.
 
      END IF
 
C
C     Next we compute the year.  Divide out multiples of 400, 100
C     4 and 1 year.  Finally combine these to get the correct
C     value for year.  (Note this is all integer arithmetic.)
C
C     Recall that DP1Y   =    365
C                 DP4Y   =  4*DPY    + 1
C                 DP100Y = 25*DP4Y   - 1
C                 DP400Y =  4*DP100Y + 1
C
      YR400  =  DAYNUM / DP400Y
      REM    =  DAYNUM - DP400Y*YR400
 
      YR100  =  MIN ( 3, REM   / DP100Y )
      REM    =  REM    - YR100 * DP100Y
 
      YR4    =  MIN (24, REM / DP4Y )
      REM    =  REM    - YR4 * DP4Y
 
      YR1    =  MIN (3,  REM / DP1Y )
      REM    =  REM    - YR1 * DP1Y
 
      DOFYR  =  REM    + 1
 
      YEAR   = YR400*400 + YR100*100 + YR4*4 + YR1 + 1
C
C     Get the month, and day of month (depending upon whether
C     we have a leap year or not).
C
      IF ( LDAYS(YEAR) .EQ. 0 ) THEN
         MONTH  = LSTLTI ( DOFYR, 12, DPJAN0 )
         DAY    = DOFYR - DPJAN0(MONTH)
      ELSE
         MONTH  = LSTLTI ( DOFYR, 12, DPBEGL )
         DAY    = DOFYR - DPBEGL(MONTH)
      END IF
C
C     If we had to adjust the year to make it positive, we now
C     need to correct it and then convert it to a B.C. year.
C
      IF ( ADJUST ) THEN
 
         YEAR   =  YEAR + OFFSET*400
         YEAR   = -YEAR + 1
         ERA    = ' B.C. '
 
      ELSE
C
C        If the year is less than 1000, we can't just write it
C        out.  We need to add the era.  If we don't do this
C        the dates look very confusing.
C
         IF ( YEAR .LT. 1000 ) THEN
            ERA   = ' A.D. '
         ELSE
            ERA   = ' '
         END IF
 
      END IF
C
C     Convert Seconds to Hours, Minute and Seconds.
C     We work with thousandths of a second in integer arithmetic
C     so that all of the truncation work with seconds will already
C     be done.  (Note that we already know that SECS is greater than
C     or equal to zero so we'll have no problems with HOURS, MINS
C     or SECS becoming negative.)
C
      TSECS  = INT(SECS*1000.0D0)
      FRAC   = SECS - DBLE(TSECS)
 
      HOURS  = TSECS / 3600000
      TSECS  = TSECS - 3600000 * HOURS
 
      MINS   = TSECS / 60000
      TSECS  = TSECS - 60000 * MINS
 
      SECS   = DBLE ( TSECS ) / 1000.0D0
C
C     We round seconds if we can do so without getting seconds to be
C     bigger than 60.
C
      IF ( SECS + 0.0005D0 .LT. 60.0D0 ) THEN
         SECS = SECS + 0.0005D0
      END IF
 
C
C     Finally, get the components of our date string.
C
      CALL INTSTR ( YEAR,   YSTR )
      IF ( DAY .GE. 10 ) THEN
         CALL INTSTR ( DAY, DSTR )
      ELSE
         DSTR = '0'
         CALL INTSTR( DAY, DSTR(2:) )
      END IF
 
C
C     We want to zero pad the hours minutes and seconds.
C
      IF ( HOURS .LT. 10 ) THEN
         BH = 2
      ELSE
         BH = 1
      END IF
 
      IF ( MINS .LT. 10 ) THEN
         BM = 2
      ELSE
         BM = 1
      END IF
 
 
      MSTR = '00'
      HSTR = '00'
      SSTR = ' '
 
C
C     Now construct the string components for hours, minutes and
C     seconds.
C
      SECS = INT(SECS*1000.0D0) / 1000.0D0
 
      CALL INTSTR ( HOURS,         HSTR(BH:) )
      CALL INTSTR ( MINS,          MSTR(BM:) )
      CALL DPSTRF ( SECS,  6, 'F', SSTR      )
C
C     The form of the output for SSTR has a leading blank followed by
C     the first significant digit.  If a decimal point is in the
C     third slot, then SSTR is of the form ' x.xxxxx'  and we need
C     to insert a leading zero.
C
      IF ( SSTR(3:3) .EQ. '.' ) THEN
         SSTR(1:1) = '0'
      END IF
C
C     We don't want any leading spaces in SSTR, (HSTR and MSTR don't
C     have leading spaces by construction.
C
      CALL LJUST ( SSTR, SSTR )
C
C     Now form the date string, squeeze out extra spaces and
C     left justify the whole thing.
C
      DATE = MESSGE
     .//     YSTR
     .//     ERA
     .//     MONTHS(MONTH)
     .//     ' '
     .//     DSTR(1:3)
     .//     ' '
     .//     HSTR(1:2)
     .//     ':'
     .//     MSTR(1:2)
     .//     ':'
     .//     SSTR(1:6)
 
 
      CALL CMPRSS ( ' ', 1, DATE, DATE )
      CALL LJUST  (         DATE, DATE )
 
      STRING = DATE
 
      RETURN
      END
