C$Procedure      JUL2GR (Julian to Gregorian Calendar)
 
      SUBROUTINE JUL2GR (  YEAR, MONTH, DAY, DOY )
 
C$ Abstract
C
C     Convert Year Month and Day on the Julian Calendar
C     to the Gregorian Calendar
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
C      None.
C
C$ Keywords
C
C      TIME
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               YEAR
      INTEGER               MONTH
      INTEGER               DAY
      INTEGER               DOY
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     YEAR      I/O  Year  of Julian Calendar/Gregorian Calendar
C     MONTH     I/O  Month of Julian Calendar/Gregorian Calendar
C     DAY       I/O  Day of Month in Julian Calendar/Gregorian Calendar
C     DOY        O   Day of Year in Gregorian Calendar
C
C$ Detailed_Input
C
C     YEAR      is an integer representing the year of an epoch, E, in
C               the Julian proleptic calendar. Note that the year 0
C               and negative values are required to represent
C               years in the pre-Christian era (B.C.)  A year, Y B.C.,
C               should be represented as -(Y-1).  For example the year
C               435 B.C. should be input as -434.
C
C     MONTH     is an integer representing the month of some epoch, E,
C               in the Julian proleptic calendar. Months
C               outside the usual range from 1 to 12 are converted
C               to the standard range using modular arithmetic and
C               the input year is adjusted appropriately.
C
C
C     DAY       is the day of the month of some epoch, E, in the Julian
C               proleptic calendar.
C
C               Note to input an epoch as the day of a year, set MONTH
C               to 1 and DAY to the day of the year.
C
C$ Detailed_Output
C
C     YEAR      is an integer representing the year of the epoch, E,
C               above in the Gregorian calendar. Note that the year
C               0 (zero) and negative values are used to represent
C               years in the pre-Christian era (B.C.)  A year, Y B.C.,
C               is be reprented as -(Y-1).  For example the year
C               435 B.C. will be returned as -434.
C
C     MONTH     is an integer representing the month of the epoch, E,
C               above in the Gregorian Calendar calendar.
C
C     DAY       is the day of the month of the epoch, E, above in the
C               Gregorian Calendar
C
C     DOY       is the day of the year of the epoch, E, above in the
C               Gregorian Calendar.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C     This is a mid-level utility routine to assist in the assignment
C     and presentation of ancient epochs.
C
C     The SPICE software deals primarily with epochs represented on
C     in the Gregororian Calendar.  However, the Gregorian calendar
C     was not adopted until October 15, 1582.  As a result, epochs
C     prior to that time are usually represented in the Julian
C     proleptic calendar.
C
C     Formally, both calendars can be extended indefinitely forward
C     and backward in time due the algorithmic nature of the
C     determination of calendar representation.
C
C     When converting "parsed" calendar epochs in the SPICE system,
C     you need to first convert to the Gregorian Calendar. From that
C     point the SPICE toolkit can easily convert the epoch to Julian
C     date or seconds past the J2000 epoch.
C
C     This routine allows you to take a numeric representation of
C     an epoch represented in the Julian proleptic calendar and
C     convert that to an epoch in the Gregorian calendar.
C
C     To convert from Gregorian Calendar to Julian proleptic
C     calendar, use the entry point GR2JUL.
C
C$ Examples
C
C     Suppose you need to find the epoch (in seconds past the
C     J2000) of some ancient epoch that occurred at
C     3:00 on March 4 of the year 121 B.C.  And that this epoch
C     is based on the Julian proleptic calendar.  We first need
C     to convert the Julian Calendar date to the Gregorian Calendar.
C
C     Here's the declarations we'll need
C
C        INTEGER               YEAR
C        INTEGER               MONTH
C        INTEGER               DAY
C        INTEGER               DOY
C
C        DOUBLE PRECISION      TVEC ( 6 )
C        DOUBLE PRECISION      TDB
C
C     You first need to convert the calendar date of this epoch
C     integers. (We don't need to worry about the hours for a moment).
C
C        YEAR  = -120
C        MONTH =  3
C        DAY   =  4
C
C     Convert this Year, Month and Day to the Gregorian Calendar.
C
C        CALL JUL2GR ( YEAR, MONTH, DAY, DOY )
C
C     Now construct a time vector for use in the routine TTRANS.
C     Note now we use the hour component of the epoch (the fourth
C     component of the time vector TVEC).
C
C        TVEC(1) = DBLE( YEAR )
C        TVEC(2) = DBLE( MONTH )
C        TVEC(3) = DBLE( DAY )
C        TVEC(4) = 3.0D0
C        TVEC(5) = 0.0D0
C        TVEC(6) = 0.0D0
C
C     Now the routine TTRANS can convert the time vector from
C     the input YMD format to barycentric dynamical time.
C
C        CALL TTRANS ( 'YDM', 'TDB', TVEC )
C
C        TDB = TVEC(1)
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 26-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in RMAINI calls.
C
C-    SPICELIB Version 1.1.1, 23-SEP-1999 (WLT)
C
C        Removed the unused variable DPMON.
C
C-    SPICELIB Version 1.1.0, 23-FEB-1998 (WLT)
C
C        The routine was upgraded so that it will handle without
C        error months that are outside the range from 1 to 12.
C
C-    SPICELIB Version 1.0.0, 13-MAR-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Convert from Julian proleptic to Gregorian Calendar
C
C-&

C$ Revisions
C
C-    SPICELIB Version 1.1.0, 26-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in RMAINI calls.
C
C-& 
 
C
C     Spicelib Functions
C
      INTEGER               LSTLTI
 
C
C     Local (in-line) Functions
C
      INTEGER               DIVBLE
      INTEGER               GDOY
      INTEGER               GDP001
      INTEGER               GLDAYS
      INTEGER               GYDAYS
      INTEGER               JDOY
      INTEGER               JDP001
      INTEGER               JLDAYS
      INTEGER               JYDAYS
 
C
C     Local parameters
C
C     We declare the variables that contain the number of days in
C     400 years (Gregorian), 100 years (Gregorian), 4 years and 1 year.
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
 
 
C
C     Local variables
C 
      INTEGER               DAYG
      INTEGER               DAYJ
      INTEGER               DOFYR
      INTEGER               DY
      INTEGER               M
      INTEGER               M100
      INTEGER               M4
      INTEGER               M400
      INTEGER               MON
      INTEGER               OFFSET
      INTEGER               OFFSTG
      INTEGER               OFFSTJ
      INTEGER               RDAYG
      INTEGER               RDAYJ
      INTEGER               TMPDAY
      INTEGER               TMPYR
      INTEGER               YR
      INTEGER               YROFF
 
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
 
      LOGICAL               FIRST
 
C
C     Saved variables    
C
      SAVE

C
C     Initial values
C
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
C
C     Definitions of statment functions.
C
C     The number of days ellapsed since Gregorian Jan 1, of year 1 A.D.
C     to Jan 1 of YR is given by:
C
      GYDAYS(YR) = 365*( YR-1)
     .             +   ((YR-1)/4)
     .             -   ((YR-1)/100)
     .             +   ((YR-1)/400)
 
C
C     The number of days ellapsed since Julian Jan 1, of year 1 A.D.
C     to Jan 1 of YR is given by:
C
      JYDAYS(YR) = 365*(YR-1)
     .             +  ((YR-1)/4)
 
C
C     Return 1 if YR is divisible by M, otherwise return 0.
C
      DIVBLE(YR,M) = MAX ( 0, 1 + ((IABS(YR)/M)*M) - IABS(YR) )
 
C
C     The number of leap days in a Gregorian year is given by:
C
      GLDAYS(YR) = DIVBLE( YR, 4   )
     .           - DIVBLE( YR, 100 )
     .           + DIVBLE( YR, 400 )
 
C
C     The number of leap days in a Julian year is given by:
C
      JLDAYS(YR) = DIVBLE( YR, 4   )
 
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
      GDOY(YR,MON,DY) = DPJAN0 ( MON )
     .                + EXTRA  ( MON ) * GLDAYS(YR)
     .                + DY
 
      JDOY(YR,MON,DY) = DPJAN0 ( MON )
     .                + EXTRA  ( MON ) * JLDAYS(YR)
     .                + DY
 
C
C     The number of days since 1 Jan 1 A.D. (Gregorian) is given by:
C
      GDP001(YR,MON,DY) = GYDAYS(YR)
     .                  + GDOY  (YR, MON, DY)
     .                  - 1
 
C
C     The number of days since 1 Jan 1 A.D. (Julianis given by:
C
      JDP001(YR,MON,DY) = JYDAYS(YR)
     .                  + JDOY  (YR, MON, DY)
     .                  - 1
 
C
C     If this is the first pass through this entry point (or the
C     companion entry point) we need to set up some reference points.
C
C     RDAYG   is the number of days past 1 A.D. Jan 1 of the Gregorian
C             calendar of the date Oct 15, 1582
C
C     RDAYJ   is the number of days past 1 A.D. Jan 1 of the Julian
C             calendar of the date Oct 5, 1582.
C
C     OFFSTJ and OFFSTG are just the offset from one count of days
C     to the other.
C
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
 
         RDAYG  = GDP001 ( 1582, 10, 15 )
         RDAYJ  = JDP001 ( 1582, 10,  5 )
         OFFSTJ = RDAYJ - RDAYG
         OFFSTG = RDAYG - RDAYJ
 
      END IF
 
C
C     Make local copies of the year, month and day.  Then get the
C     YEARs into a positive range.
C
      CALL RMAINI ( MONTH-1, 12, YROFF, MON )
 
      YR  = YEAR + YROFF
      MON = MON + 1
      DY  = DAY
 
      IF ( YR .LE. 0 ) THEN
 
         CALL RMAINI ( YR, 4, M4, TMPYR )
         YR = TMPYR

         IF ( YR .EQ. 0 ) THEN
            YR = YR + 4
            M4   = M4   - 1
         END IF
 
         OFFSET = M4 * DP4Y
 
      ELSE
 
         OFFSET = 0
 
      END IF
 
C
C     First get the day number (Julian) for the input
C     year month and day.
C
      DAYJ = JDP001 ( YR, MON, DY  ) + OFFSET
 
C
C     This day is DAYJ - RDAYJ days after 1582 Oct 5 on the
C     julian calendar.  But this is the same as the number
C     of days past 1582 Oct 15 on the Gregorian Calendar
C     So the Gregorian day number is DAYJ - RDAYJ + RDAYG
C     which is the same as DAYJ + OFFSTG.
C
      DAYG = DAYJ + OFFSTG
C
C     Now that we have the Gregorian day number it's a fairly
C     straight forward task to get the year, month and day
C     on the Gregorian calendar.
C
 
      CALL RMAINI ( DAYG, DP400Y, M400, TMPDAY )
      DAYG = TMPDAY

      M100 = MIN ( 3, DAYG/DP100Y )
      DAYG = DAYG -   M100*DP100Y
 
      M4   = MIN ( 24, DAYG/DP4Y  )
      DAYG = DAYG  -     M4*DP4Y
 
      M    = MIN (  3, DAYG/DP1Y )
      DAYG = DAYG   -     M*DP1Y
 
 
      DOFYR  = DAYG + 1
      YR     = 400 * M400
     .       + 100 * M100
     .       +   4 * M4
     .       +       M
     .       +       1
 
C
C     Now look up the month number and compute the day of the month.
C     How we do this depends on whether or not this is a leap year.
C
      IF ( GLDAYS(YR) .EQ. 0 ) THEN
         MON  = LSTLTI ( DOFYR, 12, DPJAN0 )
         DY   = DOFYR  -            DPJAN0(MON)
      ELSE
         MON  = LSTLTI ( DOFYR, 12, DPBEGL )
         DY   = DOFYR  -            DPBEGL(MON)
      END IF
 
      YEAR  = YR
      MONTH = MON
      DAY   = DY
      DOY   = DOFYR
 
      RETURN
 
 
C$Procedure      GR2JUL (Gregorian to Julian Calendar)
 
      ENTRY GR2JUL (  YEAR, MONTH, DAY, DOY )
 
C$ Abstract
C
C     Convert Year Month and Day on the  Gregorian Calendar
C     to the Julian Calendar
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
C      None.
C
C$ Keywords
C
C      TIME
C
C$ Declarations
C
C     INTEGER               YEAR
C     INTEGER               MONTH
C     INTEGER               DAY
C     INTEGER               DOY
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     YEAR      I/O  Year  of Gregorian Calendar/Julian Calendar
C     MONTH     I/O  Month of Gregorian Calendar/Julian Calendar
C     DAY       I/O  Day of Month in Gregorian Calendar/Julian Calendar
C     DOY        O   Day of Year in Julian Calendar
C
C$ Detailed_Input
C
C     YEAR      is an integer representing the year of an epoch, E, in
C               the Gregorian calendar. Note that the year 0 (zero)
C               and negative values are required to represent
C               years in the pre-Christian era (B.C.)  A year, Y B.C.
C               should be reprented as -(Y-1).  For example the year
C               435 B.C. should be input as -434.
C
C     MONTH     is an integer representing the month of some epoch, E,
C               in the Gregorian calendar. Months
C               outside the usual range from 1 to 12 are converted
C               to the standard range using modular arithmetic and
C               the input year is adjusted appropriately.
C
C     DAY       is the day of the month of some epoch, E, in the
C               Gregorian calendar.
C
C               Note to input an epoch as the day of a year, set MONTH
C               to 1 and DAY to the day of the year.
C
C$ Detailed_Output
C
C     YEAR      is an integer representing the year of the epoch, E,
C               above in the Julian calendar. Note that the year 0
C               (zero) and negative values are used to represent
C               years in the pre-Christian era (B.C.)  A year, Y B.C.,
C               is be reprented as -(Y-1).  For example the year
C               435 B.C. will be returned as -434.
C
C     MONTH     is an integer representing the month of the epoch, E,
C               above in the Julian Calendar calendar.
C
C     DAY       is the day of the month of the epoch, E, above in the
C               Julian Calendar
C
C     DOY       is the day of the year of the epoch, E, above in the
C               Julian Calendar.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C     This is a mid-level utility routine to assist in the assignment
C     and presentation of Ancient epochs.
C
C     The SPICE software deals primarily with epochs represented on
C     in the Gregororian Calendar.  However, the Gregorian calendar
C     was not adopted until October 15, 1582.  As a result, epochs
C     prior to that time are usually represented in the Julian
C     proleptic calendar.
C
C     Formally, both calendars can be extended indefinitely forward
C     and backward in time due the algorithmic nature of the
C     determination of calendar representation.
C
C     This routine allows you to take a numeric representation of
C     an epoch represented in the Gregorian calendar and
C     convert that to an epoch in the Julian calendar.
C
C     To convert from Julian Calendar to Gregorian
C     calendar, use the entry point JUL2GR.
C
C$ Examples
C
C     Suppose you need to print an epoch (given in seconds past the
C     J2000 epoch) of some ancient epoch that occured during
C     pre-Christian era, and that you want to represent this epoch
C     using the Julian proleptic calendar.
C
C     Here's the declarations we'll need
C
C        INTEGER               YEAR
C        INTEGER               MONTH
C        INTEGER               DAY
C        INTEGER               DOY
C
C        DOUBLE PRECISION      TVEC ( 6 )
C        DOUBLE PRECISION      TDB
C
C     You first need to convert TDB (the epoch in Seconds past J2000)
C     to a calendar representation.
C
C        TVEC(1) = TDB.
C
C        CALL TTRANS ( 'TDB', 'YMD', TVEC )
C
C     The output time vector will be relative to the Gregorian
C     Calendar.  Collect the year, month and day from the time
C     vectory.
C
C        YEAR    = INT ( TVEC(1) )
C        MONTH   = INT ( TVEC(2) )
C        DAY     = INT ( TVEC(3) )
C
C     The hours, minutes and seconds appear in components 4 through 6
C     of the time vector.  We can ignore them in the conversion
C     of the calendar from Gregorian to Julian.
C
C        CALL GR2JUL ( YEAR, MONTH, DAY, DOY )
C
C     Now create a string from the YEAR, MONTH, DAY and TVEC(4)
C     through TVEC(6).
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 23-FEB-1998 (WLT)
C
C        The routine was upgraded so that it will handle without
C        error months that are outside the range from 1 to 12.
C
C-    SPICELIB Version 1.0.0, 13-MAR-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Convert from Gregorian to Julian Calendar
C
C-&
 
 
C
C     If this is the first pass through this entry point (or the
C     companion entry point) we need to set up some reference points.
C
C     RDAYG   is the number of days past 1 A.D. Jan 1 of the Gregorian
C             calendar of the date Oct 15, 1582
C
C     RDAYJ   is the number of days past 1 A.D. Jan 1 of the Julian
C             calendar of the date Oct 5, 1582.
C
C     OFFSTJ and OFFSTG are just the offset from one count of days
C     to the other.
C
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
 
         RDAYG  = GDP001 ( 1582, 10, 15 )
         RDAYJ  = JDP001 ( 1582, 10,  5 )
         OFFSTJ = RDAYJ - RDAYG
         OFFSTG = RDAYG - RDAYJ
 
      END IF
C
C     Make Local Copies of YEAR, MONTH and DAY and get YEAR into
C     a positive range.
C
      CALL RMAINI ( MONTH-1, 12, YROFF, MON )
 
      YR  = YEAR + YROFF
      MON = MON  + 1
      DY  = DAY
 
      IF ( YR .LE. 0 ) THEN
 
         CALL RMAINI ( YR, 400, M400, TMPYR )
         YR = TMPYR
 
         IF ( YR .EQ. 0 ) THEN
            YR   = YR + 400
            M400 = M400 - 1
         END IF
 
         OFFSET = M400 * DP400Y
 
      ELSE
 
         OFFSET = 0
 
      END IF
 
C
C     First get the day number (Gregorian) for the input
C     year month and day.
C
      DAYG = GDP001 ( YR, MON, DY  ) + OFFSET
 
C
C     This day is DAYG - RDAYG days after 1582 Oct 15 on the
C     Gregorian calendar.  But this is the same as the number
C     of days past 1582 Oct 5 on the Julian Calendar
C     So the Julian day number is DAYG - RDAYG + RDAYJ
C     which is the same as DAYG + OFFSTJ.
C
      DAYJ = DAYG + OFFSTJ
C
C     Now that we have the Julian day number it's a fairly
C     straight forward task to get the year, month and day
C     on the Julian calendar.
C
 
      CALL RMAINI ( DAYJ, DP4Y,  M4,  TMPDAY )
      DAYJ = TMPDAY

      M    = MIN ( 3, DAYJ/DP1Y )
      DAYJ = DAYJ -      M*DP1Y
 
 
      DOFYR  = DAYJ + 1
      YR     = 4 * M4
     .       +     M
     .       +     1
 
C
C     Now look up the month number and compute the day of the month.
C     How we do this depends on whether or not this is a leap year.
C
      IF ( JLDAYS(YR) .EQ. 0 ) THEN
         MON  = LSTLTI ( DOFYR, 12, DPJAN0 )
         DY   = DOFYR  -            DPJAN0(MON)
      ELSE
         MON  = LSTLTI ( DOFYR, 12, DPBEGL )
         DY   = DOFYR  -            DPBEGL(MON)
      END IF
 
      YEAR    = YR
      MONTH   = MON
      DAY     = DY
      DOY     = DOFYR
 
      RETURN
      END
