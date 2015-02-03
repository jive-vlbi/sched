C$Procedure      TTRANS ( Time transformation )
 
      SUBROUTINE TTRANS ( FROM, TO, TVEC )

C$ Abstract
C
C     Transform a time vector from one representation and system
C     to another.
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
C     PARSING
C     TIME
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE               'zzctr.inc'

      INTEGER               MXCOMP
      PARAMETER           ( MXCOMP = 10 )
 
      CHARACTER*(*)         FROM
      CHARACTER*(*)         TO
      DOUBLE PRECISION      TVEC ( MXCOMP )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MXCOMP     P    maximum number of components allowed for TVEC.
C     TO         I    description of a time vector.
C     FROM       I    description of a time vector.
C     TVEC      I/O   time vector representing an epoch.
C
C$ Detailed_Input
C
C     TVEC       is called a time vector.  It is an array of double
C                precision numbers that represent some epoch.  To
C                determine its meaning you must examine the string
C                FROM.  Note that the number of significant entries
C                in TVEC is implied by FROM.
C
C     FROM       is a string used to describe the type of time vector
C     TO         TVEC.  FROM is the type of the input vector TVEC
C                TO is the type of the output TVEC
C
C                The interpretation of TVEC  is as follows:
C
C                TYPE      Interpretation of TVEC
C                ------    -------------------------------------------
C                YMD(F)  - year, month, day,   hour, minutes, seconds
C                YD(F)   - year,  day-of-year, hour, minutes, seconds
C                YD.D(F) - year, number of days past beginning of year
C                DAYSEC  - calendar days past 1 jan 1 AD,
C                          seconds past beg day
C                DP2000  - calendar days past 1 jan 2000,
C                          seconds past beg day
C                JDUTC   - julian date UTC.
C                FORMAL  - seconds in the formal calendar since J2000.
C                YWD(F)  - year, week, day, hour, minutes, seconds
C                YMWD(F) - year, month, week, day, hour, minutes,
C                          seconds
C                TAI     - atomic seconds past Atomic J2000.
C                TDT     - Terrestrial Dynamical Time
C                TDB     - Barycentric Dynamical Time
C                JED     - Julian Ephemeris Date (based on TDB)
C                ET      - Ephemeris time (same as TDB)
C                JDTDB   - Julian Date based on TDB (same as JED)
C                JDTDT   - Julian Date based on TDT
C
C                The number of components of TVEC implied by TYPE is
C                as follows:
C
C                   YMD     - 6
C                   YD      - 5
C                   JDUTC   - 1
C                   FORMAL  - 1
C                   YD.D    - 2
C                   DAYSEC  - 2
C                   DP2000  - 2
C                   YWD     - 6
C                   YMWD    - 7
C                   TAI     - 1
C                   TDT     - 1
C                   TDB     - 1
C                   JED     - 1
C                   ET      - 1
C                   JDTDB   - 1
C                   JDTDT   - 1
C
C
C                For all types, only the last component of the
C                time vector may be non-integer.  If other components
C                have fractional parts only their truncated integer
C                components will be recognized.
C
C                YMD and YD
C
C                   These types are assumed to be different
C                   representations on UTC time markers.  Thus
C                   the hour, minutes and seconds portions all
C                   represent time elapsed
C                   since the beginning of a day.  As such the
C                   seconds portion of HMS may range up to (but
C                   not include) 61 on days when positive leap
C                   seconds occur and may range up to (but not
C                   include) 59 on days during which negative
C                   leapseconds occur.
C
C                YD.D type.
C
C                   Y is the calendar year used in civil time keeping
C                   D is the day of the calendar year --- for any time
C                     during the first of January, the integer portion
C                     of the day will be 1.
C
C                     The fractional portion is the fractional part of
C                     the specific day.  Thus the amount of time
C                     specified by the fractional portion of the day
C                     depends upon whether or not the day has a leap
C                     second.  ".D" can be computed from the formula
C
C                           number of seconds past beginning of day
C                     .D = ---------------------------------------
C                              number of UTC seconds in the day.
C
C                FORMAL type.
C
C                   The FORMAL type for TVEC gives the number of
C                   seconds past the epoch J2000 (noon Jan 1 2000)
C                   on the formal calendar (no leap seconds ---
C                   all days contain 86400 seconds)  The formal clock
C                   is simply held still for one second during
C                   positive leap seconds.  Times during leap seconds
C                   cannot be represented in this system.
C
C                   This system is converted internally to a
C                   calendar days past epoch and seconds
C                   past beginning of day form.  For this reason,
C                   times that occur during a positive leap second
C                   can never be represented.  Moreover, if a negative
C                   leapsecond occurs, times that occur during the
C                   ``missing'' leapsecond will simply be placed
C                   at the beginning of the next day.  Thus two
C                   different FORMAL times can represent the
C                   same time around a negative leap second.
C
C                   FORMAL time is equivalent to somewhat parochial
C                   ``UTC seconds past J2000'' that is produced
C                   by the SPICE routine TPARSE.
C
C                JDUTC type.
C
C                   This system is similar to the FORMAL system
C                   described above.  All days are assumed to have
C                   86400 seconds.  All numbers of the form
C
C                      integer + 0.5
C
C                   fall at the beginning of calendar UTC days.
C
C                   There is no way to represent times during a
C                   positive leapsecond. Times during missing
C                   negative leap seconds are represented in two ways.
C
C                DAYSEC type.
C
C                   This time vector has the form of calendar
C                   days since January 1, of the year 1 A.D.
C                   and number of seconds past the beginning of the
C                   calendar day.
C                   (January 2 of the year 1 A.D. is 1 calendar
C                   day past January 1, 1 A.D.)
C
C                DP2000 type.
C
C                   This time vector has the same form as DAYSEC
C                   time vectors.  The only difference is that
C                   the reference epoch is JAN 1, 2000.
C
C                YWD and YMWD types.
C
C                   These time vectors are used to specify a time
C                   that are most conveniently expressed by phrases
C                   such as ``the third Monday of every month'' or
C                   ``Beginning with the second Wednesday of the new
C                     year and every 4th Wednesday thereafter.''
C
C                   The hours, minutes and seconds components of
C                   these time vectors are the
C                   same as for the Year-Month-Day and Year-Day UTC
C                   time vectors.
C
C                   The Y component refers to the calendar year, and
C                   in the YMWD vector, the M component refers to
C                   the calendar month.
C
C                   The W component refers to the week of the
C                   Year (YWD) or Month (YMWD).  The first week
C                   begins on the first day of the year or the first
C                   day of the month.  The D component is the day of the
C                   week with 1 corresponding to Sunday, 2 to Monday,
C                   and so on with 7 corresponding to Saturday.
C
C                   Thus the YMWD time vector
C
C                      1991
C                        11
C                         3
C                         5
C                        12
C                         0
C                         0
C
C                   refers to 12:00:00 on the third Thursday of
C                   November of 1991.
C
C                   The YWD time vector
C
C                      1997
C                        11
C                         4
C                        13
C                         5
C                        11
C
C                   refers to 12:05:11 on the eleventh Wednesday
C                   of 1997.
C
C                Formal Calendar Time Vectors
C                ============================
C                The types YMDF, YDF, YD.D(F), YWDF, YMWDF are similar
C                to the corresponding base types: YMD, YD, YD.D, YWD
C                and YMWD.  However, these types represent formal
C                time vectors.  Each day contains exactly 86400 seconds.
C                The difference between formal and non-formal systems
C                can only be seen during a positive leapsecond or
C                during the second following a negative leapsecond.
C
C                Epochs during a positive leapsecond on input are
C                placed in the first second of the next day.  Epochs
C                during a positive leapsecond on output are held
C                at 00:00:00 of the next day.
C
C                Epochs during the first second following a negative
C                leapsecond are counted as belonging to the previous
C                day if both the input and output types are formal
C                types.
C
C
C                Calendars
C                =====================
C                In all time vectors for which a year is specified,
C                the year is assumed to belong to the Gregorian
C                Calendar---every 4th year is a leapyear except
C                for centuries (such as 1900) that are not divisible
C                by 400.  This calendar is formally extended
C                indefinitely backward and forward in time.
C
C                Note that the Gregorian Calendar did not
C                formally exist prior to October 15, 1582. Prior to
C                that time the Julian Calendar was used (in the
C                Julian Calendar every 4th year is a leapyear, including
C                all centuries).
C
C                If you have epochs relative to the Julian calendar,
C                the SPICE routine JUL2GR is available for converting
C                to the formal Gregorian Calendar.
C
C
C                Epochs Prior to 1972
C                =====================
C                UTC as it exists today, was adopted in 1972.  For
C                epochs prior to 1972, it is assumed that the difference
C                between TAI and UTC is a constant value.
C
C                Years prior to 1 A.D.
C                =====================
C                A year belonging to the B.C. era,  may be
C                represented by subtracting the year from 1.
C                Thus to specify 27 B.C (Gregorian) set the
C                year component of the time vector to -26.
C
C
C                Notes:
C                ======
C                The FORMAL and JDUTC types should not be used
C                for times near a leap second.  However, for times
C                removed from leap seconds they pose no problems.
C
C                The DAYSEC and DP2000 are useful for representing
C                times that are given in atomic seconds past some
C                reference epoch other than J2000.
C
C$ Detailed_Output
C
C     TVEC       is the time vector corresponding to the input
C                time vector but with components consistent with
C                the type specified by input variable TO.
C
C$ Parameters
C
C     MXCOMP     is the maximum number of components that can appear in
C                TVEC.
C
C$ Exceptions
C
C     1) If the type of either FROM or TO is not recognized the
C        error 'SPICE(UNKNONWNTIMESYSTEM)' is signalled.
C
C     2) If a leapseconds kernel has not been loaded prior a call
C        to TTRANS the error  'SPICE(NOLEAPSECONDS)' is signalled.
C
C     3) If epochs associated with leapseconds in the leapseconds
C        kernel are not in increasing order, the error
C        'SPICE(BADLEAPSECONDS)' is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is the fundamental translator between various
C     representations of time in the SPICE system.  However, it
C     is intended to be a mid-level routine that few user's should
C     have need of calling.
C
C     In addition to translating between time systems, this routine
C     can be used to normalize the components of a time string
C     so that they are in the normal range for a particular
C     representation.  This allows you to easily do arithmetic
C     with epochs.
C
C$ Examples
C
C     Suppose you need to convert a time expressed as seconds
C     past J2000 (TDB) to Pacific Daylight time.  The following
C     example shows how you might use TTRANS to accomplish this
C     task.
C
C      TVEC(1) = ET
C
C      CALL TTRANS ( 'TDB', 'YMD', TVEC )
C
C      The seconds component of PDT is the same as the seconds
C      component of UTC.  We save and add the UTC-PDT offset
C      to the hours and minutes component of the time vector.
C
C      SECNDS  = TVEC(6)
C      TVEC(6) = 0.0D0
C
C      TVEC(4) = TVEC(4) - 7.0D0
C      TVEC(5) = TVEC(5) + 0.0D0
C
C      CALL TTRANS ( 'YMDF', 'YMDF', TVEC )
C
C      Now reset the seconds component to the original value
C      and pass the time vector to some formatting routine.
C
C      TVEC(6) = SECNDS
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
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.5.0, 09-SEP-2013 (BVS)
C
C        Updated to keep track of the POOL counter and call ZZCVPOOL.
C
C-    SPICELIB Version 1.4.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to this routine.
C
C-    SPICELIB Version 1.3.0, 15-NOV-2006 (NJB)
C
C        A reference to RTPOOL was replaced by a reference
C        to GDPOOL.
C
C-    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in RMAIND and RMAINI calls.  Changed reference to LDPOOL to
C        reference to FURNSH in an error message.
C
C-    SPICELIB Version 1.1.0, 9-JUN-1999 (WLT)
C
C        The routine was modified so that uniform time system
C        transformations (see UNITIM) are handled without
C        performing intermediate computations.  This gives a slight
C        improvement in the accuracy of some computations.
C
C        In addition, two unused variables were removed.
C
C-    Spicelib Version 1.0.0, 17-SEP-1996 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Convert from one time vector to another
C     Convert between various parsed time representations
C
C-&

C$ Revisions
C
C-    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in RMAIND and RMAINI calls.  Changed reference to LDPOOL to
C        reference to FURNSH in an error message.
C
C-& 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
      INTEGER               BSRCHC
      INTEGER               LSTLEI
      INTEGER               LSTLED
      INTEGER               LSTLTI
 
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      UNITIM
 
      LOGICAL               ODD
      LOGICAL               ELEMC
 
 
C
C     Local (in-line) functions
C
      INTEGER               DIVBLE
      INTEGER               DOY
      INTEGER               DP0001
      INTEGER               LDAYS
      INTEGER               YDAYS
 
      DOUBLE PRECISION      HMSSEC
 
C
C     Local parameters
C
C
C     Parameters
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
 
C
C     MAXLP is the maximum number of leap seconds that can be
C     stored internally.   The value of 140 should be sufficient
C     to store leap seconds through the year 2100.
C
      INTEGER               MAXLP
      PARAMETER           ( MAXLP = 140 )
 
C
C     MAXVAR is the number of kernel pool variables required by this
C     routine.
C
      INTEGER               MAXVAR
      PARAMETER           ( MAXVAR = 1 )
 
C
C
C     The following gives us an "enumeration" for all of the
C     various types of time vectors that are recognized.
C
C     DAYSEC
C     DAYP2
C     ET
C     FRML
C     JDTDB
C     JDTDT
C     JDUTC
C     JED
C     TAI
C     TDB
C     TDT
C     YD
C     YDD
C     YDDF
C     YDF
C     YMD
C     YMDF
C     YMWD
C     YMWDF
C     YWD
C     YWDF
C
 
      INTEGER               DAYSEC
      PARAMETER           ( DAYSEC = 1 )
 
      INTEGER               DAYP2
      PARAMETER           ( DAYP2 = DAYSEC + 1 )
 
      INTEGER               ET
      PARAMETER           ( ET     = DAYP2 + 1 )
 
      INTEGER               FRML
      PARAMETER           ( FRML = ET     + 1 )
 
      INTEGER               JDTDB
      PARAMETER           ( JDTDB  = FRML + 1 )
 
      INTEGER               JDTDT
      PARAMETER           ( JDTDT  = JDTDB  + 1 )
 
      INTEGER               JDUTC
      PARAMETER           ( JDUTC  = JDTDT  + 1 )
 
      INTEGER               JED
      PARAMETER           ( JED    = JDUTC  + 1 )
 
      INTEGER               TTAI
      PARAMETER           ( TTAI   = JED    + 1 )
 
      INTEGER               TDB
      PARAMETER           ( TDB    = TTAI   + 1 )
 
      INTEGER               TDT
      PARAMETER           ( TDT    = TDB    + 1 )
 
      INTEGER               YD
      PARAMETER           ( YD     = TDT    + 1 )
 
      INTEGER               YDD
      PARAMETER           ( YDD    = YD     + 1 )
 
      INTEGER               YDDF
      PARAMETER           ( YDDF   = YDD    + 1 )
 
      INTEGER               YDF
      PARAMETER           ( YDF    = YDDF   + 1 )
 
      INTEGER               YMD
      PARAMETER           ( YMD    = YDF    + 1 )
 
      INTEGER               YMDF
      PARAMETER           ( YMDF   = YMD    + 1 )
 
      INTEGER               YMWD
      PARAMETER           ( YMWD   = YMDF   + 1 )
 
      INTEGER               YMWDF
      PARAMETER           ( YMWDF  = YMWD   + 1 )
 
      INTEGER               YWD
      PARAMETER           ( YWD    = YMWDF  + 1 )
 
      INTEGER               YWDF
      PARAMETER           ( YWDF   = YWD    + 1 )
 
      INTEGER               NREC
      PARAMETER           ( NREC   = YWDF )
C
C     The following parameters just make the code seem a bit
C     more natural.
C
      LOGICAL               NO
      PARAMETER           ( NO  = .FALSE. )
 
      LOGICAL               YES
      PARAMETER           ( YES = .TRUE. )
 
C
C     Local variables
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
 
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN =  8 )
 
      CHARACTER*(TYPLEN)    UNIFRM (LBCELL : NREC )
 
      CHARACTER*(TYPLEN)    RECOG   ( NREC )
      INTEGER               PARSED  ( NREC )
      LOGICAL               NEEDY   ( NREC )
      LOGICAL               FORML   ( NREC )
      INTEGER               ORDVEC  ( NREC )
 
      CHARACTER*(32)        REST
      CHARACTER*(32)        MYFROM
      CHARACTER*(32)        MYTO
      CHARACTER*(32)        VARS   ( MAXVAR ) 
 
      DOUBLE PRECISION      DAYDP
      DOUBLE PRECISION      DAYLEN
      DOUBLE PRECISION      DP2000
      DOUBLE PRECISION      DT
      DOUBLE PRECISION      EXSECS
      DOUBLE PRECISION      FORMAL
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      HALFD
      DOUBLE PRECISION      HOURS
      DOUBLE PRECISION      JD1101
      DOUBLE PRECISION      JDSECS
      DOUBLE PRECISION      LASTDT
      DOUBLE PRECISION      MINS
      DOUBLE PRECISION      SECS
      DOUBLE PRECISION      SECSPD
      DOUBLE PRECISION      TAI
      DOUBLE PRECISION      TAITAB ( 2*MAXLP )
      DOUBLE PRECISION      TEMPD
      DOUBLE PRECISION      TSECS

      INTEGER               DAY
      INTEGER               DAYNUM
      INTEGER               DAYPTR
      INTEGER               DAYTAB ( 2*MAXLP )
      INTEGER               DN2000
      INTEGER               DOFFST
      INTEGER               DOFYR
      INTEGER               DPSUN
      INTEGER               DYEAR
      INTEGER               FMDAY
      INTEGER               FYRDAY
      INTEGER               I
      INTEGER               MONTH
      INTEGER               NREF
      INTEGER               OFFSET
      INTEGER               PFROM
      INTEGER               PTO
      INTEGER               QINT
      INTEGER               REFPTR
      INTEGER               REM
      INTEGER               SUNDAY
      INTEGER               TAIPTR
      INTEGER               TEMPI
      INTEGER               USRCTR ( CTRSIZ ) 
      INTEGER               WEEK
      INTEGER               WKDAY
      INTEGER               YEAR
      INTEGER               YR1
      INTEGER               YR100
      INTEGER               YR4
      INTEGER               YR400

      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               NODATA
      LOGICAL               UPDATE

C
C     The array EXTRA contains the number of many additional days that
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

      DATA                  NODATA  / .TRUE. /


C
C     Definitions of statment functions.
C
C     The number of days elapsed since Jan 1, of year 1 A.D. to
C     Jan 1 of YEAR is given by:
C
      YDAYS(YEAR) = 365*(YEAR-1)
     .            +    ((YEAR-1)/4)
     .            -    ((YEAR-1)/100)
     .            +    ((YEAR-1)/400)
 
C
C     Return 1 if YEAR is divisible by N, otherwise return 0.
C
      DIVBLE(YEAR,I) = MAX ( 0, 1 + ((IABS(YEAR)/I)*I) - IABS(YEAR) )
 
C
C     The number of leap days in a year is given by:
C
      LDAYS(YEAR) = DIVBLE( YEAR, 4   )
     .            - DIVBLE( YEAR, 100 )
     .            + DIVBLE( YEAR, 400 )
 
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
      DOY(YEAR,MONTH,DAY) = DPJAN0 ( MONTH )
     .                    + EXTRA  ( MONTH ) * LDAYS(YEAR)
     .                    + DAY
 
 
C
C     The number of days since 1 Jan 1 A.D. is given by:
C
      DP0001(YEAR,MONTH,DAY) = YDAYS(YEAR)
     .                       + DOY  (YEAR, MONTH, DAY)
     .                       - 1
 
 
C
C     The number of seconds represented by HOURS hours MINS minutes
C     and SECS seconds.
C
      HMSSEC ( HOURS, MINS, SECS ) = HOURS * 3600.0D0
     .                             + MINS  *   60.0D0
     .                             + SECS
 
 
 
 
 
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TTRANS' )
      END IF
 
C
C     The first time any of the entry points are called we
C     must set up the "watcher" for the kernel pool variables
C     that will be needed by this routine.
C
      IF ( FIRST ) THEN
 
         FIRST   = .FALSE.
 
         SECSPD  = SPD()
         HALFD   = SPD() / 2.0D0
 
         DN2000  = DP0001 ( 2000, 1, 1 )
         SUNDAY  = DP0001 ( 1991, 1, 6 )
 
         JD1101  = J2000() - DBLE(DN2000) - 0.5D0
 
C
C        Initialize the list of Uniform time systems.
C
         CALL SSIZEC ( NREC, UNIFRM )
 
C
C        Set up the set of recognized time vectors.
C
C        The following 4 parallel arrays are here
C        to assist in the task of classifying the
C        FROM and TO time representations. The arrays
C        contain:
C
C        RECOG   the strings that are recognized as legitimate
C                time representations
C
C        PARSED  a unique integer that can be used to stand
C                for each recognized format.  This is used
C                in the various IF THEN blocks to decide
C                how a time vector should be processed instead
C                of the name because integer compares are
C                much faster than string comparisons.
C
C        FORML   is a logical that indicates whether or not the
C                corresponding time system is a formal system
C                or UTC based system.  FORML(I) = YES implies
C                the time system is formal.  FORML(I) means it
C                isn't.
C
C        NEEDY   is a logical that indicates whether or not
C                there is a YEAR in the time system.  It should
C                be read "NEED Y" for "need year"  not "needy"
C                as when you are destitute.  NEEDY(I) = YES means
C                the time system has a year.  NEEDY(I) = NO means
C                it doesn't
C
         RECOG ( 01 ) = 'DAYSEC '
         PARSED( 01 ) =  DAYSEC
         FORML ( 01 ) =  NO
         NEEDY ( 01 ) =  NO
 
         RECOG ( 02 ) = 'DP2000 '
         PARSED( 02 ) =  DAYP2
         FORML ( 02 ) =  NO
         NEEDY ( 02 ) =  NO
 
         RECOG ( 03 ) = 'ET '
         PARSED( 03 ) =  ET
         FORML ( 03 ) =  NO
         NEEDY ( 03 ) =  NO
 
         CALL INSRTC ( 'ET', UNIFRM )
 
         RECOG ( 04 ) = 'FORMAL '
         PARSED( 04 ) =  FRML
         FORML ( 04 ) =  YES
         NEEDY ( 04 ) =  NO
 
         RECOG ( 05 ) = 'JDTDB '
         PARSED( 05 ) =  JDTDB
         FORML ( 05 ) =  NO
         NEEDY ( 05 ) =  NO
 
         CALL INSRTC ( 'JDTDB', UNIFRM )
 
         RECOG ( 06 ) = 'JDTDT '
         PARSED( 06 ) =  JDTDT
         FORML ( 06 ) =  NO
         NEEDY ( 06 ) =  NO
 
         CALL INSRTC ( 'JDTDT', UNIFRM )
 
         RECOG ( 07 ) = 'JDUTC '
         PARSED( 07 ) =  JDUTC
         FORML ( 07 ) =  YES
         NEEDY ( 07 ) =  NO
 
         RECOG ( 08 ) = 'JED '
         PARSED( 08 ) =  JED
         FORML ( 08 ) =  NO
         NEEDY ( 08 ) =  NO
 
         CALL INSRTC ( 'JED', UNIFRM )
 
         RECOG ( 09 ) = 'TAI '
         PARSED( 09 ) =  TTAI
         FORML ( 09 ) =  NO
         NEEDY ( 09 ) =  NO
 
         CALL INSRTC ( 'TAI', UNIFRM )
 
         RECOG ( 10 ) = 'TDB '
         PARSED( 10 ) =  TDB
         FORML ( 10 ) =  NO
         NEEDY ( 10 ) =  NO
 
         CALL INSRTC ( 'TDB', UNIFRM )
 
         RECOG ( 11 ) = 'TDT '
         PARSED( 11 ) =  TDT
         FORML ( 11 ) =  NO
         NEEDY ( 11 ) =  NO
 
         CALL INSRTC ( 'TDT', UNIFRM )
 
         RECOG ( 12 ) = 'YD '
         PARSED( 12 ) =  YD
         FORML ( 12 ) =  NO
         NEEDY ( 12 ) =  YES
 
         RECOG ( 13 ) = 'YD.D '
         PARSED( 13 ) =  YDD
         FORML ( 13 ) =  NO
         NEEDY ( 13 ) =  YES
 
         RECOG ( 14 ) = 'YD.DF '
         PARSED( 14 ) =  YDDF
         FORML ( 14 ) =  YES
         NEEDY ( 14 ) =  YES
 
         RECOG ( 15 ) = 'YDF '
         PARSED( 15 ) =  YDF
         FORML ( 15 ) =  YES
         NEEDY ( 15 ) =  YES
 
         RECOG ( 16 ) = 'YMD '
         PARSED( 16 ) =  YMD
         FORML ( 16 ) =  NO
         NEEDY ( 16 ) =  YES
 
         RECOG ( 17 ) = 'YMDF '
         PARSED( 17 ) =  YMDF
         FORML ( 17 ) =  YES
         NEEDY ( 17 ) =  YES
 
         RECOG ( 18 ) = 'YMWD '
         PARSED( 18 ) =  YMWD
         FORML ( 18 ) =  NO
         NEEDY ( 18 ) =  YES
 
         RECOG ( 19 ) = 'YMWDF '
         PARSED( 19 ) =  YMWDF
         FORML ( 19 ) =  YES
         NEEDY ( 19 ) =  YES
 
         RECOG ( 20 ) = 'YWD '
         PARSED( 20 ) =  YWD
         FORML ( 20 ) =  NO
         NEEDY ( 20 ) =  YES
 
         RECOG ( 21 ) = 'YWDF '
         PARSED( 21 ) =  YWDF
         FORML ( 21 ) =  YES
         NEEDY ( 21 ) =  YES
 
         CALL ORDERC ( RECOG,  NREC, ORDVEC )
         CALL REORDC ( ORDVEC, NREC, RECOG  )
         CALL REORDI ( ORDVEC, NREC, PARSED )
         CALL REORDL ( ORDVEC, NREC, FORML  )
         CALL REORDL ( ORDVEC, NREC, NEEDY  )
 
C
C        Initialize the local POOL counter to user value.
C
         CALL ZZCTRUIN( USRCTR )

C
C        Set up the kernel pool watchers
C
         VARS(1) = 'DELTET/DELTA_AT'
 
         CALL SWPOOL ( 'TTRANS', 1, VARS )
 
      END IF
 
C
C     Check to see if any of the kernel items required by this
C     routine have been updated since the last call to this
C     entry point.
C
      CALL ZZCVPOOL ( 'TTRANS', USRCTR, UPDATE )
 
      IF ( UPDATE .OR. NODATA ) THEN 
C
C        We load the TAI-UTC offsets and formal leapsecond epochs
C        into the TAITAB.  (We will modify this array in a minute).
C
         CALL GDPOOL ( 'DELTET/DELTA_AT',  1,       2*MAXLP, 
     .                 NREF,               TAITAB,  FOUND    )
 
C
C        Make sure all of the requested data was there.
C
         IF ( .NOT. FOUND ) THEN

            NODATA = .TRUE.

            CALL SETMSG ( 'The variable that points to the '          //
     .                    'leapseconds (DELTET/DELTA_AT) could not '  //
     .                    'be located in the kernel pool.  It is '    //
     .                    'likely that the leapseconds kernel has '   //
     .                    'not been loaded via the routine FURNSH.'   )
            CALL SIGERR ( 'SPICE(NOLEAPSECONDS)'                      )
            CALL CHKOUT ( 'TTRANS'                                    )
            RETURN

         END IF
 
 
C
C        Transform the TAITAB in place to give the TAI time tag
C        at the beginning of the UTC day in which a leap
C        second occurred and the TAI time tag at the beginning
C        of the next day.  Pictorially, the table is transformed
C
C               +----------------------+         +-------------------+
C               | DELTA_1 (TAI to UTC) |         | TAI at start of   |
C               |                      |         | day before TAI-UTC|
C               |                      |         | change occurred   |
C               +----------------------+         +-------------------+
C        from:  | First Formal time    |     to: | TAI time at start |
C               | associated with      |         | of next day UTC.  |
C               | DELTA_1              |         | after DELTA_1 jump|
C               +----------------------+         +-------------------+
C               | DELTA_2 (TAI to UTC) |         | TAI at start of   |
C               |                      |         | day before TAI-UTC|
C               |                      |         | jump occurred     |
C               +----------------------+         +-------------------+
C               | First Formal time    |         | TAI time at start |
C               | associated with      |         | of next day UTC.  |
C               | DELTA_2              |         | after DELTA_2 jump|
C               +----------------------+         +-------------------+
C                        .                                .
C                        .                                .
C                        .                                .
C
C
C        At the same time, load the table DAYTAB. It contains the
C        the day number past 1 Jan 1 AD for the beginning of the
C        days loaded in TAITAB.
C
         LASTDT = TAITAB(1) - 1.0D0
 
         DO I = 1, NREF, 2
 
            OFFSET         = I
            REFPTR         = I + 1
 
            DT             = TAITAB(OFFSET)
            FORMAL         = TAITAB(REFPTR)
            TAITAB(OFFSET) = FORMAL - SECSPD + LASTDT
            TAITAB(REFPTR) = FORMAL          + DT
 
            DAYNUM         = INT ( ( FORMAL + HALFD ) / SECSPD )
     .                     + DN2000
 
            DAYTAB(OFFSET) = DAYNUM - 1
            DAYTAB(REFPTR) = DAYNUM
 
            LASTDT         = DT
 
         END DO
 
C
C        Since we don't have to do it very often, make sure the
C        times in the TAI table are in increasing order.
C
         DO I = 2, NREF

            NODATA = .TRUE.
 
            IF ( TAITAB(I-1) .GE. TAITAB(I) ) THEN
               CALL SETMSG ( 'Either the leapsecond epochs taken '    //
     .                       'from the kernel pool are not properly ' //
     .                       'ordered or the UTC - TAI offsets '      //
     .                       'are completely out of range. '          )
               CALL SIGERR ( 'SPICE(BADLEAPSECONDS)'                  )
               CALL CHKOUT ( 'TTRANS'                                 )
               RETURN
            END IF
 
         END DO

C
C        At this point, we've completed all checks on kernel data.
C
         NODATA = .FALSE.

      END IF
 
C
C     Make local normalized copies of FROM and TO.
C
      CALL NEXTWD ( FROM,   MYFROM, REST )
      CALL NEXTWD ( TO,     MYTO,   REST )
      CALL UCASE  ( MYFROM, MYFROM       )
      CALL UCASE  ( MYTO,   MYTO         )
 
C
C     Make sure that the FROM and TO are recognized time types.
C
      PTO   = BSRCHC ( MYTO,   NREC, RECOG )
      PFROM = BSRCHC ( MYFROM, NREC, RECOG )
 
C
C     Eventually, we need to handle SCLKs.  When that happens
C     we will do it here and in a similarly marked spot at
C     the end of this routine.  First see if we know how to
C     handle the FROM system.
C
C     IF ( PFROM .EQ. 0 ) THEN
C
C        CALL ISSCLK ( FROM,ERROR, FOUND )
C
C        IF ( .NOT. FOUND ) THEN
C           IF ( ERROR .NE. ' ' ) THEN
C              CALL SETMSG ( ERROR )
C              CALL SIGERR ( 'SPICE(TIMESYSTEMPROBLEM)' )
C              CALL CHKOUT ( 'TTRANS' )
C              RETURN
C           END IF
C        ELSE
C           CALL SCLKTV ( FROM, TVEC )
C           PFROM = TDB
C        END IF
C
C     END IF
C
C     Now check to see if we know how to handle the  TO system.
C
C     IF ( PTO .EQ. 0 ) THEN
C
C        CALL ISSCLK ( TO, ERROR, FOUND )
C
C        IF ( .NOT. FOUND ) THEN
C
C           IF ( ERROR .NE. ' ' ) THEN
C              CALL SETMSG ( ERROR )
C              CALL SIGERR ( 'SPICE(TIMESYSTEMPROBLEM)' )
C              CALL CHKOUT ( 'TTRANS' )
C           END IF
C
C        ELSE
C
C           MKSCLK = .TRUE.
C           PTO    =  TDB
C
C        END IF
C
C     END IF
C
C
C     For now we are NOT going to deal with SCLK so if something
C     isn't recognized, we can just signal an error and quit.
C
      IF ( PFROM .EQ. 0 ) THEN
 
         CALL SETMSG ( 'The FROM time representation ''#'' is '
     .   //            'not recognized. ' )
         CALL ERRCH  ( '#', FROM                   )
         CALL SIGERR ( 'SPICE(UNKNONWNTIMESYSTEM)' )
         CALL CHKOUT ( 'TTRANS'                    )
         RETURN
 
      ELSE IF ( PTO .EQ. 0 ) THEN
 
         CALL SETMSG ( 'The TO time representation ''#'' is '
     .   //            'not recognized. ' )
         CALL ERRCH  ( '#', FROM                   )
         CALL SIGERR ( 'SPICE(UNKNONWNTIMESYSTEM)' )
         CALL CHKOUT ( 'TTRANS'                    )
         RETURN
 
      END IF
 
C
C     OK.  We have made our last attempt at diagnosing a user error.
C     From this point on we assume that the user input exactly what
C     was intended.
C
C     We convert the time vector to days past 1 jan 01 and seconds
C     past the beginning of the day.  None of the cases below
C     are particularly tricky.  There's just a lot of cases.
C
      IF      ( PFROM .EQ. YMD .OR. PFROM .EQ. YMDF) THEN
 
         YEAR   = INT( TVEC(1) )
         MONTH  = INT( TVEC(2) )
         DAY    = INT( TVEC(3) )
 
         CALL RMAINI ( MONTH-1, 12, DYEAR, MONTH )
 
         YEAR   = YEAR  + DYEAR
         MONTH  = MONTH + 1
         DOFFST = 0
 
         IF ( YEAR .LE. 0 ) THEN
 
            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI
 
            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF
 
            DOFFST = DP400Y * YR400
 
         END IF
 
         DAYNUM = DP0001 ( YEAR,          MONTH,         DAY     )
     .          + DOFFST
 
         SECS   = HMSSEC ( AINT(TVEC(4)), AINT(TVEC(5)), TVEC(6) )
 
      ELSE IF ( PFROM .EQ. YD  .OR. PFROM .EQ. YDF ) THEN
 
         YEAR   = INT( TVEC(1) )
         DAY    = INT( TVEC(2) )
         MONTH  = 1
 
         DOFFST = 0
 
         IF ( YEAR .LE. 0 ) THEN
 
            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI
 
            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF
 
            DOFFST = DP400Y * YR400
 
         END IF
 
         DAYNUM = DP0001 ( YEAR,          MONTH,         DAY     )
     .          + DOFFST
 
         SECS   = HMSSEC ( AINT(TVEC(3)), AINT(TVEC(4)), TVEC(5) )
 
      ELSE IF ( PFROM .EQ. YDD .OR. PFROM .EQ. YDDF   ) THEN
 
         YEAR   = INT(TVEC(1))
         DAY    = INT(TVEC(2))
         MONTH  = 1
 
         DOFFST = 0
 
         IF ( YEAR .LE. 0 ) THEN
 
            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI

            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF
 
            DOFFST = DP400Y * YR400
 
         END IF
 
         FRAC   = TVEC(2) - DBLE(DAY)
         DAYNUM = DP0001  ( YEAR, MONTH, DAY )
     .          + DOFFST
 
C
C        Normally the length of a day is 86400 seconds, but this day
C        might be a leapsecond day.  We will set DAYLEN to SECSPD and
C        change it if it turns out this is a day with a leapsecond.
C
         IF ( PFROM .EQ. YDDF ) THEN
            SECS = FRAC * SECSPD
         ELSE
            DAYLEN = SECSPD
            DAYPTR = LSTLEI ( DAYNUM, NREF, DAYTAB )
 
            IF ( ODD(DAYPTR) ) THEN
               DAYLEN = TAITAB(DAYPTR+1) - TAITAB(DAYPTR)
            END IF
 
            SECS = FRAC * DAYLEN
         END IF
 
 
      ELSE IF ( PFROM .EQ. FRML ) THEN
 
C
C        First lets get the number of days since 1-Jan-2000 00:00:00
C
         CALL RMAIND ( TVEC(1) + HALFD, SECSPD, DP2000, SECS )
 
         DAYNUM = INT(DP2000) + DN2000
 
 
 
      ELSE IF ( PFROM .EQ. JDUTC  ) THEN
 
C
C        JD1101 is the julian date UTC of Jan 1, 1 AD.
C
         JDSECS = (TVEC(1) - JD1101)*SECSPD
 
         CALL RMAIND ( JDSECS, SECSPD, DAYDP, SECS )
 
         DAYNUM = INT(DAYDP)
 
 
 
      ELSE IF ( PFROM .EQ. DAYSEC ) THEN
 
         DAYNUM = INT(TVEC(1))
         SECS   = TVEC(2)
 
 
 
      ELSE IF ( PFROM .EQ. DAYP2 ) THEN
 
         DAYNUM = INT(TVEC(1)) + DN2000
         SECS   = TVEC(2)
 
 
 
      ELSE IF ( PFROM .EQ. YWD .OR. PFROM .EQ. YWDF ) THEN
 
         YEAR   = INT( TVEC(1) )
         WEEK   = INT( TVEC(2) ) - 1
         WKDAY  = INT( TVEC(3) )
         MONTH  = 1
 
C
C        Compute the days past 1 jan 1 of the beginning of this
C        year and month.
C
 
         DOFFST = 0
 
         IF ( YEAR .LE. 0 ) THEN
 
            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI
 
            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF
 
            DOFFST = DP400Y * YR400
 
         END IF
 
         DAYNUM = DP0001 ( YEAR, MONTH, 1 )
     .          + DOFFST
 
         CALL RMAINI ( DAYNUM - SUNDAY, 7, QINT, DPSUN )
 
         FYRDAY = DPSUN + 1
 
         CALL RMAINI ( WKDAY - FYRDAY, 7,  QINT, OFFSET )
 
         DAYNUM = DAYNUM + WEEK*7       + OFFSET
         SECS   = HMSSEC ( AINT(TVEC(4)), AINT(TVEC(5)), TVEC(6) )
 
 
 
      ELSE IF ( PFROM .EQ. YMWD .OR. PFROM .EQ. YMWDF ) THEN
 
         YEAR   = INT( TVEC(1) )
         MONTH  = INT( TVEC(2) )
         WEEK   = INT( TVEC(3) ) - 1
         DAY    = INT( TVEC(4) )
 
         DOFFST = 0
 
         IF ( YEAR .LE. 0 ) THEN
 
            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI
 
            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF
 
            DOFFST = DP400Y * YR400
 
         END IF
 
         DAYNUM = DP0001 ( YEAR, MONTH, 1 )
     .          + DOFFST
 
         CALL RMAINI ( DAYNUM - SUNDAY, 7, QINT, DPSUN  )
         FMDAY  = DPSUN + 1
 
         CALL RMAINI ( DAY - FMDAY, 7, QINT, OFFSET )
 
         DAYNUM = DAYNUM + WEEK*7 + OFFSET
         SECS   = HMSSEC ( TVEC(5), TVEC(6), TVEC(7) )
 
 
 
 
C
C     If we get to this point the type must be one of the continuous
C     time types: 'TAI', 'TDT', 'TDB', 'JED', 'ET', 'JDTDT', 'JDTDB'.
C
      ELSE
C
C        If the output time is one of the continuous time systems
C        we can take a short cut and just perform the computation
C        directly.
C
         IF ( ELEMC ( MYTO, UNIFRM ) ) THEN
 
            TVEC(1) = UNITIM ( TVEC(1), MYFROM, MYTO )
            CALL CHKOUT ( 'TTRANS' )
            RETURN
 
         END IF
 
C
C        The output time system isn't one of the uniform time systems.
C        Convert what we have to TAI and then to the DAYNUM, SECOND
C        representation.
C
         TAI    = UNITIM ( TVEC(1), MYFROM, 'TAI'   )
         TAIPTR = LSTLED ( TAI,     NREF,    TAITAB )
 
C
C        If the TAIPTR value is odd, then the TAI time falls during
C        a day with a leap second.  We can just look up the day
C        number and compute the number of seconds into that
C        day directly ...
C
         IF ( ODD(TAIPTR) ) THEN
 
            DAYNUM = DAYTAB (TAIPTR)
            SECS   = TAI
     .             - TAITAB (TAIPTR)
 
C
C        ...Otherwise, all days since the reference TAI time have
C        the same number of seconds (SECSPD).  (This statement applies
C        to days that precede the first reference TAI time too.)
C        Thus we can simply compute the number of days and seconds
C        that have elapsed since the reference TAI time.
C
         ELSE
 
C
C           If TAI is before the first time in the table, we can
C           compute the number of days and seconds before the first
C           entry in the TAI table.
C
            TAIPTR = MAX ( TAIPTR, 1)
 
            CALL  RMAIND ( TAI     - TAITAB(TAIPTR), SECSPD,
     .                     DAYDP,                    SECS    )
 
            DAYNUM = INT ( DAYDP ) + DAYTAB(TAIPTR)
 
         END IF
 
 
      END IF
 
      IF ( FORML ( PFROM )  ) THEN
 
         CALL RMAIND ( SECS, SECSPD, DAYDP, TSECS )
         DAYNUM = DAYNUM + INT(DAYDP)
         SECS   = TSECS
 
      END IF
 
 
C     ==================================================================
C
C     Force the seconds into the range 0 to 86401 or 86400
C     depending upon whether or not the output system is a formal
C     time system or not.
C
      IF ( FORML(PTO) .AND. FORML(PFROM) ) THEN
C
C        We don't have to do anything here.
C
      ELSE
 
         IF (      SECS .GT. SECSPD - 1.0D0
     .        .OR. SECS .LT. 0.0D0          ) THEN
 
C
C           First convert to TAI...
C
            DAYPTR = MAX( 1, LSTLEI ( DAYNUM, NREF, DAYTAB ) )
            SECS   = SECS
     .             + DBLE ( DAYNUM - DAYTAB(DAYPTR) ) * SECSPD
            TAI    = TAITAB(DAYPTR) + SECS
 
C
C           ...then back to DAYNUM and SECS
C
            TAIPTR = LSTLED ( TAI, NREF, TAITAB )
 
            IF ( ODD(TAIPTR) ) THEN
 
               DAYNUM = DAYTAB ( TAIPTR )
               SECS   = TAI
     .                - TAITAB ( TAIPTR )
 
            ELSE
 
               TAIPTR = MAX ( 1, TAIPTR )
               DAYNUM = DAYTAB ( TAIPTR )
 
               CALL RMAIND ( TAI - TAITAB(TAIPTR), SECSPD, DAYDP, SECS )
 
               DAYNUM = DAYNUM + INT ( DAYDP )
 
            END IF
         END IF
 
      END IF
C
C     One last thing.  If we are going to a formal time vector,
C     we want to ignore positive leapseconds. (Negative ones
C     were handled above, the clock jumped ahead one second
C     when the second hand got to 59.)
C
C     The idea is that we want the clock
C     to stand still during the leapsecond.  Yeah this is bogus,
C     but people with analog clocks don't have any other choice.
C
C     We are in a positive leapsecond only if SECS is greater than
C     the number of seconds in a normal day.  In that case we
C     increment the day number by one and set SECS to zero.
C
 
      IF ( FORML(PTO) .AND. SECS .GT. SECSPD ) THEN
         DAYNUM = DAYNUM + 1
         SECS   = 0
      END IF
C
C     OK. Now we have DAYNUM and SECS,  convert this form to the
C     one requested.
C
C     If there is a 'Y' in the form we are to convert to, then we
C     will need some form of year, etc.  Do the work now and sort it
C     it all out at the appropriate time later on.
C
      IF ( NEEDY ( PTO )  ) THEN
 
         YR400  =  DAYNUM / DP400Y
         REM    =  DAYNUM - DP400Y*YR400
C
C        We want to be able to deal with years prior to  1 Jan 1
C        So we make sure the remainder is positive.
C
         IF ( REM .LT. 0 ) THEN
            YR400 = YR400 - 1
            REM   = REM + DP400Y
         END IF
 
         YR100  =  MIN ( 3, REM   / DP100Y )
         REM    =  REM    - YR100 * DP100Y
 
         YR4    =  MIN (24, REM / DP4Y )
         REM    =  REM    - YR4 * DP4Y
 
         YR1    =  MIN (3,  REM / DP1Y )
         REM    =  REM    - YR1 * DP1Y
 
         DOFYR  =  REM    + 1
 
         YEAR   = YR400*400 + YR100*100 + YR4*4 + YR1 + 1
 
         IF ( LDAYS(YEAR) .EQ. 0 ) THEN
            MONTH  = LSTLTI ( DOFYR, 12, DPJAN0 )
            DAY    = DOFYR - DPJAN0(MONTH)
         ELSE
            MONTH  = LSTLTI ( DOFYR, 12, DPBEGL )
            DAY    = DOFYR - DPBEGL(MONTH)
         END IF
 
 
C
C        We only want to convert that portion of seconds less than
C        86399 to hours, minutes and seconds.  Take anything extra
C        and put it in EXSECS.
C
         EXSECS = MAX ( 0.0D0, SECS - SECSPD + 1 )
         TSECS  = SECS - EXSECS
 
         CALL RMAIND ( TSECS, 3600.0D0, HOURS, TEMPD )
         CALL RMAIND ( TEMPD,   60.0D0, MINS,  TSECS )
 
         TSECS  = TSECS + EXSECS
 
      END IF
 
C=====================================================================
C
C     Finally, we convert to the requested output.
C
      IF      ( PTO .EQ. YMD .OR. PTO .EQ. YMDF ) THEN
 
         TVEC(1) = DBLE ( YEAR  )
         TVEC(2) = DBLE ( MONTH )
         TVEC(3) = DBLE ( DAY   )
         TVEC(4) = HOURS
         TVEC(5) = MINS
         TVEC(6) = TSECS
 
      ELSE IF ( PTO .EQ. YD .OR. PTO .EQ. YDF  ) THEN
 
         TVEC(1) = DBLE ( YEAR  )
         TVEC(2) = DBLE ( DOFYR )
         TVEC(3) = HOURS
         TVEC(4) = MINS
         TVEC(5) = TSECS
 
      ELSE IF ( PTO .EQ. YDD .OR. PTO .EQ. YDDF   ) THEN
 
         TVEC(1) = DBLE   ( YEAR  )
 
         IF ( PTO .EQ. YDD ) THEN
            DAYPTR  = LSTLEI ( DAYNUM, NREF, DAYTAB )
            DAYLEN  = SECSPD
 
            IF ( ODD(DAYPTR) ) THEN
               DAYLEN = TAITAB(DAYPTR+1) - TAITAB(DAYPTR)
            END IF
 
            TVEC(2) = DBLE(DOFYR) + SECS/DAYLEN
         ELSE
            TVEC(2) = DBLE(DOFYR) + SECS/SECSPD
         END IF
 
 
      ELSE IF ( PTO .EQ. FRML ) THEN
 
         TVEC(1) = DBLE ( DAYNUM - DN2000 ) * SECSPD
     .           - HALFD
     .           + SECS
 
      ELSE IF ( PTO .EQ. JDUTC  ) THEN
 
         TVEC(1) = JD1101 + DBLE( DAYNUM ) + SECS/SECSPD
 
      ELSE IF ( PTO .EQ. DAYSEC ) THEN
 
         TVEC(1) = DBLE(DAYNUM)
         TVEC(2) = SECS
 
      ELSE IF ( PTO .EQ. DAYP2 ) THEN
 
         TVEC(1) = DBLE (DAYNUM - DN2000)
         TVEC(2) = SECS
 
      ELSE IF ( PTO  .EQ. YWD .OR. PTO .EQ. YWDF ) THEN
C
C        First compute the day of the week, and the week number
C
         CALL   RMAINI  ( DAYNUM - SUNDAY, 7, QINT, DAY )
         WEEK    = 1 + (( DOFYR  - 1 )   / 7)
 
C
C        Now just put everything where it belongs.
C
         TVEC(1) = DBLE(YEAR)
         TVEC(2) = DBLE(WEEK)
         TVEC(3) = DBLE(DAY) + 1.0D0
         TVEC(4) = HOURS
         TVEC(5) = MINS
         TVEC(6) = TSECS
 
      ELSE IF ( PTO .EQ. YMWD .OR. PTO .EQ. YMWDF ) THEN
 
C
C        First compute how many weeks into the month DAYNUM is,
C        and compute the day of week number.
C
         TVEC(1) = DBLE (YEAR)
         DOFFST  = 0
 
         IF ( YEAR .LE. 0 ) THEN
 
            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI

            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF
 
            DOFFST = DP400Y * YR400
 
         END IF
 
         WEEK    = 1 + ( DAYNUM - DP0001(YEAR, MONTH, 1) - DOFFST ) / 7
         CALL RMAINI   ( DAYNUM - SUNDAY,   7, QINT, DAY )
 
C
C        Now just move the remaining stuff into TVEC.
C
         TVEC(2) = DBLE (MONTH)
         TVEC(3) = DBLE (WEEK)
         TVEC(4) = DBLE (DAY) + 1.0D0
         TVEC(5) = HOURS
         TVEC(6) = MINS
         TVEC(7) = TSECS
 
C
C     If we get to this point the type must be one of the continuous
C     time types: 'TAI', 'TDT', 'TDB', 'JED', 'ET', 'JDTDT', 'JDTDB'.
C
C     First convert to TAI and then to the appropriate output type.
C
      ELSE
 
         DAYPTR  = MAX( 1, LSTLEI ( DAYNUM, NREF, DAYTAB ) )
         SECS    = SECS
     .           + DBLE ( DAYNUM - DAYTAB(DAYPTR) ) * SECSPD
         TAI     = TAITAB(DAYPTR) + SECS
 
         TVEC(1) = UNITIM ( TAI, 'TAI', MYTO )
 
      END IF
 
 
C
C     Here's where we will handle conversion to SCLK when
C     we get around to implementing that portion of TTRANS
C
C
C     IF ( MKSCLK ) THEN
C        CALL TVSCLK ( TO, TVEC )
C     END IF
C
C     END IF
C
 
      CALL CHKOUT ( 'TTRANS' )
      RETURN
      END
