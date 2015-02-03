C$Procedure      TCHECK ( Time Check )
 
      SUBROUTINE TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
 
C$ Abstract
C
C     If component checking is enabled, this routine
C     determines whether the components of a time vector are in
C     the "usual" range for the components.  If component checking
C     is not enabled, this routine simply returns after setting
C     the outputs.
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
 
      IMPLICIT NONE
      DOUBLE PRECISION      TVEC   ( * )
      CHARACTER*(*)         TYPE
      LOGICAL               MODS
      CHARACTER*(*)         MODIFY ( * )
      LOGICAL               OK
      CHARACTER*(*)         ERROR
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TVEC       I   A vector of time components
C     TYPE       I   The type of time vector.
C     MODS       I   A logical indicating the presence of modifiers
C     MODIFY     I   The values of the modifiers
C     OK         O   Indicates success or failure of component checks.
C     ERROR      O   Diagnostic message if .NOT. OK.
C
C$ Detailed_Input
C
C     TVEC       is an array of double precision numbers that
C                represent the components of some calendar epoch.
C
C     TYPE       is kind of calendar epoch represented by TVEC
C                legitimate values are 'YMD' and 'YD'
C
C     MODS       is a logical flag indicating whether any of the
C                items in MODIFY are non-blank.  If some item
C                in MODIFY is non-blank, MODS will be TRUE.  If
C                all items in MODIFY are blank, MODS will be FALSE.
C
C     MODIFY     is an array of strings indicating how the
C                interpretation of the various components of TVEC
C                should be modified.  Blank values indicate that
C                the default interpretation should be applied.
C                Non-blank components will have the following values
C                and meanings.
C
C
C               Component   Meaning   Possible Non-blank Modifier Values
C               ---------   -------   ----------------------------------
C               1           ERA       'A.D.', 'B.C.'
C               2           Weekday   'SUN',  'MON', ... etc.
C               3           AM/PM     'A.M.', 'P.M.'
C               4           System    'UTC',  'TDB', 'TDT'
C               5           Time Zone 'UTC+i:i', 'UTC-i:i'
C
C
C$ Detailed_Output
C
C     OK        is returned TRUE if all components of TVEC are within
C               the normal range of values.  If some problem arises,
C               OK will be returned with the value FALSE.  Note that
C               component checking has not been enabled by a call
C               to TPARCH, the value of OK is automatically set to
C               TRUE.
C
C     ERROR     If OK is returned with the value TRUE, ERROR will be
C               returned as a blank.  However, if OK is FALSE, ERROR
C               will contain a diagnostic indicating what was wrong
C               with the components of TVEC. Note that
C               component checking has not been enabled by a call
C               to TPARCH, the value of ERROR is automatically set to
C               a blank.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) All problems with TVEC are diagnosed via the logical OK
C        and the message ERROR.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine works in conjunction with the entry point TPARCH.
C     If TPARCH has not been called with the input value 'YES' this
C     routine simply sets the outputs as indicated above and returns.
C
C     Usually strings such as February 32, 1997 are regarded as
C     erroneous.  However, the SPICE time subsystem is capable
C     of attaching meaning to such strings.  The routines TPARCH and
C     TCHECK allow you to treat such strings as erroneous throughout
C     the SPICE time sub-system.
C
C     This routine examines the components of a time vector and
C     determines whether or not all of the values in the vector
C     are within the normal bounds.
C
C     To pass inspection:
C
C        Years must be integers.
C
C        Months must be in the range from 1 to 12 and must be integers.
C
C        Days of the month must be in the normal ranges.  For example
C             if the month specified is January, the day of the month
C             must be greater than or equal to 1.0D0 and strictly less
C             than 32.0D0 (The normal range for February is a function
C             of whether the year specified is a leap year. The
C             Gregorian calendar is used to determine leap years.)
C
C        Day of the year must be greater than or equal to 1.0D0
C             and strictly less than 366.0D0  (367.0D0 in a leap year.
C             The Gregorian calendar is used to determine leap years.)
C
C        Hours must be greater than or equal to 0.0D0 and strictly
C             less than 24.0D0.  If the AMPM modifier is included
C             hours must be greater than or equal to 1.0D0 and strictly
C             less than 13.0D0.
C
C        Minutes must be greater than or equal to 0.0D0 and must
C             be strictly less than 60.0D0
C
C        Seconds must be greater than or equal to 0.0D0 and strictly
C             less than 60.0D0 (61.0D0 during the last minute of the
C             30th of June and the 31st of December).
C
C        If some component other than the seconds component is
C        not an integer, all components of lesser significance must
C        be zero.
C
C     This routine  is designed to work in conjunction
C     with the SPICE routine TPARTV and it is anticipated that
C     it will be called in the following fashion
C
C        CALL TPARTV ( STRING, TVEC, NTVEC,  TYPE,
C    .                 MODIFY, MODS, YABBRV, SUCCES, ERROR )
C
C        IF ( .NOT. SUCCES ) THEN
C
C           communicate the diagnostic message and
C           take other actions as appropriate
C
C           RETURN
C
C        END IF
C
C        IF ( SUCCES .AND. CHECK ) THEN
C            CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
C        END IF
C
C        IF ( .NOT. OK ) THEN
C
C           communicate the diagnostic message and
C           take other actions as appropriate
C
C           RETURN
C
C        END IF
C
C$ Examples
C
C     Suppose that you have parsed a string (via TPARTV) and want
C     to enforce normal ranges of the components.  The following
C     sequence of calls will perform the checks on components.
C
C        get the current checking setting
C
C        CALL TCHCKD ( CURNT )
C
C        turn on component checking.
C
C        CALL TPARCH ( 'YES' )
C
C        Check the components.
C
C        CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
C
C        Reset the checking setting to the original value.
C
C        CALL TPARCH ( CURNT )
C
C
C        Now handle any problems that were diagnosed by TCHECK
C
C        IF ( .NOT. OK ) THEN
C
C           do something
C
C        END IF
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
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1 10-FEB-2014 (BVS)
C
C        Fixed typo in the Declarations section in the TPARCH header:
C        STRING -> TYPE.
C
C-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Check the components of a time vector
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EQSTR
 
C
C     In-line Functions
C
      INTEGER               DIVBLE
 
 
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
 
      INTEGER               NMODS
      PARAMETER           ( NMODS = SYSTEM )
 
 
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )
 
      CHARACTER*(LNSIZE)    MESSGE
 
      CHARACTER*(10)        MNAMES ( 12 )
      CHARACTER*(7)         CNAME  (  4 )
 
      DOUBLE PRECISION      DINMON ( 12 )
 
      DOUBLE PRECISION      DOY
      DOUBLE PRECISION      DINYR
      DOUBLE PRECISION      JUN30
      DOUBLE PRECISION      HUBND
      DOUBLE PRECISION      HLBND
 
      INTEGER               COMP
      INTEGER               DAY
      INTEGER               HOUR
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LEAPDY
      INTEGER               MINUTE
      INTEGER               MONTH
      INTEGER               SECOND
      INTEGER               YEAR
 
      LOGICAL               DOCHCK
 
      SAVE
 
      DATA                  DOCHCK / .FALSE. /
 
      DATA                  DINMON / 31.0D0,  28.0D0,
     .                               31.0D0,  30.0D0,
     .                               31.0D0,  30.0D0,
     .                               31.0D0,  31.0D0,
     .                               30.0D0,  31.0D0,
     .                               30.0D0,  31.0D0  /
 
      DATA                  MNAMES / 'January',    'February',
     .                               'March',      'April',
     .                               'May',        'June',
     .                               'July',       'August',
     .                               'September',  'October',
     .                               'November',   'December' /
 
      DATA                  CNAME / 'days',        'hours',
     .                              'minutes',     'seconds' /
C
C     The in-line function DIVBLE returns 1 if YEAR is divisible
C     by I,  it returns 0 otherwise.
C
      DIVBLE(YEAR,I) = MAX ( 0, 1 + ((IABS(YEAR)/I)*I) - IABS(YEAR) )
C
C     If checking isn't enabled, there is nothing to do.
C
      IF ( .NOT. DOCHCK ) THEN
         OK    = .TRUE.
         ERROR = ' '
         RETURN
      END IF
C
C     Ok.  Checking has been enabled.  Proceed with the various
C     checks.
C
      YEAR      = NINT( TVEC(1) )
 
      LEAPDY    =  DIVBLE( YEAR, 4   )
     .          -  DIVBLE( YEAR, 100 )
     .          +  DIVBLE( YEAR, 400 )
 
      DINMON(2) = 28.0D0  + DBLE(LEAPDY)
      DINYR     = 365.0D0 + DBLE(LEAPDY)
      JUN30     = 181.0D0 + DBLE(LEAPDY)
C
C     The error message that will be attached to an out of range
C     problem for hours depends upon whether the AMPM modifier
C     was specified.  We set up valid range as well as the out
C     of range messages here.
C
      IF ( MODS .AND. MODIFY(AMPM) .NE. ' ' ) THEN
 
         HUBND  = 13.0D0
         HLBND  =  1.0D0
         MESSGE = 'The hours component of the time specified was '
     .   //       '#. When either A.M. or P.M. is specified with '
     .   //       'the time the hours component must be at least '
     .   //       '1.0D0 and less than 13.0D0. '
 
      ELSE
 
         HUBND  = 24.0D0
         HLBND  =  0.0D0
         MESSGE = 'The hours component of the time specified was '
     .   //       '#.  The hours component must be greater than '
     .   //       'or equal to 0.0D0 and less than 24.0D0. '
 
      END IF
C
C     We only check YD and YMD anything else is out of the
C     province of this routine.
C
      IF ( TYPE .NE. 'YD' .AND. TYPE .NE. 'YMD' ) THEN
 
         OK    = .FALSE.
         ERROR = 'The type of the time vector specified was #, '
     .   //      'only ''YD'' and ''YMD'' are recognized. '
         CALL REPMC ( ERROR, '#', TYPE, ERROR )
         RETURN
 
      END IF
 
 
C
C     First check.  The year must be an integer.
C
      IF ( TVEC(1) .NE. DBLE(YEAR) ) THEN
         OK    = .FALSE.
         ERROR = 'The year value was #.  This must be an '
     .   //      'integral value. '
         CALL REPMD ( ERROR,  '#', TVEC(1), 8, ERROR )
 
         RETURN
      END IF
 
 
      IF      ( TYPE .EQ. 'YD' ) THEN
 
         DAY    = 2
         HOUR   = 3
         MINUTE = 4
         SECOND = 5
         DOY    = TVEC(2)
 
         IF (      TVEC(2) .GE. DINYR + 1.0D0
     .       .OR.  TVEC(2) .LT. 1.0D0       ) THEN
 
            OK    = .FALSE.
            ERROR = 'Day # has been specified for the year #. '
     .      //      'The correct range for the day of year for '
     .      //      'this year is from 1 to #. '
 
            CALL REPMD ( ERROR, '#', TVEC(2), 8, ERROR )
            CALL REPMI ( ERROR, '#', YEAR,       ERROR )
            CALL REPMI ( ERROR, '#', 365+LEAPDY, ERROR )
            RETURN
 
         END IF
 
 
      ELSE IF ( TYPE .EQ. 'YMD' ) THEN
 
         MONTH  = NINT( TVEC(2) )
         DAY    = 3
         HOUR   = 4
         MINUTE = 5
         SECOND = 6
         DOY    = 0.0D0
 
         IF ( TVEC(2) .NE. DBLE(MONTH) ) THEN
 
            OK    = .FALSE.
            ERROR = 'The month specified, #, was not an integer. '
     .      //      'The month must be an integer in the range '
     .      //      'from 1 to 12. '
            CALL REPMD ( ERROR,  '#', TVEC(2), 3, ERROR )
            RETURN
 
         ELSE IF (      TVEC(2) .LT.   1.0D0
     .             .OR. TVEC(2) .GT.  12.0D0 ) THEN
 
            OK    = .FALSE.
            ERROR = 'The month specified was #.  The month must '
     .      //      'be an integer in the range from 1 to 12 '
     .      //      '(inclusive). '
            CALL REPMI ( ERROR, '#', MONTH, ERROR )
            RETURN
 
         ELSE IF (      TVEC(3) .LT. 1.0D0
     .             .OR. TVEC(3) .GE. DINMON(MONTH) + 1.0D0 ) THEN
 
            OK    = .FALSE.
            ERROR = 'The day of the month specified for the '
     .      //      'month of # was #.  For # the day must be at '
     .      //      'least 1.0D0 and less than #. '
            CALL REPMC ( ERROR, '#', MNAMES(MONTH), ERROR )
            CALL REPMD ( ERROR, '#', TVEC(3), 3,    ERROR )
            CALL REPMC ( ERROR, '#', MNAMES(MONTH), ERROR )
            CALL REPMD ( ERROR, '#', DINMON(MONTH)+1.0D0, 2, ERROR )
            RETURN
 
         END IF
 
         DO I = 1, MONTH -1
            DOY = DOY + DINMON(I)
         END DO
 
         DOY = DOY + TVEC(3)
 
 
      END IF
 
 
C
C     Make sure the hours, minutes and seconds are all in range.
C
      IF (     TVEC(HOUR) .GE. HUBND
     .    .OR. TVEC(HOUR) .LT. HLBND ) THEN
 
         OK    = .FALSE.
         ERROR = MESSGE
         CALL REPMD ( ERROR,  '#', TVEC(HOUR), 2, ERROR )
         RETURN
 
      ELSE IF (     TVEC(MINUTE) .GE. 60.0D0
     .         .OR. TVEC(MINUTE) .LT.  0.0D0 ) THEN
 
         OK    = .FALSE.
         ERROR = 'The minutes component of the time specified '
     .   //      'was #. This value must be greater than or '
     .   //      'equal to 0.0 and less than 60.0. '
         CALL REPMD ( ERROR,  '#', TVEC(MINUTE), 2, ERROR )
         RETURN
 
      END IF
 
      IF (     TVEC(SECOND) .GE. 60.0D0
     .    .OR. TVEC(SECOND) .LT.  0.0D0 ) THEN
C
C        We allow for the possibility that we might have a leapsecond.
C
         IF (        TVEC(SECOND) .LT. 61.0D0
     .        .AND.  TVEC(SECOND) .GT.  0.0D0
     .        .AND.  TVEC(MINUTE) .EQ. 59.0D0
     .        .AND.  TVEC(HOUR)   .EQ. 23.0D0
     .        .AND.( DOY .EQ. DINYR .OR. DOY .EQ. JUN30 ) ) THEN
C
C           Don't do anything.
C
         ELSE IF (        TVEC(SECOND) .LT. 61.0D0
     .        .AND.  TVEC(SECOND) .GT.  0.0D0
     .        .AND.  TVEC(MINUTE) .EQ. 59.0D0
     .        .AND.  TVEC(HOUR)   .EQ. 11.0D0
     .        .AND.  MODS
     .        .AND.  MODIFY(AMPM) .EQ. 'P.M.'
     .        .AND.( DOY .EQ. DINYR .OR. DOY .EQ. JUN30 ) ) THEN
C
C           Don't do anything.
C
         ELSE
 
            OK    = .FALSE.
            ERROR = 'The seconds component of time must be at least '
     .      //      '0.0D0 and less than 60.0D0 (61.0D0 during the '
     .      //      'last minute of June '
     .      //      '30 and December 31). The value supplied was #. '
 
            CALL REPMD ( ERROR,  '#', TVEC(SECOND), 8, ERROR )
            RETURN
 
         END IF
      END IF
 
C
C     One final check.  If some component is not an integer
C     the remaining components must be zero.
C
      COMP     = 0
 
      DO I = DAY, MINUTE
 
         COMP = COMP + 1
         K    = COMP
 
         IF ( TVEC(I) .NE. NINT(TVEC(I)) ) THEN
 
            DO J = I+1, SECOND
 
               K = K+1
 
               IF ( TVEC(J) .NE. 0.0D0 ) THEN
                  OK    = .FALSE.
                  ERROR = 'The ''#'' component of the date has a '
     .            //      'fractional component.  This is '
     .            //      'allowed only if all components of '
     .            //      'lesser significance have value 0.0D0. '
     .            //      'However the ''#'' component has value '
     .            //      '#. '
 
 
                  CALL REPMC ( ERROR, '#', CNAME(COMP), ERROR )
                  CALL REPMC ( ERROR, '#', CNAME(K),    ERROR )
                  CALL REPMD ( ERROR, '#', TVEC(J), 2,  ERROR )
                  RETURN
               END IF
 
            END DO
 
         END IF
 
      END DO
 
C
C     If we make it this far, all components pass the reasonableness
C     tests.
C
      OK    = .TRUE.
      ERROR = ' '
      RETURN
 
 
 
 
C$Procedure TPARCH ( Parse check---check format of strings )
 
      ENTRY TPARCH ( TYPE )
 
C$ Abstract
C
C     Restrict the set of strings that are recognized by
C     SPICE time parsing routines to those that have standard
C     values for all time components.
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
C     PARSING, TIME
C
C$ Declarations
C
C     CHARACTER*(*)         TYPE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TYPE       I   String:  Use 'YES' to restrict time inputs.
C
C$ Detailed_Input
C
C     TYPE         is a character string that is used to adjust the
C                  set of strings that will be regarded as valid
C                  time strings by SPICE time parsing routines.
C
C                  The default behavior of SPICE time software
C                  is to allow
C                  an extended range of values for the various
C                  components (tokens) of a time string.  For example,
C                  using its default behavior, TPARSE would regard
C                  1993 JAN 367 as a valid time string and return
C                  the UTCSEC value that corresponds to Jan 2, 1994.
C
C                  While this is a "reasonable" interpretation of
C                  such a string, there may be occasions when such
C                  a string should be regarded as an error.
C
C                  By calling TPARCH with a value of 'YES', the
C                  action of the time software will be modified. Strings
C                  that have components that are out of the
C                  range of values used in most English discourse
C                  will be regarded as errors.  Thus the numeric
C                  values of MONTH, DAY, HOUR, MINUTE, and SECOND
C                  must satisfy the following conditions to be
C                  regarded as legitimate calendar time strings.
C
C                  ITEM     Valid Range
C                  ----     -------------------------------------
C                  MONTH    1 to 13
C                  DAY      1 to 365 (366 for leap years) when
C                           DAY is interpreted as the day of year
C                           i.e. the month token is empty.
C                           1 to 31  if month is January
C                           1 to 28 (29 in leap years) if month is
C                                   February
C                           1 to 31  if month is March
C                           1 to 30  if month is April
C                           1 to 31  if month is May
C                           1 to 31  if month is June
C                           1 to 30  if month is July
C                           1 to 31  if month is August
C                           1 to 30  if month is September
C                           1 to 31  if month is October
C                           1 to 30  if month is November
C                           1 to 31  if month is December
C                    HOUR   0 to 23
C                    MINUTE 0 to 59
C                    SECOND 0 up to but not including 60 on days that
C                           can not have a leapsecond.
C                           0 up to but not including 61 for times
C                           that are the last second of June or
C                           December.  In other words,
C                                JUN 30, 23:59:60.xxxxxx...x
C                           and  DEC 31, 23:59:60.xxxxxx...x
C
C                    To reset the action of time software to the default
C                    action, set TYPE to a value that is not
C                    equivalent to 'YES' when case and spaces are
C                    ignored.
C
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is used to alter the collections of strings
C     that SPICE software regards as legitimate calendar strings.  The
C     default behavior of SPICE software is to accept strings such
C     as FEB 34, 1993 and to interpret these in a "natural way"
C     (FEB 34, 1993 is regarded as MARCH 6, 1993.)  This behavior
C     is sometimes useful for "private" programs that you write.
C     However, such a string may be a typo (a finger accidentally hit
C     two keys for the day instead of one).  Given that this string
C     does not appear in common usage,  you may want to consider
C     that it is more likely the result of erroneous input.  You
C     can alter the behavior of SPICE software so that it will
C     treat such a string as an error.  To do this call this entry
C     point with TYPE having the value 'YES'.
C
C        CALL TPARCH ( 'YES' )
C
C     Until the behavior is reset by calling TPARCH with a value
C     other than 'YES' (such as 'NO'),  SPICE software will treat all
C     out-of-bound components of time strings as errors.
C
C     If you are happy with the SPICE default interpretation of
C     strings, you do not need to make any calls to TPARCH.
C
C$ Examples
C
C     When accepting times as input interactively, you usually
C     READ a string typed at a keyboard and then pass that string
C     to UTC2ET to convert it to an ephemeris time.  If you want
C     to restrict the strings accepted by UTC2ET, place the
C     following call at a point early in your program.
C
C        CALL TPARCH ( 'YES' )
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
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C
C-    SPICELIB Version 1.0.1 10-FEB-2014 (BVS)
C
C        Fixed typo in the Declarations section: STRING -> TYPE.
C
C-    SPICELIB Version 1.0.0, 7-APR-1996 (WLT)
C
C        The entry point TPARCH was moved from TPARSE to the routine
C        TCHECK so that all time parsing actions could be centralized.
C
C-&
 
C$ Index_Entries
C
C     Restrict time strings to proper form
C
C-&
 
      DOCHCK = EQSTR( TYPE, 'YES' )
 
      RETURN
 
 
 
C$Procedure TCHCKD ( Time components are checked )
 
      ENTRY TCHCKD ( TYPE )
 
C$ Abstract
C
C     Determine whether component checking is enabled for time strings.
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
C
C     IMPLICIT NONE
C     CHARACTER*(*)         TYPE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TYPE       O   Answer to the question: "Is checking enabled?"
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     TYPE       is a string that gives the answer to the question
C                "Is checking of components enabled?"  If checking
C                is enabled, the value returned will be "YES" if
C                checking is not enabled, the value returned will
C                be "NO".
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point allows you to "fetch" the current settings
C     regarding the checking of components of a time string.  This
C     allows you to temporarily set the action to whatever is desired
C     in a particular piece of code and then reset the action to
C     the setting in effect prior to the routines activities.
C
C$ Examples
C
C     Suppose you'd like to write a routine that always applies
C     component checking to the components of a time string.
C
C     Use this entry point together with TPARCH and TCHECK to
C     make use of the built-in SPICE capabilities
C
C        get the current setting.
C
C        CALL TCHCKD ( CURNT )
C        CALL TPARCH ( 'YES' )
C
C           perform some time
C           parsing activities.
C
C           check the components.
C
C        CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
C
C        Set the checking activity back to the value prior
C        to the work done here.
C
C        CALL TPARCH ( CURNT )
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 7-APR-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Get the current time component checking status
C
C-&
 
      IF ( DOCHCK ) THEN
         TYPE = 'YES'
      ELSE
         TYPE = 'NO'
      END IF
 
      RETURN
      END
