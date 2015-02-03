C$Procedure      TPARTV ( Time string ---parse to a time vector)
 
      SUBROUTINE TPARTV ( STRING,
     .                    TVEC,   NTVEC, TYPE,
     .                    MODIFY, MODS,  YABBRV, SUCCES,
     .                    PICTUR, ERROR )

C$ Abstract
C
C     This routine returns the components of a time supplied
C     as a string and returns a vector of the components of
C     that string together with an array of modifiers that may
C     have been supplied with the string that may alter
C     the interpretation of the components.
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
      CHARACTER*(*)         STRING
      DOUBLE PRECISION      TVEC   ( * )
      INTEGER               NTVEC
      CHARACTER*(*)         TYPE
      CHARACTER*(*)         MODIFY ( * )
      LOGICAL               MODS
      LOGICAL               YABBRV
      LOGICAL               SUCCES
      CHARACTER*(*)         PICTUR
      CHARACTER*(*)         ERROR
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   A string to be parsed as a time
C     TVEC       O   A vector giving the components of the time.
C     NTVEC      O   The number of components supplied for TVEC
C     TYPE       O   The type of the "time vector" TVEC
C     MODIFY     O   A list of modifiers present in STRING.
C     MODS       O   A logical indicating the presence of a modifier
C     YABBRV     O   A logical indicating that a year was abbreviated
C     SUCCES     O   A logical indicating whether STRING was parsed.
C     PICTUR     O   A time format picture associated with STRING
C     ERROR      O   A diagnostic message if STRING couldn't be parsed
C
C     The function returns
C
C$ Detailed_Input
C
C     STRING     is a character string that represents some
C                julian or calendar epoch.
C
C$ Detailed_Output
C
C     TVEC       is a vector of double precision numbers that represent
C                the input string.  The number and meaning of the
C                components of TVEC depend upon the input string.  This
C                meaning can be determined from the output variable
C                TYPE.
C
C                TYPE      NTVEC     TVEC Components
C                -------------------------------------------------------
C                YMD       3 to 6    TVEC(1) is the calendar year
C                                    TVEC(2) is the numeric value of the
C                                            month (1-12)
C                                    TVEC(3) is the day of the month
C                                    TVEC(4) is the hour of the day
C                                    TVEC(5) is the minute of the hour
C                                    TVEC(6) is the second of the minute
C
C                YD        2 to 5    TVEC(1) is the calendar year
C                                    TVEC(2) is the day of the year
C                                    TVEC(3) is the hour of the day
C                                    TVEC(4) is the minute of the hour
C                                    TVEC(5) is the second of the minute
C
C                JD        1         TVEC(1) is the julian date
C
C                Note that the values of TVEC are not forced into the
C                normal ranges used in daily conversation.  TPARTV
C                simply reports what's found in the string and does
C                not pass judgement on the "correctness" of these
C                components.
C
C     NTVEC     is the actual number of components that were present
C               in the string.  For example a user might have
C               supplied only year, month and day of an epoch.
C               In such a case NTVEC will be set to 3.  The components
C               actually supplied will be 1 through NTVEC.  Values
C               not supplied are set to zero.
C
C     TYPE      is the type of time string supplied.  This is a function
C               of whether the string contains year, month and day,
C               day of year, or julian date.
C
C     MODIFY    is an array of character strings that indicate
C               whether a modifier to the calendar string was supplied.
C               If a particular modifier was not supplied, the
C               value of that component of MODIFY will be set to
C               a blank.  Modifiers are used to change the meaning
C               of time strings.
C
C               For example 12:12:29 Jan 1, 1996  means 12 hours past
C               midnight on Jan 1, 1996 in the UTC time system. But
C               if we modify the string to be:
C
C                  12:12:29 A.M. Jan 1, Tuesday PDT 1996 B.C.
C
C               the string takes on an entirely different meaning.
C
C               Five different modifiers are recognized by TPARTV:
C               the era associated with the epoch, day of week of
C               the epoch, time zone of an epoch,  AM/PM used in
C               daily time usage, and the system (UTC, TDB, or TDT).
C
C               Again whether or not modifiers are compatible with the
C               time and date components or with each other is not
C               determined by TPARTV.  TPARTV simply reports what is
C               present in the string, leaving the task of deciding
C               the meaning of the string to the calling routine.
C
C               The components of MODIFY, their meaning and possible
C               values are given below.
C
C               Component   Meaning   Possible Non-blank Modifier Values
C               ---------   -------   ----------------------------------
C               1           ERA       'A.D.', 'B.C.'
C               2           Weekday   'SUN',  'MON', ... etc.
C               3           Time Zone 'UTC+i:i', 'UTC-i:i'
C               4           AM/PM     'A.M.', 'P.M.'
C               5           System    'UTC',  'TDB', 'TDT'
C
C               TPARTV recognizes the standard abbreviations of
C               all continental U.S. time zones.
C
C                  PDT --- Pacific  Daylight Time  (UTC-07:00)
C                  PST --- Pacific  Standard Time  (UTC-08:00)
C                  MDT --- Mountain Daylight Time  (UTC-06:00)
C                  MST --- Mountain Standard Time  (UTC-07:00)
C                  CDT --- Central  Daylight Time  (UTC-05:00)
C                  CST --- Central  Standard Time  (UTC-06:00)
C                  EDT --- Eastern  Daylight Time  (UTC-04:00)
C                  EST --- Eastern  Standard Time  (UTC-05:00)
C
C               In addition it recognizes offsets from UTC expressed
C               as UTC+/-HR:MN.  Note that through out SPICELIB
C               the minutes component of the UTC offset are always
C               regarded as positive offsets from the hour offset.
C
C               All Time zones are returned in MODIFY as UTC offsets
C               as indicated in the table above.
C
C     MODS      is TRUE if some non-blank modifier was supplied.
C
C     YABBRV    is TRUE if a year was supplied in the abbreviated
C               form 'YR  where YR is a two digit integer.
C
C     SUCCES    is TRUE if the string was successfully parsed.
C               Otherwise it is set to FALSE and a diagnostic
C               is supplied in the argument ERROR.
C
C     PICTUR    is a string that gives a format picture that can
C               be used by the routine TIMOUT to construct a time
C               string of the same form as the input time string.
C
C               If some component of the input string could not be
C               identified, PICTUR is returned as a blank.  However,
C               if all components of the input string could be
C               identified and the string is simply ambiguous, PICTUR
C               will contain a format picture that corresponds to
C               the ambiguous input.  Consequently, you must check
C               the value of PICTUR to determine if TPARTV has
C               been able to construct a format picture.
C
C     ERROR     is blank if the string was successfully parsed.
C               Otherwise a human readable diagnostic is returned
C               in ERROR.
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
C     Error Free.
C
C     1) All problems are diagnosed via the variables SUCCES and
C        ERROR.
C
C$ Particulars
C
C      This routine parses in input string that represents some
C      epoch in some time system.  In addition it constructs a
C      format picture that describes the position and meaning
C      of the various components of the string.
C
C      This routine is intended to be used in close conjunction with
C      the routines TTRANS and TIMOUT.
C
C      The string is parsed by first determining its recognizable
C      substrings (integers, punctuation marks, names of months,
C      names of weekdays, time systems, time zones, etc.)  These
C      recognizable substrings are called the tokens of the input
C      string.  The meaning of some tokens are immediately determined.
C      For example named months, weekdays, time systems have clear
C      meanings.  However, the meanings of numeric components must
C      be deciphered from their magnitudes and location in
C      the string relative to the immediately recognized components
C      of the input string.
C
C      To determine the meaning of the numeric tokens in the input
C      string, a set of "productions rules" and transformations are
C      applied to the full set of tokens in the string.  These
C      transformations are repeated until the meaning of every token
C      has been determined or until further transformations yield
C      no new clues into the meaning of the numeric tokens.
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
C          219th day of 1994 + 0.12819 days.  TPARTV does not
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
C      10) If the ISO date-time separator (T) is present in the string
C          ISO allowed token patterns are examined for a match
C          with the current token list.  If no match is found the
C          search is abandoned and appropriate diagnostic messages
C          are generated.
C
C      11) If two delimiters are found in succession in the time
C          string, the time string is diagnosed as an erroneous
C          string.  ( Delimiters are comma, white space, dash, slash,
C          period, day of year mark )
C
C          Note the delimiters do not have to be the same. The pair
C          of characters ",-" counts as two successive delimiters.
C
C      12) White space, commas serve only to delimit tokens in the
C          input string.  They do not affect the meaning of any
C          of the tokens.
C
C      13) When the size of the integer components does not clearly
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
C
C      The table below gives a list of abbreviations used to
C      classify tokens.
C
C                 /   ---  slash punctuation mark
C                 H   ---  hour
C                 M   ---  Minute
C                 S   ---  Second
C                 Y   ---  year
C                 d   ---  day of year marker
C                 i   ---  unsigned integer
C                 m   ---  month
C                 n   ---  unsigned decimal number
C                 y   ---  day of year
C                 -   ---  dash punctuation mark
C                 D   ---  day of month
C                 :   ---  colon punctuation mark
C
C       Given these abbreviations the following (rather lengthy)
C       table gives the set of built in token patterns that
C       are recognized and the associated interpretation of that
C       pattern.
C
C        Pattern         Meaning         Pattern         Meaning
C        ------------------------        -------------------------
C        Y-i-it......... YmD             i/i/ii:i:n..... mDYHMS
C        Y-i-iti........ YmDH            i/i/ii:n....... mDYHM
C        Y-i-iti:i...... YmDHM           i/i/ii:n....... mDYHM
C        Y-i-iti:i:i.... YmDHMS          i:i:ii-i-Y..... HMSmDY
C        Y-i-iti:i:n.... YmDHMS          i:i:ii/i/Y..... HMSmDY
C        Y-i-iti:n...... YmDHM           i:i:ii/i/i..... HMSmDY
C        Y-i-itn........ YmDH            i:i:iimY....... HMSDmY
C        Y-i/........... Yy              i:i:imiY....... HMSmDY
C        Y-i/i:i........ YyHM            i:i:ni-i-Y..... HMSmDY
C        Y-i/i:i:i...... YyHMS           i:i:ni/i/Y..... HMSmDY
C        Y-i/i:i:n...... YyHMS           i:i:ni/i/i..... HMSmDY
C        Y-i/i:n........ YyHM            i:i:nimY....... HMSDmY
C        Y-id........... Yy              i:i:nmiY....... HMSmDY
C        Y-idi:i........ YyHM            i:ii-i-Y....... HMmDY
C        Y-idi:i:i...... YyHMS           i:ii/i/Y....... HMmDY
C        Y-idi:i:n...... YyHMS           i:ii/i/i....... HMmDY
C        Y-idi:n........ YyHM            i:iimY......... HMDmY
C        Y-it........... Yy              i:imiY......... HMmDY
C        Y-iti.......... YyH             i:ni-i-Y....... HMmDY
C        Y-iti:i........ YyHM            i:ni/i/Y....... HMmDY
C        Y-iti:i:i...... YyHMS           i:ni/i/i....... HMmDY
C        Y-iti:i:n...... YyHMS           i:nimY......... HMDmY
C        Y-iti:n........ YyHM            i:nmiY......... HMmDY
C        Y-itn.......... YyH             iYd............ yY
C        Yid............ Yy              iYdi:i......... yYHM
C        Yidi:i......... YyHM            iYdi:i:i....... yYHMS
C        Yidi:i:i....... YyHMS           iYdi:i:n....... yYHMS
C        Yidi:i:n....... YyHMS           iYdi:n......... yYHM
C        Yidi:n......... YyHM            iiY............ mDY
C        Yii............ YmD             iiYi........... mDYH
C        Yiii........... YmDH            iiYi:i......... mDYHM
C        Yiii:i......... YmDHM           iiYi:i:i....... mDYHMS
C        Yiii:i:i....... YmDHMS          iiYi:i:n....... mDYHMS
C        Yiii:i:n....... YmDHMS          iiYi:n......... mDYHM
C        Yiii:n......... YmDHM           iiYn........... mDYH
C        Yiiii.......... YmDHM           iid............ Yy
C        Yiiiii......... YmDHMS          iidi:i......... YyHM
C        Yiiiin......... YmDHMS          iidi:i:i....... YyHMS
C        Yiiin.......... YmDHM           iidi:i:n....... YyHMS
C        Yiin........... YmDH            iidi:n......... YyHM
C        Yim............ YDm             iim............ YDm
C        Yimi........... YDmH            iimi........... YDmH
C        Yimi:i......... YDmHM           iimi:i......... YDmHM
C        Yimi:i:i....... YDmHMS          iimi:i:i....... YDmHMS
C        Yimi:i:n....... YDmHMS          iimi:i:n....... YDmHMS
C        Yimi:n......... YDmHM           iimi:n......... YDmHM
C        Yimn........... YDmH            iimii.......... YDmHM
C        Yin............ YmD             iimiii......... YDmHMS
C        Ymi............ YmD             iimiin......... YDmHMS
C        Ymii........... YmDH            iimin.......... YDmHM
C        Ymii:i......... YmDHM           iimn........... YDmH
C        Ymii:i:i....... YmDHMS          imY............ DmY
C        Ymii:i:n....... YmDHMS          imYi........... DmYH
C        Ymii:n......... YmDHM           imYi:i......... DmYHM
C        Ymin........... YmDH            imYi:i:i....... DmYHMS
C        Ymn............ YmD             imYi:i:n....... DmYHMS
C        Ynm............ YDm             imYi:n......... DmYHM
C        i-Y/........... yY              imYn........... DmYH
C        i-Y/i:i........ yYHM            imi............ YmD
C        i-Y/i:i:i...... yYHMS           imi:i:iY....... DmHMSY
C        i-Y/i:i:n...... yYHMS           imi:i:nY....... DmHMSY
C        i-Y/i:n........ yYHM            imi:iY......... DmHMY
C        i-Yd........... yY              imi:nY......... DmHMY
C        i-Ydi:i........ yYHM            imii........... YmDH
C        i-Ydi:i:i...... yYHMS           imii:i......... YmDHM
C        i-Ydi:i:n...... yYHMS           imii:i:i....... YmDHMS
C        i-Ydi:n........ yYHM            imii:i:n....... YmDHMS
C        i-i-Y.......... mDY             imii:n......... YmDHM
C        i-i-Yi:i....... mDYHM           imiii.......... YmDHM
C        i-i-Yi:i:i..... mDYHMS          imiiii......... YmDHMS
C        i-i-Yi:i:n..... mDYHMS          imiiin......... YmDHMS
C        i-i-Yi:n....... mDYHM           imiin.......... YmDHM
C        i-i-it......... YmD             imin........... YmDH
C        i-i-iti........ YmDH            imn............ YmD
C        i-i-iti:i...... YmDHM           inY............ mDY
C        i-i-iti:i:i.... YmDHMS          inm............ YDm
C        i-i-iti:i:n.... YmDHMS          miY............ mDY
C        i-i-iti:n...... YmDHM           miYi........... mDYH
C        i-i-itn........ YmDH            miYi:i......... mDYHM
C        i-i/i:i........ YyHM            miYi:i:i....... mDYHMS
C        i-i/i:i:i...... YyHMS           miYi:i:n....... mDYHMS
C        i-i/i:i:n...... YyHMS           miYi:n......... mDYHM
C        i-i/i:n........ YyHM            miYn........... mDYH
C        i-idi:i........ YyHM            mii............ mDY
C        i-idi:i:i...... YyHMS           mii:i:iY....... mDHMSY
C        i-idi:i:n...... YyHMS           mii:i:nY....... mDHMSY
C        i-idi:n........ YyHM            mii:iY......... mDHMY
C        i-it........... Yy              mii:nY......... mDHMY
C        i-iti.......... YyH             miii........... mDYH
C        i-iti:i........ YyHM            miii:i......... mDYHM
C        i-iti:i:i...... YyHMS           miii:i:i....... mDYHMS
C        i-iti:i:n...... YyHMS           miii:i:n....... mDYHMS
C        i-iti:n........ YyHM            miii:n......... mDYHM
C        i-itn.......... YyH             miiii.......... mDYHM
C        i/i/Y.......... mDY             miiiii......... mDYHMS
C        i/i/Y/i:n...... mDYHM           miiiin......... mDYHMS
C        i/i/Yi:i....... mDYHM           miiin.......... mDYHM
C        i/i/Yi:i:i..... mDYHMS          miin........... mDYH
C        i/i/Yi:i:n..... mDYHMS          mnY............ mDY
C        i/i/i.......... mDY             mni............ mDY
C        i/i/ii:i....... mDYHM           nmY............ DmY
C        i/i/ii:i:i..... mDYHMS
C
C$ Examples
C
C     Suppose you need to convert various time strings to ephemeris
C     seconds past J2000.  The following pair of calls shows
C     how you would use this routine together with the routines
C     TCHECK and TTRANS to perform this task.
C
C
C         CALL TPARTV ( STRING,
C        .              TVEC,   NTVEC, TYPE,
C        .              MODIFY, MODS,  YABBRV, SUCCES,
C        .              PICTUR, ERROR )
C
C
C         IF ( .NOT. SUCCES ) THEN
C
C            Use the SPICE error handling facility to post an
C            error message and signal an error.
C
C            CALL SETMSG ( ERROR )
C            CALL SIGERR ( 'MYCHECK(BADTIME)' )
C            CALL CHKOUT ( 'MYROUTINE' )
C            RETURN
C         END IF
C
C         Check the components of TVEC to make sure everything
C         makes sense.
C
C         CALL TCHECK( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
C
C         IF ( .NOT. OK ) THEN
C
C            Use the SPICE error handling facility to post an
C            error message and signal an error.
C
C            CALL SETMSG ( ERROR )
C            CALL SIGERR ( 'MYCHECK(BADTIME)' )
C            CALL CHKOUT ( 'MYROUTINE' )
C            RETURN
C         END IF
C
C         CALL TTRANS ( TYPE, 'ET', TVEC )
C
C         ET = TVEC(1)
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
C-    SPICELIB Version 3.1.0, 15-AUG-2002 (WLT)
C
C        Replaced the call to INSSUB with ZZINSSUB so that this
C        routine can legitimately be called error free.
C
C-    SPICELIB Version 3.0.0, 10-MAY-1999 (WLT)
C
C        The routine was modified so that weekday followed by a comma
C        is recognized as a legitimate pattern when parsing.
C
C-    SPICELIB Version 2.0.0, 16-APR-1997 (WLT)
C
C        The routine was modified so that last-chance removal of
C        delimiters ',', '-', and '/' are removed one at a time
C        (instead of all at once as in version 1.0.0) and the
C        resulting representation checked against
C        the built-in list.
C
C        In addition the set of built-in patterns was increased
C        from 185 to 203.  See ZZTPATS for more details.
C
C-    SPICELIB Version 1.0.0, 10-AUG-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Parse a time string into a vector of components
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               BSRCHC
      INTEGER               ISRCHC
      INTEGER               INTMAX
      INTEGER               RTRIM
 
C
C     Private Functions
C
      LOGICAL               ZZTOKNS
      LOGICAL               ZZCMBT
      LOGICAL               ZZREMT
      LOGICAL               ZZSUBT
      LOGICAL               ZZREPT
      LOGICAL               ZZNOTE
      LOGICAL               ZZIST
      LOGICAL               ZZISPT
      LOGICAL               ZZVALT
      LOGICAL               ZZGREP
      LOGICAL               ZZTPATS
      LOGICAL               ZZUNPCK
 
C
C     Parameters
C
C
C     ERA
C     WDAY
C     ZONE
C     AMPM
C     SYSTEM
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
 
      INTEGER               NMODS
      PARAMETER           ( NMODS = SYSTEM )
 
      INTEGER               BEGS ( NMODS )
      INTEGER               ENDS ( NMODS )
 
 
      LOGICAL               HAVERA
      LOGICAL               HAVWDY
      LOGICAL               HAVZON
      LOGICAL               HAVAPM
      LOGICAL               HAVSYS
C
C     Local Variables.
C
C     The number of known time patterns NKNOWN comes from the include
C     file timepars.inc
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 12 )
 
      INTEGER               ROOM
      PARAMETER           ( ROOM = 300 )

      INTEGER               NKNOWN
      CHARACTER*(WDSIZE)    KNOWN ( ROOM )
      CHARACTER*(WDSIZE)    MEANNG( ROOM )
      CHARACTER*(WDSIZE)    REP
 
 
C
C     Time Zone Variables
C
      INTEGER               ZSIZE
      PARAMETER           ( ZSIZE = 3 )
 
      INTEGER               OSIZE
      PARAMETER           ( OSIZE = 6 )
 
      INTEGER               NZONES
      PARAMETER           ( NZONES = 8 )
 
      CHARACTER*(ZSIZE)     ZONES ( NZONES )
      CHARACTER*(OSIZE)     OFFSET( NZONES )
      CHARACTER*(1)         DELIM ( 3 )
 
 
 
      INTEGER               B
      INTEGER               B1
      INTEGER               B2
      INTEGER               E
      INTEGER               E1
      INTEGER               E2
      INTEGER               FROM
      INTEGER               I
      INTEGER               MAPTO
      INTEGER               TO
      INTEGER               USE
      INTEGER               R
 
 
 
 
      LOGICAL               RESOLV
      LOGICAL               L2R
      LOGICAL               R2L
 
      LOGICAL               FIRST
 
      SAVE
 
      DATA                  FIRST   / .TRUE. /
 
      DATA                  ZONES   / 'EST',    'EDT',
     .                                'CST',    'CDT',
     .                                'MST',    'MDT',
     .                                'PST',    'PDT'  /
 
      DATA                  OFFSET  / 'UTC-5',  'UTC-4',
     .                                'UTC-6',  'UTC-5',
     .                                'UTC-7',  'UTC-6',
     .                                'UTC-8',  'UTC-7'  /
 
C
C     Standard SPICE error handling.
C
C
C     So far there are no modifiers to the time string.
C
      MODS   = .FALSE.
      YABBRV = .FALSE.
 
      DO I = 1, NMODS
         MODIFY(I) = ' '
      END DO
 
C
C     On the first call to this routine we load the built in
C     representation patterns.
C
      IF ( FIRST ) THEN
 
         IF ( ZZTPATS( ROOM, NKNOWN, KNOWN, MEANNG ) ) THEN
            FIRST = .FALSE.
         ELSE
            PICTUR = ' '
            SUCCES = .FALSE.
            ERROR  = 'There is an incompatibility between ZZTPATS '
     .      //       'and the room allocated for KNOWN in TPARTV.'
            RETURN
         END IF
 
      END IF
 
C
C     First step is to tokenize the string.  The new representation
C     is maintained in ZZTIME.  We'll get it later if we need it.
C
      RESOLV = ZZTOKNS ( STRING, ERROR )
 
      IF ( .NOT. RESOLV ) THEN
         SUCCES = .FALSE.
         NTVEC  =  0
         TYPE   = ' '
         PICTUR = ' '
         RETURN
      END IF
 
C
C     The result of tokenizing the string will be a representation
C     that contains the following letters.
C
C           '        The quote character
C           [        The left parenthesis
C           ]        The right parenthesis
C           ,        The comma
C           -        The dash
C           .        The decimal point
C           /        The slash---used to separate date components.
C           :        The colon (used to separate time components)
C           N  ---   stands for one of the symbols A.M. or P.M.
C           O        stands for the symbol UTC+
C           Z  ---   stands for a time zone such as PDT, PSD, CDT, etc.
C           b        stands for a block of white space
C           d        stands for the day of year marker (// or ::)
C           e  ---   stands for the era (B.C. or A.D.)
C           j        stands for julian date
C           m        stands for a month
C           o        stands for the symbol UTC-
C           s  ---   stands for a time system (UTC, TDT, TDB)
C           t        stands the ISO date-T-time separator.
C           w  ---   stands for the day of the week.
C           i        stands for a sequence of digits
C
C     We will gradually remove many of these and replace the i, i.
C     and i.i with the following items
C
C           n       stands for a decimal number
C           Y       stands for a year
C           D       stands for the day in a month
C           y       stands for the day of the year
C           H       stands for hours
C           M       stands for minutes
C           S       stands for seconds.
C
C
C     We will use the following logical functions to modify
C     the tokenized representation:
C
C        ZZTOKNS --- breaks the string down into a list of recognized
C                    tokens and stores an internal model for this
C                    list.  The begins and ends of the substrings
C                    associated with the tokenization are maintained
C                    inside the routine ZZTIME (which ZZTOKNS is an
C                    entry point to).  If some substring cannot be
C                    recognized, ZZTOKNS returns the value FALSE
C                    together with a diagnostic indicating what
C                    was wrong with the input string.
C
C        ZZCMBT  --- combines one or more tokens into a single token.
C                    this is performed only once and is done either
C                    scanning left to right or right to left.
C                    It returns TRUE if a combination is performed.
C
C        ZZREMT  --- removes all instances of a token from the tokenized
C                    representation.  It returns TRUE is an item
C                    is removed.
C
C        ZZSUBT  --- substitutes the first occurrence of a
C                    subpattern (scanning left to right or right to
C                    left) with another pattern of the same length.
C                    This is where we attach new meaning to the
C                    tokenized pattern.  It returns TRUE if a
C                    substitution is performed.
C
C        ZZREPT  --- is a combination of the ZZSUBT and ZZREMT
C                    This performs ZZSUBT on the string, but then
C                    remove all occurrences of the special character
C                    * from the tokenized list. It returns TRUE
C                    is a substitution is performed.
C
C        ZZNOTE  --- returns the begin and end of the first occurrence
C                    of some token, and then removes the token
C                    from the tokenized representation.  We use this
C                    primarily to extract modifiers from the tokenized
C                    string.  These should occur only once and once
C                    removed allow us to more easily attach meaning
C                    to the remaining tokens. The value of ZZNOTE
C                    is true if the requested item could be found,
C                    otherwise it is false and the begin and end
C                    of the requested substring are set to 0.
C
C        ZZIST   --- returns TRUE if the specified token is present
C                    in the tokenized substring.
C
C        ZZISPT  --- returns true is a pair of consecutive tokens
C                    from a list are located in the representation
C                    of the tokenized string.  This is used to
C                    locate consecutive pairs of delimiters in the
C                    input string. It returns TRUE if a pair of
C                    consecutive items is located.  Otherwise
C                    it returns FALSE.
C
C        ZZVALT  --- allows you to substitute a new token for any
C                    integer (i) that lies within a specified range
C                    of values.  This is primarily used to recognize
C                    years in the input string.
C
C        ZZGREP  --- is used to get the current representation of the
C                    tokenized string (with all processing resulting
C                    from use of the manipulation routines taken into
C                    account).
C
C        ZZTPATS --- is used to set up the large list of canned patterns
C                    that are recognized as legitimate tokenizations.
C                    Almost all legitimate time strings when tokenized
C                    will match one of these patterns.
C
C        ZZUNPCK --- uses STRING together with the current
C                    representation of it's tokens to return a
C                    time vector.  If a problem is encountered with
C                    the current tokens, it returns a diagnostic
C                    message that indicates why the string
C                    could not be parsed.  Note ZZUNPCK should be
C                    called only after all string modifiers have
C                    been retrieved via a call to ZZNOTE (or by
C                    manually removing them).
C
C     Next Step is to combine some tokens so that we won't run
C     into problems later on.  We may introduce some new components
C     in the process.
C
      L2R = .TRUE.
      R2L = .NOT. L2R
 
      IF ( ZZCMBT ( 'Oi', 'z', L2R ) ) THEN
 
         RESOLV = ZZCMBT( 'z:i', 'Z', L2R )
         RESOLV = ZZSUBT( 'z',   'Z', L2R )
 
      END IF
 
      IF ( ZZCMBT ( 'oi', 'z', L2R ) ) THEN
 
         RESOLV = ZZCMBT( 'z:i', 'Z', L2R )
         RESOLV = ZZSUBT( 'z',   'Z', L2R )
 
      END IF
 
C
C     Next we resolve any months, or weekdays that are followed
C     by periods.
C
      RESOLV = ZZREPT ( 'm.', 'm*', L2R )
      RESOLV = ZZREPT ( 'w.', 'w*', L2R )
      RESOLV = ZZREPT ( 'w,', 'w*', L2R )
 
C
C     Now convert the right most integer-decimal-point pair to the
C     number representation.
C
      IF      ( ZZCMBT ( 'i.i', 'n', R2L ) ) THEN
C
C        We aren't going to do anything here.  We are simply
C        using the IF-THEN...ELSE IF ... ENDIF  to make sure
C        we only replace one decimal place.
C
      ELSE IF ( ZZCMBT ( 'i.', 'n',  R2L  ) ) THEN
C
C        Same as the previous comment.
C
      END IF
C
C     Remove any white space from the tokenization.
C
      RESOLV = ZZREMT ( 'b' )
 
C
C     User Custom Formats (this still needs a modicum of work).
C     ----------------------------------------------------------------
C     ================================================================
C
C
C     RESOLV = ZZGREP ( REP )
C     USE    = ISRCHC ( REP, NCUSTM, CUSTOM )
C
C     IF ( USE .GT. 0 ) THEN
C        RESOLV = ZZREPT ( CUSTM(USE), CMEANS(USE), L2R )
C     ELSE
C        RESOLV =  .FALSE.
C     END IF
C
C     IF ( RESOLV ) THEN
C
C        SUCCES = ZZUNPCK ( STRING, YABBRV, ...
C                           TVEC,   NTVEC, TYPE, PICTUR, ERROR )
C        ERROR  = ' '
C
C        RETURN
C     END IF
C
C
 
 
 
C
C     Julian Date
C     ----------------------------------------------------------------
C     ================================================================
C
      IF ( ZZIST ( 'j' ) ) THEN
C
C        This is some form of Julian Date. Handle this case
C        right here and return.
C
         RESOLV = ZZREPT ( '[s]', '*s*', L2R )
         MODS   = MODS .OR. ZZNOTE ( 's', B, E )
 
         IF ( MODS ) THEN
            CALL UCASE ( STRING(B:E), MODIFY(SYSTEM) )
         END IF
 
         RESOLV = ZZREPT ( '[j]', '*j*', L2R )
         RESOLV = ZZREMT ( 'j' )
 
         IF ( .NOT. ZZIST ( 'n' ) ) THEN
            RESOLV = ZZSUBT( 'i', 'n', L2R )
         END IF
 
         RESOLV = ZZCMBT ( '-n',  'n', L2R )
         RESOLV = ZZSUBT ( 'n',   'J', L2R )
 
 
C
C        We let ZZUNPK handle the parsing or diagnosis of any problems.
C
         SUCCES = ZZUNPCK ( STRING, YABBRV,
     .                      TVEC,   NTVEC, TYPE, PICTUR, ERROR )
 
         IF ( INDEX( PICTUR, 'JULIAND.' ) .GT. 0 ) THEN
            CALL SUFFIX ( '::RND', 1, PICTUR )
         END IF
 
         IF ( MODIFY(SYSTEM) .NE. ' ' ) THEN
            CALL SUFFIX ( '::',           1, PICTUR  )
            CALL SUFFIX ( MODIFY(SYSTEM), 0, PICTUR  )
         END IF
 
         RETURN
 
      END IF
 
 
 
 
C
C     Calendar Date Formats.
C     ----------------------------------------------------------------
C     ================================================================
C
C     Replace any integers greater than 1000 by Y.
C
      B      = 1000
      E      = INTMAX()
      RESOLV = ZZVALT ( STRING, B, E, 'Y' )
 
C
C     If the ISO time delimiter 't' is present we don't perform
C     any further simplifications.
C
      IF ( ZZIST ( 't' ) ) THEN
 
         RESOLV = ZZGREP ( REP )
         USE    = BSRCHC ( REP, NKNOWN, KNOWN )
 
         IF ( USE .NE. 0 ) THEN
 
            RESOLV = ZZREPT  ( KNOWN(USE), MEANNG(USE), L2R )
            SUCCES = ZZUNPCK ( STRING, YABBRV,
     .                         TVEC,   NTVEC, TYPE, PICTUR, ERROR)
 
            IF ( INDEX ( PICTUR, '.#' ) .NE.0 ) THEN
               CALL SUFFIX ( '::RND', 1, PICTUR )
            END IF
 
            IF ( MODIFY(ZONE) .NE. ' ' ) THEN
               CALL SUFFIX ( '::',         1, PICTUR )
               CALL SUFFIX ( MODIFY(ZONE), 0, PICTUR )
            END IF
 
            IF ( MODIFY(SYSTEM) .NE. ' ' ) THEN
               CALL SUFFIX ( '::',           1, PICTUR )
               CALL SUFFIX ( MODIFY(SYSTEM), 0, PICTUR )
            END IF
 
         ELSE
 
            SUCCES = .FALSE.
            NTVEC  =  0
            MODS   = .FALSE.
            TYPE   = ' '
            PICTUR = ' '
 
            ERROR  = 'The input string uses the ISO  "T" '
     .      //       'date/time delimiter but does not match any '
     .      //       'of the accepted ISO formats. '
 
         END IF
 
         RETURN
 
      END IF
 
C
C     If we reach this point, either we didn't have any custom
C     formats supplied or we didn't match any of them.
C     Resolve any abbreviated years.  We've already set integers
C     that are 1000 or greater to 'Y'  Only 1 or 2 digit integers
C     can be year abbreviations.  We replace the 3 digit integers
C     with I temporarily; locate any abbreviated years; reset all
C     the 3-digit back to 'i'.  (Note 3-digit means value between
C     100 and 999.  003 is not regarded as a 3 digit number).
C
      B      = 100
      E      = 1000
      RESOLV = ZZVALT ( STRING, B, E, 'I' )
      YABBRV = ZZREPT ( '''i', '*Y',  L2R )
 
      DO WHILE ( ZZSUBT ( 'I', 'i', L2R ) )
         B = B+1
      END DO
 
 
 
C
C     Resolve the system, and other text components.
C
      RESOLV = ZZREPT ( '[e]', '*e*', L2R )
      RESOLV = ZZREPT ( '[w]', '*w*', L2R )
      RESOLV = ZZREPT ( '[N]', '*N*', L2R )
      RESOLV = ZZREPT ( '[Z]', '*Z*', L2R )
      RESOLV = ZZREPT ( '[s]', '*s*', L2R )
      RESOLV = ZZSUBT ( 'ie',  'Ye',  L2R )
C
C     Note the positions of ERA, WEEKDAY, TIME-ZONE, AMPM marker
C     and time SYSTEM.
C
 
      HAVERA = ZZNOTE ( 'e', BEGS(ERA),    ENDS(ERA)    )
      HAVWDY = ZZNOTE ( 'w', BEGS(WDAY),   ENDS(WDAY)   )
      HAVZON = ZZNOTE ( 'Z', BEGS(ZONE),   ENDS(ZONE)   )
      HAVAPM = ZZNOTE ( 'N', BEGS(AMPM),   ENDS(AMPM)   )
      HAVSYS = ZZNOTE ( 's', BEGS(SYSTEM), ENDS(SYSTEM) )
 
      MODS   = HAVERA .OR. HAVWDY .OR. HAVZON .OR. HAVAPM .OR. HAVSYS
 
      IF ( MODS ) THEN
 
         DO I = 1, NMODS
            IF ( BEGS(I) .NE. 0 ) THEN
               CALL UCASE( STRING(BEGS(I):ENDS(I)),MODIFY(I))
            END IF
         END DO
 
         IF ( HAVERA ) THEN
            IF ( MODIFY(ERA)(1:1) .EQ. 'A' ) THEN
               MODIFY(ERA) = 'A.D.'
            ELSE
               MODIFY(ERA) = 'B.C.'
            END IF
         END IF
 
         IF ( HAVAPM ) THEN
            IF ( MODIFY(AMPM)(1:1) .EQ. 'A' ) THEN
               MODIFY(AMPM) = 'A.M.'
            ELSE
               MODIFY(AMPM) = 'P.M.'
            END IF
         END IF
 
         MODIFY(WDAY)(4:) = ' '
 
         IF ( HAVZON ) THEN
 
            MAPTO = ISRCHC ( MODIFY(ZONE), NZONES, ZONES )
 
            IF ( MAPTO .NE. 0 ) THEN
               MODIFY(ZONE) = OFFSET(MAPTO)
            END IF
 
         END IF
 
      END IF
 
C
C     Try our built in formats without any further substitution.
C
      RESOLV = ZZGREP ( REP )
      USE    = BSRCHC ( REP, NKNOWN, KNOWN )
 
      IF ( USE .GT. 0 ) THEN
 
         RESOLV = ZZREPT  ( KNOWN(USE),   MEANNG(USE), L2R   )
         SUCCES = ZZUNPCK ( STRING, YABBRV,
     .                      TVEC,   NTVEC, TYPE, PICTUR, ERROR )
 
         IF ( INDEX ( PICTUR, '.#' ) .NE.0 ) THEN
            CALL SUFFIX ( '::RND', 1, PICTUR )
         END IF
 
         IF ( MODIFY(ZONE) .NE. ' ' ) THEN
            CALL SUFFIX ( '::',         1, PICTUR )
            CALL SUFFIX ( MODIFY(ZONE), 0, PICTUR )
         END IF
 
         IF ( MODIFY(SYSTEM) .NE. ' ' ) THEN
            CALL SUFFIX ( '::',           1, PICTUR )
            CALL SUFFIX ( MODIFY(SYSTEM), 0, PICTUR )
         END IF
 
         RETURN
 
      END IF
C
C     Make sure we don't have a pair of successive delimiters
C     or a delimiter at either end of the input string.
C
      IF (  ZZISPT ( ',/-:d.', FROM, TO ) ) THEN
 
         SUCCES = .FALSE.
         NTVEC  =  0
         TYPE   = ' '
 
 
         ERROR = STRING
         CALL ZZINSSUB ( ERROR, '>', TO+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', FROM, ERROR )
         CALL PREFIX ( 'There are two successive delimiters <#> '
     .   //            'in the input string.  This is an '
     .   //            'ambiguous input. '' ',
     .                  0, ERROR)
         CALL REPMC  ( ERROR, '#', STRING(FROM:TO), ERROR )
         CALL SUFFIX ( '''', 0, ERROR )
         PICTUR = ' '
         RETURN
 
      END IF
 
C
C     A delimiter hanging at either end of the string shall be
C     regarded as an error.
C
      RESOLV = ZZGREP ( REP )
      R      = RTRIM  ( REP )
 
      IF ( INDEX( ',/-:.', REP(1:1) ) .GT. 0 ) THEN
 
         RESOLV = ZZSUBT ( REP(1:1), 'Q', L2R )
         RESOLV = .FALSE.
 
      ELSE IF ( INDEX( ',/-:.', REP(R:R) ) .GT. 0 ) THEN
 
         RESOLV = ZZSUBT ( REP(R:R), 'Q', L2R )
         RESOLV = .FALSE.
 
      END IF
 
      IF ( .NOT. RESOLV ) THEN
 
         RESOLV = ZZNOTE ( 'Q', FROM, TO )
         ERROR  = STRING
         CALL ZZINSSUB ( ERROR, '>', TO+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', FROM, ERROR )
         CALL PREFIX ( 'An unexpected delimiter (''#'') was '
     .   //            'encountered in the input string. '' ',
     .                       0, ERROR )
         CALL SUFFIX ( '''', 0, ERROR )
         CALL REPMC  ( ERROR, '#', STRING(FROM:TO), ERROR )
         PICTUR = ' '
         SUCCES = .FALSE.
         RETURN
      END IF
C
C     We probably made it unscathed through the check above.
C     Remove delimiters ',', '/', and '-' and retry the built-in
C     patterns.
C
      DELIM(1) = ','
      DELIM(2) = '-'
      DELIM(3) = '/'
 
      DO I = 1, 3
 
         RESOLV = ZZREMT ( DELIM(I) )
 
         RESOLV = ZZGREP ( REP )
         USE    = BSRCHC ( REP, NKNOWN, KNOWN )
 
         IF ( USE .GT. 0 ) THEN
 
            RESOLV = ZZREPT  ( KNOWN(USE),   MEANNG(USE), L2R   )
            SUCCES = ZZUNPCK ( STRING, YABBRV,
     .                         TVEC,   NTVEC, TYPE, PICTUR, ERROR )
 
            IF ( INDEX ( PICTUR, '.#' ) .NE.0 ) THEN
               CALL SUFFIX ( '::RND', 1, PICTUR )
            END IF
 
            IF ( MODIFY(ZONE) .NE. ' ' ) THEN
               CALL SUFFIX ( '::',         1, PICTUR )
               CALL SUFFIX ( MODIFY(ZONE), 0, PICTUR )
            END IF
 
            IF ( MODIFY(SYSTEM) .NE. ' ' ) THEN
               CALL SUFFIX ( '::',           1, PICTUR )
               CALL SUFFIX ( MODIFY(SYSTEM), 0, PICTUR )
            END IF
 
            RETURN
 
         END IF
 
      END DO
 
C
C     If we make it to this point, we must have a pretty funky
C     time string.  There are some obvious incompatibilities. We
C     check them now
C
 
      IF      ( ZZNOTE ( 'e', B, E ) ) THEN
      ELSE IF ( ZZNOTE ( 's', B, E ) ) THEN
      ELSE IF ( ZZNOTE ( 'Z', B, E ) ) THEN
      ELSE IF ( ZZNOTE ( 'w', B, E ) ) THEN
      ELSE IF ( ZZNOTE ( 'N', B, E ) ) THEN
      END IF
C
C     If B is non-zero the item in question is a duplicate
C     modifier.
C
      IF ( B .GT. 0 ) THEN
         SUCCES = .FALSE.
         NTVEC  =  0
         TYPE   = ' '
         ERROR  =  STRING
 
         CALL ZZINSSUB ( ERROR, '>', E+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', B,   ERROR )
         CALL PREFIX ( 'The substring "#" is a duplicate '
     .   //            'modifier of the input string: '' ',
     .                         0,               ERROR )
         CALL SUFFIX ( '''',   0,               ERROR )
         CALL REPMC  ( ERROR, '#', STRING(B:E), ERROR )
         PICTUR = ' '
         RETURN
      END IF
C
C     Look for unresolved markers
C
      IF      ( ZZNOTE ( '[', B, E ) ) THEN
      ELSE IF ( ZZNOTE ( ']', B, E ) ) THEN
      ELSE IF ( ZZNOTE ( 'O', B, E ) ) THEN
      ELSE IF ( ZZNOTE ( 'o', B, E ) ) THEN
      ELSE IF ( ZZNOTE ( 'z', B, E ) ) THEN
      END IF
 
      IF ( B .GT. 0 ) THEN
 
         SUCCES = .FALSE.
         NTVEC  =  0
         TYPE   = ' '
         ERROR  =  STRING
 
         CALL ZZINSSUB ( ERROR, '>', E+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', B,   ERROR )
         CALL PREFIX ( 'The substring "#" could not '
     .   //            'be resolved in the input string: '' ',
     .                         0,               ERROR )
         CALL SUFFIX ( '''',   0,               ERROR )
         CALL REPMC  ( ERROR, '#', STRING(B:E), ERROR )
         PICTUR = ' '
         RETURN
 
      END IF
 
      IF ( ZZIST ( 'm' ) .AND. ZZIST ( 'd' ) ) THEN
 
         SUCCES = .FALSE.
         NTVEC  =  0
         TYPE   = ' '
         ERROR  = STRING
         RESOLV = ZZNOTE ( 'm', B1, E1 )
         RESOLV = ZZNOTE ( 'd', B2, E2 )
 
         B     = MAX(B1, B2)
         E     = MAX(E1, E2)
 
         CALL ZZINSSUB ( ERROR, '>', E+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', B, ERROR )
 
         B     = MIN(B1, B2)
         E     = MIN(E1, E2)
 
         CALL ZZINSSUB ( ERROR, '>', E+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', B, ERROR )
 
         CALL PREFIX ( 'Both a month "#" and day of year '
     .   //            'delimiter "#" appear in the input '
     .   //            'string: '' ', 0, ERROR )
         CALL SUFFIX ( '''',          0, ERROR )
         CALL REPMC  ( ERROR, '#', STRING(B1:E1), ERROR )
         CALL REPMC  ( ERROR, '#', STRING(B2:E2), ERROR )
         PICTUR = ' '
         RETURN
 
      END IF
 
 
 
C
C     Make the remaining obvious substitutions for hours,
C     minutes, and seconds
C
      IF      ( ZZREPT ( 'i:i:i:n', 'D*H*M*S', R2L ) ) THEN
      ELSE IF ( ZZREPT ( 'i:i:i:i', 'D*H*M*S', R2L ) ) THEN
      ELSE IF ( ZZREPT ( 'i:i:n',   'H*M*S',   R2L ) ) THEN
      ELSE IF ( ZZREPT ( 'i:i:i',   'H*M*S',   R2L ) ) THEN
      ELSE IF ( ZZREPT ( 'i:n',     'H*M',     R2L ) ) THEN
      ELSE IF ( ZZREPT ( 'i:i',     'H*M',     R2L ) ) THEN
      END IF
 
      RESOLV = ZZREMT ( ':' )
C
C     Handle the obvious substitutions of an integer next to
C     a Month.
C
      IF      ( ZZSUBT ( '<miiH', 'mDY', L2R ) ) THEN
      ELSE IF ( ZZSUBT ( '<mi',   'mD',  L2R ) ) THEN
      ELSE IF ( ZZSUBT ( 'Siim>', 'SYDm',L2R ) ) THEN
      ELSE IF ( ZZSUBT ( 'im>',   'Dm',  L2R ) ) THEN
      ELSE IF ( ZZSUBT ( 'miY>',  'mDY', L2R ) ) THEN
      ELSE IF ( ZZSUBT ( 'Ymi',   'YmD', L2R ) ) THEN
      ELSE IF ( ZZSUBT ( 'Smi',   'SmD', L2R ) ) THEN
      ELSE IF ( ZZSUBT ( 'Mmi',   'MmD', L2R ) ) THEN
      ELSE IF ( ZZSUBT ( 'imY',   'DmY', L2R ) ) THEN
      ELSE IF ( ZZSUBT ( 'imH',   'DmH', L2R ) ) THEN
      ELSE IF ( ZZREPT ( 'Yid',   'Yy*', L2R ) ) THEN
      ELSE IF ( ZZREPT ( 'iYd',   'yY*', L2R ) ) THEN
      ELSE IF ( ZZREPT ( 'Ydi',   'Y*y', L2R ) ) THEN
      END IF
 
C
C     That's it we let ZZUNPCK handle the problem of diagnosing
C     or decoding the current representation.
C
      SUCCES = ZZUNPCK ( STRING, YABBRV,
     .                   TVEC,   NTVEC, TYPE, PICTUR, ERROR )
 
      IF ( PICTUR .NE. ' ' ) THEN
 
         IF ( INDEX ( PICTUR, '.#' ) .NE.0 ) THEN
            CALL SUFFIX ( '::RND', 1, PICTUR )
         END IF
 
         IF ( MODIFY(ZONE) .NE. ' ' ) THEN
            CALL SUFFIX ( '::',         1, PICTUR )
            CALL SUFFIX ( MODIFY(ZONE), 0, PICTUR )
         END IF
 
         IF ( MODIFY(SYSTEM) .NE. ' ' ) THEN
            CALL SUFFIX ( '::',           1, PICTUR )
            CALL SUFFIX ( MODIFY(SYSTEM), 0, PICTUR )
         END IF
 
      END IF
 
      RETURN
 
 
      END
