C$Procedure      M2CAL ( Parse a UTC time string )
 
      SUBROUTINE M2CAL ( UTCSTR, MSSG, TCODE )
      IMPLICIT NONE
 
C$ Abstract
C
C      See is a string is a legitimate time string.
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
C$ Keywords
C
C      PARSING, TIME
C
C$ Declarations
 
      CHARACTER*(*)         UTCSTR
      CHARACTER*(*)         MSSG
      INTEGER               TCODE
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      UTCSTR     I   Input time string, UTC.
C      MSSG       O   A diagnostic indicating why parsing failed.
C      TCODE      O   A short  parsing error flag.
C
C$ Detailed_Input
C
C      UTCSTR      is an input time string, containing a Calendar or
C                  Julian Date, UTC.
C
C                  Calendar dates consist of up to seven tokens:
C                  one each for System, Year, Month, Day, Hours,
C                  Minutes, and Seconds.
C
C                  Valid token delimiters are:
C
C                        ' '           space
C                        ','           comma
C                        '/'           slash
C                        '-'           dash
C                        ':'           colon
C
C                  The month may be an integer or a name. (At least
C                  three characters are required in a name.) The last
C                  three tokens always represent Hours, Minutes, and
C                  Seconds respectively. The first three tokens always
C                  represent Year, Month, and Day, with the order
C                  determined according to the following rules:
C
C                     1. If a month name is present, then the year is
C                        taken to be an integer greater than 1000 and
C                        less than 3000.  The day of the month is taken
C                        to be the non-negative integer less than 32.
C
C                     2. If no month name is present, the token greater
C                        than 1000 and less than 3000 is taken to be
C                        the year this must be the first token or the
C                        third.  In either case the other two tokens
C                        in order are then taken to be the month and
C                        day of month.
C
C                  Missing tokens are assigned the following defaults:
C
C                      - Month                    January
C                      - Day                      1
C                      - Hours                    0
C                      - Minutes                  0
C                      - Seconds                  0.0000000
C
C                  Note that Day of Year may be substituted for Month
C                  and Day in either of the following ways:
C
C                     1. By setting the month to January and the day to
C                        Day of Year, e.g.,
C
C                           '1986 JAN 247 12:00:01.184'
C
C                     2. By eliminating the month token altogether.
C                        (It defaults to January anyway.) The most
C                        popular form for DOY entry is:
C
C                           '1986//247 12:00:01.184'
C
C                  Julian Dates consist of two tokens.
C                  The first contains the letters 'JD', in any
C                  combinations of upper- or lower-case. The
C                  second token is a Julian Date. For convenience,
C                  the two tokens may be concatenated, as shown
C                  in the examples below. Valid token delimiters
C                  are the same as for Calendar format.
C
C                  If the token 'JD' is entered by itself, the
C                  input string is rejected as ambiguous.
C
C                  The length of UTC should not exceed 80 characters.
C
C$ Detailed_Output
C
C      MSSG        is a descriptive message indicating what went wrong
C                  if the string could not be parsed. It is blank when
C                  the string parses successfully as a time.
C
C      TCODE       is a short string that indicates why the date did not
C                  parse.
C$ Input_Files
C
C      None.
C
C$ Output_Files
C
C      None.
C
C$ Common_Variables
C
C      None.
C
C$ Detailed_Description
C
C      The input string is parsed for six tokens, delimited by any
C      of the valid delimiters (space, comma, slash, hyphen, colon).
C
C      If the first token is (or begins with) 'JD', the input is
C      taken to be a Julian Date. Extra tokens are ignored.
C
C      Otherwise, the last three tokens are assigned to hours,
C      minutes, and seconds respectively. The first three are
C      assigned to year, month, and day, according to magnitude and
C      the presence (or lack) of a month name, according to the rules
C      described under Detailed_Inputs above. The Muller-Wimberly
C      formula is used to compute the number of days past 2000 JAN 1,
C      which is then converted to UTC seconds past J2000.
C
C$ Examples
C
C      The following are examples of valid inputs to M2CAL:
C
C         '29 February 1975 3:00'       (  1 MAR 1975 03:00:00       )
C         'JD 2451545.'                 (  1 JAN 2000 12:00:00       )
C         'JD-2451545.'                 (  1 JAN 2000 12:00:00       )
C         'jd 2451545.'                 (  1 JAN 2000 12:00:00       )
C         'JD2451545.'                  (  1 JAN 2000 12:00:00       )
C
C      The following examples would be rejected as ambiguous.
C
C          '32 jan 32'
C          '85 86 january'
C          '86 3  january'
C          'January 80 81'
C          'JD,,,2451545'
C
C$ Restrictions
C
C      None.
C
C$ Required_Reading
C
C      TIME
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W. M. Owen, Jr. (JPL)
C      I. M. Underwood (JPL)
C      W. L. Taber     (JPL)
C
C$ Version
C
C-     META/2 Version 3.0.0, 3-SEP-1998 (WLT)
C
C         Replaced everything with foundation Time routine calls.
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C      Version 1, 22-APR-1987
C
C-&
 
 
 
 
C
C     NAIFLIB functions
C
      LOGICAL               RETURN
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(16)        MODIFY ( 5 )
      CHARACTER*(8)         TYPE
      CHARACTER*(LNSIZE)    PICTUR
 
      DOUBLE PRECISION      TVEC ( 8 )
 
      INTEGER               NTVEC
 
      LOGICAL               MODS
      LOGICAL               SUCCES
      LOGICAL               YABBRV
 
 
 
      IF ( RETURN() ) THEN
 
         RETURN
 
      END IF
 
      MSSG  = ' '
      TCODE = 0
 
 
 
      CALL TPARTV ( UTCSTR,
     .              TVEC,   NTVEC, TYPE,
     .              MODIFY, MODS,  YABBRV, SUCCES,
     .              PICTUR, MSSG )
 
      IF ( .NOT. SUCCES ) THEN
 
         TCODE = 1
 
      ELSE IF ( TYPE .EQ. 'JD' ) THEN
C
C        Don't do anything.
C
 
      ELSE
 
         CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, SUCCES, MSSG )
 
         IF ( .NOT. SUCCES ) THEN
            TCODE = 2
         END IF
 
      END IF
 
      RETURN
      END
