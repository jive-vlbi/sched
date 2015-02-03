C$Procedure ISO2UTC ( Convert ISO time strings to UTC strings. )
 
      SUBROUTINE ISO2UTC ( TSTRNG, UTCSTR, ERROR )
 
C$ Abstract
C
C     This routine converts date-time strings represented in
C     the format adopted by the International Standards Organization
C     (ISO) to equivalent UTC time strings recognized by the SPICELIB
C     routine TPARSE.
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
C     TIME
C
C$ Declarations
 
      CHARACTER*(*)         TSTRNG
      CHARACTER*(*)         UTCSTR
      CHARACTER*(*)         ERROR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TSTRNG     I   String representing a calendar or julian date epoch
C     UTCSTR     O   SPICELIB UTC string corresponding to TSTRNG
C     ERROR      O   Error message if something went wrong.
C
C$ Detailed_Input
C
C     TSTRNG       is an input time string, containing a time string
C                  in ISO format. This routine is not sensitive to
C                  the case of the characters that make up TSTRNG.
C                  Thus 1992-192t12:29:28 and 1992-192T12:29:28
C                  are equivalent.
C
C                  The ISO standard time formats are:
C
C                     Year Month Day    yyyy-mm-ddThh:mm:ss[.sss...]
C                                       yyyy-mm-dd
C
C                     Day of Year       yyyy-dddThh:mm:ss[.sss...]
C                                       yyyy-ddd
C
C                  The letters y,m,d,h,m,s can stand for any digit.
C                  All digits are required in these formats.  Moreover
C                  the year portion of these strings must be between
C                  1000 and 2999 inclusive.
C
C                  The length of TSTRNG should not exceed 80 characters.
C
C                  We point out that the format yyyy-ddd may be
C                  interpreted very differently by routine UTC2ET.
C                  1992-003 is interpreted by UTC2ET as March 1, 1992
C                  whereas it is interpret as January 3, 1992 by ISO2ET.
C
C                  User's should be aware of these differences in
C                  interpretation and exercise adequate care in their
C                  programs to avoid this possible confusion.
C
C$ Detailed_Output
C
C     UTCSTR       is the equivalent of TSTRNG, expressed in a UTC
C                  time string that can be parsed by the SPICELIB
C                  routine TPARSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the string is interpreted as an ISO format string and
C        the year portion is not within the range [1000, 2999] the
C        error SPICE(YEAROUTOFBOUNDS) is signalled.  UTCSTR is
C        not changed.
C
C     2) If the string does not clearly match the ISO format
C        the error SPICE(NOTISOFORMAT) is signalled. UTCSTR is not
C        changed.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C      The input string is converted to a UTC time string as defined
C      by the SPICELIB routine TPARSE.
C
C$ Examples
C
C     To convert the time string 1992-04-03T14:12:28 to the
C     corresponding ephemeris time, execute the following instructions:
C
C        TSTRNG = '1992-04-03T14:12:28'
C
C        CALL ISO2UTC ( TSTRNG, UTCSTR, ERROR )
C
C        CALL TPARSE ( UTCSTR, UTCSEC, ERROR )
C
C        CALL DELTET ( UTCSEC, 'UTC', DELTA )
C
C        ET = DELTA + UTCSEC
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      Jesperson and Fitz-Randolph, From Sundials to Atomic Clocks,
C      Dover Publications, New York, 1977.
C
C      Software Interface Specification: SFOC-2-SYS-Any-TimeForms
C      prepared by D. Wagner, Revision Date: Feb 6, 1990.
C      Document Identifier SFOC0038-01-09-03  (NAIF Document 268.00)
C
C$ Author_and_Institution
C
C     J.M. Lynch     (JPL)
C     B.V. Semenov   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.2, 28-FEB-2008 (BVS) 
C
C        Corrected the contents of the Required_Reading section.
C
C-    SPICELIB Version 1.1.1, 19-SEP-2006 (EDW)
C
C        Added text to previously empty Restrictions section.
C
C-    EKLIB Version 1.1.0, 11-JUL-1995 (KRG)
C
C        Fixed a typo in the $ Detailed_Output section of the header. 
C        The output variable was listed as ET when it should have been 
C        UTCSTR.
C
C        Changed the length of ASCII to be 100 rather than 128. This 
C        removes possible wcompiler warning messages for truncating 
C        character variables on assignments. The maximum nonblank length
C        for an input time ISO string is 80 characters, so placing it 
C        into a temporary array of 100 characters should pose no 
C        difficulties.
C
C-    EKLIB Version 1.0.0, 25-FEB-1993 (JML)
C
C-&
 
C$ Index_Entries
C
C
C     Transform ISO time strings to UTC strings
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHC
      INTEGER               RTRIM
 
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               NMNTHS
      PARAMETER           ( NMNTHS = 12 )
 
C
C     In-line functions.
C
      LOGICAL               DIGIT
      LOGICAL               COLON
      LOGICAL               HYPHEN
      LOGICAL               T
 
C
C     Local Variables
C
      CHARACTER*(100)       ASCII
      CHARACTER*(128)       MYSTR
 
      CHARACTER*(3)         MONTHS ( 0 : NMNTHS )
      SAVE                  MONTHS
 
      CHARACTER*(2)         IMONTH (     NMNTHS )
      SAVE                  IMONTH
 
      INTEGER               I
      INTEGER               L
      INTEGER               M
 
      LOGICAL               CHANGE
C
C     Initial Values
C
 
      DATA                  MONTHS  / '???', 'JAN', 'FEB', 'MAR',
     .                                       'APR', 'MAY', 'JUN',
     .                                       'JUL', 'AUG', 'SEP',
     .                                       'OCT', 'NOV', 'DEC' /
 
      DATA                  IMONTH  /        '01',  '02',  '03',
     .                                       '04',  '05',  '06',
     .                                       '07',  '08',  '09',
     .                                       '10',  '11',  '12'  /
 
 
 
C
C     In-line Function Definitions
C
      DIGIT(I)  =      ICHAR(ASCII(I:I)) .GE. ICHAR('0')
     .           .AND. ICHAR(ASCII(I:I)) .LE. ICHAR('9')
 
      HYPHEN(I) = ASCII(I:I) .EQ. '-'
      COLON(I)  = ASCII(I:I) .EQ. ':'
      T(I)      = ASCII(I:I) .EQ. 'T' .OR. ASCII(I:I) .EQ. 't'
 
 
C
C     Standard SPICELIB exception handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ISO2UTC' )
      END IF
 
C
C     Left justify the input time string, and determine the location of
C     it's last non-blank character.  Finally make some local copies.
C
      CALL      LJUST ( TSTRNG, ASCII )
      L      =  RTRIM (         ASCII )
      MYSTR  =  ASCII
      CHANGE = .FALSE.
 
C
C     Next check for one of the ISO allowed formats.
C
      IF      ( L .EQ. 8 ) THEN
 
C
C        The possible format is: yyyy-ddd.  If we get a
C        match construct the corresponding SPICE day of
C        year format using JAN  (e.g. 1991-JAN-261).
C
         IF (       DIGIT  (1)
     .        .AND. DIGIT  (2)
     .        .AND. DIGIT  (3)
     .        .AND. DIGIT  (4)
     .        .AND. HYPHEN (5)
     .        .AND. DIGIT  (6)
     .        .AND. DIGIT  (7)
     .        .AND. DIGIT  (8) ) THEN
 
            MYSTR  =  ASCII(1:5) // 'JAN' // ASCII(5:)
            CHANGE = .TRUE.
 
         END IF
 
      ELSE IF ( L .EQ. 10 ) THEN
 
C
C        The possible format is: yyyy-mm-dd. If we get a match
C        construct the corresponding SPICE yyyy-mm-dd format.
C
         IF (       DIGIT  (1)
     .        .AND. DIGIT  (2)
     .        .AND. DIGIT  (3)
     .        .AND. DIGIT  (4)
     .        .AND. HYPHEN (5)
     .        .AND. DIGIT  (6)
     .        .AND. DIGIT  (7)
     .        .AND. HYPHEN (8)
     .        .AND. DIGIT  (9)
     .        .AND. DIGIT  (10)  ) THEN
 
            M      =  BSRCHC ( ASCII(6:7), NMNTHS, IMONTH )
            MYSTR  =  ASCII(1:5) // MONTHS(M) // ASCII(8:)
            CHANGE = .TRUE.
 
         END IF
 
      ELSE IF ( L .GE. 17 ) THEN
 
C
C        There are two possible formats yyyy-dddThh:mm:ss.ssssss
C                                       yyyy-mm-ddThh:mm:ss.ssssss
C        As above, if we get a match up to the first character following
C        a 'T', convert this to a standard SPICE time string.
C
         IF (       DIGIT  (1)  .AND. DIGIT  (2)
     .        .AND. DIGIT  (3)  .AND. DIGIT  (4)
     .        .AND. HYPHEN (5)
     .        .AND. DIGIT  (6)  .AND. DIGIT  (7)  .AND. DIGIT  (8)
     .        .AND. T      (9)
     .        .AND. DIGIT  (10) .AND. DIGIT  (11)
     .        .AND. COLON  (12)
     .        .AND. DIGIT  (13) .AND. DIGIT  (14)
     .        .AND. COLON  (15)
     .        .AND. DIGIT  (16) .AND. DIGIT  (17) ) THEN
 
 
            MYSTR  =  ASCII(1:5) //
     .               'JAN'       //
     .                ASCII(5:8) //
     .               ' '         //
     .                ASCII(10:)
 
            CHANGE = .TRUE.
 
         ELSE IF (       DIGIT  (1)  .AND. DIGIT  (2)
     .             .AND. DIGIT  (3)  .AND. DIGIT  (4)
     .             .AND. HYPHEN (5)
     .             .AND. DIGIT  (6)  .AND. DIGIT  (7)
     .             .AND. HYPHEN (8)
     .             .AND. DIGIT  (9)  .AND. DIGIT  (10)
     .             .AND. T      (11)
     .             .AND. DIGIT  (12) .AND. DIGIT  (13)
     .             .AND. COLON  (14)
     .             .AND. DIGIT  (15) .AND. DIGIT  (16)
     .             .AND. COLON  (17)
     .             .AND. DIGIT  (18) .AND. DIGIT  (19) ) THEN
 
            M      =  BSRCHC ( ASCII(6:7), NMNTHS, IMONTH )
            MYSTR  =  ASCII(1:5)  //
     .                MONTHS(M)   //
     .                ASCII(8:10) //
     .                ' '         //
     .                ASCII(12:)
 
            CHANGE = .TRUE.
         END IF
 
      END IF
 
 
C
C     If we didn't make some change to the input string, it's NOT
C     an ISO format string. Say so in an error message and return.
C
      IF ( .NOT. CHANGE ) THEN
 
         ERROR = 'The input string does not match the format '  //
     .           'expected of ISO time strings. The acceptable '//
     .           'formats are: yyyy-ddd, yyyy-mm-dd, '          //
     .           'yyyy-dddThh:mm:ss[.ss...], and '              //
     .           'yyyy-mm-ddThh:mm:ss[.ss...].  The input '     //
     .           'string was #. '
 
         CALL REPMC  ( ERROR, '#', MYSTR(1:L), ERROR )
         CALL CHKOUT ( 'ISO2UTC' )
         RETURN
 
      END IF
 
C
C     Check for a year out of the range from 1000 to 2999
C
      IF ( CHANGE
     .     .AND. (      LLT ( ASCII(1:4), '1000' )
     .             .OR. LGT ( ASCII(1:4), '2999' ) ) ) THEN
 
         ERROR = 'Years outside the range from 1000 '           //
     .           'to 2999 are not supported in SPICE-ISO '      //
     .           'format. You''ve '                             //
     .           'supplied a time string of the form '          //
     .           '# ... '
         CALL REPMC ( ERROR, '#', ASCII(1:7), ERROR )
         CALL CHKOUT ( 'ISO2UTC' )
         RETURN
 
      END IF
 
 
C
C     That's it.
C
      ERROR = ' '
 
      UTCSTR = MYSTR
 
      CALL CHKOUT ( 'ISO2UTC'  )
 
      RETURN
 
      END
