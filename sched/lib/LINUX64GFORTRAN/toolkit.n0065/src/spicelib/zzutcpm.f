C$Procedure      ZZUTCPM ( UTC Plus or Minus Parse )
 
      SUBROUTINE ZZUTCPM ( STRING, START, HOFF, MOFF, LAST, SUCCES )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Parse a substring of the form ::UTC[+/-]1-12:0-59
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
C      Time --- PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         STRING
      INTEGER               START
      DOUBLE PRECISION      HOFF
      DOUBLE PRECISION      MOFF
      INTEGER               LAST
      LOGICAL               SUCCES
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   is a string containing a substring ::UTC+HR:MN
C     START      I   is the location in the string to start parsing
C     HOFF       O   is the d.p. value associated with HR.
C     MOFF       O   is the d.p. value associated with MN
C     LAST       O   is the end of the time zone substring.
C     SUCCES     O   indicates that a time zone was parsed.
C
C$ Detailed_Input
C
C     STRING     is a string that has an embedded substring of the
C                form ::UTC+HR[:MN] ( or ::UTC-HR[:MN] starting at
C                character start.
C
C     START      is the location in STRING where a time zone
C                specification is believed to begin.
C
C$ Detailed_Output
C
C     HOFF       is the double precision value associated with
C                HR in the picture above.  This value will be
C                between -12 and 12 inclusive.
C
C     MOFF       is the double precision value associated with MN
C                in the picture above.  This value will be between
C                0 and 59 inclusive (or -59 and 0 inclusive) depending
C                on the sign present in the UTC+/- substring.  The
C                sign of MOFF is the same as the sign present in the
C                string. 
C
C     LAST       is the last character of the time zone specification.
C                If the string doesn't have a correct format and
C                range of values, LAST is returns as START - 1.
C
C     SUCCES     is a logical which if true, indicates that a time
C                zone was successfully parsed.
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
C     1) There are no exceptions.  Either the string matches
C        the template or it doesn't.  No case is regarded
C        as an error.
C
C$ Particulars
C
C     This is a private routine for parsing time zones specified
C     as UTC+/-HR:MN  where HR is an unsigned integer between 0 and
C     11.  HR must have no more than 2 digits.  MN is expected
C     to be an unsigned integer between 0 and 59 inclusive.  It must
C     have no more than 2 digits.
C
C$ Examples
C
C     See TIMOUT.
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
C-    SPICELIB Version 1.0.0, 27-SEP-1996 (WLT)
C
C
C-&
C
C     Spicelib functions
C
      LOGICAL               SAMCH
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    ERROR
 
      INTEGER               NEED
      INTEGER               LENGTH
      INTEGER               SIGNAT
      INTEGER               UNSAT
      INTEGER               UNSTO
      INTEGER               NCHAR
      INTEGER               PTR
 
      DOUBLE PRECISION      SIGN
      DOUBLE PRECISION      X
 
C
C     This is a special purpose routine.  The input string must have
C     exactly the right format to be a time zone substring.  If anything
C     goes wrong, we just bail out and leave HOFF and MOFF right at
C     zero.
C
      HOFF   =  0.0D0
      MOFF   =  0.0D0
      LAST   =  START - 1
      SUCCES = .FALSE.
 
C
C     Note that NEED   = START + LEN('::UTC+x') - 1
C               SIGNAT = START + LEN('::UTC+' ) - 1
C
      LENGTH = LEN(STRING)
      NEED   = START + 6
      SIGNAT = START + 5
      UNSAT  = NEED
 
      IF ( LENGTH .LT. NEED ) THEN
         RETURN
      END IF
 
      IF       ( ICHAR(STRING(SIGNAT:SIGNAT)) .EQ. ICHAR('+') ) THEN
         SIGN =  1.0D0
      ELSE IF  ( ICHAR(STRING(SIGNAT:SIGNAT)) .EQ. ICHAR('-') ) THEN
         SIGN = -1.0D0
      ELSE
         RETURN
      END IF
C
C     So far everything looks fine, "lex" the string starting at
C     SIGNAT + 1 for an unsigned integer.
C
      CALL LX4UNS ( STRING, UNSAT, UNSTO, NCHAR )
 
      IF ( NCHAR .GT. 0 .AND. NCHAR .LT. 3 ) THEN
         CALL NPARSD ( STRING(UNSAT:UNSTO), X, ERROR, PTR )
 
         IF ( X .GE. 13.0D0 ) THEN
            RETURN
         END IF
 
         LAST = UNSTO
         HOFF = SIGN * X
 
      ELSE
         RETURN
      END IF
C
C     If we're still in the game at this point, we have at least
C     an hour offset, see if there is a minutes portion to the
C     time zone.
C
      SUCCES = .TRUE.
 
      IF ( SAMCH( STRING, UNSTO+1, ':', 1 ) ) THEN
         UNSAT = UNSTO + 2
      ELSE
         RETURN
      END IF
 
      CALL LX4UNS ( STRING, UNSAT, UNSTO, NCHAR )
 
      IF ( NCHAR .GT. 0 .AND. NCHAR .LT. 3 ) THEN
         CALL NPARSD ( STRING(UNSAT:UNSTO), X, ERROR, PTR )
 
         IF ( X .GT. 59.0D0 ) THEN
            RETURN
         END IF
 
         LAST = UNSTO
         MOFF = SIGN*X
 
      END IF
 
      RETURN
      END
