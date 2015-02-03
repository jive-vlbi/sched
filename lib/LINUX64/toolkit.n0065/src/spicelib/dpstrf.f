C$Procedure      DPSTRF ( Double Precision Number to Character )
 
      SUBROUTINE DPSTRF ( X, SIGDIG, FORMAT, STRING )
 
C$ Abstract
C
C     Take a double precision number and convert it to an
C     equivalent formatted character string representation (base 10).
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
C      CHARACTER
C      CONVERSION
C      PARSING
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      X
      INTEGER               SIGDIG
      CHARACTER*1           FORMAT
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      X          I   A double precision number
C      SIGDIG     I   The number of significant digits saved for output
C      FORMAT     I   'E' for scientific, 'F' for floating point.
C      STRING     O   A character string representation of X
C
C$ Detailed_Input
C
C      X          is a double precision number.
C
C      SIGDIG     is the number of significant digits that are desired
C                 for the output string.
C
C      FORMAT     is a character flag that indicates how the double
C                 precision number should be represented.  The two
C                 acceptable inputs are 'E' and 'F'.  If the input
C                 is 'E' then the number will be displayed with an
C                 exponent in scientific notation. It will have the
C                 form 'sx.xxx - - - xxxxxEsyy' where there are 
C                 SIGDIG x's and s is ' ' or '-' at its first occurrence
C                 and '-' or '+' in the second.
C
C                 If the input is 'F' then the number will be
C                 displayed without an exponent --- the representation
C                 will be strictly decimal.  The first symbol will be
C                 a sign ('-' or ' ').
C
C$ Detailed_Output
C
C
C      STRING     is a character representation of X to the number of
C                 significant digits specified by SIGDIG.  The number of
C                 spaces required to return the requested character
C                 string is SIGDIG + 6.  If STRING is not declared to
C                 have adequate length, the number returned will be
C                 truncated on the right.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      This routine computes an approximate character representation
C      of the input string X. The maximum number of significant
C      digits returned is 14 (in F format there may be many extra
C      zeros returned but only a maximum of 14 digits will be
C      significant.
C
C      The output string is left justified.
C
C      This routine has the advantage that it does not use an internal
C      file and is about twice as fast as an internal write. It can
C      be used as part of character function without fear of introducing
C      recursive I/O conflicts. It is intended to be an approximate
C      inverse to the subroutine NPARSD.
C
C      IF you want the character string representation of a double
C      precision number to be the same as that produced by a formatted
C      write statement use a FORTRAN write statement.
C
C      For example the number represented by the string
C
C            1.245454545454545454545E+01
C
C      when read (via a FORTRAN READ statement) into the DP variable X
C      and converted back to a character string having 14 significant
C      digits by this routine yields
C
C            1.2454545454545E+01  in E format
C            12.454545454545      in F format
C
C      The FORTRAN write statement
C
C            WRITE ( 6, FMT='(P1E)' ) X
C
C      yields
C
C            1.2454545454545454E+01
C
C      If this is too much error for your application DO NOT use this
C      routine.  You should be aware however, that a character string
C      read into a double precision number may not WRITE out with an
C      equivalent character representation as was input.
C
C      For example on a VAX 11/780 if you
C
C            READ  (5,*)         X
C            WRITE (6,FMT='(E)') X
C
C      and enter a value of 7.00000001 for the read statement
C      the output written will be 0.7000000010000001E+01
C
C
C$ Examples
C
C      Suppose that you wished to insert the character representation
C      of some DOUBLE PRECISION number into a line of text.
C
C      For example suppose X contains the double precision number
C      4.268176872928187 and you would like to insert the character
C      representation of this number to 2 places between the strings
C
C      'There are', 'meters between lamp posts'
C
C      You could perform the following sequence of steps
C
C
C            DOUBLE PRECISION  X
C            CHARACTER*5       DISTANCE
C            CHARACTER*80      MESSAGE
C
C            CALL DPSTRF ( X, 2, 'F', DISTANCE )
C
C            MESSAGE = 'There are '                //
C           .           DISTANCE                   //
C           .          'meters between lamp posts'
C           .
C
C      C
C      C     Squeeze any extra spaces out of the message string.
C      C
C            CALL CMPRSS ( ' ', 1, MESSAGE, MESSAGE )
C
C
C
C      The string MESSAGE would contain:
C
C           'There are 4.2 meters between lamp posts'
C
C$ Restrictions
C
C      The maximum number of significant digits returned is 14.
C
C      If the output string is not declared to be adequately large
C      the numeric string will be truncated to the side opposite its
C      justification (At least SIGDIG + 6 characters are needed in E
C      format, in F format the size required is dependent upon the
C      input X and the number of significant digits requested.
C      In extreme cases up to 56 characters may be required.)
C
C      This routine makes explicit use of the format of the string
C      returned by DPSTR,  should that routine change, substantial
C      work may be required to bring this routine back up to snuff.
C
C$ Exceptions
C
C      Error free.
C
C      If SIGDIG is less than one, this routine returns one significant
C      digit in the output string.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.2.0, 17-SEP-1996 (WLT)
C
C         Upgraded routine to handle arbitrary magnitude d.p. numbers.
C
C-     SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.1.0, 30-JUL-1990 (WLT)
C
C         The routine was repaired so that references to zero-length
C         strings ( for example STRING(4:3) ) are not made.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     d.p. number to character with formatting
C
C-&
 
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 30-JUL-1990 (WLT)
C
C        As previously implemented, one would occasionally reference
C        a zero length substring of the variable NUMSTR.  This was
C        O.K. under VAX Fortran, because it allows such references.
C        However, most implementations of Fortran are not as forgiving.
C
C-&
 
 
C
C     Local variables
C
      DOUBLE PRECISION      Y
 
      INTEGER               EXP
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               LASTCH
      INTEGER               MAXDIG
      INTEGER               I
      INTEGER               J
 
 
      LOGICAL               OVFLOW
 
 
 
 
      MAXDIG = MIN ( 14, MAX(1,SIGDIG)       )
 
C
C     If the format is 'E' we just let DPSTR handle the problem.
C
      IF ( FORMAT .EQ. 'E' ) THEN
 
         CALL DPSTR  ( X,  MAXDIG, STRING )
         RETURN
 
      END IF
 
C
C     If we're still here, we have a decimal format requested.  Set
C     the sign for the number.
C
      IF ( X .LT. 0.0D0 ) THEN
         STRING = '-'
      ELSE
         STRING = ' '
      END IF
C
C     If X is zero, we can handle this without any regard to the
C     exponent.
C
      IF ( X .EQ. 0.0D0 ) THEN
 
         CALL ZZVSTSTR (  X, ' ', EXP )
         CALL ZZVSBSTR ( -1, MAXDIG, .FALSE., STRING(2:), OVFLOW )
         RETURN
 
      END IF
C
C     We've already set the sign, now we deal with the unsigned
C     portion of X.
C
      Y = DABS(X)
 
C
C     Create a virtual decimal string for Y.
C
      CALL ZZVSTSTR ( Y, ' ', EXP )
 
C
C     Now we can just fill in the string by reading the appropriate
C     substring from the virtual decimal string.  We need to compute
C     the first and last virtual digits to retrieve.  To do this
C     we look at EXP.
C
      IF ( EXP .GE. 0 ) THEN
         FIRST  = -EXP-1
      ELSE
         FIRST  = -EXP
      END IF
 
      LAST   =  FIRST + MAXDIG - 1
 
      IF ( FIRST .LT. 0 .AND. LAST .GE. 0 ) THEN
         LAST = LAST + 1
      END IF
 
      FIRST = MIN ( -1, FIRST )
 
      CALL ZZVSBSTR ( FIRST, LAST, .TRUE., STRING(2:), OVFLOW )
 
      IF ( OVFLOW ) THEN
 
         FIRST = FIRST - 1
 
         CALL ZZVSBSTR ( FIRST, LAST, .TRUE., STRING(2:), OVFLOW )
C
C        We need to blank out the last digit of string.
C
         LASTCH = LAST - FIRST + 2
 
         IF (       LAST   .GT. 0
     .        .AND. LASTCH .LE. LEN(STRING) ) THEN
            STRING(LASTCH:) = ' '
         END IF
 
      END IF
 
 
      IF ( LAST .LT. 0 ) THEN
 
         J  = LAST - FIRST + 3
 
         DO I = LAST+1, -1
 
            IF ( J .LE. LEN(STRING) ) THEN
               STRING(J:J) = '0'
            END IF
            J = J + 1
 
         END DO
 
         IF ( J .LE. LEN(STRING) ) THEN
            STRING(J:J) = '.'
         END IF
 
      END IF
 
 
 
 
      RETURN
 
      END
