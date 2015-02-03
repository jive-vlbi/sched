C$Procedure      DPSTR ( Double Precision Number to Character )
 
      SUBROUTINE DPSTR ( X, SIGDIG, STRING )
 
C$ Abstract
C
C     Take a double precision number and convert it to
C     an equivalent character string representation (base 10).
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
 
      DOUBLE PRECISION X
      INTEGER          SIGDIG
      CHARACTER*(*)    STRING
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      X          I   A double precision number
C      SIGDIG     I   The number of significant digits placed in output
C      STRING     O   A character string representation of X
C
C$ Detailed_Input
C
C      X          is a double precision number.
C
C      SIGDIG     is the number of significant digits that are desired
C                 for the output string.
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
C      digits returned is 14. The representation returned will be
C      the same as that given by the FORTRAN write statement
C
C          WRITE ( STRING, FMT=(P1E23.xx)
C
C      where xx is a two digit number that represents MIN(14,SIGDIG).
C      The last decimal place is rounded. The output string is left
C      justified.
C
C      This routine has the advantage that it does not use an internal
C      file and is about 2.3 times as fast as an internal write. It can
C      be used as part of character function without fear of introducing
C      recursive I/O conflicts. It is intended to be an approximate
C      inverse to the subroutine NPARSD.
C
C      There is of course no formatting of the output string.  All
C      outputs are written in scientific notation.
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
C            1.2454545454545E+01
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
C      This routine is intended for use by routines that manipulate
C      character strings.  For example, it may be desirable for a
C      routine to be able to take a character string input such as
C
C            12 miles
C
C      and convert it to the string
C
C            1.932E+02 km
C
C      or to simply
C
C            1.932E+02
C
C      The arithmetic is of course most easily handled using numeric
C      variables.  However, it may be that a string is required for
C      subsequent processing of the input.  A SPICELIB routine NPARSD
C      exists that will take a character representation of a number
C      and convert it to a DOUBLE PRECISION number.  The 12 above
C      can be converted to double precision using NPARSD,  the d.p.
C      number can then be multiplied by the 1.61... and the result
C      converted back to a string using this routine.
C
C      Suppose the following declarations are made
C
C            CHARACTER*(80)     TEXT
C            CHARACTER*(80)     NUMBER
C            CHARACTER*(80)     SCRATCH
C
C            DOUBLE PRECISION   X
C            INTEGER            I
C
C      and that TEXT contains the string '12 mi'.  Then the following
C      code would produce a character string  '1.932E+01 KM'
C
C            CALL NEXTWD (  TEXT,   NUMBER, SCRATCH   )
C            CALL NPARSD (  NUMBER, X,      ERROR,  I )
C
C            IF ( ERROR .EQ. ' ' ) THEN
C
C               X    = X * 1.61D0
C               CALL DPSTR ( X, 5, NUMBER )
C               TEXT = NUMBER(1:10) // 'KM'
C
C            ELSE
C               .
C               .
C               create an error message, try again, etc.
C               .
C               .
C            END IF
C
C
C$ Restrictions
C
C      Note:  The format of the string returned by this routine is
C      used in DPSTRF which is in the call tree to DPFMT.  Changes
C      to the format of the output string may have unexpected
C      consequences for these SPICE routines.  Please check those
C      routines before modifying this routine.
C
C      The maximum number of significant digits returned is 14.
C
C      If the output string is not declared to be adequately large
C      (at least SIGDIG + 6), the numeric string will be truncated
C      to the side opposite its justification.
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
C      N.J. Bachman    (JPL)
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.1.1, 09-SEP-1996 (WLT)
C
C         Added a reference to the header concerning the dependency
C         of the SPICE routines DPSTRF and DPFMT on the format of
C         the string produced by this routine.
C
C-     SPICELIB Version 1.1.0, 11-JUN-1992 (WLT)
C
C         A bug that caused this routine to have a floating point
C         overflow for values of X close to zero was corrected.  In
C         addition the restriction on range of exponents supported
C         has been removed.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     d.p. number to character
C
C-&
 
 
 
C$ Revisions
C
C-     SPICELIB Version 1.1.0, 14-OCT-1992 (WLT)
C
C         A bug that caused this routine to have a floating point
C         overflow for values of X close to zero was corrected.  In
C         addition the restriction on range of exponents supported
C         has been removed.
C
C-     Beta Version 1.1.0, 16-FEB-1989 (HAN) (NJB)
C
C         Header was changed to reflect the "error free" status
C         of the module, and a comment was added stating what the
C         routine does if SIGIDG is less than one.
C
C         Declaration of the unused variable FRAC removed.
C
C-&
 
 
 
 
C
C     Maximum number of allowed significant digits.
C
      INTEGER               MOSTDG
      PARAMETER           ( MOSTDG = 14 )
 
C
C     Local variables
C
      INTEGER               MAXPOW
      PARAMETER           ( MAXPOW = 17 )
 
      DOUBLE PRECISION      POWER (0:MAXPOW)
      DOUBLE PRECISION      IPOWER(0:MAXPOW)
 
      CHARACTER*(1)         DIGITS(0:9)
      DOUBLE PRECISION      VALUES(0:9)
 
      INTEGER               MAXEXP
      PARAMETER           ( MAXEXP = 40 )
 
      CHARACTER*(2)         VAXEXP(0:40)
 
 
 
      DOUBLE PRECISION      COPY
      INTEGER               POSTN
      INTEGER               EXPONT
      INTEGER               MAXSIG
 
      DOUBLE PRECISION      EXP10
 
      INTEGER               I
      INTEGER               K
      INTEGER               LAST
 
      CHARACTER*(20)        EXPC
      CHARACTER*(28)        ZERO
 
 
      CHARACTER*(32)        NUMSTR
 
      SAVE                  POWER
      SAVE                  IPOWER
      SAVE                  DIGITS
      SAVE                  VALUES
      SAVE                  VAXEXP
 
      DATA             POWER        /  1.0D0,
     .                                 1.0D1,  1.0D2,  1.0D3,  1.0D4,
     .                                 1.0D5,  1.0D6,  1.0D7,  1.0D8,
     .                                 1.0D9,  1.0D10, 1.0D11, 1.0D12,
     .                                 1.0D13, 1.0D14, 1.0D15, 1.0D16,
     .                                 1.0D17                          /
 
      DATA             IPOWER       /1.0D0,
     .                               1.0D-1,  1.0D-2,  1.0D-3,  1.0D-4,
     .                               1.0D-5,  1.0D-6,  1.0D-7,  1.0D-8,
     .                               1.0D-9,  1.0D-10, 1.0D-11, 1.0D-12,
     .                               1.0D-13, 1.0D-14, 1.0D-15, 1.0D-16,
     .                               1.0D-17                          /
 
      DATA             DIGITS       /  '0', '1', '2', '3', '4',
     .                                 '5', '6', '7', '8', '9'    /
 
      DATA             VALUES       /   0.0D0 ,  1.0D0 ,  2.0D0 ,
     .                                  3.0D0 ,  4.0D0 ,  5.0D0 ,
     .                                  6.0D0 ,  7.0D0 ,  8.0D0 ,
     .                                  9.0D0                     /
 
      DATA             VAXEXP       / '00', '01', '02', '03', '04',
     .                                '05', '06', '07', '08', '09',
     .                                '10', '11', '12', '13', '14',
     .                                '15', '16', '17', '18', '19',
     .                                '20', '21', '22', '23', '24',
     .                                '25', '26', '27', '28', '29',
     .                                '30', '31', '32', '33', '34',
     .                                '35', '36', '37', '38', '39',
     .                                '40'                          /
 
C
C     Transfer X to the local variable COPY and leave X alone for the
C     rest of the routine.
C
      COPY   = X
 
C
C     Wipe out anything sitting in NUMSTR
C
      NUMSTR = ' '
 
C
C     At least 1 significant digit is required.  The most allowed is 14.
C     MAXSIG is the integer in this range that is closest to SIGDIG.
C
      MAXSIG = MIN ( MOSTDG, MAX( 1, SIGDIG ) )
 
C
C     Examine COPY to see if its positive, zero, or negative.
C     This determines whether we need a minus sign and where the
C     decimal point needs to go in the output string.
C
      IF      ( COPY .LT. 0 ) THEN
 
         NUMSTR(1:1) = '-'
         COPY        =  - COPY
         POSTN       =  2
         NUMSTR(3:3) = '.'
 
      ELSE IF ( COPY .GT. 0 ) THEN
 
         NUMSTR(1:1) = ' '
         POSTN       =  2
         NUMSTR(3:3) = '.'
 
      ELSE
         ZERO        = ' 0.0000000000000000000000000'
         NUMSTR      = ZERO(1:MAXSIG+2) // 'E+00'
 
         STRING = NUMSTR
         RETURN
 
      END IF
 
 
C
C     We need a first guess at the exponent string.  Compute the LOG
C     base 10 of COPY
C
      EXP10  = DLOG10(COPY)
 
 
C
C     Scale our copy of the input into the range 1 to 10.
C
      IF ( EXP10 .LT. 0 ) THEN
 
C
C        In this case the exponent will be negative.  We want the
C        largest integer exponent less than EXP10,  but the FORTRAN
C        INT function gives the INTEGER closest to EXP10 between EXP10
C        and zero.  As a result we have to subtract 1 from INT(EXP10).
C
         EXPONT =  INT(EXP10) - 1
         K      = -EXPONT
 
         DO WHILE ( K .GT. 16 )
            COPY = COPY * 1.0D16
            K    = K - 16
         END DO
 
         IF ( K .NE. 0 ) THEN
            COPY = COPY * POWER(K)
         END IF
 
 
      ELSE
 
         EXPONT = INT ( EXP10 )
         K      = EXPONT
 
         DO WHILE ( K .GT. 16 )
            COPY = COPY * 1.0D-16
            K    = K - 16
         END DO
 
         IF ( K .NE. 0 ) THEN
            COPY = COPY * IPOWER(K)
         END IF
 
 
      END IF
 
 
 
C
C     Round off the last significant digit.
C
      COPY   = (DNINT( COPY*POWER(MAXSIG-1)) + 0.125D0 )
     .       * IPOWER(MAXSIG-1)
 
C
C     We might have accidently made copy as big as 10 by the
C     round off process.  If we did we need to divide by 10 and add 1
C     to the exponent value.  (COPY must always remain between 0 and 10)
C
      IF ( COPY .GE. 10.0D0 ) THEN
         COPY   = COPY * 1.0D-1
         EXPONT = EXPONT + 1
      END IF
C
C     Get the first digit of the decimal expansion of X.
C
      I                   = INT ( COPY )
      NUMSTR(POSTN:POSTN) = DIGITS(I)
 
      COPY                = ( COPY - VALUES(I) ) * 10.0D0
 
C
C     Set the string pointer to the next position and compute the
C     position of the last significant digit
C
      POSTN               = POSTN + 2
      LAST                = POSTN + MAXSIG - 1
 
C
C     Fetch digits until we fill in the last available slot for
C     significant digits.
C
      DO WHILE ( POSTN  .LT. LAST )
 
         I                   = INT ( COPY )
         NUMSTR(POSTN:POSTN) = DIGITS(I)
         COPY                = ( COPY - VALUES(I) ) * 10.0D0
         POSTN               = POSTN + 1
 
      END DO
 
C
C     Tack on the exponent to the output. Note that the rather odd
C     if, else if, else construction below is done to maintain backward
C     compatibility of the "look" of the output.
C
C     First get the exponent symbol and sign of the exponent.
C
      IF   ( EXPONT .GE. 0  ) THEN
 
         NUMSTR(POSTN:) = 'E+'
 
      ELSE
 
         EXPONT         = - EXPONT
         NUMSTR(POSTN:) = 'E-'
 
      END IF
 
      POSTN = POSTN + 2
 
C
C     Now get the numeric representation.
C
      IF ( EXPONT .LE. 40 ) THEN
         EXPC  = VAXEXP ( EXPONT )
      ELSE
         CALL INTSTR ( EXPONT, EXPC )
      END IF
 
      NUMSTR(POSTN:) = EXPC
      STRING         = NUMSTR
 
C
C     That's all folks.
C
      RETURN
 
      END
