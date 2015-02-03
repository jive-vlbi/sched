C$Procedure      NPARSI ( Integer parsing of a character string)
 
      SUBROUTINE NPARSI (  STRING,  N, ERROR, PNTER)
 
C$ Abstract
C
C     Parse a character string that represents a number and return
C     the FORTRAN-truncated integer value.
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
C      ALPHANUMERIC
C      CONVERSION
C      PARSING
C
C$ Declarations
 
      CHARACTER*(*)    STRING
      INTEGER          N
      CHARACTER*(*)    ERROR
      INTEGER          PNTER
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     STRING     I   Character string representing a numeric value.
C     N          O   Translated integer value of STRING.
C     ERROR      O   Message indicating what errors have occurred.
C     PNTER      O   Position in character string where an error
C                    occurred.
C
C$ Detailed_Input
C
C     STRING     A character string that represents a numeric value.
C                Commas and spaces may be used in this string for
C                ease of reading and writing the number.  They
C                are treated as insignificant but non-error-producing
C                characters.
C
C                For exponential representation and of the characters
C                'E','D','e','d' may be used.
C
C                The following are legitimate numeric expressions
C
C                 +12.2 e-1
C                 -3. 1415 9276
C                 1e12
C                 E10
C
C                The program also recognizes the following  mnemonics
C                'PI', 'pi', 'Pi', 'pI'
C                '+PI', '+pi', '+Pi', '+pI'
C                '-PI', '-pi', '-Pi', '-pI'
C                and returns the value ( + OR - ) 3 as appropriate.
C
C$ Detailed_Output
C
C     N          Integer parsed value of input string  ( with
C                the implied limits on precision).  If an error is
C                encountered, N is not changed from whatever the
C                input value was.  If the input string has a fractional
C                part, the fractional part will be truncated.  Thus
C                3.18 is interpreted as 3.  -4.98 is interpreted as -4.
C
C     ERROR      This is a message indicating that the string could
C                not be parsed due to ambiguous use of symbols or
C                due to a string representing a number too large for
C                VAX double precision or integer variables. If no
C                error occurred, ERROR is blank.
C
C                In particular, blank strings, or strings that do not
C                contain either a digit or exponent character will
C                be regarded as errors.
C
C     PNTER      This indicates which character was being used when
C                the error occurred. If no error occurred, PNTER is 0.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C     Basically, all this routine does is pass the input string to
C     NPARSD which does the parsing in double precision.  If nothing
C     goes wrong in the double precision parsing of the number, the
C     returned value is checked to determine whether or not it will fit
C     into a VAX integer.  If it doesn't, an error message is returned.
C
C$ Examples
C
C     Let   LINE = 'DELTA_T_A       =   32'
C
C     The following code fragment parses the line and obtains the
C     integer value.
C
C
C        CALL NEXTWD ( LINE,  FIRST,  REST )
C        CALL NEXTWD ( REST, SECOND,  REST )
C        CALL NEXTWD ( REST,  THIRD,  REST )
C
C        CALL NPARSI (  THIRD,  VALUE, ERROR, POINTR )
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the string is non-numeric, PNTER indicates the location in
C        the string where the error occurred, and ERROR contains a
C        descriptive error message.
C
C     2) If the string is blank, ERROR is returned with a message
C        indicating the problem and PNTER will have a non-zero value.
C
C     3) If the string represents a number that is outside the range
C        of representable integers, as defined by INTMIN() and INTMAX(),
C        ERROR is returned with a message and PNTER is set to the value
C        1, as the entire numeric string is at fault.
C
C$ Files
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 29-APR-1996 (KRG)
C
C        This subroutine was modified to return a non-zero value of
C        PNTER when the value returned by NPARSD is not a representable 
C        integer, as defined by INTMIN() and INTMAX(). The value 
C        returned is one (1), since the entire input string was not 
C        correct.
C
C        The test for an error from NPARSD was also changed. It now
C        uses the integer PNTER returned from NPARSD rather then the
C        character string ERROR. This should pose no problems because
C        PNTER is non-zero if and only if there was an error and an 
C        error message was assigned to ERROR.
C
C        Some extra, and unnecessary, assignments were deleted. The 
C        assignments were:
C
C           X = DBLE ( N )
C
C           ERROR = ' '
C
C        which converted the input argument into a double before
C        calling NPARSD with X and initialized the error message
C        to be blank. NPARSD sets the value for X, ERROR, and PNTER
C        unless an error occurs, in which case X is not changed.
C        So, it is not necessary to initialize ERROR, PNTER, or X.
C
C        Finally, the values of INTMIN and INTMAX are only set on the 
C        first call to the routine. They are now SAVEd.
C
C-    SPICELIB Version 2.0.0, 15-OCT-1992 (WLT)
C
C        The abstract of this routine was modified to reflect what
C        the routine actually does---truncate the value to an
C        integer.
C
C        In addition, a blank string is no longer considered to be
C        valid input.
C
C        Finally the instances of DFLOAT in the previous version were
C        replaced by the standard intrinsic function DBLE and the
C        function DINT was replaced by IDINT in one place to make types
C        match up on both sides of an assignment.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     parse a character_string to an integer
C
C-&
 
C
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 29-APR-1996 (KRG)
C
C        This subroutine was modified to return a non-zero value of
C        PNTER when the value returned by NPARSD is not a representable 
C        integer, as defined by INTMIN() and INTMAX(). The value 
C        returned is one (1), since the entire input string was not 
C        correct.
C
C        The test for an error from NPARSD was also changed. It now
C        uses the integer PNTER returned from NPARSD rather then the
C        character string ERROR. This should pose no problems because
C        PNTER is non-zero if and only if there was an error and an 
C        error message was assigned to ERROR.
C
C        Some extra, and unnecessary, assignments were deleted. The 
C        assignments were:
C
C           X = DBLE ( N )
C
C           ERROR = ' '
C
C        which converted the input argument into a double before
C        calling NPARSD with X and initialized the error message
C        to be blank. NPARSD sets the value for X, ERROR, and PNTER
C        unless an error occurs, in which case X is not changed.
C        So, it is not necessary to initialize ERROR, PNTER, or X.
C
C        Finally, the values of INTMIN and INTMAX are only set on the 
C        first call to the routine. They are now SAVEd.
C
C-    SPICELIB Version 2.0.0, 15-OCT-1992 (WLT)
C
C        The abstract of this routine was modified to reflect what
C        the routine actually does---truncate the value to an
C        integer.
C
C        In addition, a blank string is no longer considered to be
C        valid input.
C
C        Finally the instances of DFLOAT in the previous version were
C        replaced by the standard intrinsic function DBLE and the
C        function DINT was replaced by IDINT in one place to make types
C        match up on both sides of an assignment.
C
C-     Beta Version 1.2.0, 23-FEB-1989 (WLT)
C
C         Due to a programming error, the routine was not leaving N
C         unchanged if the input string was blank.  This bug was
C         fixed and the exceptional case noted in exceptions.
C
C-     Beta Version 1.1.0, 28-OCT-1988 (HAN)
C
C         Peter Wolff (JPL) informed the NAIF staff that he found
C         an "IMPLICIT NONE" statement in the ANSI Standard Fortran
C         77 version of this routine. Because the statement is a
C         VAX extension not used by NAIF, the statement was removed.
C
C-&
 
C
C     SPICELIB functions
C
 
      INTEGER               INTMAX
      INTEGER               INTMIN
 
C
C     Local Variables
C
      DOUBLE PRECISION      XMNINT
      DOUBLE PRECISION      XMXINT
      DOUBLE PRECISION      X

      LOGICAL               FIRST
C
C     Saved Variables
C
      SAVE                  FIRST
      SAVE                  XMNINT
      SAVE                  XMXINT
C
C     Initial values
C
      DATA                  FIRST / .TRUE. /
C
C     If this is the first time NPARSI has been called, initialize
C     bounds for the range of integers.
C
      IF ( FIRST ) THEN

         FIRST  = .FALSE.
         XMXINT =  DBLE ( INTMAX () )
         XMNINT =  DBLE ( INTMIN () )
 
      END IF
C
C     NPARSD will define ERROR and PNTER if there is an error,
C     so we do not need to initialize them here.
C
      CALL NPARSD ( STRING, X, ERROR, PNTER )

      IF ( PNTER .EQ. 0 ) THEN
 
         IF (     ( DINT(X) .LT. XMNINT )
     .       .OR. ( DINT(X) .GT. XMXINT ) ) THEN
 
            PNTER = 1
            ERROR = 'NPARSI: Value entered is beyond the bounds of '//
     .              'representable integers.'
 
         ELSE
 
            N = IDINT(X)
 
         END IF
 
      END IF
 
      RETURN
      END
