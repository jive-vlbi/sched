C$Procedure ZZMULT ( Safer multiplication )

      DOUBLE PRECISION FUNCTION ZZMULT ( A, B )

C$ Abstract
C
C     Safely calculate the value A*B, avoiding the possibility
C     of floating point exceptions (FPE) due to numeric underflow
C     or numeric overflow.
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
C     MATH
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     A          I   Multiplier.
C     B          I   Multiplicand.
C
C$ Detailed_Input
C
C     A          Multiplier for the multiplication operation.
C
C     B          Multiplicand for the multiplication operation.
C
C$ Detailed_Output
C
C     ZZMULT      The value A*B.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) SPICE(NUMERICOVERFLOW) signals if the logarithm base 10
C        of the multiplication is greater than EXPNT (defined
C        below).
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     We want to avoid a floating point exception signal from
C     the platform. This routine does not trap exceptions,
C     the intended purpose is to prevent exceptions.
C
C     Given, for the IEEE 754 double-precision binary
C     floating-point format, the order of magnitude of the minimum
C     normal positive double equals -307 and the order of magnitude of
C     the maximum double equals 308.
C
C       -307 <= LOG10(|A|) + LOG10(|B|) <= 308
C
C     or
C
C         -307                   308
C        10     <=  |A|*|B| <= 10
C
C     Satisfying this condition should guarantee no floating
C     point exceptions.
C
C     Underflow returns zero without an error signal as per SPICE
C     convention.
C
C     Important, this routine does not calculate or enforce a
C     precision on the multiplication evaluation. A safe evaluation
C     may result in a result unusable due to precision loss.
C
C     The routine does not depend on platform-specific arithmetic
C     exception handling, even though the bound for the computed
C     ratio is platform-specific.
C
C     The range [-307,308] is valid for IEEE double precision.
C     It may occur this routine runs on a non compliant platform,
C     so calculate the range based on the DPMAX() value.
C
C     Assign a parameter EXPNT such that EXPNT equals the order of
C     DPMAX. The routine uses the range [-(EXPNT-1), EXPNT].
C
C     This routine checks the sum of the base 10 logarithms
C     of A and B to ensure the magnitude of A*B is
C     within the range [-(EXPNT-1), EXPNT].
C
C$ Examples
C
C     Demonstrate the use of ZZMULT with DPMAX and zero as the
C     multiplier and multiplicand.
C
C           PROGRAM ZZMULT_T
C           IMPLICIT NONE
C
C           DOUBLE PRECISION      A
C           DOUBLE PRECISION      B
C           DOUBLE PRECISION      MULT
C
C     C
C     C     SPICE functions.
C     C
C           DOUBLE PRECISION      ZZMULT
C           DOUBLE PRECISION      DPMAX
C
C
C     C
C     C     Set error reporting to REPORT.
C     C
C           CALL ERRACT( 'SET', 'REPORT' )
C
C
C     C
C     C     Standard, safe evaluation.
C     C
C           A = 1.D0
C           B = 10.D0
C
C           MULT = ZZMULT( A, B )
C           WRITE(*,*) 'MULT 1*10         = ', MULT
C
C
C
C     C
C     C     A numeric overflow event as defined in ZZMULT.
C     C
C           A = 1.D0
C           B = DPMAX()
C
C           MULT = ZZMULT( A, B )
C           WRITE(*,*) 'MULT 1*DPMAX()    = ', MULT
C
C
C
C     C
C     C     A numeric overflow event as defined in ZZMULT.
C     C
C           A = 1.D208
C           B = 1.D307
C
C           MULT = ZZMULT( A, B )
C           WRITE(*,*) 'MULT 1.D515       = ', MULT
C
C
C
C     C
C     C     A multiply by zero event.
C     C
C           A = 1.D0
C           B = 0.D0
C
C           MULT = ZZMULT( A, B )
C           WRITE(*,*) 'MULT 1*0          = ', MULT
C
C
C
C     C
C     C     A multiply by zero event.
C     C
C           A = 0.D0
C           B = 0.D0
C
C           MULT = ZZMULT( A, B )
C           WRITE(*,*) 'MULT 0*0          = ', MULT
C
C
C           END
C
C   The program outputs:
C
C   -The function returns the evaluation value. No error.
C
C      MULT 1*10         =    10.000000000000000
C
C
C   -The function signals a NUMERICOVERFLOW error, and sets the
C    return value to zero.
C
C     =================================================================
C
C     Toolkit version: N0064
C
C     SPICE(NUMERICOVERFLOW) --
C
C     Numerical overflow event. Multiplier value, 1.0000000000000E+00,
C     multiplicand value, 1.7976931348623E+308.
C
C     A traceback follows.  The name of the highest level module is
C     first.
C     ZZMULT
C
C     =================================================================
C      MULT 1*DPMAX()   =  0.0000000000000000
C
C
C   -The function signals a NUMERICOVERFLOW error, and sets the
C    return value to zero.
C
C     =================================================================
C
C     Toolkit version: N0064
C
C     SPICE(NUMERICOVERFLOW) --
C
C     Numerical overflow event. Multiplier value, 1.0000000000000E+208,
C     multiplicand value, 1.0000000000000E+307.
C
C     A traceback follows.  The name of the highest level module is
C     first.
C     ZZMULT
C
C     =================================================================
C      MULT 1.D515       =    0.0000000000000000
C
C
C   -The function returns the evaluation value. No error.
C
C      MULT 1*0          =    0.0000000000000000
C
C
C   -The function returns the evaluation value. No error.
C
C      MULT 0*0          =    0.0000000000000000
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
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 31-JAN-2014 (EDW)
C
C-&

C$ Index_Entries
C
C     multiplication, avoid floating point exception
C
C-&

C
C     SPICELIB functions
C

      LOGICAL               RETURN
      DOUBLE PRECISION      DPMAX

C
C     Local variables
C

      DOUBLE PRECISION      LOGA
      DOUBLE PRECISION      LOGB

C
C     The bounds on the potential result of the calculation.
C
      DOUBLE PRECISION      EXPNT

C
C     First entry flag.
C
      LOGICAL               FIRST
      SAVE

      DATA                  FIRST / .TRUE. /

C
C     Return on error.
C
      IF ( RETURN() ) THEN
         ZZMULT = 0.D0
         RETURN
      END IF


C
C     Participate in error tracing.
C
      CALL CHKIN ( 'ZZMULT' )


C
C     Calculate the bounds parameter on first entry.
C     The double precision maximum value has the form
C     "d*(10**EXPNT)." The value of interest is "EXPNT."
C
      IF (FIRST) THEN

         FIRST  = .FALSE.

C
C        A "floor" evaluation.
C
         EXPNT = DBLE( INT( LOG10( DPMAX() ) ) )

       END IF


C
C     If either A or B equals zero the multiplication is zero.
C
      IF ( (A .EQ. 0.D0) .OR. (B .EQ. 0.D0) ) THEN

         ZZMULT = 0.D0
         CALL CHKOUT ( 'ZZMULT' )
         RETURN

      END IF


C
C     Calculate base 10 logarithms of the absolute value of the
C     numerator and denominator. Recall the base 10 log of a negative
C     real is a complex number (an irritating reality). Our interest
C     is the magnitude of the result, not the sign.
C
C     An earlier check returned if A or B equal zero.
C
      LOGA = LOG10( DABS(A) )
      LOGB = LOG10( DABS(B) )

C
C     Local possible overflow check.
C
      IF ( (LOGA + LOGB) .GT. EXPNT ) THEN

         ZZMULT = 0.D0
         CALL SETMSG ( 'Numerical overflow event. '
     .              // 'Multiplier value, #1, '
     .              // 'multiplicand value, #2.'  )
         CALL ERRDP  ( '#1', A                    )
         CALL ERRDP  ( '#2', B                    )
         CALL SIGERR ( 'SPICE(NUMERICOVERFLOW)'   )
         CALL CHKOUT ( 'ZZMULT'                   )
         RETURN

      END IF

C
C     Local possible underflow check. Accept this may occur,
C     return a zero.
C
      IF ( (LOGA + LOGB) .LT. -(EXPNT-1.D0) ) THEN

         ZZMULT = 0.D0
         CALL CHKOUT ( 'ZZMULT' )
         RETURN

      END IF

C
C     This operation should be safe. Probably.
C
      ZZMULT = A*B

      CALL CHKOUT ( 'ZZMULT' )

      RETURN
      END
