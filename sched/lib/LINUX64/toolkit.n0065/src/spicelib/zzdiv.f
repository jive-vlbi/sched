C$Procedure ZZDIV ( Safer division )

      DOUBLE PRECISION FUNCTION ZZDIV ( NUMR, DENOM )

C$ Abstract
C
C     Safely calculate the value NUMR/DENOM, avoiding the possibility
C     of floating point exceptions (FPE), due to numeric underflow,
C     numeric overflow, or divide-by-zero.
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

      DOUBLE PRECISION      NUMR
      DOUBLE PRECISION      DENOM

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NUMR       I   Numerator of division.
C     DENOM      I   Denominator of division.
C
C$ Detailed_Input
C
C     NUMR       Numerator for the division operation.
C
C     DENOM      Denominator for the division operation.
C
C$ Detailed_Output
C
C     ZZDIV      The value NUMR/DENOM.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) SPICE(DIVIDEBYZERO) signals if DENOM equals zero. This
C        signal occurs for NUMR/0 and 0/0 cases.
C
C     2) SPICE(NUMERICOVERFLOW) signals if the logarithm base 10
C        of the division is greater than EXPNT (defined
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
C       -307 <= LOG10(|NUMR|) - LOG10(|DENOM|) <= 308
C
C     or
C
C         -307                         308
C        10     <=  |NUMR|/|DENOM| <= 10
C
C     Satisfying this condition should guarantee no floating
C     point exceptions.
C
C     Underflow returns zero without an error signal as per SPICE
C     convention.
C
C     Important, this routine does not calculate or enforce a
C     precision on the division evaluation. A safe evaluation
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
C     This routine checks the difference between the base 10 logarithms
C     of NUMR and DENOM to ensure the magnitude of NUMR/DENOM is
C     within the range [-(EXPNT-1), EXPNT].
C
C$ Examples
C
C     Demonstrate the use of ZZDIV with DPMAX and zero as the
C     numerator and denominator.
C
C           PROGRAM ZZDIV_T
C           IMPLICIT NONE
C
C           DOUBLE PRECISION      NUMR
C           DOUBLE PRECISION      DENOM
C           DOUBLE PRECISION      DIV
C
C     C
C     C     SPICE functions.
C     C
C           DOUBLE PRECISION      ZZDIV
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
C           NUMR  = 1.D0
C           DENOM = 10.D0
C
C           DIV = ZZDIV( NUMR, DENOM )
C           WRITE(*,*) 'DIV 1/10      = ', DIV
C
C
C
C     C
C     C     A numeric underflow event as defined in ZZDIV.
C     C
C           NUMR  = 1.D0
C           DENOM = DPMAX()
C
C           DIV = ZZDIV( NUMR, DENOM )
C           WRITE(*,*) 'DIV 1/DPMAX() = ', DIV
C
C
C
C     C
C     C     A numeric overflow event as defined in ZZDIV.
C     C
C           NUMR  = DPMAX()
C           DENOM = 1.D0
C
C           DIV = ZZDIV( NUMR, DENOM )
C           WRITE(*,*) 'DIV DPMAX()/1 = ', DIV
C
C
C     C
C     C     A divide by zero event.
C     C
C           NUMR  = 1.D0
C           DENOM = 0.D0
C
C           DIV = ZZDIV( NUMR, DENOM )
C           WRITE(*,*) 'DIV 1/0       = ', DIV
C
C
C     C
C     C     A 0/0 event. ZZDIV treats this as a divide by zero
C     C     event.
C     C
C           NUMR  = 0.D0
C           DENOM = 0.D0
C
C           DIV = ZZDIV( NUMR, DENOM )
C           WRITE(*,*) 'DIV 0/0       = ', DIV
C
C
C           END
C
C   The program outputs:
C
C   -The function returns the evaluation value. No error.
C
C      DIV 1/10      =   0.10000000000000001
C
C
C
C      -The function returns zero for an underflow state. No error.
C
C      DIV 1/DPMAX() =    0.0000000000000000
C
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
C     Numerical overflow event. Numerator value 1.7976931348623E+308,
C     denominator value 1.0000000000000E+00.
C
C     A traceback follows.  The name of the highest level module is
C     first.
C     ZZDIV
C
C     =================================================================
C      DIV DPMAX()/1 =    0.0000000000000000
C
C
C
C   -The function signals a DIVIDEBYZERO error, and sets the
C    return value to zero.
C
C     =================================================================
C
C     Toolkit version: N0064
C
C     SPICE(DIVIDEBYZERO) --
C
C     Numerical divide by zero event. Numerator value
C     1.0000000000000E+00.
C
C     A traceback follows.  The name of the highest level module is
C     first.
C     ZZDIV
C
C     =================================================================
C      DIV 1/0       =    0.0000000000000000
C
C
C
C   -The function signals a DIVIDEBYZERO error, and sets the
C    return value to zero.
C
C     =================================================================
C
C     Toolkit version: N0064
C
C     SPICE(DIVIDEBYZERO) --
C
C     Numerical divide by zero event. Numerator value
C     0.0000000000000E+00.
C
C     A traceback follows.  The name of the highest level module is
C     first.
C     ZZDIV
C
C     =================================================================
C      DIV 0/0       =    0.0000000000000000
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
C     division, avoid floating point exception
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

      DOUBLE PRECISION      LOGNUM
      DOUBLE PRECISION      LOGDEN

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
         ZZDIV = 0.D0
         RETURN
      END IF


C
C     Participate in error tracing.
C
      CALL CHKIN ( 'ZZDIV' )


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
C     If the denominator is zero, return zero and signal an error.
C     This is equivalent to a signaling NaN (not-a-number) for
C     the 0/0 case.
C
      IF ( DENOM .EQ. 0.D0 ) THEN

         ZZDIV = 0.D0
         CALL SETMSG ( 'Numerical divide by zero event. '
     .              // 'Numerator value #1.'             )
         CALL ERRDP  ( '#1', NUMR                        )
         CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'             )
         CALL CHKOUT ( 'ZZDIV'                           )
         RETURN

      END IF


C
C     If the numerator is zero, the division is zero. DENOM
C     is known non-zero.
C
      IF ( NUMR .EQ. 0.D0 ) THEN

         ZZDIV = 0.D0
         CALL CHKOUT ( 'ZZDIV' )
         RETURN

      END IF


C
C     Calculate base 10 logarithms of the absolute value of the
C     numerator and denominator. Recall the base 10 log of a negative
C     real is a complex number (an irritating reality). Our interest
C     is the magnitude of the result, not the sign.
C
C     An earlier check returned if NUMR or DENOM equals zero, so the
C     LOG10 call is safe from an infinite return value. An infinite
C     return value defeats the purpose of this routine.
C
      LOGNUM = LOG10( DABS(NUMR)  )
      LOGDEN = LOG10( DABS(DENOM) )

C
C     Local possible overflow check.
C
      IF ( (LOGNUM - LOGDEN) .GT. EXPNT ) THEN

         ZZDIV = 0.D0
         CALL SETMSG ( 'Numerical overflow event. '
     .              // 'Numerator value #1, denominator value #2.')
         CALL ERRDP  ( '#1', NUMR                                 )
         CALL ERRDP  ( '#2', DENOM                                )
         CALL SIGERR ( 'SPICE(NUMERICOVERFLOW)'                   )
         CALL CHKOUT ( 'ZZDIV'                                    )
         RETURN

      END IF

C
C     Local possible underflow check. Accept this may occur,
C     return a zero.
C
      IF ( (LOGNUM - LOGDEN) .LT. -(EXPNT-1.D0) ) THEN

         ZZDIV = 0.D0
         CALL CHKOUT ( 'ZZDIV' )
         RETURN

      END IF

C
C     This operation should be safe. Probably.
C
      ZZDIV = NUMR/DENOM

      CALL CHKOUT ( 'ZZDIV' )

      RETURN
      END
