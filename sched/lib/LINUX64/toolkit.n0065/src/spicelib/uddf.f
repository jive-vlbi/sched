C$Procedure UDDF ( First derivative of a function, df(x)/dx )

      SUBROUTINE UDDF ( UDFUNC, X, DX, DERIV )

C$ Abstract
C
C     Routine to calculate the first derivative of a caller-specified
C     scalar function using a three-point estimation.
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
C    None.
C
C$ Keywords
C
C    MATH
C    DERIVATIVE
C
C$ Declarations

      IMPLICIT NONE

      EXTERNAL              UDFUNC

      DOUBLE PRECISION      X
      DOUBLE PRECISION      DX
      DOUBLE PRECISION      DERIV

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     UDFUNC     I   The routine that computes the scalar value
C                    of interest.
C     X          I   Independent variable of UDFUNC.
C     DX         I   Interval from X for derivative calculation.
C     DERIV      O   Approximate derivative of UDFUNC at X.
C
C$ Detailed_Input
C
C     UDFUNC     the routine that returns the value of the scalar 
C                quantity function of interest at X. The calling 
C                sequence for UDFUNC is:
C
C                   CALL UDFUNC ( X, VALUE )
C
C                where:
C
C                   X       the double precision value of the 
C                           independent variable of the function
C                           at which to determine the scalar value.
C
C                   VALUE   the double precision value returned by 
C                           UDFUNC at X.
C
C                Functionally:
C
C                   VALUE = UDFUNC ( X )
C
C     X          a scalar double precision value at which to determine
C                the derivative of UDFUNC.
C
C                For many SPICE uses, X will represent ephemeris time, 
C                expressed as seconds past J2000 TDB.
C
C     DX         a scalar double precision value representing half the 
C                interval in units of X separating the evaluation
C                values of UDFUNC; the evaluations occur at (X + DX) 
C                and (X - DX).
C
C                DX may be negative but must be non-zero.
C
C$ Detailed_Output
C
C     DERIV      the scalar double precision approximate value of the 
C                first derivative of UDFUNC with respect to X.
C
C                Functionally:
C
C                            d UDFUNC ( x ) |
C                   DERIV =  --             |
C                            dx             |
C                                            X
C
C$ Parameters
C
C    None.
C
C$ Exceptions
C
C     1) A routine in the call tree of this routine signals
C        SPICE(DIVIDEBYZERO) if DX has a value of zero.
C
C$ Files
C
C     If the evaluation of UDFUNC requires SPICE kernel data, the
C     appropriate kernels must be loaded before calling this routine.
C
C        - SPK data: the calling application must load ephemeris data
C          for the targets, observer, and any intermediate objects in 
C          a chain connecting the targets and observer for the time
C          used in the evaluation. If aberration corrections are 
C          used, the states of target and observer relative to the 
C          solar system barycenter must be calculable from the 
C          available ephemeris data.
C
C        - If non-inertial reference frames are used, then PCK
C          files, frame kernels, C-kernels, and SCLK kernels may be
C          needed.
C
C     Such kernel data are normally loaded once per program run, NOT 
C     every time this routine is called. 
C
C$ Particulars
C
C     This routine provides a simple interface to numerically calculate
C     the first derivative of a scalar quantity function, UDFUNC.
C     UDFUNC is expected to be "well behaved" across at the evaluation
C     interval [ X - DX, X + DX ]. This means a linear approximation to
C     the function over the interval is sufficiently accurate to 
C     calculate the approximate derivative at X.
C
C     The routine QDERIV performs the differentiation using a three 
C     point estimation.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C     Calculate the time derivative of the light time corresponding to 
C     the apparent position of Mercury relative to the Moon at 
C     time "JAN 1 2009."
C
C           PROGRAM UDDF_T
C           
C           EXTERNAL                 UDFUNC      
C     
C           DOUBLE PRECISION         ET
C           DOUBLE PRECISION         DT
C           DOUBLE PRECISION         DERIV
C     
C     C
C     C     Load leapsecond and SPK kernels. The name of the 
C     C     meta kernel file shown here is fictitious; you 
C     C     must supply the name of a file available 
C     C     on your own computer system.
C     C
C           CALL FURNSH ( 'standard.tm' )
C     
C     C
C     C     Use a shift of one second off the epoch of interest.
C     C
C           DT = 1.D0
C
C     C
C     C     Convert the epoch date string to ephemeris seconds.
C     C
C           CALL STR2ET ( 'JAN 1 2009', ET )
C     
C     C
C     C     Calculate the derivative of UDFUNC at ET.
C     C
C           CALL UDDF ( UDFUNC, ET, DT, DERIV )
C     
C     C
C     C     Output the calculated derivative.
C     C
C           WRITE(*,*) DERIV
C     
C           END
C     
C     C
C     C     A scalar quantity function that returns the light-time
C     C     between the Moon and Mercury at ET.
C     C
C           SUBROUTINE UDFUNC ( ET, VALUE )
C     
C           IMPLICIT NONE
C     
C           DOUBLE PRECISION         ET
C           DOUBLE PRECISION         VALUE
C     
C           DOUBLE PRECISION         POS  (3)
C           DOUBLE PRECISION         LT
C     
C     C
C     C     Evaluate the apparent position of Mercury with respect 
C     C     to the Moon at ET.
C     C
C           CALL SPKPOS ( 'MERCURY', ET, 'J2000', 'LT+S', 'MOON',
C          .               POS,   LT )
C     
C     C
C     C     Return the light-time value as the scalar quantity.
C     C
C           VALUE = LT
C     
C           END
C
C     The program outputs (OS X Intel run):
C
C         -0.00013567094
C
C$ Restrictions
C
C    The function UDFUNC must exist everywhere within [X - DX, X + DX].
C
C$ Literature_References
C
C    See QDERIV header
C
C$ Author_and_Institution
C
C    N.J. Bachman   (JPL)
C    E.D. Wright    (JPL)
C
C$ Version
C
C-   SPICELIB Version 1.0.0  31-MAR-2010 (EDW) 
C
C-&

C$ Index_Entries
C
C    first derivative of a user-defined scalar function
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED

C
C     Local Variables
C
      INTEGER               N
      DOUBLE PRECISION      DFDX  (1)
      DOUBLE PRECISION      UDVAL (2)

 
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'UDDF' )
 
C
C     Apply a three-point estimation of the derivative for
C     UDFUNC at X by evaluating UDFUNC at [X-DX, X+DX].
C
C     The QDERIV call returns a single value in the DFDX array.
C
      N  = 1

C
C     Evaluate the scalar function at the interval boundaries.
C     Check for a FAILED event.
C
      CALL UDFUNC ( X-DX, UDVAL(1) )

      IF (  FAILED() ) THEN
         CALL CHKOUT ( 'UDDF' )
         RETURN
      END IF
      
      CALL UDFUNC ( X+DX, UDVAL(2) )

      IF (  FAILED() ) THEN
         CALL CHKOUT ( 'UDDF' )
         RETURN
      END IF

C
C     Estimate the derivative at X.
C
      CALL QDERIV ( N, UDVAL(1), UDVAL(2), DX, DFDX )

      IF (  FAILED() ) THEN
         CALL CHKOUT ( 'UDDF' )
         RETURN
      END IF

      DERIV = DFDX(1)

      CALL CHKOUT ( 'UDDF' )
      RETURN

      END

