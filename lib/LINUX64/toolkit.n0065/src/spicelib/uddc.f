C$Procedure UDDC ( Derivative of function less than zero, df(x)/dx < 0 )

      SUBROUTINE UDDC ( UDFUNC, X, DX, ISDECR )

C$ Abstract
C
C    This routine calculates the derivative of UDFUNC with respect 
C    to time for X, then determines if the derivative has 
C    a negative value.
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
C    MATH
C    DERIVATIVE
C
C$ Declarations

      IMPLICIT NONE

      EXTERNAL              UDFUNC

      DOUBLE PRECISION      X
      DOUBLE PRECISION      DX
      LOGICAL               ISDECR

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     UDFUNC     I   The routine that computes the scalar value
C                    of interest.
C     X          I   Independent variable of UDFUNC.
C     DX         I   Interval from X for derivative calculation.
C     ISDECR     O   Boolean indicating if the derivative is negative.
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
C     ISDECR      a scalar boolean indicating if the first derivative
C                 of UDFUNC with respect to the independent variable 
C                 at X is less than zero.
C
C                 Functionally:
C
C                               d UDFUNC(x) |
C                    ISDECR =   --          |  <  0
C                               dx          |
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
C    This routine only wraps a UDDF call, examining the sign of the
C    derivative value returned by UDDF.
C
C$ Examples
C
C    See GFUDS.
C
C$ Restrictions
C
C    None.
C
C$ Literature_References
C
C    See UDDF header
C
C$ Author_and_Institution
C
C    N.J. Bachman   (JPL)
C    E.D. Wright    (JPL)
C
C$ Version
C
C-   SPICELIB Version 1.0.0,  31-MAR-2010 (EDW) 
C
C-&

C$ Index_Entries
C
C    first derivative of scalar function less than zero
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
      DOUBLE PRECISION      DERIV

      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'UDDC' )

      ISDECR = .FALSE.
 
C
C     Numerically calculate the derivative of UDFUNC at X.
C
      CALL UDDF ( UDFUNC, X, DX, DERIV )

      IF (  FAILED() ) THEN
         CALL CHKOUT ( 'UDDC' )
         RETURN
      END IF
  
      ISDECR = DERIV .LT. 0.D0

      CALL CHKOUT ( 'UDDC' )
      RETURN

      END
