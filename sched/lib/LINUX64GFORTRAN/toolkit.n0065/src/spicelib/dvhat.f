C$Procedure DVHAT ( Derivative and unit vector "V-hat" of a state)

      SUBROUTINE DVHAT ( S1, SOUT )

C$ Abstract
C
C     Find the unit vector corresponding to a state vector and the
C     derivative of the unit vector.
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
C     VECTOR
C     DERIVATIVE
C     MATH
C
C$ Declarations

      DOUBLE PRECISION   S1   ( 6 )
      DOUBLE PRECISION   SOUT ( 6 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     S1        I     State to be normalized.
C     SOUT      O     Unit vector S1 / |S1|, and its time derivative.
C
C$ Detailed_Input
C
C     S1       This is any double precision state. If the position
C              component of the state is the zero vector, this routine
C              will detect it and will not attempt to divide by zero.
C
C$ Detailed_Output
C
C     SOUT     SOUT is a state containing the unit vector pointing in
C              the direction of position component of S1 and the
C              derivative of the unit vector with respect to time.
C
C              SOUT may overwrite S1.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If S1 represents the zero vector, then the position
C        component of SOUT will also be the zero vector.  The
C        velocity component will be the velocity component
C        of S1.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Let S1 be a state vector with position and velocity components P
C     and V respectively.  From these components one can compute the
C     unit vector parallel to P, call it U and the derivative of U
C     with respect to time, DU.  This pair (U,DU) is the state returned
C     by this routine in SOUT.
C
C$ Examples
C
C   Any numerical results shown for this example may differ between
C   platforms as the results depend on the SPICE kernels used as input
C   and the machine specific arithmetic implementation.
C
C   Suppose that STATE gives the apparent state of a body with
C   respect to an observer.  This routine can be used to compute the
C   instantaneous angular rate of the object across the sky as seen
C   from the observers vantage.
C
C           PROGRAM DVHAT_T
C           IMPLICIT NONE
C
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      OMEGA
C           DOUBLE PRECISION      STATE  (6)
C           DOUBLE PRECISION      USTATE (6)
C
C           DOUBLE PRECISION      VNORM
C
C           CHARACTER*(32)        EPOCH
C           CHARACTER*(32)        TARGET
C           CHARACTER*(32)        FRAME
C           CHARACTER*(32)        ABCORR
C           CHARACTER*(32)        OBSRVR
C
C     C
C     C     Load SPK, PCK, and LSK kernels, use a meta kernel for 
C     C     convenience.
C     C
C           CALL FURNSH ( 'standard.tm' )
C
C     C
C     C     Define an arbitrary epoch, convert the epoch to ephemeris 
C     C     time.
C     C
C           EPOCH = 'Jan 1 2009'
C           CALL STR2ET ( EPOCH, ET )
C
C     C
C     C     Calculate the state of the moon with respect to the 
C     C     earth-moon barycenter in J2000, corrected for light time
C     C     and stellar aberration at ET.
C     C
C           TARGET   = 'MOON'
C           FRAME    = 'J2000'
C           ABCORR   = 'LT+S'
C           OBSRVR   = 'EARTH BARYCENTER'
C
C           CALL SPKEZR ( TARGET, ET, FRAME, ABCORR, OBSRVR, STATE, LT )
C
C     C
C     C     Calculate the unit vector of STATE and the derivative of the
C     C     unit vector.
C     C
C           CALL DVHAT ( STATE, USTATE )
C
C     C
C     C     Calculate the instantaneous angular velocity from the
C     C     magnitude of the derivative of the unit vector.
C     C
C     C          v = r x omega
C     C
C     C          ||omega|| = ||v||  for  r . v = 0
C     C                      -----
C     C                      ||r||
C     C
C     C          ||omega|| = ||v||  for  ||r|| = 1
C     C
C           OMEGA = VNORM( USTATE(4) )
C
C           WRITE(*,*) 'Instantaneous angular velocity, rad/sec', OMEGA
C
C           END
C
C   The program outputs:
C
C       Instantaneous angular velocity, rad/sec  2.48106658E-06
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 06-MAY-2010 (EDW)
C
C        Expanded the code example into a complete program.
C
C        Reordered header sections to proper NAIF convention.
C        Removed Revision section, it listed a duplication of a 
C        Version section entry. 
C
C-    SPICELIB Version 1.1.0, 02-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VPERP and VSCL calls.
C
C-    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT)
C
C-&


C$ Index_Entries
C
C     State of a unit vector parallel to a state vector
C
C-&

      INTEGER               POS
      PARAMETER           ( POS = 1 )

      INTEGER               VEL
      PARAMETER           ( VEL = 4 )

      DOUBLE PRECISION      LENGTH

C
C     Get the position portion of the output state and the length of
C     the input position.
C
      CALL UNORM ( S1(POS), SOUT(POS), LENGTH )

      IF ( LENGTH .EQ. 0.0D0 ) THEN
C
C        If the length of the input position is zero, just copy
C        the input velocity to the output velocity.
C
         CALL VEQU ( S1(VEL), SOUT(VEL) )

      ELSE
C
C        Otherwise the derivative of the unit vector is just the
C        component of the input velocity perpendicular to the input
C        position, scaled by the reciprocal of the length of the
C        input position.
C
         CALL VPERP  ( S1(VEL),      SOUT(POS), SOUT(VEL) )
         CALL VSCLIP ( 1.0D0/LENGTH,            SOUT(VEL) )

      END IF

      RETURN
      END




