C$Procedure      STELAB     ( Stellar Aberration )
 
      SUBROUTINE STELAB ( POBJ, VOBS, APPOBJ )
 
C$ Abstract
C
C      Correct the apparent position of an object for stellar
C      aberration.
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
C      EPHEMERIS
C
C$ Declarations
 
      DOUBLE PRECISION   POBJ   ( 3 )
      DOUBLE PRECISION   VOBS   ( 3 )
      DOUBLE PRECISION   APPOBJ ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      POBJ       I   Position of an object with respect to the
C                     observer.
C      VOBS       I   Velocity of the observer with respect to the
C                     Solar System barycenter.
C      APPOBJ     O   Apparent position of the object with respect to
C                     the observer, corrected for stellar aberration.
C
C$ Detailed_Input
C
C      POBJ        is the position (x, y, z, km) of an object with
C                  respect to the observer, possibly corrected for
C                  light time.
C
C      VOBS        is the velocity (dx/dt, dy/dt, dz/dt, km/sec)
C                  of the observer with respect to the Solar System
C                  barycenter.
C
C$ Detailed_Output
C
C      APPOBJ      is the apparent position of the object relative
C                  to the observer, corrected for stellar aberration.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the velocity of the observer is greater than or equal
C        to the speed of light, the error SPICE(VALUEOUTOFRANGE)
C        is signaled.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      Let r be the vector from the observer to the object, and v be
C          -                                                    -
C      the velocity of the observer with respect to the Solar System
C      barycenter. Let w be the angle between them. The aberration
C      angle phi is given by
C
C           sin(phi) = v sin(w) / c
C
C      Let h be the vector given by the cross product
C          -
C
C            h = r X v
C            -   -   -
C
C      Rotate r by phi radians about h to obtain the apparent position
C             -                      -
C      of the object.
C
C$ Examples
C
C      In the following example, STELAB is used to correct the position
C      of a target body for stellar aberration.
C
C
C          (Previous subroutine calls have loaded the SPK file and
C           the leapseconds kernel file.)
C
C
C      C
C      C     Get the geometric state of the observer OBS relative to 
C      C     the solar system barycenter.
C      C
C            CALL SPKSSB ( OBS, ET, 'J2000', SOBS )
C
C      C
C      C     Get the light-time corrected position TPOS of the target
C      C     body TARG as seen by the observer. Normally we would
C      C     call SPKPOS to obtain this vector, but we already have
C      C     the state of the observer relative to the solar system
C      C     barycenter, so we can avoid looking up that state twice
C      C     by calling SPKAPO.
C      C
C            CALL SPKAPO ( TARG, ET, 'J2000', SOBS, 'LT', TPOS, LT )
C
C      C
C      C     Apply the correction for stellar aberration to the
C      C     light-time corrected position of the target body.
C      C     The corrected position is returned in the argument
C      C     PCORR.
C      C
C            CALL STELAB ( TPOS, SOBS(4), PCORR )
C
C
C      Note that this example is somewhat contrived. The sequence
C      of calls above could be replaced by a single call to SPKEZP,
C      using the aberration correction flag 'LT+S'.
C
C      For more information on aberration-corrected states or
C      positions, see the headers of any of the routines
C
C         SPKEZR
C         SPKEZ
C         SPKPOS
C         SPKEZP
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      1) W.M. Owen, Jr., JPL IOM #314.8-524, "The Treatment of
C         Aberration in Optical Navigation", 8 February 1985.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.1.1, 8-JAN-2008 (NJB)
C
C         The header example was updated to remove references
C         to SPKAPP.
C
C-     SPICELIB Version 1.1.0, 8-FEB-1999 (WLT)
C
C         The example was corrected so that SOBS(4) is passed
C         into STELAB instead of STARG(4).
C
C-     SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.1, 8-AUG-1990 (HAN)
C
C         Examples section of the header was updated to replace
C         calls to the GEF ephemeris readers by calls to the
C         new SPK ephemeris reader.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
C
C-&
 
C$ Index_Entries
C
C     stellar aberration
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 2.1.0, 9-MAR-1989 (HAN)
C
C         Declaration of the variable LIGHT was removed from the code.
C         The variable was declared but never used.
C
C-     Beta Version 2.0.0, 28-DEC-1988 (HAN)
C
C         Error handling was added to check the velocity of the
C         observer. If the velocity of the observer is greater
C         than or equal to the speed of light, the error
C         SPICE(VALUEOUTOFRANGE) is signalled.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      LOGICAL               RETURN
 
 
C
C     Local variables
C
 
      DOUBLE PRECISION      ONEBYC
      DOUBLE PRECISION      U      ( 3 )
      DOUBLE PRECISION      VBYC   ( 3 )
      DOUBLE PRECISION      LENSQR
      DOUBLE PRECISION      H      ( 3 )
      DOUBLE PRECISION      SINPHI
      DOUBLE PRECISION      PHI
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STELAB' )
      END IF
 
 
C
C     We are not going to compute the aberrated vector in exactly the
C     way described in the particulars section.  We can combine some
C     steps and we take some precautions to prevent floating point
C     overflows.
C
C
C     Get a unit vector that points in the direction of the object
C     ( u_obj ).
C
      CALL VHAT ( POBJ, U )
 
C
C     Get the velocity vector scaled with respect to the speed of light
C     ( v/c ).
C
      ONEBYC = 1.0D0 / CLIGHT()
 
      CALL VSCL  ( ONEBYC, VOBS, VBYC )
 
C
C     If the square of the length of the velocity vector is greater than
C     or equal to one, the speed of the observer is greater than or
C     equal to the speed of light. The observer speed is definitely out
C     of range. Signal an error and check out.
C
      LENSQR = VDOT ( VBYC, VBYC )
 
      IF ( LENSQR .GE. 1.0D0 ) THEN
 
         CALL SETMSG ( 'Velocity components of observer were:  '      //
     .                 'dx/dt = *, dy/dt = *, dz/dt = *.'              )
         CALL ERRDP  ( '*', VOBS (1)            )
         CALL ERRDP  ( '*', VOBS (2)            )
         CALL ERRDP  ( '*', VOBS (3)            )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)' )
         CALL CHKOUT ( 'STELAB'                 )
         RETURN
 
      END IF
 
C
C     Compute u_obj x (v/c)
C
      CALL VCRSS ( U, VBYC, H )
 
 
C
C     If the magnitude of the vector H is zero, the observer is moving
C     along the line of sight to the object, and no correction is
C     required. Otherwise, rotate the position of the object by phi
C     radians about H to obtain the apparent position.
C
      SINPHI  = VNORM ( H )
 
      IF ( SINPHI .NE. 0.D0 ) THEN
 
         PHI = DASIN ( SINPHI )
         CALL  VROTV ( POBJ, H, PHI, APPOBJ )
 
      ELSE
 
         CALL MOVED ( POBJ, 3, APPOBJ )
 
      END IF
 
 
      CALL CHKOUT ( 'STELAB' )
      RETURN
      END
