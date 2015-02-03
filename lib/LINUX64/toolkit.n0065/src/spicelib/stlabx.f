C$Procedure      STLABX ( Stellar aberration, transmission case )
 
      SUBROUTINE STLABX ( POBJ, VOBS, CORPOS )
      IMPLICIT NONE
 
C$ Abstract
C
C     Correct the position of a target for the stellar aberration 
C     effect on radiation transmitted from a specified observer to 
C     the target.  
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
      DOUBLE PRECISION   CORPOS ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      POBJ       I   Position of an object with respect to the
C                     observer.
C      VOBS       I   Velocity of the observer with respect to the
C                     Solar System barycenter.
C      CORPOS     O   Corrected position of the object.
C
C$ Detailed_Input
C
C      POBJ        is the cartesian position vector of an object with
C                  respect to the observer, possibly corrected for
C                  light time.  Units are km.
C
C      VOBS        is the cartesian velocity vector of the observer
C                  with respect to the Solar System barycenter.  Units
C                  are km/s.
C
C$ Detailed_Output
C
C      CORPOS      is the  position of the object relative to the 
C                  observer, corrected for the stellar aberration
C                  effect on radiation directed toward the target.  This
C                  correction is the inverse of the usual stellar 
C                  aberration correction:  the corrected vector 
C                  indicates the direction in which radiation must be
C                  emitted from the observer, as seen in an inertial
C                  reference frame having velocity equal to that of the
C                  observer, in order to reach the position indicated by
C                  the input vector POBJ.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the velocity of the observer is greater than or equal
C        to the speed of light, the error is diagnosed by a routine
C        called by this routine.  The outputs are undefined.
C
C$ Files
C
C     None.
C
C$ Particulars
C  
C     In order to transmit radiation from an observer to a specified 
C     target, the emission direction must be corrected for one way
C     light time and for the motion of the observer relative to the 
C     solar system barycenter.  The correction for the observer's 
C     motion when transmitting to a target is the inverse of the 
C     usual stellar aberration correction applied to the light-time 
C     corrected position of the target as seen by the observer.
C     
C     Below is the description of the stellar aberration correction
C     used in the SPICELIB routine STELAB (with the notation changed
C     slightly):
C
C        Let r be the vector from the observer to the object, and v be
C        the velocity of the observer with respect to the Solar System
C        barycenter. Let w be the angle between them. The aberration
C        angle phi is given by
C
C           sin(phi) = v sin(w) / c
C
C        Let h be the vector given by the cross product
C
C           h = r X v
C
C        Rotate r by phi radians about h to obtain the apparent position
C        of the object.
C
C     This routine applies the inverse correction, so here the rotation 
C     about h is by -phi radians.
C
C$ Examples
C
C     In the following example, STLABX is used to correct the position
C     of a target body for the stellar aberration effect on radiation
C     transmitted to the target.
C
C          [Previous subroutine calls have loaded an SPK file and
C           the leapseconds kernel file.  The SPK file contains
C           sufficient data to enable computation of observer and
C           target states relative to the solar system barycenter.]
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
C            CALL SPKAPO ( TARG, ET, 'J2000', SOBS, 'XLT', TPOS, LT )
C
C      C
C      C     Apply the correction for stellar aberration to the
C      C     light-time corrected position of the target body.
C      C     The corrected position is returned in the argument
C      C     PCORR.
C      C
C            CALL STLABX ( TPOS, SOBS(4), PCORR )
C
C
C      Note that this example is somewhat contrived. The sequence
C      of calls above could be replaced by a single call to SPKEZP,
C      using the aberration correction flag 'XLT+S'.
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
C     None.
C
C$ Literature_References
C
C     1) W.M. Owen, Jr., JPL IOM #314.8-524, "The Treatment of
C        Aberration in Optical Navigation", 8 February 1985.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 8-JAN-2008 (NJB)
C
C        The header example was updated to remove references
C        to SPKAPP.
C
C-    SPICELIB Version 1.0.0, 02-JAN-2002 (IMU) (WLT) (NJB)
C
C-&
 
C$ Index_Entries
C
C     stellar aberration for transmission case
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C 
      DOUBLE PRECISION      NEGVEL ( 3 )

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STLABX' )
      END IF

C
C     Obtain the negative of the observer's velocity.  This 
C     velocity, combined with the target's position, will yield
C     the inverse of the usual stellar aberration correction,
C     which is exactly what we seek.
C
      CALL VMINUS ( VOBS, NEGVEL )

      CALL STELAB ( POBJ, NEGVEL, CORPOS )

      CALL CHKOUT ( 'STLABX' )
      RETURN
      END

