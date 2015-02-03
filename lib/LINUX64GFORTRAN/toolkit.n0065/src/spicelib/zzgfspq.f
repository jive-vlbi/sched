C$Procedure ZZGFSPQ ( GF, separation quantity )

      SUBROUTINE ZZGFSPQ ( ET, TARG1, TARG2, R1, R2,
     .                     OBS, ABCORR, REF, VALUE )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Compute the angular separation between the limbs of two objects.
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
C     GEOMETRY
C     ANGLE
C
C$ Declarations

      IMPLICIT NONE
      DOUBLE PRECISION      ET
      INTEGER               TARG1
      INTEGER               TARG2
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      INTEGER               OBS
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         REF
      DOUBLE PRECISION      VALUE


C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB
C     TARG1      I   NAIF ID for first target
C     TARG2      I   NAIF ID for second target
C     R1         I   Radius of a spherical model for TARG1
C     R2         I   Radius of a spherical model for TARG2
C     OBS        I   NAIF ID of observer
C     ABCORR     I   Aberration correction flag
C     REF        I   Reference frame of the angular separation
C     VALUE      O   Value of angular separation between objects
C
C$ Detailed_Input
C
C     ET       is the time in ephemeris seconds past J2000 TDB at
C              which the separation is to be measured.
C
C     TARG1
C     TARG2    the NAIF IDs of the two objects for which to
C              determine the angular separation.
C
C     R1
C     R2       are the radii of the two objects TARG1 and TARG2
C              respectively.
C
C     OBS      the NAIF ID identifying the body observing
C              TARG1 and TARG2.
C
C     ABCORR   the string description of the aberration corrections
C              to apply to the state evaluations to account for
C              one-way light time and stellar aberration.
C
C              This routine accepts the same aberration corrections
C              as does the SPICE routine SPKEZR. See the header of
C              SPKEZR for a detailed description of the aberration
C              correction options. For convenience, the options are
C              listed below:
C
C                 'NONE'     Apply no correction.
C
C                 'LT'       "Reception" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'LT+S'     "Reception" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'CN'       "Reception" case:  converged
C                            Newtonian light time correction.
C
C                 'CN+S'     "Reception" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 'XLT'      "Transmission" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'XLT+S'    "Transmission" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'XCN'      "Transmission" case:  converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 The ABCORR string lacks sensitivity to case, leading
C                 and trailing blanks.
C
C     REF         is the name of the reference frame relative to which
C                 the angular separation should be expressed. This may
C                 be any frame supported by the SPICE system, including
C                 built-in frames (documented in the Frames Required
C                 Reading) and frames defined by a loaded frame kernel.
C
C                 When REF designates a non-inertial frame, the
C                 orientation of the frame is evaluated at an epoch
C                 dependent on the selected aberration correction.
C
C$ Detailed_Output
C
C     VALUE   is the light-time (and stellar aberration corrected
C             if this feature is enabled) separation of the two
C             objects TARG1 and TARG2 as observed from OBS.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) SPICE(BADRADIUS) signals if either R1 or R2 have a negative
C        value.
C
C     2) If the ephemeris data required to perform the needed state
C        look-ups are not loaded, routines called by this routine
C        will signal the error SPICE(SPKINSUFFDATA).
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine determines the apparent separation between the limbs
C     of two objects as observed from a third.  The value reported is
C     corrected for light time. Moreover, if at the time this routine
C     is called, stellar aberration corrections are enabled, this
C     correction will also be applied to the apparent positions of the
C     centers of the two objects.
C
C$ Examples
C
C     None.
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
C     W.L. Taber     (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 28-JUN-2012 (EDW)
C
C        Minor edits to header, correcting spelling error and improving
C        syntax of Exceptions.
C
C        Corrected a typo in SETMSG error message where TARG1 was quoted
C        twice instead of quoting TARG1 and TARG2. This edit does change
C        the error message output.
C
C-    SPICELIB Version 1.0.0, 03-MAR-2009 (EDW)
C
C-&

C$ Index_Entries
C
C     compute the apparent relative angular separation
C
C-&

C
C     SPICELIB functions.
C
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP
      LOGICAL               FAILED
      LOGICAL               RETURN


C
C     Local Variables.
C
      DOUBLE PRECISION      ANG1
      DOUBLE PRECISION      ANG2
      DOUBLE PRECISION      PV1    ( 3 )
      DOUBLE PRECISION      PV2    ( 3 )
      DOUBLE PRECISION      RANGE1
      DOUBLE PRECISION      RANGE2
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      LT

C
C     ATOL is a tolerance value for computing arc sine.
C

      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL  = 1.D-12 )


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN( 'ZZGFSPQ' )

C
C     First check for bad inputs.
C
      IF ( R1 .LT. 0.D0 .OR. R2 .LT. 0.D0 ) THEN

         CALL SETMSG  ('A negative radius for a body was '
     .   //            'encountered. The radius for body # was given '
     .   //            'as #, the radius of body # was given as #. ')

         CALL ERRINT  ('#', TARG1         )
         CALL ERRDP   ('#', R1            )
         CALL ERRINT  ('#', TARG2         )
         CALL ERRDP   ('#', R2            )
         CALL SIGERR  ('SPICE(BADRADIUS)' )
         CALL CHKOUT  ('ZZGFSPQ'          )
         RETURN
      END IF


C
C     Get the state of the TARG1, TARG2 objects relative to OBS.
C
      CALL SPKEZP ( TARG1, ET, REF, ABCORR,  OBS, PV1, LT )

      IF  (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFSPQ' )
         RETURN
      END IF


       CALL SPKEZP ( TARG2, ET, REF, ABCORR, OBS, PV2, LT )

      IF (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFSPQ' )
         RETURN
      END IF

C
C     Compute the range to the objects of interest.
C
      RANGE1 = VNORM( PV1 )
      RANGE2 = VNORM( PV2 )

C
C     Compute the apparent angular radii as seen from OBS.
C
      IF ( RANGE1 .GT. R1 ) THEN

         ANG1 = DASINE( R1/RANGE1, ATOL )

         IF (  FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFSPQ' )
            RETURN
         END IF

      ELSE

         ANG1 = HALFPI ()

      END IF

      IF ( RANGE2 .GT. R2 ) THEN

         ANG2 = DASINE ( R2/RANGE2, ATOL )

         IF (  FAILED() ) THEN
            CALL CHKOUT ('ZZGFSPQ')
            RETURN
         END IF

      ELSE

         ANG2 = HALFPI()

      END IF

C
C     Finally compute the apparent separation.
C
      THETA = VSEP ( PV1, PV2 )
      VALUE = THETA - ANG1 - ANG2

      CALL CHKOUT ('ZZGFSPQ')
      RETURN
      END
