C$Procedure ZZGFPAQ ( Private --- GF, phase angle between bodies )

      SUBROUTINE ZZGFPAQ ( ET, TARG, ILLMN, OBS, ABCORR, VALUE )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Compute the apparent phase angle for a target, observer,
C     illuminator set of ephemeris objects.
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
C     PHASE ANGLE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      INTEGER               TARG
      INTEGER               ILLMN
      INTEGER               OBS
      CHARACTER*(*)         ABCORR
      DOUBLE PRECISION      VALUE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB.
C     TARG       I   Target body ID.
C     ILLMN      I   Illuminating body ID.
C     OBS        I   Observer body ID.
C     ABCORR     I   Aberration correction flag.
C     VALUE      O   Value of phase angle.
C
C$ Detailed_Input
C
C     ET       the time in ephemeris seconds past J2000 TDB at which
C              to compute the phase angle.
C
C     TARG     the SPICE integer ID for the target body.
C
C     ILLMN    the SPICE integer ID for the illuminating body.
C
C     OBS      the SPICE integer ID for the observer.
C
C     ABCORR   the string description of the aberration corrections to
C              apply to the state evaluations to account for one-way
C              light time and stellar aberration.
C
C              Any aberration correction accepted by the SPICE
C              routine SPKEZR is accepted here. See the header
C              of SPKEZR for a detailed description of the
C              aberration correction options. For convenience,
C              the options are listed below:
C
C                 'NONE'     Apply no correction. Returns the "true"
C                            geometric state.
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
C              The ABCORR string lacks sensitivity to case, leading
C              and trailing blanks.
C
C$ Detailed_Output
C
C     VALUE      is the optionally light-time corrected phase angle
C                between TARG and ILLMN as observed from OBS.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine calculates the phase angle using the location of the
C     bodies (if point objects) or the center of the bodies (if finite
C     bodies).
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
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 23-JUN-2010 (EDW)
C
C-&

C$ Index_Entries
C
C   compute the phase of two objects wrt an illumination source
C
C-&

C
C     SPICELIB functions.
C

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               EQSTR

      DOUBLE PRECISION      VSEP
      DOUBLE PRECISION      PI

C
C     Local Variables.
C
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      PV1    ( 3 )
      DOUBLE PRECISION      PV2    ( 3 )

      CHARACTER*(5)         REF

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFPAQ' )

C
C     This calculation is invariant with respect to reference frame.
C     Use J2000 for convenience.
C
      REF = 'J2000'

C
C     Get the position of the TARG object relative to OBS at ET.
C
      CALL SPKEZP ( TARG, ET, REF, ABCORR,  OBS, PV1, LT )

      IF  (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFPAQ' )
         RETURN
      END IF


C
C     Get the state of the ILLMN object relative to TARG at ET
C     for no aberration correction, or ET - LT otherwise.
C
      IF ( EQSTR( ABCORR, 'NONE' ) ) THEN

         CALL SPKEZP ( ILLMN, ET,      REF, ABCORR, TARG, PV2, LT )

      ELSE

         CALL SPKEZP ( ILLMN, ET - LT, REF, ABCORR, TARG, PV2, LT )

      END IF

      IF (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFPAQ' )
         RETURN
      END IF

C
C                       ILLMN      OBS
C       ILLMN as seen      ^       /
C       from TARG at       |      /
C       ET - LT.           |     /
C                         >|..../< phase angle
C                          |   /
C                        . |  /
C                      .   | /
C                     .     v     TARG as seen from OBS
C               SEP   .   TARG    at ET
C                      .  /
C                        /
C                       v
C
C        PI = SEP + PHASE
C
C        so
C
C        PHASE = PI - SEP
C
C     Calculate the angle separating the vectors relative to TARG
C
      SEP = VSEP( PV1, PV2 )

C
C     The angle of interest is that between -PV1 and PV2 measured from
C     TARG. Subtract SEP from PI to calculate this angle.
C
      VALUE = PI() - SEP

C
C     All done.
C
      CALL CHKOUT (  'ZZGFPAQ' )
      RETURN

      END

