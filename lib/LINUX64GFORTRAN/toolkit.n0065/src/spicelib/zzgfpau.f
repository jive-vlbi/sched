C$Procedure ZZGFPAU ( Private --- GF, phase angle utility routine )

      SUBROUTINE ZZGFPAU ( TARGET, ILLMN,  ABCORR, OBSRVR,
     .                     UDFUNC, ET,     DECRES, RVL,
     .                     XTARG,  XILLMN, XABCOR, XOBS, XABLK )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is the umbrella routine for the three entry points needed by
C     GFEVNT in order to find body centered phase angle events.
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
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLMN
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      ET
      LOGICAL               DECRES
      DOUBLE PRECISION      RVL
      INTEGER               XTARG
      INTEGER               XILLMN
      CHARACTER*(*)         XABCOR
      INTEGER               XOBS
      LOGICAL               XABLK  ( NABCOR )


C$ Brief_I/O
C
C     VARIABLE  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     TARGET    I-O  ZZGFPAIN, ZZGFPAX
C     ILLMN     I-O  ZZGFPAIN, ZZGFPAX
C     ABCORR    I-O  ZZGFPAIN, ZZGFPAX
C     OBSRVR    I-O  ZZGFPAIN, ZZGFPAX
C     ET         I   ZZGFPADC, ZZGFPAGQ
C     UDFUNC     I   ZZGFPADC
C     DECRES     O   ZZGFPADC
C     RVL        O   ZZGFPAGQ
C     XTARG      O   ZZGFPAX
C     XILLMN     O   ZZGFPAX
C     XABCOR     O   ZZGFPAX
C     XOBS       O   ZZGFPAX
C     XABLK      O   ZZGFPAX
C
C$ Detailed_Input
C
C     TARGET   the string name of a target body.  Optionally, you may
C              supply the integer ID code for the object as an
C              integer string.  For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C              The target and observer define a position vector
C              that points from the observer to the target.
C
C     ILLMN    the string name of the illuminating body. This will
C              normally be 'SUN' but the algorithm can use any
C              ephemeris object
C
C     ABCORR   the string description of the aberration corrections to
C              apply to the state evaluations to account for one-way
C              light time and stellar aberration.
C
C              Any signal receive aberration correction accepted by
C              the SPICE routine SPKEZR is accepted here. See the header
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
C     OBSRVR   the string name of an observing body.  Optionally, you
C              may supply the ID code of the object as an integer
C              string. For example, both 'EARTH' and '399' are
C              legitimate strings to indicate the observer as Earth.
C
C     ET       the time in TDB seconds past J2000 at which to calculate
C              the value of or characteristic of the phase angle
C              between SVTARG and SVILLMN as seen from SVOBS.
C
C     For more information, see individual entry points.
C
C$ Detailed_Output
C
C     DECRES   is .TRUE. if the phase angle is decreasing at ET.
C              Otherwise it is .FALSE..
C
C     RVL      the phase angle between SVTARG and SVILLMN as seen
C              from SVOBS at time ET.
C
C     XTARG    SPICE ID value for the target body initialized
C              via ZZGFPAIN.
C
C     XILLMN   SPICE ID value for the illuminator body initialized
C              via ZZGFPAIN.
C
C     XABCOR   String value for the aberration correction initialized
C              via ZZGFPAIN.
C
C     XOBS     SPICE ID value for the observing body initialized
C              via ZZGFPAIN.
C
C     XABLK    Aberration correction attribute block as initialized
C              by ABCORR.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) SPICE(BOGUSENTRY) signals if a direct call to ZZGFPAU occurs.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine serves as the umbrella routine for three entry points
C     needed by GFEVNT in solving for phase angle conditions.
C
C     The three entry points are
C
C        ZZGFPAIN --- an initialization routine that must be called
C                     prior to attempting to solve for any range
C                     rate event.
C
C        ZZGFPADC --- determines whether or not phase angle is
C                     decreasing at some time.
C
C        ZZGFPAGQ --- returns the phase angle of the two objects
C                     of concern as a function of ET.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     ZZGFPAIN must be called prior to use of any of the other
C     entry points (think constructor).
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     E.D. Wright    (JPL)
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 16-SEP-2012 (EDW) (NJB)
C
C-&

C$ Index_Entries
C
C     find phase angle events
C
C-&

      EXTERNAL              UDFUNC

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      DVSEP
      DOUBLE PRECISION      VDOT


C
C     Local parameters
C

C
C     The phase angle calculation is invariant with respect to
C     reference frame. Use J2000 for convenience.
C
      CHARACTER*(*)         REF
      PARAMETER           ( REF    = 'J2000' )


C
C     Local Variables
C
      CHARACTER*(CORLEN)    SVABCO

      DOUBLE PRECISION      DSEP
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      S1   (6)
      DOUBLE PRECISION      S2   (6)
      DOUBLE PRECISION      UVEC (3)

      INTEGER               SVILLM
      INTEGER               SVOBS
      INTEGER               SVTARG
      INTEGER               I

      LOGICAL               SVABLK ( NABCOR )
      LOGICAL               FOUND


C
C     Saved Variables
C
      SAVE                  SVABLK
      SAVE                  SVABCO
      SAVE                  SVTARG
      SAVE                  SVOBS
      SAVE                  SVILLM

      CALL CHKIN  ( 'ZZGFPAU'           )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZGFPAU'           )

      RETURN



C$Procedure  ZZGFPAIN ( Private --- GF, phase angle initialization )

      ENTRY ZZGFPAIN ( TARGET, ILLMN, ABCORR, OBSRVR )

C$ Abstract
C
C     This is the initialization entry point used for describing
C     the event that is to be solved for by ZZGFSOLV.
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
C
C      CHARACTER*(*)         TARGET
C      CHARACTER*(*)         ABCORR
C      CHARACTER*(*)         OBSRVR
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TARGET     I   Name of the target body.
C     ILLMN      I   Name of the illuminating body.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name of the observing body.
C
C$ Detailed_Input
C
C     TARGET   the string name of a target body.  Optionally, you may
C              supply the integer ID code for the object as an
C              integer string.  For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C              Case and leading or trailing blanks are not significant
C              in the string TARGET.
C
C     ILLMN    the string name of the illuminating body. This will
C              normally be 'SUN' but the algorithm can use any
C              ephemeris object
C
C              Case and leading or trailing blanks are not significant
C              in the string ILLMN.
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
C                'CN+S'     "Reception" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C              Note that this routine accepts only reception mode
C              aberration corrections.
C
C              The ABCORR string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     OBSRVR   the string name of an observing body.  Optionally, you
C              may supply the ID code of the object as an integer
C              string. For example, both 'EARTH' and '399' are
C              legitimate strings to indicate the observer as Earth.
C
C              Case and leading or trailing blanks are not significant
C              in the string OBSRVR.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) SPICE(IDCODENOTFOUND) signals if the object name for the
C        target, TARGET, is not a recognized name.
C
C     2) SPICE(IDCODENOTFOUND) signals if the object name for the
C        illuminator, ILLMN, is not a recognized name.
C
C     3) SPICE(IDCODENOTFOUND) signals if the object name for the
C        observer, OBSRVR, is not a recognized name.
C
C     4) SPICE(BODIESNOTDISTINCT) signals if the three objects
C        associated with a PHASE ANGLE search are not distinct.
C
C     5) SPICE(INVALIDOPTION) signals for any transmit mode aberration
C        correction.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     N.J. Bachman   (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 19-OCT-2010 (EDW)(NJB)
C
C-&

C$ Index_Entries
C
C     phase angle initialization routine
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFPAIN' )

C
C     Find NAIF IDs for TARGET, ILLMN, and OBSRVR.
C
      CALL BODS2C ( TARGET, SVTARG, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The target object, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFPAIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( ILLMN, SVILLM, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The illuminator object, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFPAIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( OBSRVR, SVOBS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The observer object, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFPAIN'                                 )
         RETURN

      END IF


C
C     Make sure the observer, illuminator, and target are distinct.
C
      IF ( (SVTARG .EQ. SVOBS)   .OR.
     .     (SVTARG .EQ. SVILLM) .OR.
     .     (SVOBS  .EQ. SVILLM)      ) THEN

         CALL SETMSG ( 'The observer, illuminator, and '
     .   //            'target must be distinct objects, but '
     .   //            'are not: OBSRVR = #, TARGET = #, '
     .   //            'are not: ILLMN= #.' )
         CALL ERRCH  ( '#', OBSRVR                         )
         CALL ERRCH  ( '#', TARGET                         )
         CALL ERRCH  ( '#', ILLMN                          )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'          )
         CALL CHKOUT ( 'ZZGFPAIN'                          )
         RETURN

      END IF

C
C     Squeeze all blanks out of the aberration correction
C     string; ensure the string is in upper case.
C
      CALL CMPRSS ( ' ', 0, ABCORR, SVABCO )
      CALL UCASE  ( SVABCO,         SVABCO )

C
C     Check the aberration correction. If SPKEZR can't handle it,
C     neither can we.
C
      CALL ZZVALCOR ( SVABCO, SVABLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFPAIN' )
         RETURN
      END IF

C
C     Restrict correction to reception cases.
C

      IF( SVABLK(XMTIDX) ) THEN

         CALL SETMSG ( 'Invalid aberration correction ''#''. '
     .   //            'Phase angle geometry calculations currently '
     .   //            'restricted to reception cases.')
         CALL ERRCH  ( '#', ABCORR )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)' )
         CALL CHKOUT ( 'ZZGFPAIN' )
         RETURN

      END IF


      CALL CHKOUT ( 'ZZGFPAIN' )
      RETURN




C$Procedure ZZGFPADC ( Private --- GF, when phase angle is decreasing )

      ENTRY ZZGFPADC ( UDFUNC, ET, DECRES )

C$ Abstract
C
C     Computes whether or not the phase angle between the observer
C     and the target is decreasing at time ET.
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
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C     LOGICAL               DECRES
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB.
C     DECRES     O   .TRUE. if phase angle is decreasing, .FALSE.
C                    otherwise.
C
C$ Detailed_Input
C
C     ET         the time in ephemeris seconds past J2000 TDB at which
C                to determine whether the phase angle between SVTARG
C                and SVILLMN as seen from SVOBS is decreasing.
C
C$ Detailed_Output
C
C     DECRES     is .TRUE. if the phase angle is decreasing at ET.
C                Otherwise it is .FALSE..
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
C     A function f(x) is strictly decreasing at x0 if and only if there
C     exists some delta > 0 such that for all dx satisfying
C
C        0  <  dx  < delta
C
C     we have
C
C        f(x0)       <  f(x0 + dx)
C
C     and
C
C        f(x0 - dx)  <  f(x)
C
C     Note that a strictly decreasing function need not be
C     differentiable in a neighborhood of x0; it can have jump
C     discontinuities in any neighborhood of x0 and even at x0.
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
C     N.J. Bachman   (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 10-JAN-2012 (EDW)
C
C-&

C$ Index_Entries
C
C     when phase angle is decreasing
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFPADC' )


C
C     Get the state of the TARG object relative to OBS at ET.
C
      CALL SPKEZ ( SVTARG, ET, REF, SVABCO, SVOBS, S1, LT )

      IF  (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFPADC' )
         RETURN
      END IF


C
C     Get the state of the ILLMN object relative to TARG at ET
C     for no aberration correction, or ET - LT otherwise.
C
      IF ( SVABLK(GEOIDX) ) THEN

C
C        No correction, geometric.
C
         CALL SPKEZ ( SVILLM, ET,      REF, SVABCO, SVTARG, S2, LT )

      ELSE

         CALL SPKEZ ( SVILLM, ET - LT, REF, SVABCO, SVTARG, S2, LT )

         IF (  FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFPADC' )
            RETURN
         END IF

C
C        Correct velocity for time derivative of the observer target
C        light-time. We need to do this since the SPK evaluation occurs
C        at ET - LT.
C
C        d( ET - LT ) = (1 - d LT )
C        ------------        ----
C        dt                  dt
C
C        LT = ||R||
C             -----
C               C
C                                     ^
C        d LT    = 1   d ||R||  = 1 < R, V >
C        ----      -   -------    -
C        dt        C   dt         C
C
C
         CALL VHAT ( S1, UVEC )

         DLT = VDOT ( UVEC, S1(4) ) / CLIGHT()

C
C        Apply the correction to the velocity vector components.
C
         CALL VSCLIP ( 1.D0 - DLT,  S2(4) )

      END IF

      IF (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFPADC' )
         RETURN
      END IF

C
C                       ILLMN      OBS
C       ILLMN as seen      *       /
C       from TARG at       |      /
C       ET - LT.           |     /
C                         >|..../< phase angle
C                          |   /
C                        . |  /
C                      .   | /
C                     .     *     TARG as seen from OBS
C               SEP   .   TARG    at ET
C                      .  /
C                        /
C                       *
C
C     Calculate the derivative of the angle separating the vectors
C     relative to TARG.
C
C        PI = SEP + PHASE
C
C        dPHASE     dSEP
C        ------ = - ----
C         dt         dt
C
      DSEP   = DVSEP( S1, S2 )

      DECRES = -DSEP .LT. 0.D0

      CALL CHKOUT ( 'ZZGFPADC' )
      RETURN




C$Procedure ZZGFPAGQ ( Private --- GF, phase angle between two bodies )

      ENTRY ZZGFPAGQ ( ET, RVL )

C$ Abstract
C
C     Determine the phase angle between the centers of the two
C     bodies.
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
C
C      DOUBLE PRECISION      ET
C      DOUBLE PRECISION      RVL
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB.
C     RVL        O   Phase angle at time ET.
C
C$ Detailed_Input
C
C     ET         the time in ephemeris seconds past J2000 TDB at which
C                to compute the phase angle.
C
C$ Detailed_Output
C
C     RVL        the phase angle between SVTARG and SVILLMN as seen
C                from SVOBS at time ET.
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
C     None.
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
C     N.J. Bachman   (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 19-OCT-2010 (EDW)
C
C-&

C$ Index_Entries
C
C     get phase angle between two bodies
C
C-&

      CALL ZZGFPAQ ( ET, SVTARG, SVILLM, SVOBS, SVABCO, RVL )

      RETURN




C$Procedure ZZGFPAX ( Private -- GF, retrieve ZZGFPAIN values )

      ENTRY ZZGFPAX ( XTARG, XILLMN, XABCOR, XOBS, XABLK )

C$ Abstract
C
C     Retrieve values set in ZZGFPAIN.
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
C     None.
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     XTARG      O   Saved value for the target body.
C     XILLMN     O   Saved value for the illuminator body.
C     XABCOR     O   Saved value for the aberration correction.
C     XOBS       O   Saved value for the observing body.
C     XABLK      O   Saved value for the aberration correction
C                    attribute block.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     XTARG    SPICE ID value for the target body initialized
C              via ZZGFPAIN.
C
C     XILLMN   SPICE ID value for the illuminator body initialized
C              via ZZGFPAIN.
C
C     XABCOR   String value for the aberration correction initialized
C              via ZZGFPAIN.
C
C     XOBS     SPICE ID value for the observing body initialized
C              via ZZGFPAIN.
C
C     XABLK    Aberration correction attribute block as initialized
C              by ABCORR.
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
C     None.
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
C     N.J. Bachman   (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 16-SEP-2012 (EDW)
C
C-&

C$ Index_Entries
C
C     get saved phase angle parameters
C
C-&

         XTARG  = SVTARG
         XILLMN = SVILLM
         XABCOR = SVABCO
         XOBS   = SVOBS

         DO I=1, ABATSZ
            XABLK(I) = SVABLK(I)
         END DO

      RETURN
      END


