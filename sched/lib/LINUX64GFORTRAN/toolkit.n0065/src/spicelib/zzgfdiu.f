C$Procedure ZZGFDIU ( Private --- GF, distance utilities )
 
      SUBROUTINE ZZGFDIU ( TARGET, ABCORR, OBSRVR, 
     .                     UDFUNC, ET,     DECRES, DIST )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is the umbrella routine for the entry points used by
C     GFEVNT in order to find distance events.
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
C     GF
C     NAIF_IDS
C     SPK
C     TIME
C
C$ Keywords
C
C     DISTANCE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      ET
      LOGICAL               DECRES
      DOUBLE PRECISION      DIST

C$ Brief_I/O
C
C     VARIABLE  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     TARGID     I   ZZGFDIIN
C     ABCORR     I   ZZGFDIIN
C     OBSID      I   ZZGFDIIN
C     ET         I   ZZGFDIDC, ZZGFDIGQ
C     REF        I   ZZGFDIIN
C     UDFUNC     I   ZZGFDIDC
C     DECRES     O   ZZGFDIDC
C     DIST       O   ZZGFDIGQ
C
C$ Detailed_Input
C
C     See individual entry points.
C
C$ Detailed_Output
C
C     See individual entry points.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     See individual entry points.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target and observer, for the
C          times at which state or positions are computed, must be
C          loaded. If aberration corrections are used, the states of
C          target and observer relative to the solar system barycenter
C          must be calculable from the available ephemeris data.
C          Typically ephemeris data are made available by loading one
C          or more SPK files via FURNSH.
C
C        - If non-inertial reference frames are used, then PCK
C          files, frame kernels, C-kernels, and SCLK kernels may be
C          needed.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This is an umbrella for routines required by the GF scalar 
C     quantity search algorithm to support searches involving
C     distance constraints. 
C
C     The entry points of this routine are:
C
C        ZZGFDIIN   Saves the user-supplied inputs defining the
C                   distance computation to be performed. Initializes
C                   the distance search.  
C
C        ZZGFDIDC   Determines whether or not distance is decreasing
C                   at a specified epoch.
C
C        ZZGFDIGQ   Returns the distance between the observer and target
C                   at a specified epoch.
C
C$ Examples
C
C     See GFEVNT.
C
C$ Restrictions
C
C     This is a SPICELIB private routine; it should not be called by
C     user applications.
C
C     ZZGFDIIN must be called prior to use of any of the other
C     entry points.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB version 2.0.0 18-FEB-2011 (EDW)
C
C        Code edits to implement use of ZZGFRELX.
C        These edits include removal of unneeded routines:
C
C           ZZGFDIUR
C           ZZGFDILT
C
C        and corresponding unused variables.
C
C        Update to header entries.
C
C-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW) 
C
C-&

C$ Index_Entries
C
C     umbrella routine for finding distance events
C
C-&      

      EXTERNAL              UDFUNC

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C

C
C     Local Variables
C
      CHARACTER*(CORLEN)    SVCORR

      DOUBLE PRECISION      LT
      DOUBLE PRECISION      STATE  ( 6 )

      INTEGER               SVOBS
      INTEGER               SVTARG

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FOUND

C
C     Saved Variables
C
      SAVE                  SVCORR
      SAVE                  SVOBS
      SAVE                  SVTARG

      
C
C     This routine should never be called directly.
C
      CALL CHKIN  ( 'ZZGFDIU'           )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZGFDIU'           )
      RETURN




C$Procedure  ZZGFDIIN ( Private --- GF, distance initialization )
 
      ENTRY ZZGFDIIN ( TARGET, ABCORR, OBSRVR )
 
C$ Abstract
C
C     Initialize the GF distance constraint search utilities.
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
C     GF
C     NAIF_IDS
C     SPK
C     TIME
C
C$ Keywords
C
C     DISTANCE
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
C     TARGET     I   Target body name.
C     ABCORR     I   Aberration correction specifier.
C     OBSRVR     I   Observer name.
C
C$ Detailed_Input
C
C     TARGET     is the name of a target body. Optionally, you may
C                supply the integer ID code for the object as
C                an integer string. For example both 'MOON' and
C                '301' are legitimate strings that indicate the 
C                moon is the target body.
C
C                The target and observer define a position vector
C                which points from the observer to the target.
C
C                Case and leading or trailing blanks are not
C                significant in the string TARGET.
C
C
C     ABCORR     indicates the aberration corrections to be applied
C                when computing the target's position and orientation.
C                Any value accepted by SPKEZR may be used.
C                
C                See the header of the SPICE routine SPKEZR for a
C                detailed description of the aberration correction
C                options. 
C
C                Case and embedded blanks are not significant in
C                ABCORR.
C
C
C     OBSRVR     is the name of the body from which the occultation is
C                observed. Optionally, you may supply the integer NAIF
C                ID code for the body as a string.
C
C                Case and leading or trailing blanks are not
C                significant in the string OBSRVR.
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
C     1)  If name of the target or the observer cannot be translated
C         to a NAIF ID code, the error SPICE(IDCODENOTFOUND) is
C         signaled.
C
C     2)  If  target body coincides with the observer body OBSRVR, the
C         error SPICE(BODIESNOTDISTINCT) will be signaled.
C
C     3)  If the aberration correction string is invalid, the error
C         will be diagnosed by a routine in the call tree of this
C         routine.                 
C
C$ Files
C
C     See the header of the umbrella routine ZZGFDIU.
C
C$ Particulars
C
C     This routine must be called once before each GF search for
C     distance events.
C
C$ Examples
C
C     See GFEVNT.
C
C$ Restrictions
C
C     This is a SPICELIB private routine; it should not be called by
C     user applications.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB version 2.0.0 18-FEB-2011 (EDW)
C
C        REFVAL removed from routine argument list due to use
C        of ZZGFRELX to calculate the events.
C
C-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW) 
C
C-&

C$ Index_Entries
C
C     distance initialization routine
C
C-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFDIIN' )

C
C     Find NAIF IDs for TARGET and OBSRVR.
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
         CALL CHKOUT ( 'ZZGFDIIN'                                 )
         RETURN

      END IF
 

      CALL BODS2C ( OBSRVR, SVOBS, FOUND )

      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFDIIN'                                 )
         RETURN

      END IF

C
C     Make sure the observer and target are distinct.
C
      IF ( SVTARG .EQ. SVOBS ) THEN

         CALL SETMSG ( 'The observer and target must be '
     .   //            'distinct objects, but are not: '
     .   //            'OBSRVR = #; TARGET = #.'           )
         CALL ERRCH  ( '#', OBSRVR                         )
         CALL ERRCH  ( '#', TARGET                         )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'          )
         CALL CHKOUT ( 'ZZGFDIIN'                          )
         RETURN

      END IF

C
C     Squeeze all blanks out of the aberration correction
C     string; ensure the string is in upper case.
C     
      CALL CMPRSS ( ' ', 0, ABCORR, SVCORR )
      CALL UCASE  ( SVCORR,         SVCORR )

C
C     Check the aberration correction. If SPKEZR can't handle it,
C     neither can we.
C     
      CALL ZZVALCOR ( SVCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFDIIN' )
         RETURN
      END IF

      CALL CHKOUT ( 'ZZGFDIIN' )
      RETURN




C$Procedure ZZGFDIDC ( Private --- GF, is distance decreasing? )
 
      ENTRY ZZGFDIDC ( UDFUNC, ET, DECRES )
 
C$ Abstract
C
C     Indicate whether the observer-target distance is decreasing at a
C     specified time.
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
C     GF
C     NAIF_IDS
C     SPK
C     TIME
C
C$ Keywords
C
C     DISTANCE
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
C     DECRES     O   Flag indicating whether distance is decreasing.
C
C$ Detailed_Input
C
C     ET         is the time, expressed as seconds past J2000 TDB, at
C                which to determine whether or not the distance between
C                the observer and target is decreasing.
C
C$ Detailed_Output
C
C     DECRES     is a logical flag that indicates whether the
C                observer-target distance is decreasing at ET. The
C                observer, target, and aberration correction used to
C                compute the distance are defined by the latest call to
C                the initialization entry point ZZGFDIIN.
C
C                DECRES is .TRUE. if and only if the observer-target
C                distance is decreasing at ET.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the state of the target relative to the observer
C        at ET can not be found due to an SPK lookup failure,
C        the error will be diagnosed by routines in the call
C        tree of this routine.
C
C$ Files
C
C     See the header of the umbrella routine ZZGFDIU.
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
C     See GFREL.
C
C$ Restrictions
C
C     This is a SPICELIB private routine; it should not be called by
C     user applications.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB version 2.0.0 18-FEB-2011 (EDW)
C
C        Added UDFUNC to argument list for use of ZZGFRELX when
C        calculating the events.
C
C-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW) 
C
C-&

C$ Index_Entries
C
C     indicate whether distance is decreasing
C
C-&


C
C     Standard SPICE error handling.
C 
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFDIDC' ) 

      CALL SPKEZ ( SVTARG, ET, 'J2000', SVCORR, SVOBS, STATE, LT )

      IF (  FAILED() ) THEN
          CALL CHKOUT ( 'ZZGFDIDC' )
          RETURN
      END IF 

C
C     The observer-target distance is decreasing if and only
C     if the dot product of the velocity and position is
C     negative.
C 
      DECRES  =  VDOT( STATE, STATE(4) )  .LT.  0.D0

      CALL CHKOUT  ( 'ZZGFDIDC' )
      RETURN




C$Procedure ZZGFDIGQ ( Private --- GF, get observer-target distance )
 
      ENTRY ZZGFDIGQ ( ET, DIST )
 
C$ Abstract
C
C     Return the distance between the target and observer
C     at a specified epoch.
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
C     GF
C     NAIF_IDS
C     SPK
C     TIME
C
C$ Keywords
C
C     DISTANCE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
C
C      DOUBLE PRECISION      ET
C      DOUBLE PRECISION      DIST
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB.
C     DIST       O   Distance at time ET.
C
C$ Detailed_Input
C
C     ET         is the time, expressed as seconds past J2000 TDB, at
C                which the distance between the observer and target is
C                to be computed.
C
C$ Detailed_Output
C
C     DIST       is the distance between the observer and target as
C                seen by the observer at time ET. The observer, target,
C                and aberration correction used to compute the distance
C                are defined by the latest call to the initialization
C                entry point ZZGFDIIN.
C
C                Units are km.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the position of the target relative to the observer
C        at ET can not be found due to an SPK lookup failure,
C        the error will be diagnosed by routines in the call
C        tree of this routine.
C
C$ Files
C
C     See the header of the umbrella routine ZZGFDIU.
C
C$ Particulars
C
C     This routine determines the apparent distance between the target
C     and observer as seen from the observer at time ET. This
C     functionality supports GFREL's comparisons of relative extrema in
C     order to determine absolute extrema.
C
C$ Examples
C
C     See GFREL.
C
C$ Restrictions
C
C     This is a SPICELIB private routine; it should not be called by
C     user applications.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW) 
C    
C-&

C$ Index_Entries
C
C     return distance between two bodies
C
C-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFDIGQ' )

      CALL ZZGFDIQ ( SVTARG, ET, SVCORR, SVOBS, DIST )

      CALL CHKOUT ( 'ZZGFDIGQ' )
      RETURN

      END


