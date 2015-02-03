C$Procedure      ZZGFDIQ ( GF, return distance between objects )
 
      SUBROUTINE ZZGFDIQ ( TARGID, ET, ABCORR, OBSID, DIST )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return the distance between two ephemeris objects, optionally
C     corrected for light time and stellar aberration.
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

      INTEGER               TARGID
      DOUBLE PRECISION      ET
      CHARACTER*(*)         ABCORR
      INTEGER               OBSID
      DOUBLE PRECISION      DIST

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TARGID     I   Target body.
C     ET         I   Observer epoch.
C     ABCORR     I   Aberration correction flag.
C     OBSID      I   Observing body.
C     DIST       O   Distance between target and observer.
C
C$ Detailed_Input
C
C     TARGID      is the NAIF ID code for a target body. The target and
C                 observer define a position vector that points from
C                 the observer to the target.
C
C     ET          is the ephemeris time, expressed as seconds past
C                 J2000 TDB, at which the position of the target body
C                 relative to the observer is to be computed. ET refers
C                 to time at the observer's location.
C
C     ABCORR      indicates the aberration corrections to be applied to
C                 the position of the target body to account for
C                 one-way light time and stellar aberration. Any
C                 aberration correction accepted by SPKEZR may be used.
C
C$ Detailed_Output
C
C     DIST        is the norm (magnitude) of the specified Cartesian
C                 3-vector representing the position of the target body
C                 relative to the specified observer, where the
C                 position is corrected for the specified aberrations.
C                 The position vector points from the observer's
C                 location at ET to the aberration-corrected location
C                 of the target.
C
C                 Units are km.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If an error occurs while reading an SPK or other kernel file,
C        the error  will be diagnosed by a routine in the call tree 
C        of this routine.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target and observer for the
C          input epoch must be loaded. If aberration corrections are
C          used, the states of target and observer relative to the
C          solar system barycenter must be calculable from the
C          available ephemeris data. Typically ephemeris data are made
C          available by loading one or more SPK files via FURNSH.
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
C     This routine centralizes distance computations performed by
C     entry points in the GF distance utility package ZZGFDIU.
C
C$ Examples
C
C     See the entry point ZZGFDIGQ in ZZGFDIU.
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
C     compute the apparent distance between two objects
C
C-&       

 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORM

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Variables
C
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      POS   ( 3 )
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFDIQ' ) 

C
C     Get the position of the target relative to the observer.
C
      CALL SPKEZP ( TARGID, ET, 'J2000', ABCORR, OBSID, POS, LT )

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFDIQ' )
         RETURN
      END IF 


      DIST = VNORM ( POS )
 
      CALL CHKOUT  ( 'ZZGFDIQ' ) 
      RETURN
      END
