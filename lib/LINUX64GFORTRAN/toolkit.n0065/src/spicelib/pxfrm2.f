C$Procedure      PXFRM2 ( Position Transform Matrix, Different Epochs )
 
      SUBROUTINE PXFRM2 ( FROM, TO, ETFROM, ETTO, ROTATE  )

C$ Abstract
C
C     Return the 3x3 matrix that transforms position vectors from one
C     specified frame at a specified epoch to another specified
C     frame at another specified epoch.
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
C     FRAMES 
C
C$ Keywords
C
C     FRAMES
C     TRANSFORM
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         FROM
      CHARACTER*(*)         TO
      DOUBLE PRECISION      ETFROM
      DOUBLE PRECISION      ETTO
      DOUBLE PRECISION      ROTATE  ( 3, 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FROM       I   Name of the frame to transform from.
C     TO         I   Name of the frame to transform to.
C     ETFROM     I   Evaluation time of 'FROM' frame.
C     ETTO       I   Evaluation time of 'TO' frame.
C     ROTATE     O   A position transformation matrix from
C                    frame FROM to frame TO.
C
C$ Detailed_Input
C
C     FROM       is the name of a reference frame recognized by 
C                SPICELIB that corresponds to the input ETFROM.
C
C
C     TO         is the name of a reference frame recognized by
C                SPICELIB that corresponds to the desired output
C                at ETTO.
C
C
C     ETFROM     is the epoch in ephemeris seconds past the epoch
C                of J2000 (TDB) corresponding to the FROM reference 
C                frame.
C
C
C     ETTO       is the epoch in ephemeris seconds past the epoch
C                of J2000 (TDB) that corresponds to the TO reference
C                frame.
C
C
C$ Detailed_Output
C
C     ROTATE     is the transformation matrix that relates the reference
C                frame FROM at epoch ETFROM to the frame TO at epoch 
C                ETTO.
C
C                If (x, y, z) is a position relative to the reference
C                frame FROM at time ETFROM then the vector ( x', y',
C                z') is the same position relative to the frame TO at
C                epoch ETTO. Here the vector ( x', y', z' ) is defined
C                by the equation:
C
C                   -   -       -        -     -  -
C                  | x'  |     |          |   | x  |
C                  | y'  |  =  |  ROTATE  |   | y  |
C                  | z'  |     |          |   | z  |
C                   -   -       -        -     -  -
C
C$ Parameters
C
C     None.    
C
C
C$ Exceptions
C
C     1)  If sufficient information has not been supplied via loaded
C         SPICE kernels to compute the transformation between the
C         two frames, the error will be diagnosed by a routine
C         in the call tree to this routine.
C
C     2)  If either frame FROM or TO is not recognized the error
C         'SPICE(UNKNOWNFRAME)' will be signaled.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.  Kernels that may be required include
C     SPK files, PCK files, frame kernels, C-kernels, and SCLK kernels.
C
C     Such kernel data are normally loaded once per program
C     run, NOT every time this routine is called. 
C
C$ Particulars
C
C     PXFRM2 is most commonly used to transform a position between
C     time-dependant reference frames.  
C
C     For more examples of where to use PXFRM2, please see:
C            
C           SINCPT
C           SURFPT
C           SUBSLR
C           ILUMIN
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C     1) Suppose that MGS has taken a picture of Mars at time ETREC with
C        the MOC narrow angle camera. We want to know the latitude and
C        longitude associated with two pixels projected to Mars'
C        surface: the boresight and one along the boundary of the
C        field of view (FOV). Due to light time, the photons taken in
C        the picture left Mars at time ETEMIT, when Mars was at a
C        different state than at time ETREC.
C
C        In order to solve this problem, we could use the SINCPT
C        routine for both pixels, but this would be slow.  Instead, we
C        will assume that the light time for each pixel is the same. We
C        will call SINCPT once to get the light time and surface point
C        associated with the boresight. Then, we will rotate one of the
C        FOV boundary vectors from the camera frame at ETREC to the
C        body-fixed Mars frame at ETEMIT, and call the faster routine
C        SURFPT to retrieve the surface point for one of the FOV
C        boundary vectors.
C
C        This example problem could be extended to find the latitude
C        and longitude associated with every pixel in an instrument's
C        field of view, but this example is simplified to only solve
C        for two pixels:  the boresight and one along the boundary of
C        the field of view.
C
C        Assumptions:
C
C           1)  The light times from the surface points in the camera's
C               field of view to the camera are equal.
C
C           2)  The camera offset from the center of gravity of the 
C               spacecraft is zero. If the data are more accurate
C               and precise, this assumption can be easily discarded.
C
C           3)  An ellipsoid shape model for the target body is 
C               sufficient.
C
C           4)  The boundary field of view vector returned from GETFOV
C               is associated with a boundary field of view pixel. If
C               this example were extended to include a geometric camera
C               model, this assumption would not be needed since the
C               direction vectors associated with each pixel would be
C               calculated from the geometric camera model.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C           KPL/MK
C
C           File name: mgs_ex.tm
C           
C           This is the meta-kernel file for the example problem for
C           the subroutine PXFRM2. These kernel files can be found in
C           the NAIF archives.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C           
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              de421.bsp                     Planetary ephemeris
C              pck00009.tpc                  Planet orientation and
C                                            radii
C              naif0009.tls                  Leapseconds
C              mgs_ext12_ipng_mgs95j.bsp     MGS ephemeris
C              mgs_moc_v20.ti                MGS MOC instrument
C                                            parameters
C              mgs_sclkscet_00061.tsc        MGS SCLK coefficients
C              mgs_sc_ext12.bc               MGS s/c bus attitude
C                   
C           \begindata
C
C           KERNELS_TO_LOAD = ( 'de421.bsp',
C                               'pck00009.tpc',
C                               'naif0009.tls',
C                               'mgs_ext12_ipng_mgs95j.bsp',
C                               'mgs_moc_v20.ti',
C                               'mgs_sclkscet_00061.tsc',
C                               'mgs_sc_ext12.bc' )
C
C           \begintext
C
C           End of meta-kernel.
C
C
C       Example code begins here.
C
C           PROGRAM EX_PXFRM2
C           IMPLICIT NONE
C     C
C     C     SPICELIB functions
C     C
C     C     Degrees per radian
C     C
C           DOUBLE PRECISION      DPR
C     C
C     C     Distance between two vectors
C     C
C           DOUBLE PRECISION      VDIST
C     C
C     C     Local parameters
C     C
C     C     ABCORR is the desired light time and stellar
C     C     aberration correction setting.
C     C
C           CHARACTER*(*)         ABCORR
C           PARAMETER           ( ABCORR = 'CN+S' )
C     C
C     C     MGS_MOC_NA is the name of the camera that took
C     C     the picture being analyzed.
C     C
C           CHARACTER*(*)         CAMERA
C           PARAMETER           ( CAMERA = 'MGS_MOC_NA' )
C
C           CHARACTER*(*)         METAKR
C           PARAMETER           ( METAKR = 'mgs_ex.tm' )
C
C           INTEGER               FRNMLN
C           PARAMETER           ( FRNMLN = 32 )
C
C           INTEGER               NCORNR
C           PARAMETER           ( NCORNR = 4 )
C
C           INTEGER               SHPLEN
C           PARAMETER           ( SHPLEN = 80 )
C
C     C
C     C     Local variables
C     C
C     C     OBSREF is the observer reference frame on MGS.
C     C
C           CHARACTER*(FRNMLN)    OBSREF
C           CHARACTER*(SHPLEN)    SHAPE
C
C           DOUBLE PRECISION      BOUNDS ( 3, NCORNR )
C           DOUBLE PRECISION      BNDVEC ( 3 )
C           DOUBLE PRECISION      BSIGHT ( 3 )
C     C
C     C     ETEMIT is the time at which the photons were
C     C     emitted from Mars.  ETREC is the time at
C     C     which the picture was taken by MGS.
C     C
C           DOUBLE PRECISION      ETREC
C           DOUBLE PRECISION      ETEMIT
C           DOUBLE PRECISION      DIST
C     C
C     C     LAT and LON are the latitude and longitude
C     C     associated with one of the boundary FOV vectors.
C     C
C           DOUBLE PRECISION      LAT
C           DOUBLE PRECISION      LON
C     C
C     C     PMGSMR is the opposite of the apparent position of
C     C     Mars with respect to MGS.
C     C
C           DOUBLE PRECISION      PMGSMR ( 3 )
C     C
C     C     RADII is a vector of the semi-axes of Mars.
C     C
C           DOUBLE PRECISION      RADII  ( 3 )
C           DOUBLE PRECISION      RADIUS
C     C
C     C     ROTATE is a position transformation matrix from
C     C     the camera frame at ETREC to the IAU_MARS frame
C     C     at ETEMIT.
C     C
C           DOUBLE PRECISION      ROTATE ( 3, 3 )
C           DOUBLE PRECISION      SPOINT ( 3 )
C           DOUBLE PRECISION      SRFVEC ( 3 )
C           DOUBLE PRECISION      TMP    ( 3 )
C
C           INTEGER               CAMID
C           INTEGER               DIM
C           INTEGER               N
C
C           LOGICAL               FOUND
C     C
C     C     ------------------ Program Setup ------------------
C     C
C     C     Load kernel files via the meta-kernel.
C     C
C           CALL FURNSH ( METAKR )
C     C
C     C     Convert the time the picture was taken from a
C     C     UTC time string to seconds past J2000, TDB.
C     C
C           CALL STR2ET ( '2003 OCT 13 06:00:00 UTC', ETREC )
C     C
C     C     Assume the one-way light times from different
C     C     surface points on Mars to MGS within the camera's
C     C     FOV are equal. This means the photons that make
C     C     up different pixels were all emitted from Mars at
C     C     ETEMIT and received by the MGS MOC camera at ETREC. It
C     C     would be slow to process images using SINCPT for every
C     C     pixel. Instead, we will use SINCPT on the
C     C     boresight pixel and use SURFPT for one of the FOV
C     C     boundary pixels. If this example program were extended
C     C     to include all of the camera's pixels, SURFPT would
C     C     be used for the remaining pixels.
C     C
C     C     Get the MGS MOC Narrow angle camera (MGS_MOC_NA)
C     C     ID code. Then look up the field of view (FOV)
C     C     parameters by calling GETFOV.
C     C
C           CALL BODN2C ( CAMERA, CAMID, FOUND )
C
C           IF ( .NOT. FOUND ) THEN
C
C              CALL SETMSG ( 'Could not find ID code for ' //
C          .                 'instrument #.'               )
C              CALL ERRCH  ( '#', CAMERA                   )
C              CALL SIGERR ( 'SPICE(NOTRANSLATION)'        )
C
C           END IF
C     C
C     C     GETFOV will return the name of the camera-fixed frame
C     C     in the string OBSREF, the camera boresight vector in
C     C     the array BSIGHT, and the FOV corner vectors in the
C     C     array BOUNDS.
C     C
C           CALL GETFOV ( CAMID,  NCORNR, SHAPE, OBSREF,
C          .              BSIGHT, N,      BOUNDS       )
C
C           WRITE (*,*) 'Observation Reference frame:  ', OBSREF
C
C     C
C     C     ----------- Boresight Surface Intercept -----------
C     C
C     C     Retrieve the time, surface intercept point, and vector
C     C     from MGS to the boresight surface intercept point
C     C     in IAU_MARS coordinates.
C     C
C           CALL SINCPT ( 'ELLIPSOID', 'MARS',  ETREC, 'IAU_MARS',
C          .               ABCORR,     'MGS',   OBSREF, BSIGHT,
C          .               SPOINT,      ETEMIT, SRFVEC, FOUND  )
C
C           IF ( .NOT. FOUND ) THEN
C
C              CALL SETMSG ( 'Intercept not found for the ' //
C          .                 'boresight vector.'  )
C              CALL SIGERR ( 'SPICE(NOINTERCEPT)' )
C
C           END IF
C     C
C     C     Convert the intersection point of the boresight
C     C     vector and Mars from rectangular into latitudinal
C     C     coordinates. Convert radians to degrees.
C     C
C           CALL RECLAT ( SPOINT, RADIUS, LON, LAT )
C
C           LON = LON * DPR ()
C           LAT = LAT * DPR ()
C
C           WRITE (*,*) 'Boresight surface intercept ' //
C          .            'coordinates:'
C           WRITE (*,*) '   Radius    (km) :  ', RADIUS
C           WRITE (*,*) '   Latitude  (deg):  ', LAT
C           WRITE (*,*) '   Longitude (deg):  ', LON
C
C     C
C     C     --------- A Boundary FOV Surface Intercept (SURFPT) ------
C     C
C     C     Now we will transform one of the FOV corner vectors into the
C     C     IAU_MARS frame so the surface intercept point can be
C     C     calculated using SURFPT, which is faster than SUBPNT.
C     C
C     C     If this example program were extended to include all
C     C     of the pixels in the camera's FOV, a few steps, such as
C     C     finding the rotation matrix from the camera frame to the
C     C     IAU_MARS frame, looking up the radii values for Mars,
C     C     and finding the position of MGS with respect to Mars could
C     C     be done once and used for every pixel.
C     C
C     C     Find the rotation matrix from the ray's reference
C     C     frame at the time the photons were received (ETREC)
C     C     to IAU_MARS at the time the photons were emitted
C     C     (ETEMIT).
C     C
C           CALL PXFRM2 ( OBSREF, 'IAU_MARS', ETREC, ETEMIT, ROTATE )
C
C     C
C     C     Look up the radii values for Mars.
C     C
C           CALL BODVRD ( 'MARS', 'RADII', 3, DIM, RADII )
C
C     C
C     C     Find the position of the center of Mars with respect
C     C     to MGS.  The position of the observer with respect
C     C     to Mars is required for the call to SURFPT.  Note:
C     C     the apparent position of MGS with respect to Mars is
C     C     not the same as the negative of Mars with respect to MGS.
C     C
C           CALL VSUB   ( SPOINT, SRFVEC, PMGSMR )
C
C     C
C     C     The selected boundary FOV pixel must be rotated into the
C     C     IAU_MARS reference frame.
C     C
C           CALL MXV    ( ROTATE, BOUNDS(1,1), BNDVEC )
C
C     C
C     C     Calculate the surface point of the boundary FOV
C     C     vector.
C     C
C           CALL SURFPT ( PMGSMR,   BNDVEC, RADII(1), RADII(2),
C          .              RADII(3), SPOINT, FOUND )
C
C           IF ( .NOT. FOUND ) THEN
C
C              CALL SETMSG ( 'Could not calculate surface point.')
C              CALL SIGERR ( 'SPICE(NOTFOUND)' )
C
C           END IF
C
C           CALL VEQU   ( SPOINT, TMP )
C     C
C     C     Convert the intersection point of the boundary
C     C     FOV vector and Mars from rectangular into
C     C     latitudinal coordinates. Convert radians
C     C     to degrees.
C     C
C           CALL RECLAT ( SPOINT, RADIUS, LON, LAT )
C
C           LON = LON * DPR ()
C           LAT = LAT * DPR ()
C
C           WRITE (*,*) 'Boundary vector surface intercept ' //
C          .               'coordinates using SURFPT:'
C           WRITE (*,*) '   Radius    (km) :  ', RADIUS
C           WRITE (*,*) '   Latitude  (deg):  ', LAT
C           WRITE (*,*) '   Longitude (deg):  ', LON
C           WRITE (*,*) '   Emit time using'
C           WRITE (*,*) '   boresight LT(s):  ', ETEMIT
C
C     C
C     C     ------ A Boundary FOV Surface Intercept Verification ----
C     C
C     C     For verification only, we will calculate the surface
C     C     intercept coordinates for the selected boundary vector
C     C     using SINCPT and compare to the faster SURFPT method.
C     C
C           CALL SINCPT ( 'ELLIPSOID', 'MARS',  ETREC, 'IAU_MARS',
C          .               ABCORR,     'MGS',   OBSREF, BOUNDS(1,1),
C          .               SPOINT,      ETEMIT, SRFVEC, FOUND )
C
C           IF ( .NOT. FOUND ) THEN
C
C              CALL SETMSG ( 'Intercept not found for the ' //
C          .                 'boresight vector.'  )
C              CALL SIGERR ( 'SPICE(NOINTERCEPT)' )
C
C           END IF
C     C
C     C     Convert the intersection point of the selected boundary
C     C     vector and Mars from rectangular into latitudinal
C     C     coordinates. Convert radians to degrees.
C     C
C           CALL RECLAT ( SPOINT, RADIUS, LON, LAT )
C
C           LON = LON * DPR ()
C           LAT = LAT * DPR ()
C
C           WRITE (*,*) 'Boundary vector surface intercept ' //
C          .               'coordinates using SINCPT:'
C           WRITE (*,*) '   Radius    (km) :  ', RADIUS
C           WRITE (*,*) '   Latitude  (deg):  ', LAT
C           WRITE (*,*) '   Longitude (deg):  ', LON
C           WRITE (*,*) '   Emit time using'
C           WRITE (*,*) '   boundary LT(s) :  ', ETEMIT
C
C     C
C     C     We expect this to be a very small distance.
C     C
C           DIST = VDIST ( TMP, SPOINT )
C
C           WRITE (*,*) 'Distance between surface points'
C           WRITE (*,*) 'of the selected boundary vector using'
C           WRITE (*,*) 'SURFPT and SINCPT:'
C           WRITE (*,*) '   Distance (mm):     ', DIST*(10**6)
C
C           END
C       
C     When this program was executed using gfortran on a PC Linux
C     64 bit environment, the output was:
C
C           Observation Reference frame:  MGS_MOC_NA
C           Boresight surface intercept coordinates:
C              Radius    (km) :     3384.9404101592791
C              Latitude  (deg):    -48.479579821639035
C              Longitude (deg):    -123.43645396290199
C           Boundary vector surface intercept coordinates using SURFPT:
C              Radius    (km) :     3384.9411359300038
C              Latitude  (deg):    -48.477481877892437
C              Longitude (deg):    -123.47407986665237
C              Emit time using
C              boresight LT(s):     119296864.18105948
C           Boundary vector surface intercept coordinates using SINCPT:
C              Radius    (km) :     3384.9411359139663
C              Latitude  (deg):    -48.477481924252693
C              Longitude (deg):    -123.47407904898704
C              Emit time using
C              boundary LT(s) :     119296864.18105946
C           Distance between surface points
C           of the selected boundary vector using
C           SURFPT and SINCPT:
C              Distance (mm):       32.139879867352690
C
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
C     S.C. Krening  (JPL)
C     B.V. Semenov  (JPL)
C     W.L. Taber    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 23-SEP-2013 (SCK) (WLT) (BVS)
C
C-&


C$ Index_Entries
C
C     Position transformation matrix for different epochs
C
C-&

C$ Revisions
C
C     None.
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C
C     JCODE represents the NAIF ID of the J2000 reference frame.
C     The J2000 frame has a NAIF ID of 1. Any inertial reference
C     frame could have been used for this program instead of J2000.
C
      INTEGER               JCODE
      PARAMETER           ( JCODE = 1 )

C
C     Saved frame name length.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Local variables
C
      INTEGER               FCODE
      INTEGER               TCODE

      DOUBLE PRECISION      JF ( 3, 3 )
      DOUBLE PRECISION      TJ ( 3, 3 )

C
C     Saved frame name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVFROM
      INTEGER               SVFCOD

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVTO
      INTEGER               SVTCDE

      LOGICAL               FIRST

C
C     Saved frame name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVFROM
      SAVE                  SVFCOD

      SAVE                  SVCTR2
      SAVE                  SVTO
      SAVE                  SVTCDE

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C  
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'PXFRM2' ) 

C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counters.
C
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )

         FIRST = .FALSE.

      END IF
   
C
C     The frame names must be converted to their corresponding IDs.
C
      CALL ZZNAMFRM ( SVCTR1, SVFROM, SVFCOD, FROM, FCODE )
      CALL ZZNAMFRM ( SVCTR2, SVTO,   SVTCDE, TO,   TCODE )

C
C     Only non-zero ID codes are legitimate frame ID codes.  Zero
C     indicates that the frame was not recognized.
C
      IF      ( (FCODE .NE. 0) .AND. (TCODE .NE. 0) ) THEN
 
C
C        The following three lines of code calculate the following:
C
C        1)  [JF]      The rotation matrix is calculated from the frame
C                          FROM to the inertial J2000 frame at ETFROM.
C        2)  [TJ]      The rotation matrix is calculated from the J2000
C                          frame to the TO frame at ETTO.
C        3)  [ROTATE]  The rotation matrix from frame FROM at ETFROM to
C                          frame TO at ETTO is given by the following:
C
C                              [ROTATE] = [TF] = [TJ][JF]
C
         CALL REFCHG ( FCODE, JCODE, ETFROM, JF )
         CALL REFCHG ( JCODE, TCODE, ETTO,   TJ )
         CALL MXM    ( TJ,    JF,    ROTATE     )
 
      ELSE IF ( (FCODE .EQ. 0) .AND. (TCODE .EQ. 0) ) THEN
 
         CALL SETMSG ( 'Neither frame # nor # was '
     .   //            'recognized as a known reference frame. ' )
         CALL ERRCH  ( '#', FROM )
         CALL ERRCH  ( '#', TO   )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
 
      ELSE IF   (FCODE .EQ. 0) THEN
 
         CALL SETMSG ( 'The frame # was not recognized as a '
     .   //            'known reference frame. ' )
         CALL ERRCH  ( '#', FROM )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
 
      ELSE 
C
C        TCODE is zero
C 
         CALL SETMSG ( 'The frame # was not recognized as a '
     .   //            'known reference frame. ' )
         CALL ERRCH  ( '#', TO   )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
 
      END IF

      CALL CHKOUT ( 'PXFRM2' )
      RETURN

      END
