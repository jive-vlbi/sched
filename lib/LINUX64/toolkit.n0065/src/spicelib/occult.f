C$Procedure      OCCULT ( find occultation type at time )

      SUBROUTINE OCCULT ( TARG1,  SHAPE1, FRAME1, 
     .                    TARG2,  SHAPE2, FRAME2, 
     .                    ABCORR, OBSRVR, ET,     OCLTID )

C$ Abstract
C
C     Determines the occultation condition (not occulted, partially,
C     etc.) of one target relative to another target as seen by
C     an observer at a given time.
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
C     SPK
C     TIME
C     KERNEL
C
C$ Keywords
C
C     GEOMETRY
C     OCCULTATION
C     ELLIPSOID
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'
      INCLUDE 'occult.inc'

      CHARACTER*(*)         TARG1
      CHARACTER*(*)         SHAPE1
      CHARACTER*(*)         FRAME1
      CHARACTER*(*)         TARG2
      CHARACTER*(*)         SHAPE2
      CHARACTER*(*)         FRAME2
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      ET
      INTEGER               OCLTID


C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARG1      I   Name or ID of first target.
C     SHAPE1     I   Type of shape model used for first target.
C     FRAME1     I   Body-fixed, body-centered frame for first body.
C     TARG2      I   Name or ID of second target.
C     SHAPE2     I   Type of shape model used for second target.
C     FRAME2     I   Body-fixed, body-centered frame for second body.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name or ID of the observer.
C     ET         I   Time of the observation (seconds past J2000).
C     OCLTID     O   Occultation identification code.
C
C$ Detailed_Input
C
C     TARG1      is the name of the first target body. Both object
C                names and NAIF IDs are accepted. For example, both
C                'Moon' and '301' are accepted.
C
C     SHAPE1     is a string indicating the geometric model used to
C                represent the shape of the first target body. The
C                supported options are:
C
C                   'ELLIPSOID'     Use a triaxial ellipsoid model
C                                   with radius values provided via the
C                                   kernel pool. A kernel variable 
C                                   having a name of the form
C
C                                      'BODYnnn_RADII' 
C
C                                   where nnn represents the NAIF
C                                   integer code associated with the
C                                   body, must be present in the kernel
C                                   pool. This variable must be
C                                   associated with three numeric
C                                   values giving the lengths of the
C                                   ellipsoid's X, Y, and Z semi-axes.
C
C                   'POINT'         Treat the body as a single point.
C                                   When a point target is specified,
C                                   the occultation type must be
C                                   set to 'ANY'.
C                                   
C                At least one of the target bodies TARG1 or TARG2 must
C                be modeled as an ellipsoid.
C
C                Case and leading or trailing blanks are not
C                significant in the string.
C
C     FRAME1     is the name of the body-fixed, body-centered reference
C                frame associated with the first target body. Examples
C                of such names are 'IAU_SATURN' (for Saturn) and
C                'ITRF93' (for the Earth).
C
C                If the first target body is modeled as a point, FRAME1
C                should be left blank (Ex: ' ').
C
C                Case and leading or trailing blanks bracketing a
C                non-blank frame name are not significant in the string.
C
C     TARG2      is the name of the second target body. See the
C                description of TARG1 above for more details.
C
C     SHAPE2     is the shape specification for the body designated
C                by TARG2. See the description of SHAPE1 above for
C                details.
C
C     FRAME2     is the name of the body-fixed, body-centered reference
C                frame associated with the second target body. See the
C                description of FRAME1 above for more details.
C
C     ABCORR     indicates the aberration corrections to be applied to
C                the state of each target body to account for one-way
C                light time. Stellar aberration corrections are
C                ignored if specified, since these corrections don't
C                improve the accuracy of the occultation determination.
C
C                See the header of the SPICE routine SPKEZR for a
C                detailed description of the aberration correction
C                options. For convenience, the options supported by
C                this routine are listed below:
C
C                   'NONE'     Apply no correction.   
C
C                   'LT'       "Reception" case: correct for
C                              one-way light time using a Newtonian
C                              formulation.
C
C                   'CN'       "Reception" case: converged
C                              Newtonian light time correction.
C
C                   'XLT'      "Transmission" case: correct for
C                              one-way light time using a Newtonian
C                              formulation.
C
C                   'XCN'      "Transmission" case: converged
C                              Newtonian light time correction.
C
C                Case and blanks are not significant in the string
C                ABCORR.
C
C     OBSRVR     is the name of the body from which the occultation
C                is observed. See the description of TARG1 above for
C                more details.
C
C     ET         is the observation time in seconds past the J2000
C                epoch.
C
C                    
C$ Detailed_Output
C
C     OCLTID     is an integer occultation code indicating the geometric
C                relationship of the three bodies.  
C
C                The meaning of the sign of OCLTID is given below.
C
C                    Code sign          Meaning
C                    ---------          ------------------------------
C                       > 0             The second ellipsoid is
C                                       partially or fully occulted
C                                       by the first.
C
C                       < 0             The first ellipsoid is 
C                                       partially of fully
C                                       occulted by the second.
C
C                       = 0             No occultation.
C
C                Possible OCLTID values and meanings are given below.
C                The variable names indicate the type of occultation
C                and which target is in the back. For example, TOTAL1
C                represents a total occultation in which the first
C                target is in the back (or occulted by) the second
C                target.
C
C                    Name      Code     Meaning
C                    ------    -----    ------------------------------
C                    TOTAL1     -3      Total occultation of first
C                                       target by second.
C
C                    ANNLR1     -2      Annular occultation of first
C                                       target by second. The second
C                                       target does not block the limb
C                                       of the first.
C
C                    PARTL1     -1      Partial occultation of first
C                                       target by second target.
C
C                    NOOCC       0      No occultation or transit: both
C                                       objects are completely visible
C                                       to the observer.
C
C                    PARTL2      1      Partial occultation of second
C                                       target by first target.
C
C                    ANNLR2      2      Annular occultation of second
C                                       target by first.
C
C                    TOTAL2      3      Total occultation of second
C                                       target by first.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the target or observer body names input by the user are
C         not recognized, the error will be diagnosed by a routine in
C         the call tree of this routine.
C
C     2)  If the input shapes are not accepted, the error will be
C         diagnosed by a routine in the call tree of this routine.
C
C     3)  If both input shapes are points, the error will be
C         diagnosed by a routine in the call tree of this routine.
C
C     4)  If the radii of a target body modeled as an ellipsoid cannot
C         be determined by searching the kernel pool for a kernel
C         variable having a name of the form
C
C            'BODYnnn_RADII' 
C
C         where nnn represents the NAIF integer code associated with
C         the body, the error will be diagnosed by a routine in the
C         call tree of this routine.
C
C     5)  If any of the target or observer bodies (TARG1, TARG2, or
C         OBSRVR) are the same, the error will be diagnosed
C         by a routine in the call tree of this routine.
C         
C     6)  If the loaded kernels provide insufficient data to 
C         compute any required state vector, the deficiency will
C         be diagnosed by a routine in the call tree of this routine.
C
C     7)  If an error occurs while reading an SPK or other kernel,
C         the error will be diagnosed by a routine in the call tree 
C         of this routine.
C
C     8)  Invalid aberration correction specifications will be
C         diagnosed by a routine in the call tree of this routine.
C
C$ Files
C
C     Appropriate SPICE kernels must be loaded by the calling program
C     before this routine is called.
C
C     The following data are required:
C
C        - SPK data: the calling application must load ephemeris data
C          for the targets and observer for the specified input time.
C          If aberration corrections are used, the states of the target
C          bodies and of the observer relative to the solar system
C          barycenter must be calculable from the available ephemeris
C          data. Typically ephemeris data are made available by loading
C          one or more SPK files via FURNSH.
C
C        - PCK data: bodies modeled as triaxial ellipsoids must have
C          semi-axis lengths provided by variables in the kernel pool.
C          Typically these data are made available by loading a text
C          PCK file via FURNSH.
C
C     Kernel data are normally loaded once per program run, NOT every
C     time this routine is called.
C
C$ Particulars
C
C     For many purposes, modeling extended bodies as triaxial
C     ellipsoids is adequate for determining whether one body is
C     occulted by another as seen from a specified observer.
C     
C$ Examples
C
C     1) Find whether MRO is occulted by Mars as seen by
C        the DSS-13 ground station at a few specific
C        times.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C           KPL/MK
C
C           File: mro_ex_occult.tm
C
C           This is the meta-kernel file for the example problem for
C           the subroutine OCCULT. These kernel files can be found in
C           the NAIF archives.
C 
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C           
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C                 File name                       Contents
C                 ---------                       --------
C                 de421.bsp                       Planetary ephemeris
C                 earthstns_itrf93_050714.bsp     DSN station ephemeris
C                 pck00010.tpc                    Planet orientation and
C                                                 radii
C                 earth_000101_120409_120117.bpc  High precision Earth
C                                                 orientation
C                 mro_psp_rec.bsp                 MRO ephemeris
C                 naif0010.tls                    Leapseconds
C                 earth_topo_050714.tf            Topocentric reference
C                                                 frames for
C                                                 DSN stations
C
C           \begindata
C
C           KERNELS_TO_LOAD = ( 'de421.bsp',
C                               'earthstns_itrf93_050714.bsp',
C                               'pck00010.tpc',
C                               'earth_000101_120409_120117.bpc',
C                               'mro_psp_rec.bsp',
C                               'naif0010.tls',
C                               'earth_topo_050714.tf' )
C           \begintext
C
C        Example code begins here.
C
C           PROGRAM OCCULT_MRO
C           IMPLICIT NONE
C
C           INCLUDE              'occult.inc'
C
C     C
C     C     Local parameters
C     C
C           CHARACTER*(*)         META
C           PARAMETER           ( META  = mro_ex_occult.tm' )
C
C           CHARACTER*(*)         FRMT
C           PARAMETER           ( FRMT  = '(A18, A5, A21, A5, A4, A6)' )
C
C           INTEGER               CHSIZ
C           PARAMETER           ( CHSIZ = 30 )
C
C     C
C     C     Local variables
C     C
C           CHARACTER*(CHSIZ)     ABCORR
C           CHARACTER*(CHSIZ)     FORM
C           CHARACTER*(CHSIZ)     OBSRVR
C           CHARACTER*(CHSIZ)     SHAPE1
C           CHARACTER*(CHSIZ)     SHAPE2
C           CHARACTER*(CHSIZ)     TARG1
C           CHARACTER*(CHSIZ)     TARG2
C           CHARACTER*(CHSIZ)     TIME
C           CHARACTER*(CHSIZ)     TSTART
C           CHARACTER*(CHSIZ)     TEND
C           CHARACTER*(CHSIZ)     OUTCH ( 4 )
C
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      ET1
C           DOUBLE PRECISION      ETEND
C
C           INTEGER               DT
C           INTEGER               OCLTID
C
C     C
C     C     Saved variables
C     C
C           SAVE OUTCH
C
C           DATA OUTCH ( 1 ) / 'totally occulted by'   /
C           DATA OUTCH ( 2 ) / 'transited by'          /
C           DATA OUTCH ( 3 ) / 'partially occulted by' /
C           DATA OUTCH ( 4 ) / 'not occulted by'       /
C
C     C
C     C     Initialize the time range. Set the output time
C     C     format to PST. Set DT to an hour interval in
C     C     units of seconds.
C     C
C           TSTART = '2012-JAN-5 1:15:00 UTC'
C           TEND   = '2012-JAN-5 2:50:00 UTC'
C           FORM   = 'YYYY-MON-DD HR:MN ::UTC-8'
C           DT     = 1000
C
C     C
C     C     Initialize the targets, observer, and aberration
C     C     correction.
C     C
C           TARG1  = 'MRO'
C           SHAPE1 = 'POINT'
C           TARG2  = 'MARS'
C           SHAPE2 = 'ELLIPSOID'
C           OBSRVR = 'DSS-13'
C           ABCORR = 'CN'
C
C     C
C     C     Load kernel files via the meta-kernel.
C     C
C           CALL FURNSH ( META )
C     C
C     C     Calculate the start and stop times in ET.
C     C
C           CALL STR2ET ( TSTART, ET1   )
C           CALL STR2ET ( TEND,   ETEND )
C
C           ET = ET1
C           DO WHILE ( ET .LT. ETEND )
C     C
C     C        Calculate the type of occultation that
C     C        corresponds to time ET.
C     C
C              CALL OCCULT ( TARG1,  SHAPE1, ' ',
C          .                 TARG2,  SHAPE2, 'IAU_MARS',
C          .                 ABCORR, OBSRVR,  ET, OCLTID )
C     C
C     C        Output the results.
C     C
C              CALL TIMOUT ( ET, FORM, TIME )
C
C              IF ( OCLTID .EQ. TOTAL1 ) THEN
C                 WRITE (*,FRMT) TIME, TARG1, OUTCH(1), TARG2,
C          .                     'wrt ', OBSRVR
C
C              ELSEIF ( OCLTID .EQ. ANNLR1 ) THEN
C                 WRITE (*,FRMT) TIME, TARG1, OUTCH(2), TARG2,
C          .                     'wrt ', OBSRVR
C
C              ELSEIF ( OCLTID .EQ. PARTL1 ) THEN
C                 WRITE (*,FRMT) TIME, TARG1, OUTCH(3), TARG2,
C          .                     'wrt ', OBSRVR,
C          .                     'NOT POSSIBLE FOR POINT'
C
C              ELSEIF ( OCLTID .EQ. NOOCC ) THEN
C                 WRITE (*,FRMT) TIME, TARG1, OUTCH(4), TARG2,
C          .                     'wrt ', OBSRVR
C
C              ELSEIF ( OCLTID .EQ. PARTL2 ) THEN
C                 WRITE (*,FRMT) TIME, TARG2, OUTCH(3), TARG1,
C          .                     'wrt ', OBSRVR,
C          .                     'NOT POSSIBLE FOR POINT'
C
C              ELSEIF ( OCLTID .EQ. ANNLR2 ) THEN
C                 WRITE (*,FRMT) TIME, TARG2, OUTCH(2), TARG1,
C          .                     'wrt ', OBSRVR
C
C              ELSEIF ( OCLTID .EQ. TOTAL2 ) THEN
C                 WRITE (*,FRMT) TIME, TARG2, OUTCH(1), TARG1,
C          .                     'wrt ', OBSRVR
C
C              ELSE
C                 WRITE (*,*) 'Bad occultation ID:  ', OCLTID
C
C              END IF
C     C
C     C        Increment the time.
C     C
C              ET = ET + DT
C
C           END DO
C
C           END
C
C        When this program was executed using gfortran on a PC Linux
C        64 bit environment, the output was:
C
C           2012-JAN-04 17:15 MARS transited by         MRO  wrt DSS-13
C           2012-JAN-04 17:31 MRO  not occulted by      MARS wrt DSS-13
C           2012-JAN-04 17:48 MRO  totally occulted by  MARS wrt DSS-13
C           2012-JAN-04 18:04 MRO  totally occulted by  MARS wrt DSS-13
C           2012-JAN-04 18:21 MRO  not occulted by      MARS wrt DSS-13
C           2012-JAN-04 18:38 MARS transited by         MRO  wrt DSS-13
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
C     S.C. Krening   (JPL)
C     N.J. Bachman   (JPL)
C
C$ Version 
C
C-    SPICELIB Version 1.0.0, 14-NOV-2013 (SCK) (NJB)
C
C
C-&

C$ Index_Entries
C
C     occultation type at a specified time
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     External routines
C
      EXTERNAL              ZZGFOCIN
      EXTERNAL              ZZGFOCST
C
C     Local parameters
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )
C
C     Local variables
C
      CHARACTER*(BDNMLN)    BACK
      CHARACTER*(BDNMLN)    BNAME
      CHARACTER*(FRNMLN)    BFRAME
      CHARACTER*(SHPLEN)    BSHAPE
      CHARACTER*(BDNMLN)    FNAME
      CHARACTER*(BDNMLN)    FRONT
      CHARACTER*(FRNMLN)    FFRAME
      CHARACTER*(SHPLEN)    FSHAPE
      CHARACTER*(SHPLEN)    OCCTYP ( 3 )
      CHARACTER*(SHPLEN)    SHAP1
      CHARACTER*(SHPLEN)    SHAP2

      INTEGER               I
      INTEGER               INDEX
      INTEGER               MLTFAC

      LOGICAL               ELLPS2
      LOGICAL               OCSTAT
C
C     Saved variables
C
      SAVE OCCTYP

C
C     The variable OCCTYP associates the string of an occultation
C     type (from gf.inc) with its positive integer code (from
C     occult.inc).  The variable OCCTYP is set up so each string is
C     stored at the index relating to that configuration's positive
C     integer code.  The positive integer codes assume the first
C     target is occulting (in front of) the second target.
C
C                 Ex:  PARTL2 = 1               (from occult.inc)
C                      OCCTYP ( 1 ) = 'PARTIAL' (from gf.inc)
C
C     The table below shows the relation between each index of OCCTYP,
C     the occultation condition, which target is in front and back, the
C     multiplication factor, and the output integer occultation code.
C     Note that the output integer occultation code is the integer index
C     of OCCTYP multiplied by the multiplication factor.  
C
C                 OCLTID = Index * MLTFAC
C
C     MLTFAC is 1 if TARG1 is in front, and -1 if TARG1 is in back.
C     The setup of OCCTYP could be changed, but it is important to keep
C     the output integer occultation codes consistent with the values
C     from occult.inc.
C
C         Index   Occult. Condition   Front   Back   MLTFAC  OCLTID
C         -----   -----------------   -----   -----  ------  ------
C           1          Partial        TARG1   TARG2    1       1
C           1          Partial        TARG2   TARG1   -1      -1
C           2          Annular        TARG1   TARG2    1       2
C           2          Annular        TARG2   TARG1   -1      -2
C           3          Total          TARG1   TARG2    1       3
C           3          Total          TARG2   TARG1   -1      -3
C

      DATA OCCTYP ( PARTL2 ) / PARTL  /
      DATA OCCTYP ( ANNLR2 ) / ANNULR /
      DATA OCCTYP ( TOTAL2 ) / FULL   /

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'OCCULT' )

C
C     Left justify the shapes and target names and make them upper case.
C
      CALL LJUST ( SHAPE1, SHAP1 )
      CALL UCASE ( SHAP1,  SHAP1 )

      CALL LJUST ( SHAPE2, SHAP2 )
      CALL UCASE ( SHAP2,  SHAP2 )

      CALL LJUST ( TARG1,  FNAME )
      CALL UCASE ( FNAME,  FNAME )

      CALL LJUST ( TARG2,  BNAME )
      CALL UCASE ( BNAME,  BNAME )

C
C     The variable ELLPS2 is a flag that relates if both targets are
C     represented as ellipsoids. If not, only the 'any' occultation
C     check can be completed.
C
      IF ( ( SHAP1 .EQ. EDSHAP )  .AND.
     .     ( SHAP2 .EQ. EDSHAP ) ) THEN

         ELLPS2 = .TRUE.

      ELSE

         ELLPS2 = .FALSE.

      END IF

C
C     Test two main cases:
C     1) The first target is the front body.
C     2) The second target is the front body.
C
C     First, initialize the occultation code to reflect no occultation.
C
      OCLTID = NOOCC

      DO  I = 1, 2 
C
C        The first time through, make the first target the
C        front. On the second time, make the second target the front.
C        For details on the variable MLTFAC, please see the detailed
C        explanation of the OCCTYP variable near the start of the code.
C
         IF ( I .EQ. 1 ) THEN

            FRONT  = FNAME
            FSHAPE = SHAP1
            FFRAME = FRAME1
            BACK   = BNAME
            BSHAPE = SHAP2
            BFRAME = FRAME2
            MLTFAC = 1
            
         ELSE 
            
            FRONT  = BNAME
            FSHAPE = SHAP2
            FFRAME = FRAME2
            BACK   = FNAME
            BSHAPE = SHAP1
            BFRAME = FRAME1
            MLTFAC = -1
            
         END IF
C
C        Check if there is any occultation with the current front/back
C        configuration. ZZGFOCIN performs initializations. ZZGFOCST
C        returns a true/false logical indicating if there is an
C        occultation.
C
         CALL ZZGFOCIN  ( ANY, FRONT,  FSHAPE, FFRAME, 
     .                         BACK,   BSHAPE, BFRAME,  
     .                         OBSRVR, ABCORR        )
         
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'OCCULT' )
            RETURN
         END IF

         CALL ZZGFOCST ( ET, OCSTAT )
         
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'OCCULT' )
            RETURN
         END IF
         
C
C        If there was an occultation, and both targets are represented
C        as ellipsoids, test the three types of occultations: partial,
C        annular, and full. Note: If the integer parameters within
C        occult.inc are changed, the following DO loop will need to be
C        altered.
C
         IF ( OCSTAT .AND. ELLPS2 ) THEN

            DO  INDEX = PARTL2, TOTAL2
               
               CALL ZZGFOCIN ( OCCTYP(INDEX), FRONT,  FSHAPE, FFRAME,
     .                                        BACK,   BSHAPE, BFRAME, 
     .                                        OBSRVR, ABCORR        )
               
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'OCCULT' )
                  RETURN
               END IF
               
               CALL ZZGFOCST ( ET, OCSTAT )
               
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'OCCULT' )
                  RETURN
               END IF
C
C              If the occultation condition is true, return the integer
C              occultation ID code.
C
               IF ( OCSTAT ) THEN
                  
                  OCLTID = MLTFAC * INDEX
                  CALL CHKOUT ( 'OCCULT' )
                  RETURN

               END IF      
C
C              End the DO loop that checks the occultation type.
C         
            END DO
C
C        If the search for 'any' occultation was true and the front
C        target is an ellipse, this is a total occultation. (Other
C        target is a point).
C           
         ELSE IF ( OCSTAT .AND. ( FSHAPE .EQ. EDSHAP ) ) THEN

            OCLTID = MLTFAC * TOTAL2
            CALL CHKOUT ( 'OCCULT' )
            RETURN

C
C        If the search for 'any' occultation was true and the back
C        target is an ellipse, this is an annular occultation. (Other
C        target is a point).
C    
         ELSE IF ( OCSTAT .AND. ( BSHAPE .EQ. EDSHAP ) ) THEN

            OCLTID = MLTFAC * ANNLR2
            CALL CHKOUT ( 'OCCULT' )
            RETURN

         END IF
C
C        End the DO loop that checks the front/back orientation of
C        the input targets.
C
      END DO

C
C     If the occultation searches show that there was no occultation
C     at the given time, return an occultation code that indicates
C     no occultation. If this part of the code has been reached and 
C     the occultation code indicates an occultation was found, an error
C     has occurred.  
C     
      IF ( OCLTID .NE. NOOCC ) THEN
         
         CALL SETMSG ( 'This error should never be reached; '
     .        //       'the occultation code result # is invalid.')
         CALL ERRINT ( '#', OCLTID  )
         CALL SIGERR ( 'SPICE(BUG)' )
         CALL CHKOUT ( 'OCCULT'      )
         RETURN
         
      END IF
      
      CALL CHKOUT ( 'OCCULT' )
      RETURN
      END

