C$Procedure      TKFRAM (Text kernel frame transformation )
 
      SUBROUTINE TKFRAM ( ID, ROT, FRAME, FOUND )
 
C$ Abstract
C
C     This routine returns the rotation from the input frame
C     specified by ID to the associated frame given by FRAME.
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
C     POINTING
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               ID
      DOUBLE PRECISION      ROT   ( 3, 3 )
      INTEGER               FRAME
      LOGICAL               FOUND
 
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 200 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ----------------------------------------------
C     ID         I   Class identification code for the instrument
C     ROT        O   The rotation from ID to FRAME.
C     FRAME      O   The integer code of some reference frame.
C     FOUND      O   TRUE if the rotation could be determined.
C
C$ Detailed_Input
C
C     ID          The identification code used to specify an
C                 instrument in the SPICE system.
C
C$ Detailed_Output
C
C     ROT         is a rotation matrix that gives the transformation
C                 from the frame specified by ID to the frame
C                 specified by FRAME.
C
C     FRAME       is the id code of the frame used to define the
C                 orientation of the frame given by ID.  ROT gives
C                 the transformation from the IF frame to
C                 the frame specified by FRAME.
C
C     FOUND       is a logical indicating whether or not a frame
C                 definition for frame ID was constructed from
C                 kernel pool data.  If ROT and FRAME were constructed
C                 FOUND will be returned with the value TRUE.
C                 Otherwise it will be returned with the value FALSE.
C
C$ Parameters
C
C     BUFSIZ      is the number of rotation, frame id pairs that
C                 can have their instance data buffered for the
C                 sake of improving run-time performance.  This
C                 value MUST be positive and should probably be
C                 at least 10.
C
C$ Exceptions
C
C     1)  If some instance value associated with this frame
C         cannot be located, or does not have the proper type
C         or dimension, the error will be diagnosed by the
C         routine BADKPV. In such a case FOUND will be set to .FALSE.
C
C     2)  If the input ID has the value 0, the error
C         SPICE(ZEROFRAMEID) will be signaled. FOUND will be set
C         to FALSE.
C
C     3)  If the name of the frame corresponding to ID cannot be
C         determined, the error 'SPICE(INCOMPLETEFRAME)' is signaled.
C
C     4)  If the frame given by ID is defined relative to a frame
C         that is unrecognized, the error SPICE(BADFRAMESPEC)
C         will be signaled.  FOUND will be set to FALSE.
C
C     5)  If the kernel pool specification for ID is not one of
C         MATRIX, ANGLES, or QUATERNION, then the error
C         SPICE(UNKNOWNFRAMESPEC) will be signaled. FOUND will be
C         set to FALSE.
C
C     6)  If the frame ID is equal to the relative frame ID (i.e. the
C         frame is defined relative to itself), the error
C         SPICE(BADFRAMESPEC2) will be signaled.  FOUND will be set to
C         FALSE.
C
C$ Files
C
C     This routine makes use of the loaded text kernels to
C     determine the rotation from a constant offset frame
C     to its defining frame.
C
C$ Particulars
C
C     This routine is used to construct the rotation from some frame
C     that is a constant rotation offset from some other reference
C     frame. This rotation is derived from data stored in the kernel
C     pool.
C
C     It is considered to be an low level routine that
C     will need to be called directly only by persons performing
C     high volume processing.
C
C$ Examples
C
C     This is intended to be used as a low level routine by
C     the frame system software.  However, you could use this
C     routine to directly retrieve the rotation from an offset
C     frame to its relative frame.  One instance in which you
C     might do this is if you have a properly specified topocentric
C     frame for some site on earth and you wish to determine
C     the geodetic latitude and longitude of the site.  Here's how.
C
C        Suppose the name of the topocentric frame is: 'MYTOPO'.
C        First we get the id-code of the topocentric frame.
C
C        CALL NAMFRM ( 'MYTOPO', FRCODE )
C
C        Next get the rotation from the topocentric frame to
C        the bodyfixed frame.
C
C        CALL TKFRAM ( FRCODE, ROT, FRAME, FOUND )
C
C        Make sure the topoframe is relative to one of the earth
C        fixed frames.
C
C        CALL FRMNAM( FRAME, TEST )
C
C        IF (       TEST .NE. 'IAU_EARTH'
C       .     .AND. TEST .NE. 'EARTH_FIXED'
C       .     .AND. TEST .NE. 'ITRF93'  ) THEN
C
C           WRITE (*,*) 'The frame MYTOPO does not appear to be '
C           WRITE (*,*) 'defined relative to an earth fixed frame.'
C           STOP
C
C        END IF
C
C        Things look ok. Get the location of the Z-axis in the
C        topocentric frame.
C
C        Z(1) = ROT(1,3)
C        Z(2) = ROT(2,3)
C        Z(3) = ROT(3,3)
C
C        Convert the Z vector to latitude longitude and radius.
C
C        CALL RECLAT ( Z, LAT, LONG, RAD )
C
C        WRITE (*,*) 'The geodetic coordinates of the center of'
C        WRITE (*,*) 'the topographic frame are: '
C        WRITE (*,*)
C        WRITE (*,*) 'Latitude  (deg): ', LAT *DPR()
C        WRITE (*,*) 'Longitude (deg): ', LONG*DPR()
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
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.2.0, 08-JAN-2014 (BVS)
C
C        Added an error check for frames defined relative to
C        themselves.
C
C        Increased BUFSIZ from 20 to 200.
C
C-    SPICELIB Version 2.1.0, 23-APR-2009 (NJB)
C
C        Bug fix: watch is deleted only for frames
C        that are deleted from the buffer.
C
C-    SPICELIB Version 2.0.0, 19-MAR-2009 (NJB)
C
C        Bug fix: this routine now deletes watches set on
C        kernel variables of frames that are discarded from
C        the local buffering system.
C
C-    SPICELIB Version 1.2.0, 09-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in CONVRT, UCRSS, VHATG and VSCL calls.
C
C-    SPICELIB Version 1.1.0, 21-NOV-2001 (FST)
C
C        Updated this routine to dump the buffer of frame ID codes
C        it saves when it or one of the modules in its call tree signals
C        an error.  This fixes a bug where a frame's ID code is
C        buffered, but the matrix and kernel pool watcher were not set
C        properly.
C
C-    SPICELIB Version 1.0.0, 18-NOV-1996 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Fetch the rotation and frame of a text kernel frame
C     Fetch the rotation and frame of a constant offset frame
C
C-&

C$ Revisions
C
C-    SPICELIB Version 1.2.0, 09-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in CONVRT, UCRSS, VHATG and VSCL calls.
C
C-& 
 
 
C
C     Spicelib Functions
C
      DOUBLE PRECISION      VDOT

      INTEGER               LNKNFN
      INTEGER               LNKTL
      INTEGER               RTRIM

      LOGICAL               BADKPV
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               NITEMS
      PARAMETER           ( NITEMS = 14 )
 
      INTEGER               NVARS
      PARAMETER           ( NVARS = 14 )
 
      INTEGER               NDPS
      PARAMETER           ( NDPS  =  9 )
 
      INTEGER               NINTS
      PARAMETER           ( NINTS =  1 )
 
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
      INTEGER               SMWDSZ
      PARAMETER           ( SMWDSZ = 8 )

C
C     Local Variables
C
      CHARACTER*(WDSIZE)    AGENT
      CHARACTER*(WDSIZE)    ALT    ( NITEMS )
      CHARACTER*(WDSIZE)    ALTNAT
      CHARACTER*(WDSIZE)    FRNAME
      CHARACTER*(WDSIZE)    IDSTR
      CHARACTER*(WDSIZE)    ITEM   ( NVARS  )
      CHARACTER*(WDSIZE)    OLDAGT
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    SPEC
      CHARACTER*(1)         TYPE
      CHARACTER*(WDSIZE)    UNITS
      CHARACTER*(SMWDSZ)    VERSN
 
      DOUBLE PRECISION      ANGLES ( 3 )
      DOUBLE PRECISION      BUFFD  ( NDPS, BUFSIZ )
      DOUBLE PRECISION      MATRIX ( 3,    3      )
      DOUBLE PRECISION      QUATRN ( 0:3 )
      DOUBLE PRECISION      QTMP   ( 0:3 )
      DOUBLE PRECISION      TEMPD
 
      INTEGER               AR
      INTEGER               AT
      INTEGER               AXES   ( 3 )
      INTEGER               BUFFI  ( NINTS,     BUFSIZ )
      INTEGER               I
      INTEGER               IDENTS ( 1,         BUFSIZ )
      INTEGER               IDNT   ( 1 )
      INTEGER               N
      INTEGER               OLDID
      INTEGER               POOL   ( 2, LBPOOL: BUFSIZ )
      INTEGER               R
      INTEGER               TAIL

      LOGICAL               BUFFRD
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               FULL
      LOGICAL               UPDATE
 
C
C     Saved variables
C 
      SAVE

C
C     Initial values
C
      DATA                  AT     /  0      /
      DATA                  FIRST  / .TRUE.  /
 
 
C
C     Programmer's note: this routine makes use of the *implementation*
C     of LOCATI. If that routine is changed, the logic this routine
C     uses to locate buffered, old frame IDs may need to change as well.
C

 
C
C     Before we even check in, if N is less than 1 we can
C     just return.
C
C
C     Perform any initializations that might be needed for this
C     routine.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
         VERSN = '1.0.0'
 
         CALL LNKINI ( BUFSIZ, POOL )
 
      END IF
 
C
C     Now do the standard SPICE error handling.  Sure this is
C     a bit unconventional, but nothing will be hurt by doing
C     the stuff above first.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'TKFRAM' )
 
C
C     So far, we've not FOUND the rotation to the specified frame.
C
      FOUND = .FALSE.
 
C
C     Check the ID to make sure it is non-zero.
C
      IF (  ID .EQ. 0 ) THEN
 
         CALL LNKINI ( BUFSIZ, POOL )
 
         CALL SETMSG ( 'Frame identification codes are '
     .   //            'required to be non-zero.  You''ve '
     .   //            'specified a frame with ID value '
     .   //            'zero. '                            )
         CALL SIGERR ( 'SPICE(ZEROFRAMEID)'                )
         CALL CHKOUT ( 'TKFRAM'                            )
         RETURN
 
      END IF
 
C
C     Find out whether our linked list pool is already full.
C     We'll use this information later to decide whether we're
C     going to have to delete a watcher.
C
      FULL = LNKNFN(POOL) .EQ. 0

      IF ( FULL ) THEN
C
C        If the input frame ID is not buffered, we'll need to
C        overwrite an existing buffer entry. In this case
C        the call to LOCATI we're about to make will overwrite
C        the ID code in the slot we're about to use. We need
C        this ID code, so extract it now while we have the
C        opportunity. The old ID sits at the tail of the list
C        whose head node is AT.
C
         TAIL  = LNKTL  ( AT, POOL )

         OLDID = IDENTS ( 1,  TAIL )

C
C        Create the name of the agent associated with the old
C        frame.
C
         OLDAGT = 'TKFRAME_#'
         CALL REPMI ( OLDAGT, '#', OLDID, OLDAGT )

      END IF

C
C     Look up the address of the instance data. 
C
      IDNT(1) = ID
      CALL LOCATI ( IDNT, 1, IDENTS, POOL, AT, BUFFRD )


      IF (  FULL  .AND. ( .NOT. BUFFRD )  ) THEN
C
C        Since the buffer is already full, we'll delete the watcher for
C        the kernel variables associated with OLDID, since there's no
C        longer a need for that watcher.
C
C        First clear the update status of the old agent; DWPOOL won't
C        delete an agent with a unchecked update.
C
         CALL CVPOOL ( OLDAGT, UPDATE )
         CALL DWPOOL ( OLDAGT )

      END IF

C
C     Until we have better information we put the identity matrix
C     into the output rotation and set FRAME to zero.
C
      CALL IDENT ( ROT )
      FRAME = 0
 
C
C     If we have to look up the data for our frame, we do
C     it now and perform any conversions and computations that
C     will be needed when it's time to convert coordinates to
C     directions.
C
C     Construct the name of the agent associated with the
C     requested frame.  (Each frame has its own agent).
C
      CALL INTSTR ( ID, IDSTR  )
      CALL FRMNAM ( ID, FRNAME )
 
      IF ( FRNAME .EQ. ' ' ) THEN
 
         CALL LNKINI ( BUFSIZ, POOL )
 
         CALL SETMSG ( 'The Text Kernel (TK) frame with id-code '
     .   //            '# does not have a recognized name. '      )
         CALL ERRINT ( '#', ID                                    )
         CALL SIGERR ( 'SPICE(INCOMPLETFRAME)'                    )
         CALL CHKOUT ( 'TKFRAM'                                   )
         RETURN
 
      END IF
 
      AGENT  = 'TKFRAME_' // IDSTR
      R      =  RTRIM(AGENT)
 
      ALTNAT = 'TKFRAME_' // FRNAME
      AR     =  RTRIM(ALTNAT)
 
C
C     If the frame is buffered, we check the kernel pool to
C     see if there has been an update to this frame.
C
      IF ( BUFFRD ) THEN
 
         CALL CVPOOL ( AGENT(1:R), UPDATE )
 
      ELSE
C
C        If the frame is not buffered we definitely need to update
C        things.

         UPDATE = .TRUE.
 
      END IF
 
 
      IF ( .NOT. UPDATE ) THEN
C
C        Just look up the rotation matrix and relative-to
C        information from the local buffer.
C
         ROT(1,1)  = BUFFD ( 1, AT )
         ROT(2,1)  = BUFFD ( 2, AT )
         ROT(3,1)  = BUFFD ( 3, AT )
         ROT(1,2)  = BUFFD ( 4, AT )
         ROT(2,2)  = BUFFD ( 5, AT )
         ROT(3,2)  = BUFFD ( 6, AT )
         ROT(1,3)  = BUFFD ( 7, AT )
         ROT(2,3)  = BUFFD ( 8, AT )
         ROT(3,3)  = BUFFD ( 9, AT )
 
         FRAME     = BUFFI ( 1, AT )
 
      ELSE
C
C        Determine how the frame is specified and what it
C        is relative to.  The variables that specify
C        how the frame is represented and what it is relative to
C        are TKFRAME_#_SPEC and TKFRAME_#_RELATIVE where # is
C        replaced by the text value of ID or the frame name.
C
         ITEM(1) = AGENT(1:R) // '_SPEC'
         ITEM(2) = AGENT(1:R) // '_RELATIVE'
 
         ALT (1) = ALTNAT(1:AR) // '_SPEC'
         ALT (2) = ALTNAT(1:AR) // '_RELATIVE'
C
C        See if the friendlier version of the kernel pool variables
C        are available.
C
         DO I = 1, 2

            CALL DTPOOL ( ALT(I), FOUND, N, TYPE )
 
            IF ( FOUND ) THEN
               ITEM(I) = ALT(I)
            END IF

         END DO
 
C
C        If either the SPEC or RELATIVE frame are missing from
C        the kernel pool, we simply return.
C
         IF (     BADKPV ( 'TKFRAM', ITEM(1), '=', 1, 1, 'C' )
     .       .OR. BADKPV ( 'TKFRAM', ITEM(2), '=', 1, 1, 'C' ) ) THEN
 
            CALL LNKINI ( BUFSIZ, POOL )
            FRAME =  0
            CALL IDENT  (  ROT     )
            CALL CHKOUT ( 'TKFRAM' )
            RETURN
 
         END IF
 
C
C        If we make it this far, look up the SPEC and RELATIVE frame.
C
         CALL GCPOOL ( ITEM(1),   1, 1, N, SPEC,  FND )
         CALL GCPOOL ( ITEM(2),   1, 1, N, NAME,  FND )
 
C
C        Look up the id-code for this frame.
C
         CALL NAMFRM ( NAME, FRAME )
 
         IF ( FRAME .EQ. 0 ) THEN
 
            CALL LNKINI ( BUFSIZ, POOL )
 
            CALL SETMSG ( 'The frame to which frame # is '
     .      //            'relatively defined is not '
     .      //            'recognized. The kernel pool '
     .      //            'specification of the relative '
     .      //            'frame is ''#''.  This is not a '
     .      //            'recognized frame. ' )
            CALL ERRINT ( '#', ID   )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(BADFRAMESPEC)' )
            CALL CHKOUT ( 'TKFRAM' )
            RETURN
 
         END IF

C
C        Make sure that the RELATIVE frame ID is distinct from the
C        frame ID. If they are the same, SPICE will go into an
C        indefinite loop.
C
         IF ( FRAME .EQ. ID ) THEN
 
            CALL LNKINI ( BUFSIZ, POOL )
 
            CALL SETMSG ( 'Bad fixed offset frame specification: '
     .      //            'the frame ''#'' (frame ID #) is '
     .      //            'defined relative to itself. SPICE '
     .      //            'cannot work with such frames. ' )
            CALL ERRCH  ( '#', NAME )
            CALL ERRINT ( '#', ID   )
            CALL SIGERR ( 'SPICE(BADFRAMESPEC2)' )
            CALL CHKOUT ( 'TKFRAM' )
            RETURN
 
         END IF

C
C        Convert SPEC to upper case so that we can easily check
C        to see if this is one of the expected specification types.
C
         CALL UCASE ( SPEC, SPEC )
 
         IF ( SPEC .EQ. 'MATRIX' ) THEN
C
C           This is the easiest case.  Just grab the matrix
C           from the kernel pool (and polish it up a bit just
C           to make sure we have a rotation matrix).
C
C           We give preference to the kernel pool variable
C           TKFRAME_<name>_MATRIX if it is available.
C
            ITEM(3) = AGENT (1:R)  // '_MATRIX'
            ALT (3) = ALTNAT(1:AR) // '_MATRIX'
 
            CALL DTPOOL ( ALT(3), FOUND, N, TYPE )
 
            IF ( FOUND ) THEN
               ITEM(3) = ALT(3)
            END IF
 
            IF ( BADKPV ( 'TKFRAM', ITEM(3), '=', 9, 1, 'N' ) ) THEN
 
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
 
            END IF
 
C
C           The variable meets current expectations, look it up
C           from the kernel pool.
C
            CALL GDPOOL ( ITEM(3),   1, 9, N, MATRIX,  FND )
 
C
C           In this case the full transformation matrix has been
C           specified.  We simply polish it up a bit.
C
            CALL MOVED  ( MATRIX, 9, ROT )
            CALL SHARPR (            ROT )

C
C           The matrix might not be right-handed, so correct
C           the sense of the second and third columns if necessary.
C
            IF ( VDOT( ROT(1,2), MATRIX(1,2) ) .LT. 0.0D0 ) THEN
               CALL VSCLIP ( -1.0D0, ROT(1,2) )
            END IF
 
            IF ( VDOT( ROT(1,3), MATRIX(1,3) ) .LT. 0.0D0 ) THEN
               CALL VSCLIP ( -1.0D0, ROT(1,3) )
            END IF
 
 
         ELSE IF ( SPEC .EQ. 'ANGLES' ) THEN
C
C           Look up the angles, their units and axes for the
C           frame specified by ID. (Note that UNITS are optional).
C           As in the previous case we give preference to the
C           form TKFRAME_<name>_<item> over TKFRAME_<id>_<item>.
C
            ITEM(3) = AGENT(1:R)   // '_ANGLES'
            ITEM(4) = AGENT(1:R)   // '_AXES'
            ITEM(5) = AGENT(1:R)   // '_UNITS'
 
            ALT (3) = ALTNAT(1:AR) // '_ANGLES'
            ALT (4) = ALTNAT(1:AR) // '_AXES'
            ALT (5) = ALTNAT(1:AR) // '_UNITS'
C
C           Again, we give preference to the more friendly form
C           of TKFRAME specification.
C
            DO I = 3, 5
 
               CALL DTPOOL ( ALT(I), FOUND, N, TYPE )
 
               IF ( FOUND ) THEN
                  ITEM(I) = ALT(I)
               END IF
 
            END DO
 
 
            IF (    BADKPV( 'TKFRAM', ITEM(3), '=', 3, 1, 'N' )
     .         .OR. BADKPV( 'TKFRAM', ITEM(4), '=', 3, 1, 'N' ) )
     .      THEN
 
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
 
            END IF
 
            UNITS = 'RADIANS'
 
            CALL GDPOOL ( ITEM(3),   1, 3, N, ANGLES, FND )
            CALL GIPOOL ( ITEM(4),   1, 3, N, AXES,   FND )
            CALL GCPOOL ( ITEM(5),   1, 1, N, UNITS,  FND )
 
C
C           Convert angles to radians.
C
            DO I = 1, 3

               CALL CONVRT ( ANGLES(I), UNITS, 'RADIANS', TEMPD )
               ANGLES(I) = TEMPD

            END DO
 
            IF ( FAILED() ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF
 
C
C           Compute the rotation from instrument frame to CK frame.
C
            CALL EUL2M (  ANGLES(1),   ANGLES(2),   ANGLES(3),
     .                    AXES  (1),   AXES  (2),   AXES  (3), ROT )
 
 
         ELSE IF ( SPEC .EQ. 'QUATERNION' ) THEN
C
C           Look up the quaternion and convert it to a rotation
C           matrix. Again there are two possible variables that
C           may point to the quaternion. We give preference to
C           the form TKFRAME_<name>_Q over the form TKFRAME_<id>_Q.
C
            ITEM(3) = AGENT(1:R)   // '_Q'
            ALT (3) = ALTNAT(1:AR) // '_Q'
 
            CALL DTPOOL ( ALT(3), FOUND, N, TYPE )
 
            IF ( FOUND ) THEN
               ITEM(3) = ALT(3)
            END IF
 
 
            IF ( BADKPV ( 'TKFRAM', ITEM(3), '=', 4, 1, 'N' ) ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF
C
C           In this case we have the quaternion representation.
C           Again, we do a small amount of polishing of the input.
C
            CALL GDPOOL ( ITEM(3), 1, 4, N, QUATRN, FND )
            CALL VHATG  ( QUATRN,  4,       QTMP   )
            CALL Q2M    ( QTMP,             ROT    )
 
         ELSE
C
C           We don't recognize the SPEC for this frame.  Say
C           so.  Also note that perhaps the user needs to upgrade
C           the toolkit.
C
            CALL LNKINI ( BUFSIZ, POOL )
 
            CALL SETMSG ( 'The frame specification "# = '
     .      //            '''#''" is not one of the reconized '
     .      //            'means of specifying a text-kernel '
     .      //            'constant offset frame (as of '
     .      //            'version # of the routine TKFRAM). '
     .      //            'This may reflect a typographical '
     .      //            'error or may indicate that you '
     .      //            'need to consider updating your '
     .      //            'version of the SPICE toolkit. ' )
 
 
 
            CALL ERRCH  ( '#', ITEM(1) )
            CALL ERRCH  ( '#', SPEC    )
            CALL ERRCH  ( '#', VERSN   )
            CALL SIGERR ( 'SPICE(UNKNOWNFRAMESPEC)' )
            CALL CHKOUT ( 'TKFRAM' )
            RETURN
 
         END IF
 
C
C        Buffer the identifier, relative frame and rotation matrix.
C
         BUFFD ( 1, AT ) = ROT(1,1)
         BUFFD ( 2, AT ) = ROT(2,1)
         BUFFD ( 3, AT ) = ROT(3,1)
         BUFFD ( 4, AT ) = ROT(1,2)
         BUFFD ( 5, AT ) = ROT(2,2)
         BUFFD ( 6, AT ) = ROT(3,2)
         BUFFD ( 7, AT ) = ROT(1,3)
         BUFFD ( 8, AT ) = ROT(2,3)
         BUFFD ( 9, AT ) = ROT(3,3)
 
         BUFFI ( 1, AT ) = FRAME
 
C
C        If these were not previously buffered, we need to set
C        a watch on the various items that might be used to define
C        this frame.
C
         IF ( .NOT. BUFFRD ) THEN
C
C           Immediately check for an update so that we will
C           not redundantly look for this item the next time this
C           routine is called.
C
            ITEM( 1) = AGENT(1:R)   // '_RELATIVE'
            ITEM( 2) = AGENT(1:R)   // '_SPEC'
            ITEM( 3) = AGENT(1:R)   // '_AXES'
            ITEM( 4) = AGENT(1:R)   // '_MATRIX'
            ITEM( 5) = AGENT(1:R)   // '_Q'
            ITEM( 6) = AGENT(1:R)   // '_ANGLES'
            ITEM( 7) = AGENT(1:R)   // '_UNITS'
 
            ITEM( 8) = ALTNAT(1:AR) // '_RELATIVE'
            ITEM( 9) = ALTNAT(1:AR) // '_SPEC'
            ITEM(10) = ALTNAT(1:AR) // '_AXES'
            ITEM(11) = ALTNAT(1:AR) // '_MATRIX'
            ITEM(12) = ALTNAT(1:AR) // '_Q'
            ITEM(13) = ALTNAT(1:AR) // '_ANGLES'
            ITEM(14) = ALTNAT(1:AR) // '_UNITS'
 
            CALL SWPOOL ( AGENT, 14, ITEM )
            CALL CVPOOL ( AGENT, UPDATE   )
 
         END IF
 
      END IF
 
      IF ( FAILED() ) THEN
         CALL LNKINI ( BUFSIZ, POOL )
         CALL CHKOUT ( 'TKFRAM' )
         RETURN
      END IF
 
C
C     All errors cause the routine to exit before we get to this
C     point.  If we reach this point we didn't have an error and
C     hence did find the rotation from ID to FRAME.
C
      FOUND = .TRUE.
 
C
C     That's it
C
      CALL CHKOUT ( 'TKFRAM' )
      RETURN
 
      END
