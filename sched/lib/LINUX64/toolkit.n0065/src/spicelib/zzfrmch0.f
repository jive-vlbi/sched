C$Procedure      ZZFRMCH0 (Frame Change)
 
      SUBROUTINE ZZFRMCH0 ( FRAME1, FRAME2, ET, XFORM )
 
C$ Abstract
C
C     Return the state transformation matrix from one
C     frame to another.
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
C     FRAMES
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE              'errhnd.inc'
      INCLUDE              'frmtyp.inc'

      INTEGER               FRAME1
      INTEGER               FRAME2
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      XFORM ( 6, 6 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRAME1     I   the frame id-code for some reference frame
C     FRAME2     I   the frame id-code for some reference frame
C     ET         I   an epoch in TDB seconds past J2000.
C     XFORM      O   a state transformation matrix
C
C$ Detailed_Input
C
C     FRAME1      is the frame id-code in which some states are known.
C
C     FRAME2      is the frame id-code for some frame in which you
C                 would like to represent states.
C
C     ET          is the epoch at which to compute the state
C                 transformation matrix.  This epoch should be
C                 in TDB seconds past the ephemeris epoch of J2000.
C
C$ Detailed_Output
C
C     XFORM       is a 6 x 6 state transformation matrix that can
C                 be used to transform states relative to the frame
C                 correspsonding to frame FRAME2 to states relative
C                 to the frame FRAME2.  More explicitely, if STATE
C                 is the state of some object relative to the reference
C                 frame of FRAME1 then STATE2 is the state of the
C                 same object relative to FRAME2 where STATE2 is
C                 computed via the subroutine call below
C
C                    CALL MXVG ( XFORM, STATE, 6, 6, STATE2 )
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If either of the reference frames is unrecognized, the error
C        SPICE(UNKNOWNFRAME) will be signalled.
C
C     2) If the auxillary information needed to compute a non-inertial
C        frame is not available an error will be diagnosed and signalled
C        by a routine in the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows you to compute the state transformation matrix
C     between two reference frames.
C
C     The currently supported reference frames are IAU bodyfixed frames
C     and inertial reference frames.
C
C$ Examples
C
C     Example 1.  Suppose that you have a state STATE1 at epoch ET
C     relative to  FRAME1 and wish to determine its representation
C     STATE2 relative to FRAME2.  The following subroutine calls
C     would suffice to make this transformation.
C
C        CALL FRMCHG ( FRAME1, FRAME2, ET,   XFORM )
C        CALL MXVG   ( XFORM,  STATE1, 6, 6, STATE2 )
C
C
C
C     Example 2.  Suppose that you have the angular velocity, W, of some
C     rotation relative to FRAME1 at epoch ET and that you wish to
C     express this angular velocity with respect to FRAME2.  The
C     following subroutines will suffice to perform this computation.
C
C        CALL FRMCHG ( FRAME1, FRAME2, ET, STXFRM )
C
C     Recall that a state transformation matrix has the following form.
C
C
C            -               -
C           |                 |
C           |    R        0   |
C           |                 |
C           |                 |
C           |   dR            |
C           |   --        R   |
C           |   dt            |
C           |                 |
C            -               -
C
C
C     The velocity of an arbitrary point P undergoing rotation with the
C     angular velocity W is W x P
C
C     Thus the velocity of P in FRAME2 is:
C
C
C        dR
C        --  P    +    R (W x P )
C        dt
C
C           dR  t
C     =  (  -- R  R P   +  W  x P  )            ( 1 )
C           dt
C
C
C           dR  t                                              t
C     But   -- R  is skew symmetric  (simply differentiate  R*R to see
C           dt
C                    dR  t
C     this ).  Hence -- R R P  can be written as Ax(R*P) for some fixed
C                    dt
C
C     vector A.  Moreover the vector A can be read from the upper
C
C                            dR  t
C     triangular portion of  -- R  .  So that equation (1) above can
C                            dt
C
C     be re-written as
C
C         dR  t
C     = ( -- R  R*P   +  R*(WxP)  )
C         dt
C
C     = Ax(R*P) + R*W x R*P
C
C     = ( [A+R*W] x R*P )
C
C
C     From this final expression it follows that in FRAME2 the angular
C     velocity vector is given by [A+R*W].
C
C     The code below implements these ideas.
C
C        CALL FRMCHG ( FRAME1, FRAME2, ET, STXFRM )
C
C
C        DO I = 1, 3
C           DO J = 1, 3
C
C              RT  ( I, J ) = STXFRM ( I,   J )
C              DRDT( I, J ) = STXFRM ( I+3, J )
C
C           END DO
C        END DO
C
C        CALL MXMT ( DRDT, R, AMATRIX )
C
C        Read the angular velocity of R from the skew symmetric matrix
C
C         dR  t
C         -- R
C         dt
C
C        Recall that if A has components A1, A2, A3 then the matrix
C        cooresponding to the cross product linear mapping is:
C
C            -               -
C           |   0  -A3    A2  |
C           |                 |
C           |  A3   0    -A1  |
C           |                 |
C           | -A2   A1    0   |
C            -               -
C
C        A(1) = -AMATRIX(2,3)
C        A(2) =  AMATRIX(1,3)
C        A(3) = -AMATRIX(1,2)
C
C        CALL MXV  ( R, W1,  W  )
C        CALL VADD ( A, W,   W2 )
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 14-DEC-2008 (NJB)
C
C        Upgraded long error message associated with frame
C        connection failure.
C
C-    SPICELIB Version 1.1.0, 25-JUL-1996 (WLT)
C
C        Bug Fix:
C
C        The previous edition of the routine had a bug in the
C        first pass of the DO WHILE that looks for a frame
C        in the chain of frames associated with FRAME2 that is
C        in common with the chain of frames for FRAME1.
C
C        On machines where variables are created as static
C        variables, this error could lead to finding a frame
C        when a legitimate path between FRAME1 and FRAME2
C        did not exist.
C
C-    SPICELIB Version 1.0.1, 06-MAR-1996 (WLT)
C
C        An typo was fixed in the Brief I/O section. It used
C        to say TDT instead of the correct time system TDB.
C
C-    SPICELIB Version 1.0.0, 28-SEP-1994 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Transform states from one frame to another
C
C-&
 
C
C     SPICE functions
C
      INTEGER               ISRCHI

      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               MAXNOD
      PARAMETER           ( MAXNOD = 10 )
 
      INTEGER               MAXXFM
      PARAMETER           ( MAXXFM = MAXNOD + 4 )
  
C
C     The root of all reference frames is J2000 (Frame ID = 1).
C
      INTEGER               ROOTF
      PARAMETER           ( ROOTF = 1 )
 
C
C     Local Variables
C
      CHARACTER*(LMSGLN)    ERRMSG

      DOUBLE PRECISION      TEMPXF( 6, 6 )
C
C     TRANS contains the transformations from FRAME1 to FRAME2
C     TRANS(1...6,1...6,I) has the transfromation from FRAME(I)
C     to FRAME(I+1).  We make extra room in TRANS because we
C     plan to add transformations beyond the obvious chain from
C     FRAME1 to a root node.
C
      DOUBLE PRECISION      TRANS ( 6, 6, MAXXFM ) 

C
C     TRANS2 is used to store intermediate transformations from
C     FRAME2 to some node in the chain from FRAME1 to PCK or
C     INERTL frames.
C
      DOUBLE PRECISION      TRANS2( 6, 6, 2      )
  
  
      INTEGER               CENT
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               CMNODE
C
C     FRAME contains the frames we transform from in going from
C     FRAME1 to FRAME2.  FRAME(1) = FRAME1 by  construction.
C
      INTEGER               FRAME ( MAXNOD )
      INTEGER               GET
      INTEGER               I
      INTEGER               INC
      INTEGER               J
      INTEGER               K
      INTEGER               L
 
C
C     NODE counts the number of transformations needed to go
C     from FRAME1 to FRAME2.
C
      INTEGER               NODE

      INTEGER               PUT
      INTEGER               RELTO
      INTEGER               THIS 

      LOGICAL               DONE
      LOGICAL               FOUND
      LOGICAL               GOTONE
  
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ZZFRMCH0')
C
C     Do the obvious thing first.  If FRAME1 and FRAME2 are the
C     same then we simply return the identity matrix.
C
      IF ( FRAME1 .EQ. FRAME2 ) THEN
 
         DO I = 1, 6
 
            XFORM(I,I) = 1.0D0
 
            DO J = 1, I-1
               XFORM(I,J) = 0.0D0
               XFORM(J,I) = 0.0D0
            END DO
 
         END DO
 
         CALL CHKOUT ( 'ZZFRMCH0' )
         RETURN
 
      END IF
C
C     Now perform the obvious check to make sure that both
C     frames are recognized.
C
      CALL FRINFO ( FRAME1, CENT, CLASS, CLSSID, FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The number # is not a recognized id-code '
     .   //            'for a reference frame. ' )
         CALL ERRINT ( '#', FRAME1 )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
         CALL CHKOUT ( 'ZZFRMCH0' )
         RETURN
 
      END IF
 
 
      CALL FRINFO ( FRAME2, CENT, CLASS, CLSSID, FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The number # is not a recognized id-code '
     .   //            'for a reference frame. ' )
         CALL ERRINT ( '#', FRAME2 )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
         CALL CHKOUT ( 'ZZFRMCH0' )
         RETURN
 
      END IF
 
 
 
      NODE        = 1
      FRAME(NODE) = FRAME1
      FOUND       = .TRUE.
C
C     Follow the chain of transformations until we run into
C     one that transforms to J2000 (frame id = 1) or we hit FRAME2.
C
      DO WHILE (       FRAME(NODE) .NE. ROOTF
     .           .AND. NODE        .LT. MAXNOD
     .           .AND. FRAME(NODE) .NE. FRAME2
     .           .AND. FOUND   )
C
C        Find out what transformation is available for this
C        frame.
C
         CALL ZZFRMGT0 ( FRAME(NODE),     ET,
     .                   TRANS(1,1,NODE), FRAME(NODE+1), FOUND )
 
         IF ( FOUND ) THEN
C
C           We found a transformation matrix.  TRANS(1,1,NODE)
C           now contains the transformation from FRAME(NODE)
C           to FRAME(NODE+1).  We need to look up the information
C           for the next NODE.
C
            NODE = NODE + 1
 
         END IF
 
      END DO
 
      DONE =      FRAME(NODE) .EQ. ROOTF
     .       .OR. FRAME(NODE) .EQ. FRAME2
     .       .OR. .NOT. FOUND
 
 
      DO WHILE ( .NOT. DONE )
C
C        The only way to get to this point is to have run out of
C        room in the array of reference frame transformation
C        buffers.  We will now build the transformation from
C        the previous NODE to whatever the next node in the
C        chain is.  We'll do this until we get to one of the
C        root classes or we run into FRAME2.
C
         CALL ZZFRMGT0( FRAME(NODE), ET, TRANS(1,1,NODE), RELTO, FOUND )
 
 
         IF ( FOUND  ) THEN
C
C           Recall that TRANS(1,1,NODE-1) contains the transformation
C           from FRAME(NODE-1) to FRAME(NODE).  We are going to replace
C           FRAME(NODE) with the frame indicated by RELTO.  This means
C           that TRANS(1,1,NODE-1) should be replaced with the
C           transformation from FRAME(NODE) to RELTO.
C
            FRAME(NODE) = RELTO
            CALL ZZMSXF ( TRANS(1,1,NODE-1), 2, TEMPXF )
 
            DO I = 1,6
               DO J = 1,6
                  TRANS(I,J,NODE-1) = TEMPXF(I,J)
               END DO
            END DO
 
         END IF
C
C        We are done if the class of the last frame is J2000
C        or if the last frame is FRAME2 or if we simply couldn't get
C        another transformation.
C
         DONE =        FRAME(NODE) .EQ. ROOTF
     .            .OR. FRAME(NODE) .EQ. FRAME2
     .            .OR. .NOT. FOUND
 
      END DO
 
C
C     Right now we have the following situation.  We have in hand
C     a collection of transformations between frames. (Assuming
C     that is that NODE .GT. 1.  If NODE .EQ. 1 then we have
C     no transformations computed yet.
C
C
C     TRANS(1...6, 1...6, 1    )    transforms FRAME1   to FRAME(2)
C     TRANS(1...6, 1...6, 2    )    transforms FRAME(2) to FRAME(3)
C     TRANS(1...6, 1...6, 3    )    transforms FRAME(3) to FRAME(4)
C        .
C        .
C        .
C     TRANS(1...6, 1...6, NODE-1 )  transforms FRAME(NODE-1)
C                                   to         FRAME(NODE)
C
C
C     One of the following situations is true.
C
C     1)  FRAME(NODE) is the root of all frames, J2000.
C
C     2)  FRAME(NODE) is the same as FRAME2
C
C     3)  There is no transformation from FRAME(NODE) to another
C         more fundamental frame.  The chain of transformations
C         from FRAME1 stops at FRAME(NODE).  This means that the
C         "frame atlas" is incomplete because we can't get to the
C         root frame.
C
C     We now have to do essentially the same thing for FRAME2.
C
 
      IF ( FRAME(NODE) .EQ. FRAME2 ) THEN
C
C        We can handle this one immediately with the private routine
C        ZZMSXF which multiplies a series of state transformation
C        matrices.
C
         CALL ZZMSXF (  TRANS, NODE-1, XFORM )
         CALL CHKOUT ( 'ZZFRMCH0' )
         RETURN
 
      END IF
 
C
C     We didn't luck out above.  So we follow the chain of
C     transformation for FRAME2.  Note that at the moment the
C     chain of transformations from FRAME2 to other frames
C     does not share a node in the chain for FRAME1.
C    ( GOTONE = .FALSE. ) .
C
      THIS   =  FRAME2
      GOTONE = .FALSE.
 
C
C     First see if there is any chain to follow.
C
      DONE   =  THIS .EQ. ROOTF
 
C
C     Set up the matrices TRANS2(,,1) and TRANS(,,2)  and set up
C     PUT and GET pointers so that we know where to GET the partial
C     transformation from and where to PUT partial results.
C
      IF ( .NOT. DONE ) THEN
 
         DO K = 1, 2
            DO I=1,3
               DO J = 4,6
                  TRANS2(I,J,K) = 0.0D0
               END DO
            END DO
         END DO
 
         PUT = 1
         GET = 1
         INC = 1
 
      END IF
 
C
C     Follow the chain of transformations until we run into
C     one that transforms to the root frame or we land in the
C     chain of nodes for FRAME1.
C
C     Note that this time we will simply keep track of the full
C     translation from FRAME2 to the last node.
C
      DO WHILE (  .NOT. DONE )
C
C        Find out what transformation is available for this
C        frame.
C
         IF ( THIS  .EQ. FRAME2 ) THEN
C
C           This is the first pass, just put the transformation
C           directly into TRANS2(,,PUT).
C
            CALL ZZFRMGT0 ( THIS, ET, TRANS2(1,1,PUT), RELTO, FOUND )
 
 
            IF ( FOUND ) THEN
               THIS   = RELTO
               GET    = PUT
               PUT    = PUT + INC
               INC    =     - INC
               CMNODE = ISRCHI ( THIS, NODE, FRAME )
               GOTONE = CMNODE .GT. 0
            END IF
 
         ELSE
C
C           Fetch the transformation into a temporary spot TEMPXF
C
            CALL ZZFRMGT0 ( THIS, ET, TEMPXF, RELTO, FOUND )
 
            IF ( FOUND ) THEN
C
C              Next multiply TEMPXF on the right by the last partial
C              product (in TRANS2(,,GET) ).  We do this in line because
C              we can cut down the number of multiplies to 3/8 of the
C              normal result of MXMG.  For a discussion of why this
C              works see ZZMSXF.
C
               DO I=1,3
                  DO J=1,3
 
                     TRANS2(I,J,PUT) = TEMPXF(I,1)*TRANS2(1,J,GET)
     .                               + TEMPXF(I,2)*TRANS2(2,J,GET)
     .                               + TEMPXF(I,3)*TRANS2(3,J,GET)
                  END DO
               END DO
 
               DO I=4,6
                  DO J=1,3
 
                     TRANS2(I,J,PUT) = TEMPXF(I,1)*TRANS2(1,J,GET)
     .                               + TEMPXF(I,2)*TRANS2(2,J,GET)
     .                               + TEMPXF(I,3)*TRANS2(3,J,GET)
     .                               + TEMPXF(I,4)*TRANS2(4,J,GET)
     .                               + TEMPXF(I,5)*TRANS2(5,J,GET)
     .                               + TEMPXF(I,6)*TRANS2(6,J,GET)
                  END DO
               END DO
C
C              Note that we don't have to compute the upper right
C              hand block.  It's already set to zero by construction.
C
C              Finally we can just copy the lower right hand block
C              from the upper left hand block of the matrix.
C
               DO I = 4,6
                  K = I - 3
                  DO J = 4,6
                     L = J - 3
                     TRANS2(I,J,PUT) = TRANS2(K,L,PUT)
                  END DO
               END DO
 
C
C              Adjust GET and PUT so that GET points to the slots
C              where we just stored the result of our multiply and
C              so that PUT points to the next available storage
C              locations.
C
               GET    = PUT
               PUT    = PUT + INC
               INC    =     - INC
               THIS   = RELTO
 
               CMNODE = ISRCHI ( THIS, NODE, FRAME )
               GOTONE = CMNODE .GT. 0
 
            END IF
 
         END IF
 
C
C        See if we have a common node and determine whether or not
C        we are done with this loop.
C
         DONE   =       THIS  .EQ. ROOTF
     .            .OR.  GOTONE
     .            .OR. .NOT. FOUND
 
 
      END DO
 
C
C     There are two possible scenarios.  Either the chain of
C     transformations from FRAME2 ran into a node in the chain for
C     FRAME1 or it didn't.  (The common node might very well be
C     the root node.)  If we didn't run into a common one, then
C     the two chains don't intersect and there is no way to
C     get from FRAME1 to FRAME2.
C
      IF ( .NOT. GOTONE ) THEN

         CALL ZZNOFCON ( ET, FRAME1, FRAME(NODE), FRAME2, THIS, ERRMSG )

         IF ( FAILED() ) THEN
C
C           We were unable to create the error message. This
C           unfortunate situation could arise if a frame kernel 
C           is corrupted.
C
            CALL CHKOUT ( 'ZZFRMCH0' )
            RETURN

         END IF
C
C        The normal case: signal an error with a descriptive long
C        error message.
C
         CALL SETMSG ( ERRMSG                  )
         CALL SIGERR ( 'SPICE(NOFRAMECONNECT)' )
         CALL CHKOUT ( 'ZZFRMCH0'              )
         RETURN
 
      END IF
 
C
C     Recall that we have the following.
C
C     TRANS(1...6, 1...6, 1    )    transforms FRAME(1) to FRAME(2)
C     TRANS(1...6, 1...6, 2    )    transforms FRAME(2) to FRAME(3)
C     TRANS(1...6, 1...6, 3    )    transforms FRAME(3) to FRAME(4)
C
C     TRANS(1...6, 1...6, CMNODE-1) transforms FRAME(CMNODE-1)
C                                   to         FRAME(CMNODE)
C
C     and that TRANS2(1,1,GET) transforms from FRAME2 to CMNODE.
C     Hence the inverse of TRANS2(1,1,GET) transforms from CMNODE
C     to FRAME2.
C
C     If we compute the inverse of TRANS2 and store it in
C     the next available slot of TRANS (.i.e. TRANS(1,1,CMNODE)
C     we can simply apply our custom routine that multiplies a
C     sequence of transformation matrices together to get the
C     result from FRAME1 to FRAME2.
C
      CALL INVSTM ( TRANS2(1,1,GET), TRANS(1,1,CMNODE) )
      CALL ZZMSXF ( TRANS,   CMNODE, XFORM             )
 
      CALL CHKOUT ( 'ZZFRMCH0' )
      RETURN
 
      END
