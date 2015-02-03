C$Procedure  CKE02 ( C-kernel, evaluate pointing record, data type 2 )
 
      SUBROUTINE CKE02 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
 
C$ Abstract
C
C   Evaluate a pointing record returned by CKR02 from a CK data type 2
C   segment. Return the C-matrix and angular velocity vector associated
C   with the time CLKOUT.
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
C   CK
C     ROTATION
C
C$ Keywords
C
C   POINTING
C
C$ Declarations
 
      LOGICAL               NEEDAV
      DOUBLE PRECISION      RECORD ( *    )
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      AV     ( 3    )
      DOUBLE PRECISION      CLKOUT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NEEDAV     I   True if angular velocity is requested.
C     RECORD     I   Data type 2 pointing record.
C     CMAT       O   C-matrix.
C     AV         O   Angular velocity vector.
C     CLKOUT     O   SCLK associated with C-matrix.
C
C$ Detailed_Input
C
C     NEEDAV     is true if angular velocity is requested.
C
C     RECORD     is a set of double precision numbers returned by CKR02
C                that contain sufficient information from a data type
C                2 pointing segment to evaluate the C-matrix and the
C                angular velocity vector for a particular instance.
C
C                The contents of RECORD are as follows:
C
C                   RECORD( 1  ) = start SCLKDP of interval
C
C                   RECORD( 2  ) = SCLK for which pointing was found
C
C                   RECORD( 3  ) = seconds / tick rate
C
C                   RECORD( 4  ) = q0
C                   RECORD( 5  ) = q1
C                   RECORD( 6  ) = q2
C                   RECORD( 7  ) = q3
C
C                   RECORD( 8  ) = av1
C                   RECORD( 9  ) = av2
C                   RECORD( 10 ) = av3
C
C                The quantities q0 - q3 are the components of the
C                quaternion that represents the C - matrix associated
C                with the start of the interval. The quantities av1,
C                av2, and av3 are the components of the angular velocity
C                vector.
C
C$ Detailed_Output
C
C
C     CMAT       is a rotation matrix that transforms the components
C                of a vector expressed in the inertial frame given in
C                the segment to components expressed in the instrument
C                fixed frame at the returned time.
C
C                Thus, if a vector v has components x, y, z in the
C                inertial frame, then v has components x', y', z' in the
C                instrument fixed frame where:
C
C                     [ x' ]     [          ] [ x ]
C                     | y' |  =  |   CMAT   | | y |
C                     [ z' ]     [          ] [ z ]
C
C                If the x', y', z' components are known, use the
C                transpose of the C-matrix to determine x, y, z as
C                follows.
C
C                     [ x ]      [          ]T    [ x' ]
C                     | y |  =   |   CMAT   |     | y' |
C                     [ z ]      [          ]     [ z' ]
C                              (Transpose of CMAT)
C
C     AV         is the angular velocity vector. The angular velocity
C                contained in RECORD is returned only if NEEDAV is true.
C
C                The direction of the angular velocity vector gives
C                the right-handed axis about which the instrument fixed
C                reference frame is rotating. The magnitude of AV is
C                the magnitude of the instantaneous velocity of the
C                rotation, in radians per second.
C
C                The angular velocity vector is returned in component
C                form
C
C                         AV = [ AV1  , AV2  , AV3  ]
C
C                which is in terms of the inertial coordinate frame
C                specified in the segment descriptor.
C
C     CLKOUT     is the encoded SCLK associated with the returned
C                C-matrix and angular velocity vector.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) No checking is done to determine whether RECORD is valid.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     For a detailed description of the structure of a type 2 pointing
C     segment, see the CK Required Reading.
C
C     Pointing data in a type 2 segment consists of intervals during
C     which the orientation of the spacecraft structure can be described
C     by an initial C-matrix and a constant angular velocity vector.
C     From the information contained in the pointing record returned by
C     CKR02, this subroutine calculates and returns the C-matrix
C     associated with the time returned by CKR02. It also returns the
C     angular velocity vector contained in the pointing record.
C
C$ Examples
C
C     A call to a CKEnn routine is almost always preceded by a call to
C     the corresponding CKRnn routine, which gets the logical record
C     that CKEnn evaluates.
C
C     The following code fragment searches through a file (represented
C     by HANDLE) for all segments applicable to the Voyager 2 wide angle
C     camera, for a particular spacecraft clock time, that are of data
C     types 1 or 2. It then evaluates the pointing for that epoch and
C     prints the result.
C
C
C           SC     = -32
C           INST   = -32002
C     C
C     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel.
C     C
C           CALL FURNSH ( 'VGR_SCLK.TSC'        )
C           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE )
C
C     C
C     C     Get the spacecraft clock time. Must encode it for use
C     C     in the C-kernel.
C     C
C
C           WRITE (*,*) 'Enter spacecraft clock time string:'
C           READ (*,FMT='(A)') SCLKCH
C           CALL SCENCD ( SC, SCLKCH, SCLKDP )
C
C     C
C     C     Search from the beginning through all segments.
C     C
C           CALL DAFBFS ( HANDLE )
C           CALL DAFFNA ( SFND   )
C
C           DO WHILE ( SFND )
C
C              CALL DAFGN ( IDENT                 )
C              CALL DAFGS ( DESCR                 )
C              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C
C              IF ( INST          .EQ. ICD( 1 )  .AND.
C          .        SCLKDP + TOL  .GE. DCD( 1 )  .AND.
C          .        SCLKDP - TOL  .LE. DCD( 2 ) ) THEN
C
C                 DTYPE = ICD ( 3 )
C
C                 IF ( DTYPE .EQ. 1 ) THEN
C
C                    CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
C          .                      RECORD, FOUND                       )
C
C                    IF ( FOUND ) THEN
C                       CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
C                    END IF
C
C                 ELSE  IF ( DTYPE .EQ. 2 ) THEN
C
C                    CALL CKR02 ( HANDLE, DESCR, SCLKDP, TOL,
C          .                      RECORD, FOUND )
C
C                    IF ( FOUND ) THEN
C                       CALL CKE02 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
C                    END IF
C
C                 END IF
C
C                 IF ( FOUND ) THEN
C
C                    WRITE (*,*) 'Segment descriptor and identifier:'
C                    WRITE (*,*) DCD, ICD
C                    WRITE (*,*) IDENT
C
C                    WRITE (*,*) 'C-matrix:'
C                    WRITE (*,*) CMAT
C
C                 END IF
C
C              END IF
C
C              CALL DAFFNA ( SFND )
C
C           END DO
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
C     J.M. Lynch     (JPL)
C     W.L. Taber     (JPL)
C     E.D. Wright    (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS)
C
C        Removed non-standard end-of-declarations marker
C        'C%&END_DECLARATIONS' from comments.
C
C-    SPICELIB Version 1.0.2, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 30-AUG-1991 (JML)
C
C-&
 
C$ Index_Entries
C
C     evaluate ck type_2 pointing data record
C
C-&
 
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
      DOUBLE PRECISION      VNORM
 
 
 
 
C
C     Local variables
C
      DOUBLE PRECISION      QUAT   ( 4    )
      DOUBLE PRECISION      CBASE  ( 3, 3 )
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      AVTEMP (    3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      TIME
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKE02' )
      END IF
 
C
C     Copy the returned encoded SCLK time into CLKOUT.
C
      CLKOUT = RECORD(2)
 
C     The quaternion stored in RECORD represents the C - matrix
C     corresponding to the start time of the interval.  The angular
C     velocity vector is constant throughout the interval and gives
C     the axis and rate by which the spacecraft is rotating.
C
C     Copy the quaternion and the angular velocity from RECORD.
C
C        RECORD ( 4 ) = q0
C        RECORD ( 5 ) = q1
C        RECORD ( 6 ) = q2
C        RECORD ( 7 ) = q3
C
C        RECORD ( 8  ) = av1
C        RECORD ( 9  ) = av2
C        RECORD ( 10 ) = av3
C
      CALL VEQUG ( RECORD(4), 4, QUAT )
      CALL VEQU  ( RECORD(8), AVTEMP  )
 
C
C     Calculate the angle of the rotation.
C
C        RECORD ( 1 ) = The start time of the interval.
C        RECORD ( 2 ) = The time that pointing was returned for.
C        RECORD ( 3 ) = The number of seconds per SCLK tick.
C
      TIME  =  ( RECORD(2) - RECORD(1) ) * RECORD(3)
 
      ANGLE =    TIME * VNORM ( AVTEMP )
 
C
C     Construct a matrix which rotates vectors by ANGLE radians about
C     AVTEMP.
C
      CALL AXISAR ( AVTEMP, ANGLE, ROT )
 
C
C     Convert the quaternion to a C - matrix.
C
      CALL Q2M ( QUAT, CBASE )
C
C     Rotate each of the axis vectors of the spacecraft instrument frame
C     by ANGLE radians about AVTEMP. (AVTEMP is given in the same
C     inertial frame as the C - matrix.)  The resulting matrix is the
C     transpose of the requested C - matrix.
C
C        [       ]       [       ] T         [        ] T
C        [  ROT  ]   *   [ CBASE ]     =     [  CMAT  ]
C        [       ]       [       ]           [        ]
C
C     OR
C
C        [       ]       [       ] T         [        ]
C        [ CBASE ]   *   [  ROT  ]     =     [  CMAT  ]
C        [       ]       [       ]           [        ]
C
 
      CALL MXMT ( CBASE, ROT, CMAT )
 
C
C     Return the angular velocity only if it is requested.
C
      IF ( NEEDAV ) THEN
         CALL VEQU ( AVTEMP, AV )
      END IF
 
 
      CALL CHKOUT ( 'CKE02' )
      RETURN
      END
