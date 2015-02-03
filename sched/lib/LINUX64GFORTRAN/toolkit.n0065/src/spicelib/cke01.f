C$Procedure      CKE01 ( CK evaluate pointing record, data type 1 )
 
      SUBROUTINE CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
 
C$ Abstract
C
C     Evaluate a pointing record returned by CKR01 from a CK data type 1
C     segment. Return the C-matrix and optionally the angular velocity
C     vector associated with the time CLKOUT.
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
C     CK
C     DAF
C     ROTATION
C
C$ Keywords
C
C     POINTING
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
C     NEEDAV     I   True if angular velocity vector is required.
C     RECORD     I   Data type 1 pointing record.
C     CMAT       O   C-matrix.
C     AV         O   Angular velocity vector.
C     CLKOUT     O   Output spacecraft clock time.
C
C$ Detailed_Input
C
C     NEEDAV     is true when angular velocity data is requested.
C
C     RECORD     is a set of double precision numbers returned by CKR01
C                that contain sufficient information from a data type
C                1 pointing segment to evaluate the C-matrix and
C                possibly the angular velocity vector (if NEEDAV is
C                true) for a particular instance.
C
C                The contents of RECORD are as follows:
C
C                   RECORD( 1 ) = CLKOUT
C
C                   RECORD( 2 ) = q0
C                   RECORD( 3 ) = q1
C                   RECORD( 4 ) = q2
C                   RECORD( 5 ) = q3
C
C                   RECORD( 6 ) = Av1  ]
C                   RECORD( 7 ) = Av2  |-- Optional
C                   RECORD( 8 ) = Av3  ]
C
C
C                The quantities q0 - q3 represent a quaternion.
C                The quantities Av1, Av2, and Av3 represent the angular
C                velocity vector.
C
C                CLKOUT is the encoded spacecraft clock time
C                associated with the quaternion and, optionally, the
C                angular velocity vector.
C
C$ Detailed_Output
C
C     CMAT       is a rotation matrix that transforms the components of
C                of a vector expressed in the reference frame given in
C                the segment to components expressed in the instrument
C                fixed frame at time CLKOUT.
C
C                Thus, if a vector v has components x, y, z in the
C                reference frame, then v has components x', y', z' in
C                the instrument fixed frame at time CLKOUT:
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
C     AV         is the angular velocity vector. This is returned only
C                if it has been requested, as indicated by NEEDAV. In
C                other words, if NEEDAV is true, the angular velocity
C                portion of RECORD must be present.
C
C                The angular velocity vector is the vector whose
C                direction gives the right-handed axis about which
C                the reference frame tied to the instrument is
C                instantaneously rotating at time CLKOUT.
C
C                The angular velocity vector is returned in component
C                form
C
C                         AV = [ AV1  , AV2  , AV3  ]
C
C                which is in terms of the reference coordinate frame
C                specified in the segment descriptor.
C
C                The magnitude of AV is the magnitude of the instantane-
C                ous velocity of the rotation, in radians per second.
C
C     CLKOUT     The encoded spacecraft clock time associated with the
C                returned C-matrix and, optionally, the returned angular
C                velocity vector.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) No checking is done to determine whether RECORD is a valid
C        record.
C
C     2) If NEEDAV is true, then RECORD is assumed to contain angular
C        velocity data. No checking is performed to verify this
C        assumption.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     For a detailed description of the structure of a type 1 pointing
C     segment, see the CK Required Reading file.
C
C     The only real work done by CKE01 is to convert the pointing
C     portion of the record from quaternion form to C-matrix form.
C
C     The angular velocity vector will only be returned if it has been
C     requested. In other words, if NEEDAV is true, the routine will
C     expect the angular velocity component of the record to be present.
C
C$ Examples
C
C     A call to a CKEnn routine is almost always preceded by a call to
C     the corresponding CKRnn routine, which gets the logical record
C     that CKEnn evaluates.
C
C     The following code fragment searches through a file represented
C     by HANDLE for all segments applicable to the Voyager 2 wide angle
C     camera, for a particular spacecraft clock time, which have data
C     type 1.  It then evaluates the pointing for that epoch and prints
C     the result.
C
C     C
C     C     - Get the spacecraft clock time. Must encode it for use
C     C       in the C-kernel.
C     C
C     C     - Set the time tolerance high to catch anything close to
C     C       the input time.
C     C
C     C     - We don't need angular velocity data.
C     C
C
C           SC     = -32
C           INST   = -32002
C           TOL    =  1000.D0
C           NEEDAV = .FALSE.
C           DTYPE  =  1
C     C
C     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel.
C     C
C           CALL FURNSH ( 'VGR_SCLK.TSC'        )
C           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE )
C     C
C     C     Convert the input request time to ticks.
C     C
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
C              IF (        INST          .EQ. ICD( 1 )
C          .               DTYPE         .EQ. ICD( 3 )
C          .        .AND.  SCLKDP + TOL  .GE. DCD( 1 )
C          .        .AND.  SCLKDP - TOL  .LE. DCD( 2 )  ) THEN
C
C                 CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
C          .                   RECORD, FOUND )
C
C                 IF ( FOUND ) THEN
C
C                    CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
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
C     1) None.
C
C$ Author_and_Institution
C
C     J.E. McLean    (JPL)
C     M.J. Spencer   (JPL)
C     R.E. Thurman   (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 22-AUG-2006 (EDW)
C
C        Replaced header references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 1.2.0, 14-NOV-1995 (WLT)
C
C        Changed "inertial frame" to simply reference frame to
C        reflect new capabilities of the SPICE system.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 30-AUG-1991 (MJS) (JML)
C
C        1) Previously, in the standard SPICE error handling, the
C           logical function RETURN was not written as a function;
C           it is now written as a function.
C
C        2) The example program was changed so that the tolerance
C           and data type are used in selecting which segments to read.
C
C        3) It was specified that the angular velocity vector
C           gives the right-handed axis about which the instrument
C           frame rotates.
C
C-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
C
C        The example program was corrected so that the input
C        instrument code was tested against ICD(1) instead of
C        ICD(3).
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
 
C$ Index_Entries
C
C     evaluate ck type_1 pointing data record
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 14-NOV-1995 (WLT)
C
C        Changed "inertial frame" to simply reference frame to
C        reflect new capabilities of the SPICE system.
C
C        This change affects only documentation not code.
C
C-    SPICELIB Version 1.1.0, 30-AUG-1991 (MJS) (JML)
C
C        1) In the standard SPICE error handling, the line:
C
C              IF ( RETURN ) THEN
C
C           was changed to
C
C              IF ( RETURN() ) THEN
C
C        2) The example program was changed so that the tolerance
C           and data type are used in selecting  which segments to read.
C
C        3) It was specified that the angular velocity vector
C           gives the right-handed axis about which the instrument
C           frame rotates.
C
C-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
C
C        1) The example program was corrected so that the input
C           instrument code was tested against ICD(1) instead of
C           ICD(3).
C        2) SCLK was removed from the Required Reading section.
C
C-    Beta Version 1.1.0, 29-AUG-1990 (MJS) (JEM)
C
C        The following changes were made as a result of the
C        NAIF CK Code and Documentation Review:
C
C        1) The argument SCLK was removed from the calling sequence.
C        2) Header was updated.
C        3) The call to the routine QUAT2M_3 was replaced by a call to
C           the routine Q2M.
C
C-    Beta Version 1.0.0, 18-MAY-1990 (RET) (IMU)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN( 'CKE01' )
      END IF
 
C
C     Dissect the record.
C
      CLKOUT = RECORD( 1 )
 
      CALL Q2M ( RECORD( 2 ), CMAT )
 
      IF ( NEEDAV ) THEN
 
         AV( 1 ) = RECORD( 6 )
         AV( 2 ) = RECORD( 7 )
         AV( 3 ) = RECORD( 8 )
 
      END IF
 
      CALL CHKOUT( 'CKE01' )
      RETURN
      END
