C$Procedure      CKPFS ( C-kernel, get pointing from segment )
 
      SUBROUTINE CKPFS ( HANDLE, DESCR, SCLKDP, TOL,  NEEDAV,
     .                   CMAT,   AV,    CLKOUT, FOUND         )
 
C$ Abstract
C
C     Evaluate pointing data from a segment for a given time.
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
C
C$ Keywords
C
C     POINTING
C
C$ Declarations

      INCLUDE               'ckparam.inc'
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( *    )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TOL
      LOGICAL               NEEDAV
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      AV     ( 3    )
      DOUBLE PRECISION      CLKOUT
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   CK file handle.
C     DESCR      I   Segment descriptor.
C     SCLKDP     I   Spacecraft clock time.
C     TOL        I   Time tolerance.
C     NEEDAV     I   True when angular velocity data is requested.
C     CMAT       O   C-matrix.
C     AV         O   Angular velocity vector.
C     CLKOUT     O   Output spacecraft clock time.
C     FOUND      O   True when requested pointing is available.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of the binary CK file containing the
C                desired segment. The file should have been opened
C                for read access, either by CKLPF or DAFOPR.
C
C     DESCR      is the packed descriptor of the segment.
C
C     SCLKDP     is the encoded spacecraft clock time for which
C                pointing is desired.
C
C     TOL        is a time tolerance, measured in the same units as
C                encoded spacecraft clock.  The C-matrix returned by
C                CKPFS is the one whose time is closest to SCLKDP and
C                within TOL units of SCLKDP.
C
C     NEEDAV     is true when angular velocity data is requested.
C
C
C$ Detailed_Output
C
C     CMAT       is a rotation matrix that transforms the components of
C                of a vector expressed in the reference frame given in
C                the segment to components expressed in the instrument
C                fixed frame at time CLKOUT.
C
C                Thus, if a vector v has components x, y, z in the
C                CK base frame, then v has components x', y', z' in
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
C                             (Transpose of CMAT)
C
C     AV         is the angular velocity vector. This is returned only
C                if it has been requested, as indicated by NEEDAV. In
C                other words, if NEEDAV is true, then the pointing
C                records in the segment must contain AV data.
C
C                The angular velocity vector is the right-handed axis
C                about which the reference frame tied to the instrument
C                is instantaneously rotating at time CLKOUT. The
C                magnitude of AV is the magnitude of the instantaneous
C                velocity of the rotation, in radians per second.
C
C                The components of AV are given relative to the
C                reference frame specified in the segment descriptor.
C
C     CLKOUT     is the encoded spacecraft clock time associated with
C                the returned C-matrix and, optionally, the returned
C                angular velocity vector.
C
C     FOUND      is true if a C-matrix and an angular velocity vector
C                (if requested) were found to satisfy the pointing
C                request. FOUND will be false otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the data type of the segment is not one of those supported
C         by this routine, the error SPICE(CKUNKNOWNDATATYPE) is
C         signaled.
C
C     2)  If the specified handle does not belong to any file that is
C         currently known to be open, an error is diagnosed by a
C         routine that this routine calls.
C
C     3)  If DESCR is not a valid, packed descriptor of a segment in
C         the CK file specified by HANDLE, the results of this routine
C         are unpredictable.
C
C     4)  If TOL is negative, FOUND is false.
C
C     5)  If NEEDAV is true, but the segment doesn't contain AV data,
C         an error is signaled by a routine that this routine calls.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The structure of this routine is just a big case statement. Each
C     segment data type is supported by two routines:
C
C        CKRnn   which reads a single logical pointing record from a
C                segment of type nn.  (A logical record is defined as
C                a collection of numbers sufficient to determine the
C                C-matrix, and optionally the angular velocity vector,
C                at the input time.)
C
C        CKEnn   which evaluates the pointing record returned by CKRnn
C                to give the C-matrix and optionally the angular
C                velocity vector at the input time.
C
C     The data type is determined from the segment descriptor, and the
C     appropriate routines are called.
C
C$ Examples
C
C     CKPFS allows you to be more selective than CKGP or CKGPAV about
C     choosing segments to satisfy CK pointing requests.
C
C     Suppose MOC.BC is a CK file consisting of several segments
C     containing Mars Observer Camera pointing data. Each segment
C     covers the same time period, but produces different pointing
C     values (one segment may contain predict values, another may
C     contain telemetry-based values, and others may contain different
C     corrected versions).
C
C     The following code fragment shows how different the results are
C     for each segment. The program steps through the file segment by
C     segment and requests pointing for the same time from each 
C     segment. The results are printed to the screen.
C
C     GETIME is an imaginary routine used to get an encoded SCLK time
C     (SCLKDP) and time tolerance from the user.
C
C           SC     = -94
C           INST   = -94001
C           NEEDAV = .TRUE.
C
C           CALL CKLPF ( 'MOC.BC', HANDLE )
C
C           CALL GETIME ( SCLKDP, TOL, QUIT )
C
C     C
C     C     For each time, begin a forward search through the file, and
C     C     for each segment found, get its descriptor, identifier, and
C     C     evaluate the pointing.
C     C
C           DO WHILE ( .NOT. QUIT )
C
C              CALL DAFBFS ( HANDLE )
C              CALL DAFFNA ( FOUND  )
C
C              DO WHILE ( FOUND )
C
C                 CALL DAFGS ( DESCR )
C                 CALL DAFGN ( IDENT )
C
C                 CALL CKPFS ( HANDLE, DESCR, SCLKDP, TOL,   NEEDAV,
C          .                   CMAT,   AV,    CLKOUT, PFOUND         )
C
C                 IF ( PFOUND ) THEN
C                    WRITE (*,*) 'Segment:          ', IDENT
C                    WRITE (*,*) 'C-Matrix:         ', CMAT
C                    WRITE (*,*) 'Angular velocity: ', AV
C
C                 ELSE
C                    CALL SCDECD ( SC, SCLKDP, SCLKCH )
C                    WRITE (*,*) 'Data not found at time ', SCLKCH
C
C                 END IF
C
C                 CALL DAFFNA ( FOUND )
C
C              END DO
C
C              CALL GETIME ( SCLKDP, TOL, QUIT )
C
C           END DO
C
C
C$ Restrictions
C
C     A C-kernel file should have been loaded by either CKLPF
C     or DAFOPR.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J.M. Lynch     (JPL)
C     B.V. Semenov   (JPL)
C     M.J. Spencer   (JPL)
C     R.E. Thurman   (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.0.0, 24-MAR-2014 (NJB)
C
C        Bug fix: this routine now sets the output FOUND to 
C        .FALSE. if a SPICE error is detected.
C     
C        The routine was updated to handle data type 6 segments. 
C        Several comment typos were corrected.
C
C-    SPICELIB Version 5.0.0, 19-AUG-2002 (NJB)
C
C        The routine was updated to handle data type 5 segments. 
C
C-    SPICELIB Version 4.0.0, 02-MAY-1999 (BVS)
C
C        The routine was updated to handle data type 4 segments. 
C        The RECSIZ size parameter was eliminated. The dimension 
C        of the RECORD buffer is now defined by the CKMRSZ parameter
C        specified in the 'ckparam.inc' include file.
C
C-    SPICELIB Version 3.0.0, 11-SEP-1992 (JML)
C
C        The routine was updated to handle data type 3 segments.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
C
C         The routine was updated to handle data type 2 segments.
C
C         FOUND is now initialized to false.
C
C-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
C
C         The restriction that a C-kernel file must be loaded
C         was explicitly stated.
C
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
 
C$ Index_Entries
C
C     get pointing from ck segment
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 6.0.0, 01-FEB-2014 (NJB)
C
C        The routine was updated to handle data type 6 segments. 
C
C-    SPICELIB Version 5.0.0, 19-AUG-2002 (NJB)
C
C        The routine was updated to handle data type 5 segments. 
C
C-    SPICELIB Version 4.0.0, 02-MAY-1999 (BVS)
C
C        The routine was updated to handle data type 4 segments.
C
C           a) 'ckparam.inc' include file was included.
C
C           b) RECSIZ size parameter was eliminated.
C
C           c) Size of the RECORD was reset to CKMRSZ, parameter
C              defined in the 'ckparam.inc' include file.
C
C           d) Calls to CKR04 and CKE04 were added to the case
C              statement.
C
C-    SPICELIB Version 3.0.0, 11-SEP-1992 (JML)
C
C        The routine was updated to handle data type 3 segments.
C
C           a) RECSIZ was increased to 17.
C
C           b) Calls to CKR03 and CKE03 were added to the case
C              statement.
C
C-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
C
C        1) The routine was updated to handle data type 2 segments.
C
C        2) FOUND is initialized to false to guard against it being
C           left unchanged from its previous value when an error is
C           detected.
C
C-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
C
C        1) The restriction that a C-kernel file must be loaded
C           was explicitly stated.
C
C-    Beta Version 1.1.0, 30-AUG-1990 (MJS)
C
C        The following changes were made as a result of the
C        NAIF CK Code and Documentation Review:
C
C        1) The variable SCLK was changed to SCLKDP.
C        2) The declarations for the parameters RECSIZ, NDC, and NIC
C           were moved from the "Declarations" section of the header
C           to the "Local parameters" section of the code below the
C           header. These parameters are not meant to modified by
C           users.
C        3) The header was updated.
C        4) The comments in the code were improved.
C
C-    Beta Version 1.0.0, 07-MAY-1990 (RET) (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
C        NDC        is the number of double precision components in an
C                   unpacked C-kernel segment descriptor.
C
C        NIC        is the number of integer components in an unpacked
C                   C-kernel segment descriptor.
C
      INTEGER               NDC
      PARAMETER           ( NDC    = 2  )
 
      INTEGER               NIC
      PARAMETER           ( NIC    = 6  )
 
C
C     Local variables
C
      INTEGER               ICD    ( NIC    )
      INTEGER               TYPE
 
      DOUBLE PRECISION      DCD    ( NDC    )
      DOUBLE PRECISION      RECORD ( CKMRSZ )
  
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKPFS' )
      END IF
 
C
C     Start off with FOUND set to false. 
C
      FOUND  = .FALSE.
 
C
C     Upgrading CKPFS to accommodate new data types involves following
C     these steps:
C
C     1)  Write the two new routines CKRnn and CKEnn. (You may need to
C         add or subtract from the arguments used in the existing CKRnn
C         and CKEnn calling sequences, but should not have to change
C         the inputs or outputs to CKPFS.)
C
C     2)  Insert a new case into the code of CKPFS.
C
C     3)  Depending on the size of RECORD returned from CKRnn, modify
C         the parameter RECSIZ.  (You will only need to change it if
C         RECSIZ is not large enough for the new CKRnn's RECORD.)
C
 
C
C     Unpack the descriptor to see what the data type of the segment is,
C     and call the appropriate read-and-evaluate routines.
C
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )
 
      TYPE = ICD( 3 )
 
      IF ( TYPE .EQ. 1 ) THEN
 
         CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, RECORD,
     .                FOUND )
 
         IF ( FOUND ) THEN
            CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF
 
      ELSE  IF ( TYPE .EQ. 2 ) THEN
 
         CALL CKR02 ( HANDLE, DESCR, SCLKDP, TOL, RECORD, FOUND )
 
         IF ( FOUND ) THEN
            CALL CKE02 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF
 
      ELSE  IF ( TYPE .EQ. 3 ) THEN
 
         CALL CKR03 (HANDLE, DESCR, SCLKDP, TOL, NEEDAV, RECORD, FOUND)
 
         IF ( FOUND ) THEN
            CALL CKE03 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF
 
      ELSE  IF ( TYPE .EQ. 4 ) THEN
 
         CALL CKR04 (HANDLE, DESCR, SCLKDP, TOL, NEEDAV, RECORD, FOUND)
 
         IF ( FOUND ) THEN
            CALL CKE04 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF
 
      ELSE  IF ( TYPE .EQ. 5 ) THEN
 
         CALL CKR05 (HANDLE, DESCR, SCLKDP, TOL, NEEDAV, RECORD, FOUND)
 
         IF ( FOUND ) THEN
            CALL CKE05 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF

      ELSE  IF ( TYPE .EQ. 6 ) THEN
 
         CALL CKR06 (HANDLE, DESCR, SCLKDP, TOL, NEEDAV, RECORD, FOUND)
 
         IF ( FOUND ) THEN
            CALL CKE06 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF

      ELSE
   
         CALL SETMSG ( 'The data type # is not currently supported.' )
         CALL ERRINT ( '#', TYPE                                     )
         CALL SIGERR ( 'SPICE(CKUNKNOWNDATATYPE)'                    )
 
      END IF
 
C
C     In case an evaluator signaled an error, we check the SPICE
C     error status here. If a SPICE error occurred, indicate no
C     data were found.
C
      IF ( FAILED() ) THEN
         FOUND = .FALSE.
      END IF
      
      CALL CHKOUT ( 'CKPFS' )
      RETURN 
      END
