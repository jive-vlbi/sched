C$Procedure      CKGR05 ( C-kernel, get record, type 05 )
 
      SUBROUTINE CKGR05 ( HANDLE, DESCR, RECNO, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Given the handle and descriptor of a type 5 segment in a CK file,
C     return a specified pointing instance from that segment.
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
 
      INCLUDE 'ck05.inc'

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR   ( * )
      INTEGER               RECNO
      DOUBLE PRECISION      RECORD  ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of the file containing the segment.
C     DESCR      I   The segment descriptor.
C     RECNO      I   The number of the pointing instance to be returned.
C     RECORD     O   The pointing record.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of the binary CK file containing the
C                desired segment.
C
C     DESCR      is the packed descriptor of the data type 5 segment.
C
C     RECNO      is the number of the discrete pointing instance to be
C                returned from the data type 5 segment.
C
C$ Detailed_Output
C
C     RECORD     is the pointing instance indexed by RECNO in the
C                segment.  The contents are as follows:
C
C                   RECORD( 1 ) = CLKOUT
C
C                CLKOUT is the encoded spacecraft clock time associated
C                with the returned pointing values.
C
C                   RECORD( 2 ) = SUBTYP
C
C                SUBTYP is the CK type 5 subtype code.  This code
C                identifies the structure and meaning of the rest
C                of the record.  However, all subtypes have a 
C                quaternion stored in elements 3-6.
C
C                   RECORD( 3 ) = q0
C                   RECORD( 4 ) = q1
C                   RECORD( 5 ) = q2
C                   RECORD( 6 ) = q3
C
C                Subtype 1 ends here; there are no angular velocity
C                data.  Angular velocity is derived by differentiating
C                Lagrange interpolating polynomials.
C
C                   RECORD(  7 ) =  ]
C                   RECORD(  8 ) =  ] --- For subtypes 0 and 2, these 
C                   RECORD(  9 ) =  ]     elements contain a quaternion 
C                   RECORD( 10 ) =  ]     derivative.  For subtype 3,
C                                         elements 7-9 contain an
C                                         angular velocity vector;
C                                         element 10 is unassigned. 
C
C                                         All subtypes except subtype
C                                         2 stop here.
C
C                   RECORD( 11 ) =  ]
C                   RECORD( 12 ) =  ] --- For subtype 2, these 
C                   RECORD( 13 ) =  ]     elements contain an angular 
C                                         velocity vector.
C
C
C                   RECORD( 14 ) =  ]
C                   RECORD( 15 ) =  ] --- For subtype 2, these 
C                   RECORD( 16 ) =  ]     elements contain the 
C                                         derivative of an angular 
C                                         velocity vector.
C
C                The quantities q0 - q3 are the components of the
C                quaternion that represents the C-matrix that transforms
C                vectors from the inertial reference frame of the
C                segment to the instrument frame at time CLKOUT.
C
C                Quaternion derivatives, angular velocity, or the
C                derivative of angular velocity are returned only
C                these are supported by the segment subtype and 
C                if the segment descriptor indicates that angular
C                velocity is present.
C                 
C                The components of the angular velocity vector are
C                specified relative to the inertial reference frame of
C                the segment.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the segment is not of data type 5, the error
C         SPICE(CKWRONGDATATYPE) is signaled.
C
C     2)  If RECNO is less than one or greater than the number of
C         records in the specified segment, the error
C         SPICE(CKNONEXISTREC) is signaled.
C
C     3)  If the specified handle does not belong to any DAF file that
C         is currently known to be open, an error is diagnosed by a
C         routine that this routine calls.
C
C     4)  If DESCR is not a valid descriptor of a segment in the CK
C         file specified by HANDLE, the results of this routine are
C         unpredictable.
C
C     5)  If the segment subtype is not recognized, the error 
C         SPICE(NOTSUPPORTED) is signaled.
C
C$ Files
C
C     The file specified by HANDLE should be open for read or
C     write access.
C
C$ Particulars
C
C     For a detailed description of the structure of a type 5 segment,
C     see the CK required reading.
C
C     This is a utility routine that may be used to read the individual
C     pointing instances that make up a type 5 segment.  It is normally
C     used in conjunction with CKNR05, which gives the number of
C     pointing instances stored in a segment.
C
C$ Examples
C
C     Suppose that MOC.BC is a CK file that contains segments of
C     data type 5.  Then the following code fragment extracts the
C     SCLK time and boresight vector for each pointing instance 
C     in the first segment in the file.
C
C
C           INTEGER               ICD     ( 6 )
C           INTEGER               HANDLE
C           INTEGER               NREC
C           INTEGER               I
C
C           DOUBLE PRECISION      DCD     ( 2 )
C           DOUBLE PRECISION      DESCR   ( 5 )
C           DOUBLE PRECISION      RECORD  ( 16 )
C           DOUBLE PRECISION      QUAT    ( 4 )
C           DOUBLE PRECISION      BORE    ( 3 )
C           DOUBLE PRECISION      CMAT    ( 3, 3 )
C           DOUBLE PRECISION      SCLKDP
C
C           LOGICAL               FOUND
C
C     C
C     C     First load the file. (The file may also be opened by using
C     C     CKLPF.)
C     C
C           CALL DAFOPR ( 'MOC.BC', HANDLE )
C
C     C
C     C     Begin forward search.  Find the first array.
C     C
C           CALL DAFBFS ( HANDLE )
C           CALL DAFFNA ( FOUND  )
C
C     C
C     C     Get segment descriptor.
C     C
C           CALL DAFGS ( DESCR )
C
C     C
C     C     Unpack the segment descriptor into its double precision
C     C     and integer components.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C
C     C
C     C     The data type for a segment is located in the third integer
C     C     component of the descriptor.
C     C
C           IF ( ICD( 3 ) .EQ. 5 ) THEN
C     C
C     C        How many records does this segment contain?
C     C
C              CALL CKNR05 ( HANDLE, DESCR, NREC )
C
C              DO I = 1, NREC
C     C
C     C           Get the Ith pointing instance in the segment.
C     C
C                 CALL CKGR05 ( HANDLE, DESCR, I, RECORD )
C
C     C
C     C           Unpack from RECORD the time tag and quaternion.
C     C           The locations of these items in the record are
C     C           independent of the subtype.
C     C
C                 SCLKDP = RECORD ( 1 )
C
C                 CALL MOVED ( RECORD(3), 4, QUAT )
C
C     C
C     C           The boresight vector is the third row of the C-matrix.
C     C
C                 CALL Q2M ( QUAT, CMAT )
C
C                 BORE(1) = CMAT(3,1)
C                 BORE(2) = CMAT(3,2)
C                 BORE(3) = CMAT(3,3)
C     C
C     C           Write out the results.
C     C
C                 WRITE (*,*) 'Record: ', I
C                 WRITE (*,*)
C                 WRITE (*,*) 'SCLK time = ', SCLKDP
C                 WRITE (*,*)
C                 WRITE (*,*) 'boresight: ', BORE
C
C              END DO
C
C           END IF
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
C     N.J. Bachman (JPL)
C     J.M. Lynch (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 27-AUG-2002 (NJB) (JML)
C
C-&
 
C$ Index_Entries
C
C     get ck type_5 record
C
C-&
 
 
 
 
 
 
C
C     SPICELIB functions
C
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
C        DTYPE      is the data type of the segment that this routine
C                   operates on.
C
C
 
      INTEGER               NDC
      PARAMETER           ( NDC    = 2 )
 
      INTEGER               NIC
      PARAMETER           ( NIC    = 6 )
  
      INTEGER               DTYPE
      PARAMETER           ( DTYPE  = 5 )
 
C
C     Local variables
C
      INTEGER               ICD    ( NIC )
      INTEGER               NREC
      INTEGER               BEG
      INTEGER               END
      INTEGER               ADDR
      INTEGER               PACKSZ
      INTEGER               SUBTYP
 
      DOUBLE PRECISION      DCD    ( NDC )
      DOUBLE PRECISION      NPOINT
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKGR05' )
      END IF
 
C
C     The unpacked descriptor contains the following information
C     about the segment:
C
C        DCD(1)  Initial encoded SCLK
C        DCD(2)  Final encoded SCLK
C        ICD(1)  Instrument
C        ICD(2)  Inertial reference frame
C        ICD(3)  Data type
C        ICD(4)  Angular velocity flag
C        ICD(5)  Initial address of segment data
C        ICD(6)  Final address of segment data
C
C     From the descriptor, determine
C
C       1 - Is this really a type 5 segment?
C       2 - The beginning address of the segment.
C       3 - The number of pointing instances in the segment (it's the
C           last word in the segment).
C       4 - The existence of angular velocity data, which determines how
C           big the pointing portion of the returned record will be.
C
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )
 
      IF ( ICD( 3 ) .NE. DTYPE ) THEN

         CALL SETMSG ( 'Data type of the segment should be 5: Passed '//
     .                 'descriptor shows type = #.'                    )
         CALL ERRINT ( '#', ICD( 3 )                                   )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                        )
         CALL CHKOUT ( 'CKGR05'                                        )
         RETURN

      END IF

C
C     Capture the segment's address range.
C
      BEG  = ICD( 5 )
      END  = ICD( 6 )

C
C     Read the subtype from the segment.
C
      CALL DAFGDA ( HANDLE, END-3, END-3, RECORD(2) )

      SUBTYP = RECORD(2)
 
      IF ( SUBTYP .EQ. C05TP0 ) THEN

         PACKSZ = C05PS0

      ELSE IF ( SUBTYP .EQ. C05TP1 ) THEN

         PACKSZ = C05PS1

      ELSE IF ( SUBTYP .EQ. C05TP2 ) THEN

         PACKSZ = C05PS2

      ELSE IF ( SUBTYP .EQ. C05TP3 ) THEN

         PACKSZ = C05PS3

      ELSE
         
         CALL SETMSG ( 'Unexpected CK type 5 subtype # found in ' //
     .                 'type 5 segment.'                          )
         CALL ERRINT ( '#',  SUBTYP                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'CKGR05'                                   )
         RETURN
      
      END IF
   
      CALL DAFGDA ( HANDLE, END, END, NPOINT )
 
      NREC = NINT ( NPOINT )
 
 
C
C     If a request was made for a record which doesn't exist, then
C     signal an error and leave.
C
      IF ( ( RECNO .LT. 1 ) .OR. ( RECNO .GT. NREC ) ) THEN
         CALL SETMSG ( 'Requested record number (#) does not exist. ' //
     .                 'There are # records in the segment.'           )
         CALL ERRINT ( '#', RECNO                                      )
         CALL ERRINT ( '#', NREC                                       )
         CALL SIGERR ( 'SPICE(CKNONEXISTREC)'                          )
         CALL CHKOUT ( 'CKGR05'                                        )
         RETURN
      END IF
 
C
C     Get the pointing record indexed by RECNO.
C
      ADDR = BEG + PACKSZ * ( RECNO - 1 )
 
      CALL DAFGDA ( HANDLE, ADDR, ADDR + PACKSZ - 1, RECORD( 3 ) )
 
C
C     Next get the SCLK time.  Need to go past all of the NREC pointing
C     records (PACKSZ * NREC numbers), and then to the RECNOth SCLK
C     time.
C
      ADDR = BEG + PACKSZ*NREC + RECNO - 1
 
      CALL DAFGDA ( HANDLE, ADDR, ADDR, RECORD( 1 ) )
 
      CALL CHKOUT ( 'CKGR05' )
      RETURN
      END
