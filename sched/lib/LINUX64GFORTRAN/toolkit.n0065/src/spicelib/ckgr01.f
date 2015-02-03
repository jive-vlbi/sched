C$Procedure      CKGR01 ( C-kernel, get record, type 01 )
 
      SUBROUTINE CKGR01 ( HANDLE, DESCR, RECNO, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Given the handle and descriptor of a data type 1 segment in a
C     CK file, return a specified pointing record from that segment.
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
C     RECNO      I   The number of the pointing record to be returned.
C     RECORD     O   The pointing record.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of the binary CK file containing the
C                desired segment. The file should have been opened
C                for read access, either by CKLPF or DAFOPR.
C
C     DESCR      is the packed descriptor of the data type 1 segment.
C
C     RECNO      is the number of the individual pointing record to be
C                returned from the data type 1 segment.
C
C$ Detailed_Output
C
C     RECORD     is the pointing record indexed by RECNO in the segment.
C                The contents are as follows:
C
C                   RECORD( 1 ) = CLKOUT
C
C                   RECORD( 2 ) = q0
C                   RECORD( 3 ) = q1
C                   RECORD( 4 ) = q2
C                   RECORD( 5 ) = q3
C
C                   RECORD( 6 ) = Av1  ]
C                   RECORD( 7 ) = Av2  |-- Returned optionally
C                   RECORD( 8 ) = Av3  ]
C
C                CLKOUT is the encoded spacecraft clock time associated
C                with the returned pointing values.
C
C                The quantities q0 - q3 represent a quaternion.
C                The quantities Av1, Av2, and Av3 represent the
C                angular velocity vector, and are returned only if the
C                segment contains angular velocity data. The
C                components of the angular velocity vector are
C                specified relative to the inertial reference
C                frame of the segment.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the segment is not of data type 1, the error
C         SPICE(CKWRONGDATATYPE) is signalled.
C
C     2)  If RECNO is less than one or greater than the number of
C         records in the specified segment, the error
C         SPICE(CKNONEXISTREC) is signalled.
C
C     3)  If the specified handle does not belong to any file that is
C         currently known to be open, an error is diagnosed by a
C         routine that this routine calls.
C
C     4)  If DESCR is not a valid, packed descriptor of a segment in
C         the CK file specified by HANDLE, the results of this routine
C         are unpredictable.
C
C$ Files
C
C     The file specified by HANDLE should be open for read access.
C
C$ Particulars
C
C     For a detailed description of the structure of a type 1 segment,
C     see the CK required reading.
C
C     This is a utility routine that performs as follows.  It finds out
C     how many records are in the segment, checks to see if the request
C     fits the bounds of the segment, and then moves directly to get
C     the requested data.
C
C$ Examples
C
C     The following code fragment prints the records of the first
C     segment in a CK file.  Suppose MOC.CK is valid CK file that
C     contains segments of data type 1.
C
C           INTEGER               ICD     ( 6 )
C           INTEGER               HANDLE
C           INTEGER               NREC
C           INTEGER               I
C           DOUBLE PRECISION      DCD     ( 2 )
C           DOUBLE PRECISION      DESCR   ( 5 )
C           DOUBLE PRECISION      RECORD  ( 8 )
C           LOGICAL               FOUND
C
C     C
C     C     First load the file. (The file may also be opened by using
C     C     CKLPF.)
C     C
C           CALL DAFOPR ( 'MOC.CK', HANDLE )
C
C     C
C     C     Begin forward search.  Find first array.
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
C           IF ( ICD( 3 ) .EQ. 1 ) THEN
C
C     C
C     C        How many records does this segment contain?
C     C
C              CALL CKNR01 ( HANDLE, DESCR, NREC )
C
C              DO I = 1, NREC
C
C     C
C     C           Get the record associated with record number I.
C     C
C                 CALL CKGR01 ( HANDLE, DESCR, I, RECORD )
C                 WRITE (*,*) 'Record ', I, ':'
C                 WRITE (*,*)  RECORD
C              END DO
C
C           END IF
C
C$ Restrictions
C
C     The binay CK file containing the segment whose descriptor was
C     passed to this routine must be opened for read access by either
C     CKLPF or DAFOPR.
C
C$ Literature_References
C
C     None.
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
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.2, 06-MAR-1991 (JML)
C
C        A correction was made to the example program in the
C        header.  The array of double precision components of
C        the descriptor ( DCD ) had originally been declared
C        as an integer.
C
C-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
C
C        The restriction that a C-kernel file must be loaded
C        was explicitly stated.
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
 
C$ Index_Entries
C
C     get ck type_1 record
C
C-&
 
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 07-SEP-2000 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C
C-    SPICELIB Version 1.0.2, 06-MAR-1991 (JML)
C
C        A correction was made to the example program in the
C        header.  The array of double precision components of
C        the descriptor ( DCD ) had originally been declared
C        as an integer.
C
C-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
C
C        1) The restriction that a C-kernel file must be loaded
C           was explicitly stated.
C        2) ROTATIONS was removed from the required reading section.
C        3) Minor changes were made to the wording of the header.
C
C
C-    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM)
C
C        The following changes were made as a result of the
C        NAIF CK Code and Documentation Review:
C
C        1) The name of this routine was changed from CK01GR to
C           CKGR01 in order to be consistent with the SPICELIB
C           naming convention.
C        2) The declarations for the parameters QSIZ, QAVSIZ, NDC, and
C           NIC were moved from the "Declarations" section of the
C           header to the "Local parameters" section of the code below
C           the header. These parameters are not meant to modified by
C           users.
C        3) The header was corrected, improved, and updated to reflect
C           the changes.
C        4) The in-code comments were improved.
C
C-    Beta Version 1.0.0, 23-MAY-1990 (RET) (IMU)
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
C        QSIZ       is the number of double precision numbers making up
C                   the quaternion portion of a pointing record.
C
C        QAVSIZ     is the number of double precision numbers making up
C                   the quaternion and angular velocity portion of a
C                   pointing record.
C
 
      INTEGER               NDC
      PARAMETER           ( NDC    = 2 )
 
      INTEGER               NIC
      PARAMETER           ( NIC    = 6 )
 
      INTEGER               QSIZ
      PARAMETER           ( QSIZ   = 4 )
 
      INTEGER               QAVSIZ
      PARAMETER           ( QAVSIZ = 7 )
 
C
C     Local variables
C
      INTEGER               ICD    ( NIC )
      INTEGER               NREC
      INTEGER               BEG
      INTEGER               ADDR
      INTEGER               PSIZ
 
      DOUBLE PRECISION      DCD    ( NDC )
      DOUBLE PRECISION      N
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKGR01' )
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
C       1 - Is this really a type 1 segment?
C       2 - The beginning address of the segment.
C       3 - The number of records in the segment (it's the last number
C           in the segment).
C       4 - The existence of angular velocity data, which determines how
C           big the pointing portion of the returned record will be.
C
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )
 
      IF ( ICD( 3 ) .NE. 1 ) THEN
         CALL SETMSG ( 'Data type of the segment should be 1: Passed '//
     .                 'descriptor shows type = #.'                    )
         CALL ERRINT ( '#', ICD( 3 )                                   )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                        )
         CALL CHKOUT ( 'CKGR01'                                        )
         RETURN
      END IF
 
      BEG  = ICD( 5 )
 
      CALL DAFGDA ( HANDLE, ICD( 6 ), ICD( 6 ), N )
      NREC = INT( N )
 
      IF ( ICD( 4 ) .EQ. 1 ) THEN
         PSIZ = QAVSIZ
      ELSE
         PSIZ = QSIZ
      END IF
 
C
C     If a request was made for a record which doesn't exist, then
C     signal an error and leave.
C
      IF ( RECNO .LT. 1 .OR. RECNO .GT. NREC ) THEN
         CALL SETMSG ( 'Requested record number (#) does not exist. ' //
     .                 'There are # records in the segment.'           )
         CALL ERRINT ( '#', RECNO                                      )
         CALL ERRINT ( '#', NREC                                       )
         CALL SIGERR ( 'SPICE(CKNONEXISTREC)'                          )
         CALL CHKOUT ( 'CKGR01'                                        )
         RETURN
      END IF
 
C
C     Get the pointing record indexed by RECNO.
C
      ADDR = BEG + PSIZ*( RECNO - 1 )
      CALL DAFGDA ( HANDLE, ADDR, ADDR + ( PSIZ - 1 ), RECORD( 2 ))
 
C
C     Next get the SCLK time.  Need to go past all of the NREC pointing
C     records (PSIZ * NREC numbers), and then to the RECNOth SCLK
C     time.
C
      ADDR = BEG + PSIZ*NREC + RECNO - 1
      CALL DAFGDA ( HANDLE, ADDR, ADDR, RECORD( 1 ) )
 
 
      CALL CHKOUT ( 'CKGR01' )
      RETURN
      END
