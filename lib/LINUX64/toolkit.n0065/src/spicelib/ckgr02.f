C$Procedure      CKGR02 ( C-kernel, get record, type 02 )
 
      SUBROUTINE CKGR02 ( HANDLE, DESCR, RECNO, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Given the handle and descriptor of a type 2 segment in a CK file,
C     return a specified pointing record from that segment.
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
C                for read or write access, either by CKLPF, DAFOPR,
C                or DAFOPW.
C
C     DESCR      is the packed descriptor of the data type 2 segment.
C
C     RECNO      is the number of the individual pointing record to be
C                returned from the data type 2 segment.
C
C$ Detailed_Output
C
C     RECORD     is the pointing record indexed by RECNO in the segment.
C                The contents are as follows:
C
C                   RECORD( 1  ) = start SCLK time of interval
C                   RECORD( 2  ) = end SCLK time of interval
C                   RECORD( 3  ) = seconds per tick rate
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
C
C                See the section on data type 2 in the CK Required
C                Reading for a complete description on how pointing
C                is obtained from a type 2 record.
C
C                Note that the RECORD returned by this routine is
C                slightly different from that returned by CKR02.
C                The second element of the record returned by CKR02
C                contains the SCLK time at which pointing was
C                requested, whereas this routine returns the SCLK
C                time of the right endpoint of the interval for which
C                the constant angular velocity model is valid.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the segment is not of data type 2, the error
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
C     4)  If DESCR is not a valid descriptor of a segment in the CK
C         file specified by HANDLE, the results of this routine are
C         unpredictable.
C
C$ Files
C
C     The file specified by HANDLE should be open for read or write
C     access.
C
C$ Particulars
C
C     For a detailed description of the structure of a type 2 segment,
C     see the CK Required Reading.
C
C     This is a utility routine that may be used to read the individual
C     pointing records that make up a data type 2 segment. It is
C     normally used in combination with CKNR02, which gives the number
C     of pointing instances stored in a segment.
C
C$ Examples
C
C     Suppose GLL_PLT.BC is a CK file that contains segments of data
C     type 2. Then the following code fragment uses CKNR02 and CKGR02
C     to extract each pointing record in the first segment in the file.
C
C
C           INTEGER               ICD     ( 6 )
C           INTEGER               HANDLE
C           INTEGER               NREC
C           INTEGER               I
C
C           DOUBLE PRECISION      DCD     ( 2  )
C           DOUBLE PRECISION      DESCR   ( 5  )
C           DOUBLE PRECISION      RECORD  ( 10 )
C
C           LOGICAL               FOUND
C
C     C
C     C     First load the file. (The file may also be opened by using
C     C     CKLPF.)
C     C
C           CALL DAFOPR ( 'GLL_PLT.BC', HANDLE )
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
C           IF ( ICD( 3 ) .EQ. 2 ) THEN
C
C     C
C     C        How many records does this segment contain?
C     C
C              CALL CKNR02 ( HANDLE, DESCR, NREC )
C
C              DO I = 1, NREC
C
C     C
C     C           Get the Ith record in the segment.
C     C
C                 CALL CKGR02 ( HANDLE, DESCR, I, RECORD )
C     C
C     C           Process the pointing data.
C     C
C                 .
C                 .
C                 .
C
C              END DO
C
C           END IF
C
C$ Restrictions
C
C     1) The binary CK file containing the segment whose descriptor was
C        passed to this routine must be opened for read or write access
C        by either CKLPF, DAFOPR, or DAFOPW.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J.M. Lynch (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
C
C-&
 
C$ Index_Entries
C
C     get ck type_2 record
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
C        PSIZ       is the number of double precision numbers making up
C                   the quaternion, angular velocity, and seconds per
C                   tick rate portion of a pointing record.
C
C        DTYPE      is the data type.
C
 
      INTEGER               NDC
      PARAMETER           ( NDC    = 2 )
 
      INTEGER               NIC
      PARAMETER           ( NIC    = 6 )
 
      INTEGER               PSIZ
      PARAMETER           ( PSIZ   = 8 )
 
      INTEGER               DTYPE
      PARAMETER           ( DTYPE  = 2 )
 
C
C     Local variables
C
      INTEGER               ICD    ( NIC )
      INTEGER               NREC
      INTEGER               BEG
      INTEGER               ADDR
 
      DOUBLE PRECISION      DCD    ( NDC  )
      DOUBLE PRECISION      PREC   ( PSIZ )
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKGR02' )
      END IF
 
 
C
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
 
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )
 
      IF ( ICD( 3 ) .NE. DTYPE ) THEN
         CALL SETMSG ( 'Data type of the segment should be 2: Passed '//
     .                 'descriptor shows type = #.'                    )
         CALL ERRINT ( '#', ICD( 3 )                                   )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                        )
         CALL CHKOUT ( 'CKGR02'                                        )
         RETURN
      END IF
 
 
C
C     Find out how many pointing instances there are in the segment.
C
      CALL CKNR02 ( HANDLE, DESCR, NREC )
 
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
         CALL CHKOUT ( 'CKGR02'                                        )
         RETURN
      END IF
 
C
C     The address of the first double precision number in the array
C     is stored in the fifth integer component of the descriptor.
C
      BEG  = ICD( 5 )
 
 
C
C     Get the pointing record indexed by RECNO.
C
      ADDR = BEG + PSIZ * ( RECNO - 1 )
 
      CALL DAFGDA ( HANDLE, ADDR, ADDR + ( PSIZ - 1 ), PREC )
 
      RECORD ( 3 ) = PREC ( PSIZ )
 
      CALL MOVED ( PREC, 7, RECORD ( 4 ) )
 
C
C     Next get the interval start time.  Need to go past all of the
C     NREC pointing records (PSIZ * NREC numbers), and then to the
C     RECNOth SCLK start time.
C
      ADDR = BEG  +  PSIZ * NREC  +  RECNO  -  1
 
      CALL DAFGDA ( HANDLE, ADDR, ADDR, RECORD( 1 ) )
 
C
C     Next get the interval stop time.  Need to go past all of the
C     NREC pointing records and start times ( (PSIZ+1)*NREC numbers ),
C     and then to the RECNOth SCLK stop time.
C
      ADDR = BEG  +  ( PSIZ + 1 ) * NREC  +  RECNO  -  1
 
      CALL DAFGDA ( HANDLE, ADDR, ADDR, RECORD( 2 ) )
 
      CALL CHKOUT ( 'CKGR02' )
      RETURN
      END
