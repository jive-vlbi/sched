C$Procedure      CKGR04 ( C-kernel, get record, type 04 )

      SUBROUTINE CKGR04 ( HANDLE, DESCR, RECNO, RECORD )

C$ Abstract
C
C     Given the handle and descriptor of a type 4 segment in 
C     a CK file, return a specified pointing record from that 
C     segment.
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

      IMPLICIT              NONE
      
      INCLUDE               'ckparam.inc'

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
C     DESCR      is the packed descriptor of the data type 4 segment.
C
C     RECNO      is the number of the pointing record to be returned
C                from the data type 4 segment.
C
C$ Detailed_Output
C
C     RECORD     is the pointing record indexed by RECNO in the
C                segment. The contents of the record are as follows:
C
C                ---------------------------------------------------
C                | The midpoint of the approximation interval      |
C                ---------------------------------------------------
C                | The radius of the approximation interval        |
C                ---------------------------------------------------
C                | Number of coefficients for q0                   |
C                ---------------------------------------------------
C                | Number of coefficients for q1                   |
C                ---------------------------------------------------
C                | Number of coefficients for q2                   |
C                ---------------------------------------------------
C                | Number of coefficients for q3                   |
C                ---------------------------------------------------
C                | Number of coefficients for AV1                  |
C                ---------------------------------------------------
C                | Number of coefficients for AV2                  |
C                ---------------------------------------------------
C                | Number of coefficients for AV3                  |
C                ---------------------------------------------------
C                | q0 Cheby coefficients                           |
C                ---------------------------------------------------
C                | q1 Cheby coefficients                           |
C                ---------------------------------------------------
C                | q2 Cheby coefficients                           |
C                ---------------------------------------------------
C                | q3 Cheby coefficients                           |
C                ---------------------------------------------------
C                | AV1 Cheby coefficients (optional)               |
C                ---------------------------------------------------
C                | AV2 Cheby coefficients (optional)               |
C                ---------------------------------------------------
C                | AV3 Cheby coefficients (optional)               |
C                ---------------------------------------------------
C
C$ Parameters
C
C     See 'ckparam.inc'.
C
C$ Exceptions
C
C     1)  If the segment is not of data type 4, the error
C         SPICE(CKWRONGDATATYPE) is signalled.
C
C     2)  If RECNO is less than one or greater than the number of
C         records in the specified segment, the error
C         SPICE(CKNONEXISTREC) is signalled.
C
C     3)  If the specified handle does not belong to any DAF file that
C         is currently known to be open, an error is diagnosed by a
C         routine that this routine calls.
C
C     4)  If DESCR is not a valid descriptor of a segment in the CK
C         file specified by HANDLE, the results of this routine are
C         unpredictable.
C
C$ Files
C
C     The file specified by HANDLE should be open for read or
C     write access.
C
C$ Particulars
C
C     For a detailed description of the structure of a type 4 segment,
C     see the CK required reading.
C
C     This is a utility routine that may be used to read the individual
C     pointing records that make up a type 4 segment. It is normally
C     used in conjunction with CKNR04, which gives the number of
C     pointing records stored in a segment.
C
C$ Examples
C
C     Suppose that DATA.BC is a CK file that contains segments of
C     data type 4. Then the following code fragment extracts the
C     data packets contained in the segment.
C
C     C
C     C     CK parameters include file.
C     C
C           INCLUDE               'ckparam.inc'
C     C
C     C     Declarations.
C     C
C           DOUBLE PRECISION      DCD    ( 2 )
C           DOUBLE PRECISION      DESCR  ( 5 )
C           DOUBLE PRECISION      PKTDAT ( CK4RSZ )
C
C           INTEGER               AVFLAG
C           INTEGER               HANDLE
C           INTEGER               I
C           INTEGER               ICD    ( 6 )
C           INTEGER               K
C           INTEGER               LASTAD
C           INTEGER               NCOEF  ( QAVSIZ )
C           INTEGER               NREC
C
C           LOGICAL               FOUND
C     C
C     C     First load the file. (The file may also be opened by using
C     C     CKLPF.)
C     C
C           CALL DAFOPR ( 'DATA.BC', HANDLE )
C     C
C     C     Begin forward search. Find the first array.
C     C
C           CALL DAFBFS ( HANDLE )
C           CALL DAFFNA ( FOUND  )
C     C
C     C     Get segment descriptor.
C     C
C           CALL DAFGS ( DESCR )
C     C
C     C     Unpack the segment descriptor into its double precision
C     C     and integer components.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C
C           IF ( ICD( 3 ) .EQ. 4 ) THEN
C     C
C     C        How many records does this segment contain?
C     C
C              CALL CKNR04 ( HANDLE, DESCR, NREC )
C
C              DO I = 1, NREC
C     C
C     C           Get the data records stored in the segment.
C     C
C                 CALL CKGR04 ( HANDLE, DESCR, I, PKTDAT )
C     C
C     C           Print data packet contents. Print coverage interval 
C     C           midpoint & radii first.
C     C
C                 WRITE (2,*) PKTDAT (1)
C                 WRITE (2,*) PKTDAT (2)
C     C
C     C           Decode numbers of coefficients.
C     C
C                 CALL ZZCK4D2I ( PKTDAT(3), QAVSIZ, CK4PCD, NCOEF )
C     C
C     C           Print number of coefficients for Q0, Q1, Q2 and Q3.
C     C
C                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 1 ), NCOEF( 2 ) 
C                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 3 ), NCOEF( 4 )
C     C
C     C           Print number coefficients for AV1, AV2 and AV3.
C     C
C                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 5 ), NCOEF( 6 )
C                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 7 )
C     C
C     C           Print Cheby coefficients.
C     C
C                 LASTAD = 0
C
C                 DO K = 1, QAVSIZ
C                    LASTAD = LASTAD + NCOEF( K )
C                 END DO
C
C                 DO K = 4, LASTAD + 4
C                    WRITE (2,*) PKTDAT (K)
C                 END DO
C
C              END DO
C
C           END IF
C
C$ Restrictions
C
C     1) The binary CK file containing the segment whose descriptor
C        was passed to this routine must be opened for read or write
C        access by either CKLPF, DAFOPR, or DAFOPW.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     Y.K. Zaiko     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 18-APR-2014 (BVS)
C
C        Minor header edits.
C
C-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
C
C-&

C$ Index_Entries
C
C     get CK type_4 record
C
C-&
C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               NDC
      PARAMETER           ( NDC    = 2 )

      INTEGER               NIC
      PARAMETER           ( NIC    = 6 )

C
C     Length (in DPs) of non-coefficient front part of RECORD when
C     it contains decoded numbers of coefficients. It is one less 
C     than the length of the same part in a record exchanged between
C     CKR04 and CKE04 because it doesn't contain time at which 
C     pointing has to be evaluated.
C
      INTEGER                 SHFTAD
      PARAMETER             ( SHFTAD = CK4SFT - 1 )  
      
C
C     Local variables
C
      DOUBLE PRECISION      DCD    ( NDC )
      
      INTEGER               ENDS   ( 1 )
      INTEGER               ICD    ( NIC )
      INTEGER               K
      INTEGER               NREC
      INTEGER               NUMALL
      INTEGER               NUMCFT ( QAVSIZ )

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKGR04' )
      END IF

C
C     Unpack descriptor and check segment data type. Signal an error 
C     if it's not 4.
C
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )

      IF ( ICD( 3 ) .NE. CK4DTP ) THEN
         CALL SETMSG ( 'Data type of the segment should be 4: '      //
     .                 'Passed  descriptor shows type = #.'          )
         CALL ERRINT ( '#', ICD( 3 )                                 )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                      )
         CALL CHKOUT ( 'CKGR04'                                      )
         RETURN
      END IF

C
C     If a request was made for a data record which doesn't
C     exist, then signal an error and leave.
C
      CALL CKNR04 ( HANDLE, DESCR, NREC )

      IF ( ( RECNO .LT. 1 ) .OR. ( RECNO .GT. NREC ) ) THEN
         CALL SETMSG ( 'Requested record number (#) does not '       //
     .                 'exist. There are # records in the segment.'  )
         CALL ERRINT ( '#', RECNO                                    )
         CALL ERRINT ( '#', NREC                                     )
         CALL SIGERR ( 'SPICE(CKNONEXISTREC)'                        )
         CALL CHKOUT ( 'CKGR04'                                      )
         RETURN
      END IF

C
C     Get the data record indexed by RECNO.
C
      CALL SGFPKT ( HANDLE, DESCR, RECNO, RECNO, RECORD, ENDS )

C
C     Decode 7 numbers of coefficients from double precision value.
C
      CALL ZZCK4D2I ( RECORD( 3 ), QAVSIZ, CK4PCD, NUMCFT )
 
C
C     Compute total number of coefficients in the fetched packet.
C
      NUMALL = 0
      
      DO K = 1, QAVSIZ
         NUMALL = NUMALL + NUMCFT ( K )
      END DO
      
C
C     Move polynomial coefficients to the right to free space for 
C     decoded numbers of coefficients and insert these numbers 
C     starting from the third position.
C
      DO K = NUMALL, 1, -1
         RECORD ( K + SHFTAD ) = RECORD( K + 3 )
      END DO

      DO K = 1, 7
         RECORD( K + 2 ) = NUMCFT( K )
      END DO

C
C     All done.
C
      CALL CHKOUT ( 'CKGR04' )
      
      RETURN
      
      END
