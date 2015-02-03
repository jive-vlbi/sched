C$Procedure      CKNR04 ( C-kernel, number of records, data type 4 )

      SUBROUTINE CKNR04 ( HANDLE, DESCR, NREC )

C$ Abstract
C
C     Given the handle of a CK file and the descriptor of a type 4
C     segment in that file, return the number of pointing instances
C     in that segment.
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
      INTEGER               NREC

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of the file containing the segment.
C     DESCR      I   The descriptor of the type 4 segment.
C     NREC       O   The number of pointing records in the segment.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of the binary CK file containing the
C                segment. The file should have been opened for read
C                or write access, either by CKLPF, DAFOPR, or DAFOPW.
C
C     DESCR      The packed descriptor of a data type 4 segment.
C
C$ Detailed_Output
C
C     NREC       The number of pointing records in the type 4 
C                segment.
C
C$ Parameters
C
C     See 'ckparam.inc'.
C
C$ Exceptions
C
C     1)  If the segment indicated by DESCR is not a type 4 segment,
C         the error 'SPICE(CKWRONGDATATYPE)' is signalled.
C
C     2)  If the specified handle does not belong to any DAF file that
C         is currently known to be open, an error is diagnosed by a
C         routine that this routine calls.
C
C     3)  If DESCR is not a valid descriptor of a segment in the CK
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
C     For a complete description of the internal structure of a type 4
C     segment, see the CK required reading.
C
C     This routine returns the number of pointing records contained
C     in the specified segment. It is normally used in conjunction
C     with CKGR04 which returns the Ith pointing record in the 
C     segment.
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
C     number of CK type_4 records
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
      PARAMETER           ( NDC = 2 )

      INTEGER               NIC
      PARAMETER           ( NIC = 6 )

C
C     Local variables
C
      DOUBLE PRECISION      DCD    ( NDC )
      
      INTEGER               ICD    ( NIC )
      
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKNR04' )
      END IF

C
C     Check whether our segment is of the type 4 by unpacking 
C     descriptor and checking value of its third integer component.
C
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )

      IF ( ICD( 3 ) .NE. CK4DTP ) THEN
         CALL SETMSG ( 'Data type of the segment should be 4: '      //
     .                 'Passed descriptor shows type = #.'           )
         CALL ERRINT ( '#', ICD ( 3 )                                )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                      )
         CALL CHKOUT ( 'CKNR04'                                      )
         RETURN
      END IF

C
C     The number of records (packets) can be obtained by a call to 
C     SGMETA. This number is a meta item 12 (see sgparam.inc for 
C     details.)
C
      CALL SGMETA ( HANDLE, DESCR, 12, NREC )

C
C     All done.
C
      CALL CHKOUT ( 'CKNR04' )
      
      RETURN
      
      END
