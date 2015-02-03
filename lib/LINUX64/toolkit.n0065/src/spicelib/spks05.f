C$Procedure SPKS05 ( S/P Kernel, subset, type 5 )
 
      SUBROUTINE SPKS05 ( HANDLE, BADDR, EADDR, BEGIN, END )
      IMPLICIT NONE
 
C$ Abstract
C
C     Extract a subset of the data in an SPK segment of type 5
C     into a new segment.
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
C     SPK
C     DAF
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      INTEGER               HANDLE
      INTEGER               BADDR
      INTEGER               EADDR
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      END
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of file containing source segment.
C     BADDR      I   Beginning address in file of source segment.
C     EADDR      I   Ending address in file of source segment.
C     BEGIN      I   Beginning (initial epoch) of subset.
C     END        I   End (final epoch) of subset.
C
C$ Detailed_Input
C
C     HANDLE,
C     BADDR,
C     EADDR       are the file handle assigned to an SPK file, and the
C                 beginning and ending addresses of a segment within
C                 that file.  Together they determine a complete set of
C                 ephemeris data, from which a subset is to be
C                 extracted.
C
C     BEGIN,
C     END         are the initial and final epochs (ephemeris time)
C                 of the subset.
C
C                 The first epoch for which there will be ephemeris
C                 data in the new segment will be the greatest time
C                 in the source segment that is less than or equal
C                 to BEGIN.
C
C                 The last epoch for which there will be ephemeris
C                 data in the new segment will be the smallest time
C                 in the source segment that is greater than or equal
C                 to END.
C
C$ Detailed_Output
C
C     See $Files section.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     Data is extracted from the file connected to the input
C     handle, and written to the current DAF open for writing.
C
C     The segment descriptor and summary must already have been written
C     prior to calling this routine.  The segment must be ended
C     external to this routine.
C
C$ Particulars
C
C     This routine is intended solely for use as a utility by the
C     routine SPKSUB.
C
C     It transfers a subset of a type 05 SPK data segment to
C     a properly initialized segment of a second SPK file.
C
C     The exact structure of a segment of data type 05 is described
C     in the section on type 05 in the SPK Required Reading.
C
C$ Examples
C
C     This routine is intended only for use as a utility by SPKSUB.
C     To use this routine successfully, you must:
C
C        Open the SPK file from which to extract data.
C        Locate the segment from which data should be extracted.
C
C        Open the SPK file to which this data should be written.
C        Begin a new segment (array).
C        Write the summary information for the array.
C
C        Call this routine to extract the appropriate data from the
C        SPK open for read.
C
C        End the array to which this routine writes data.
C
C     Much of this procedure is carried out by the routine SPKSUB.  The
C     examples of that routine illustrate more fully the process
C     described above.
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
C     J.M. Lynch      (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     subset type_5 spk segment
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      DATA     ( 6 )
      DOUBLE PRECISION      GM
 
      INTEGER               I
      INTEGER               NDIR
      INTEGER               NREC
      INTEGER               OFFE
      INTEGER               OFFSET
      INTEGER               REC      ( 2 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS05' )
      END IF
 
C
C     Get the number of records in the segment. While we're at it,
C     get the GM of the central body as well (it's adjacent to NREC)
C     since we'll need it anyway.
C
      CALL DAFGDA ( HANDLE, EADDR-1, EADDR, DATA )
 
      NREC = INT ( DATA(2) )
      GM   = DATA(1)
 
C
C     From the number of records, we can compute
C
C        NDIR      The number of directory epochs.
C
C        OFFE      The offset of the first epoch.
C
      NDIR = NREC / 100
      OFFE = EADDR - NDIR - NREC - 2
 
C
C     Examine the epochs in forward order, looking for the first
C     epoch greater than or equal to END (or the final epoch,
C     whichever comes first). This epoch corresponds to the last
C     state to be transferred.
C
      REC(2) = 1
      CALL DAFGDA ( HANDLE, OFFE+REC(2), OFFE+REC(2), DATA )
 
      DO WHILE ( REC(2) .LT. NREC  .AND.  DATA(1) .LT. END )
         REC(2) = REC(2) + 1
         CALL DAFGDA ( HANDLE, OFFE+REC(2), OFFE+REC(2), DATA )
      END DO
 
C
C     Now examine them in reverse order, looking for the first
C     epoch less than or equal to BEGIN (or the initial epoch,
C     whichever comes first). This epoch corresponds to the first
C     state to be transferred.
C
      REC(1) = NREC
      CALL DAFGDA ( HANDLE, OFFE+REC(1), OFFE+REC(1), DATA )
 
      DO WHILE ( REC(1) .GT. 1  .AND.  DATA(1) .GT. BEGIN )
         REC(1) = REC(1) - 1
         CALL DAFGDA ( HANDLE, OFFE+REC(1), OFFE+REC(1), DATA )
      END DO
 
C
C     Copy states REC(1) through REC(2) to the output file.
C
      DO I = REC(1), REC(2)
         OFFSET = BADDR  -  1  +  (I - 1) * 6
 
         CALL DAFGDA ( HANDLE, OFFSET+1, OFFSET+6, DATA    )
         CALL DAFADA (                             DATA, 6 )
      END DO
 
C
C     Copy epochs REC(1) through REC(2) to the output file.
C
      DO I = REC(1), REC(2)
         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )
      END DO
 
C
C     Put every 100'th epoch into the directory.
C
      DO I = REC(1)+99, REC(2), 100
         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )
      END DO
 
C
C     Store the GM of the central body and the number of records
C     to end the segment.
C
      CALL DAFADA ( GM,                           1 )
      CALL DAFADA ( DBLE ( REC(2) - REC(1) + 1 ), 1 )
 
      CALL CHKOUT ( 'SPKS05' )
      RETURN
      END
