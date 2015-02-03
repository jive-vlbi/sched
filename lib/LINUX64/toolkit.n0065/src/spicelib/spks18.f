C$Procedure SPKS18 ( S/P Kernel, subset, type 18 )
 
      SUBROUTINE SPKS18 ( HANDLE, BADDR, EADDR, BEGIN, END )
      IMPLICIT NONE
 
C$ Abstract
C
C     Extract a subset of the data in an SPK segment of type 18
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

      INCLUDE 'spk18.inc'
 
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
C     1)  This routine relies on the caller to ensure that the
C         interval [BEGIN, END] is contained in the coverage
C         interval of the segment.
C
C     2)  If BEGIN > END, no data is written to the target file.
C
C     3)  If a unexpected SPK type 18 subtype is found in the input
C         segment, the error SPICE(INVALIDVALUE) is signaled.
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
C     It transfers a subset of a type 18 SPK data segment to
C     a properly initialized segment of a second SPK file.
C
C     The exact structure of a segment of data type 18 is described
C     in the section on type 18 in the SPK Required Reading.
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 22-DEC-2012 (NJB) 
C
C        Bug fix: code applicable to SPK type 9 for
C        creating padding in the output segment was 
C        deleted.
C
C-    SPICELIB Version 1.0.0, 16-AUG-2002 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     subset type_18 spk segment
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 22-DEC-2012 (NJB) 
C
C        Bug fix: code applicable to SPK type 9 for
C        creating padding in the output segment was 
C        deleted.
C
C        The offending code was meant to ensure that
C        the output segment's size is at least the
C        window size corresponding to the segment's
C        interpolation degree. This is correct behavior
C        for SPK types 9 and 13; for these types, 
C        segments are not allowed to have sizes less 
C        than the nominal window size. 
C
C        However, for type 18, segments can have as
C        few as two data packets, regardless of their
C        interpolation degree. The code that creates
C        padding packets in this case reads from 
C        invalid locations. 
C
C        Also, the variable WINSIZ was introduced, and
C        comments indicating that the stored size 
C        parameter in the segment control area is the
C        window size minus one were corrected.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               MSIZE
      PARAMETER           ( MSIZE  = 3 )

C
C     Local variables
C
      DOUBLE PRECISION      DATA     ( 12 )
 
      INTEGER               I
      INTEGER               NDIR
      INTEGER               NREC
      INTEGER               OFFE
      INTEGER               OFFSET
      INTEGER               PACKSZ
      INTEGER               REC      ( 2 )
      INTEGER               SUBTYP
      INTEGER               WINSIZ
      INTEGER               WNSZM1
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS18' )
      END IF
 
C
C     See whether there's any work to do; return immediately if not.
C
      IF (  BEGIN  .GT.  END  ) THEN
         CALL CHKOUT ( 'SPKS18' )
         RETURN
      END IF
 
C
C     Read the segment structure metadata.
C
C     Get the type 18 segment subtype.  Next get the quantity "window
C     size." Also get the number of records in the segment.
C
      CALL DAFGDA ( HANDLE, EADDR-MSIZE+1, EADDR, DATA )
 
      SUBTYP = NINT ( DATA(1) )
      WINSIZ = NINT ( DATA(2) )
      NREC   = NINT ( DATA(3) )

C
C     Set the packet size based on the subtype.
C
      IF ( SUBTYP .EQ. S18TP0 ) THEN

         PACKSZ = S18PS0

      ELSE IF ( SUBTYP .EQ. S18TP1 ) THEN

         PACKSZ = S18PS1

      ELSE
         
         CALL SETMSG ( 'Unexpected SPK type 18 subtype found in ' //
     .                 'type 18 record.'                          )
         CALL ERRINT ( '#',  SUBTYP                               )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                      )
         CALL CHKOUT ( 'SPKS18'                                   )
         RETURN
      
      END IF 

C
C     From the number of records, we can compute
C
C        NDIR      The number of directory epochs.
C
C        OFFE      The offset of the first epoch.
C
      NDIR = ( NREC - 1 ) / 100
      OFFE = EADDR - NDIR - NREC - MSIZE
 
C
C     Examine the epochs in forward order, looking for the first
C     epoch greater than or equal to END (or the final epoch,
C     whichever comes first). This epoch corresponds to the last
C     state to be transferred.
C
      REC(2) = 1
      CALL DAFGDA ( HANDLE, OFFE+REC(2), OFFE+REC(2), DATA )
 
      DO WHILE  ( REC(2) .LT. NREC  .AND.  DATA(1) .LT. END )
         REC(2) = REC(2) + 1
         CALL DAFGDA ( HANDLE, OFFE+REC(2), OFFE+REC(2), DATA )
      END DO
 
C
C     Let WNSZM1 be one less than the window size.
C
C     Make sure that there are WNSZM1/2 additional states to the right
C     of the one having index REC(2), if possible.  If not, take as
C     many states as we can.
C
      WNSZM1  =   WINSIZ - 1

      REC(2)  =   MIN (  NREC,   REC(2) + WNSZM1/2   )
 
C
C     Now examine the epochs in reverse order, looking for the first
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
C     Make sure that there are WNSZM1/2 additional states to the left
C     of the one having index REC(1), if possible.  If not, take as
C     many states as we can.
C
      REC(1)  =   MAX (  1,   REC(1) - WNSZM1/2   )
 
C
C     Copy states REC(1) through REC(2) to the output file.
C
      DO  I = REC(1), REC(2)
         OFFSET = BADDR  -  1  +  (I - 1) * PACKSZ
 
         CALL DAFGDA ( HANDLE, OFFSET+1, OFFSET+PACKSZ, DATA         )
         CALL DAFADA (                                  DATA, PACKSZ )
      END DO
 
C
C     Copy epochs REC(1) through REC(2) to the output file.
C
      DO  I = REC(1), REC(2)
         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )
      END DO
 
C
C     Put every 100'th epoch into the directory, except the last
C     epoch, if that epoch's index would be a multiple of 100.
C
      DO  I = REC(1)+99, REC(2)-1, 100
         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )
      END DO
 
C
C     Store subtype, the window size, and the number of
C     records to end the segment.
C
      CALL DAFADA ( DBLE ( SUBTYP ),                  1 )
      CALL DAFADA ( DBLE ( WINSIZ ),                  1 )
      CALL DAFADA ( DBLE ( REC(2) - REC(1) + 1 ),     1 )
 
      CALL CHKOUT ( 'SPKS18' )
      RETURN
      END
