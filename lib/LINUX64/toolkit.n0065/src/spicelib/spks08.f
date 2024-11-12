C$Procedure SPKS08 ( S/P Kernel, subset, type 8 )
 
      SUBROUTINE SPKS08 ( HANDLE, BADDR, EADDR, BEGIN, END )
      IMPLICIT NONE
 
C$ Abstract
C
C     Extract a subset of the data in an SPK segment of type 8
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
C     1)  This routine relies on the caller to ensure that the
C         interval [BEGIN, END] is contained in the coverage
C         interval of the segment.
C
C     2)  If BEGIN > END, no data is written to the target file.
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
C     It transfers a subset of a type 08 SPK data segment to
C     a properly initialized segment of a second SPK file.
C
C     The exact structure of a segment of data type 08 is described
C     in the section on type 08 in the SPK Required Reading.
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
C     J.M. Lynch      (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 2.0.0, 20-AUG-1994 (NJB)
C
C        Bug fix:  START value for output segment has been corrected.
C        Bug fix:  Sufficient bracketing states are now included in the
C        output segment to ensure duplication of states given by source
C        segment.
C
C-    SPICELIB Version 1.0.0, 08-AUG-1993 (NJB) (JML) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     subset type_8 spk segment
C
C-&
 
C$ Revisions
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
      DOUBLE PRECISION      RATIO
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEP
 
      INTEGER               DEGREE
      INTEGER               I
      INTEGER               NREC
      INTEGER               OFFSET
      INTEGER               REC      ( 2 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS08' )
      END IF
 
C
C     Look up the following items:
C
C        -- The start epoch
C        -- The step size
C        -- The polynomial degree
C        -- The number of records in the segment
C
      CALL DAFGDA ( HANDLE, EADDR-3, EADDR, DATA )
 
      START  =        DATA(1)
      STEP   =        DATA(2)
      DEGREE = NINT ( DATA(3) )
      NREC   = NINT ( DATA(4) )
 
C
C     See whether there's any work to do; return immediately if not.
C
      IF (      (  END   .LT.   BEGIN                    )
     .     .OR. (  END   .LT.   START                    )
     .     .OR. (  BEGIN .GT. ( START + (NREC-1)*STEP )  )   ) THEN
 
         CALL CHKOUT ( 'SPKS08' )
         RETURN
 
      END IF
 
C
C     Compute the index of the state having the last epoch
C     epoch less than or equal to BEGIN (or the initial epoch,
C     whichever comes last). This epoch corresponds to the first
C     state to be transferred.
C
 
      RATIO  =   MAX (  0.D0,   ( BEGIN - START ) / STEP  )
 
      REC(1) =   1  +  MIN (  INT(RATIO),  NREC-1 )
 
C
C     Make sure that there are DEGREE/2 additional states to the left
C     of the one having index REC(1), if possible.  If not, take as
C     many states as we can.
C
      REC(1)  =   MAX (  1,   REC(1) - DEGREE/2   )
 
C
C     Make sure that REC(1) is small enough so that there are are at
C     least DEGREE+1 states in the segment.
C
      REC(1)  =   MIN (  REC(1),  NREC - DEGREE  )
 
C
C     Now compute the index of the state having the first epoch greater
C     than or equal to END (or the final epoch, whichever comes first).
C     This epoch corresponds to the last state to be transferred.
C
      RATIO  =  ( END - START ) / STEP
 
 
      IF ( RATIO .EQ. AINT(RATIO) ) THEN
         REC(2)  =   1   +   MIN(  INT(RATIO),      NREC-1  )
      ELSE
         REC(2)  =   1   +   MIN(  INT(RATIO) + 1,  NREC-1  )
      END IF
 
C
C     Make sure that there are DEGREE/2 additional states to the right
C     of the one having index REC(2), if possible.  If not, take as
C     many states as we can.
C
      REC(2)  =   MIN (  NREC,   REC(2) + DEGREE/2   )
 
C
C     Make sure that REC(2) is large enough so that there are are at
C     least DEGREE+1 states in the segment.
C
      REC(2)  =   MAX (  REC(2),  DEGREE + 1 )
 
C
C     Copy states REC(1) through REC(2) to the output file.
C
      DO  I = REC(1), REC(2)
         OFFSET = BADDR  -  1  +  (I - 1) * 6
 
         CALL DAFGDA ( HANDLE, OFFSET+1, OFFSET+6, DATA    )
         CALL DAFADA (                             DATA, 6 )
      END DO
 
C
C     Store the start time, step size, polynomial degree and the
C     number of records to end the segment.
C
      CALL DAFADA (  START + ( REC(1) - 1 ) * STEP,    1  )
      CALL DAFADA (  STEP,                             1  )
      CALL DAFADA (  DBLE ( DEGREE ),                  1  )
      CALL DAFADA (  DBLE ( REC(2) - REC(1) + 1 ),     1  )
 
      CALL CHKOUT ( 'SPKS08' )
      RETURN
      END
