C$Procedure SPKS17 ( S/P Kernel, subset, type 17 )
 
      SUBROUTINE SPKS17 ( HANDLE, BADDR, EADDR, BEGIN, END )
      IMPLICIT NONE
 
C$ Abstract
C
C     Extract a subset of the data in an SPK segment of type 17
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
C     routine SPKSUB. It transfers a subset of a type 17 SPK data
C     segment to a properly initialized segment of a second SPK file.
C
C     The exact structure of a segment of data type 17 is described
C     in the section on type 17 in the SPK Required Reading.
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.0, 3-JAN-1997 (WLT)
C
C-&
 
 
C$ Index_Entries
C
C     subset type_17 spk segment
C
C-&
 
C$ Revisions
C
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               NITEMS
      PARAMETER           ( NITEMS = 12 )
 
      DOUBLE PRECISION      DATA     ( NITEMS )
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS17' )
      END IF
 
C
C     See whether there's any work to do; return immediately if not.
C
      IF (  BEGIN  .GT.  END  ) THEN
         CALL CHKOUT ( 'SPKS17' )
         RETURN
      END IF
 
 
C
C     This couldn't be much easier.  First copy the entire
C     type 17 segment out of the file.
C
      CALL DAFGDA ( HANDLE, BADDR, EADDR, DATA )
 
C
C     Now write the data into the output file.
C
      CALL DAFADA ( DATA, NITEMS )
 
 
      CALL CHKOUT ( 'SPKS17' )
      RETURN
      END
