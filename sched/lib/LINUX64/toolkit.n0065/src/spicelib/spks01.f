C$Procedure      SPKS01 ( S/P Kernel, subset, type 1 )
 
      SUBROUTINE SPKS01 ( HANDLE, BADDR, EADDR, BEGIN, END )
      IMPLICIT NONE
 
C$ Abstract
C
C     Extract a subset of the data in a SPK segment of type 1
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
C     HANDLE     I   Handle of source segment.
C     BADDR      I   Beginning address of source segment.
C     EADDR      I   Ending address of source segment.
C     BEGIN      I   Beginning (initial epoch) of subset.
C     END        I   End (final epoch) of subset.
C
C$ Detailed_Input
C
C     HANDLE,
C     BADDR,
C     EADDR       are the file handle assigned to a SPK file, and the
C                 beginning and ending addresses of a segment within
C                 the file.  Together they determine a complete set of
C                 ephemeris data, from which a subset is to be
C                 extracted.
C
C     BEGIN,
C     END         are the initial and final epochs (ephemeris time)
C                 of the subset.
C
C$ Detailed_Output
C
C     None.
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
C     See argument HANDLE.
C
C$ Particulars
C
C     The exact structure of a segment of data type 1 is detailed in
C     the SPK Required Reading file.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
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
C-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
C
C        SPK01 was removed from the Required_Reading section of the
C        header. The information in the SPK01 Required Reading file
C        is now part of the SPK Required Reading file.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     subset type_1 spk segment
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      DATA     ( 71 )
 
      INTEGER               NREC
      INTEGER               NDIR
      INTEGER               OFFE
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               OFFSET
      INTEGER               I
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS01' )
      END IF
 
C
C     Get the number of records in the segment. From that, we can
C     compute
C
C        NDIR      The number of directory epochs.
C
C        OFFE      The offset of the first epoch.
C
C
C     the number of directory epochs.
C
      CALL DAFGDA ( HANDLE, EADDR, EADDR, DATA )
      NREC = INT ( DATA(1) )
 
      NDIR  = NREC / 100
      OFFE  = EADDR - NDIR - NREC - 1
 
C
C     Well, the new segment has already been begun. We just have to
C     decide what to move, and move it (using DAFADA).
C
C     Let's agree right now that speed is not of the greatest
C     importance here. We can probably do this with two passes
C     through the record epochs, and one pass through the records.
C
C        1) Determine the first and last records to be included
C           in the subset.
C
C        2) Move the records.
C
C        3) Write the epochs.
C
C     We can leap through the epochs one last time to get the
C     directory epochs.
C
 
C
C     First pass: which records are to be moved?
C
      FIRST = 0
      LAST  = 0
 
      DO I = 1, NREC
         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA )
 
         IF (        FIRST   .EQ. 0
     .        .AND.  DATA(1) .GE. BEGIN )   FIRST = I
 
         IF (        FIRST   .NE. 0
     .        .AND.  LAST    .EQ. 0
     .        .AND.  DATA(1) .GE. END )     LAST = I
 
      END DO
 
C
C     Second pass. Move the records.
C
      OFFSET = (BADDR - 1) + (FIRST - 1) * 71
 
      DO I = FIRST, LAST
         CALL DAFGDA ( HANDLE, OFFSET+1, OFFSET+71, DATA     )
         CALL DAFADA (                              DATA, 71 )
 
         OFFSET = OFFSET + 71
      END DO
 
C
C     Third pass. Move the epochs.
C
      DO I = FIRST, LAST
         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )
      END DO
 
C
C     Get every 100'th epoch for the directory.
C
      DO I = FIRST + 99, LAST, 100
         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )
      END DO
 
C
C     Add the number of records, and we're done.
C
      DATA(1) = DBLE ( LAST - FIRST + 1 )
      CALL DAFADA ( DATA, 1 )
 
      CALL CHKOUT ( 'SPKS01' )
      RETURN
      END
