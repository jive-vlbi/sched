C$Procedure ZZEKACPS ( EK, allocate contiguous pages for segment )
 
      SUBROUTINE ZZEKACPS ( HANDLE, SEGDSC, TYPE, N, P, BASE  )
 
C$ Abstract
C
C     Allocate a series of contiguous data pages for a specified EK
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
C     EK
C
C$ Keywords
C
C     PRIVATE
C     UTILITY
C
C$ Declarations
 
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               TYPE
      INTEGER               N
      INTEGER               P
      INTEGER               BASE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     TYPE       I   Data type of page.
C     N          I   Number of pages to allocate.
C     P          O   Page number.
C     BASE       O   DAS base address of page.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     SEGDSC         is the descriptor of the segment for which to
C                    allocate a series of data pages.
C
C     TYPE           is the data type of the desired pages.
C
C     N              is the number of pages desired.  All pages
C                    allocated are new.   A new page is one that has not
C                    been allocated before.
C
C$ Detailed_Output
C
C     P              is the number of the first page of the allocated
C                    series.  The rest of the pages have numbers
C
C                       P+1, P+2, ... , P+N-1
C
C                    These numbers are recognized by the EK paged access
C                    routines.
C
C     BASE           is the DAS base address of the first allocated
C                    page.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.  The file will not be modified.
C
C     2)  If an I/O error occurs while reading or writing the indicated
C         file, the error will be diagnosed by routines called by this
C         routine.  The file may be corrupted.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine operates by side effects:  it allocates a series of
C     new, contiguous EK data pages for a specified segment.  The
C     segment's metadata are updated to reflect aquisition of the pages.
C
C     This routine, not ZZEKAPS, should be used when contiguous pages
C     are required.
C
C     Each allocated page is initialized as follows:
C
C        - The page's link count is zeroed out.
C
C        - The page's forward pointer is zeroed out.
C
C     After all pages are allocated, the metadata for the segment are
C     adjusted to reflect ownership of the allocated pages.
C
C     The changes made by this routine to the target EK file become
C     permanent when the file is closed.  Failure to close the file
C     properly will leave it in an indeterminate state.
C
C$ Examples
C
C     See ZZEKWPAI.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               B
      INTEGER               I
      INTEGER               IDX
      INTEGER               P2
      INTEGER               TREE
 
C
C     Use discovery check-in.
C
C
C     Allocate the pages.
C
      CALL ZZEKPGAN ( HANDLE, TYPE, P, BASE )
 
      DO I = 2, N
         CALL ZZEKPGAN ( HANDLE, TYPE, P2, B )
      END DO
 
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Initialize the pages.
C
      DO I = 1, N
C
C        Zero out the page's link count and forward pointer.
C
         CALL ZZEKSLNK ( HANDLE, TYPE, P+I-1, 0 )
         CALL ZZEKSFWD ( HANDLE, TYPE, P+I-1, 0 )
 
      END DO
 
C
C     Update the segment's metadata.  Insert the number of each new
C     page into the page tree of the appropriate data type.
C
      IF ( TYPE .EQ. CHR ) THEN
 
         TREE  =  SEGDSC ( CPTIDX )
 
      ELSE IF ( TYPE .EQ. DP ) THEN
 
         TREE  =  SEGDSC ( DPTIDX )
 
      ELSE
C
C        The remaining possibility is that TYPE is INT.  If we had had
C        an unrecognized type, one of the allocation routines would have
C        complained.
C
         TREE  =  SEGDSC ( IPTIDX )
 
      END IF
 
 
      DO I = 1, N
         CALL ZZEKTRAP ( HANDLE, TREE, P+I-1, IDX )
      END DO
 
      RETURN
      END
