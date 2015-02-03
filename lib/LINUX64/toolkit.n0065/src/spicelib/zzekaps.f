C$Procedure      ZZEKAPS ( EK, allocate page for segment )
 
      SUBROUTINE ZZEKAPS ( HANDLE, SEGDSC, TYPE, NEW, P, BASE  )
 
C$ Abstract
C
C     Allocate a data page for a specified EK segment.
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
      LOGICAL               NEW
      INTEGER               P
      INTEGER               BASE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     TYPE       I   Data type of page.
C     NEW        I   Flag indicating whether page is new.
C     P          O   Page number.
C     BASE       O   DAS base address of page.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     SEGDSC         is the descriptor of the segment for which to
C                    allocate a data page.
C
C     TYPE           is the data type of the desired page.
C
C     NEW            is a logical flag indicating whether a new page
C                    is desired.  A new page is one that has not been
C                    allocated before.  If NEW is .FALSE., a page
C                    on the free list may be returned.
C
C$ Detailed_Output
C
C     P              is the page number of the allocated page.  This
C                    number is recognized by the EK paged access
C                    routines.
C
C     BASE           is the DAS base address of the allocated page.
C
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
C     This routine operates by side effects:  it allocates an EK data
C     page for a specified segment.  The segment's metadata is updated
C     to reflect aquisition of the page.
C
C     The allocated page is initialized as follows:
C
C        - The page's link count is zeroed out.
C
C        - The page's forward pointer is zeroed out.
C
C        - The metadata for the segment is adjusted to reflect ownership
C          of the allocated page.
C
C     The changes made by this routine to the target EK file become
C     permanent when the file is closed.  Failure to close the file
C     properly will leave it in an indeterminate state.
C
C$ Examples
C
C     See EKIFLD.
C
C$ Restrictions
C
C     1)  This routine cannot be used to allocate series of contiguous
C         pages!  Use ZZEKACPS if contiguous pages are required.
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
C-    Beta Version 1.0.0, 08-NOV-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               IDX
      INTEGER               TREE
 
 
C
C     Use discovery check-in.
C
      IF ( NEW ) THEN
C
C        We must allocate a new page.
C
         CALL ZZEKPGAN ( HANDLE, TYPE, P, BASE )
 
      ELSE
C
C        We can allocate a page from the free list if one is available.
C        Otherwise take a new page.
C
         CALL ZZEKPGAL ( HANDLE, TYPE, P, BASE )
 
      END IF
 
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Zero out the page's link count and forward pointer.
C
      CALL ZZEKSLNK ( HANDLE, TYPE, P, 0 )
      CALL ZZEKSFWD ( HANDLE, TYPE, P, 0 )
 
C
C     Update the segment's metadata.  For type 1 segments,
C     the new page into the page tree of the appropriate data type.
C
      IF ( TYPE .EQ. CHR ) THEN
 
         TREE  =  SEGDSC ( CPTIDX )
 
 
      ELSE IF ( TYPE .EQ. DP ) THEN
 
         TREE  =  SEGDSC ( DPTIDX )
 
 
      ELSE IF ( TYPE .EQ. INT ) THEN
C
C        The remaining possibility is that TYPE is INT.  If we had had
C        an unrecognized type, one of the allocation routines would have
C        complained.
C
         TREE  =  SEGDSC ( IPTIDX )
 
      END IF
 
 
      CALL ZZEKTRAP ( HANDLE, TREE, P, IDX )
 
 
      RETURN
      END
