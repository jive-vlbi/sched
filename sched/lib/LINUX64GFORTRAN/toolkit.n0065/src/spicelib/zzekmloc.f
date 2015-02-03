C$Procedure      ZZEKMLOC ( EK, return integer metadata location )
 
      SUBROUTINE ZZEKMLOC ( HANDLE, SEGNO, PAGE, BASE )
 
C$ Abstract
C
C     Return the integer metadata location of a specified segment.  The
C     number and DAS integer base address of the first integer
C     page of the metadata are returned.
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
C     EK
C     PRIVATE
C
C$ Declarations
 
      INCLUDE 'ekfilpar.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               PAGE
      INTEGER               BASE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGNO      I   Segment number.
C     PAGE       O   Integer metadata start page number.
C     BASE       O   Page base.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.  The EK may be open for read
C                    or write access.
C
C     SEGNO          is the number of the segment whose integer metadata
C                    location is sought.
C
C$ Detailed_Output
C
C    PAGE            is the number of the first page containing integer
C                    metadata for the specified segment.  The segment
C                    descriptor starts at the first address of this
C                    page.
C
C    BASE            is the DAS integer base address of the page
C                    whose number is given by PAGE.  BASE is the
C                    predecessor of the first DAS integer word
C                    belonging to this page.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     2)  If SEGNO is out of range, the error SPICE(INVALIDINDEX)
C         will be signalled.
C
C     3)  If an I/O error occurs while reading or writing the indicated
C         file, the error will be diagnosed by routines called by this
C         routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine operates by side effects:  it deletes a record
C     from an EK segment.  Deleting a record implies:
C
C        1) All column entries in the record are deleted.
C
C        2) Link counts are decremented for data pages containing
C           column entries in the record to be deleted.  Pages whose
C           link counts drop to zero are freed.
C
C        3) All column indexes are updated for the parent segment.
C
C        4) The link count is decremented for the page containing the
C           record pointer structure of the record to be deleted.  If
C           the link count drops to zero, the page is freed.
C
C        5) The pointer to the deleted record is deleted from the
C           record tree for the parent segment.
C
C        6) The segment's metadata is updated to reflect the new
C           record count.
C
C$ Examples
C
C     See EKINSR.
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
C-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Added the required discovery CHKIN.
C
C-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               EKNSEG
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKTRBS
 
C
C     Local variables
C
      INTEGER               NSEG
      INTEGER               TBASE
      INTEGER               TREE
 
 
C
C     Use discovery check-in.
C
C
C     Validate the segment number to start out.
C
C
C     Get the segment count; valididate SEGNO.
C
      NSEG  =  EKNSEG ( HANDLE )
 
C
C     Check out SEGNO.
C
      IF (  ( SEGNO .LT. 1 ) .OR. ( SEGNO .GT. NSEG )  )  THEN
 
         CALL CHKIN  ( 'ZZEKMLOC'                                )
         CALL SETMSG ( 'Segment number = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  SEGNO                               )
         CALL ERRINT ( '#',  NSEG                                )
         CALL SIGERR ( 'SPICE(INVALIDINDEX )'                    )
         CALL CHKOUT ( 'ZZEKMLOC'                                )
         RETURN
 
      END IF
 
C
C     Find the segment in the segment tree.
C     Obtain the base address of the first integer page.
C
      TBASE = ZZEKTRBS ( 1 )
 
C
C     Look up the head node of the segment tree.
C
      CALL DASRDI ( HANDLE, TBASE+SGTIDX, TBASE+SGTIDX, TREE )
 
C
C     Get the segment pointer for the segment having index SEGNO.
C     This pointer is actually the page number we're looking for.
C
      CALL ZZEKTRDP ( HANDLE, TREE, SEGNO, PAGE )
 
C
C     Return the base address of the metadata page as well.
C
      BASE  =  ZZEKTRBS ( PAGE )
 
      END
