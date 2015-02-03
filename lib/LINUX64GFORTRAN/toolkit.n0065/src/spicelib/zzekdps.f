C$Procedure      ZZEKDPS ( EK, delete page from segment )
 
      SUBROUTINE ZZEKDPS ( HANDLE, SEGDSC, TYPE, P )
 
C$ Abstract
C
C     Delete a specified data page for a specified EK segment.
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
 
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               TYPE
      INTEGER               P
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC    I-O  Segment descriptor.
C     TYPE       I   Data type of page.
C     P          I   Page number.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     SEGDSC         is the descriptor of the segment from which to
C                    delete a data page.
C
C     TYPE           is the data type of the page.
C
C     P              is number of the page to delete.  This
C                    number is recognized by the EK paged access
C                    routines.
C
C$ Detailed_Output
C
C     SEGDSC         is the descriptor of the segment from which the
C                    specified page was deleted.  If P is the current
C                    data page of TYPE, the descriptor element
C                    specifying the last word in use of this data type
C                    will be updated on exit from this routine.
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
C     This routine operates by side effects:  it deletes an EK data
C     page from a specified segment.  The segment's metadata is updated
C     to reflect deletion of the page.  If the deleted page is the last
C     one of its type in use in the specified segment, the last word in
C     use of that type is set to the maximum value.  This prevents
C     further attempts to write to the page.
C
C     The changes made by this routine to the target EK file become
C     permanent when the file is closed.  Failure to close the file
C     properly will leave it in an indeterminate state.
C
C$ Examples
C
C     See EKDELR.
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
C-    Beta Version 2.0.0, 02-APR-1996 (NJB)
C
C        Updated to make SEGDSC an in-out argument.  The last word
C        in use of the data type of P is set to the maximum value
C        on output.  Also, an error in the deletion of the page
C        from the parent data page tree was corrected.
C
C-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKTRLS
 
C
C     Local variables
C
      INTEGER               MBASE
      INTEGER               LOC
      INTEGER               TREE
 
 
C
C     Use discovery check-in.
C
C
C     Ashes to ashes, dust to dust.  This page goes to the free list.
C
      CALL ZZEKPGFR ( HANDLE, TYPE, P )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Update the segment's metadata.  For type 1 segments,
C     the new page into the page tree of the appropriate data type.
C
C     If this page is the last one in use in the segment, set the last
C     word in use of the appropriate type to the maximum number.  This
C     prevents further writing to the page we're deleting.
C
      MBASE  =  SEGDSC ( IMDIDX )
 
 
      IF ( TYPE .EQ. CHR ) THEN
 
         TREE  =  SEGDSC ( CPTIDX )
 
         IF ( SEGDSC(LCPIDX) .EQ. P ) THEN
            CALL DASUDI ( HANDLE, MBASE+LCWIDX, MBASE+LCWIDX, CPSIZE )
         END IF
 
         IF ( P .EQ. SEGDSC(LCPIDX) ) THEN
            SEGDSC(LCWIDX) = CPSIZE
         END IF
 
 
      ELSE IF ( TYPE .EQ. DP ) THEN
 
         TREE  =  SEGDSC ( DPTIDX )
 
         IF ( SEGDSC(LDPIDX) .EQ. P ) THEN
            CALL DASUDI ( HANDLE, MBASE+LDWIDX, MBASE+LDWIDX, DPSIZE )
         END IF
 
         IF ( P .EQ. SEGDSC(LDPIDX) ) THEN
            SEGDSC(LDWIDX) = DPSIZE
         END IF
 
      ELSE IF ( TYPE .EQ. INT ) THEN
C
C        The remaining possibility is that TYPE is INT.  If we had had
C        an unrecognized type, one of the allocation routines would have
C        complained.
C
         TREE  =  SEGDSC ( IPTIDX )
 
         IF ( SEGDSC(LIPIDX) .EQ. P ) THEN
            CALL DASUDI ( HANDLE, MBASE+LIWIDX, MBASE+LIWIDX, IPSIZE )
         END IF
 
         IF ( P .EQ. SEGDSC(LIPIDX) ) THEN
            SEGDSC(LIWIDX) = IPSIZE
         END IF
 
      END IF
 
C
C     Remove the page's number from the data page tree of the
C     appropriate type.  This removal requires finding the key that
C     points to the page to be removed.
C
      LOC  =  ZZEKTRLS ( HANDLE, TREE, P )
 
      CALL ZZEKTRDL (  HANDLE, TREE, LOC )
 
      RETURN
      END
