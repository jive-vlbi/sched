C$Procedure     ZZEKWPAL ( EK, write paged array, logical )
 
      SUBROUTINE ZZEKWPAL ( HANDLE, SEGDSC, NVALS, LVALS, P, BASE )
 
C$ Abstract
C
C     Write a logical array out to a contiguous set of EK pages.
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
C
C$ Declarations
 
 
      INCLUDE  'ekbool.inc'
      INCLUDE  'ekdatpag.inc'
      INCLUDE  'ekpage.inc'
      INCLUDE  'eksegdsc.inc'
      INCLUDE  'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( * )
      INTEGER               NVALS
      LOGICAL               LVALS  ( * )
      INTEGER               P
      INTEGER               BASE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to EK file.
C     SEGDSC     I   Descriptor of segment that owns the array.
C     NVALS      I   Number of values to write.
C     LVALS      I   Logical values.
C     P          O   Number of first page containing array.
C     BASE       O   Base address of first page.
C
C$ Detailed_Input
C
C     HANDLE         the handle of an EK file that is open for writing.
C
C     SEGDSC         is a descriptor for the segment to which data is
C                    to be added.  The segment descriptor is not
C                    updated by this routine, but some fields in the
C                    descriptor will become invalid after this routine
C                    returns.
C
C     NVALS          is the number of logical values to write.
C
C     LVALS          is an array of logical values.  The values will
C                    be stored as characters, with one character used
C                    per element of LVALS.
C
C$ Detailed_Output
C
C     P              is the number of the first page to which the
C                    input values are written.  The character
C                    representing LVALS(1) is written to the first DAS
C                    character of page P.  The values are written
C                    to a contiguous set of pages in increasing order.
C
C     BASE           is the base address of P.  BASE is the predecessor
C                    of the first DAS address belonging to page P.
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
C     2)  If an I/O error occurs while reading or writing the indicated
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
C     This routine writes an array of logical values to a contiguous
C     set of new character pages.  The first element of the input array
C     is written to a range of values starting at the first DAS address
C     of the first page of the set.
C
C     This routine supports creation of null flag arrays for fixed-count
C     column classes.
C
C     Note that the values do not occupy a contiguous range of DAS
C     character words, since each page contains several addresses
C     reserved for bookkeeping information, and since there may be
C     unused space at the end of a data page.
C
C$ Examples
C
C     See ZZEKAC07.
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
C-    Beta Version 1.0.0, 08-NOV-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      CHARACTER*(PGSIZC)    PAGE
 
      INTEGER               FROM
      INTEGER               NPAGE
      INTEGER               TO
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKWPAL' )
      END IF
 
C
C     Decide how many pages are required to hold the array, and
C     allocate that many new, contiguous pages.
C
      NPAGE  =  ( NVALS + CPSIZE - 1 ) / CPSIZE
 
      CALL ZZEKACPS ( HANDLE, SEGDSC, CHR, NPAGE, P, BASE )
 
C
C     Write the input data out to the target file a page at a time.
C
C     We'll use FROM to indicate the element of LVALS we're
C     considering and TO to indicate the element of PAGE to write
C     to.
C
      TO      =  1
      PAGE    =  ' '
 
      DO FROM = 1, NVALS
 
C
C        The Assignment.
C
         IF ( LVALS(FROM) ) THEN
            PAGE( TO : TO ) =  CTRUE
         ELSE
            PAGE( TO : TO ) =  CFALSE
         END IF
 
         TO  =  TO + 1
 
 
         IF (  ( TO .GT. CPSIZE ) .OR. ( FROM .EQ. NVALS )  ) THEN
C
C           Either the current data page is full, or we've buffered
C           the last of the available data.  It's time to write out the
C           current page.
C
C           Write out the data page.
C
            CALL ZZEKPGWC ( HANDLE, P, PAGE  )
 
C
C           Set the link count.
C
            CALL ZZEKSLNK ( HANDLE, CHR, P, TO-1 )
 
C
C           Next page.
C
            P   =  P + 1
            TO  =  1
 
         END IF
 
      END DO
 
 
      CALL CHKOUT ( 'ZZEKWPAL' )
      RETURN
      END
