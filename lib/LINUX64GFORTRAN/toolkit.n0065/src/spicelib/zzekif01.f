C$Procedure ZZEKIF01 ( EK, initialize type 1 segment for fast load )
 
      SUBROUTINE ZZEKIF01 ( HANDLE, SEGNO, RCPTRS )
 
C$ Abstract
C
C     Initialize a new type 1 EK segment to allow fast loading.
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
 
 
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RCPTRS ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGNO      I   Segment number.
C     RCPTRS     O   Array of record pointers.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an EK file open for write access.
C                    A new type 1 segment is to be created in this file
C                    via a fast load.  The segment's metadata has
C                    already been set up by EKBSEG.
C
C     SEGNO          is the number of the segment to prepare for a
C                    fast load.
C
C$ Detailed_Output
C
C     RCPTRS         is an array of record pointers for the input
C                    segment.  This array must not be modified by the
C                    caller.
C
C                    The array RCPTRS must be passed as an input to
C                    each column addition routine called while
C                    writing the specified segment.
C
C                    RCPTRS must be declared with dimension equal to
C                    the number of rows in the segment.
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
C     This routine carries out the type-1-specific preparation for
C     populating a type 1 EK segment with data via the fast column
C     loader routines.  This routine expects the segment's metadata to
C     already have been written by EKBSEG.
C
C$ Examples
C
C     See EKIFLD.
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
C-    SPICELIB Version 1.0.1, 06-SEP-2006 (NJB)
C
C        Added Restrictions section to header.  Changed
C        previous version line's product from "Beta" to "SPICELIB."
C
C-    SPICELIB Version 1.0.0, 06-NOV-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               BASE
      INTEGER               I
      INTEGER               J
      INTEGER               NCOLS
      INTEGER               NPAGE
      INTEGER               NR
      INTEGER               NROWS
      INTEGER               NRP
      INTEGER               P
      INTEGER               PBASE
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               RPSIZE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               TOP
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKIF01' )
      END IF
 
C
C     Read in the segment descriptor.
C
      CALL ZZEKSDSC (  HANDLE,  SEGNO,  SEGDSC )
 
      NCOLS  =  SEGDSC(NCIDX)
      NROWS  =  SEGDSC(NRIDX)
 
C
C     Empty the EK scratch area stack.
C
      CALL ZZEKSTOP ( TOP )
      CALL ZZEKSDEC ( TOP )
 
C
C     Push the handle and segment number onto the stack.
C
      CALL ZZEKSPSH ( 1, HANDLE )
      CALL ZZEKSPSH ( 1, SEGNO  )
 
C
C     The segment will require a record pointer structure for each row
C     in the segment.  Right now, all we're going to do is allocate
C     integer pages to hold these structures and save the base
C     addresses of each structure.
C
C     We compute the number of record pointers that can fit on one page.
C     We also compute the number of pages we'll need to hold the
C     pointers.
C
      RPSIZE  =    DPTBAS + NCOLS
      NRP     =    IPSIZE / RPSIZE
      NPAGE   =  ( NROWS + NRP - 1 ) / NRP
 
C
C     We'll compute addresses of record pointers a pageful at a time.
C
      REMAIN  =  NROWS
      RECNO   =  0
 
      DO I = 1, NPAGE
C
C        Allocate a page to hold the record pointers.  A page from
C        the free list is acceptable, hence the argument .FALSE.
C        passed to ZZEKAPS.
C
         CALL ZZEKAPS  ( HANDLE, SEGDSC, INT, .FALSE., P, PBASE )
 
C
C        NR is the number of record pointers we'll eventually write to
C        this page.
C
         NR  =  MIN ( NRP, REMAIN )
 
         DO J = 1, NR
C
C           Record the base address of the current record pointer
C           in the record pointer array.
C
            BASE                =  (J-1) * RPSIZE
            RCPTRS ( RECNO+J )  =  PBASE + BASE
 
         END DO
 
         RECNO   =  RECNO   +  NR
         REMAIN  =  REMAIN  -  NR
 
      END DO
 
 
      CALL CHKOUT ( 'ZZEKIF01' )
      RETURN
      END
