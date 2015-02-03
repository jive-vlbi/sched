C$Procedure      ZZEKFF01 ( EK, finish fast load, segment type 1 )
 
      SUBROUTINE ZZEKFF01 ( HANDLE, SEGNO, RCPTRS )
 
C$ Abstract
C
C     Complete a fast load operation on a new type 1 E-kernel segment.
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
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekpage.inc'
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
C     RCPTRS    I-O  Record pointers.
C
C$ Detailed_Input
C
C     HANDLE         the handle of an EK file that is open for writing.
C                    A `begin segment for fast load' operation must
C                    have already been performed for the designated
C                    segment.
C
C     SEGNO          is the number of the type 1 segment to complete.
C
C     RCPTRS         is an array of record pointers for the input
C                    segment.  This array is obtained as an output
C                    from EKIFLD, the routine called to initiate a
C                    fast load.
C
C$ Detailed_Output
C
C     WORK           is the input work space array, after use.  WORK
C                    will generally be modified by this routine.
C
C     See the $Particulars section for a description of the
C     effects of this routine.
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
C     2)  If an attempt is made to finish a segment other than the
C         one last initialized by EKIFLD, the error SPICE(WRONGSEGMENT)
C         is signalled.
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
C     This routine completes a type 1 EK segment after the data has been
C     written via the fast column loader routines.
C
C$ Examples
C
C     See EKFFLD.
C
C$ Restrictions
C
C     1)  Only one segment can be created at a time using the fast
C         load routines.
C
C     2)  No other EK operation may interrupt a fast load.  For
C         example, it is not valid to issue a query while a fast load
C         is in progress.
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
C     Local parameters
C
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )
 
C
C     Local variables
C
      INTEGER               ADDRSS
      INTEGER               ADRBUF ( BUFSIZ )
      INTEGER               BASE
      INTEGER               COL
      INTEGER               COLIDX
      INTEGER               COLORD ( MXCLSG )
      INTEGER               I
      INTEGER               IPAGE  ( PGSIZI )
      INTEGER               J
      INTEGER               LOC
      INTEGER               MBASE
      INTEGER               NCOLS
      INTEGER               NPAGE
      INTEGER               NR
      INTEGER               NROWS
      INTEGER               NRP
      INTEGER               P
      INTEGER               PAGLOC
      INTEGER               PBASE
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               ROW
      INTEGER               RPSIZE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               STKBAS
      INTEGER               STKHAN
      INTEGER               STKSEG
      INTEGER               TREE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKFF01' )
      END IF
 
C
C     Dig the handle and segment number out of the EK stack.  If the
C     stacked values don't match the inputs HANDLE and SEGNO, we've
C     got trouble.
C
      CALL ZZEKSRD ( 1, 1, STKHAN )
      CALL ZZEKSRD ( 2, 2, STKSEG )
 
      IF (  ( STKHAN .NE. HANDLE ) .OR. ( STKSEG .NE. SEGNO )  ) THEN
 
         CALL SETMSG ( 'Attempt to finish fast load of wrong segment.'//
     .                 '  Input segment number is #; stacked segment '//
     .                 'number is #.  Input handle is #; stacked '    //
     .                 'handle is #.'                                 )
         CALL ERRINT ( '#',  SEGNO                                    )
         CALL ERRINT ( '#',  STKSEG                                   )
         CALL ERRINT ( '#',  HANDLE                                   )
         CALL ERRINT ( '#',  STKHAN                                   )
         CALL SIGERR ( 'SPICE(WRONGSEGMENT)'                          )
         CALL CHKOUT ( 'ZZEKFF01'                                     )
         RETURN
 
      END IF
 
C
C     Look up the segment descriptor for the indicated segment.  Find
C     out how many rows and columns the segment contains.
C
      CALL ZZEKMLOC (  HANDLE,  SEGNO,    P,             MBASE   )
      CALL DASRDI   (  HANDLE,  MBASE+1,  MBASE+SDSCSZ,  SEGDSC  )
 
      NROWS  =  SEGDSC ( NRIDX )
      NCOLS  =  SEGDSC ( NCIDX )
 
C
C     Determine the order in which the columns were added.  The order
C     may differ from that in which the columns were declared.  The
C     ordinal position of each column is stored on the stack right
C     before its address data.  COLORD will map ordinal positions given
C     by a column declaration to ordinal positions on the stack.
C
C
      DO I = 1, NCOLS
 
         LOC  =  (I-1)*(NROWS+1)  +  3
 
         CALL ZZEKSRD ( LOC, LOC, COLIDX )
 
         COLORD ( COLIDX ) = I
 
      END DO
 
C
C     We'll need to create a record pointer structure for each row
C     in the segment.  We compute the number of record pointers that
C     can fit on one page.  We also compute the number of pages we'll
C     need to hold the pointers.
C
      RPSIZE  =    DPTBAS + NCOLS
      NRP     =    IPSIZE / RPSIZE
      NPAGE   =  ( NROWS + NRP - 1 ) / NRP
 
C
C     We'll write out record pointers a pageful at a time.  Each
C     record pointer is initialized to indicate that the record is
C     old, and that there is no corresponding modified record.
C
      REMAIN  =  NROWS
      RECNO   =  0
 
      DO I = 1, NPAGE
C
C        Get the base address of the current page.  The address
C        can be derived from the address of the first record pointer
C        structure on the page.
C
         ADDRSS  =  RCPTRS(RECNO+1) + 1
 
         CALL ZZEKPGPG ( INT, ADDRSS, P, PBASE )
 
         CALL CLEARI   ( IPSIZE, IPAGE )
 
C
C        NR is the number of record pointers we'll write to this page.
C
         NR  =  MIN ( NRP, REMAIN )
 
         DO J = 1, NR
C
C           Initialize the modified record pointer and status for
C           each record pointer on the page.
C
            BASE                     =  (J-1) * RPSIZE
            IPAGE ( BASE + STAIDX )  =  OLD
            IPAGE ( BASE + RCPIDX )  =  UNINIT
 
         END DO
 
C
C        For each column, take NR addresses off the stack and
C        write them into the page.
C
         DO COL = 1, NCOLS
C
C           The stack starts out with the target file handle and
C           segment number.  Next comes the data for each column.
C           Each column is identified by its ordinal position.  The
C           addresses for the data of each column follow.  The addresses
C           for each column are stored contiguously.
C
            J       =  COLORD( COL )
            STKBAS  =  (J-1)*(NROWS+1)  +  3
            LOC     =  STKBAS           +  RECNO
 
            CALL ZZEKSRD ( LOC+1, LOC+NR, ADRBUF )
 
            DO ROW = 1, NR
 
               BASE           =  (ROW-1) * RPSIZE
               PAGLOC         =  BASE + DPTBAS + COL
               IPAGE(PAGLOC)  =  ADRBUF(ROW)
 
            END DO
 
         END DO
 
C
C        Write out the initialized pointer page.
C
         CALL ZZEKPGWI ( HANDLE, P, IPAGE )
 
 
         RECNO   =  RECNO   +  NR
         REMAIN  =  REMAIN  -  NR
 
      END DO
 
C
C     Create the record pointer tree for this segment.
C
      CALL ZZEKTRIT (  HANDLE,  TREE                   )
      CALL ZZEKTR1S (  HANDLE,  TREE,  NROWS,  RCPTRS  )
 
C
C     Update the record tree pointer and row count in the segment
C     descriptor.  Set the records of the last DAS words in use
C     to their maximum values, to ensure allocation of new pages
C     if further writes are done.
C
      CALL ZZEKMLOC (  HANDLE,  SEGNO,        P,            BASE   )
      CALL DASUDI   (  HANDLE,  BASE+RTIDX,   BASE+RTIDX,   TREE   )
      CALL DASUDI   (  HANDLE,  BASE+NRIDX,   BASE+NRIDX,   NROWS  )
      CALL DASUDI   (  HANDLE,  BASE+LCWIDX,  BASE+LCWIDX,  CPSIZE  )
      CALL DASUDI   (  HANDLE,  BASE+LDWIDX,  BASE+LDWIDX,  DPSIZE  )
      CALL DASUDI   (  HANDLE,  BASE+LIWIDX,  BASE+LIWIDX,  IPSIZE  )
 
 
      CALL CHKOUT ( 'ZZEKFF01' )
      RETURN
      END
