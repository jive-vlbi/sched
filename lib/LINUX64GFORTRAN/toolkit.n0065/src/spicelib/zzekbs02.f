C$Procedure      ZZEKBS02 ( EK, begin segment, type 1 )
 
      SUBROUTINE ZZEKBS02 (  HANDLE,  TABNAM,  NCOLS,
     .                       CNAMES,  CDSCRS,  SEGNO  )
 
C$ Abstract
C
C     Start a new type 2 segment in an E-kernel.
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
 
 
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekfilpar.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      CHARACTER*(*)         TABNAM
      INTEGER               NCOLS
      CHARACTER*(*)         CNAMES ( * )
      INTEGER               CDSCRS ( CDSCSZ, * )
      INTEGER               SEGNO
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TABNAM     I   Table name.
C     NCOLS      I   Number of columns in the segment.
C     CNAMES     I   Names of columns.
C     CDSCRS    I-O  Descriptors of columns.
C     SEGNO      O   Segment number.
C
C$ Detailed_Input
C
C     HANDLE         the handle of an EK file that is open for writing.
C
C     TABNAM         is the name of the EK table to which the current
C                    segment belongs.  All segments in the EK file
C                    designated by HANDLE must have identical column
C                    attributes. TABNAM must not exceed 32 characters
C                    in length.  Case is not significant.  Table names
C                    must start with a letter and contain only
C                    characters from the set {A-Z,a-z,0-9,$,_}.
C
C     NCOLS          is the number of columns in a new segment.
C
C     CNAMES,
C     CDSCRS         are, respectively, and array of column names and
C                    their corresponding descriptors:  the Ith element
C                    of CNAMES and the Ith descriptor apply to
C                    the Ith column in the segment.
C
C
C$ Detailed_Output
C
C     CDSCRS         are the input column descriptors, with their name
C                    base and ordinal position elements filled in.
C
C     SEGNO          is the number of the segment created by this
C                    routine.  Segment numbers are used as unique
C                    identifiers by other EK access routines.
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
C     This routine operates by side effects:  it prepares an EK for
C     the addition of a new type 2 segment.  Type 2 segments have
C     fixed record counts:  they do not support record insertion,
C     or deletion operations.  They do not support arbitrary column
C     entry update operations either, since some updates change the
C     size of the affected entries.
C
C     Type 2 segments may contains columns of class 7 through 9.
C
C     By way of contrast, type 1 segments support variable record
C     counts.
C
C$ Examples
C
C     See EKBSEG.
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
C-    Beta Version 1.0.0, 17-NOV-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               EKNSEG
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               MXSPEC
      PARAMETER           ( MXSPEC = 512 )
 
      INTEGER               NAMLIM
      PARAMETER           ( NAMLIM = 32 )
 
C
C     Local variables
C
      CHARACTER*(PGSIZC)    CPAGE
      CHARACTER*(CNAMSZ)    TMPCNM
      CHARACTER*(TNAMSZ)    TMPTNM
 
      INTEGER               BASE
      INTEGER               CBASE
      INTEGER               CP1
      INTEGER               CP
      INTEGER               CPAGNO
      INTEGER               CPT
      INTEGER               DPT
      INTEGER               DSCBAS
      INTEGER               I
      INTEGER               IPAGE  ( PGSIZI )
      INTEGER               IPAGNO
      INTEGER               IPT
      INTEGER               METASZ
      INTEGER               NAMBAS
      INTEGER               NCPAGE
      INTEGER               NIPAGE
      INTEGER               P
      INTEGER               P1
      INTEGER               P1BASE
      INTEGER               ROOM
      INTEGER               SGTREE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKBS02' )
      END IF
 
C
C     Before trying to actually write anything, do every error
C     check we can.
C
C     Is this file handle valid--is the file open for paged write
C     access?  Signal an error if not.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKBS02' )
         RETURN
      END IF
 
 
C
C     The metadata layout has the following form:
C
C        +------------------------------------------+
C        |                                          |
C        |            segment descriptor            |
C        |                                          |
C        +------------------------------------------+
C        |            column descriptor 1           |
C        +------------------------------------------+
C        |            column descriptor 2           |
C        +------------------------------------------+
C                              .
C                              .
C                              .
C        +------------------------------------------+
C        |           column descriptor m            |
C        +------------------------------------------+
C
C     The column descriptors may span multiple pages, but they
C     always occupy contiguous DAS integer addresses.
C
C     In addition, the metadata area includes a character page
C     that contains the segment's table name and the table's
C     column names.
C
C     Calculate the number of contiguous integer pages we'll need.
C     This value is a function of the number of columns.
C
      METASZ  =    SDSCSZ  +  NCOLS * CDSCSZ
 
      NIPAGE  =  ( METASZ  +  PGSIZI  -  1 ) / PGSIZI
C
C     Allocate NIPAGE new integer pages.  Insisting on new pages
C     enforces contiguity.  Also allocate one character page, which
C     need not be new.
C
      CALL ZZEKPGAN ( HANDLE, INT, P1, P1BASE )
 
      DO I = 2, NIPAGE
         CALL ZZEKPGAN ( HANDLE, INT, P, BASE )
      END DO
 
C
C     Calculate the number of contiguous character pages we'll need.
C
      NCPAGE  =  ( TNAMSZ  +  NCOLS * CNAMSZ  +  PGSIZC - 1 ) / PGSIZC
 
      CALL ZZEKPGAN ( HANDLE, CHR, CP1, CBASE )
 
      DO I = 2, NCPAGE
         CALL ZZEKPGAN ( HANDLE, CHR, P, BASE )
      END DO
 
 
C
C     On the third day of Christmas, we initialized three data page
C     trees:  one for each data type.
C
      CALL ZZEKTRIT ( HANDLE, CPT  )
      CALL ZZEKTRIT ( HANDLE, DPT )
      CALL ZZEKTRIT ( HANDLE, IPT )
 
C
C     Prepare the contents of the first integer page:  initialize
C     everything other than the column descriptors.
C
C     The last data word in use for each data type is initialized
C     to indicate that no room is left in the current page.  This
C     forces allocation of a new page when data must be added.  The
C     `last word' counts of each type for both the data and modified
C     record trees are initialized in this fashion.
C
      CALL CLEARI ( PGSIZI, IPAGE )
C
C     The value at index EKTIDX is the segment type.
C
      IPAGE ( EKTIDX )  =  2
      IPAGE ( SNOIDX )  =  EKNSEG ( HANDLE )  +  1
      IPAGE ( IMDIDX )  =  P1BASE
      IPAGE ( TNMIDX )  =  CBASE
      IPAGE ( NCIDX  )  =  NCOLS
      IPAGE ( NRIDX  )  =  0
      IPAGE ( RTIDX  )  =  0
      IPAGE ( CPTIDX )  =  CPT
      IPAGE ( DPTIDX )  =  DPT
      IPAGE ( IPTIDX )  =  IPT
      IPAGE ( MFLIDX )  =  ITRUE
      IPAGE ( IFLIDX )  =  IFALSE
      IPAGE ( SHDIDX )  =  IFALSE
      IPAGE ( CFHIDX )  =  0
      IPAGE ( CSNIDX )  =  0
      IPAGE ( LCPIDX )  =  0
      IPAGE ( LDPIDX )  =  0
      IPAGE ( LIPIDX )  =  0
      IPAGE ( LCWIDX )  =  CPSIZE
      IPAGE ( LDWIDX )  =  DPSIZE
      IPAGE ( LIWIDX )  =  IPSIZE
      IPAGE ( NMLIDX )  =  CBASE + TNAMSZ
 
C
C     Initialize the character metadata page:  fill in the table name.
C     The table name gets converted to upper case and is left justified.
C
      CPAGE  =  ' '
 
      CALL LJUST  ( TABNAM, TMPTNM )
      CALL UCASE  ( TMPTNM, TMPTNM )
 
      CPAGE ( :TNAMSZ )  =  TMPTNM
 
C
C     Now for the column-specific tasks.  We write out a descriptor for
C     each column.  At the same time, we write out the column's name.
C
      IPAGNO = 1
      CPAGNO = 1
      P      = P1
      CP     = CP1
 
      DO I = 1, NCOLS
C
C        Insert the column's ordinal position in the segment into
C        the column's descriptor.
C
         CDSCRS( ORDIDX, I )  =  I
 
C
C        Write the Ith column name into the character metdata page.  (We
C        know the name is non-blank.)  Blank-pad the name on the right,
C        up to a length of CNAMSZ characters, if necessary.  Convert the
C        name to upper case as well.
C
         CALL UCASE  ( CNAMES(I), TMPCNM )
 
         NAMBAS  =  TNAMSZ  +  (I-1)*CNAMSZ  -  (CPAGNO-1)*PGSIZC
         ROOM    =  PGSIZC  -  NAMBAS
 
         IF ( CNAMSZ .LE. ROOM ) THEN
 
            CPAGE ( NAMBAS+1 : NAMBAS+CNAMSZ ) =  TMPCNM
 
C
C           Fill the column name's base address into the descriptor.
C
            CDSCRS( NAMIDX, I ) =  CBASE + (CPAGNO-1)*PGSIZC + NAMBAS
 
         ELSE
C
C           Some or all of the column name will overflow onto the next
C           page.
C
            IF ( ROOM .GT. 0 ) THEN
 
               CPAGE ( NAMBAS+1 : NAMBAS+ROOM ) = TMPCNM(:ROOM)
 
               CDSCRS( NAMIDX, I ) =  CBASE + (CPAGNO-1)*PGSIZC + NAMBAS
 
            ELSE
 
               CDSCRS( NAMIDX, I ) =  CBASE +  CPAGNO*PGSIZC
 
            END IF
 
C
C           Write out the page we just filled up.
C
            CALL ZZEKPGWC ( HANDLE, CP, CPAGE )
 
C
C           The next character page will hold the overflow.  The next
C           page is the successor of page CP, since we allocated
C           consecutive character pages.
C
            CP       =  CP     + 1
            CPAGNO   =  CPAGNO + 1
            CPAGE    =  TMPCNM(ROOM+1:)
 
         END IF
 
C
C        Add the column descriptor to the metadata page, if the
C        descriptor will fit.  We may need to allocate another page
C        to hold the descriptor.
C
         DSCBAS  =  SDSCSZ  +  (I-1)*CDSCSZ  -  (IPAGNO-1)*PGSIZI
         ROOM    =  PGSIZI  -  DSCBAS
 
         IF ( CDSCSZ .LE. ROOM ) THEN
C
C           The whole descriptor fits in the current page.
C
            CALL MOVEI ( CDSCRS(1,I), CDSCSZ, IPAGE(DSCBAS+1) )
 
 
         ELSE
C
C           Some or all of the descriptor will overflow onto the next
C           page.
C
            IF ( ROOM .GT. 0 ) THEN
               CALL MOVEI ( CDSCRS(1,I), ROOM, IPAGE(DSCBAS+1) )
            END IF
 
C
C           Write out the page we just filled up.
C
            CALL ZZEKPGWI ( HANDLE, P, IPAGE )
 
C
C           The next integer page will hold the overflow.  The next page
C           is the successor of page P, since we allocated consecutive
C           pages.
C
            P       =  P + 1
            IPAGNO  =  IPAGNO + 1
 
            CALL CLEARI ( PGSIZI,                        IPAGE )
            CALL MOVEI  ( CDSCRS(ROOM+1,I), CDSCSZ-ROOM, IPAGE )
 
         END IF
 
C
C        If we encountered a DAS error, leave now.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZEKBS02' )
            RETURN
         END IF
 
 
      END DO
 
C
C     Write out the last integer metadata page, and write out the
C     character metadata page.
C
      CALL ZZEKPGWI ( HANDLE, P,  IPAGE )
      CALL ZZEKPGWC ( HANDLE, CP, CPAGE )
 
C
C     At this point, the segment's metadata is filled in.  We must
C     update the file's segment list information to account for this
C     segment.  All we need do is add a new entry to the file's
C     segment pointer tree.  First, look up the tree.
C
      CALL ZZEKPGBS ( INT, 1, BASE )
 
      CALL DASRDI   ( HANDLE, BASE+SGTIDX, BASE+SGTIDX, SGTREE )
 
C
C     Append the head node of this segment at the end of the segment
C     tree.  The tree will point to the first integer metadata page of
C     the new segment.
C
      CALL ZZEKTRAP ( HANDLE, SGTREE, P1, SEGNO )
 
      CALL CHKOUT ( 'ZZEKBS02' )
      RETURN
      END
