C$Procedure   ZZEKRD09 ( EK, read class 9 column entry elements )
 
      SUBROUTINE ZZEKRD09 (  HANDLE,  SEGDSC,  COLDSC,
     .                       RECNO,   CVLEN,   CVAL,    ISNULL  )
 
      IMPLICIT NONE
      
C$ Abstract
C
C     Read a column entry from a specified record in a class 9 column.
C     Class 9 columns contain fixed record count, fixed-length,
C     scalar character values.
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
 
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekclas09.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECNO
      INTEGER               CVLEN
      CHARACTER*(*)         CVAL
      LOGICAL               ISNULL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to EK file.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECNO      I   Record number.
C     CVLEN      O   Length of returned character value.
C     CVAL       O   Character value in column entry.
C     ISNULL     O   Flag indicating whether column entry is null.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.
C
C     SEGDSC         is the descriptor of the segment from which data is
C                    to be read.
C
C     COLDSC         is the descriptor of the column from which data is
C                    to be read.
C
C     RECNO          is the number of the record containing the column
C                    entry to be written.
C
C$ Detailed_Output
C
C     CVLEN          is the length of the returned string value.  This
C                    is the declared string length of the column being
C                    read.  Note this definition differs from that used
C                    for class 3 columns.  In the class 9 case, no 
C                    string length is stored in the file, so extra work
C                    at run time would be required to determine whether
C                    truncation would occur.  
C
C     CVAL           is the value read from the specified column entry.
C                    CVAL must have sufficient length to hold the
C                    returned string value.  Entries that are shorter 
C                    than the string length of CVAL are padded with 
C                    trailing blanks.
C
C     ISNULL         is a logical flag indicating whether the entry is
C                    null.
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
C     2)  If the ordinal position of the column specified by COLDSC
C         is out of range, the error SPICE(INVALIDINDEX) is signalled.
C
C     3)  If the output string CVAL is too short to accommodate the
C         returned string value, the error SPICE(STRINGTRUNCATED)
C         is signalled.  CVAL must be at least as long as the declared
C         length of the column being read.
C
C     4)  If an I/O error occurs while reading the indicated file,
C         the error will be diagnosed by routines called by this
C         routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine is a utility for reading data from class 9 columns.
C
C$ Examples
C
C     See EKRCEC.
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
C-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
C
C        Error check for string truncation on output was added.
C        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
C        to SPICE(UNINITIALIZED). 
C
C        The argument RECPTR was renamed to RECNO.  The reference to
C        ZZEKRP2N was removed.
C
C        Miscellaneous header corrections were made.
C        
C-    SPICELIB Version 1.0.0, 09-NOV-1995 (NJB)
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
C
C        Error check for string truncation on output was added.
C        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
C        to SPICE(UNINITIALIZED).  
C
C        The argument RECPTR was renamed to RECNO.  The reference to
C        ZZEKRP2N was removed.
C
C
C-&


C
C     Local variables
C
      CHARACTER*(1)         CFLAG
      CHARACTER*(CNAMSZ)    COLUMN
 
      INTEGER               ADDRSS
      INTEGER               COLIDX
      INTEGER               DATBAS
      INTEGER               L
      INTEGER               MDAT   ( MDSZ09 )
      INTEGER               METLOC
      INTEGER               NCOLS
      INTEGER               NFLBAS
      INTEGER               NREC
      INTEGER               OFFSET
      INTEGER               Q
      INTEGER               R
      INTEGER               SPP
      INTEGER               UNIT
 
      LOGICAL               NULLOK
 
C
C     Use discovery check-in.
C
C     Make sure the column exists.
C
      NCOLS   =  SEGDSC ( NCIDX  )
      COLIDX  =  COLDSC ( ORDIDX )
      METLOC  =  COLDSC ( METIDX )
      NULLOK  =  COLDSC ( NFLIDX ) .EQ. ITRUE
      L       =  COLDSC ( LENIDX )
 
      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN
 
         CALL DASHLU ( HANDLE, UNIT )
 
         CALL CHKIN  ( 'ZZEKRD09'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' //
     .                 'SEGNO = #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NREC                              )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                    )
         CALL ERRINT ( '#',  RECNO                             )
         CALL ERRFNM ( '#',  UNIT                              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKRD09'                              )
         RETURN
 
      END IF
 
C
C     Since class 9 columns have fixed-length strings, we already
C     know the string length.
C
      CVLEN  =  L
 
      IF ( CVLEN .GT. LEN(CVAL) ) THEN
C              
C        We have a string truncation error.  Look up the column
C        name, record number, and file name before signalling an
C        error.
C        
         CALL DASHLU   ( HANDLE, UNIT )
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )  
            
         CALL CHKIN  ( 'ZZEKRD09'                                  )
         CALL SETMSG ( 'String value has length #; output string '//
     .                 'can hold only # characters.  COLUMN = #; '// 
     .                 'SEGNO = #; RECNO = #; EK = #'              )
         CALL ERRINT ( '#',  CVLEN                                 )
         CALL ERRINT ( '#',  LEN(CVAL)                             )
         CALL ERRCH  ( '#',  COLUMN                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL ERRFNM ( '#',  UNIT                                  )
         CALL SIGERR ( 'SPICE(STRINGTRUNCATED)'                    )
         CALL CHKOUT ( 'ZZEKRD09'                                  )
         RETURN
            
      END IF

C
C     Read the metadata block.  There are two items in the block:
C
C        1) The base address of the first page of the data
C        2) The base address of the null flag array, if nulls are
C           permitted.
C
      CALL DASRDI ( HANDLE, METLOC+1, METLOC+MDSZ09, MDAT )
 
      DATBAS = MDAT ( DBIX09 )
      NFLBAS = MDAT ( NFIX09 )
 
C
C     If null values are permitted, the first step is to get
C     the null flag for the value of interest.  Compute the
C     address of this flag.
C
C     There are CPSIZE null flags per page, and each page has size
C     PGSIZC.  The null flags start at the beginning of the page.
C
      IF ( NULLOK ) THEN
 
         Q       =  (RECNO-1)  / CPSIZE
         R       =  RECNO  - Q * CPSIZE
         OFFSET  =  R      + Q * PGSIZC
         ADDRSS  =  NFLBAS + OFFSET
 
         CALL DASRDC ( HANDLE, ADDRSS, ADDRSS, 1, 1, CFLAG )
 
         ISNULL  =  CFLAG .EQ. CTRUE
 
         IF ( ISNULL ) THEN
            RETURN
         END IF
 
      END IF
 
C
C     If we're still here, we'll read the data value.
C
      ISNULL  =  .FALSE.
 
C
C     The address calculation for the value is similar to that
C     for the null flag.  However, the string length must be
C     taken into account.
C
      SPP     =  CPSIZE     /   L
      Q       =  (RECNO-1)  /   SPP
      R       =  RECNO      -   Q*SPP
      ADDRSS  =  DATBAS     +   Q*PGSIZC  +  (R-1)*L  +  1
 
      CALL DASRDC ( HANDLE, ADDRSS, ADDRSS+L-1, 1, L, CVAL )
 
C
C     Blank-pad CVAL if required.
C
      IF ( LEN(CVAL) .GT. L ) THEN
         CVAL( L+1 : )  =  ' '
      END IF
 
 
      RETURN
      END
