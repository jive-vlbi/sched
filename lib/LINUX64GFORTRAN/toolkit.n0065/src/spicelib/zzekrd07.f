C$Procedure   ZZEKRD07 ( EK, read class 7 column entry )
 
      SUBROUTINE ZZEKRD07 (  HANDLE,  SEGDSC,  COLDSC,
     .                       RECPTR,  IVAL,    ISNULL  )
 
C$ Abstract
C
C     Read a column entry from a specified record in a class 7 column.
C     Class 7 columns contain fixed-count, scalar integer values.
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
      INCLUDE 'ekclas07.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      INTEGER               IVAL
      LOGICAL               ISNULL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to EK file.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECPTR     I   Record pointer.
C     IVAL       O   Integer value in column entry.
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
C     RECPTR         is a pointer to the record containing the column
C                    entry to be written.  For class 7 columns, record
C                    pointers are identical to record numbers.
C
C$ Detailed_Output
C
C     IVAL           is the value read from the specified column entry.
C
C     ISNULL         is a logical flag indicating whether the column
C                    entry is null.
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
C     3)  If an I/O error occurs while reading the indicated file,
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
C     This routine is a utility for reading data from class 7 columns.
C
C$ Examples
C
C     See EKRCEI.
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
C     Non-SPICELIB functions
C
      INTEGER               ZZEKRP2N
 
C
C     Local variables
C
      CHARACTER*(1)         CFLAG
 
      INTEGER               ADDRSS
      INTEGER               COLIDX
      INTEGER               DATBAS
      INTEGER               MDAT   ( MDSZ07 )
      INTEGER               METLOC
      INTEGER               NCOLS
      INTEGER               NFLBAS
      INTEGER               NREC
      INTEGER               OFFSET
      INTEGER               Q
      INTEGER               R
      INTEGER               RECNO
      INTEGER               UNIT
 
      LOGICAL               NULLOK
 
C
C     Use discovery check-in.
C
C
C     Make sure the column exists.
C
      NCOLS   =  SEGDSC ( NCIDX  )
      COLIDX  =  COLDSC ( ORDIDX )
      METLOC  =  COLDSC ( METIDX )
      NULLOK  =  COLDSC ( NFLIDX ) .EQ. ITRUE
 
 
      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN
 
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         CALL DASHLU ( HANDLE, UNIT )
 
         CALL CHKIN  ( 'ZZEKRD07'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' //
     .                 'SEGNO = #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NREC                              )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                    )
         CALL ERRINT ( '#',  RECNO                             )
         CALL ERRFNM ( '#',  UNIT                              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKRD07'                              )
         RETURN
 
      END IF
 
C
C     Read the metadata block.  There are two items in the block:
C
C        1) The base address of the first page of the data
C        2) The base address of the null flag array, if nulls are
C           permitted.
C
      CALL DASRDI ( HANDLE, METLOC+1, METLOC+MDSZ07, MDAT )
 
      DATBAS = MDAT ( DBIX07 )
      NFLBAS = MDAT ( NFIX07 )
 
C
C     If null values are permitted, the first step is to get
C     the null flag for the value of interest.  Compute the
C     address of this flag.
C
C     There are CPSIZE null flags per page, and each page has size
C     PGSIZC.  The null flags start at the beginning of the page.
C
      IF ( NULLOK ) THEN
 
         Q       =  (RECPTR-1) / CPSIZE
         R       =  RECPTR - Q * CPSIZE
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
C     The address calculation for the value is analogous to that
C     for the null flag.
C
      Q       =  (RECPTR-1)  / IPSIZE
      R       =  RECPTR - Q * IPSIZE
      OFFSET  =  R      + Q * PGSIZI
      ADDRSS  =  DATBAS + OFFSET
 
      CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, IVAL )
 
      RETURN
      END
