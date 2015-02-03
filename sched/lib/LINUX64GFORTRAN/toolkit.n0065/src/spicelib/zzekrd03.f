C$Procedure   ZZEKRD03 ( EK, read class 3 column entry elements )
 
      SUBROUTINE ZZEKRD03 (  HANDLE,  SEGDSC,  COLDSC,
     .                       RECPTR,  CVLEN,   CVAL,    ISNULL  )
 
      IMPLICIT NONE
      
C$ Abstract
C
C     Read a column entry from a specified record in a class 3 column.
C     Class 3 columns contain scalar character values.
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
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
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
C     RECPTR     I   Record pointer.
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
C     RECPTR         is a pointer to the record containing the column
C                    entry to be written.
C
C$ Detailed_Output
C
C     CVLEN          is the length of the returned string value.  This
C                    is the index of the last non-blank character of 
C                    the string.  This definition applies to both fixed-
C                    and variable-length strings.
C
C                    CVLEN is set to 1 if the column entry is null.
C
C     CVAL           is the value read from the specified column entry.
C                    If CVAL has insufficient length to hold the
C                    returned string value, the output value is
C                    truncated on the right.  Entries that are shorter
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
C     2)  If the specified column entry has not been initialized, the
C         error SPICE(UNINITIALIZED) is signaled.
C
C     3)  If the ordinal position of the column specified by COLDSC
C         is out of range, the error SPICE(INVALIDINDEX) is signaled.
C
C     4)  If the output string CVAL is too short to accommodate the
C         returned string value, the output value is truncated on the
C         right.  No error is signaled.
C
C     5)  If an I/O error occurs while reading the indicated file,
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
C     This routine is a utility for reading data from class 3 columns.
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
C-    SPICELIB Version 1.3.0, 31-MAY-2010 (NJB)
C
C        Bug fix: call to DASRDI was overwriting local memory. This
C        problem did not affect operation of the routine except on
C        the Mac/Intel/OSX/ifort/32-bit platform, on which it caused
C        a segmentation fault when this routine was compiled with
C        default optimization.
C
C-    SPICELIB Version 1.2.0, 23-JUL-1999 (NJB)
C
C        Error check for string truncation on output was removed.
C        This error check interfered with the use of this routine
C        (via a call to ZZEKRSC) within ZZEKJSRT, which relies on
C        being able to read into a buffer initial substrings of scalar 
C        data.
C        
C-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
C
C        Error check for string truncation on output was added.
C        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
C        to SPICE(UNINITIALIZED).  Error messages were enhanced so
C        as to use column names rather than indices.  Miscellaneous
C        header fixes were made.
C        
C-    SPICELIB Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.2.0, 23-JUL-1999 (NJB)
C
C        Error check for string truncation on output was removed.
C        This error check interfered with the use of this routine
C        (via a call to ZZEKRSC) within ZZEKJSRT, which relies on
C        being able to read into a buffer initial substrings of scalar 
C        data.
C        
C-    SPICELIB Version 1.1.0, 25-JUL-1997 (NJB)
C
C        Error check for string truncation on output was added.
C        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
C        to SPICE(UNINITIALIZED), since the previous string exceeded
C        the maximum allowed length for the short error message.
C
C        Error messages were enhanced so as to use column names rather
C        than indices.
C
C-&



 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKRP2N
 
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    COLUMN
      
      INTEGER               AVAIL
      INTEGER               B
      INTEGER               BPOS
      INTEGER               COLIDX
      INTEGER               DATPTR
      INTEGER               E
      INTEGER               EPOS
      INTEGER               L
      INTEGER               N
      INTEGER               NCOLS
      INTEGER               NREC
      INTEGER               P
      INTEGER               PBASE
      INTEGER               RELPTR
      INTEGER               PTRLOC
      INTEGER               RECNO
      INTEGER               UNIT
 
C
C     Use discovery check-in.
C
C     Make sure the column exists.
C
      NCOLS   =  SEGDSC ( NCIDX  )
      COLIDX  =  COLDSC ( ORDIDX )
 
      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN
 
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         CALL DASHLU ( HANDLE, UNIT )
 
         CALL CHKIN  ( 'ZZEKRD03'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' //
     .                 'SEGNO = #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NREC                              )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                    )
         CALL ERRINT ( '#',  RECNO                             )
         CALL ERRFNM ( '#',  UNIT                              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKRD03'                              )
         RETURN
 
      END IF
 
C
C     Compute the data pointer location, and read both the pointer
C     and the stored string size.
C
      PTRLOC  =  RECPTR + DPTBAS + COLIDX
 
      CALL DASRDI  ( HANDLE, PTRLOC, PTRLOC, DATPTR )
 
 
      IF ( DATPTR .GT. 0 ) THEN
C
C        Read the value.  This is slightly more complicated than
C        the numeric cases, because the value may be spread across
C        multiple pages.  Also, we must not write past the end of the
C        output string.
C
C        We'll need the number of the page at which the first character
C        of the string is stored.  This page contains at least one
C        character of the data value.
C
         CALL ZZEKGEI ( HANDLE, DATPTR, CVLEN )
         
C
C        Set the data pointer to the start of the string data, skipping
C        over the encoded string length.
C
         DATPTR  =  DATPTR + ENCSIZ
 
         N       =  MIN ( CVLEN, LEN(CVAL) )
 
C
C        Read the available data from the page under consideration.
C
         CALL ZZEKPGPG ( CHR, DATPTR, P, PBASE )
 
         RELPTR =  DATPTR - PBASE
         AVAIL  =  MIN  ( N,  CPSIZE - RELPTR + 1 )
         B      =  DATPTR
         E      =  DATPTR + AVAIL - 1
         BPOS   =  1
         EPOS   =  AVAIL
         L      =  EPOS - BPOS + 1
 
         CALL DASRDC ( HANDLE, B, E, BPOS, EPOS, CVAL )
 
         N      =  N - L
 
 
         DO WHILE ( N .GT. 0 )
C
C           Read the forward page pointer from the current page; find
C           the base address of the referenced page.
C
            CALL ZZEKGEI  ( HANDLE,  PBASE+CFPIDX,  P     )
            CALL ZZEKPGBS ( CHR,     P,             PBASE )
 
            AVAIL  =  MIN  ( N, CPSIZE )
            B      =  PBASE + 1
            E      =  PBASE + AVAIL
            BPOS   =  EPOS  + 1
            EPOS   =  EPOS  + AVAIL
 
            CALL DASRDC ( HANDLE, B, E, BPOS, EPOS, CVAL )
 
            N      =  N    - AVAIL
            BPOS   =  EPOS + 1
 
         END DO
 
C
C        Blank-pad CVAL if required.
C
         IF ( LEN(CVAL) .GT. EPOS ) THEN
            CVAL( EPOS+1 : )  =  ' '
         END IF
 
 
         ISNULL  =  .FALSE.
 
 
      ELSE IF ( DATPTR .EQ. NULL ) THEN
C
C        The value is null.
C
         ISNULL =  .TRUE.
         CVLEN  =   1
 
 
      ELSE IF (       ( DATPTR .EQ. UNINIT )
     .           .OR. ( DATPTR .EQ. NOBACK )  )  THEN
C
C        The data value is absent.  This is an error.
C
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         CALL DASHLU   (  HANDLE,  UNIT           )
         CALL ZZEKCNAM ( HANDLE,   COLDSC, COLUMN )  
 
         CALL CHKIN  ( 'ZZEKRD03'                                    )
         CALL SETMSG ( 'Attempted to read uninitialized column '    //
     .                 'entry.  SEGNO = #; COLUMN = #; RECNO = #; ' //
     .                 'EK = #'                                      )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                          )
         CALL ERRCH  ( '#',  COLUMN                                  )
         CALL ERRINT ( '#',  RECNO                                   )
         CALL ERRFNM ( '#',  UNIT                                    )
         CALL SIGERR ( 'SPICE(UNINITIALIZED)'                        )
         CALL CHKOUT ( 'ZZEKRD03'                                    )
         RETURN
 
 
      ELSE
C
C        The data pointer is corrupted.
C
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         CALL DASHLU   (  HANDLE,  UNIT           )
         CALL ZZEKCNAM ( HANDLE,   COLDSC, COLUMN )  
 
         CALL CHKIN  ( 'ZZEKRD03'                                )
         CALL SETMSG ( 'Data pointer is corrupted. SEGNO = #; '  //
     .                 'COLUMN =  #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
         CALL ERRCH  ( '#',  COLUMN                              )
         CALL ERRINT ( '#',  RECNO                               )
         CALL ERRFNM ( '#',  UNIT                                )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKRD03'                                )
         RETURN
 
      END IF
 
 
      RETURN
      END
