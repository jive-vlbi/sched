C$Procedure   ZZEKRD02 ( EK, read class 2 column entry )
 
      SUBROUTINE ZZEKRD02 (  HANDLE,  SEGDSC,  COLDSC,
     .                       RECPTR,  DVAL,    ISNULL  )
 
C$ Abstract
C
C     Read a column entry from a specified record in a class 2 column.
C     Class 2 columns contain scalar double precision values.
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
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      DOUBLE PRECISION      DVAL
      LOGICAL               ISNULL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to EK file.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECPTR     I   Record pointer.
C     DVAL       O   Double precision value in column entry.
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
C     DVAL           is the value read from the specified column entry.
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
C         error SPICE(UNINITIALIZEDVALUE) is signalled.
C
C     3)  If the ordinal position of the column specified by COLDSC
C         is out of range, the error SPICE(INVALIDINDEX) is signalled.
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
C     This routine is a utility for reading data from class 2 columns.
C
C$ Examples
C
C     See EKRCED.
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
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKRP2N
 
C
C     Local variables
C
      INTEGER               COLIDX
      INTEGER               DATPTR
      INTEGER               NCOLS
      INTEGER               NREC
      INTEGER               PTRLOC
      INTEGER               RECNO
      INTEGER               UNIT
 
C
C     Use discovery check-in.
C
C
C     Make sure the column exists.
C
      NCOLS   =  SEGDSC ( NCIDX  )
      COLIDX  =  COLDSC ( ORDIDX )
 
      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN
 
         CALL DASHLU ( HANDLE, UNIT )
 
         CALL CHKIN  ( 'ZZEKRD02'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NREC                              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKRD02'                              )
         RETURN
 
      END IF
 
C
C     Compute the data pointer location, and read the pointer.
C
      PTRLOC  =  RECPTR + DPTBAS + COLIDX
 
      CALL DASRDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )
 
 
      IF ( DATPTR .GT. 0 ) THEN
C
C        Just read the value.
C
         CALL DASRDD ( HANDLE, DATPTR, DATPTR, DVAL )
         ISNULL  =  .FALSE.
 
 
      ELSE IF ( DATPTR .EQ. NULL ) THEN
C
C        The value is null.
C
         ISNULL =  .TRUE.
 
 
      ELSE IF (       ( DATPTR .EQ. UNINIT )
     .           .OR. ( DATPTR .EQ. NOBACK )  )  THEN
C
C        The data value is absent.  This is an error.
C
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         CALL DASHLU (  HANDLE,  UNIT  )
 
         CALL CHKIN  ( 'ZZEKRD02'                                    )
         CALL SETMSG ( 'Attempted to read uninitialized column '    //
     .                 'entry.  SEGNO = #; COLIDX = #; RECNO = #; ' //
     .                 'EK = #'                                      )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                          )
         CALL ERRINT ( '#',  COLIDX                                  )
         CALL ERRINT ( '#',  RECNO                                   )
         CALL ERRFNM ( '#',  UNIT                                    )
         CALL SIGERR ( 'SPICE(UNINITIALIZEDVALUE)'                   )
         CALL CHKOUT ( 'ZZEKRD02'                                    )
         RETURN
 
 
      ELSE
C
C        The data pointer is corrupted.
C
         CALL DASHLU (  HANDLE,  UNIT  )
 
         CALL CHKIN  ( 'ZZEKRD02'                                )
         CALL SETMSG ( 'Data pointer is corrupted. SEGNO = #; '  //
     .                 'COLIDX =  #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
         CALL ERRINT ( '#',  COLIDX                              )
         CALL ERRINT ( '#',  RECNO                               )
         CALL ERRFNM ( '#',  UNIT                                )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKRD02'                                )
         RETURN
 
      END IF
 
 
      RETURN
      END
