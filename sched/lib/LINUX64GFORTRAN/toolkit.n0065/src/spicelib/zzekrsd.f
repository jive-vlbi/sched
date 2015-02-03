C$Procedure   ZZEKRSD ( EK, read scalar, double precision )
 
      SUBROUTINE ZZEKRSD (  HANDLE,  SEGDSC,  COLDSC,  RECPTR,
     .                      ELTIDX,  DVAL,    ISNULL,  FOUND  )
 
C$ Abstract
C
C     Read scalar data from a double precision column in a specified EK
C     record.
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
 
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      INTEGER               ELTIDX
      DOUBLE PRECISION      DVAL
      LOGICAL               ISNULL
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to EK file.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECPTR     I   Pointer to record from which data is to be read.
C     ELTIDX     I   Index of column entry element to be read.
C     DVAL       O   D.p. value in column entry.
C     ISNULL     O   Flag indicating whether column entry is null.
C     FOUND      O   Flag indicting whether entry element was found.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.
C
C     SEGDSC         is the descriptor of the segment from which data is
C                    to be read.
C
C     COLDSC         is the column descriptor corresponding to the
C                    column from which data is to be read.
C
C     RECPTR         is a pointer to the record from which data is to be
C                    read.
C
C     ELTIDX         is the index of the column entry element to read.
C                    If the column entry is scalar, this argument is
C                    ignored.
C
C$ Detailed_Output
C
C     DVAL           is the specified column entry.  DVAL is valid only
C                    when FOUND is set to .TRUE.
C
C     ISNULL         is a logical flag indicating whether the entry is
C                    null.  ISNULL is valid only when FOUND is set to
C                    .TRUE.
C
C     FOUND          is a logical flag indicating whether the specified
C                    column entry element was found.  For vector-valued
C                    columns, if ELTIDX refers to a non-existent
C                    column entry element, FOUND is set to .FALSE.
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     2)  If COLDSC is not the name of a declared column, the error
C         will be diagnosed by routines called by this routine.
C
C     3)  If COLDSC specifies a column of whose data type is not
C         double precision, the error SPICE(WRONGDATATYPE) will be
C         signalled.
C
C     4)  If COLDSC specifies a column of whose class is not
C         an double precision class known to this routine, the error
C         SPICE(NOCLASS) will be signalled.
C
C     5)  If the indicated column is array-valued, and if ELTIDX is
C         non-positive, the error will be diagnosed by routines called
C         by this routine.  However, if ELTIDX is greater than the
C         number of elements in the specified column entry, FOUND is
C         set to .FALSE. and no error is signalled.
C
C     6)  If an I/O error occurs while reading the indicated file,
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
C     The ZZEKRSx routines are low-level readers that expect column
C     entries to be defined by descriptors.  Since these routines do not
C     look up descriptors, in cases where many successive accesses to
C     the same segment and column are required, these routines are
C     considerably more efficient than the high-level readers.
C
C     These routines do not participate in tracing.
C
C$ Examples
C
C     See ZZEKECMP.
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
C-    Beta Version 1.0.0, 06-NOV-1995 (NJB)
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
 
      INTEGER               CLASS
      INTEGER               DTYPE
      INTEGER               RECNO
      INTEGER               SEGNO
      INTEGER               UNIT
 
C
C     Use discovery check-in.
C
C
C     Nothing found to begin with.
C
      FOUND  =  .FALSE.
 
C
C     This column had better be of d.p. or TIME type.
C
      DTYPE  =  COLDSC(TYPIDX)
 
 
      IF ( ( DTYPE .NE. DP ) .AND. ( DTYPE .NE. TIME )  ) THEN
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
         CALL DASHLU   ( HANDLE, UNIT )
 
         SEGNO  =  SEGDSC   ( SNOIDX )
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
 
         CALL CHKIN  ( 'ZZEKRSD'                                       )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Column # is of type #; ZZEKRSD only works '   //
     .                 'with DP or TIME columns.  RECNO = #; SEGNO = '//
     .                 '#; EK = #.'                                    )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                          )
         CALL CHKOUT ( 'ZZEKRSD'                                       )
         RETURN
 
      END IF
 
C
C     Now it's time to read data from the file.  Call the low-level
C     reader appropriate to the column's class.
C
      CLASS  =  COLDSC ( CLSIDX )
 
 
      IF ( CLASS .EQ. 2 ) THEN
 
         CALL ZZEKRD02 ( HANDLE, SEGDSC, COLDSC, RECPTR, DVAL, ISNULL  )
 
         FOUND  =  .TRUE.
 
 
      ELSE IF ( CLASS .EQ. 5 ) THEN
C
C        Class 5 columns contain d.p. array entries.
C
         CALL ZZEKRD05 ( HANDLE,  SEGDSC,  COLDSC,  RECPTR,
     .                   ELTIDX,  ELTIDX,  DVAL,    ISNULL,  FOUND )
 
 
      ELSE IF ( CLASS .EQ. 8 ) THEN
 
         CALL ZZEKRD08 ( HANDLE, SEGDSC, COLDSC, RECPTR, DVAL, ISNULL  )
 
         FOUND  =  .TRUE.
 
 
      ELSE
 
C
C        This is an unsupported d.p. column class.
C
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
         CALL DASHLU   ( HANDLE, UNIT )
 
         SEGNO  =  SEGDSC   ( SNOIDX )
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
 
         CALL CHKIN  ( 'ZZEKRSD'                                       )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Class # from input column descriptor is not ' //
     .                 'a supported d.p. class.  COLUMN = #; '        //
     .                 'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKRSD'                                       )
         RETURN
 
      END IF
 
 
      RETURN
      END
