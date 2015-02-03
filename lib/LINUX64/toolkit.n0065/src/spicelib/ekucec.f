C$Procedure     EKUCEC ( EK, update d.p. column entry )
 
      SUBROUTINE EKUCEC (  HANDLE,  SEGNO,  RECNO,  COLUMN,
     .                     NVALS,   CVALS,  ISNULL          )
 
C$ Abstract
C
C     Update a character column entry in a specified EK record.
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
C     FILES
C     UTILITY
C
C$ Declarations
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RECNO
      CHARACTER*(*)         COLUMN
      INTEGER               NVALS
      CHARACTER*(*)         CVALS  ( * )
      LOGICAL               ISNULL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to EK file.
C     SEGNO      I   Index of segment containing record.
C     RECNO      I   Record in which entry is to be updated.
C     COLUMN     I   Column name.
C     NVALS      I   Number of values in in new column entry.
C     CVALS      I   Character string values to add to column.
C     ISNULL     I   Flag indicating whether column entry is null.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle attached to an EK open for
C                    write access.
C
C     SEGNO          is the index of the segment containing the column
C                    entry to be updated.
C
C     RECNO          is the index of the record containing the column
C                    entry to be updated.  This record number is
C                    relative to the start of the segment indicated by
C                    SEGNO; the first record in the segment has index 1.
C
C     COLUMN         is the name of the column containing the entry to
C                    be updated.
C
C     NVALS,
C     CVALS          are, respectively, the number of values to add to
C                    the specified column and the set of values
C                    themselves.  The data values are written in to the
C                    specifed column and record.
C
C                    If the  column has fixed-size entries, then NVALS
C                    must equal the entry size for the specified column.
C
C                    For columns with variable-sized entries, the size
C                    of the new entry need not match the size of the
C                    entry it replaces.  In particular, the new entry
C                    may be larger.
C
C     ISNULL         is a logical flag indicating whether the entry is
C                    null.  If ISNULL is .FALSE., the column entry
C                    defined by NVALS and CVALS is added to the
C                    specified kernel file.
C
C                    If ISNULL is .TRUE., NVALS and CVALS are ignored.
C                    The contents of the column entry are undefined.
C                    If the column has fixed-length, variable-size
C                    entries, the number of entries is considered to
C                    be 1.
C
C                    The new entry may be null even though it replaces
C                    a non-null value, and vice versa.
C
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
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
C     2)  If SEGNO is out of range, the error will diagnosed by routines
C         called by this routine.
C
C     3)  If COLUMN is not the name of a declared column, the error
C         will be diagnosed by routines called by this routine.
C
C     4)  If COLUMN specifies a column of whose data type is not
C         CHARACTER, the error SPICE(WRONGDATATYPE) will
C         be signalled.
C
C     5)  If RECNO is out of range, the error will diagnosed by routines
C         called by this routine.
C
C     6)  If the specified column has fixed-size entries and NVALS
C         does not match this size, the error will diagnosed by routines
C         called by this routine.
C
C     7)  If the specified column has variable-size entries and NVALS
C         is non-positive, the error will diagnosed by routines
C         called by this routine.
C
C     8)  If an attempt is made to add a null value to a column that
C         doesn't take null values, the error will diagnosed by routines
C         called by this routine.
C
C     9)  If COLUMN specifies a column of whose class is not
C         a character class known to this routine, the error
C         SPICE(NOCLASS) will be signalled.
C
C     10) If an I/O error occurs while reading or writing the indicated
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
C     This routine operates by side effects:  it modifies the named
C     EK file by adding data to the specified record in the specified
C     column.  Data may be added to a segment in random order; it is not
C     necessary to fill in columns or rows sequentially. Data may only
C     be added one logical element at a time.  Partial assignments of
C     logical elements are not supported.
C
C     Since columns of data type TIME are implemented using double
C     precision column classes, this routine may be used to update
C     columns of type TIME.
C
C$ Examples
C
C     1)  Replace the value in the third record of the column CCOL in
C         the fifth segment of an EK file designated by HANDLE.  Set
C         the new value to '999'.
C
C            CALL EKUCEC ( HANDLE, 5, 3, 'CCOL', 1, '999', .FALSE. )
C
C
C     2)  Same as (1), but this time add a null value.  The argument
C         '999' is ignored because the null flag is set to .TRUE.
C
C            CALL EKUCEC ( HANDLE, 5, 3, 'CCOL', 1, '999', .TRUE. )
C
C
C     3)  Replace the entry in the third record of the column CARRAY in
C         the fifth segment of an EK file designated by HANDLE.  Set
C         the new value using an array CBUFF of 10 string values.
C
C            CALL EKUCEC ( HANDLE, 5, 3, 'CARRAY', 10, CBUFF, .FALSE. )
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
C-    SPICELIB Version 1.1.0, 20-JUN-1999 (WLT)
C
C        Removed unbalanced call to CHKOUT.
C
C-    Beta Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     replace character entry in an EK column
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               CLASS
      INTEGER               DTYPE
      INTEGER               RECPTR
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               UNIT
 
      LOGICAL               ISSHAD
 
C
C     Use discovery check-in.
C
C     First step:  find the descriptor for the named segment.  Using
C     this descriptor, get the column descriptor.
C
      CALL ZZEKSDSC ( HANDLE, SEGNO,  SEGDSC          )
      CALL ZZEKCDSC ( HANDLE, SEGDSC, COLUMN, COLDSC  )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     This column had better be of character type.
C
      DTYPE  =  COLDSC(TYPIDX)
 
      IF ( DTYPE .NE. CHR ) THEN
 
         CALL CHKIN  ( 'EKUCEC'                                        )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Column # is of type #; EKUCEC only works '    //
     .                 'with character columns.  RECNO = #; '      //
     .                 'SEGNO = #; EK = #.'                            )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                          )
         CALL CHKOUT ( 'EKUCEC'                                        )
         RETURN
 
      END IF
 
C
C     Look up the record pointer for the target record.
C
      CALL ZZEKTRDP ( HANDLE, SEGDSC(RTIDX), RECNO, RECPTR )
 
C
C     Determine whether the EK is shadowed.
C
      CALL EKSHDW ( HANDLE, ISSHAD )
 
C
C     If the EK is shadowed, we must back up the current column entry
C     if the entry has not already been backed up.  ZZEKRBCK will
C     handle this task.
C
      IF ( ISSHAD ) THEN
         CALL ZZEKRBCK ( 'UPDATE', HANDLE, SEGDSC, COLDSC, RECNO )
      END IF
 
C
C     Now it's time to carry out the replacement.
C
      CLASS  =  COLDSC ( CLSIDX )
 
 
      IF ( CLASS .EQ. 3 ) THEN
C
C        Class 3 columns contain scalar character data.
C
         CALL ZZEKUE03 ( HANDLE, SEGDSC, COLDSC, RECPTR, CVALS, ISNULL )
 
 
      ELSE IF ( CLASS .EQ. 6 ) THEN
C
C        Class 6 columns contain array-valued character data.
C
         CALL ZZEKUE06 ( HANDLE,  SEGDSC, COLDSC,
     .                   RECPTR,  NVALS,  CVALS,  ISNULL )
 
      ELSE
C
C        This is an unsupported character column class.
C
         SEGNO  =  SEGDSC ( SNOIDX )
 
         CALL CHKIN  ( 'EKUCEC'                                        )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Class # from input column descriptor is not ' //
     .                 'a supported character class.  COLUMN = #; '   //
     .                 'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'EKUCEC'                                        )
         RETURN
 
      END IF
 
      RETURN
      END
