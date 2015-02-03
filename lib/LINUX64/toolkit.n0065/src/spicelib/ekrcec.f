C$Procedure   EKRCEC ( EK, read column entry element, character )
 
      SUBROUTINE EKRCEC ( HANDLE,  SEGNO,  RECNO,  COLUMN,
     .                    NVALS,   CVALS,  ISNULL          )
 
C$ Abstract
C
C     Read data from a character column in a specified EK record.
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
C     RECNO      I   Record from which data is to be read.
C     COLUMN     I   Column name.
C     NVALS      O   Number of values in column entry.
C     CVALS      O   Character values in column entry.
C     ISNULL     O   Flag indicating whether column entry is null.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.  The file may be open for
C                    read or write access.  
C
C     SEGNO          is the index of the segment from which data is to
C                    be read.
C
C     RECNO          is the index of the record from which data is to be
C                    read.  This record number is relative to the start
C                    of the segment indicated by SEGNO; the first
C                    record in the segment has index 1.
C
C     COLUMN         is the name of the column from which data is to be
C                    read.
C
C
C$ Detailed_Output
C
C     NVALS,
C     CVALS          are, respectively, the number of values found in
C                    the specified column entry and the set of values
C                    themselves.  The array CVALS must have sufficient
C                    string length to accommodate the longest string
C                    in the returned column entry.
C
C                    For columns having fixed-size entries, when a 
C                    a column entry is null, NVALS is still set to the
C                    column entry size.  For columns having variable-
C                    size entries, NVALS is set to 1 for null entries.
C
C     ISNULL         is a logical flag indicating whether the returned 
C                    column entry is null.  
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
C     3)  If RECNO is out of range, the error will diagnosed by routines
C         called by this routine.
C
C     4)  If COLUMN is not the name of a declared column, the error
C         will be diagnosed by routines called by this routine.
C
C     5)  If COLUMN specifies a column of whose data type is not
C         character, the error SPICE(WRONGDATATYPE) will be
C         signalled.
C
C     6)  If COLUMN specifies a column of whose class is not
C         a character class known to this routine, the error
C         SPICE(NOCLASS) will be signalled.
C
C     7)  If an attempt is made to read an uninitialized column entry,
C         the error will be diagnosed by routines called by this 
C         routine.  A null entry is considered to be initialized, but
C         entries do not contain null values by default.
C
C     8)  If an I/O error occurs while reading or writing the indicated
C         file, the error will be diagnosed by routines called by this
C         routine.
C
C     9)  If any element of the column entry would be truncated when
C         assigned to an element of CVALS, the error will be diagnosed
C         by routines called by this routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine is a utility that allows an EK file to be read
C     directly without using the high-level query interface.
C
C$ Examples
C
C     1)  Read the value in the third record of the column CCOL in
C         the fifth segment of an EK file designated by HANDLE.
C
C            CALL EKRCEC ( HANDLE, 5, 3, 'CCOL', N, CVAL, ISNULL )
C
C$ Restrictions
C
C     1) EK files open for write access are not necessarily readable.
C        In particular, a column entry can be read only if it has been
C        initialized. The caller is responsible for determining
C        when it is safe to read from files open for write access.
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
C-    SPICELIB Version 1.2.0, 20-JUN-1999 (WLT)
C
C        Removed unbalanced call to CHKOUT.
C
C-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
C
C        Bug fix:  Record number, not record pointer, is now supplied
C        to look up data in the class 9 case.  Miscellaneous header
C        changes were made as well.  Check for string truncation on
C        output has been added.
C
C-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     read character data from EK column
C
C-&

C$ Revisions
C
C-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
C
C        Bug fix:  Record number, not record pointer, is now supplied
C        to look up data in the class 9 case.  For class 9 columns,
C        column entry locations are calculated directly from record 
C        numbers, no indirection is used.
C
C        Miscellaneous header changes were made as well.
C
C        The routines
C
C           ZZEKRD03
C           ZZEKRD06
C           ZZEKRD09
C
C        now check for string truncation on output and signal errors
C        if truncation occurs.
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKESIZ
 
C
C     Local variables
C
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               CLASS
      INTEGER               CVLEN
      INTEGER               DTYPE
      INTEGER               RECPTR
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               UNIT
 
      LOGICAL               FOUND
 
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
 
         CALL CHKIN  ( 'EKRCEC'                                        )
         CALL DASHLU ( HANDLE,  UNIT                                   )
         CALL SETMSG ( 'Column # is of type #; EKRCEC only works '    //
     .                 'with character columns.  RECNO = #; SEGNO = ' //
     .                 '#; EK = #.'                                    )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRFNM ( '#',  UNIT                                      )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                          )
         CALL CHKOUT ( 'EKRCEC'                                        )
         RETURN
 
      END IF
 
C
C     Now it's time to read data from the file.  Call the low-level
C     reader appropriate to the column's class.
C
      CLASS  =  COLDSC ( CLSIDX )
 
 
      IF ( CLASS .EQ. 3 ) THEN
C
C        Look up the record pointer for the target record.
C
         CALL ZZEKTRDP (  HANDLE,  SEGDSC(RTIDX),  RECNO, RECPTR )
 
         CALL ZZEKRD03 (  HANDLE,  SEGDSC, COLDSC,
     .                    RECPTR,  CVLEN,  CVALS,  ISNULL  )
         NVALS  =  1
 
 
      ELSE IF ( CLASS .EQ. 6 ) THEN
 
         CALL ZZEKTRDP ( HANDLE, SEGDSC(RTIDX), RECNO, RECPTR )
 
         NVALS  =  ZZEKESIZ ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
         CALL ZZEKRD06 (  HANDLE,  SEGDSC,  COLDSC,  RECPTR,
     .                    1,       NVALS,   CVALS,   ISNULL,  FOUND  )
 
 
      ELSE IF ( CLASS .EQ. 9 ) THEN
C
C        Records in class 9 columns are identified by a record number
C        rather than a pointer.
C 
         CALL ZZEKRD09 (  HANDLE,  SEGDSC, COLDSC,
     .                    RECNO,   CVLEN,  CVALS,  ISNULL  )
         NVALS  =  1
 
      ELSE
 
C
C        This is an unsupported character column class.
C
         SEGNO  =  SEGDSC ( SNOIDX )
 
         CALL CHKIN  ( 'EKRCEC'                                        )
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
         CALL CHKOUT ( 'EKRCEC'                                        )
         RETURN
 
      END IF
 
 
      RETURN
      END
