C$Procedure ZZEKLLTC ( EK, last less than, character )
 
      SUBROUTINE ZZEKLLTC (  HANDLE,  SEGDSC,  COLDSC,
     .                       CKEY,    PRVLOC,  PRVPTR  )
 
C$ Abstract
C
C     Find the last column value less than a specified key,
C     for a specified, indexed character EK column.
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
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      CHARACTER*(*)         CKEY
      INTEGER               PRVLOC
      INTEGER               PRVPTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     CKEY       I   Character key.
C     PRVLOC     O   Ordinal position of predecessor of CKEY.
C     PRVPTR     O   Pointer to record containing predecessor of CKEY.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.  The file may be open for
C                    reading or writing.
C
C     SEGDSC         is the segment descriptor of the segment
C                    containing the column specified by COLDSC.
C
C     COLDSC         is the column descriptor of the column to be
C                    searched.
C
C     CKEY           is a character string key.  The last column entry
C                    less than this key is sought.
C
C$ Detailed_Output
C
C     PRVLOC         is the ordinal position, according to the order
C                    relation implied by the column's index, of the
C                    record containing the last element less than CKEY.
C
C                    If all elements of the column are greater than
C                    or equal to CKEY, PRVLOC is set to zero.
C
C     PRVPTR         is a pointer to the record containing the element
C                    whose ordinal position is PRVLOC.
C
C                    If all elements of the column are greater than
C                    or equal to CKEY, PRVPTR is set to zero.
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
C     2)  If the data type of the input column is not character,
C         the error SPICE(INVALIDTYPE) is signalled.
C
C     3)  If the input column is not indexed, the error
C         SPICE(NOTINDEXED) is signalled.
C
C     4)  If the index type of the input column is not recognized,
C         the error will be diagnosed by routines called by this
C         routine.
C
C     5)  If an I/O error occurs while reading or writing the indicated
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
C     This routine finds the last column element less than a specified
C     character key, within a specified segment and column.
C
C     In order to support the capability of creating an index for a
C     column that has already been populated with data, this routine
C     does not require that number of elements referenced by the
C     input column's index match the number of elements in the column;
C     the index is allowed to reference fewer elements.  However,
C     every record referenced by the index must be populated with data.
C
C$ Examples
C
C     See ZZEKILLT.
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
      LOGICAL               ZZEKSCMP
 
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    COLUMN
 
      INTEGER               BEGIN
      INTEGER               BEGPTR
      INTEGER               DTYPE
      INTEGER               END
      INTEGER               ENDPTR
      INTEGER               MIDDLE
      INTEGER               MIDPTR
      INTEGER               NROWS
 
      LOGICAL               INDEXD
 
C
C     Use discovery check-in.
C
C
C     If the column's not indexed, we have no business being here.
C
      INDEXD  =  COLDSC(IXTIDX) .NE. IFALSE
 
      IF ( .NOT. INDEXD ) THEN
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL CHKIN  ( 'ZZEKLLTC'                 )
         CALL SETMSG ( 'Column # is not indexed.' )
         CALL ERRCH  ( '#',  COLUMN               )
         CALL SIGERR ( 'SPICE(NOTINDEXED)'        )
         CALL CHKOUT ( 'ZZEKLLTC'                 )
         RETURN
 
      END IF
 
 
C
C     Check the column's data type.
C
      DTYPE  =  COLDSC(TYPIDX)
 
      IF ( DTYPE .NE. CHR ) THEN
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL CHKIN  ( 'ZZEKLLTC'                               )
         CALL SETMSG ( 'Column # should be CHR but has type #.' )
         CALL ERRCH  ( '#',  COLUMN                             )
         CALL ERRINT ( '#',  DTYPE                              )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                     )
         CALL CHKOUT ( 'ZZEKLLTC'                               )
         RETURN
 
      END IF
 
C
C     Handle the case of an empty segment gracefully.
C
      NROWS  =  SEGDSC(NRIDX)
 
      IF ( NROWS .EQ. 0 ) THEN
 
         PRVLOC = 0
         PRVPTR = 0
 
         RETURN
 
      END IF
 
C
C     The algorithm used here is very like unto that used in LSTLTD.
C
      BEGIN   =   1
      END     =   NROWS
 
C
C     Get the record pointers BEGPTR and ENDPTR of the least and
C     greatest elements in the column.
C
      CALL ZZEKIXLK ( HANDLE, COLDSC, BEGIN, BEGPTR )
      CALL ZZEKIXLK ( HANDLE, COLDSC, END,   ENDPTR )
 
 
      IF  (  ZZEKSCMP ( GE,      HANDLE,  SEGDSC,
     .                  COLDSC,  BEGPTR,  1,       CHR,
     .                  CKEY,    0.D0,    0,      .FALSE.  )  ) THEN
C
C        The smallest entry of the column is greater than or equal to
C        the input value of the same data type, so none of the entries
C        are less than the input value.
C
         PRVLOC = 0
         PRVPTR = 0
 
 
      ELSE IF (  ZZEKSCMP ( LT,      HANDLE,  SEGDSC,
     .                      COLDSC,  ENDPTR,  1,       CHR,
     .                      CKEY,    0.D0,    0,      .FALSE. )  ) THEN
C
C        The last element of the array is less than the input value of
C        the same data type, so it's the last item less than the input
C        value.
C
         PRVLOC = NROWS
 
         CALL ZZEKIXLK ( HANDLE, COLDSC, PRVLOC, PRVPTR )
 
 
      ELSE
C
C        The input value lies between some pair of column entries.
C        The value is greater than the smallest column entry and
C        less than or equal to the greatest entry.
C
         DO WHILE ( END .GT. BEGIN+1 )
C
C           Find the address of the element whose ordinal position
C           is halfway between BEGIN and END.
C
            MIDDLE   =  ( BEGIN  + END )  / 2
 
            CALL ZZEKIXLK ( HANDLE, COLDSC, MIDDLE, MIDPTR )
 
 
            IF (  ZZEKSCMP ( LT,      HANDLE,  SEGDSC,
     .                       COLDSC,  MIDPTR,  1,      CHR,
     .                       CKEY,    0.D0,    0,     .FALSE. )  ) THEN
C
C              The middle value is less than the input value of the
C              same data type.
C
               BEGIN = MIDDLE
            ELSE
               END   = MIDDLE
            END IF
C
C           The input value is greater than the element having ordinal
C           position BEGIN and less than or equal to the element having
C           ordinal position END.
C
         END DO
 
         PRVLOC = BEGIN
         CALL ZZEKIXLK ( HANDLE, COLDSC, PRVLOC, PRVPTR )
 
      END IF
 
 
      RETURN
      END
