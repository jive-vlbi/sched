C$Procedure ZZEKERC1 ( EK, LLE using record pointers, char, type 1 )
 
      SUBROUTINE ZZEKERC1 (  HANDLE,  SEGDSC,  COLDSC,  CKEY,
     .                       RECPTR,  NULL,    PRVIDX,  PRVPTR  )
 
C$ Abstract
C
C     Find the last column value less than or equal to a specified key,
C     for a specifed character EK column having a type 1 index, using
C     dictionary ordering on character data values and record pointers.
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
 
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      CHARACTER*(*)         CKEY
      INTEGER               RECPTR
      LOGICAL               NULL
      INTEGER               PRVIDX
      INTEGER               PRVPTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     CKEY       I   Character key.
C     RECPTR     I   Record pointer.
C     NULL       I   Null flag.
C     PRVIDX     O   Ordinal position of predecessor of CKEY.
C     PRVPTR     O   Record pointer for predecessor of CKEY.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for read or write
C                    access.
C
C     SEGDSC         is the segment descriptor of the segment
C                    containing the column specified by COLDSC.
C
C     COLDSC         is the column descriptor of the column to be
C                    searched.
C
C     CKEY,
C     RECPTR         are, respectively, a character key and a pointer
C                    to an EK record containing that key.  The last
C                    column entry less than or equal to this key is
C                    sought.  The order relation used is dictionary
C                    ordering on the pair (CKEY, RECPTR).
C
C     NULL           is a logical flag indicating whether the input
C                    key is null.  When NULL is .TRUE., CKEY is
C                    ignored by this routine.
C
C$ Detailed_Output
C
C     PRVIDX         is the ordinal position, according to the order
C                    relation implied by the column's index, of the
C                    record containing the last element less than or
C                    equal to CKEY, where the order relation is
C                    as indicated above.  If the column contains
C                    elements equal to CKEY, PRVIDX is the index of the
C                    record designated by the input RECPTR.
C
C                    If all elements of the column are greater than
C                    CKEY, PRVIDX is set to zero.
C
C     PRVPTR         is a pointer to the record containing the element
C                    whose ordinal position is PRVIDX.
C
C                    If all elements of the column are greater than
C                    CKEY, PRVPTR is set to zero.
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
C     3)  If the tree is empty, PRVIDX and PRVPTR are set to zero.
C         This case is not considered an error.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine finds the last column element less than or equal
C     to a specified character key, within a specified segment and
C     column.  The column must be indexed by a type 1 index.  The order
C     relation used is dictionary ordering on ordered pairs consisting
C     of data values and record pointers:  if the data values in two
C     column entries are equal, the associated record pointers determine
C     the order relation of the column entries.
C
C     Type 1 indexes are implemented as DAS B*-trees.  The data
C     pointers of an index tree contain record pointers.  Therefore, the
C     tree implements an abstract order vector.
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
C     See ZZEKLERC.
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
C-    Beta Version 1.1.0, 18-MAY-1997 (NJB)
C
C        Errors in comparisons of items of equal value were fixed.
C        In such cases, items are compared according to order of
C        their record pointers.
C
C-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKTRSZ
 
      LOGICAL               ZZEKSCMP
 
C
C     Local variables
C
      INTEGER               BEGIN
      INTEGER               BEGPTR
      INTEGER               END
      INTEGER               ENDPTR
      INTEGER               MIDDLE
      INTEGER               MIDPTR
      INTEGER               NREC
      INTEGER               TREE
      INTEGER               TSIZE
 
      LOGICAL               LEQ
 
C
C     Use discovery check-in.
C
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Make sure the number of records in the segment is at least as
C     large as the number of entries in the index:  we must not look
C     up any entries that don't exist!
C
      TREE   =  COLDSC   ( IXPIDX )
      TSIZE  =  ZZEKTRSZ ( HANDLE, TREE )
      NREC   =  SEGDSC   ( NRIDX  )
 
      IF ( TSIZE .GT. NREC ) THEN
 
         CALL CHKIN  ( 'ZZEKERC1'                                      )
         CALL SETMSG ( 'Index size = # but column contains # records.' )
         CALL ERRINT ( '#',  TSIZE                                     )
         CALL ERRINT ( '#',  NREC                                      )
         CALL SIGERR ( 'SPICE(SIZEMISMATCH)'                           )
         CALL CHKOUT ( 'ZZEKERC1'                                      )
         RETURN
 
      END IF
 
C
C     Handle the case of an empty tree gracefully.
C
      IF ( TSIZE .EQ. 0 ) THEN
 
         PRVIDX = 0
         PRVPTR = 0
 
         RETURN
 
      END IF
 
C
C     The algorithm used here is very like unto that used in LSTLED.
C
      BEGIN   =   1
      END     =   TSIZE
 
C
C     Get the record pointers BEGPTR and ENDPTR of the least and
C     greatest elements in the column.
C
      CALL ZZEKTRDP ( HANDLE, TREE, BEGIN, BEGPTR )
      CALL ZZEKTRDP ( HANDLE, TREE, END,   ENDPTR )
 
C
C     Compare the input value to the smallest value in the column.
C
      IF  (  ZZEKSCMP ( GT,      HANDLE,  SEGDSC,  COLDSC,
     .                  BEGPTR,  1,       CHR,     CKEY,
     .                  0.D0,    0,       NULL            )  ) THEN
C
C        The smallest entry of the column is greater than
C        the input value, so none of the entries
C        are less than or equal to the input value.
C
         PRVIDX = 0
         PRVPTR = 0
 
         RETURN
 
      ELSE IF  (  ZZEKSCMP ( EQ,      HANDLE,  SEGDSC,  COLDSC,
     .                       BEGPTR,  1,       CHR,     CKEY,
     .                       0.D0,    0,       NULL            ) 
     . 
     .          .AND.      (  RECPTR  .LT.  BEGPTR )             ) THEN
C
C        The smallest entry of the column is greater than the input 
C        value, based on a comparison of record pointers, so none of the
C        entries are less than or equal to the input value.
C
         PRVIDX = 0
         PRVPTR = 0
 
         RETURN
 
      END IF
 
C
C     At this point, we know the input value is greater than or equal
C     to the smallest element of the column.
C
C     Compare the input value to the greatest value in the column.
C
      IF (  ZZEKSCMP ( LT,      HANDLE,  SEGDSC,   COLDSC,
     .                 ENDPTR,  1,       CHR,      CKEY,
     .                 0.D0,    0,       NULL             )  ) THEN
C
C        The last element of the column is less than the
C        input value.
C
         PRVIDX  =  TSIZE
 
         CALL ZZEKTRDP ( HANDLE, TREE, PRVIDX, PRVPTR )
 
         RETURN
 
 
      ELSE IF (  ZZEKSCMP ( EQ,      HANDLE,  SEGDSC,  COLDSC,
     .                      ENDPTR,  1,       CHR,     CKEY,
     .                      0.D0,    0,       NULL             )
     .
     .         .AND.      (  ENDPTR  .LE.  RECPTR )       ) THEN
C
C        The last element of the column is less than or equal to the
C        input value, based on a comparison of record pointers.
C
         PRVIDX  =  TSIZE
         PRVPTR  =  ENDPTR
 
         RETURN
 
      END IF
 
C
C     The input value lies between some pair of column entries.
C     The value is greater than or equal to the smallest column entry
C     and less than the greatest entry, according to the dictionary
C     ordering we're using.
C
C     Below, we'll use the variable LEQ to indicate whether the "middle"
C     element in our search is less than or equal to the input value.
C
      DO WHILE ( END .GT. BEGIN+1 )
C
C        Find the record pointer of the element whose ordinal position
C        is halfway between BEGIN and END.
C
         MIDDLE   =  ( BEGIN  + END )  / 2
 
         CALL ZZEKTRDP ( HANDLE, TREE, MIDDLE, MIDPTR )
 
C
C        Determine the order relation between CKEY and the column
C        entry at record MIDPTR.
C
         IF (  ZZEKSCMP ( LT,      HANDLE,  SEGDSC,  COLDSC,
     .                    MIDPTR,  1,       CHR,     CKEY,
     .                    0.D0,    0,       NULL            )  ) THEN
C
C           The column element at record MIDPTR is less than
C           or equal to CKEY, based on data values.
C
            LEQ  =  .TRUE.
 
 
         ELSE IF ( ZZEKSCMP ( EQ,      HANDLE,  SEGDSC,  COLDSC,
     .                        MIDPTR,  1,       CHR,     CKEY,
     .                        0.D0,    0,       NULL           ) ) THEN
C
C           The column entry's value matches CKEY.  We must
C           compare record pointers at this point.
C
            LEQ  =  MIDPTR .LE. RECPTR
 
         ELSE
C
C           The inequality of data values is strict.
C
            LEQ  = .FALSE.
 
         END IF
 
 
         IF ( LEQ ) THEN
C
C           The middle value is less than or equal to the input
C           value.
C
            BEGIN = MIDDLE
         ELSE
            END   = MIDDLE
         END IF
C
C        The input value is greater than or equal to the element
C        having ordinal position BEGIN and strictly less than the
C        element having ordinal position END.
C
      END DO
 
      PRVIDX = BEGIN
      CALL ZZEKTRDP ( HANDLE, TREE, PRVIDX, PRVPTR )
 
      RETURN
      END
