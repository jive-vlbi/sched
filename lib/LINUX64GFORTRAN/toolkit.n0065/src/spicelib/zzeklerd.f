C$Procedure ZZEKLERD ( EK, LLE, using record numbers, d.p. )
 
      SUBROUTINE ZZEKLERD (  HANDLE,  SEGDSC,  COLDSC,  DKEY,
     .                       RECPTR,  NULL,    PRVIDX,  PRVPTR  )
 
C$ Abstract
C
C     Find the last column value less than or equal to a specified key,
C     for a specified, indexed d.p. EK column, using dictionary
C     ordering on d.p. data values and record pointers.
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
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      DOUBLE PRECISION      DKEY
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
C     DKEY       I   Double precision key.
C     RECPTR     I   Record pointer.
C     NULL       I   Null flag.
C     PRVIDX     O   Ordinal position of predecessor of DKEY.
C     PRVPTR     O   Pointer to record containing predecessor of DKEY.
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
C     DKEY,
C     RECPTR         are, respectively, a double precision key and
C                    a pointer to the EK record containing that key.
C                    The last column entry less than or equal to
C                    this key is sought.  The order relation used
C                    is dictionary ordering on the pair (DKEY, RECPTR).
C
C     NULL           is a logical flag indicating whether the input
C                    key is null.  When NULL is .TRUE., DKEY is
C                    ignored by this routine.
C
C$ Detailed_Output
C
C     PRVIDX         is the ordinal position, according to the order
C                    relation implied by the column's index, of the
C                    record containing the last element less than or
C                    equal to DKEY, where the order relation is
C                    as indicated above.  If the column contains
C                    elements equal to DKEY, PRVIDX is the index of the
C                    last such element.
C
C                    If all elements of the column are greater than
C                    DKEY, PRVIDX is set to zero.
C
C     PRVPTR         is a pointer to the record containing the element
C                    whose ordinal position is PRVIDX.
C
C                    If all elements of the column are greater than
C                    DKEY, PRVPTR is set to zero.
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     2)  If the data type of the input column is not d.p.,
C         the error SPICE(INVALIDTYPE) is signalled.
C
C     3)  If the input column is not indexed, the error
C         SPICE(NOTINDEXED) is signalled.
C
C     4)  If the index type of the input column is not recognized,
C         the error SPICE(INVALIDTYPE) is signalled.
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
C     This routine finds the last column element less than or equal
C     to a specified d.p. key, within a specified segment and
C     column.  The order relation used is dictionary ordering on the
C     pair (DKEY, RECPTR).
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
C     See ZZEKIID1.
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
C-    Beta Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Removed an unbalanced call to CHKOUT.
C
C-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    COLUMN
 
      INTEGER               DTYPE
      INTEGER               ITYPE
 
      LOGICAL               INDEXD
 
C
C     Use discovery check-in.
C
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
 
C
C     If the column's not indexed, we have no business being here.
C
      INDEXD  =  COLDSC(IXTIDX) .NE. IFALSE
 
      IF ( .NOT. INDEXD ) THEN
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL CHKIN  ( 'ZZEKLERD'                 )
         CALL SETMSG ( 'Column # is not indexed.' )
         CALL ERRCH  ( '#',  COLUMN               )
         CALL SIGERR ( 'SPICE(NOTINDEXED)'        )
         CALL CHKOUT ( 'ZZEKLERD'                 )
         RETURN
 
      END IF
 
 
C
C     Check the column's data type.
C
      DTYPE  =  COLDSC(TYPIDX)
 
      IF (  ( DTYPE .NE. DP ) .AND. ( DTYPE .NE. TIME )  ) THEN
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL CHKIN  ( 'ZZEKLERD'                                      )
         CALL SETMSG ( 'Column # should be DP or TIME but has type #.' )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                            )
         CALL CHKOUT ( 'ZZEKLERD'                                      )
         RETURN
 
      END IF
 
C
C     Hand the problem off to the subroutine that understands this
C     column's index type.
C
      ITYPE  =  COLDSC(IXTIDX)
 
      IF ( ITYPE .EQ. 1 ) THEN
 
         CALL ZZEKERD1 (  HANDLE,  SEGDSC,  COLDSC,  DKEY,
     .                    RECPTR,  NULL,    PRVIDX,  PRVPTR  )
 
      ELSE
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL CHKIN  ( 'ZZEKLERD'                   )
         CALL SETMSG ( 'Column # has index type #.' )
         CALL ERRCH  ( '#',  COLUMN                 )
         CALL ERRINT ( '#',  ITYPE                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'         )
         CALL CHKOUT ( 'ZZEKLERD'                   )
         RETURN
 
      END IF
 
      RETURN
      END
