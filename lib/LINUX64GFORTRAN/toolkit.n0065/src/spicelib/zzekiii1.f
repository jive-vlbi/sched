C$Procedure ZZEKIII1 ( EK, insert into index, integer, type 1 )
 
      SUBROUTINE ZZEKIII1 ( HANDLE, SEGDSC, COLDSC, IKEY, RECPTR, NULL )
 
C$ Abstract
C
C     Insert into a type 1 EK index a record pointer associated with an
C     integer key.  The key and record pointer determine the insertion
C     point via dictionary ordering on (value, record pointer) pairs.
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
      INTEGER               IKEY
      INTEGER               RECPTR
      LOGICAL               NULL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     IKEY       I   Integer key.
C     RECPTR     I   Record pointer.
C     NULL       I   Null flag.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     SEGDSC         is the segment descriptor of the segment
C                    containing the column specified by COLDSC.
C
C     COLDSC         is the column descriptor of the column to
C                    which the index corresponds.
C
C     IKEY           is an integer key.
C
C     RECPTR         is a record pointer associated with the input key.
C
C     NULL           is a logical flag indicating whether the input
C                    value is null.
C
C$ Detailed_Output
C
C     None.  This routine operates by side effects.  See $Particulars
C     for a description of the effect of this routine.
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
C     2)  If the data type of the input column is not integer,
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
C     This routine updates the index of an EK segment to reflect the
C     addition of a record to the segment.  The index must be
C     associated with an integer, scalar column.  The type of the
C     index must be 1.
C
C     The ordinal position of the new item is determined by the key
C     IKEY.  The new item will follow the last item already present
C     in the column having a value less than or equal to IKEY.
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
C     See ZZEKAD01.
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
C-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
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
      INTEGER               PRVIDX
      INTEGER               PRVPTR
      INTEGER               TREE
 
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
 
         CALL CHKIN  ( 'ZZEKIII1'                 )
         CALL SETMSG ( 'Column # is not indexed.' )
         CALL ERRCH  ( '#',  COLUMN               )
         CALL SIGERR ( 'SPICE(NOTINDEXED)'        )
         CALL CHKOUT ( 'ZZEKIII1'                 )
         RETURN
 
      END IF
 
C
C     Check the column's data type.
C
      DTYPE  =  COLDSC(TYPIDX)
 
      IF ( DTYPE .NE. INT ) THEN
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL CHKIN  ( 'ZZEKIII1'                               )
         CALL SETMSG ( 'Column # should be INT but has type #.' )
         CALL ERRCH  ( '#',  COLUMN                             )
         CALL ERRINT ( '#',  DTYPE                              )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                     )
         CALL CHKOUT ( 'ZZEKIII1'                               )
         RETURN
 
      END IF
 
 
      ITYPE  =  COLDSC(IXTIDX)
 
 
      IF ( ITYPE .EQ. 1 ) THEN
C
C        Get the tree pointer from the column descriptor.
C
         TREE  =  COLDSC(IXPIDX)
 
C
C        Locate the predecessor of the input key, record pointer pair.
C
         CALL ZZEKLERI (  HANDLE,  SEGDSC,  COLDSC,  IKEY,
     .                    RECPTR,  NULL,    PRVIDX,  PRVPTR  )
 
C
C        Insert the new record pointer right after the item we've found.
C
         CALL ZZEKTRIN ( HANDLE, TREE, PRVIDX+1, RECPTR )
 
 
      ELSE
 
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )
 
         CALL CHKIN  ( 'ZZEKIII1'                   )
         CALL SETMSG ( 'Column # has index type #.' )
         CALL ERRCH  ( '#',  COLUMN                 )
         CALL ERRINT ( '#',  ITYPE                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'         )
         CALL CHKOUT ( 'ZZEKIII1'                   )
         RETURN
 
      END IF
 
      RETURN
      END
