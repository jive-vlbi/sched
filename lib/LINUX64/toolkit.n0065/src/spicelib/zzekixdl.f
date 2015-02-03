C$Procedure     ZZEKIXDL ( EK, delete record from index )
 
      SUBROUTINE ZZEKIXDL ( HANDLE, SEGDSC, COLDSC, RECPTR )
 
C$ Abstract
C
C     Update an EK column index to reflect deletion of a record
C     specified by a record pointer.
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
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   EK file handle.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECPTR     I   Record pointer to locate.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of an EK file open for write access.
C
C     SEGDSC         is the segment descriptor of the segment
C                    containing the column to be searched.
C
C     COLDSC         is the column descriptor of the column to be
C                    searched.
C
C     RECPTR         is a pointer to a record whose corresponding
C                    index entry is to be deleted.
C
C$ Detailed_Output
C
C     None.  This routine operates by side effects.  See $Particulars
C     for a discussion of the action of this routine.
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
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine supports EK update or delete operations, both of
C     which involve removing pointers to records from column indexes.
C
C$ Examples
C
C     See ZZEKDE01.
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
      LOGICAL               RETURN
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKRP2N
 
C
C     Local variables
C
      INTEGER               IDX
      INTEGER               IDXTYP
      INTEGER               RECNO
      INTEGER               TREE
      INTEGER               UNIT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKIXDL' )
      END IF
 
 
      IDXTYP  =  COLDSC ( IXTIDX )
 
      IF ( IDXTYP .NE. IFALSE ) THEN
C
C        This column is indexed.
C
C        Some entry in the index points to RECPTR.  Find the entry
C        and delete it.
C
         CALL ZZEKFRX ( HANDLE, SEGDSC, COLDSC, RECPTR, IDX )
 
 
         IF ( IDXTYP .EQ. 1 ) THEN
C
C           For type 1 indexes, the index pointer is the root node of
C           a B*-tree.  Just use the tree deletion routine.
C
            TREE  = COLDSC ( IXPIDX )
 
            CALL ZZEKTRDL ( HANDLE, TREE, IDX )
 
         ELSE
C
C           Sorry, no other types of indexes are supported.
C
            CALL SETMSG ( 'The index type # is not supported.' )
            CALL ERRINT ( '#',   IDXTYP                        )
            CALL SIGERR ( 'SPICE(INVALIDTYPE)'                 )
            CALL CHKOUT ( 'ZZEKIXDL'                           )
            RETURN
 
         END IF
 
 
      ELSE
C
C        This routine should not have been called if the column in
C        question is not indexed.
C
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         CALL DASHLU ( HANDLE, UNIT )
 
         CALL SETMSG ( 'Column was not indexed. File = #; ' //
     .                 'RECNO = #; COLIDX = #.'              )
         CALL ERRFNM ( '#',  UNIT                            )
         CALL ERRINT ( '#',  RECNO                           )
         CALL ERRINT ( '#',  COLDSC(ORDIDX)                  )
         CALL SIGERR ( 'SPICE(BUG)'                          )
         CALL CHKOUT ( 'ZZEKIXDL'                            )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'ZZEKIXDL' )
      RETURN
      END
