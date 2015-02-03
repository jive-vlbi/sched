C$Procedure ZZDDHRMU ( Private --- DDH Remove Unit )
 
      SUBROUTINE ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN,
     .                      UTLCK,  UTLUN, NUT           )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Remove an entry from the unit table.
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
C     None.
C
C$ Keywords
C
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE               'zzddhman.inc'
 
      INTEGER               UINDEX
      INTEGER               NFT
 
      INTEGER               UTCST ( * )
      INTEGER               UTHAN ( * )
      LOGICAL               UTLCK ( * )
      INTEGER               UTLUN ( * )
      INTEGER               NUT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UINDEX     I   Row index to remove from the unit table.
C     NFT        I   Number of entries in the file table.
C     UTCST,
C     UTHAN,
C     UTLCK,
C     UTLUN     I/O  Unit table.
C     NUT       I/O  Number of entries in the unit table.
C
C$ Detailed_Input
C
C     HANDLE     is the index of the row in the unit table for the
C                unit to remove.
C
C     NFT        is the number of entries in the file table after
C                the file whose unit is about to be disconnected
C                has been removed.
C
C     UTCST,
C     UTHAN,
C     UTLCK,
C     UTLUN,     are the cost, handle, locked, and logical unit columns
C                of the unit table respectively.
C
C     NUT        is the number of entries in the unit table.
C
C$ Detailed_Output
C
C     UTCST,
C     UTHAN,
C     UTLCK,
C     UTLUN,     are the cost, handle, locked, and logical unit columns
C                of the unit table respectively.  The contents will
C                change, for specifics see the Particulars section
C                below.
C
C     NUT        is the number of entries in the unit table.  Depending
C                on the state of the file table, this may or may not
C                change.  See the $Particulars section below for
C                details.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) SPICE(INDEXOUTOFRANGE) is signaled when the input UINDEX is
C        either greater than NUT or less than 1.
C
C     2) If NUT is 0 on input, then this module simply returns.
C
C$ Particulars
C
C     This routine only manipulates the contents of the unit table.
C     It is utilized to delete an entry in the unit table that is
C     the result of a file 'unload' or close operation.
C
C     If the number of files listed in the file table exceeds the
C     number of entries in the unit table, then this module will
C     reserve the logical unit listed in the row to remove, zero
C     out the cost and return.  In this event, NUT will remain
C     unchanged.
C
C     However, if there are less files in the file table than the
C     number of entries in the unit table, then this routine removes
C     the row and compresses the unit table, as one would expect.
C
C     The file attached to UNIT is not closed by this routine, the
C     closure should occur before invoking this module.
C
C$ Examples
C
C     See ZZDDHHLU for sample usage.
C
C$ Restrictions
C
C     1) This routine operates when an error condition introduced
C        by a prior call to SIGERR exists.  It calls no routines
C        that return on entry when proper inputs are provided.
C        Any updates to this routine must preserve this behavior.
C
C     2) The file attached to the unit that is to be removed should
C        already have been removed from the file table.  This is
C        necessary so the value of NFT reflects the number of files
C        available after the removal.
C
C     3) The logical unit in UTLUN(UINDEX) must be closed or buffered
C        externally prior to calling this routine.  Knowledge of its
C        value could be lost otherwise.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 05-NOV-2001 (FST)
C
C
C-&
 
C
C     Local Variables
C
      INTEGER               I
 
C
C     First check to see if NUT is 0.  If so, just return, as there
C     are no rows to remove.
C
      IF ( NUT .EQ. 0 ) THEN
         RETURN
      END IF
 
C
C     Check to see if we found the UINDEX in the unit table.
C     If not, use discovery check-in, signal an error and return.
C
      IF ( ( UINDEX .GT. NUT ) .OR. ( UINDEX .LT. 1 ) ) THEN
 
         CALL CHKIN  ( 'ZZDDHRMU'                                     )
         CALL SETMSG ( 'Attempt to remove row # from the unit table '
     .   //            'failed because valid row indices range from '
     .   //            '1 to NUT.'                                    )
         CALL ERRINT ( '#', UINDEX                                    )
         CALL ERRINT ( '#', NUT                                       )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZDDHRMU'                                     )
         RETURN
 
      END IF
 
C
C     We have found the row we need to remove from the table.
C     Check to see whether we are to remove this row or simply
C     mark it as zero cost and reserve the unit.  We know this
C     is the case when NFT is greater than or equal to NUT.
C
      IF ( NFT .GE. NUT ) THEN
 
C
C        Zero the cost, clear the handle, and unlock the unit.
C
         UTCST(UINDEX) = 0
         UTHAN(UINDEX) = 0
         UTLCK(UINDEX) = .FALSE.
 
C
C        Reserve the unit for the handle manager's usage and
C        return.
C
         CALL RESLUN ( UTLUN(UINDEX) )
         RETURN
 
      END IF
 
C
C     If we reach here, then we have to remove the row from the
C     unit table and compress.
C
      DO I = UINDEX+1, NUT
 
         UTCST(I-1) = UTCST(I)
         UTHAN(I-1) = UTHAN(I)
         UTLCK(I-1) = UTLCK(I)
         UTLUN(I-1) = UTLUN(I)
 
      END DO
 
C
C     Decrement NUT.
C
      NUT = NUT - 1
 
      RETURN
      END
