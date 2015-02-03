C$Procedure ZZDDHGTU ( Private --- DDH Get Unit )
 
      SUBROUTINE ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Get or prepare an entry in the unit table to receive a new
C     file.
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
 
      INCLUDE              'zzddhman.inc'
 
      INTEGER               UTCST  ( * )
      INTEGER               UTHAN  ( * )
      LOGICAL               UTLCK  ( * )
      INTEGER               UTLUN  ( * )
      INTEGER               NUT
 
      INTEGER               UINDEX
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UTCST,
C     UTHAN,
C     UTLCK,
C     UTLUN,    I/O   Unit table.
C     NUT       I/O   Number of entries in the unit table.
C     UINDEX     O    Row in the unit table that can be replaced.
C
C$ Detailed_Input
C
C     UTCST,
C     UTHAN,
C     UTLCK,
C     UTLUN,     are the arrays respectively containing the cost,
C                handle, locked, and logical unit columns of the
C                unit table.
C
C     NUT        is the number of entries in the unit table.
C
C$ Detailed_Output
C
C     UTCST,
C     UTHAN,
C     UTLCK,
C     UTLUN,     are the arrays respectively containing the cost,
C                handle, locked, and logical unit columns of the
C                unit table.  This may change as a new unit is
C                added or old ones are removed.
C
C     NUT        is the number of entries in the unit table.  This may
C                change as new entries are added.
C
C     UINDEX     is the index of the row where the new unit should
C                be attached.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     This routine may disconnect a file from its logical unit, to
C     successfully process the caller's request for a unit.
C
C$ Exceptions
C
C     1) If GETLUN fails to assign a logical unit for any reason to
C        the row of interest, this routine sets the logical unit to -1,
C        since negative logical units in Fortran are not permitted.
C
C$ Particulars
C
C      This routine only manipulates the contents of the unit table.
C      Any "zero" cost rows in the table indicate rows where the
C      listed logical unit has been reserved, but no file is currently
C      attached.
C
C      Callers of this routine should check FAILED since this
C      routine may invoke GETLUN.
C
C$ Examples
C
C     See ZZDDHHLU for sample usage.
C
C$ Restrictions
C
C     1) This routine must not be used to retrieve a unit for a
C        file that is already connected to a unit listed in the
C        unit table.
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
C-    SPICELIB Version 1.0.0, 29-MAY-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Variables
C
      INTEGER               I
      INTEGER               ORDERV ( UTSIZE )
 
      LOGICAL               DONE
 
 
C
C     Standard SPICE discovery error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
C
C     First check the case when the unit table is completely empty.
C
      IF ( NUT .EQ. 0 ) THEN
 
         NUT    = 1
         UINDEX = 1
 
         UTCST(UINDEX) = 0
         UTHAN(UINDEX) = 0
         UTLCK(UINDEX) = .FALSE.
 
         CALL GETLUN ( UTLUN(UINDEX) )
 
C
C        Check FAILED to see if GETLUN signaled an error.  If so, then
C        return an invalid unit to the caller.
C
         IF ( FAILED() ) THEN
            UTLUN(UINDEX) = -1
            RETURN
         END IF
 
C
C        If we end up here, then GETLUN succeeded and we have the new
C        unit.  Now return.
C
         RETURN
 
      END IF
 
C
C     If we reach here, then the table contains at least one entry.
C     Order the table rows by cost.
C
      CALL ORDERI ( UTCST, NUT, ORDERV )
 
C
C     Now check to for '0' cost rows as this indicates rows whose
C     logical units are reserved for this suite of routines usage,
C     but are not currently assigned a file.
C
      IF ( UTCST(ORDERV(1)) .LE. 0 ) THEN
 
         UINDEX = ORDERV(1)
 
C
C        '0' cost rows end up in the unit table as the result of a
C        row deletion, occurring when excess files are present.
C        When this process occurs, the logical unit listed in this
C        row is reserved for this module's usage only with RESLUN.
C        Free it, since we're about to reassign it.
C
         CALL FRELUN ( UTLUN(UINDEX) )
 
         RETURN
 
      END IF
 
C
C     Now if no '0' cost rows exist, check to see if we can
C     expand the table.
C
      IF ( NUT .LT. UTSIZE ) THEN
 
C
C        Now increment NUT and set UINDEX.
C
         NUT    = NUT + 1
         UINDEX = NUT
 
C
C        Prepare the default values for the new row.
C
         UTCST(UINDEX) = 0
         UTHAN(UINDEX) = 0
         UTLCK(UINDEX) = .FALSE.
 
         CALL GETLUN ( UTLUN(UINDEX) )
 
C
C        Check FAILED to see if GETLUN signaled an error.  If so, then
C        return an invalid unit to the caller.
C
         IF ( FAILED() ) THEN
            UTLUN(UINDEX) = -1
            RETURN
         END IF
 
C
C        If we end up here, then GETLUN worked properly.  Now return.
C
         RETURN
 
      END IF
 
C
C     If we reach here, then we have no zero-cost rows and a full unit
C     table.  Now it's time to determine which entry in the table to
C     bump.  We do this by stepping through the order vector until
C     we find the first 'non-locked' row.
C
      I    = 0
      DONE = .FALSE.
 
      DO WHILE ( (.NOT. DONE) .AND. (I .NE. NUT) )
 
         I    = I + 1
         DONE = .NOT. UTLCK( ORDERV(I) )
 
      END DO
 
C
C     Before going any further, signal an error if we discover
C     we have not found a row.
C
      IF ( .NOT. DONE ) THEN
 
         UINDEX = 0
         CALL CHKIN  ( 'ZZDDHGTU' )
         CALL SETMSG ( 'The unit table is full and all entries are '
     .   //            'locked.  This should never happen. Contact '
     .   //            'NAIF.'                                       )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'ZZDDHGTU'                                    )
         RETURN
 
      END IF
 
C
C     Clear UTCST and UTHAN since we intend to disconnect
C     the unit upon return.
C
      UTCST( ORDERV(I) ) = 0
      UTHAN( ORDERV(I) ) = 0
 
C
C     Set UINDEX and CLSLUN, then return.
C
      UINDEX = ORDERV(I)
 
C
C     At this point we need to close the unit from the row of interest.
C
      CLOSE ( UNIT = UTLUN(UINDEX) )
 
      RETURN
      END
