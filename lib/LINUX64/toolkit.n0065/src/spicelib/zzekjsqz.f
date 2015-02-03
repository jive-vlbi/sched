C$Procedure  ZZEKJSQZ ( Private: EK, join row set squeeze )
 
      SUBROUTINE ZZEKJSQZ ( JRSBAS )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Compress a join row set by eliminating segment vectors for
C     which there are no corresponding row vectors.
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
 
      INCLUDE 'ekjrs.inc'
      INCLUDE 'ekqlimit.inc'
 
      INTEGER               JRSBAS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     JRSBAS     I   Scratch area base address of join row set.
C
C$ Detailed_Input
C
C     JRSBAS         is the base address, in the scratch area, of a
C                    join row set to be compressed.
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
C     1)  If JRSBAS is not the base address of a structurally valid
C         join row set, the results of this routine will be
C         unpredictable.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine operates by side effects:  it modifies the join row
C     set designated by the input argument JRSBAS.  Every row vector
C     marked for deletion is removed.  Every empty segment vector is
C     removed, along with the row count and row vector base for that
C     segment vector.  The join row set is compressed to remove all
C     gaps.   All counts are updated to reflect the updated join row
C     set.
C
C     The purpose of the compression performed by this routine is to
C     save work during joins by reducing the size of the cartesian
C     products of sets of segment vectors.  Also, special cases
C     involving null segment vectors can be avoided by this clean-up
C     mechanism.  Finally, it may be possible to save space in the EK
C     scratch area freed by the compression.
C
C$ Examples
C
C     See EKSRCH.
C
C$ Restrictions
C
C     1) Relies on the EK scratch area.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 07-AUG-2006 (NJB)
C
C        Bug fix:  added intialization of variable NRVDEL to support
C                  operation under the Macintosh Intel Fortran
C                  compiler. Note that this bug did not affect
C                  operation of this routine on other platforms.
C
C-    SPICELIB Version 1.0.0, 10-OCT-1995 (NJB)
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 07-AUG-2006 (NJB)
C
C        Bug fix:  added intialization of variable NRVDEL to support
C                  operation under the Macintosh Intel Fortran
C                  compiler. Note that this bug did not affect
C                  operation of this routine on other platforms. The
C                  statement referencing the uninitialized variable
C                  was:
C
C           IF (  ( RC .EQ. 0 ) .OR. ( NRVDEL .EQ. RC )  ) THEN
C        
C        In the previous version of the code, NRVDEL is uninitialized
C        when NRVDEL is 0.  NRVDEL *is* initialized when RC is
C        non-zero, so the logical value of the IF expression is not
C        affected by the lack of proper intialization.
C
C        However, the Intel Fortran compiler for the Mac flags a runtime
C        error when the above code is exercised.  So NRVDEL is now 
C        initialized prior to the above IF statement.
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               CNTLOC
      INTEGER               DELTA
      INTEGER               I
      INTEGER               J
      INTEGER               NSVDEL
      INTEGER               NRVDEL
      INTEGER               NEWNSV
      INTEGER               NR
      INTEGER               NRLOC
      INTEGER               NSV
      INTEGER               NSVLOC
      INTEGER               NTAB
      INTEGER               NTLOC
      INTEGER               PCPAIR ( 2 )
      INTEGER               PTARG
      INTEGER               PTBASE
      INTEGER               PTRLOC
      INTEGER               RC
      INTEGER               RBASE
      INTEGER               ROWVEC ( MAXTAB+1 )
      INTEGER               RTARG
      INTEGER               RVSIZE
      INTEGER               SEGVEC ( MAXTAB )
      INTEGER               SETBAS
      INTEGER               SIZE
      INTEGER               SIZLOC
      INTEGER               SVBASE
      INTEGER               SVSIZE
      INTEGER               VTARG
 
C
C     Use discovery check-in.
C
C
C     Look up the counts that are of interest:
C
C       -- The table count
C       -- The segment vector count
C       -- The join row set size
C
C     Save the address of each count.
C
      SIZLOC  =  JRSBAS + JSZIDX
      NSVLOC  =  JRSBAS + JSCIDX
      NTLOC   =  JRSBAS + JTCIDX
 
      CALL ZZEKSRD ( SIZLOC, SIZLOC, SIZE   )
      CALL ZZEKSRD ( NTLOC,  NTLOC,  NTAB   )
      CALL ZZEKSRD ( NSVLOC, NSVLOC, NSV    )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Set the sizes of segment and row vectors.
C
      SVSIZE  =  NTAB
      RVSIZE  =  NTAB + 1
 
C
C     For each segment vector, obtain the row count.  Clean up after
C     null segment vectors:  compress out the space allocated for their
C     row vector pointers.  Keep track of the number of deletions.
C
      NSVDEL  =  0
      NRVDEL  =  0
      VTARG   =  JRSBAS + JSVBAS
 
      DO I = 1, NSV
C
C        The location of the row count is CNTLOC.  The row vector base
C        pointer precedes the row count.
C
         CNTLOC  =  JRSBAS + JSVBAS + NSV*SVSIZE + 2*(I-1) + 2
         PTRLOC  =  CNTLOC - 1
 
         CALL ZZEKSRD ( CNTLOC, CNTLOC, RC )
 
 
         IF ( RC .GT. 0 ) THEN
C
C           The row vector set for this segment vector is non-empty.
C           scan the rows, looking for those marked for deletion, and
C           update the row count to reflect the number of rows that
C           we're going to keep.
C
            CALL ZZEKSRD ( PTRLOC, PTRLOC, SETBAS )
 
            NRVDEL = 0
 
            DO J = 1, RC
 
               RBASE  =  JRSBAS + SETBAS + (J-1)*RVSIZE
 
               CALL ZZEKSRD ( RBASE+1, RBASE+1, ROWVEC(1) )
 
               IF ( ROWVEC(1) .EQ. 0 ) THEN
                  NRVDEL = NRVDEL + 1
               END IF
 
            END DO
 
         END IF
 
C
C        Compute the base address of the current segment vector.
C
         SVBASE  =  JRSBAS + JSVBAS + (I-1)*SVSIZE
 
 
         IF (  ( RC .EQ. 0 ) .OR. ( NRVDEL .EQ. RC )  ) THEN
C
C           We're going to delete the current segment vector.  We'll
C           just skip over it without advancing our target pointers.
C
            NSVDEL   =  NSVDEL + 1
 
 
         ELSE IF ( NSVDEL .GT. 0 ) THEN
C
C           We need to shift the current segment vector to its
C           destination.
C
            CALL ZZEKSRD  ( SVBASE+1, SVBASE+SVSIZE, SEGVEC )
            CALL ZZEKSUPD ( VTARG+1,  VTARG+SVSIZE,  SEGVEC )
 
            VTARG = VTARG + SVSIZE
 
         ELSE
C
C           No segment vectors have been deleted yet.  We still must
C           update the target in case we shift vectors later on in this
C           loop.
C
            VTARG = VTARG + SVSIZE
 
         END IF
 
      END DO
 
C
C     At this point, we've compressed out the null segment vectors.
C     The next step is to compress out the row vector counts and row
C     vector pointers that corresponded to those segment vectors.  We
C     also want to remove the gap between the segment vectors and the
C     row vector pointer/count pairs.
C
C     We need to do this only if we deleted some segment vectors.
C
      IF ( NSVDEL .GT. 0 ) THEN
 
         NEWNSV =  NSV    - NSVDEL
         PTARG  =  JRSBAS + JSVBAS + (NEWNSV)*SVSIZE
 
         DO I = 1, NSV
C
C           The row count is RC.
C
            SVSIZE  =  NTAB
            CNTLOC  =  JRSBAS + JSVBAS + NSV*SVSIZE + 2*(I-1) + 2
 
            CALL ZZEKSRD ( CNTLOC, CNTLOC, RC )
 
            PTBASE  =  CNTLOC - 2
 
            IF ( RC .GT. 0 ) THEN
C
C              Shift the current row vector pointer and row vector
C              count.
C
               CALL ZZEKSRD  ( PTBASE+1, PTBASE+2, PCPAIR )
               CALL ZZEKSUPD ( PTARG+1,  PTARG+2,  PCPAIR )
 
               PTARG =  PTARG + 2
 
            END IF
 
         END DO
 
 
      ELSE
 
         NEWNSV = NSV
 
      END IF
 
C
C     Update the segment vector count.
C
      CALL ZZEKSUPD ( NSVLOC, NSVLOC, NEWNSV )
 
C
C     Remove any gaps that may exist between any of the row vectors,
C     or between the end of the segment vector's row vector counts
C     and base addresses and the first row vector.
C
C     The initial target location is the first element following the
C     last segment vector's row vector count.  RTARG is used as a base
C     address; it precedes this location by 1.
C
C     If we deleted any segment vectors, the segment vector pointers
C     embedded in the row vectors must change.  Make these updates
C     if necessary.
C
C
      RTARG   =  JRSBAS + JSVBAS + NEWNSV*(SVSIZE+2)
 
      DO I = 1, NEWNSV
C
C        Find the row count and row pointer for the current segment
C        vector.
C
         CNTLOC  =  JRSBAS + JSVBAS + NEWNSV*SVSIZE + 2*(I-1) + 2
 
         CALL ZZEKSRD ( CNTLOC, CNTLOC, RC )
 
         PTRLOC  =  CNTLOC - 1
 
C
C        Get the row vector set base pointer.  After capturing the
C        current value, we'll update this pointer to account for
C        the shifting of row vectors.
C
         CALL ZZEKSRD  ( PTRLOC, PTRLOC, SETBAS )
 
         RBASE   =  JRSBAS + SETBAS
         DELTA   =  RTARG  - RBASE
 
         CALL ZZEKSUPD ( PTRLOC, PTRLOC, SETBAS+DELTA )
 
C
C        Shift the row vectors for the current segment vector,
C        leaving behind the row vectors marked for deletion.
C
         NRVDEL = 0
 
         DO J = 1, RC
 
            CALL ZZEKSRD  ( RBASE+1, RBASE+RVSIZE, ROWVEC )
 
            IF ( ROWVEC(1) .EQ. 0 ) THEN
C
C              This row vector is to be deleted; don't copy it.
C
               RBASE  = RBASE  + RVSIZE
               NRVDEL = NRVDEL + 1
 
            ELSE
C
C              The segment vector pointer is base-relative.
C
               ROWVEC( RVSIZE )  =  JSVBAS + (I-1)*SVSIZE
 
               CALL ZZEKSUPD ( RTARG+1, RTARG+RVSIZE, ROWVEC )
 
               RBASE  =  RBASE + RVSIZE
               RTARG  =  RTARG + RVSIZE
 
            END IF
 
         END DO
 
C
C        Update the row count for the current segment vector, if
C        necessary.  Note that no segment vector will become empty
C        as a result of the row vector deletions we've done; we
C        already eliminated any segment vectors for which that
C        could happen, before we entered this loop.
C
         IF ( NRVDEL .GT. 0 ) THEN
            CALL ZZEKSUPD (  CNTLOC,  CNTLOC,  RC - NRVDEL  )
         END IF
 
      END DO
 
 
C
C     Update the total row count and size of the join row set.
C
      NR  =  0
 
      DO I = 1, NEWNSV
 
         CNTLOC  =  JRSBAS + JSVBAS + NEWNSV*SVSIZE + (I-1)*2 + 2
 
         CALL ZZEKSRD ( CNTLOC, CNTLOC, RC )
 
         NR      =  NR     + RC
 
      END DO
 
 
      NRLOC  =  JRSBAS   +   JRCIDX
      SIZE   =  JSVBAS   +   NEWNSV * ( SVSIZE + 2 )  +  NR * RVSIZE
 
      CALL ZZEKSUPD ( NRLOC,  NRLOC,  NR   )
      CALL ZZEKSUPD ( SIZLOC, SIZLOC, SIZE )
 
      RETURN
      END
