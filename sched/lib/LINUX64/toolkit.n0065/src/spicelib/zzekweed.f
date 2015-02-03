C$Procedure  ZZEKWEED ( Private: EK, weed out redundant row vectors )
 
      SUBROUTINE ZZEKWEED ( NJRS, BASES, NROWS )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Weed out redundant, fully qualified row vectors from a join row
C     set union.
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
C     EK
C     PRIVATE
C
C$ Declarations
 
      INCLUDE 'ekjrs.inc'
      INCLUDE 'ekqlimit.inc'
 
      INTEGER               NJRS
      INTEGER               BASES ( * )
      INTEGER               NROWS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NJRS      I-O  Number of join row sets in union.
C     BASES     I-O  Scratch area base addresses of join row sets.
C     NROWS      O   Total number of row vectors in join row set union.
C
C$ Detailed_Input
C
C     NJRS           is the number of join row sets in a join row set
C                    union to be weeded.
C
C     BASES          is an array of base addresses, in the scratch area,
C                    of a collection of join row sets from which
C                    redundant row vectors are to be weeded out.  A row
C                    vector is is redundant if and only if it is
C                    identical to another row vector, and the qualifying
C                    segment vectors of the two row vectors are
C                    identical as well.
C
C$ Detailed_Output
C
C     NJRS           is the number of join row sets after redundant
C                    rows have been removed.  If any join row sets
C                    become empty as a result of this weeding-out,
C                    the count of join row sets is reduced accordingly.
C
C     BASES          is the set of bases of join rows in the join row
C                    set union after weeding has been completed.
C                    Bases of empty join row sets are compressed out;
C                    the valid elements of the array are the first
C                    NJRS elements of BASES, where NJRS has been
C                    updated by this routine.
C
C     NROWS          is the total number of rows in the join row set
C                    union after the weeding process is finished.
C
C     See $Particulars for a more detailed description of the effect of
C     this routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If JRSBAS is not the base address of a structurally valid
C         join row set union, the results of this routine will be
C         unpredictable.
C
C     2)  If NJRS is non-positive, or if NJRS exceeds the maximum
C         allowed number of constraint relations MAXCON, the error
C         SPICE(INVALIDCOUNT) will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine operates by side effects:  it modifies the join row
C     set designated by the input argument JRSBAS.  Every redundant
C     row vector is removed, and join row sets from which row vectors
C     are removed are compressed.  Empty join row sets are removed
C     from the union, as reflected by updates to NJRS and BASES.
C
C     The principal purpose of this routine is to support execution of
C     queries involving OR clauses; such queries may cause row vectors
C     satisfying both disjuncts to be included multiple times in the
C     set of matching row vectors.
C
C     The layout of a join row set in the EK scratch area is shown
C     in the join row set parameter include file.
C
C$ Examples
C
C     See EKSRCH.
C
C$ Restrictions
C
C     1) Loading or unloading EK files between name resolution of the
C        the input query and passing the query to this routine will
C        invalidate the checking done by this routine, and may cause
C        the routine to fail.
C
C     2) Assumes redundant row vectors never occur in any join row set.
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
C-    Beta Version 1.1.0,  8-JAN-1996 (WLT)
C
C        Replaced a call to REPMI with ERRINT in the first
C        error check.
C
C-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               SAMEAI
 
C
C     Local variables
C
      INTEGER               I
      INTEGER               BASE
      INTEGER               CAND
      INTEGER               CANDSV ( MAXTAB )
      INTEGER               CRV
      INTEGER               CRWBAS
      INTEGER               CRWVEC ( MAXTAB + 1 )
      INTEGER               CSGBAS
      INTEGER               CSV
      INTEGER               J
      INTEGER               LOC
      INTEGER               NCNDRV
      INTEGER               NCNDSV
      INTEGER               NDEL
      INTEGER               NPRDRV
      INTEGER               NPRDSV
      INTEGER               NR
      INTEGER               NRLOC
      INTEGER               NSVLOC
      INTEGER               NTAB
      INTEGER               PRED
      INTEGER               PREDSV ( MAXTAB )
      INTEGER               PRV
      INTEGER               PRWBAS
      INTEGER               PRWVEC ( MAXTAB + 1 )
      INTEGER               PSGBAS
      INTEGER               PSV
      INTEGER               RVSIZE
      INTEGER               SVSIZE
 
      LOGICAL               HIT
 
C
C     Use discovery check-in.
C
      IF ( ( NJRS .LT. 1 ) .OR. ( NJRS .GT. MXJRS )  ) THEN
 
         CALL CHKIN  ( 'ZZEKWEED'                                      )
         CALL SETMSG ( 'The number of join row sets in the union is #' )
         CALL ERRINT ( '#', NJRS                                       )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                           )
         CALL CHKOUT ( 'ZZEKWEED'                                      )
         RETURN
 
      END IF
 
C
C     Make sure that the addressing routines are properly initialized.
C
      CALL ZZEKVSET ( NJRS, BASES )
 
C
C     Get the segment vector and row vector sizes.  The sizes that
C     apply to the first join row set will suffice throughout.
C
      LOC     =  BASES(1) + JTCIDX
 
      CALL ZZEKSRD ( LOC, LOC, NTAB )
 
      SVSIZE  =  NTAB
      RVSIZE  =  NTAB + 1
 
C
C     Mark redundant rows vectors for deletion.  One saving grace is
C     that redundant rows can never occur in the same join row set, as
C     long as that join row set represents a set of rows satisfying
C     a conjunction of constraints.
C
      DO CAND = 2, NJRS
C
C        We'll compare row vectors in the CAND join row set to row
C        vectors in the preceeding join row sets.  Only row vectors
C        corresponding to matching segment vectors need be compared.
C        Therefore, we'll loop over the segment vectors in the CAND
C        join row set, and for each such segment vector, loop over the
C        segment vectors in the preceding join row sets.  If a match
C        occurs, we'll compare row vectors corresponding to those
C        segment vectors.
C
C        NCNDSV will contain the number of segment vectors in the
C        `candidate' join row set.
C
         NSVLOC  =  BASES(CAND)  +  JSCIDX
 
         CALL ZZEKSRD  ( NSVLOC, NSVLOC, NCNDSV  )
 
 
         DO CSV = 1, NCNDSV
C
C           Look up the candidate segment vector.
C
            CSGBAS  =  BASES(CAND)  +  JSVBAS  +  (CSV-1)*SVSIZE
 
            CALL ZZEKSRD  ( CSGBAS+1,  CSGBAS+SVSIZE,  CANDSV  )
 
C
C           Get the row vector count and base address of the set of
C           row vectors for the candidate segment vector, in case
C           we need them.  (Referring to the diagram of the join
C           row set structure in the join row set parameter include
C           file may be helpful here.)
C
            BASE  =      BASES(CAND)
     .                +  JSVBAS
     .                +  NCNDSV  * SVSIZE
     .                +  (CSV-1) * 2
 
            CALL ZZEKSRD ( BASE+1,  BASE+1,  CRWBAS )
            CRWBAS = CRWBAS + BASES(CAND)
 
            CALL ZZEKSRD ( BASE+2,  BASE+2,  NCNDRV )
 
 
C
C           For the current predecessor join row set, look up the
C           segment vectors in that join row set and compare them to the
C           candidate.
C
            DO PRED = 1, CAND-1
C
C              Get the count of segment vectors in the current
C              predecessor join row set.
C
               NSVLOC = BASES(PRED) + JSCIDX
 
               CALL ZZEKSRD  ( NSVLOC, NSVLOC, NPRDSV )
 
 
               DO PSV = 1, NPRDSV
C
C                 Look up the predecessor segment vector.
C
                  PSGBAS  =  BASES(PRED)  +  JSVBAS  +  (PSV-1)*SVSIZE
 
                  CALL ZZEKSRD  ( CSGBAS+1,  CSGBAS+SVSIZE,  PREDSV  )
 
C
C                 Compare the segment vectors and hope for the best.
C
                  IF (  SAMEAI ( CANDSV, PREDSV, SVSIZE )  ) THEN
C
C                    Unfortunately, the two segment vectors match, so
C                    there's something to do.  We'll have to compare
C                    every row vector corresponding to the candidate
C                    segment vector with every row vector corresponding
C                    to the predecessor.
C
C                    Get the row vector count and base address of the
C                    set of row vectors for the current predecessor
C                    segment vector.  We already have on hand the
C                    corresponding quantities for the candidate
C                    segment vector.
C
 
                     BASE  =      BASES(PRED)
     .                         +  JSVBAS
     .                         +  NPRDSV  * SVSIZE
     .                         +  (PSV-1) * 2
 
                     CALL ZZEKSRD ( BASE+1,  BASE+1,  PRWBAS )
                     PRWBAS = PRWBAS + BASES(PRED)
 
                     CALL ZZEKSRD ( BASE+2,  BASE+2,  NPRDRV )
 
C
C                    Compare all row vectors.
C
                     DO CRV = 1, NCNDRV
 
 
                        BASE  =  CRWBAS + (CRV-1)*RVSIZE
                        CALL ZZEKSRD ( BASE+1, BASE+RVSIZE, CRWVEC )
 
                        PRV   =  1
                        HIT   =  .FALSE.
 
 
                        DO WHILE (         ( PRV .LE.  NPRDRV )
     .                              .AND.  (     .NOT. HIT    )   )
 
                           BASE  =  PRWBAS + (PRV-1)*RVSIZE
                           CALL ZZEKSRD ( BASE+1, BASE+RVSIZE, PRWVEC )
 
 
                           IF ( SAMEAI(CRWVEC, PRWVEC, RVSIZE) ) THEN
C
C                             The row vectors, together with their
C                             qualifying segment vectors, match.  The
C                             higher-indexed vector is considered
C                             redundant.  To mark this vector for
C                             deletion, we simply zero out the first
C                             element of the row vector.  This makes the
C                             row vector invalid, so it will not match
C                             any valid row vector we see later.
C
                              BASE  =  CRWBAS + (CRV-1)*RVSIZE
                              CALL ZZEKSUPD ( BASE+1, BASE+1, 0 )
                              HIT   =  .TRUE.
                           ELSE
                              PRV   =  PRV + 1
                           END IF
 
                        END DO
 
                     END DO
 
 
                  END IF
C
C                 We've finished comparing row vectors for a pair of
C                 segment vectors, if it was necessary to do so.
C
 
               END DO
C
C              We've compared all segment vectors in the current
C              predecessor join row set with the candidate segment
C              vector.
C
 
            END DO
C
C           We've compared all segment vectors in all predecessor join
C           row sets to the current segment vector.
C
 
         END DO
C
C        We've compared the candidate join row set to its predecessors.
C
 
      END DO
C
C     We've compared all of the join row sets.
C
C
C     Now, clean up the join row set union by compressing out deleted
C     rows, segment vectors, and join row sets.
C
      J      =  1
      NDEL   =  0
 
      DO I = 1, NJRS
C
C        Compress the current join row set.  If it ends up empty,
C        expel it from the union.
C
         CALL ZZEKJSQZ ( BASES(I) )
 
         NRLOC = BASES(I) + JRCIDX
         CALL ZZEKSRD  ( NRLOC, NRLOC, NR )
 
         IF ( NR .EQ. 0 ) THEN
C
C           This entire join row set can be deleted from the union.
C           Consider the next join row set.
C
            NDEL     =  NDEL + 1
 
         ELSE
 
            BASES(J) =  BASES(I)
            J        =  J + 1
 
         END IF
 
      END DO
 
 
      NJRS  = NJRS - NDEL
 
C
C     Count the rows remaining after our clean-up operation.
C
      NROWS = 0
 
      DO I = 1, NJRS
 
         NRLOC = BASES(I) + JRCIDX
 
         CALL ZZEKSRD ( NRLOC, NRLOC, NR )
 
         NROWS = NROWS + NR
 
      END DO
 
 
      RETURN
      END
