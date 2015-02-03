C$Procedure ZZCKCVR3 ( Private --- C-kernel segment coverage, type 03 )
 
      SUBROUTINE ZZCKCVR3 ( HANDLE, ARRBEG, ARREND, SCHEDL )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine the "window" of coverage of a type 03 C-kernel segment.
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
C     CK
C     DAF
C
C$ Keywords
C
C     CK
C     UTILITY
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               HANDLE
      INTEGER               ARRBEG
      INTEGER               ARREND
      DOUBLE PRECISION      SCHEDL ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a C-kernel open for read access
C     ARRBEG     I   Beginning DAF address
C     ARREND     I   Ending DAF address
C     SCHEDL    I/O  An initialized window/schedule of interval
C
C$ Detailed_Input
C
C     HANDLE     is the handle of some DAF that is open for reading.
C
C     ARRBEG     is the beginning address of a type 03 segment
C
C     ARREND     is the ending address of a type 03 segment.
C
C     SCHEDL     is a schedule (window) of intervals, to which the
C                intervals of coverage for this segment will be added.
C
C$ Detailed_Output
C
C     SCHEDL     the input schedule updated to include the intervals
C                of coverage for this segment.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     This routine reads the contents of the file associated with
C     HANDLE to locate coverage intervals.
C
C$ Exceptions
C
C     1) The error SPICE(BADCK3SEGMENT) is signaled if the derived
C        segment length from ARRBEG and ARREND does not match
C        the possible lengths computed from the segment metadata.
C
C     2) Routines in the call tree of this routine may signal errors
C        if insufficient room in SCHEDL exists or other error
C        conditions relating to file access arise.
C
C$ Particulars
C
C     This is a utility routine that determines the intervals
C     of coverage for a type 03 C-kernel segment.
C
C$ Examples
C
C     See CKBRIEF's main driver.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 27-AUG-2002 (FST)
C
C        Updated this routine to use DAFGDA instead of DAFRDA.
C        This allows the module to process non-native kernels.
C
C-    SPICELIB Version 1.0.0, 14-Feb-2000 (WLT)
C
C        Happy Valentine's Day.
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               RETURN
 
C
C     Local Variables
C
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      BUFFER ( 2 )
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      START
      DOUBLE PRECISION      TICK
 
      INTEGER               INTAT
      INTEGER               INTBEG
      INTEGER               INVLS
      INTEGER               LSTINT
      INTEGER               LSTTIK
      INTEGER               NDIR
      INTEGER               NREC
      INTEGER               RSIZE
      INTEGER               TICKAT
 
      INTEGER               NAVSLN
      INTEGER               AVSLN
      INTEGER               SEGLEN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCKCVR3' )
      END IF
 
C
C     Get the number of intervals and pointing instances ( records )
C     in this segment, and from that determine the number of respective
C     directory epochs.
C
      CALL DAFGDA ( HANDLE, ARREND - 1, ARREND, BUFFER )
 
      INVLS  = NINT ( BUFFER ( 1 ) )
      NREC   = NINT ( BUFFER ( 2 ) )
      NDIR   = (NREC-1)/ 100
 
C
C     Determine the size of the pointing packets.  This is dependent
C     on whether angular rate data is present in the segment or not.
C     We can determine this with the following computation:
C
C     Assume a record size of 4, i.e. no angular rate data.
C
      NAVSLN = 5*NREC + NDIR + INVLS + (INVLS-1)/100 + 2
 
C
C     Assume a record size of 7, i.e. angular rate data.
C
      AVSLN = 8*NREC + NDIR + INVLS + (INVLS-1)/100 + 2
 
C
C     Compute the actual length of the segment.
C
      SEGLEN = ARREND - ARRBEG + 1
 
      IF ( SEGLEN .EQ. NAVSLN ) THEN
         RSIZE = 4
      ELSE IF ( SEGLEN .EQ. AVSLN ) THEN
         RSIZE = 7
      ELSE
         CALL SETMSG ( 'The requested segment in file # reports a '
     .   //            'length of # d.p. numbers, but the metadata '
     .   //            'in the segment indicates the length must '
     .   //            'either be # (no angular rate data) or # '
     .   //            '(angular rate data). Perhaps the segment '
     .   //            'is not type 3?'                              )
         CALL ERRHAN ( '#', HANDLE                                   )
         CALL ERRINT ( '#', SEGLEN                                   )
         CALL ERRINT ( '#', NAVSLN                                   )
         CALL ERRINT ( '#', AVSLN                                    )
         CALL SIGERR ( 'SPICE(BADCK3SEGMENT)'                        )
         CALL CHKOUT ( 'ZZCKCVR3'                                    )
         RETURN
      END IF
 
C
C     Recall that the segment is layed out as:
C
C       +------------------------------+
C       |                              |
C       |  Pointing                    |
C       |                              |
C       +------------------------------+
C       |                        |
C       |  SCLK times            |
C       |                        |
C       +------------------------+
C       |                        |
C       |  SCLK directory        |
C       |                        |
C       +------------------------+
C       |                        |
C       |  Interval start times  |
C       |                        |
C       +------------------------+
C       |                        |
C       |  Start times directory |
C       |                        |
C       +------------------------+
C       |                        |
C       |  Number of intervals   |
C       |                        |
C       +------------------------+
C       |                        |
C       |  Number of pointing    |
C       |      instances         |
C       |                        |
C       +------------------------+
C
 
      TICKAT = ARRBEG + RSIZE*NREC
      LSTTIK = TICKAT + NREC - 1
      INTBEG = ARRBEG + RSIZE*NREC + NREC + NDIR
      INTAT  = INTBEG
      LSTINT = INTBEG + INVLS - 1
 
      CALL DAFGDA ( HANDLE, INTAT,  INTAT,   START )
      CALL DAFGDA ( HANDLE, TICKAT, TICKAT,  TICK  )
 
      DO WHILE ( TICK .LT. START .AND. TICKAT .LT. LSTTIK )
         TICKAT = TICKAT + 1
         CALL DAFGDA ( HANDLE, TICKAT, TICKAT, TICK )
      END DO
 
C
C     If we did not find a TICK at least as big as START, we can
C     just return now.
C
      IF ( TICK .LT. START ) THEN
         CALL CHKOUT ( 'ZZCKCVR3' )
         RETURN
      END IF
 
      DO WHILE (      INTAT  .LE. LSTINT
     .          .AND. TICKAT .LE. LSTTIK )
 
C
C        At this point, we have an interval that begins at START
C        and ends at FINISH (unless of course we never found a "good"
C        TICK to start with.)
C
         BEGIN  = START
 
C
C        If the the start of the interval was the start of the LAST
C        interval available, we can short cut the remainder of the
C        reads.
C
         IF ( INTAT .EQ. LSTINT ) THEN
            CALL DAFGDA ( HANDLE, LSTTIK, LSTTIK, FINISH )
            CALL WNINSD ( START, FINISH, SCHEDL )
            CALL CHKOUT ( 'ZZCKCVR3' )
            RETURN
         END IF
 
C
C        This is the expected case.  Get the start of the next
C        interval.
C
         INTAT  = INTAT  + 1
         CALL DAFGDA ( HANDLE, INTAT, INTAT, START )
 
C
C        Read forward from the last tick until we reach the
C        START of the next interval or until we run out of TICKS.
C
         DO WHILE ( TICK .LT. START .AND. TICKAT .LT. LSTTIK )
            FINISH = TICK
            TICKAT = TICKAT + 1
            CALL DAFGDA ( HANDLE, TICKAT, TICKAT, TICK  )
         END DO
 
C
C        A structurally correct CK-3 segment should never allow
C        the next test to pass, but it's just easier to check than
C        police the writers of C-kernels.  The only way to get into
C        the block below is if TICKAT .EQ. LSTTIK
C
         IF ( TICK .LT. START ) THEN
            FINISH = TICK
            TICKAT = TICKAT + 1
         END IF
 
C
C        Insert the interval into the window.
C
         CALL WNINSD ( BEGIN, FINISH, SCHEDL )
 
      END DO
 
      CALL CHKOUT ( 'ZZCKCVR3' )
      RETURN
      END
