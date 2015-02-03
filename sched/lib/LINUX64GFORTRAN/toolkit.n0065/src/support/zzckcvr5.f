C$Procedure ZZCKCVR5 ( Private --- C-kernel segment coverage, type 05 )
 
      SUBROUTINE ZZCKCVR5 ( HANDLE, ARRBEG, ARREND, SCHEDL )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine the "window" of coverage of a type 05 C-kernel segment.
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
 
      INCLUDE              'ck05.inc'
 
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
C     ARRBEG     is the beginning address of a type 05 segment
C
C     ARREND     is the ending address of a type 05 segment.
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
C     Several parameters associated with the type 05 C-kernel
C     are utilized to compute the packet size of each subtype.
C     See the include file 'ck05.inc' for details.
C
C$ Files
C
C     This routine reads the contents of the file associated with
C     HANDLE to locate coverage intervals.
C
C$ Exceptions
C
C     1) The error SPICE(NOTSUPPORTED) is signaled if the subtype
C        of the CK type 05 segment is not recognized.
C
C     2) Routines in the call tree of this routine may signal errors
C        if insufficient room in SCHEDL exists or other error
C        conditions relating to file access arise.
C
C$ Particulars
C
C     This is a utility routine that determines the intervals
C     of coverage for a type 05 C-kernel segment.
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
C-    SPICELIB Version 1.0.0, 28-AUG-2002 (FST)
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
      DOUBLE PRECISION      BUFFER ( 4 )
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
      INTEGER               SUBTYP
      INTEGER               TICKAT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCKCVR5' )
      END IF
 
C
C     Get the meta-data associated with this segment that we
C     require to produce the schedule.
C
C     BUFFER(1) = Subtype Code
C     BUFFER(2) = Window Size
C     BUFFER(3) = Number of Interpolation Intervals
C     BUFFER(4) = Number of Packets
C
      CALL DAFGDA ( HANDLE, ARREND - 3, ARREND, BUFFER )
 
      SUBTYP = NINT ( BUFFER ( 1 ) )
      INVLS  = NINT ( BUFFER ( 3 ) )
      NREC   = NINT ( BUFFER ( 4 ) )
      NDIR   = (NREC-1)/ 100
 
C
C     Compute the packet size.  This requires parameters listed
C     in the include file 'ck05.inc' and is based on the subtype.
C
      IF      ( SUBTYP .EQ. C05TP0 ) THEN
         RSIZE = C05PS0
      ELSE IF ( SUBTYP .EQ. C05TP1 ) THEN
         RSIZE = C05PS1
      ELSE IF ( SUBTYP .EQ. C05TP2 ) THEN
         RSIZE = C05PS2
      ELSE IF ( SUBTYP .EQ. C05TP3 ) THEN
         RSIZE = C05PS3
      ELSE
         CALL SETMSG ( 'CK type 5 subtype <#> is not supported.' )
         CALL ERRINT ( '#', BUFFER(1)                            )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                     )
         CALL CHKOUT ( 'ZZCKCVR5'                                )
         RETURN
      END IF
 
C
C     Recall that the segment is layed out as:
C
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
C       |    Seconds per tick    |
C       +------------------------+
C       |      Subtype code      |
C       +------------------------+
C       |      Window size       |
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
         CALL CHKOUT ( 'ZZCKCVR5' )
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
            CALL CHKOUT ( 'ZZCKCVR5' )
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
C        A structurally correct CK-5 segment should never allow
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
 
 
      CALL CHKOUT ( 'ZZCKCVR5' )
      RETURN
      END
