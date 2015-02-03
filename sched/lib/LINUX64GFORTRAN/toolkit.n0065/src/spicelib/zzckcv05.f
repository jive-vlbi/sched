C$Procedure ZZCKCV05 ( Private --- C-kernel segment coverage, type 05 )
 
      SUBROUTINE ZZCKCV05 ( HANDLE,  ARRBEG,  ARREND,  SCLKID,  
     .                      DC,      TOL,     TIMSYS,  SCHEDL  )
 
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
      INTEGER               SCLKID
      DOUBLE PRECISION      DC     ( 2 )
      DOUBLE PRECISION      TOL
      CHARACTER*(*)         TIMSYS
      DOUBLE PRECISION      SCHEDL ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a C-kernel open for read access
C     ARRBEG     I   Beginning DAF address
C     ARREND     I   Ending DAF address
C     SCLKID     I   ID of SCLK associated with segment.
C     DC         I   D.p. component of CK segment descriptor.
C     TOL        I   Tolerance in ticks.
C     TIMSYS     I   Time system used to represent coverage.
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
C     SCLKID     is the ID code of the spacecraft clock associated with
C                the object for which the segment contains pointing.
C                This is the ID code used by the SCLK conversion
C                routines.
C
C     DC         is the double precision component of the descriptor of
C                the CK segment.  The components are the segment start
C                and stop times.
C
C                Each interpolation interval is replaced with its
C                intersection with the segment coverage interval
C
C                   [ DC(1), DC(2) ]
C
C                before being expanded by TOL. Interpolation intervals
C                that don't intersect the segment coverage interval are
C                discarded, even if after expansion by TOL they would
C                have non-empty intersection with the segment coverage
C                interval.
C
C     TOL        is a tolerance value expressed in ticks of the
C                spacecraft clock associated with the segment. After
C                truncation by the segment coverage interval, and
C                before insertion into the coverage window, each
C                non-empty truncated interpolation interval is expanded
C                by TOL:  the left endpoint of each interval is reduced
C                by TOL and the right endpoint is increased by TOL.
C                Any intervals that overlap as a result of the
C                expansion are merged.
C
C                The coverage window returned when TOL > 0 indicates
C                the coverage provided by the file to the CK readers
C                CKGPAV and CKGP when that value of TOL is passed to
C                them as an input.
C
C                
C     TIMSYS     is a string indicating the time system used in the
C                output coverage window.  TIMSYS may have the values:
C
C                   'SCLK'    Elements of SCHEDL are expressed in
C                             encoded SCLK ("ticks"), where the clock
C                             is associated with the object designated
C                             by IDCODE.
C
C                   'TDB'     Elements of SCHEDL are expressed as
C                             seconds past J2000 TDB.
C
C                TIMSYS must be consistent with the system used for
C                the contents of SCHEDL on input, if any.
C
C
C     SCHEDL     is a schedule (window) of intervals, to which the
C                intervals of coverage for this segment will be added.
C
C$ Detailed_Output
C
C     SCHEDL     the input schedule updated to include the intervals
C                of coverage for this segment.  The schedule has
C                been adjusted to account for the provided tolerance
C                value.  Coverage lying outside the interval
C
C                   DC(1) - TOL : DC(2) + TOL
C
C                is excluded.
C
C                The elements of SCHEDL are given in the time system
C                indicated by TIMSYS.
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
C     1)  The error SPICE(NOTSUPPORTED) is signaled if the subtype of
C         the CK type 05 segment is not recognized.
C
C     2)  Routines in the call tree of this routine may signal errors
C         if insufficient room in SCHEDL exists or other error
C         conditions relating to file access arise.
C
C     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
C         signaled.
C
C     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
C         is signaled.
C
C     5)  If a time conversion error occurs, the error will be 
C         diagnosed by a routine in the call tree of this routine.
C
C$ Particulars
C
C     This is a utility routine that determines the intervals
C     of coverage for a type 05 C-kernel segment.
C
C$ Examples
C
C     See CKCOV.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 07-JAN-2005 (NJB) (FST) (WLT)
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               EQSTR
      LOGICAL               RETURN

C
C     Local Variables
C
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      BUFFER ( 4 )
      DOUBLE PRECISION      ET
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

      LOGICAL               BAIL
      LOGICAL               ISTDB
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCKCV05' )
      END IF
 
C
C     Check tolerance value.
C
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be non-negative; actual value '//
     .                 'was #.'                                       )
         CALL ERRDP  ( '#',  TOL                                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZCKCV05'                                     )
         RETURN

      END IF

C
C     Set a logical flag indicating whether the time systm is SCLK.
C
      ISTDB  = EQSTR ( TIMSYS, 'TDB' ) 

C
C     Check time system.
C
      IF ( .NOT. ISTDB ) THEN

         IF (  .NOT.  EQSTR ( TIMSYS, 'SCLK' )  )THEN

            CALL SETMSG ( 'Time system spec TIMSYS was #; allowed ' //
     .                    'values are SCLK and TDB.'                )
            CALL ERRCH  ( '#',  TIMSYS                              )
            CALL SIGERR ( 'SPICE(INVALIDOPTION)'                    )
            CALL CHKOUT ( 'ZZCKCV05'                                )
            RETURN

         END IF

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
         CALL CHKOUT ( 'ZZCKCV05'                                )
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
         CALL CHKOUT ( 'ZZCKCV05' )
         RETURN
      END IF
 

      BAIL = .FALSE.

      DO WHILE (       ( INTAT  .LE.  LSTINT )
     .           .AND. ( TICKAT .LE.  LSTTIK )
     .           .AND. (        .NOT. BAIL   )  )
C
C        At this point, we have an interval that begins at START
C        and ends at FINISH (unless of course we never found a "good"
C        TICK to start with.)
C
         BEGIN = START
 
C
C        If the start of the interval was the start of the LAST
C        interval available, we can short cut the remainder of the
C        reads.
C
         IF ( INTAT .EQ. LSTINT ) THEN

            CALL DAFGDA ( HANDLE, LSTTIK, LSTTIK, FINISH )

            BAIL = .TRUE.
C
C           The routine will return at the end of this loop
C           iteration.  But first, we may have to update BEGIN
C           and FINISH, depending on the values of TOL and TIMSYS,
C           and we have to insert these values into SCHEDL.
C           We'll carry out these tasks at the end of this IF block.

 
         ELSE
C
C           This is the expected case.  Get the start of the next
C           interval.
C
            INTAT  = INTAT  + 1
            CALL DAFGDA ( HANDLE, INTAT, INTAT, START )
 
C
C           Read forward from the last tick until we reach the
C           START of the next interval or until we run out of TICKS.
C
            DO WHILE ( TICK .LT. START .AND. TICKAT .LT. LSTTIK )
               FINISH = TICK
               TICKAT = TICKAT + 1
               CALL DAFGDA ( HANDLE, TICKAT, TICKAT, TICK  )
            END DO
 
C
C           A structurally correct CK-5 segment should never allow the
C           next test to pass, but it's just easier to check than
C           police the writers of C-kernels.  The only way to get into
C           the block below is if TICKAT .EQ. LSTTIK
C
            IF ( TICK .LT. START ) THEN
               FINISH = TICK
               TICKAT = TICKAT + 1
            END IF
 
         END IF

C
C        Truncate the interval using the segment bounds.
C
         BEGIN   =  MAX ( BEGIN,  DC(1) )
         FINISH  =  MIN ( FINISH, DC(2) )

C
C        Adjust the interval using the tolerance.  Empty
C        intervals *do not get expanded*; this choice is
C        consistent with the type 5 reading algorithm.
C
         IF ( BEGIN .LE. FINISH ) THEN

            IF ( TOL .GT. 0.D0 ) THEN

               BEGIN   =  MAX( BEGIN  - TOL, 0.D0 )
               FINISH  =       FINISH + TOL

            END IF 

         END IF

C
C        Convert the time to TDB if necessary.
C
         IF ( ISTDB ) THEN

            CALL SCT2E ( SCLKID, BEGIN,  ET )
            BEGIN  = ET

            CALL SCT2E ( SCLKID, FINISH, ET )
            FINISH = ET

         END IF

C
C        Insert the interval into the window.
C
         IF ( BEGIN .LE. FINISH ) THEN
            CALL WNINSD ( BEGIN, FINISH, SCHEDL )
         END IF

      END DO
 
 
      CALL CHKOUT ( 'ZZCKCV05' )
      RETURN
      END
