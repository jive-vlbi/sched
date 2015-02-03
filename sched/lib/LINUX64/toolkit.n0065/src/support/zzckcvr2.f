C$Procedure ZZCKCVR2 ( Private --- C-kernel segment coverage, type 02 )
 
      SUBROUTINE ZZCKCVR2 ( HANDLE, ARRBEG, ARREND, SCHEDL )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine the "window" of coverage of a type 02 C-kernel segment.
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
C     ARRBEG     is the beginning address of a type 02 segment
C
C     ARREND     is the ending address of a type 02 segment.
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
C     Routines in the call tree of this routine may signal errors
C     if insufficient room in SCHEDL exists or other error
C     conditions relating to file access arise.
C
C$ Particulars
C
C     This is a utility routine that determines the intervals
C     of coverage for a type 02 C-kernel segment.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SUPPORT Version 2.1.0, 13-FEB-2003 (BVS)
C
C        Replaced MAX with MIN in the assignment of GET. This bug
C        caused the routine either to look beyond the end of the
C        start/stop time blocks of the segment (for NREC < BSIZE) or to
C        attempt to fill in internal buffers with more data than they
C        were declared to hold (for NREC > BSIZE.)
C
C-    SUPPORT Version 2.0.0, 27-AUG-2002 (FST)
C
C        Updated this routine to use DAFGDA instead of DAFRDA.
C        This allows the module to process non-native kernels.
C
C        Header and code clean up for delivery to SUPPORT.
C
C-    SUPPORT Version 1.0.0, 14-Feb-2000 (WLT)
C
C        Happy Valentine's Day.
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               PSIZE
      PARAMETER           ( PSIZE = 8 )
 
      INTEGER               BSIZE
      PARAMETER           ( BSIZE = 100 )
 
C
C     Local Variables
C
      DOUBLE PRECISION      FIRST ( BSIZE )
      DOUBLE PRECISION      LAST  ( BSIZE )
 
      INTEGER               ARRSIZ
      INTEGER               BEGAT
      INTEGER               ENDAT
      INTEGER               GET
      INTEGER               GOT
      INTEGER               I
      INTEGER               NREC
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCKCVR2' )
      END IF
 
C
C     Determine the size of the array and the number of records
C     in it.
C
      ARRSIZ = ARREND - ARRBEG + 1
      NREC   = NINT (  ( 100.D0 * (DBLE(ARRSIZ)) + 1.D0 ) / 1001.D0  )
 
C
C     The variable GOT tells us how many time endpoints we've
C     gotten so far.
C
      GOT = 0
 
      DO WHILE ( GOT .LT. NREC )
 
         GET = MIN ( BSIZE, NREC - GOT )
 
         BEGAT = ARRBEG + NREC*PSIZE + GOT
         ENDAT = ARRBEG + NREC*PSIZE + NREC + GOT
 
C
C        Retrieve the list next list of windows.
C
         CALL DAFGDA ( HANDLE, BEGAT, BEGAT + GET - 1, FIRST )
         CALL DAFGDA ( HANDLE, ENDAT, ENDAT + GET - 1, LAST  )
 
C
C        Insert the coverage intervals into the schedule.
C
         DO I = 1, GET
            CALL WNINSD ( FIRST(I), LAST(I), SCHEDL )
         END DO
 
         GOT = GOT + GET
 
      END DO
 
      CALL CHKOUT ( 'ZZCKCVR2' )
      RETURN
      END
