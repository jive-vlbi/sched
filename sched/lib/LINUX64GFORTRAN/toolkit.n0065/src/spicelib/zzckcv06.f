C$Procedure ZZCKCV06 ( Private --- C-kernel segment coverage, type 06 )
 
      SUBROUTINE ZZCKCV06 ( HANDLE, ARRBEG, ARREND, SCLKID,  
     .                      DC,     TOL,    TIMSYS, SCHEDL )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine the "window" of coverage of a type 06 C-kernel segment.
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
 
      INCLUDE              'ck06.inc'
 
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
C     HANDLE     I   Handle of a C-kernel open for read access.
C     ARRBEG     I   Beginning DAF address.
C     ARREND     I   Ending DAF address.
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
C     ARRBEG     is the beginning address of the type 06 segment
C                to be examined.
C
C     ARREND     is the ending address of the type 06 segment.
C
C     SCLKID     is the ID code of the spacecraft clock associated with
C                the object for which the segment contains pointing.
C                This is the ID code used by the SCLK conversion
C                routines.
C
C     DC         is the double precision component of the descriptor of
C                the CK segment. The components are the segment start
C                and stop times.
C
C                Each mini-segment interval is replaced with its
C                intersection with the segment coverage interval
C
C                   [ DC(1), DC(2) ]
C
C                before being expanded by TOL. Mini-segment intervals
C                that don't intersect the segment coverage interval are
C                discarded, even if after expansion by TOL they would
C                have non-empty intersection with the segment coverage
C                interval.
C
C     TOL        is a tolerance value expressed in ticks of the
C                spacecraft clock associated with the segment. After
C                truncation by the segment coverage interval, and
C                before insertion into the coverage window, each
C                non-empty truncated mini-segment interval is expanded
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
C                output coverage window.TIMSYS may have the values:
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
C                of coverage for this segment. The schedule has
C                been adjusted to account for the provided tolerance
C                value. Coverage lying outside the interval
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
C     Several parameters associated with the type 06 C-kernel
C     are utilized to compute the packet size of each subtype.
C     See the include file 'ck06.inc' for details.
C
C$ Exceptions
C
C     1)  The error SPICE(NOTSUPPORTED) is signaled if the subtype of
C         the CK type 06 segment is not recognized.
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
C$ Files
C
C     This routine reads the contents of the file associated with
C     HANDLE to locate coverage intervals.
C
C$ Particulars
C
C     This is a utility routine that determines the intervals
C     of coverage for a type 06 C-kernel segment.
C
C$ Examples
C
C     See CKCOV.
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 05-FEB-2014 (NJB) (BVS) (FST) (WLT)
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Number of elements in a type 6 mini-segment
C     interval directory:
C
      INTEGER               NVDSIZ
      PARAMETER           ( NVDSIZ = 100 )

C
C     Mini-segment epoch directory size:
C
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

C
C     Type 6 control area size:
C
      INTEGER               NSGPAR
      PARAMETER           ( NSGPAR = 2 )

C
C     Type 6 mini-segment control area size:
C
      INTEGER               MCTLSZ 
      PARAMETER           ( MCTLSZ = 4 )

C
C     Local Variables
C
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      BUFFER ( MCTLSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      IVLBDS ( 2 )
      DOUBLE PRECISION      LSTEPC
 
      INTEGER               EPADDR
      INTEGER               I
      INTEGER               IVLBAS
      INTEGER               MINIE
      INTEGER               NDIR
      INTEGER               NINTVL
      INTEGER               NIVDIR
      INTEGER               NREC
      INTEGER               PTRBAS

      LOGICAL               ISTDB
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZCKCV06' )
 
C
C     Check tolerance value.
C
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be non-negative; actual value '//
     .                 'was #.'                                       )
         CALL ERRDP  ( '#',  TOL                                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZCKCV06'                                     )
         RETURN

      END IF

C
C     Set a logical flag indicating whether the time system is SCLK.
C
      ISTDB = EQSTR ( TIMSYS, 'TDB' ) 

C
C     Check time system.
C
      IF ( .NOT. ISTDB ) THEN

         IF (  .NOT.  EQSTR ( TIMSYS, 'SCLK' )  )THEN

            CALL SETMSG ( 'Time system spec TIMSYS was #; allowed ' //
     .                    'values are SCLK and TDB.'                )
            CALL ERRCH  ( '#',  TIMSYS                              )
            CALL SIGERR ( 'SPICE(INVALIDOPTION)'                    )
            CALL CHKOUT ( 'ZZCKCV06'                                )
            RETURN

         END IF

      END IF

C
C     Fetch the mini-segment count from the segment.
C
      CALL DAFGDA ( HANDLE, ARREND, ARREND, BUFFER )

      NINTVL = NINT( BUFFER(1) )

C
C     Each mini-segment contributes a coverage interval to the
C     total coverage of the segment. Since mini-segments can
C     contain gaps, we need to examine not only the mini-segment
C     interval bounds but the final epochs of the mini-segments.
C
C     Let IVLBAS be the base address of the mini-segment interval
C     bounds. Let PTRBAS be the base address of the mini-segment
C     pointers.
C     
C     First compute PTRBAS. There are NINTVL+1 pointers.
C
      PTRBAS = ARREND - NSGPAR - ( NINTVL + 1 )

C
C     Compute the number of mini-segment interval directories.
C     There are NINTVL + 1 interval boundaries, so the directory
C     count is
C
C        (  ( NINTVL + 1 ) - 1  )  /  NVDSIZ
C
C
      NIVDIR = NINTVL / NVDSIZ

C
C     The interval bounds and their directories precede the
C     mini-segment pointers.
C
      IVLBAS = PTRBAS - NIVDIR - ( NINTVL + 1 )

C
C     Now loop over the mini-segments and find the contribution
C     from each one.
C     
      DO I = 1,  NINTVL
C
C        Find the interval bounds for this mini-segment.
C
         CALL DAFGDA ( HANDLE, IVLBAS+I, IVLBAS+I+1, IVLBDS )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZCKCV06' )
            RETURN
         END IF

C
C        Now find the last epoch of this mini-segment, since
C        there could be a gap at the end.
C
C        Find the begin and end pointers for the current 
C        mini-segment. Convert these from relative to 
C        absolute DAF addresses.
C        
         CALL DAFGDA ( HANDLE, PTRBAS+I, PTRBAS+I+1, BUFFER )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZCKCV06' )
            RETURN
         END IF

         MINIE = ( ARRBEG - 1 )  +  NINT( BUFFER(2) )  -  1

C
C        Fetch the mini-segment's record count NREC.
C
         CALL DAFGDA ( HANDLE, MINIE, MINIE, BUFFER )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZCKCV06' )
            RETURN
         END IF

         NREC = NINT( BUFFER(1) )

C
C        Compute the number of epoch directories for this
C        mini-segment.
C
         NDIR = ( NREC - 1 ) / DIRSIZ

C
C        The last epoch precedes the mini-segment control
C        area and the epoch directories.
C        
         EPADDR = MINIE - MCTLSZ - NDIR 

         CALL DAFGDA ( HANDLE, EPADDR, EPADDR, LSTEPC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZCKCV06' )
            RETURN
         END IF


         BEGIN  = IVLBDS(1)         
C
C        The smaller of LSTEPC and IVLBDS(2) is the
C        end of the mini-segment's coverage.
C      
         FINISH = MIN ( LSTEPC, IVLBDS(2) )
C
C        Truncate the interval using the segment bounds.
C
         BEGIN   =  MAX ( BEGIN,  DC(1) )
         FINISH  =  MIN ( FINISH, DC(2) )

C
C        Adjust the interval using the tolerance. Empty
C        intervals *do not get expanded*; this choice is
C        consistent with the type 6 reading algorithm.
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

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZCKCV06' )
               RETURN
            END IF

         END IF

C
C        Insert the interval into the window.
C
         IF ( BEGIN .LE. FINISH ) THEN

            CALL WNINSD ( BEGIN, FINISH, SCHEDL )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZCKCV06' )
               RETURN
            END IF

         END IF

      END DO
 
 
      CALL CHKOUT ( 'ZZCKCV06' )
      RETURN
      END
