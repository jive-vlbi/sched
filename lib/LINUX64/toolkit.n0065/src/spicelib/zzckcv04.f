C$Procedure ZZCKCV04 ( Private --- C-kernel segment coverage, type 04 )
 
      SUBROUTINE ZZCKCV04 ( HANDLE,  ARRBEG,  ARREND, 
     .                      SCLKID,  TOL,     TIMSYS,  SCHEDL  )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine the "window" of coverage of a type 04 C-kernel segment.
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
 
      INCLUDE              'ckparam.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               HANDLE
      INTEGER               ARRBEG
      INTEGER               ARREND
      INTEGER               SCLKID
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
C     TOL        I   Tolerance in ticks.
C     TIMSYS     I   Time system used to represent coverage.
C     SCHEDL    I/O  An initialized window/schedule of interval
C     CK4RSZ     P   C-kernel Type 04 Maximum Record Size
C
C$ Detailed_Input
C
C     HANDLE     is the handle of some DAF that is open for reading.
C
C     ARRBEG     is the beginning address of a type 04 segment
C
C     ARREND     is the ending address of a type 04 segment.
C
C
C     SCLKID     is the ID code of the spacecraft clock associated with
C                the object for which the segment contains pointing.
C                This is the ID code used by the SCLK conversion
C                routines.
C
C     TOL        is a tolerance value expressed in ticks of the
C                spacecraft clock associated with the segment. Before
C                each interval is inserted into the coverage window,
C                the intervals are expanded by TOL:  the left endpoint
C                of each interval is reduced by TOL and the right
C                endpoint is increased by TOL.  Any intervals that
C                overlap as a result of the expansion are merged.
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
C                of coverage for this segment.
C
C$ Parameters
C
C     CK4RSZ     is the maximum length of a CK4 record (with angular
C                velocity). Defined in the include file 'ckparam.inc'.
C
C$ Files
C
C     This routine reads the contents of the file associated with
C     HANDLE to locate coverage intervals.
C
C$ Exceptions
C
C     1) Routines in the call tree of this routine may signal errors
C        if insufficient room in SCHEDL exists or other error
C        conditions relating to file access arise.
C
C     2) If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
C        signaled.
C
C     3) If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
C        is signaled.
C
C     4) If a time conversion error occurs, the error will be 
C        diagnosed by a routine in the call tree of this routine.
C
C$ Particulars
C
C     This is a utility routine that determines the intervals
C     of coverage for a type 04 C-kernel segment.
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
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 30-DEC-2004 (NJB) (FST)
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               INTMAX
      LOGICAL               EQSTR
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI = 6 )
 
C
C     Local Variables
C
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( 5 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      VALUES ( CK4RSZ )
 
      INTEGER               ENDS   ( 2 )
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               NREC

      LOGICAL               ISTDB
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ZZCKCV04' )

C
C     Check tolerance value.
C
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be non-negative; actual value '//
     .                 'was #.'                                       )
         CALL ERRDP  ( '#',  TOL                                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZCKCV04'                                     )
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
            CALL CHKOUT ( 'ZZCKCV04'                                )
            RETURN

         END IF

      END IF
 
C
C     Build a descriptor record that satisfies the requirements
C     of CKNR04 and SGFPKT.
C
C     Note: This is a hack dependent on the implementation of
C     the generic segments routines.  But for C-kernels it
C     should always work, as ND and NI aren't changing any
C     time soon.
C
      IC ( 1 ) = INTMAX()
      IC ( 2 ) = INTMAX()
      IC ( 3 ) = 4
      IC ( 4 ) = INTMAX()
      IC ( 5 ) = ARRBEG
      IC ( 6 ) = ARREND
 
      DC ( 1 ) = 0.0D0
      DC ( 2 ) = 0.0D0
 
      CALL DAFPS ( ND, NI, DC, IC, DESCR )
 
C
C     Determine the number of records in the array.
C
      CALL CKNR04 ( HANDLE, DESCR, NREC )
 
      DO I = 1, NREC
 
C
C        Extract each packet of pointing coefficients.
C
         CALL SGFPKT ( HANDLE, DESCR, I, I, VALUES, ENDS )
 
C
C        Compute the left and right end points of the interval
C        of coverage related to this packet.
C
         LEFT  = VALUES ( 1 ) - VALUES ( 2 )
         RIGHT = VALUES ( 1 ) + VALUES ( 2 )
 
C
C        Adjust the interval using the tolerance.
C
         IF ( TOL .GT. 0.D0 ) THEN

            LEFT   =  MAX( LEFT  - TOL, 0.D0 )
            RIGHT  =       RIGHT + TOL

         END IF

C
C        Convert the time to TDB if necessary.
C
         IF ( ISTDB ) THEN

            CALL SCT2E ( SCLKID, LEFT,  ET )
            LEFT  = ET

            CALL SCT2E ( SCLKID, RIGHT, ET )
            RIGHT = ET

         END IF

C
C        Store the results in the schedule.
C
         CALL WNINSD ( LEFT, RIGHT, SCHEDL )
 
      END DO
 
      CALL CHKOUT ( 'ZZCKCV04' )
      RETURN
      END
