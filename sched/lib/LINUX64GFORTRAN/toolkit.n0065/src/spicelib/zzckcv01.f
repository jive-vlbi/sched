C$Procedure ZZCKCV01 ( Private --- C-kernel segment coverage, type 01 )
 
      SUBROUTINE ZZCKCV01 ( HANDLE,  ARRBEG,  ARREND, 
     .                      SCLKID,  TOL,     TIMSYS,  SCHEDL  )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine the "window" of coverage of a type 01 C-kernel segment.
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
C
C$ Detailed_Input
C
C     HANDLE     is the handle of some DAF that is open for reading.
C
C     ARRBEG     is the beginning address of a type 01 segment
C
C     ARREND     is the ending address of a type 01 segment.
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
C                of coverage for this segment. Since type 01 segments,
C                don't have interpolation intervals, each epoch
C                associated with a pointing instance is treated as a
C                singleton interval.
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
C     1) The error SPICE(BADCK1SEGMENT) is signaled if the derived
C        segment length from ARRBEG and ARREND does not match
C        the possible lengths computed from the segment metadata.
C
C     2) Routines in the call tree of this routine may signal errors
C        if insufficient room in SCHEDL exists or other error
C        conditions relating to file access arise.
C
C     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
C         signaled.
C
C     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
C         is signaled.
C
C     5) If a time conversion error occurs, the error will be 
C         diagnosed by a routine in the call tree of this routine.
CC
C$ Particulars
C
C     This is a utility routine that determines the intervals of
C     coverage for a type 01 C-kernel segment. Since type 01 segments,
C     don't have interpolation intervals, each epoch associated with a
C     pointing instance is treated as a singleton interval.
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SUPPORT Version 1.0.0, 03-JAN-2005 (WLT)(NJB)(BVS)
C
C        Initial version.
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               EQSTR
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )

      INTEGER               QAVSIZ
      PARAMETER           ( QAVSIZ = 7 )

      INTEGER               QSIZ
      PARAMETER           ( QSIZ   = 4 )

C
C     Local Variables
C
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FINISH

      INTEGER               I
      INTEGER               N
      INTEGER               NREC
      INTEGER               OFFSET
      INTEGER               PSIZ
      INTEGER               REMAIN
      INTEGER               TBASE
      INTEGER               NAVSLN 
      INTEGER               AVSLN
      INTEGER               SEGLEN

      LOGICAL               ISTDB

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCKCV01' )
      END IF

C
C     Check tolerance value.
C
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be non-negative; actual value '//
     .                 'was #.'                                       )
         CALL ERRDP  ( '#',  TOL                                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZCKCV01'                                     )
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
            CALL CHKOUT ( 'ZZCKCV01'                                )
            RETURN

         END IF

      END IF
 
C
C     The coverage window is the set of singleton intervals consisting
C     of the epochs of the pointing records. We'll need to find the 
C     epochs.
C
C     First, get the number of records in this segment.
C
      CALL DAFGDA ( HANDLE, ARREND, ARREND, BUFFER )

      NREC = INT( BUFFER(1) )

C
C     Determine the size of the pointing packets. This is dependent
C     on whether angular rate data is present in the segment or not.
C     We can determine this with the following computation:
C
C     Assume a record size of 4, i.e. no angular rate data.
C
      NAVSLN = 5 * NREC + ( NREC - 1 ) / 100 + 1
 
C
C     Assume a record size of 7, i.e. angular rate data.
C
      AVSLN  = 8 * NREC + ( NREC - 1 ) / 100 + 1
 
C
C     Compute the actual length of the segment.
C
      SEGLEN = ARREND - ARRBEG + 1
 
      IF ( SEGLEN .EQ. NAVSLN ) THEN
         PSIZ = QSIZ
      ELSE IF ( SEGLEN .EQ. AVSLN ) THEN
         PSIZ = QAVSIZ
      ELSE
         CALL SETMSG ( 'The requested segment in file # reports a '
     .   //            'length of # d.p. numbers, but the metadata '
     .   //            'in the segment indicates the length must '
     .   //            'either be # (no angular rate data) or # '
     .   //            '(angular rate data). Perhaps the segment '
     .   //            'is not type 1?'                              )
         CALL ERRHAN ( '#', HANDLE                                   )
         CALL ERRINT ( '#', SEGLEN                                   )
         CALL ERRINT ( '#', NAVSLN                                   )
         CALL ERRINT ( '#', AVSLN                                    )
         CALL SIGERR ( 'SPICE(BADCK1SEGMENT)'                        )
         CALL CHKOUT ( 'ZZCKCV01'                                    )
         RETURN
      END IF
 
C
C     The epochs start right after the pointing data. Let TBASE be the
C     address preceding the first epoch.
C
      TBASE  = ARRBEG  +  NREC * PSIZ  -  1 

C
C     Grab the epochs. Make a singleton interval out of each one; add
C     the interval to the coverage window.
C
C     For efficiency, we'll read the epochs into a buffer of length
C     BUFSIZ.
C                  
      REMAIN = NREC
      OFFSET = 0

      DO WHILE ( REMAIN .GT. 0 )

C
C        Buffer the next set of epochs.
C
         N  =  MIN ( BUFSIZ, REMAIN )

         CALL DAFGDA ( HANDLE, 
     .                 TBASE + OFFSET + 1, 
     .                 TBASE + OFFSET + N,
     .                 BUFFER              )

C
C        Insert the current batch of N singleton intervals.
C                     
         DO I = 1, N 

            BEGIN   =  BUFFER(I)
            FINISH  =  BUFFER(I)


            IF ( TOL .GT. 0.D0 ) THEN
C
C              Adjust the interval using the tolerance.
C
               BEGIN   =  MAX( BEGIN  - TOL, 0.D0 )
               FINISH  =       FINISH + TOL

            END IF

C
C           Convert the time to TDB if necessary.
C
            IF ( ISTDB ) THEN

               CALL SCT2E ( SCLKID, BEGIN,  ET )
               BEGIN  = ET

               CALL SCT2E ( SCLKID, FINISH, ET )
               FINISH = ET

            END IF

            CALL WNINSD ( BEGIN, FINISH, SCHEDL )

         END DO

         OFFSET = OFFSET + N
         REMAIN = REMAIN - N

      END DO
 
      CALL CHKOUT ( 'ZZCKCV01' )
      RETURN
      END
