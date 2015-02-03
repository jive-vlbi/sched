C$Procedure      DELTET ( Delta ET, ET - UTC )
 
      SUBROUTINE DELTET ( EPOCH, EPTYPE, DELTA )
 
C$ Abstract
C
C     Return the value of Delta ET (ET-UTC) for an input epoch.
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
C     TIME
C     KERNEL
C
C$ Keywords
C
C     TIME
C
C$ Declarations
 
      DOUBLE PRECISION EPOCH
      CHARACTER*(*)    EPTYPE
      DOUBLE PRECISION DELTA
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      EPOCH      I   Input epoch (seconds past J2000).
C      EPTYPE     I   Type of input epoch ('UTC' or 'ET').
C      DELTA      O   Delta ET (ET-UTC) at input epoch.
C
C$ Detailed_Input
C
C      EPOCH       is the epoch at which Delta ET is to be computed.
C                  This may be either UTC or ephemeris seconds past
C                  J2000, as specified by EPTYPE.
C
C      EPTYPE      indicates the type of input epoch. It may be either
C                  of the following:
C
C                     'UTC'    input is UTC seconds past J2000.
C                     'ET'     input is ephemeris seconds past J2000.
C
C
C$ Detailed_Output
C
C      DELTA       is the value of
C
C                     Delta ET = ET - UTC
C
C                  at the input epoch. This is added to UTC to give
C                  ET, or subtracted from ET to give UTC. The routine
C                  is reversible: that is, given the following calls,
C
C                     CALL DELTET ( UTC,      'UTC', DEL1 )
C                     CALL DELTET ( UTC+DEL1, 'ET',  DEL2 )
C
C                  the expression
C
C                     ( DEL1 .EQ. DEL2 )
C
C                  is always true.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input epoch is not recognized, the error
C        SPICE(INVALIDEPOCH) is signaled.
C
C     2) If the variables necessary for the computation of DELTA
C        have not been loaded into the kernel pool, the error
C        SPICE(KERNELVARNOTFOUND) is signaled.
C
C     3) If the number of leapseconds in the pool is greater than
C        the local leapseconds buffer size, the error 
C        SPICE(BUFFEROVERFLOW) is signaled.   
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      The constants necessary for computing the offset are taken
C      from the kernel pool, where they are assumed to have been
C      loaded from a kernel file.
C
C      The tables are consulted to determine the number of leap seconds
C      preceding the input epoch. Also, an approximation to the periodic
C      yearly variation (which has an amplitude of just under two
C      milliseconds) in the difference between ET and TAI (Atomic Time)
C      is computed. The final value of Delta ET is given by
C
C            Delta ET = ( ET - TAI ) + leap seconds
C
C$ Examples
C
C      The following example shows how DELTET may be used to convert
C      from UTC seconds past J2000 to ephemeris seconds past J2000.
C
C            CALL DELTET ( UTCSEC, 'UTC', DELTA )
C            ET = UTCSEC + DELTA
C
C      The following example shows how DELTET may be used to convert
C      from ephemeris seconds past J2000 to UTC seconds past J2000.
C
C            CALL DELTET ( ET, 'ET', DELTA )
C            UTCSEC = ET - DELTA
C
C      See the TIME required reading for further examples.
C
C$ Restrictions
C
C      The routines UTC2ET and ET2UTC are preferred for conversions
C      between UTC and ET. This routine is provided mainly as a utility
C      for UTC2ET and ET2UTC.
C
C      The kernel pool containing leapseconds and relativistic terms
C      MUST be loaded prior to calling this subroutine. Examples
C      demonstrating how to load a kernel pool are included in the
C      Required Reading file time.req and in the "Examples"
C      section of this header. For more general information about
C      kernel pools, please consult the Required Reading file
C      kernel.req.
C
C$ Literature_References
C
C      Astronomical Almanac.
C
C$ Author_and_Institution
C
C      W.M. Owen       (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.2, 18-APR-2014 (BVS)
C
C        Minor header edits.
C
C-    SPICELIB Version 1.2.1, 18-MAY-2010 (BVS)
C
C        Removed "C$" marker from text in the header.
C
C-    SPICELIB Version 1.2.0, 24-AUG-1998 (WLT)
C
C        The previous upgrade introduced an error in the fetch
C        of the variable DELTET/M from the kernel pool.  This
C        error was corrected.
C
C-    SPICELIB Version 1.1.0, 20-APR-1998 (NJB)
C
C        Calls to RTPOOL were replaced with calls to GDPOOL, which
C        does more robust error checking.  Check for buffer overflow
C        was added.  Local declarations were re-organized.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU)
C
C-&
 
C$ Index_Entries
C
C     difference between ephemeris time and utc
C
C-&
 
 
C$ Revisions
C
C-     SPICELIB Version 1.2.0, 24-AUG-1998 (WLT)
C
C         The previous upgrade introduced an error in the fetch
C         of the variable DELTET/M from the kernel pool.  This
C         error was corrected.
C

C-     SPICELIB Version 1.1.0, 20-APR-1998 (NJB)
C
C         Calls to RTPOOL were replaced with calls to GDPOOL, which
C         does more robust error checking.
C
C-     Beta Version 1.1.0, 06-OCT-1988 (IMU)
C
C         Tim Colvin of Rand noticed that times returned by UTC2ET
C         and TPARSE differed by one second. Upon closer inspection,
C         crack NAIF staff members deduced that in fact Mr. Colvin
C         had not loaded the kernel pool, and were surprised to learn
C         that no error had occurred.
C
C         Multiple FOUND flags and a bevy of new error messages were
C         implemented to cope with this unfortunate oversight.
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               NMAX
      PARAMETER           ( NMAX = 200 )
 
C
C     Local variables
C
      CHARACTER*20          MISSED   ( 5 )
      CHARACTER*4           TYPE
      CHARACTER*1           DTYPE
      
      DOUBLE PRECISION      AET
      DOUBLE PRECISION      DLEAP  ( 2,NMAX )
      DOUBLE PRECISION      DTA
      DOUBLE PRECISION      EA
      DOUBLE PRECISION      EB
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETTAI
      DOUBLE PRECISION      K
      DOUBLE PRECISION      LEAPS
      DOUBLE PRECISION      M    ( 0:1 )
      DOUBLE PRECISION      MA
      
      INTEGER               I
      INTEGER               N
      INTEGER               NLEAP
      
      LOGICAL               FOUND    ( 5 )
 
C
C     Saved variables
C
      SAVE                  MISSED
 
C
C     Initial values
C
      DATA                  MISSED   / 'DELTET/DELTA_T_A, #',
     .                                 'DELTET/K, #',
     .                                 'DELTET/EB, #',
     .                                 'DELTET/M, #',
     .                                 'DELTET/DELTA_AT, #'    /
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DELTET' )
      END IF
 
C
C     Convert the epoch type to uppercase, to simplify comparisons.
C
      CALL UCASE ( EPTYPE, TYPE )
 
C
C     Extract the necessary constants from the kernel pool.
C     Leap seconds and their epochs are interleaved in DELTA_AT.
C
C     DLEAP(1,i) is the number of leap seconds at DLEAP(2,i) UTC
C     seconds past J2000.
C
      CALL GDPOOL ( 'DELTET/DELTA_T_A', 1, 1, N, DTA, FOUND(1) )
      CALL GDPOOL ( 'DELTET/K',         1, 1, N, K,   FOUND(2) )
      CALL GDPOOL ( 'DELTET/EB',        1, 1, N, EB,  FOUND(3) )
      CALL GDPOOL ( 'DELTET/M',         1, 2, N, M,   FOUND(4) )

C
C     Check that the number of leapseconds is not too great for our
C     buffer size (not likely).
C
      CALL DTPOOL ( 'DELTET/DELTA_AT', FOUND(5), NLEAP, DTYPE )

      IF ( NLEAP .GT. (2*NMAX) ) THEN
      
         CALL SETMSG ( 'Number of leapseconds, #, is greater than ' //
     .                 'the number that can be buffered, #.'         )
         CALL ERRINT ( '#',  NLEAP/2                                 )
         CALL ERRINT ( '#',  NMAX                                    )
         CALL SIGERR ( 'SPICE(BUFFERTOOSMALL)'                       )
         CALL CHKOUT ( 'DELTET'                                      )
         RETURN
         
      END IF
      
      CALL GDPOOL ( 'DELTET/DELTA_AT',  1,            2*NMAX,
     .               NLEAP,             DLEAP(1,1),   FOUND(5) )
 
      NLEAP = NLEAP / 2
 
      IF ( .NOT. (       FOUND(1)
     .             .AND. FOUND(2)
     .             .AND. FOUND(3)
     .             .AND. FOUND(4)
     .             .AND. FOUND(5) ) ) THEN
 
         CALL SETMSG ( 'The following, needed to compute '      //
     .                 'Delta ET (ET - UTC), could not be '     //
     .                 'found in the kernel pool: #'               )
 
         DO I = 1, 5
            IF ( .NOT. FOUND(I) ) THEN
               CALL ERRCH ( '#', MISSED(I) )
            END IF
         END DO
 
         CALL ERRCH  ( ', #', '.' )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)' )
 
         CALL CHKOUT ( 'DELTET' )
         RETURN
 
      END IF
 
 
 
C
C     There are two separate quantities to be determined. First,
C     the appropriate number of leap seconds. Second, the size of
C     the periodic term ET-TAI.
C
 
C
C     For epochs before the first leap second, return Delta ET at
C     the epoch of the leap second minus one second.
C
      LEAPS = DLEAP(1,1) - 1
 
C
C     When counting leap seconds for UTC epochs, we can compare
C     directly against the values in DLEAP.
C
      IF ( TYPE .EQ. 'UTC' ) THEN
 
         DO I = 1, NLEAP
            IF ( EPOCH .GE. DLEAP(2,I) ) THEN
               LEAPS = DLEAP(1,I)
            END IF
         END DO
 
C
C     For ET epochs, things are a little tougher. In order to compare
C     the input epoch against the epochs of the leap seconds, we need
C     to compute ET-TAI at each of the leap epochs. To make sure that
C     the computation is reversible, it is always done at the nearest
C     ET second (the "approximate ET", or AET).
C
C     There must be a hundred ways to do this more efficiently.
C     For now, we'll settle for one that works.
C
      ELSE IF ( TYPE .EQ. 'ET' ) THEN
 
         DO I = 1, NLEAP
            IF ( EPOCH .GT. DLEAP(2,I) ) THEN
 
               AET   = DNINT ( DLEAP(2,I) + DTA + DLEAP(1,I) )
 
               MA    = M(0) + M(1) * AET
               EA    = MA   + EB   * SIN ( MA )
               ETTAI =        K    * SIN ( EA )
 
               ET    = DLEAP(2,I) + DTA + DLEAP(1,I) + ETTAI
 
               IF ( EPOCH .GE. ET ) THEN
                  LEAPS = DLEAP(1,I)
               END IF
 
            END IF
         END DO
 
C
C     Uh, those are the only choices.
C
      ELSE
 
         CALL SETMSG ( 'Epoch type was #' )
         CALL ERRCH  ( '#', TYPE )
         CALL SIGERR ( 'SPICE(INVALIDEPOCH)' )
 
         CALL CHKOUT ( 'DELTET' )
         RETURN
 
      END IF
 
 
C
C     Add the constant offset, leap seconds, and the relativistic term
C     (as before, computed at the nearest ET second).
C
      IF ( TYPE .EQ. 'ET' ) THEN
         AET = DNINT ( EPOCH )
 
      ELSE IF ( TYPE .EQ. 'UTC' ) THEN
         AET = DNINT ( EPOCH + DTA + LEAPS )
      END IF
 
      MA    = M(0) + M(1) * AET
      EA    = MA   + EB   * SIN ( MA )
      ETTAI =        K    * SIN ( EA )
 
      DELTA = DTA + LEAPS + ETTAI
 
      CALL CHKOUT ( 'DELTET' )
      RETURN
      END
