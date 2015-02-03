C$Procedure      LSTMID ( Find ET corresponding to LST )
 
      SUBROUTINE LSTMID ( INPET, BODYID, LON, SCRATE, 
     .                    MNTYPE, SECOFF, MNET )
 
C$ Abstract
C
C     This routine finds ET corresponding the local solar time,
C     specified as an integer number of seconds past midnight of
C     current local solar day, that is nearest, previous, or next to a
C     given ET.
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
C     None.
C
C$ Declarations
 
      IMPLICIT NONE
 
      DOUBLE PRECISION      INPET
      INTEGER               BODYID
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      SCRATE
      CHARACTER*(*)         MNTYPE
      INTEGER               SECOFF
      DOUBLE PRECISION      MNET

 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INPET      I   Input ET.
C     BODYID     I   NAIF ID of the body.
C     LON        I   Planetocentric longitude on the body.
C     SCRATE     I   Number of ET seconds in one local second.
C     MNTYPE     I   Time to look for: nearest, previous or next.
C     SECOFF     I   Offset from previous midnight in local seconds.
C     MNET       O   Output ET corresponding to the local time.
C
C$ Detailed_Input
C
C     ET             Input ephemeris time.
C
C     BODYID         NAIF ID of the body of interest.
C
C     LON            Planetocentric longitude on the body of interest.
C
C     SCRATE         Local time rate, i.e. number of ET seconds 
C                    in one local second.
C
C     MNTYPE         Time to look for -- 'NEAREST',
C                    'PREVIOUS' or 'NEXT'.
C
C     SECOFF         Offset from previous midnight in local seconds.
C                    Must be between 0 and SPD().
C
C$ Detailed_Output
C
C     MNET           Ephemeris time of the nearest, previous or next
C                    given local time to a given input ET time.
C
C$ Parameters
C
C     MAXITR         Maximum number of iteration allowed in the loop.
C                    Currently set to 100. Usually there is no more 
C                    than 3-5 iterations are need to get to the 
C                    midnight time.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If number of iteration exceeds MAXITR, routine reports
C        SPICE(TOOMANYITERATIONS) error.
C
C     2) If value of MNTYPE is not one of the 'NEAREST', 'PREVIOUS' 
C        or 'NEXT', routine reports SPICE(BADMIDNIGHTTYPE) error.
C
C     3) If value of SECOFF is not between 0 and 86000, 
C        routine reports SPICE(BADTIMEOFFSET) error.
C
C$ Particulars
C
C     This routine is required to implement local solar days (SOLs) 
C     counting for the local solar time (LST). It's essential that 
C     caller provides correct SCRATE for a body. Also the routine 
C     assumes that there is enough SPICE kernel data loaded into 
C     the program to compute LST within +/- two local solar days 
C     of an input ET.
C
C$ Examples
C
C     Let our inputs be:
C
C        ETSTR  = '1997 AUG 12 12:00:12 TDB'
C        BODYID = 499
C        LON    = - 33.1D0
C        SCRATE = 1.0277116753731D0
C
C        CALL STR2ET( ETSTR, ET )
C        LON = LON * RPD()
C
C     then to compute nearest local midnight LSTMID must be called 
C     follows:
C
C        CALL LSTMID( ET, BODYID, LON, SCRATE, 'NEAREST', 0, MNET )
C     
C     to compute previous local noon:
C
C        CALL LSTMID( ET, BODYID, LON, SCRATE, 'PREVIOUS', 
C       .                                   INT( SPD()) / 2, MNET )
C     
C     to compute next local 6:00 a.m.:
C
C        CALL LSTMID( ET, BODYID, LON, SCRATE, 'NEXT', 
C       .                                   INT( SPD()) / 4, MNET )
C
C$ Restrictions
C
C     Sufficient SPICE kernel data loaded into the calling program to 
C     compute LST within +/- two local solar days of an input ET.
C
C$ Author_and_Institution
C
C     B.V.Semenov      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    CHRONOS Version 1.2.0, May 10, 2006 (BVS)
C
C        Bug fix: changed logic computing iteration loop convergence
C        parameter to ensure that the loop terminates successfully for
C        input local times at the end of the local day (23:59:xx).
C        Fixed/improved header sections and in-line comments in a few
C        places.
C
C-    CHRONOS Version 1.1.0, October 16, 2002 (BVS)
C
C        Bug fix: the search loop now forces termination is the
C        the delta value at MAXITR is 1 or -1. This has to be done
C        because of the integer output from ET2LST.
C
C-    CHRONOS Version 1.0.0, May 14, 1998 (BVS)
C
C
C-&

C
C     Local parameters.
C
      INTEGER               MAXITR
      PARAMETER           ( MAXITR = 100 )
      
C
C     Local variables.
C
      DOUBLE PRECISION      ET
               
      INTEGER               LSECS   
      INTEGER               HR
      INTEGER               MN
      INTEGER               SC
      INTEGER               I
      INTEGER               INTSPD
      INTEGER               DAYSEC
      
      CHARACTER*(24)        TIME
      CHARACTER*(6)         AMPM

C
C     SPICELIB functions.
C
      DOUBLE PRECISION      SPD
      LOGICAL               RETURN

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LSTMID' )
      END IF

      ET = INPET
      INTSPD = INT ( SPD() )
      
C
C     Check if input offset within the right bounds.
C
      IF ( SECOFF .LT. 0 .OR. SECOFF .GT. INTSPD ) THEN
         CALL SETMSG ( 'Input offset expressed as count of ' //
     .                 'local seconds must be between 0 and 86400.')
         CALL SIGERR ( 'SPICE(BADTIMEOFFSET)'                      )
      END IF

C
C     Set initial ET for our iterations depending on whether we need 
C     compute ET for nearest, previous, or next given LST.
C
      IF       ( MNTYPE .EQ. 'NEAREST' ) THEN

C
C        Leave initial ET unchanged and assign non-zero LSECS just to
C        start iterations loop.
C
         LSECS = 1

      ELSE IF  ( MNTYPE .EQ. 'PREVIOUS' ) THEN

C
C        Adjust current ET by the number of ET second between current
C        local time and previous desired local time.
C
         CALL ET2LST( ET, BODYID, LON, 'PLANETOCENTRIC', 
     .                HR, MN, SC, TIME, AMPM )
      
         LSECS = HR * 3600 + MN * 60 + SC - SECOFF
         IF ( LSECS .LT. 0 ) THEN
            LSECS = INTSPD + LSECS 
         END IF
         
         ET = ET - SCRATE * LSECS
         
      ELSE IF  ( MNTYPE .EQ. 'NEXT' ) THEN

C
C        Adjust current ET by the number of ET second between current
C        local time and following desired local time.
C
         CALL ET2LST( ET, BODYID, LON, 'PLANETOCENTRIC', 
     .                HR, MN, SC, TIME, AMPM ) 
     
         LSECS = INTSPD - ( HR * 3600 + MN * 60 + SC - SECOFF )
         IF ( LSECS .GE. INTSPD ) THEN
            LSECS = LSECS - INTSPD 
         END IF
         
         ET = ET + SCRATE * LSECS
         
      ELSE

C
C        Unrecognizable MNTYPE. Complain and exit.
C
         CALL SETMSG ( 'Cannot recognize specification of the kind '//
     .                 'of local midnight to compute ''#''. ' //
     .                 'Recognizable values are ''NEAREST'', ' //
     .                 '''PREVIOUS'' and ''NEXT''.'               )
         CALL ERRCH  ( '#', MNTYPE                                )
         CALL SIGERR ( 'SPICE(BADMIDNIGHTTYPE)'                   )
         
      END IF
         
C
C     We stop iterations when LSECS is exactly 0. We can do it because
C     ET2LST returns integer number of local hours, minutes and seconds
C     :). Before iterating we set counter to 0.
C
      I = 0 
               
      DO WHILE( LSECS .NE. 0 )
            
C
C        Get local time at current ET.
C
         CALL ET2LST( ET, BODYID, LON, 'PLANETOCENTRIC', 
     .                      HR, MN, SC, TIME, AMPM )

C
C        Compute the number of local seconds since last midnight.
C
         DAYSEC = HR * 3600 + MN * 60 + SC

C
C        Calculate the difference (in local seconds) between local
C        seconds since midnight given on the input and computed during
C        this iteration. Adjust it by the number of seconds in the day
C        if it happens to be greater than a half a day to make sure
C        that the loop does not "run away" into previous or next day.
C
         IF      ( DAYSEC - SECOFF .GT. INTSPD / 2 ) THEN
            LSECS = DAYSEC - INTSPD - SECOFF
         ELSE IF ( SECOFF - DAYSEC .GT. INTSPD / 2 ) THEN
            LSECS = DAYSEC + INTSPD - SECOFF
         ELSE
            LSECS = HR * 3600 + MN * 60 + SC - SECOFF
         END IF
         
C
C        Adjust current ET for the next iteration.
C              
         ET = ET - LSECS  * SCRATE

C
C        Increase iterations counter and bail out if it's over the
C        limit.
C
         I = I + 1
         
         IF ( I .GT. MAXITR ) THEN

C
C           Check if the last difference value is 1 or -1 local second.
C           If so, force the loop termination because it has probably
C           been this way for many iteration already. Otherwise
C           complain and exit.
C
            IF ( ( LSECS .EQ. 1 ) .OR. ( LSECS .EQ. -1 ) ) THEN
               LSECS = 0
            ELSE
               CALL SETMSG ( 'More than # iterations in the loop '   //
     .                       'determining local solar time midnight '//
     .                       'ET.'                                   )
               CALL ERRINT ( '#', MAXITR                             )
               CALL SIGERR ( 'SPICE(TOOMANYITERATIONS)'              )
            END IF

         END IF
               
      END DO
      
C
C     Assign output value and return.
C
      MNET = ET
            
      CALL CHKOUT ( 'LSTMID' )
      RETURN      
      END

