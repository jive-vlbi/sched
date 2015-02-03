C$Procedure      LTIME ( Light Time )
 
      SUBROUTINE LTIME ( ETOBS, OBS, DIR, TARG, ETTARG, ELAPSD )
 
C$ Abstract
C
C     This routine computes the transmit (or receive) time
C     of a signal at a specified target, given the receive
C     (or transmit) time at a specified observer. The elapsed
C     time between transmit and receive is also returned.
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
C      None.
C
C$ Keywords
C
C       SPK
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      ETOBS
      INTEGER               OBS
      CHARACTER*(2)         DIR
      INTEGER               TARG
      DOUBLE PRECISION      ETTARG
      DOUBLE PRECISION      ELAPSD
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ETOBS      I   Epoch of a signal at some observer
C      OBS        I   NAIF-id of some observer
C      DIR        I   Direction the signal travels ( '->' or '<-' )
C      TARG       I   NAIF-id of the target object
C      ETTARG     O   Epoch of the signal at the target
C      ELAPSD     O   Time between transmit and receipt of the signal
C
C$ Detailed_Input
C
C     ETOBS       is an epoch expressed in ephemeris second (TDB)
C                 past the epoch of the J2000 reference system.
C                 This is the time at which an electromagnetic
C                 signal is "at" the observer.
C
C     OBS         is the NAIF-id of some observer.
C
C     DIR         is the direction the signal travels.  The
C                 acceptable values are '->' and '<-'.  When
C                 you read the calling sequence from left to
C                 right, the "arrow" given by DIR indicates
C                 which way the electromagnetic signal is travelling.
C
C                 If the argument list reads as below,
C
C                  ..., OBS, '->', TARG, ...
C
C                 the signal is travelling from the observer to the
C                 target.
C
C                 If the argument reads as
C
C                  ..., OBS, '<-', TARG
C
C                 the signal is travelling from the target to
C                 the observer.
C
C     TARG        is the NAIF-id of the target.
C
C$ Detailed_Output
C
C     ETTARG      is the epoch expressed in ephemeris seconds (TDB)
C                 past the epoch of the J2000 reference system
C                 at which the electromagnetic signal is "at" the
C                 target body.
C
C                 Note ETTARG is computed using only Newtonian
C                 assumptions about the propagation of light.
C
C     ELAPSD      is the number of ephemeris seconds (TDB) between
C                 transmission and receipt of the signal.
C
C                 ELAPSD = DABS( ETOBS - ETTARG )
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     1) If DIR is not one of '->' or '<-' the error
C       'SPICE(BADDIRECTION)' will be signalled. In this case
C        ETTARG and ELAPSD will not be altered from their
C        input values.
C
C     2) If insufficient ephemeris information is available to
C        compute the outputs ETTARG and ELAPSD, or if observer
C        or target is not recognized, the problems is diagnosed
C        by a routine in the call tree of this routine.
C
C        In this case, the value of ETTARG will be set to ETOBS
C        and ELAPSD will be set to zero.
C
C$ Particulars
C
C
C     Suppose a radio signal travels between two solar system
C     objects. Given an ephemeris for the two objects, which way
C     the signal is travelling, and the time when the signal is
C     "at" at one of the objects (the observer OBS), this routine
C     determines when the signal is "at" the other object (the
C     target TARG).   It also returns the elapsed time between
C     transmission and receipt of the signal.
C
C
C$ Examples
C
C     Example 1.
C     ----------
C     Suppose a signal is transmitted at time ET from the Goldstone
C     tracking site (id-code 399001) to a spacecraft whose id-code
C     is -77.
C
C
C           signal travelling to spacecraft
C       *  -._.-._.-._.-._.-._.-._.-._.-._.->  *
C
C       Goldstone (OBS=399001)            Spacecraft (TARG = -77)
C       at epoch ETOBS(given)             at epoch ETTARG(unknown)
C
C     Assuming that all of the required SPICE kernels have been
C     loaded, the code fragment below shows how to compute the
C     time (ARRIVE) at which the signal arrives at the spacecraft
C     and how long (HOWLNG) it took the signal to reach the spacecraft.
C     (Note that we display the arrival time as the number of seconds
C     past J2000.)
C
C        OBS   = 399001
C        TARG  = -77
C        ETOBS = ET
C
C        CALL LTIME ( ETOBS, OBS, '->', TARG, ARRIVE, HOWLNG )
C        CALL ETCAL
C
C        WRITE (*,*) 'The signal arrived at time: ', ARRIVE
C        WRITE (*,*) 'It took ', HOWLNG, ' seconds to get there.'
C
C
C     Example 2.
C     ----------
C     Suppose a signal is received at the Goldstone tracking sight
C     at epoch ET from the spacecraft of the previous example.
C
C               signal sent from spacecraft
C         *  <-._.-._.-._.-._.-._.-._.-._.-._.- *
C
C       Goldstone (OBS=399001)               Spacecraft (TARG = -77)
C       at epoch ETOBS(given)                at epoch ETTARG(unknown)
C
C     Again assuming that all the required kernels have been loaded
C     the code fragment below computes the epoch at which the
C     signal was transmitted from the spacecraft.
C
C        OBS   = 399001
C        TARG  = -77
C        ETOBS = ET
C
C        CALL LTIME ( ETOBS, OBS, '<-', TARG, SENT, HOWLNG )
C        CALL ETCAL
C
C        WRITE (*,*) 'The signal was transmitted at: ', SENT
C        WRITE (*,*) 'It took ', HOWLNG, ' seconds to get here.'
C
C     EXAMPLE 3
C     ---------
C     Suppose there is a transponder on board the spacecraft of
C     the previous examples that transmits a signal back to the
C     sender exactly 1 microsecond after a signal arrives at
C     the spacecraft.  If we send a signal from Goldstone
C     to the spacecraft and wait to receive it at Canberra.
C     What will be the epoch at which the return signal arrives
C     in Canberra? ( The id-code for Canberra is 399002 ).
C
C     Again, assuming we've loaded all the necessary kernels,
C     the fragment below will give us the answer.
C
C        GSTONE = 399001
C        SC     = -77
C        CANBER = 399002
C        ETGOLD = ET
C
C        CALL LTIME ( ETGOLD, GSTONE, '->', SC, SCGET, LT1 )
C
C     Account for the microsecond delay between receipt and transmit
C
C        SCSEND = SCGET + 0.000001
C
C        CALL LTIME ( SCSEND, SC, '->', CANBER, ETCANB, LT2 )
C
C        RNDTRP = ETCANB - ETGOLD
C
C        WRITE (*,*) 'The  signal arrives in Canberra at: ', ETCANB
C        WRITE (*,*) 'Round trip time for the signal was: ', RNDTRP
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.1.2, 22-SEP-2004 (EDW)
C
C        Placed Copyright after Abstract.
C
C-    SPICELIB Version 1.1.1, 18-NOV-1996 (WLT)
C
C        Errors in the examples section were corrected.
C
C-    SPICELIB Version 1.1.0, 10-JUL-1996 (WLT)
C
C        Added Copyright Notice to the header.
C
C-    SPICELIB Version 1.0.0, 10-NOV-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Compute uplink and downlink light time
C
C-&

C
C     SPICELIB Functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VDIST
      INTEGER               RTRIM
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local Variables
C
      DOUBLE PRECISION      C
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MYET
      DOUBLE PRECISION      SOBS  ( 6 )
      DOUBLE PRECISION      STARG ( 6 )
 
      INTEGER               BCENTR
      INTEGER               R
 
      IF ( RETURN() )THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'LTIME' )
C
C     First perform the obvious error check.
C
      IF ( DIR .NE. '->' .AND. DIR .NE. '<-' ) THEN
 
         CALL SETMSG ( 'The direction specifier for the signal '
     .   //            'was ''#''  it must be either ''->'' or '
     .   //            '''<-''. '            )
         R =  RTRIM  (      DIR              )
         CALL ERRCH  ( '#', DIR(1:R)         )
         CALL SIGERR ( 'SPICE(BADDIRECTION)' )
         CALL CHKOUT ( 'LTIME'               )
         RETURN
 
      END IF
 
C
C     We need two constants, the speed of light and the id-code
C     for the solar system barycenter.
C
      C      = CLIGHT ()
      BCENTR = 0
      MYET   = ETOBS
 
C
C     First get the barycenter relative states of the observer
C     and target.
C
      CALL SPKGEO ( OBS,  MYET, 'J2000', BCENTR, SOBS,  LT )
      CALL SPKGEO ( TARG, MYET, 'J2000', BCENTR, STARG, LT )
 
      ELAPSD = VDIST( SOBS, STARG ) / C
 
C
C     The rest is straight forward.  We either add the elapsed
C     time to get the next state or subtract the elapsed time.
C     This depends on whether we are receiving or transmitting
C     at the observer.
C
C     Note that 3 iterations as performed here gives us
C     Newtonian accuracy to the nanosecond level for all
C     known objects in the solar system.  The ephemeris
C     is certain to be much worse than this.
C
      IF ( DIR .EQ. '->' ) THEN
 
         ETTARG = MYET + ELAPSD
 
         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )
 
         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET + ELAPSD
 
         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )
 
         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET + ELAPSD
 
         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )
 
         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET + ELAPSD
 
      ELSE
 
         ETTARG = MYET - ELAPSD
 
         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )
 
         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET - ELAPSD
 
         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )
 
         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET - ELAPSD
 
         CALL SPKGEO ( TARG, ETTARG, 'J2000', BCENTR, STARG, LT )
 
         ELAPSD = VDIST( SOBS, STARG ) / C
         ETTARG = MYET - ELAPSD
 
      END IF
 
      IF ( FAILED() ) THEN
         ETTARG = MYET
         ELAPSD = 0.0D0
      END IF
 
      CALL CHKOUT ( 'LTIME' )
      RETURN
 
      END
