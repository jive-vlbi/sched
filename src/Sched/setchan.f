      SUBROUTINE SETCHAN( KS )
C
C     Set a variety of channel data that might need to be defaulted
C     and that can be set before the frequency setup is finally 
C     settled.  These are meant to be items that are independent
C     of the station hardware, other than the fact that the number
C     of BBCs needs to be known.
C
C     This routine should come before SETFREQ to allow setting of
C     sidebands etc.  Because of this, the defaulting of sidebands
C     will not be fully general in cases where freqref is not 
C     available before SETFREQ.  It will not opt for upper/lower
C     pairs in that case.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER           ICH, ISETF, LEN1, KS
      LOGICAL           NEEDSB, DUALIF, GOTPOL, GOTNSD, GOTFQ
      DOUBLE PRECISION  MAXBW 
      REAL              BITRATE
C --------------------------------------------------------------------
      IF( KS .LE. 3 .AND. SDEBUG ) CALL WLOG( 0, 'SETCHAN: Starting' )
C
      ISETF = ISETNUM(KS)
C
C     Deal with polarization.
C
C     If it is still unset after this, it might be determined in 
C     SETFCAT based on knowing which IF is which polarization.
C
C     If no POL or IF was specified, default to dual.  Test on first
C     channel.  Then panic if some other channels were set.  Set
C     POL(1,KS) after tests to avoid confusing output from ERRSET.
C
      IF( POL(1,KS) .EQ. ' ' .AND. IFCHAN(1,KS) .EQ. ' ' ) THEN
         IF( NCHAN(KS) .GE. 2 ) THEN
            DO ICH = 2, NCHAN(KS)
               IF( POL(ICH,KS) .NE. ' ' ) THEN
                  CALL WLOG( 1, 'POL specified for some channel '//
     1               'but not for first.' )
                  CALL ERRSET( KS )
               END IF
               IF( IFCHAN(ICH,KS) .NE. ' ' ) THEN
                  CALL WLOG( 1, 'IFCHAN specified for some channel '//
     1               'but not for first.' )
                  CALL ERRSET( KS )
               END IF
            END DO
         END IF
         POL(1,KS) = 'DUAL'
      END IF
C
C     First deal with a specification of DUAL polarization.
C
      IF( POL(1,KS) .EQ. 'DUAL' ) THEN
         DO ICH = 1, NCHAN(KS), 2
            POL(ICH,KS) = 'RCP'
            POL(ICH+1,KS) = 'LCP'
         END DO
      END IF
C
C     If RCHAN and LCHAN were used, perhaps we can set POL.
C     Also detect if dual polarization was used.  Some related
C     defaulting was done when the setup file was read by RDSET.
C
      DUALPOL(KS) = .FALSE.
      GOTPOL = .TRUE.
      DO ICH = 1, NCHAN(KS)
         IF( POL(ICH,KS)(1:3) .NE. 'RCP' .AND.
     1       POL(ICH,KS)(1:3) .NE. 'LCP' .AND.
     2       POL(ICH,KS)(1:1) .NE. 'X' .AND.
     3       POL(ICH,KS)(1:1) .NE. 'Y' ) THEN
            IF( IFCHAN(ICH,KS) .EQ. RCHAN(KS) ) THEN
               POL(ICH,KS) = 'RCP'
            ELSE IF( IFCHAN(ICH,KS) .EQ. LCHAN(KS) ) THEN
               POL(ICH,KS) = 'LCP'
            ELSE
               GOTPOL = .FALSE.
            END IF
         END IF
         IF( POL(ICH,KS) .NE. POL(1,KS) ) DUALPOL(KS) = .TRUE.
      END DO
C
C     The only allowed situation where the polarization is not yet
C     known is if the IF's have been been specified, but we don't
C     yet know what polarization they are.  Below in setting 
C     sidebands, we should try to recognize this situation.  So
C     test for more than one specified IF.
C
      IF( GOTPOL ) THEN
         DUALIF = DUALPOL(KS)
      ELSE
         DUALIF = .FALSE.
         DO ICH = 1, NCHAN(KS)
            IF( IFCHAN(ICH,KS) .NE. ' ' .AND. IFCHAN(1,KS) .NE. ' '
     1          .AND. IFCHAN(ICH,KS) .NE. IFCHAN(1,KS) ) THEN
               DUALIF = .TRUE.
            END IF
         END DO
      END IF
C
C     Set the sample rate or bandwidths if one is not given.  Assume 
C     Nyquist rate.  Strictly, the sample rate is not needed for
C     some formats, but it won't hurt.  The bandwidth is needed for
C     all but VLAONLY modes and SETDEFS (which calls this routine)
C     has already filtered against that.
C
      MAXBW = 0.0
      DO ICH = 1, NCHAN(KS)
         MAXBW = MAX( MAXBW, BBFILT(ICH,KS) )
      END DO
      IF( SAMPRATE(KS) .EQ. 0.0 .AND. MAXBW .GT. 0.0 ) THEN
         SAMPRATE(KS) = 2.0 * MAXBW
      END IF
      IF( SAMPRATE(KS) .GT. 0.0 .AND. MAXBW .EQ. 0.0 ) THEN
         DO ICH = 1, NCHAN(KS)
            BBFILT(ICH,KS) = SAMPRATE(KS) / 2.0
         END DO
      END IF
      IF( SAMPRATE(KS) .EQ. 0.0 .AND. MAXBW .EQ. 0.0 ) THEN
         CALL WLOG( 1, '         Setup: ' // 
     1         SETNAME(KS)(1:LEN1(SETNAME(KS))) )
         CALL WLOG( 1, '         Station: '// SETSTA(1,KS) )
         CALL ERRLOG( 'SETCHAN: SAMPRATE or BBFILTER needed.' )         
      END IF
C
C     Get the total bandwidth and bitrate.  Useful later.
C     This is the nominal bit rate.  An adjustment for headers is
C     needed for the actual write rate.  BITRATE is the same
C     as TOTBPS(KS) will be after SETFORM, but that has not been
C     called yet.
C
      TOTBW(KS) = 0.0D0
      DO ICH = 1, NCHAN(KS)
         TOTBW(KS) = TOTBW(KS) + BBFILT(ICH,KS)
         BITRATE = NCHAN(KS) * SAMPRATE(KS) * BITS(1,KS)
      END DO
C
C     Deal with sidebands.
C
C     If neither NETSIDE nor SIDEBD have been set, we have freedom
C     to set NETSIDE as we please.  Choose upper unless there are
C     too few BBC's, or the RDBE/PFB is in use, in which case lower 
C     is required for SIDEDB which may force NETSIDE to be lower.  
C     Note that the VLA also has a forced sideband, but it is net upper
C     so we can use the regular default here.  
C
C     If we must use upper and lower, then we need to be sure
C     that the frequencies specified so far don't preclude the option.
C     Do this as a check, because if we can't use upper and lower,
C     then we can't do the setup.  Also there is a potential problem 
C     with BBC assignments when using both sidebands.  Let the BBC 
C     setting stuff later worry about that.
C
      NEEDSB = NETSIDE(1,KS) .NE. 'U' .AND. NETSIDE(1,KS) .NE. 'L' .AND.
     2         SIDEBD(1,KS) .NE. 'U' .AND. SIDEBD(1,KS) .NE. 'L'
C
C     Abort if there are too few BBCs period.  This is probably
C     redundant with other checks, but oh well.  With the new systems,
C     we no longer have dual sidebands per BBC, so the check is a bit
C     more complicated.
C
      IF( NEEDSB .AND. ( MSCHN(ISETF) .GT. 2 * MINBBC(ISETF) .OR. 
     1    ( MSCHN(ISETF) .GT. MINBBC(ISETF) .AND. 
     2     ( DAR(ISETSTA(KS))(1:4) .EQ. 'RDBE' .OR.
     3       DAR(ISETSTA(KS)) .EQ. 'WIDAR' ) ) ) ) THEN
         WRITE( SETMSG, '( A, I3 )' )
     1        'SETCHAN: Too many channels for maximum BBCs '//
     2        'available at some site: ', MINBBC(ISETF)
         CALL ERRSET(KS)
      END IF
C
C     The RDBE PFB personality presents a special problem.  The baseband
C     channels must be lower sideband (SIDEBD) in all circumstances.
C     But we don't know the IF sideband yet unless NETSIDE or (FIRSTLO
C     and FREQREF) were given.  Unfortunately, SCHED has not yet dealt
C     with the frequencies.  Plus SETFREQ, which has not yet been
C     called, will invoke sideband inversion when necessary.  It is
C     all too complicated to deal with here.  Just require that NETSIDE
C     or FIRSTLO plus FREQREF be provided.  It is common to use 
C     FIRSTLO and BBSYN, but for this NETSIDE should be given.
C     The DBBC_PFB may be the same.  Treat it as such for now.
C
C     The PFB is not expected to be in regular use for long, so I don't 
C     want to jump through too many hoops for it.
C
      IF( DBE(KS) .EQ. 'RDBE_PFB' .OR. DBE(KS) .EQ. 'DBBC_PFB' ) THEN
         DO ICH = 1, NCHAN(KS)
            GOTNSD = NETSIDE(ICH,KS) .NE. 'L' .OR. 
     1               NETSIDE(ICH,KS) .NE. 'U' 
            GOTFQ  = FIRSTLO(ICH,KS) .NE. 0.D0 .AND.
     1               FREQREF(ICH,KS) .EQ. 0.D0 
            IF( .NOT. GOTNSD .AND. .NOT. GOTFQ ) THEN
               CALL WLOG( 1, 'SETCHAN:  For the '// DBE(KS) //
     1            'personality, SCHED needs NETSIDE or FIRSTLO '//
     2            'and FREQREF' )
               CALL WLOG( 1, '          specified in the setup file.' )
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( 4A )' )
     1             '          Neither is given for ', 
     2             STATION(ISETSTA(KS)), ' in setup file: ',
     3             SETFILE(ISETF)(1:LEN1(SETFILE(ISETF)))
               CALL WLOG( 1, MSGTXT )
               CALL ERRLOG( 
     1              'Please provide NETSIDE or FIRSTLO+FREQREF' )
            END IF
C
C           Now we can set NETSIDE assuming FREQREF and FIRSTLO 
C           are availble.
C
            IF( .NOT. GOTNSD .AND. GOTFQ ) THEN
               IF( FIRSTLO(ICH,KS) .GT. FREQREF(ICH,KS) ) THEN
                  NETSIDE(ICH,KS) = 'U'
               ELSE
                  NETSIDE(ICH,KS) = 'L'
               END IF
            END IF
         END DO
         NEEDSB = .FALSE.
      END IF
C
C     Deal with defaulting when we need to use both the upper and 
C     lower sidebands from each BBC.  Only do when there are too few 
C     BBC's to do otherwise.  Note that we would not have gotten past
C     an earlier test if this were one of the systems that cannot
C     handle dual sidebands per BBC.
C
C     There are pathalogical cases where this will not necessarily
C     come out right.  Hope that they will be caught in checking 
C     and users will then specify the setup more completely.
C
      IF( NEEDSB .AND. MSCHN(ISETF) .GT. MINBBC(ISETF) ) THEN
         SETMSG = ' '
         WRITE( SETMSG, '( A, I3 )' )
     1        'SETCHAN: Must use dual sidebands because of the ' //
     2        'maximum BBCs available at some site: ', MINBBC(ISETF)
         CALL WLOG( 0, SETMSG )
         IF( DUALPOL(KS) .OR. DUALIF ) THEN
            DO ICH = 1, NCHAN(KS), 4
               NETSIDE(ICH,KS)   = 'L'
               NETSIDE(ICH+1,KS) = 'L'
               NETSIDE(ICH+2,KS) = 'U'
               NETSIDE(ICH+3,KS) = 'U'
C
C              Check compatible frequencies.  Don't worry if all are
C              zero - test will be ok anyway.
C
               IF( FREQREF(ICH+1,KS) .NE. FREQREF(ICH,KS) .OR. 
     1             FREQREF(ICH+2,KS) .NE. FREQREF(ICH,KS) .OR. 
     2             FREQREF(ICH+3,KS) .NE. FREQREF(ICH,KS) ) THEN
                  CALL WLOG( 1, 'SETCHAN: Frequencies not compatible '//
     1                  'with defaulted sidebands for ')
                  CALL WLOG( 1, '         dual polarization, dual '//
     1                     ' sideband observations' )
                  CALL ERRSET(KS)
               END IF
C
C              Check appropriate polarization specification.
C
               IF( POL(ICH+1,KS) .EQ. POL(ICH,KS) .OR. 
     1             POL(ICH+2,KS) .EQ. POL(ICH+3,KS) ) THEN
                  CALL WLOG( 1, 'SETCHAN: Polarizations not appropriate'
     1                  //' for defaulted sidebands for ')
                  CALL WLOG( 1, '         dual polarization, dual '//
     1                     ' sideband observations' )
                  CALL ERRSET(KS)
               END IF
            END DO
         ELSE
            DO ICH = 1, NCHAN(KS), 2
               NETSIDE(ICH,KS)   = 'L'
               NETSIDE(ICH+1,KS) = 'U'
C
C              Check compatible frequencies.  Don't worry if both zero.
C
               IF( FREQREF(ICH+1,KS) .NE. FREQREF(ICH,KS) ) THEN
                  CALL WLOG( 1, 'SETCHAN: Frequencies not compatible '//
     1               'with dual sideband observations.')
                  CALL ERRSET(KS)
               END IF
            END DO
C
         END IF
C
C     Now the easy case (one channel per BBC).  Don't worry if both
C     are zero.
C
      ELSE IF( NEEDSB ) THEN
         DO ICH = 1, NCHAN(KS)
            NETSIDE(ICH,KS) = 'U'
         END DO
         IF( DUALPOL(KS) ) THEN
            DO ICH = 1, NCHAN(KS), 2
               IF( FREQREF(ICH+1,KS) .NE. FREQREF(ICH,KS) ) THEN
                  CALL WLOG( 1, 'SETCHAN: Frequencies not compatible '//
     1               'with dual polarization observations.')
                  CALL ERRSET(KS)
               END IF
            END DO
         END IF
      END IF
C
      RETURN
      END
