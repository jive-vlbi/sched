      SUBROUTINE FMTPICK( NEEDFMT )
C
C     I don't think this routine will ever be called in the era of
C     disk.  I'm leaving it around for now just in case.
C
C     Routine for SCHED, called by SETFORM, that deals with picking
C     formats for VLBA/MKIV stations when not all are forced by 
C     user input or constraints.  We would like to pick the set that
C     gives a processing speedup factor of 2, if we can.
C     Since setting even one format will usually force the rest, 
C     this has to be done in a manner coordinated across all setups.
C     The correlation is the result of the requirement that all
C     track bit rates be the same and the desire that the number of
C     tracks in use at a station stay constant.
C
C     Note that if there were any high bandwidth modes (TWOHEAD, 
C     FASTTRK), the formats would already have been forced and we
C     would not be in this routine.
C
C     At least for now, we won't try to adjust the formats to allow
C     the chosen correlator integration times.  Maybe run a check 
C     on that later.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER           KS, ISETF, ISCN, MINNTRK
      REAL              MAXFBPS(MAXSET)
      REAL              TTBPS(MAXSET), BTBPS(MAXSET)
      REAL              MAXBPS, USEFBPS
      REAL              ACCBPS, ACCTIM, AVGBPS
      REAL              USENTRK, SETBPS(MAXSET)
      LOGICAL           NEEDFMT(*), OK
      DOUBLE PRECISION  TOTIME, SETTIME(MAXSET)
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'FMTPICK starting' )
C
C     Ask for feedback if the routine is called.
C
      CALL WLOG( 1, 'FMTPICK:  Please inform Craig Walker at '//
     1  'cwalker@nrao.edu if you see this message.' )
      CALL WLOG( 1, '          It means you reached a part of SCHED '//
     1  'not thought to still be active.' )
      CALL WLOG( 1, '          Please include your input file.' )
C
C     Get the total observing time that each setup file (not group) is 
C     used.
C
      TOTIME  = 0.0D0
      DO ISETF = 1, NSETF
         SETTIME(ISETF) = 0.D0
      END DO
      DO ISCN = 1, NSCANS
         IF( .NOT. NOREC(ISCN) ) THEN
            TOTIME = TOTIME + DUR(ISCN)
            ISETF = SETNUM(ISCN)
            SETTIME(ISETF) = SETTIME(ISETF) + DUR(ISCN)
         END IF
      END DO
C
C     Get the maximum total nominal bit rate both overall (MAXBPS) and for 
C     each setup file (MAXFBPS).  Only use unset formats (this keeps
C     any isolated, forced formats from affecting things).
C     Get the maximum and minimum track bits per second for each setup 
C     file (BTBPS and TTBPS).  Use the nominal bit rate TOTBPS as the
C     actual bit rate WRTBPS is affected by format specific overheads.
C
      MAXBPS = 0.0
      MINNTRK = 0.0
      DO ISETF = 1, NSETF
         MAXFBPS(ISETF) = 0.0
         TTBPS(ISETF) = 99999.
         BTBPS(ISETF) = 0.0
      END DO
      DO KS = 1, NSET
         IF( VLBAMKIV(KS) .AND. RECUSED(KS) .AND. NEEDFMT(KS) ) THEN
            ISETF = ISETNUM(KS)
            MAXBPS = MAX( MAXBPS, TOTBPS(KS) )
            MINNTRK = MAX( MINNTRK, MINTRAK(KS) )
            MAXFBPS(ISETF) = MAX( MAXFBPS(ISETF), TOTBPS(KS) )
            TTBPS(ISETF) = MIN( TTBPS(ISETF), MAXTBPS(KS) )
            BTBPS(ISETF) = MAX( BTBPS(ISETF), MINTBPS(KS) )
         END IF
      END DO
C
C     Check a limit
C
      IF( MAXBPS .GT. 256.0 ) THEN
         CALL WLOG( 1, 'FMTPICK: There is a bit rate above 256' //
     1      ' that has not yet been set.' )
         CALL WLOG( 1, '         Probable programming error ' )
         CALL WLOG( 1, '         Workaround - set TPMODE or full '//
     1        'FORMAT for more VLBA/MKIV formats.' )        
         CALL ERRLOG( 'Please report' )
      END IF
C
C     Get the average total bit rate based on the maximum for 
C     each setup file.
C
      ACCBPS = 0.0
      ACCTIM = 0.0      
      DO ISETF = 1, NSETF
         IF( MAXFBPS(ISETF) .GT. 0.0 ) THEN
            ACCBPS = ACCBPS + MAXFBPS(ISETF) * SETTIME(ISETF)
            ACCTIM = ACCTIM + SETTIME(ISETF)
         END IF
      END DO
      IF( ACCTIM .GT. 0.0 ) THEN
         AVGBPS = ACCBPS / ACCTIM
      ELSE 
C
C        It seems that there are no setups used for VLBAMKIV.
C        Let SETFORM deal with the problem.
C
         GO TO 999
      END IF
C
C     Get the number of tracks to use.  USENTRK tries to get speedup
C     2.  MAXBPS insures that there are enough tracks for the fastest
C     setups.  MINNTRK insures that there are enough tracks for 
C     the slowest bit rate scans.
C
      USENTRK = NINT( AVGBPS / 4.0 )
      IF( USENTRK .GT. 24 .OR. MAXBPS .GT. 128.0 .OR. 
     1    MINNTRK .GT. 16 ) THEN
         USENTRK = 32
      ELSE IF( USENTRK .GT. 12 .OR. MAXBPS .GT. 64.0 .OR. 
     1    MINNTRK .GT. 8 ) THEN
         USENTRK = 16
      ELSE
         USENTRK = 8
      END IF
C
C     Ok, now we have a number of tracks to use.  Treating it as
C     the same for all stations at this point.  Find the track bit
C     rate to use for each setup file.  Using USEFBPS here in case
C     some setup had a weird number of channels.
C
      DO ISETF = 1, NSETF
         IF( MAXFBPS(ISETF) .NE. 0.0 ) THEN
            IF( MAXFBPS(ISETF) .GT. 128.0 ) THEN
               USEFBPS = 256.0 
            ELSE IF( MAXFBPS(ISETF) .GT. 64.0 ) THEN
               USEFBPS = 128.0 
            ELSE IF( MAXFBPS(ISETF) .GT. 32.0 ) THEN
               USEFBPS = 64.0 
            ELSE IF( MAXFBPS(ISETF) .GT. 16.0 ) THEN
               USEFBPS = 32.0 
            ELSE
               USEFBPS = 16.0 
            END IF
C
C           Get the bit rate for this setup file.
C
            SETBPS(ISETF) = MAXFBPS(ISETF) / USENTRK
C
C           React to it being out of bounds.  If it is too low, we
C           can boost it and waste tracks.  If it is too high, we
C           made an error and need too many tracks.  Something was
C           wrong with MINNTRK.
C
            IF( SETBPS(ISETF) .GT. TTBPS(ISETF) ) THEN
               CALL WLOG( 1, 'FMTPICK: Derived too high a track' //
     1            ' bit rate' )
               CALL WLOG( 1, '         Setup file: ' // SETFILE(ISETF) )
               CALL WLOG( 1, '         Probable programming error ' )
               CALL WLOG( 1, '         Workaround - set TPMODE or full '
     1              // 'FORMAT for more VLBA/MKIV formats.' )        
               CALL ERRLOG( 'Please report' )
C
            ELSE IF( SETBPS(ISETF) .LT. BTBPS(ISETF) ) THEN
               SETBPS(ISETF) = BTBPS(ISETF)
            END IF
         END IF
      END DO
C
C     Ok now I think I can set all the formats.
C
      DO KS = 1, NSET
         IF( VLBAMKIV(KS) .AND. RECUSED(KS) .AND. NEEDFMT(KS) ) THEN
            ISETF = ISETNUM(KS)
            CALL SETFMT( KS, SETBPS(ISETF), OK )
            IF( OK ) NEEDFMT(KS) = .FALSE.
         END IF
      END DO
C
  999 CONTINUE
      RETURN
      END
