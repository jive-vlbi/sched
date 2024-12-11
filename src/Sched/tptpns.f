      SUBROUTINE TPTPNS
C
C     Subroutine for SCHED called by SCHIN that transfers the tape
C     initialization information to station dependent cells in the
C     arrays in sched.inc.  The information was temporarily stored
C     in the RTP arrays in tpinit.inc by TPINI.  This two step
C     process was used because the station list was not necessarily
C     known when the tape initialization information was read.
C
C     Also resolve which NDRIVES input to use.
C
C     Modified for post-tape era where it becomes a routine for
C     setting some defaults.  July 22, 2010.  RCW.
C
      INCLUDE 'sched.inc'
C
      INTEGER       ISTA
      LOGICAL       ISLBA
C --------------------------------------------------------------
C     Now fill the arrays that used to be influenced by TAPEINI.
C     Commented ones no longer used.
C     Some of these that are defaulted here should be eliminated 
C     eventually but they are tangled in the VEX stuff in 
C     interesting ways.
C
      DO ISTA = 1, NSTA
C         TPTIME(ISTA)   = -1.0D0
C         TPSDRIV(ISTA)  = 1
C         TPSINDX(ISTA)  = 1
C         TPLENG(ISTA)   = 17600.
         NDRIVE(ISTA)   = 1
         NHDPOS(ISTA)   = 14
         DENSITY(ISTA)  = 'H'
         HEADMODE(ISTA) = ' '
         MEDIA(ISTA)    = ' '
      END DO
C
C     Deal with MEDIA defaults etc.
C
      ANYTAPE = .FALSE.
      ALLDISK = .TRUE.
C
      DO ISTA = 1, NSTA
C
C        Determine whether to use tape.  This is the default if
C        tape is available in the station catalog and MEDIADEF was
C        not set to DISK
C
C        Retain a proper deterimination of USETAPE to trigger a
C        complaint if it turns out to be positive.
C
         USETAPE(ISTA) = VLBITP .AND. 
     1        RECORDER(STANUM(ISTA)) .NE. 'NONE' .AND.
     2        ( INDEX( MEDIA(ISTA), 'TAPE' ) .NE. 0 .OR.
     3          DISK(STANUM(ISTA)) .EQ. 'NONE' .OR.
     4          (  MEDIA(ISTA) .EQ. ' ' .AND. 
     5           MEDIADEF(STANUM(ISTA)) .EQ. 'TAPE' ) )
         IF( USETAPE(ISTA) ) THEN
            ANYTAPE = .TRUE.
            CALL ERRLOG( 'SCHED thinks you asked for tape at '//
     1      STATION(STANUM(ISTA))//'but tape is no longer supported.' )
         END IF     
C
C        Determine whether to use disk.
C
         USEDISK(ISTA) = VLBITP .AND. 
     1        DISK(STANUM(ISTA)) .NE. 'NONE' .AND.
     2        ( INDEX( MEDIA(ISTA), 'DISK' ) .NE. 0 .OR.
     3          RECORDER(STANUM(ISTA)) .EQ. 'NONE' .OR.
     4          (  MEDIA(ISTA) .EQ. ' ' .AND. 
     5          ( MEDIADEF(STANUM(ISTA)) .EQ. 'DISK' .OR. 
     6            MEDIADEF(STANUM(ISTA)) .EQ. 'NONE' ) ) )
C
C        LBA recorders use disks but have different constraints to Mk5
C        This change added by Cormac.  I'm (Craig) not sure I like it
C        as I was planning to use ALLDISK to say not to worry about 
C        tape.  But in practice, as of Version 9.3, ALLDISK is only
C        used in SETTPS for long recording scan warnings (not wanted
C        for the LBA) and I test against MARK5A there.  So I'll
C        leave it for now.  Reconsider if used for something else.
C
         ISLBA = .FALSE.
         IF (DISK(STANUM(ISTA))(1:5) .EQ. 'LBADR') ISLBA = .TRUE.
         ALLDISK = ALLDISK .AND. USEDISK(ISTA) .AND. .NOT. ISLBA
C
C        If VLBITP, insist that we use one for recording 
C        observations.
C
         IF( VLBITP .AND. .NOT. USEDISK(ISTA) ) THEN
            WRITE( MSGTXT, '( 2A )' ) 
     1         'TPTPNS:  **** You have specified OBSTYPE = VLBI ',
     2         'or equivalent.'
            CALL WLOG( 1, MSGTXT )
            WRITE( MSGTXT, '( 4A )' ) '              But you '// 
     1          'are not using a recording system.'
            CALL WLOG( 1, MSGTXT )
            CALL ERRLOG( 'TPTPNS:  Specify the recording system.' )
C
         END IF
C
      END DO
C
      RETURN
      END
