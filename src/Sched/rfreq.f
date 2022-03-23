      SUBROUTINE RFREQ( IULINE )
C
C     Routine for SCHED that reads specifications of rest frequencies as
C     a function of channel.  Each group of rest frequencies will have
C     a name and will be invoked by name for individual scans.  For now,
C     the desired frequencies must be in a group in the main input 
C     data stream that is preceeded by input that includes the
C     command 'LINEINIT'.  More than one such input can be given
C     and the input can be given in the PEAKFILE where reference
C     pointing is specified.
C     Someday I may allow input from a file.
C
      INCLUDE 'sched.inc'
C
      INTEGER           NLKEY, I, ISET, MODE, IULINE
      PARAMETER         (NLKEY=MAXLCH+2)
      DOUBLE PRECISION  LKEY(NLKEY), LVAL(NLKEY), ENDMARK, BLANK
      DATA              ISET / 0 /
      SAVE              ISET
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0,
     1      'RFREQ:  Starting to read line frequencies' )
C
C     Set up the input variables.  Avoid data statements because g77
C     does not like putting strings into other types.  Also didn't
C     bother using keyadd etc for this small set.
C
      CALL KPACK( '/       ', ENDMARK )
      CALL KPACK( '        ', BLANK )
      CALL KPACK( 'LINESET ', LKEY(1) )
      CALL KPACK( 'ENDLINES', LKEY(2) )
      DO I = 3, NLKEY
         CALL KPACK( 'RESTFREQ', LKEY(I) )
      END DO
C
C     Loop through input records.
C
  100 CONTINUE
C
C        Set defaults.
C
         LVAL(1) = BLANK
         LVAL(2) = UNSET
         DO I = 3, NLKEY
            LVAL(I) = 0.D0
         END DO
C
C        Read the data.
C
         MODE = 0
         CALL KEYIN( LKEY, LVAL, NLKEY, ENDMARK, MODE, IULINE, 6 )
         IF( MODE .EQ. 1 ) THEN
            CALL ERRLOG( 'RFREQ: Input data ended while reading'
     1             //' spectral line rest frequencies.' )
         END IF
C
C        Detect end of input.
C
         IF( LVAL(2) .EQ. 0.D0 ) GO TO 999
C
C        Got a new set.
C
         ISET = ISET + 1
         IF( ISET .GT. MAXLGP ) THEN
            WRITE( MSGTXT, '(A, I3, A)' ) ' SCHED only set up to use ',
     1         MAXLGP, ' rest frequency groups.'
            CALL ERRLOG( MSGTXT )
         END IF
C
C        Get the data.  Fill out the RESTFREQ array using the first
C        value when others are not provided.  NRESTFQ is no longer
C        used elsewhere, but keep the count in case it is useful
C        for future modifications.
C
         WRITE( LINENAME(ISET), '(A8)' ) LVAL(1)
         CALL UPCASE( LINENAME(ISET) )
         NRESTFQ(ISET) = 0
         DO I = 1, MAXLCH
            RESTFREQ(I,ISET) = LVAL(I+2)
            IF( RESTFREQ(I,ISET) .GT. 0.D0 ) THEN
               NRESTFQ(ISET) = NRESTFQ(ISET) + 1
            ELSE
               RESTFREQ(I,ISET) = RESTFREQ(1,ISET)
            END IF
         END DO
C
         GO TO 100
C
  999 CONTINUE
      NLGP = ISET
C
      RETURN
      END
