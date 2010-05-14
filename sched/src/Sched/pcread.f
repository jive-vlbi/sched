      SUBROUTINE PCREAD( INSCH )
C
C     Read lists of phase centers.
C
C     Each list has a name and a list of sources.
C     Later the sources need to be found in the source catalogs
C     and added to the lists used.
C
      INCLUDE 'sched.inc'
C
C     Don't use all the KEYADD structure for this short list.
C
      INTEGER           MPKEY, ISRC, IK, MODE, NPKEY, INSCH
      PARAMETER         ( MPKEY = 2100 )
      DOUBLE PRECISION  PKEY(MPKEY), PVAL(MPKEY), ENDMARK, BLANK8
C -------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0,
     1      'PCREAD:  Starting to read groups of phase centers' )
C
C     Set up the input variables.  Avoid data statements because g77
C     does not like putting strings into other types.  Also didn't
C     bother using keyadd etc for this small set.
C
      CALL KPACK( '/       ', ENDMARK )
      CALL KPACK( '        ', BLANK8 )
C
C     Set keys.  Only do if haven't been here before.  Shouldn't
C     go through here more than once, but ....
C
      IF( PKEY(1) .NE. ENDMARK ) THEN
         CALL KPACK( 'ENDCENT',  PKEY(1) )
         PVAL(1) = UNSET
         CALL KPACK( 'NAME', PKEY(2) )
         PKEY(3) = BLANK8
         IK = 4
         DO ISRC = 1, MCSRC
            CALL KPACK( 'SOURCES ', PKEY(IK) )
            PKEY(IK+1) = BLANK8
            IK = IK + 2
         END DO
         NPKEY = IK - 1
      END IF
C
      NCENT = 0
C
C     Loop through input records and gather the groups.
C
  100 CONTINUE
C
C        Set everything back to defaults.
C
         DO IK = 2, NPKEY
            PVAL(IK) = BLANK8
         END DO
C
C        Read the next group.  These groups will be in the
C        normal schedule input, so don't set up to read from 
C        a file.
C
         MODE = 0
         CALL KEYIN( PKEY, PVAL, NPKEY, ENDMARK, MODE, INSCH, 6 )
         IF( MODE .EQ. 1 ) THEN
            CALL ERRLOG( 'PCREAD: Input data ended while reading'
     1           //' phase center groups.' )
         END IF
C
C        Detect end of input.
C
         IF( PVAL(1) .EQ. 0.D0 ) THEN
            GO TO 999
         END IF
C
C        Got a new group.
C
         NCENT = NCENT + 1
         NCSRC(NCENT) = 0
C
C        Extract the phase center information.
C
         WRITE( CTRNAME(NCENT), '( A8, A4 )' ) PVAL(2), PVAL(3)
         IK = 4
         DO ISRC = 1, MCSRC
            WRITE( CTRSRCN(ISRC,NCENT), '( A8, A4 )' ) 
     1           PVAL(IK), PVAL(IK+1)
            IF( CTRSRCN(ISRC,NCENT) .EQ. ' ' ) GO TO 200
            NCSRC(NCENT) = NCSRC(NCENT) + 1
            IK = IK + 2
         END DO
C
C        Jump here when out of sources.
C
  200    CONTINUE
C
C        Go back for another group.
C
         GO TO 100
C
C     Jump here when out of input records.
C
  999 CONTINUE
      RETURN
      END
