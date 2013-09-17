      SUBROUTINE GETPAIRS
C
C     Make pairs of sources with a pointing center and a named group
C     of phase centers.  This is useful when writing out source lists
C     etc.
C
C     A given pointing source could, in principle, have more than
C     one list of centers (think multi-band work) or a given list
C     of centers could be used with more than one pointing center
C     (not sure why).  Set up the infrastructure to be compatible
C     with either, but then don't allow either because I don't see
C     how Adam's V2D spec can support either.  
C
      INCLUDE   'sched.inc'
C
C      integer   len1
      INTEGER   ISCN, IPAIR, JPAIR
C -----------------------------------------------------------------
C
C     Construct the pairs.
C
      NPAIR = 0
      DO ISCN = SCAN1, SCANL
         IF( ICENT(ISCN) .NE. 0 ) THEN
            IF( NPAIR .NE. 0 ) THEN
               DO IPAIR = 1, NPAIR
                  IF( PAIRSRC(IPAIR) .EQ. SRCNUM(ISCN) .AND.
     1                PAIRCENT(IPAIR) .EQ. ICENT(ISCN) ) THEN
                     GO TO 100
                  END IF
               END DO
            END IF
            NPAIR = NPAIR + 1
            PAIRSRC(NPAIR) = SRCNUM(ISCN)
            PAIRCENT(NPAIR) = ICENT(ISCN)
  100       CONTINUE
         END IF
      END DO     
C
C     Have a cow for now if there is more than one phase list
C     center per pointing source.  v3d doesn't support that.
C     I think the centers can be duplicated with different 
C     pointing centers although I don't know why you would do that.
C     The check is simply done by looking for duplicate source
C     in the pairs list.
C
      IF( NPAIR .GE. 2 ) THEN
         DO IPAIR = 2, NPAIR
            DO JPAIR = 1, IPAIR - 1           
               IF( PAIRSRC(JPAIR) .EQ. PAIRSRC(IPAIR) .OR.
     1             PAIRCENT(JPAIR) .EQ. PAIRCENT(IPAIR) ) THEN
                  MSGTXT = ' '
                  CALL WLOG( 1, ' ' )
                  WRITE( MSGTXT, '( A, A, 3X, A )' )
     1              'Sources: ', SRCNAME(SRLSTN(PAIRSRC(JPAIR))),
     2               SRCNAME(SRLSTN(PAIRSRC(IPAIR)))
                  CALL WLOG(1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, 3X, A )' )
     1              'Centers: ', CTRNAME(PAIRCENT(JPAIR)), 
     2              CTRNAME(PAIRCENT(IPAIR))
                  CALL WLOG(1, MSGTXT )
                  MSGTXT = ' '
                  IF( PAIRSRC(JPAIR) .EQ. PAIRSRC(IPAIR) ) 
     1               CALL ERRLOG( 'Two lists of phase centers '//
     2               'cannot be used with the same pointng center' )
                  IF( PAIRCENT(JPAIR) .EQ. PAIRCENT(IPAIR) )
     1               CALL WLOG( 1, 'Do you really indend to use '//
     2               'two pointing centers with the same list '//
     3               'of phase centers?' )
               END IF
            END DO
         END DO
      END IF
C
      RETURN
      END
