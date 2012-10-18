C      PROGRAM SYN3_C
C
C      Help figure out what the unused synthesizers should be set to.
C
      PARAMETER  (NF3 = 28 )
      PARAMETER  (NLO = 23 )
      REAL  F3(NF3), IFF
      REAL  LO(NLO)
      CHARACTER  MARK(NF3,NLO)*1
      LOGICAL  OK, POK
C
      DATA  F3 / 15.9, 15.6, 15.4, 15.1, 
     1           14.9, 14.6, 14.4, 14.1, 
     2           13.9, 13.6, 13.4, 13.1, 
     3           12.9, 12.6, 12.4, 12.1, 
     4           11.9, 11.6, 11.4, 11.1, 
     5           10.9, 10.6, 10.4, 10.1, 
     6            9.9,  9.6,  9.4,  9.1 /
      DATA  LO /        3.4,  3.6,  3.9,
     1            4.1,  4.4,  4.6,  4.9,
     2            5.1,  5.4,  5.6,  5.9,
     3            6.1,  6.4,  6.6,  6.9,
     4            7.1,  7.4,  7.6,  7.9,
     5            8.1,  8.4,  8.6,  8.9 /

C --------------------------------------------------------------------
      WRITE(*,*)  'RFI risks'
      WRITE(*,*)  ' F3  * N    LO * N   IFF'
      DO I = 1, NF3
         DO J = 1, NLO
            MARK(I,J) = '-'
            DO K = 1, 3
               DO L = 1, 5
                  IFF = ABS( F3(I) * K - LO(J) * L )
                  IF( IFF .GT. 0.45 .AND. IFF .LT. 1.05 ) THEN
                     WRITE(*,'( F7.3, I2, F7.3, I2, F7.3, 4X, 2F7.0)' )
     1                  F3(I), K, LO(J), L, IFF, (LO(J)-IFF) * 1000.,
     2                  (LO(J)+IFF) * 1000.
                     MARK(I,J) = 'X'
                  END IF
               END DO
            END DO
         END DO
         WRITE(*,*) ' '
      END DO
C
C     Write a matrix to show what is or is not good.
C
      WRITE(*,*) ' '
      WRITE(*,'( 8X, 23F4.1)' )   (LO(J),J=1,NLO)
      DO I = 1, NF3
         WRITE(*,'( F6.1, 2X, 23( 2X, A1, 1X) )' ) 
     1       F3(I), (MARK(I,J),J=1,NLO)
      END DO
C
C     Write a matrix for 2 LO combinations.
C
      WRITE(*,*) ' '
      WRITE(*,'( 12X, 28F5.1)' )   (F3(I),I=1,NF3)
      DO J = 1, NLO-1
         DO K = J+1, NLO
            WRITE(*,'( 2F5.1, 2X, 28( 2X, 2A1, 1X) )' ) 
     1             LO(J), LO(K), (MARK(I,J),MARK(I,K),I=1,NF3)
         END DO
      END DO
C
C     Look for pairs of F3's that allow all pairs of LOs.
C     There aren't any.
C
      WRITE(*,*) ' '
      WRITE(*,*) 'Possible pairs of F3s'
      DO I = 1, NF3 - 1
         DO J = I+1, NF3
            POK = .TRUE.
            DO K = 1, NLO-1
               DO L = K+1, NLO
                  OK = ( MARK(I,K) .EQ. '-' .AND. 
     1                   MARK(I,L) .EQ. '-' ) .OR.
     2                 ( MARK(J,K) .EQ. '-' .AND. 
     3                   MARK(J,L) .EQ. '-' )
                  IF( .NOT. OK ) POK = .FALSE.
               END DO
            END DO
            IF( POK ) WRITE( *, '( 2F6.1 )' ) F3(I), F3(J)                
         END DO
      END DO
C
C     Look for triplets of F3s that work.  There aren't any.
C
      WRITE(*,*) ' '
      WRITE(*,*) 'Possible tripples of F3s'
      DO I = 1, NF3 - 2
         DO J = I+1, NF3-1
            DO M = J+1, NF3
               POK = .TRUE.
               DO K = 1, NLO-1
                  DO L = K+1, NLO
                     OK = ( MARK(I,K) .EQ. '-' .AND. 
     1                      MARK(I,L) .EQ. '-' ) .OR.
     2                    ( MARK(J,K) .EQ. '-' .AND. 
     3                      MARK(J,K) .EQ. '-' ) .OR. 
     4                    ( MARK(M,K) .EQ. '-' .AND. 
     5                      MARK(M,L) .EQ. '-' )
                     IF( .NOT. OK ) POK = .FALSE.
                  END DO
               END DO
               IF( POK ) WRITE( *, '( 3F6.1 )' ) F3(I), F3(J), F3(M)
            END DO
         END DO
      END DO
      STOP
      END
