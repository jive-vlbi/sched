C      PROGRAM SYN3_C
C
C      Help figure out what the unused synthesizers should be set to.
C      Something is needed in SCHED's setdefs.f
C
      PARAMETER  (NF3 = 28 )
      PARAMETER  (NLO = 14*4 )
      REAL  F3(NF3), IFF
      REAL  LO(NLO)
      CHARACTER  MARK(NF3,NLO)*1
      CHARACTER  MRK2(NLO,NLO)*1
      LOGICAL  OK, POK
C
      DATA  F3 / 15.9, 15.6, 15.4, 15.1, 
     1           14.9, 14.6, 14.4, 14.1, 
     2           13.9, 13.6, 13.4, 13.1, 
     3           12.9, 12.6, 12.4, 12.1, 
     4           11.9, 11.6, 11.4, 11.1, 
     5           10.9, 10.6, 10.4, 10.1, 
     6            9.9,  9.6,  9.4,  9.1 /
C --------------------------------------------------------------------
      WRITE(*,*)  'RFI risks'
      WRITE(*,*)  ' F3  * N    LO * N   IFF'
C
C     Construct the LO array.
C
      DO I = 1, NLO
         LO(I) = 2.1 + MOD( I - 1 , 2 ) * 0.3 + ( ( I - 1 ) / 2 ) * 0.5
      END DO     
C
C     The range for C band.
      IC1 = 6
      IC2 = 6 + 22
C
C     Identify the cases where there are likely to be signals in the IF.
C     These are pairs of F3 and LO
C
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
C     Identify the cases where there are likely to be signals in the IF.
C     These are pairs of two LOs
C
      DO I = 1, NLO
         DO J = 1, NLO
            MRK2(I,J) = '~'
            DO K = 1, 5
               DO L = 1, 5
                  IFF = ABS( LO(I) * K - LO(J) * L )
                  IF( IFF .GT. 0.45 .AND. IFF .LT. 1.05 ) THEN
                     WRITE(*,'( F7.3, I2, F7.3, I2, F7.3, 4X, 2F7.0, '//
     1                     ' 4X, 2F7.0 )' )
     2                  LO(I), K, LO(J), L, IFF, 
     3                  (LO(I)-IFF) * 1000., (LO(I)+IFF) * 1000., 
     4                  (LO(J)-IFF) * 1000., (LO(J)+IFF) * 1000.
                     IF( K + L .LE. 4 ) THEN
                        MRK2(I,J) = 'O'
                     ELSE IF( MRK2(I,J) .NE. 'O' .AND. 
     1                   K + L .LE. 6 ) THEN
                        MRK2(I,J) = 'L'
                     ELSE IF( MRK2(I,J) .NE. 'L' .AND. 
     1                        MRK2(I,J) .NE. 'O' ) THEN
                        MRK2(I,J) = 'l'
                     END IF
                  END IF
               END DO
            END DO
         END DO
         WRITE(*,*) ' '
      END DO
C
C     Write a matrix to show what is or is not good for the unused synthesizers.
C
      WRITE(*,*) ' '
      WRITE(*,'( 8X, 28F4.1)' )   (F3(J),J=1,NF3)
      DO I = 1, NLO
         WRITE(*,'( F6.1, 2X, 28( 2X, A1, 1X) )' ) 
     1       LO(I), (MARK(J,I),J=1,NF3)
      END DO
C
C     Write a matrix to show what is or is not good for the pairs of LOs.
C     Keep this one in the C band range.
C
      WRITE(*,*) ' '
      WRITE(*,'( 8X, 23F4.1)' )   (LO(J),J=IC1,IC2)
      DO I = IC1, IC2
         WRITE(*,'( F6.1, 2X, 23( 2X, A1, 1X) )' ) 
     1       LO(I), (MRK2(I,J),J=IC1,IC2)
      END DO
C
C     Write a matrix for 2 LO combinations.
C     Also mark cases where there will be issues between the LOs.
C
      WRITE(*,*) ' '
      WRITE(*,'( 15X, 28F5.1)' )   (F3(I),I=1,NF3)
      DO J = 1, NLO-1
         DO K = J+1, NLO
            WRITE(*,'( 2F5.1, 2X, A1, 2X, 28( 3X, 2A1 ) )' ) 
     1             LO(J), LO(K), MRK2(J,K), 
     2             (MARK(I,J),MARK(I,K),I=1,NF3)
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
C
C     Do a matrix with all possible LO combinations in pairs.



      STOP
      END
