      SUBROUTINE PLAXIN
C
C     Routine for sched that init the value for all type
C     of axis
C
      INCLUDE 'plot.inc'
C
      INTEGER          K, I, J
C ----------------------------------------------------------------------
C
C     Set the default XY Axis Value.
C     If X axis equal UT/GST/LST set to the schedule time
C     If X axis equal Km set to the schedule max baseline
C
      DO 100 K = 1, PXYMAX
         IF( PXYTYP(K) .EQ. 'UT' ) THEN
            DO 12 I=1,2
               DO 10 J=1,3
                  PXSVAL(K,I,J) = PUTVAL(I,J)
 10            CONTINUE
 12         CONTINUE
            DO 14 I=1,4
               PXSSGN(K,I) = 1
 14         CONTINUE
C
C        If X axis equal LST set to the schedule time
C
         ELSE IF( PXYTYP(K) .EQ. 'LST' ) THEN
            DO 22 I=1,2
               DO 20 J=1,3
                  PXSVAL(K,I,J) = PLSVAL(I,J)
 20            CONTINUE
 22         CONTINUE
            DO 23 I=1,4
               PXSSGN(K,I) = 1
 23         CONTINUE
C
C        If X axis equal GST set to the schedule time
C
         ELSE IF( PXYTYP(K) .EQ. 'GST' ) THEN
            DO 26 I=1,2
               DO 24 J=1,3
                  PXSVAL(K,I,J) = PSGVAL(I,J)
 24            CONTINUE
 26         CONTINUE
            DO 27 I=1,4
               PXSSGN(K,I) = 1
 27         CONTINUE
C
C        If XY axis equal Km set to the schedule limits
C
         ELSE IF( PXYTYP(K) .EQ. 'Km' ) THEN
            DO 28 I=1,4
               PXSVAL(K,I,1) = ABS( PKMVAL(I) )
               IF( PKMVAL(I) .GE. 0 ) THEN
                  PXSSGN(K,I) = 1
               ELSE
                  PXSSGN(K,I) = -1
               END IF
 28         CONTINUE
C
C        If XY axis equal Wv set to the schedule limits
C
         ELSE IF( PXYTYP(K) .EQ. 'Wv' ) THEN
            DO 29 I=1,4
               PXSVAL(K,I,1) = ABS( PWLVAL(I) )
               IF( PWLVAL(I) .GE. 0 ) THEN
                  PXSSGN(K,I) = 1
               ELSE
                  PXSSGN(K,I) = -1
               END IF
 29         CONTINUE
C
         ELSE
C
            PXSVAL(K,1,1) = ABS( PXSLIM(K,1) )
            IF( PXSLIM(K,1) .GE. 0 ) THEN
               PXSSGN(K,1) = 1
            ELSE
               PXSSGN(K,1) = -1
            END IF
C
            PXSVAL(K,2,1) = ABS( PXSLIM(K,2) )
            IF( PXSLIM(K,2) .GE. 0 ) THEN
               PXSSGN(K,2) = 1
            ELSE
               PXSSGN(K,2) = -1
            END IF
C
            PXSVAL(K,3,1) = PXSVAL(K,1,1)
            PXSVAL(K,4,1) = PXSVAL(K,2,1)
            PXSSGN(K,3)   = PXSSGN(K,1)
            PXSSGN(K,4)   = PXSSGN(K,2)
C
C           All MM SS to 0
C
            DO 40 I = 1, 4
               DO 30 J = 2, 3
                  PXSVAL(K,I,J) = 0
 30            CONTINUE
 40         CONTINUE
C
         END IF
C
 100  CONTINUE
C
      RETURN
      END
