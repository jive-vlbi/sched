      SUBROUTINE PLSINP( XX, YY, CH, ID )  
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
      INCLUDE 'beam.inc'
C
C     Routine for Sched that check if an option is been selected in
C     the options area panel
C
      CHARACTER     CH, SG, STRING*4, MESSAG*8
      LOGICAL       PBTR
      INTEGER       ID, I, J, K, Z, N, IDOFS, REV, PL, LEN1
      INTEGER       IS, INDEXR, II, JJ
      REAL          XX, YY, XL, CSIZ
C ----------------------------------------------------------------------
C
      ID = 0
      PL = LEN1(EXPCODE)
C
C     Active area check
C
C -----------------------------------------------------------------
C     Area FILES
C -----------------------------------------------------------------
C
      IF( PMNBCK(1) .EQ. 3 ) THEN
         IDOFS  = 300
         PBTR   = .TRUE.
         PBMEXE = .TRUE.
C
C        Find which File Image box and highlight it
C
         DO 310 I=1,PFLBXM
           IF( PFLBOX(1,I) .LE. XX .AND. PFLBOX(2,I) .GE. XX .AND.
     1         PFLBOX(3,I) .LE. YY .AND. PFLBOX(4,I) .GE. YY ) THEN
C
              CALL PLBUTS( PBTR, PFLBOX(1,PFLBCK), PFLBOX(2,PFLBCK),
     1                     PFLBOX(3,PFLBCK), PFLBOX(4,PFLBCK), 0, 0,
     2                     ' ', 0, 0 )
C
              CALL PLBUTS( PBTR, PFLBOX(1,I), PFLBOX(2,I), PFLBOX(3,I),
     1                     PFLBOX(4,I), 0, 0, ' ', 0, 1 )
C
              PFLBCK = I
              PFLFIL(1) = EXPCODE(1:PL)//POPTYP(POPBCK)//PFLEXT(PFLBCK)
              J = INDEX( PFLEXT(PFLBCK), '/') - 1
              PFLFIL(2) = EXPCODE(1:PL)//POPTYP(POPBCK)//
     1                                       PFLEXT(PFLBCK)(1:J)
              CALL PLSTXT( PFLTXT(1,1), PFLTXT(2,1), PFLTXT(3,1),
     1                     PFLTXT(4,1), PFLFIL(2), 0, .TRUE. )
              RETURN
           END IF
 310     CONTINUE
C
C        Find if selected Edit Name of Image File 
C
         IF( PFLTXT(1,1) .LE. XX .AND. PFLTXT(2,1) .GE. XX .AND.
     1       PFLTXT(3,1) .LE. YY .AND. PFLTXT(4,1) .GE. YY ) THEN
C
            CALL PLEDIT( PFLTXT(1,1), PFLTXT(2,1), PFLTXT(3,1),
     1                   PFLTXT(4,1), PFLFIL(2) )
            CALL PLSTXT( PFLTXT(1,1), PFLTXT(2,1), PFLTXT(3,1),
     1                   PFLTXT(4,1), PFLFIL(2), 0, .TRUE. )
            K = LEN1( PFLFIL(2) )
            J = INDEX( PFLEXT(PFLBCK), '/')
            PFLFIL(1) = PFLFIL(2)(1:K)//PFLEXT(PFLBCK)(J:)
            RETURN
         END IF
C
C        Find if selected Edit Name of Inputs File 
C
         IF( PFLTXT(1,2) .LE. XX .AND. PFLTXT(2,2) .GE. XX .AND.
     1       PFLTXT(3,2) .LE. YY .AND. PFLTXT(4,2) .GE. YY ) THEN
C
            CALL PLEDIT( PFLTXT(1,2), PFLTXT(2,2), PFLTXT(3,2),
     1                   PFLTXT(4,2), PFLFIL(3) )
            CALL PLSTXT( PFLTXT(1,2), PFLTXT(2,2), PFLTXT(3,2),
     1                   PFLTXT(4,2), PFLFIL(3), 0, .TRUE. )
            RETURN
         END IF
C
C        Find which button and highlight it
C
         DO 320 I=1,PFLBTM
           IF( PFLBUT(1,I).LE.XX .AND. PFLBUT(2,I).GE.XX .AND.
     1             PFLBUT(3,I).LE.YY .AND. PFLBUT(4,I).GE.YY ) THEN
C
              CALL PLBUTA( PFLBUT(1,I), PFLBUT(2,I), PFLBUT(3,I),
     1                     PFLBUT(4,I), PFLLAB(I), -1 )
C
              ID = I + IDOFS 
              RETURN
           END IF
 320     CONTINUE
C
C        Find if Setup File ALL selected
C
         IF( PSFBXP(1,6) .LE. XX .AND. PSFBXP(2,6) .GE. XX .AND.
     1       PSFBXP(3,6) .LE. YY .AND. PSFBXP(4,6) .GE. YY ) THEN
C
C           Select/Deselect All Setup Files
C
            IF( PSFBCK .EQ. 0 ) THEN
               PSFBCK = 1
               PXYSEC = .TRUE.
            ELSE
               PSFBCK = 0
            END IF
            DO 322 I=1,NSETF
               PSFPOI(I) = PSFBCK
 322        CONTINUE

            CALL PLBUTX( PSFBXP(1,6), PSFBXP(2,6),
     1                   PSFBXP(3,6), PSFBXP(4,6), 0.,
     2                   0., ' ', 0, PSFBCK )
C
C          Select/Deselect visible setups
C
            
            DO 324 J=1,5
               I = ((PSFCNT - 1) * 5) + J
               IF( I .LE. NSETF ) THEN
                  CALL PLBUTX( PSFBXP(1,J), PSFBXP(2,J),
     1                         PSFBXP(3,J), PSFBXP(4,J), 0.,
     2                         0., ' ', 0, PSFBCK )
               END IF
 324        CONTINUE
            RETURN
         END IF
C
C        Find if Setup selected
C
         K = NSETF - ((PSFCNT - 1) * 5)
         IF( K .GT. 5 ) K = 5
         DO 330 I=1,K
            IF( PSFBXP(1,I) .LE. XX .AND. PSFBXP(2,I) .GE. XX .AND.
     1          PSFBXP(3,I) .LE. YY .AND. PSFBXP(4,I) .GE. YY ) THEN
C
C              Select/Deselect Setup if visible
C
               J = I + ((PSFCNT - 1) * 5)
               IF( PSFPOI(J) .EQ. 0 ) THEN
                  PSFPOI(J) = 1
               ELSE
                  PSFPOI(J) = 0
               END IF
               CALL PLBUTX( PSFBXP(1,I), PSFBXP(2,I),
     1                      PSFBXP(3,I), PSFBXP(4,I), 0.,
     2                      0., ' ', 0,  PSFPOI(J) )
C
C              Select/Deselect ALL Setups
C
               J = 0
               DO 326 II=1,NSETF
                  IF( PSFPOI(II) .EQ. 1 ) J = J + 1
 326           CONTINUE
               IF( J .EQ. NSETF ) THEN
                  PSFBCK = 1
                  PXYSEC = .TRUE.
               ELSE
                  PSFBCK = 0
               END IF
               CALL PLBUTX( PSFBXP(1,6), PSFBXP(2,6),
     1                      PSFBXP(3,6), PSFBXP(4,6), 0.,
     2                      0., ' ', 0, PSFBCK )
               RETURN
            END IF
 330     CONTINUE
C
C        Find if Scroll UP selected
C
         IF( PSFBXM(1,1) .LE. XX .AND. PSFBXM(2,1) .GE. XX .AND.
     1       PSFBXM(3,1) .LE. YY .AND. PSFBXM(4,1) .GE. YY ) THEN
C
C           Plot Previous Sources group only if groups counter
C           is greater than one
C
            IF( PSFCNT .GT. 1 ) THEN
               XL = (PPSDIM(2) - PPSDIM(1) - (0.035 * 4)) * (-1)
               CSIZ  = PPNDIM(3)
               PSFCNT = PSFCNT - 1
C
C              Clear text area
C
               CALL PLSTXT( PSFTXT(1,2), PSFTXT(2,2), PSFTXT(3,2),
     1                      PSFTXT(4,2), ' ', 0, .TRUE. )
C
C              Write Previous Setup group
C
               DO 340 I=1,5
                  J = ((PSFCNT - 1) * 5) + I
                  IS  = INDEXR( SETFILE(J), '/' ) + 1
                  IF( IS .EQ. 0 ) IS = 1
                  CALL PLBUTX( PSFBXP(1,I), PSFBXP(2,I), PSFBXP(3,I),
     1                         PSFBXP(4,I), XL, CSIZ, SETFILE(J)(IS:),
     2                         0, PSFPOI(J) )
 340           CONTINUE
            END IF
            RETURN
         END IF
C
C        Find if Scroll DOWN selected
C
         IF( PSFBXM(1,2) .LE. XX .AND. PSFBXM(2,2) .GE. XX .AND.
     1       PSFBXM(3,2) .LE. YY .AND. PSFBXM(4,2) .GE. YY ) THEN
C
C           Plot Next Setup group only if current group 
C           is less than max Setups
C
            J = PSFCNT * 5
            IF( J .LT. NSETF ) THEN
               PSFCNT = PSFCNT + 1
C
C              Clear text area
C
               XL = (PPSDIM(2) - PPSDIM(1) - (0.035 * 4)) * (-1)
               CSIZ  = PPNDIM(3)
C
               CALL PLSTXT( PSFTXT(1,2), PSFTXT(2,2), PSFTXT(3,2),
     1                      PSFTXT(4,2), ' ', 0, .TRUE. )
C
C              Write Previous Setup group
C
               K = NSETF - ((PSFCNT - 1) * 5)
               IF( K .GT. 5 ) K = 5
               DO 350 I=1,K
                  J = ((PSFCNT - 1) * 5) + I
                  IS  = INDEXR( SETFILE(J), '/' ) + 1
                  IF( IS .EQ. 0 ) IS = 1
                  CALL PLBUTX( PSFBXP(1,I), PSFBXP(2,I), PSFBXP(3,I),
     1                      PSFBXP(4,I), XL, CSIZ, SETFILE(J)(IS:),
     2                      0, PSFPOI(J) )
 350           CONTINUE
            END IF
            RETURN
         END IF
C
C -----------------------------------------------------------------
C     Area AXIS
C -----------------------------------------------------------------
C
      ELSE IF( PMNBCK(1) .EQ. 4 ) THEN
         IDOFS = 400
         PBTR  = .TRUE.
         XL = (PPSDIM(2) - PPSDIM(1) - (0.035 * 4)) * (-1)
         CSIZ  = PPNDIM(3)
C
C        Find which Type box and highlight it
C
         DO 410 I=1,POPBXM
           IF( POPBOX(1,I) .LE. XX .AND. POPBOX(2,I) .GE. XX .AND.
     1         POPBOX(3,I) .LE. YY .AND. POPBOX(4,I) .GE. YY ) THEN
C
              CALL PLBUTS( PBTR, POPBOX(1,POPBCK), POPBOX(2,POPBCK),
     1                     POPBOX(3,POPBCK), POPBOX(4,POPBCK), 0, 0,
     2                     ' ', 0, 0 )
C
              CALL PLBUTS( PBTR, POPBOX(1,I), POPBOX(2,I), POPBOX(3,I),
     1                     POPBOX(4,I), 0, 0, ' ', 0, 1 )
C              
C             If new type selected reset the Axis Type
C             and exponents of increment
C              
              IF( POPBCK .NE. I ) THEN
                 POPBCK = I
                 PXYBIG = .FALSE.
                 PXYONE = .FALSE.
                 IF( I .EQ. 1 ) THEN
                    PXYBCK(1) = 12
                    PXYBCK(2) = 12
                    PXYBIG    = .TRUE.
                 ELSE IF( I .EQ. 2 .OR. I .EQ. 5 ) THEN
                    PXYBCK(1) = 2
                    PXYBCK(2) = 5
                 ELSE IF( I .EQ. 3 ) THEN
                    PXYBCK(1) = 10
                    PXYBCK(2) = 11
                 ELSE IF( I .EQ. 4 ) THEN
                    PXYBCK(1) = 2
                    PXYBCK(2) = 9
                 ELSE IF( I .EQ. 6 ) THEN
                    PXYBCK(1) = 12
                    PXYBCK(2) = 12
                 ENDIF
                 CALL PLAXIS( 1 )
C
                 DO 412 J = 1, 4
                   PXSEXP(J) = 0
 412             CONTINUE
C
              ENDIF
C
              PFLFIL(1) = EXPCODE(1:PL)//POPTYP(POPBCK)//PFLEXT(PFLBCK)
              J = INDEX( PFLEXT(PFLBCK), '/') - 1
              PFLFIL(2) = EXPCODE(1:PL)//POPTYP(POPBCK)//
     1                                       PFLEXT(PFLBCK)(1:J)
              RETURN
C
           END IF
 410     CONTINUE
C
C        Check the Scales Selections
C
        IF( POPTYP(POPBCK) .EQ. 'BM' ) THEN
C
C         BEAM: Find if Frequency selected
C
         DO 409 I=1,2
           IF( PBMBXM(1,I) .LE. XX .AND. PBMBXM(2,I) .GE. XX .AND.
     1         PBMBXM(3,I) .LE. YY .AND. PBMBXM(4,I) .GE. YY ) THEN
C
              IF( NSETF .GT. 1 ) THEN
                 PBMEXE = .TRUE.
C
                 IF( I .EQ. 2) THEN
                    PSFBCK = PSFBCK - 1
                    IF( PSFBCK .LT. 1 ) THEN
                        PSFBCK = NSETF
                    END IF
                 ELSE
                    PSFBCK = PSFBCK + 1
                    IF( PSFBCK .GT. NSETF ) THEN
                        PSFBCK = 1
                    END IF
                 END IF
                 IF( SFFREQ(1,PSFBCK) .GT. 0.0 ) THEN
                    PBMFRQ = 30000.0 / SFFREQ(1,PSFBCK)
                 ELSE
                    PBMFRQ = 0.0
                 END IF
                 J = PBMFRQ * 10
                 CALL PGNUMB( J, -1, 1, STRING, N )
                 CALL PLSTXT( PBMTXT(1,1), PBMTXT(2,1), PBMTXT(3,1),
     1                        PBMTXT(4,1), STRING(1:N), 0, .TRUE. )
                 RETURN
              END IF
           END IF
 409     CONTINUE
C
C        BEAM: Find if Pixel selected
C
         DO 411 I=3,4
           IF( PBMBXM(1,I) .LE. XX .AND. PBMBXM(2,I) .GE. XX .AND.
     1         PBMBXM(3,I) .LE. YY .AND. PBMBXM(4,I) .GE. YY ) THEN
C
              IF( I .EQ. 4) THEN
                 PBMPIX = PBMPIX / 2
                 IF( PBMPIX .LT. MINPIX ) THEN
                     PBMPIX = MAXPIX
                 END IF
              ELSE
                 PBMPIX = PBMPIX * 2
                 IF( PBMPIX .GT. MAXPIX ) THEN
                     PBMPIX = MINPIX
                 END IF
              END IF
              CALL PGNUMB( PBMPIX, 0, 1, STRING, N )
              CALL PLSTXT( PBMTXT(1,2), PBMTXT(2,2), PBMTXT(3,2),
     1                     PBMTXT(4,2), STRING(1:N), 0, .TRUE. )
              RETURN
           END IF
 411     CONTINUE
C
C        BEAM: Find if Oversampling selected
C
         DO 413 I=5,6
           IF( PBMBXM(1,I) .LE. XX .AND. PBMBXM(2,I) .GE. XX .AND.
     1         PBMBXM(3,I) .LE. YY .AND. PBMBXM(4,I) .GE. YY ) THEN
C
              PBMEXE = .TRUE.
C
              IF( I .EQ. 6) THEN
                 PBMCEL = PBMCEL - 1
                 IF( PBMCEL .LT. 1 ) THEN
                     PBMCEL = 10
                 END IF
              ELSE
                 PBMCEL = PBMCEL + 1
                 IF( PBMCEL .GT. 10 ) THEN
                     PBMCEL = 1
                 END IF
              END IF
              CALL PGNUMB( PBMCEL, 0, 1, STRING, N )
              CALL PLSTXT( PBMTXT(1,3), PBMTXT(2,3), PBMTXT(3,3),
     1                     PBMTXT(4,3), STRING(1:N), 0, .TRUE. )
              RETURN
           END IF
 413     CONTINUE
C
C        BEAM: Find if Weight selected
C
         DO 415 I=7,8
           IF( PBMBXM(1,I) .LE. XX .AND. PBMBXM(2,I) .GE. XX .AND.
     1         PBMBXM(3,I) .LE. YY .AND. PBMBXM(4,I) .GE. YY ) THEN
C
              PBMEXE = .TRUE.
C
              IF( I .EQ. 8) THEN
                 PBMWGT = PBMWGT - 1
                 IF( PBMWGT .LT. 0 ) THEN
                     PBMWGT = 1
                 END IF
              ELSE
                 PBMWGT = PBMWGT + 1
                 IF( PBMWGT .GT. 1 ) THEN
                     PBMWGT = 0
                 END IF
              END IF
C
              IF( PBMWGT .EQ. 0 ) THEN
                 MESSAG = 'Natural'
              ELSE
                 MESSAG = 'Uniform'
              END IF
              CALL PLSTXT( PBMTXT(1,4), PBMTXT(2,4), PBMTXT(3,4),
     1                     PBMTXT(4,4), MESSAG, 0, .TRUE. )
              RETURN
           END IF
 415     CONTINUE
C
C        BEAM: Find if Image Function selected
C
         DO 417 I=9,10
           IF( PBMBXM(1,I) .LE. XX .AND. PBMBXM(2,I) .GE. XX .AND.
     1         PBMBXM(3,I) .LE. YY .AND. PBMBXM(4,I) .GE. YY ) THEN
C
              IF( I .EQ. 10) THEN
                 PBMCTF = PBMCTF - 1
                 IF( PBMCTF .LT. 0 ) THEN
                     PBMCTF = 2
                 END IF
              ELSE
                 PBMCTF = PBMCTF + 1
                 IF( PBMCTF .GT. 2 ) THEN
                     PBMCTF = 0
                 END IF
              END IF
              CALL PGNUMB( PBMCTF, 0, 1, STRING, N )
              CALL PLSTXT( PBMTXT(1,5), PBMTXT(2,5), PBMTXT(3,5),
     1                     PBMTXT(4,5), STRING(1:N), 0, .TRUE. )
              RETURN
           END IF
 417     CONTINUE
C
C        BEAM: Find if Color Palette selected
C
         DO 419 I=11,12
           IF( PBMBXM(1,I) .LE. XX .AND. PBMBXM(2,I) .GE. XX .AND.
     1         PBMBXM(3,I) .LE. YY .AND. PBMBXM(4,I) .GE. YY ) THEN
C
              IF( I .EQ. 12) THEN
                 PBMPAL = PBMPAL - 1
                 IF( PBMPAL .LT. 0 ) THEN
                     PBMPAL = 2
                 END IF
              ELSE
                 PBMPAL = PBMPAL + 1
                 IF( PBMPAL .GT. 2 ) THEN
                     PBMPAL = 0
                 END IF
              END IF
C
              IF( PBMPAL .EQ. 0 ) THEN
                 MESSAG = 'Gray'
              ELSE IF( PBMPAL .EQ. 1 ) THEN
                 MESSAG = 'Thermal'
              ELSE
                 MESSAG = 'RGB'
              END IF
              CALL PLSTXT( PBMTXT(1,6), PBMTXT(2,6), PBMTXT(3,6),
     1                     PBMTXT(4,6), MESSAG, 0, .TRUE. )
              RETURN
           END IF
 419     CONTINUE
C
C        BEAM: Find if Contours selected
C
         DO 421 I=13,14
           IF( PBMBXM(1,I) .LE. XX .AND. PBMBXM(2,I) .GE. XX .AND.
     1         PBMBXM(3,I) .LE. YY .AND. PBMBXM(4,I) .GE. YY ) THEN
C
              IF( PBMCON ) THEN
                 PBMCON = .FALSE.
                 MESSAG = 'No'
              ELSE
                 PBMCON = .TRUE.
                 MESSAG = 'Yes'
              END IF
              CALL PLSTXT( PBMTXT(1,7), PBMTXT(2,7), PBMTXT(3,7),
     1                     PBMTXT(4,7), MESSAG, 0, .TRUE. )
              RETURN
           END IF
 421     CONTINUE
C
        ELSE
C
C        OTHER AXIS TYPES
C
C        Find if X Type selected
C
         DO 420 I=1,2
           IF( PXYBXM(1,I) .LE. XX .AND. PXYBXM(2,I) .GE. XX .AND.
     1         PXYBXM(3,I) .LE. YY .AND. PXYBXM(4,I) .GE. YY ) THEN
              IF( I .EQ. 1) THEN
                 PXYBCK(1) = PXYBCK(1) - 1
                 IF( PXYBCK(1) .LT. POPLIM(POPBCK,1) ) THEN
                     PXYBCK(1) = POPLIM(POPBCK,2)
                 END IF
              ELSE
                 PXYBCK(1) = PXYBCK(1) + 1
                 IF( PXYBCK(1) .GT. POPLIM(POPBCK,2) ) THEN
                     PXYBCK(1) = POPLIM(POPBCK,1)
                 END IF
              END IF
              CALL PLSTXT( PXYTXT(1,1), PXYTXT(2,1), PXYTXT(3,1),
     1                     PXYTXT(4,1), PXYTYP(PXYBCK(1)), 0, .TRUE. )
              CALL PLWLSC( CH, 0, 1 )
              CALL PLTMOF( CH, 0 )
              CALL PLAXST( 'X' )
              RETURN
           END IF
 420     CONTINUE
C
C        Find if UT Time Offset selected
C
         IF( PXYTXT(1,3) .LE. XX .AND. PXYTXT(2,3) .GE. XX .AND.
     1       PXYTXT(3,3) .LE. YY .AND. PXYTXT(4,3) .GE. YY .AND.
     2       POFPOI(PXYBCK(1)) .EQ. 1 ) THEN
C
            CALL PLTMOF( CH, 1 )
            RETURN
         END IF
C
C        Find if LST Antenna Offset selected
C
         IF( PXYTXT(1,4) .LE. XX .AND. PXYTXT(2,4) .GE. XX .AND.
     1       PXYTXT(3,4) .LE. YY .AND. PXYTXT(4,4) .GE. YY .AND.
     2       POFPOI(PXYBCK(1)) .EQ. 2 ) THEN
C
            CALL PLTMOF( CH, 1 )
            RETURN
         END IF
C
C        Find if Wavelength Scale Box selected
C
         IF( PXYTXT(1,5) .LE. XX .AND. PXYTXT(2,5) .GE. XX .AND.
     1       PXYTXT(3,5) .LE. YY .AND. PXYTXT(4,5) .GE. YY .AND.
     2       PXYTYP(PXYBCK(1)) .EQ. 'Wv' ) THEN
C
            CALL PLWLSC( CH, 1, 1 )
            RETURN
         END IF
C
C        Find if Y Type selected
C
         DO 430 I=3,4
           IF( PXYBXM(1,I) .LE. XX .AND. PXYBXM(2,I) .GE. XX .AND.
     1         PXYBXM(3,I) .LE. YY .AND. PXYBXM(4,I) .GE. YY ) THEN
              IF( I .EQ. 3) THEN
                 PXYBCK(2) = PXYBCK(2) - 1
                 IF( PXYBCK(2) .LT. POPLIM(POPBCK,3) ) THEN
                     PXYBCK(2) = POPLIM(POPBCK,4)
                 END IF
              ELSE
                 PXYBCK(2) = PXYBCK(2) + 1
                 IF( PXYBCK(2) .GT. POPLIM(POPBCK,4) ) THEN
                     PXYBCK(2) = POPLIM(POPBCK,3)
                 END IF
              END IF
              CALL PLSTXT( PXYTXT(1,2), PXYTXT(2,2), PXYTXT(3,2),
     1                     PXYTXT(4,2), PXYTYP(PXYBCK(2)), 0, .TRUE. )
              CALL PLWLSC( CH, 0, 2 )
              CALL PLAXST( 'Y' )
              RETURN
           END IF
 430   CONTINUE
C
C      Find if XY Exponent selected. If Antenna is
C      true exclude Y axis
C
       K = 4
       IF( PYANTN ) K = 2
       DO 440 I=1,K
          IF( PXSBXM(1,I) .LE. XX .AND. PXSBXM(2,I) .GE. XX .AND.
     1        PXSBXM(3,I) .LE. YY .AND. PXSBXM(4,I) .GE. YY ) THEN
C
             IF( CH .EQ. 'A' .OR. CH .EQ. '+' ) THEN
                PXSEXP(I) = PXSEXP(I) + 1
                IF( PXSEXP(I) .GT. PXYEXP ) PXSEXP(I) = 0
             ELSE
                PXSEXP(I) = PXSEXP(I) - 1
                IF( PXSEXP(I) .LT. 0) PXSEXP(I) = PXYEXP 
             END IF
             CALL PLBUTC( PXSBXM(1,I), PXSBXM(2,I), PXSBXM(3,I),
     1                    PXSBXM(4,I), ' ', PXSEXP(I), 1 )
             RETURN
           END IF
 440     CONTINUE
C
C      Find if Default Value selected. If Antenna is
C      true exclude Y axis
C
       K = 4
       IF( PYANTN ) K = 2
       DO 445 I=1,K
          IF( PXSDEF(1,I) .LE. XX .AND. PXSDEF(2,I) .GE. XX .AND.
     1        PXSDEF(3,I) .LE. YY .AND. PXSDEF(4,I) .GE. YY ) THEN
             CALL PLAXDF( I )
             RETURN
          END IF
 445   CONTINUE
C
C        Find if XMin Coordinate element selected
C
         J = 3
         IF( PXYBIG ) J = 1
         DO 450 I=1,J
            K = I
            IF( PXYBIG ) K = 3
            IF( PXLTXT(1,I) .LE. XX .AND. PXLTXT(2,K) .GE. XX .AND.
     1          PXLTXT(3,I) .LE. YY .AND. PXLTXT(4,I) .GE. YY ) THEN
C
               CALL PLAXCK( PXLTXT(1,I), PXLTXT(2,K), PXLTXT(3,I),
     1                      PXLTXT(4,I), 1, I, CH )
C
               RETURN
            END IF
 450     CONTINUE  
C
C        Find if XMax Coordinate element selected
C
         J = 3
         IF( PXYBIG ) J = 1
         DO 460 I=1,J
            K = I
            IF( PXYBIG ) K = 3
            IF( PXRTXT(1,I) .LE. XX .AND. PXRTXT(2,K) .GE. XX .AND.
     1          PXRTXT(3,I) .LE. YY .AND. PXRTXT(4,I) .GE. YY ) THEN
C
               CALL PLAXCK( PXRTXT(1,I), PXRTXT(2,K), PXRTXT(3,I),
     1                      PXRTXT(4,I), 2, I, CH )
C
               RETURN
            END IF
 460     CONTINUE  
C
C        Find if Y Coordinate only if Antenna Type is false
C        To change the number of antennas use SOURCE Panel
C
         IF( .NOT. PYANTN ) THEN
C
C        Find if YBottom Coordinate element selected
C
         J = 3
         IF( PXYBIG .OR. PXYONE ) J = 1
         DO 470 I=1,J
            K = I
            Z = I
            IF( PXYBIG .OR. PXYONE ) K = 3
            IF( PXYONE ) Z = 3
            IF( PYBTXT(1,Z) .LE. XX .AND. PYBTXT(2,K) .GE. XX .AND.
     1          PYBTXT(3,I) .LE. YY .AND. PYBTXT(4,I) .GE. YY ) THEN
C
               CALL PLAXCK( PYBTXT(1,Z), PYBTXT(2,K), PYBTXT(3,I),
     1                      PYBTXT(4,I), 3, I, CH )
C
               RETURN
            END IF
 470     CONTINUE  
C
C        Find if YTop Coordinate element selected
C
         J = 3
         IF( PXYBIG .OR. PXYONE ) J = 1
         DO 480 I=1,J
            K = I
            Z = I
            IF( PXYBIG .OR. PXYONE ) K = 3
            IF( PXYONE ) Z = 3
            IF( PYTTXT(1,Z) .LE. XX .AND. PYTTXT(2,K) .GE. XX .AND.
     1          PYTTXT(3,I) .LE. YY .AND. PYTTXT(4,I) .GE. YY ) THEN
C
               CALL PLAXCK( PYTTXT(1,Z), PYTTXT(2,K), PYTTXT(3,I),
     1                      PYTTXT(4,I), 4, I, CH )
C
               RETURN
            END IF
 480     CONTINUE  
C
C        END Antenna Check
C
         END IF
C
C        Find if Sign Coordinate element selected. If antennas is
C        true exclude Y axis. If Sign Lock is True and UV is active
C        modify both axis
C
         K = 4
         IF( PYANTN ) K = 2
         DO 490 I=1,K
            J = 1
            JJ = 2
            IF( I .GT. 2 ) THEN
               J = 2
               JJ = -2
            ENDIF
C
            IF( PSGTXT(1,I) .LE. XX .AND. PSGTXT(2,I) .GE. XX .AND.
     1          PSGTXT(3,I) .LE. YY .AND. PSGTXT(4,I) .GE. YY .AND.
     2          PASIGN(J) ) THEN
C
               IF( PXSSGN(PXYBCK(J),I) .EQ. -1 ) THEN
                  SG = '+'
                  PXSSGN(PXYBCK(J),I) = 1
               ELSE
                  SG = '-'
                  PXSSGN(PXYBCK(J),I) = -1
               END IF
               CALL PLSTXT( PSGTXT(1,I), PSGTXT(2,I), PSGTXT(3,I),
     1                         PSGTXT(4,I), SG, 1, .TRUE. )
C
               IF( POPTYP(POPBCK) .EQ. 'UV' .AND. PLOSGN ) THEN
                  II = I + JJ
                  PXSSGN(PXYBCK(J),II) = PXSSGN(PXYBCK(J),I)
                  CALL PLSTXT( PSGTXT(1,II),PSGTXT(2,II),PSGTXT(3,II),       
     1                         PSGTXT(4,II), SG, 1, .TRUE. )
               ENDIF
C
               RETURN
            END IF
 490     CONTINUE  
C
C      Find if Days shift selected
C
         DO 492 I=1,2
          IF( PSGTXT(1,I) .LE. XX .AND. PSGTXT(2,I) .GE. XX .AND.
     1        PSGTXT(3,I) .LE. YY .AND. PSGTXT(4,I) .GE. YY .AND.
     2        PXYBCK(1)   .GE. 1  .AND. PXYBCK(1)   .LE. 3 ) THEN
             IF( PADAYS(PXYBCK(1),3) .GT. 0 ) THEN
                IF( CH .EQ. 'A' ) THEN
                   PADAYS(PXYBCK(1),I) = PADAYS(PXYBCK(1),I) + 1
                   IF( PADAYS(PXYBCK(1),I) .GT. PADAYS(PXYBCK(1),3) )
     1                 PADAYS(PXYBCK(1),I) = 0
                ELSE
                   PADAYS(PXYBCK(1),I) = PADAYS(PXYBCK(1),I) - 1
                   IF( PADAYS(PXYBCK(1),I) .LT. 0 )
     1                 PADAYS(PXYBCK(1),I) = PADAYS(PXYBCK(1),3)
                END IF
                CALL PGNUMB( PADAYS(PXYBCK(1),I), 0, 1, SG, J )
                CALL PLSTXT( PSGTXT(1,I), PSGTXT(2,I), PSGTXT(3,I),
     1                       PSGTXT(4,I), SG, 1, .TRUE. )
                
             END IF
             RETURN
          ENDIF
 492     CONTINUE
C
C        Find if Lock sign/value selected
C
         IF( POPTYP(POPBCK) .EQ. 'UV' ) THEN
C
          IF( PLOBOX(1,1) .LE. XX .AND. PLOBOX(2,1) .GE. XX .AND.
     1        PLOBOX(3,1) .LE. YY .AND. PLOBOX(4,1) .GE. YY ) THEN
             IF( PLOSGN ) THEN
                PLOSGN = .FALSE.
                REV = 0
             ELSE
                PLOSGN = .TRUE.
                REV = 1
             ENDIF
             CALL PLBUTS( PBTR, PLOBOX(1,1), PLOBOX(2,1),
     1                    PLOBOX(3,1), PLOBOX(4,1), 0, 0,
     2                    ' ', 0, REV)
             RETURN
          ENDIF
C
          IF( PLOBOX(1,2) .LE. XX .AND. PLOBOX(2,2) .GE. XX .AND.
     1        PLOBOX(3,2) .LE. YY .AND. PLOBOX(4,2) .GE. YY ) THEN
             IF( PLOVAL ) THEN
                PLOVAL = .FALSE.
                REV = 0
             ELSE
                PLOVAL = .TRUE.
                REV = 1
             ENDIF
             CALL PLBUTS( PBTR, PLOBOX(1,2), PLOBOX(2,2),
     1                    PLOBOX(3,2), PLOBOX(4,2), 0, 0,
     2                    ' ', 0, REV)
             RETURN
          ENDIF
         ENDIF
C
C        Find if Plot Sun selected
C
         IF( POPTYP(POPBCK) .EQ. 'RD' .OR.
     1       POPTYP(POPBCK) .EQ. 'UP' .OR.
     2       POPTYP(POPBCK) .EQ. 'AL' ) THEN
C
          IF( PSUBOX(1) .LE. XX .AND. PSUBOX(2) .GE. XX .AND.
     1        PSUBOX(3) .LE. YY .AND. PSUBOX(4) .GE. YY ) THEN
             IF( PXYSUN ) THEN
                PXYSUN = .FALSE.
             ELSE
                PXYSUN = .TRUE.
             ENDIF
             CALL PLSUN( ' ', 1 )
C
             RETURN
          END IF
C
C         Find if Sun Elevation Sign Selected
C
          IF( PSUSGN(1) .LE. XX .AND. PSUSGN(2) .GE. XX .AND.
     1        PSUSGN(3) .LE. YY .AND. PSUSGN(4) .GE. YY .AND.
     2        PXYSUN .AND. POPTYP(POPBCK) .NE. 'RD' ) THEN
             CALL PLSUN( ' ', 2 )
             RETURN
          END IF
C
C         Find if Sun Elevation Defaults Selected
C
          IF( PSUDEF(1) .LE. XX .AND. PSUDEF(2) .GE. XX .AND.
     1        PSUDEF(3) .LE. YY .AND. PSUDEF(4) .GE. YY .AND.
     2        PXYSUN .AND. POPTYP(POPBCK) .NE. 'RD' ) THEN
             PSUNEL = OPMINEL(1)
             PSUEXP = 0
             CALL PLSUN( ' ', 1 )
             RETURN
          END IF
C
C         Find if Sun Elevation Exponent Selected
C
          IF( PSUBXM(1) .LE. XX .AND. PSUBXM(2) .GE. XX .AND.
     1        PSUBXM(3) .LE. YY .AND. PSUBXM(4) .GE. YY .AND.
     2        PXYSUN .AND. POPTYP(POPBCK) .NE. 'RD' ) THEN
             CALL PLSUN( CH, 3 )
             RETURN
           END IF
C
C         Find if Sun Elevation Value Selected
C
          IF( PSUTXT(1) .LE. XX .AND. PSUTXT(2) .GE. XX .AND.
     1        PSUTXT(3) .LE. YY .AND. PSUTXT(4) .GE. YY .AND.
     2        PXYSUN .AND. POPTYP(POPBCK) .NE. 'RD' ) THEN
             CALL PLSUN( CH, 4 )
             RETURN
          END IF
C
         END IF
C
        END IF
C
C
C -----------------------------------------------------------------
C     Area STATIONS / SOURCES
C -----------------------------------------------------------------
C
      ELSE IF( PMNBCK(1) .EQ. 5 ) THEN
         IDOFS = 500
         PBMEXE = .TRUE.
         XL = (PPSDIM(2) - PPSDIM(1) - (0.035 * 4)) * (-1)
         CSIZ  = PPNDIM(3)
C
C        Find Baseline box and highlight it
C
         DO 500 I=1,2
            IF( PSTBXB(1,I) .LE. XX .AND. PSTBXB(2,I) .GE. XX .AND.
     1          PSTBXB(3,I) .LE. YY .AND. PSTBXB(4,I) .GE. YY ) THEN
               IF( I .EQ. 1 ) THEN
                  PSTBAS = .TRUE.
               ELSE
                  PSTBAS = .FALSE.
               END IF
C
               REV = 0
               IF ( PSTBAS ) REV = 1
               CALL PLBUTS( .TRUE., PSTBXB(1,1), PSTBXB(2,1),
     1                 PSTBXB(3,1), PSTBXB(4,1), 0, 0, ' ', 0, REV )
C
               REV = 1
               IF ( PSTBAS ) REV = 0
               CALL PLBUTS( .TRUE., PSTBXB(1,2), PSTBXB(2,2),
     1                 PSTBXB(3,2), PSTBXB(4,2), 0, 0, ' ', 0, REV )
               RETURN
            END IF
 500     CONTINUE
C
C        Find if Stations Scroll Selected
C
         IF( PSTSCR ) THEN
C
C          Scroll Left
C
           IF( PSTBXM(1,1) .LE. XX .AND. PSTBXM(2,1) .GE. XX .AND.
     1         PSTBXM(3,1) .LE. YY .AND. PSTBXM(4,1) .GE. YY ) THEN
C
              IF( PSTCNT .GT. 0 ) THEN
                 PSTCNT = PSTCNT - 1
                 CALL PLSTSL( 1 )
              END IF
C
              RETURN
           END IF
C
C          Scroll Right
C
           IF( PSTBXM(1,2) .LE. XX .AND. PSTBXM(2,2) .GE. XX .AND.
     1         PSTBXM(3,2) .LE. YY .AND. PSTBXM(4,2) .GE. YY ) THEN
C
              J = ( PSTCNT * ( PSTPLT / 2 ) ) + PSTPLT
              IF( J .LT. NSTA ) THEN
                 PSTCNT = PSTCNT + 1
                 CALL PLSTSL( 1 )
              END IF
C
              RETURN
           END IF
C
         END IF
C
C        Find which Station PLOT box and highlight it
C
         PBTR  = .FALSE.
         J = PSTCNT * ( PSTPLT / 2 )
         K = NSTA - J
         IF( K .GT. PSTPLT ) K = PSTPLT
C
         DO 510 I=1,K
           IF( PSTBXP(1,I) .LE. XX .AND. PSTBXP(2,I) .GE. XX .AND.
     1         PSTBXP(3,I) .LE. YY .AND. PSTBXP(4,I) .GE. YY ) THEN
C
              J = J + I
C
              IF( PSTBCK(J,1) .GT. 0 ) THEN
                 REV = 0
                 PSTNUM = PSTNUM - 1
              ELSE
                 REV = 1
                 PSTNUM = PSTNUM + 1
              ENDIF   
C
              CALL PLBUTS( PBTR, PSTBXP(1,I), PSTBXP(2,I), PSTBXP(3,I),
     1                     PSTBXP(4,I), 0, 0, ' ', 0, REV )
              PSTBCK(J,1) = REV
              PXYSEC = .TRUE.
              RETURN
           END IF
 510     CONTINUE
C
C        Find which HIGHLIGHT box and highlight it
C
         DO 520 I=1,K
           IF( PSTBXH(1,I) .LE. XX .AND. PSTBXH(2,I) .GE. XX .AND.
     1         PSTBXH(3,I) .LE. YY .AND. PSTBXH(4,I) .GE. YY ) THEN
C
              J = J + I
              REV = 1
              IF( PSTBCK(J,2) .GT. 0 ) REV = 0
              CALL PLBUTS( PBTR, PSTBXH(1,I), PSTBXH(2,I), PSTBXH(3,I),
     1                     PSTBXH(4,I), 0, 0, ' ', 0, REV )
              PSTBCK(J,2) = REV
              PXYSEC = .TRUE.
              RETURN
           END IF
 520     CONTINUE
C
C        Find if SET/UNSET PLOT selected
C
         DO 540 I=1,3,2
           IF( PSTSUN(1,I) .LE. XX .AND. PSTSUN(2,I) .GE. XX .AND.
     1         PSTSUN(3,I) .LE. YY .AND. PSTSUN(4,I) .GE. YY ) THEN
C
              IF( I .EQ. 3 ) then
                  REV    = 0
                  PSTNUM = 0
              ELSE
                  REV    = 1
                  PSTNUM = NSTA
              ENDIF
C
              DO 530 J=1, NSTA
                IF( J .LE. K ) THEN
                   CALL PLBUTS( PBTR, PSTBXP(1,J), PSTBXP(2,J),
     1                          PSTBXP(3,J), PSTBXP(4,J), 0, 0,
     2                          ' ', 0, REV )
                END IF
                PSTBCK(J,1) = REV
 530          CONTINUE
              PXYSEC = .TRUE.
              RETURN
C
           END IF
 540      CONTINUE
C
C        Find if SET/UNSET HIGHLIGHT selected
C
         DO 560 I=2,4,2
           IF( PSTSUN(1,I) .LE. XX .AND. PSTSUN(2,I) .GE. XX .AND.
     1         PSTSUN(3,I) .LE. YY .AND. PSTSUN(4,I) .GE. YY ) THEN
C
              REV = 1
              IF( I .EQ. 4 ) REV = 0
              DO 550 J=1, NSTA
                IF( J .LE. K ) THEN
                   CALL PLBUTS( PBTR, PSTBXH(1,J), PSTBXH(2,J),
     1                          PSTBXH(3,J), PSTBXH(4,J), 0, 0,
     2                          ' ', 0, REV )
                END IF
                PSTBCK(J,2) = REV
 550          CONTINUE
              PXYSEC = .TRUE.
              RETURN
C
           END IF
 560      CONTINUE
C
C        Find if Sources Scroll Selected
C
         IF( PSOSCR ) THEN
C
C          Scroll Left
C
           IF( PSOBXM(1,1) .LE. XX .AND. PSOBXM(2,1) .GE. XX .AND.
     1         PSOBXM(3,1) .LE. YY .AND. PSOBXM(4,1) .GE. YY ) THEN
C
              IF( PSOCNT .GT. 0 ) THEN
                 PSOCNT = PSOCNT - 1
                 CALL PLSOSL( 1 )
              END IF
C
              RETURN
           END IF
C
C          Scroll Right
C
           IF( PSOBXM(1,2) .LE. XX .AND. PSOBXM(2,2) .GE. XX .AND.
     1         PSOBXM(3,2) .LE. YY .AND. PSOBXM(4,2) .GE. YY ) THEN
C
              J = ( PSOCNT * ( PSOPLT / 2 ) ) + PSOPLT
              IF( J .LT. NSRC ) THEN
                 PSOCNT = PSOCNT + 1
                 CALL PLSOSL( 1 )
              END IF
C
              RETURN
           END IF
C
         END IF
C
C        Find which Sources PLOT box and highlight it
C
         PBTR  = .TRUE.
         J = PSOCNT * ( PSOPLT / 2 )
         K = NSRC - J
         IF( K .GT. PSOPLT ) K = PSOPLT
C
         DO 570 I=1,K
           IF( PSOBXP(1,I) .LE. XX .AND. PSOBXP(2,I) .GE. XX .AND.
     1         PSOBXP(3,I) .LE. YY .AND. PSOBXP(4,I) .GE. YY ) THEN
C
              J = J + I
C
              IF( PSOBCK(J) .GT. 0 ) THEN
                 REV = 0
                 PSONUM = PSONUM - 1
              ELSE
                 REV = 1
                 PSONUM = PSONUM + 1
              ENDIF   
C
              CALL PLBUTS( PBTR, PSOBXP(1,I), PSOBXP(2,I), PSOBXP(3,I),
     1                     PSOBXP(4,I), 0, 0, ' ', 0, REV )
              PSOBCK(J) = REV
              PXYSEC = .TRUE.
              RETURN
           END IF
 570     CONTINUE
C
C        Find if SET/UNSET Sources selected
C
         DO 590 I=1,2
           IF( PSOSUN(1,I) .LE. XX .AND. PSOSUN(2,I) .GE. XX .AND.
     1         PSOSUN(3,I) .LE. YY .AND. PSOSUN(4,I) .GE. YY ) THEN
C
              IF( I .EQ. 2 ) then
                  REV    = 0
                  PSONUM = 0
              ELSE
                  REV    = 1
                  PSONUM = NSRC
              ENDIF
C
              DO 580 J=1, NSRC
                IF( J .LE. K ) THEN
                   CALL PLBUTS( PBTR, PSOBXP(1,J), PSOBXP(2,J),
     1                          PSOBXP(3,J), PSOBXP(4,J), 0, 0,
     2                          ' ', 0, REV )
                END IF
                PSOBCK(J) = REV
 580          CONTINUE
              PXYSEC = .TRUE.
              RETURN
C
           END IF
 590      CONTINUE
C
C -----------------------------------------------------------------
C     Area OPTIONS
C -----------------------------------------------------------------
C
      ELSE IF( PMNBCK(1) .EQ. 6 ) THEN
         IDOFS = 600
C
C        Find if Display Line Width selected
C
         DO 600 I=1,2
           IF( PLWBXM(1,I) .LE. XX .AND. PLWBXM(2,I) .GE. XX .AND.
     1         PLWBXM(3,I) .LE. YY .AND. PLWBXM(4,I) .GE. YY ) THEN
              IF( I .EQ. 1) THEN
                 PLYLW(1) = PLYLW(1) + 1
              ELSE
                 PLYLW(1) = PLYLW(1) - 1
              END IF
              CALL PLOPLW( 1 )
              CALL PLSSYM( PCTTXT(1,2), PCTTXT(2,2), PCTTXT(3,2),
     1                     PCTTXT(4,2) )
              RETURN
           END IF
 600     CONTINUE
C
C        Find if Printer Line Width selected
C
         DO 605 I=3,4
           IF( PLWBXM(1,I) .LE. XX .AND. PLWBXM(2,I) .GE. XX .AND.
     1         PLWBXM(3,I) .LE. YY .AND. PLWBXM(4,I) .GE. YY ) THEN
              IF( I .EQ. 3) THEN
                 PLYLW(2) = PLYLW(2) + 1
              ELSE
                 PLYLW(2) = PLYLW(2) - 1
              END IF
              CALL PLOPLW( 2 )
              RETURN
           END IF
 605     CONTINUE
C
C        Find if Display Axis and Labels Width selected
C
         DO 610 I=1,2
           IF( PAWBXM(1,I) .LE. XX .AND. PAWBXM(2,I) .GE. XX .AND.
     1         PAWBXM(3,I) .LE. YY .AND. PAWBXM(4,I) .GE. YY ) THEN
              IF( I .EQ. 1) THEN
                 PLYAW(1) = PLYAW(1) + 1
              ELSE
                 PLYAW(1) = PLYAW(1) - 1
              END IF
              CALL PLOPAW( 1 )
              RETURN
           END IF
 610     CONTINUE
C
C        Find if Printer Axis and Label Width selected
C
         DO 615 I=3,4
           IF( PAWBXM(1,I) .LE. XX .AND. PAWBXM(2,I) .GE. XX .AND.
     1         PAWBXM(3,I) .LE. YY .AND. PAWBXM(4,I) .GE. YY ) THEN
              IF( I .EQ. 3) THEN
                 PLYAW(2) = PLYAW(2) + 1
              ELSE
                 PLYAW(2) = PLYAW(2) - 1
              END IF
              CALL PLOPAW( 2 )
              RETURN
           END IF
 615     CONTINUE
C
C        Find if Background Types selected
C
         DO 620 I=1,2
           IF( PBGBXM(1,I) .LE. XX .AND. PBGBXM(2,I) .GE. XX .AND.
     1         PBGBXM(3,I) .LE. YY .AND. PBGBXM(4,I) .GE. YY ) THEN
              IF( I .EQ. 1) THEN
                 PBGCK = PBGCK + 1
                 IF( PBGCK .GT. 3 ) PBGCK = 1
              ELSE
                 PBGCK = PBGCK - 1
                 IF( PBGCK .LT. 1 ) PBGCK = 3
              END IF
              CALL PLSTXT( PBGTXT(1,1), PBGTXT(2,1), PBGTXT(3,1),
     1                     PBGTXT(4,1), PBGLAB(PBGCK), 0, .FALSE. )
              CALL PLSCOL( PBGTXT(1,2), PBGTXT(2,2), PBGTXT(3,2),
     1                     PBGTXT(4,2), PBGCK, PLYBG(PBGCK) )
              RETURN
           END IF
 620     CONTINUE
C
C        Find if Background Colors selected
C
         DO 630 I=3,4
           IF( PBGBXM(1,I) .LE. XX .AND. PBGBXM(2,I) .GE. XX .AND.
     1         PBGBXM(3,I) .LE. YY .AND. PBGBXM(4,I) .GE. YY ) THEN
              IF( I .EQ. 3) THEN
                 PLYBG(PBGCK) = PLYBG(PBGCK) + 1
              ELSE
                 PLYBG(PBGCK) = PLYBG(PBGCK) - 1
              END IF
              CALL PLSCOL( PBGTXT(1,2), PBGTXT(2,2), PBGTXT(3,2),
     1                     PBGTXT(4,2), PBGCK, PLYBG(PBGCK) )
              IF( PBGCK .EQ. 1 ) THEN
                 CALL PLSSYM( PCTTXT(1,2), PCTTXT(2,2), PCTTXT(3,2),
     1                        PCTTXT(4,2) )
              END IF
              RETURN
           END IF
 630     CONTINUE
C
C        Find if Catalog Names selected
C
         DO 640 I=1,2
           IF( PCTBXM(1,I) .LE. XX .AND. PCTBXM(2,I) .GE. XX .AND.
     1         PCTBXM(3,I) .LE. YY .AND. PCTBXM(4,I) .GE. YY ) THEN
              IF( I .EQ. 1) THEN
                 PCTCK = PCTCK + 1
                 IF( PCTCK .GT. PCTMAX ) PCTCK = 1
              ELSE
                 PCTCK = PCTCK - 1
                 IF( PCTCK .LT. 1 ) PCTCK = PCTMAX
              END IF
              CALL PLSTXT( PCTTXT(1,1), PCTTXT(2,1), PCTTXT(3,1),
     1                     PCTTXT(4,1), PCTLAB(PCTCK), 0, .FALSE. )
              CALL PLSSYM( PCTTXT(1,2), PCTTXT(2,2), PCTTXT(3,2),
     1                     PCTTXT(4,2) )
              RETURN
           END IF
 640     CONTINUE
C
C        Find if Catalog Symbols selected
C
         DO 642 I=3,4
           IF( PCTBXM(1,I) .LE. XX .AND. PCTBXM(2,I) .GE. XX .AND.
     1         PCTBXM(3,I) .LE. YY .AND. PCTBXM(4,I) .GE. YY ) THEN
              IF( I .EQ. 3) THEN
                 PLYCT(PCTCK,2) = PLYCT(PCTCK,2) + 1
                 IF( PLYCT(PCTCK,2) .GT. 18 ) PLYCT(PCTCK,2) = 1
              ELSE
                 PLYCT(PCTCK,2) = PLYCT(PCTCK,2) - 1
                 IF( PLYCT(PCTCK,2) .LT. 1 ) PLYCT(PCTCK,2) = 18
              END IF
              CALL PLSSYM( PCTTXT(1,2), PCTTXT(2,2), PCTTXT(3,2),
     1                     PCTTXT(4,2) )
              RETURN
           END IF
 642     CONTINUE
C
C        Find if Catalog Colors selected
C
         DO 644 I=5,6
           IF( PCTBXM(1,I) .LE. XX .AND. PCTBXM(2,I) .GE. XX .AND.
     1         PCTBXM(3,I) .LE. YY .AND. PCTBXM(4,I) .GE. YY ) THEN
              IF( I .EQ. 5) THEN
                 PLYCT(PCTCK,1) = PLYCT(PCTCK,1) + 1
                 IF( PLYCT(PCTCK,1) .GT. 15 ) PLYCT(PCTCK,1) = 0
              ELSE
                 PLYCT(PCTCK,1) = PLYCT(PCTCK,1) - 1
                 IF( PLYCT(PCTCK,1) .LT. 0 ) PLYCT(PCTCK,1) = 15
              END IF
              CALL PLSSYM( PCTTXT(1,2), PCTTXT(2,2), PCTTXT(3,2),
     1                     PCTTXT(4,2) )
              RETURN
           END IF
 644     CONTINUE
C
C        Find Actions Button
C
         DO 650 I=1,3
           IF( PLYBUT(1,I) .LE. XX .AND. PLYBUT(2,I) .GE. XX .AND.
     1         PLYBUT(3,I) .LE. YY .AND. PLYBUT(4,I) .GE. YY ) THEN
C
              CALL PLBUTA( PLYBUT(1,I), PLYBUT(2,I), PLYBUT(3,I),
     1                     PLYBUT(4,I), PLYLAB(I), -1 )
C
              ID = I + IDOFS 
              RETURN
           END IF
 650     CONTINUE
C
C -----------------------------------------------------------------
C     END Input Area
C -----------------------------------------------------------------
C
      END IF
C
      ID = 0
      RETURN
      END
