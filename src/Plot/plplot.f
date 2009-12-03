      SUBROUTINE PLPLOT( PLDEV, POPEN, WCL)
C
C     Routine for sched that plot in a selected device
C
C     Modified 21 Dec. 2001 by Craig Walker to plot different
C     sources on different panels when doing UV plots.  See 
C     comments that start with CRCW.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER   PLDEV*(*), XAXIS*8, YAXIS*8
      CHARACTER   ANSWER*3
      LOGICAL     POPEN, COLOR, SCREEN
      INTEGER     KX, KY, KSRC, JSRC, KSTA, KSET, I, KSFN
      INTEGER     WID, WCL, IC1, IC2, LANSW, PGOPEN
      DOUBLE PRECISION  RXMIN, RXMAX, RYMIN, RYMAX
CRCW  For multiple UV plots:
      INTEGER     NSRCPLT, ISRC, NPX, NPY, IPSRC
C ----------------------------------------------------------------------
C
C     Warning for the Done Button on Plots RD
C
      IF( POPBCK .EQ. 3 .AND. WCL .EQ. 0 ) THEN
         CALL PLBORD( 0.32, 0.95, 0.35, 0.73, 1, 1 )
         CALL PGSCH( 2.0 )
         CALL PGSCI( 2 )
         CALL PGSLW( 6 )
         CALL PGPTXT(0.52, 0.62, 0.0, 0.0, 'WARNING:')
         CALL PGSCI( 15 )
         CALL PGSLW( 6 )
         CALL PGSLW( 5 )
         CALL PGPTXT(0.37, 0.52, 0.0, 0.0, 'Use the DONE button' )
         CALL PGPTXT(0.382, 0.44, 0.0, 0.0, 'in the PLOT window')
         PRWARN = .TRUE.
      END IF
C
C     If the device is already open select it, else open the device
C     
      IF( POPEN .AND. WCL .EQ. 0 ) THEN
         CALL PGSLCT( PPNWIN )
         CALL PGERAS
      ELSE
         WID = PGOPEN( PLDEV )
         CALL PGASK( .FALSE. )
         CALL PGSLCT( WID )
         IF( WCL .EQ. 0 ) THEN
            POPEN = .TRUE.
            PPNWIN = WID
         END IF
      END IF
C
C     Get the plot type.  If hardcopy, have to use different
C     background and fill colors to get plots right.  If no
C     color capability, don't set colors.
C
      CALL PGQINF( 'HARDCOPY', ANSWER, LANSW )
      SCREEN = ANSWER(1:3) .EQ. 'NO'
      CALL PGQCOL( IC1, IC2 )
      COLOR = IC2 .GT. 1
      CALL PLOTBG( 0, 0 )
C
C     Set the view surface for the Portrait format of Plots
C
      IF( .NOT. SCREEN .AND.
     1  ( PFLBCK .EQ. 2 .OR. PFLBCK .EQ. 4 .OR. PFLBCK .EQ. 6 ) ) THEN
         CALL PGPAP( 8.0, 0.618 )
      END IF
C
C     Set Source numbers
C
      KSRC = 0
      JSRC = 0
C
C     Set Station numbers
C
      KSTA = 0
C
C     Check and Set the Setup File
C
      KSET = 0
      IF( PSFBCK .EQ. 0 ) THEN
         KSFN = 0
         DO 10 I=1,NSETF
            IF( KSET .EQ. 0 .AND. PSFPOI(I) .EQ. 1 ) KSET = I
            IF( PSFPOI(I) .EQ. 1 ) KSFN = KSFN + 1
 10      CONTINUE
         IF( KSFN .GT. 1 ) KSET = KSFN * (-1)
      END IF
C
C      IF( KSET .EQ. 0 ) KSET = 1
C      IF( SFFREQ(1,KSET) .GT. 0.0 ) THEN
C         PBMFRQ = 30000.0 / SFFREQ(1,KSET)
C      ELSE
C         PBMFRQ = 0.0
C      END IF

C
C     Set the XY Axis Type and pointers
C
      XAXIS = PXYTYP(PXYBCK(1))
      KX    = PXYBCK(1)
      YAXIS = PXYTYP(PXYBCK(2))
      KY    = PXYBCK(2)
C
C     If all type of plot was selected, subdivide the window
C     else set value for individual plot
C
      IF( POPBCK .EQ. 5 ) THEN
         CALL PGSUBP ( 2, 2 )
      ELSE
         CALL PLAXVL( XAXIS, YAXIS, KX, KY, 
     1                RXMIN, RXMAX, RYMIN, RYMAX )
         CALL PGSUBP ( 1, 1 )
         CALL PGPANL ( 1, 1 )
      END IF
C
C     Plots UV
C
      IF( POPBCK .EQ. 1 .OR. POPBCK .EQ. 5 ) THEN
         IF( POPBCK .EQ. 5 ) THEN
            CALL PLAXVL( 'Km', 'Km', 12, 12, 
     1                   RXMIN, RXMAX, RYMIN, RYMAX )
            CALL PGPANL ( 1, 1 )
         END IF
C
CRCW     If there are multiple sources and we are only doing
C        a uv plot, make multiple windows, unless there are too many
C        First count the requested sources.  If there is more than one,
C        use the first window to list the stations and the second
C        to plot their locations if CONFIG is set.  Then in each 
C        uvplot window, don't list stations.  NSRCPLT will end up
C        being the total number of windows needed.
C
         NSRCPLT = 0
         DO ISRC = 1, NSRC
            IF( PSOBCK(ISRC) .EQ. 1 ) NSRCPLT = NSRCPLT + 1
         END DO
         IF( NSRCPLT .GT. 1 ) NSRCPLT = NSRCPLT + 1
         IF( CONFIG ) NSRCPLT = NSRCPLT + 1
         IF( NSRCPLT .LE. 1 .OR. NSRCPLT .GT. 25 .OR. 
     1       POPBCK .EQ. 5 ) THEN
C
C           This is the original version.  But I have changed the
C           meaning of KSRC in PLOTUV and eliminated JSRC from
C           the call.
C
            CALL PLOTUV( KX, KSET, KSRC, KSTA, SCREEN, COLOR,
     1                RXMAX, RXMIN, RYMIN, RYMAX )
         ELSE
            IF( NSRCPLT .LE. 4 ) THEN
               NPX = 2
               NPY = 2
            ELSE IF( NSRCPLT .LE. 6 ) THEN
               NPX = 3
               NPY = 2
            ELSE IF( NSRCPLT .LE. 9 ) THEN
               NPX = 3
               NPY = 3
            ELSE IF( NSRCPLT .LE. 12 ) THEN
               NPX = 4
               NPY = 3
            ELSE IF( NSRCPLT .LE. 16 ) THEN
               NPX = 4
               NPY = 4
            ELSE IF( NSRCPLT .LE. 20 ) THEN
               NPX = 5
               NPY = 4
            ELSE 
               NPX = 5
               NPY = 5
            END IF
C
C           Write the stations and the station map.
C           This is a modified PLOTUV.
C
C           Make the first call to PLOTSTA in such a way that
C           it will use the whole screen for the user interaction.
C
            CALL PGSAVE
C
            IF( SCREEN ) THEN
               CALL PGSUBP ( 1, 1 )
               CALL PLOTSTA( KSET, SCREEN, COLOR, .TRUE. )
            END IF
C
C           Make the non-interactive call to plot stations
C           and give the stations list.
C
            CALL PGSUBP ( NPX, NPY )
            CALL PLOTSTA( KSET, SCREEN, COLOR, .FALSE. )
            CALL PLUVNAM( KSET, SCREEN )
C
            CALL PGUNSA
C
C           Don't use PGPANL because PLOTUV will call PGPAGE which
C           will advance to the next panel.
C
            IPSRC = 0
            DO ISRC = 1, NSRC
               IF( PSOBCK(ISRC) .EQ. 1 ) THEN
                  IPSRC = IPSRC + 1
C                  IPX = MOD( IPSRC - 1, NPX ) + 1
C                  IPY = ( IPSRC - 1 ) / NPX + 1
C                  CALL PGPANL ( IPX, IPY )
                  CALL PLOTUV( KX, KSET, ISRC, KSTA, 
     1                SCREEN, COLOR, RXMAX, RXMIN, RYMIN, RYMAX )
               END IF
            END DO
         END IF
C
CRCW     End of mods for multiple UV plots.
C
      END IF
C
C     Plots XY
C
      IF( POPBCK .EQ. 2 .OR. POPBCK .EQ. 5 ) THEN
         IF( POPBCK .EQ. 5 ) THEN
            IF( PXYBCK(1) .GT. 7 ) THEN
               XAXIS = 'LST'
               KX    = 3
            END IF
            IF( PXYBCK(2) .LT. 4 .OR. PXYBCK(2) .GT. 7 ) THEN
               YAXIS = 'EL'
               KY    = 5
            END IF
            CALL PLAXVL( XAXIS, YAXIS, KX, KY, 
     1                   RXMIN, RXMAX, RYMIN, RYMAX )
            CALL PGPANL ( 1, 2 )
         END IF
         CALL PLOTXY( KSET, KSRC, KSTA, XAXIS, YAXIS,
     1                SCREEN, COLOR, RXMIN, RXMAX, RYMIN, RYMAX )
      END IF
C
C     Plots RD
C
      IF( POPBCK .EQ. 3 .OR. POPBCK .EQ. 5 ) THEN
         IF( POPBCK .EQ. 5 ) THEN
            CALL PLAXVL( 'RA', 'DEC', 10, 11, 
     1                   RXMIN, RXMAX, RYMIN, RYMAX )
            CALL PGPANL ( 2, 1 )
         END IF
         CALL PLOTRD( SCREEN, COLOR, RXMIN, RXMAX, RYMIN, RYMAX )
      END IF
C
C     Plots Uptime
C
      IF( POPBCK .EQ. 4 .OR. POPBCK .EQ. 5 ) THEN
         IF( POPBCK .EQ. 5 ) THEN
            YAXIS = 'Ant'
            IF( PXYBCK(1) .GT. 3 ) THEN
               XAXIS = 'LST'
               KX    = 3
            END IF
            CALL PLAXVL( XAXIS, YAXIS, KX, 9, 
     1                   RXMIN, RXMAX, RYMIN, RYMAX )
            CALL PGPANL ( 2, 2 )
         END IF
         CALL PLOTUP( KSET, XAXIS, YAXIS,  SCREEN, COLOR,
     1                RXMIN, RXMAX, RYMIN, RYMAX )
      END IF
C
C     Plots BM ( Beam )
C
      IF( POPBCK .EQ. 6 ) THEN
         CALL PLOTBM( KSET, KSTA, SCREEN, COLOR )
      END IF
C
C     If selected close the window else restore panel
C
      IF( WCL .EQ. 1) THEN
         CALL PGSLCT( WID )
         CALL PGCLOS
      ELSE
         CALL PGSUBP ( 1, 1 )
         CALL PGPANL ( 1, 1 )
      ENDIF
C
      RETURN
      END
