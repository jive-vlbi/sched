      SUBROUTINE PLINIT( STAT )
C
C     Routine for sched that initialize all default plotting
C     parameters.
C
C     Fix of rounding for km case, short baselines.  May 7, 2018 RCW
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER          I, J, K, STAT, PB, PGOPEN, LEN1, IERR
      REAL             XYSIZ, XYMAX, Y1, X1
C ----------------------------------------------------------------------
C
C     Set all DATA statements
C
C ----------------------------------------------------------------------
C     Menu Panel
C ----------------------------------------------------------------------
C
C     Button Value
C
      DATA PMNVAL / '  PLOT', ' CLOSE', ' FILES', '  AXIS', 'SOURCES',
     1              'OPTIONS', 'RESTART', ' FINISH', 'TERMINAL',
     2              '  EXIT' /
C
C     Buttons Policy: -1 = Immediate Action
C                      1 = Input Area
C
      DATA PMNACT / -1, -1, 1, 1, 1, 1, -1, -1, -1, -1 /
C
C -------------------------------------------------------------------
C     Stations Area Panel
C -------------------------------------------------------------------
C
      DATA PSTLAB / 'Set All', 'Unset All', 'Both stations selected',
     1              'Either station selected' /
C
C -------------------------------------------------------------------
C     Files Area Panel
C -------------------------------------------------------------------
C
C     Radio Button Value
C
      DATA PFLVAL / 'PS landscape', 'PS portrait',
     1              'Color PS land.', 'Color PS port.',
     2              'GIF landscape', 'GIF portrait' /
C
C     Action Button Value
C
      DATA PFLLAB / ' Save As', 'Save', 'Load' /
C
C     File Extension Value
C
      DATA PFLEXT / '.ps/PS',   '.ps/VPS',  '.ps/CPS',
     1              '.ps/VCPS', '.gif/GIF', '.gif/VGIF' /
C
C -------------------------------------------------------------------
C     Axis Area Panel
C -------------------------------------------------------------------
C
C     Radio Button Value
C
      DATA POPVAL / 'UV Plot', 'XY Plot', 'RD Plot', 'Uptime',
     1              'ALL Plot', 'Beam' /
C
C     Plot Types
C
      DATA POPTYP / 'UV', 'XY', 'RD', 'UP', 'AL', 'BM' /
C
C     XY Axis Type
C
      DATA PXYTYP / 'UT', 'GST', 'LST', 'AZ', 'EL', 'HA', 'PA',
     1              'Sec', 'Ant', 'RA', 'DEC', 'Km', 'Wv' /
C
C     Time Ofset Pointers
C
      DATA POFPOI /  1, -1, 2, 0, 0, 0 ,0 ,0, 0, 0, 0, 0, 0 /
C
C     XY Axis Data Types
C
      DATA PXSTYP / 'dHH MM SS',  'dHH MM SS', 'dHH MM SS', 
     1              ' DDD MM SS', ' DD MM SS', 'sHH MM SS',
     2              'sDDD MM SS', 'Value',       'Num',
     3              ' HH MM SS' , 'sDD MM SS', 'sKm', 'sWv' /
C
C     XY Axis Data Limits
C
      DATA PXSLIM /   0,  0,  0,   0,   0, -12, 
     1             -180,  0,  1,   0, -90, -100000, -10000000,
     2               24, 24, 24, 360,  90, +12,
     3             +180, 30, 22,  24,  90,  100000,  10000000 /
C
C     XY Axis Add/Subtract Pointers
C
      DATA PXSEXP / 0, 0, 0, 0 /
C
C -------------------------------------------------------------------
C     Option Area Panel
C -------------------------------------------------------------------
C
C     Background Types
C
      DATA PBGLAB / 'DISPLAY', 'B/W PRINTER', 'Color PRINTER' /
C
C     Catalog Names
C
      DATA PCTLAB / 'SUN', 'SCHED', 'VLA', 'USNO', 'MERLIN', 'VLBA' /
C
C     Action Button Value
C
      DATA PLYLAB / '  SAVE', '  LOAD', 'DEFAULTS' /
C
C -------------------------------------------------------------------
C     RDPlot Control Buttons
C -------------------------------------------------------------------
C
C     Action Button Value
C
      DATA PRDVAL / 'ZOOM',      ' ',           ' ',        ' ',
     1            'WINDOW',      'FZOOM',      ' ',        ' ',
     2            'CALIBRATORS', 'RADEC',       ' ',        ' ',
     3            'CATALOG ON',  'CATALOG OFF', 'AXIS OFF', 'AXIS ON',
     4            'LABEL ON',    'LABEL OFF',   ' ',        ' ',
     5            'SUN ON',      'SUN OFF',     ' ',        ' ',
     6            'RESET',       ' ',           ' ',        ' ',
     7            'DONE',        ' ',           ' ',        ' '  /
C
C     Active Button pointer
C
      DATA PRDACT / 1, 1, 1, 0, 0, 0, 0, 0 /
C
C     Inactive Button pointer ( Calibrators selected )
C
C      DATA PRDDIS / 0, 1, 0, 1, 0, 0, 0, 0 /
      DATA PRDDIS / 0, 0, 0, 0, 0, 0, 0, 0 /
C
C
C -------------------------------------------------------------------
C     Set Menu Panel Defaults and Dimensions
C -------------------------------------------------------------------
C
C     Set the Terminal Mode to false. The RESTART operation
C     is processed by the GUI
C
      PMTTYT = .FALSE.
C 
C     Set the active default selection area (PMNBCK)
C     The first member is the current selected area
C     the second is the previous selected area.
C     the new selected area is decorated only if different
C     to the previous.
C
      PMNBCK(1) = 5
      PMNBCK(2) = 0
C
C     Set the Aspect and characteristics off MENU Panel.
C
C
C     Menu     ( PPNDIM )  Array index 1 = Width
C                                      2 = Aspect Ratio
C                                      3 = Font Size
C
C     Options  ( PPSDIM )  Array index 1 = X bottom left corner
C                                      2 = X top right corner
C                                      3 = Y bottom left corner
C                                      4 = Y top right corner
C
C     Colors   ( PPNCOL )  Array index 1 = Background
C                                      2 = Foreground
C                                      3 = 3D Light Shadow
C                                      4 = 3D Dark  Shadow
C                                      5 = Menu Button Select bg
C                                      6 = Menu Button Select fg
C                                      7 = Select Button bg
C                                      8 = Radio  Button bg
C                                      9 = Select Button set
C                                     10 = Select Button reset
C
      PPNDIM(1) = 4.0
      PPNDIM(2) = 1.0
      PPNDIM(3) = 1.5
C
      PPSDIM(1) = 0.30
      PPSDIM(2) = 0.95
      PPSDIM(3) = 0.05
      PPSDIM(4) = 0.95
C
      PPNCOL(1)  = 0
      PPNCOL(2)  = 15
      PPNCOL(3)  = 1
      PPNCOL(4)  = 15
      PPNCOL(5)  = 11
      PPNCOL(6)  = 1
      PPNCOL(7)  = 4
      PPNCOL(8)  = 2
      PPNCOL(9)  = 3
      PPNCOL(10) = 2
C
C     Set Menu Buttons Dimension
C
      DO 5 I=PMNBTM,1,-1
         PMNBUT(1,I) = 0.0
         PMNBUT(2,I) = 0.20
         PMNBUT(3,I) = 0.0 + ((0.07 + 0.01) * (PMNBTM - I))
         PMNBUT(4,I) = PMNBUT(3,I) + 0.07
 5    CONTINUE
C
C -------------------------------------------------------------------
C     Set Stations Area Panel Defaults and Dimensions
C -------------------------------------------------------------------
C
C     Set Stations Select Buttons (PSTBCK):
C     To select   all plot      buttons ( 1 )
C     To unselect all highlight buttons ( 0 )
C
      DO 10 I=1,PSTMAX
         PSTBCK(I, 1) = 1
         PSTBCK(I, 2) = 0
 10   CONTINUE
C
C     Set Sources Select Buttons (PSOBCK):
C     To select   all plot      buttons ( 1 )
C
      DO 15 I=1,PSOMAX
         PSOBCK(I) = 1
 15   CONTINUE
C
C     Set the pointer to the first 22 antennas and
C     to the first 10 sources
C
      PSTCNT = 0
      PSOCNT = 0
C
C     If Antennas greater than max viewable set the scroll pointer to
C     true
C
      IF( NSTA .GT. PSTPLT ) THEN
         PSTSCR = .TRUE.
      ELSE
         PSTSCR = .FALSE.
      END IF
C
C     If Sources greater than max viewable set the scroll pointer to
C     true
C
      IF( NSRC .GT. PSOPLT ) THEN
         PSOSCR = .TRUE.
      ELSE
         PSOSCR = .FALSE.
      END IF
C
C     Set Max Y Axis value for UPTIME to number of antennas
C     and sources in schedule
C
      PSTNUM = NSTA
      PSONUM = NSRC
C
C     Set Stations Single Baseline (PSTBAS) to false (Either).
C     To plot only the baselines of the stations
C     selected turn it to true (Both).
C
      PSTBAS = .FALSE.
C
C     Set Station scroll bar
C
      PSTSBR(1) = PPSDIM(2) - 0.26
      PSTSBR(2) = PPSDIM(2)
      PSTSBR(3) = PPSDIM(4) - 0.01
      PSTSBR(4) = PSTSBR(3) + 0.035
C
C     Set Stations Selection Boxes Dimension
C
      XYSIZ = 0.03
      Y1 = PSTSBR(3) - 0.02
      K  = PSTPLT / 2
      DO 20 I=1,K
C
C        Left Plot Box
C
         PSTBXP(1,I) = PPSDIM(1)
         PSTBXP(2,I) = PSTBXP(1,I) + XYSIZ
         PSTBXP(3,I) = Y1 - (XYSIZ * I) - (0.01 * (I - 1))
         PSTBXP(4,I) = PSTBXP(3,I) + XYSIZ
C
C        Left Hilight Box
C
         PSTBXH(1,I) = PSTBXP(2,I) + 0.01
         PSTBXH(2,I) = PSTBXH(1,I) + XYSIZ
         PSTBXH(3,I) = PSTBXP(3,I)
         PSTBXH(4,I) = PSTBXP(4,I)
 20   CONTINUE
C
      J  = 0
      K = ( PSTPLT / 2 ) + 1
      DO 30 I=K,PSTPLT
         J = J + 1
C
C        Right Plot Box
C
         PSTBXP(1,I) = PPSDIM(1) + 0.35
         PSTBXP(2,I) = PSTBXP(1,I) + XYSIZ
         PSTBXP(3,I) = Y1 - (XYSIZ * J) - (0.01 * (J - 1))
         PSTBXP(4,I) = PSTBXP(3,I) + XYSIZ
C
C        Right Hilight Box
C
         PSTBXH(1,I) = PSTBXP(2,I) + 0.01
         PSTBXH(2,I) = PSTBXH(1,I) + XYSIZ
         PSTBXH(3,I) = PSTBXP(3,I)
         PSTBXH(4,I) = PSTBXP(4,I)
 30   CONTINUE
C
C     Left Select All Plot Box
C
      K  = PSTPLT / 2
      PSTSUN(1,1) = PPSDIM(1)
      PSTSUN(2,1) = PSTSUN(1,1) + XYSIZ
      PSTSUN(3,1) = PSTBXP(3,K) - XYSIZ - 0.02
      PSTSUN(4,1) = PSTSUN(3,1) + XYSIZ
C
C     Left Select All Hilight Box
C
      PSTSUN(1,2) = PSTSUN(2,1) + 0.01
      PSTSUN(2,2) = PSTSUN(1,2) + XYSIZ
      PSTSUN(3,2) = PSTSUN(3,1)
      PSTSUN(4,2) = PSTSUN(4,1)
C
C     Right Unselect All Plot Box
C
      PSTSUN(1,3) = PPSDIM(1) + 0.35
      PSTSUN(2,3) = PSTSUN(1,3) + XYSIZ
      PSTSUN(3,3) = PSTSUN(3,1)
      PSTSUN(4,3) = PSTSUN(4,1)
C
C     Right Unselect All Hilight Box
C
      PSTSUN(1,4) = PSTSUN(2,3) + 0.01
      PSTSUN(2,4) = PSTSUN(1,4) + XYSIZ
      PSTSUN(3,4) = PSTSUN(3,1)
      PSTSUN(4,4) = PSTSUN(4,1)
C
C     Set Baseline Selection Boxe Dimension
C
      PSTBXB(1,1) = PPSDIM(1) + 0.2
      PSTBXB(2,1) = PSTBXB(1,1) + XYSIZ
      PSTBXB(3,1) = PSTSUN(3,1) - 0.07
      PSTBXB(4,1) = PSTBXB(3,1) + XYSIZ
C
      PSTBXB(1,2) = PSTBXB(1,1)
      PSTBXB(2,2) = PSTBXB(2,1)
      PSTBXB(3,2) = PSTBXB(3,1) - 0.04
      PSTBXB(4,2) = PSTBXB(3,2) + XYSIZ
C
C     Set Sources scroll bar
C
      PSOSBR(1) = PPSDIM(2) - 0.30
      PSOSBR(2) = PPSDIM(2)
      PSOSBR(3) = PSTBXB(3,2) - 0.08
      PSOSBR(4) = PSOSBR(3) + 0.035
C
C     Set Sources Selection Boxes Dimension
C
      Y1 = PSOSBR(3) - 0.035
      XYSIZ = 0.03
      K = PSOPLT / 2
C
      DO 50 I=1,K
C
C        Left Plot Box
C
         PSOBXP(1,I) = PPSDIM(1)
         PSOBXP(2,I) = PSOBXP(1,I) + XYSIZ
         PSOBXP(3,I) = Y1 - (XYSIZ * I) - (0.01 * (I - 1))
         PSOBXP(4,I) = PSOBXP(3,I) + XYSIZ
C
C        Right Plot Box
C
         J = I + K
         PSOBXP(1,J) = PPSDIM(1) + 0.35
         PSOBXP(2,J) = PSOBXP(1,J) + XYSIZ
         PSOBXP(3,J) = PSOBXP(3,I)
         PSOBXP(4,J) = PSOBXP(4,I)
 50   CONTINUE
C
C     Select All Sources Plot Box
C
      PSOSUN(1,1) = PSOBXP(1,5)
      PSOSUN(2,1) = PSOBXP(2,5)
      PSOSUN(3,1) = PSOBXP(3,5) - 0.05
      PSOSUN(4,1) = PSOSUN(3,1) + XYSIZ
C
C     Unselect All Sources Plot Box
C
      PSOSUN(1,2) = PSOBXP(1,10)
      PSOSUN(2,2) = PSOBXP(2,10)
      PSOSUN(3,2) = PSOSUN(3,1)
      PSOSUN(4,2) = PSOSUN(4,1)
C
C -------------------------------------------------------------------
C     Set Axis Area Panel Defaults and Dimensions
C -------------------------------------------------------------------
C
C     Set the type of default plot (UV)
C
      POPBCK = 1
      PXYBIG = .TRUE.
      PXYONE = .FALSE.
      PYANTN = .FALSE.
C
C     Set Sec(z) plot to automatic limits calculation
C
      PXYSEC = .TRUE.
C
C     Set Wavelength scale to Kilo lamda
C
      PXYWLE = 3
C
C     Set XY Axis Values Exponent
C
      PXYEXP = 4
C
C     Set Plot Type Radio Boxes Dimensions
C
      XYSIZ = 0.035
      PB = POPBXM / 2
      Y1 = PPSDIM(4) - 0.03
      I = 0
      DO 62 J=1,2
         DO 60 K=1,PB
            I = I + 1
            POPBOX(1,I) = PPSDIM(1) + ((J - 1) * 0.35)
            POPBOX(2,I) = POPBOX(1,I) + XYSIZ
            POPBOX(3,I) = Y1 - (XYSIZ * K) - (0.01 * (K - 1))
            POPBOX(4,I) = POPBOX(3,I) + XYSIZ
 60      CONTINUE
         IF ( (PB * 2) .LT. POPBXM ) PB = PB + 1
 62   CONTINUE
C
C     If The Boxes is not pair shift to te left the last box
C
      IF ( (PB * 2) .GT. POPBXM ) THEN
          POPBOX(1,POPBXM) = PPSDIM(1)
          POPBOX(2,POPBXM) = POPBOX(1,POPBXM) + XYSIZ
      END IF
C
C     Set the Axis Type Selection for the different
C     Plot Types
C
C     UV Xaxis to Km
C
      POPLIM(1,1) = 12
      POPLIM(1,2) = 13
      POPLIM(1,3) = 12
      POPLIM(1,4) = 13
C
C     XY Xaxis from UT to PA and Yaxis from AZ to PA
C
      POPLIM(2,1) = 1
      POPLIM(2,2) = 8
      POPLIM(2,3) = 4
      POPLIM(2,4) = 8
C
C     RD Xaxis to RA and Yaxis to DEC
C
      POPLIM(3,1) = 10
      POPLIM(3,2) = 10
      POPLIM(3,3) = 11
      POPLIM(3,4) = 11
C
C     UPT Xaxis from UT to LST and Yaxis to Ant
C
      POPLIM(4,1) = 1
      POPLIM(4,2) = 3
      POPLIM(4,3) = 9
      POPLIM(4,4) = 9
C
C     ALL not defined and set to all
C
      POPLIM(5,1) = 1
      POPLIM(5,2) = 13
      POPLIM(5,3) = 1
      POPLIM(5,4) = 13
C
C     BM XYaxis to Km ( dummy )
C
      POPLIM(6,1) = 12
      POPLIM(6,2) = 12
      POPLIM(6,3) = 12
      POPLIM(6,4) = 12
C
C     Set the default Axis Type to Km and big input area
C     to true
C
      PXYBCK(1) = 12
      PXYBCK(2) = 12
      PXYBIG    = .TRUE.
C
C     Set the default Local Time Offset
C
      POFVAL(1) =  0
      POFVAL(2) =  0
C
C     Set X Axis Type Text Areas
C
      Y1 = POPBOX(3,POPBXM) - 0.16 - 0.01
      PXYTXT(1,1) = PPSDIM(1) + 0.05
      PXYTXT(2,1) = PXYTXT(1,1) + 0.1
      PXYTXT(3,1) = Y1 
      PXYTXT(4,1) = PXYTXT(3,1) + 0.07
C
C     Set Scroll Bar Dimensions
C
      PXYSBR(1,1) = PXYTXT(2,1) + 0.01
      PXYSBR(2,1) = PXYSBR(1,1) + 0.035
      PXYSBR(3,1) = PXYTXT(3,1)
      PXYSBR(4,1) = PXYTXT(4,1)
C
C     Set Time Offset Text Areas
C
      PXYTXT(1,3) = PXYSBR(2,1) + 0.01
      PXYTXT(2,3) = PXYTXT(1,3) + 0.1
      PXYTXT(3,3) = PXYTXT(3,1)
      PXYTXT(4,3) = PXYTXT(4,1)
C
C     Set Antenna Offset Text Areas
C
      PXYTXT(1,4) = PXYTXT(1,3)
      PXYTXT(2,4) = PXYTXT(1,4) + 0.2
      PXYTXT(3,4) = PXYTXT(3,1)
      PXYTXT(4,4) = PXYTXT(4,1)
C
C     Set  Wavelength scale Text Areas
C
      PXYTXT(1,5) = PXYTXT(1,4)
      PXYTXT(2,5) = PXYTXT(1,5) + 0.08
      PXYTXT(3,5) = PXYTXT(3,1)
      PXYTXT(4,5) = PXYTXT(4,1)
C
C     Set Y Axis Type Text Areas
C
      PXYTXT(1,2) = PPSDIM(1) + 0.5
      PXYTXT(2,2) = PXYTXT(1,2) + 0.1
      PXYTXT(3,2) = Y1
      PXYTXT(4,2) = Y1 + 0.07
C
C     Set Scroll Bar Dimensions
C
      PXYSBR(1,2) = PXYTXT(2,2) + 0.01
      PXYSBR(2,2) = PXYSBR(1,2) + 0.035
      PXYSBR(3,2) = PXYTXT(3,2)
      PXYSBR(4,2) = PXYTXT(4,2)
C
C     Set X Axis Min/Max Scale Area Dimensions,
C     Sign/Days Area, Exponent and Default Value Buttons
C
      Y1    = PXYTXT(3,2) - 0.16 - 0.01
      X1    = PPSDIM(2) - 0.035 - (0.01 * 3) - 0.1 - (0.08 * 2)
      XYSIZ = 0.1
      DO 70 I=1,3
         PXLTXT(1,I) = X1
         PXLTXT(2,I) = PXLTXT(1,I) + XYSIZ
         PXLTXT(3,I) = Y1
         PXLTXT(4,I) = Y1 + 0.07
         X1 = PXLTXT(2,I) + 0.01
         XYSIZ = 0.08
 70   CONTINUE
C
      PSGTXT(1,1) = PXLTXT(1,1) - 0.01 - 0.05
      PSGTXT(2,1) = PSGTXT(1,1) + 0.05
      PSGTXT(3,1) = PXLTXT(3,1)
      PSGTXT(4,1) = PXLTXT(4,1)
C
      XYSIZ = 0.035
      PXSBXM(1,1) = PXLTXT(2,3) + 0.01
      PXSBXM(2,1) = PXSBXM(1,1) + XYSIZ
      PXSBXM(3,1) = PXLTXT(4,3) - XYSIZ
      PXSBXM(4,1) = PXLTXT(4,3)
C
      PXSDEF(1,1) = PXSBXM(1,1)
      PXSDEF(2,1) = PXSBXM(2,1)
      PXSDEF(3,1) = PXLTXT(3,3)
      PXSDEF(4,1) = PXSDEF(3,1) + XYSIZ
C
      Y1    = PXLTXT(3,1) - 0.07 - 0.02
      DO 72 I=1,3
         PXRTXT(1,I) = PXLTXT(1,I)
         PXRTXT(2,I) = PXLTXT(2,I)
         PXRTXT(3,I) = Y1
         PXRTXT(4,I) = Y1 + 0.07
 72   CONTINUE
C
      PSGTXT(1,2) = PSGTXT(1,1)
      PSGTXT(2,2) = PSGTXT(2,1)
      PSGTXT(3,2) = PXRTXT(3,1)
      PSGTXT(4,2) = PXRTXT(4,1)
C
      XYSIZ = 0.035
      PXSBXM(1,2) = PXSBXM(1,1)
      PXSBXM(2,2) = PXSBXM(2,1)
      PXSBXM(3,2) = PXRTXT(4,3) - XYSIZ
      PXSBXM(4,2) = PXRTXT(4,3)
C
      PXSDEF(1,2) = PXSBXM(1,1)
      PXSDEF(2,2) = PXSBXM(2,1)
      PXSDEF(3,2) = PXRTXT(3,3)
      PXSDEF(4,2) = PXSDEF(3,2) + XYSIZ
C
C     Set Y Axis Bottom/Top Scale Area Dimensions
C     and ExponentButton
C
      Y1    = PXRTXT(3,1) - 0.16 - 0.01
      DO 74 I=1,3
         PYBTXT(1,I) = PXLTXT(1,I)
         PYBTXT(2,I) = PXLTXT(2,I)
         PYBTXT(3,I) = Y1
         PYBTXT(4,I) = Y1 + 0.07
 74   CONTINUE
C
      PSGTXT(1,3) = PSGTXT(1,1)
      PSGTXT(2,3) = PSGTXT(2,1)
      PSGTXT(3,3) = PYBTXT(3,1)
      PSGTXT(4,3) = PYBTXT(4,1)
C
      XYSIZ = 0.035
      PXSBXM(1,3) = PXSBXM(1,1)
      PXSBXM(2,3) = PXSBXM(2,1)
      PXSBXM(3,3) = PYBTXT(4,3) - XYSIZ
      PXSBXM(4,3) = PYBTXT(4,3)
C
      PXSDEF(1,3) = PXSBXM(1,1)
      PXSDEF(2,3) = PXSBXM(2,1)
      PXSDEF(3,3) = PYBTXT(3,3)
      PXSDEF(4,3) = PXSDEF(3,3) + XYSIZ
C
      Y1    = PYBTXT(3,1) - 0.07 - 0.02
      DO 76 I=1,3
         PYTTXT(1,I) = PXLTXT(1,I)
         PYTTXT(2,I) = PXLTXT(2,I)
         PYTTXT(3,I) = Y1
         PYTTXT(4,I) = Y1 + 0.07
 76   CONTINUE
C
      PSGTXT(1,4) = PSGTXT(1,1)
      PSGTXT(2,4) = PSGTXT(2,1)
      PSGTXT(3,4) = PYTTXT(3,1)
      PSGTXT(4,4) = PYTTXT(4,1)
C
      XYSIZ = 0.035
      PXSBXM(1,4) = PXSBXM(1,1)
      PXSBXM(2,4) = PXSBXM(2,1)
      PXSBXM(3,4) = PYTTXT(4,3) - XYSIZ
      PXSBXM(4,4) = PYTTXT(4,3)
C
      PXSDEF(1,4) = PXSBXM(1,1)
      PXSDEF(2,4) = PXSBXM(2,1)
      PXSDEF(3,4) = PYTTXT(3,3)
      PXSDEF(4,4) = PXSDEF(3,4) + XYSIZ
C
C     Set Lock sign/value Selection Box 
C
      PLOSGN = .FALSE.
      PLOVAL = .TRUE.
C
      XYSIZ = 0.035
      PLOBOX(1,1) = PPSDIM(1)
      PLOBOX(2,1) = PLOBOX(1,1) + XYSIZ
      PLOBOX(3,1) = PPSDIM(3) - 0.03
      PLOBOX(4,1) = PLOBOX(3,1) + XYSIZ
C
      PLOBOX(1,2) = PPSDIM(1) + 0.35
      PLOBOX(2,2) = PLOBOX(1,2) + XYSIZ
      PLOBOX(3,2) = PLOBOX(3,1)
      PLOBOX(4,2) = PLOBOX(4,1)
C
C
C     Set Sun Defaults and Selection Box 
C
      PSUNEL = OPMINEL(1)
      PXYSUN = .FALSE.
C
      XYSIZ = 0.035
      PSUBOX(1) = PPSDIM(1)
      PSUBOX(2) = PSUBOX(1) + XYSIZ
      PSUBOX(3) = PPSDIM(3) - 0.03
      PSUBOX(4) = PSUBOX(3) + XYSIZ
C
C     Set Sun selector for Elevation sign 
C
      PSUSGN(1) = PPSDIM(2) - 0.035 - (0.01 * 2) - 0.08 - 0.05 
      PSUSGN(2) = PSUSGN(1) + 0.05
      PSUSGN(3) = PPSDIM(3) - 0.03
      PSUSGN(4) = PSUSGN(3) + 0.07
C
C     Set Sun selector for Elevation value 
C
      PSUTXT(1) = PSUSGN(2) + 0.01
      PSUTXT(2) = PSUTXT(1) + 0.08
      PSUTXT(3) = PSUSGN(3) 
      PSUTXT(4) = PSUSGN(4)
C
C     Set Sun Controls for Elevation value selector
C
      PSUBXM(1) = PSUTXT(2) + 0.01
      PSUBXM(2) = PSUBXM(1) + XYSIZ
      PSUBXM(3) = PSUTXT(4) - XYSIZ
      PSUBXM(4) = PSUTXT(4)
C
      PSUDEF(1) = PSUBXM(1)
      PSUDEF(2) = PSUBXM(2)
      PSUDEF(3) = PSUTXT(3)
      PSUDEF(4) = PSUDEF(3) + XYSIZ
C
C     Set Beam Text Areas for Frequency
C
      Y1 = POPBOX(3,POPBXM) - 0.17
      PBMTXT(1,1) = PPSDIM(1) + 0.46
      PBMTXT(2,1) = PBMTXT(1,1) + 0.14
      PBMTXT(3,1) = Y1
      PBMTXT(4,1) = Y1 + 0.07
C
C     Set Scroll Bar Dimensions
C
      PBMSBR(1,1) = PBMTXT(2,1) + 0.01
      PBMSBR(2,1) = PBMSBR(1,1) + 0.035
      PBMSBR(3,1) = PBMTXT(3,1)
      PBMSBR(4,1) = PBMTXT(4,1)
C
C     Set Beam Text Areas for Pixels
C
      Y1 = Y1 - 0.09
      PBMTXT(1,2) = PPSDIM(1) + 0.5 
      PBMTXT(2,2) = PBMTXT(1,2) + 0.1
      PBMTXT(3,2) = Y1
      PBMTXT(4,2) = Y1 + 0.07
C
C     Set Scroll Bar Dimensions
C
      PBMSBR(1,2) = PBMSBR(1,1)
      PBMSBR(2,2) = PBMSBR(2,1)
      PBMSBR(3,2) = PBMTXT(3,2)
      PBMSBR(4,2) = PBMTXT(4,2)
C
C     Set Beam Text Areas for Pixels Size
C
      Y1 = Y1 - 0.09
      PBMTXT(1,3) = PBMTXT(1,2) 
      PBMTXT(2,3) = PBMTXT(2,2)
      PBMTXT(3,3) = Y1
      PBMTXT(4,3) = Y1 + 0.07
C
C     Set Scroll Bar Dimensions
C
      PBMSBR(1,3) = PBMSBR(1,1)
      PBMSBR(2,3) = PBMSBR(2,1)
      PBMSBR(3,3) = PBMTXT(3,3)
      PBMSBR(4,3) = PBMTXT(4,3)
C
C     Set Beam Text Areas for Weight
C
      Y1 = Y1 - 0.09
      PBMTXT(1,4) = PBMTXT(1,2) - 0.1
      PBMTXT(2,4) = PBMTXT(2,2)
      PBMTXT(3,4) = Y1
      PBMTXT(4,4) = Y1 + 0.07
C
C     Set Scroll Bar Dimensions
C
      PBMSBR(1,4) = PBMSBR(1,1)
      PBMSBR(2,4) = PBMSBR(2,1)
      PBMSBR(3,4) = PBMTXT(3,4)
      PBMSBR(4,4) = PBMTXT(4,4)
C
C     Set Beam Text Areas for Image Function
C
      Y1 = Y1 - 0.09
      PBMTXT(1,5) = PBMTXT(1,2) 
      PBMTXT(2,5) = PBMTXT(2,2)
      PBMTXT(3,5) = Y1
      PBMTXT(4,5) = Y1 + 0.07
C
C     Set Scroll Bar Dimensions
C
      PBMSBR(1,5) = PBMSBR(1,1)
      PBMSBR(2,5) = PBMSBR(2,1)
      PBMSBR(3,5) = PBMTXT(3,5)
      PBMSBR(4,5) = PBMTXT(4,5)
C
C     Set Beam Text Areas for Image Palette
C
      Y1 = Y1 - 0.09
      PBMTXT(1,6) = PBMTXT(1,2) - 0.1 
      PBMTXT(2,6) = PBMTXT(2,2)
      PBMTXT(3,6) = Y1
      PBMTXT(4,6) = Y1 + 0.07
C
C     Set Scroll Bar Dimensions
C
      PBMSBR(1,6) = PBMSBR(1,1)
      PBMSBR(2,6) = PBMSBR(2,1)
      PBMSBR(3,6) = PBMTXT(3,6)
      PBMSBR(4,6) = PBMTXT(4,6)
C
C     Set Beam Text Areas for Image Palette
C
      Y1 = Y1 - 0.09
      PBMTXT(1,7) = PBMTXT(1,2) 
      PBMTXT(2,7) = PBMTXT(2,2)
      PBMTXT(3,7) = Y1
      PBMTXT(4,7) = Y1 + 0.07
C
C     Set Scroll Bar Dimensions
C
      PBMSBR(1,7) = PBMSBR(1,1)
      PBMSBR(2,7) = PBMSBR(2,1)
      PBMSBR(3,7) = PBMTXT(3,7)
      PBMSBR(4,7) = PBMTXT(4,7)
C
C     Set default for Beam
C
      PBMPIX = 64
      PBMCEL = 1
      PBMCTF = 0
      PBMPAL = 0
      PBMWGT = 0
      PBMEXE = .TRUE.
      PBMCON = .FALSE.
C
C     Set default frequency to the first setup file
C
      IF( SFFREQ(1,1) .GT. 0.0 ) THEN
         PBMFRQ = 30000.0 / SFFREQ(1,1)
      ELSE
         PBMFRQ = 0.0
      END IF
C
C     Set Schedule Axis Value for KM Axis Type
C
      CALL MAXBAS( XYSIZ )
C
C     Round to major closest power of ten value
C     Fixed for values under 1000  May 7, 2018  RCW
C
      XYMAX = 0.0
      DO 78 I = 3, 0, -1
        IF( ( XYSIZ / 10**I ) .GT. 0.0 ) THEN
           IF( MOD( XYSIZ, 10.0**I ) .GT. 0.0 ) THEN
              J = ( XYSIZ + 10.0**I ) / 10.0**I
              XYMAX = J * 10.0**I
           END IF
        END IF
 78   CONTINUE
      XYSIZ = XYMAX
C
      PKMVAL(1) = -XYSIZ
      PKMVAL(2) = XYSIZ
      PKMVAL(3) = -XYSIZ
      PKMVAL(4) = XYSIZ
C
C     Limits in Kilo Lamda
C
      PWLVAL(1) = PKMVAL(1) * 10**2
      PWLVAL(2) = PKMVAL(2) * 10**2
      PWLVAL(3) = PKMVAL(3) * 10**2
      PWLVAL(4) = PKMVAL(4) * 10**2
C
C     Set Schedule Axis Value for UT Axis Type
C
      CALL PLTIME( 'UT', 0.D0, TFIRST, TEND, TWOPI, RADHR,
     1              PUTVAL, PANDAY, PADAYS )
C
C     Set Schedule Axis Value for GST Axis Type
C
      CALL PLTIME( 'GST', 0.D0, TFIRST, TEND, TWOPI, RADHR,
     1             PSGVAL, PANDAY, PADAYS )
C
C     Set Schedule Axis Value for LST Axis Type
C
      CALL PLTIME( 'LST', 0.D0, TFIRST, TEND, TWOPI, RADHR,
     1             PLSVAL, PANDAY, PADAYS )
C
C     Set all defaults value for axis type
C
      CALL PLAXIN

C -------------------------------------------------------------------
C     Set Files Area Panel Defaults and Dimensions
C -------------------------------------------------------------------
C
C     Set the default save image button (PFLBCK)
C     to PS B/W
C
      PFLBCK = 1
C
C     Set the default save image PGPLOT Device Name and
C     plain file name to UV type and PS file
C
      I = LEN1(EXPCODE)
      PFLFIL(1) = EXPCODE(1:I)//POPTYP(POPBCK)//PFLEXT(PFLBCK)
      J = INDEX( PFLEXT(PFLBCK), '/') - 1
      PFLFIL(2) = EXPCODE(1:I)//POPTYP(POPBCK)//PFLEXT(PFLBCK)(1:J)
C
C     Set the default save and load parameter File Name
C     to EXPCODE with extension .par
C
      PFLFIL(3) = EXPCODE(1:I)//'.par'
C
C     Set Radio Boxes and Action Button Dimensions for
C     Select Image Format
C
      XYSIZ = 0.04
      Y1 = PPSDIM(4) - 0.03
      DO 80 I=1,PFLBXM
         PFLBOX(1,I) = PPSDIM(1)
         PFLBOX(2,I) = PFLBOX(1,I) + XYSIZ
         PFLBOX(3,I) = Y1 - (XYSIZ * I) - (0.01 * (I - 1))
         PFLBOX(4,I) = PFLBOX(3,I) + XYSIZ
 80    CONTINUE
C
C     Set Save Image Buttons and Area Text Dimensions
C
      PFLBUT(1,1) = PPSDIM(1)
      PFLBUT(2,1) = PFLBUT(1,1) + 0.20
      PFLBUT(3,1) = PFLBOX(3,PFLBXM) - 0.02 - 0.07
      PFLBUT(4,1) = PFLBUT(3,1) + 0.07
C
      PFLTXT(1,1) = PFLBUT(2,1) + 0.01
      PFLTXT(2,1) = PPSDIM(2)
      PFLTXT(3,1) = PFLBUT(3,1)
      PFLTXT(4,1) = PFLBUT(4,1)
C
C     Set Save/Load Inputs Buttons and Area Text Dimensions
C
      PFLBUT(1,2) = PPSDIM(1)
      PFLBUT(2,2) = PFLBUT(1,2) + 0.12
      PFLBUT(3,2) = PFLBUT(3,1) - 0.07 - 0.1
      PFLBUT(4,2) = PFLBUT(3,2) + 0.07
C
      PFLTXT(1,2) = PFLBUT(2,2) + 0.01
      PFLTXT(2,2) = PPSDIM(2) - 0.12 - 0.01
      PFLTXT(3,2) = PFLBUT(3,2)
      PFLTXT(4,2) = PFLBUT(4,2)
C
      PFLBUT(1,3) = PFLTXT(2,2) +  0.01
      PFLBUT(2,3) = PFLBUT(1,3) + 0.11
      PFLBUT(3,3) = PFLBUT(3,2)
      PFLBUT(4,3) = PFLBUT(4,2)
C
C     Set Setup Files Select box (PSFBCK)
C     to all selected and fill the pointer array.
C
      PSFBCK = 1
      DO 82 I=1,NSETF
         PSFPOI(I) = PSFBCK
82    CONTINUE 
C
C     Set Setup Files Select box group (PSFCNT)
C     to the first group of five Files.
C
      PSFCNT = 1
C
C     Set Setup Files Selection Boxes Dimension
C
      XYSIZ = 0.035
      Y1 = PFLBUT(3,3) - 0.12 - 0.01
      DO 90 I=1,5
         PSFBXP(1,I) = PPSDIM(2) - (0.035 * 4)
         PSFBXP(2,I) = PSFBXP(1,I) + XYSIZ
         PSFBXP(3,I) = Y1 - (XYSIZ * I) - (0.01 * (I - 1))
         PSFBXP(4,I) = PSFBXP(3,I) + XYSIZ
 90   CONTINUE
C
C     Set Setup Files Text Scroll Area Dimension
C
      PSFTXT(1,2) = PPSDIM(1)
      PSFTXT(2,2) = PPSDIM(2) - 0.035
      PSFTXT(3,2) = PSFBXP(3,5) - 0.01
      PSFTXT(4,2) = PSFBXP(4,1) + 0.01
C
C     Set Scroll Bar Dimensions
C
      PSFSBR(1) = PSFTXT(2,2) + 0.01
      PSFSBR(2) = PSFSBR(1) + 0.035
      PSFSBR(3) = PSFTXT(3,2)
      PSFSBR(4) = PSFTXT(4,2)
C
C     Set Setup Files ALL Boxe Dimension
C
      PSFBXP(1,6) = PPSDIM(2) - (0.035 * 4)
      PSFBXP(2,6) = PSFBXP(1,6) + XYSIZ
      PSFBXP(3,6) = PSFTXT(4,2) + (0.01 * 2)
      PSFBXP(4,6) = PSFBXP(3,I) + XYSIZ
C
C     Set Setup Files ALL Text Area Dimension
C
      PSFTXT(1,1) = PPSDIM(1)
      PSFTXT(2,1) = PPSDIM(2) - 0.035
      PSFTXT(3,1) = PSFBXP(3,6) - 0.01
      PSFTXT(4,1) = PSFBXP(4,6) + 0.01
C
C -------------------------------------------------------------------
C     Set Options Area Panel Defaults and Dimensions
C -------------------------------------------------------------------
C
C     Try to read default layout from file ~/.schedrc
C     If fail set to default.
C
      IERR = 0
      CALL PLOPIO( 1, IERR )
      IF( IERR .GT. 0 ) CALL PLOPIO( 0, IERR )
C
C     Default Background to DISPLAY
C     Default Catalog to SCHED
C
      PBGCK = 1
      PCTCK = 2
C
C     Set Display Line Width Text Areas
C
      Y1 = PPSDIM(4) - 0.12
      PLWTXT(1,1) = PPSDIM(1) + 0.15
      PLWTXT(2,1) = PLWTXT(1,1) + 0.1
      PLWTXT(3,1) = Y1 
      PLWTXT(4,1) = PLWTXT(3,1) + 0.07
C
C     Set Scroll Bar Dimensions
C
      PLWSBR(1,1) = PLWTXT(2,1) + 0.01
      PLWSBR(2,1) = PLWSBR(1,1) + 0.035
      PLWSBR(3,1) = PLWTXT(3,1)
      PLWSBR(4,1) = PLWTXT(4,1)
C
C     Set Printer Line Width Text Areas
C
      PLWTXT(1,2) = PPSDIM(1) + 0.5
      PLWTXT(2,2) = PLWTXT(1,2) + 0.1
      PLWTXT(3,2) = Y1 
      PLWTXT(4,2) = PLWTXT(3,2) + 0.07
C
C     Set Scroll Bar Dimensions
C
      PLWSBR(1,2) = PLWTXT(2,2) + 0.01
      PLWSBR(2,2) = PLWSBR(1,2) + 0.035
      PLWSBR(3,2) = PLWTXT(3,2)
      PLWSBR(4,2) = PLWTXT(4,2)
C
C     Set Display Axis and Label Width Text Areas
C
      Y1 = PLWTXT(3,1) - 0.2
      PAWTXT(1,1) = PPSDIM(1) + 0.15
      PAWTXT(2,1) = PAWTXT(1,1) + 0.1
      PAWTXT(3,1) = Y1 
      PAWTXT(4,1) = PAWTXT(3,1) + 0.07
C
C     Set Scroll Bar Dimensions
C
      PAWSBR(1,1) = PAWTXT(2,1) + 0.01
      PAWSBR(2,1) = PAWSBR(1,1) + 0.035
      PAWSBR(3,1) = PAWTXT(3,1)
      PAWSBR(4,1) = PAWTXT(4,1)
C
C     Set Printer Axis and Label Width Text Areas
C
      PAWTXT(1,2) = PPSDIM(1) + 0.5
      PAWTXT(2,2) = PAWTXT(1,2) + 0.1
      PAWTXT(3,2) = Y1 
      PAWTXT(4,2) = PAWTXT(3,2) + 0.07
C
C     Set Scroll Bar Dimensions
C
      PAWSBR(1,2) = PAWTXT(2,2) + 0.01
      PAWSBR(2,2) = PAWSBR(1,2) + 0.035
      PAWSBR(3,2) = PAWTXT(3,2)
      PAWSBR(4,2) = PAWTXT(4,2)
C
C     Set Background Selection Areas
C
      Y1 = PAWTXT(3,1) - 0.2
      PBGTXT(1,1) = PPSDIM(1)
      PBGTXT(2,1) = PBGTXT(1,1) + 0.35
      PBGTXT(3,1) = Y1 
      PBGTXT(4,1) = PBGTXT(3,1) + 0.07
C
C     Set Scroll Bar Dimensions
C
      PBGSBR(1,1) = PBGTXT(2,1) + 0.01
      PBGSBR(2,1) = PBGSBR(1,1) + 0.035
      PBGSBR(3,1) = PBGTXT(3,1)
      PBGSBR(4,1) = PBGTXT(4,1)
C
C     Set Background Color Areas
C
      PBGTXT(1,2) = PPSDIM(1) + 0.5
      PBGTXT(2,2) = PBGTXT(1,2) + 0.1
      PBGTXT(3,2) = PBGTXT(3,1) 
      PBGTXT(4,2) = PBGTXT(4,1)
C
C     Set Scroll Bar Dimensions
C
      PBGSBR(1,2) = PBGTXT(2,2) + 0.01
      PBGSBR(2,2) = PBGSBR(1,2) + 0.035
      PBGSBR(3,2) = PBGSBR(3,1) 
      PBGSBR(4,2) = PBGSBR(4,1)
C
C     Set Catalog symbols and color selections
C
      Y1 = PBGTXT(3,1) - 0.2
      PCTTXT(1,1) = PPSDIM(1)
      PCTTXT(2,1) = PCTTXT(1,1) + 0.35
      PCTTXT(3,1) = Y1 
      PCTTXT(4,1) = PCTTXT(3,1) + 0.07
C
C     Set Scroll Bar Dimensions
C
      PCTSBR(1,1) = PCTTXT(2,1) + 0.01
      PCTSBR(2,1) = PCTSBR(1,1) + 0.035
      PCTSBR(3,1) = PCTTXT(3,1)
      PCTSBR(4,1) = PCTTXT(4,1)
C
C     Set Catalog symbols and color selections
C
      PCTTXT(1,2) = PPSDIM(1) + 0.5
      PCTTXT(2,2) = PCTTXT(1,2) + 0.1
      PCTTXT(3,2) = PCTTXT(3,1) 
      PCTTXT(4,2) = PCTTXT(4,1)
C
C     Set Scroll Bar Dimensions for symbols
C
      PCTSBR(1,2) = PCTTXT(1,2) - 0.01 - 0.035
      PCTSBR(2,2) = PCTSBR(1,2) + 0.035
      PCTSBR(3,2) = PCTTXT(3,2)
      PCTSBR(4,2) = PCTTXT(4,2)
C
C     Set Scroll Bar Dimensions for colors
C
      PCTSBR(1,3) = PCTTXT(2,2) + 0.01
      PCTSBR(2,3) = PCTSBR(1,3) + 0.035
      PCTSBR(3,3) = PCTTXT(3,2)
      PCTSBR(4,3) = PCTTXT(4,2)
C
C     Set Operational Buttons Dimension
C
      DO 100 I = 1, 3
         PLYBUT(1,I) = PPSDIM(1) + ( (0.20 + 0.02) * (I - 1) )
         PLYBUT(2,I) = PLYBUT(1,I) + 0.20
         PLYBUT(3,I) = PPSDIM(3)
         PLYBUT(4,I) = PLYBUT(3,I) + 0.07
 100  CONTINUE
C
C -------------------------------------------------------------------
C     Set RDPlot Control Defaults and Dimensions
C -------------------------------------------------------------------
C
C     Initial Zoom, Calibrators, Catalog and Window state to FALSE
C
      PRZOOM = .FALSE.
      PRDCAL = .FALSE.
      PRDWIN = .FALSE.
      PRDCAT = .FALSE.
      PRDNAM = .FALSE.
      PRWARN = .FALSE.
      PRZOFX = .FALSE.
      PRDAXI = .TRUE.
C
C     Zoom check to IN only
C
      PZOOCK = -1
C
C     Calibrators check not set
C
      PCALCK = 0
C
C     No initial Active Button 
C
      PRDBCK = 0
C
C     Initial Button label state do default
C
      DO 102 I=1,PRDBTN
 102     PRDLAB(I) = 1
C
C
C     Set Control Buttons Dimension
C
      Y1 = 0.04
      X1 = ( 1.0 - ( ( PRDBTN - 1 ) * 0.004 ) ) / PRDBTN 
      DO 110 I=1,PRDBTN
         PRDBUT(1,I) = 0.0 + (( X1 + 0.004) * (I - 1))
         PRDBUT(2,I) = PRDBUT(1,I) + X1
         PRDBUT(3,I) = Y1
         PRDBUT(4,I) = PRDBUT(3,I) + 0.04
 110  CONTINUE
C
C -------------------------------------------------------------------
C     Try to open the X11 pgplot server.
C     If fail set bad status ( -1 ) and return
C -------------------------------------------------------------------
C
      STAT = 0
      PPOPEN = PGOPEN( '/XW' )
      IF( PPOPEN .LE. 0) THEN
         STAT = -1
         RETURN
      END IF
C
C     Set the default background color to LIGHT GREY
C     to do this invert the position of color 0 (BLACK) and 15
C     Alloc and set backgound colors.
C
       CALL PLOTBG( 1, 0 )
C
C     Make the Pannel area and define Viewport and window coordinate.
C     Set the ask page and clipping to false.
C
      CALL PGPAP( PPNDIM(1), PPNDIM(2) )
      CALL PGSVP( 0.0, 1.0, 0.0, 1.0)
      CALL PGASK(.FALSE.)
      CALL PGSCLP( 0 )
      CALL PGSWIN(0.0, 1.0, 0.0, 1.0)
C
      RETURN
      END
