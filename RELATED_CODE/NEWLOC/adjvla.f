      SUBROUTINE ADJVLA
C
C     Routing for NEWLOC that makes adjustments to the VLA positions
C     and does some sanity checking on those positions.
C
      INCLUDE  'newloc.inc'
C
      INTEGER    IV, LEN1, IS
      DOUBLE PRECISION  ADJX, ADJY, ADJZ
C -------------------------------------------------------------------
C
C     Get some indices.  First where are PT, LA, and N8 in the geodetic data.
C
      ISN8 = 0
      DO I = 1, NS
         IF( STA(I) .EQ. 'PIETOWN' ) ISPT = I
         IF( STA(I) .EQ. 'LA-VLBA' ) ISLA = I
         IF( STA(I) .EQ. 'VLA-N8' )  ISN8 = I
      END DO
C
C     Now where are N8, PT, and VLA center in VLA data.
C
      IPPT = 0
      DO IP = 1, NV
         IF( VNAME(IP) .EQ. 'VLA_N8 ' ) IPN8 = IP
         IF( VNAME(IP) .EQ. 'VLA_VPT' ) IPPT = IP
         IF( VNAME(IP) .EQ. 'VLA_CEN' .OR. 
     1       ( VNAME(IP) .EQ. 'VLA' .AND. LEN1(VNAME(IP)) .EQ. 3 ) ) 
     2       IPCEN = IP
      END DO
      WRITE(*,*) ' '
      WRITE(*,*) 'Adjusting VLA antenna positions.'
      WRITE(*,*) 'Indices: Num sta, Geo: Pie Town, VLA, VLAN8:', 
     1            NS, ISPT, ISLA, ISN8
      WRITE(*,*) '         Num VLA, VLA: Pie Town, VLA, VLAN8:',
     1            NV, IPN8, IPPT, IPCEN
C
C     If dealing with a geodesy solution without the VLA, put in a
C     something reasonable.
C     
C     Recall that VLA-N8 got inserted above as the first station so
C     we probably don't need to add that station.  Set up to do it
C     just in case, but trigger filling in the data based on it being
C     zero, not on the station missing.
C
      IF( ISN8 .EQ. 0 ) THEN 
         WRITE(*,*) 'WARNING - Using VLA-N8 position from 2008a'
         WRITE(*,*) '    You should probably not be here. '
         WRITE(*,*) 
     1       '    The geodetic solution should contain a VLA antenna.'
         NS = NS + 1
         ISN8 = NS
         STA(ISN8) = 'VLA-N8'
C
C        This is data from the 2008a geodesy solution, adjusted for the
C        difference between 2008a_astro and 2008a geodesy.
C
         VLBIX(ISN8)  = -1601147745.04D0 / 1.D3
         VLBIEX(ISN8) = 3.682D0 / 1.D3
         VLBIY(ISN8)  = -5041733510.15D0 / 1.D3
         VLBIEY(ISN8) = 7.278D0 / 1.D3
         VLBIZ(ISN8)  = 3555235769.20D0 / 1.D3
         VLBIEZ(ISN8) = 4.841D0 / 1.D3
         VLBIOF(ISN8) = 0.0D0
         VLBIRX(ISN8) = -13.95 / 1.D3
         VLBIRY(ISN8) =   0.47 / 1.D3
         VLBIRZ(ISN8) =  -6.71 / 1.D3
         VLBIJDAY(ISN8) = 50449
         write(*,*) ' Just forced N8'
      END IF
C
C     Convert VLA positions to meters in a system with axes
C     parallel to the ITRF.  With the new VLA coordinates that are
C     aligned with the ITRF, this is just the misalignment.  The
C     numbers are set in newloc.inc.  There was an 0.76" offset
C     set in the 2011 version, but I don't know that we have any
C     real information not that PT is gone to justify that.  Opt
C     for the zero option.
C
      CL = MNS * COS( VLALONG )
      SL = MNS * SIN( VLALONG )
      WRITE(*,'(A,3F14.9)') 'VLA rotation wrt ITRF radians: ', 
     1     VLALONG, CL, SL

C
      DO IP = 1, NV
         IRVLAX(IP) = VLAX(IP) * CL + VLAY(IP) * SL 
         IRVLAY(IP) = -1.D0 * VLAX(IP) * SL + VLAY(IP) * CL
         IRVLAZ(IP) = MNS * VLAZ(IP)
      END DO
C
C     Now shift the VLA positions to match the ITRF at N8.
C     For the VLA error, use the N8 error plus 1 cm
C     for the VLA position error (mainly variations when
C     different antennas are on a pad) plus a factor times the
C     VLA offset from the VLA center to account for orientation
C     problems etc.  Have that factor, times about 52 km, come
C     out to about 3 cm.  So use 0.03/52000 = 5.D-7
C     The 3 is off the cuff guess based on how well baselines
C     match and the 52 is the Pie Town baseline length.
C
C
      ADJX = VLBIX(ISN8) - IRVLAX(IPN8)
      ADJY = VLBIY(ISN8) - IRVLAY(IPN8)
      ADJZ = VLBIZ(ISN8) - IRVLAZ(IPN8)
      WRITE(*,*) 'Adj: ', ISN8, IPN8, VLBIZ(ISN8), IRVLAZ(IPN8),
     1           ADJX, ADJY, ADJZ
      DO IP = 1, NV
         IRVLAX(IP) = IRVLAX(IP) + ADJX
         IRVLAY(IP) = IRVLAY(IP) + ADJY
         IRVLAZ(IP) = IRVLAZ(IP) + ADJZ
         VLALEN = SQRT( VLAX(IP)**2 + VLAY(IP)**2 + VLAZ(IP)**2 ) * MNS
         FACTOR = 0.03D0 / 52000.D0
         VLAMER = 0.01D0
         IRVLAEX(IP) = SQRT( VLBIEX(ISN8)**2 + VLAMER**2 + 
     1         ( FACTOR * VLALEN )**2 )
         IRVLAEY(IP) = SQRT( VLBIEY(ISN8)**2 + VLAMER**2 + 
     1         ( FACTOR * VLALEN )**2 )
         IRVLAEZ(IP) = SQRT( VLBIEZ(ISN8)**2 + VLAMER**2 + 
     1         ( FACTOR * VLALEN )**2 )
      END DO
C
C     Transfer the VLA data to the output positions array.
C     Since we did not read an old locations.dat, just transfer
C     all of them.
C
      DO IV = 1, NV
         NS = NS + 1
         IS = NS
         IF( NS .GT. MS ) THEN
            WRITE(*,*) 'ADJVLA: Too many stations', NS
            STOP
         END IF
         STA(IS) = VNAME(IV)
         IF( GOTPOS(IS) .NE. ' ' ) THEN
            WRITE(*,*) 'Unexpected - VLA station had geo position'
            STOP
         END IF
         GOTPOS(IS) = 'VLA'
         GOTRAT(IS) = 'N8'
         VLBIX(IS) = IRVLAX(IV)
         VLBIY(IS) = IRVLAY(IV)
         VLBIZ(IS) = IRVLAZ(IV)
         VLBIEX(IS) = IRVLAEX(IV)
         VLBIEY(IS) = IRVLAEY(IV)
         VLBIEZ(IS) = IRVLAEZ(IV)
         VLBIRX(IS) = VLBIRX(ISN8)
         VLBIRY(IS) = VLBIRY(ISN8)
         VLBIRZ(IS) = VLBIRZ(ISN8)
         VLBIOF(IS) = VLAAXOF(IV)
         VLBIJDAY(IS) = VLBIJDAY(ISN8)
         FRAME(IS)  = VLAFRM
      END DO
C
C
C     The following are various ways of looking at the data to check the 
C     coordinate conversion.  If PT is not in the VLA data set, then don't
C     bother.
C
C     Write the position difference for Pietown.
C
      IF( IPPT .GT. 0 ) THEN
         WRITE(*,*) 'VLA positions shifted so that VLA_N8 is '//
     1     'at the VLBI VLA-N8 position'
         WRITE(*,*) 'VLA coordinates rotated by a VLA '//
     1     'longitude - see below.'
         WRITE(*,*) 'Compare Pie Town positions (m):'
         WRITE(*,'(A,3F16.4)') ' Geodetic:      ', 
     1      VLBIX(ISPT), VLBIY(ISPT), VLBIZ(ISPT)
         WRITE(*,'(A,3F16.4)') ' Converted VLA: ', 
     1      IRVLAX(IPPT), IRVLAY(IPPT), IRVLAZ(IPPT)
         WRITE(*,'(A,3F16.4)') ' Difference:    ',
     1      VLBIX(ISPT) - IRVLAX(IPPT), VLBIY(ISPT) - IRVLAY(IPPT),
     3      VLBIZ(ISPT) - IRVLAZ(IPPT)
C
C        Consider the baseline length difference.
C
         BXY8PT = SQRT( ( VLBIX(ISPT) - VLBIX(ISN8) )**2 + 
     1               ( VLBIY(ISPT) - VLBIY(ISN8) )**2 )
         VLBN8PT = SQRT( BXY8PT**2 + ( VLBIZ(ISPT) - VLBIZ(ISN8) )**2 )
C
         AXY8PT = SQRT( ( IRVLAX(IPPT) - IRVLAX(IPN8) )**2 + 
     1               ( IRVLAY(IPPT) - IRVLAY(IPN8) )**2 )
         VLAN8PT = SQRT( AXY8PT**2 + ( IRVLAZ(IPPT) - IRVLAZ(IPN8) )**2)
C
         WRITE(*,*) ' PT-N8 Baseline Differences (VLBI-VLA):'
         WRITE(*,'(A,F16.4, A)') 
     1     ' Length Difference (m):', VLBN8PT - VLAN8PT
C
C        Consider the baseline longitude difference.
C
         BLONN8PT = ATAN2( VLBIY(ISPT) - VLBIY(ISN8), 
     1                  VLBIX(ISPT) - VLBIX(ISN8) ) / RADSEC
         ALONN8PT = ATAN2( IRVLAY(IPPT) - IRVLAY(IPN8), 
     1                  IRVLAX(IPPT) - IRVLAX(IPN8) ) / RADSEC
         WRITE(*,'(A,F10.5, A, I4, I3, F9.5, A, F11.6, A )') 
     1    ' Longitude Difference ("):', BLONN8PT - ALONN8PT,
     2    '       VLA Longitude: ', VLALD, VLALM, VLALS, 
     3    '  = ', VLALONG / ( RADSEC * 3600.D0 ), ' deg.'
C
C        Look at the latitude difference.
C
         BLAT8PT = ATAN2( VLBIZ(ISPT) - VLBIZ(ISN8), BXY8PT ) / RADSEC
         ALAT8PT = ATAN2( IRVLAZ(IPPT) - IRVLAZ(IPN8), AXY8PT ) / RADSEC
         WRITE(*,'(A, F10.5 )' ) 
     1      ' Latitude difference ("):', 
     2      BLAT8PT - ALAT8PT
C
      END IF
C
      RETURN
      END
