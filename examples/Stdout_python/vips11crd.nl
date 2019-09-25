!*  Schedule for VLBA_NL   *!
!*  Experiment VIPS11   *!
!* Schedule Version:       2.00 *!
!* Processed by SCHED version:  11.50 *!
!* PI:       Greg Taylor *!
!* Address:  University of New Mexico *!
!*           Department of Physics and Astronomy *!
!*           800 Yale Blvd NE *!
!*           Albuquerque NM 87131 *!
!* Phone:    505-277-5238 *!
!* EMAIL:    gbtaylor@unm.edu *!
!* Fax:      505-277-1520 *!
!* Phone during observation: 505-838-7411 *!
!* Observing mode: continuum *!
!* Notes:    Fringe finder and pol. EVPA cal #1: 3C279 *!
!*           Pol. EVPA cal #2: J0854+2006 *!
!*           Pol. EVPA cal #3: J1310+3220 *!
!*           Pol. D-term cal: OQ208 *!
!*  Start at 00h29m01s     Thu, 2006 Feb 16  Day of year   47   *!
program=VIPS11  

diskformat=mark5c
media=(1,disk)

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 00h29m01s to 00h31m00s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
maxcaltime= 120
fe=(1,6cm),(2,6cm),(3,6cm),(4,6cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,-5.9),( 2, 3.9),( 3,15.6)
logging=STANDARD
nchan= 4
format=VLBA1:2
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,U),(2,U),(3,L),(4,L)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,775.75),( 2,775.75),( 3,808.25),( 4,808.25)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S2),(7,S3),(8,S4)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,M1),(6,M2),(7,M3),(8,M4)
pcalxfreq1=(1,250),(2,250),(3,6250),(4,6250),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,250),(2,250),(3,6250),(4,6250),(5,0),(6,0),(7,0),(8,0)
samplerate=16M
disk=off
  date = 2006Feb16
stop=00h29m01s   !NEXT!        
qual=  0
disk=off
stop=00h31m00s   !NEXT!

!* --- Scan from 00h32m26s to 00h33m14s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=00h32m26s   !NEXT!        
qual=  0
disk=off
stop=00h33m14s   !NEXT!

!* --- Scan from 00h33m26s to 00h34m14s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=00h33m26s   !NEXT!        
qual=  0
disk=off
stop=00h34m14s   !NEXT!

!* --- Scan from 00h34m28s to 00h35m16s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=00h34m28s   !NEXT!        
qual=  0
disk=off
stop=00h35m16s   !NEXT!

!* --- Scan from 00h35m27s to 00h36m15s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=00h35m27s   !NEXT!        
qual=  0
disk=off
stop=00h36m15s   !NEXT!

!* --- Scan from 00h36m26s to 00h37m13s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=00h36m26s   !NEXT!        
qual=  0
disk=off
stop=00h37m13s   !NEXT!

!* --- Scan from 00h37m25s to 00h38m13s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=00h37m25s   !NEXT!        
qual=  0
disk=off
stop=00h38m13s   !NEXT!

!* --- Scan from 00h38m23s to 00h39m11s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=00h38m23s   !NEXT!        
qual=  0
disk=off
stop=00h39m11s   !NEXT!

!* --- Scan from 00h39m21s to 00h40m09s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=00h39m21s   !NEXT!        
qual=  0
disk=off
stop=00h40m09s   !NEXT!

!* --- Scan from 00h40m20s to 00h41m08s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=00h40m20s   !NEXT!        
qual=  0
disk=off
stop=00h41m08s   !NEXT!

!* --- Scan from 00h41m17s to 00h42m05s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=00h41m17s   !NEXT!        
qual=  0
disk=off
stop=00h42m05s   !NEXT!

!* --- Scan from 00h42m14s to 00h43m02s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=00h42m14s   !NEXT!        
qual=  0
disk=off
stop=00h43m02s   !NEXT!

!* --- Scan from 00h43m12s to 00h44m00s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=00h43m12s   !NEXT!        
qual=  0
disk=off
stop=00h44m00s   !NEXT!

!* --- Scan from 00h44m10s to 00h44m58s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=00h44m10s   !NEXT!        
qual=  0
disk=off
stop=00h44m58s   !NEXT!

!* --- Scan from 00h45m14s to 00h46m02s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=00h45m14s   !NEXT!        
qual=  0
disk=off
stop=00h46m02s   !NEXT!

!* --- Scan from 00h46m13s to 00h47m01s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=00h46m13s   !NEXT!        
qual=  0
disk=off
stop=00h47m01s   !NEXT!

!* --- Scan from 00h47m12s to 00h48m00s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=00h47m12s   !NEXT!        
qual=  0
disk=off
stop=00h48m00s   !NEXT!

!* --- Scan from 00h48m12s to 00h49m00s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=00h48m12s   !NEXT!        
qual=  0
disk=off
stop=00h49m00s   !NEXT!

!* --- Scan from 00h49m10s to 00h49m58s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=00h49m10s   !NEXT!        
qual=  0
disk=off
stop=00h49m58s   !NEXT!

!* --- Scan from 00h50m09s to 00h50m57s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=00h50m09s   !NEXT!        
qual=  0
disk=off
stop=00h50m57s   !NEXT!

!* --- Scan from 00h51m07s to 00h51m55s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=00h51m07s   !NEXT!        
qual=  0
disk=off
stop=00h51m55s   !NEXT!

!* --- Scan from 00h52m05s to 00h52m52s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=00h52m05s   !NEXT!        
qual=  0
disk=off
stop=00h52m52s   !NEXT!

!* --- Scan from 00h53m05s to 00h53m52s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=00h53m05s   !NEXT!        
qual=  0
disk=off
stop=00h53m52s   !NEXT!

!* --- Scan from 00h54m04s to 00h54m52s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=00h54m04s   !NEXT!        
qual=  0
disk=off
stop=00h54m52s   !NEXT!

!* --- Scan from 00h55m02s to 00h55m50s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=00h55m02s   !NEXT!        
qual=  0
disk=off
stop=00h55m50s   !NEXT!

!* --- Scan from 00h56m04s to 00h56m52s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=00h56m04s   !NEXT!        
qual=  0
disk=off
stop=00h56m52s   !NEXT!

!* --- Scan from 00h57m06s to 00h57m54s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=00h57m06s   !NEXT!        
qual=  0
disk=off
stop=00h57m54s   !NEXT!

!* --- Scan from 00h58m04s to 00h58m52s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=00h58m04s   !NEXT!        
qual=  0
disk=off
stop=00h58m52s   !NEXT!

!* --- Scan from 00h59m08s to 00h59m56s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=00h59m08s   !NEXT!        
qual=  0
disk=off
stop=00h59m56s   !NEXT!

!* --- Scan from 01h00m17s to 01h01m05s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=01h00m17s   !NEXT!        
qual=  0
disk=off
stop=01h01m05s   !NEXT!

!* --- Scan from 01h01m26s to 01h02m14s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=01h01m26s   !NEXT!        
qual=  0
disk=off
stop=01h02m14s   !NEXT!

!* --- Scan from 01h02m24s to 01h03m11s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=01h02m24s   !NEXT!        
qual=  0
disk=off
stop=01h03m11s   !NEXT!

!* --- Scan from 01h03m29s to 01h04m17s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=01h03m29s   !NEXT!        
qual=  0
disk=off
stop=01h04m17s   !NEXT!

!* --- Scan from 01h04m29s to 01h05m17s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=01h04m29s   !NEXT!        
qual=  0
disk=off
stop=01h05m17s   !NEXT!

!* --- Scan from 01h05m25s to 01h06m12s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=01h05m25s   !NEXT!        
qual=  0
disk=off
stop=01h06m12s   !NEXT!

!* --- Scan from 01h06m30s to 01h07m17s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=01h06m30s   !NEXT!        
qual=  0
disk=off
stop=01h07m17s   !NEXT!

!* --- Scan from 01h07m33s to 01h08m21s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=01h07m33s   !NEXT!        
qual=  0
disk=off
stop=01h08m21s   !NEXT!

!* --- Scan from 01h08m37s to 01h09m25s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=01h08m37s   !NEXT!        
qual=  0
disk=off
stop=01h09m25s   !NEXT!

!* --- Scan from 01h09m39s to 01h10m27s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=01h09m39s   !NEXT!        
qual=  0
disk=off
stop=01h10m27s   !NEXT!

!* --- Scan from 01h10m41s to 01h11m29s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=01h10m41s   !NEXT!        
qual=  0
disk=off
stop=01h11m29s   !NEXT!

!* --- Scan from 01h11m42s to 01h12m30s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=01h11m42s   !NEXT!        
qual=  0
disk=off
stop=01h12m30s   !NEXT!

!* --- Scan from 01h12m38s to 01h13m26s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=01h12m38s   !NEXT!        
qual=  0
disk=off
stop=01h13m26s   !NEXT!

!* --- Scan from 01h13m41s to 01h14m29s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=01h13m41s   !NEXT!        
qual=  0
disk=off
stop=01h14m29s   !NEXT!

!* --- Scan from 01h14m37s to 01h15m25s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=01h14m37s   !NEXT!        
qual=  0
disk=off
stop=01h15m25s   !NEXT!

!* --- Scan from 01h15m44s to 01h16m32s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=01h15m44s   !NEXT!        
qual=  0
disk=off
stop=01h16m32s   !NEXT!

!* --- Scan from 01h16m43s to 01h17m31s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=01h16m43s   !NEXT!        
qual=  0
disk=off
stop=01h17m31s   !NEXT!

!* --- Scan from 01h17m46s to 01h18m34s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=01h17m46s   !NEXT!        
qual=  0
disk=off
stop=01h18m34s   !NEXT!

!* --- Scan from 01h18m45s to 01h19m33s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=01h18m45s   !NEXT!        
qual=  0
disk=off
stop=01h19m33s   !NEXT!

!* --- Scan from 01h19m42s to 01h20m30s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=01h19m42s   !NEXT!        
qual=  0
disk=off
stop=01h20m30s   !NEXT!

!* --- Scan from 01h20m39s to 01h21m27s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=01h20m39s   !NEXT!        
qual=  0
disk=off
stop=01h21m27s   !NEXT!

!* --- Scan from 01h21m36s to 01h22m24s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=01h21m36s   !NEXT!        
qual=  0
disk=off
stop=01h22m24s   !NEXT!

!* --- Scan from 01h22m33s to 01h23m21s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=01h22m33s   !NEXT!        
qual=  0
disk=off
stop=01h23m21s   !NEXT!

!* --- Scan from 01h23m30s to 01h24m18s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=01h23m30s   !NEXT!        
qual=  0
disk=off
stop=01h24m18s   !NEXT!

!* --- Scan from 01h24m37s to 01h25m25s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=01h24m37s   !NEXT!        
qual=  0
disk=off
stop=01h25m25s   !NEXT!

!* --- Scan from 01h25m37s to 01h26m25s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=01h25m37s   !NEXT!        
qual=  0
disk=off
stop=01h26m25s   !NEXT!

!* --- Scan from 01h26m43s to 01h27m31s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=01h26m43s   !NEXT!        
qual=  0
disk=off
stop=01h27m31s   !NEXT!

!* --- Scan from 01h27m42s to 01h28m30s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=01h27m42s   !NEXT!        
qual=  0
disk=off
stop=01h28m30s   !NEXT!

!* --- Scan from 01h28m40s to 01h29m27s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=01h28m40s   !NEXT!        
qual=  0
disk=off
stop=01h29m27s   !NEXT!

!* --- Scan from 01h30m11s to 01h30m59s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=01h30m11s   !NEXT!        
qual=  0
disk=off
stop=01h30m59s   !NEXT!

!* --- Scan from 01h31m09s to 01h31m57s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=01h31m09s   !NEXT!        
qual=  0
disk=off
stop=01h31m57s   !NEXT!

!* --- Scan from 01h32m07s to 01h32m54s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=01h32m07s   !NEXT!        
qual=  0
disk=off
stop=01h32m54s   !NEXT!

!* --- Scan from 01h33m05s to 01h33m52s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=01h33m05s   !NEXT!        
qual=  0
disk=off
stop=01h33m52s   !NEXT!

!* --- Scan from 01h34m05s to 01h34m53s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=01h34m05s   !NEXT!        
qual=  0
disk=off
stop=01h34m53s   !NEXT!

!* --- Scan from 01h35m09s to 01h35m57s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=01h35m09s   !NEXT!        
qual=  0
disk=off
stop=01h35m57s   !NEXT!

!* --- Scan from 01h36m08s to 01h36m55s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=01h36m08s   !NEXT!        
qual=  0
disk=off
stop=01h36m55s   !NEXT!

!* --- Scan from 01h37m06s to 01h37m54s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=01h37m06s   !NEXT!        
qual=  0
disk=off
stop=01h37m54s   !NEXT!

!* --- Scan from 01h38m07s to 01h38m55s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=01h38m07s   !NEXT!        
qual=  0
disk=off
stop=01h38m55s   !NEXT!

!* --- Scan from 01h39m06s to 01h39m54s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=01h39m06s   !NEXT!        
qual=  0
disk=off
stop=01h39m54s   !NEXT!

!* --- Scan from 01h40m35s to 01h41m23s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=01h40m35s   !NEXT!        
qual=  0
disk=off
stop=01h41m23s   !NEXT!

!* --- Scan from 01h41m30s to 01h42m18s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=01h41m30s   !NEXT!        
qual=  0
disk=off
stop=01h42m18s   !NEXT!

!* --- Scan from 01h42m26s to 01h43m14s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=01h42m26s   !NEXT!        
qual=  0
disk=off
stop=01h43m14s   !NEXT!

!* --- Scan from 01h45m03s to 01h47m02s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=01h45m03s   !NEXT!        
qual=  0
disk=off
stop=01h47m02s   !NEXT!

!* --- Scan from 01h48m35s to 01h49m23s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=01h48m35s   !NEXT!        
qual=  0
disk=off
stop=01h49m23s   !NEXT!

!* --- Scan from 01h49m32s to 01h50m20s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=01h49m32s   !NEXT!        
qual=  0
disk=off
stop=01h50m20s   !NEXT!

!* --- Scan from 01h50m37s to 01h51m25s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=01h50m37s   !NEXT!        
qual=  0
disk=off
stop=01h51m25s   !NEXT!

!* --- Scan from 01h51m38s to 01h52m26s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=01h51m38s   !NEXT!        
qual=  0
disk=off
stop=01h52m26s   !NEXT!

!* --- Scan from 01h52m41s to 01h53m29s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=01h52m41s   !NEXT!        
qual=  0
disk=off
stop=01h53m29s   !NEXT!

!* --- Scan from 01h53m40s to 01h54m28s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=01h53m40s   !NEXT!        
qual=  0
disk=off
stop=01h54m28s   !NEXT!

!* --- Scan from 01h54m41s to 01h55m29s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=01h54m41s   !NEXT!        
qual=  0
disk=off
stop=01h55m29s   !NEXT!

!* --- Scan from 01h55m40s to 01h56m28s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=01h55m40s   !NEXT!        
qual=  0
disk=off
stop=01h56m28s   !NEXT!

!* --- Scan from 01h56m44s to 01h57m32s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=01h56m44s   !NEXT!        
qual=  0
disk=off
stop=01h57m32s   !NEXT!

!* --- Scan from 01h57m44s to 01h58m32s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=01h57m44s   !NEXT!        
qual=  0
disk=off
stop=01h58m32s   !NEXT!

!* --- Scan from 01h58m42s to 01h59m30s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=01h58m42s   !NEXT!        
qual=  0
disk=off
stop=01h59m30s   !NEXT!

!* --- Scan from 01h59m41s to 02h00m29s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=01h59m41s   !NEXT!        
qual=  0
disk=off
stop=02h00m29s   !NEXT!

!* --- Scan from 02h00m39s to 02h01m27s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=02h00m39s   !NEXT!        
qual=  0
disk=off
stop=02h01m27s   !NEXT!

!* --- Scan from 02h01m42s to 02h02m30s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=02h01m42s   !NEXT!        
qual=  0
disk=off
stop=02h02m30s   !NEXT!

!* --- Scan from 02h02m56s to 02h03m44s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=02h02m56s   !NEXT!        
qual=  0
disk=off
stop=02h03m44s   !NEXT!

!* --- Scan from 02h04m03s to 02h04m51s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=02h04m03s   !NEXT!        
qual=  0
disk=off
stop=02h04m51s   !NEXT!

!* --- Scan from 02h05m01s to 02h05m49s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=02h05m01s   !NEXT!        
qual=  0
disk=off
stop=02h05m49s   !NEXT!

!* --- Scan from 02h06m02s to 02h06m50s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=02h06m02s   !NEXT!        
qual=  0
disk=off
stop=02h06m50s   !NEXT!

!* --- Scan from 02h07m09s to 02h07m57s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=02h07m09s   !NEXT!        
qual=  0
disk=off
stop=02h07m57s   !NEXT!

!* --- Scan from 02h08m05s to 02h08m53s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=02h08m05s   !NEXT!        
qual=  0
disk=off
stop=02h08m53s   !NEXT!

!* --- Scan from 02h09m06s to 02h09m54s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=02h09m06s   !NEXT!        
qual=  0
disk=off
stop=02h09m54s   !NEXT!

!* --- Scan from 02h10m08s to 02h10m56s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=02h10m08s   !NEXT!        
qual=  0
disk=off
stop=02h10m56s   !NEXT!

!* --- Scan from 02h11m14s to 02h12m02s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=02h11m14s   !NEXT!        
qual=  0
disk=off
stop=02h12m02s   !NEXT!

!* --- Scan from 02h12m11s to 02h12m59s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=02h12m11s   !NEXT!        
qual=  0
disk=off
stop=02h12m59s   !NEXT!

!* --- Scan from 02h13m19s to 02h14m07s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=02h13m19s   !NEXT!        
qual=  0
disk=off
stop=02h14m07s   !NEXT!

!* --- Scan from 02h14m19s to 02h15m07s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=02h14m19s   !NEXT!        
qual=  0
disk=off
stop=02h15m07s   !NEXT!

!* --- Scan from 02h15m17s to 02h16m05s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=02h15m17s   !NEXT!        
qual=  0
disk=off
stop=02h16m05s   !NEXT!

!* --- Scan from 02h16m14s to 02h17m02s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=02h16m14s   !NEXT!        
qual=  0
disk=off
stop=02h17m02s   !NEXT!

!* --- Scan from 02h17m19s to 02h18m07s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=02h17m19s   !NEXT!        
qual=  0
disk=off
stop=02h18m07s   !NEXT!

!* --- Scan from 02h18m19s to 02h19m07s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=02h18m19s   !NEXT!        
qual=  0
disk=off
stop=02h19m07s   !NEXT!

!* --- Scan from 02h19m19s to 02h20m07s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=02h19m19s   !NEXT!        
qual=  0
disk=off
stop=02h20m07s   !NEXT!

!* --- Scan from 02h20m23s to 02h21m11s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=02h20m23s   !NEXT!        
qual=  0
disk=off
stop=02h21m11s   !NEXT!

!* --- Scan from 02h21m27s to 02h22m15s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=02h21m27s   !NEXT!        
qual=  0
disk=off
stop=02h22m15s   !NEXT!

!* --- Scan from 02h22m32s to 02h23m20s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=02h22m32s   !NEXT!        
qual=  0
disk=off
stop=02h23m20s   !NEXT!

!* --- Scan from 02h23m40s to 02h24m27s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=02h23m40s   !NEXT!        
qual=  0
disk=off
stop=02h24m27s   !NEXT!

!* --- Scan from 02h24m36s to 02h25m24s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=02h24m36s   !NEXT!        
qual=  0
disk=off
stop=02h25m24s   !NEXT!

!* --- Scan from 02h25m42s to 02h26m30s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=02h25m42s   !NEXT!        
qual=  0
disk=off
stop=02h26m30s   !NEXT!

!* --- Scan from 02h27m10s to 02h27m58s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=02h27m10s   !NEXT!        
qual=  0
disk=off
stop=02h27m58s   !NEXT!

!* --- Scan from 02h28m08s to 02h28m56s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=02h28m08s   !NEXT!        
qual=  0
disk=off
stop=02h28m56s   !NEXT!

!* --- Scan from 02h29m04s to 02h29m52s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=02h29m04s   !NEXT!        
qual=  0
disk=off
stop=02h29m52s   !NEXT!

!* --- Scan from 02h30m05s to 02h30m53s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=02h30m05s   !NEXT!        
qual=  0
disk=off
stop=02h30m53s   !NEXT!

!* --- Scan from 02h31m02s to 02h31m50s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=02h31m02s   !NEXT!        
qual=  0
disk=off
stop=02h31m50s   !NEXT!

!* --- Scan from 02h31m58s to 02h32m46s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=02h31m58s   !NEXT!        
qual=  0
disk=off
stop=02h32m46s   !NEXT!

!* --- Scan from 02h32m55s to 02h33m43s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=02h32m55s   !NEXT!        
qual=  0
disk=off
stop=02h33m43s   !NEXT!

!* --- Scan from 02h34m01s to 02h34m49s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=02h34m01s   !NEXT!        
qual=  0
disk=off
stop=02h34m49s   !NEXT!

!* --- Scan from 02h34m57s to 02h35m45s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=02h34m57s   !NEXT!        
qual=  0
disk=off
stop=02h35m45s   !NEXT!

!* --- Scan from 02h36m00s to 02h36m48s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=02h36m00s   !NEXT!        
qual=  0
disk=off
stop=02h36m48s   !NEXT!

!* --- Scan from 02h37m01s to 02h37m49s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=02h37m01s   !NEXT!        
qual=  0
disk=off
stop=02h37m49s   !NEXT!

!* --- Scan from 02h38m01s to 02h38m49s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=02h38m01s   !NEXT!        
qual=  0
disk=off
stop=02h38m49s   !NEXT!

!* --- Scan from 02h39m00s to 02h39m48s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=02h39m00s   !NEXT!        
qual=  0
disk=off
stop=02h39m48s   !NEXT!

!* --- Scan from 02h39m57s to 02h40m45s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=02h39m57s   !NEXT!        
qual=  0
disk=off
stop=02h40m45s   !NEXT!

!* --- Scan from 02h40m56s to 02h41m44s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=02h40m56s   !NEXT!        
qual=  0
disk=off
stop=02h41m44s   !NEXT!

!* --- Scan from 02h41m57s to 02h42m44s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=02h41m57s   !NEXT!        
qual=  0
disk=off
stop=02h42m44s   !NEXT!

!* --- Scan from 02h42m55s to 02h43m43s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=02h42m55s   !NEXT!        
qual=  0
disk=off
stop=02h43m43s   !NEXT!

!* --- Scan from 02h43m56s to 02h44m44s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=02h43m56s   !NEXT!        
qual=  0
disk=off
stop=02h44m44s   !NEXT!

!* --- Scan from 02h45m04s to 02h45m52s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=02h45m04s   !NEXT!        
qual=  0
disk=off
stop=02h45m52s   !NEXT!

!* --- Scan from 02h46m08s to 02h46m56s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=02h46m08s   !NEXT!        
qual=  0
disk=off
stop=02h46m56s   !NEXT!

!* --- Scan from 02h47m08s to 02h47m56s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=02h47m08s   !NEXT!        
qual=  0
disk=off
stop=02h47m56s   !NEXT!

!* --- Scan from 02h48m08s to 02h48m56s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=02h48m08s   !NEXT!        
qual=  0
disk=off
stop=02h48m56s   !NEXT!

!* --- Scan from 02h49m07s to 02h49m55s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=02h49m07s   !NEXT!        
qual=  0
disk=off
stop=02h49m55s   !NEXT!

!* --- Scan from 02h50m07s to 02h50m55s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=02h50m07s   !NEXT!        
qual=  0
disk=off
stop=02h50m55s   !NEXT!

!* --- Scan from 02h51m06s to 02h51m53s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=02h51m06s   !NEXT!        
qual=  0
disk=off
stop=02h51m53s   !NEXT!

!* --- Scan from 02h52m04s to 02h52m52s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=02h52m04s   !NEXT!        
qual=  0
disk=off
stop=02h52m52s   !NEXT!

!* --- Scan from 02h53m06s to 02h53m54s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=02h53m06s   !NEXT!        
qual=  0
disk=off
stop=02h53m54s   !NEXT!

!* --- Scan from 02h54m06s to 02h54m54s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=02h54m06s   !NEXT!        
qual=  0
disk=off
stop=02h54m54s   !NEXT!

!* --- Scan from 02h55m06s to 02h55m54s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=02h55m06s   !NEXT!        
qual=  0
disk=off
stop=02h55m54s   !NEXT!

!* --- Scan from 02h56m08s to 02h56m56s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=02h56m08s   !NEXT!        
qual=  0
disk=off
stop=02h56m56s   !NEXT!

!* --- Scan from 02h57m06s to 02h57m54s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=02h57m06s   !NEXT!        
qual=  0
disk=off
stop=02h57m54s   !NEXT!

!* --- Scan from 02h58m05s to 02h58m53s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=02h58m05s   !NEXT!        
qual=  0
disk=off
stop=02h58m53s   !NEXT!

!* --- Scan from 02h59m06s to 02h59m54s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=02h59m06s   !NEXT!        
qual=  0
disk=off
stop=02h59m54s   !NEXT!

!* --- Scan from 03h00m05s to 03h00m53s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=03h00m05s   !NEXT!        
qual=  0
disk=off
stop=03h00m53s   !NEXT!

!* --- Scan from 03h02m40s to 03h04m40s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=03h02m40s   !NEXT!        
qual=  0
disk=off
stop=03h04m40s   !NEXT!

!* --- Scan from 03h06m35s to 03h07m22s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=03h06m35s   !NEXT!        
qual=  0
disk=off
stop=03h07m22s   !NEXT!

!* --- Scan from 03h08m15s to 03h09m03s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=03h08m15s   !NEXT!        
qual=  0
disk=off
stop=03h09m03s   !NEXT!

!* --- Scan from 03h09m14s to 03h10m02s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=03h09m14s   !NEXT!        
qual=  0
disk=off
stop=03h10m02s   !NEXT!

!* --- Scan from 03h11m14s to 03h12m02s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=03h11m14s   !NEXT!        
qual=  0
disk=off
stop=03h12m02s   !NEXT!

!* --- Scan from 03h12m12s to 03h13m00s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=03h12m12s   !NEXT!        
qual=  0
disk=off
stop=03h13m00s   !NEXT!

!* --- Scan from 03h13m14s to 03h14m02s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=03h13m14s   !NEXT!        
qual=  0
disk=off
stop=03h14m02s   !NEXT!

!* --- Scan from 03h14m22s to 03h15m10s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=03h14m22s   !NEXT!        
qual=  0
disk=off
stop=03h15m10s   !NEXT!

!* --- Scan from 03h15m24s to 03h16m12s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=03h15m24s   !NEXT!        
qual=  0
disk=off
stop=03h16m12s   !NEXT!

!* --- Scan from 03h16m26s to 03h17m14s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=03h16m26s   !NEXT!        
qual=  0
disk=off
stop=03h17m14s   !NEXT!

!* --- Scan from 03h18m01s to 03h18m49s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=03h18m01s   !NEXT!        
qual=  0
disk=off
stop=03h18m49s   !NEXT!

!* --- Scan from 03h19m04s to 03h19m52s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=03h19m04s   !NEXT!        
qual=  0
disk=off
stop=03h19m52s   !NEXT!

!* --- Scan from 03h20m14s to 03h21m02s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=03h20m14s   !NEXT!        
qual=  0
disk=off
stop=03h21m02s   !NEXT!

!* --- Scan from 03h22m50s to 03h23m38s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=03h22m50s   !NEXT!        
qual=  0
disk=off
stop=03h23m38s   !NEXT!

!* --- Scan from 03h25m53s to 03h26m40s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=03h25m53s   !NEXT!        
qual=  0
disk=off
stop=03h26m40s   !NEXT!

!* --- Scan from 03h26m52s to 03h27m40s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=03h26m52s   !NEXT!        
qual=  0
disk=off
stop=03h27m40s   !NEXT!

!* --- Scan from 03h28m07s to 03h28m55s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=03h28m07s   !NEXT!        
qual=  0
disk=off
stop=03h28m55s   !NEXT!

!* --- Scan from 03h29m52s to 03h30m40s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=03h29m52s   !NEXT!        
qual=  0
disk=off
stop=03h30m40s   !NEXT!

!* --- Scan from 03h30m50s to 03h31m38s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=03h30m50s   !NEXT!        
qual=  0
disk=off
stop=03h31m38s   !NEXT!

!* --- Scan from 03h31m46s to 03h32m34s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=03h31m46s   !NEXT!        
qual=  0
disk=off
stop=03h32m34s   !NEXT!

!* --- Scan from 03h32m55s to 03h33m43s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=03h32m55s   !NEXT!        
qual=  0
disk=off
stop=03h33m43s   !NEXT!

!* --- Scan from 03h33m52s to 03h34m40s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=03h33m52s   !NEXT!        
qual=  0
disk=off
stop=03h34m40s   !NEXT!

!* --- Scan from 03h35m53s to 03h36m41s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=03h35m53s   !NEXT!        
qual=  0
disk=off
stop=03h36m41s   !NEXT!

!* --- Scan from 03h37m05s to 03h37m52s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=03h37m05s   !NEXT!        
qual=  0
disk=off
stop=03h37m52s   !NEXT!

!* --- Scan from 03h38m07s to 03h38m55s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=03h38m07s   !NEXT!        
qual=  0
disk=off
stop=03h38m55s   !NEXT!

!* --- Scan from 03h39m05s to 03h39m52s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=03h39m05s   !NEXT!        
qual=  0
disk=off
stop=03h39m52s   !NEXT!

!* --- Scan from 03h40m09s to 03h40m57s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=03h40m09s   !NEXT!        
qual=  0
disk=off
stop=03h40m57s   !NEXT!

!* --- Scan from 03h41m10s to 03h41m58s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=03h41m10s   !NEXT!        
qual=  0
disk=off
stop=03h41m58s   !NEXT!

!* --- Scan from 03h42m09s to 03h42m57s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=03h42m09s   !NEXT!        
qual=  0
disk=off
stop=03h42m57s   !NEXT!

!* --- Scan from 03h43m10s to 03h43m58s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=03h43m10s   !NEXT!        
qual=  0
disk=off
stop=03h43m58s   !NEXT!

!* --- Scan from 03h44m08s to 03h44m56s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=03h44m08s   !NEXT!        
qual=  0
disk=off
stop=03h44m56s   !NEXT!

!* --- Scan from 03h45m07s to 03h45m55s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=03h45m07s   !NEXT!        
qual=  0
disk=off
stop=03h45m55s   !NEXT!

!* --- Scan from 03h46m07s to 03h46m55s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=03h46m07s   !NEXT!        
qual=  0
disk=off
stop=03h46m55s   !NEXT!

!* --- Scan from 03h47m06s to 03h47m54s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=03h47m06s   !NEXT!        
qual=  0
disk=off
stop=03h47m54s   !NEXT!

!* --- Scan from 03h48m05s to 03h48m53s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=03h48m05s   !NEXT!        
qual=  0
disk=off
stop=03h48m53s   !NEXT!

!* --- Scan from 03h49m05s to 03h49m52s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=03h49m05s   !NEXT!        
qual=  0
disk=off
stop=03h49m52s   !NEXT!

!* --- Scan from 03h50m03s to 03h50m51s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=03h50m03s   !NEXT!        
qual=  0
disk=off
stop=03h50m51s   !NEXT!

!* --- Scan from 03h51m03s to 03h51m51s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=03h51m03s   !NEXT!        
qual=  0
disk=off
stop=03h51m51s   !NEXT!

!* --- Scan from 03h52m04s to 03h52m52s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=03h52m04s   !NEXT!        
qual=  0
disk=off
stop=03h52m52s   !NEXT!

!* --- Scan from 03h53m03s to 03h53m51s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=03h53m03s   !NEXT!        
qual=  0
disk=off
stop=03h53m51s   !NEXT!

!* --- Scan from 03h54m09s to 03h54m57s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=03h54m09s   !NEXT!        
qual=  0
disk=off
stop=03h54m57s   !NEXT!

!* --- Scan from 03h55m07s to 03h55m55s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=03h55m07s   !NEXT!        
qual=  0
disk=off
stop=03h55m55s   !NEXT!

!* --- Scan from 03h56m08s to 03h56m56s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=03h56m08s   !NEXT!        
qual=  0
disk=off
stop=03h56m56s   !NEXT!

!* --- Scan from 03h57m07s to 03h57m54s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=03h57m07s   !NEXT!        
qual=  0
disk=off
stop=03h57m54s   !NEXT!

!* --- Scan from 03h58m05s to 03h58m52s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=03h58m05s   !NEXT!        
qual=  0
disk=off
stop=03h58m52s   !NEXT!

!* --- Scan from 03h59m03s to 03h59m50s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=03h59m03s   !NEXT!        
qual=  0
disk=off
stop=03h59m50s   !NEXT!

!* --- Scan from 04h00m06s to 04h00m54s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=04h00m06s   !NEXT!        
qual=  0
disk=off
stop=04h00m54s   !NEXT!

!* --- Scan from 04h01m08s to 04h01m56s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=04h01m08s   !NEXT!        
qual=  0
disk=off
stop=04h01m56s   !NEXT!

!* --- Scan from 04h02m10s to 04h02m58s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=04h02m10s   !NEXT!        
qual=  0
disk=off
stop=04h02m58s   !NEXT!

!* --- Scan from 04h03m12s to 04h04m00s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=04h03m12s   !NEXT!        
qual=  0
disk=off
stop=04h04m00s   !NEXT!

!* --- Scan from 04h04m13s to 04h05m01s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=04h04m13s   !NEXT!        
qual=  0
disk=off
stop=04h05m01s   !NEXT!

!* --- Scan from 04h05m13s to 04h06m01s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=04h05m13s   !NEXT!        
qual=  0
disk=off
stop=04h06m01s   !NEXT!

!* --- Scan from 04h06m11s to 04h06m59s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=04h06m11s   !NEXT!        
qual=  0
disk=off
stop=04h06m59s   !NEXT!

!* --- Scan from 04h07m09s to 04h07m57s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=04h07m09s   !NEXT!        
qual=  0
disk=off
stop=04h07m57s   !NEXT!

!* --- Scan from 04h08m07s to 04h08m55s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=04h08m07s   !NEXT!        
qual=  0
disk=off
stop=04h08m55s   !NEXT!

!* --- Scan from 04h11m03s to 04h13m03s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=04h11m03s   !NEXT!        
qual=  0
disk=off
stop=04h13m03s   !NEXT!

!* --- Scan from 04h15m15s to 04h16m03s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=04h15m15s   !NEXT!        
qual=  0
disk=off
stop=04h16m03s   !NEXT!

!* --- Scan from 04h16m14s to 04h17m02s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=04h16m14s   !NEXT!        
qual=  0
disk=off
stop=04h17m02s   !NEXT!

!* --- Scan from 04h17m18s to 04h18m06s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=04h17m18s   !NEXT!        
qual=  0
disk=off
stop=04h18m06s   !NEXT!

!* --- Scan from 04h18m17s to 04h19m04s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=04h18m17s   !NEXT!        
qual=  0
disk=off
stop=04h19m04s   !NEXT!

!* --- Scan from 04h19m15s to 04h20m03s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=04h19m15s   !NEXT!        
qual=  0
disk=off
stop=04h20m03s   !NEXT!

!* --- Scan from 04h20m26s to 04h21m14s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=04h20m26s   !NEXT!        
qual=  0
disk=off
stop=04h21m14s   !NEXT!

!* --- Scan from 04h21m27s to 04h23m26s   Thu, 2006 Feb 16 --- *!
sname='J0854+2006'  ra=08h54m48.874924s  dec= 20d06'30.64088"  qual=999  calib='N'
disk=off
stop=04h21m27s   !NEXT!        
qual=  0
disk=off
stop=04h23m26s   !NEXT!

!* --- Scan from 04h23m42s to 04h24m30s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=04h23m42s   !NEXT!        
qual=  0
disk=off
stop=04h24m30s   !NEXT!

!* --- Scan from 04h25m06s to 04h25m54s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=04h25m06s   !NEXT!        
qual=  0
disk=off
stop=04h25m54s   !NEXT!

!* --- Scan from 04h26m08s to 04h26m56s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=04h26m08s   !NEXT!        
qual=  0
disk=off
stop=04h26m56s   !NEXT!

!* --- Scan from 04h27m07s to 04h27m54s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=04h27m07s   !NEXT!        
qual=  0
disk=off
stop=04h27m54s   !NEXT!

!* --- Scan from 04h28m03s to 04h28m51s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=04h28m03s   !NEXT!        
qual=  0
disk=off
stop=04h28m51s   !NEXT!

!* --- Scan from 04h29m26s to 04h30m14s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=04h29m26s   !NEXT!        
qual=  0
disk=off
stop=04h30m14s   !NEXT!

!* --- Scan from 04h30m23s to 04h31m11s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=04h30m23s   !NEXT!        
qual=  0
disk=off
stop=04h31m11s   !NEXT!

!* --- Scan from 04h31m24s to 04h32m12s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=04h31m24s   !NEXT!        
qual=  0
disk=off
stop=04h32m12s   !NEXT!

!* --- Scan from 04h32m20s to 04h33m08s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=04h32m20s   !NEXT!        
qual=  0
disk=off
stop=04h33m08s   !NEXT!

!* --- Scan from 04h33m17s to 04h34m05s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=04h33m17s   !NEXT!        
qual=  0
disk=off
stop=04h34m05s   !NEXT!

!* --- Scan from 04h34m18s to 04h35m05s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=04h34m18s   !NEXT!        
qual=  0
disk=off
stop=04h35m05s   !NEXT!

!* --- Scan from 04h35m23s to 04h36m11s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=04h35m23s   !NEXT!        
qual=  0
disk=off
stop=04h36m11s   !NEXT!

!* --- Scan from 04h36m22s to 04h37m10s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=04h36m22s   !NEXT!        
qual=  0
disk=off
stop=04h37m10s   !NEXT!

!* --- Scan from 04h37m28s to 04h38m16s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=04h37m28s   !NEXT!        
qual=  0
disk=off
stop=04h38m16s   !NEXT!

!* --- Scan from 04h38m24s to 04h39m12s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=04h38m24s   !NEXT!        
qual=  0
disk=off
stop=04h39m12s   !NEXT!

!* --- Scan from 04h39m32s to 04h40m20s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=04h39m32s   !NEXT!        
qual=  0
disk=off
stop=04h40m20s   !NEXT!

!* --- Scan from 04h40m28s to 04h41m16s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=04h40m28s   !NEXT!        
qual=  0
disk=off
stop=04h41m16s   !NEXT!

!* --- Scan from 04h41m39s to 04h42m27s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=04h41m39s   !NEXT!        
qual=  0
disk=off
stop=04h42m27s   !NEXT!

!* --- Scan from 04h42m40s to 04h44m40s   Thu, 2006 Feb 16 --- *!
sname='J0854+2006'  ra=08h54m48.874924s  dec= 20d06'30.64088"  qual=999  calib='N'
disk=off
stop=04h42m40s   !NEXT!        
qual=  0
disk=off
stop=04h44m40s   !NEXT!

!* --- Scan from 04h44m53s to 04h45m41s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=04h44m53s   !NEXT!        
qual=  0
disk=off
stop=04h45m41s   !NEXT!

!* --- Scan from 04h46m25s to 04h47m12s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=04h46m25s   !NEXT!        
qual=  0
disk=off
stop=04h47m12s   !NEXT!

!* --- Scan from 04h47m23s to 04h48m11s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=04h47m23s   !NEXT!        
qual=  0
disk=off
stop=04h48m11s   !NEXT!

!* --- Scan from 04h48m27s to 04h49m14s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=04h48m27s   !NEXT!        
qual=  0
disk=off
stop=04h49m14s   !NEXT!

!* --- Scan from 04h49m28s to 04h50m16s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=04h49m28s   !NEXT!        
qual=  0
disk=off
stop=04h50m16s   !NEXT!

!* --- Scan from 04h50m27s to 04h51m15s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=04h50m27s   !NEXT!        
qual=  0
disk=off
stop=04h51m15s   !NEXT!

!* --- Scan from 04h51m31s to 04h52m19s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=04h51m31s   !NEXT!        
qual=  0
disk=off
stop=04h52m19s   !NEXT!

!* --- Scan from 04h52m33s to 04h53m20s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=04h52m33s   !NEXT!        
qual=  0
disk=off
stop=04h53m20s   !NEXT!

!* --- Scan from 04h53m32s to 04h54m20s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=04h53m32s   !NEXT!        
qual=  0
disk=off
stop=04h54m20s   !NEXT!

!* --- Scan from 04h54m37s to 04h55m25s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=04h54m37s   !NEXT!        
qual=  0
disk=off
stop=04h55m25s   !NEXT!

!* --- Scan from 04h55m47s to 04h56m35s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=04h55m47s   !NEXT!        
qual=  0
disk=off
stop=04h56m35s   !NEXT!

!* --- Scan from 04h56m53s to 04h57m41s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=04h56m53s   !NEXT!        
qual=  0
disk=off
stop=04h57m41s   !NEXT!

!* --- Scan from 04h58m05s to 04h58m52s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=04h58m05s   !NEXT!        
qual=  0
disk=off
stop=04h58m52s   !NEXT!

!* --- Scan from 04h59m12s to 05h00m00s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=04h59m12s   !NEXT!        
qual=  0
disk=off
stop=05h00m00s   !NEXT!

!* --- Scan from 05h00m20s to 05h01m08s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=05h00m20s   !NEXT!        
qual=  0
disk=off
stop=05h01m08s   !NEXT!

!* --- Scan from 05h01m24s to 05h02m12s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=05h01m24s   !NEXT!        
qual=  0
disk=off
stop=05h02m12s   !NEXT!

!* --- Scan from 05h02m26s to 05h03m14s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=05h02m26s   !NEXT!        
qual=  0
disk=off
stop=05h03m14s   !NEXT!

!* --- Scan from 05h03m30s to 05h04m18s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=05h03m30s   !NEXT!        
qual=  0
disk=off
stop=05h04m18s   !NEXT!

!* --- Scan from 05h04m32s to 05h05m20s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=05h04m32s   !NEXT!        
qual=  0
disk=off
stop=05h05m20s   !NEXT!

!* --- Scan from 05h05m41s to 05h06m28s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=05h05m41s   !NEXT!        
qual=  0
disk=off
stop=05h06m28s   !NEXT!

!* --- Scan from 05h06m44s to 05h07m32s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=05h06m44s   !NEXT!        
qual=  0
disk=off
stop=05h07m32s   !NEXT!

!* --- Scan from 05h07m42s to 05h08m30s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=05h07m42s   !NEXT!        
qual=  0
disk=off
stop=05h08m30s   !NEXT!

!* --- Scan from 05h08m41s to 05h09m29s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=05h08m41s   !NEXT!        
qual=  0
disk=off
stop=05h09m29s   !NEXT!

!* --- Scan from 05h09m40s to 05h10m28s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=05h09m40s   !NEXT!        
qual=  0
disk=off
stop=05h10m28s   !NEXT!

!* --- Scan from 05h10m47s to 05h11m35s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=05h10m47s   !NEXT!        
qual=  0
disk=off
stop=05h11m35s   !NEXT!

!* --- Scan from 05h11m50s to 05h12m37s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=05h11m50s   !NEXT!        
qual=  0
disk=off
stop=05h12m37s   !NEXT!

!* --- Scan from 05h13m05s to 05h13m52s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=05h13m05s   !NEXT!        
qual=  0
disk=off
stop=05h13m52s   !NEXT!

!* --- Scan from 05h14m08s to 05h14m56s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=05h14m08s   !NEXT!        
qual=  0
disk=off
stop=05h14m56s   !NEXT!

!* --- Scan from 05h15m21s to 05h16m08s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=05h15m21s   !NEXT!        
qual=  0
disk=off
stop=05h16m08s   !NEXT!

!* --- Scan from 05h16m20s to 05h17m08s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=05h16m20s   !NEXT!        
qual=  0
disk=off
stop=05h17m08s   !NEXT!

!* --- Scan from 05h17m24s to 05h18m12s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=05h17m24s   !NEXT!        
qual=  0
disk=off
stop=05h18m12s   !NEXT!

!* --- Scan from 05h18m24s to 05h19m12s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=05h18m24s   !NEXT!        
qual=  0
disk=off
stop=05h19m12s   !NEXT!

!* --- Scan from 05h19m47s to 05h20m35s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=05h19m47s   !NEXT!        
qual=  0
disk=off
stop=05h20m35s   !NEXT!

!* --- Scan from 05h22m52s to 05h24m52s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=05h22m52s   !NEXT!        
qual=  0
disk=off
stop=05h24m52s   !NEXT!

!* --- Scan from 05h28m55s to 05h29m43s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=05h28m55s   !NEXT!        
qual=  0
disk=off
stop=05h29m43s   !NEXT!

!* --- Scan from 05h31m08s to 05h31m56s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=05h31m08s   !NEXT!        
qual=  0
disk=off
stop=05h31m56s   !NEXT!

!* --- Scan from 05h32m06s to 05h32m54s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=05h32m06s   !NEXT!        
qual=  0
disk=off
stop=05h32m54s   !NEXT!

!* --- Scan from 05h33m05s to 05h33m53s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=05h33m05s   !NEXT!        
qual=  0
disk=off
stop=05h33m53s   !NEXT!

!* --- Scan from 05h34m05s to 05h34m53s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=05h34m05s   !NEXT!        
qual=  0
disk=off
stop=05h34m53s   !NEXT!

!* --- Scan from 05h35m08s to 05h35m56s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=05h35m08s   !NEXT!        
qual=  0
disk=off
stop=05h35m56s   !NEXT!

!* --- Scan from 05h36m04s to 05h36m51s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=05h36m04s   !NEXT!        
qual=  0
disk=off
stop=05h36m51s   !NEXT!

!* --- Scan from 05h37m02s to 05h37m50s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=05h37m02s   !NEXT!        
qual=  0
disk=off
stop=05h37m50s   !NEXT!

!* --- Scan from 05h38m02s to 05h38m50s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=05h38m02s   !NEXT!        
qual=  0
disk=off
stop=05h38m50s   !NEXT!

!* --- Scan from 05h40m05s to 05h40m53s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=05h40m05s   !NEXT!        
qual=  0
disk=off
stop=05h40m53s   !NEXT!

!* --- Scan from 05h41m06s to 05h41m54s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=05h41m06s   !NEXT!        
qual=  0
disk=off
stop=05h41m54s   !NEXT!

!* --- Scan from 05h42m07s to 05h42m55s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=05h42m07s   !NEXT!        
qual=  0
disk=off
stop=05h42m55s   !NEXT!

!* --- Scan from 05h43m07s to 05h43m55s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=05h43m07s   !NEXT!        
qual=  0
disk=off
stop=05h43m55s   !NEXT!

!* --- Scan from 05h45m28s to 05h46m16s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=05h45m28s   !NEXT!        
qual=  0
disk=off
stop=05h46m16s   !NEXT!

!* --- Scan from 05h46m29s to 05h47m17s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=05h46m29s   !NEXT!        
qual=  0
disk=off
stop=05h47m17s   !NEXT!

!* --- Scan from 05h47m35s to 05h48m23s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=05h47m35s   !NEXT!        
qual=  0
disk=off
stop=05h48m23s   !NEXT!

!* --- Scan from 05h50m03s to 05h50m51s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=05h50m03s   !NEXT!        
qual=  0
disk=off
stop=05h50m51s   !NEXT!

!* --- Scan from 05h51m05s to 05h51m53s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=05h51m05s   !NEXT!        
qual=  0
disk=off
stop=05h51m53s   !NEXT!

!* --- Scan from 05h52m03s to 05h52m51s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=05h52m03s   !NEXT!        
qual=  0
disk=off
stop=05h52m51s   !NEXT!

!* --- Scan from 05h53m27s to 05h54m14s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=05h53m27s   !NEXT!        
qual=  0
disk=off
stop=05h54m14s   !NEXT!

!* --- Scan from 05h54m29s to 05h55m17s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=05h54m29s   !NEXT!        
qual=  0
disk=off
stop=05h55m17s   !NEXT!

!* --- Scan from 05h55m34s to 05h56m22s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=05h55m34s   !NEXT!        
qual=  0
disk=off
stop=05h56m22s   !NEXT!

!* --- Scan from 05h57m17s to 05h58m05s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=05h57m17s   !NEXT!        
qual=  0
disk=off
stop=05h58m05s   !NEXT!

!* --- Scan from 05h58m15s to 05h59m03s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=05h58m15s   !NEXT!        
qual=  0
disk=off
stop=05h59m03s   !NEXT!

!* --- Scan from 05h59m18s to 06h00m05s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=05h59m18s   !NEXT!        
qual=  0
disk=off
stop=06h00m05s   !NEXT!

!* --- Scan from 06h00m22s to 06h01m10s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=06h00m22s   !NEXT!        
qual=  0
disk=off
stop=06h01m10s   !NEXT!

!* --- Scan from 06h01m21s to 06h02m09s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=06h01m21s   !NEXT!        
qual=  0
disk=off
stop=06h02m09s   !NEXT!

!* --- Scan from 06h02m22s to 06h03m10s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=06h02m22s   !NEXT!        
qual=  0
disk=off
stop=06h03m10s   !NEXT!

!* --- Scan from 06h03m22s to 06h04m10s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=06h03m22s   !NEXT!        
qual=  0
disk=off
stop=06h04m10s   !NEXT!

!* --- Scan from 06h04m21s to 06h05m09s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=06h04m21s   !NEXT!        
qual=  0
disk=off
stop=06h05m09s   !NEXT!

!* --- Scan from 06h05m20s to 06h06m08s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=06h05m20s   !NEXT!        
qual=  0
disk=off
stop=06h06m08s   !NEXT!

!* --- Scan from 06h06m19s to 06h07m07s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=06h06m19s   !NEXT!        
qual=  0
disk=off
stop=06h07m07s   !NEXT!

!* --- Scan from 06h07m20s to 06h08m08s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=06h07m20s   !NEXT!        
qual=  0
disk=off
stop=06h08m08s   !NEXT!

!* --- Scan from 06h08m26s to 06h09m13s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=06h08m26s   !NEXT!        
qual=  0
disk=off
stop=06h09m13s   !NEXT!

!* --- Scan from 06h09m26s to 06h10m14s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=06h09m26s   !NEXT!        
qual=  0
disk=off
stop=06h10m14s   !NEXT!

!* --- Scan from 06h10m27s to 06h11m15s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=06h10m27s   !NEXT!        
qual=  0
disk=off
stop=06h11m15s   !NEXT!

!* --- Scan from 06h11m31s to 06h12m19s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=06h11m31s   !NEXT!        
qual=  0
disk=off
stop=06h12m19s   !NEXT!

!* --- Scan from 06h12m31s to 06h13m19s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=06h12m31s   !NEXT!        
qual=  0
disk=off
stop=06h13m19s   !NEXT!

!* --- Scan from 06h13m35s to 06h14m23s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=06h13m35s   !NEXT!        
qual=  0
disk=off
stop=06h14m23s   !NEXT!

!* --- Scan from 06h14m37s to 06h15m25s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=06h14m37s   !NEXT!        
qual=  0
disk=off
stop=06h15m25s   !NEXT!

!* --- Scan from 06h15m41s to 06h16m28s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=06h15m41s   !NEXT!        
qual=  0
disk=off
stop=06h16m28s   !NEXT!

!* --- Scan from 06h16m42s to 06h17m30s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=06h16m42s   !NEXT!        
qual=  0
disk=off
stop=06h17m30s   !NEXT!

!* --- Scan from 06h17m52s to 06h18m40s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=06h17m52s   !NEXT!        
qual=  0
disk=off
stop=06h18m40s   !NEXT!

!* --- Scan from 06h18m56s to 06h19m43s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=06h18m56s   !NEXT!        
qual=  0
disk=off
stop=06h19m43s   !NEXT!

!* --- Scan from 06h20m11s to 06h20m59s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=06h20m11s   !NEXT!        
qual=  0
disk=off
stop=06h20m59s   !NEXT!

!* --- Scan from 06h21m09s to 06h21m57s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=06h21m09s   !NEXT!        
qual=  0
disk=off
stop=06h21m57s   !NEXT!

!* --- Scan from 06h22m36s to 06h23m24s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=06h22m36s   !NEXT!        
qual=  0
disk=off
stop=06h23m24s   !NEXT!

!* --- Scan from 06h24m00s to 06h24m48s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=06h24m00s   !NEXT!        
qual=  0
disk=off
stop=06h24m48s   !NEXT!

!* --- Scan from 06h25m01s to 06h25m48s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=06h25m01s   !NEXT!        
qual=  0
disk=off
stop=06h25m48s   !NEXT!

!* --- Scan from 06h25m56s to 06h26m44s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=06h25m56s   !NEXT!        
qual=  0
disk=off
stop=06h26m44s   !NEXT!

!* --- Scan from 06h26m59s to 06h27m47s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=06h26m59s   !NEXT!        
qual=  0
disk=off
stop=06h27m47s   !NEXT!

!* --- Scan from 06h27m56s to 06h28m44s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=06h27m56s   !NEXT!        
qual=  0
disk=off
stop=06h28m44s   !NEXT!

!* --- Scan from 06h28m58s to 06h29m46s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=06h28m58s   !NEXT!        
qual=  0
disk=off
stop=06h29m46s   !NEXT!

!* --- Scan from 06h29m58s to 06h30m46s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=06h29m58s   !NEXT!        
qual=  0
disk=off
stop=06h30m46s   !NEXT!

!* --- Scan from 06h32m13s to 06h34m13s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=06h32m13s   !NEXT!        
qual=  0
disk=off
stop=06h34m13s   !NEXT!

!* --- Scan from 06h35m03s to 06h35m51s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=06h35m03s   !NEXT!        
qual=  0
disk=off
stop=06h35m51s   !NEXT!

!* --- Scan from 06h36m05s to 06h36m53s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=06h36m05s   !NEXT!        
qual=  0
disk=off
stop=06h36m53s   !NEXT!

!* --- Scan from 06h37m44s to 06h38m32s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=06h37m44s   !NEXT!        
qual=  0
disk=off
stop=06h38m32s   !NEXT!

!* --- Scan from 06h38m47s to 06h39m35s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=06h38m47s   !NEXT!        
qual=  0
disk=off
stop=06h39m35s   !NEXT!

!* --- Scan from 06h39m59s to 06h40m47s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=06h39m59s   !NEXT!        
qual=  0
disk=off
stop=06h40m47s   !NEXT!

!* --- Scan from 06h40m59s to 06h41m47s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=06h40m59s   !NEXT!        
qual=  0
disk=off
stop=06h41m47s   !NEXT!

!* --- Scan from 06h42m03s to 06h42m50s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=06h42m03s   !NEXT!        
qual=  0
disk=off
stop=06h42m50s   !NEXT!

!* --- Scan from 06h43m05s to 06h43m53s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=06h43m05s   !NEXT!        
qual=  0
disk=off
stop=06h43m53s   !NEXT!

!* --- Scan from 06h44m04s to 06h44m52s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=06h44m04s   !NEXT!        
qual=  0
disk=off
stop=06h44m52s   !NEXT!

!* --- Scan from 06h45m02s to 06h45m49s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=06h45m02s   !NEXT!        
qual=  0
disk=off
stop=06h45m49s   !NEXT!

!* --- Scan from 06h46m02s to 06h46m50s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=06h46m02s   !NEXT!        
qual=  0
disk=off
stop=06h46m50s   !NEXT!

!* --- Scan from 06h47m53s to 06h48m41s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=06h47m53s   !NEXT!        
qual=  0
disk=off
stop=06h48m41s   !NEXT!

!* --- Scan from 06h48m54s to 06h49m42s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=06h48m54s   !NEXT!        
qual=  0
disk=off
stop=06h49m42s   !NEXT!

!* --- Scan from 06h49m51s to 06h50m39s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=06h49m51s   !NEXT!        
qual=  0
disk=off
stop=06h50m39s   !NEXT!

!* --- Scan from 06h51m02s to 06h51m50s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=06h51m02s   !NEXT!        
qual=  0
disk=off
stop=06h51m50s   !NEXT!

!* --- Scan from 06h52m05s to 06h52m53s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=06h52m05s   !NEXT!        
qual=  0
disk=off
stop=06h52m53s   !NEXT!

!* --- Scan from 06h53m09s to 06h53m57s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=06h53m09s   !NEXT!        
qual=  0
disk=off
stop=06h53m57s   !NEXT!

!* --- Scan from 06h54m10s to 06h56m09s   Thu, 2006 Feb 16 --- *!
sname='J0854+2006'  ra=08h54m48.874924s  dec= 20d06'30.64088"  qual=999  calib='N'
disk=off
stop=06h54m10s   !NEXT!        
qual=  0
disk=off
stop=06h56m09s   !NEXT!

!* --- Scan from 06h56m52s to 06h57m40s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=06h56m52s   !NEXT!        
qual=  0
disk=off
stop=06h57m40s   !NEXT!

!* --- Scan from 06h57m50s to 06h58m38s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=06h57m50s   !NEXT!        
qual=  0
disk=off
stop=06h58m38s   !NEXT!

!* --- Scan from 06h58m52s to 06h59m40s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=06h58m52s   !NEXT!        
qual=  0
disk=off
stop=06h59m40s   !NEXT!

!* --- Scan from 06h59m56s to 07h00m44s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=06h59m56s   !NEXT!        
qual=  0
disk=off
stop=07h00m44s   !NEXT!

!* --- Scan from 07h00m58s to 07h01m46s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=07h00m58s   !NEXT!        
qual=  0
disk=off
stop=07h01m46s   !NEXT!

!* --- Scan from 07h01m56s to 07h02m44s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=07h01m56s   !NEXT!        
qual=  0
disk=off
stop=07h02m44s   !NEXT!

!* --- Scan from 07h02m56s to 07h03m44s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=07h02m56s   !NEXT!        
qual=  0
disk=off
stop=07h03m44s   !NEXT!

!* --- Scan from 07h03m54s to 07h04m42s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=07h03m54s   !NEXT!        
qual=  0
disk=off
stop=07h04m42s   !NEXT!

!* --- Scan from 07h04m52s to 07h05m39s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=07h04m52s   !NEXT!        
qual=  0
disk=off
stop=07h05m39s   !NEXT!

!* --- Scan from 07h05m50s to 07h06m38s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=07h05m50s   !NEXT!        
qual=  0
disk=off
stop=07h06m38s   !NEXT!

!* --- Scan from 07h06m49s to 07h07m37s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=07h06m49s   !NEXT!        
qual=  0
disk=off
stop=07h07m37s   !NEXT!

!* --- Scan from 07h07m47s to 07h08m35s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=07h07m47s   !NEXT!        
qual=  0
disk=off
stop=07h08m35s   !NEXT!

!* --- Scan from 07h08m48s to 07h09m35s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=07h08m48s   !NEXT!        
qual=  0
disk=off
stop=07h09m35s   !NEXT!

!* --- Scan from 07h09m46s to 07h10m34s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=07h09m46s   !NEXT!        
qual=  0
disk=off
stop=07h10m34s   !NEXT!

!* --- Scan from 07h10m50s to 07h11m37s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=07h10m50s   !NEXT!        
qual=  0
disk=off
stop=07h11m37s   !NEXT!

!* --- Scan from 07h11m51s to 07h12m39s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=07h11m51s   !NEXT!        
qual=  0
disk=off
stop=07h12m39s   !NEXT!

!* --- Scan from 07h12m53s to 07h13m41s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=07h12m53s   !NEXT!        
qual=  0
disk=off
stop=07h13m41s   !NEXT!

!* --- Scan from 07h13m53s to 07h14m41s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=07h13m53s   !NEXT!        
qual=  0
disk=off
stop=07h14m41s   !NEXT!

!* --- Scan from 07h14m52s to 07h15m40s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=07h14m52s   !NEXT!        
qual=  0
disk=off
stop=07h15m40s   !NEXT!

!* --- Scan from 07h15m50s to 07h16m38s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=07h15m50s   !NEXT!        
qual=  0
disk=off
stop=07h16m38s   !NEXT!

!* --- Scan from 07h16m49s to 07h17m37s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=07h16m49s   !NEXT!        
qual=  0
disk=off
stop=07h17m37s   !NEXT!

!* --- Scan from 07h17m58s to 07h18m45s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=07h17m58s   !NEXT!        
qual=  0
disk=off
stop=07h18m45s   !NEXT!

!* --- Scan from 07h18m55s to 07h19m43s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=07h18m55s   !NEXT!        
qual=  0
disk=off
stop=07h19m43s   !NEXT!

!* --- Scan from 07h19m54s to 07h20m42s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=07h19m54s   !NEXT!        
qual=  0
disk=off
stop=07h20m42s   !NEXT!

!* --- Scan from 07h20m53s to 07h21m41s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=07h20m53s   !NEXT!        
qual=  0
disk=off
stop=07h21m41s   !NEXT!

!* --- Scan from 07h21m51s to 07h22m39s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=07h21m51s   !NEXT!        
qual=  0
disk=off
stop=07h22m39s   !NEXT!

!* --- Scan from 07h23m10s to 07h23m58s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=07h23m10s   !NEXT!        
qual=  0
disk=off
stop=07h23m58s   !NEXT!

!* --- Scan from 07h24m09s to 07h24m57s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=07h24m09s   !NEXT!        
qual=  0
disk=off
stop=07h24m57s   !NEXT!

!* --- Scan from 07h25m14s to 07h26m02s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=07h25m14s   !NEXT!        
qual=  0
disk=off
stop=07h26m02s   !NEXT!

!* --- Scan from 07h26m11s to 07h26m59s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=07h26m11s   !NEXT!        
qual=  0
disk=off
stop=07h26m59s   !NEXT!

!* --- Scan from 07h27m18s to 07h28m06s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=07h27m18s   !NEXT!        
qual=  0
disk=off
stop=07h28m06s   !NEXT!

!* --- Scan from 07h28m16s to 07h29m04s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=07h28m16s   !NEXT!        
qual=  0
disk=off
stop=07h29m04s   !NEXT!

!* --- Scan from 07h29m19s to 07h30m07s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=07h29m19s   !NEXT!        
qual=  0
disk=off
stop=07h30m07s   !NEXT!

!* --- Scan from 07h30m18s to 07h31m06s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=07h30m18s   !NEXT!        
qual=  0
disk=off
stop=07h31m06s   !NEXT!

!* --- Scan from 07h31m16s to 07h32m04s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=07h31m16s   !NEXT!        
qual=  0
disk=off
stop=07h32m04s   !NEXT!

!* --- Scan from 07h32m15s to 07h33m03s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=07h32m15s   !NEXT!        
qual=  0
disk=off
stop=07h33m03s   !NEXT!

!* --- Scan from 07h33m11s to 07h33m59s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=07h33m11s   !NEXT!        
qual=  0
disk=off
stop=07h33m59s   !NEXT!

!* --- Scan from 07h34m09s to 07h34m57s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=07h34m09s   !NEXT!        
qual=  0
disk=off
stop=07h34m57s   !NEXT!

!* --- Scan from 07h35m08s to 07h35m56s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=07h35m08s   !NEXT!        
qual=  0
disk=off
stop=07h35m56s   !NEXT!

!* --- Scan from 07h36m21s to 07h37m09s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=07h36m21s   !NEXT!        
qual=  0
disk=off
stop=07h37m09s   !NEXT!

!* --- Scan from 07h37m17s to 07h38m04s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=07h37m17s   !NEXT!        
qual=  0
disk=off
stop=07h38m04s   !NEXT!

!* --- Scan from 07h39m31s to 07h41m31s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=07h39m31s   !NEXT!        
qual=  0
disk=off
stop=07h41m31s   !NEXT!

!* --- Scan from 07h42m51s to 07h43m39s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=07h42m51s   !NEXT!        
qual=  0
disk=off
stop=07h43m39s   !NEXT!

!* --- Scan from 07h43m51s to 07h44m39s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=07h43m51s   !NEXT!        
qual=  0
disk=off
stop=07h44m39s   !NEXT!

!* --- Scan from 07h44m50s to 07h45m38s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=07h44m50s   !NEXT!        
qual=  0
disk=off
stop=07h45m38s   !NEXT!

!* --- Scan from 07h45m49s to 07h46m37s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=07h45m49s   !NEXT!        
qual=  0
disk=off
stop=07h46m37s   !NEXT!

!* --- Scan from 07h46m46s to 07h47m34s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=07h46m46s   !NEXT!        
qual=  0
disk=off
stop=07h47m34s   !NEXT!

!* --- Scan from 07h47m44s to 07h48m31s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=07h47m44s   !NEXT!        
qual=  0
disk=off
stop=07h48m31s   !NEXT!

!* --- Scan from 07h48m50s to 07h49m38s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=07h48m50s   !NEXT!        
qual=  0
disk=off
stop=07h49m38s   !NEXT!

!* --- Scan from 07h49m46s to 07h50m33s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=07h49m46s   !NEXT!        
qual=  0
disk=off
stop=07h50m33s   !NEXT!

!* --- Scan from 07h50m48s to 07h51m36s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=07h50m48s   !NEXT!        
qual=  0
disk=off
stop=07h51m36s   !NEXT!

!* --- Scan from 07h51m56s to 07h52m43s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=07h51m56s   !NEXT!        
qual=  0
disk=off
stop=07h52m43s   !NEXT!

!* --- Scan from 07h52m59s to 07h53m47s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=07h52m59s   !NEXT!        
qual=  0
disk=off
stop=07h53m47s   !NEXT!

!* --- Scan from 07h54m10s to 07h54m58s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=07h54m10s   !NEXT!        
qual=  0
disk=off
stop=07h54m58s   !NEXT!

!* --- Scan from 07h55m06s to 07h55m54s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=07h55m06s   !NEXT!        
qual=  0
disk=off
stop=07h55m54s   !NEXT!

!* --- Scan from 07h56m04s to 07h56m52s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=07h56m04s   !NEXT!        
qual=  0
disk=off
stop=07h56m52s   !NEXT!

!* --- Scan from 07h57m47s to 07h58m35s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=07h57m47s   !NEXT!        
qual=  0
disk=off
stop=07h58m35s   !NEXT!

!* --- Scan from 07h58m46s to 07h59m34s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=07h58m46s   !NEXT!        
qual=  0
disk=off
stop=07h59m34s   !NEXT!

!* --- Scan from 07h59m48s to 08h00m36s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=07h59m48s   !NEXT!        
qual=  0
disk=off
stop=08h00m36s   !NEXT!

!* --- Scan from 08h00m52s to 08h01m40s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=08h00m52s   !NEXT!        
qual=  0
disk=off
stop=08h01m40s   !NEXT!

!* --- Scan from 08h01m53s to 08h02m41s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=08h01m53s   !NEXT!        
qual=  0
disk=off
stop=08h02m41s   !NEXT!

!* --- Scan from 08h02m55s to 08h03m43s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=08h02m55s   !NEXT!        
qual=  0
disk=off
stop=08h03m43s   !NEXT!

!* --- Scan from 08h04m06s to 08h04m54s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=08h04m06s   !NEXT!        
qual=  0
disk=off
stop=08h04m54s   !NEXT!

!* --- Scan from 08h05m04s to 08h05m52s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=08h05m04s   !NEXT!        
qual=  0
disk=off
stop=08h05m52s   !NEXT!

!* --- Scan from 08h06m07s to 08h06m55s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=08h06m07s   !NEXT!        
qual=  0
disk=off
stop=08h06m55s   !NEXT!

!* --- Scan from 08h07m09s to 08h07m57s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=08h07m09s   !NEXT!        
qual=  0
disk=off
stop=08h07m57s   !NEXT!

!* --- Scan from 08h08m07s to 08h08m55s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=08h08m07s   !NEXT!        
qual=  0
disk=off
stop=08h08m55s   !NEXT!

!* --- Scan from 08h09m07s to 08h09m55s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=08h09m07s   !NEXT!        
qual=  0
disk=off
stop=08h09m55s   !NEXT!

!* --- Scan from 08h10m08s to 08h10m56s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=08h10m08s   !NEXT!        
qual=  0
disk=off
stop=08h10m56s   !NEXT!

!* --- Scan from 08h11m07s to 08h11m54s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=08h11m07s   !NEXT!        
qual=  0
disk=off
stop=08h11m54s   !NEXT!

!* --- Scan from 08h12m07s to 08h12m54s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=08h12m07s   !NEXT!        
qual=  0
disk=off
stop=08h12m54s   !NEXT!

!* --- Scan from 08h13m06s to 08h13m54s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=08h13m06s   !NEXT!        
qual=  0
disk=off
stop=08h13m54s   !NEXT!

!* --- Scan from 08h14m05s to 08h14m53s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=08h14m05s   !NEXT!        
qual=  0
disk=off
stop=08h14m53s   !NEXT!

!* --- Scan from 08h15m07s to 08h15m55s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=08h15m07s   !NEXT!        
qual=  0
disk=off
stop=08h15m55s   !NEXT!

!* --- Scan from 08h16m10s to 08h16m58s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=08h16m10s   !NEXT!        
qual=  0
disk=off
stop=08h16m58s   !NEXT!

!* --- Scan from 08h19m20s to 08h21m20s   Thu, 2006 Feb 16 --- *!
sname='J1310+3220'  ra=13h10m28.663845s  dec= 32d20'43.78295"  qual=999  calib='N'
disk=off
stop=08h19m20s   !NEXT!        
qual=  0
disk=off
stop=08h21m20s   !NEXT!

!* --- Scan from 08h23m44s to 08h24m32s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=08h23m44s   !NEXT!        
qual=  0
disk=off
stop=08h24m32s   !NEXT!

!* --- Scan from 08h24m43s to 08h25m31s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=08h24m43s   !NEXT!        
qual=  0
disk=off
stop=08h25m31s   !NEXT!

!* --- Scan from 08h25m44s to 08h26m32s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=08h25m44s   !NEXT!        
qual=  0
disk=off
stop=08h26m32s   !NEXT!

!* --- Scan from 08h26m41s to 08h27m29s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=08h26m41s   !NEXT!        
qual=  0
disk=off
stop=08h27m29s   !NEXT!

!* --- Scan from 08h27m49s to 08h28m37s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=08h27m49s   !NEXT!        
qual=  0
disk=off
stop=08h28m37s   !NEXT!

!* --- Scan from 08h28m48s to 08h29m36s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=08h28m48s   !NEXT!        
qual=  0
disk=off
stop=08h29m36s   !NEXT!

!* --- Scan from 08h29m50s to 08h30m38s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=08h29m50s   !NEXT!        
qual=  0
disk=off
stop=08h30m38s   !NEXT!

!* --- Scan from 08h30m50s to 08h31m38s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=08h30m50s   !NEXT!        
qual=  0
disk=off
stop=08h31m38s   !NEXT!

!* --- Scan from 08h31m52s to 08h32m40s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=08h31m52s   !NEXT!        
qual=  0
disk=off
stop=08h32m40s   !NEXT!

!* --- Scan from 08h32m53s to 08h33m41s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=08h32m53s   !NEXT!        
qual=  0
disk=off
stop=08h33m41s   !NEXT!

!* --- Scan from 08h33m57s to 08h34m45s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=08h33m57s   !NEXT!        
qual=  0
disk=off
stop=08h34m45s   !NEXT!

!* --- Scan from 08h34m58s to 08h35m45s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=08h34m58s   !NEXT!        
qual=  0
disk=off
stop=08h35m45s   !NEXT!

!* --- Scan from 08h36m02s to 08h36m50s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=08h36m02s   !NEXT!        
qual=  0
disk=off
stop=08h36m50s   !NEXT!

!* --- Scan from 08h38m09s to 08h38m57s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=08h38m09s   !NEXT!        
qual=  0
disk=off
stop=08h38m57s   !NEXT!

!* --- Scan from 08h39m09s to 08h39m57s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=08h39m09s   !NEXT!        
qual=  0
disk=off
stop=08h39m57s   !NEXT!

!* --- Scan from 08h40m05s to 08h40m53s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=08h40m05s   !NEXT!        
qual=  0
disk=off
stop=08h40m53s   !NEXT!

!* --- Scan from 08h41m06s to 08h41m54s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=08h41m06s   !NEXT!        
qual=  0
disk=off
stop=08h41m54s   !NEXT!

!* --- Scan from 08h42m10s to 08h42m58s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=08h42m10s   !NEXT!        
qual=  0
disk=off
stop=08h42m58s   !NEXT!

!* --- Scan from 08h45m02s to 08h47m01s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=08h45m02s   !NEXT!        
qual=  0
disk=off
stop=08h47m01s   !NEXT!

!* --- Scan from 08h49m08s to 08h51m08s   Thu, 2006 Feb 16 --- *!
sname='3C279'  ra=12h56m11.166560s  dec=-05d47'21.52458"  qual=999  calib='N'
disk=off
stop=08h49m08s   !NEXT!        
qual=  0
disk=off
stop=08h51m08s   !NEXT!

!* --- Scan from 08h53m46s to 08h54m34s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=08h53m46s   !NEXT!        
qual=  0
disk=off
stop=08h54m34s   !NEXT!

!* --- Scan from 08h54m52s to 08h55m40s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=08h54m52s   !NEXT!        
qual=  0
disk=off
stop=08h55m40s   !NEXT!

!* --- Scan from 08h55m53s to 08h56m41s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=08h55m53s   !NEXT!        
qual=  0
disk=off
stop=08h56m41s   !NEXT!

!* --- Scan from 08h56m50s to 08h57m38s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=08h56m50s   !NEXT!        
qual=  0
disk=off
stop=08h57m38s   !NEXT!

!* --- Scan from 08h57m48s to 08h58m36s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=08h57m48s   !NEXT!        
qual=  0
disk=off
stop=08h58m36s   !NEXT!

!* --- Scan from 08h58m53s to 08h59m40s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=08h58m53s   !NEXT!        
qual=  0
disk=off
stop=08h59m40s   !NEXT!

!* --- Scan from 09h00m24s to 09h01m12s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=09h00m24s   !NEXT!        
qual=  0
disk=off
stop=09h01m12s   !NEXT!

!* --- Scan from 09h01m34s to 09h02m22s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=09h01m34s   !NEXT!        
qual=  0
disk=off
stop=09h02m22s   !NEXT!

!* --- Scan from 09h02m31s to 09h03m19s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=09h02m31s   !NEXT!        
qual=  0
disk=off
stop=09h03m19s   !NEXT!

!* --- Scan from 09h03m31s to 09h04m19s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=09h03m31s   !NEXT!        
qual=  0
disk=off
stop=09h04m19s   !NEXT!

!* --- Scan from 09h04m44s to 09h05m31s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=09h04m44s   !NEXT!        
qual=  0
disk=off
stop=09h05m31s   !NEXT!

!* --- Scan from 09h05m47s to 09h06m35s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=09h05m47s   !NEXT!        
qual=  0
disk=off
stop=09h06m35s   !NEXT!

!* --- Scan from 09h08m27s to 09h09m15s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=09h08m27s   !NEXT!        
qual=  0
disk=off
stop=09h09m15s   !NEXT!

!* --- Scan from 09h09m25s to 09h10m13s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=09h09m25s   !NEXT!        
qual=  0
disk=off
stop=09h10m13s   !NEXT!

!* --- Scan from 09h10m27s to 09h11m15s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=09h10m27s   !NEXT!        
qual=  0
disk=off
stop=09h11m15s   !NEXT!

!* --- Scan from 09h11m33s to 09h12m21s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=09h11m33s   !NEXT!        
qual=  0
disk=off
stop=09h12m21s   !NEXT!

!* --- Scan from 09h12m35s to 09h13m23s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=09h12m35s   !NEXT!        
qual=  0
disk=off
stop=09h13m23s   !NEXT!

!* --- Scan from 09h13m36s to 09h14m24s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=09h13m36s   !NEXT!        
qual=  0
disk=off
stop=09h14m24s   !NEXT!

!* --- Scan from 09h14m38s to 09h15m26s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=09h14m38s   !NEXT!        
qual=  0
disk=off
stop=09h15m26s   !NEXT!

!* --- Scan from 09h15m36s to 09h16m24s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=09h15m36s   !NEXT!        
qual=  0
disk=off
stop=09h16m24s   !NEXT!

!* --- Scan from 09h16m37s to 09h17m24s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=09h16m37s   !NEXT!        
qual=  0
disk=off
stop=09h17m24s   !NEXT!

!* --- Scan from 09h17m34s to 09h18m22s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=09h17m34s   !NEXT!        
qual=  0
disk=off
stop=09h18m22s   !NEXT!

!* --- Scan from 09h18m34s to 09h19m22s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=09h18m34s   !NEXT!        
qual=  0
disk=off
stop=09h19m22s   !NEXT!

!* --- Scan from 09h19m33s to 09h20m21s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=09h19m33s   !NEXT!        
qual=  0
disk=off
stop=09h20m21s   !NEXT!

!* --- Scan from 09h20m33s to 09h21m21s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=09h20m33s   !NEXT!        
qual=  0
disk=off
stop=09h21m21s   !NEXT!

!* --- Scan from 09h21m31s to 09h22m19s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=09h21m31s   !NEXT!        
qual=  0
disk=off
stop=09h22m19s   !NEXT!

!* --- Scan from 09h22m31s to 09h23m19s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=09h22m31s   !NEXT!        
qual=  0
disk=off
stop=09h23m19s   !NEXT!

!* --- Scan from 09h23m29s to 09h24m17s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=09h23m29s   !NEXT!        
qual=  0
disk=off
stop=09h24m17s   !NEXT!

!* --- Scan from 09h24m30s to 09h25m18s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=09h24m30s   !NEXT!        
qual=  0
disk=off
stop=09h25m18s   !NEXT!

!* --- Scan from 09h25m28s to 09h26m16s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=09h25m28s   !NEXT!        
qual=  0
disk=off
stop=09h26m16s   !NEXT!

!* --- Scan from 09h26m28s to 09h27m16s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=09h26m28s   !NEXT!        
qual=  0
disk=off
stop=09h27m16s   !NEXT!

!* --- Scan from 09h29m33s to 09h31m33s   Thu, 2006 Feb 16 --- *!
sname='J1310+3220'  ra=13h10m28.663845s  dec= 32d20'43.78295"  qual=999  calib='N'
disk=off
stop=09h29m33s   !NEXT!        
qual=  0
disk=off
stop=09h31m33s   !NEXT!

!* --- Scan from 09h33m50s to 09h34m38s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=09h33m50s   !NEXT!        
qual=  0
disk=off
stop=09h34m38s   !NEXT!

!* --- Scan from 09h34m51s to 09h35m38s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=09h34m51s   !NEXT!        
qual=  0
disk=off
stop=09h35m38s   !NEXT!

!* --- Scan from 09h35m58s to 09h36m46s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=09h35m58s   !NEXT!        
qual=  0
disk=off
stop=09h36m46s   !NEXT!

!* --- Scan from 09h36m55s to 09h37m43s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=09h36m55s   !NEXT!        
qual=  0
disk=off
stop=09h37m43s   !NEXT!

!* --- Scan from 09h38m03s to 09h38m51s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=09h38m03s   !NEXT!        
qual=  0
disk=off
stop=09h38m51s   !NEXT!

!* --- Scan from 09h39m00s to 09h39m47s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=09h39m00s   !NEXT!        
qual=  0
disk=off
stop=09h39m47s   !NEXT!

!* --- Scan from 09h39m58s to 09h40m46s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=09h39m58s   !NEXT!        
qual=  0
disk=off
stop=09h40m46s   !NEXT!

!* --- Scan from 09h43m56s to 09h45m55s   Thu, 2006 Feb 16 --- *!
sname='3C279'  ra=12h56m11.166560s  dec=-05d47'21.52458"  qual=999  calib='N'
disk=off
stop=09h43m56s   !NEXT!        
qual=  0
disk=off
stop=09h45m55s   !NEXT!

!* --- Scan from 09h47m54s to 09h48m42s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=09h47m54s   !NEXT!        
qual=  0
disk=off
stop=09h48m42s   !NEXT!

!* --- Scan from 09h48m52s to 09h49m39s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=09h48m52s   !NEXT!        
qual=  0
disk=off
stop=09h49m39s   !NEXT!

!* --- Scan from 09h49m54s to 09h50m41s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=09h49m54s   !NEXT!        
qual=  0
disk=off
stop=09h50m41s   !NEXT!

!* --- Scan from 09h50m58s to 09h51m46s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=09h50m58s   !NEXT!        
qual=  0
disk=off
stop=09h51m46s   !NEXT!

!* --- Scan from 09h51m59s to 09h52m47s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=09h51m59s   !NEXT!        
qual=  0
disk=off
stop=09h52m47s   !NEXT!

!* --- Scan from 09h53m03s to 09h53m51s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=09h53m03s   !NEXT!        
qual=  0
disk=off
stop=09h53m51s   !NEXT!

!* --- Scan from 09h54m01s to 09h54m49s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=09h54m01s   !NEXT!        
qual=  0
disk=off
stop=09h54m49s   !NEXT!

!* --- Scan from 09h54m59s to 09h55m47s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=09h54m59s   !NEXT!        
qual=  0
disk=off
stop=09h55m47s   !NEXT!

!* --- Scan from 09h55m59s to 09h56m47s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=09h55m59s   !NEXT!        
qual=  0
disk=off
stop=09h56m47s   !NEXT!

!* --- Scan from 09h56m57s to 09h57m45s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=09h56m57s   !NEXT!        
qual=  0
disk=off
stop=09h57m45s   !NEXT!

!* --- Scan from 09h58m02s to 09h58m50s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=09h58m02s   !NEXT!        
qual=  0
disk=off
stop=09h58m50s   !NEXT!

!* --- Scan from 09h59m00s to 09h59m48s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=09h59m00s   !NEXT!        
qual=  0
disk=off
stop=09h59m48s   !NEXT!

!* --- Scan from 09h59m58s to 10h00m46s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=09h59m58s   !NEXT!        
qual=  0
disk=off
stop=10h00m46s   !NEXT!

!* --- Scan from 10h00m56s to 10h01m44s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=10h00m56s   !NEXT!        
qual=  0
disk=off
stop=10h01m44s   !NEXT!

!* --- Scan from 10h01m55s to 10h02m43s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=10h01m55s   !NEXT!        
qual=  0
disk=off
stop=10h02m43s   !NEXT!

!* --- Scan from 10h03m00s to 10h03m47s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=10h03m00s   !NEXT!        
qual=  0
disk=off
stop=10h03m47s   !NEXT!

!* --- Scan from 10h04m06s to 10h04m54s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=10h04m06s   !NEXT!        
qual=  0
disk=off
stop=10h04m54s   !NEXT!

!* --- Scan from 10h05m04s to 10h05m52s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=10h05m04s   !NEXT!        
qual=  0
disk=off
stop=10h05m52s   !NEXT!

!* --- Scan from 10h06m00s to 10h06m48s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=10h06m00s   !NEXT!        
qual=  0
disk=off
stop=10h06m48s   !NEXT!

!* --- Scan from 10h06m59s to 10h07m47s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=10h06m59s   !NEXT!        
qual=  0
disk=off
stop=10h07m47s   !NEXT!

!* --- Scan from 10h08m01s to 10h08m49s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=10h08m01s   !NEXT!        
qual=  0
disk=off
stop=10h08m49s   !NEXT!

!* --- Scan from 10h09m04s to 10h09m52s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=10h09m04s   !NEXT!        
qual=  0
disk=off
stop=10h09m52s   !NEXT!

!* --- Scan from 10h10m07s to 10h10m54s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=10h10m07s   !NEXT!        
qual=  0
disk=off
stop=10h10m54s   !NEXT!

!* --- Scan from 10h11m18s to 10h12m06s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=10h11m18s   !NEXT!        
qual=  0
disk=off
stop=10h12m06s   !NEXT!

!* --- Scan from 10h12m20s to 10h13m08s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=10h12m20s   !NEXT!        
qual=  0
disk=off
stop=10h13m08s   !NEXT!

!* --- Scan from 10h13m19s to 10h14m07s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=10h13m19s   !NEXT!        
qual=  0
disk=off
stop=10h14m07s   !NEXT!

!* --- Scan from 10h14m14s to 10h15m02s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=10h14m14s   !NEXT!        
qual=  0
disk=off
stop=10h15m02s   !NEXT!

!* --- Scan from 10h15m16s to 10h16m04s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=10h15m16s   !NEXT!        
qual=  0
disk=off
stop=10h16m04s   !NEXT!

!* --- Scan from 10h16m18s to 10h17m05s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=10h16m18s   !NEXT!        
qual=  0
disk=off
stop=10h17m05s   !NEXT!

!* --- Scan from 10h17m16s to 10h18m04s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=10h17m16s   !NEXT!        
qual=  0
disk=off
stop=10h18m04s   !NEXT!

!* --- Scan from 10h18m18s to 10h19m05s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=10h18m18s   !NEXT!        
qual=  0
disk=off
stop=10h19m05s   !NEXT!

!* --- Scan from 10h19m17s to 10h20m05s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=10h19m17s   !NEXT!        
qual=  0
disk=off
stop=10h20m05s   !NEXT!

!* --- Scan from 10h20m15s to 10h21m03s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=10h20m15s   !NEXT!        
qual=  0
disk=off
stop=10h21m03s   !NEXT!

!* --- Scan from 10h21m14s to 10h22m02s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=10h21m14s   !NEXT!        
qual=  0
disk=off
stop=10h22m02s   !NEXT!

!* --- Scan from 10h22m12s to 10h23m00s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=10h22m12s   !NEXT!        
qual=  0
disk=off
stop=10h23m00s   !NEXT!

!* --- Scan from 10h23m12s to 10h24m00s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=10h23m12s   !NEXT!        
qual=  0
disk=off
stop=10h24m00s   !NEXT!

!* --- Scan from 10h24m10s to 10h24m58s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=10h24m10s   !NEXT!        
qual=  0
disk=off
stop=10h24m58s   !NEXT!

!* --- Scan from 10h25m14s to 10h26m02s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=10h25m14s   !NEXT!        
qual=  0
disk=off
stop=10h26m02s   !NEXT!

!* --- Scan from 10h26m19s to 10h27m07s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=10h26m19s   !NEXT!        
qual=  0
disk=off
stop=10h27m07s   !NEXT!

!* --- Scan from 10h27m20s to 10h28m08s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=10h27m20s   !NEXT!        
qual=  0
disk=off
stop=10h28m08s   !NEXT!

!* --- Scan from 10h28m21s to 10h29m09s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=10h28m21s   !NEXT!        
qual=  0
disk=off
stop=10h29m09s   !NEXT!

!* --- Scan from 10h29m22s to 10h30m10s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=10h29m22s   !NEXT!        
qual=  0
disk=off
stop=10h30m10s   !NEXT!

!* --- Scan from 10h30m21s to 10h31m09s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=10h30m21s   !NEXT!        
qual=  0
disk=off
stop=10h31m09s   !NEXT!

!* --- Scan from 10h31m22s to 10h32m10s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=10h31m22s   !NEXT!        
qual=  0
disk=off
stop=10h32m10s   !NEXT!

!* --- Scan from 10h32m20s to 10h33m08s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=10h32m20s   !NEXT!        
qual=  0
disk=off
stop=10h33m08s   !NEXT!

!* --- Scan from 10h35m17s to 10h37m17s   Thu, 2006 Feb 16 --- *!
sname='J1310+3220'  ra=13h10m28.663845s  dec= 32d20'43.78295"  qual=999  calib='N'
disk=off
stop=10h35m17s   !NEXT!        
qual=  0
disk=off
stop=10h37m17s   !NEXT!

!* --- Scan from 10h40m57s to 10h42m56s   Thu, 2006 Feb 16 --- *!
sname='3C279'  ra=12h56m11.166560s  dec=-05d47'21.52458"  qual=999  calib='N'
disk=off
stop=10h40m57s   !NEXT!        
qual=  0
disk=off
stop=10h42m56s   !NEXT!

!* --- Scan from 10h45m06s to 10h45m54s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=10h45m06s   !NEXT!        
qual=  0
disk=off
stop=10h45m54s   !NEXT!

!* --- Scan from 10h46m05s to 10h46m53s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=10h46m05s   !NEXT!        
qual=  0
disk=off
stop=10h46m53s   !NEXT!

!* --- Scan from 10h47m03s to 10h47m51s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=10h47m03s   !NEXT!        
qual=  0
disk=off
stop=10h47m51s   !NEXT!

!* --- Scan from 10h48m00s to 10h48m48s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=10h48m00s   !NEXT!        
qual=  0
disk=off
stop=10h48m48s   !NEXT!

!* --- Scan from 10h48m58s to 10h49m46s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=10h48m58s   !NEXT!        
qual=  0
disk=off
stop=10h49m46s   !NEXT!

!* --- Scan from 10h49m59s to 10h50m47s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=10h49m59s   !NEXT!        
qual=  0
disk=off
stop=10h50m47s   !NEXT!

!* --- Scan from 10h50m58s to 10h51m45s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=10h50m58s   !NEXT!        
qual=  0
disk=off
stop=10h51m45s   !NEXT!

!* --- Scan from 10h52m00s to 10h52m48s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=10h52m00s   !NEXT!        
qual=  0
disk=off
stop=10h52m48s   !NEXT!

!* --- Scan from 10h52m56s to 10h53m44s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=10h52m56s   !NEXT!        
qual=  0
disk=off
stop=10h53m44s   !NEXT!

!* --- Scan from 10h53m58s to 10h54m46s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=10h53m58s   !NEXT!        
qual=  0
disk=off
stop=10h54m46s   !NEXT!

!* --- Scan from 10h54m56s to 10h55m44s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=10h54m56s   !NEXT!        
qual=  0
disk=off
stop=10h55m44s   !NEXT!

!* --- Scan from 10h55m52s to 10h56m40s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=10h55m52s   !NEXT!        
qual=  0
disk=off
stop=10h56m40s   !NEXT!

!* --- Scan from 10h57m02s to 10h57m49s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=10h57m02s   !NEXT!        
qual=  0
disk=off
stop=10h57m49s   !NEXT!

!* --- Scan from 10h57m57s to 10h58m45s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=10h57m57s   !NEXT!        
qual=  0
disk=off
stop=10h58m45s   !NEXT!
disk=off
stop=10h58m50s   !NEXT!
     !QUIT! 
