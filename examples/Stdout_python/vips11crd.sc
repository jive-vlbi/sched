!*  Schedule for VLBA_SC   *!
!*  Experiment VIPS11   *!
!* Schedule Version:       2.00 *!
!* Processed by SCHED version:  11.50  Release 11.5; September 2018 *!
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

!* --- Scan from 00h37m27s to 00h38m15s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=00h37m27s   !NEXT!        
qual=  0
disk=off
stop=00h38m15s   !NEXT!

!* --- Scan from 00h38m24s to 00h39m12s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=00h38m24s   !NEXT!        
qual=  0
disk=off
stop=00h39m12s   !NEXT!

!* --- Scan from 00h39m19s to 00h40m07s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=00h39m19s   !NEXT!        
qual=  0
disk=off
stop=00h40m07s   !NEXT!

!* --- Scan from 00h40m19s to 00h41m07s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=00h40m19s   !NEXT!        
qual=  0
disk=off
stop=00h41m07s   !NEXT!

!* --- Scan from 00h41m17s to 00h42m04s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=00h41m17s   !NEXT!        
qual=  0
disk=off
stop=00h42m04s   !NEXT!

!* --- Scan from 00h42m18s to 00h43m06s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=00h42m18s   !NEXT!        
qual=  0
disk=off
stop=00h43m06s   !NEXT!

!* --- Scan from 00h43m14s to 00h44m02s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=00h43m14s   !NEXT!        
qual=  0
disk=off
stop=00h44m02s   !NEXT!

!* --- Scan from 00h44m22s to 00h45m10s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=00h44m22s   !NEXT!        
qual=  0
disk=off
stop=00h45m10s   !NEXT!

!* --- Scan from 00h45m20s to 00h46m08s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=00h45m20s   !NEXT!        
qual=  0
disk=off
stop=00h46m08s   !NEXT!

!* --- Scan from 00h46m21s to 00h47m09s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=00h46m21s   !NEXT!        
qual=  0
disk=off
stop=00h47m09s   !NEXT!

!* --- Scan from 00h47m19s to 00h48m07s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=00h47m19s   !NEXT!        
qual=  0
disk=off
stop=00h48m07s   !NEXT!

!* --- Scan from 00h48m17s to 00h49m05s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=00h48m17s   !NEXT!        
qual=  0
disk=off
stop=00h49m05s   !NEXT!

!* --- Scan from 00h49m16s to 00h50m04s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=00h49m16s   !NEXT!        
qual=  0
disk=off
stop=00h50m04s   !NEXT!

!* --- Scan from 00h50m15s to 00h51m03s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=00h50m15s   !NEXT!        
qual=  0
disk=off
stop=00h51m03s   !NEXT!

!* --- Scan from 00h51m13s to 00h52m01s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=00h51m13s   !NEXT!        
qual=  0
disk=off
stop=00h52m01s   !NEXT!

!* --- Scan from 00h52m14s to 00h53m01s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=00h52m14s   !NEXT!        
qual=  0
disk=off
stop=00h53m01s   !NEXT!

!* --- Scan from 00h53m12s to 00h54m00s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=00h53m12s   !NEXT!        
qual=  0
disk=off
stop=00h54m00s   !NEXT!

!* --- Scan from 00h54m10s to 00h54m58s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=00h54m10s   !NEXT!        
qual=  0
disk=off
stop=00h54m58s   !NEXT!

!* --- Scan from 00h55m08s to 00h55m56s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=00h55m08s   !NEXT!        
qual=  0
disk=off
stop=00h55m56s   !NEXT!

!* --- Scan from 00h56m06s to 00h56m54s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=00h56m06s   !NEXT!        
qual=  0
disk=off
stop=00h56m54s   !NEXT!

!* --- Scan from 00h57m06s to 00h57m54s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=00h57m06s   !NEXT!        
qual=  0
disk=off
stop=00h57m54s   !NEXT!

!* --- Scan from 00h58m06s to 00h58m54s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=00h58m06s   !NEXT!        
qual=  0
disk=off
stop=00h58m54s   !NEXT!

!* --- Scan from 00h59m04s to 00h59m52s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=00h59m04s   !NEXT!        
qual=  0
disk=off
stop=00h59m52s   !NEXT!

!* --- Scan from 01h00m06s to 01h00m54s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=01h00m06s   !NEXT!        
qual=  0
disk=off
stop=01h00m54s   !NEXT!

!* --- Scan from 01h01m08s to 01h01m56s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=01h01m08s   !NEXT!        
qual=  0
disk=off
stop=01h01m56s   !NEXT!

!* --- Scan from 01h02m06s to 01h02m54s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=01h02m06s   !NEXT!        
qual=  0
disk=off
stop=01h02m54s   !NEXT!

!* --- Scan from 01h03m20s to 01h04m08s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=01h03m20s   !NEXT!        
qual=  0
disk=off
stop=01h04m08s   !NEXT!

!* --- Scan from 01h04m18s to 01h05m06s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=01h04m18s   !NEXT!        
qual=  0
disk=off
stop=01h05m06s   !NEXT!

!* --- Scan from 01h05m15s to 01h06m03s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=01h05m15s   !NEXT!        
qual=  0
disk=off
stop=01h06m03s   !NEXT!

!* --- Scan from 01h06m26s to 01h07m14s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=01h06m26s   !NEXT!        
qual=  0
disk=off
stop=01h07m14s   !NEXT!

!* --- Scan from 01h07m24s to 01h08m12s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=01h07m24s   !NEXT!        
qual=  0
disk=off
stop=01h08m12s   !NEXT!

!* --- Scan from 01h08m22s to 01h09m10s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=01h08m22s   !NEXT!        
qual=  0
disk=off
stop=01h09m10s   !NEXT!

!* --- Scan from 01h09m19s to 01h10m07s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=01h09m19s   !NEXT!        
qual=  0
disk=off
stop=01h10m07s   !NEXT!

!* --- Scan from 01h10m17s to 01h11m05s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=01h10m17s   !NEXT!        
qual=  0
disk=off
stop=01h11m05s   !NEXT!

!* --- Scan from 01h11m48s to 01h12m36s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=01h11m48s   !NEXT!        
qual=  0
disk=off
stop=01h12m36s   !NEXT!

!* --- Scan from 01h12m47s to 01h13m35s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=01h12m47s   !NEXT!        
qual=  0
disk=off
stop=01h13m35s   !NEXT!

!* --- Scan from 01h13m46s to 01h14m34s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=01h13m46s   !NEXT!        
qual=  0
disk=off
stop=01h14m34s   !NEXT!

!* --- Scan from 01h14m41s to 01h15m29s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=01h14m41s   !NEXT!        
qual=  0
disk=off
stop=01h15m29s   !NEXT!

!* --- Scan from 01h15m39s to 01h16m27s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=01h15m39s   !NEXT!        
qual=  0
disk=off
stop=01h16m27s   !NEXT!

!* --- Scan from 01h16m39s to 01h17m27s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=01h16m39s   !NEXT!        
qual=  0
disk=off
stop=01h17m27s   !NEXT!

!* --- Scan from 01h17m39s to 01h18m26s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=01h17m39s   !NEXT!        
qual=  0
disk=off
stop=01h18m26s   !NEXT!

!* --- Scan from 01h18m35s to 01h19m23s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=01h18m35s   !NEXT!        
qual=  0
disk=off
stop=01h19m23s   !NEXT!

!* --- Scan from 01h19m38s to 01h20m26s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=01h19m38s   !NEXT!        
qual=  0
disk=off
stop=01h20m26s   !NEXT!

!* --- Scan from 01h20m34s to 01h21m21s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=01h20m34s   !NEXT!        
qual=  0
disk=off
stop=01h21m21s   !NEXT!

!* --- Scan from 01h21m33s to 01h22m21s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=01h21m33s   !NEXT!        
qual=  0
disk=off
stop=01h22m21s   !NEXT!

!* --- Scan from 01h22m39s to 01h23m27s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=01h22m39s   !NEXT!        
qual=  0
disk=off
stop=01h23m27s   !NEXT!

!* --- Scan from 01h23m35s to 01h24m23s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=01h23m35s   !NEXT!        
qual=  0
disk=off
stop=01h24m23s   !NEXT!

!* --- Scan from 01h24m32s to 01h25m20s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=01h24m32s   !NEXT!        
qual=  0
disk=off
stop=01h25m20s   !NEXT!

!* --- Scan from 01h25m43s to 01h26m30s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=01h25m43s   !NEXT!        
qual=  0
disk=off
stop=01h26m30s   !NEXT!

!* --- Scan from 01h28m13s to 01h30m12s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=01h28m13s   !NEXT!        
qual=  0
disk=off
stop=01h30m12s   !NEXT!

!* --- Scan from 01h31m41s to 01h32m29s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=01h31m41s   !NEXT!        
qual=  0
disk=off
stop=01h32m29s   !NEXT!

!* --- Scan from 01h32m39s to 01h33m26s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=01h32m39s   !NEXT!        
qual=  0
disk=off
stop=01h33m26s   !NEXT!

!* --- Scan from 01h33m41s to 01h34m29s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=01h33m41s   !NEXT!        
qual=  0
disk=off
stop=01h34m29s   !NEXT!

!* --- Scan from 01h34m44s to 01h35m32s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=01h34m44s   !NEXT!        
qual=  0
disk=off
stop=01h35m32s   !NEXT!

!* --- Scan from 01h35m46s to 01h36m34s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=01h35m46s   !NEXT!        
qual=  0
disk=off
stop=01h36m34s   !NEXT!

!* --- Scan from 01h36m49s to 01h37m36s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=01h36m49s   !NEXT!        
qual=  0
disk=off
stop=01h37m36s   !NEXT!

!* --- Scan from 01h37m47s to 01h38m35s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=01h37m47s   !NEXT!        
qual=  0
disk=off
stop=01h38m35s   !NEXT!

!* --- Scan from 01h38m48s to 01h39m36s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=01h38m48s   !NEXT!        
qual=  0
disk=off
stop=01h39m36s   !NEXT!

!* --- Scan from 01h39m46s to 01h40m34s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=01h39m46s   !NEXT!        
qual=  0
disk=off
stop=01h40m34s   !NEXT!

!* --- Scan from 01h40m45s to 01h41m33s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=01h40m45s   !NEXT!        
qual=  0
disk=off
stop=01h41m33s   !NEXT!

!* --- Scan from 01h41m44s to 01h42m32s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=01h41m44s   !NEXT!        
qual=  0
disk=off
stop=01h42m32s   !NEXT!

!* --- Scan from 01h42m42s to 01h43m30s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=01h42m42s   !NEXT!        
qual=  0
disk=off
stop=01h43m30s   !NEXT!

!* --- Scan from 01h43m41s to 01h44m29s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=01h43m41s   !NEXT!        
qual=  0
disk=off
stop=01h44m29s   !NEXT!

!* --- Scan from 01h44m44s to 01h45m31s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=01h44m44s   !NEXT!        
qual=  0
disk=off
stop=01h45m31s   !NEXT!

!* --- Scan from 01h45m42s to 01h46m30s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=01h45m42s   !NEXT!        
qual=  0
disk=off
stop=01h46m30s   !NEXT!

!* --- Scan from 01h46m51s to 01h47m39s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=01h46m51s   !NEXT!        
qual=  0
disk=off
stop=01h47m39s   !NEXT!

!* --- Scan from 01h47m50s to 01h48m37s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=01h47m50s   !NEXT!        
qual=  0
disk=off
stop=01h48m37s   !NEXT!

!* --- Scan from 01h48m48s to 01h49m36s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=01h48m48s   !NEXT!        
qual=  0
disk=off
stop=01h49m36s   !NEXT!

!* --- Scan from 01h49m46s to 01h50m34s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=01h49m46s   !NEXT!        
qual=  0
disk=off
stop=01h50m34s   !NEXT!

!* --- Scan from 01h50m44s to 01h51m32s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=01h50m44s   !NEXT!        
qual=  0
disk=off
stop=01h51m32s   !NEXT!

!* --- Scan from 01h52m09s to 01h52m56s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=01h52m09s   !NEXT!        
qual=  0
disk=off
stop=01h52m56s   !NEXT!

!* --- Scan from 01h53m07s to 01h53m55s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=01h53m07s   !NEXT!        
qual=  0
disk=off
stop=01h53m55s   !NEXT!

!* --- Scan from 01h54m07s to 01h54m54s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=01h54m07s   !NEXT!        
qual=  0
disk=off
stop=01h54m54s   !NEXT!

!* --- Scan from 01h55m08s to 01h55m55s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=01h55m08s   !NEXT!        
qual=  0
disk=off
stop=01h55m55s   !NEXT!

!* --- Scan from 01h56m09s to 01h56m57s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=01h56m09s   !NEXT!        
qual=  0
disk=off
stop=01h56m57s   !NEXT!

!* --- Scan from 01h57m12s to 01h58m00s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=01h57m12s   !NEXT!        
qual=  0
disk=off
stop=01h58m00s   !NEXT!

!* --- Scan from 01h58m10s to 01h58m58s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=01h58m10s   !NEXT!        
qual=  0
disk=off
stop=01h58m58s   !NEXT!

!* --- Scan from 01h59m05s to 01h59m53s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=01h59m05s   !NEXT!        
qual=  0
disk=off
stop=01h59m53s   !NEXT!

!* --- Scan from 02h00m05s to 02h00m53s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=02h00m05s   !NEXT!        
qual=  0
disk=off
stop=02h00m53s   !NEXT!

!* --- Scan from 02h01m05s to 02h01m53s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=02h01m05s   !NEXT!        
qual=  0
disk=off
stop=02h01m53s   !NEXT!

!* --- Scan from 02h02m06s to 02h02m54s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=02h02m06s   !NEXT!        
qual=  0
disk=off
stop=02h02m54s   !NEXT!

!* --- Scan from 02h03m04s to 02h03m52s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=02h03m04s   !NEXT!        
qual=  0
disk=off
stop=02h03m52s   !NEXT!

!* --- Scan from 02h03m59s to 02h04m47s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=02h03m59s   !NEXT!        
qual=  0
disk=off
stop=02h04m47s   !NEXT!

!* --- Scan from 02h05m00s to 02h05m48s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=02h05m00s   !NEXT!        
qual=  0
disk=off
stop=02h05m48s   !NEXT!

!* --- Scan from 02h05m59s to 02h06m47s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=02h05m59s   !NEXT!        
qual=  0
disk=off
stop=02h06m47s   !NEXT!

!* --- Scan from 02h07m00s to 02h07m48s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=02h07m00s   !NEXT!        
qual=  0
disk=off
stop=02h07m48s   !NEXT!

!* --- Scan from 02h07m58s to 02h08m45s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=02h07m58s   !NEXT!        
qual=  0
disk=off
stop=02h08m45s   !NEXT!

!* --- Scan from 02h08m55s to 02h09m42s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=02h08m55s   !NEXT!        
qual=  0
disk=off
stop=02h09m42s   !NEXT!

!* --- Scan from 02h09m53s to 02h10m41s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=02h09m53s   !NEXT!        
qual=  0
disk=off
stop=02h10m41s   !NEXT!

!* --- Scan from 02h10m53s to 02h11m41s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=02h10m53s   !NEXT!        
qual=  0
disk=off
stop=02h11m41s   !NEXT!

!* --- Scan from 02h11m55s to 02h12m43s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=02h11m55s   !NEXT!        
qual=  0
disk=off
stop=02h12m43s   !NEXT!

!* --- Scan from 02h12m56s to 02h13m44s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=02h12m56s   !NEXT!        
qual=  0
disk=off
stop=02h13m44s   !NEXT!

!* --- Scan from 02h13m56s to 02h14m44s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=02h13m56s   !NEXT!        
qual=  0
disk=off
stop=02h14m44s   !NEXT!

!* --- Scan from 02h14m57s to 02h15m45s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=02h14m57s   !NEXT!        
qual=  0
disk=off
stop=02h15m45s   !NEXT!

!* --- Scan from 02h16m01s to 02h16m49s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=02h16m01s   !NEXT!        
qual=  0
disk=off
stop=02h16m49s   !NEXT!

!* --- Scan from 02h17m01s to 02h17m49s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=02h17m01s   !NEXT!        
qual=  0
disk=off
stop=02h17m49s   !NEXT!

!* --- Scan from 02h18m01s to 02h18m49s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=02h18m01s   !NEXT!        
qual=  0
disk=off
stop=02h18m49s   !NEXT!

!* --- Scan from 02h19m00s to 02h19m48s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=02h19m00s   !NEXT!        
qual=  0
disk=off
stop=02h19m48s   !NEXT!

!* --- Scan from 02h19m59s to 02h20m47s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=02h19m59s   !NEXT!        
qual=  0
disk=off
stop=02h20m47s   !NEXT!

!* --- Scan from 02h21m01s to 02h21m48s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=02h21m01s   !NEXT!        
qual=  0
disk=off
stop=02h21m48s   !NEXT!

!* --- Scan from 02h21m59s to 02h22m47s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=02h21m59s   !NEXT!        
qual=  0
disk=off
stop=02h22m47s   !NEXT!

!* --- Scan from 02h22m58s to 02h23m45s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=02h22m58s   !NEXT!        
qual=  0
disk=off
stop=02h23m45s   !NEXT!

!* --- Scan from 02h23m57s to 02h24m45s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=02h23m57s   !NEXT!        
qual=  0
disk=off
stop=02h24m45s   !NEXT!

!* --- Scan from 02h24m55s to 02h25m43s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=02h24m55s   !NEXT!        
qual=  0
disk=off
stop=02h25m43s   !NEXT!

!* --- Scan from 02h25m56s to 02h26m43s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=02h25m56s   !NEXT!        
qual=  0
disk=off
stop=02h26m43s   !NEXT!

!* --- Scan from 02h26m58s to 02h27m45s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=02h26m58s   !NEXT!        
qual=  0
disk=off
stop=02h27m45s   !NEXT!

!* --- Scan from 02h28m42s to 02h29m30s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=02h28m42s   !NEXT!        
qual=  0
disk=off
stop=02h29m30s   !NEXT!

!* --- Scan from 02h29m39s to 02h30m27s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=02h29m39s   !NEXT!        
qual=  0
disk=off
stop=02h30m27s   !NEXT!

!* --- Scan from 02h30m40s to 02h31m28s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=02h30m40s   !NEXT!        
qual=  0
disk=off
stop=02h31m28s   !NEXT!

!* --- Scan from 02h31m37s to 02h32m25s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=02h31m37s   !NEXT!        
qual=  0
disk=off
stop=02h32m25s   !NEXT!

!* --- Scan from 02h32m33s to 02h33m21s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=02h32m33s   !NEXT!        
qual=  0
disk=off
stop=02h33m21s   !NEXT!

!* --- Scan from 02h33m43s to 02h34m30s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=02h33m43s   !NEXT!        
qual=  0
disk=off
stop=02h34m30s   !NEXT!

!* --- Scan from 02h34m39s to 02h35m26s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=02h34m39s   !NEXT!        
qual=  0
disk=off
stop=02h35m26s   !NEXT!

!* --- Scan from 02h35m42s to 02h36m30s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=02h35m42s   !NEXT!        
qual=  0
disk=off
stop=02h36m30s   !NEXT!

!* --- Scan from 02h36m40s to 02h37m28s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=02h36m40s   !NEXT!        
qual=  0
disk=off
stop=02h37m28s   !NEXT!

!* --- Scan from 02h37m48s to 02h38m36s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=02h37m48s   !NEXT!        
qual=  0
disk=off
stop=02h38m36s   !NEXT!

!* --- Scan from 02h38m55s to 02h39m43s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=02h38m55s   !NEXT!        
qual=  0
disk=off
stop=02h39m43s   !NEXT!

!* --- Scan from 02h40m00s to 02h40m48s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=02h40m00s   !NEXT!        
qual=  0
disk=off
stop=02h40m48s   !NEXT!

!* --- Scan from 02h42m39s to 02h44m38s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=02h42m39s   !NEXT!        
qual=  0
disk=off
stop=02h44m38s   !NEXT!

!* --- Scan from 02h46m09s to 02h46m57s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=02h46m09s   !NEXT!        
qual=  0
disk=off
stop=02h46m57s   !NEXT!

!* --- Scan from 02h47m07s to 02h47m55s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=02h47m07s   !NEXT!        
qual=  0
disk=off
stop=02h47m55s   !NEXT!

!* --- Scan from 02h48m09s to 02h48m57s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=02h48m09s   !NEXT!        
qual=  0
disk=off
stop=02h48m57s   !NEXT!

!* --- Scan from 02h49m27s to 02h50m15s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=02h49m27s   !NEXT!        
qual=  0
disk=off
stop=02h50m15s   !NEXT!

!* --- Scan from 02h50m34s to 02h51m22s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=02h50m34s   !NEXT!        
qual=  0
disk=off
stop=02h51m22s   !NEXT!

!* --- Scan from 02h51m44s to 02h52m32s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=02h51m44s   !NEXT!        
qual=  0
disk=off
stop=02h52m32s   !NEXT!

!* --- Scan from 02h52m43s to 02h53m31s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=02h52m43s   !NEXT!        
qual=  0
disk=off
stop=02h53m31s   !NEXT!

!* --- Scan from 02h53m42s to 02h54m30s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=02h53m42s   !NEXT!        
qual=  0
disk=off
stop=02h54m30s   !NEXT!

!* --- Scan from 02h54m41s to 02h55m29s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=02h54m41s   !NEXT!        
qual=  0
disk=off
stop=02h55m29s   !NEXT!

!* --- Scan from 02h55m39s to 02h56m27s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=02h55m39s   !NEXT!        
qual=  0
disk=off
stop=02h56m27s   !NEXT!

!* --- Scan from 02h56m40s to 02h57m28s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=02h56m40s   !NEXT!        
qual=  0
disk=off
stop=02h57m28s   !NEXT!

!* --- Scan from 02h57m40s to 02h58m28s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=02h57m40s   !NEXT!        
qual=  0
disk=off
stop=02h58m28s   !NEXT!

!* --- Scan from 02h58m40s to 02h59m28s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=02h58m40s   !NEXT!        
qual=  0
disk=off
stop=02h59m28s   !NEXT!

!* --- Scan from 02h59m53s to 03h00m41s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=02h59m53s   !NEXT!        
qual=  0
disk=off
stop=03h00m41s   !NEXT!

!* --- Scan from 03h01m04s to 03h01m52s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=03h01m04s   !NEXT!        
qual=  0
disk=off
stop=03h01m52s   !NEXT!

!* --- Scan from 03h02m02s to 03h02m50s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=03h02m02s   !NEXT!        
qual=  0
disk=off
stop=03h02m50s   !NEXT!

!* --- Scan from 03h03m02s to 03h03m50s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=03h03m02s   !NEXT!        
qual=  0
disk=off
stop=03h03m50s   !NEXT!

!* --- Scan from 03h04m01s to 03h04m49s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=03h04m01s   !NEXT!        
qual=  0
disk=off
stop=03h04m49s   !NEXT!

!* --- Scan from 03h05m08s to 03h05m56s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=03h05m08s   !NEXT!        
qual=  0
disk=off
stop=03h05m56s   !NEXT!

!* --- Scan from 03h06m10s to 03h06m58s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=03h06m10s   !NEXT!        
qual=  0
disk=off
stop=03h06m58s   !NEXT!

!* --- Scan from 03h07m11s to 03h07m59s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=03h07m11s   !NEXT!        
qual=  0
disk=off
stop=03h07m59s   !NEXT!

!* --- Scan from 03h08m11s to 03h08m59s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=03h08m11s   !NEXT!        
qual=  0
disk=off
stop=03h08m59s   !NEXT!

!* --- Scan from 03h09m09s to 03h09m56s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=03h09m09s   !NEXT!        
qual=  0
disk=off
stop=03h09m56s   !NEXT!

!* --- Scan from 03h10m07s to 03h10m55s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=03h10m07s   !NEXT!        
qual=  0
disk=off
stop=03h10m55s   !NEXT!

!* --- Scan from 03h11m09s to 03h11m57s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=03h11m09s   !NEXT!        
qual=  0
disk=off
stop=03h11m57s   !NEXT!

!* --- Scan from 03h12m08s to 03h12m56s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=03h12m08s   !NEXT!        
qual=  0
disk=off
stop=03h12m56s   !NEXT!

!* --- Scan from 03h13m12s to 03h14m00s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=03h13m12s   !NEXT!        
qual=  0
disk=off
stop=03h14m00s   !NEXT!

!* --- Scan from 03h14m07s to 03h14m55s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=03h14m07s   !NEXT!        
qual=  0
disk=off
stop=03h14m55s   !NEXT!

!* --- Scan from 03h15m05s to 03h15m53s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=03h15m05s   !NEXT!        
qual=  0
disk=off
stop=03h15m53s   !NEXT!

!* --- Scan from 03h16m06s to 03h16m54s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=03h16m06s   !NEXT!        
qual=  0
disk=off
stop=03h16m54s   !NEXT!

!* --- Scan from 03h17m05s to 03h17m53s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=03h17m05s   !NEXT!        
qual=  0
disk=off
stop=03h17m53s   !NEXT!

!* --- Scan from 03h18m05s to 03h18m52s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=03h18m05s   !NEXT!        
qual=  0
disk=off
stop=03h18m52s   !NEXT!

!* --- Scan from 03h19m23s to 03h20m11s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=03h19m23s   !NEXT!        
qual=  0
disk=off
stop=03h20m11s   !NEXT!

!* --- Scan from 03h20m45s to 03h21m33s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=03h20m45s   !NEXT!        
qual=  0
disk=off
stop=03h21m33s   !NEXT!

!* --- Scan from 03h21m42s to 03h22m29s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=03h21m42s   !NEXT!        
qual=  0
disk=off
stop=03h22m29s   !NEXT!

!* --- Scan from 03h22m46s to 03h23m34s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=03h22m46s   !NEXT!        
qual=  0
disk=off
stop=03h23m34s   !NEXT!

!* --- Scan from 03h23m44s to 03h24m32s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=03h23m44s   !NEXT!        
qual=  0
disk=off
stop=03h24m32s   !NEXT!

!* --- Scan from 03h24m40s to 03h25m28s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=03h24m40s   !NEXT!        
qual=  0
disk=off
stop=03h25m28s   !NEXT!

!* --- Scan from 03h25m50s to 03h26m38s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=03h25m50s   !NEXT!        
qual=  0
disk=off
stop=03h26m38s   !NEXT!

!* --- Scan from 03h26m46s to 03h27m34s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=03h26m46s   !NEXT!        
qual=  0
disk=off
stop=03h27m34s   !NEXT!

!* --- Scan from 03h28m55s to 03h29m43s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=03h28m55s   !NEXT!        
qual=  0
disk=off
stop=03h29m43s   !NEXT!

!* --- Scan from 03h29m54s to 03h30m42s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=03h29m54s   !NEXT!        
qual=  0
disk=off
stop=03h30m42s   !NEXT!

!* --- Scan from 03h30m51s to 03h31m39s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=03h30m51s   !NEXT!        
qual=  0
disk=off
stop=03h31m39s   !NEXT!

!* --- Scan from 03h31m51s to 03h32m39s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=03h31m51s   !NEXT!        
qual=  0
disk=off
stop=03h32m39s   !NEXT!

!* --- Scan from 03h36m49s to 03h37m37s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=03h36m49s   !NEXT!        
qual=  0
disk=off
stop=03h37m37s   !NEXT!

!* --- Scan from 03h37m57s to 03h38m45s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=03h37m57s   !NEXT!        
qual=  0
disk=off
stop=03h38m45s   !NEXT!

!* --- Scan from 03h39m03s to 03h39m51s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=03h39m03s   !NEXT!        
qual=  0
disk=off
stop=03h39m51s   !NEXT!

!* --- Scan from 03h40m07s to 03h40m55s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=03h40m07s   !NEXT!        
qual=  0
disk=off
stop=03h40m55s   !NEXT!

!* --- Scan from 03h41m09s to 03h41m57s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=03h41m09s   !NEXT!        
qual=  0
disk=off
stop=03h41m57s   !NEXT!

!* --- Scan from 03h42m11s to 03h42m59s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=03h42m11s   !NEXT!        
qual=  0
disk=off
stop=03h42m59s   !NEXT!

!* --- Scan from 03h43m12s to 03h43m59s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=03h43m12s   !NEXT!        
qual=  0
disk=off
stop=03h43m59s   !NEXT!

!* --- Scan from 03h44m10s to 03h44m57s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=03h44m10s   !NEXT!        
qual=  0
disk=off
stop=03h44m57s   !NEXT!

!* --- Scan from 03h45m08s to 03h45m56s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=03h45m08s   !NEXT!        
qual=  0
disk=off
stop=03h45m56s   !NEXT!

!* --- Scan from 03h46m07s to 03h46m55s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=03h46m07s   !NEXT!        
qual=  0
disk=off
stop=03h46m55s   !NEXT!

!* --- Scan from 03h47m07s to 03h47m55s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=03h47m07s   !NEXT!        
qual=  0
disk=off
stop=03h47m55s   !NEXT!

!* --- Scan from 03h48m06s to 03h48m53s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=03h48m06s   !NEXT!        
qual=  0
disk=off
stop=03h48m53s   !NEXT!

!* --- Scan from 03h49m06s to 03h49m53s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=03h49m06s   !NEXT!        
qual=  0
disk=off
stop=03h49m53s   !NEXT!

!* --- Scan from 03h50m07s to 03h50m55s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=03h50m07s   !NEXT!        
qual=  0
disk=off
stop=03h50m55s   !NEXT!

!* --- Scan from 03h51m06s to 03h51m54s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=03h51m06s   !NEXT!        
qual=  0
disk=off
stop=03h51m54s   !NEXT!

!* --- Scan from 03h52m05s to 03h52m53s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=03h52m05s   !NEXT!        
qual=  0
disk=off
stop=03h52m53s   !NEXT!

!* --- Scan from 03h53m05s to 03h53m52s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=03h53m05s   !NEXT!        
qual=  0
disk=off
stop=03h53m52s   !NEXT!

!* --- Scan from 03h54m09s to 03h54m57s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=03h54m09s   !NEXT!        
qual=  0
disk=off
stop=03h54m57s   !NEXT!

!* --- Scan from 03h55m48s to 03h56m35s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=03h55m48s   !NEXT!        
qual=  0
disk=off
stop=03h56m35s   !NEXT!

!* --- Scan from 03h56m49s to 03h57m37s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=03h56m49s   !NEXT!        
qual=  0
disk=off
stop=03h57m37s   !NEXT!

!* --- Scan from 03h57m46s to 03h58m34s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=03h57m46s   !NEXT!        
qual=  0
disk=off
stop=03h58m34s   !NEXT!

!* --- Scan from 03h59m16s to 04h00m04s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=03h59m16s   !NEXT!        
qual=  0
disk=off
stop=04h00m04s   !NEXT!

!* --- Scan from 04h00m13s to 04h01m01s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=04h00m13s   !NEXT!        
qual=  0
disk=off
stop=04h01m01s   !NEXT!

!* --- Scan from 04h01m15s to 04h02m03s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=04h01m15s   !NEXT!        
qual=  0
disk=off
stop=04h02m03s   !NEXT!

!* --- Scan from 04h02m18s to 04h03m06s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=04h02m18s   !NEXT!        
qual=  0
disk=off
stop=04h03m06s   !NEXT!

!* --- Scan from 04h03m20s to 04h04m07s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=04h03m20s   !NEXT!        
qual=  0
disk=off
stop=04h04m07s   !NEXT!

!* --- Scan from 04h04m27s to 04h05m15s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=04h04m27s   !NEXT!        
qual=  0
disk=off
stop=04h05m15s   !NEXT!

!* --- Scan from 04h05m30s to 04h06m18s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=04h05m30s   !NEXT!        
qual=  0
disk=off
stop=04h06m18s   !NEXT!

!* --- Scan from 04h06m28s to 04h07m16s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=04h06m28s   !NEXT!        
qual=  0
disk=off
stop=04h07m16s   !NEXT!

!* --- Scan from 04h07m24s to 04h08m11s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=04h07m24s   !NEXT!        
qual=  0
disk=off
stop=04h08m11s   !NEXT!

!* --- Scan from 04h08m23s to 04h09m11s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=04h08m23s   !NEXT!        
qual=  0
disk=off
stop=04h09m11s   !NEXT!

!* --- Scan from 04h11m28s to 04h13m27s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=04h11m28s   !NEXT!        
qual=  0
disk=off
stop=04h13m27s   !NEXT!

!* --- Scan from 04h15m48s to 04h17m48s   Thu, 2006 Feb 16 --- *!
sname='J0854+2006'  ra=08h54m48.874924s  dec= 20d06'30.64088"  qual=999  calib='N'
disk=off
stop=04h15m48s   !NEXT!        
qual=  0
disk=off
stop=04h17m48s   !NEXT!

!* --- Scan from 04h18m07s to 04h18m55s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=04h18m07s   !NEXT!        
qual=  0
disk=off
stop=04h18m55s   !NEXT!

!* --- Scan from 04h19m06s to 04h19m53s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=04h19m06s   !NEXT!        
qual=  0
disk=off
stop=04h19m53s   !NEXT!

!* --- Scan from 04h20m01s to 04h20m49s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=04h20m01s   !NEXT!        
qual=  0
disk=off
stop=04h20m49s   !NEXT!

!* --- Scan from 04h21m02s to 04h21m49s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=04h21m02s   !NEXT!        
qual=  0
disk=off
stop=04h21m49s   !NEXT!

!* --- Scan from 04h22m01s to 04h22m48s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=04h22m01s   !NEXT!        
qual=  0
disk=off
stop=04h22m48s   !NEXT!

!* --- Scan from 04h23m02s to 04h23m50s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=04h23m02s   !NEXT!        
qual=  0
disk=off
stop=04h23m50s   !NEXT!

!* --- Scan from 04h24m01s to 04h24m49s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=04h24m01s   !NEXT!        
qual=  0
disk=off
stop=04h24m49s   !NEXT!

!* --- Scan from 04h24m58s to 04h25m46s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=04h24m58s   !NEXT!        
qual=  0
disk=off
stop=04h25m46s   !NEXT!

!* --- Scan from 04h25m58s to 04h26m45s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=04h25m58s   !NEXT!        
qual=  0
disk=off
stop=04h26m45s   !NEXT!

!* --- Scan from 04h26m57s to 04h27m45s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=04h26m57s   !NEXT!        
qual=  0
disk=off
stop=04h27m45s   !NEXT!

!* --- Scan from 04h27m59s to 04h28m46s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=04h27m59s   !NEXT!        
qual=  0
disk=off
stop=04h28m46s   !NEXT!

!* --- Scan from 04h29m12s to 04h30m00s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=04h29m12s   !NEXT!        
qual=  0
disk=off
stop=04h30m00s   !NEXT!

!* --- Scan from 04h30m09s to 04h30m57s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=04h30m09s   !NEXT!        
qual=  0
disk=off
stop=04h30m57s   !NEXT!

!* --- Scan from 04h31m11s to 04h31m59s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=04h31m11s   !NEXT!        
qual=  0
disk=off
stop=04h31m59s   !NEXT!

!* --- Scan from 04h32m09s to 04h32m57s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=04h32m09s   !NEXT!        
qual=  0
disk=off
stop=04h32m57s   !NEXT!

!* --- Scan from 04h33m05s to 04h33m53s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=04h33m05s   !NEXT!        
qual=  0
disk=off
stop=04h33m53s   !NEXT!

!* --- Scan from 04h34m31s to 04h35m19s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=04h34m31s   !NEXT!        
qual=  0
disk=off
stop=04h35m19s   !NEXT!

!* --- Scan from 04h35m31s to 04h36m19s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=04h35m31s   !NEXT!        
qual=  0
disk=off
stop=04h36m19s   !NEXT!

!* --- Scan from 04h36m29s to 04h37m17s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=04h36m29s   !NEXT!        
qual=  0
disk=off
stop=04h37m17s   !NEXT!

!* --- Scan from 04h37m25s to 04h38m12s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=04h37m25s   !NEXT!        
qual=  0
disk=off
stop=04h38m12s   !NEXT!

!* --- Scan from 04h38m24s to 04h39m12s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=04h38m24s   !NEXT!        
qual=  0
disk=off
stop=04h39m12s   !NEXT!

!* --- Scan from 04h39m28s to 04h40m16s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=04h39m28s   !NEXT!        
qual=  0
disk=off
stop=04h40m16s   !NEXT!

!* --- Scan from 04h40m33s to 04h41m21s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=04h40m33s   !NEXT!        
qual=  0
disk=off
stop=04h41m21s   !NEXT!

!* --- Scan from 04h41m34s to 04h43m33s   Thu, 2006 Feb 16 --- *!
sname='J0854+2006'  ra=08h54m48.874924s  dec= 20d06'30.64088"  qual=999  calib='N'
disk=off
stop=04h41m34s   !NEXT!        
qual=  0
disk=off
stop=04h43m33s   !NEXT!

!* --- Scan from 04h43m43s to 04h44m31s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=04h43m43s   !NEXT!        
qual=  0
disk=off
stop=04h44m31s   !NEXT!

!* --- Scan from 04h44m39s to 04h45m27s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=04h44m39s   !NEXT!        
qual=  0
disk=off
stop=04h45m27s   !NEXT!

!* --- Scan from 04h45m44s to 04h46m32s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=04h45m44s   !NEXT!        
qual=  0
disk=off
stop=04h46m32s   !NEXT!

!* --- Scan from 04h46m43s to 04h47m31s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=04h46m43s   !NEXT!        
qual=  0
disk=off
stop=04h47m31s   !NEXT!

!* --- Scan from 04h47m45s to 04h48m33s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=04h47m45s   !NEXT!        
qual=  0
disk=off
stop=04h48m33s   !NEXT!

!* --- Scan from 04h48m45s to 04h49m33s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=04h48m45s   !NEXT!        
qual=  0
disk=off
stop=04h49m33s   !NEXT!

!* --- Scan from 04h49m44s to 04h50m31s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=04h49m44s   !NEXT!        
qual=  0
disk=off
stop=04h50m31s   !NEXT!

!* --- Scan from 04h50m43s to 04h51m30s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=04h50m43s   !NEXT!        
qual=  0
disk=off
stop=04h51m30s   !NEXT!

!* --- Scan from 04h51m41s to 04h52m29s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=04h51m41s   !NEXT!        
qual=  0
disk=off
stop=04h52m29s   !NEXT!

!* --- Scan from 04h52m49s to 04h53m37s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=04h52m49s   !NEXT!        
qual=  0
disk=off
stop=04h53m37s   !NEXT!

!* --- Scan from 04h53m50s to 04h54m37s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=04h53m50s   !NEXT!        
qual=  0
disk=off
stop=04h54m37s   !NEXT!

!* --- Scan from 04h54m50s to 04h55m38s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=04h54m50s   !NEXT!        
qual=  0
disk=off
stop=04h55m38s   !NEXT!

!* --- Scan from 04h55m57s to 04h56m45s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=04h55m57s   !NEXT!        
qual=  0
disk=off
stop=04h56m45s   !NEXT!

!* --- Scan from 04h56m57s to 04h57m45s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=04h56m57s   !NEXT!        
qual=  0
disk=off
stop=04h57m45s   !NEXT!

!* --- Scan from 04h58m01s to 04h58m49s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=04h58m01s   !NEXT!        
qual=  0
disk=off
stop=04h58m49s   !NEXT!

!* --- Scan from 04h59m04s to 04h59m51s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=04h59m04s   !NEXT!        
qual=  0
disk=off
stop=04h59m51s   !NEXT!

!* --- Scan from 05h00m03s to 05h00m51s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=05h00m03s   !NEXT!        
qual=  0
disk=off
stop=05h00m51s   !NEXT!

!* --- Scan from 05h01m08s to 05h01m56s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=05h01m08s   !NEXT!        
qual=  0
disk=off
stop=05h01m56s   !NEXT!

!* --- Scan from 05h02m24s to 05h03m12s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=05h02m24s   !NEXT!        
qual=  0
disk=off
stop=05h03m12s   !NEXT!

!* --- Scan from 05h03m22s to 05h04m10s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=05h03m22s   !NEXT!        
qual=  0
disk=off
stop=05h04m10s   !NEXT!

!* --- Scan from 05h04m26s to 05h05m13s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=05h04m26s   !NEXT!        
qual=  0
disk=off
stop=05h05m13s   !NEXT!

!* --- Scan from 05h05m30s to 05h06m18s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=05h05m30s   !NEXT!        
qual=  0
disk=off
stop=05h06m18s   !NEXT!

!* --- Scan from 05h06m32s to 05h07m20s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=05h06m32s   !NEXT!        
qual=  0
disk=off
stop=05h07m20s   !NEXT!

!* --- Scan from 05h07m36s to 05h08m24s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=05h07m36s   !NEXT!        
qual=  0
disk=off
stop=05h08m24s   !NEXT!

!* --- Scan from 05h10m35s to 05h12m35s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=05h10m35s   !NEXT!        
qual=  0
disk=off
stop=05h12m35s   !NEXT!

!* --- Scan from 05h14m26s to 05h15m14s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=05h14m26s   !NEXT!        
qual=  0
disk=off
stop=05h15m14s   !NEXT!

!* --- Scan from 05h15m30s to 05h16m18s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=05h15m30s   !NEXT!        
qual=  0
disk=off
stop=05h16m18s   !NEXT!

!* --- Scan from 05h16m36s to 05h17m23s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=05h16m36s   !NEXT!        
qual=  0
disk=off
stop=05h17m23s   !NEXT!

!* --- Scan from 05h17m44s to 05h18m32s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=05h17m44s   !NEXT!        
qual=  0
disk=off
stop=05h18m32s   !NEXT!

!* --- Scan from 05h18m53s to 05h19m41s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=05h18m53s   !NEXT!        
qual=  0
disk=off
stop=05h19m41s   !NEXT!

!* --- Scan from 05h20m08s to 05h20m56s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=05h20m08s   !NEXT!        
qual=  0
disk=off
stop=05h20m56s   !NEXT!

!* --- Scan from 05h22m24s to 05h23m12s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=05h22m24s   !NEXT!        
qual=  0
disk=off
stop=05h23m12s   !NEXT!

!* --- Scan from 05h23m27s to 05h24m15s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=05h23m27s   !NEXT!        
qual=  0
disk=off
stop=05h24m15s   !NEXT!

!* --- Scan from 05h24m31s to 05h25m19s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=05h24m31s   !NEXT!        
qual=  0
disk=off
stop=05h25m19s   !NEXT!

!* --- Scan from 05h25m34s to 05h26m22s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=05h25m34s   !NEXT!        
qual=  0
disk=off
stop=05h26m22s   !NEXT!

!* --- Scan from 05h26m39s to 05h27m27s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=05h26m39s   !NEXT!        
qual=  0
disk=off
stop=05h27m27s   !NEXT!

!* --- Scan from 05h27m42s to 05h28m30s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=05h27m42s   !NEXT!        
qual=  0
disk=off
stop=05h28m30s   !NEXT!

!* --- Scan from 05h28m45s to 05h29m33s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=05h28m45s   !NEXT!        
qual=  0
disk=off
stop=05h29m33s   !NEXT!

!* --- Scan from 05h29m53s to 05h30m41s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=05h29m53s   !NEXT!        
qual=  0
disk=off
stop=05h30m41s   !NEXT!

!* --- Scan from 05h30m48s to 05h31m36s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=05h30m48s   !NEXT!        
qual=  0
disk=off
stop=05h31m36s   !NEXT!

!* --- Scan from 05h31m48s to 05h32m36s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=05h31m48s   !NEXT!        
qual=  0
disk=off
stop=05h32m36s   !NEXT!

!* --- Scan from 05h32m55s to 05h33m42s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=05h32m55s   !NEXT!        
qual=  0
disk=off
stop=05h33m42s   !NEXT!

!* --- Scan from 05h33m59s to 05h34m47s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=05h33m59s   !NEXT!        
qual=  0
disk=off
stop=05h34m47s   !NEXT!

!* --- Scan from 05h35m33s to 05h36m21s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=05h35m33s   !NEXT!        
qual=  0
disk=off
stop=05h36m21s   !NEXT!

!* --- Scan from 05h36m37s to 05h37m25s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=05h36m37s   !NEXT!        
qual=  0
disk=off
stop=05h37m25s   !NEXT!

!* --- Scan from 05h37m34s to 05h38m22s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=05h37m34s   !NEXT!        
qual=  0
disk=off
stop=05h38m22s   !NEXT!

!* --- Scan from 05h38m37s to 05h39m25s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=05h38m37s   !NEXT!        
qual=  0
disk=off
stop=05h39m25s   !NEXT!

!* --- Scan from 05h39m35s to 05h40m22s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=05h39m35s   !NEXT!        
qual=  0
disk=off
stop=05h40m22s   !NEXT!

!* --- Scan from 05h40m37s to 05h41m25s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=05h40m37s   !NEXT!        
qual=  0
disk=off
stop=05h41m25s   !NEXT!

!* --- Scan from 05h42m58s to 05h43m46s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=05h42m58s   !NEXT!        
qual=  0
disk=off
stop=05h43m46s   !NEXT!

!* --- Scan from 05h43m56s to 05h44m44s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=05h43m56s   !NEXT!        
qual=  0
disk=off
stop=05h44m44s   !NEXT!

!* --- Scan from 05h44m59s to 05h45m47s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=05h44m59s   !NEXT!        
qual=  0
disk=off
stop=05h45m47s   !NEXT!

!* --- Scan from 05h46m03s to 05h46m51s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=05h46m03s   !NEXT!        
qual=  0
disk=off
stop=05h46m51s   !NEXT!

!* --- Scan from 05h47m03s to 05h47m51s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=05h47m03s   !NEXT!        
qual=  0
disk=off
stop=05h47m51s   !NEXT!

!* --- Scan from 05h48m04s to 05h48m52s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=05h48m04s   !NEXT!        
qual=  0
disk=off
stop=05h48m52s   !NEXT!

!* --- Scan from 05h49m05s to 05h49m53s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=05h49m05s   !NEXT!        
qual=  0
disk=off
stop=05h49m53s   !NEXT!

!* --- Scan from 05h50m05s to 05h50m53s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=05h50m05s   !NEXT!        
qual=  0
disk=off
stop=05h50m53s   !NEXT!

!* --- Scan from 05h51m04s to 05h51m52s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=05h51m04s   !NEXT!        
qual=  0
disk=off
stop=05h51m52s   !NEXT!

!* --- Scan from 05h52m05s to 05h52m53s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=05h52m05s   !NEXT!        
qual=  0
disk=off
stop=05h52m53s   !NEXT!

!* --- Scan from 05h53m08s to 05h53m56s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=05h53m08s   !NEXT!        
qual=  0
disk=off
stop=05h53m56s   !NEXT!

!* --- Scan from 05h54m16s to 05h55m03s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=05h54m16s   !NEXT!        
qual=  0
disk=off
stop=05h55m03s   !NEXT!

!* --- Scan from 05h55m18s to 05h56m06s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=05h55m18s   !NEXT!        
qual=  0
disk=off
stop=05h56m06s   !NEXT!

!* --- Scan from 05h56m20s to 05h57m08s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=05h56m20s   !NEXT!        
qual=  0
disk=off
stop=05h57m08s   !NEXT!

!* --- Scan from 05h57m24s to 05h58m11s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=05h57m24s   !NEXT!        
qual=  0
disk=off
stop=05h58m11s   !NEXT!

!* --- Scan from 05h58m24s to 05h59m11s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=05h58m24s   !NEXT!        
qual=  0
disk=off
stop=05h59m11s   !NEXT!

!* --- Scan from 05h59m28s to 06h00m15s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=05h59m28s   !NEXT!        
qual=  0
disk=off
stop=06h00m15s   !NEXT!

!* --- Scan from 06h00m30s to 06h01m17s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=06h00m30s   !NEXT!        
qual=  0
disk=off
stop=06h01m17s   !NEXT!

!* --- Scan from 06h01m35s to 06h02m23s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=06h01m35s   !NEXT!        
qual=  0
disk=off
stop=06h02m23s   !NEXT!

!* --- Scan from 06h02m37s to 06h03m25s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=06h02m37s   !NEXT!        
qual=  0
disk=off
stop=06h03m25s   !NEXT!

!* --- Scan from 06h03m38s to 06h04m26s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=06h03m38s   !NEXT!        
qual=  0
disk=off
stop=06h04m26s   !NEXT!

!* --- Scan from 06h04m43s to 06h05m31s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=06h04m43s   !NEXT!        
qual=  0
disk=off
stop=06h05m31s   !NEXT!

!* --- Scan from 06h06m26s to 06h07m14s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=06h06m26s   !NEXT!        
qual=  0
disk=off
stop=06h07m14s   !NEXT!

!* --- Scan from 06h07m32s to 06h08m20s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=06h07m32s   !NEXT!        
qual=  0
disk=off
stop=06h08m20s   !NEXT!

!* --- Scan from 06h08m33s to 06h10m32s   Thu, 2006 Feb 16 --- *!
sname='J0854+2006'  ra=08h54m48.874924s  dec= 20d06'30.64088"  qual=999  calib='N'
disk=off
stop=06h08m33s   !NEXT!        
qual=  0
disk=off
stop=06h10m32s   !NEXT!

!* --- Scan from 06h10m43s to 06h11m30s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=06h10m43s   !NEXT!        
qual=  0
disk=off
stop=06h11m30s   !NEXT!

!* --- Scan from 06h11m38s to 06h12m26s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=06h11m38s   !NEXT!        
qual=  0
disk=off
stop=06h12m26s   !NEXT!

!* --- Scan from 06h13m55s to 06h14m43s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=06h13m55s   !NEXT!        
qual=  0
disk=off
stop=06h14m43s   !NEXT!

!* --- Scan from 06h14m53s to 06h15m41s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=06h14m53s   !NEXT!        
qual=  0
disk=off
stop=06h15m41s   !NEXT!

!* --- Scan from 06h15m51s to 06h16m39s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=06h15m51s   !NEXT!        
qual=  0
disk=off
stop=06h16m39s   !NEXT!

!* --- Scan from 06h16m49s to 06h17m37s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=06h16m49s   !NEXT!        
qual=  0
disk=off
stop=06h17m37s   !NEXT!

!* --- Scan from 06h17m54s to 06h18m42s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=06h17m54s   !NEXT!        
qual=  0
disk=off
stop=06h18m42s   !NEXT!

!* --- Scan from 06h19m35s to 06h21m35s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=06h19m35s   !NEXT!        
qual=  0
disk=off
stop=06h21m35s   !NEXT!

!* --- Scan from 06h23m06s to 06h23m54s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=06h23m06s   !NEXT!        
qual=  0
disk=off
stop=06h23m54s   !NEXT!

!* --- Scan from 06h24m04s to 06h24m52s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=06h24m04s   !NEXT!        
qual=  0
disk=off
stop=06h24m52s   !NEXT!

!* --- Scan from 06h24m59s to 06h25m47s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=06h24m59s   !NEXT!        
qual=  0
disk=off
stop=06h25m47s   !NEXT!

!* --- Scan from 06h26m01s to 06h26m49s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=06h26m01s   !NEXT!        
qual=  0
disk=off
stop=06h26m49s   !NEXT!

!* --- Scan from 06h27m02s to 06h27m50s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=06h27m02s   !NEXT!        
qual=  0
disk=off
stop=06h27m50s   !NEXT!

!* --- Scan from 06h28m00s to 06h28m48s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=06h28m00s   !NEXT!        
qual=  0
disk=off
stop=06h28m48s   !NEXT!

!* --- Scan from 06h29m00s to 06h29m47s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=06h29m00s   !NEXT!        
qual=  0
disk=off
stop=06h29m47s   !NEXT!

!* --- Scan from 06h30m00s to 06h30m48s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=06h30m00s   !NEXT!        
qual=  0
disk=off
stop=06h30m48s   !NEXT!

!* --- Scan from 06h31m01s to 06h31m49s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=06h31m01s   !NEXT!        
qual=  0
disk=off
stop=06h31m49s   !NEXT!

!* --- Scan from 06h32m00s to 06h32m47s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=06h32m00s   !NEXT!        
qual=  0
disk=off
stop=06h32m47s   !NEXT!

!* --- Scan from 06h32m56s to 06h33m43s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=06h32m56s   !NEXT!        
qual=  0
disk=off
stop=06h33m43s   !NEXT!

!* --- Scan from 06h34m00s to 06h34m48s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=06h34m00s   !NEXT!        
qual=  0
disk=off
stop=06h34m48s   !NEXT!

!* --- Scan from 06h35m01s to 06h35m49s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=06h35m01s   !NEXT!        
qual=  0
disk=off
stop=06h35m49s   !NEXT!

!* --- Scan from 06h36m11s to 06h36m58s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=06h36m11s   !NEXT!        
qual=  0
disk=off
stop=06h36m58s   !NEXT!

!* --- Scan from 06h37m14s to 06h38m01s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=06h37m14s   !NEXT!        
qual=  0
disk=off
stop=06h38m01s   !NEXT!

!* --- Scan from 06h38m13s to 06h39m01s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=06h38m13s   !NEXT!        
qual=  0
disk=off
stop=06h39m01s   !NEXT!

!* --- Scan from 06h39m11s to 06h39m59s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=06h39m11s   !NEXT!        
qual=  0
disk=off
stop=06h39m59s   !NEXT!

!* --- Scan from 06h40m11s to 06h40m59s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=06h40m11s   !NEXT!        
qual=  0
disk=off
stop=06h40m59s   !NEXT!

!* --- Scan from 06h41m41s to 06h42m28s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=06h41m41s   !NEXT!        
qual=  0
disk=off
stop=06h42m28s   !NEXT!

!* --- Scan from 06h42m40s to 06h43m28s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=06h42m40s   !NEXT!        
qual=  0
disk=off
stop=06h43m28s   !NEXT!

!* --- Scan from 06h44m06s to 06h44m54s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=06h44m06s   !NEXT!        
qual=  0
disk=off
stop=06h44m54s   !NEXT!

!* --- Scan from 06h45m07s to 06h45m55s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=06h45m07s   !NEXT!        
qual=  0
disk=off
stop=06h45m55s   !NEXT!

!* --- Scan from 06h46m04s to 06h46m52s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=06h46m04s   !NEXT!        
qual=  0
disk=off
stop=06h46m52s   !NEXT!

!* --- Scan from 06h47m47s to 06h48m35s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=06h47m47s   !NEXT!        
qual=  0
disk=off
stop=06h48m35s   !NEXT!

!* --- Scan from 06h48m45s to 06h49m33s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=06h48m45s   !NEXT!        
qual=  0
disk=off
stop=06h49m33s   !NEXT!

!* --- Scan from 06h49m47s to 06h50m35s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=06h49m47s   !NEXT!        
qual=  0
disk=off
stop=06h50m35s   !NEXT!

!* --- Scan from 06h50m52s to 06h51m40s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=06h50m52s   !NEXT!        
qual=  0
disk=off
stop=06h51m40s   !NEXT!

!* --- Scan from 06h51m54s to 06h52m42s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=06h51m54s   !NEXT!        
qual=  0
disk=off
stop=06h52m42s   !NEXT!

!* --- Scan from 06h52m52s to 06h53m40s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=06h52m52s   !NEXT!        
qual=  0
disk=off
stop=06h53m40s   !NEXT!

!* --- Scan from 06h53m52s to 06h54m40s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=06h53m52s   !NEXT!        
qual=  0
disk=off
stop=06h54m40s   !NEXT!

!* --- Scan from 06h54m50s to 06h55m38s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=06h54m50s   !NEXT!        
qual=  0
disk=off
stop=06h55m38s   !NEXT!

!* --- Scan from 06h55m48s to 06h56m36s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=06h55m48s   !NEXT!        
qual=  0
disk=off
stop=06h56m36s   !NEXT!

!* --- Scan from 06h56m47s to 06h57m35s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=06h56m47s   !NEXT!        
qual=  0
disk=off
stop=06h57m35s   !NEXT!

!* --- Scan from 06h57m47s to 06h58m35s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=06h57m47s   !NEXT!        
qual=  0
disk=off
stop=06h58m35s   !NEXT!

!* --- Scan from 06h58m45s to 06h59m33s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=06h58m45s   !NEXT!        
qual=  0
disk=off
stop=06h59m33s   !NEXT!

!* --- Scan from 06h59m44s to 07h00m32s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=06h59m44s   !NEXT!        
qual=  0
disk=off
stop=07h00m32s   !NEXT!

!* --- Scan from 07h00m43s to 07h01m31s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=07h00m43s   !NEXT!        
qual=  0
disk=off
stop=07h01m31s   !NEXT!

!* --- Scan from 07h01m43s to 07h02m31s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=07h01m43s   !NEXT!        
qual=  0
disk=off
stop=07h02m31s   !NEXT!

!* --- Scan from 07h02m42s to 07h03m30s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=07h02m42s   !NEXT!        
qual=  0
disk=off
stop=07h03m30s   !NEXT!

!* --- Scan from 07h03m43s to 07h04m30s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=07h03m43s   !NEXT!        
qual=  0
disk=off
stop=07h04m30s   !NEXT!

!* --- Scan from 07h04m41s to 07h05m29s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=07h04m41s   !NEXT!        
qual=  0
disk=off
stop=07h05m29s   !NEXT!

!* --- Scan from 07h05m45s to 07h06m33s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=07h05m45s   !NEXT!        
qual=  0
disk=off
stop=07h06m33s   !NEXT!

!* --- Scan from 07h06m47s to 07h07m35s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=07h06m47s   !NEXT!        
qual=  0
disk=off
stop=07h07m35s   !NEXT!

!* --- Scan from 07h07m48s to 07h08m36s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=07h07m48s   !NEXT!        
qual=  0
disk=off
stop=07h08m36s   !NEXT!

!* --- Scan from 07h08m49s to 07h09m37s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=07h08m49s   !NEXT!        
qual=  0
disk=off
stop=07h09m37s   !NEXT!

!* --- Scan from 07h09m48s to 07h10m36s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=07h09m48s   !NEXT!        
qual=  0
disk=off
stop=07h10m36s   !NEXT!

!* --- Scan from 07h10m46s to 07h11m34s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=07h10m46s   !NEXT!        
qual=  0
disk=off
stop=07h11m34s   !NEXT!

!* --- Scan from 07h11m46s to 07h12m33s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=07h11m46s   !NEXT!        
qual=  0
disk=off
stop=07h12m33s   !NEXT!

!* --- Scan from 07h12m47s to 07h13m35s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=07h12m47s   !NEXT!        
qual=  0
disk=off
stop=07h13m35s   !NEXT!

!* --- Scan from 07h13m45s to 07h14m33s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=07h13m45s   !NEXT!        
qual=  0
disk=off
stop=07h14m33s   !NEXT!

!* --- Scan from 07h14m42s to 07h15m30s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=07h14m42s   !NEXT!        
qual=  0
disk=off
stop=07h15m30s   !NEXT!

!* --- Scan from 07h15m41s to 07h16m29s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=07h15m41s   !NEXT!        
qual=  0
disk=off
stop=07h16m29s   !NEXT!

!* --- Scan from 07h16m42s to 07h17m30s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=07h16m42s   !NEXT!        
qual=  0
disk=off
stop=07h17m30s   !NEXT!

!* --- Scan from 07h17m43s to 07h18m31s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=07h17m43s   !NEXT!        
qual=  0
disk=off
stop=07h18m31s   !NEXT!

!* --- Scan from 07h18m45s to 07h19m33s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=07h18m45s   !NEXT!        
qual=  0
disk=off
stop=07h19m33s   !NEXT!

!* --- Scan from 07h19m41s to 07h20m29s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=07h19m41s   !NEXT!        
qual=  0
disk=off
stop=07h20m29s   !NEXT!

!* --- Scan from 07h20m39s to 07h21m27s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=07h20m39s   !NEXT!        
qual=  0
disk=off
stop=07h21m27s   !NEXT!

!* --- Scan from 07h21m39s to 07h22m27s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=07h21m39s   !NEXT!        
qual=  0
disk=off
stop=07h22m27s   !NEXT!

!* --- Scan from 07h22m42s to 07h23m30s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=07h22m42s   !NEXT!        
qual=  0
disk=off
stop=07h23m30s   !NEXT!

!* --- Scan from 07h23m37s to 07h24m25s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=07h23m37s   !NEXT!        
qual=  0
disk=off
stop=07h24m25s   !NEXT!

!* --- Scan from 07h24m35s to 07h25m23s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=07h24m35s   !NEXT!        
qual=  0
disk=off
stop=07h25m23s   !NEXT!

!* --- Scan from 07h25m35s to 07h26m22s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=07h25m35s   !NEXT!        
qual=  0
disk=off
stop=07h26m22s   !NEXT!

!* --- Scan from 07h26m43s to 07h27m31s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=07h26m43s   !NEXT!        
qual=  0
disk=off
stop=07h27m31s   !NEXT!

!* --- Scan from 07h27m46s to 07h28m34s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=07h27m46s   !NEXT!        
qual=  0
disk=off
stop=07h28m34s   !NEXT!

!* --- Scan from 07h28m47s to 07h29m35s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=07h28m47s   !NEXT!        
qual=  0
disk=off
stop=07h29m35s   !NEXT!

!* --- Scan from 07h29m43s to 07h30m31s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=07h29m43s   !NEXT!        
qual=  0
disk=off
stop=07h30m31s   !NEXT!

!* --- Scan from 07h30m48s to 07h31m35s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=07h30m48s   !NEXT!        
qual=  0
disk=off
stop=07h31m35s   !NEXT!

!* --- Scan from 07h31m47s to 07h32m35s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=07h31m47s   !NEXT!        
qual=  0
disk=off
stop=07h32m35s   !NEXT!

!* --- Scan from 07h32m45s to 07h33m33s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=07h32m45s   !NEXT!        
qual=  0
disk=off
stop=07h33m33s   !NEXT!

!* --- Scan from 07h38m02s to 07h38m50s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=07h38m02s   !NEXT!        
qual=  0
disk=off
stop=07h38m50s   !NEXT!

!* --- Scan from 07h39m00s to 07h39m48s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=07h39m00s   !NEXT!        
qual=  0
disk=off
stop=07h39m48s   !NEXT!

!* --- Scan from 07h39m58s to 07h40m46s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=07h39m58s   !NEXT!        
qual=  0
disk=off
stop=07h40m46s   !NEXT!

!* --- Scan from 07h40m56s to 07h41m44s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=07h40m56s   !NEXT!        
qual=  0
disk=off
stop=07h41m44s   !NEXT!

!* --- Scan from 07h41m54s to 07h42m42s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=07h41m54s   !NEXT!        
qual=  0
disk=off
stop=07h42m42s   !NEXT!

!* --- Scan from 07h43m00s to 07h43m48s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=07h43m00s   !NEXT!        
qual=  0
disk=off
stop=07h43m48s   !NEXT!

!* --- Scan from 07h43m59s to 07h44m47s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=07h43m59s   !NEXT!        
qual=  0
disk=off
stop=07h44m47s   !NEXT!

!* --- Scan from 07h44m57s to 07h45m45s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=07h44m57s   !NEXT!        
qual=  0
disk=off
stop=07h45m45s   !NEXT!

!* --- Scan from 07h46m00s to 07h46m48s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=07h46m00s   !NEXT!        
qual=  0
disk=off
stop=07h46m48s   !NEXT!

!* --- Scan from 07h46m59s to 07h47m47s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=07h46m59s   !NEXT!        
qual=  0
disk=off
stop=07h47m47s   !NEXT!

!* --- Scan from 07h48m04s to 07h48m52s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=07h48m04s   !NEXT!        
qual=  0
disk=off
stop=07h48m52s   !NEXT!

!* --- Scan from 07h49m06s to 07h49m54s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=07h49m06s   !NEXT!        
qual=  0
disk=off
stop=07h49m54s   !NEXT!

!* --- Scan from 07h50m12s to 07h51m00s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=07h50m12s   !NEXT!        
qual=  0
disk=off
stop=07h51m00s   !NEXT!

!* --- Scan from 07h51m10s to 07h51m58s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=07h51m10s   !NEXT!        
qual=  0
disk=off
stop=07h51m58s   !NEXT!

!* --- Scan from 07h52m14s to 07h53m02s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=07h52m14s   !NEXT!        
qual=  0
disk=off
stop=07h53m02s   !NEXT!

!* --- Scan from 07h53m18s to 07h54m06s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=07h53m18s   !NEXT!        
qual=  0
disk=off
stop=07h54m06s   !NEXT!

!* --- Scan from 07h54m32s to 07h55m20s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=07h54m32s   !NEXT!        
qual=  0
disk=off
stop=07h55m20s   !NEXT!

!* --- Scan from 07h55m31s to 07h56m19s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=07h55m31s   !NEXT!        
qual=  0
disk=off
stop=07h56m19s   !NEXT!

!* --- Scan from 07h56m32s to 07h57m20s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=07h56m32s   !NEXT!        
qual=  0
disk=off
stop=07h57m20s   !NEXT!

!* --- Scan from 07h57m29s to 07h58m17s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=07h57m29s   !NEXT!        
qual=  0
disk=off
stop=07h58m17s   !NEXT!

!* --- Scan from 07h58m31s to 07h59m19s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=07h58m31s   !NEXT!        
qual=  0
disk=off
stop=07h59m19s   !NEXT!

!* --- Scan from 07h59m29s to 08h00m17s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=07h59m29s   !NEXT!        
qual=  0
disk=off
stop=08h00m17s   !NEXT!

!* --- Scan from 08h00m25s to 08h01m13s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=08h00m25s   !NEXT!        
qual=  0
disk=off
stop=08h01m13s   !NEXT!

!* --- Scan from 08h01m34s to 08h02m22s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=08h01m34s   !NEXT!        
qual=  0
disk=off
stop=08h02m22s   !NEXT!

!* --- Scan from 08h02m38s to 08h03m26s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=08h02m38s   !NEXT!        
qual=  0
disk=off
stop=08h03m26s   !NEXT!

!* --- Scan from 08h03m42s to 08h04m30s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=08h03m42s   !NEXT!        
qual=  0
disk=off
stop=08h04m30s   !NEXT!

!* --- Scan from 08h04m39s to 08h05m27s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=08h04m39s   !NEXT!        
qual=  0
disk=off
stop=08h05m27s   !NEXT!

!* --- Scan from 08h05m40s to 08h06m28s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=08h05m40s   !NEXT!        
qual=  0
disk=off
stop=08h06m28s   !NEXT!

!* --- Scan from 08h06m38s to 08h07m26s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=08h06m38s   !NEXT!        
qual=  0
disk=off
stop=08h07m26s   !NEXT!

!* --- Scan from 08h07m34s to 08h08m22s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=08h07m34s   !NEXT!        
qual=  0
disk=off
stop=08h08m22s   !NEXT!

!* --- Scan from 08h08m36s to 08h09m24s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=08h08m36s   !NEXT!        
qual=  0
disk=off
stop=08h09m24s   !NEXT!

!* --- Scan from 08h10m33s to 08h11m21s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=08h10m33s   !NEXT!        
qual=  0
disk=off
stop=08h11m21s   !NEXT!

!* --- Scan from 08h11m34s to 08h12m22s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=08h11m34s   !NEXT!        
qual=  0
disk=off
stop=08h12m22s   !NEXT!

!* --- Scan from 08h12m35s to 08h13m23s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=08h12m35s   !NEXT!        
qual=  0
disk=off
stop=08h13m23s   !NEXT!

!* --- Scan from 08h13m41s to 08h14m29s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=08h13m41s   !NEXT!        
qual=  0
disk=off
stop=08h14m29s   !NEXT!

!* --- Scan from 08h14m42s to 08h15m30s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=08h14m42s   !NEXT!        
qual=  0
disk=off
stop=08h15m30s   !NEXT!

!* --- Scan from 08h15m42s to 08h16m30s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=08h15m42s   !NEXT!        
qual=  0
disk=off
stop=08h16m30s   !NEXT!

!* --- Scan from 08h16m44s to 08h17m32s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=08h16m44s   !NEXT!        
qual=  0
disk=off
stop=08h17m32s   !NEXT!

!* --- Scan from 08h17m44s to 08h18m32s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=08h17m44s   !NEXT!        
qual=  0
disk=off
stop=08h18m32s   !NEXT!

!* --- Scan from 08h18m44s to 08h19m32s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=08h18m44s   !NEXT!        
qual=  0
disk=off
stop=08h19m32s   !NEXT!

!* --- Scan from 08h19m44s to 08h20m32s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=08h19m44s   !NEXT!        
qual=  0
disk=off
stop=08h20m32s   !NEXT!

!* --- Scan from 08h20m48s to 08h21m35s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=08h20m48s   !NEXT!        
qual=  0
disk=off
stop=08h21m35s   !NEXT!

!* --- Scan from 08h23m56s to 08h25m56s   Thu, 2006 Feb 16 --- *!
sname='J1310+3220'  ra=13h10m28.663845s  dec= 32d20'43.78295"  qual=999  calib='N'
disk=off
stop=08h23m56s   !NEXT!        
qual=  0
disk=off
stop=08h25m56s   !NEXT!

!* --- Scan from 08h28m21s to 08h29m09s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=08h28m21s   !NEXT!        
qual=  0
disk=off
stop=08h29m09s   !NEXT!

!* --- Scan from 08h29m20s to 08h30m08s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=08h29m20s   !NEXT!        
qual=  0
disk=off
stop=08h30m08s   !NEXT!

!* --- Scan from 08h30m20s to 08h31m08s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=08h30m20s   !NEXT!        
qual=  0
disk=off
stop=08h31m08s   !NEXT!

!* --- Scan from 08h31m20s to 08h32m08s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=08h31m20s   !NEXT!        
qual=  0
disk=off
stop=08h32m08s   !NEXT!

!* --- Scan from 08h32m18s to 08h33m06s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=08h32m18s   !NEXT!        
qual=  0
disk=off
stop=08h33m06s   !NEXT!

!* --- Scan from 08h33m18s to 08h34m06s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=08h33m18s   !NEXT!        
qual=  0
disk=off
stop=08h34m06s   !NEXT!

!* --- Scan from 08h34m20s to 08h35m08s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=08h34m20s   !NEXT!        
qual=  0
disk=off
stop=08h35m08s   !NEXT!

!* --- Scan from 08h35m18s to 08h36m06s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=08h35m18s   !NEXT!        
qual=  0
disk=off
stop=08h36m06s   !NEXT!

!* --- Scan from 08h40m00s to 08h40m48s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=08h40m00s   !NEXT!        
qual=  0
disk=off
stop=08h40m48s   !NEXT!

!* --- Scan from 08h40m59s to 08h41m47s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=08h40m59s   !NEXT!        
qual=  0
disk=off
stop=08h41m47s   !NEXT!

!* --- Scan from 08h41m56s to 08h42m44s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=08h41m56s   !NEXT!        
qual=  0
disk=off
stop=08h42m44s   !NEXT!

!* --- Scan from 08h42m55s to 08h43m43s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=08h42m55s   !NEXT!        
qual=  0
disk=off
stop=08h43m43s   !NEXT!

!* --- Scan from 08h43m59s to 08h44m47s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=08h43m59s   !NEXT!        
qual=  0
disk=off
stop=08h44m47s   !NEXT!

!* --- Scan from 08h49m18s to 08h50m06s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=08h49m18s   !NEXT!        
qual=  0
disk=off
stop=08h50m06s   !NEXT!

!* --- Scan from 08h50m22s to 08h51m10s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=08h50m22s   !NEXT!        
qual=  0
disk=off
stop=08h51m10s   !NEXT!

!* --- Scan from 08h51m25s to 08h52m13s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=08h51m25s   !NEXT!        
qual=  0
disk=off
stop=08h52m13s   !NEXT!

!* --- Scan from 08h55m14s to 08h57m14s   Thu, 2006 Feb 16 --- *!
sname='3C279'  ra=12h56m11.166560s  dec=-05d47'21.52458"  qual=999  calib='N'
disk=off
stop=08h55m14s   !NEXT!        
qual=  0
disk=off
stop=08h57m14s   !NEXT!

!* --- Scan from 08h59m42s to 09h00m30s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=08h59m42s   !NEXT!        
qual=  0
disk=off
stop=09h00m30s   !NEXT!

!* --- Scan from 09h00m38s to 09h01m26s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=09h00m38s   !NEXT!        
qual=  0
disk=off
stop=09h01m26s   !NEXT!

!* --- Scan from 09h01m37s to 09h02m25s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=09h01m37s   !NEXT!        
qual=  0
disk=off
stop=09h02m25s   !NEXT!

!* --- Scan from 09h02m45s to 09h03m33s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=09h02m45s   !NEXT!        
qual=  0
disk=off
stop=09h03m33s   !NEXT!

!* --- Scan from 09h03m47s to 09h04m34s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=09h03m47s   !NEXT!        
qual=  0
disk=off
stop=09h04m34s   !NEXT!

!* --- Scan from 09h04m44s to 09h05m32s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=09h04m44s   !NEXT!        
qual=  0
disk=off
stop=09h05m32s   !NEXT!

!* --- Scan from 09h05m41s to 09h06m29s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=09h05m41s   !NEXT!        
qual=  0
disk=off
stop=09h06m29s   !NEXT!

!* --- Scan from 09h06m56s to 09h07m44s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=09h06m56s   !NEXT!        
qual=  0
disk=off
stop=09h07m44s   !NEXT!

!* --- Scan from 09h08m24s to 09h09m12s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=09h08m24s   !NEXT!        
qual=  0
disk=off
stop=09h09m12s   !NEXT!

!* --- Scan from 09h09m29s to 09h10m17s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=09h09m29s   !NEXT!        
qual=  0
disk=off
stop=09h10m17s   !NEXT!

!* --- Scan from 09h10m26s to 09h11m14s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=09h10m26s   !NEXT!        
qual=  0
disk=off
stop=09h11m14s   !NEXT!

!* --- Scan from 09h11m26s to 09h12m14s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=09h11m26s   !NEXT!        
qual=  0
disk=off
stop=09h12m14s   !NEXT!

!* --- Scan from 09h32m36s to 09h34m35s   Thu, 2006 Feb 16 --- *!
sname='J1310+3220'  ra=13h10m28.663845s  dec= 32d20'43.78295"  qual=999  calib='N'
disk=off
stop=09h32m36s   !NEXT!        
qual=  0
disk=off
stop=09h34m35s   !NEXT!

!* --- Scan from 09h37m18s to 09h38m06s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=09h37m18s   !NEXT!        
qual=  0
disk=off
stop=09h38m06s   !NEXT!

!* --- Scan from 09h40m22s to 09h42m22s   Thu, 2006 Feb 16 --- *!
sname='3C279'  ra=12h56m11.166560s  dec=-05d47'21.52458"  qual=999  calib='N'
disk=off
stop=09h40m22s   !NEXT!        
qual=  0
disk=off
stop=09h42m22s   !NEXT!

!* --- Scan from 10h32m58s to 10h34m58s   Thu, 2006 Feb 16 --- *!
sname='J1310+3220'  ra=13h10m28.663845s  dec= 32d20'43.78295"  qual=999  calib='N'
disk=off
stop=10h32m58s   !NEXT!        
qual=  0
disk=off
stop=10h34m58s   !NEXT!

!* --- Scan from 10h38m38s to 10h40m38s   Thu, 2006 Feb 16 --- *!
sname='3C279'  ra=12h56m11.166560s  dec=-05d47'21.52458"  qual=999  calib='N'
disk=off
stop=10h38m38s   !NEXT!        
qual=  0
disk=off
stop=10h40m38s   !NEXT!
disk=off
stop=10h40m43s   !NEXT!
     !QUIT! 
