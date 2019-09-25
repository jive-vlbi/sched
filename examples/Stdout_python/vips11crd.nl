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

!* --- Scan from 00h43m17s to 00h44m05s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=00h43m17s   !NEXT!        
qual=  0
disk=off
stop=00h44m05s   !NEXT!

!* --- Scan from 00h44m15s to 00h45m03s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=00h44m15s   !NEXT!        
qual=  0
disk=off
stop=00h45m03s   !NEXT!

!* --- Scan from 00h45m12s to 00h46m00s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=00h45m12s   !NEXT!        
qual=  0
disk=off
stop=00h46m00s   !NEXT!

!* --- Scan from 00h46m10s to 00h46m58s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=00h46m10s   !NEXT!        
qual=  0
disk=off
stop=00h46m58s   !NEXT!

!* --- Scan from 00h47m15s to 00h48m03s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=00h47m15s   !NEXT!        
qual=  0
disk=off
stop=00h48m03s   !NEXT!

!* --- Scan from 00h48m14s to 00h49m02s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=00h48m14s   !NEXT!        
qual=  0
disk=off
stop=00h49m02s   !NEXT!

!* --- Scan from 00h49m11s to 00h49m59s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=00h49m11s   !NEXT!        
qual=  0
disk=off
stop=00h49m59s   !NEXT!

!* --- Scan from 00h50m07s to 00h50m54s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=00h50m07s   !NEXT!        
qual=  0
disk=off
stop=00h50m54s   !NEXT!

!* --- Scan from 00h51m02s to 00h51m50s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=00h51m02s   !NEXT!        
qual=  0
disk=off
stop=00h51m50s   !NEXT!

!* --- Scan from 00h51m57s to 00h52m45s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=00h51m57s   !NEXT!        
qual=  0
disk=off
stop=00h52m45s   !NEXT!

!* --- Scan from 00h52m52s to 00h53m40s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=00h52m52s   !NEXT!        
qual=  0
disk=off
stop=00h53m40s   !NEXT!

!* --- Scan from 00h53m48s to 00h54m36s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=00h53m48s   !NEXT!        
qual=  0
disk=off
stop=00h54m36s   !NEXT!

!* --- Scan from 00h54m45s to 00h55m33s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=00h54m45s   !NEXT!        
qual=  0
disk=off
stop=00h55m33s   !NEXT!

!* --- Scan from 00h55m47s to 00h56m35s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=00h55m47s   !NEXT!        
qual=  0
disk=off
stop=00h56m35s   !NEXT!

!* --- Scan from 00h56m52s to 00h57m40s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=00h56m52s   !NEXT!        
qual=  0
disk=off
stop=00h57m40s   !NEXT!

!* --- Scan from 00h57m49s to 00h58m37s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=00h57m49s   !NEXT!        
qual=  0
disk=off
stop=00h58m37s   !NEXT!

!* --- Scan from 00h58m45s to 00h59m33s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=00h58m45s   !NEXT!        
qual=  0
disk=off
stop=00h59m33s   !NEXT!

!* --- Scan from 00h59m42s to 01h00m30s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=00h59m42s   !NEXT!        
qual=  0
disk=off
stop=01h00m30s   !NEXT!

!* --- Scan from 01h00m53s to 01h01m41s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=01h00m53s   !NEXT!        
qual=  0
disk=off
stop=01h01m41s   !NEXT!

!* --- Scan from 01h01m52s to 01h02m40s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=01h01m52s   !NEXT!        
qual=  0
disk=off
stop=01h02m40s   !NEXT!

!* --- Scan from 01h02m58s to 01h03m46s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=01h02m58s   !NEXT!        
qual=  0
disk=off
stop=01h03m46s   !NEXT!

!* --- Scan from 01h03m57s to 01h04m45s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=01h03m57s   !NEXT!        
qual=  0
disk=off
stop=01h04m45s   !NEXT!

!* --- Scan from 01h04m55s to 01h05m43s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=01h04m55s   !NEXT!        
qual=  0
disk=off
stop=01h05m43s   !NEXT!

!* --- Scan from 01h05m56s to 01h06m43s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=01h05m56s   !NEXT!        
qual=  0
disk=off
stop=01h06m43s   !NEXT!

!* --- Scan from 01h06m54s to 01h07m42s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=01h06m54s   !NEXT!        
qual=  0
disk=off
stop=01h07m42s   !NEXT!

!* --- Scan from 01h07m52s to 01h08m40s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=01h07m52s   !NEXT!        
qual=  0
disk=off
stop=01h08m40s   !NEXT!

!* --- Scan from 01h08m51s to 01h09m38s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=01h08m51s   !NEXT!        
qual=  0
disk=off
stop=01h09m38s   !NEXT!

!* --- Scan from 01h09m49s to 01h10m36s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=01h09m49s   !NEXT!        
qual=  0
disk=off
stop=01h10m36s   !NEXT!

!* --- Scan from 01h10m49s to 01h11m36s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=01h10m49s   !NEXT!        
qual=  0
disk=off
stop=01h11m36s   !NEXT!

!* --- Scan from 01h11m49s to 01h12m36s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=01h11m49s   !NEXT!        
qual=  0
disk=off
stop=01h12m36s   !NEXT!

!* --- Scan from 01h12m47s to 01h13m35s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=01h12m47s   !NEXT!        
qual=  0
disk=off
stop=01h13m35s   !NEXT!

!* --- Scan from 01h13m49s to 01h14m37s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=01h13m49s   !NEXT!        
qual=  0
disk=off
stop=01h14m37s   !NEXT!

!* --- Scan from 01h14m52s to 01h15m40s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=01h14m52s   !NEXT!        
qual=  0
disk=off
stop=01h15m40s   !NEXT!

!* --- Scan from 01h15m50s to 01h16m38s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=01h15m50s   !NEXT!        
qual=  0
disk=off
stop=01h16m38s   !NEXT!

!* --- Scan from 01h17m20s to 01h18m08s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=01h17m20s   !NEXT!        
qual=  0
disk=off
stop=01h18m08s   !NEXT!

!* --- Scan from 01h18m17s to 01h19m05s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=01h18m17s   !NEXT!        
qual=  0
disk=off
stop=01h19m05s   !NEXT!

!* --- Scan from 01h19m44s to 01h20m32s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=01h19m44s   !NEXT!        
qual=  0
disk=off
stop=01h20m32s   !NEXT!

!* --- Scan from 01h20m42s to 01h21m30s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=01h20m42s   !NEXT!        
qual=  0
disk=off
stop=01h21m30s   !NEXT!

!* --- Scan from 01h21m40s to 01h22m28s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=01h21m40s   !NEXT!        
qual=  0
disk=off
stop=01h22m28s   !NEXT!

!* --- Scan from 01h22m38s to 01h23m26s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=01h22m38s   !NEXT!        
qual=  0
disk=off
stop=01h23m26s   !NEXT!

!* --- Scan from 01h23m39s to 01h24m26s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=01h23m39s   !NEXT!        
qual=  0
disk=off
stop=01h24m26s   !NEXT!

!* --- Scan from 01h25m13s to 01h26m01s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=01h25m13s   !NEXT!        
qual=  0
disk=off
stop=01h26m01s   !NEXT!

!* --- Scan from 01h26m08s to 01h26m56s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=01h26m08s   !NEXT!        
qual=  0
disk=off
stop=01h26m56s   !NEXT!

!* --- Scan from 01h27m05s to 01h27m53s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=01h27m05s   !NEXT!        
qual=  0
disk=off
stop=01h27m53s   !NEXT!

!* --- Scan from 01h28m16s to 01h29m04s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=01h28m16s   !NEXT!        
qual=  0
disk=off
stop=01h29m04s   !NEXT!

!* --- Scan from 01h29m19s to 01h30m07s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=01h29m19s   !NEXT!        
qual=  0
disk=off
stop=01h30m07s   !NEXT!

!* --- Scan from 01h30m19s to 01h31m07s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=01h30m19s   !NEXT!        
qual=  0
disk=off
stop=01h31m07s   !NEXT!

!* --- Scan from 01h31m15s to 01h32m03s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=01h31m15s   !NEXT!        
qual=  0
disk=off
stop=01h32m03s   !NEXT!

!* --- Scan from 01h32m12s to 01h33m00s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=01h32m12s   !NEXT!        
qual=  0
disk=off
stop=01h33m00s   !NEXT!

!* --- Scan from 01h33m13s to 01h34m01s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=01h33m13s   !NEXT!        
qual=  0
disk=off
stop=01h34m01s   !NEXT!

!* --- Scan from 01h34m10s to 01h34m58s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=01h34m10s   !NEXT!        
qual=  0
disk=off
stop=01h34m58s   !NEXT!

!* --- Scan from 01h35m07s to 01h35m55s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=01h35m07s   !NEXT!        
qual=  0
disk=off
stop=01h35m55s   !NEXT!

!* --- Scan from 01h36m04s to 01h36m52s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=01h36m04s   !NEXT!        
qual=  0
disk=off
stop=01h36m52s   !NEXT!

!* --- Scan from 01h37m01s to 01h37m49s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=01h37m01s   !NEXT!        
qual=  0
disk=off
stop=01h37m49s   !NEXT!

!* --- Scan from 01h37m58s to 01h38m45s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=01h37m58s   !NEXT!        
qual=  0
disk=off
stop=01h38m45s   !NEXT!

!* --- Scan from 01h39m01s to 01h39m49s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=01h39m01s   !NEXT!        
qual=  0
disk=off
stop=01h39m49s   !NEXT!

!* --- Scan from 01h39m56s to 01h40m44s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=01h39m56s   !NEXT!        
qual=  0
disk=off
stop=01h40m44s   !NEXT!

!* --- Scan from 01h40m52s to 01h41m40s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=01h40m52s   !NEXT!        
qual=  0
disk=off
stop=01h41m40s   !NEXT!

!* --- Scan from 01h41m47s to 01h42m35s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=01h41m47s   !NEXT!        
qual=  0
disk=off
stop=01h42m35s   !NEXT!

!* --- Scan from 01h42m43s to 01h43m31s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=01h42m43s   !NEXT!        
qual=  0
disk=off
stop=01h43m31s   !NEXT!

!* --- Scan from 01h43m38s to 01h44m26s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=01h43m38s   !NEXT!        
qual=  0
disk=off
stop=01h44m26s   !NEXT!

!* --- Scan from 01h44m34s to 01h45m22s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=01h44m34s   !NEXT!        
qual=  0
disk=off
stop=01h45m22s   !NEXT!

!* --- Scan from 01h45m29s to 01h46m17s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=01h45m29s   !NEXT!        
qual=  0
disk=off
stop=01h46m17s   !NEXT!

!* --- Scan from 01h46m29s to 01h47m17s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=01h46m29s   !NEXT!        
qual=  0
disk=off
stop=01h47m17s   !NEXT!

!* --- Scan from 01h47m30s to 01h48m18s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=01h47m30s   !NEXT!        
qual=  0
disk=off
stop=01h48m18s   !NEXT!

!* --- Scan from 01h50m05s to 01h52m05s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=01h50m05s   !NEXT!        
qual=  0
disk=off
stop=01h52m05s   !NEXT!

!* --- Scan from 01h53m38s to 01h54m26s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=01h53m38s   !NEXT!        
qual=  0
disk=off
stop=01h54m26s   !NEXT!

!* --- Scan from 01h54m36s to 01h55m24s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=01h54m36s   !NEXT!        
qual=  0
disk=off
stop=01h55m24s   !NEXT!

!* --- Scan from 01h55m38s to 01h56m26s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=01h55m38s   !NEXT!        
qual=  0
disk=off
stop=01h56m26s   !NEXT!

!* --- Scan from 01h56m41s to 01h57m29s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=01h56m41s   !NEXT!        
qual=  0
disk=off
stop=01h57m29s   !NEXT!

!* --- Scan from 01h57m43s to 01h58m31s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=01h57m43s   !NEXT!        
qual=  0
disk=off
stop=01h58m31s   !NEXT!

!* --- Scan from 01h58m46s to 01h59m34s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=01h58m46s   !NEXT!        
qual=  0
disk=off
stop=01h59m34s   !NEXT!

!* --- Scan from 01h59m49s to 02h00m37s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=01h59m49s   !NEXT!        
qual=  0
disk=off
stop=02h00m37s   !NEXT!

!* --- Scan from 02h00m49s to 02h01m36s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=02h00m49s   !NEXT!        
qual=  0
disk=off
stop=02h01m36s   !NEXT!

!* --- Scan from 02h01m50s to 02h02m37s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=02h01m50s   !NEXT!        
qual=  0
disk=off
stop=02h02m37s   !NEXT!

!* --- Scan from 02h02m49s to 02h03m36s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=02h02m49s   !NEXT!        
qual=  0
disk=off
stop=02h03m36s   !NEXT!

!* --- Scan from 02h03m49s to 02h04m37s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=02h03m49s   !NEXT!        
qual=  0
disk=off
stop=02h04m37s   !NEXT!

!* --- Scan from 02h04m48s to 02h05m36s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=02h04m48s   !NEXT!        
qual=  0
disk=off
stop=02h05m36s   !NEXT!

!* --- Scan from 02h05m48s to 02h06m36s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=02h05m48s   !NEXT!        
qual=  0
disk=off
stop=02h06m36s   !NEXT!

!* --- Scan from 02h06m46s to 02h07m34s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=02h06m46s   !NEXT!        
qual=  0
disk=off
stop=02h07m34s   !NEXT!

!* --- Scan from 02h07m46s to 02h08m34s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=02h07m46s   !NEXT!        
qual=  0
disk=off
stop=02h08m34s   !NEXT!

!* --- Scan from 02h08m47s to 02h09m35s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=02h08m47s   !NEXT!        
qual=  0
disk=off
stop=02h09m35s   !NEXT!

!* --- Scan from 02h09m45s to 02h10m33s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=02h09m45s   !NEXT!        
qual=  0
disk=off
stop=02h10m33s   !NEXT!

!* --- Scan from 02h10m45s to 02h11m32s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=02h10m45s   !NEXT!        
qual=  0
disk=off
stop=02h11m32s   !NEXT!

!* --- Scan from 02h11m43s to 02h12m31s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=02h11m43s   !NEXT!        
qual=  0
disk=off
stop=02h12m31s   !NEXT!

!* --- Scan from 02h12m42s to 02h13m30s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=02h12m42s   !NEXT!        
qual=  0
disk=off
stop=02h13m30s   !NEXT!

!* --- Scan from 02h13m45s to 02h14m32s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=02h13m45s   !NEXT!        
qual=  0
disk=off
stop=02h14m32s   !NEXT!

!* --- Scan from 02h15m02s to 02h15m50s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=02h15m02s   !NEXT!        
qual=  0
disk=off
stop=02h15m50s   !NEXT!

!* --- Scan from 02h16m09s to 02h16m57s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=02h16m09s   !NEXT!        
qual=  0
disk=off
stop=02h16m57s   !NEXT!

!* --- Scan from 02h17m06s to 02h17m54s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=02h17m06s   !NEXT!        
qual=  0
disk=off
stop=02h17m54s   !NEXT!

!* --- Scan from 02h18m13s to 02h19m01s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=02h18m13s   !NEXT!        
qual=  0
disk=off
stop=02h19m01s   !NEXT!

!* --- Scan from 02h19m11s to 02h19m59s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=02h19m11s   !NEXT!        
qual=  0
disk=off
stop=02h19m59s   !NEXT!

!* --- Scan from 02h20m08s to 02h20m56s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=02h20m08s   !NEXT!        
qual=  0
disk=off
stop=02h20m56s   !NEXT!

!* --- Scan from 02h21m07s to 02h21m55s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=02h21m07s   !NEXT!        
qual=  0
disk=off
stop=02h21m55s   !NEXT!

!* --- Scan from 02h22m08s to 02h22m56s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=02h22m08s   !NEXT!        
qual=  0
disk=off
stop=02h22m56s   !NEXT!

!* --- Scan from 02h23m07s to 02h23m55s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=02h23m07s   !NEXT!        
qual=  0
disk=off
stop=02h23m55s   !NEXT!

!* --- Scan from 02h24m08s to 02h24m55s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=02h24m08s   !NEXT!        
qual=  0
disk=off
stop=02h24m55s   !NEXT!

!* --- Scan from 02h25m07s to 02h25m55s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=02h25m07s   !NEXT!        
qual=  0
disk=off
stop=02h25m55s   !NEXT!

!* --- Scan from 02h26m22s to 02h27m10s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=02h26m22s   !NEXT!        
qual=  0
disk=off
stop=02h27m10s   !NEXT!

!* --- Scan from 02h27m18s to 02h28m06s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=02h27m18s   !NEXT!        
qual=  0
disk=off
stop=02h28m06s   !NEXT!

!* --- Scan from 02h28m15s to 02h29m03s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=02h28m15s   !NEXT!        
qual=  0
disk=off
stop=02h29m03s   !NEXT!

!* --- Scan from 02h29m30s to 02h30m18s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=02h29m30s   !NEXT!        
qual=  0
disk=off
stop=02h30m18s   !NEXT!

!* --- Scan from 02h30m33s to 02h31m20s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=02h30m33s   !NEXT!        
qual=  0
disk=off
stop=02h31m20s   !NEXT!

!* --- Scan from 02h32m04s to 02h32m52s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=02h32m04s   !NEXT!        
qual=  0
disk=off
stop=02h32m52s   !NEXT!

!* --- Scan from 02h33m04s to 02h33m52s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=02h33m04s   !NEXT!        
qual=  0
disk=off
stop=02h33m52s   !NEXT!

!* --- Scan from 02h34m06s to 02h34m54s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=02h34m06s   !NEXT!        
qual=  0
disk=off
stop=02h34m54s   !NEXT!

!* --- Scan from 02h35m08s to 02h35m55s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=02h35m08s   !NEXT!        
qual=  0
disk=off
stop=02h35m55s   !NEXT!

!* --- Scan from 02h36m07s to 02h36m55s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=02h36m07s   !NEXT!        
qual=  0
disk=off
stop=02h36m55s   !NEXT!

!* --- Scan from 02h37m07s to 02h37m54s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=02h37m07s   !NEXT!        
qual=  0
disk=off
stop=02h37m54s   !NEXT!

!* --- Scan from 02h38m06s to 02h38m54s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=02h38m06s   !NEXT!        
qual=  0
disk=off
stop=02h38m54s   !NEXT!

!* --- Scan from 02h39m04s to 02h39m52s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=02h39m04s   !NEXT!        
qual=  0
disk=off
stop=02h39m52s   !NEXT!

!* --- Scan from 02h40m03s to 02h40m51s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=02h40m03s   !NEXT!        
qual=  0
disk=off
stop=02h40m51s   !NEXT!

!* --- Scan from 02h41m05s to 02h41m53s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=02h41m05s   !NEXT!        
qual=  0
disk=off
stop=02h41m53s   !NEXT!

!* --- Scan from 02h42m07s to 02h42m55s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=02h42m07s   !NEXT!        
qual=  0
disk=off
stop=02h42m55s   !NEXT!

!* --- Scan from 02h43m08s to 02h43m56s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=02h43m08s   !NEXT!        
qual=  0
disk=off
stop=02h43m56s   !NEXT!

!* --- Scan from 02h44m15s to 02h45m03s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=02h44m15s   !NEXT!        
qual=  0
disk=off
stop=02h45m03s   !NEXT!

!* --- Scan from 02h45m13s to 02h46m01s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=02h45m13s   !NEXT!        
qual=  0
disk=off
stop=02h46m01s   !NEXT!

!* --- Scan from 02h47m23s to 02h48m11s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=02h47m23s   !NEXT!        
qual=  0
disk=off
stop=02h48m11s   !NEXT!

!* --- Scan from 02h48m46s to 02h49m34s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=02h48m46s   !NEXT!        
qual=  0
disk=off
stop=02h49m34s   !NEXT!

!* --- Scan from 02h51m35s to 02h53m35s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=02h51m35s   !NEXT!        
qual=  0
disk=off
stop=02h53m35s   !NEXT!

!* --- Scan from 02h55m24s to 02h56m12s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=02h55m24s   !NEXT!        
qual=  0
disk=off
stop=02h56m12s   !NEXT!

!* --- Scan from 02h58m24s to 02h59m12s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=02h58m24s   !NEXT!        
qual=  0
disk=off
stop=02h59m12s   !NEXT!

!* --- Scan from 02h59m22s to 03h00m10s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=02h59m22s   !NEXT!        
qual=  0
disk=off
stop=03h00m10s   !NEXT!

!* --- Scan from 03h00m24s to 03h01m12s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=03h00m24s   !NEXT!        
qual=  0
disk=off
stop=03h01m12s   !NEXT!

!* --- Scan from 03h01m41s to 03h02m29s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=03h01m41s   !NEXT!        
qual=  0
disk=off
stop=03h02m29s   !NEXT!

!* --- Scan from 03h02m43s to 03h03m31s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=03h02m43s   !NEXT!        
qual=  0
disk=off
stop=03h03m31s   !NEXT!

!* --- Scan from 03h03m56s to 03h04m44s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=03h03m56s   !NEXT!        
qual=  0
disk=off
stop=03h04m44s   !NEXT!

!* --- Scan from 03h04m54s to 03h05m42s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=03h04m54s   !NEXT!        
qual=  0
disk=off
stop=03h05m42s   !NEXT!

!* --- Scan from 03h05m54s to 03h06m42s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=03h05m54s   !NEXT!        
qual=  0
disk=off
stop=03h06m42s   !NEXT!

!* --- Scan from 03h07m03s to 03h07m51s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=03h07m03s   !NEXT!        
qual=  0
disk=off
stop=03h07m51s   !NEXT!

!* --- Scan from 03h08m04s to 03h08m51s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=03h08m04s   !NEXT!        
qual=  0
disk=off
stop=03h08m51s   !NEXT!

!* --- Scan from 03h09m06s to 03h09m54s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=03h09m06s   !NEXT!        
qual=  0
disk=off
stop=03h09m54s   !NEXT!

!* --- Scan from 03h10m05s to 03h10m52s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=03h10m05s   !NEXT!        
qual=  0
disk=off
stop=03h10m52s   !NEXT!

!* --- Scan from 03h11m12s to 03h12m00s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=03h11m12s   !NEXT!        
qual=  0
disk=off
stop=03h12m00s   !NEXT!

!* --- Scan from 03h12m22s to 03h13m10s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=03h12m22s   !NEXT!        
qual=  0
disk=off
stop=03h13m10s   !NEXT!

!* --- Scan from 03h13m31s to 03h14m19s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=03h13m31s   !NEXT!        
qual=  0
disk=off
stop=03h14m19s   !NEXT!

!* --- Scan from 03h15m14s to 03h16m02s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=03h15m14s   !NEXT!        
qual=  0
disk=off
stop=03h16m02s   !NEXT!

!* --- Scan from 03h16m15s to 03h17m03s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=03h16m15s   !NEXT!        
qual=  0
disk=off
stop=03h17m03s   !NEXT!

!* --- Scan from 03h17m14s to 03h18m02s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=03h17m14s   !NEXT!        
qual=  0
disk=off
stop=03h18m02s   !NEXT!

!* --- Scan from 03h18m24s to 03h19m12s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=03h18m24s   !NEXT!        
qual=  0
disk=off
stop=03h19m12s   !NEXT!

!* --- Scan from 03h19m28s to 03h20m16s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=03h19m28s   !NEXT!        
qual=  0
disk=off
stop=03h20m16s   !NEXT!

!* --- Scan from 03h20m31s to 03h21m19s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=03h20m31s   !NEXT!        
qual=  0
disk=off
stop=03h21m19s   !NEXT!

!* --- Scan from 03h21m31s to 03h22m18s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=03h21m31s   !NEXT!        
qual=  0
disk=off
stop=03h22m18s   !NEXT!

!* --- Scan from 03h23m10s to 03h23m58s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=03h23m10s   !NEXT!        
qual=  0
disk=off
stop=03h23m58s   !NEXT!

!* --- Scan from 03h24m10s to 03h24m58s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=03h24m10s   !NEXT!        
qual=  0
disk=off
stop=03h24m58s   !NEXT!

!* --- Scan from 03h25m07s to 03h25m55s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=03h25m07s   !NEXT!        
qual=  0
disk=off
stop=03h25m55s   !NEXT!

!* --- Scan from 03h26m23s to 03h27m11s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=03h26m23s   !NEXT!        
qual=  0
disk=off
stop=03h27m11s   !NEXT!

!* --- Scan from 03h27m28s to 03h28m16s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=03h27m28s   !NEXT!        
qual=  0
disk=off
stop=03h28m16s   !NEXT!

!* --- Scan from 03h28m31s to 03h29m19s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=03h28m31s   !NEXT!        
qual=  0
disk=off
stop=03h29m19s   !NEXT!

!* --- Scan from 03h30m09s to 03h30m57s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=03h30m09s   !NEXT!        
qual=  0
disk=off
stop=03h30m57s   !NEXT!

!* --- Scan from 03h31m05s to 03h31m53s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=03h31m05s   !NEXT!        
qual=  0
disk=off
stop=03h31m53s   !NEXT!

!* --- Scan from 03h32m01s to 03h32m49s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=03h32m01s   !NEXT!        
qual=  0
disk=off
stop=03h32m49s   !NEXT!

!* --- Scan from 03h32m57s to 03h33m45s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=03h32m57s   !NEXT!        
qual=  0
disk=off
stop=03h33m45s   !NEXT!

!* --- Scan from 03h33m53s to 03h34m41s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=03h33m53s   !NEXT!        
qual=  0
disk=off
stop=03h34m41s   !NEXT!

!* --- Scan from 03h34m49s to 03h35m37s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=03h34m49s   !NEXT!        
qual=  0
disk=off
stop=03h35m37s   !NEXT!

!* --- Scan from 03h35m45s to 03h36m33s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=03h35m45s   !NEXT!        
qual=  0
disk=off
stop=03h36m33s   !NEXT!

!* --- Scan from 03h36m41s to 03h37m29s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=03h36m41s   !NEXT!        
qual=  0
disk=off
stop=03h37m29s   !NEXT!

!* --- Scan from 03h37m39s to 03h38m27s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=03h37m39s   !NEXT!        
qual=  0
disk=off
stop=03h38m27s   !NEXT!

!* --- Scan from 03h41m37s to 03h42m25s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=03h41m37s   !NEXT!        
qual=  0
disk=off
stop=03h42m25s   !NEXT!

!* --- Scan from 03h42m49s to 03h43m37s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=03h42m49s   !NEXT!        
qual=  0
disk=off
stop=03h43m37s   !NEXT!

!* --- Scan from 03h44m32s to 03h45m20s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=03h44m32s   !NEXT!        
qual=  0
disk=off
stop=03h45m20s   !NEXT!

!* --- Scan from 03h46m13s to 03h47m01s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=03h46m13s   !NEXT!        
qual=  0
disk=off
stop=03h47m01s   !NEXT!

!* --- Scan from 03h47m11s to 03h47m59s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=03h47m11s   !NEXT!        
qual=  0
disk=off
stop=03h47m59s   !NEXT!

!* --- Scan from 03h48m14s to 03h49m02s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=03h48m14s   !NEXT!        
qual=  0
disk=off
stop=03h49m02s   !NEXT!

!* --- Scan from 03h49m42s to 03h50m30s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=03h49m42s   !NEXT!        
qual=  0
disk=off
stop=03h50m30s   !NEXT!

!* --- Scan from 03h52m40s to 03h54m39s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=03h52m40s   !NEXT!        
qual=  0
disk=off
stop=03h54m39s   !NEXT!

!* --- Scan from 03h57m02s to 03h57m50s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=03h57m02s   !NEXT!        
qual=  0
disk=off
stop=03h57m50s   !NEXT!

!* --- Scan from 03h58m00s to 03h58m48s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=03h58m00s   !NEXT!        
qual=  0
disk=off
stop=03h58m48s   !NEXT!

!* --- Scan from 03h59m00s to 03h59m48s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=03h59m00s   !NEXT!        
qual=  0
disk=off
stop=03h59m48s   !NEXT!

!* --- Scan from 03h59m59s to 04h00m47s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=03h59m59s   !NEXT!        
qual=  0
disk=off
stop=04h00m47s   !NEXT!

!* --- Scan from 04h00m57s to 04h01m45s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=04h00m57s   !NEXT!        
qual=  0
disk=off
stop=04h01m45s   !NEXT!

!* --- Scan from 04h01m58s to 04h02m45s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=04h01m58s   !NEXT!        
qual=  0
disk=off
stop=04h02m45s   !NEXT!

!* --- Scan from 04h02m57s to 04h03m45s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=04h02m57s   !NEXT!        
qual=  0
disk=off
stop=04h03m45s   !NEXT!

!* --- Scan from 04h03m55s to 04h04m43s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=04h03m55s   !NEXT!        
qual=  0
disk=off
stop=04h04m43s   !NEXT!

!* --- Scan from 04h04m55s to 04h05m43s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=04h04m55s   !NEXT!        
qual=  0
disk=off
stop=04h05m43s   !NEXT!

!* --- Scan from 04h05m54s to 04h06m41s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=04h05m54s   !NEXT!        
qual=  0
disk=off
stop=04h06m41s   !NEXT!

!* --- Scan from 04h06m55s to 04h07m43s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=04h06m55s   !NEXT!        
qual=  0
disk=off
stop=04h07m43s   !NEXT!

!* --- Scan from 04h07m53s to 04h08m41s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=04h07m53s   !NEXT!        
qual=  0
disk=off
stop=04h08m41s   !NEXT!

!* --- Scan from 04h08m53s to 04h09m41s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=04h08m53s   !NEXT!        
qual=  0
disk=off
stop=04h09m41s   !NEXT!

!* --- Scan from 04h09m59s to 04h10m47s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=04h09m59s   !NEXT!        
qual=  0
disk=off
stop=04h10m47s   !NEXT!

!* --- Scan from 04h11m00s to 04h11m48s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=04h11m00s   !NEXT!        
qual=  0
disk=off
stop=04h11m48s   !NEXT!

!* --- Scan from 04h12m00s to 04h12m48s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=04h12m00s   !NEXT!        
qual=  0
disk=off
stop=04h12m48s   !NEXT!

!* --- Scan from 04h13m04s to 04h13m52s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=04h13m04s   !NEXT!        
qual=  0
disk=off
stop=04h13m52s   !NEXT!

!* --- Scan from 04h13m59s to 04h14m47s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=04h13m59s   !NEXT!        
qual=  0
disk=off
stop=04h14m47s   !NEXT!

!* --- Scan from 04h14m58s to 04h15m46s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=04h14m58s   !NEXT!        
qual=  0
disk=off
stop=04h15m46s   !NEXT!

!* --- Scan from 04h16m08s to 04h16m56s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=04h16m08s   !NEXT!        
qual=  0
disk=off
stop=04h16m56s   !NEXT!

!* --- Scan from 04h17m05s to 04h17m53s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=04h17m05s   !NEXT!        
qual=  0
disk=off
stop=04h17m53s   !NEXT!

!* --- Scan from 04h18m14s to 04h19m02s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=04h18m14s   !NEXT!        
qual=  0
disk=off
stop=04h19m02s   !NEXT!

!* --- Scan from 04h19m13s to 04h20m01s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=04h19m13s   !NEXT!        
qual=  0
disk=off
stop=04h20m01s   !NEXT!

!* --- Scan from 04h20m10s to 04h20m58s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=04h20m10s   !NEXT!        
qual=  0
disk=off
stop=04h20m58s   !NEXT!

!* --- Scan from 04h21m09s to 04h21m57s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=04h21m09s   !NEXT!        
qual=  0
disk=off
stop=04h21m57s   !NEXT!

!* --- Scan from 04h22m12s to 04h23m00s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=04h22m12s   !NEXT!        
qual=  0
disk=off
stop=04h23m00s   !NEXT!

!* --- Scan from 04h23m11s to 04h23m58s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=04h23m11s   !NEXT!        
qual=  0
disk=off
stop=04h23m58s   !NEXT!

!* --- Scan from 04h24m15s to 04h25m03s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=04h24m15s   !NEXT!        
qual=  0
disk=off
stop=04h25m03s   !NEXT!

!* --- Scan from 04h25m23s to 04h26m10s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=04h25m23s   !NEXT!        
qual=  0
disk=off
stop=04h26m10s   !NEXT!

!* --- Scan from 04h26m26s to 04h28m26s   Thu, 2006 Feb 16 --- *!
sname='J0854+2006'  ra=08h54m48.874924s  dec= 20d06'30.64088"  qual=999  calib='N'
disk=off
stop=04h26m26s   !NEXT!        
qual=  0
disk=off
stop=04h28m26s   !NEXT!

!* --- Scan from 04h28m41s to 04h29m29s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=04h28m41s   !NEXT!        
qual=  0
disk=off
stop=04h29m29s   !NEXT!

!* --- Scan from 04h29m38s to 04h30m26s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=04h29m38s   !NEXT!        
qual=  0
disk=off
stop=04h30m26s   !NEXT!

!* --- Scan from 04h30m39s to 04h31m27s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=04h30m39s   !NEXT!        
qual=  0
disk=off
stop=04h31m27s   !NEXT!

!* --- Scan from 04h31m35s to 04h32m22s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=04h31m35s   !NEXT!        
qual=  0
disk=off
stop=04h32m22s   !NEXT!

!* --- Scan from 04h32m39s to 04h33m26s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=04h32m39s   !NEXT!        
qual=  0
disk=off
stop=04h33m26s   !NEXT!

!* --- Scan from 04h33m40s to 04h34m28s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=04h33m40s   !NEXT!        
qual=  0
disk=off
stop=04h34m28s   !NEXT!

!* --- Scan from 04h34m51s to 04h35m39s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=04h34m51s   !NEXT!        
qual=  0
disk=off
stop=04h35m39s   !NEXT!

!* --- Scan from 04h35m59s to 04h36m47s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=04h35m59s   !NEXT!        
qual=  0
disk=off
stop=04h36m47s   !NEXT!

!* --- Scan from 04h37m33s to 04h38m21s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=04h37m33s   !NEXT!        
qual=  0
disk=off
stop=04h38m21s   !NEXT!

!* --- Scan from 04h38m37s to 04h39m25s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=04h38m37s   !NEXT!        
qual=  0
disk=off
stop=04h39m25s   !NEXT!

!* --- Scan from 04h39m40s to 04h40m28s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=04h39m40s   !NEXT!        
qual=  0
disk=off
stop=04h40m28s   !NEXT!

!* --- Scan from 04h40m43s to 04h41m30s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=04h40m43s   !NEXT!        
qual=  0
disk=off
stop=04h41m30s   !NEXT!

!* --- Scan from 04h41m48s to 04h42m36s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=04h41m48s   !NEXT!        
qual=  0
disk=off
stop=04h42m36s   !NEXT!

!* --- Scan from 04h43m27s to 04h44m15s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=04h43m27s   !NEXT!        
qual=  0
disk=off
stop=04h44m15s   !NEXT!

!* --- Scan from 04h44m27s to 04h45m15s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=04h44m27s   !NEXT!        
qual=  0
disk=off
stop=04h45m15s   !NEXT!

!* --- Scan from 04h45m23s to 04h46m11s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=04h45m23s   !NEXT!        
qual=  0
disk=off
stop=04h46m11s   !NEXT!

!* --- Scan from 04h46m26s to 04h47m14s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=04h46m26s   !NEXT!        
qual=  0
disk=off
stop=04h47m14s   !NEXT!

!* --- Scan from 04h47m27s to 04h49m27s   Thu, 2006 Feb 16 --- *!
sname='J0854+2006'  ra=08h54m48.874924s  dec= 20d06'30.64088"  qual=999  calib='N'
disk=off
stop=04h47m27s   !NEXT!        
qual=  0
disk=off
stop=04h49m27s   !NEXT!

!* --- Scan from 04h49m37s to 04h50m25s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=04h49m37s   !NEXT!        
qual=  0
disk=off
stop=04h50m25s   !NEXT!

!* --- Scan from 04h50m33s to 04h51m21s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=04h50m33s   !NEXT!        
qual=  0
disk=off
stop=04h51m21s   !NEXT!

!* --- Scan from 04h52m11s to 04h52m59s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=04h52m11s   !NEXT!        
qual=  0
disk=off
stop=04h52m59s   !NEXT!

!* --- Scan from 04h53m09s to 04h53m57s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=04h53m09s   !NEXT!        
qual=  0
disk=off
stop=04h53m57s   !NEXT!

!* --- Scan from 04h54m13s to 04h55m01s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=04h54m13s   !NEXT!        
qual=  0
disk=off
stop=04h55m01s   !NEXT!

!* --- Scan from 04h55m14s to 04h56m02s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=04h55m14s   !NEXT!        
qual=  0
disk=off
stop=04h56m02s   !NEXT!

!* --- Scan from 04h56m13s to 04h57m01s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=04h56m13s   !NEXT!        
qual=  0
disk=off
stop=04h57m01s   !NEXT!

!* --- Scan from 04h57m18s to 04h58m06s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=04h57m18s   !NEXT!        
qual=  0
disk=off
stop=04h58m06s   !NEXT!

!* --- Scan from 04h58m20s to 04h59m08s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=04h58m20s   !NEXT!        
qual=  0
disk=off
stop=04h59m08s   !NEXT!

!* --- Scan from 04h59m20s to 05h00m07s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=04h59m20s   !NEXT!        
qual=  0
disk=off
stop=05h00m07s   !NEXT!

!* --- Scan from 05h00m24s to 05h01m12s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=05h00m24s   !NEXT!        
qual=  0
disk=off
stop=05h01m12s   !NEXT!

!* --- Scan from 05h01m35s to 05h02m23s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=05h01m35s   !NEXT!        
qual=  0
disk=off
stop=05h02m23s   !NEXT!

!* --- Scan from 05h02m43s to 05h03m31s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=05h02m43s   !NEXT!        
qual=  0
disk=off
stop=05h03m31s   !NEXT!

!* --- Scan from 05h03m49s to 05h04m37s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=05h03m49s   !NEXT!        
qual=  0
disk=off
stop=05h04m37s   !NEXT!

!* --- Scan from 05h04m52s to 05h05m40s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=05h04m52s   !NEXT!        
qual=  0
disk=off
stop=05h05m40s   !NEXT!

!* --- Scan from 05h05m56s to 05h06m44s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=05h05m56s   !NEXT!        
qual=  0
disk=off
stop=05h06m44s   !NEXT!

!* --- Scan from 05h06m58s to 05h07m46s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=05h06m58s   !NEXT!        
qual=  0
disk=off
stop=05h07m46s   !NEXT!

!* --- Scan from 05h08m07s to 05h08m54s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=05h08m07s   !NEXT!        
qual=  0
disk=off
stop=05h08m54s   !NEXT!

!* --- Scan from 05h09m10s to 05h09m58s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=05h09m10s   !NEXT!        
qual=  0
disk=off
stop=05h09m58s   !NEXT!

!* --- Scan from 05h10m08s to 05h10m56s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=05h10m08s   !NEXT!        
qual=  0
disk=off
stop=05h10m56s   !NEXT!

!* --- Scan from 05h11m07s to 05h11m54s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=05h11m07s   !NEXT!        
qual=  0
disk=off
stop=05h11m54s   !NEXT!

!* --- Scan from 05h12m06s to 05h12m54s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=05h12m06s   !NEXT!        
qual=  0
disk=off
stop=05h12m54s   !NEXT!

!* --- Scan from 05h13m25s to 05h14m13s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=05h13m25s   !NEXT!        
qual=  0
disk=off
stop=05h14m13s   !NEXT!

!* --- Scan from 05h14m29s to 05h15m17s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=05h14m29s   !NEXT!        
qual=  0
disk=off
stop=05h15m17s   !NEXT!

!* --- Scan from 05h15m24s to 05h16m12s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=05h15m24s   !NEXT!        
qual=  0
disk=off
stop=05h16m12s   !NEXT!

!* --- Scan from 05h16m24s to 05h17m12s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=05h16m24s   !NEXT!        
qual=  0
disk=off
stop=05h17m12s   !NEXT!

!* --- Scan from 05h17m33s to 05h18m21s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=05h17m33s   !NEXT!        
qual=  0
disk=off
stop=05h18m21s   !NEXT!

!* --- Scan from 05h18m37s to 05h19m25s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=05h18m37s   !NEXT!        
qual=  0
disk=off
stop=05h19m25s   !NEXT!

!* --- Scan from 05h19m41s to 05h20m29s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=05h19m41s   !NEXT!        
qual=  0
disk=off
stop=05h20m29s   !NEXT!

!* --- Scan from 05h20m38s to 05h21m26s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=05h20m38s   !NEXT!        
qual=  0
disk=off
stop=05h21m26s   !NEXT!

!* --- Scan from 05h23m44s to 05h25m44s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=05h23m44s   !NEXT!        
qual=  0
disk=off
stop=05h25m44s   !NEXT!

!* --- Scan from 05h28m30s to 05h29m18s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=05h28m30s   !NEXT!        
qual=  0
disk=off
stop=05h29m18s   !NEXT!

!* --- Scan from 05h29m28s to 05h30m16s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=05h29m28s   !NEXT!        
qual=  0
disk=off
stop=05h30m16s   !NEXT!

!* --- Scan from 05h30m31s to 05h31m19s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=05h30m31s   !NEXT!        
qual=  0
disk=off
stop=05h31m19s   !NEXT!

!* --- Scan from 05h31m27s to 05h32m15s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=05h31m27s   !NEXT!        
qual=  0
disk=off
stop=05h32m15s   !NEXT!

!* --- Scan from 05h32m25s to 05h33m13s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=05h32m25s   !NEXT!        
qual=  0
disk=off
stop=05h33m13s   !NEXT!

!* --- Scan from 05h33m26s to 05h34m14s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=05h33m26s   !NEXT!        
qual=  0
disk=off
stop=05h34m14s   !NEXT!

!* --- Scan from 05h34m36s to 05h35m24s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=05h34m36s   !NEXT!        
qual=  0
disk=off
stop=05h35m24s   !NEXT!

!* --- Scan from 05h35m38s to 05h36m25s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=05h35m38s   !NEXT!        
qual=  0
disk=off
stop=05h36m25s   !NEXT!

!* --- Scan from 05h36m45s to 05h37m33s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=05h36m45s   !NEXT!        
qual=  0
disk=off
stop=05h37m33s   !NEXT!

!* --- Scan from 05h38m27s to 05h39m15s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=05h38m27s   !NEXT!        
qual=  0
disk=off
stop=05h39m15s   !NEXT!

!* --- Scan from 05h39m49s to 05h40m37s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=05h39m49s   !NEXT!        
qual=  0
disk=off
stop=05h40m37s   !NEXT!

!* --- Scan from 05h42m15s to 05h43m02s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=05h42m15s   !NEXT!        
qual=  0
disk=off
stop=05h43m02s   !NEXT!

!* --- Scan from 05h43m15s to 05h44m02s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=05h43m15s   !NEXT!        
qual=  0
disk=off
stop=05h44m02s   !NEXT!

!* --- Scan from 05h44m15s to 05h45m03s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=05h44m15s   !NEXT!        
qual=  0
disk=off
stop=05h45m03s   !NEXT!

!* --- Scan from 05h45m15s to 05h46m03s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=05h45m15s   !NEXT!        
qual=  0
disk=off
stop=05h46m03s   !NEXT!

!* --- Scan from 05h46m26s to 05h47m14s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=05h46m26s   !NEXT!        
qual=  0
disk=off
stop=05h47m14s   !NEXT!

!* --- Scan from 05h48m47s to 05h49m35s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=05h48m47s   !NEXT!        
qual=  0
disk=off
stop=05h49m35s   !NEXT!

!* --- Scan from 05h50m37s to 05h51m25s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=05h50m37s   !NEXT!        
qual=  0
disk=off
stop=05h51m25s   !NEXT!

!* --- Scan from 05h51m43s to 05h52m31s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=05h51m43s   !NEXT!        
qual=  0
disk=off
stop=05h52m31s   !NEXT!

!* --- Scan from 05h52m46s to 05h53m34s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=05h52m46s   !NEXT!        
qual=  0
disk=off
stop=05h53m34s   !NEXT!

!* --- Scan from 05h53m49s to 05h54m37s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=05h53m49s   !NEXT!        
qual=  0
disk=off
stop=05h54m37s   !NEXT!

!* --- Scan from 05h55m44s to 05h56m31s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=05h55m44s   !NEXT!        
qual=  0
disk=off
stop=05h56m31s   !NEXT!

!* --- Scan from 05h56m41s to 05h57m29s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=05h56m41s   !NEXT!        
qual=  0
disk=off
stop=05h57m29s   !NEXT!

!* --- Scan from 05h57m44s to 05h58m32s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=05h57m44s   !NEXT!        
qual=  0
disk=off
stop=05h58m32s   !NEXT!

!* --- Scan from 05h58m49s to 05h59m37s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=05h58m49s   !NEXT!        
qual=  0
disk=off
stop=05h59m37s   !NEXT!

!* --- Scan from 05h59m48s to 06h00m36s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=05h59m48s   !NEXT!        
qual=  0
disk=off
stop=06h00m36s   !NEXT!

!* --- Scan from 06h00m49s to 06h01m36s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=06h00m49s   !NEXT!        
qual=  0
disk=off
stop=06h01m36s   !NEXT!

!* --- Scan from 06h01m48s to 06h02m36s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=06h01m48s   !NEXT!        
qual=  0
disk=off
stop=06h02m36s   !NEXT!

!* --- Scan from 06h02m48s to 06h03m36s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=06h02m48s   !NEXT!        
qual=  0
disk=off
stop=06h03m36s   !NEXT!

!* --- Scan from 06h03m47s to 06h04m34s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=06h03m47s   !NEXT!        
qual=  0
disk=off
stop=06h04m34s   !NEXT!

!* --- Scan from 06h04m46s to 06h05m34s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=06h04m46s   !NEXT!        
qual=  0
disk=off
stop=06h05m34s   !NEXT!

!* --- Scan from 06h05m47s to 06h06m35s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=06h05m47s   !NEXT!        
qual=  0
disk=off
stop=06h06m35s   !NEXT!

!* --- Scan from 06h06m53s to 06h07m41s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=06h06m53s   !NEXT!        
qual=  0
disk=off
stop=06h07m41s   !NEXT!

!* --- Scan from 06h07m54s to 06h08m42s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=06h07m54s   !NEXT!        
qual=  0
disk=off
stop=06h08m42s   !NEXT!

!* --- Scan from 06h08m55s to 06h09m43s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=06h08m55s   !NEXT!        
qual=  0
disk=off
stop=06h09m43s   !NEXT!

!* --- Scan from 06h10m04s to 06h10m52s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=06h10m04s   !NEXT!        
qual=  0
disk=off
stop=06h10m52s   !NEXT!

!* --- Scan from 06h11m06s to 06h11m54s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=06h11m06s   !NEXT!        
qual=  0
disk=off
stop=06h11m54s   !NEXT!

!* --- Scan from 06h12m20s to 06h13m08s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=06h12m20s   !NEXT!        
qual=  0
disk=off
stop=06h13m08s   !NEXT!

!* --- Scan from 06h13m21s to 06h14m09s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=06h13m21s   !NEXT!        
qual=  0
disk=off
stop=06h14m09s   !NEXT!

!* --- Scan from 06h14m23s to 06h15m11s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=06h14m23s   !NEXT!        
qual=  0
disk=off
stop=06h15m11s   !NEXT!

!* --- Scan from 06h15m34s to 06h16m22s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=06h15m34s   !NEXT!        
qual=  0
disk=off
stop=06h16m22s   !NEXT!

!* --- Scan from 06h16m39s to 06h17m27s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=06h16m39s   !NEXT!        
qual=  0
disk=off
stop=06h17m27s   !NEXT!

!* --- Scan from 06h17m59s to 06h18m47s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=06h17m59s   !NEXT!        
qual=  0
disk=off
stop=06h18m47s   !NEXT!

!* --- Scan from 06h19m03s to 06h19m51s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=06h19m03s   !NEXT!        
qual=  0
disk=off
stop=06h19m51s   !NEXT!

!* --- Scan from 06h20m15s to 06h21m03s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=06h20m15s   !NEXT!        
qual=  0
disk=off
stop=06h21m03s   !NEXT!

!* --- Scan from 06h21m11s to 06h21m59s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=06h21m11s   !NEXT!        
qual=  0
disk=off
stop=06h21m59s   !NEXT!

!* --- Scan from 06h22m09s to 06h22m57s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=06h22m09s   !NEXT!        
qual=  0
disk=off
stop=06h22m57s   !NEXT!

!* --- Scan from 06h23m10s to 06h23m58s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=06h23m10s   !NEXT!        
qual=  0
disk=off
stop=06h23m58s   !NEXT!

!* --- Scan from 06h24m11s to 06h24m59s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=06h24m11s   !NEXT!        
qual=  0
disk=off
stop=06h24m59s   !NEXT!

!* --- Scan from 06h25m12s to 06h25m59s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=06h25m12s   !NEXT!        
qual=  0
disk=off
stop=06h25m59s   !NEXT!

!* --- Scan from 06h26m14s to 06h27m02s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=06h26m14s   !NEXT!        
qual=  0
disk=off
stop=06h27m02s   !NEXT!

!* --- Scan from 06h27m12s to 06h28m00s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=06h27m12s   !NEXT!        
qual=  0
disk=off
stop=06h28m00s   !NEXT!

!* --- Scan from 06h28m13s to 06h29m01s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=06h28m13s   !NEXT!        
qual=  0
disk=off
stop=06h29m01s   !NEXT!

!* --- Scan from 06h29m11s to 06h29m59s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=06h29m11s   !NEXT!        
qual=  0
disk=off
stop=06h29m59s   !NEXT!

!* --- Scan from 06h30m11s to 06h30m59s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=06h30m11s   !NEXT!        
qual=  0
disk=off
stop=06h30m59s   !NEXT!

!* --- Scan from 06h32m27s to 06h34m27s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=06h32m27s   !NEXT!        
qual=  0
disk=off
stop=06h34m27s   !NEXT!

!* --- Scan from 06h36m14s to 06h37m02s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=06h36m14s   !NEXT!        
qual=  0
disk=off
stop=06h37m02s   !NEXT!

!* --- Scan from 06h37m13s to 06h38m01s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=06h37m13s   !NEXT!        
qual=  0
disk=off
stop=06h38m01s   !NEXT!

!* --- Scan from 06h38m41s to 06h39m29s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=06h38m41s   !NEXT!        
qual=  0
disk=off
stop=06h39m29s   !NEXT!

!* --- Scan from 06h40m07s to 06h40m55s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=06h40m07s   !NEXT!        
qual=  0
disk=off
stop=06h40m55s   !NEXT!

!* --- Scan from 06h41m31s to 06h42m19s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=06h41m31s   !NEXT!        
qual=  0
disk=off
stop=06h42m19s   !NEXT!

!* --- Scan from 06h43m28s to 06h44m16s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=06h43m28s   !NEXT!        
qual=  0
disk=off
stop=06h44m16s   !NEXT!

!* --- Scan from 06h44m26s to 06h45m14s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=06h44m26s   !NEXT!        
qual=  0
disk=off
stop=06h45m14s   !NEXT!

!* --- Scan from 06h45m24s to 06h46m12s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=06h45m24s   !NEXT!        
qual=  0
disk=off
stop=06h46m12s   !NEXT!

!* --- Scan from 06h46m22s to 06h47m10s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=06h46m22s   !NEXT!        
qual=  0
disk=off
stop=06h47m10s   !NEXT!

!* --- Scan from 06h47m23s to 06h48m11s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=06h47m23s   !NEXT!        
qual=  0
disk=off
stop=06h48m11s   !NEXT!

!* --- Scan from 06h48m27s to 06h49m15s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=06h48m27s   !NEXT!        
qual=  0
disk=off
stop=06h49m15s   !NEXT!

!* --- Scan from 06h49m25s to 06h50m12s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=06h49m25s   !NEXT!        
qual=  0
disk=off
stop=06h50m12s   !NEXT!

!* --- Scan from 06h50m26s to 06h51m13s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=06h50m26s   !NEXT!        
qual=  0
disk=off
stop=06h51m13s   !NEXT!

!* --- Scan from 06h51m24s to 06h52m12s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=06h51m24s   !NEXT!        
qual=  0
disk=off
stop=06h52m12s   !NEXT!

!* --- Scan from 06h52m32s to 06h53m20s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=06h52m32s   !NEXT!        
qual=  0
disk=off
stop=06h53m20s   !NEXT!

!* --- Scan from 06h53m30s to 06h54m18s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=06h53m30s   !NEXT!        
qual=  0
disk=off
stop=06h54m18s   !NEXT!

!* --- Scan from 06h54m32s to 06h55m20s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=06h54m32s   !NEXT!        
qual=  0
disk=off
stop=06h55m20s   !NEXT!

!* --- Scan from 06h55m33s to 06h56m21s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=06h55m33s   !NEXT!        
qual=  0
disk=off
stop=06h56m21s   !NEXT!

!* --- Scan from 06h56m32s to 06h57m20s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=06h56m32s   !NEXT!        
qual=  0
disk=off
stop=06h57m20s   !NEXT!

!* --- Scan from 06h57m29s to 06h58m17s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=06h57m29s   !NEXT!        
qual=  0
disk=off
stop=06h58m17s   !NEXT!

!* --- Scan from 06h58m56s to 06h59m44s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=06h58m56s   !NEXT!        
qual=  0
disk=off
stop=06h59m44s   !NEXT!

!* --- Scan from 06h59m53s to 07h00m41s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=06h59m53s   !NEXT!        
qual=  0
disk=off
stop=07h00m41s   !NEXT!

!* --- Scan from 07h00m55s to 07h01m43s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=07h00m55s   !NEXT!        
qual=  0
disk=off
stop=07h01m43s   !NEXT!

!* --- Scan from 07h01m59s to 07h02m47s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=07h01m59s   !NEXT!        
qual=  0
disk=off
stop=07h02m47s   !NEXT!

!* --- Scan from 07h03m01s to 07h03m49s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=07h03m01s   !NEXT!        
qual=  0
disk=off
stop=07h03m49s   !NEXT!

!* --- Scan from 07h04m03s to 07h04m51s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=07h04m03s   !NEXT!        
qual=  0
disk=off
stop=07h04m51s   !NEXT!

!* --- Scan from 07h05m04s to 07h05m52s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=07h05m04s   !NEXT!        
qual=  0
disk=off
stop=07h05m52s   !NEXT!

!* --- Scan from 07h06m14s to 07h07m02s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=07h06m14s   !NEXT!        
qual=  0
disk=off
stop=07h07m02s   !NEXT!

!* --- Scan from 07h07m12s to 07h08m00s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=07h07m12s   !NEXT!        
qual=  0
disk=off
stop=07h08m00s   !NEXT!

!* --- Scan from 07h08m13s to 07h09m01s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=07h08m13s   !NEXT!        
qual=  0
disk=off
stop=07h09m01s   !NEXT!

!* --- Scan from 07h09m13s to 07h10m01s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=07h09m13s   !NEXT!        
qual=  0
disk=off
stop=07h10m01s   !NEXT!

!* --- Scan from 07h10m11s to 07h10m59s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=07h10m11s   !NEXT!        
qual=  0
disk=off
stop=07h10m59s   !NEXT!

!* --- Scan from 07h11m10s to 07h11m57s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=07h11m10s   !NEXT!        
qual=  0
disk=off
stop=07h11m57s   !NEXT!

!* --- Scan from 07h12m12s to 07h13m00s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=07h12m12s   !NEXT!        
qual=  0
disk=off
stop=07h13m00s   !NEXT!

!* --- Scan from 07h13m10s to 07h13m58s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=07h13m10s   !NEXT!        
qual=  0
disk=off
stop=07h13m58s   !NEXT!

!* --- Scan from 07h14m09s to 07h14m57s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=07h14m09s   !NEXT!        
qual=  0
disk=off
stop=07h14m57s   !NEXT!

!* --- Scan from 07h15m09s to 07h15m57s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=07h15m09s   !NEXT!        
qual=  0
disk=off
stop=07h15m57s   !NEXT!

!* --- Scan from 07h16m07s to 07h16m54s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=07h16m07s   !NEXT!        
qual=  0
disk=off
stop=07h16m54s   !NEXT!

!* --- Scan from 07h17m26s to 07h18m13s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=07h17m26s   !NEXT!        
qual=  0
disk=off
stop=07h18m13s   !NEXT!

!* --- Scan from 07h18m27s to 07h19m15s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=07h18m27s   !NEXT!        
qual=  0
disk=off
stop=07h19m15s   !NEXT!

!* --- Scan from 07h19m28s to 07h20m15s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=07h19m28s   !NEXT!        
qual=  0
disk=off
stop=07h20m15s   !NEXT!

!* --- Scan from 07h20m30s to 07h21m18s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=07h20m30s   !NEXT!        
qual=  0
disk=off
stop=07h21m18s   !NEXT!

!* --- Scan from 07h21m25s to 07h22m13s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=07h21m25s   !NEXT!        
qual=  0
disk=off
stop=07h22m13s   !NEXT!

!* --- Scan from 07h22m23s to 07h23m11s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=07h22m23s   !NEXT!        
qual=  0
disk=off
stop=07h23m11s   !NEXT!

!* --- Scan from 07h23m27s to 07h24m15s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=07h23m27s   !NEXT!        
qual=  0
disk=off
stop=07h24m15s   !NEXT!

!* --- Scan from 07h24m26s to 07h25m14s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=07h24m26s   !NEXT!        
qual=  0
disk=off
stop=07h25m14s   !NEXT!

!* --- Scan from 07h25m24s to 07h26m12s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=07h25m24s   !NEXT!        
qual=  0
disk=off
stop=07h26m12s   !NEXT!

!* --- Scan from 07h26m23s to 07h27m11s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=07h26m23s   !NEXT!        
qual=  0
disk=off
stop=07h27m11s   !NEXT!

!* --- Scan from 07h27m19s to 07h28m07s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=07h27m19s   !NEXT!        
qual=  0
disk=off
stop=07h28m07s   !NEXT!

!* --- Scan from 07h28m17s to 07h29m05s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=07h28m17s   !NEXT!        
qual=  0
disk=off
stop=07h29m05s   !NEXT!

!* --- Scan from 07h29m16s to 07h30m04s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=07h29m16s   !NEXT!        
qual=  0
disk=off
stop=07h30m04s   !NEXT!

!* --- Scan from 07h30m38s to 07h31m26s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=07h30m38s   !NEXT!        
qual=  0
disk=off
stop=07h31m26s   !NEXT!

!* --- Scan from 07h31m36s to 07h32m24s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=07h31m36s   !NEXT!        
qual=  0
disk=off
stop=07h32m24s   !NEXT!

!* --- Scan from 07h32m33s to 07h33m21s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=07h32m33s   !NEXT!        
qual=  0
disk=off
stop=07h33m21s   !NEXT!

!* --- Scan from 07h33m32s to 07h34m20s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=07h33m32s   !NEXT!        
qual=  0
disk=off
stop=07h34m20s   !NEXT!

!* --- Scan from 07h34m33s to 07h35m21s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=07h34m33s   !NEXT!        
qual=  0
disk=off
stop=07h35m21s   !NEXT!

!* --- Scan from 07h35m31s to 07h36m19s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=07h35m31s   !NEXT!        
qual=  0
disk=off
stop=07h36m19s   !NEXT!

!* --- Scan from 07h36m29s to 07h37m17s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=07h36m29s   !NEXT!        
qual=  0
disk=off
stop=07h37m17s   !NEXT!

!* --- Scan from 07h37m32s to 07h38m20s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=07h37m32s   !NEXT!        
qual=  0
disk=off
stop=07h38m20s   !NEXT!

!* --- Scan from 07h39m33s to 07h41m33s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=07h39m33s   !NEXT!        
qual=  0
disk=off
stop=07h41m33s   !NEXT!

!* --- Scan from 07h42m50s to 07h43m38s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=07h42m50s   !NEXT!        
qual=  0
disk=off
stop=07h43m38s   !NEXT!

!* --- Scan from 07h43m47s to 07h44m35s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=07h43m47s   !NEXT!        
qual=  0
disk=off
stop=07h44m35s   !NEXT!

!* --- Scan from 07h44m49s to 07h45m37s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=07h44m49s   !NEXT!        
qual=  0
disk=off
stop=07h45m37s   !NEXT!

!* --- Scan from 07h45m48s to 07h46m36s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=07h45m48s   !NEXT!        
qual=  0
disk=off
stop=07h46m36s   !NEXT!

!* --- Scan from 07h46m44s to 07h47m32s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=07h46m44s   !NEXT!        
qual=  0
disk=off
stop=07h47m32s   !NEXT!

!* --- Scan from 07h47m53s to 07h48m41s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=07h47m53s   !NEXT!        
qual=  0
disk=off
stop=07h48m41s   !NEXT!

!* --- Scan from 07h48m53s to 07h49m41s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=07h48m53s   !NEXT!        
qual=  0
disk=off
stop=07h49m41s   !NEXT!

!* --- Scan from 07h49m55s to 07h51m54s   Thu, 2006 Feb 16 --- *!
sname='J0854+2006'  ra=08h54m48.874924s  dec= 20d06'30.64088"  qual=999  calib='N'
disk=off
stop=07h49m55s   !NEXT!        
qual=  0
disk=off
stop=07h51m54s   !NEXT!

!* --- Scan from 07h52m07s to 07h52m55s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=07h52m07s   !NEXT!        
qual=  0
disk=off
stop=07h52m55s   !NEXT!

!* --- Scan from 07h53m15s to 07h54m03s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=07h53m15s   !NEXT!        
qual=  0
disk=off
stop=07h54m03s   !NEXT!

!* --- Scan from 07h54m17s to 07h55m05s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=07h54m17s   !NEXT!        
qual=  0
disk=off
stop=07h55m05s   !NEXT!

!* --- Scan from 07h55m22s to 07h56m09s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=07h55m22s   !NEXT!        
qual=  0
disk=off
stop=07h56m09s   !NEXT!

!* --- Scan from 07h56m23s to 07h57m11s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=07h56m23s   !NEXT!        
qual=  0
disk=off
stop=07h57m11s   !NEXT!

!* --- Scan from 07h57m32s to 07h58m20s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=07h57m32s   !NEXT!        
qual=  0
disk=off
stop=07h58m20s   !NEXT!

!* --- Scan from 07h58m30s to 07h59m18s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=07h58m30s   !NEXT!        
qual=  0
disk=off
stop=07h59m18s   !NEXT!

!* --- Scan from 07h59m35s to 08h00m23s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=07h59m35s   !NEXT!        
qual=  0
disk=off
stop=08h00m23s   !NEXT!

!* --- Scan from 08h00m36s to 08h01m24s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=08h00m36s   !NEXT!        
qual=  0
disk=off
stop=08h01m24s   !NEXT!

!* --- Scan from 08h01m35s to 08h02m23s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=08h01m35s   !NEXT!        
qual=  0
disk=off
stop=08h02m23s   !NEXT!

!* --- Scan from 08h02m34s to 08h03m22s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=08h02m34s   !NEXT!        
qual=  0
disk=off
stop=08h03m22s   !NEXT!

!* --- Scan from 08h03m35s to 08h04m22s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=08h03m35s   !NEXT!        
qual=  0
disk=off
stop=08h04m22s   !NEXT!

!* --- Scan from 08h04m33s to 08h05m21s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=08h04m33s   !NEXT!        
qual=  0
disk=off
stop=08h05m21s   !NEXT!

!* --- Scan from 08h05m33s to 08h06m21s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=08h05m33s   !NEXT!        
qual=  0
disk=off
stop=08h06m21s   !NEXT!

!* --- Scan from 08h06m32s to 08h07m20s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=08h06m32s   !NEXT!        
qual=  0
disk=off
stop=08h07m20s   !NEXT!

!* --- Scan from 08h07m32s to 08h08m20s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=08h07m32s   !NEXT!        
qual=  0
disk=off
stop=08h08m20s   !NEXT!

!* --- Scan from 08h08m34s to 08h09m22s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=08h08m34s   !NEXT!        
qual=  0
disk=off
stop=08h09m22s   !NEXT!

!* --- Scan from 08h09m39s to 08h10m27s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=08h09m39s   !NEXT!        
qual=  0
disk=off
stop=08h10m27s   !NEXT!

!* --- Scan from 08h10m39s to 08h11m27s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=08h10m39s   !NEXT!        
qual=  0
disk=off
stop=08h11m27s   !NEXT!

!* --- Scan from 08h11m40s to 08h12m27s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=08h11m40s   !NEXT!        
qual=  0
disk=off
stop=08h12m27s   !NEXT!

!* --- Scan from 08h12m41s to 08h13m29s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=08h12m41s   !NEXT!        
qual=  0
disk=off
stop=08h13m29s   !NEXT!

!* --- Scan from 08h13m41s to 08h14m28s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=08h13m41s   !NEXT!        
qual=  0
disk=off
stop=08h14m28s   !NEXT!

!* --- Scan from 08h14m40s to 08h15m28s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=08h14m40s   !NEXT!        
qual=  0
disk=off
stop=08h15m28s   !NEXT!

!* --- Scan from 08h17m51s to 08h19m50s   Thu, 2006 Feb 16 --- *!
sname='J1310+3220'  ra=13h10m28.663845s  dec= 32d20'43.78295"  qual=999  calib='N'
disk=off
stop=08h17m51s   !NEXT!        
qual=  0
disk=off
stop=08h19m50s   !NEXT!

!* --- Scan from 08h22m11s to 08h22m59s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=08h22m11s   !NEXT!        
qual=  0
disk=off
stop=08h22m59s   !NEXT!

!* --- Scan from 08h23m13s to 08h24m01s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=08h23m13s   !NEXT!        
qual=  0
disk=off
stop=08h24m01s   !NEXT!

!* --- Scan from 08h24m11s to 08h24m59s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=08h24m11s   !NEXT!        
qual=  0
disk=off
stop=08h24m59s   !NEXT!

!* --- Scan from 08h25m07s to 08h25m55s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=08h25m07s   !NEXT!        
qual=  0
disk=off
stop=08h25m55s   !NEXT!

!* --- Scan from 08h26m10s to 08h26m58s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=08h26m10s   !NEXT!        
qual=  0
disk=off
stop=08h26m58s   !NEXT!

!* --- Scan from 08h27m09s to 08h27m57s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=08h27m09s   !NEXT!        
qual=  0
disk=off
stop=08h27m57s   !NEXT!

!* --- Scan from 08h28m10s to 08h28m58s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=08h28m10s   !NEXT!        
qual=  0
disk=off
stop=08h28m58s   !NEXT!

!* --- Scan from 08h29m07s to 08h29m55s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=08h29m07s   !NEXT!        
qual=  0
disk=off
stop=08h29m55s   !NEXT!

!* --- Scan from 08h30m08s to 08h30m56s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=08h30m08s   !NEXT!        
qual=  0
disk=off
stop=08h30m56s   !NEXT!

!* --- Scan from 08h31m04s to 08h31m52s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=08h31m04s   !NEXT!        
qual=  0
disk=off
stop=08h31m52s   !NEXT!

!* --- Scan from 08h32m01s to 08h32m49s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=08h32m01s   !NEXT!        
qual=  0
disk=off
stop=08h32m49s   !NEXT!

!* --- Scan from 08h33m14s to 08h34m02s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=08h33m14s   !NEXT!        
qual=  0
disk=off
stop=08h34m02s   !NEXT!

!* --- Scan from 08h34m10s to 08h34m58s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=08h34m10s   !NEXT!        
qual=  0
disk=off
stop=08h34m58s   !NEXT!

!* --- Scan from 08h35m10s to 08h35m58s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=08h35m10s   !NEXT!        
qual=  0
disk=off
stop=08h35m58s   !NEXT!

!* --- Scan from 08h36m15s to 08h37m03s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=08h36m15s   !NEXT!        
qual=  0
disk=off
stop=08h37m03s   !NEXT!

!* --- Scan from 08h37m19s to 08h38m07s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=08h37m19s   !NEXT!        
qual=  0
disk=off
stop=08h38m07s   !NEXT!

!* --- Scan from 08h38m17s to 08h39m05s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=08h38m17s   !NEXT!        
qual=  0
disk=off
stop=08h39m05s   !NEXT!

!* --- Scan from 08h39m22s to 08h40m10s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=08h39m22s   !NEXT!        
qual=  0
disk=off
stop=08h40m10s   !NEXT!

!* --- Scan from 08h40m26s to 08h41m14s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=08h40m26s   !NEXT!        
qual=  0
disk=off
stop=08h41m14s   !NEXT!

!* --- Scan from 08h41m23s to 08h42m11s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=08h41m23s   !NEXT!        
qual=  0
disk=off
stop=08h42m11s   !NEXT!

!* --- Scan from 08h42m23s to 08h43m11s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=08h42m23s   !NEXT!        
qual=  0
disk=off
stop=08h43m11s   !NEXT!

!* --- Scan from 08h43m27s to 08h44m15s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=08h43m27s   !NEXT!        
qual=  0
disk=off
stop=08h44m15s   !NEXT!

!* --- Scan from 08h46m20s to 08h48m19s   Thu, 2006 Feb 16 --- *!
sname='DA193'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=08h46m20s   !NEXT!        
qual=  0
disk=off
stop=08h48m19s   !NEXT!

!* --- Scan from 08h50m26s to 08h52m26s   Thu, 2006 Feb 16 --- *!
sname='3C279'  ra=12h56m11.166560s  dec=-05d47'21.52458"  qual=999  calib='N'
disk=off
stop=08h50m26s   !NEXT!        
qual=  0
disk=off
stop=08h52m26s   !NEXT!

!* --- Scan from 08h54m54s to 08h55m42s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=08h54m54s   !NEXT!        
qual=  0
disk=off
stop=08h55m42s   !NEXT!

!* --- Scan from 08h55m55s to 08h56m42s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=08h55m55s   !NEXT!        
qual=  0
disk=off
stop=08h56m42s   !NEXT!

!* --- Scan from 08h56m52s to 08h57m40s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=08h56m52s   !NEXT!        
qual=  0
disk=off
stop=08h57m40s   !NEXT!

!* --- Scan from 08h57m50s to 08h58m38s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=08h57m50s   !NEXT!        
qual=  0
disk=off
stop=08h58m38s   !NEXT!

!* --- Scan from 08h58m54s to 08h59m42s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=08h58m54s   !NEXT!        
qual=  0
disk=off
stop=08h59m42s   !NEXT!

!* --- Scan from 09h02m45s to 09h03m33s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=09h02m45s   !NEXT!        
qual=  0
disk=off
stop=09h03m33s   !NEXT!

!* --- Scan from 09h03m43s to 09h04m31s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=09h03m43s   !NEXT!        
qual=  0
disk=off
stop=09h04m31s   !NEXT!

!* --- Scan from 09h04m45s to 09h05m33s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=09h04m45s   !NEXT!        
qual=  0
disk=off
stop=09h05m33s   !NEXT!

!* --- Scan from 09h05m53s to 09h06m41s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=09h05m53s   !NEXT!        
qual=  0
disk=off
stop=09h06m41s   !NEXT!

!* --- Scan from 09h06m56s to 09h07m44s   Thu, 2006 Feb 16 --- *!
sname='J08056+2106'  ra=08h05m38.530000s  dec= 21d06'51.51000"  qual=999  calib=' '
disk=off
stop=09h06m56s   !NEXT!        
qual=  0
disk=off
stop=09h07m44s   !NEXT!

!* --- Scan from 09h07m58s to 09h08m46s   Thu, 2006 Feb 16 --- *!
sname='J07538+2146'  ra=07h53m53.290000s  dec= 21d46'01.74000"  qual=999  calib=' '
disk=off
stop=09h07m58s   !NEXT!        
qual=  0
disk=off
stop=09h08m46s   !NEXT!

!* --- Scan from 09h09m00s to 09h09m47s   Thu, 2006 Feb 16 --- *!
sname='J07437+2328'  ra=07h43m44.970000s  dec= 23d28'39.00000"  qual=999  calib=' '
disk=off
stop=09h09m00s   !NEXT!        
qual=  0
disk=off
stop=09h09m47s   !NEXT!

!* --- Scan from 09h09m58s to 09h10m46s   Thu, 2006 Feb 16 --- *!
sname='J07486+2400'  ra=07h48m36.110000s  dec= 24d00'24.15000"  qual=999  calib=' '
disk=off
stop=09h09m58s   !NEXT!        
qual=  0
disk=off
stop=09h10m46s   !NEXT!

!* --- Scan from 09h10m59s to 09h11m47s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=09h10m59s   !NEXT!        
qual=  0
disk=off
stop=09h11m47s   !NEXT!

!* --- Scan from 09h11m57s to 09h12m45s   Thu, 2006 Feb 16 --- *!
sname='J07369+2604'  ra=07h36m58.070000s  dec= 26d04'49.89000"  qual=999  calib=' '
disk=off
stop=09h11m57s   !NEXT!        
qual=  0
disk=off
stop=09h12m45s   !NEXT!

!* --- Scan from 09h12m56s to 09h13m44s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=09h12m56s   !NEXT!        
qual=  0
disk=off
stop=09h13m44s   !NEXT!

!* --- Scan from 09h13m55s to 09h14m43s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=09h13m55s   !NEXT!        
qual=  0
disk=off
stop=09h14m43s   !NEXT!

!* --- Scan from 09h14m56s to 09h15m44s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=09h14m56s   !NEXT!        
qual=  0
disk=off
stop=09h15m44s   !NEXT!

!* --- Scan from 09h15m58s to 09h16m45s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=09h15m58s   !NEXT!        
qual=  0
disk=off
stop=09h16m45s   !NEXT!

!* --- Scan from 09h16m56s to 09h17m44s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=09h16m56s   !NEXT!        
qual=  0
disk=off
stop=09h17m44s   !NEXT!

!* --- Scan from 09h17m54s to 09h18m42s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=09h17m54s   !NEXT!        
qual=  0
disk=off
stop=09h18m42s   !NEXT!

!* --- Scan from 09h18m52s to 09h19m40s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=09h18m52s   !NEXT!        
qual=  0
disk=off
stop=09h19m40s   !NEXT!

!* --- Scan from 09h19m50s to 09h20m38s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=09h19m50s   !NEXT!        
qual=  0
disk=off
stop=09h20m38s   !NEXT!

!* --- Scan from 09h20m55s to 09h21m43s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=09h20m55s   !NEXT!        
qual=  0
disk=off
stop=09h21m43s   !NEXT!

!* --- Scan from 09h21m55s to 09h22m43s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=09h21m55s   !NEXT!        
qual=  0
disk=off
stop=09h22m43s   !NEXT!

!* --- Scan from 09h22m53s to 09h23m41s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=09h22m53s   !NEXT!        
qual=  0
disk=off
stop=09h23m41s   !NEXT!

!* --- Scan from 09h23m59s to 09h24m47s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=09h23m59s   !NEXT!        
qual=  0
disk=off
stop=09h24m47s   !NEXT!

!* --- Scan from 09h25m03s to 09h25m51s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=09h25m03s   !NEXT!        
qual=  0
disk=off
stop=09h25m51s   !NEXT!

!* --- Scan from 09h26m05s to 09h26m53s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=09h26m05s   !NEXT!        
qual=  0
disk=off
stop=09h26m53s   !NEXT!

!* --- Scan from 09h27m05s to 09h27m53s   Thu, 2006 Feb 16 --- *!
sname='J08171+2352'  ra=08h17m10.550000s  dec= 23d52'23.95000"  qual=999  calib=' '
disk=off
stop=09h27m05s   !NEXT!        
qual=  0
disk=off
stop=09h27m53s   !NEXT!

!* --- Scan from 09h28m03s to 09h28m51s   Thu, 2006 Feb 16 --- *!
sname='J08137+2435'  ra=08h13m47.140000s  dec= 24d35'59.21000"  qual=999  calib=' '
disk=off
stop=09h28m03s   !NEXT!        
qual=  0
disk=off
stop=09h28m51s   !NEXT!

!* --- Scan from 09h29m02s to 09h29m50s   Thu, 2006 Feb 16 --- *!
sname='J08130+2542'  ra=08h13m03.840000s  dec= 25d42'11.09000"  qual=999  calib=' '
disk=off
stop=09h29m02s   !NEXT!        
qual=  0
disk=off
stop=09h29m50s   !NEXT!

!* --- Scan from 09h30m01s to 09h30m49s   Thu, 2006 Feb 16 --- *!
sname='J08086+2646'  ra=08h08m36.770000s  dec= 26d46'36.78000"  qual=999  calib=' '
disk=off
stop=09h30m01s   !NEXT!        
qual=  0
disk=off
stop=09h30m49s   !NEXT!

!* --- Scan from 09h33m11s to 09h35m11s   Thu, 2006 Feb 16 --- *!
sname='J1310+3220'  ra=13h10m28.663845s  dec= 32d20'43.78295"  qual=999  calib='N'
disk=off
stop=09h33m11s   !NEXT!        
qual=  0
disk=off
stop=09h35m11s   !NEXT!

!* --- Scan from 09h37m30s to 09h38m18s   Thu, 2006 Feb 16 --- *!
sname='J07462+1807'  ra=07h46m16.670000s  dec= 18d07'19.66000"  qual=999  calib=' '
disk=off
stop=09h37m30s   !NEXT!        
qual=  0
disk=off
stop=09h38m18s   !NEXT!

!* --- Scan from 09h38m28s to 09h39m16s   Thu, 2006 Feb 16 --- *!
sname='J07500+1823'  ra=07h50m00.330000s  dec= 18d23'11.40000"  qual=999  calib=' '
disk=off
stop=09h38m28s   !NEXT!        
qual=  0
disk=off
stop=09h39m16s   !NEXT!

!* --- Scan from 09h39m30s to 09h40m18s   Thu, 2006 Feb 16 --- *!
sname='J08028+1809'  ra=08h02m48.030000s  dec= 18d09'49.27000"  qual=999  calib=' '
disk=off
stop=09h39m30s   !NEXT!        
qual=  0
disk=off
stop=09h40m18s   !NEXT!

!* --- Scan from 09h40m34s to 09h41m22s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=09h40m34s   !NEXT!        
qual=  0
disk=off
stop=09h41m22s   !NEXT!

!* --- Scan from 09h41m40s to 09h42m28s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=09h41m40s   !NEXT!        
qual=  0
disk=off
stop=09h42m28s   !NEXT!

!* --- Scan from 09h42m46s to 09h43m34s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=09h42m46s   !NEXT!        
qual=  0
disk=off
stop=09h43m34s   !NEXT!

!* --- Scan from 09h43m44s to 09h44m32s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=09h43m44s   !NEXT!        
qual=  0
disk=off
stop=09h44m32s   !NEXT!

!* --- Scan from 09h44m44s to 09h45m32s   Thu, 2006 Feb 16 --- *!
sname='J08324+1821'  ra=08h32m24.880000s  dec= 18d21'22.00000"  qual=999  calib=' '
disk=off
stop=09h44m44s   !NEXT!        
qual=  0
disk=off
stop=09h45m32s   !NEXT!

!* --- Scan from 09h45m40s to 09h46m28s   Thu, 2006 Feb 16 --- *!
sname='J08322+1832'  ra=08h32m16.040000s  dec= 18d32'12.12000"  qual=999  calib=' '
disk=off
stop=09h45m40s   !NEXT!        
qual=  0
disk=off
stop=09h46m28s   !NEXT!

!* --- Scan from 09h46m38s to 09h47m26s   Thu, 2006 Feb 16 --- *!
sname='J08290+1754'  ra=08h29m04.830000s  dec= 17d54'15.86000"  qual=999  calib=' '
disk=off
stop=09h46m38s   !NEXT!        
qual=  0
disk=off
stop=09h47m26s   !NEXT!

!* --- Scan from 09h47m43s to 09h48m31s   Thu, 2006 Feb 16 --- *!
sname='J08328+1554'  ra=08h32m49.400000s  dec= 15d54'08.61000"  qual=999  calib=' '
disk=off
stop=09h47m43s   !NEXT!        
qual=  0
disk=off
stop=09h48m31s   !NEXT!

!* --- Scan from 09h50m26s to 09h52m25s   Thu, 2006 Feb 16 --- *!
sname='3C279'  ra=12h56m11.166560s  dec=-05d47'21.52458"  qual=999  calib='N'
disk=off
stop=09h50m26s   !NEXT!        
qual=  0
disk=off
stop=09h52m25s   !NEXT!

!* --- Scan from 09h54m21s to 09h55m09s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=09h54m21s   !NEXT!        
qual=  0
disk=off
stop=09h55m09s   !NEXT!

!* --- Scan from 09h55m22s to 09h56m10s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=09h55m22s   !NEXT!        
qual=  0
disk=off
stop=09h56m10s   !NEXT!

!* --- Scan from 09h56m22s to 09h57m10s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=09h56m22s   !NEXT!        
qual=  0
disk=off
stop=09h57m10s   !NEXT!

!* --- Scan from 09h57m25s to 09h58m13s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=09h57m25s   !NEXT!        
qual=  0
disk=off
stop=09h58m13s   !NEXT!

!* --- Scan from 09h58m20s to 09h59m08s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=09h58m20s   !NEXT!        
qual=  0
disk=off
stop=09h59m08s   !NEXT!

!* --- Scan from 09h59m20s to 10h00m07s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=09h59m20s   !NEXT!        
qual=  0
disk=off
stop=10h00m07s   !NEXT!

!* --- Scan from 10h00m26s to 10h01m14s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=10h00m26s   !NEXT!        
qual=  0
disk=off
stop=10h01m14s   !NEXT!

!* --- Scan from 10h01m23s to 10h02m11s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=10h01m23s   !NEXT!        
qual=  0
disk=off
stop=10h02m11s   !NEXT!

!* --- Scan from 10h02m23s to 10h03m11s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=10h02m23s   !NEXT!        
qual=  0
disk=off
stop=10h03m11s   !NEXT!

!* --- Scan from 10h03m34s to 10h04m22s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=10h03m34s   !NEXT!        
qual=  0
disk=off
stop=10h04m22s   !NEXT!

!* --- Scan from 10h04m43s to 10h05m31s   Thu, 2006 Feb 16 --- *!
sname='J08193+2747'  ra=08h19m18.860000s  dec= 27d47'30.72000"  qual=999  calib=' '
disk=off
stop=10h04m43s   !NEXT!        
qual=  0
disk=off
stop=10h05m31s   !NEXT!

!* --- Scan from 10h05m42s to 10h06m29s   Thu, 2006 Feb 16 --- *!
sname='J08219+2857'  ra=08h21m54.070000s  dec= 28d57'39.57000"  qual=999  calib=' '
disk=off
stop=10h05m42s   !NEXT!        
qual=  0
disk=off
stop=10h06m29s   !NEXT!

!* --- Scan from 10h06m39s to 10h07m27s   Thu, 2006 Feb 16 --- *!
sname='J08236+2928'  ra=08h23m41.130000s  dec= 29d28'28.17000"  qual=999  calib=' '
disk=off
stop=10h06m39s   !NEXT!        
qual=  0
disk=off
stop=10h07m27s   !NEXT!

!* --- Scan from 10h07m46s to 10h08m34s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=10h07m46s   !NEXT!        
qual=  0
disk=off
stop=10h08m34s   !NEXT!

!* --- Scan from 10h08m43s to 10h09m31s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=10h08m43s   !NEXT!        
qual=  0
disk=off
stop=10h09m31s   !NEXT!

!* --- Scan from 10h09m43s to 10h10m31s   Thu, 2006 Feb 16 --- *!
sname='J09048+2729'  ra=09h04m53.760000s  dec= 27d29'53.87000"  qual=999  calib=' '
disk=off
stop=10h09m43s   !NEXT!        
qual=  0
disk=off
stop=10h10m31s   !NEXT!

!* --- Scan from 10h10m40s to 10h11m28s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=10h10m40s   !NEXT!        
qual=  0
disk=off
stop=10h11m28s   !NEXT!

!* --- Scan from 10h11m38s to 10h12m26s   Thu, 2006 Feb 16 --- *!
sname='J09056+2849'  ra=09h05m41.770000s  dec= 28d49'28.25000"  qual=999  calib=' '
disk=off
stop=10h11m38s   !NEXT!        
qual=  0
disk=off
stop=10h12m26s   !NEXT!

!* --- Scan from 10h13m12s to 10h14m00s   Thu, 2006 Feb 16 --- *!
sname='J07414+2557'  ra=07h41m29.740000s  dec= 25d57'32.27000"  qual=999  calib=' '
disk=off
stop=10h13m12s   !NEXT!        
qual=  0
disk=off
stop=10h14m00s   !NEXT!

!* --- Scan from 10h14m10s to 10h14m58s   Thu, 2006 Feb 16 --- *!
sname='J07414+2706'  ra=07h41m25.730000s  dec= 27d06'45.42000"  qual=999  calib=' '
disk=off
stop=10h14m10s   !NEXT!        
qual=  0
disk=off
stop=10h14m58s   !NEXT!

!* --- Scan from 10h15m08s to 10h15m56s   Thu, 2006 Feb 16 --- *!
sname='J07464+2549'  ra=07h46m25.870000s  dec= 25d49'02.15000"  qual=999  calib=' '
disk=off
stop=10h15m08s   !NEXT!        
qual=  0
disk=off
stop=10h15m56s   !NEXT!

!* --- Scan from 10h16m07s to 10h16m55s   Thu, 2006 Feb 16 --- *!
sname='J07466+2734'  ra=07h46m40.430000s  dec= 27d34'59.06000"  qual=999  calib=' '
disk=off
stop=10h16m07s   !NEXT!        
qual=  0
disk=off
stop=10h16m55s   !NEXT!

!* --- Scan from 10h17m07s to 10h17m55s   Thu, 2006 Feb 16 --- *!
sname='J07481+3006'  ra=07h48m09.470000s  dec= 30d06'30.54000"  qual=999  calib=' '
disk=off
stop=10h17m07s   !NEXT!        
qual=  0
disk=off
stop=10h17m55s   !NEXT!

!* --- Scan from 10h18m07s to 10h18m55s   Thu, 2006 Feb 16 --- *!
sname='J07405+2852'  ra=07h40m33.540000s  dec= 28d52'47.27000"  qual=999  calib=' '
disk=off
stop=10h18m07s   !NEXT!        
qual=  0
disk=off
stop=10h18m55s   !NEXT!

!* --- Scan from 10h19m05s to 10h19m53s   Thu, 2006 Feb 16 --- *!
sname='J07365+2840'  ra=07h36m31.200000s  dec= 28d40'36.84000"  qual=999  calib=' '
disk=off
stop=10h19m05s   !NEXT!        
qual=  0
disk=off
stop=10h19m53s   !NEXT!

!* --- Scan from 10h20m02s to 10h20m50s   Thu, 2006 Feb 16 --- *!
sname='J07362+2954'  ra=07h36m13.660000s  dec= 29d54'22.20000"  qual=999  calib=' '
disk=off
stop=10h20m02s   !NEXT!        
qual=  0
disk=off
stop=10h20m50s   !NEXT!

!* --- Scan from 10h21m02s to 10h21m50s   Thu, 2006 Feb 16 --- *!
sname='J07448+2920'  ra=07h44m51.370000s  dec= 29d20'06.05000"  qual=999  calib=' '
disk=off
stop=10h21m02s   !NEXT!        
qual=  0
disk=off
stop=10h21m50s   !NEXT!

!* --- Scan from 10h22m02s to 10h22m50s   Thu, 2006 Feb 16 --- *!
sname='J07516+2657'  ra=07h51m37.130000s  dec= 26d57'08.02000"  qual=999  calib=' '
disk=off
stop=10h22m02s   !NEXT!        
qual=  0
disk=off
stop=10h22m50s   !NEXT!

!* --- Scan from 10h23m01s to 10h23m49s   Thu, 2006 Feb 16 --- *!
sname='J07576+2528'  ra=07h57m38.080000s  dec= 25d28'12.75000"  qual=999  calib=' '
disk=off
stop=10h23m01s   !NEXT!        
qual=  0
disk=off
stop=10h23m49s   !NEXT!

!* --- Scan from 10h23m59s to 10h24m47s   Thu, 2006 Feb 16 --- *!
sname='J08026+2509'  ra=08h02m41.590000s  dec= 25d09'10.91000"  qual=999  calib=' '
disk=off
stop=10h23m59s   !NEXT!        
qual=  0
disk=off
stop=10h24m47s   !NEXT!

!* --- Scan from 10h25m05s to 10h25m53s   Thu, 2006 Feb 16 --- *!
sname='J08245+2438'  ra=08h24m33.010000s  dec= 24d38'43.12000"  qual=999  calib=' '
disk=off
stop=10h25m05s   !NEXT!        
qual=  0
disk=off
stop=10h25m53s   !NEXT!

!* --- Scan from 10h26m11s to 10h26m59s   Thu, 2006 Feb 16 --- *!
sname='J08391+2002'  ra=08h39m10.900000s  dec= 20d02'07.34000"  qual=999  calib=' '
disk=off
stop=10h26m11s   !NEXT!        
qual=  0
disk=off
stop=10h26m59s   !NEXT!

!* --- Scan from 10h27m18s to 10h28m05s   Thu, 2006 Feb 16 --- *!
sname='J08170+1958'  ra=08h17m05.490000s  dec= 19d58'42.93000"  qual=999  calib=' '
disk=off
stop=10h27m18s   !NEXT!        
qual=  0
disk=off
stop=10h28m05s   !NEXT!

!* --- Scan from 10h28m19s to 10h29m07s   Thu, 2006 Feb 16 --- *!
sname='J08234+2223'  ra=08h23m24.760000s  dec= 22d23'03.30000"  qual=999  calib=' '
disk=off
stop=10h28m19s   !NEXT!        
qual=  0
disk=off
stop=10h29m07s   !NEXT!

!* --- Scan from 10h29m22s to 10h30m10s   Thu, 2006 Feb 16 --- *!
sname='J08367+2355'  ra=08h36m46.310000s  dec= 23d55'31.65000"  qual=999  calib=' '
disk=off
stop=10h29m22s   !NEXT!        
qual=  0
disk=off
stop=10h30m10s   !NEXT!

!* --- Scan from 10h30m20s to 10h31m08s   Thu, 2006 Feb 16 --- *!
sname='J08376+2454'  ra=08h37m40.250000s  dec= 24d54'23.12000"  qual=999  calib=' '
disk=off
stop=10h30m20s   !NEXT!        
qual=  0
disk=off
stop=10h31m08s   !NEXT!

!* --- Scan from 10h31m15s to 10h32m03s   Thu, 2006 Feb 16 --- *!
sname='J08374+2501'  ra=08h37m25.880000s  dec= 25d01'39.85000"  qual=999  calib=' '
disk=off
stop=10h31m15s   !NEXT!        
qual=  0
disk=off
stop=10h32m03s   !NEXT!

!* --- Scan from 10h34m12s to 10h36m12s   Thu, 2006 Feb 16 --- *!
sname='J1310+3220'  ra=13h10m28.663845s  dec= 32d20'43.78295"  qual=999  calib='N'
disk=off
stop=10h34m12s   !NEXT!        
qual=  0
disk=off
stop=10h36m12s   !NEXT!

!* --- Scan from 10h39m35s to 10h41m34s   Thu, 2006 Feb 16 --- *!
sname='3C279'  ra=12h56m11.166560s  dec=-05d47'21.52458"  qual=999  calib='N'
disk=off
stop=10h39m35s   !NEXT!        
qual=  0
disk=off
stop=10h41m34s   !NEXT!

!* --- Scan from 10h43m27s to 10h44m15s   Thu, 2006 Feb 16 --- *!
sname='J08395+1802'  ra=08h39m30.720000s  dec= 18d02'47.14000"  qual=999  calib=' '
disk=off
stop=10h43m27s   !NEXT!        
qual=  0
disk=off
stop=10h44m15s   !NEXT!

!* --- Scan from 10h44m25s to 10h45m12s   Thu, 2006 Feb 16 --- *!
sname='J08420+1835'  ra=08h42m05.090000s  dec= 18d35'40.98000"  qual=999  calib=' '
disk=off
stop=10h44m25s   !NEXT!        
qual=  0
disk=off
stop=10h45m12s   !NEXT!

!* --- Scan from 10h45m27s to 10h46m15s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=10h45m27s   !NEXT!        
qual=  0
disk=off
stop=10h46m15s   !NEXT!

!* --- Scan from 10h46m31s to 10h47m19s   Thu, 2006 Feb 16 --- *!
sname='J08566+2057'  ra=08h56m39.740000s  dec= 20d57'43.43000"  qual=999  calib=' '
disk=off
stop=10h46m31s   !NEXT!        
qual=  0
disk=off
stop=10h47m19s   !NEXT!

!* --- Scan from 10h47m27s to 10h48m15s   Thu, 2006 Feb 16 --- *!
sname='J08569+2111'  ra=08h56m57.240000s  dec= 21d11'43.64000"  qual=999  calib=' '
disk=off
stop=10h47m27s   !NEXT!        
qual=  0
disk=off
stop=10h48m15s   !NEXT!

!* --- Scan from 10h48m27s to 10h49m14s   Thu, 2006 Feb 16 --- *!
sname='J09052+2052'  ra=09h05m14.340000s  dec= 20d52'13.29000"  qual=999  calib=' '
disk=off
stop=10h48m27s   !NEXT!        
qual=  0
disk=off
stop=10h49m14s   !NEXT!

!* --- Scan from 10h49m30s to 10h50m18s   Thu, 2006 Feb 16 --- *!
sname='J08569+1739'  ra=08h56m56.690000s  dec= 17d39'47.77000"  qual=999  calib=' '
disk=off
stop=10h49m30s   !NEXT!        
qual=  0
disk=off
stop=10h50m18s   !NEXT!

!* --- Scan from 10h50m36s to 10h51m23s   Thu, 2006 Feb 16 --- *!
sname='J08362+2139'  ra=08h36m16.220000s  dec= 21d39'03.60000"  qual=999  calib=' '
disk=off
stop=10h50m36s   !NEXT!        
qual=  0
disk=off
stop=10h51m23s   !NEXT!

!* --- Scan from 10h51m40s to 10h52m28s   Thu, 2006 Feb 16 --- *!
sname='J08257+2704'  ra=08h25m47.390000s  dec= 27d04'22.04000"  qual=999  calib=' '
disk=off
stop=10h51m40s   !NEXT!        
qual=  0
disk=off
stop=10h52m28s   !NEXT!

!* --- Scan from 10h52m40s to 10h53m28s   Thu, 2006 Feb 16 --- *!
sname='J08281+2920'  ra=08h28m09.380000s  dec= 29d20'19.57000"  qual=999  calib=' '
disk=off
stop=10h52m40s   !NEXT!        
qual=  0
disk=off
stop=10h53m28s   !NEXT!

!* --- Scan from 10h53m41s to 10h54m28s   Thu, 2006 Feb 16 --- *!
sname='J08363+2728'  ra=08h36m22.890000s  dec= 27d28'52.54000"  qual=999  calib=' '
disk=off
stop=10h53m41s   !NEXT!        
qual=  0
disk=off
stop=10h54m28s   !NEXT!

!* --- Scan from 10h54m39s to 10h55m27s   Thu, 2006 Feb 16 --- *!
sname='J08392+2850'  ra=08h39m15.830000s  dec= 28d50'38.76000"  qual=999  calib=' '
disk=off
stop=10h54m39s   !NEXT!        
qual=  0
disk=off
stop=10h55m27s   !NEXT!

!* --- Scan from 10h55m41s to 10h56m29s   Thu, 2006 Feb 16 --- *!
sname='J08520+2833'  ra=08h52m05.170000s  dec= 28d33'59.75000"  qual=999  calib=' '
disk=off
stop=10h55m41s   !NEXT!        
qual=  0
disk=off
stop=10h56m29s   !NEXT!

!* --- Scan from 10h56m37s to 10h57m25s   Thu, 2006 Feb 16 --- *!
sname='J08532+2813'  ra=08h53m17.830000s  dec= 28d13'50.02000"  qual=999  calib=' '
disk=off
stop=10h56m37s   !NEXT!        
qual=  0
disk=off
stop=10h57m25s   !NEXT!

!* --- Scan from 10h57m38s to 10h58m26s   Thu, 2006 Feb 16 --- *!
sname='J09050+2748'  ra=09h05m04.050000s  dec= 27d48'17.69000"  qual=999  calib=' '
disk=off
stop=10h57m38s   !NEXT!        
qual=  0
disk=off
stop=10h58m26s   !NEXT!
disk=off
stop=10h58m31s   !NEXT!
     !QUIT! 
