!*  Schedule for VLBA_NL   *!
!*  Experiment HSA1CM   *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:  11.50 *!
!* PI:       Craig Walker *!
!* Address:  NRAO/Socorro *!
!*  *!
!*  *!
!*  *!
!* Phone:    575-835-7247 *!
!* EMAIL:    cwalker@nrao.edu *!
!* Fax: *!
!* Phone during observation: 575-835-7247 *!
!* Observing mode: 512 Mb/s 1.3 cm *!
!* Notes:    EB and GB pointing every roughly 60 minutes (10m). *!
!*           During 2 blocks of atmospheric cal, no EB/GB pointing. *!
!*           This schedule assumes use of the EB cassegrain receiver. *!
!*  *!
!*  Start at 00h30m00s     Fri, 2008 May 23  Day of year  144   *!
program=HSA1CM  

diskformat=mark5c
media=(1,disk)

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 00h30m00s to 00h31m00s   Fri, 2008 May 23 --- *!
sname='1124-186'  ra=11h27m04.392448s  dec=-18d57'17.44169"  qual=999  calib=' '
maxcaltime=  20
fe=(2,1cm),(4,1cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1, 9.1),( 2,15.1),( 3,12.1)
logging=STANDARD
nchan= 4
format=VLBA1:4
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,D),(2,D),(3,D),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,961.25),( 2,857.25),( 3,779.25),( 4,558.25)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S2),(7,S3),(8,S4)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,M1),(6,M2),(7,M3),(8,M4)
pcalxfreq1=(1,750),(2,750),(3,13750),(4,13750),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,750),(2,750),(3,13750),(4,13750),(5,0),(6,0),(7,0),(8,0)
samplerate=32M
disk=off
  date = 2008May23
stop=00h30m00s   !NEXT!        
qual=  0
disk=off
stop=00h31m00s   !NEXT!

!* --- Scan from 00h35m32s to 00h36m32s   Fri, 2008 May 23 --- *!
sname='1302+574'  ra=13h02m52.465282s  dec= 57d48'37.60942"  qual=999  calib=' '
disk=off
stop=00h35m32s   !NEXT!        
qual=  0
disk=off
stop=00h36m32s   !NEXT!

!* --- Scan from 00h39m52s to 00h40m52s   Fri, 2008 May 23 --- *!
sname='1354-020'  ra=13h54m06.895319s  dec=-02d06'03.19051"  qual=999  calib=' '
disk=off
stop=00h39m52s   !NEXT!        
qual=  0
disk=off
stop=00h40m52s   !NEXT!

!* --- Scan from 00h43m57s to 00h44m57s   Fri, 2008 May 23 --- *!
sname='1419+542'  ra=14h19m46.597401s  dec= 54d23'14.78721"  qual=999  calib=' '
disk=off
stop=00h43m57s   !NEXT!        
qual=  0
disk=off
stop=00h44m57s   !NEXT!

!* --- Scan from 00h46m34s to 00h47m34s   Fri, 2008 May 23 --- *!
sname='1638+572'  ra=16h38m13.456293s  dec= 57d20'23.97918"  qual=999  calib=' '
disk=off
stop=00h46m34s   !NEXT!        
qual=  0
disk=off
stop=00h47m34s   !NEXT!

!* --- Scan from 00h49m43s to 00h50m43s   Fri, 2008 May 23 --- *!
sname='1800+782'  ra=18h00m45.683918s  dec= 78d28'04.01851"  qual=999  calib=' '
disk=off
stop=00h49m43s   !NEXT!        
qual=  0
disk=off
stop=00h50m43s   !NEXT!

!* --- Scan from 00h52m00s to 00h53m00s   Fri, 2008 May 23 --- *!
sname='0017+813'  ra=00h17m08.474953s  dec= 81d35'08.13633"  qual=999  calib=' '
disk=off
stop=00h52m00s   !NEXT!        
qual=  0
disk=off
stop=00h53m00s   !NEXT!

!* --- Scan from 00h54m39s to 00h55m39s   Fri, 2008 May 23 --- *!
sname='0102+582'  ra=01h02m45.762384s  dec= 58d24'11.13662"  qual=999  calib=' '
disk=off
stop=00h54m39s   !NEXT!        
qual=  0
disk=off
stop=00h55m39s   !NEXT!

!* --- Scan from 00h56m46s to 00h57m46s   Fri, 2008 May 23 --- *!
sname='0303+471'  ra=03h03m35.242226s  dec= 47d16'16.27546"  qual=999  calib=' '
disk=off
stop=00h56m46s   !NEXT!        
qual=  0
disk=off
stop=00h57m46s   !NEXT!

!* --- Scan from 01h00m31s to 01h01m31s   Fri, 2008 May 23 --- *!
sname='0753+535'  ra=07h53m01.384573s  dec= 53d52'59.63716"  qual=999  calib=' '
disk=off
stop=01h00m31s   !NEXT!        
qual=  0
disk=off
stop=01h01m31s   !NEXT!

!* --- Scan from 01h05m13s to 01h06m13s   Fri, 2008 May 23 --- *!
sname='0818+422'  ra=08h18m15.999602s  dec= 42d22'45.41494"  qual=999  calib=' '
disk=off
stop=01h05m13s   !NEXT!        
qual=  0
disk=off
stop=01h06m13s   !NEXT!

!* --- Scan from 01h08m22s to 01h09m22s   Fri, 2008 May 23 --- *!
sname='0958+653'  ra=09h58m47.245101s  dec= 65d33'54.81806"  qual=999  calib=' '
disk=off
stop=01h08m22s   !NEXT!        
qual=  0
disk=off
stop=01h09m22s   !NEXT!

!* --- Scan from 01h10m16s to 01h11m16s   Fri, 2008 May 23 --- *!
sname='1048+714'  ra=10h48m27.619917s  dec= 71d43'35.93828"  qual=999  calib=' '
disk=off
stop=01h10m16s   !NEXT!        
qual=  0
disk=off
stop=01h11m16s   !NEXT!

!* --- Scan from 01h14m47s to 01h15m47s   Fri, 2008 May 23 --- *!
sname='1104+381'  ra=11h04m27.313943s  dec= 38d12'31.79919"  qual=999  calib=' '
disk=off
stop=01h14m47s   !NEXT!        
qual=  0
disk=off
stop=01h15m47s   !NEXT!

!* --- Scan from 01h17m38s to 01h22m38s   Fri, 2008 May 23 --- *!
sname='4C39.25'  ra=09h27m03.013936s  dec= 39d02'20.85186"  qual=999  calib='V'
synth=( 2, 7.6),( 3,11.6)
ifchan=(2,B),(4,B)
bbsynth=( 1,943.50),( 2,959.75),( 3,959.75),( 4,975.25)
pcal=OFF
pcalxbit1=(2,S2),(3,S3),(4,S4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(1,M1),(2,M2),(3,M3),(4,M4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxfreq1=(1,0),(2,0),(3,0),(4,0)
pcalxfreq2=(1,0),(2,0),(3,0),(4,0)
disk=off
stop=01h17m38s   !NEXT!        
qual=  0
disk=off
stop=01h22m38s   !NEXT!

!* --- Scan from 01h22m38s to 01h23m08s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=01h23m08s   !NEXT!

!* --- Scan from 01h23m08s to 01h23m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h23m38s   !NEXT!

!* --- Scan from 01h23m38s to 01h24m08s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=01h24m08s   !NEXT!

!* --- Scan from 01h24m08s to 01h24m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h24m38s   !NEXT!

!* --- Scan from 01h24m38s to 01h25m08s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=01h25m08s   !NEXT!

!* --- Scan from 01h25m08s to 01h25m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h25m38s   !NEXT!

!* --- Scan from 01h25m38s to 01h26m08s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=01h26m08s   !NEXT!

!* --- Scan from 01h26m08s to 01h26m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h26m38s   !NEXT!

!* --- Scan from 01h26m38s to 01h27m08s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=01h27m08s   !NEXT!

!* --- Scan from 01h27m08s to 01h27m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h27m38s   !NEXT!

!* --- Scan from 01h27m38s to 01h28m08s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=01h28m08s   !NEXT!

!* --- Scan from 01h28m08s to 01h28m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h28m38s   !NEXT!

!* --- Scan from 01h28m38s to 01h29m08s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=01h29m08s   !NEXT!

!* --- Scan from 01h29m08s to 01h29m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h29m38s   !NEXT!

!* --- Scan from 01h29m38s to 01h30m08s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=01h30m08s   !NEXT!

!* --- Scan from 01h30m08s to 01h30m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h30m38s   !NEXT!

!* --- Scan from 01h30m38s to 01h31m08s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=01h31m08s   !NEXT!

!* --- Scan from 01h31m08s to 01h31m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h31m38s   !NEXT!

!* --- Scan from 01h31m38s to 01h32m08s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=01h32m08s   !NEXT!

!* --- Scan from 01h32m08s to 01h32m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h32m38s   !NEXT!

!* --- Scan from 01h32m38s to 01h34m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h34m18s   !NEXT!

!* --- Scan from 01h34m18s to 01h35m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h35m08s   !NEXT!

!* --- Scan from 01h35m08s to 01h35m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h35m58s   !NEXT!

!* --- Scan from 01h35m58s to 01h36m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h36m48s   !NEXT!

!* --- Scan from 01h36m48s to 01h37m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h37m38s   !NEXT!

!* --- Scan from 01h37m38s to 01h38m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h38m28s   !NEXT!

!* --- Scan from 01h38m28s to 01h39m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h39m18s   !NEXT!

!* --- Scan from 01h39m18s to 01h40m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h40m08s   !NEXT!

!* --- Scan from 01h40m08s to 01h40m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h40m58s   !NEXT!

!* --- Scan from 01h40m58s to 01h41m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h41m48s   !NEXT!

!* --- Scan from 01h41m48s to 01h42m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h42m38s   !NEXT!

!* --- Scan from 01h42m38s to 01h43m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h43m28s   !NEXT!

!* --- Scan from 01h43m28s to 01h44m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h44m18s   !NEXT!

!* --- Scan from 01h44m18s to 01h45m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h45m08s   !NEXT!

!* --- Scan from 01h45m08s to 01h45m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h45m58s   !NEXT!

!* --- Scan from 01h45m58s to 01h46m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h46m48s   !NEXT!

!* --- Scan from 01h46m48s to 01h47m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h47m38s   !NEXT!

!* --- Scan from 01h47m38s to 01h48m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h48m28s   !NEXT!

!* --- Scan from 01h48m28s to 01h49m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h49m18s   !NEXT!

!* --- Scan from 01h49m18s to 01h50m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h50m08s   !NEXT!

!* --- Scan from 01h50m08s to 01h50m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h50m58s   !NEXT!

!* --- Scan from 01h50m58s to 01h51m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h51m48s   !NEXT!

!* --- Scan from 01h51m48s to 01h52m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h52m38s   !NEXT!

!* --- Scan from 01h52m38s to 01h53m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h53m28s   !NEXT!

!* --- Scan from 01h53m28s to 01h54m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h54m18s   !NEXT!

!* --- Scan from 01h54m18s to 01h55m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h55m08s   !NEXT!

!* --- Scan from 01h55m08s to 01h55m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h55m58s   !NEXT!

!* --- Scan from 01h55m58s to 01h56m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h56m48s   !NEXT!

!* --- Scan from 01h56m48s to 01h57m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h57m38s   !NEXT!

!* --- Scan from 01h57m38s to 01h58m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=01h58m28s   !NEXT!

!* --- Scan from 01h58m28s to 01h59m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=01h59m18s   !NEXT!

!* --- Scan from 01h59m18s to 02h00m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h00m08s   !NEXT!

!* --- Scan from 02h00m08s to 02h00m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h00m58s   !NEXT!

!* --- Scan from 02h00m58s to 02h01m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h01m48s   !NEXT!

!* --- Scan from 02h01m48s to 02h02m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h02m38s   !NEXT!

!* --- Scan from 02h02m38s to 02h03m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h03m28s   !NEXT!

!* --- Scan from 02h03m28s to 02h04m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h04m18s   !NEXT!

!* --- Scan from 02h04m18s to 02h05m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h05m08s   !NEXT!

!* --- Scan from 02h05m08s to 02h05m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h05m58s   !NEXT!

!* --- Scan from 02h05m58s to 02h06m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h06m48s   !NEXT!

!* --- Scan from 02h06m48s to 02h07m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h07m38s   !NEXT!

!* --- Scan from 02h07m38s to 02h08m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h08m28s   !NEXT!

!* --- Scan from 02h08m28s to 02h09m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h09m18s   !NEXT!

!* --- Scan from 02h09m18s to 02h10m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h10m08s   !NEXT!

!* --- Scan from 02h10m08s to 02h10m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h10m58s   !NEXT!

!* --- Scan from 02h10m58s to 02h11m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h11m48s   !NEXT!

!* --- Scan from 02h11m48s to 02h12m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h12m38s   !NEXT!

!* --- Scan from 02h12m38s to 02h13m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h13m28s   !NEXT!

!* --- Scan from 02h13m28s to 02h14m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h14m18s   !NEXT!

!* --- Scan from 02h14m18s to 02h15m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h15m08s   !NEXT!

!* --- Scan from 02h15m08s to 02h15m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h15m58s   !NEXT!

!* --- Scan from 02h15m58s to 02h16m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h16m48s   !NEXT!

!* --- Scan from 02h16m48s to 02h17m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h17m38s   !NEXT!

!* --- Scan from 02h17m38s to 02h18m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h18m28s   !NEXT!

!* --- Scan from 02h18m28s to 02h19m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h19m18s   !NEXT!

!* --- Scan from 02h19m18s to 02h20m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h20m08s   !NEXT!

!* --- Scan from 02h20m08s to 02h20m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h20m58s   !NEXT!

!* --- Scan from 02h20m58s to 02h21m28s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=02h21m28s   !NEXT!

!* --- Scan from 02h21m28s to 02h21m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h21m58s   !NEXT!

!* --- Scan from 02h21m58s to 02h22m28s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=02h22m28s   !NEXT!

!* --- Scan from 02h22m28s to 02h22m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h22m58s   !NEXT!

!* --- Scan from 02h22m58s to 02h23m28s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=02h23m28s   !NEXT!

!* --- Scan from 02h23m28s to 02h23m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h23m58s   !NEXT!

!* --- Scan from 02h23m58s to 02h24m28s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=02h24m28s   !NEXT!

!* --- Scan from 02h24m28s to 02h24m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h24m58s   !NEXT!

!* --- Scan from 02h24m58s to 02h25m28s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=02h25m28s   !NEXT!

!* --- Scan from 02h25m28s to 02h25m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h25m58s   !NEXT!

!* --- Scan from 02h25m58s to 02h26m28s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=02h26m28s   !NEXT!

!* --- Scan from 02h26m28s to 02h26m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h26m58s   !NEXT!

!* --- Scan from 02h26m58s to 02h27m28s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=02h27m28s   !NEXT!

!* --- Scan from 02h27m28s to 02h27m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h27m58s   !NEXT!

!* --- Scan from 02h27m58s to 02h28m28s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=02h28m28s   !NEXT!

!* --- Scan from 02h28m28s to 02h28m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h28m58s   !NEXT!

!* --- Scan from 02h28m58s to 02h29m28s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=02h29m28s   !NEXT!

!* --- Scan from 02h29m28s to 02h29m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h29m58s   !NEXT!

!* --- Scan from 02h29m58s to 02h30m28s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=02h30m28s   !NEXT!

!* --- Scan from 02h30m28s to 02h30m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h30m58s   !NEXT!

!* --- Scan from 02h30m58s to 02h32m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h32m38s   !NEXT!

!* --- Scan from 02h32m38s to 02h33m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h33m28s   !NEXT!

!* --- Scan from 02h33m28s to 02h34m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h34m18s   !NEXT!

!* --- Scan from 02h34m18s to 02h35m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h35m08s   !NEXT!

!* --- Scan from 02h35m08s to 02h35m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h35m58s   !NEXT!

!* --- Scan from 02h35m58s to 02h36m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h36m48s   !NEXT!

!* --- Scan from 02h36m48s to 02h37m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h37m38s   !NEXT!

!* --- Scan from 02h37m38s to 02h38m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h38m28s   !NEXT!

!* --- Scan from 02h38m28s to 02h39m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h39m18s   !NEXT!

!* --- Scan from 02h39m18s to 02h40m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h40m08s   !NEXT!

!* --- Scan from 02h40m08s to 02h40m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h40m58s   !NEXT!

!* --- Scan from 02h40m58s to 02h41m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h41m48s   !NEXT!

!* --- Scan from 02h41m48s to 02h42m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h42m38s   !NEXT!

!* --- Scan from 02h42m38s to 02h43m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h43m28s   !NEXT!

!* --- Scan from 02h43m28s to 02h44m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h44m18s   !NEXT!

!* --- Scan from 02h44m18s to 02h45m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h45m08s   !NEXT!

!* --- Scan from 02h45m08s to 02h45m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h45m58s   !NEXT!

!* --- Scan from 02h45m58s to 02h46m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h46m48s   !NEXT!

!* --- Scan from 02h46m48s to 02h47m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h47m38s   !NEXT!

!* --- Scan from 02h47m38s to 02h48m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h48m28s   !NEXT!

!* --- Scan from 02h48m28s to 02h49m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h49m18s   !NEXT!

!* --- Scan from 02h49m18s to 02h50m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h50m08s   !NEXT!

!* --- Scan from 02h50m08s to 02h50m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h50m58s   !NEXT!

!* --- Scan from 02h50m58s to 02h51m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h51m48s   !NEXT!

!* --- Scan from 02h51m48s to 02h52m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h52m38s   !NEXT!

!* --- Scan from 02h52m38s to 02h53m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h53m28s   !NEXT!

!* --- Scan from 02h53m28s to 02h54m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h54m18s   !NEXT!

!* --- Scan from 02h54m18s to 02h55m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h55m08s   !NEXT!

!* --- Scan from 02h55m08s to 02h55m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h55m58s   !NEXT!

!* --- Scan from 02h55m58s to 02h56m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h56m48s   !NEXT!

!* --- Scan from 02h56m48s to 02h57m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h57m38s   !NEXT!

!* --- Scan from 02h57m38s to 02h58m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=02h58m28s   !NEXT!

!* --- Scan from 02h58m28s to 02h59m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=02h59m18s   !NEXT!

!* --- Scan from 02h59m18s to 03h00m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h00m08s   !NEXT!

!* --- Scan from 03h00m08s to 03h00m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h00m58s   !NEXT!

!* --- Scan from 03h00m58s to 03h01m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h01m48s   !NEXT!

!* --- Scan from 03h01m48s to 03h02m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h02m38s   !NEXT!

!* --- Scan from 03h02m38s to 03h03m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h03m28s   !NEXT!

!* --- Scan from 03h03m28s to 03h04m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h04m18s   !NEXT!

!* --- Scan from 03h04m18s to 03h05m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h05m08s   !NEXT!

!* --- Scan from 03h05m08s to 03h05m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h05m58s   !NEXT!

!* --- Scan from 03h05m58s to 03h06m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h06m48s   !NEXT!

!* --- Scan from 03h06m48s to 03h07m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h07m38s   !NEXT!

!* --- Scan from 03h07m38s to 03h08m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h08m28s   !NEXT!

!* --- Scan from 03h08m28s to 03h09m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h09m18s   !NEXT!

!* --- Scan from 03h09m18s to 03h10m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h10m08s   !NEXT!

!* --- Scan from 03h10m08s to 03h10m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h10m58s   !NEXT!

!* --- Scan from 03h10m58s to 03h11m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h11m48s   !NEXT!

!* --- Scan from 03h11m48s to 03h12m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h12m38s   !NEXT!

!* --- Scan from 03h12m38s to 03h13m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h13m28s   !NEXT!

!* --- Scan from 03h13m28s to 03h14m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h14m18s   !NEXT!

!* --- Scan from 03h14m18s to 03h15m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h15m08s   !NEXT!

!* --- Scan from 03h15m08s to 03h15m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h15m58s   !NEXT!

!* --- Scan from 03h15m58s to 03h16m48s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h16m48s   !NEXT!

!* --- Scan from 03h16m48s to 03h17m38s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h17m38s   !NEXT!

!* --- Scan from 03h17m38s to 03h18m28s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h18m28s   !NEXT!

!* --- Scan from 03h18m28s to 03h19m18s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h19m18s   !NEXT!

!* --- Scan from 03h19m18s to 03h20m08s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h20m08s   !NEXT!

!* --- Scan from 03h20m08s to 03h20m58s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h20m58s   !NEXT!

!* --- Scan from 03h22m07s to 03h24m07s   Fri, 2008 May 23 --- *!
sname='1739+522'  ra=17h40m36.977850s  dec= 52d11'43.40744"  qual=999  calib='V'
disk=off
stop=03h22m07s   !NEXT!        
qual=  0
disk=off
stop=03h24m07s   !NEXT!

!* --- Scan from 03h27m54s to 03h29m54s   Fri, 2008 May 23 --- *!
sname='4C39.25'  ra=09h27m03.013936s  dec= 39d02'20.85186"  qual=999  calib='V'
disk=off
stop=03h27m54s   !NEXT!        
qual=  0
disk=off
stop=03h29m54s   !NEXT!

!* --- Scan from 03h29m54s to 03h30m24s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=03h30m24s   !NEXT!

!* --- Scan from 03h30m24s to 03h30m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h30m54s   !NEXT!

!* --- Scan from 03h30m54s to 03h31m24s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=03h31m24s   !NEXT!

!* --- Scan from 03h31m24s to 03h31m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h31m54s   !NEXT!

!* --- Scan from 03h31m54s to 03h32m24s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=03h32m24s   !NEXT!

!* --- Scan from 03h32m24s to 03h32m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h32m54s   !NEXT!

!* --- Scan from 03h32m54s to 03h33m24s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=03h33m24s   !NEXT!

!* --- Scan from 03h33m24s to 03h33m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h33m54s   !NEXT!

!* --- Scan from 03h33m54s to 03h34m24s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=03h34m24s   !NEXT!

!* --- Scan from 03h34m24s to 03h34m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h34m54s   !NEXT!

!* --- Scan from 03h34m54s to 03h35m24s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=03h35m24s   !NEXT!

!* --- Scan from 03h35m24s to 03h35m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h35m54s   !NEXT!

!* --- Scan from 03h35m54s to 03h36m24s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=03h36m24s   !NEXT!

!* --- Scan from 03h36m24s to 03h36m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h36m54s   !NEXT!

!* --- Scan from 03h36m54s to 03h37m24s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=03h37m24s   !NEXT!

!* --- Scan from 03h37m24s to 03h37m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h37m54s   !NEXT!

!* --- Scan from 03h37m54s to 03h38m24s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=03h38m24s   !NEXT!

!* --- Scan from 03h38m24s to 03h38m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h38m54s   !NEXT!

!* --- Scan from 03h38m54s to 03h39m24s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=03h39m24s   !NEXT!

!* --- Scan from 03h39m24s to 03h39m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h39m54s   !NEXT!

!* --- Scan from 03h39m54s to 03h41m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h41m34s   !NEXT!

!* --- Scan from 03h41m34s to 03h42m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h42m24s   !NEXT!

!* --- Scan from 03h42m24s to 03h43m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h43m14s   !NEXT!

!* --- Scan from 03h43m14s to 03h44m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h44m04s   !NEXT!

!* --- Scan from 03h44m04s to 03h44m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h44m54s   !NEXT!

!* --- Scan from 03h44m54s to 03h45m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h45m44s   !NEXT!

!* --- Scan from 03h45m44s to 03h46m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h46m34s   !NEXT!

!* --- Scan from 03h46m34s to 03h47m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h47m24s   !NEXT!

!* --- Scan from 03h47m24s to 03h48m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h48m14s   !NEXT!

!* --- Scan from 03h48m14s to 03h49m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h49m04s   !NEXT!

!* --- Scan from 03h49m04s to 03h49m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h49m54s   !NEXT!

!* --- Scan from 03h49m54s to 03h50m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h50m44s   !NEXT!

!* --- Scan from 03h50m44s to 03h51m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h51m34s   !NEXT!

!* --- Scan from 03h51m34s to 03h52m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h52m24s   !NEXT!

!* --- Scan from 03h52m24s to 03h53m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h53m14s   !NEXT!

!* --- Scan from 03h53m14s to 03h54m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h54m04s   !NEXT!

!* --- Scan from 03h54m04s to 03h54m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h54m54s   !NEXT!

!* --- Scan from 03h54m54s to 03h55m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h55m44s   !NEXT!

!* --- Scan from 03h55m44s to 03h56m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h56m34s   !NEXT!

!* --- Scan from 03h56m34s to 03h57m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h57m24s   !NEXT!

!* --- Scan from 03h57m24s to 03h58m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h58m14s   !NEXT!

!* --- Scan from 03h58m14s to 03h59m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=03h59m04s   !NEXT!

!* --- Scan from 03h59m04s to 03h59m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=03h59m54s   !NEXT!

!* --- Scan from 03h59m54s to 04h00m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h00m44s   !NEXT!

!* --- Scan from 04h00m44s to 04h01m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h01m34s   !NEXT!

!* --- Scan from 04h01m34s to 04h02m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h02m24s   !NEXT!

!* --- Scan from 04h02m24s to 04h03m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h03m14s   !NEXT!

!* --- Scan from 04h03m14s to 04h04m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h04m04s   !NEXT!

!* --- Scan from 04h04m04s to 04h04m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h04m54s   !NEXT!

!* --- Scan from 04h04m54s to 04h05m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h05m44s   !NEXT!

!* --- Scan from 04h05m44s to 04h06m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h06m34s   !NEXT!

!* --- Scan from 04h06m34s to 04h07m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h07m24s   !NEXT!

!* --- Scan from 04h07m24s to 04h08m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h08m14s   !NEXT!

!* --- Scan from 04h08m14s to 04h09m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h09m04s   !NEXT!

!* --- Scan from 04h09m04s to 04h09m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h09m54s   !NEXT!

!* --- Scan from 04h09m54s to 04h10m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h10m44s   !NEXT!

!* --- Scan from 04h10m44s to 04h11m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h11m34s   !NEXT!

!* --- Scan from 04h11m34s to 04h12m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h12m24s   !NEXT!

!* --- Scan from 04h12m24s to 04h13m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h13m14s   !NEXT!

!* --- Scan from 04h13m14s to 04h14m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h14m04s   !NEXT!

!* --- Scan from 04h14m04s to 04h14m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h14m54s   !NEXT!

!* --- Scan from 04h14m54s to 04h15m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h15m44s   !NEXT!

!* --- Scan from 04h15m44s to 04h16m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h16m34s   !NEXT!

!* --- Scan from 04h16m34s to 04h17m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h17m24s   !NEXT!

!* --- Scan from 04h17m24s to 04h18m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h18m14s   !NEXT!

!* --- Scan from 04h18m14s to 04h19m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h19m04s   !NEXT!

!* --- Scan from 04h19m04s to 04h19m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h19m54s   !NEXT!

!* --- Scan from 04h19m54s to 04h20m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h20m44s   !NEXT!

!* --- Scan from 04h20m44s to 04h21m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h21m34s   !NEXT!

!* --- Scan from 04h21m34s to 04h22m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h22m24s   !NEXT!

!* --- Scan from 04h22m24s to 04h23m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h23m14s   !NEXT!

!* --- Scan from 04h23m14s to 04h24m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h24m04s   !NEXT!

!* --- Scan from 04h24m04s to 04h24m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h24m54s   !NEXT!

!* --- Scan from 04h24m54s to 04h25m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h25m44s   !NEXT!

!* --- Scan from 04h25m44s to 04h26m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h26m34s   !NEXT!

!* --- Scan from 04h26m34s to 04h27m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h27m24s   !NEXT!

!* --- Scan from 04h27m24s to 04h28m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h28m14s   !NEXT!

!* --- Scan from 04h28m14s to 04h28m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=04h28m44s   !NEXT!

!* --- Scan from 04h28m44s to 04h29m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h29m14s   !NEXT!

!* --- Scan from 04h29m14s to 04h29m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=04h29m44s   !NEXT!

!* --- Scan from 04h29m44s to 04h30m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h30m14s   !NEXT!

!* --- Scan from 04h30m14s to 04h30m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=04h30m44s   !NEXT!

!* --- Scan from 04h30m44s to 04h31m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h31m14s   !NEXT!

!* --- Scan from 04h31m14s to 04h31m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=04h31m44s   !NEXT!

!* --- Scan from 04h31m44s to 04h32m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h32m14s   !NEXT!

!* --- Scan from 04h32m14s to 04h32m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=04h32m44s   !NEXT!

!* --- Scan from 04h32m44s to 04h33m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h33m14s   !NEXT!

!* --- Scan from 04h33m14s to 04h33m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=04h33m44s   !NEXT!

!* --- Scan from 04h33m44s to 04h34m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h34m14s   !NEXT!

!* --- Scan from 04h34m14s to 04h34m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=04h34m44s   !NEXT!

!* --- Scan from 04h34m44s to 04h35m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h35m14s   !NEXT!

!* --- Scan from 04h35m14s to 04h35m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=04h35m44s   !NEXT!

!* --- Scan from 04h35m44s to 04h36m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h36m14s   !NEXT!

!* --- Scan from 04h36m14s to 04h36m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=04h36m44s   !NEXT!

!* --- Scan from 04h36m44s to 04h37m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h37m14s   !NEXT!

!* --- Scan from 04h37m14s to 04h37m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=04h37m44s   !NEXT!

!* --- Scan from 04h37m44s to 04h38m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h38m14s   !NEXT!

!* --- Scan from 04h38m14s to 04h39m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h39m54s   !NEXT!

!* --- Scan from 04h39m54s to 04h40m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h40m44s   !NEXT!

!* --- Scan from 04h40m44s to 04h41m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h41m34s   !NEXT!

!* --- Scan from 04h41m34s to 04h42m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h42m24s   !NEXT!

!* --- Scan from 04h42m24s to 04h43m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h43m14s   !NEXT!

!* --- Scan from 04h43m14s to 04h44m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h44m04s   !NEXT!

!* --- Scan from 04h44m04s to 04h44m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h44m54s   !NEXT!

!* --- Scan from 04h44m54s to 04h45m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h45m44s   !NEXT!

!* --- Scan from 04h45m44s to 04h46m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h46m34s   !NEXT!

!* --- Scan from 04h46m34s to 04h47m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h47m24s   !NEXT!

!* --- Scan from 04h47m24s to 04h48m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h48m14s   !NEXT!

!* --- Scan from 04h48m14s to 04h49m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h49m04s   !NEXT!

!* --- Scan from 04h49m04s to 04h49m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h49m54s   !NEXT!

!* --- Scan from 04h49m54s to 04h50m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h50m44s   !NEXT!

!* --- Scan from 04h50m44s to 04h51m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h51m34s   !NEXT!

!* --- Scan from 04h51m34s to 04h52m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h52m24s   !NEXT!

!* --- Scan from 04h52m24s to 04h53m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h53m14s   !NEXT!

!* --- Scan from 04h53m14s to 04h54m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h54m04s   !NEXT!

!* --- Scan from 04h54m04s to 04h54m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h54m54s   !NEXT!

!* --- Scan from 04h54m54s to 04h55m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h55m44s   !NEXT!

!* --- Scan from 04h55m44s to 04h56m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h56m34s   !NEXT!

!* --- Scan from 04h56m34s to 04h57m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h57m24s   !NEXT!

!* --- Scan from 04h57m24s to 04h58m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h58m14s   !NEXT!

!* --- Scan from 04h58m14s to 04h59m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=04h59m04s   !NEXT!

!* --- Scan from 04h59m04s to 04h59m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=04h59m54s   !NEXT!

!* --- Scan from 04h59m54s to 05h00m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h00m44s   !NEXT!

!* --- Scan from 05h00m44s to 05h01m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h01m34s   !NEXT!

!* --- Scan from 05h01m34s to 05h02m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h02m24s   !NEXT!

!* --- Scan from 05h02m24s to 05h03m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h03m14s   !NEXT!

!* --- Scan from 05h03m14s to 05h04m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h04m04s   !NEXT!

!* --- Scan from 05h04m04s to 05h04m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h04m54s   !NEXT!

!* --- Scan from 05h04m54s to 05h05m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h05m44s   !NEXT!

!* --- Scan from 05h05m44s to 05h06m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h06m34s   !NEXT!

!* --- Scan from 05h06m34s to 05h07m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h07m24s   !NEXT!

!* --- Scan from 05h07m24s to 05h08m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h08m14s   !NEXT!

!* --- Scan from 05h08m14s to 05h09m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h09m04s   !NEXT!

!* --- Scan from 05h09m04s to 05h09m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h09m54s   !NEXT!

!* --- Scan from 05h09m54s to 05h10m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h10m44s   !NEXT!

!* --- Scan from 05h10m44s to 05h11m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h11m34s   !NEXT!

!* --- Scan from 05h11m34s to 05h12m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h12m24s   !NEXT!

!* --- Scan from 05h12m24s to 05h13m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h13m14s   !NEXT!

!* --- Scan from 05h13m14s to 05h14m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h14m04s   !NEXT!

!* --- Scan from 05h14m04s to 05h14m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h14m54s   !NEXT!

!* --- Scan from 05h14m54s to 05h15m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h15m44s   !NEXT!

!* --- Scan from 05h15m44s to 05h16m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h16m34s   !NEXT!

!* --- Scan from 05h16m34s to 05h17m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h17m24s   !NEXT!

!* --- Scan from 05h17m24s to 05h18m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h18m14s   !NEXT!

!* --- Scan from 05h18m14s to 05h19m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h19m04s   !NEXT!

!* --- Scan from 05h19m04s to 05h19m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h19m54s   !NEXT!

!* --- Scan from 05h19m54s to 05h20m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h20m44s   !NEXT!

!* --- Scan from 05h20m44s to 05h21m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h21m34s   !NEXT!

!* --- Scan from 05h21m34s to 05h22m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h22m24s   !NEXT!

!* --- Scan from 05h22m24s to 05h23m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h23m14s   !NEXT!

!* --- Scan from 05h23m14s to 05h24m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h24m04s   !NEXT!

!* --- Scan from 05h24m04s to 05h24m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h24m54s   !NEXT!

!* --- Scan from 05h24m54s to 05h25m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h25m44s   !NEXT!

!* --- Scan from 05h25m44s to 05h26m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h26m34s   !NEXT!

!* --- Scan from 05h26m34s to 05h27m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h27m24s   !NEXT!

!* --- Scan from 05h27m24s to 05h28m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h28m14s   !NEXT!

!* --- Scan from 05h29m20s to 05h31m20s   Fri, 2008 May 23 --- *!
sname='1739+522'  ra=17h40m36.977850s  dec= 52d11'43.40744"  qual=999  calib='V'
disk=off
stop=05h29m20s   !NEXT!        
qual=  0
disk=off
stop=05h31m20s   !NEXT!

!* --- Scan from 05h34m50s to 05h36m50s   Fri, 2008 May 23 --- *!
sname='4C39.25'  ra=09h27m03.013936s  dec= 39d02'20.85186"  qual=999  calib='V'
disk=off
stop=05h34m50s   !NEXT!        
qual=  0
disk=off
stop=05h36m50s   !NEXT!

!* --- Scan from 05h38m20s to 05h38m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=999  calib='T'
disk=off
stop=05h38m20s   !NEXT!        
qual=  0
disk=off
stop=05h38m50s   !NEXT!

!* --- Scan from 05h38m50s to 05h39m20s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=05h39m20s   !NEXT!

!* --- Scan from 05h39m20s to 05h39m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h39m50s   !NEXT!

!* --- Scan from 05h39m50s to 05h40m20s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=05h40m20s   !NEXT!

!* --- Scan from 05h40m20s to 05h40m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h40m50s   !NEXT!

!* --- Scan from 05h40m50s to 05h41m20s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=05h41m20s   !NEXT!

!* --- Scan from 05h41m20s to 05h41m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h41m50s   !NEXT!

!* --- Scan from 05h41m50s to 05h42m20s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=05h42m20s   !NEXT!

!* --- Scan from 05h42m20s to 05h42m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h42m50s   !NEXT!

!* --- Scan from 05h42m50s to 05h43m20s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=05h43m20s   !NEXT!

!* --- Scan from 05h43m20s to 05h43m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h43m50s   !NEXT!

!* --- Scan from 05h43m50s to 05h44m20s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=05h44m20s   !NEXT!

!* --- Scan from 05h44m20s to 05h44m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h44m50s   !NEXT!

!* --- Scan from 05h44m50s to 05h45m20s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=05h45m20s   !NEXT!

!* --- Scan from 05h45m20s to 05h45m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h45m50s   !NEXT!

!* --- Scan from 05h45m50s to 05h46m20s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=05h46m20s   !NEXT!

!* --- Scan from 05h46m20s to 05h46m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h46m50s   !NEXT!

!* --- Scan from 05h46m50s to 05h48m30s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h48m30s   !NEXT!

!* --- Scan from 05h48m30s to 05h49m20s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h49m20s   !NEXT!

!* --- Scan from 05h49m20s to 05h50m10s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h50m10s   !NEXT!

!* --- Scan from 05h50m10s to 05h51m00s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h51m00s   !NEXT!

!* --- Scan from 05h51m00s to 05h51m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h51m50s   !NEXT!

!* --- Scan from 05h51m50s to 05h52m40s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h52m40s   !NEXT!

!* --- Scan from 05h52m40s to 05h53m30s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h53m30s   !NEXT!

!* --- Scan from 05h53m30s to 05h54m20s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h54m20s   !NEXT!

!* --- Scan from 05h54m20s to 05h55m10s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h55m10s   !NEXT!

!* --- Scan from 05h55m10s to 05h56m00s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h56m00s   !NEXT!

!* --- Scan from 05h56m00s to 05h56m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h56m50s   !NEXT!

!* --- Scan from 05h56m50s to 05h57m40s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h57m40s   !NEXT!

!* --- Scan from 05h57m40s to 05h58m30s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=05h58m30s   !NEXT!

!* --- Scan from 05h58m30s to 05h59m20s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=05h59m20s   !NEXT!

!* --- Scan from 05h59m20s to 06h00m10s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h00m10s   !NEXT!

!* --- Scan from 06h00m10s to 06h01m00s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h01m00s   !NEXT!

!* --- Scan from 06h01m00s to 06h01m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h01m50s   !NEXT!

!* --- Scan from 06h01m50s to 06h02m40s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h02m40s   !NEXT!

!* --- Scan from 06h02m40s to 06h03m30s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h03m30s   !NEXT!

!* --- Scan from 06h03m30s to 06h04m20s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h04m20s   !NEXT!

!* --- Scan from 06h04m20s to 06h05m10s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h05m10s   !NEXT!

!* --- Scan from 06h05m10s to 06h06m00s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h06m00s   !NEXT!

!* --- Scan from 06h06m00s to 06h06m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h06m50s   !NEXT!

!* --- Scan from 06h06m50s to 06h07m40s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h07m40s   !NEXT!

!* --- Scan from 06h07m40s to 06h08m30s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h08m30s   !NEXT!

!* --- Scan from 06h08m30s to 06h09m20s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h09m20s   !NEXT!

!* --- Scan from 06h09m20s to 06h10m10s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h10m10s   !NEXT!

!* --- Scan from 06h10m10s to 06h11m00s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h11m00s   !NEXT!

!* --- Scan from 06h11m00s to 06h11m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h11m50s   !NEXT!

!* --- Scan from 06h11m50s to 06h12m40s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h12m40s   !NEXT!

!* --- Scan from 06h12m40s to 06h13m30s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h13m30s   !NEXT!

!* --- Scan from 06h13m30s to 06h14m20s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h14m20s   !NEXT!

!* --- Scan from 06h14m20s to 06h15m10s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h15m10s   !NEXT!

!* --- Scan from 06h15m10s to 06h16m00s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h16m00s   !NEXT!

!* --- Scan from 06h16m00s to 06h16m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h16m50s   !NEXT!

!* --- Scan from 06h16m50s to 06h17m40s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h17m40s   !NEXT!

!* --- Scan from 06h17m40s to 06h18m30s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h18m30s   !NEXT!

!* --- Scan from 06h18m30s to 06h19m20s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h19m20s   !NEXT!

!* --- Scan from 06h19m20s to 06h20m10s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h20m10s   !NEXT!

!* --- Scan from 06h20m10s to 06h21m00s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h21m00s   !NEXT!

!* --- Scan from 06h21m00s to 06h21m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h21m50s   !NEXT!

!* --- Scan from 06h21m50s to 06h22m40s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h22m40s   !NEXT!

!* --- Scan from 06h22m40s to 06h23m30s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h23m30s   !NEXT!

!* --- Scan from 06h23m30s to 06h24m20s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h24m20s   !NEXT!

!* --- Scan from 06h24m20s to 06h25m10s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h25m10s   !NEXT!

!* --- Scan from 06h25m10s to 06h26m00s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h26m00s   !NEXT!

!* --- Scan from 06h26m00s to 06h26m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h26m50s   !NEXT!

!* --- Scan from 06h26m50s to 06h27m40s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h27m40s   !NEXT!

!* --- Scan from 06h27m40s to 06h28m30s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h28m30s   !NEXT!

!* --- Scan from 06h28m30s to 06h29m20s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h29m20s   !NEXT!

!* --- Scan from 06h29m20s to 06h30m10s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h30m10s   !NEXT!

!* --- Scan from 06h30m10s to 06h31m00s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h31m00s   !NEXT!

!* --- Scan from 06h31m00s to 06h31m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h31m50s   !NEXT!

!* --- Scan from 06h31m50s to 06h32m40s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h32m40s   !NEXT!

!* --- Scan from 06h32m40s to 06h33m30s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h33m30s   !NEXT!

!* --- Scan from 06h33m30s to 06h34m20s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h34m20s   !NEXT!

!* --- Scan from 06h34m20s to 06h35m10s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h35m10s   !NEXT!

!* --- Scan from 06h35m10s to 06h36m00s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=06h36m00s   !NEXT!

!* --- Scan from 06h36m00s to 06h36m50s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=06h36m50s   !NEXT!

!* --- Scan from 06h42m03s to 06h43m03s   Fri, 2008 May 23 --- *!
sname='1936-155'  ra=19h39m26.657740s  dec=-15d25'43.05832"  qual=999  calib=' '
synth=( 2,15.1),( 3,12.1)
ifchan=(2,D),(4,D)
bbsynth=( 1,961.25),( 2,857.25),( 3,779.25),( 4,558.25)
pcal=1MHZ
pcalxbit1=(2,S3),(3,S1),(4,S3),(5,S1),(6,S2),(7,S3),(8,S4)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,M1),(6,M2),(7,M3),(8,M4)
pcalxfreq1=(1,750),(2,750),(3,13750),(4,13750)
pcalxfreq2=(1,750),(2,750),(3,13750),(4,13750)
disk=off
stop=06h42m03s   !NEXT!        
qual=  0
disk=off
stop=06h43m03s   !NEXT!

!* --- Scan from 06h46m44s to 06h47m44s   Fri, 2008 May 23 --- *!
sname='2113+293'  ra=21h15m29.413455s  dec= 29d33'38.36694"  qual=999  calib=' '
disk=off
stop=06h46m44s   !NEXT!        
qual=  0
disk=off
stop=06h47m44s   !NEXT!

!* --- Scan from 06h52m08s to 06h53m08s   Fri, 2008 May 23 --- *!
sname='1550+052'  ra=15h50m35.269240s  dec= 05d27'10.44823"  qual=999  calib=' '
disk=off
stop=06h52m08s   !NEXT!        
qual=  0
disk=off
stop=06h53m08s   !NEXT!

!* --- Scan from 06h56m48s to 06h57m48s   Fri, 2008 May 23 --- *!
sname='1638+572'  ra=16h38m13.456293s  dec= 57d20'23.97918"  qual=999  calib=' '
disk=off
stop=06h56m48s   !NEXT!        
qual=  0
disk=off
stop=06h57m48s   !NEXT!

!* --- Scan from 06h59m01s to 07h00m01s   Fri, 2008 May 23 --- *!
sname='1727+453'  ra=17h27m27.650808s  dec= 45d30'39.73139"  qual=999  calib=' '
disk=off
stop=06h59m01s   !NEXT!        
qual=  0
disk=off
stop=07h00m01s   !NEXT!

!* --- Scan from 07h01m21s to 07h02m21s   Fri, 2008 May 23 --- *!
sname='1746+622'  ra=17h46m14.034146s  dec= 62d26'54.73842"  qual=999  calib=' '
disk=off
stop=07h01m21s   !NEXT!        
qual=  0
disk=off
stop=07h02m21s   !NEXT!

!* --- Scan from 07h04m46s to 07h05m46s   Fri, 2008 May 23 --- *!
sname='0016+731'  ra=00h19m45.786427s  dec= 73d27'30.01745"  qual=999  calib=' '
disk=off
stop=07h04m46s   !NEXT!        
qual=  0
disk=off
stop=07h05m46s   !NEXT!

!* --- Scan from 07h07m54s to 07h08m54s   Fri, 2008 May 23 --- *!
sname='0059+581'  ra=01h02m45.762384s  dec= 58d24'11.13662"  qual=999  calib=' '
disk=off
stop=07h07m54s   !NEXT!        
qual=  0
disk=off
stop=07h08m54s   !NEXT!

!* --- Scan from 07h11m35s to 07h12m35s   Fri, 2008 May 23 --- *!
sname='0636+680'  ra=06h42m04.257418s  dec= 67d58'35.62085"  qual=999  calib=' '
disk=off
stop=07h11m35s   !NEXT!        
qual=  0
disk=off
stop=07h12m35s   !NEXT!

!* --- Scan from 07h13m19s to 07h14m19s   Fri, 2008 May 23 --- *!
sname='0716+714'  ra=07h21m53.448459s  dec= 71d20'36.36339"  qual=999  calib=' '
disk=off
stop=07h13m19s   !NEXT!        
qual=  0
disk=off
stop=07h14m19s   !NEXT!

!* --- Scan from 07h15m11s to 07h16m11s   Fri, 2008 May 23 --- *!
sname='0718+792'  ra=07h26m11.735177s  dec= 79d11'31.01624"  qual=999  calib=' '
disk=off
stop=07h15m11s   !NEXT!        
qual=  0
disk=off
stop=07h16m11s   !NEXT!

!* --- Scan from 07h17m54s to 07h18m54s   Fri, 2008 May 23 --- *!
sname='0820+560'  ra=08h24m47.236351s  dec= 55d52'42.66938"  qual=999  calib=' '
disk=off
stop=07h17m54s   !NEXT!        
qual=  0
disk=off
stop=07h18m54s   !NEXT!

!* --- Scan from 07h20m46s to 07h21m46s   Fri, 2008 May 23 --- *!
sname='1039+811'  ra=10h44m23.062554s  dec= 80d54'39.44303"  qual=999  calib=' '
disk=off
stop=07h20m46s   !NEXT!        
qual=  0
disk=off
stop=07h21m46s   !NEXT!

!* --- Scan from 07h23m38s to 07h24m38s   Fri, 2008 May 23 --- *!
sname='1300+580'  ra=13h02m52.465282s  dec= 57d48'37.60942"  qual=999  calib=' '
disk=off
stop=07h23m38s   !NEXT!        
qual=  0
disk=off
stop=07h24m38s   !NEXT!

!* --- Scan from 07h27m07s to 07h29m07s   Fri, 2008 May 23 --- *!
sname='1739+522'  ra=17h40m36.977850s  dec= 52d11'43.40744"  qual=999  calib='V'
synth=( 2, 7.6),( 3,11.6)
ifchan=(2,B),(4,B)
bbsynth=( 1,943.50),( 2,959.75),( 3,959.75),( 4,975.25)
pcal=OFF
pcalxbit1=(2,S2),(3,S3),(4,S4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(1,M1),(2,M2),(3,M3),(4,M4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxfreq1=(1,0),(2,0),(3,0),(4,0)
pcalxfreq2=(1,0),(2,0),(3,0),(4,0)
disk=off
stop=07h27m07s   !NEXT!        
qual=  0
disk=off
stop=07h29m07s   !NEXT!

!* --- Scan from 07h32m20s to 07h34m20s   Fri, 2008 May 23 --- *!
sname='J1800+3848'  ra=18h00m24.765362s  dec= 38d48'30.69749"  qual=999  calib='V'
disk=off
stop=07h32m20s   !NEXT!        
qual=  0
disk=off
stop=07h34m20s   !NEXT!

!* --- Scan from 07h36m20s to 07h36m50s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=999  calib=' '
disk=off
stop=07h36m20s   !NEXT!        
qual=  0
disk=off
stop=07h36m50s   !NEXT!

!* --- Scan from 07h36m50s to 07h37m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h37m20s   !NEXT!

!* --- Scan from 07h37m20s to 07h37m50s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=07h37m50s   !NEXT!

!* --- Scan from 07h37m50s to 07h38m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h38m20s   !NEXT!

!* --- Scan from 07h38m20s to 07h38m50s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=07h38m50s   !NEXT!

!* --- Scan from 07h38m50s to 07h39m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h39m20s   !NEXT!

!* --- Scan from 07h39m20s to 07h39m50s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=07h39m50s   !NEXT!

!* --- Scan from 07h39m50s to 07h40m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h40m20s   !NEXT!

!* --- Scan from 07h40m20s to 07h40m50s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=07h40m50s   !NEXT!

!* --- Scan from 07h40m50s to 07h41m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h41m20s   !NEXT!

!* --- Scan from 07h41m20s to 07h41m50s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=07h41m50s   !NEXT!

!* --- Scan from 07h41m50s to 07h42m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h42m20s   !NEXT!

!* --- Scan from 07h42m20s to 07h42m50s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=07h42m50s   !NEXT!

!* --- Scan from 07h42m50s to 07h43m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h43m20s   !NEXT!

!* --- Scan from 07h43m20s to 07h43m50s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=07h43m50s   !NEXT!

!* --- Scan from 07h43m50s to 07h44m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h44m20s   !NEXT!

!* --- Scan from 07h44m20s to 07h46m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h46m00s   !NEXT!

!* --- Scan from 07h46m00s to 07h46m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=07h46m50s   !NEXT!

!* --- Scan from 07h46m50s to 07h47m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h47m40s   !NEXT!

!* --- Scan from 07h47m40s to 07h48m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=07h48m30s   !NEXT!

!* --- Scan from 07h48m30s to 07h49m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h49m20s   !NEXT!

!* --- Scan from 07h49m20s to 07h50m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=07h50m10s   !NEXT!

!* --- Scan from 07h50m10s to 07h51m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h51m00s   !NEXT!

!* --- Scan from 07h51m00s to 07h51m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=07h51m50s   !NEXT!

!* --- Scan from 07h51m50s to 07h52m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h52m40s   !NEXT!

!* --- Scan from 07h52m40s to 07h53m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=07h53m30s   !NEXT!

!* --- Scan from 07h53m30s to 07h54m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h54m20s   !NEXT!

!* --- Scan from 07h54m20s to 07h55m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=07h55m10s   !NEXT!

!* --- Scan from 07h55m10s to 07h56m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h56m00s   !NEXT!

!* --- Scan from 07h56m00s to 07h56m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=07h56m50s   !NEXT!

!* --- Scan from 07h56m50s to 07h57m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h57m40s   !NEXT!

!* --- Scan from 07h57m40s to 07h58m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=07h58m30s   !NEXT!

!* --- Scan from 07h58m30s to 07h59m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=07h59m20s   !NEXT!

!* --- Scan from 07h59m20s to 08h00m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h00m10s   !NEXT!

!* --- Scan from 08h00m10s to 08h01m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h01m00s   !NEXT!

!* --- Scan from 08h01m00s to 08h01m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h01m50s   !NEXT!

!* --- Scan from 08h01m50s to 08h02m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h02m40s   !NEXT!

!* --- Scan from 08h02m40s to 08h03m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h03m30s   !NEXT!

!* --- Scan from 08h03m30s to 08h04m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h04m20s   !NEXT!

!* --- Scan from 08h04m20s to 08h05m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h05m10s   !NEXT!

!* --- Scan from 08h05m10s to 08h06m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h06m00s   !NEXT!

!* --- Scan from 08h06m00s to 08h06m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h06m50s   !NEXT!

!* --- Scan from 08h06m50s to 08h07m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h07m40s   !NEXT!

!* --- Scan from 08h07m40s to 08h08m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h08m30s   !NEXT!

!* --- Scan from 08h08m30s to 08h09m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h09m20s   !NEXT!

!* --- Scan from 08h09m20s to 08h10m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h10m10s   !NEXT!

!* --- Scan from 08h10m10s to 08h11m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h11m00s   !NEXT!

!* --- Scan from 08h11m00s to 08h11m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h11m50s   !NEXT!

!* --- Scan from 08h11m50s to 08h12m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h12m40s   !NEXT!

!* --- Scan from 08h12m40s to 08h13m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h13m30s   !NEXT!

!* --- Scan from 08h13m30s to 08h14m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h14m20s   !NEXT!

!* --- Scan from 08h14m20s to 08h15m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h15m10s   !NEXT!

!* --- Scan from 08h15m10s to 08h16m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h16m00s   !NEXT!

!* --- Scan from 08h16m00s to 08h16m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h16m50s   !NEXT!

!* --- Scan from 08h16m50s to 08h17m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h17m40s   !NEXT!

!* --- Scan from 08h17m40s to 08h18m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h18m30s   !NEXT!

!* --- Scan from 08h18m30s to 08h19m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h19m20s   !NEXT!

!* --- Scan from 08h19m20s to 08h20m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h20m10s   !NEXT!

!* --- Scan from 08h20m10s to 08h21m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h21m00s   !NEXT!

!* --- Scan from 08h21m00s to 08h21m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h21m50s   !NEXT!

!* --- Scan from 08h21m50s to 08h22m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h22m40s   !NEXT!

!* --- Scan from 08h22m40s to 08h23m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h23m30s   !NEXT!

!* --- Scan from 08h23m30s to 08h24m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h24m20s   !NEXT!

!* --- Scan from 08h24m20s to 08h25m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h25m10s   !NEXT!

!* --- Scan from 08h25m10s to 08h26m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h26m00s   !NEXT!

!* --- Scan from 08h26m00s to 08h26m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h26m50s   !NEXT!

!* --- Scan from 08h26m50s to 08h27m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h27m40s   !NEXT!

!* --- Scan from 08h27m40s to 08h28m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h28m30s   !NEXT!

!* --- Scan from 08h28m30s to 08h29m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h29m20s   !NEXT!

!* --- Scan from 08h29m20s to 08h30m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h30m10s   !NEXT!

!* --- Scan from 08h30m10s to 08h31m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h31m00s   !NEXT!

!* --- Scan from 08h31m00s to 08h31m30s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=08h31m30s   !NEXT!

!* --- Scan from 08h31m30s to 08h32m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h32m00s   !NEXT!

!* --- Scan from 08h32m00s to 08h32m30s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=08h32m30s   !NEXT!

!* --- Scan from 08h32m30s to 08h33m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h33m00s   !NEXT!

!* --- Scan from 08h33m00s to 08h33m30s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=08h33m30s   !NEXT!

!* --- Scan from 08h33m30s to 08h34m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h34m00s   !NEXT!

!* --- Scan from 08h34m00s to 08h34m30s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=08h34m30s   !NEXT!

!* --- Scan from 08h34m30s to 08h35m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h35m00s   !NEXT!

!* --- Scan from 08h35m00s to 08h35m30s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=08h35m30s   !NEXT!

!* --- Scan from 08h35m30s to 08h36m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h36m00s   !NEXT!

!* --- Scan from 08h36m00s to 08h36m30s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=08h36m30s   !NEXT!

!* --- Scan from 08h36m30s to 08h37m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h37m00s   !NEXT!

!* --- Scan from 08h37m00s to 08h37m30s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=08h37m30s   !NEXT!

!* --- Scan from 08h37m30s to 08h38m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h38m00s   !NEXT!

!* --- Scan from 08h38m00s to 08h38m30s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=08h38m30s   !NEXT!

!* --- Scan from 08h38m30s to 08h39m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h39m00s   !NEXT!

!* --- Scan from 08h39m00s to 08h39m30s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=08h39m30s   !NEXT!

!* --- Scan from 08h39m30s to 08h40m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h40m00s   !NEXT!

!* --- Scan from 08h40m00s to 08h40m30s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=08h40m30s   !NEXT!

!* --- Scan from 08h40m30s to 08h41m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h41m00s   !NEXT!

!* --- Scan from 08h41m00s to 08h42m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h42m40s   !NEXT!

!* --- Scan from 08h42m40s to 08h43m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h43m30s   !NEXT!

!* --- Scan from 08h43m30s to 08h44m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h44m20s   !NEXT!

!* --- Scan from 08h44m20s to 08h45m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h45m10s   !NEXT!

!* --- Scan from 08h45m10s to 08h46m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h46m00s   !NEXT!

!* --- Scan from 08h46m00s to 08h46m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h46m50s   !NEXT!

!* --- Scan from 08h46m50s to 08h47m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h47m40s   !NEXT!

!* --- Scan from 08h47m40s to 08h48m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h48m30s   !NEXT!

!* --- Scan from 08h48m30s to 08h49m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h49m20s   !NEXT!

!* --- Scan from 08h49m20s to 08h50m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h50m10s   !NEXT!

!* --- Scan from 08h50m10s to 08h51m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h51m00s   !NEXT!

!* --- Scan from 08h51m00s to 08h51m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h51m50s   !NEXT!

!* --- Scan from 08h51m50s to 08h52m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h52m40s   !NEXT!

!* --- Scan from 08h52m40s to 08h53m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h53m30s   !NEXT!

!* --- Scan from 08h53m30s to 08h54m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h54m20s   !NEXT!

!* --- Scan from 08h54m20s to 08h55m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h55m10s   !NEXT!

!* --- Scan from 08h55m10s to 08h56m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h56m00s   !NEXT!

!* --- Scan from 08h56m00s to 08h56m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h56m50s   !NEXT!

!* --- Scan from 08h56m50s to 08h57m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h57m40s   !NEXT!

!* --- Scan from 08h57m40s to 08h58m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=08h58m30s   !NEXT!

!* --- Scan from 08h58m30s to 08h59m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=08h59m20s   !NEXT!

!* --- Scan from 08h59m20s to 09h00m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h00m10s   !NEXT!

!* --- Scan from 09h00m10s to 09h01m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h01m00s   !NEXT!

!* --- Scan from 09h01m00s to 09h01m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h01m50s   !NEXT!

!* --- Scan from 09h01m50s to 09h02m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h02m40s   !NEXT!

!* --- Scan from 09h02m40s to 09h03m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h03m30s   !NEXT!

!* --- Scan from 09h03m30s to 09h04m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h04m20s   !NEXT!

!* --- Scan from 09h04m20s to 09h05m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h05m10s   !NEXT!

!* --- Scan from 09h05m10s to 09h06m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h06m00s   !NEXT!

!* --- Scan from 09h06m00s to 09h06m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h06m50s   !NEXT!

!* --- Scan from 09h06m50s to 09h07m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h07m40s   !NEXT!

!* --- Scan from 09h07m40s to 09h08m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h08m30s   !NEXT!

!* --- Scan from 09h08m30s to 09h09m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h09m20s   !NEXT!

!* --- Scan from 09h09m20s to 09h10m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h10m10s   !NEXT!

!* --- Scan from 09h10m10s to 09h11m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h11m00s   !NEXT!

!* --- Scan from 09h11m00s to 09h11m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h11m50s   !NEXT!

!* --- Scan from 09h11m50s to 09h12m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h12m40s   !NEXT!

!* --- Scan from 09h12m40s to 09h13m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h13m30s   !NEXT!

!* --- Scan from 09h13m30s to 09h14m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h14m20s   !NEXT!

!* --- Scan from 09h14m20s to 09h15m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h15m10s   !NEXT!

!* --- Scan from 09h15m10s to 09h16m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h16m00s   !NEXT!

!* --- Scan from 09h16m00s to 09h16m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h16m50s   !NEXT!

!* --- Scan from 09h16m50s to 09h17m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h17m40s   !NEXT!

!* --- Scan from 09h17m40s to 09h18m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h18m30s   !NEXT!

!* --- Scan from 09h18m30s to 09h19m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h19m20s   !NEXT!

!* --- Scan from 09h19m20s to 09h20m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h20m10s   !NEXT!

!* --- Scan from 09h20m10s to 09h21m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h21m00s   !NEXT!

!* --- Scan from 09h21m00s to 09h21m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h21m50s   !NEXT!

!* --- Scan from 09h21m50s to 09h22m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h22m40s   !NEXT!

!* --- Scan from 09h22m40s to 09h23m30s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h23m30s   !NEXT!

!* --- Scan from 09h23m30s to 09h24m20s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h24m20s   !NEXT!

!* --- Scan from 09h24m20s to 09h25m10s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h25m10s   !NEXT!

!* --- Scan from 09h25m10s to 09h26m00s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h26m00s   !NEXT!

!* --- Scan from 09h26m00s to 09h26m50s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h26m50s   !NEXT!

!* --- Scan from 09h26m50s to 09h27m40s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h27m40s   !NEXT!

!* --- Scan from 09h29m33s to 09h31m33s   Fri, 2008 May 23 --- *!
sname='1739+522'  ra=17h40m36.977850s  dec= 52d11'43.40744"  qual=999  calib='V'
disk=off
stop=09h29m33s   !NEXT!        
qual=  0
disk=off
stop=09h31m33s   !NEXT!

!* --- Scan from 09h35m34s to 09h37m34s   Fri, 2008 May 23 --- *!
sname='J1800+3848'  ra=18h00m24.765362s  dec= 38d48'30.69749"  qual=999  calib='V'
disk=off
stop=09h35m34s   !NEXT!        
qual=  0
disk=off
stop=09h37m34s   !NEXT!

!* --- Scan from 09h37m34s to 09h38m04s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=09h38m04s   !NEXT!

!* --- Scan from 09h38m04s to 09h38m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h38m34s   !NEXT!

!* --- Scan from 09h38m34s to 09h39m04s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=09h39m04s   !NEXT!

!* --- Scan from 09h39m04s to 09h39m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h39m34s   !NEXT!

!* --- Scan from 09h39m34s to 09h40m04s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=09h40m04s   !NEXT!

!* --- Scan from 09h40m04s to 09h40m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h40m34s   !NEXT!

!* --- Scan from 09h40m34s to 09h41m04s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=09h41m04s   !NEXT!

!* --- Scan from 09h41m04s to 09h41m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h41m34s   !NEXT!

!* --- Scan from 09h41m34s to 09h42m04s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=09h42m04s   !NEXT!

!* --- Scan from 09h42m04s to 09h42m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h42m34s   !NEXT!

!* --- Scan from 09h42m34s to 09h43m04s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=09h43m04s   !NEXT!

!* --- Scan from 09h43m04s to 09h43m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h43m34s   !NEXT!

!* --- Scan from 09h43m34s to 09h44m04s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=09h44m04s   !NEXT!

!* --- Scan from 09h44m04s to 09h44m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h44m34s   !NEXT!

!* --- Scan from 09h44m34s to 09h45m04s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=09h45m04s   !NEXT!

!* --- Scan from 09h45m04s to 09h45m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h45m34s   !NEXT!

!* --- Scan from 09h45m34s to 09h46m04s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=09h46m04s   !NEXT!

!* --- Scan from 09h46m04s to 09h46m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h46m34s   !NEXT!

!* --- Scan from 09h46m34s to 09h47m04s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=09h47m04s   !NEXT!

!* --- Scan from 09h47m04s to 09h47m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h47m34s   !NEXT!

!* --- Scan from 09h47m34s to 09h49m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h49m14s   !NEXT!

!* --- Scan from 09h49m14s to 09h50m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h50m04s   !NEXT!

!* --- Scan from 09h50m04s to 09h50m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h50m54s   !NEXT!

!* --- Scan from 09h50m54s to 09h51m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h51m44s   !NEXT!

!* --- Scan from 09h51m44s to 09h52m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h52m34s   !NEXT!

!* --- Scan from 09h52m34s to 09h53m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h53m24s   !NEXT!

!* --- Scan from 09h53m24s to 09h54m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h54m14s   !NEXT!

!* --- Scan from 09h54m14s to 09h55m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h55m04s   !NEXT!

!* --- Scan from 09h55m04s to 09h55m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h55m54s   !NEXT!

!* --- Scan from 09h55m54s to 09h56m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h56m44s   !NEXT!

!* --- Scan from 09h56m44s to 09h57m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h57m34s   !NEXT!

!* --- Scan from 09h57m34s to 09h58m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=09h58m24s   !NEXT!

!* --- Scan from 09h58m24s to 09h59m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=09h59m14s   !NEXT!

!* --- Scan from 09h59m14s to 10h00m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h00m04s   !NEXT!

!* --- Scan from 10h00m04s to 10h00m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h00m54s   !NEXT!

!* --- Scan from 10h00m54s to 10h01m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h01m44s   !NEXT!

!* --- Scan from 10h01m44s to 10h02m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h02m34s   !NEXT!

!* --- Scan from 10h02m34s to 10h03m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h03m24s   !NEXT!

!* --- Scan from 10h03m24s to 10h04m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h04m14s   !NEXT!

!* --- Scan from 10h04m14s to 10h05m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h05m04s   !NEXT!

!* --- Scan from 10h05m04s to 10h05m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h05m54s   !NEXT!

!* --- Scan from 10h05m54s to 10h06m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h06m44s   !NEXT!

!* --- Scan from 10h06m44s to 10h07m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h07m34s   !NEXT!

!* --- Scan from 10h07m34s to 10h08m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h08m24s   !NEXT!

!* --- Scan from 10h08m24s to 10h09m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h09m14s   !NEXT!

!* --- Scan from 10h09m14s to 10h10m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h10m04s   !NEXT!

!* --- Scan from 10h10m04s to 10h10m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h10m54s   !NEXT!

!* --- Scan from 10h10m54s to 10h11m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h11m44s   !NEXT!

!* --- Scan from 10h11m44s to 10h12m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h12m34s   !NEXT!

!* --- Scan from 10h12m34s to 10h13m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h13m24s   !NEXT!

!* --- Scan from 10h13m24s to 10h14m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h14m14s   !NEXT!

!* --- Scan from 10h14m14s to 10h15m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h15m04s   !NEXT!

!* --- Scan from 10h15m04s to 10h15m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h15m54s   !NEXT!

!* --- Scan from 10h15m54s to 10h16m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h16m44s   !NEXT!

!* --- Scan from 10h16m44s to 10h17m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h17m34s   !NEXT!

!* --- Scan from 10h17m34s to 10h18m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h18m24s   !NEXT!

!* --- Scan from 10h18m24s to 10h19m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h19m14s   !NEXT!

!* --- Scan from 10h19m14s to 10h20m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h20m04s   !NEXT!

!* --- Scan from 10h20m04s to 10h20m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h20m54s   !NEXT!

!* --- Scan from 10h20m54s to 10h21m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h21m44s   !NEXT!

!* --- Scan from 10h21m44s to 10h22m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h22m34s   !NEXT!

!* --- Scan from 10h22m34s to 10h23m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h23m24s   !NEXT!

!* --- Scan from 10h23m24s to 10h24m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h24m14s   !NEXT!

!* --- Scan from 10h24m14s to 10h25m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h25m04s   !NEXT!

!* --- Scan from 10h25m04s to 10h25m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h25m54s   !NEXT!

!* --- Scan from 10h25m54s to 10h26m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h26m44s   !NEXT!

!* --- Scan from 10h26m44s to 10h27m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h27m34s   !NEXT!

!* --- Scan from 10h27m34s to 10h28m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h28m24s   !NEXT!

!* --- Scan from 10h28m24s to 10h29m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h29m14s   !NEXT!

!* --- Scan from 10h29m14s to 10h30m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h30m04s   !NEXT!

!* --- Scan from 10h30m04s to 10h30m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h30m54s   !NEXT!

!* --- Scan from 10h30m54s to 10h31m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h31m44s   !NEXT!

!* --- Scan from 10h31m44s to 10h32m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h32m34s   !NEXT!

!* --- Scan from 10h32m34s to 10h33m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h33m24s   !NEXT!

!* --- Scan from 10h33m24s to 10h34m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h34m14s   !NEXT!

!* --- Scan from 10h34m14s to 10h34m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=10h34m44s   !NEXT!

!* --- Scan from 10h34m44s to 10h35m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h35m14s   !NEXT!

!* --- Scan from 10h35m14s to 10h35m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=10h35m44s   !NEXT!

!* --- Scan from 10h35m44s to 10h36m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h36m14s   !NEXT!

!* --- Scan from 10h36m14s to 10h36m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=10h36m44s   !NEXT!

!* --- Scan from 10h36m44s to 10h37m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h37m14s   !NEXT!

!* --- Scan from 10h37m14s to 10h37m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=10h37m44s   !NEXT!

!* --- Scan from 10h37m44s to 10h38m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h38m14s   !NEXT!

!* --- Scan from 10h38m14s to 10h38m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=10h38m44s   !NEXT!

!* --- Scan from 10h38m44s to 10h39m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h39m14s   !NEXT!

!* --- Scan from 10h39m14s to 10h39m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=10h39m44s   !NEXT!

!* --- Scan from 10h39m44s to 10h40m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h40m14s   !NEXT!

!* --- Scan from 10h40m14s to 10h40m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=10h40m44s   !NEXT!

!* --- Scan from 10h40m44s to 10h41m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h41m14s   !NEXT!

!* --- Scan from 10h41m14s to 10h41m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=10h41m44s   !NEXT!

!* --- Scan from 10h41m44s to 10h42m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h42m14s   !NEXT!

!* --- Scan from 10h42m14s to 10h42m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=10h42m44s   !NEXT!

!* --- Scan from 10h42m44s to 10h43m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h43m14s   !NEXT!

!* --- Scan from 10h43m14s to 10h43m44s   Fri, 2008 May 23 --- *!
sname='1705+456'  ra=17h07m17.753418s  dec= 45d36'10.55276"  qual=  0  calib=' '
disk=off
stop=10h43m44s   !NEXT!

!* --- Scan from 10h43m44s to 10h44m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h44m14s   !NEXT!

!* --- Scan from 10h44m14s to 10h45m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h45m54s   !NEXT!

!* --- Scan from 10h45m54s to 10h46m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h46m44s   !NEXT!

!* --- Scan from 10h46m44s to 10h47m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h47m34s   !NEXT!

!* --- Scan from 10h47m34s to 10h48m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h48m24s   !NEXT!

!* --- Scan from 10h48m24s to 10h49m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h49m14s   !NEXT!

!* --- Scan from 10h49m14s to 10h50m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h50m04s   !NEXT!

!* --- Scan from 10h50m04s to 10h50m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h50m54s   !NEXT!

!* --- Scan from 10h50m54s to 10h51m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h51m44s   !NEXT!

!* --- Scan from 10h51m44s to 10h52m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h52m34s   !NEXT!

!* --- Scan from 10h52m34s to 10h53m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h53m24s   !NEXT!

!* --- Scan from 10h53m24s to 10h54m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h54m14s   !NEXT!

!* --- Scan from 10h54m14s to 10h55m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h55m04s   !NEXT!

!* --- Scan from 10h55m04s to 10h55m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h55m54s   !NEXT!

!* --- Scan from 10h55m54s to 10h56m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h56m44s   !NEXT!

!* --- Scan from 10h56m44s to 10h57m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h57m34s   !NEXT!

!* --- Scan from 10h57m34s to 10h58m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=10h58m24s   !NEXT!

!* --- Scan from 10h58m24s to 10h59m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=10h59m14s   !NEXT!

!* --- Scan from 10h59m14s to 11h00m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h00m04s   !NEXT!

!* --- Scan from 11h00m04s to 11h00m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h00m54s   !NEXT!

!* --- Scan from 11h00m54s to 11h01m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h01m44s   !NEXT!

!* --- Scan from 11h01m44s to 11h02m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h02m34s   !NEXT!

!* --- Scan from 11h02m34s to 11h03m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h03m24s   !NEXT!

!* --- Scan from 11h03m24s to 11h04m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h04m14s   !NEXT!

!* --- Scan from 11h04m14s to 11h05m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h05m04s   !NEXT!

!* --- Scan from 11h05m04s to 11h05m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h05m54s   !NEXT!

!* --- Scan from 11h05m54s to 11h06m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h06m44s   !NEXT!

!* --- Scan from 11h06m44s to 11h07m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h07m34s   !NEXT!

!* --- Scan from 11h07m34s to 11h08m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h08m24s   !NEXT!

!* --- Scan from 11h08m24s to 11h09m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h09m14s   !NEXT!

!* --- Scan from 11h09m14s to 11h10m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h10m04s   !NEXT!

!* --- Scan from 11h10m04s to 11h10m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h10m54s   !NEXT!

!* --- Scan from 11h10m54s to 11h11m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h11m44s   !NEXT!

!* --- Scan from 11h11m44s to 11h12m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h12m34s   !NEXT!

!* --- Scan from 11h12m34s to 11h13m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h13m24s   !NEXT!

!* --- Scan from 11h13m24s to 11h14m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h14m14s   !NEXT!

!* --- Scan from 11h14m14s to 11h15m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h15m04s   !NEXT!

!* --- Scan from 11h15m04s to 11h15m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h15m54s   !NEXT!

!* --- Scan from 11h15m54s to 11h16m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h16m44s   !NEXT!

!* --- Scan from 11h16m44s to 11h17m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h17m34s   !NEXT!

!* --- Scan from 11h17m34s to 11h18m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h18m24s   !NEXT!

!* --- Scan from 11h18m24s to 11h19m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h19m14s   !NEXT!

!* --- Scan from 11h19m14s to 11h20m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h20m04s   !NEXT!

!* --- Scan from 11h20m04s to 11h20m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h20m54s   !NEXT!

!* --- Scan from 11h20m54s to 11h21m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h21m44s   !NEXT!

!* --- Scan from 11h21m44s to 11h22m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h22m34s   !NEXT!

!* --- Scan from 11h22m34s to 11h23m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h23m24s   !NEXT!

!* --- Scan from 11h23m24s to 11h24m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h24m14s   !NEXT!

!* --- Scan from 11h24m14s to 11h25m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h25m04s   !NEXT!

!* --- Scan from 11h25m04s to 11h25m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h25m54s   !NEXT!

!* --- Scan from 11h25m54s to 11h26m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h26m44s   !NEXT!

!* --- Scan from 11h26m44s to 11h27m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h27m34s   !NEXT!

!* --- Scan from 11h27m34s to 11h28m24s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h28m24s   !NEXT!

!* --- Scan from 11h28m24s to 11h29m14s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h29m14s   !NEXT!

!* --- Scan from 11h29m14s to 11h30m04s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h30m04s   !NEXT!

!* --- Scan from 11h30m04s to 11h30m54s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h30m54s   !NEXT!

!* --- Scan from 11h30m54s to 11h31m44s   Fri, 2008 May 23 --- *!
sname='NGC6323'  ra=17h13m18.040800s  dec= 43d46'56.76100"  qual=  0  calib=' '
disk=off
stop=11h31m44s   !NEXT!

!* --- Scan from 11h31m44s to 11h32m34s   Fri, 2008 May 23 --- *!
sname='1709+431'  ra=17h09m41.087500s  dec= 43d18'44.54700"  qual=  0  calib='T'
disk=off
stop=11h32m34s   !NEXT!

!* --- Scan from 11h33m25s to 11h35m25s   Fri, 2008 May 23 --- *!
sname='1739+522'  ra=17h40m36.977850s  dec= 52d11'43.40744"  qual=999  calib='V'
disk=off
stop=11h33m25s   !NEXT!        
qual=  0
disk=off
stop=11h35m25s   !NEXT!

!* --- Scan from 11h40m03s to 11h42m03s   Fri, 2008 May 23 --- *!
sname='3C454.3'  ra=22h53m57.747938s  dec= 16d08'53.56093"  qual=999  calib='V'
disk=off
stop=11h40m03s   !NEXT!        
qual=  0
disk=off
stop=11h42m03s   !NEXT!

!* --- Scan from 11h46m56s to 11h47m56s   Fri, 2008 May 23 --- *!
sname='1638+572'  ra=16h38m13.456293s  dec= 57d20'23.97918"  qual=999  calib=' '
synth=( 2,15.1),( 3,12.1)
ifchan=(2,D),(4,D)
bbsynth=( 1,961.25),( 2,857.25),( 3,779.25),( 4,558.25)
pcal=1MHZ
pcalxbit1=(2,S3),(3,S1),(4,S3),(5,S1),(6,S2),(7,S3),(8,S4)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,M1),(6,M2),(7,M3),(8,M4)
pcalxfreq1=(1,750),(2,750),(3,13750),(4,13750)
pcalxfreq2=(1,750),(2,750),(3,13750),(4,13750)
disk=off
stop=11h46m56s   !NEXT!        
qual=  0
disk=off
stop=11h47m56s   !NEXT!

!* --- Scan from 11h48m56s to 11h49m56s   Fri, 2008 May 23 --- *!
sname='1727+453'  ra=17h27m27.650808s  dec= 45d30'39.73139"  qual=999  calib=' '
disk=off
stop=11h48m56s   !NEXT!        
qual=  0
disk=off
stop=11h49m56s   !NEXT!

!* --- Scan from 11h50m39s to 11h51m39s   Fri, 2008 May 23 --- *!
sname='1740+521'  ra=17h40m36.977850s  dec= 52d11'43.40750"  qual=999  calib=' '
disk=off
stop=11h50m39s   !NEXT!        
qual=  0
disk=off
stop=11h51m39s   !NEXT!

!* --- Scan from 11h55m18s to 11h56m18s   Fri, 2008 May 23 --- *!
sname='1743-035'  ra=17h43m58.856137s  dec=-03d50'04.61668"  qual=999  calib=' '
disk=off
stop=11h55m18s   !NEXT!        
qual=  0
disk=off
stop=11h56m18s   !NEXT!

!* --- Scan from 11h57m16s to 11h58m16s   Fri, 2008 May 23 --- *!
sname='1751+093'  ra=17h51m32.818573s  dec= 09d39'00.72851"  qual=999  calib=' '
disk=off
stop=11h57m16s   !NEXT!        
qual=  0
disk=off
stop=11h58m16s   !NEXT!

!* --- Scan from 12h01m01s to 12h02m01s   Fri, 2008 May 23 --- *!
sname='2225-045'  ra=22h25m47.259291s  dec=-04d57'01.39073"  qual=999  calib=' '
disk=off
stop=12h01m01s   !NEXT!        
qual=  0
disk=off
stop=12h02m01s   !NEXT!

!* --- Scan from 12h04m18s to 12h05m18s   Fri, 2008 May 23 --- *!
sname='2236+282'  ra=22h36m22.470860s  dec= 28d28'57.41329"  qual=999  calib=' '
disk=off
stop=12h04m18s   !NEXT!        
qual=  0
disk=off
stop=12h05m18s   !NEXT!

!* --- Scan from 12h08m13s to 12h09m13s   Fri, 2008 May 23 --- *!
sname='0050-092'  ra=00h50m41.317390s  dec=-09d29'05.21021"  qual=999  calib=' '
disk=off
stop=12h08m13s   !NEXT!        
qual=  0
disk=off
stop=12h09m13s   !NEXT!

!* --- Scan from 12h10m37s to 12h11m37s   Fri, 2008 May 23 --- *!
sname='0121+042'  ra=01h21m56.861699s  dec= 04d22'24.73436"  qual=999  calib=' '
disk=off
stop=12h10m37s   !NEXT!        
qual=  0
disk=off
stop=12h11m37s   !NEXT!

!* --- Scan from 12h14m06s to 12h15m06s   Fri, 2008 May 23 --- *!
sname='0136+475'  ra=01h36m58.594810s  dec= 47d51'29.10006"  qual=999  calib=' '
disk=off
stop=12h14m06s   !NEXT!        
qual=  0
disk=off
stop=12h15m06s   !NEXT!

!* --- Scan from 12h17m48s to 12h18m48s   Fri, 2008 May 23 --- *!
sname='0217+734'  ra=02h17m30.813363s  dec= 73d49'32.62176"  qual=999  calib=' '
disk=off
stop=12h17m48s   !NEXT!        
qual=  0
disk=off
stop=12h18m48s   !NEXT!

!* --- Scan from 12h20m40s to 12h21m40s   Fri, 2008 May 23 --- *!
sname='0726+791'  ra=07h26m11.735177s  dec= 79d11'31.01624"  qual=999  calib=' '
disk=off
stop=12h20m40s   !NEXT!        
qual=  0
disk=off
stop=12h21m40s   !NEXT!

!* --- Scan from 12h24m01s to 12h25m01s   Fri, 2008 May 23 --- *!
sname='1302+574'  ra=13h02m52.465282s  dec= 57d48'37.60942"  qual=999  calib=' '
disk=off
stop=12h24m01s   !NEXT!        
qual=  0
disk=off
stop=12h25m01s   !NEXT!
disk=off
stop=12h25m06s   !NEXT!
     !QUIT! 