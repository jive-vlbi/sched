!*  Schedule for VLBA_MK   *!
!*  Experiment eg3mmb   *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:  11.50  Release 11.5; September 2018 *!
!* PI:       Craig Walker *!
!* Address:  National Radio Astronomy Observatory *!
!*           P. O. Box O *!
!*           Socorro, New Mexico, 87801 *!
!*            U.S.A. *!
!* Phone:    505 835 7247 *!
!* EMAIL:    cwalker@nrao.edu *!
!* Fax:      505 835 7027 *!
!* Phone during observation: 505 835 7247 *!
!* Observing mode: 6cm 128-4-2 *!
!* Notes:    Will use reference pointing. *!
!*  *!
!*  *!
!*  *!
!*  Start at 02h30m00s     Wed, 2000 Jul 05  Day of year  187   *!
program=eg3mmb  

diskformat=mark5c
media=(1,disk)

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 02h30m00s to 02h40m00s   Wed, 2000 Jul 05 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59820"  qual=999  calib='V'
maxcaltime= 120
fe=(1,7mm),(3,7mm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
logging=STANDARD
nchan= 4
format=VLBA1:4
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,715.75),( 2,715.75),( 3,815.75),( 4,815.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
pcal=OFF
pcalxbit1=(1,S1),(2,S2),(3,S3),(4,S4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(1,M1),(2,M2),(3,M3),(4,M4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxfreq1=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
samplerate=32M
disk=off
  date = 2000Jul05
stop=02h30m00s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=02h40m00s   !NEXT!

!* --- Scan from 02h40m00s to 02h50m00s   Wed, 2000 Jul 05 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59820"  qual=  0  calib='V'
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
disk=off
stop=02h50m00s   !NEXT!

!* --- Scan from 04h02m38s to 04h03m38s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='3C345'  ra=16h42m58.809967s  dec= 39d48'36.99399"  qual=999  calib='V'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,715.75),( 2,715.75),( 3,815.75),( 4,815.75)
disk=off
stop=04h02m38s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=04h03m38s   !NEXT!

!* --- Scan from 04h03m44s to 04h04m44s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='3C345'  ra=16h42m58.809967s  dec= 39d48'36.99399"  qual=999  calib='V'
disk=off
stop=04h03m44s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=04h04m44s   !NEXT!

!* --- Scan from 04h06m00s to 04h16m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
disk=off
stop=04h06m00s   !NEXT!        
qual=  0
disk=off
stop=04h16m00s   !NEXT!

!* --- Scan from 04h16m00s to 04h26m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=04h26m00s   !NEXT!

!* --- Scan from 04h27m43s to 04h28m43s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='3C345'  ra=16h42m58.809967s  dec= 39d48'36.99399"  qual=999  calib='V'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,715.75),( 2,715.75),( 3,815.75),( 4,815.75)
disk=off
stop=04h27m43s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=04h28m43s   !NEXT!

!* --- Scan from 04h30m00s to 04h40m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
disk=off
stop=04h30m00s   !NEXT!        
qual=  0
disk=off
stop=04h40m00s   !NEXT!

!* --- Scan from 04h40m00s to 04h50m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=04h50m00s   !NEXT!

!* --- Scan from 04h57m34s to 04h58m34s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=04h57m34s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=04h58m34s   !NEXT!

!* --- Scan from 04h58m40s to 04h59m40s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
disk=off
stop=04h58m40s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=04h59m40s   !NEXT!

!* --- Scan from 05h00m00s to 05h02m00s   Wed, 2000 Jul 05 --- *!
sname='AHSCO'  ra=17h11m16.980000s  dec=-32d19'31.20000"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,763.36),( 2,763.36),( 3,792.13),( 4,792.13)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=05h00m00s   !NEXT!        
qual=  0
disk=off
stop=05h02m00s   !NEXT!

!* --- Scan from 05h03m40s to 05h04m40s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=05h03m40s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=05h04m40s   !NEXT!

!* --- Scan from 05h04m46s to 05h05m46s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
disk=off
stop=05h04m46s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=05h05m46s   !NEXT!

!* --- Scan from 05h06m00s to 05h16m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=05h06m00s   !NEXT!        
qual=  0
disk=off
stop=05h16m00s   !NEXT!

!* --- Scan from 05h16m00s to 05h26m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=05h26m00s   !NEXT!

!* --- Scan from 05h27m40s to 05h28m40s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=05h27m40s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=05h28m40s   !NEXT!

!* --- Scan from 05h28m46s to 05h29m46s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
disk=off
stop=05h28m46s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=05h29m46s   !NEXT!

!* --- Scan from 05h30m00s to 05h40m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=05h30m00s   !NEXT!        
qual=  0
disk=off
stop=05h40m00s   !NEXT!

!* --- Scan from 05h40m00s to 05h50m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=05h50m00s   !NEXT!

!* --- Scan from 05h51m40s to 05h52m40s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=05h51m40s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=05h52m40s   !NEXT!

!* --- Scan from 05h52m46s to 05h53m46s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
disk=off
stop=05h52m46s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=05h53m46s   !NEXT!

!* --- Scan from 05h54m00s to 06h04m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=05h54m00s   !NEXT!        
qual=  0
disk=off
stop=06h04m00s   !NEXT!

!* --- Scan from 06h04m00s to 06h14m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=06h14m00s   !NEXT!

!* --- Scan from 06h15m40s to 06h16m40s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=06h15m40s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=06h16m40s   !NEXT!

!* --- Scan from 06h16m46s to 06h17m46s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
disk=off
stop=06h16m46s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=06h17m46s   !NEXT!

!* --- Scan from 06h18m00s to 06h28m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=06h18m00s   !NEXT!        
qual=  0
disk=off
stop=06h28m00s   !NEXT!

!* --- Scan from 06h28m00s to 06h38m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=06h38m00s   !NEXT!

!* --- Scan from 06h39m39s to 06h40m39s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=06h39m39s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=06h40m39s   !NEXT!

!* --- Scan from 06h40m45s to 06h41m45s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
disk=off
stop=06h40m45s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=06h41m45s   !NEXT!

!* --- Scan from 06h42m00s to 06h52m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=06h42m00s   !NEXT!        
qual=  0
disk=off
stop=06h52m00s   !NEXT!

!* --- Scan from 06h52m00s to 07h02m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=07h02m00s   !NEXT!

!* --- Scan from 07h05m48s to 07h06m48s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='3C345'  ra=16h42m58.809967s  dec= 39d48'36.99399"  qual=999  calib='V'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,715.75),( 2,715.75),( 3,815.75),( 4,815.75)
disk=off
stop=07h05m48s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=07h06m48s   !NEXT!

!* --- Scan from 07h06m54s to 07h07m54s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='3C345'  ra=16h42m58.809967s  dec= 39d48'36.99399"  qual=999  calib='V'
disk=off
stop=07h06m54s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=07h07m54s   !NEXT!

!* --- Scan from 07h08m00s to 07h18m00s   Wed, 2000 Jul 05 --- *!
sname='3C345'  ra=16h42m58.809967s  dec= 39d48'36.99399"  qual=999  calib='V'
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
disk=off
stop=07h08m00s   !NEXT!        
qual=  0
disk=off
stop=07h18m00s   !NEXT!

!* --- Scan from 07h19m39s to 07h20m39s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=07h19m39s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=07h20m39s   !NEXT!

!* --- Scan from 07h20m45s to 07h21m45s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
disk=off
stop=07h20m45s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=07h21m45s   !NEXT!

!* --- Scan from 07h22m00s to 07h32m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=07h22m00s   !NEXT!        
qual=  0
disk=off
stop=07h32m00s   !NEXT!

!* --- Scan from 07h32m00s to 07h42m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=07h42m00s   !NEXT!

!* --- Scan from 07h43m39s to 07h44m39s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=07h43m39s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=07h44m39s   !NEXT!

!* --- Scan from 07h44m45s to 07h45m45s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
disk=off
stop=07h44m45s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=07h45m45s   !NEXT!

!* --- Scan from 07h46m00s to 07h56m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=07h46m00s   !NEXT!        
qual=  0
disk=off
stop=07h56m00s   !NEXT!

!* --- Scan from 07h56m00s to 08h06m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=08h06m00s   !NEXT!

!* --- Scan from 08h07m37s to 08h08m37s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=08h07m37s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=08h08m37s   !NEXT!

!* --- Scan from 08h08m43s to 08h09m43s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
disk=off
stop=08h08m43s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=08h09m43s   !NEXT!

!* --- Scan from 08h10m00s to 08h20m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=08h10m00s   !NEXT!        
qual=  0
disk=off
stop=08h20m00s   !NEXT!

!* --- Scan from 08h20m00s to 08h30m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=08h30m00s   !NEXT!

!* --- Scan from 08h31m35s to 08h32m35s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=08h31m35s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=08h32m35s   !NEXT!

!* --- Scan from 08h32m41s to 08h33m41s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
disk=off
stop=08h32m41s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=08h33m41s   !NEXT!

!* --- Scan from 08h34m00s to 08h44m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=08h34m00s   !NEXT!        
qual=  0
disk=off
stop=08h44m00s   !NEXT!

!* --- Scan from 08h44m00s to 08h54m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=08h54m00s   !NEXT!

!* --- Scan from 08h55m34s to 08h56m34s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=08h55m34s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=08h56m34s   !NEXT!

!* --- Scan from 08h56m40s to 08h57m40s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
disk=off
stop=08h56m40s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=08h57m40s   !NEXT!

!* --- Scan from 08h58m00s to 09h08m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=08h58m00s   !NEXT!        
qual=  0
disk=off
stop=09h08m00s   !NEXT!

!* --- Scan from 09h08m00s to 09h18m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
disk=off
stop=09h18m00s   !NEXT!

!* --- Scan from 09h20m25s to 09h21m25s   Wed, 2000 Jul 05 --- *!
!*  Following scan added by Sched for reference pointing  *!
sname='SIO-RCAS'  ra=23h58m24.734000s  dec= 51d23'19.57000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1, 3.9),( 2, 7.6),( 3,11.6)
format=VLBA1:1
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.59),( 2,721.59),( 3,821.59),( 4,821.59)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
samplerate=4M
disk=off
stop=09h20m25s   !NEXT!        
qual=  0
peakchan =  1
disk=off
stop=09h21m25s   !NEXT!

!* --- Scan from 09h22m00s to 09h32m00s   Wed, 2000 Jul 05 --- *!
sname='3C454.3'  ra=22h53m57.747939s  dec= 16d08'53.56091"  qual=999  calib='V'
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2, 4.9),( 3,12.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,760.75),( 2,760.75),( 3,824.75),( 4,824.75)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
samplerate=32M
disk=off
stop=09h22m00s   !NEXT!        
qual=  0
disk=off
stop=09h32m00s   !NEXT!
disk=off
stop=09h32m05s   !NEXT!
     !QUIT! 
