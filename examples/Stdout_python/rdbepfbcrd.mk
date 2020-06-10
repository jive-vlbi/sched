!*  Schedule for VLBA_MK   *!
!*  Experiment rdbepfb  *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:  11.60  Release 11.6; Feburary 2020 *!
!* PI:       Craig Walker *!
!* Address:  National Radio Astronomy Observatory *!
!*           P. O. Box O *!
!*           Socorro, New Mexico, 87801 *!
!*            U.S.A. *!
!* Phone:    575 835 7247 *!
!* EMAIL:    cwalker@nrao.edu *!
!* Fax:      575 835 7027 *!
!* Phone during observation: 575 835 7247 *!
!* Observing mode: 6cm 128-4-2 *!
!* Notes: *!
!*  *!
!*  *!
!*  *!
!*  Start at 15h45m17s     Fri, 2010 Dec 03  Day of year  337   *!
program=rdbepfb 

diskformat=mark5c
media=(1,disk)

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 15h45m17s to 15h49m57s   Fri, 2010 Dec 03 --- *!
sname='3C279'  ra=12h56m11.166577s  dec=-05d47'21.52513"  qual=999  calib='V'
maxcaltime= 120
fe=(1,6cm),(3,6cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
logging=STANDARD
nchan= 4
format=VLBA1:4
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,840.01),( 2,840.01),( 3,936.01),( 4,936.01)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S2),(7,S3),(8,S4)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,M1),(6,M2),(7,M3),(8,M4)
pcalxfreq1=(1,10),(2,10),(3,13010),(4,13010),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,10),(2,10),(3,13010),(4,13010),(5,0),(6,0),(7,0),(8,0)
samplerate=32M
disk=off
  date = 2010Dec03
stop=15h45m17s   !NEXT!        
qual=  0
disk=off
stop=15h49m57s   !NEXT!

!* --- Scan from 15h50m27s to 15h55m07s   Fri, 2010 Dec 03 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59820"  qual=999  calib='V'
disk=off
stop=15h50m27s   !NEXT!        
qual=  0
disk=off
stop=15h55m07s   !NEXT!

!* --- Scan from 15h55m36s to 16h00m16s   Fri, 2010 Dec 03 --- *!
sname='3C279'  ra=12h56m11.166577s  dec=-05d47'21.52513"  qual=999  calib='V'
disk=off
stop=15h55m36s   !NEXT!        
qual=  0
disk=off
stop=16h00m16s   !NEXT!

!* --- Scan from 16h00m45s to 16h05m25s   Fri, 2010 Dec 03 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59820"  qual=999  calib='V'
disk=off
stop=16h00m45s   !NEXT!        
qual=  0
disk=off
stop=16h05m25s   !NEXT!

!* --- Scan from 16h05m54s to 16h10m34s   Fri, 2010 Dec 03 --- *!
sname='3C279'  ra=12h56m11.166577s  dec=-05d47'21.52513"  qual=999  calib='V'
disk=off
stop=16h05m54s   !NEXT!        
qual=  0
disk=off
stop=16h10m34s   !NEXT!

!* --- Scan from 16h11m03s to 16h15m43s   Fri, 2010 Dec 03 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59820"  qual=999  calib='V'
disk=off
stop=16h11m03s   !NEXT!        
qual=  0
disk=off
stop=16h15m43s   !NEXT!

!* --- Scan from 16h16m13s to 16h20m53s   Fri, 2010 Dec 03 --- *!
sname='3C279'  ra=12h56m11.166577s  dec=-05d47'21.52513"  qual=999  calib='V'
disk=off
stop=16h16m13s   !NEXT!        
qual=  0
disk=off
stop=16h20m53s   !NEXT!

!* --- Scan from 16h21m22s to 16h26m02s   Fri, 2010 Dec 03 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59820"  qual=999  calib='V'
disk=off
stop=16h21m22s   !NEXT!        
qual=  0
disk=off
stop=16h26m02s   !NEXT!

!* --- Scan from 16h26m32s to 16h31m12s   Fri, 2010 Dec 03 --- *!
sname='3C279'  ra=12h56m11.166577s  dec=-05d47'21.52513"  qual=999  calib='V'
disk=off
stop=16h26m32s   !NEXT!        
qual=  0
disk=off
stop=16h31m12s   !NEXT!

!* --- Scan from 16h31m41s to 16h36m21s   Fri, 2010 Dec 03 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59820"  qual=999  calib='V'
disk=off
stop=16h31m41s   !NEXT!        
qual=  0
disk=off
stop=16h36m21s   !NEXT!

!* --- Scan from 16h36m50s to 16h41m30s   Fri, 2010 Dec 03 --- *!
sname='M87'  ra=12h30m49.423383s  dec= 12d23'28.04374"  qual=999  calib='V'
disk=off
stop=16h36m50s   !NEXT!        
qual=  0
disk=off
stop=16h41m30s   !NEXT!

!* --- Scan from 16h41m50s to 16h46m30s   Fri, 2010 Dec 03 --- *!
sname='M84'  ra=12h25m03.743334s  dec= 12d53'13.13927"  qual=999  calib='V'
disk=off
stop=16h41m50s   !NEXT!        
qual=  0
disk=off
stop=16h46m30s   !NEXT!

!* --- Scan from 16h46m50s to 16h51m30s   Fri, 2010 Dec 03 --- *!
sname='M87'  ra=12h30m49.423383s  dec= 12d23'28.04374"  qual=999  calib='V'
disk=off
stop=16h46m50s   !NEXT!        
qual=  0
disk=off
stop=16h51m30s   !NEXT!

!* --- Scan from 16h51m50s to 16h56m30s   Fri, 2010 Dec 03 --- *!
sname='M84'  ra=12h25m03.743334s  dec= 12d53'13.13927"  qual=999  calib='V'
disk=off
stop=16h51m50s   !NEXT!        
qual=  0
disk=off
stop=16h56m30s   !NEXT!

!* --- Scan from 16h56m50s to 17h01m30s   Fri, 2010 Dec 03 --- *!
sname='M87'  ra=12h30m49.423383s  dec= 12d23'28.04374"  qual=999  calib='V'
disk=off
stop=16h56m50s   !NEXT!        
qual=  0
disk=off
stop=17h01m30s   !NEXT!

!* --- Scan from 17h01m50s to 17h06m30s   Fri, 2010 Dec 03 --- *!
sname='M84'  ra=12h25m03.743334s  dec= 12d53'13.13927"  qual=999  calib='V'
disk=off
stop=17h01m50s   !NEXT!        
qual=  0
disk=off
stop=17h06m30s   !NEXT!

!* --- Scan from 17h06m50s to 17h11m30s   Fri, 2010 Dec 03 --- *!
sname='M87'  ra=12h30m49.423383s  dec= 12d23'28.04374"  qual=999  calib='V'
disk=off
stop=17h06m50s   !NEXT!        
qual=  0
disk=off
stop=17h11m30s   !NEXT!

!* --- Scan from 17h11m50s to 17h16m30s   Fri, 2010 Dec 03 --- *!
sname='M84'  ra=12h25m03.743334s  dec= 12d53'13.13927"  qual=999  calib='V'
disk=off
stop=17h11m50s   !NEXT!        
qual=  0
disk=off
stop=17h16m30s   !NEXT!

!* --- Scan from 17h16m50s to 17h21m30s   Fri, 2010 Dec 03 --- *!
sname='M87'  ra=12h30m49.423383s  dec= 12d23'28.04374"  qual=999  calib='V'
disk=off
stop=17h16m50s   !NEXT!        
qual=  0
disk=off
stop=17h21m30s   !NEXT!

!* --- Scan from 17h21m50s to 17h26m30s   Fri, 2010 Dec 03 --- *!
sname='M84'  ra=12h25m03.743334s  dec= 12d53'13.13927"  qual=999  calib='V'
disk=off
stop=17h21m50s   !NEXT!        
qual=  0
disk=off
stop=17h26m30s   !NEXT!

!* --- Scan from 17h26m50s to 17h31m30s   Fri, 2010 Dec 03 --- *!
sname='M87'  ra=12h30m49.423383s  dec= 12d23'28.04374"  qual=999  calib='V'
disk=off
stop=17h26m50s   !NEXT!        
qual=  0
disk=off
stop=17h31m30s   !NEXT!

!* --- Scan from 17h31m50s to 17h36m30s   Fri, 2010 Dec 03 --- *!
sname='M84'  ra=12h25m03.743334s  dec= 12d53'13.13927"  qual=999  calib='V'
disk=off
stop=17h31m50s   !NEXT!        
qual=  0
disk=off
stop=17h36m30s   !NEXT!

!* --- Scan from 17h36m50s to 17h41m30s   Fri, 2010 Dec 03 --- *!
sname='M87'  ra=12h30m49.423383s  dec= 12d23'28.04374"  qual=999  calib='V'
disk=off
stop=17h36m50s   !NEXT!        
qual=  0
disk=off
stop=17h41m30s   !NEXT!

!* --- Scan from 17h41m50s to 17h46m30s   Fri, 2010 Dec 03 --- *!
sname='M84'  ra=12h25m03.743334s  dec= 12d53'13.13927"  qual=999  calib='V'
disk=off
stop=17h41m50s   !NEXT!        
qual=  0
disk=off
stop=17h46m30s   !NEXT!

!* --- Scan from 17h46m50s to 17h51m30s   Fri, 2010 Dec 03 --- *!
sname='M87'  ra=12h30m49.423383s  dec= 12d23'28.04374"  qual=999  calib='V'
disk=off
stop=17h46m50s   !NEXT!        
qual=  0
disk=off
stop=17h51m30s   !NEXT!

!* --- Scan from 17h51m50s to 17h56m30s   Fri, 2010 Dec 03 --- *!
sname='M84'  ra=12h25m03.743334s  dec= 12d53'13.13927"  qual=999  calib='V'
disk=off
stop=17h51m50s   !NEXT!        
qual=  0
disk=off
stop=17h56m30s   !NEXT!

!* --- Scan from 17h56m50s to 18h01m30s   Fri, 2010 Dec 03 --- *!
sname='M87'  ra=12h30m49.423383s  dec= 12d23'28.04374"  qual=999  calib='V'
disk=off
stop=17h56m50s   !NEXT!        
qual=  0
disk=off
stop=18h01m30s   !NEXT!

!* --- Scan from 18h01m50s to 18h06m30s   Fri, 2010 Dec 03 --- *!
sname='M84'  ra=12h25m03.743334s  dec= 12d53'13.13927"  qual=999  calib='V'
disk=off
stop=18h01m50s   !NEXT!        
qual=  0
disk=off
stop=18h06m30s   !NEXT!

!* --- Scan from 18h06m50s to 18h11m30s   Fri, 2010 Dec 03 --- *!
sname='M87'  ra=12h30m49.423383s  dec= 12d23'28.04374"  qual=999  calib='V'
disk=off
stop=18h06m50s   !NEXT!        
qual=  0
disk=off
stop=18h11m30s   !NEXT!

!* --- Scan from 18h11m50s to 18h16m30s   Fri, 2010 Dec 03 --- *!
sname='M84'  ra=12h25m03.743334s  dec= 12d53'13.13927"  qual=999  calib='V'
disk=off
stop=18h11m50s   !NEXT!        
qual=  0
disk=off
stop=18h16m30s   !NEXT!
disk=off
stop=18h16m35s   !NEXT!
     !QUIT! 
