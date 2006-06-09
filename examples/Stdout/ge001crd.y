!*  Schedule for VLA27     *!
!*  Experiment ge001    *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:   6.05  June 2006 *!
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
!* Notes: *!
!*  *!
!*  *!
!*  *!
!*  Start at 18h30m00s     Wed, 1997 Mar 05  Day of year   64   *!
program=ge001   
autoallocate=on
autoreverse=on

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 18h30m00s to 18h52m00s   Wed, 1997 Mar 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
maxcaltime= 120
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (4,  4.3601000000)
extlosideband = (4,U)
logging=STANDARD
nchan= 8
format=VLBA1:2
barrel=roll_auto
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,1),(4,2),(5,3),(6,4),(7,3),(8,4)
ifchan=(1,A),(2,D),(3,A),(4,D),(5,A),(6,D),(7,A),(8,D)
sideband=(1,L),(2,L),(3,U),(4,U),(5,L),(6,L),(7,U),(8,U)
bits=(1,2),(2,2),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2)
period=(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1)
level=(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,619.39),( 2,619.39),( 3,619.39),( 4,619.39),( 5,635.39),( 6,635.39)
bbsynth=( 7,635.39),( 8,635.39)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S5),(4,S7),(5,S1),(6,S3),(7,S5),(8,S7)
pcalxbit2=(1,S2),(2,S4),(3,S6),(4,S8),(5,S2),(6,S4),(7,S6),(8,S8)
pcalxfreq1=(1,490),(2,510),(3,490),(4,510),(5,6490),(6,6510),(7,6490),(8,6510)
pcalxfreq2=(1,490),(2,510),(3,490),(4,510),(5,6490),(6,6510),(7,6490),(8,6510)
samplerate=16M
tape=(1,STOP) write=(1,off)
  date = 1997Mar05
stop=18h30m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=18h52m00s   !NEXT!

!* --- Scan from 19h00m00s to 19h22m00s   Wed, 1997 Mar 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=19h00m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=19h22m00s   !NEXT!

!* --- Scan from 19h30m00s to 19h52m00s   Wed, 1997 Mar 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=19h30m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=19h52m00s   !NEXT!

!* --- Scan from 20h00m00s to 20h22m00s   Wed, 1997 Mar 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=20h00m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=20h22m00s   !NEXT!

!* --- Scan from 20h30m00s to 20h52m00s   Wed, 1997 Mar 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=20h30m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=20h52m00s   !NEXT!

!* --- Scan from 21h00m00s to 21h22m00s   Wed, 1997 Mar 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=21h00m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=21h22m00s   !NEXT!

!* --- Scan from 21h30m00s to 21h52m00s   Wed, 1997 Mar 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=21h30m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=21h52m00s   !NEXT!

!* --- Scan from 22h00m00s to 22h22m00s   Wed, 1997 Mar 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=22h00m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=22h22m00s   !NEXT!

!* --- Scan from 22h30m00s to 22h52m00s   Wed, 1997 Mar 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=22h30m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=22h52m00s   !NEXT!

!* --- Scan from 23h00m00s to 23h22m00s   Wed, 1997 Mar 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=23h00m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=23h22m00s   !NEXT!

!* --- Scan from 23h30m00s to 23h52m00s   Wed, 1997 Mar 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=23h30m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=23h52m00s   !NEXT!

!* --- Scan from 00h00m01s to 00h22m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
  date = 1997Mar06
stop=00h00m01s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=00h22m00s   !NEXT!

!* --- Scan from 00h37m00s to 00h59m00s   Thu, 1997 Mar 06 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=00h37m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=00h59m00s   !NEXT!

!* --- Scan from 01h07m00s to 01h29m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=01h07m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=01h29m00s   !NEXT!

!* --- Scan from 01h37m00s to 01h59m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=01h37m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=01h59m00s   !NEXT!

!* --- Scan from 02h07m00s to 02h29m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=02h07m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=02h29m00s   !NEXT!

!* --- Scan from 02h37m00s to 02h59m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=02h37m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=02h59m00s   !NEXT!

!* --- Scan from 03h07m00s to 03h29m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=03h07m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=03h29m00s   !NEXT!

!* --- Scan from 03h37m00s to 03h59m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=03h37m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=03h59m00s   !NEXT!

!* --- Scan from 04h07m00s to 04h29m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=04h07m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=04h29m00s   !NEXT!

!* --- Scan from 04h37m00s to 04h59m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=04h37m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=04h59m00s   !NEXT!

!* --- Scan from 05h07m00s to 05h29m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=05h07m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h29m00s   !NEXT!

!* --- Scan from 05h37m00s to 05h59m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=05h37m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h59m00s   !NEXT!

!* --- Scan from 06h07m00s to 06h29m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=06h07m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h29m00s   !NEXT!

!* --- Scan from 06h37m00s to 06h59m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=06h37m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h59m00s   !NEXT!

!* --- Scan from 07h07m00s to 07h29m00s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h07m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h29m00s   !NEXT!

!* --- Scan from 07h44m00s to 08h06m00s   Thu, 1997 Mar 06 --- *!
sname='3C345'  ra=16h42m58.809968s  dec= 39d48'36.99400"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h44m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h06m00s   !NEXT!

!* --- Scan from 08h06m30s to 08h28m30s   Thu, 1997 Mar 06 --- *!
sname='NRAO512'  ra=16h40m29.632770s  dec= 39d46'46.02848"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h06m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h28m30s   !NEXT!

!* --- Scan from 08h32m30s to 08h54m30s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h32m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h54m30s   !NEXT!

!* --- Scan from 08h58m30s to 09h20m30s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h58m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h20m30s   !NEXT!

!* --- Scan from 09h24m30s to 09h46m30s   Thu, 1997 Mar 06 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h24m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h46m30s   !NEXT!

!* --- Scan from 12h52m30s to 13h14m30s   Thu, 1997 Mar 06 --- *!
sname='3C273'  ra=12h29m06.699732s  dec= 02d03'08.59815"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h52m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h14m30s   !NEXT!

!* --- Scan from 13h18m30s to 13h40m30s   Thu, 1997 Mar 06 --- *!
sname='3C273'  ra=12h29m06.699732s  dec= 02d03'08.59815"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=13h18m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h40m30s   !NEXT!

tape=(1,STOP)    write=(1,off)   dur=0s 
stop=13h40m35s   !NEXT!
     !QUIT! 
