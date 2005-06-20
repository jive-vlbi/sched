!*  Schedule for VLBA_MK   *!
!*  Experiment egmk5    *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:   6.0  Release: March 2005 *!
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
!*  Start at 18h06m40s     Wed, 1995 Jul 05  Day of year  186   *!
program=egmk5   
autoallocate=on
autoreverse=on

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 18h06m40s to 18h11m00s   Wed, 1995 Jul 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
maxcaltime= 120
fe=(1,6cm),(3,6cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
logging=STANDARD
nchan= 4
format=VLBA1:4
barrel=roll_auto
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S2),(7,S3),(8,S4)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,M1),(6,M2),(7,M3),(8,M4)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,0),(8,0)
samplerate=16M
tape=(1,STOP) write=(1,off)
  date = 1995Jul05
stop=18h06m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=18h11m00s   !NEXT!

!* --- Scan from 18h11m00s to 18h15m20s   Wed, 1995 Jul 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=18h15m20s   !NEXT!

!* --- Scan from 18h19m00s to 18h23m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=18h19m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=18h23m20s   !NEXT!

!* --- Scan from 18h23m20s to 18h27m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=18h27m40s   !NEXT!

!* --- Scan from 18h27m40s to 18h32m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=18h32m00s   !NEXT!

!* --- Scan from 18h32m00s to 18h36m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=18h36m20s   !NEXT!

!* --- Scan from 18h36m20s to 18h40m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=18h40m40s   !NEXT!

!* --- Scan from 18h40m40s to 18h45m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=18h45m00s   !NEXT!

!* --- Scan from 18h45m00s to 18h49m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=18h49m20s   !NEXT!

!* --- Scan from 18h49m20s to 18h53m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=18h53m40s   !NEXT!

!* --- Scan from 18h53m40s to 18h58m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=18h58m00s   !NEXT!

!* --- Scan from 18h58m00s to 19h02m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=19h02m20s   !NEXT!

!* --- Scan from 19h02m20s to 19h06m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=19h06m40s   !NEXT!

!* --- Scan from 19h06m40s to 19h11m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=19h11m00s   !NEXT!

!* --- Scan from 19h14m40s to 19h19m00s   Wed, 1995 Jul 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=19h14m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=19h19m00s   !NEXT!

!* --- Scan from 19h19m00s to 19h23m20s   Wed, 1995 Jul 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=19h23m20s   !NEXT!

!* --- Scan from 19h27m00s to 19h31m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=19h27m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=19h31m20s   !NEXT!

!* --- Scan from 19h31m20s to 19h35m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=19h35m40s   !NEXT!

!* --- Scan from 19h35m40s to 19h40m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=19h40m00s   !NEXT!

!* --- Scan from 19h40m00s to 19h44m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=19h44m20s   !NEXT!

!* --- Scan from 19h44m20s to 19h48m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=19h48m40s   !NEXT!

!* --- Scan from 19h48m40s to 19h53m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=19h53m00s   !NEXT!

!* --- Scan from 19h53m00s to 19h57m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=19h57m20s   !NEXT!

!* --- Scan from 19h57m20s to 20h01m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=20h01m40s   !NEXT!

!* --- Scan from 20h01m40s to 20h06m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=20h06m00s   !NEXT!

!* --- Scan from 20h06m00s to 20h10m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=20h10m20s   !NEXT!

!* --- Scan from 20h10m20s to 20h14m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=20h14m40s   !NEXT!

!* --- Scan from 20h14m40s to 20h19m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=20h19m00s   !NEXT!

!* --- Scan from 20h22m40s to 20h27m00s   Wed, 1995 Jul 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=20h22m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=20h27m00s   !NEXT!

!* --- Scan from 20h27m00s to 20h31m20s   Wed, 1995 Jul 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=20h31m20s   !NEXT!

!* --- Scan from 20h35m00s to 20h39m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=20h35m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=20h39m20s   !NEXT!

!* --- Scan from 20h39m20s to 20h43m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=20h43m40s   !NEXT!

!* --- Scan from 20h43m40s to 20h48m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=20h48m00s   !NEXT!

!* --- Scan from 20h48m00s to 20h52m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=20h52m20s   !NEXT!

!* --- Scan from 20h52m20s to 20h56m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=20h56m40s   !NEXT!

!* --- Scan from 20h56m40s to 21h01m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=21h01m00s   !NEXT!

!* --- Scan from 21h01m00s to 21h05m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=21h05m20s   !NEXT!

!* --- Scan from 21h05m20s to 21h09m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=21h09m40s   !NEXT!

!* --- Scan from 21h09m40s to 21h14m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=21h14m00s   !NEXT!

!* --- Scan from 21h14m00s to 21h18m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=21h18m20s   !NEXT!

!* --- Scan from 21h18m20s to 21h22m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=21h22m40s   !NEXT!

!* --- Scan from 21h22m40s to 21h27m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=21h27m00s   !NEXT!

!* --- Scan from 21h30m40s to 21h35m00s   Wed, 1995 Jul 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=21h30m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=21h35m00s   !NEXT!

!* --- Scan from 21h35m00s to 21h39m20s   Wed, 1995 Jul 05 --- *!
sname='DA193'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=21h39m20s   !NEXT!

!* --- Scan from 21h43m00s to 21h47m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=21h43m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=21h47m20s   !NEXT!

!* --- Scan from 21h47m20s to 21h51m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=21h51m40s   !NEXT!

!* --- Scan from 21h51m40s to 21h56m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=21h56m00s   !NEXT!

!* --- Scan from 21h56m00s to 22h00m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=22h00m20s   !NEXT!

!* --- Scan from 22h00m20s to 22h04m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=22h04m40s   !NEXT!

!* --- Scan from 22h04m40s to 22h09m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=22h09m00s   !NEXT!

!* --- Scan from 22h09m00s to 22h13m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=22h13m20s   !NEXT!

!* --- Scan from 22h13m20s to 22h17m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=22h17m40s   !NEXT!

!* --- Scan from 22h17m40s to 22h22m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=22h22m00s   !NEXT!

!* --- Scan from 22h22m00s to 22h26m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=22h26m20s   !NEXT!

!* --- Scan from 22h26m20s to 22h30m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=22h30m40s   !NEXT!

!* --- Scan from 22h30m40s to 22h35m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=22h35m00s   !NEXT!

!* --- Scan from 22h51m00s to 22h55m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=22h51m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=22h55m20s   !NEXT!

!* --- Scan from 22h55m20s to 22h59m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=22h59m40s   !NEXT!

!* --- Scan from 22h59m40s to 23h04m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=23h04m00s   !NEXT!

!* --- Scan from 23h04m00s to 23h08m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=23h08m20s   !NEXT!

!* --- Scan from 23h08m20s to 23h12m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=23h12m40s   !NEXT!

!* --- Scan from 23h12m40s to 23h17m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=23h17m00s   !NEXT!

!* --- Scan from 23h17m00s to 23h21m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=23h21m20s   !NEXT!

!* --- Scan from 23h21m20s to 23h25m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=23h25m40s   !NEXT!

!* --- Scan from 23h25m40s to 23h30m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=23h30m00s   !NEXT!

!* --- Scan from 23h30m00s to 23h34m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=23h34m20s   !NEXT!

!* --- Scan from 23h34m20s to 23h38m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=23h38m40s   !NEXT!

!* --- Scan from 23h38m40s to 23h43m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=23h43m00s   !NEXT!

!* --- Scan from 23h46m40s to 23h51m00s   Wed, 1995 Jul 05 --- *!
sname='OQ208'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=23h46m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=23h51m00s   !NEXT!

!* --- Scan from 23h51m00s to 23h55m20s   Wed, 1995 Jul 05 --- *!
sname='OQ208'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=23h55m20s   !NEXT!

!* --- Scan from 23h59m00s to 00h03m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=23h59m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
date=1995Jul06
stop=00h03m20s   !NEXT!

!* --- Scan from 00h03m20s to 00h07m40s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=00h07m40s   !NEXT!

!* --- Scan from 00h07m40s to 00h12m00s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=00h12m00s   !NEXT!

!* --- Scan from 00h12m00s to 00h16m20s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=00h16m20s   !NEXT!

!* --- Scan from 00h16m20s to 00h20m40s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=00h20m40s   !NEXT!

!* --- Scan from 00h20m40s to 00h25m00s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=00h25m00s   !NEXT!

!* --- Scan from 00h25m00s to 00h29m20s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=00h29m20s   !NEXT!

!* --- Scan from 00h29m20s to 00h33m40s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=00h33m40s   !NEXT!

!* --- Scan from 00h33m40s to 00h38m00s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=00h38m00s   !NEXT!

!* --- Scan from 00h38m00s to 00h42m20s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=00h42m20s   !NEXT!

!* --- Scan from 00h42m20s to 00h46m40s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=00h46m40s   !NEXT!

!* --- Scan from 00h46m40s to 00h51m00s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=00h51m00s   !NEXT!

!* --- Scan from 00h54m40s to 00h59m00s   Thu, 1995 Jul 06 --- *!
sname='OQ208'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=00h54m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=00h59m00s   !NEXT!

!* --- Scan from 00h59m00s to 01h03m20s   Thu, 1995 Jul 06 --- *!
sname='OQ208'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=01h03m20s   !NEXT!

!* --- Scan from 01h07m00s to 01h11m20s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=01h07m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=01h11m20s   !NEXT!

!* --- Scan from 01h11m20s to 01h15m40s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=01h15m40s   !NEXT!

!* --- Scan from 01h15m40s to 01h20m00s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=01h20m00s   !NEXT!

!* --- Scan from 01h20m00s to 01h24m20s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=01h24m20s   !NEXT!

!* --- Scan from 01h24m20s to 01h28m40s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=01h28m40s   !NEXT!

!* --- Scan from 01h28m40s to 01h33m00s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=01h33m00s   !NEXT!

!* --- Scan from 01h33m00s to 01h37m20s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=01h37m20s   !NEXT!

!* --- Scan from 01h37m20s to 01h41m40s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=01h41m40s   !NEXT!

!* --- Scan from 01h41m40s to 01h46m00s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=01h46m00s   !NEXT!

!* --- Scan from 01h46m00s to 01h50m20s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=01h50m20s   !NEXT!

!* --- Scan from 01h50m20s to 01h54m40s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,+RUN)  write=(1,on)
stop=01h54m40s   !NEXT!

!* --- Scan from 01h54m40s to 01h59m00s   Thu, 1995 Jul 06 --- *!
sname='4C39.25'  ra=09h27m03.013933s  dec= 39d02'20.85188"  qual=  0  calib='V'
fe=(2,2cm),(4,2cm)
synth=( 1,14.6),( 2,10.9),( 3,10.9)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,677.49),( 2,677.49),( 3,685.49),( 4,685.49)
tape=(1,+RUN)  write=(1,on)
stop=01h59m00s   !NEXT!

tape=(1,STOP)    write=(1,off)   dur=0s 
stop=01h59m05s   !NEXT!
     !QUIT! 
