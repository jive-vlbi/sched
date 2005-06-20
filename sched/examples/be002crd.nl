!*  Schedule for VLBA_NL   *!
!*  Experiment BE002    *!
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
!* Observing mode: Continuum *!
!* Notes: *!
!*  *!
!*  *!
!*  *!
!*  Start at 01h30m00s     Sun, 1995 Oct 22  Day of year  295   *!
program=BE002   
autoallocate=on
autoreverse=on

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 01h30m00s to 01h35m30s   Sun, 1995 Oct 22 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
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
bits=(1,1),(2,1),(3,1),(4,1)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S3),(7,S1),(8,S3)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,S2),(6,S4),(7,S2),(8,S4)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,1510),(8,1510)
samplerate=16M
tape=(1,STOP) write=(1,off)
  date = 1995Oct22
stop=01h30m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=01h35m30s   !NEXT!

!* --- Scan from 01h35m30s to 01h41m15s   Sun, 1995 Oct 22 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=01h41m15s   !NEXT!

!* --- Scan from 01h43m15s to 01h46m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=01h43m15s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=01h46m15s   !NEXT!

!* --- Scan from 01h46m15s to 01h49m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=01h49m30s   !NEXT!

!* --- Scan from 01h49m30s to 01h51m48s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=01h51m48s   !NEXT!

!* --- Scan from 01h51m48s to 01h55m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=01h55m06s   !NEXT!

!* --- Scan from 01h57m06s to 02h00m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=01h57m06s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=02h00m06s   !NEXT!

!* --- Scan from 02h00m06s to 02h03m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=02h03m21s   !NEXT!

!* --- Scan from 02h03m21s to 02h05m39s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=02h05m39s   !NEXT!

!* --- Scan from 02h05m39s to 02h08m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=02h08m57s   !NEXT!

!* --- Scan from 02h10m57s to 02h13m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=02h10m57s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=02h13m57s   !NEXT!

!* --- Scan from 02h13m57s to 02h17m13s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=02h17m13s   !NEXT!

!* --- Scan from 02h17m13s to 02h19m31s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=02h19m31s   !NEXT!

!* --- Scan from 02h19m31s to 02h22m49s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=02h22m49s   !NEXT!

!* --- Scan from 02h24m49s to 02h27m49s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=02h24m49s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=02h27m49s   !NEXT!

!* --- Scan from 02h27m49s to 02h31m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=02h31m04s   !NEXT!

!* --- Scan from 02h31m04s to 02h33m22s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=02h33m22s   !NEXT!

!* --- Scan from 02h33m22s to 02h36m40s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=02h36m40s   !NEXT!

!* --- Scan from 02h38m40s to 02h41m40s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=02h38m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=02h41m40s   !NEXT!

!* --- Scan from 02h41m40s to 02h44m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=02h44m55s   !NEXT!

!* --- Scan from 02h44m55s to 02h47m14s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=02h47m14s   !NEXT!

!* --- Scan from 02h47m14s to 02h50m32s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=02h50m32s   !NEXT!

!* --- Scan from 02h52m32s to 02h55m32s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=02h52m32s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=02h55m32s   !NEXT!

!* --- Scan from 02h55m32s to 02h58m47s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=02h58m47s   !NEXT!

!* --- Scan from 02h58m47s to 03h01m05s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=03h01m05s   !NEXT!

!* --- Scan from 03h01m05s to 03h04m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=03h04m24s   !NEXT!

!* --- Scan from 03h06m24s to 03h09m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=03h06m24s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=03h09m24s   !NEXT!

!* --- Scan from 03h09m24s to 03h12m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=03h12m39s   !NEXT!

!* --- Scan from 03h12m39s to 03h14m57s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=03h14m57s   !NEXT!

!* --- Scan from 03h14m57s to 03h18m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=03h18m15s   !NEXT!

!* --- Scan from 03h20m15s to 03h23m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=03h20m15s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=03h23m15s   !NEXT!

!* --- Scan from 03h23m15s to 03h26m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=03h26m31s   !NEXT!

!* --- Scan from 03h26m31s to 03h28m49s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=03h28m49s   !NEXT!

!* --- Scan from 03h28m49s to 03h32m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=03h32m07s   !NEXT!

!* --- Scan from 03h34m07s to 03h37m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=03h34m07s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=03h37m07s   !NEXT!

!* --- Scan from 03h37m07s to 03h40m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=03h40m23s   !NEXT!

!* --- Scan from 03h40m23s to 03h42m41s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=03h42m41s   !NEXT!

!* --- Scan from 03h42m41s to 03h46m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=03h46m00s   !NEXT!

!* --- Scan from 03h48m00s to 03h51m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=03h48m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=03h51m00s   !NEXT!

!* --- Scan from 03h51m00s to 03h54m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=03h54m15s   !NEXT!

!* --- Scan from 03h54m15s to 03h56m33s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=03h56m33s   !NEXT!

!* --- Scan from 03h56m33s to 03h59m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=03h59m52s   !NEXT!

!* --- Scan from 04h01m52s to 04h04m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=04h01m52s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=04h04m52s   !NEXT!

!* --- Scan from 04h04m52s to 04h08m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=04h08m07s   !NEXT!

!* --- Scan from 04h08m07s to 04h10m26s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=04h10m26s   !NEXT!

!* --- Scan from 04h10m26s to 04h13m44s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=04h13m44s   !NEXT!

!* --- Scan from 04h15m44s to 04h18m44s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=04h15m44s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=04h18m44s   !NEXT!

!* --- Scan from 04h18m44s to 04h21m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=04h21m59s   !NEXT!

!* --- Scan from 04h21m59s to 04h24m18s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=04h24m18s   !NEXT!

!* --- Scan from 04h24m18s to 04h27m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=04h27m36s   !NEXT!

!* --- Scan from 04h29m36s to 04h32m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=04h29m36s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=04h32m36s   !NEXT!

!* --- Scan from 04h32m36s to 04h35m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=04h35m52s   !NEXT!

!* --- Scan from 04h35m52s to 04h38m10s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=04h38m10s   !NEXT!

!* --- Scan from 04h38m10s to 04h41m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=04h41m29s   !NEXT!

!* --- Scan from 04h43m29s to 04h46m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=04h43m29s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=04h46m29s   !NEXT!

!* --- Scan from 04h46m29s to 04h49m44s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=04h49m44s   !NEXT!

!* --- Scan from 04h49m44s to 04h52m03s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=04h52m03s   !NEXT!

!* --- Scan from 04h52m03s to 04h55m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=04h55m21s   !NEXT!

!* --- Scan from 04h57m21s to 05h00m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=04h57m21s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h00m21s   !NEXT!

!* --- Scan from 05h00m21s to 05h03m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=05h03m36s   !NEXT!

!* --- Scan from 05h03m36s to 05h05m55s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=05h05m55s   !NEXT!

!* --- Scan from 05h05m55s to 05h09m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=05h09m14s   !NEXT!

!* --- Scan from 05h11m14s to 05h14m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=05h11m14s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h14m14s   !NEXT!

!* --- Scan from 05h14m14s to 05h17m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=05h17m29s   !NEXT!

!* --- Scan from 05h17m29s to 05h19m48s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=05h19m48s   !NEXT!

!* --- Scan from 05h19m48s to 05h23m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=05h23m07s   !NEXT!

!* --- Scan from 05h25m07s to 05h28m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=05h25m07s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h28m07s   !NEXT!

!* --- Scan from 05h28m07s to 05h31m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=05h31m22s   !NEXT!

!* --- Scan from 05h31m22s to 05h33m41s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=05h33m41s   !NEXT!

!* --- Scan from 05h33m41s to 05h36m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=05h36m59s   !NEXT!

!* --- Scan from 05h38m59s to 05h41m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=05h38m59s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h41m59s   !NEXT!

!* --- Scan from 05h41m59s to 05h45m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=05h45m14s   !NEXT!

!* --- Scan from 05h45m14s to 05h47m33s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=05h47m33s   !NEXT!

!* --- Scan from 05h47m33s to 05h50m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=05h50m52s   !NEXT!

!* --- Scan from 05h52m52s to 05h55m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=05h52m52s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h55m52s   !NEXT!

!* --- Scan from 05h55m52s to 05h59m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=05h59m07s   !NEXT!

!* --- Scan from 05h59m42s to 06h01m42s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=05h59m42s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h01m42s   !NEXT!

!* --- Scan from 06h02m16s to 06h05m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=06h02m16s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h05m16s   !NEXT!

!* --- Scan from 06h07m16s to 06h10m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=06h07m16s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h10m16s   !NEXT!

!* --- Scan from 06h10m16s to 06h13m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=06h13m31s   !NEXT!

!* --- Scan from 06h13m31s to 06h15m50s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=06h15m50s   !NEXT!

!* --- Scan from 06h15m50s to 06h19m08s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=06h19m08s   !NEXT!

!* --- Scan from 06h21m08s to 06h24m08s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=06h21m08s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h24m08s   !NEXT!

!* --- Scan from 06h24m08s to 06h27m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=06h27m24s   !NEXT!

!* --- Scan from 06h27m24s to 06h29m43s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=06h29m43s   !NEXT!

!* --- Scan from 06h29m43s to 06h33m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=06h33m01s   !NEXT!

!* --- Scan from 06h35m01s to 06h38m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=06h35m01s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h38m01s   !NEXT!

!* --- Scan from 06h38m01s to 06h41m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=06h41m16s   !NEXT!

!* --- Scan from 06h41m16s to 06h43m35s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=06h43m35s   !NEXT!

!* --- Scan from 06h43m35s to 06h46m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=06h46m54s   !NEXT!

!* --- Scan from 06h48m54s to 06h51m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=06h48m54s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h51m54s   !NEXT!

!* --- Scan from 06h51m54s to 06h55m09s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=06h55m09s   !NEXT!

!* --- Scan from 06h55m09s to 06h57m28s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=06h57m28s   !NEXT!

!* --- Scan from 06h57m28s to 07h00m47s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=07h00m47s   !NEXT!

!* --- Scan from 07h02m47s to 07h05m47s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=07h02m47s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h05m47s   !NEXT!

!* --- Scan from 07h05m47s to 07h09m02s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=07h09m02s   !NEXT!

!* --- Scan from 07h09m02s to 07h11m21s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=07h11m21s   !NEXT!

!* --- Scan from 07h11m21s to 07h14m40s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=07h14m40s   !NEXT!

!* --- Scan from 07h16m40s to 07h19m40s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=07h16m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h19m40s   !NEXT!

!* --- Scan from 07h19m40s to 07h22m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=07h22m55s   !NEXT!

!* --- Scan from 07h24m22s to 07h26m22s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h24m22s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h26m22s   !NEXT!

!* --- Scan from 07h26m42s to 07h29m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h26m42s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h29m42s   !NEXT!

!* --- Scan from 07h31m42s to 07h34m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=07h31m42s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h34m42s   !NEXT!

!* --- Scan from 07h34m42s to 07h37m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=07h37m57s   !NEXT!

!* --- Scan from 07h37m57s to 07h40m17s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=07h40m17s   !NEXT!

!* --- Scan from 07h40m17s to 07h43m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=07h43m36s   !NEXT!

!* --- Scan from 07h45m36s to 07h48m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=07h45m36s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h48m36s   !NEXT!

!* --- Scan from 07h48m36s to 07h51m51s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=07h51m51s   !NEXT!

!* --- Scan from 07h52m13s to 07h54m13s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h52m13s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h54m13s   !NEXT!

!* --- Scan from 07h54m35s to 07h57m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h54m35s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h57m35s   !NEXT!

!* --- Scan from 07h59m35s to 08h02m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=07h59m35s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h02m35s   !NEXT!

!* --- Scan from 08h02m35s to 08h05m51s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=08h05m51s   !NEXT!

!* --- Scan from 08h06m17s to 08h08m17s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h06m17s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h08m17s   !NEXT!

!* --- Scan from 08h08m43s to 08h11m43s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h08m43s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h11m43s   !NEXT!

!* --- Scan from 08h13m43s to 08h16m43s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=08h13m43s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h16m43s   !NEXT!

!* --- Scan from 08h18m45s to 08h21m45s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h18m45s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h21m45s   !NEXT!

!* --- Scan from 08h21m45s to 08h24m00s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=08h24m00s   !NEXT!

!* --- Scan from 08h26m02s to 08h29m02s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h26m02s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h29m02s   !NEXT!

!* --- Scan from 08h31m02s to 08h34m02s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=08h31m02s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h34m02s   !NEXT!

!* --- Scan from 08h34m02s to 08h37m18s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=08h37m18s   !NEXT!

!* --- Scan from 08h37m48s to 08h39m48s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h37m48s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h39m48s   !NEXT!

!* --- Scan from 08h40m16s to 08h43m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h40m16s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h43m16s   !NEXT!

!* --- Scan from 08h45m16s to 08h48m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=08h45m16s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h48m16s   !NEXT!

!* --- Scan from 08h50m51s to 08h53m51s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h50m51s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h53m51s   !NEXT!

!* --- Scan from 08h53m51s to 08h56m07s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=08h56m07s   !NEXT!

!* --- Scan from 08h58m58s to 09h01m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h58m58s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h01m58s   !NEXT!

!* --- Scan from 09h03m58s to 09h06m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=09h03m58s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h06m58s   !NEXT!

!* --- Scan from 09h06m58s to 09h10m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=09h10m14s   !NEXT!

!* --- Scan from 09h10m36s to 09h12m36s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h10m36s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h12m36s   !NEXT!

!* --- Scan from 09h12m57s to 09h15m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h12m57s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h15m57s   !NEXT!

!* --- Scan from 09h17m57s to 09h20m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=09h17m57s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h20m57s   !NEXT!

!* --- Scan from 09h24m07s to 09h27m07s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h24m07s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h27m07s   !NEXT!

!* --- Scan from 09h27m07s to 09h29m22s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=09h29m22s   !NEXT!

!* --- Scan from 09h32m39s to 09h35m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h32m39s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h35m39s   !NEXT!

!* --- Scan from 09h37m39s to 09h40m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=09h37m39s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h40m39s   !NEXT!

!* --- Scan from 09h40m39s to 09h43m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=09h43m54s   !NEXT!

!* --- Scan from 09h43m54s to 09h46m13s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=09h46m13s   !NEXT!

!* --- Scan from 09h46m13s to 09h49m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=09h49m31s   !NEXT!

!* --- Scan from 09h51m31s to 09h54m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=09h51m31s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h54m31s   !NEXT!

!* --- Scan from 09h57m49s to 10h00m49s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h57m49s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h00m49s   !NEXT!

!* --- Scan from 10h00m49s to 10h03m04s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=10h03m04s   !NEXT!

!* --- Scan from 10h06m22s to 10h09m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h06m22s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h09m22s   !NEXT!

!* --- Scan from 10h11m22s to 10h14m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=10h11m22s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h14m22s   !NEXT!

!* --- Scan from 10h14m22s to 10h17m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=10h17m37s   !NEXT!

!* --- Scan from 10h17m37s to 10h19m56s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=10h19m56s   !NEXT!

!* --- Scan from 10h19m56s to 10h23m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=10h23m14s   !NEXT!

!* --- Scan from 10h25m14s to 10h28m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=10h25m14s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h28m14s   !NEXT!

!* --- Scan from 10h31m15s to 10h34m15s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h31m15s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h34m15s   !NEXT!

!* --- Scan from 10h34m15s to 10h36m30s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=10h36m30s   !NEXT!

!* --- Scan from 10h39m24s to 10h42m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h39m24s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h42m24s   !NEXT!

!* --- Scan from 10h44m24s to 10h47m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=10h44m24s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h47m24s   !NEXT!

!* --- Scan from 10h47m24s to 10h50m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=10h50m39s   !NEXT!

!* --- Scan from 10h50m39s to 10h52m58s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=10h52m58s   !NEXT!

!* --- Scan from 10h52m58s to 10h56m17s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=10h56m17s   !NEXT!

!* --- Scan from 10h58m17s to 11h01m17s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=10h58m17s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h01m17s   !NEXT!

!* --- Scan from 11h03m11s to 11h06m11s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=11h03m11s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h06m11s   !NEXT!

!* --- Scan from 11h06m11s to 11h08m27s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=11h08m27s   !NEXT!

!* --- Scan from 11h10m06s to 11h13m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=11h10m06s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h13m06s   !NEXT!

!* --- Scan from 11h15m06s to 11h18m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=11h15m06s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h18m06s   !NEXT!

!* --- Scan from 11h18m06s to 11h21m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=11h21m21s   !NEXT!

!* --- Scan from 11h21m21s to 11h23m40s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=11h23m40s   !NEXT!

!* --- Scan from 11h23m40s to 11h26m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=11h26m59s   !NEXT!

!* --- Scan from 11h28m59s to 11h31m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=11h28m59s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h31m59s   !NEXT!

!* --- Scan from 11h33m39s to 11h36m39s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=11h33m39s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h36m39s   !NEXT!

!* --- Scan from 11h36m39s to 11h38m54s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=11h38m54s   !NEXT!

!* --- Scan from 11h40m37s to 11h43m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=11h40m37s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h43m37s   !NEXT!

!* --- Scan from 11h45m37s to 11h48m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=11h45m37s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h48m37s   !NEXT!

!* --- Scan from 11h48m37s to 11h51m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=11h51m52s   !NEXT!

!* --- Scan from 11h51m52s to 11h54m11s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=11h54m11s   !NEXT!

!* --- Scan from 11h54m11s to 11h57m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=11h57m30s   !NEXT!

!* --- Scan from 11h59m30s to 12h02m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=11h59m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h02m30s   !NEXT!

!* --- Scan from 12h04m12s to 12h07m12s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h04m12s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h07m12s   !NEXT!

!* --- Scan from 12h07m12s to 12h09m27s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=12h09m27s   !NEXT!

!* --- Scan from 12h11m11s to 12h14m11s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h11m11s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h14m11s   !NEXT!

!* --- Scan from 12h16m11s to 12h19m11s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=12h16m11s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h19m11s   !NEXT!

!* --- Scan from 12h19m11s to 12h22m26s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=12h22m26s   !NEXT!

!* --- Scan from 12h22m26s to 12h24m45s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=12h24m45s   !NEXT!

!* --- Scan from 12h24m45s to 12h28m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=12h28m04s   !NEXT!

!* --- Scan from 12h30m04s to 12h33m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=12h30m04s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h33m04s   !NEXT!

!* --- Scan from 12h34m45s to 12h37m45s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h34m45s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h37m45s   !NEXT!

!* --- Scan from 12h37m45s to 12h40m01s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=12h40m01s   !NEXT!

!* --- Scan from 12h41m44s to 12h44m44s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h41m44s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h44m44s   !NEXT!

!* --- Scan from 12h46m44s to 12h49m44s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=12h46m44s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h49m44s   !NEXT!

!* --- Scan from 12h49m44s to 12h52m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=12h52m59s   !NEXT!

!* --- Scan from 12h52m59s to 12h55m18s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=12h55m18s   !NEXT!

!* --- Scan from 12h55m18s to 12h58m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=12h58m37s   !NEXT!

!* --- Scan from 13h00m37s to 13h03m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=13h00m37s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h03m37s   !NEXT!

!* --- Scan from 13h05m18s to 13h08m18s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=13h05m18s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h08m18s   !NEXT!

!* --- Scan from 13h08m18s to 13h10m33s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=13h10m33s   !NEXT!

!* --- Scan from 13h12m16s to 13h15m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=13h12m16s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h15m16s   !NEXT!

!* --- Scan from 13h17m16s to 13h20m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=13h17m16s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h20m16s   !NEXT!

!* --- Scan from 13h20m16s to 13h23m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=13h23m31s   !NEXT!

!* --- Scan from 13h23m31s to 13h25m50s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=13h25m50s   !NEXT!

!* --- Scan from 13h25m50s to 13h29m08s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=13h29m08s   !NEXT!

!* --- Scan from 13h31m08s to 13h34m08s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=13h31m08s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h34m08s   !NEXT!

!* --- Scan from 13h35m48s to 13h38m48s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=13h35m48s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h38m48s   !NEXT!

!* --- Scan from 13h38m48s to 13h41m03s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=13h41m03s   !NEXT!

!* --- Scan from 13h42m45s to 13h45m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=13h42m45s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h45m45s   !NEXT!

!* --- Scan from 13h47m45s to 13h50m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=13h47m45s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h50m45s   !NEXT!

!* --- Scan from 13h50m45s to 13h54m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=13h54m00s   !NEXT!

!* --- Scan from 13h54m00s to 13h56m19s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=13h56m19s   !NEXT!

!* --- Scan from 13h56m19s to 13h59m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=13h59m37s   !NEXT!

!* --- Scan from 14h01m37s to 14h04m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=14h01m37s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h04m37s   !NEXT!

!* --- Scan from 14h06m15s to 14h09m15s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=14h06m15s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h09m15s   !NEXT!

!* --- Scan from 14h09m15s to 14h11m31s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=14h11m31s   !NEXT!

!* --- Scan from 14h13m10s to 14h16m10s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=14h13m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h16m10s   !NEXT!

!* --- Scan from 14h18m10s to 14h21m10s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=14h18m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h21m10s   !NEXT!

!* --- Scan from 14h21m10s to 14h24m25s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=14h24m25s   !NEXT!

!* --- Scan from 14h24m25s to 14h26m44s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=14h26m44s   !NEXT!

!* --- Scan from 14h26m44s to 14h30m02s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=14h30m02s   !NEXT!

!* --- Scan from 14h32m02s to 14h35m02s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=14h32m02s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h35m02s   !NEXT!

!* --- Scan from 14h36m39s to 14h39m39s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=14h36m39s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h39m39s   !NEXT!

!* --- Scan from 14h39m39s to 14h41m54s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=14h41m54s   !NEXT!

!* --- Scan from 14h43m31s to 14h46m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=14h43m31s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h46m31s   !NEXT!

!* --- Scan from 14h48m31s to 14h51m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=14h48m31s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h51m31s   !NEXT!

!* --- Scan from 14h51m31s to 14h54m46s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=14h54m46s   !NEXT!

!* --- Scan from 14h54m46s to 14h57m05s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=14h57m05s   !NEXT!

!* --- Scan from 14h57m05s to 15h00m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=15h00m23s   !NEXT!

!* --- Scan from 15h02m23s to 15h05m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=15h02m23s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h05m23s   !NEXT!

!* --- Scan from 15h06m57s to 15h09m57s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=15h06m57s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h09m57s   !NEXT!

!* --- Scan from 15h09m57s to 15h12m12s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=15h12m12s   !NEXT!

!* --- Scan from 15h13m47s to 15h16m47s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=15h13m47s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h16m47s   !NEXT!

!* --- Scan from 15h18m47s to 15h21m47s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=15h18m47s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h21m47s   !NEXT!

!* --- Scan from 15h21m47s to 15h25m02s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=15h25m02s   !NEXT!

!* --- Scan from 15h25m02s to 15h27m21s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=15h27m21s   !NEXT!

!* --- Scan from 15h27m21s to 15h30m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=15h30m39s   !NEXT!

!* --- Scan from 15h32m39s to 15h35m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=15h32m39s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h35m39s   !NEXT!

!* --- Scan from 15h37m10s to 15h40m10s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=15h37m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h40m10s   !NEXT!

!* --- Scan from 15h40m10s to 15h42m25s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=15h42m25s   !NEXT!

!* --- Scan from 15h43m57s to 15h46m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=15h43m57s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h46m57s   !NEXT!

!* --- Scan from 15h48m57s to 15h51m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=15h48m57s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h51m57s   !NEXT!

!* --- Scan from 15h51m57s to 15h55m13s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=15h55m13s   !NEXT!

!* --- Scan from 15h55m13s to 15h57m21s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=15h57m21s   !NEXT!

!* --- Scan from 15h57m21s to 16h00m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=16h00m30s   !NEXT!

!* --- Scan from 16h03m30s to 16h09m00s   Sun, 1995 Oct 22 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,15.4),( 2, 4.1)
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
tape=(1,STOP) write=(1,off)
stop=16h03m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=16h09m00s   !NEXT!

!* --- Scan from 16h09m00s to 16h14m45s   Sun, 1995 Oct 22 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=16h14m45s   !NEXT!

tape=(1,STOP)    write=(1,off)   dur=0s 
stop=16h14m50s   !NEXT!
     !QUIT! 
