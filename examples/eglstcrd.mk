!*  Schedule for VLBA_MK   *!
!*  Experiment eglst    *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:   6.0  Release: March 2005 *!
!* PI:       J.M. Wrobel *!
!* Address:  NRAO, P.O. Box O *!
!*           Socorro, NM 87801 *!
!*           USA *!
!*  *!
!* Phone:    +1-505-835-7392 (w) / +1-505-835-3972 (h) *!
!* EMAIL:    jwrobel@nrao.edu *!
!* Fax:      +1-505-835-7027 *!
!* Phone during observation: +1-505-835-7392 (w) / +1-505-835-3972 (h) *!
!* Observing mode: 128-4-2 *!
!* Notes:    fringe finder, manual pcal: J2202+4216=BLLAC *!
!*           phase reference source    : J2109+3532 *!
!*           phase-ref check source    : J2052+3635 *!
!*           amplitude check source    : J2202+4216=BLLAC *!
!*  Start at 10h54m07s     Tue, 1999 Apr 20  Day of year  110   *!
program=eglst   
autoallocate=on
autoreverse=on

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 10h54m07s to 10h55m07s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
maxcaltime= 120
fe=(1,20cm),(3,20cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,15.4),( 2, 2.4),( 3,15.4)
logging=STANDARD
nchan= 4
format=VLBA1:4
barrel=roll_auto
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S2),(7,S3),(8,S4)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,M1),(6,M2),(7,M3),(8,M4)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,0),(8,0)
samplerate=16M
tape=(1,STOP) write=(1,off)
  date = 1999Apr20
stop=10h54m07s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h55m07s   !NEXT!

!* --- Scan from 10h55m07s to 10h56m07s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=10h56m07s   !NEXT!

!* --- Scan from 10h56m07s to 10h59m06s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=10h59m06s   !NEXT!

!* --- Scan from 10h59m06s to 11h00m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h00m06s   !NEXT!

!* --- Scan from 11h00m06s to 11h01m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=11h01m06s   !NEXT!

!* --- Scan from 11h01m06s to 11h04m05s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h04m05s   !NEXT!

!* --- Scan from 11h04m05s to 11h05m05s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h05m05s   !NEXT!

!* --- Scan from 11h05m05s to 11h06m05s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=11h06m05s   !NEXT!

!* --- Scan from 11h06m05s to 11h09m05s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h09m05s   !NEXT!

!* --- Scan from 11h09m05s to 11h10m04s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h10m04s   !NEXT!

!* --- Scan from 11h10m04s to 11h11m04s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=11h11m04s   !NEXT!

!* --- Scan from 11h11m04s to 11h14m04s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h14m04s   !NEXT!

!* --- Scan from 11h14m04s to 11h15m04s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h15m04s   !NEXT!

!* --- Scan from 11h15m04s to 11h16m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=11h16m03s   !NEXT!

!* --- Scan from 11h16m03s to 11h19m03s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h19m03s   !NEXT!

!* --- Scan from 11h19m03s to 11h20m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h20m03s   !NEXT!

!* --- Scan from 11h20m03s to 11h21m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=11h21m03s   !NEXT!

!* --- Scan from 11h21m03s to 11h24m02s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h24m02s   !NEXT!

!* --- Scan from 11h24m02s to 11h25m02s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h25m02s   !NEXT!

!* --- Scan from 11h25m02s to 11h26m02s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=11h26m02s   !NEXT!

!* --- Scan from 11h26m02s to 11h29m01s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h29m01s   !NEXT!

!* --- Scan from 11h29m01s to 11h30m01s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h30m01s   !NEXT!

!* --- Scan from 11h30m01s to 11h31m01s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=11h31m01s   !NEXT!

!* --- Scan from 11h31m01s to 11h34m01s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h34m01s   !NEXT!

!* --- Scan from 11h34m01s to 11h35m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h35m00s   !NEXT!

!* --- Scan from 11h35m00s to 11h36m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=11h36m00s   !NEXT!

!* --- Scan from 11h36m00s to 11h39m00s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h39m00s   !NEXT!

!* --- Scan from 11h39m00s to 11h40m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h40m00s   !NEXT!

!* --- Scan from 11h40m00s to 11h40m59s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=11h40m59s   !NEXT!

!* --- Scan from 11h40m59s to 11h43m59s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h43m59s   !NEXT!

!* --- Scan from 11h43m59s to 11h44m59s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h44m59s   !NEXT!

!* --- Scan from 11h44m59s to 11h45m59s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=11h45m59s   !NEXT!

!* --- Scan from 11h45m59s to 11h48m58s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h48m58s   !NEXT!

!* --- Scan from 11h48m58s to 11h49m58s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h49m58s   !NEXT!

!* --- Scan from 11h49m58s to 11h50m58s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=11h50m58s   !NEXT!

!* --- Scan from 11h50m58s to 11h53m57s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h53m57s   !NEXT!

!* --- Scan from 11h53m57s to 11h54m57s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h54m57s   !NEXT!

!* --- Scan from 11h54m57s to 11h55m57s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=11h55m57s   !NEXT!

!* --- Scan from 11h55m57s to 11h58m56s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h58m56s   !NEXT!

!* --- Scan from 11h58m56s to 11h59m56s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=11h59m56s   !NEXT!

!* --- Scan from 11h59m56s to 12h00m56s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=12h00m56s   !NEXT!

!* --- Scan from 12h00m56s to 12h03m56s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h03m56s   !NEXT!

!* --- Scan from 12h03m56s to 12h04m55s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h04m55s   !NEXT!

!* --- Scan from 12h04m55s to 12h05m55s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=12h05m55s   !NEXT!

!* --- Scan from 12h05m55s to 12h08m55s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h08m55s   !NEXT!

!* --- Scan from 12h08m55s to 12h09m55s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h09m55s   !NEXT!

!* --- Scan from 12h09m55s to 12h10m54s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=12h10m54s   !NEXT!

!* --- Scan from 12h10m54s to 12h13m54s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h13m54s   !NEXT!

!* --- Scan from 12h13m54s to 12h14m54s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h14m54s   !NEXT!

!* --- Scan from 12h14m54s to 12h15m54s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=12h15m54s   !NEXT!

!* --- Scan from 12h15m54s to 12h18m53s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h18m53s   !NEXT!

!* --- Scan from 12h18m53s to 12h19m53s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h19m53s   !NEXT!

!* --- Scan from 12h19m53s to 12h20m53s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=12h20m53s   !NEXT!

!* --- Scan from 12h20m53s to 12h23m52s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h23m52s   !NEXT!

!* --- Scan from 12h23m52s to 12h24m52s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h24m52s   !NEXT!

!* --- Scan from 12h24m52s to 12h25m52s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=12h25m52s   !NEXT!

!* --- Scan from 12h25m52s to 12h28m51s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h28m51s   !NEXT!

!* --- Scan from 12h28m51s to 12h29m51s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h29m51s   !NEXT!

!* --- Scan from 12h29m51s to 12h30m51s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=12h30m51s   !NEXT!

!* --- Scan from 12h30m51s to 12h33m51s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h33m51s   !NEXT!

!* --- Scan from 12h33m51s to 12h34m51s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h34m51s   !NEXT!

!* --- Scan from 12h36m51s to 12h37m51s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
tape=(1,STOP) write=(1,off)
stop=12h36m51s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h37m51s   !NEXT!

!* --- Scan from 12h37m51s to 12h40m51s   Tue, 1999 Apr 20 --- *!
sname='J2052+3635'  ra=20h52m52.057400s  dec= 36d35'35.29900"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=12h40m51s   !NEXT!

!* --- Scan from 12h40m51s to 12h41m50s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h41m50s   !NEXT!

!* --- Scan from 12h41m50s to 12h46m50s   Tue, 1999 Apr 20 --- *!
sname='BLLAC'  ra=22h02m43.291386s  dec= 42d16'39.97984"  qual=  0  calib='N'
tape=(1,+RUN)  write=(1,on)
stop=12h46m50s   !NEXT!

!* --- Scan from 12h46m50s to 12h47m49s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=12h47m49s   !NEXT!

!* --- Scan from 12h47m49s to 12h50m49s   Tue, 1999 Apr 20 --- *!
sname='J2052+3635'  ra=20h52m52.057400s  dec= 36d35'35.29900"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=12h50m49s   !NEXT!

!* --- Scan from 12h50m49s to 12h51m49s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h51m49s   !NEXT!

!* --- Scan from 12h51m49s to 12h56m48s   Tue, 1999 Apr 20 --- *!
sname='BLLAC'  ra=22h02m43.291386s  dec= 42d16'39.97984"  qual=  0  calib='N'
tape=(1,+RUN)  write=(1,on)
stop=12h56m48s   !NEXT!

!* --- Scan from 12h56m48s to 12h58m49s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,STOP) write=(1,off)
stop=12h58m49s   !NEXT!

!* --- Scan from 12h58m49s to 12h59m48s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=12h59m48s   !NEXT!

!* --- Scan from 12h59m48s to 13h02m48s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h02m48s   !NEXT!

!* --- Scan from 13h02m48s to 13h03m48s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h03m48s   !NEXT!

!* --- Scan from 13h03m48s to 13h04m48s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=13h04m48s   !NEXT!

!* --- Scan from 13h04m48s to 13h07m47s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h07m47s   !NEXT!

!* --- Scan from 13h07m47s to 13h08m47s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h08m47s   !NEXT!

!* --- Scan from 13h08m47s to 13h09m47s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=13h09m47s   !NEXT!

!* --- Scan from 13h09m47s to 13h12m46s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h12m46s   !NEXT!

!* --- Scan from 13h12m46s to 13h13m46s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h13m46s   !NEXT!

!* --- Scan from 13h13m46s to 13h14m46s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=13h14m46s   !NEXT!

!* --- Scan from 13h14m46s to 13h17m45s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h17m45s   !NEXT!

!* --- Scan from 13h17m45s to 13h18m45s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h18m45s   !NEXT!

!* --- Scan from 13h18m45s to 13h19m45s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=13h19m45s   !NEXT!

!* --- Scan from 13h19m45s to 13h22m45s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h22m45s   !NEXT!

!* --- Scan from 13h22m45s to 13h23m44s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h23m44s   !NEXT!

!* --- Scan from 13h23m44s to 13h24m44s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=13h24m44s   !NEXT!

!* --- Scan from 13h24m44s to 13h27m44s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h27m44s   !NEXT!

!* --- Scan from 13h27m44s to 13h28m44s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h28m44s   !NEXT!

!* --- Scan from 13h28m44s to 13h29m43s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=13h29m43s   !NEXT!

!* --- Scan from 13h29m43s to 13h32m43s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h32m43s   !NEXT!

!* --- Scan from 13h32m43s to 13h33m43s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h33m43s   !NEXT!

!* --- Scan from 13h33m43s to 13h34m43s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=13h34m43s   !NEXT!

!* --- Scan from 13h34m43s to 13h37m42s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h37m42s   !NEXT!

!* --- Scan from 13h37m42s to 13h38m42s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h38m42s   !NEXT!

!* --- Scan from 13h38m42s to 13h39m42s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=13h39m42s   !NEXT!

!* --- Scan from 13h39m42s to 13h42m41s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h42m41s   !NEXT!

!* --- Scan from 13h42m41s to 13h43m41s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h43m41s   !NEXT!

!* --- Scan from 13h43m41s to 13h44m41s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=13h44m41s   !NEXT!

!* --- Scan from 13h44m41s to 13h47m41s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h47m41s   !NEXT!

!* --- Scan from 13h47m41s to 13h48m40s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h48m40s   !NEXT!

!* --- Scan from 13h48m40s to 13h49m40s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=13h49m40s   !NEXT!

!* --- Scan from 13h49m40s to 13h52m40s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h52m40s   !NEXT!

!* --- Scan from 13h52m40s to 13h53m40s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h53m40s   !NEXT!

!* --- Scan from 13h53m40s to 13h54m39s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=13h54m39s   !NEXT!

!* --- Scan from 13h54m39s to 13h57m39s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h57m39s   !NEXT!

!* --- Scan from 13h57m39s to 13h58m39s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=13h58m39s   !NEXT!

!* --- Scan from 13h58m39s to 13h59m39s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=13h59m39s   !NEXT!

!* --- Scan from 13h59m39s to 14h02m38s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h02m38s   !NEXT!

!* --- Scan from 14h02m38s to 14h03m38s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h03m38s   !NEXT!

!* --- Scan from 14h03m38s to 14h04m38s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=14h04m38s   !NEXT!

!* --- Scan from 14h04m38s to 14h07m37s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h07m37s   !NEXT!

!* --- Scan from 14h07m37s to 14h08m37s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h08m37s   !NEXT!

!* --- Scan from 14h08m37s to 14h09m37s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=14h09m37s   !NEXT!

!* --- Scan from 14h09m37s to 14h12m36s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h12m36s   !NEXT!

!* --- Scan from 14h12m36s to 14h13m36s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h13m36s   !NEXT!

!* --- Scan from 14h13m36s to 14h14m36s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=14h14m36s   !NEXT!

!* --- Scan from 14h14m36s to 14h17m36s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h17m36s   !NEXT!

!* --- Scan from 14h17m36s to 14h18m35s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h18m35s   !NEXT!

!* --- Scan from 14h18m35s to 14h19m35s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=14h19m35s   !NEXT!

!* --- Scan from 14h19m35s to 14h22m35s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h22m35s   !NEXT!

!* --- Scan from 14h22m35s to 14h23m35s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h23m35s   !NEXT!

!* --- Scan from 14h23m35s to 14h24m34s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=14h24m34s   !NEXT!

!* --- Scan from 14h24m34s to 14h27m34s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h27m34s   !NEXT!

!* --- Scan from 14h27m34s to 14h28m34s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h28m34s   !NEXT!

!* --- Scan from 14h28m34s to 14h29m34s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=14h29m34s   !NEXT!

!* --- Scan from 14h29m34s to 14h32m33s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h32m33s   !NEXT!

!* --- Scan from 14h32m33s to 14h33m33s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h33m33s   !NEXT!

!* --- Scan from 14h33m33s to 14h34m33s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=14h34m33s   !NEXT!

!* --- Scan from 14h34m33s to 14h37m32s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h37m32s   !NEXT!

!* --- Scan from 14h37m32s to 14h38m32s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h38m32s   !NEXT!

!* --- Scan from 14h38m32s to 14h39m32s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=14h39m32s   !NEXT!

!* --- Scan from 14h39m32s to 14h42m32s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h42m32s   !NEXT!

!* --- Scan from 14h42m32s to 14h43m31s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h43m31s   !NEXT!

!* --- Scan from 14h43m31s to 14h44m31s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=14h44m31s   !NEXT!

!* --- Scan from 14h44m31s to 14h47m31s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h47m31s   !NEXT!

!* --- Scan from 14h47m31s to 14h48m31s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h48m31s   !NEXT!

!* --- Scan from 14h48m31s to 14h49m30s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=14h49m30s   !NEXT!

!* --- Scan from 14h49m30s to 14h52m30s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h52m30s   !NEXT!

!* --- Scan from 14h52m30s to 14h53m30s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h53m30s   !NEXT!

!* --- Scan from 14h53m30s to 14h54m30s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=14h54m30s   !NEXT!

!* --- Scan from 14h54m30s to 14h57m29s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h57m29s   !NEXT!

!* --- Scan from 14h57m29s to 14h58m29s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=14h58m29s   !NEXT!

!* --- Scan from 14h58m29s to 14h59m29s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=14h59m29s   !NEXT!

!* --- Scan from 14h59m29s to 15h02m28s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h02m28s   !NEXT!

!* --- Scan from 15h02m28s to 15h03m28s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h03m28s   !NEXT!

!* --- Scan from 15h03m28s to 15h04m28s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=15h04m28s   !NEXT!

!* --- Scan from 15h04m28s to 15h07m27s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h07m27s   !NEXT!

!* --- Scan from 15h07m27s to 15h08m27s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h08m27s   !NEXT!

!* --- Scan from 15h08m27s to 15h09m27s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=15h09m27s   !NEXT!

!* --- Scan from 15h09m27s to 15h12m27s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h12m27s   !NEXT!

!* --- Scan from 15h12m27s to 15h13m26s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h13m26s   !NEXT!

!* --- Scan from 15h13m26s to 15h14m26s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=15h14m26s   !NEXT!

!* --- Scan from 15h14m26s to 15h17m26s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h17m26s   !NEXT!

!* --- Scan from 15h17m26s to 15h18m26s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h18m26s   !NEXT!

!* --- Scan from 15h18m26s to 15h19m25s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=15h19m25s   !NEXT!

!* --- Scan from 15h19m25s to 15h22m25s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h22m25s   !NEXT!

!* --- Scan from 15h22m25s to 15h23m25s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h23m25s   !NEXT!

!* --- Scan from 15h23m25s to 15h24m25s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=15h24m25s   !NEXT!

!* --- Scan from 15h24m25s to 15h27m24s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h27m24s   !NEXT!

!* --- Scan from 15h27m24s to 15h28m24s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h28m24s   !NEXT!

!* --- Scan from 15h28m24s to 15h29m24s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=15h29m24s   !NEXT!

!* --- Scan from 15h29m24s to 15h32m23s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h32m23s   !NEXT!

!* --- Scan from 15h32m23s to 15h33m23s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h33m23s   !NEXT!

!* --- Scan from 15h33m23s to 15h34m23s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=15h34m23s   !NEXT!

!* --- Scan from 15h34m23s to 15h37m23s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h37m23s   !NEXT!

!* --- Scan from 15h37m23s to 15h38m22s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h38m22s   !NEXT!

!* --- Scan from 15h40m23s to 15h41m23s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
tape=(1,STOP) write=(1,off)
stop=15h40m23s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h41m23s   !NEXT!

!* --- Scan from 15h41m23s to 15h44m22s   Tue, 1999 Apr 20 --- *!
sname='J2052+3635'  ra=20h52m52.057400s  dec= 36d35'35.29900"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=15h44m22s   !NEXT!

!* --- Scan from 15h44m22s to 15h45m22s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h45m22s   !NEXT!

!* --- Scan from 15h45m22s to 15h50m21s   Tue, 1999 Apr 20 --- *!
sname='BLLAC'  ra=22h02m43.291386s  dec= 42d16'39.97984"  qual=  0  calib='N'
tape=(1,+RUN)  write=(1,on)
stop=15h50m21s   !NEXT!

!* --- Scan from 15h50m21s to 15h51m21s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=15h51m21s   !NEXT!

!* --- Scan from 15h51m21s to 15h54m21s   Tue, 1999 Apr 20 --- *!
sname='J2052+3635'  ra=20h52m52.057400s  dec= 36d35'35.29900"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=15h54m21s   !NEXT!

!* --- Scan from 15h54m21s to 15h55m21s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=15h55m21s   !NEXT!

!* --- Scan from 15h55m21s to 16h00m20s   Tue, 1999 Apr 20 --- *!
sname='BLLAC'  ra=22h02m43.291386s  dec= 42d16'39.97984"  qual=  0  calib='N'
tape=(1,+RUN)  write=(1,on)
stop=16h00m20s   !NEXT!

!* --- Scan from 16h00m20s to 16h02m20s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,STOP) write=(1,off)
stop=16h02m20s   !NEXT!

!* --- Scan from 16h02m20s to 16h03m20s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h03m20s   !NEXT!

!* --- Scan from 16h03m20s to 16h06m20s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h06m20s   !NEXT!

!* --- Scan from 16h06m20s to 16h07m20s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h07m20s   !NEXT!

!* --- Scan from 16h07m20s to 16h08m19s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=16h08m19s   !NEXT!

!* --- Scan from 16h08m19s to 16h11m19s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h11m19s   !NEXT!

!* --- Scan from 16h11m19s to 16h12m19s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h12m19s   !NEXT!

!* --- Scan from 16h12m19s to 16h13m19s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=16h13m19s   !NEXT!

!* --- Scan from 16h13m19s to 16h16m18s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h16m18s   !NEXT!

!* --- Scan from 16h16m18s to 16h17m18s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h17m18s   !NEXT!

!* --- Scan from 16h17m18s to 16h18m18s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=16h18m18s   !NEXT!

!* --- Scan from 16h18m18s to 16h21m17s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h21m17s   !NEXT!

!* --- Scan from 16h21m17s to 16h22m17s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h22m17s   !NEXT!

!* --- Scan from 16h22m17s to 16h23m17s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=16h23m17s   !NEXT!

!* --- Scan from 16h23m17s to 16h26m16s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h26m16s   !NEXT!

!* --- Scan from 16h26m16s to 16h27m16s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h27m16s   !NEXT!

!* --- Scan from 16h27m16s to 16h28m16s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=16h28m16s   !NEXT!

!* --- Scan from 16h28m16s to 16h31m16s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h31m16s   !NEXT!

!* --- Scan from 16h31m16s to 16h32m16s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h32m16s   !NEXT!

!* --- Scan from 16h32m16s to 16h33m15s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=16h33m15s   !NEXT!

!* --- Scan from 16h33m15s to 16h36m15s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h36m15s   !NEXT!

!* --- Scan from 16h36m15s to 16h37m15s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h37m15s   !NEXT!

!* --- Scan from 16h37m15s to 16h38m15s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=16h38m15s   !NEXT!

!* --- Scan from 16h38m15s to 16h41m14s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h41m14s   !NEXT!

!* --- Scan from 16h41m14s to 16h42m14s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h42m14s   !NEXT!

!* --- Scan from 16h42m14s to 16h43m14s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=16h43m14s   !NEXT!

!* --- Scan from 16h43m14s to 16h46m13s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h46m13s   !NEXT!

!* --- Scan from 16h46m13s to 16h47m13s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h47m13s   !NEXT!

!* --- Scan from 16h47m13s to 16h48m13s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=16h48m13s   !NEXT!

!* --- Scan from 16h48m13s to 16h51m12s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h51m12s   !NEXT!

!* --- Scan from 16h51m12s to 16h52m12s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h52m12s   !NEXT!

!* --- Scan from 16h52m12s to 16h53m12s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=16h53m12s   !NEXT!

!* --- Scan from 16h53m12s to 16h56m12s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h56m12s   !NEXT!

!* --- Scan from 16h56m12s to 16h57m11s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=16h57m11s   !NEXT!

!* --- Scan from 16h57m11s to 16h58m11s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=16h58m11s   !NEXT!

!* --- Scan from 16h58m11s to 17h01m11s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h01m11s   !NEXT!

!* --- Scan from 17h01m11s to 17h02m11s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h02m11s   !NEXT!

!* --- Scan from 17h02m11s to 17h03m10s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=17h03m10s   !NEXT!

!* --- Scan from 17h03m10s to 17h06m10s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h06m10s   !NEXT!

!* --- Scan from 17h06m10s to 17h07m10s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h07m10s   !NEXT!

!* --- Scan from 17h07m10s to 17h08m10s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=17h08m10s   !NEXT!

!* --- Scan from 17h08m10s to 17h11m09s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h11m09s   !NEXT!

!* --- Scan from 17h11m09s to 17h12m09s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h12m09s   !NEXT!

!* --- Scan from 17h12m09s to 17h13m09s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=17h13m09s   !NEXT!

!* --- Scan from 17h13m09s to 17h16m08s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h16m08s   !NEXT!

!* --- Scan from 17h16m08s to 17h17m08s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h17m08s   !NEXT!

!* --- Scan from 17h17m08s to 17h18m08s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=17h18m08s   !NEXT!

!* --- Scan from 17h18m08s to 17h21m07s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h21m07s   !NEXT!

!* --- Scan from 17h21m07s to 17h22m07s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h22m07s   !NEXT!

!* --- Scan from 17h22m07s to 17h23m07s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=17h23m07s   !NEXT!

!* --- Scan from 17h23m07s to 17h26m07s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h26m07s   !NEXT!

!* --- Scan from 17h26m07s to 17h27m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h27m06s   !NEXT!

!* --- Scan from 17h27m06s to 17h28m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=17h28m06s   !NEXT!

!* --- Scan from 17h28m06s to 17h31m06s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h31m06s   !NEXT!

!* --- Scan from 17h31m06s to 17h32m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h32m06s   !NEXT!

!* --- Scan from 17h32m06s to 17h33m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=17h33m06s   !NEXT!

!* --- Scan from 17h33m06s to 17h36m05s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h36m05s   !NEXT!

!* --- Scan from 17h36m05s to 17h37m05s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h37m05s   !NEXT!

!* --- Scan from 17h37m05s to 17h38m05s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=17h38m05s   !NEXT!

!* --- Scan from 17h38m05s to 17h41m04s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h41m04s   !NEXT!

!* --- Scan from 17h41m04s to 17h42m04s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h42m04s   !NEXT!

!* --- Scan from 17h42m04s to 17h43m04s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=17h43m04s   !NEXT!

!* --- Scan from 17h43m04s to 17h46m03s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h46m03s   !NEXT!

!* --- Scan from 17h46m03s to 17h47m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h47m03s   !NEXT!

!* --- Scan from 17h47m03s to 17h48m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=17h48m03s   !NEXT!

!* --- Scan from 17h48m03s to 17h51m03s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h51m03s   !NEXT!

!* --- Scan from 17h51m03s to 17h52m02s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h52m02s   !NEXT!

!* --- Scan from 17h52m02s to 17h53m02s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=17h53m02s   !NEXT!

!* --- Scan from 17h53m02s to 17h56m02s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h56m02s   !NEXT!

!* --- Scan from 17h56m02s to 17h57m02s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=17h57m02s   !NEXT!

!* --- Scan from 17h57m02s to 17h58m01s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=17h58m01s   !NEXT!

!* --- Scan from 17h58m01s to 18h01m01s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h01m01s   !NEXT!

!* --- Scan from 18h01m01s to 18h02m01s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h02m01s   !NEXT!

!* --- Scan from 18h02m01s to 18h03m01s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=18h03m01s   !NEXT!

!* --- Scan from 18h03m01s to 18h06m00s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h06m00s   !NEXT!

!* --- Scan from 18h06m00s to 18h07m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h07m00s   !NEXT!

!* --- Scan from 18h07m00s to 18h08m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=18h08m00s   !NEXT!

!* --- Scan from 18h08m00s to 18h10m59s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h10m59s   !NEXT!

!* --- Scan from 18h10m59s to 18h11m59s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h11m59s   !NEXT!

!* --- Scan from 18h11m59s to 18h12m59s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=18h12m59s   !NEXT!

!* --- Scan from 18h12m59s to 18h15m58s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h15m58s   !NEXT!

!* --- Scan from 18h15m58s to 18h16m58s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h16m58s   !NEXT!

!* --- Scan from 18h16m58s to 18h17m58s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=18h17m58s   !NEXT!

!* --- Scan from 18h17m58s to 18h20m58s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h20m58s   !NEXT!

!* --- Scan from 18h20m58s to 18h21m57s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h21m57s   !NEXT!

!* --- Scan from 18h21m57s to 18h22m57s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=18h22m57s   !NEXT!

!* --- Scan from 18h22m57s to 18h25m57s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h25m57s   !NEXT!

!* --- Scan from 18h25m57s to 18h26m57s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h26m57s   !NEXT!

!* --- Scan from 18h26m57s to 18h27m57s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=18h27m57s   !NEXT!

!* --- Scan from 18h27m57s to 18h30m56s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h30m56s   !NEXT!

!* --- Scan from 18h30m56s to 18h31m56s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h31m56s   !NEXT!

!* --- Scan from 18h31m56s to 18h32m56s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
tape=(1,+RUN)  write=(1,on)
stop=18h32m56s   !NEXT!

!* --- Scan from 18h32m56s to 18h35m55s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h35m55s   !NEXT!

!* --- Scan from 18h35m55s to 18h36m55s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h36m55s   !NEXT!

!* --- Scan from 18h36m55s to 18h37m55s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,740.51),( 2,740.51),( 3,732.51),( 4,732.51)
tape=(1,+RUN)  write=(1,on)
stop=18h37m55s   !NEXT!

!* --- Scan from 18h37m55s to 18h40m54s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h40m54s   !NEXT!

!* --- Scan from 18h40m54s to 18h41m54s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=18h41m54s   !NEXT!

tape=(1,STOP)    write=(1,off)   dur=0s 
stop=18h41m59s   !NEXT!
     !QUIT! 
