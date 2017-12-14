!*  Schedule for VLBA_SC   *!
!*  Experiment eglst    *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:  11.50 *!
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
!*  Start at 09h50m18s     Tue, 1999 Apr 20  Day of year  110   *!
program=eglst   

diskformat=mark5c
media=(1,disk)

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 09h50m18s to 09h52m17s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
maxcaltime= 120
fe=(2,4cm),(4,4cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
logging=STANDARD
nchan= 4
format=VLBA1:2
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S2),(7,S3),(8,S4)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,M1),(6,M2),(7,M3),(8,M4)
pcalxfreq1=(1,250),(2,250),(3,6250),(4,6250),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,250),(2,250),(3,6250),(4,6250),(5,0),(6,0),(7,0),(8,0)
samplerate=16M
disk=off
  date = 1999Apr20
stop=09h50m18s   !NEXT!        
qual=  0
disk=off
stop=09h52m17s   !NEXT!

!* --- Scan from 09h52m23s to 09h53m23s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=09h52m23s   !NEXT!        
qual=  0
disk=off
stop=09h53m23s   !NEXT!

!* --- Scan from 09h53m34s to 09h56m34s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=09h53m34s   !NEXT!        
qual=  0
disk=off
stop=09h56m34s   !NEXT!

!* --- Scan from 09h56m45s to 09h57m45s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=09h56m45s   !NEXT!        
qual=  0
disk=off
stop=09h57m45s   !NEXT!

!* --- Scan from 09h57m51s to 09h58m51s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=09h57m51s   !NEXT!        
qual=  0
disk=off
stop=09h58m51s   !NEXT!

!* --- Scan from 09h59m03s to 10h02m02s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=09h59m03s   !NEXT!        
qual=  0
disk=off
stop=10h02m02s   !NEXT!

!* --- Scan from 10h02m14s to 10h03m13s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=10h02m14s   !NEXT!        
qual=  0
disk=off
stop=10h03m13s   !NEXT!

!* --- Scan from 10h03m19s to 10h04m19s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=10h03m19s   !NEXT!        
qual=  0
disk=off
stop=10h04m19s   !NEXT!

!* --- Scan from 10h04m31s to 10h07m30s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=10h04m31s   !NEXT!        
qual=  0
disk=off
stop=10h07m30s   !NEXT!

!* --- Scan from 10h07m42s to 10h08m42s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=10h07m42s   !NEXT!        
qual=  0
disk=off
stop=10h08m42s   !NEXT!

!* --- Scan from 10h08m48s to 10h09m48s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=10h08m48s   !NEXT!        
qual=  0
disk=off
stop=10h09m48s   !NEXT!

!* --- Scan from 10h09m59s to 10h12m59s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=10h09m59s   !NEXT!        
qual=  0
disk=off
stop=10h12m59s   !NEXT!

!* --- Scan from 10h13m10s to 10h14m10s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=10h13m10s   !NEXT!        
qual=  0
disk=off
stop=10h14m10s   !NEXT!

!* --- Scan from 10h14m16s to 10h15m16s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=10h14m16s   !NEXT!        
qual=  0
disk=off
stop=10h15m16s   !NEXT!

!* --- Scan from 10h15m27s to 10h18m27s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=10h15m27s   !NEXT!        
qual=  0
disk=off
stop=10h18m27s   !NEXT!

!* --- Scan from 10h18m38s to 10h19m38s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=10h18m38s   !NEXT!        
qual=  0
disk=off
stop=10h19m38s   !NEXT!

!* --- Scan from 10h19m44s to 10h20m44s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=10h19m44s   !NEXT!        
qual=  0
disk=off
stop=10h20m44s   !NEXT!

!* --- Scan from 10h20m56s to 10h23m55s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=10h20m56s   !NEXT!        
qual=  0
disk=off
stop=10h23m55s   !NEXT!

!* --- Scan from 10h24m07s to 10h25m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=10h24m07s   !NEXT!        
qual=  0
disk=off
stop=10h25m06s   !NEXT!

!* --- Scan from 10h25m12s to 10h26m12s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=10h25m12s   !NEXT!        
qual=  0
disk=off
stop=10h26m12s   !NEXT!

!* --- Scan from 10h26m24s to 10h29m23s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=10h26m24s   !NEXT!        
qual=  0
disk=off
stop=10h29m23s   !NEXT!

!* --- Scan from 10h29m35s to 10h30m35s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=10h29m35s   !NEXT!        
qual=  0
disk=off
stop=10h30m35s   !NEXT!

!* --- Scan from 10h30m41s to 10h31m41s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=10h30m41s   !NEXT!        
qual=  0
disk=off
stop=10h31m41s   !NEXT!

!* --- Scan from 10h31m52s to 10h34m52s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=10h31m52s   !NEXT!        
qual=  0
disk=off
stop=10h34m52s   !NEXT!

!* --- Scan from 10h35m03s to 10h36m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=10h35m03s   !NEXT!        
qual=  0
disk=off
stop=10h36m03s   !NEXT!

!* --- Scan from 10h36m09s to 10h37m09s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=10h36m09s   !NEXT!        
qual=  0
disk=off
stop=10h37m09s   !NEXT!

!* --- Scan from 10h37m20s to 10h40m20s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=10h37m20s   !NEXT!        
qual=  0
disk=off
stop=10h40m20s   !NEXT!

!* --- Scan from 10h40m32s to 10h41m32s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=10h40m32s   !NEXT!        
qual=  0
disk=off
stop=10h41m32s   !NEXT!

!* --- Scan from 10h41m38s to 10h42m37s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=10h41m38s   !NEXT!        
qual=  0
disk=off
stop=10h42m37s   !NEXT!

!* --- Scan from 10h42m49s to 10h45m49s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=10h42m49s   !NEXT!        
qual=  0
disk=off
stop=10h45m49s   !NEXT!

!* --- Scan from 10h46m01s to 10h47m01s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=10h46m01s   !NEXT!        
qual=  0
disk=off
stop=10h47m01s   !NEXT!

!* --- Scan from 10h47m07s to 10h48m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=10h47m07s   !NEXT!        
qual=  0
disk=off
stop=10h48m06s   !NEXT!

!* --- Scan from 10h48m18s to 10h51m18s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=10h48m18s   !NEXT!        
qual=  0
disk=off
stop=10h51m18s   !NEXT!

!* --- Scan from 10h51m30s to 10h52m30s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=10h51m30s   !NEXT!        
qual=  0
disk=off
stop=10h52m30s   !NEXT!

!* --- Scan from 10h52m36s to 10h53m36s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=10h52m36s   !NEXT!        
qual=  0
disk=off
stop=10h53m36s   !NEXT!

!* --- Scan from 10h53m48s to 10h56m48s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=10h53m48s   !NEXT!        
qual=  0
disk=off
stop=10h56m48s   !NEXT!

!* --- Scan from 10h57m00s to 10h58m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=10h57m00s   !NEXT!        
qual=  0
disk=off
stop=10h58m00s   !NEXT!

!* --- Scan from 10h58m06s to 10h59m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=10h58m06s   !NEXT!        
qual=  0
disk=off
stop=10h59m06s   !NEXT!

!* --- Scan from 10h59m19s to 11h02m18s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=10h59m19s   !NEXT!        
qual=  0
disk=off
stop=11h02m18s   !NEXT!

!* --- Scan from 11h02m31s to 11h03m31s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=11h02m31s   !NEXT!        
qual=  0
disk=off
stop=11h03m31s   !NEXT!

!* --- Scan from 11h03m37s to 11h04m37s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=11h03m37s   !NEXT!        
qual=  0
disk=off
stop=11h04m37s   !NEXT!

!* --- Scan from 11h04m50s to 11h07m49s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=11h04m50s   !NEXT!        
qual=  0
disk=off
stop=11h07m49s   !NEXT!

!* --- Scan from 11h08m02s to 11h09m02s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=11h08m02s   !NEXT!        
qual=  0
disk=off
stop=11h09m02s   !NEXT!

!* --- Scan from 11h09m08s to 11h10m08s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=11h09m08s   !NEXT!        
qual=  0
disk=off
stop=11h10m08s   !NEXT!

!* --- Scan from 11h10m21s to 11h13m21s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=11h10m21s   !NEXT!        
qual=  0
disk=off
stop=11h13m21s   !NEXT!

!* --- Scan from 11h13m35s to 11h14m34s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=11h13m35s   !NEXT!        
qual=  0
disk=off
stop=11h14m34s   !NEXT!

!* --- Scan from 11h14m40s to 11h15m40s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=11h14m40s   !NEXT!        
qual=  0
disk=off
stop=11h15m40s   !NEXT!

!* --- Scan from 11h15m54s to 11h18m54s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=11h15m54s   !NEXT!        
qual=  0
disk=off
stop=11h18m54s   !NEXT!

!* --- Scan from 11h19m08s to 11h20m08s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=11h19m08s   !NEXT!        
qual=  0
disk=off
stop=11h20m08s   !NEXT!

!* --- Scan from 11h20m14s to 11h21m13s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=11h20m14s   !NEXT!        
qual=  0
disk=off
stop=11h21m13s   !NEXT!

!* --- Scan from 11h21m28s to 11h24m27s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=11h21m28s   !NEXT!        
qual=  0
disk=off
stop=11h24m27s   !NEXT!

!* --- Scan from 11h24m42s to 11h25m42s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=11h24m42s   !NEXT!        
qual=  0
disk=off
stop=11h25m42s   !NEXT!

!* --- Scan from 11h25m48s to 11h26m48s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=11h25m48s   !NEXT!        
qual=  0
disk=off
stop=11h26m48s   !NEXT!

!* --- Scan from 11h27m02s to 11h30m02s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=11h27m02s   !NEXT!        
qual=  0
disk=off
stop=11h30m02s   !NEXT!

!* --- Scan from 11h30m17s to 11h31m17s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=11h30m17s   !NEXT!        
qual=  0
disk=off
stop=11h31m17s   !NEXT!

!* --- Scan from 11h31m23s to 11h32m23s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=11h31m23s   !NEXT!        
qual=  0
disk=off
stop=11h32m23s   !NEXT!

!* --- Scan from 11h32m38s to 11h35m38s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=11h32m38s   !NEXT!        
qual=  0
disk=off
stop=11h35m38s   !NEXT!

!* --- Scan from 11h35m54s to 11h36m53s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=11h35m54s   !NEXT!        
qual=  0
disk=off
stop=11h36m53s   !NEXT!

!* --- Scan from 11h36m59s to 11h37m59s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=11h36m59s   !NEXT!        
qual=  0
disk=off
stop=11h37m59s   !NEXT!

!* --- Scan from 11h38m15s to 11h41m15s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=11h38m15s   !NEXT!        
qual=  0
disk=off
stop=11h41m15s   !NEXT!

!* --- Scan from 11h41m31s to 11h42m31s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=11h41m31s   !NEXT!        
qual=  0
disk=off
stop=11h42m31s   !NEXT!

!* --- Scan from 11h42m37s to 11h43m37s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=11h42m37s   !NEXT!        
qual=  0
disk=off
stop=11h43m37s   !NEXT!

!* --- Scan from 11h43m53s to 11h46m53s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=11h43m53s   !NEXT!        
qual=  0
disk=off
stop=11h46m53s   !NEXT!

!* --- Scan from 11h47m10s to 11h48m09s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=11h47m10s   !NEXT!        
qual=  0
disk=off
stop=11h48m09s   !NEXT!

!* --- Scan from 11h48m15s to 11h49m15s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=11h48m15s   !NEXT!        
qual=  0
disk=off
stop=11h49m15s   !NEXT!

!* --- Scan from 11h49m32s to 11h52m32s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=11h49m32s   !NEXT!        
qual=  0
disk=off
stop=11h52m32s   !NEXT!

!* --- Scan from 11h52m49s to 11h53m49s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=11h52m49s   !NEXT!        
qual=  0
disk=off
stop=11h53m49s   !NEXT!

!* --- Scan from 11h53m55s to 11h54m54s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=11h53m55s   !NEXT!        
qual=  0
disk=off
stop=11h54m54s   !NEXT!

!* --- Scan from 11h55m11s to 11h58m11s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=11h55m11s   !NEXT!        
qual=  0
disk=off
stop=11h58m11s   !NEXT!

!* --- Scan from 11h58m28s to 11h59m28s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=11h58m28s   !NEXT!        
qual=  0
disk=off
stop=11h59m28s   !NEXT!

!* --- Scan from 11h59m34s to 12h00m33s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=11h59m34s   !NEXT!        
qual=  0
disk=off
stop=12h00m33s   !NEXT!

!* --- Scan from 12h00m50s to 12h03m49s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=12h00m50s   !NEXT!        
qual=  0
disk=off
stop=12h03m49s   !NEXT!

!* --- Scan from 12h04m06s to 12h05m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=12h04m06s   !NEXT!        
qual=  0
disk=off
stop=12h05m06s   !NEXT!

!* --- Scan from 12h05m12s to 12h06m12s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=12h05m12s   !NEXT!        
qual=  0
disk=off
stop=12h06m12s   !NEXT!

!* --- Scan from 12h06m27s to 12h09m27s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=12h06m27s   !NEXT!        
qual=  0
disk=off
stop=12h09m27s   !NEXT!

!* --- Scan from 12h09m42s to 12h10m42s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=12h09m42s   !NEXT!        
qual=  0
disk=off
stop=12h10m42s   !NEXT!

!* --- Scan from 12h10m48s to 12h11m48s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=12h10m48s   !NEXT!        
qual=  0
disk=off
stop=12h11m48s   !NEXT!

!* --- Scan from 12h12m02s to 12h15m02s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=12h12m02s   !NEXT!        
qual=  0
disk=off
stop=12h15m02s   !NEXT!

!* --- Scan from 12h15m16s to 12h16m16s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=12h15m16s   !NEXT!        
qual=  0
disk=off
stop=12h16m16s   !NEXT!

!* --- Scan from 12h16m22s to 12h17m22s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=12h16m22s   !NEXT!        
qual=  0
disk=off
stop=12h17m22s   !NEXT!

!* --- Scan from 12h17m35s to 12h20m34s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=12h17m35s   !NEXT!        
qual=  0
disk=off
stop=12h20m34s   !NEXT!

!* --- Scan from 12h20m47s to 12h21m47s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=12h20m47s   !NEXT!        
qual=  0
disk=off
stop=12h21m47s   !NEXT!

!* --- Scan from 12h21m53s to 12h22m53s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=12h21m53s   !NEXT!        
qual=  0
disk=off
stop=12h22m53s   !NEXT!

!* --- Scan from 12h23m06s to 12h26m05s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=12h23m06s   !NEXT!        
qual=  0
disk=off
stop=12h26m05s   !NEXT!

!* --- Scan from 12h26m19s to 12h27m19s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=12h26m19s   !NEXT!        
qual=  0
disk=off
stop=12h27m19s   !NEXT!

!* --- Scan from 12h27m25s to 12h28m24s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=12h27m25s   !NEXT!        
qual=  0
disk=off
stop=12h28m24s   !NEXT!

!* --- Scan from 12h28m38s to 12h31m37s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=12h28m38s   !NEXT!        
qual=  0
disk=off
stop=12h31m37s   !NEXT!

!* --- Scan from 12h31m51s to 12h32m51s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=12h31m51s   !NEXT!        
qual=  0
disk=off
stop=12h32m51s   !NEXT!

!* --- Scan from 12h32m57s to 12h33m57s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=12h32m57s   !NEXT!        
qual=  0
disk=off
stop=12h33m57s   !NEXT!

!* --- Scan from 12h34m11s to 12h37m10s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=12h34m11s   !NEXT!        
qual=  0
disk=off
stop=12h37m10s   !NEXT!

!* --- Scan from 12h37m25s to 12h38m24s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=12h37m25s   !NEXT!        
qual=  0
disk=off
stop=12h38m24s   !NEXT!

!* --- Scan from 12h38m30s to 12h39m30s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=12h38m30s   !NEXT!        
qual=  0
disk=off
stop=12h39m30s   !NEXT!

!* --- Scan from 12h39m45s to 12h42m44s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=12h39m45s   !NEXT!        
qual=  0
disk=off
stop=12h42m44s   !NEXT!

!* --- Scan from 12h42m59s to 12h43m59s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=12h42m59s   !NEXT!        
qual=  0
disk=off
stop=12h43m59s   !NEXT!

!* --- Scan from 12h44m05s to 12h45m05s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=12h44m05s   !NEXT!        
qual=  0
disk=off
stop=12h45m05s   !NEXT!

!* --- Scan from 12h45m20s to 12h48m20s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=12h45m20s   !NEXT!        
qual=  0
disk=off
stop=12h48m20s   !NEXT!

!* --- Scan from 12h48m35s to 12h49m35s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=12h48m35s   !NEXT!        
qual=  0
disk=off
stop=12h49m35s   !NEXT!

!* --- Scan from 12h49m41s to 12h50m41s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=12h49m41s   !NEXT!        
qual=  0
disk=off
stop=12h50m41s   !NEXT!

!* --- Scan from 12h51m01s to 12h54m00s   Tue, 1999 Apr 20 --- *!
sname='J2052+3635'  ra=20h52m52.057400s  dec= 36d35'35.29900"  qual=999  calib='M'
disk=off
stop=12h51m01s   !NEXT!        
qual=  0
disk=off
stop=12h54m00s   !NEXT!

!* --- Scan from 12h54m22s to 12h55m22s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=12h54m22s   !NEXT!        
qual=  0
disk=off
stop=12h55m22s   !NEXT!

!* --- Scan from 12h56m27s to 13h01m26s   Tue, 1999 Apr 20 --- *!
sname='BLLAC'  ra=22h02m43.291386s  dec= 42d16'39.97984"  qual=999  calib='N'
disk=off
stop=12h56m27s   !NEXT!        
qual=  0
disk=off
stop=13h01m26s   !NEXT!

!* --- Scan from 13h02m20s to 13h03m20s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=13h02m20s   !NEXT!        
qual=  0
disk=off
stop=13h03m20s   !NEXT!

!* --- Scan from 13h03m48s to 13h06m47s   Tue, 1999 Apr 20 --- *!
sname='J2052+3635'  ra=20h52m52.057400s  dec= 36d35'35.29900"  qual=999  calib='M'
disk=off
stop=13h03m48s   !NEXT!        
qual=  0
disk=off
stop=13h06m47s   !NEXT!

!* --- Scan from 13h07m16s to 13h08m16s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=13h07m16s   !NEXT!        
qual=  0
disk=off
stop=13h08m16s   !NEXT!

!* --- Scan from 13h09m16s to 13h14m15s   Tue, 1999 Apr 20 --- *!
sname='BLLAC'  ra=22h02m43.291386s  dec= 42d16'39.97984"  qual=999  calib='N'
disk=off
stop=13h09m16s   !NEXT!        
qual=  0
disk=off
stop=13h14m15s   !NEXT!

!* --- Scan from 13h15m24s to 13h16m23s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=13h15m24s   !NEXT!        
qual=  0
disk=off
stop=13h16m23s   !NEXT!

!* --- Scan from 13h16m42s to 13h19m41s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=13h16m42s   !NEXT!        
qual=  0
disk=off
stop=13h19m41s   !NEXT!

!* --- Scan from 13h20m00s to 13h21m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=13h20m00s   !NEXT!        
qual=  0
disk=off
stop=13h21m00s   !NEXT!

!* --- Scan from 13h21m06s to 13h22m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=13h21m06s   !NEXT!        
qual=  0
disk=off
stop=13h22m06s   !NEXT!

!* --- Scan from 13h22m24s to 13h25m23s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=13h22m24s   !NEXT!        
qual=  0
disk=off
stop=13h25m23s   !NEXT!

!* --- Scan from 13h25m41s to 13h26m41s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=13h25m41s   !NEXT!        
qual=  0
disk=off
stop=13h26m41s   !NEXT!

!* --- Scan from 13h26m47s to 13h27m46s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=13h26m47s   !NEXT!        
qual=  0
disk=off
stop=13h27m46s   !NEXT!

!* --- Scan from 13h28m03s to 13h31m02s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=13h28m03s   !NEXT!        
qual=  0
disk=off
stop=13h31m02s   !NEXT!

!* --- Scan from 13h31m18s to 13h32m18s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=13h31m18s   !NEXT!        
qual=  0
disk=off
stop=13h32m18s   !NEXT!

!* --- Scan from 13h32m24s to 13h33m23s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=13h32m24s   !NEXT!        
qual=  0
disk=off
stop=13h33m23s   !NEXT!

!* --- Scan from 13h33m38s to 13h36m37s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=13h33m38s   !NEXT!        
qual=  0
disk=off
stop=13h36m37s   !NEXT!

!* --- Scan from 13h36m51s to 13h37m50s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=13h36m51s   !NEXT!        
qual=  0
disk=off
stop=13h37m50s   !NEXT!

!* --- Scan from 13h37m56s to 13h38m56s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=13h37m56s   !NEXT!        
qual=  0
disk=off
stop=13h38m56s   !NEXT!

!* --- Scan from 13h39m10s to 13h42m09s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=13h39m10s   !NEXT!        
qual=  0
disk=off
stop=13h42m09s   !NEXT!

!* --- Scan from 13h42m23s to 13h43m23s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=13h42m23s   !NEXT!        
qual=  0
disk=off
stop=13h43m23s   !NEXT!

!* --- Scan from 13h43m29s to 13h44m29s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=13h43m29s   !NEXT!        
qual=  0
disk=off
stop=13h44m29s   !NEXT!

!* --- Scan from 13h44m43s to 13h47m42s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=13h44m43s   !NEXT!        
qual=  0
disk=off
stop=13h47m42s   !NEXT!

!* --- Scan from 13h47m57s to 13h48m57s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=13h47m57s   !NEXT!        
qual=  0
disk=off
stop=13h48m57s   !NEXT!

!* --- Scan from 13h49m03s to 13h50m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=13h49m03s   !NEXT!        
qual=  0
disk=off
stop=13h50m03s   !NEXT!

!* --- Scan from 13h50m18s to 13h53m17s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=13h50m18s   !NEXT!        
qual=  0
disk=off
stop=13h53m17s   !NEXT!

!* --- Scan from 13h53m33s to 13h54m33s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=13h53m33s   !NEXT!        
qual=  0
disk=off
stop=13h54m33s   !NEXT!

!* --- Scan from 13h54m39s to 13h55m39s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=13h54m39s   !NEXT!        
qual=  0
disk=off
stop=13h55m39s   !NEXT!

!* --- Scan from 13h55m55s to 13h58m55s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=13h55m55s   !NEXT!        
qual=  0
disk=off
stop=13h58m55s   !NEXT!

!* --- Scan from 13h59m12s to 14h00m12s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=13h59m12s   !NEXT!        
qual=  0
disk=off
stop=14h00m12s   !NEXT!

!* --- Scan from 14h00m18s to 14h01m18s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=14h00m18s   !NEXT!        
qual=  0
disk=off
stop=14h01m18s   !NEXT!

!* --- Scan from 14h01m36s to 14h04m35s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=14h01m36s   !NEXT!        
qual=  0
disk=off
stop=14h04m35s   !NEXT!

!* --- Scan from 14h04m55s to 14h05m55s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=14h04m55s   !NEXT!        
qual=  0
disk=off
stop=14h05m55s   !NEXT!

!* --- Scan from 14h06m01s to 14h07m01s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=14h06m01s   !NEXT!        
qual=  0
disk=off
stop=14h07m01s   !NEXT!

!* --- Scan from 14h07m22s to 14h10m22s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=14h07m22s   !NEXT!        
qual=  0
disk=off
stop=14h10m22s   !NEXT!

!* --- Scan from 14h10m46s to 14h11m45s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=14h10m46s   !NEXT!        
qual=  0
disk=off
stop=14h11m45s   !NEXT!

!* --- Scan from 14h11m51s to 14h12m51s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=14h11m51s   !NEXT!        
qual=  0
disk=off
stop=14h12m51s   !NEXT!

!* --- Scan from 14h13m18s to 14h16m18s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=14h13m18s   !NEXT!        
qual=  0
disk=off
stop=14h16m18s   !NEXT!

!* --- Scan from 14h16m53s to 14h17m53s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=14h16m53s   !NEXT!        
qual=  0
disk=off
stop=14h17m53s   !NEXT!

!* --- Scan from 14h17m59s to 14h18m59s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=14h17m59s   !NEXT!        
qual=  0
disk=off
stop=14h18m59s   !NEXT!

!* --- Scan from 14h19m47s to 14h22m47s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=14h19m47s   !NEXT!        
qual=  0
disk=off
stop=14h22m47s   !NEXT!

!* --- Scan from 14h25m26s to 14h26m26s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=14h25m26s   !NEXT!        
qual=  0
disk=off
stop=14h26m26s   !NEXT!

!* --- Scan from 14h26m32s to 14h27m32s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=14h26m32s   !NEXT!        
qual=  0
disk=off
stop=14h27m32s   !NEXT!

!* --- Scan from 14h28m44s to 14h31m43s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=14h28m44s   !NEXT!        
qual=  0
disk=off
stop=14h31m43s   !NEXT!

!* --- Scan from 14h32m32s to 14h33m31s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=14h32m32s   !NEXT!        
qual=  0
disk=off
stop=14h33m31s   !NEXT!

!* --- Scan from 14h33m37s to 14h34m37s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=14h33m37s   !NEXT!        
qual=  0
disk=off
stop=14h34m37s   !NEXT!

!* --- Scan from 14h35m16s to 14h38m15s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=14h35m16s   !NEXT!        
qual=  0
disk=off
stop=14h38m15s   !NEXT!

!* --- Scan from 14h38m55s to 14h39m55s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=14h38m55s   !NEXT!        
qual=  0
disk=off
stop=14h39m55s   !NEXT!

!* --- Scan from 14h40m01s to 14h41m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=14h40m01s   !NEXT!        
qual=  0
disk=off
stop=14h41m00s   !NEXT!

!* --- Scan from 14h41m34s to 14h44m33s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=14h41m34s   !NEXT!        
qual=  0
disk=off
stop=14h44m33s   !NEXT!

!* --- Scan from 14h45m04s to 14h46m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=14h45m04s   !NEXT!        
qual=  0
disk=off
stop=14h46m03s   !NEXT!

!* --- Scan from 14h46m09s to 14h47m09s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=14h46m09s   !NEXT!        
qual=  0
disk=off
stop=14h47m09s   !NEXT!

!* --- Scan from 14h47m36s to 14h50m35s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=14h47m36s   !NEXT!        
qual=  0
disk=off
stop=14h50m35s   !NEXT!

!* --- Scan from 14h50m59s to 14h51m59s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=14h50m59s   !NEXT!        
qual=  0
disk=off
stop=14h51m59s   !NEXT!

!* --- Scan from 14h52m05s to 14h53m05s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=14h52m05s   !NEXT!        
qual=  0
disk=off
stop=14h53m05s   !NEXT!

!* --- Scan from 14h53m28s to 14h56m27s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=14h53m28s   !NEXT!        
qual=  0
disk=off
stop=14h56m27s   !NEXT!

!* --- Scan from 14h56m54s to 14h57m54s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=14h56m54s   !NEXT!        
qual=  0
disk=off
stop=14h57m54s   !NEXT!

!* --- Scan from 14h58m00s to 14h59m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=14h58m00s   !NEXT!        
qual=  0
disk=off
stop=14h59m00s   !NEXT!

!* --- Scan from 14h59m29s to 15h02m29s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=14h59m29s   !NEXT!        
qual=  0
disk=off
stop=15h02m29s   !NEXT!

!* --- Scan from 15h03m07s to 15h04m07s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=15h03m07s   !NEXT!        
qual=  0
disk=off
stop=15h04m07s   !NEXT!

!* --- Scan from 15h04m13s to 15h05m13s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=15h04m13s   !NEXT!        
qual=  0
disk=off
stop=15h05m13s   !NEXT!

!* --- Scan from 15h05m56s to 15h08m55s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=15h05m56s   !NEXT!        
qual=  0
disk=off
stop=15h08m55s   !NEXT!

!* --- Scan from 15h09m54s to 15h10m54s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=15h09m54s   !NEXT!        
qual=  0
disk=off
stop=15h10m54s   !NEXT!

!* --- Scan from 15h11m00s to 15h12m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=15h11m00s   !NEXT!        
qual=  0
disk=off
stop=15h12m00s   !NEXT!

!* --- Scan from 15h12m55s to 15h15m54s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=15h12m55s   !NEXT!        
qual=  0
disk=off
stop=15h15m54s   !NEXT!

!* --- Scan from 15h16m26s to 15h17m26s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=15h16m26s   !NEXT!        
qual=  0
disk=off
stop=15h17m26s   !NEXT!

!* --- Scan from 15h17m32s to 15h18m32s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=15h17m32s   !NEXT!        
qual=  0
disk=off
stop=15h18m32s   !NEXT!

!* --- Scan from 15h18m48s to 15h21m48s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=15h18m48s   !NEXT!        
qual=  0
disk=off
stop=15h21m48s   !NEXT!

!* --- Scan from 15h22m05s to 15h23m04s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=15h22m05s   !NEXT!        
qual=  0
disk=off
stop=15h23m04s   !NEXT!

!* --- Scan from 15h23m10s to 15h24m10s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=15h23m10s   !NEXT!        
qual=  0
disk=off
stop=15h24m10s   !NEXT!

!* --- Scan from 15h24m28s to 15h27m28s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=15h24m28s   !NEXT!        
qual=  0
disk=off
stop=15h27m28s   !NEXT!

!* --- Scan from 15h27m45s to 15h28m45s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=15h27m45s   !NEXT!        
qual=  0
disk=off
stop=15h28m45s   !NEXT!

!* --- Scan from 15h28m51s to 15h29m50s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=15h28m51s   !NEXT!        
qual=  0
disk=off
stop=15h29m50s   !NEXT!

!* --- Scan from 15h30m07s to 15h33m07s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=15h30m07s   !NEXT!        
qual=  0
disk=off
stop=15h33m07s   !NEXT!

!* --- Scan from 15h33m23s to 15h34m22s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=15h33m23s   !NEXT!        
qual=  0
disk=off
stop=15h34m22s   !NEXT!

!* --- Scan from 15h34m28s to 15h35m28s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=15h34m28s   !NEXT!        
qual=  0
disk=off
stop=15h35m28s   !NEXT!

!* --- Scan from 15h35m44s to 15h38m43s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=15h35m44s   !NEXT!        
qual=  0
disk=off
stop=15h38m43s   !NEXT!

!* --- Scan from 15h38m58s to 15h39m58s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=15h38m58s   !NEXT!        
qual=  0
disk=off
stop=15h39m58s   !NEXT!

!* --- Scan from 15h40m04s to 15h41m04s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=15h40m04s   !NEXT!        
qual=  0
disk=off
stop=15h41m04s   !NEXT!

!* --- Scan from 15h41m18s to 15h44m17s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=15h41m18s   !NEXT!        
qual=  0
disk=off
stop=15h44m17s   !NEXT!

!* --- Scan from 15h44m31s to 15h45m31s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=15h44m31s   !NEXT!        
qual=  0
disk=off
stop=15h45m31s   !NEXT!

!* --- Scan from 15h45m37s to 15h46m37s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=15h45m37s   !NEXT!        
qual=  0
disk=off
stop=15h46m37s   !NEXT!

!* --- Scan from 15h46m50s to 15h49m50s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=15h46m50s   !NEXT!        
qual=  0
disk=off
stop=15h49m50s   !NEXT!

!* --- Scan from 15h50m03s to 15h51m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=15h50m03s   !NEXT!        
qual=  0
disk=off
stop=15h51m03s   !NEXT!

!* --- Scan from 15h51m09s to 15h52m09s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=15h51m09s   !NEXT!        
qual=  0
disk=off
stop=15h52m09s   !NEXT!

!* --- Scan from 15h52m21s to 15h55m21s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=15h52m21s   !NEXT!        
qual=  0
disk=off
stop=15h55m21s   !NEXT!

!* --- Scan from 15h55m33s to 15h56m33s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=15h55m33s   !NEXT!        
qual=  0
disk=off
stop=15h56m33s   !NEXT!

!* --- Scan from 15h56m39s to 15h57m39s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=15h56m39s   !NEXT!        
qual=  0
disk=off
stop=15h57m39s   !NEXT!

!* --- Scan from 15h57m51s to 16h00m51s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=15h57m51s   !NEXT!        
qual=  0
disk=off
stop=16h00m51s   !NEXT!

!* --- Scan from 16h01m03s to 16h02m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=16h01m03s   !NEXT!        
qual=  0
disk=off
stop=16h02m03s   !NEXT!

!* --- Scan from 16h02m09s to 16h03m09s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=16h02m09s   !NEXT!        
qual=  0
disk=off
stop=16h03m09s   !NEXT!

!* --- Scan from 16h03m20s to 16h06m20s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=16h03m20s   !NEXT!        
qual=  0
disk=off
stop=16h06m20s   !NEXT!

!* --- Scan from 16h06m32s to 16h07m31s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=16h06m32s   !NEXT!        
qual=  0
disk=off
stop=16h07m31s   !NEXT!

!* --- Scan from 16h07m37s to 16h08m37s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=16h07m37s   !NEXT!        
qual=  0
disk=off
stop=16h08m37s   !NEXT!

!* --- Scan from 16h08m49s to 16h11m48s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=16h08m49s   !NEXT!        
qual=  0
disk=off
stop=16h11m48s   !NEXT!

!* --- Scan from 16h12m00s to 16h13m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=16h12m00s   !NEXT!        
qual=  0
disk=off
stop=16h13m00s   !NEXT!

!* --- Scan from 16h13m06s to 16h14m05s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=16h13m06s   !NEXT!        
qual=  0
disk=off
stop=16h14m05s   !NEXT!

!* --- Scan from 16h14m17s to 16h17m16s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=16h14m17s   !NEXT!        
qual=  0
disk=off
stop=16h17m16s   !NEXT!

!* --- Scan from 16h17m28s to 16h18m28s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=16h17m28s   !NEXT!        
qual=  0
disk=off
stop=16h18m28s   !NEXT!

!* --- Scan from 16h18m34s to 16h19m34s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=16h18m34s   !NEXT!        
qual=  0
disk=off
stop=16h19m34s   !NEXT!

!* --- Scan from 16h19m45s to 16h22m45s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=16h19m45s   !NEXT!        
qual=  0
disk=off
stop=16h22m45s   !NEXT!

!* --- Scan from 16h22m56s to 16h23m56s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=16h22m56s   !NEXT!        
qual=  0
disk=off
stop=16h23m56s   !NEXT!

!* --- Scan from 16h24m02s to 16h25m02s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=16h24m02s   !NEXT!        
qual=  0
disk=off
stop=16h25m02s   !NEXT!

!* --- Scan from 16h25m17s to 16h28m16s   Tue, 1999 Apr 20 --- *!
sname='J2052+3635'  ra=20h52m52.057400s  dec= 36d35'35.29900"  qual=999  calib='M'
disk=off
stop=16h25m17s   !NEXT!        
qual=  0
disk=off
stop=16h28m16s   !NEXT!

!* --- Scan from 16h28m31s to 16h29m31s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=16h28m31s   !NEXT!        
qual=  0
disk=off
stop=16h29m31s   !NEXT!

!* --- Scan from 16h30m11s to 16h35m11s   Tue, 1999 Apr 20 --- *!
sname='BLLAC'  ra=22h02m43.291386s  dec= 42d16'39.97984"  qual=999  calib='N'
disk=off
stop=16h30m11s   !NEXT!        
qual=  0
disk=off
stop=16h35m11s   !NEXT!

!* --- Scan from 16h35m47s to 16h36m47s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=16h35m47s   !NEXT!        
qual=  0
disk=off
stop=16h36m47s   !NEXT!

!* --- Scan from 16h37m02s to 16h40m01s   Tue, 1999 Apr 20 --- *!
sname='J2052+3635'  ra=20h52m52.057400s  dec= 36d35'35.29900"  qual=999  calib='M'
disk=off
stop=16h37m02s   !NEXT!        
qual=  0
disk=off
stop=16h40m01s   !NEXT!

!* --- Scan from 16h40m16s to 16h41m16s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=16h40m16s   !NEXT!        
qual=  0
disk=off
stop=16h41m16s   !NEXT!

!* --- Scan from 16h41m51s to 16h46m50s   Tue, 1999 Apr 20 --- *!
sname='BLLAC'  ra=22h02m43.291386s  dec= 42d16'39.97984"  qual=999  calib='N'
disk=off
stop=16h41m51s   !NEXT!        
qual=  0
disk=off
stop=16h46m50s   !NEXT!

!* --- Scan from 16h47m26s to 16h48m25s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=16h47m26s   !NEXT!        
qual=  0
disk=off
stop=16h48m25s   !NEXT!

!* --- Scan from 16h48m37s to 16h51m36s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=16h48m37s   !NEXT!        
qual=  0
disk=off
stop=16h51m36s   !NEXT!

!* --- Scan from 16h51m48s to 16h52m48s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=16h51m48s   !NEXT!        
qual=  0
disk=off
stop=16h52m48s   !NEXT!

!* --- Scan from 16h52m54s to 16h53m54s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=16h52m54s   !NEXT!        
qual=  0
disk=off
stop=16h53m54s   !NEXT!

!* --- Scan from 16h54m05s to 16h57m05s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=16h54m05s   !NEXT!        
qual=  0
disk=off
stop=16h57m05s   !NEXT!

!* --- Scan from 16h57m16s to 16h58m16s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=16h57m16s   !NEXT!        
qual=  0
disk=off
stop=16h58m16s   !NEXT!

!* --- Scan from 16h58m22s to 16h59m22s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=16h58m22s   !NEXT!        
qual=  0
disk=off
stop=16h59m22s   !NEXT!

!* --- Scan from 16h59m34s to 17h02m33s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=16h59m34s   !NEXT!        
qual=  0
disk=off
stop=17h02m33s   !NEXT!

!* --- Scan from 17h02m45s to 17h03m45s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=17h02m45s   !NEXT!        
qual=  0
disk=off
stop=17h03m45s   !NEXT!

!* --- Scan from 17h03m51s to 17h04m50s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=17h03m51s   !NEXT!        
qual=  0
disk=off
stop=17h04m50s   !NEXT!

!* --- Scan from 17h05m02s to 17h08m01s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=17h05m02s   !NEXT!        
qual=  0
disk=off
stop=17h08m01s   !NEXT!

!* --- Scan from 17h08m13s to 17h09m13s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=17h08m13s   !NEXT!        
qual=  0
disk=off
stop=17h09m13s   !NEXT!

!* --- Scan from 17h09m19s to 17h10m19s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=17h09m19s   !NEXT!        
qual=  0
disk=off
stop=17h10m19s   !NEXT!

!* --- Scan from 17h10m30s to 17h13m30s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=17h10m30s   !NEXT!        
qual=  0
disk=off
stop=17h13m30s   !NEXT!

!* --- Scan from 17h13m41s to 17h14m41s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=17h13m41s   !NEXT!        
qual=  0
disk=off
stop=17h14m41s   !NEXT!

!* --- Scan from 17h14m47s to 17h15m47s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=17h14m47s   !NEXT!        
qual=  0
disk=off
stop=17h15m47s   !NEXT!

!* --- Scan from 17h15m59s to 17h18m58s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=17h15m59s   !NEXT!        
qual=  0
disk=off
stop=17h18m58s   !NEXT!

!* --- Scan from 17h19m10s to 17h20m10s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=17h19m10s   !NEXT!        
qual=  0
disk=off
stop=17h20m10s   !NEXT!

!* --- Scan from 17h20m16s to 17h21m15s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=17h20m16s   !NEXT!        
qual=  0
disk=off
stop=17h21m15s   !NEXT!

!* --- Scan from 17h21m27s to 17h24m27s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=17h21m27s   !NEXT!        
qual=  0
disk=off
stop=17h24m27s   !NEXT!

!* --- Scan from 17h24m38s to 17h25m38s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=17h24m38s   !NEXT!        
qual=  0
disk=off
stop=17h25m38s   !NEXT!

!* --- Scan from 17h25m44s to 17h26m44s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=17h25m44s   !NEXT!        
qual=  0
disk=off
stop=17h26m44s   !NEXT!

!* --- Scan from 17h26m55s to 17h29m55s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=17h26m55s   !NEXT!        
qual=  0
disk=off
stop=17h29m55s   !NEXT!

!* --- Scan from 17h30m07s to 17h31m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=17h30m07s   !NEXT!        
qual=  0
disk=off
stop=17h31m06s   !NEXT!

!* --- Scan from 17h31m12s to 17h32m12s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=17h31m12s   !NEXT!        
qual=  0
disk=off
stop=17h32m12s   !NEXT!

!* --- Scan from 17h32m24s to 17h35m23s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=17h32m24s   !NEXT!        
qual=  0
disk=off
stop=17h35m23s   !NEXT!

!* --- Scan from 17h35m35s to 17h36m35s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=17h35m35s   !NEXT!        
qual=  0
disk=off
stop=17h36m35s   !NEXT!

!* --- Scan from 17h36m41s to 17h37m41s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=17h36m41s   !NEXT!        
qual=  0
disk=off
stop=17h37m41s   !NEXT!

!* --- Scan from 17h37m52s to 17h40m52s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=17h37m52s   !NEXT!        
qual=  0
disk=off
stop=17h40m52s   !NEXT!

!* --- Scan from 17h41m03s to 17h42m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=17h41m03s   !NEXT!        
qual=  0
disk=off
stop=17h42m03s   !NEXT!

!* --- Scan from 17h42m09s to 17h43m09s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=17h42m09s   !NEXT!        
qual=  0
disk=off
stop=17h43m09s   !NEXT!

!* --- Scan from 17h43m21s to 17h46m20s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=17h43m21s   !NEXT!        
qual=  0
disk=off
stop=17h46m20s   !NEXT!

!* --- Scan from 17h46m32s to 17h47m32s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=17h46m32s   !NEXT!        
qual=  0
disk=off
stop=17h47m32s   !NEXT!

!* --- Scan from 17h47m38s to 17h48m38s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=17h47m38s   !NEXT!        
qual=  0
disk=off
stop=17h48m38s   !NEXT!

!* --- Scan from 17h48m49s to 17h51m49s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=17h48m49s   !NEXT!        
qual=  0
disk=off
stop=17h51m49s   !NEXT!

!* --- Scan from 17h52m00s to 17h53m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=17h52m00s   !NEXT!        
qual=  0
disk=off
stop=17h53m00s   !NEXT!

!* --- Scan from 17h53m06s to 17h54m06s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=17h53m06s   !NEXT!        
qual=  0
disk=off
stop=17h54m06s   !NEXT!

!* --- Scan from 17h54m18s to 17h57m17s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=17h54m18s   !NEXT!        
qual=  0
disk=off
stop=17h57m17s   !NEXT!

!* --- Scan from 17h57m29s to 17h58m29s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=17h57m29s   !NEXT!        
qual=  0
disk=off
stop=17h58m29s   !NEXT!

!* --- Scan from 17h58m35s to 17h59m34s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=17h58m35s   !NEXT!        
qual=  0
disk=off
stop=17h59m34s   !NEXT!

!* --- Scan from 17h59m46s to 18h02m46s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=17h59m46s   !NEXT!        
qual=  0
disk=off
stop=18h02m46s   !NEXT!

!* --- Scan from 18h02m57s to 18h03m57s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=18h02m57s   !NEXT!        
qual=  0
disk=off
stop=18h03m57s   !NEXT!

!* --- Scan from 18h04m03s to 18h05m03s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=18h04m03s   !NEXT!        
qual=  0
disk=off
stop=18h05m03s   !NEXT!

!* --- Scan from 18h05m14s to 18h08m14s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=18h05m14s   !NEXT!        
qual=  0
disk=off
stop=18h08m14s   !NEXT!

!* --- Scan from 18h08m26s to 18h09m26s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=18h08m26s   !NEXT!        
qual=  0
disk=off
stop=18h09m26s   !NEXT!

!* --- Scan from 18h09m32s to 18h10m31s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=18h09m32s   !NEXT!        
qual=  0
disk=off
stop=18h10m31s   !NEXT!

!* --- Scan from 18h10m43s to 18h13m42s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=18h10m43s   !NEXT!        
qual=  0
disk=off
stop=18h13m42s   !NEXT!

!* --- Scan from 18h13m54s to 18h14m54s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
disk=off
stop=18h13m54s   !NEXT!        
qual=  0
disk=off
stop=18h14m54s   !NEXT!

!* --- Scan from 18h15m00s to 18h16m00s   Tue, 1999 Apr 20 --- *!
sname='J2109+3532'  ra=21h09m31.878500s  dec= 35d32'57.60200"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
ifchan=(1,B),(2,D),(3,B),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,813.75),( 2,813.75),( 3,821.75),( 4,821.75)
disk=off
stop=18h15m00s   !NEXT!        
qual=  0
disk=off
stop=18h16m00s   !NEXT!

!* --- Scan from 18h16m11s to 18h19m11s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
disk=off
stop=18h16m11s   !NEXT!        
qual=  0
disk=off
stop=18h19m11s   !NEXT!

!* --- Scan from 18h21m40s to 18h24m39s   Tue, 1999 Apr 20 --- *!
sname='J2115+3645'  ra=21h15m40.397800s  dec= 36d45'50.65800"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,940.25),( 2,940.25),( 3,932.25),( 4,932.25)
disk=off
stop=18h21m40s   !NEXT!        
qual=  0
disk=off
stop=18h24m39s   !NEXT!
disk=off
stop=18h24m44s   !NEXT!
     !QUIT! 
