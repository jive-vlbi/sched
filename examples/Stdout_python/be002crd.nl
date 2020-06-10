!*  Schedule for VLBA_NL   *!
!*  Experiment BE002    *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:  11.60  Release 11.6; Feburary 2020 *!
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
!*  Start at 01h29m55s     Sun, 1995 Oct 22  Day of year  295   *!
program=BE002   

diskformat=mark5a
media=(1,disk)

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 01h29m55s to 01h35m30s   Sun, 1995 Oct 22 --- *!
sname='3C454.3'  ra=22h53m57.747939s  dec= 16d08'53.56091"  qual=999  calib='V'
maxcaltime= 120
fe=(1,6cm),(3,6cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
logging=STANDARD
nchan= 4
format=VLBA1:2
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
disk=off
  date = 1995Oct22
stop=01h29m55s   !NEXT!        
qual=  0
disk=on
stop=01h35m30s   !NEXT!

!* --- Scan from 01h35m45s to 01h41m20s   Sun, 1995 Oct 22 --- *!
sname='3C454.3'  ra=22h53m57.747939s  dec= 16d08'53.56091"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=01h35m45s   !NEXT!        
qual=  0
disk=on
stop=01h41m20s   !NEXT!

!* --- Scan from 01h43m15s to 01h46m20s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=01h43m15s   !NEXT!        
qual=  0
disk=on
stop=01h46m20s   !NEXT!

!* --- Scan from 01h46m35s to 01h49m40s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=01h46m35s   !NEXT!        
qual=  0
disk=on
stop=01h49m40s   !NEXT!

!* --- Scan from 01h50m05s to 01h52m10s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=01h50m05s   !NEXT!        
qual=  0
disk=on
stop=01h52m10s   !NEXT!

!* --- Scan from 01h52m34s to 01h55m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=01h52m34s   !NEXT!        
qual=  0
disk=on
stop=01h55m39s   !NEXT!

!* --- Scan from 01h57m34s to 02h00m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=01h57m34s   !NEXT!        
qual=  0
disk=on
stop=02h00m39s   !NEXT!

!* --- Scan from 02h00m54s to 02h03m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=02h00m54s   !NEXT!        
qual=  0
disk=on
stop=02h03m59s   !NEXT!

!* --- Scan from 02h04m23s to 02h06m28s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=02h04m23s   !NEXT!        
qual=  0
disk=on
stop=02h06m28s   !NEXT!

!* --- Scan from 02h06m53s to 02h09m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=02h06m53s   !NEXT!        
qual=  0
disk=on
stop=02h09m58s   !NEXT!

!* --- Scan from 02h11m53s to 02h14m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=02h11m53s   !NEXT!        
qual=  0
disk=on
stop=02h14m58s   !NEXT!

!* --- Scan from 02h15m13s to 02h18m18s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=02h15m13s   !NEXT!        
qual=  0
disk=on
stop=02h18m18s   !NEXT!

!* --- Scan from 02h18m43s to 02h20m48s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=02h18m43s   !NEXT!        
qual=  0
disk=on
stop=02h20m48s   !NEXT!

!* --- Scan from 02h21m12s to 02h24m17s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=02h21m12s   !NEXT!        
qual=  0
disk=on
stop=02h24m17s   !NEXT!

!* --- Scan from 02h26m12s to 02h29m17s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=02h26m12s   !NEXT!        
qual=  0
disk=on
stop=02h29m17s   !NEXT!

!* --- Scan from 02h29m32s to 02h32m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=02h29m32s   !NEXT!        
qual=  0
disk=on
stop=02h32m37s   !NEXT!

!* --- Scan from 02h33m02s to 02h35m07s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=02h33m02s   !NEXT!        
qual=  0
disk=on
stop=02h35m07s   !NEXT!

!* --- Scan from 02h35m31s to 02h38m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=02h35m31s   !NEXT!        
qual=  0
disk=on
stop=02h38m36s   !NEXT!

!* --- Scan from 02h40m31s to 02h43m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=02h40m31s   !NEXT!        
qual=  0
disk=on
stop=02h43m36s   !NEXT!

!* --- Scan from 02h43m51s to 02h46m56s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=02h43m51s   !NEXT!        
qual=  0
disk=on
stop=02h46m56s   !NEXT!

!* --- Scan from 02h47m21s to 02h49m26s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=02h47m21s   !NEXT!        
qual=  0
disk=on
stop=02h49m26s   !NEXT!

!* --- Scan from 02h49m51s to 02h52m56s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=02h49m51s   !NEXT!        
qual=  0
disk=on
stop=02h52m56s   !NEXT!

!* --- Scan from 02h54m51s to 02h57m56s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=02h54m51s   !NEXT!        
qual=  0
disk=on
stop=02h57m56s   !NEXT!

!* --- Scan from 02h58m11s to 03h01m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=02h58m11s   !NEXT!        
qual=  0
disk=on
stop=03h01m16s   !NEXT!

!* --- Scan from 03h01m41s to 03h03m46s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=03h01m41s   !NEXT!        
qual=  0
disk=on
stop=03h03m46s   !NEXT!

!* --- Scan from 03h04m10s to 03h07m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=03h04m10s   !NEXT!        
qual=  0
disk=on
stop=03h07m15s   !NEXT!

!* --- Scan from 03h09m10s to 03h12m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=03h09m10s   !NEXT!        
qual=  0
disk=on
stop=03h12m15s   !NEXT!

!* --- Scan from 03h12m30s to 03h15m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=03h12m30s   !NEXT!        
qual=  0
disk=on
stop=03h15m35s   !NEXT!

!* --- Scan from 03h16m00s to 03h18m05s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=03h16m00s   !NEXT!        
qual=  0
disk=on
stop=03h18m05s   !NEXT!

!* --- Scan from 03h18m30s to 03h21m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=03h18m30s   !NEXT!        
qual=  0
disk=on
stop=03h21m35s   !NEXT!

!* --- Scan from 03h23m30s to 03h26m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=03h23m30s   !NEXT!        
qual=  0
disk=on
stop=03h26m35s   !NEXT!

!* --- Scan from 03h26m50s to 03h29m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=03h26m50s   !NEXT!        
qual=  0
disk=on
stop=03h29m55s   !NEXT!

!* --- Scan from 03h30m20s to 03h32m25s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=03h30m20s   !NEXT!        
qual=  0
disk=on
stop=03h32m25s   !NEXT!

!* --- Scan from 03h32m49s to 03h35m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=03h32m49s   !NEXT!        
qual=  0
disk=on
stop=03h35m54s   !NEXT!

!* --- Scan from 03h37m49s to 03h40m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=03h37m49s   !NEXT!        
qual=  0
disk=on
stop=03h40m54s   !NEXT!

!* --- Scan from 03h41m09s to 03h44m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=03h41m09s   !NEXT!        
qual=  0
disk=on
stop=03h44m14s   !NEXT!

!* --- Scan from 03h44m39s to 03h46m44s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=03h44m39s   !NEXT!        
qual=  0
disk=on
stop=03h46m44s   !NEXT!

!* --- Scan from 03h47m09s to 03h50m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=03h47m09s   !NEXT!        
qual=  0
disk=on
stop=03h50m14s   !NEXT!

!* --- Scan from 03h52m09s to 03h55m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=03h52m09s   !NEXT!        
qual=  0
disk=on
stop=03h55m14s   !NEXT!

!* --- Scan from 03h55m29s to 03h58m34s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=03h55m29s   !NEXT!        
qual=  0
disk=on
stop=03h58m34s   !NEXT!

!* --- Scan from 03h58m59s to 04h01m04s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=03h58m59s   !NEXT!        
qual=  0
disk=on
stop=04h01m04s   !NEXT!

!* --- Scan from 04h01m29s to 04h04m34s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=04h01m29s   !NEXT!        
qual=  0
disk=on
stop=04h04m34s   !NEXT!

!* --- Scan from 04h06m29s to 04h09m34s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=04h06m29s   !NEXT!        
qual=  0
disk=on
stop=04h09m34s   !NEXT!

!* --- Scan from 04h09m49s to 04h12m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=04h09m49s   !NEXT!        
qual=  0
disk=on
stop=04h12m54s   !NEXT!

!* --- Scan from 04h13m19s to 04h15m24s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=04h13m19s   !NEXT!        
qual=  0
disk=on
stop=04h15m24s   !NEXT!

!* --- Scan from 04h15m49s to 04h18m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=04h15m49s   !NEXT!        
qual=  0
disk=on
stop=04h18m54s   !NEXT!

!* --- Scan from 04h20m49s to 04h23m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=04h20m49s   !NEXT!        
qual=  0
disk=on
stop=04h23m54s   !NEXT!

!* --- Scan from 04h24m09s to 04h27m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=04h24m09s   !NEXT!        
qual=  0
disk=on
stop=04h27m14s   !NEXT!

!* --- Scan from 04h27m39s to 04h29m44s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=04h27m39s   !NEXT!        
qual=  0
disk=on
stop=04h29m44s   !NEXT!

!* --- Scan from 04h30m09s to 04h33m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=04h30m09s   !NEXT!        
qual=  0
disk=on
stop=04h33m14s   !NEXT!

!* --- Scan from 04h35m09s to 04h38m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=04h35m09s   !NEXT!        
qual=  0
disk=on
stop=04h38m14s   !NEXT!

!* --- Scan from 04h38m29s to 04h41m34s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=04h38m29s   !NEXT!        
qual=  0
disk=on
stop=04h41m34s   !NEXT!

!* --- Scan from 04h41m59s to 04h44m04s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=04h41m59s   !NEXT!        
qual=  0
disk=on
stop=04h44m04s   !NEXT!

!* --- Scan from 04h44m29s to 04h47m34s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=04h44m29s   !NEXT!        
qual=  0
disk=on
stop=04h47m34s   !NEXT!

!* --- Scan from 04h49m29s to 04h52m34s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=04h49m29s   !NEXT!        
qual=  0
disk=on
stop=04h52m34s   !NEXT!

!* --- Scan from 04h52m49s to 04h55m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=04h52m49s   !NEXT!        
qual=  0
disk=on
stop=04h55m54s   !NEXT!

!* --- Scan from 04h56m19s to 04h58m24s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=04h56m19s   !NEXT!        
qual=  0
disk=on
stop=04h58m24s   !NEXT!

!* --- Scan from 04h58m49s to 05h01m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=04h58m49s   !NEXT!        
qual=  0
disk=on
stop=05h01m54s   !NEXT!

!* --- Scan from 05h03m49s to 05h06m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=05h03m49s   !NEXT!        
qual=  0
disk=on
stop=05h06m54s   !NEXT!

!* --- Scan from 05h07m09s to 05h10m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=05h07m09s   !NEXT!        
qual=  0
disk=on
stop=05h10m14s   !NEXT!

!* --- Scan from 05h10m40s to 05h12m45s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=05h10m40s   !NEXT!        
qual=  0
disk=on
stop=05h12m45s   !NEXT!

!* --- Scan from 05h13m10s to 05h16m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=05h13m10s   !NEXT!        
qual=  0
disk=on
stop=05h16m15s   !NEXT!

!* --- Scan from 05h18m10s to 05h21m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=05h18m10s   !NEXT!        
qual=  0
disk=on
stop=05h21m15s   !NEXT!

!* --- Scan from 05h21m30s to 05h24m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=05h21m30s   !NEXT!        
qual=  0
disk=on
stop=05h24m35s   !NEXT!

!* --- Scan from 05h25m00s to 05h27m05s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=05h25m00s   !NEXT!        
qual=  0
disk=on
stop=05h27m05s   !NEXT!

!* --- Scan from 05h27m30s to 05h30m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=05h27m30s   !NEXT!        
qual=  0
disk=on
stop=05h30m35s   !NEXT!

!* --- Scan from 05h32m30s to 05h35m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=05h32m30s   !NEXT!        
qual=  0
disk=on
stop=05h35m35s   !NEXT!

!* --- Scan from 05h35m50s to 05h38m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=05h35m50s   !NEXT!        
qual=  0
disk=on
stop=05h38m55s   !NEXT!

!* --- Scan from 05h39m20s to 05h41m25s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=05h39m20s   !NEXT!        
qual=  0
disk=on
stop=05h41m25s   !NEXT!

!* --- Scan from 05h41m50s to 05h44m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=05h41m50s   !NEXT!        
qual=  0
disk=on
stop=05h44m55s   !NEXT!

!* --- Scan from 05h46m50s to 05h49m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=05h46m50s   !NEXT!        
qual=  0
disk=on
stop=05h49m55s   !NEXT!

!* --- Scan from 05h50m10s to 05h53m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=05h50m10s   !NEXT!        
qual=  0
disk=on
stop=05h53m15s   !NEXT!

!* --- Scan from 05h53m41s to 05h55m46s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=05h53m41s   !NEXT!        
qual=  0
disk=on
stop=05h55m46s   !NEXT!

!* --- Scan from 05h56m12s to 05h59m17s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=05h56m12s   !NEXT!        
qual=  0
disk=on
stop=05h59m17s   !NEXT!

!* --- Scan from 06h01m12s to 06h04m17s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=06h01m12s   !NEXT!        
qual=  0
disk=on
stop=06h04m17s   !NEXT!

!* --- Scan from 06h04m32s to 06h07m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=06h04m32s   !NEXT!        
qual=  0
disk=on
stop=06h07m37s   !NEXT!

!* --- Scan from 06h08m11s to 06h10m16s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=06h08m11s   !NEXT!        
qual=  0
disk=on
stop=06h10m16s   !NEXT!

!* --- Scan from 06h10m51s to 06h13m56s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=06h10m51s   !NEXT!        
qual=  0
disk=on
stop=06h13m56s   !NEXT!

!* --- Scan from 06h15m51s to 06h18m56s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=06h15m51s   !NEXT!        
qual=  0
disk=on
stop=06h18m56s   !NEXT!

!* --- Scan from 06h19m11s to 06h22m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=06h19m11s   !NEXT!        
qual=  0
disk=on
stop=06h22m16s   !NEXT!

!* --- Scan from 06h23m07s to 06h25m12s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=06h23m07s   !NEXT!        
qual=  0
disk=on
stop=06h25m12s   !NEXT!

!* --- Scan from 06h26m04s to 06h29m09s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=06h26m04s   !NEXT!        
qual=  0
disk=on
stop=06h29m09s   !NEXT!

!* --- Scan from 06h31m04s to 06h34m09s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=06h31m04s   !NEXT!        
qual=  0
disk=on
stop=06h34m09s   !NEXT!

!* --- Scan from 06h34m24s to 06h37m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=06h34m24s   !NEXT!        
qual=  0
disk=on
stop=06h37m29s   !NEXT!

!* --- Scan from 06h38m40s to 06h40m45s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=06h38m40s   !NEXT!        
qual=  0
disk=on
stop=06h40m45s   !NEXT!

!* --- Scan from 06h41m45s to 06h44m50s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=06h41m45s   !NEXT!        
qual=  0
disk=on
stop=06h44m50s   !NEXT!

!* --- Scan from 06h46m45s to 06h49m50s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=06h46m45s   !NEXT!        
qual=  0
disk=on
stop=06h49m50s   !NEXT!

!* --- Scan from 06h50m05s to 06h53m10s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=06h50m05s   !NEXT!        
qual=  0
disk=on
stop=06h53m10s   !NEXT!

!* --- Scan from 06h53m57s to 06h56m02s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=06h53m57s   !NEXT!        
qual=  0
disk=on
stop=06h56m02s   !NEXT!

!* --- Scan from 06h56m44s to 06h59m49s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=06h56m44s   !NEXT!        
qual=  0
disk=on
stop=06h59m49s   !NEXT!

!* --- Scan from 07h01m44s to 07h04m49s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=07h01m44s   !NEXT!        
qual=  0
disk=on
stop=07h04m49s   !NEXT!

!* --- Scan from 07h05m04s to 07h08m09s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=07h05m04s   !NEXT!        
qual=  0
disk=on
stop=07h08m09s   !NEXT!

!* --- Scan from 07h08m45s to 07h10m50s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=07h08m45s   !NEXT!        
qual=  0
disk=on
stop=07h10m50s   !NEXT!

!* --- Scan from 07h11m24s to 07h14m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=07h11m24s   !NEXT!        
qual=  0
disk=on
stop=07h14m29s   !NEXT!

!* --- Scan from 07h16m24s to 07h19m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=07h16m24s   !NEXT!        
qual=  0
disk=on
stop=07h19m29s   !NEXT!

!* --- Scan from 07h19m44s to 07h22m49s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=07h19m44s   !NEXT!        
qual=  0
disk=on
stop=07h22m49s   !NEXT!

!* --- Scan from 07h24m21s to 07h26m26s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=07h24m21s   !NEXT!        
qual=  0
disk=on
stop=07h26m26s   !NEXT!

!* --- Scan from 07h26m55s to 07h30m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=07h26m55s   !NEXT!        
qual=  0
disk=on
stop=07h30m00s   !NEXT!

!* --- Scan from 07h31m55s to 07h35m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=07h31m55s   !NEXT!        
qual=  0
disk=on
stop=07h35m00s   !NEXT!

!* --- Scan from 07h35m15s to 07h38m20s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=07h35m15s   !NEXT!        
qual=  0
disk=on
stop=07h38m20s   !NEXT!

!* --- Scan from 07h38m46s to 07h40m51s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=07h38m46s   !NEXT!        
qual=  0
disk=on
stop=07h40m51s   !NEXT!

!* --- Scan from 07h41m17s to 07h44m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=07h41m17s   !NEXT!        
qual=  0
disk=on
stop=07h44m22s   !NEXT!

!* --- Scan from 07h46m17s to 07h49m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=07h46m17s   !NEXT!        
qual=  0
disk=on
stop=07h49m22s   !NEXT!

!* --- Scan from 07h49m37s to 07h52m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=07h49m37s   !NEXT!        
qual=  0
disk=on
stop=07h52m42s   !NEXT!

!* --- Scan from 07h53m08s to 07h55m13s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=07h53m08s   !NEXT!        
qual=  0
disk=on
stop=07h55m13s   !NEXT!

!* --- Scan from 07h55m37s to 07h58m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=07h55m37s   !NEXT!        
qual=  0
disk=on
stop=07h58m42s   !NEXT!

!* --- Scan from 08h00m37s to 08h03m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=08h00m37s   !NEXT!        
qual=  0
disk=on
stop=08h03m42s   !NEXT!

!* --- Scan from 08h03m57s to 08h07m02s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=08h03m57s   !NEXT!        
qual=  0
disk=on
stop=08h07m02s   !NEXT!

!* --- Scan from 08h07m28s to 08h09m33s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=08h07m28s   !NEXT!        
qual=  0
disk=on
stop=08h09m33s   !NEXT!

!* --- Scan from 08h09m58s to 08h13m03s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=08h09m58s   !NEXT!        
qual=  0
disk=on
stop=08h13m03s   !NEXT!

!* --- Scan from 08h14m58s to 08h18m03s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=08h14m58s   !NEXT!        
qual=  0
disk=on
stop=08h18m03s   !NEXT!

!* --- Scan from 08h18m18s to 08h21m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=08h18m18s   !NEXT!        
qual=  0
disk=on
stop=08h21m23s   !NEXT!

!* --- Scan from 08h21m48s to 08h23m53s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=08h21m48s   !NEXT!        
qual=  0
disk=on
stop=08h23m53s   !NEXT!

!* --- Scan from 08h24m18s to 08h27m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=08h24m18s   !NEXT!        
qual=  0
disk=on
stop=08h27m23s   !NEXT!

!* --- Scan from 08h29m18s to 08h32m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=08h29m18s   !NEXT!        
qual=  0
disk=on
stop=08h32m23s   !NEXT!

!* --- Scan from 08h36m58s to 08h40m03s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=08h36m58s   !NEXT!        
qual=  0
disk=on
stop=08h40m03s   !NEXT!

!* --- Scan from 08h40m18s to 08h42m23s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=08h40m18s   !NEXT!        
qual=  0
disk=on
stop=08h42m23s   !NEXT!

!* --- Scan from 08h46m56s to 08h50m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=08h46m56s   !NEXT!        
qual=  0
disk=on
stop=08h50m01s   !NEXT!

!* --- Scan from 08h51m56s to 08h55m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=08h51m56s   !NEXT!        
qual=  0
disk=on
stop=08h55m01s   !NEXT!

!* --- Scan from 08h55m16s to 08h58m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=08h55m16s   !NEXT!        
qual=  0
disk=on
stop=08h58m21s   !NEXT!

!* --- Scan from 08h58m46s to 09h00m51s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=08h58m46s   !NEXT!        
qual=  0
disk=on
stop=09h00m51s   !NEXT!

!* --- Scan from 09h01m16s to 09h04m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=09h01m16s   !NEXT!        
qual=  0
disk=on
stop=09h04m21s   !NEXT!

!* --- Scan from 09h06m16s to 09h09m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=09h06m16s   !NEXT!        
qual=  0
disk=on
stop=09h09m21s   !NEXT!

!* --- Scan from 09h11m35s to 09h14m40s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=09h11m35s   !NEXT!        
qual=  0
disk=on
stop=09h14m40s   !NEXT!

!* --- Scan from 09h14m40s to 09h16m46s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=on
stop=09h16m46s   !NEXT!

!* --- Scan from 09h18m59s to 09h22m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=09h18m59s   !NEXT!        
qual=  0
disk=on
stop=09h22m04s   !NEXT!

!* --- Scan from 09h23m59s to 09h27m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=09h23m59s   !NEXT!        
qual=  0
disk=on
stop=09h27m04s   !NEXT!

!* --- Scan from 09h27m19s to 09h30m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=09h27m19s   !NEXT!        
qual=  0
disk=on
stop=09h30m24s   !NEXT!

!* --- Scan from 09h30m49s to 09h32m54s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=09h30m49s   !NEXT!        
qual=  0
disk=on
stop=09h32m54s   !NEXT!

!* --- Scan from 09h33m19s to 09h36m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=09h33m19s   !NEXT!        
qual=  0
disk=on
stop=09h36m24s   !NEXT!

!* --- Scan from 09h38m19s to 09h41m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=09h38m19s   !NEXT!        
qual=  0
disk=on
stop=09h41m24s   !NEXT!

!* --- Scan from 09h43m24s to 09h46m29s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=09h43m24s   !NEXT!        
qual=  0
disk=on
stop=09h46m29s   !NEXT!

!* --- Scan from 09h46m44s to 09h48m49s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=09h46m44s   !NEXT!        
qual=  0
disk=on
stop=09h48m49s   !NEXT!

!* --- Scan from 09h50m48s to 09h53m53s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=09h50m48s   !NEXT!        
qual=  0
disk=on
stop=09h53m53s   !NEXT!

!* --- Scan from 09h55m48s to 09h58m53s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=09h55m48s   !NEXT!        
qual=  0
disk=on
stop=09h58m53s   !NEXT!

!* --- Scan from 09h59m08s to 10h02m13s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=09h59m08s   !NEXT!        
qual=  0
disk=on
stop=10h02m13s   !NEXT!

!* --- Scan from 10h02m38s to 10h04m43s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=10h02m38s   !NEXT!        
qual=  0
disk=on
stop=10h04m43s   !NEXT!

!* --- Scan from 10h05m08s to 10h08m13s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=10h05m08s   !NEXT!        
qual=  0
disk=on
stop=10h08m13s   !NEXT!

!* --- Scan from 10h10m08s to 10h13m13s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=10h10m08s   !NEXT!        
qual=  0
disk=on
stop=10h13m13s   !NEXT!

!* --- Scan from 10h15m10s to 10h18m15s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=10h15m10s   !NEXT!        
qual=  0
disk=on
stop=10h18m15s   !NEXT!

!* --- Scan from 10h18m30s to 10h20m35s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=10h18m30s   !NEXT!        
qual=  0
disk=on
stop=10h20m35s   !NEXT!

!* --- Scan from 10h22m34s to 10h25m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=10h22m34s   !NEXT!        
qual=  0
disk=on
stop=10h25m39s   !NEXT!

!* --- Scan from 10h27m34s to 10h30m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=10h27m34s   !NEXT!        
qual=  0
disk=on
stop=10h30m39s   !NEXT!

!* --- Scan from 10h30m54s to 10h33m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=10h30m54s   !NEXT!        
qual=  0
disk=on
stop=10h33m59s   !NEXT!

!* --- Scan from 10h34m24s to 10h36m29s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=10h34m24s   !NEXT!        
qual=  0
disk=on
stop=10h36m29s   !NEXT!

!* --- Scan from 10h36m54s to 10h39m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=10h36m54s   !NEXT!        
qual=  0
disk=on
stop=10h39m59s   !NEXT!

!* --- Scan from 10h41m54s to 10h44m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=10h41m54s   !NEXT!        
qual=  0
disk=on
stop=10h44m59s   !NEXT!

!* --- Scan from 10h46m54s to 10h49m59s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=10h46m54s   !NEXT!        
qual=  0
disk=on
stop=10h49m59s   !NEXT!

!* --- Scan from 10h50m14s to 10h52m19s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=10h50m14s   !NEXT!        
qual=  0
disk=on
stop=10h52m19s   !NEXT!

!* --- Scan from 10h54m17s to 10h57m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=10h54m17s   !NEXT!        
qual=  0
disk=on
stop=10h57m22s   !NEXT!

!* --- Scan from 10h59m17s to 11h02m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=10h59m17s   !NEXT!        
qual=  0
disk=on
stop=11h02m22s   !NEXT!

!* --- Scan from 11h02m37s to 11h05m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=11h02m37s   !NEXT!        
qual=  0
disk=on
stop=11h05m42s   !NEXT!

!* --- Scan from 11h06m07s to 11h08m12s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=11h06m07s   !NEXT!        
qual=  0
disk=on
stop=11h08m12s   !NEXT!

!* --- Scan from 11h08m37s to 11h11m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=11h08m37s   !NEXT!        
qual=  0
disk=on
stop=11h11m42s   !NEXT!

!* --- Scan from 11h13m37s to 11h16m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=11h13m37s   !NEXT!        
qual=  0
disk=on
stop=11h16m42s   !NEXT!

!* --- Scan from 11h18m36s to 11h21m41s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=11h18m36s   !NEXT!        
qual=  0
disk=on
stop=11h21m41s   !NEXT!

!* --- Scan from 11h21m56s to 11h24m01s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=11h21m56s   !NEXT!        
qual=  0
disk=on
stop=11h24m01s   !NEXT!

!* --- Scan from 11h25m56s to 11h29m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=11h25m56s   !NEXT!        
qual=  0
disk=on
stop=11h29m01s   !NEXT!

!* --- Scan from 11h30m56s to 11h34m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=11h30m56s   !NEXT!        
qual=  0
disk=on
stop=11h34m01s   !NEXT!

!* --- Scan from 11h34m16s to 11h37m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=11h34m16s   !NEXT!        
qual=  0
disk=on
stop=11h37m21s   !NEXT!

!* --- Scan from 11h37m46s to 11h39m51s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=11h37m46s   !NEXT!        
qual=  0
disk=on
stop=11h39m51s   !NEXT!

!* --- Scan from 11h40m16s to 11h43m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=11h40m16s   !NEXT!        
qual=  0
disk=on
stop=11h43m21s   !NEXT!

!* --- Scan from 11h45m16s to 11h48m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=11h45m16s   !NEXT!        
qual=  0
disk=on
stop=11h48m21s   !NEXT!

!* --- Scan from 11h50m13s to 11h53m18s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=11h50m13s   !NEXT!        
qual=  0
disk=on
stop=11h53m18s   !NEXT!

!* --- Scan from 11h53m33s to 11h55m38s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=11h53m33s   !NEXT!        
qual=  0
disk=on
stop=11h55m38s   !NEXT!

!* --- Scan from 11h57m32s to 12h00m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=11h57m32s   !NEXT!        
qual=  0
disk=on
stop=12h00m37s   !NEXT!

!* --- Scan from 12h02m32s to 12h05m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=12h02m32s   !NEXT!        
qual=  0
disk=on
stop=12h05m37s   !NEXT!

!* --- Scan from 12h05m52s to 12h08m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=12h05m52s   !NEXT!        
qual=  0
disk=on
stop=12h08m57s   !NEXT!

!* --- Scan from 12h09m22s to 12h11m27s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=12h09m22s   !NEXT!        
qual=  0
disk=on
stop=12h11m27s   !NEXT!

!* --- Scan from 12h11m51s to 12h14m56s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=12h11m51s   !NEXT!        
qual=  0
disk=on
stop=12h14m56s   !NEXT!

!* --- Scan from 12h16m51s to 12h19m56s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=12h16m51s   !NEXT!        
qual=  0
disk=on
stop=12h19m56s   !NEXT!

!* --- Scan from 12h21m46s to 12h24m51s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=12h21m46s   !NEXT!        
qual=  0
disk=on
stop=12h24m51s   !NEXT!

!* --- Scan from 12h25m06s to 12h27m11s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=12h25m06s   !NEXT!        
qual=  0
disk=on
stop=12h27m11s   !NEXT!

!* --- Scan from 12h29m03s to 12h32m08s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=12h29m03s   !NEXT!        
qual=  0
disk=on
stop=12h32m08s   !NEXT!

!* --- Scan from 12h34m03s to 12h37m08s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=12h34m03s   !NEXT!        
qual=  0
disk=on
stop=12h37m08s   !NEXT!

!* --- Scan from 12h37m23s to 12h40m28s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=12h37m23s   !NEXT!        
qual=  0
disk=on
stop=12h40m28s   !NEXT!

!* --- Scan from 12h40m52s to 12h42m57s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=12h40m52s   !NEXT!        
qual=  0
disk=on
stop=12h42m57s   !NEXT!

!* --- Scan from 12h43m22s to 12h46m27s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=12h43m22s   !NEXT!        
qual=  0
disk=on
stop=12h46m27s   !NEXT!

!* --- Scan from 12h48m22s to 12h51m27s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=12h48m22s   !NEXT!        
qual=  0
disk=on
stop=12h51m27s   !NEXT!

!* --- Scan from 12h53m14s to 12h56m19s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=12h53m14s   !NEXT!        
qual=  0
disk=on
stop=12h56m19s   !NEXT!

!* --- Scan from 12h56m34s to 12h58m39s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=12h56m34s   !NEXT!        
qual=  0
disk=on
stop=12h58m39s   !NEXT!

!* --- Scan from 13h00m28s to 13h03m33s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=13h00m28s   !NEXT!        
qual=  0
disk=on
stop=13h03m33s   !NEXT!

!* --- Scan from 13h05m28s to 13h08m33s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=13h05m28s   !NEXT!        
qual=  0
disk=on
stop=13h08m33s   !NEXT!

!* --- Scan from 13h08m48s to 13h11m53s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=13h08m48s   !NEXT!        
qual=  0
disk=on
stop=13h11m53s   !NEXT!

!* --- Scan from 13h12m17s to 13h14m22s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=13h12m17s   !NEXT!        
qual=  0
disk=on
stop=13h14m22s   !NEXT!

!* --- Scan from 13h14m47s to 13h17m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=13h14m47s   !NEXT!        
qual=  0
disk=on
stop=13h17m52s   !NEXT!

!* --- Scan from 13h19m47s to 13h22m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=13h19m47s   !NEXT!        
qual=  0
disk=on
stop=13h22m52s   !NEXT!

!* --- Scan from 13h24m36s to 13h27m41s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=13h24m36s   !NEXT!        
qual=  0
disk=on
stop=13h27m41s   !NEXT!

!* --- Scan from 13h27m56s to 13h30m01s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=13h27m56s   !NEXT!        
qual=  0
disk=on
stop=13h30m01s   !NEXT!

!* --- Scan from 13h31m47s to 13h34m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=13h31m47s   !NEXT!        
qual=  0
disk=on
stop=13h34m52s   !NEXT!

!* --- Scan from 13h36m47s to 13h39m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=13h36m47s   !NEXT!        
qual=  0
disk=on
stop=13h39m52s   !NEXT!

!* --- Scan from 13h40m07s to 13h43m12s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=13h40m07s   !NEXT!        
qual=  0
disk=on
stop=13h43m12s   !NEXT!

!* --- Scan from 13h43m36s to 13h45m41s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=13h43m36s   !NEXT!        
qual=  0
disk=on
stop=13h45m41s   !NEXT!

!* --- Scan from 13h46m05s to 13h49m10s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=13h46m05s   !NEXT!        
qual=  0
disk=on
stop=13h49m10s   !NEXT!

!* --- Scan from 13h51m05s to 13h54m10s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=13h51m05s   !NEXT!        
qual=  0
disk=on
stop=13h54m10s   !NEXT!

!* --- Scan from 13h55m51s to 13h58m56s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=13h55m51s   !NEXT!        
qual=  0
disk=on
stop=13h58m56s   !NEXT!

!* --- Scan from 13h59m11s to 14h01m16s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=13h59m11s   !NEXT!        
qual=  0
disk=on
stop=14h01m16s   !NEXT!

!* --- Scan from 14h02m58s to 14h06m03s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=14h02m58s   !NEXT!        
qual=  0
disk=on
stop=14h06m03s   !NEXT!

!* --- Scan from 14h07m58s to 14h11m03s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=14h07m58s   !NEXT!        
qual=  0
disk=on
stop=14h11m03s   !NEXT!

!* --- Scan from 14h11m18s to 14h14m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=14h11m18s   !NEXT!        
qual=  0
disk=on
stop=14h14m23s   !NEXT!

!* --- Scan from 14h14m47s to 14h16m52s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=14h14m47s   !NEXT!        
qual=  0
disk=on
stop=14h16m52s   !NEXT!

!* --- Scan from 14h17m16s to 14h20m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=14h17m16s   !NEXT!        
qual=  0
disk=on
stop=14h20m21s   !NEXT!

!* --- Scan from 14h22m16s to 14h25m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=14h22m16s   !NEXT!        
qual=  0
disk=on
stop=14h25m21s   !NEXT!

!* --- Scan from 14h26m59s to 14h30m04s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=14h26m59s   !NEXT!        
qual=  0
disk=on
stop=14h30m04s   !NEXT!

!* --- Scan from 14h30m19s to 14h32m24s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=14h30m19s   !NEXT!        
qual=  0
disk=on
stop=14h32m24s   !NEXT!

!* --- Scan from 14h34m01s to 14h37m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=14h34m01s   !NEXT!        
qual=  0
disk=on
stop=14h37m06s   !NEXT!

!* --- Scan from 14h39m01s to 14h42m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=14h39m01s   !NEXT!        
qual=  0
disk=on
stop=14h42m06s   !NEXT!

!* --- Scan from 14h42m06s to 14h45m12s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=on
stop=14h45m12s   !NEXT!

!* --- Scan from 14h45m12s to 14h47m23s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=14h47m23s   !NEXT!

!* --- Scan from 14h47m23s to 14h50m33s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=  0  calib='V'
disk=on
stop=14h50m33s   !NEXT!

!* --- Scan from 14h52m28s to 14h55m33s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=14h52m28s   !NEXT!        
qual=  0
disk=on
stop=14h55m33s   !NEXT!

!* --- Scan from 14h56m32s to 14h59m37s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=14h56m32s   !NEXT!        
qual=  0
disk=on
stop=14h59m37s   !NEXT!

!* --- Scan from 14h59m52s to 15h01m57s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=14h59m52s   !NEXT!        
qual=  0
disk=on
stop=15h01m57s   !NEXT!

!* --- Scan from 15h02m57s to 15h06m02s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=15h02m57s   !NEXT!        
qual=  0
disk=on
stop=15h06m02s   !NEXT!

!* --- Scan from 15h07m57s to 15h11m02s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=15h07m57s   !NEXT!        
qual=  0
disk=on
stop=15h11m02s   !NEXT!

!* --- Scan from 15h11m02s to 15h14m08s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=on
stop=15h14m08s   !NEXT!

!* --- Scan from 15h14m08s to 15h16m19s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=15h16m19s   !NEXT!

!* --- Scan from 15h16m19s to 15h19m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=  0  calib='V'
disk=on
stop=15h19m29s   !NEXT!

!* --- Scan from 15h21m24s to 15h24m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=15h21m24s   !NEXT!        
qual=  0
disk=on
stop=15h24m29s   !NEXT!

!* --- Scan from 15h25m29s to 15h28m34s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=15h25m29s   !NEXT!        
qual=  0
disk=on
stop=15h28m34s   !NEXT!

!* --- Scan from 15h28m49s to 15h30m54s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=15h28m49s   !NEXT!        
qual=  0
disk=on
stop=15h30m54s   !NEXT!

!* --- Scan from 15h31m56s to 15h35m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=15h31m56s   !NEXT!        
qual=  0
disk=on
stop=15h35m01s   !NEXT!

!* --- Scan from 15h36m56s to 15h40m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=15h36m56s   !NEXT!        
qual=  0
disk=on
stop=15h40m01s   !NEXT!

!* --- Scan from 15h40m01s to 15h43m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=on
stop=15h43m07s   !NEXT!

!* --- Scan from 15h43m07s to 15h45m17s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=15h45m17s   !NEXT!

!* --- Scan from 15h45m17s to 15h48m28s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=  0  calib='V'
disk=on
stop=15h48m28s   !NEXT!

!* --- Scan from 15h50m23s to 15h53m28s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=15h50m23s   !NEXT!        
qual=  0
disk=on
stop=15h53m28s   !NEXT!

!* --- Scan from 15h54m30s to 15h57m35s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
disk=off
stop=15h54m30s   !NEXT!        
qual=  0
disk=on
stop=15h57m35s   !NEXT!

!* --- Scan from 15h57m50s to 15h59m55s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805611s  dec= 39d48'49.16496"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=15h57m50s   !NEXT!        
qual=  0
disk=on
stop=15h59m55s   !NEXT!

!* --- Scan from 16h00m59s to 16h04m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
disk=off
stop=16h00m59s   !NEXT!        
qual=  0
disk=on
stop=16h04m04s   !NEXT!

!* --- Scan from 16h05m59s to 16h09m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=16h05m59s   !NEXT!        
qual=  0
disk=on
stop=16h09m04s   !NEXT!

!* --- Scan from 16h09m04s to 16h12m10s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=  0  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=on
stop=16h12m10s   !NEXT!

!* --- Scan from 16h12m10s to 16h14m21s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962121s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=16h14m21s   !NEXT!

!* --- Scan from 16h14m21s to 16h17m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160096s  dec= 41d30'42.10404"  qual=  0  calib='V'
disk=on
stop=16h17m31s   !NEXT!

!* --- Scan from 16h20m26s to 16h26m01s   Sun, 1995 Oct 22 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59820"  qual=999  calib='V'
fe=(1,6cm),(3,6cm)
synth=( 1,13.6),( 2, 4.1),( 3,13.6)
format=VLBA1:2
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,882.49),( 2,882.49),( 3,890.49),( 4,890.49)
disk=off
stop=16h20m26s   !NEXT!        
qual=  0
disk=on
stop=16h26m01s   !NEXT!

!* --- Scan from 16h26m16s to 16h31m51s   Sun, 1995 Oct 22 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59820"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.1),( 3,15.1)
format=VLBA1:4
ifchan=(1,B),(2,D),(3,B),(4,D)
bbsynth=( 1,813.49),( 2,813.49),( 3,821.49),( 4,821.49)
disk=off
stop=16h26m16s   !NEXT!        
qual=  0
disk=on
stop=16h31m51s   !NEXT!
disk=off
stop=16h31m56s   !NEXT!
     !QUIT! 
