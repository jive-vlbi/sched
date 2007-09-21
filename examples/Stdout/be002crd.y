!*  Schedule for VLA1      *!
!*  Experiment BE002    *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:   7.00  Development late 2006 *!
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
diskformat=mark5a
media=(1,disk)

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 01h30m00s to 01h35m30s   Sun, 1995 Oct 22 --- *!
sname='3C454.3'  ra=22h53m57.747943s  dec= 16d08'53.56089"  qual=999  calib='V'
maxcaltime= 120
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
logging=STANDARD
nchan= 4
format=VLBA1:4
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bits=(1,1),(2,1),(3,1),(4,1)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S3),(7,S1),(8,S3)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,S2),(6,S4),(7,S2),(8,S4)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,1510),(8,1510)
samplerate=16M
disk=off
  date = 1995Oct22
stop=01h30m00s   !NEXT!        
qual=  0
disk=on
stop=01h35m30s   !NEXT!

!* --- Scan from 01h35m30s to 01h41m15s   Sun, 1995 Oct 22 --- *!
sname='3C454.3'  ra=22h53m57.747943s  dec= 16d08'53.56089"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=01h41m15s   !NEXT!

!* --- Scan from 01h43m15s to 01h46m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=01h43m15s   !NEXT!        
qual=  0
disk=on
stop=01h46m15s   !NEXT!

!* --- Scan from 01h46m15s to 01h49m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=01h49m31s   !NEXT!

!* --- Scan from 01h49m31s to 01h51m49s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=01h51m49s   !NEXT!

!* --- Scan from 01h51m49s to 01h55m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=01h55m07s   !NEXT!

!* --- Scan from 01h57m07s to 02h00m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=01h57m07s   !NEXT!        
qual=  0
disk=on
stop=02h00m07s   !NEXT!

!* --- Scan from 02h00m07s to 02h03m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=02h03m22s   !NEXT!

!* --- Scan from 02h03m22s to 02h05m40s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=02h05m40s   !NEXT!

!* --- Scan from 02h05m40s to 02h08m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=02h08m58s   !NEXT!

!* --- Scan from 02h10m58s to 02h13m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=02h10m58s   !NEXT!        
qual=  0
disk=on
stop=02h13m58s   !NEXT!

!* --- Scan from 02h13m58s to 02h17m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=02h17m14s   !NEXT!

!* --- Scan from 02h17m14s to 02h19m32s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=02h19m32s   !NEXT!

!* --- Scan from 02h19m32s to 02h22m50s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=02h22m50s   !NEXT!

!* --- Scan from 02h24m50s to 02h27m50s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=02h24m50s   !NEXT!        
qual=  0
disk=on
stop=02h27m50s   !NEXT!

!* --- Scan from 02h27m50s to 02h31m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=02h31m06s   !NEXT!

!* --- Scan from 02h31m06s to 02h33m24s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=02h33m24s   !NEXT!

!* --- Scan from 02h33m24s to 02h36m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=02h36m42s   !NEXT!

!* --- Scan from 02h38m42s to 02h41m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=02h38m42s   !NEXT!        
qual=  0
disk=on
stop=02h41m42s   !NEXT!

!* --- Scan from 02h41m42s to 02h44m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=02h44m57s   !NEXT!

!* --- Scan from 02h44m57s to 02h47m16s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=02h47m16s   !NEXT!

!* --- Scan from 02h47m16s to 02h50m34s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=02h50m34s   !NEXT!

!* --- Scan from 02h52m34s to 02h55m34s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=02h52m34s   !NEXT!        
qual=  0
disk=on
stop=02h55m34s   !NEXT!

!* --- Scan from 02h55m34s to 02h58m50s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=02h58m50s   !NEXT!

!* --- Scan from 02h58m50s to 03h01m08s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=03h01m08s   !NEXT!

!* --- Scan from 03h01m08s to 03h04m26s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=03h04m26s   !NEXT!

!* --- Scan from 03h06m26s to 03h09m26s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=03h06m26s   !NEXT!        
qual=  0
disk=on
stop=03h09m26s   !NEXT!

!* --- Scan from 03h09m26s to 03h12m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=03h12m42s   !NEXT!

!* --- Scan from 03h12m42s to 03h15m00s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=03h15m00s   !NEXT!

!* --- Scan from 03h15m00s to 03h18m19s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=03h18m19s   !NEXT!

!* --- Scan from 03h20m19s to 03h23m19s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=03h20m19s   !NEXT!        
qual=  0
disk=on
stop=03h23m19s   !NEXT!

!* --- Scan from 03h23m19s to 03h26m34s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=03h26m34s   !NEXT!

!* --- Scan from 03h26m34s to 03h28m53s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=03h28m53s   !NEXT!

!* --- Scan from 03h28m53s to 03h32m11s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=03h32m11s   !NEXT!

!* --- Scan from 03h34m11s to 03h37m11s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=03h34m11s   !NEXT!        
qual=  0
disk=on
stop=03h37m11s   !NEXT!

!* --- Scan from 03h37m11s to 03h40m27s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=03h40m27s   !NEXT!

!* --- Scan from 03h40m27s to 03h42m45s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=03h42m45s   !NEXT!

!* --- Scan from 03h42m45s to 03h46m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=03h46m04s   !NEXT!

!* --- Scan from 03h48m04s to 03h51m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=03h48m04s   !NEXT!        
qual=  0
disk=on
stop=03h51m04s   !NEXT!

!* --- Scan from 03h51m04s to 03h54m19s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=03h54m19s   !NEXT!

!* --- Scan from 03h54m19s to 03h56m38s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=03h56m38s   !NEXT!

!* --- Scan from 03h56m38s to 03h59m56s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=03h59m56s   !NEXT!

!* --- Scan from 04h01m56s to 04h04m56s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=04h01m56s   !NEXT!        
qual=  0
disk=on
stop=04h04m56s   !NEXT!

!* --- Scan from 04h04m56s to 04h08m12s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=04h08m12s   !NEXT!

!* --- Scan from 04h08m12s to 04h10m31s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=04h10m31s   !NEXT!

!* --- Scan from 04h10m31s to 04h13m49s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=04h13m49s   !NEXT!

!* --- Scan from 04h15m49s to 04h18m49s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=04h15m49s   !NEXT!        
qual=  0
disk=on
stop=04h18m49s   !NEXT!

!* --- Scan from 04h18m49s to 04h22m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=04h22m04s   !NEXT!

!* --- Scan from 04h22m04s to 04h24m23s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=04h24m23s   !NEXT!

!* --- Scan from 04h24m23s to 04h27m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=04h27m42s   !NEXT!

!* --- Scan from 04h29m42s to 04h32m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=04h29m42s   !NEXT!        
qual=  0
disk=on
stop=04h32m42s   !NEXT!

!* --- Scan from 04h32m42s to 04h35m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=04h35m57s   !NEXT!

!* --- Scan from 04h35m57s to 04h38m16s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=04h38m16s   !NEXT!

!* --- Scan from 04h38m16s to 04h41m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=04h41m35s   !NEXT!

!* --- Scan from 04h43m35s to 04h46m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=04h43m35s   !NEXT!        
qual=  0
disk=on
stop=04h46m35s   !NEXT!

!* --- Scan from 04h46m35s to 04h49m50s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=04h49m50s   !NEXT!

!* --- Scan from 04h49m50s to 04h52m09s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=04h52m09s   !NEXT!

!* --- Scan from 04h52m09s to 04h55m28s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=04h55m28s   !NEXT!

!* --- Scan from 04h57m28s to 05h00m28s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=04h57m28s   !NEXT!        
qual=  0
disk=on
stop=05h00m28s   !NEXT!

!* --- Scan from 05h00m28s to 05h03m43s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=05h03m43s   !NEXT!

!* --- Scan from 05h03m43s to 05h06m02s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=05h06m02s   !NEXT!

!* --- Scan from 05h06m02s to 05h09m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=05h09m21s   !NEXT!

!* --- Scan from 05h11m21s to 05h14m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=05h11m21s   !NEXT!        
qual=  0
disk=on
stop=05h14m21s   !NEXT!

!* --- Scan from 05h14m21s to 05h17m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=05h17m36s   !NEXT!

!* --- Scan from 05h17m36s to 05h19m55s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=05h19m55s   !NEXT!

!* --- Scan from 05h19m55s to 05h23m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=05h23m14s   !NEXT!

!* --- Scan from 05h25m14s to 05h28m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=05h25m14s   !NEXT!        
qual=  0
disk=on
stop=05h28m14s   !NEXT!

!* --- Scan from 05h28m14s to 05h31m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=05h31m29s   !NEXT!

!* --- Scan from 05h31m29s to 05h33m48s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=05h33m48s   !NEXT!

!* --- Scan from 05h33m48s to 05h37m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=05h37m07s   !NEXT!

!* --- Scan from 05h39m07s to 05h42m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=05h39m07s   !NEXT!        
qual=  0
disk=on
stop=05h42m07s   !NEXT!

!* --- Scan from 05h42m07s to 05h45m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=05h45m22s   !NEXT!

!* --- Scan from 05h45m22s to 05h47m42s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=05h47m42s   !NEXT!

!* --- Scan from 05h47m42s to 05h51m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=05h51m00s   !NEXT!

!* --- Scan from 05h53m00s to 05h56m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=05h53m00s   !NEXT!        
qual=  0
disk=on
stop=05h56m00s   !NEXT!

!* --- Scan from 05h56m00s to 05h59m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=05h59m16s   !NEXT!

!* --- Scan from 05h59m51s to 06h01m51s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=05h59m51s   !NEXT!        
qual=  0
disk=on
stop=06h01m51s   !NEXT!

!* --- Scan from 06h02m24s to 06h05m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=06h02m24s   !NEXT!        
qual=  0
disk=on
stop=06h05m24s   !NEXT!

!* --- Scan from 06h07m24s to 06h10m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=06h07m24s   !NEXT!        
qual=  0
disk=on
stop=06h10m24s   !NEXT!

!* --- Scan from 06h10m24s to 06h13m40s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=06h13m40s   !NEXT!

!* --- Scan from 06h13m40s to 06h15m59s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=06h15m59s   !NEXT!

!* --- Scan from 06h15m59s to 06h19m18s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=06h19m18s   !NEXT!

!* --- Scan from 06h21m18s to 06h24m18s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=06h21m18s   !NEXT!        
qual=  0
disk=on
stop=06h24m18s   !NEXT!

!* --- Scan from 06h24m18s to 06h27m33s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=06h27m33s   !NEXT!

!* --- Scan from 06h27m33s to 06h29m52s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=06h29m52s   !NEXT!

!* --- Scan from 06h29m52s to 06h33m11s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=06h33m11s   !NEXT!

!* --- Scan from 06h35m11s to 06h38m11s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=06h35m11s   !NEXT!        
qual=  0
disk=on
stop=06h38m11s   !NEXT!

!* --- Scan from 06h38m11s to 06h41m26s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=06h41m26s   !NEXT!

!* --- Scan from 06h41m26s to 06h43m45s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=06h43m45s   !NEXT!

!* --- Scan from 06h43m45s to 06h47m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=06h47m04s   !NEXT!

!* --- Scan from 06h49m04s to 06h52m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=06h49m04s   !NEXT!        
qual=  0
disk=on
stop=06h52m04s   !NEXT!

!* --- Scan from 06h52m04s to 06h55m20s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=06h55m20s   !NEXT!

!* --- Scan from 06h55m20s to 06h57m39s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=06h57m39s   !NEXT!

!* --- Scan from 06h57m39s to 07h00m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=07h00m58s   !NEXT!

!* --- Scan from 07h02m58s to 07h05m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=07h02m58s   !NEXT!        
qual=  0
disk=on
stop=07h05m58s   !NEXT!

!* --- Scan from 07h05m58s to 07h09m13s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=07h09m13s   !NEXT!

!* --- Scan from 07h09m13s to 07h11m32s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=07h11m32s   !NEXT!

!* --- Scan from 07h11m32s to 07h14m51s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=07h14m51s   !NEXT!

!* --- Scan from 07h16m51s to 07h19m51s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=07h16m51s   !NEXT!        
qual=  0
disk=on
stop=07h19m51s   !NEXT!

!* --- Scan from 07h19m51s to 07h23m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=07h23m06s   !NEXT!

!* --- Scan from 07h24m32s to 07h26m32s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=07h24m32s   !NEXT!        
qual=  0
disk=on
stop=07h26m32s   !NEXT!

!* --- Scan from 07h26m32s to 07h29m51s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=07h29m51s   !NEXT!

!* --- Scan from 07h31m51s to 07h34m51s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=07h31m51s   !NEXT!        
qual=  0
disk=on
stop=07h34m51s   !NEXT!

!* --- Scan from 07h34m51s to 07h38m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=07h38m06s   !NEXT!

!* --- Scan from 07h38m06s to 07h40m26s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=07h40m26s   !NEXT!

!* --- Scan from 07h40m26s to 07h43m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=07h43m45s   !NEXT!

!* --- Scan from 07h45m45s to 07h48m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=07h45m45s   !NEXT!        
qual=  0
disk=on
stop=07h48m45s   !NEXT!

!* --- Scan from 07h48m45s to 07h52m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=07h52m01s   !NEXT!

!* --- Scan from 07h52m24s to 07h54m24s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=07h52m24s   !NEXT!        
qual=  0
disk=on
stop=07h54m24s   !NEXT!

!* --- Scan from 07h54m46s to 07h57m46s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=07h54m46s   !NEXT!        
qual=  0
disk=on
stop=07h57m46s   !NEXT!

!* --- Scan from 07h59m46s to 08h02m46s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=07h59m46s   !NEXT!        
qual=  0
disk=on
stop=08h02m46s   !NEXT!

!* --- Scan from 08h02m46s to 08h06m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=08h06m01s   !NEXT!

!* --- Scan from 08h06m28s to 08h08m28s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=08h06m28s   !NEXT!        
qual=  0
disk=on
stop=08h08m28s   !NEXT!

!* --- Scan from 08h08m54s to 08h11m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=08h08m54s   !NEXT!        
qual=  0
disk=on
stop=08h11m54s   !NEXT!

!* --- Scan from 08h13m54s to 08h16m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=08h13m54s   !NEXT!        
qual=  0
disk=on
stop=08h16m54s   !NEXT!

!* --- Scan from 08h18m57s to 08h21m57s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=08h18m57s   !NEXT!        
qual=  0
disk=on
stop=08h21m57s   !NEXT!

!* --- Scan from 08h21m57s to 08h24m12s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=08h24m12s   !NEXT!

!* --- Scan from 08h26m14s to 08h29m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=08h26m14s   !NEXT!        
qual=  0
disk=on
stop=08h29m14s   !NEXT!

!* --- Scan from 08h31m14s to 08h34m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=08h31m14s   !NEXT!        
qual=  0
disk=on
stop=08h34m14s   !NEXT!

!* --- Scan from 08h34m14s to 08h37m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=08h37m30s   !NEXT!

!* --- Scan from 08h38m00s to 08h40m00s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=08h38m00s   !NEXT!        
qual=  0
disk=on
stop=08h40m00s   !NEXT!

!* --- Scan from 08h40m28s to 08h43m28s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=08h40m28s   !NEXT!        
qual=  0
disk=on
stop=08h43m28s   !NEXT!

!* --- Scan from 08h45m28s to 08h48m28s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=08h45m28s   !NEXT!        
qual=  0
disk=on
stop=08h48m28s   !NEXT!

!* --- Scan from 08h51m05s to 08h54m05s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=08h51m05s   !NEXT!        
qual=  0
disk=on
stop=08h54m05s   !NEXT!

!* --- Scan from 08h54m05s to 08h56m20s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=08h56m20s   !NEXT!

!* --- Scan from 08h59m12s to 09h02m12s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=08h59m12s   !NEXT!        
qual=  0
disk=on
stop=09h02m12s   !NEXT!

!* --- Scan from 09h04m12s to 09h07m12s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=09h04m12s   !NEXT!        
qual=  0
disk=on
stop=09h07m12s   !NEXT!

!* --- Scan from 09h07m12s to 09h10m28s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=09h10m28s   !NEXT!

!* --- Scan from 09h10m50s to 09h12m50s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=999  calib='V'
disk=off
stop=09h10m50s   !NEXT!        
qual=  0
disk=on
stop=09h12m50s   !NEXT!

!* --- Scan from 09h13m11s to 09h16m11s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=09h13m11s   !NEXT!        
qual=  0
disk=on
stop=09h16m11s   !NEXT!

!* --- Scan from 09h18m11s to 09h21m11s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=09h18m11s   !NEXT!        
qual=  0
disk=on
stop=09h21m11s   !NEXT!

!* --- Scan from 09h24m22s to 09h27m22s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=09h24m22s   !NEXT!        
qual=  0
disk=on
stop=09h27m22s   !NEXT!

!* --- Scan from 09h27m22s to 09h29m38s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=09h29m38s   !NEXT!

!* --- Scan from 09h32m55s to 09h35m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=09h32m55s   !NEXT!        
qual=  0
disk=on
stop=09h35m55s   !NEXT!

!* --- Scan from 09h37m55s to 09h40m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=09h37m55s   !NEXT!        
qual=  0
disk=on
stop=09h40m55s   !NEXT!

!* --- Scan from 09h40m55s to 09h44m10s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=09h44m10s   !NEXT!

!* --- Scan from 09h44m10s to 09h46m29s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=09h46m29s   !NEXT!

!* --- Scan from 09h46m29s to 09h49m47s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=09h49m47s   !NEXT!

!* --- Scan from 09h51m47s to 09h54m47s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=09h51m47s   !NEXT!        
qual=  0
disk=on
stop=09h54m47s   !NEXT!

!* --- Scan from 09h58m05s to 10h01m05s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=09h58m05s   !NEXT!        
qual=  0
disk=on
stop=10h01m05s   !NEXT!

!* --- Scan from 10h01m05s to 10h03m21s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=10h03m21s   !NEXT!

!* --- Scan from 10h06m39s to 10h09m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=10h06m39s   !NEXT!        
qual=  0
disk=on
stop=10h09m39s   !NEXT!

!* --- Scan from 10h11m39s to 10h14m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=10h11m39s   !NEXT!        
qual=  0
disk=on
stop=10h14m39s   !NEXT!

!* --- Scan from 10h14m39s to 10h17m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=10h17m54s   !NEXT!

!* --- Scan from 10h17m54s to 10h20m13s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=10h20m13s   !NEXT!

!* --- Scan from 10h20m13s to 10h23m32s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=10h23m32s   !NEXT!

!* --- Scan from 10h25m32s to 10h28m32s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=10h25m32s   !NEXT!        
qual=  0
disk=on
stop=10h28m32s   !NEXT!

!* --- Scan from 10h31m32s to 10h34m32s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=10h31m32s   !NEXT!        
qual=  0
disk=on
stop=10h34m32s   !NEXT!

!* --- Scan from 10h34m32s to 10h36m48s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=10h36m48s   !NEXT!

!* --- Scan from 10h39m42s to 10h42m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=10h39m42s   !NEXT!        
qual=  0
disk=on
stop=10h42m42s   !NEXT!

!* --- Scan from 10h44m42s to 10h47m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=10h44m42s   !NEXT!        
qual=  0
disk=on
stop=10h47m42s   !NEXT!

!* --- Scan from 10h47m42s to 10h50m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=10h50m57s   !NEXT!

!* --- Scan from 10h50m57s to 10h53m16s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=10h53m16s   !NEXT!

!* --- Scan from 10h53m16s to 10h56m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=10h56m35s   !NEXT!

!* --- Scan from 10h58m35s to 11h01m35s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=10h58m35s   !NEXT!        
qual=  0
disk=on
stop=11h01m35s   !NEXT!

!* --- Scan from 11h03m29s to 11h06m29s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=11h03m29s   !NEXT!        
qual=  0
disk=on
stop=11h06m29s   !NEXT!

!* --- Scan from 11h06m29s to 11h08m45s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=11h08m45s   !NEXT!

!* --- Scan from 11h10m24s to 11h13m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=11h10m24s   !NEXT!        
qual=  0
disk=on
stop=11h13m24s   !NEXT!

!* --- Scan from 11h15m24s to 11h18m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=11h15m24s   !NEXT!        
qual=  0
disk=on
stop=11h18m24s   !NEXT!

!* --- Scan from 11h18m24s to 11h21m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=11h21m39s   !NEXT!

!* --- Scan from 11h21m39s to 11h23m58s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=11h23m58s   !NEXT!

!* --- Scan from 11h23m58s to 11h27m17s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=11h27m17s   !NEXT!

!* --- Scan from 11h29m17s to 11h32m17s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=11h29m17s   !NEXT!        
qual=  0
disk=on
stop=11h32m17s   !NEXT!

!* --- Scan from 11h33m57s to 11h36m57s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=11h33m57s   !NEXT!        
qual=  0
disk=on
stop=11h36m57s   !NEXT!

!* --- Scan from 11h36m57s to 11h39m13s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=11h39m13s   !NEXT!

!* --- Scan from 11h40m55s to 11h43m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=11h40m55s   !NEXT!        
qual=  0
disk=on
stop=11h43m55s   !NEXT!

!* --- Scan from 11h45m55s to 11h48m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=11h45m55s   !NEXT!        
qual=  0
disk=on
stop=11h48m55s   !NEXT!

!* --- Scan from 11h48m55s to 11h52m11s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=11h52m11s   !NEXT!

!* --- Scan from 11h52m11s to 11h54m30s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=11h54m30s   !NEXT!

!* --- Scan from 11h54m30s to 11h57m49s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=11h57m49s   !NEXT!

!* --- Scan from 11h59m49s to 12h02m49s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=11h59m49s   !NEXT!        
qual=  0
disk=on
stop=12h02m49s   !NEXT!

!* --- Scan from 12h04m31s to 12h07m31s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=12h04m31s   !NEXT!        
qual=  0
disk=on
stop=12h07m31s   !NEXT!

!* --- Scan from 12h07m31s to 12h09m46s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=12h09m46s   !NEXT!

!* --- Scan from 12h11m30s to 12h14m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=12h11m30s   !NEXT!        
qual=  0
disk=on
stop=12h14m30s   !NEXT!

!* --- Scan from 12h16m30s to 12h19m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=12h16m30s   !NEXT!        
qual=  0
disk=on
stop=12h19m30s   !NEXT!

!* --- Scan from 12h19m30s to 12h22m46s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=12h22m46s   !NEXT!

!* --- Scan from 12h22m46s to 12h25m05s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=12h25m05s   !NEXT!

!* --- Scan from 12h25m05s to 12h28m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=12h28m23s   !NEXT!

!* --- Scan from 12h30m23s to 12h33m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=12h30m23s   !NEXT!        
qual=  0
disk=on
stop=12h33m23s   !NEXT!

!* --- Scan from 12h35m05s to 12h38m05s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=12h35m05s   !NEXT!        
qual=  0
disk=on
stop=12h38m05s   !NEXT!

!* --- Scan from 12h38m05s to 12h40m21s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=12h40m21s   !NEXT!

!* --- Scan from 12h42m05s to 12h45m05s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=12h42m05s   !NEXT!        
qual=  0
disk=on
stop=12h45m05s   !NEXT!

!* --- Scan from 12h47m05s to 12h50m05s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=12h47m05s   !NEXT!        
qual=  0
disk=on
stop=12h50m05s   !NEXT!

!* --- Scan from 12h50m05s to 12h53m20s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=12h53m20s   !NEXT!

!* --- Scan from 12h53m20s to 12h55m39s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=12h55m39s   !NEXT!

!* --- Scan from 12h55m39s to 12h58m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=12h58m58s   !NEXT!

!* --- Scan from 13h00m58s to 13h03m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=13h00m58s   !NEXT!        
qual=  0
disk=on
stop=13h03m58s   !NEXT!

!* --- Scan from 13h05m39s to 13h08m39s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=13h05m39s   !NEXT!        
qual=  0
disk=on
stop=13h08m39s   !NEXT!

!* --- Scan from 13h08m39s to 13h10m54s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=13h10m54s   !NEXT!

!* --- Scan from 13h12m37s to 13h15m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=13h12m37s   !NEXT!        
qual=  0
disk=on
stop=13h15m37s   !NEXT!

!* --- Scan from 13h17m37s to 13h20m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=13h17m37s   !NEXT!        
qual=  0
disk=on
stop=13h20m37s   !NEXT!

!* --- Scan from 13h20m37s to 13h23m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=13h23m52s   !NEXT!

!* --- Scan from 13h23m52s to 13h26m11s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=13h26m11s   !NEXT!

!* --- Scan from 13h26m11s to 13h29m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=13h29m30s   !NEXT!

!* --- Scan from 13h31m30s to 13h34m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=13h31m30s   !NEXT!        
qual=  0
disk=on
stop=13h34m30s   !NEXT!

!* --- Scan from 13h36m10s to 13h39m10s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=13h36m10s   !NEXT!        
qual=  0
disk=on
stop=13h39m10s   !NEXT!

!* --- Scan from 13h39m10s to 13h41m25s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=13h41m25s   !NEXT!

!* --- Scan from 13h43m07s to 13h46m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=13h43m07s   !NEXT!        
qual=  0
disk=on
stop=13h46m07s   !NEXT!

!* --- Scan from 13h48m07s to 13h51m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=13h48m07s   !NEXT!        
qual=  0
disk=on
stop=13h51m07s   !NEXT!

!* --- Scan from 13h51m07s to 13h54m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=13h54m22s   !NEXT!

!* --- Scan from 13h54m22s to 13h56m41s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=13h56m41s   !NEXT!

!* --- Scan from 13h56m41s to 14h00m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=14h00m00s   !NEXT!

!* --- Scan from 14h02m00s to 14h05m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=14h02m00s   !NEXT!        
qual=  0
disk=on
stop=14h05m00s   !NEXT!

!* --- Scan from 14h06m38s to 14h09m38s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=14h06m38s   !NEXT!        
qual=  0
disk=on
stop=14h09m38s   !NEXT!

!* --- Scan from 14h09m38s to 14h11m53s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=14h11m53s   !NEXT!

!* --- Scan from 14h13m33s to 14h16m33s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=14h13m33s   !NEXT!        
qual=  0
disk=on
stop=14h16m33s   !NEXT!

!* --- Scan from 14h18m33s to 14h21m33s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=14h18m33s   !NEXT!        
qual=  0
disk=on
stop=14h21m33s   !NEXT!

!* --- Scan from 14h21m33s to 14h24m48s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=14h24m48s   !NEXT!

!* --- Scan from 14h24m48s to 14h27m07s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=14h27m07s   !NEXT!

!* --- Scan from 14h27m07s to 14h30m26s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=14h30m26s   !NEXT!

!* --- Scan from 14h32m26s to 14h35m26s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=14h32m26s   !NEXT!        
qual=  0
disk=on
stop=14h35m26s   !NEXT!

!* --- Scan from 14h37m02s to 14h40m02s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=14h37m02s   !NEXT!        
qual=  0
disk=on
stop=14h40m02s   !NEXT!

!* --- Scan from 14h40m02s to 14h42m17s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=14h42m17s   !NEXT!

!* --- Scan from 14h43m55s to 14h46m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=14h43m55s   !NEXT!        
qual=  0
disk=on
stop=14h46m55s   !NEXT!

!* --- Scan from 14h48m55s to 14h51m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=14h48m55s   !NEXT!        
qual=  0
disk=on
stop=14h51m55s   !NEXT!

!* --- Scan from 14h51m55s to 14h55m10s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=14h55m10s   !NEXT!

!* --- Scan from 14h55m10s to 14h57m29s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=14h57m29s   !NEXT!

!* --- Scan from 14h57m29s to 15h00m48s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=15h00m48s   !NEXT!

!* --- Scan from 15h02m48s to 15h05m48s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=15h02m48s   !NEXT!        
qual=  0
disk=on
stop=15h05m48s   !NEXT!

!* --- Scan from 15h07m22s to 15h10m22s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=15h07m22s   !NEXT!        
qual=  0
disk=on
stop=15h10m22s   !NEXT!

!* --- Scan from 15h10m22s to 15h12m37s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=15h12m37s   !NEXT!

!* --- Scan from 15h14m12s to 15h17m12s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=15h14m12s   !NEXT!        
qual=  0
disk=on
stop=15h17m12s   !NEXT!

!* --- Scan from 15h19m12s to 15h22m12s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=15h19m12s   !NEXT!        
qual=  0
disk=on
stop=15h22m12s   !NEXT!

!* --- Scan from 15h22m12s to 15h25m27s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=15h25m27s   !NEXT!

!* --- Scan from 15h25m27s to 15h27m46s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962127s  dec= 41d20'01.18344"  qual=  0  calib='V'
disk=on
stop=15h27m46s   !NEXT!

!* --- Scan from 15h27m46s to 15h31m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
disk=on
stop=15h31m04s   !NEXT!

!* --- Scan from 15h33m04s to 15h36m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=15h33m04s   !NEXT!        
qual=  0
disk=on
stop=15h36m04s   !NEXT!

!* --- Scan from 15h37m36s to 15h40m36s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
disk=off
stop=15h37m36s   !NEXT!        
qual=  0
disk=on
stop=15h40m36s   !NEXT!

!* --- Scan from 15h40m36s to 15h42m51s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=15h42m51s   !NEXT!

!* --- Scan from 15h44m23s to 15h47m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
disk=off
stop=15h44m23s   !NEXT!        
qual=  0
disk=on
stop=15h47m23s   !NEXT!

!* --- Scan from 15h49m23s to 15h52m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=15h49m23s   !NEXT!        
qual=  0
disk=on
stop=15h52m23s   !NEXT!

!* --- Scan from 15h52m23s to 15h55m28s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10389"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=15h55m28s   !NEXT!

!* --- Scan from 16h03m45s to 16h09m15s   Sun, 1995 Oct 22 --- *!
sname='3C273'  ra=12h29m06.699732s  dec= 02d03'08.59815"  qual=999  calib='V'
extlo = (1,  4.3601000000)
extlosideband = (1,U)
extlo = (3,  4.3601000000)
extlosideband = (3,U)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,622.39),( 2,622.39),( 3,630.39),( 4,630.39)
disk=off
stop=16h03m45s   !NEXT!        
qual=  0
disk=on
stop=16h09m15s   !NEXT!

!* --- Scan from 16h09m15s to 16h15m00s   Sun, 1995 Oct 22 --- *!
sname='3C273'  ra=12h29m06.699732s  dec= 02d03'08.59815"  qual=  0  calib='V'
extlo = (1,  9.0399000000)
extlosideband = (1,L)
extlo = (3,  9.0399000000)
extlosideband = (3,L)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,626.41),( 2,626.41),( 3,618.41),( 4,618.41)
disk=on
stop=16h15m00s   !NEXT!
disk=off
stop=16h15m05s   !NEXT!
     !QUIT! 
