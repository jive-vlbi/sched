!*  Schedule for VLA27     *!
!*  Experiment CE001    *!
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
program=CE001   

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 01h30m00s to 01h35m30s   Sun, 1995 Oct 22 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
maxcaltime= 120
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
logging=STANDARD
nchan= 4
format=NONE
barrel=roll_off
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bits=(1,1),(2,1),(3,1),(4,1)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S3),(7,S1),(8,S3)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,S2),(6,S4),(7,S2),(8,S4)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,1510),(8,1510)
  date = 1995Oct22
stop=01h30m00s   !NEXT!        
qual=  0
stop=01h35m30s   !NEXT!

!* --- Scan from 01h35m45s to 01h41m15s   Sun, 1995 Oct 22 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=01h41m15s   !NEXT!

!* --- Scan from 01h43m15s to 01h46m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=01h46m15s   !NEXT!

!* --- Scan from 01h46m30s to 01h49m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=01h49m30s   !NEXT!

!* --- Scan from 01h49m48s to 01h51m48s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=01h51m48s   !NEXT!

!* --- Scan from 01h52m06s to 01h55m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=01h55m06s   !NEXT!

!* --- Scan from 01h57m06s to 02h00m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=02h00m06s   !NEXT!

!* --- Scan from 02h00m21s to 02h03m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=02h03m21s   !NEXT!

!* --- Scan from 02h03m39s to 02h05m39s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=02h05m39s   !NEXT!

!* --- Scan from 02h05m57s to 02h08m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=02h08m57s   !NEXT!

!* --- Scan from 02h10m57s to 02h13m57s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=02h13m57s   !NEXT!

!* --- Scan from 02h14m13s to 02h17m13s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=02h17m13s   !NEXT!

!* --- Scan from 02h17m31s to 02h19m31s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=02h19m31s   !NEXT!

!* --- Scan from 02h19m49s to 02h22m49s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=02h22m49s   !NEXT!

!* --- Scan from 02h24m49s to 02h27m49s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=02h27m49s   !NEXT!

!* --- Scan from 02h28m04s to 02h31m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=02h31m04s   !NEXT!

!* --- Scan from 02h31m22s to 02h33m22s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=02h33m22s   !NEXT!

!* --- Scan from 02h33m40s to 02h36m40s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=02h36m40s   !NEXT!

!* --- Scan from 02h38m40s to 02h41m40s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=02h41m40s   !NEXT!

!* --- Scan from 02h41m55s to 02h44m55s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=02h44m55s   !NEXT!

!* --- Scan from 02h45m14s to 02h47m14s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=02h47m14s   !NEXT!

!* --- Scan from 02h47m32s to 02h50m32s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=02h50m32s   !NEXT!

!* --- Scan from 02h52m32s to 02h55m32s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=02h55m32s   !NEXT!

!* --- Scan from 02h55m47s to 02h58m47s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=02h58m47s   !NEXT!

!* --- Scan from 02h59m05s to 03h01m05s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=03h01m05s   !NEXT!

!* --- Scan from 03h01m24s to 03h04m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=03h04m24s   !NEXT!

!* --- Scan from 03h06m24s to 03h09m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=03h09m24s   !NEXT!

!* --- Scan from 03h09m39s to 03h12m39s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=03h12m39s   !NEXT!

!* --- Scan from 03h12m57s to 03h14m57s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=03h14m57s   !NEXT!

!* --- Scan from 03h15m15s to 03h18m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=03h18m15s   !NEXT!

!* --- Scan from 03h20m15s to 03h23m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=03h23m15s   !NEXT!

!* --- Scan from 03h23m31s to 03h26m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=03h26m31s   !NEXT!

!* --- Scan from 03h26m49s to 03h28m49s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=03h28m49s   !NEXT!

!* --- Scan from 03h29m07s to 03h32m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=03h32m07s   !NEXT!

!* --- Scan from 03h34m07s to 03h37m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=03h37m07s   !NEXT!

!* --- Scan from 03h37m23s to 03h40m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=03h40m23s   !NEXT!

!* --- Scan from 03h40m41s to 03h42m41s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=03h42m41s   !NEXT!

!* --- Scan from 03h43m00s to 03h46m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=03h46m00s   !NEXT!

!* --- Scan from 03h48m00s to 03h51m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=03h51m00s   !NEXT!

!* --- Scan from 03h51m15s to 03h54m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=03h54m15s   !NEXT!

!* --- Scan from 03h54m33s to 03h56m33s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=03h56m33s   !NEXT!

!* --- Scan from 03h56m52s to 03h59m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=03h59m52s   !NEXT!

!* --- Scan from 04h01m52s to 04h04m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=04h04m52s   !NEXT!

!* --- Scan from 04h05m07s to 04h08m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=04h08m07s   !NEXT!

!* --- Scan from 04h08m26s to 04h10m26s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=04h10m26s   !NEXT!

!* --- Scan from 04h10m44s to 04h13m44s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=04h13m44s   !NEXT!

!* --- Scan from 04h15m44s to 04h18m44s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=04h18m44s   !NEXT!

!* --- Scan from 04h18m59s to 04h21m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=04h21m59s   !NEXT!

!* --- Scan from 04h22m18s to 04h24m18s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=04h24m18s   !NEXT!

!* --- Scan from 04h24m36s to 04h27m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=04h27m36s   !NEXT!

!* --- Scan from 04h29m36s to 04h32m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=04h32m36s   !NEXT!

!* --- Scan from 04h32m52s to 04h35m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=04h35m52s   !NEXT!

!* --- Scan from 04h36m10s to 04h38m10s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=04h38m10s   !NEXT!

!* --- Scan from 04h38m29s to 04h41m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=04h41m29s   !NEXT!

!* --- Scan from 04h43m29s to 04h46m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=04h46m29s   !NEXT!

!* --- Scan from 04h46m44s to 04h49m44s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=04h49m44s   !NEXT!

!* --- Scan from 04h50m03s to 04h52m03s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=04h52m03s   !NEXT!

!* --- Scan from 04h52m21s to 04h55m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=04h55m21s   !NEXT!

!* --- Scan from 04h57m21s to 05h00m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=05h00m21s   !NEXT!

!* --- Scan from 05h00m36s to 05h03m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=05h03m36s   !NEXT!

!* --- Scan from 05h03m55s to 05h05m55s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=05h05m55s   !NEXT!

!* --- Scan from 05h06m14s to 05h09m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=05h09m14s   !NEXT!

!* --- Scan from 05h11m14s to 05h14m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=05h14m14s   !NEXT!

!* --- Scan from 05h14m29s to 05h17m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=05h17m29s   !NEXT!

!* --- Scan from 05h17m48s to 05h19m48s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=05h19m48s   !NEXT!

!* --- Scan from 05h20m07s to 05h23m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=05h23m07s   !NEXT!

!* --- Scan from 05h25m07s to 05h28m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=05h28m07s   !NEXT!

!* --- Scan from 05h28m22s to 05h31m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=05h31m22s   !NEXT!

!* --- Scan from 05h31m41s to 05h33m41s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=05h33m41s   !NEXT!

!* --- Scan from 05h33m59s to 05h36m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=05h36m59s   !NEXT!

!* --- Scan from 05h38m59s to 05h41m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=05h41m59s   !NEXT!

!* --- Scan from 05h42m14s to 05h45m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=05h45m14s   !NEXT!

!* --- Scan from 05h45m33s to 05h47m33s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=05h47m33s   !NEXT!

!* --- Scan from 05h47m52s to 05h50m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=05h50m52s   !NEXT!

!* --- Scan from 05h52m52s to 05h55m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=05h55m52s   !NEXT!

!* --- Scan from 05h56m07s to 05h59m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=05h59m07s   !NEXT!

!* --- Scan from 05h59m26s to 06h01m26s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=06h01m26s   !NEXT!

!* --- Scan from 06h01m45s to 06h04m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=06h04m45s   !NEXT!

!* --- Scan from 06h06m45s to 06h09m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=06h09m45s   !NEXT!

!* --- Scan from 06h10m00s to 06h13m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=06h13m00s   !NEXT!

!* --- Scan from 06h13m19s to 06h15m19s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=06h15m19s   !NEXT!

!* --- Scan from 06h15m38s to 06h18m38s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=06h18m38s   !NEXT!

!* --- Scan from 06h20m38s to 06h23m38s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=06h23m38s   !NEXT!

!* --- Scan from 06h23m53s to 06h26m53s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=06h26m53s   !NEXT!

!* --- Scan from 06h27m12s to 06h29m12s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=06h29m12s   !NEXT!

!* --- Scan from 06h29m31s to 06h32m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=06h32m31s   !NEXT!

!* --- Scan from 06h34m31s to 06h37m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=06h37m31s   !NEXT!

!* --- Scan from 06h37m46s to 06h40m46s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=06h40m46s   !NEXT!

!* --- Scan from 06h41m05s to 06h43m05s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=06h43m05s   !NEXT!

!* --- Scan from 06h43m23s to 06h46m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=06h46m23s   !NEXT!

!* --- Scan from 06h48m23s to 06h51m23s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=06h51m23s   !NEXT!

!* --- Scan from 06h51m38s to 06h54m38s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=06h54m38s   !NEXT!

!* --- Scan from 06h54m57s to 06h56m57s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=06h56m57s   !NEXT!

!* --- Scan from 06h57m16s to 07h00m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=07h00m16s   !NEXT!

!* --- Scan from 07h02m16s to 07h05m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=07h05m16s   !NEXT!

!* --- Scan from 07h05m31s to 07h08m31s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=07h08m31s   !NEXT!

!* --- Scan from 07h08m50s to 07h10m50s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=07h10m50s   !NEXT!

!* --- Scan from 07h11m09s to 07h14m09s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=07h14m09s   !NEXT!

!* --- Scan from 07h16m09s to 07h19m09s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=07h19m09s   !NEXT!

!* --- Scan from 07h19m24s to 07h22m24s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=07h22m24s   !NEXT!

!* --- Scan from 07h22m43s to 07h24m43s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=07h24m43s   !NEXT!

!* --- Scan from 07h25m01s to 07h28m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=07h28m01s   !NEXT!

!* --- Scan from 07h30m01s to 07h33m01s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=07h33m01s   !NEXT!

!* --- Scan from 07h33m16s to 07h36m16s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=07h36m16s   !NEXT!

!* --- Scan from 07h36m35s to 07h38m35s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=07h38m35s   !NEXT!

!* --- Scan from 07h38m54s to 07h41m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=07h41m54s   !NEXT!

!* --- Scan from 07h43m54s to 07h46m54s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=07h46m54s   !NEXT!

!* --- Scan from 07h47m10s to 07h50m10s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=07h50m10s   !NEXT!

!* --- Scan from 07h50m31s to 07h52m31s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=07h52m31s   !NEXT!

!* --- Scan from 07h52m53s to 07h55m53s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=07h55m53s   !NEXT!

!* --- Scan from 07h57m53s to 08h00m53s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=08h00m53s   !NEXT!

!* --- Scan from 08h01m08s to 08h04m08s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=08h04m08s   !NEXT!

!* --- Scan from 08h04m34s to 08h06m34s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=08h06m34s   !NEXT!

!* --- Scan from 08h06m59s to 08h09m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=08h09m59s   !NEXT!

!* --- Scan from 08h11m59s to 08h14m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=08h14m59s   !NEXT!

!* --- Scan from 08h16m32s to 08h19m32s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=08h19m32s   !NEXT!

!* --- Scan from 08h19m47s to 08h21m47s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=08h21m47s   !NEXT!

!* --- Scan from 08h23m32s to 08h26m32s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=08h26m32s   !NEXT!

!* --- Scan from 08h28m32s to 08h31m32s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=08h31m32s   !NEXT!

!* --- Scan from 08h31m48s to 08h34m48s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=08h34m48s   !NEXT!

!* --- Scan from 08h35m18s to 08h37m18s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=08h37m18s   !NEXT!

!* --- Scan from 08h37m46s to 08h40m46s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=08h40m46s   !NEXT!

!* --- Scan from 08h42m46s to 08h45m46s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=08h45m46s   !NEXT!

!* --- Scan from 08h48m18s to 08h51m18s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=08h51m18s   !NEXT!

!* --- Scan from 08h51m33s to 08h53m33s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=08h53m33s   !NEXT!

!* --- Scan from 08h56m21s to 08h59m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=08h59m21s   !NEXT!

!* --- Scan from 09h01m21s to 09h04m21s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=09h04m21s   !NEXT!

!* --- Scan from 09h04m36s to 09h07m36s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=09h07m36s   !NEXT!

!* --- Scan from 09h07m59s to 09h09m59s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=09h09m59s   !NEXT!

!* --- Scan from 09h10m20s to 09h13m20s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=09h13m20s   !NEXT!

!* --- Scan from 09h15m20s to 09h18m20s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=09h18m20s   !NEXT!

!* --- Scan from 09h21m29s to 09h24m29s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=09h24m29s   !NEXT!

!* --- Scan from 09h24m45s to 09h26m45s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=09h26m45s   !NEXT!

!* --- Scan from 09h30m00s to 09h33m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=09h33m00s   !NEXT!

!* --- Scan from 09h35m00s to 09h38m00s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=09h38m00s   !NEXT!

!* --- Scan from 09h38m15s to 09h41m15s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=09h41m15s   !NEXT!

!* --- Scan from 09h41m34s to 09h43m34s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=09h43m34s   !NEXT!

!* --- Scan from 09h43m52s to 09h46m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=09h46m52s   !NEXT!

!* --- Scan from 09h48m52s to 09h51m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=09h51m52s   !NEXT!

!* --- Scan from 09h55m10s to 09h58m10s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=09h58m10s   !NEXT!

!* --- Scan from 09h58m25s to 10h00m25s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=10h00m25s   !NEXT!

!* --- Scan from 10h03m44s to 10h06m44s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=10h06m44s   !NEXT!

!* --- Scan from 10h08m44s to 10h11m44s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=10h11m44s   !NEXT!

!* --- Scan from 10h11m59s to 10h14m59s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=10h14m59s   !NEXT!

!* --- Scan from 10h15m18s to 10h17m18s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=10h17m18s   !NEXT!

!* --- Scan from 10h17m37s to 10h20m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=10h20m37s   !NEXT!

!* --- Scan from 10h22m37s to 10h25m37s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=10h25m37s   !NEXT!

!* --- Scan from 10h28m39s to 10h31m39s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=10h31m39s   !NEXT!

!* --- Scan from 10h31m55s to 10h33m55s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=10h33m55s   !NEXT!

!* --- Scan from 10h36m52s to 10h39m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=10h39m52s   !NEXT!

!* --- Scan from 10h41m52s to 10h44m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=10h44m52s   !NEXT!

!* --- Scan from 10h45m07s to 10h48m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=10h48m07s   !NEXT!

!* --- Scan from 10h48m26s to 10h50m26s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=10h50m26s   !NEXT!

!* --- Scan from 10h50m45s to 10h53m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=10h53m45s   !NEXT!

!* --- Scan from 10h55m45s to 10h58m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=10h58m45s   !NEXT!

!* --- Scan from 11h00m46s to 11h03m46s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=11h03m46s   !NEXT!

!* --- Scan from 11h04m02s to 11h06m02s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=11h06m02s   !NEXT!

!* --- Scan from 11h07m48s to 11h10m48s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=11h10m48s   !NEXT!

!* --- Scan from 11h12m48s to 11h15m48s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=11h15m48s   !NEXT!

!* --- Scan from 11h16m04s to 11h19m04s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=11h19m04s   !NEXT!

!* --- Scan from 11h19m23s to 11h21m23s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=11h21m23s   !NEXT!

!* --- Scan from 11h21m41s to 11h24m41s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=11h24m41s   !NEXT!

!* --- Scan from 11h26m41s to 11h29m41s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=11h29m41s   !NEXT!

!* --- Scan from 11h31m21s to 11h34m21s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=11h34m21s   !NEXT!

!* --- Scan from 11h34m36s to 11h36m36s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=11h36m36s   !NEXT!

!* --- Scan from 11h38m19s to 11h41m19s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=11h41m19s   !NEXT!

!* --- Scan from 11h43m19s to 11h46m19s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=11h46m19s   !NEXT!

!* --- Scan from 11h46m34s to 11h49m34s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=11h49m34s   !NEXT!

!* --- Scan from 11h49m53s to 11h51m53s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=11h51m53s   !NEXT!

!* --- Scan from 11h52m11s to 11h55m11s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=11h55m11s   !NEXT!

!* --- Scan from 11h57m11s to 12h00m11s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=12h00m11s   !NEXT!

!* --- Scan from 12h01m53s to 12h04m53s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=12h04m53s   !NEXT!

!* --- Scan from 12h05m08s to 12h07m08s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=12h07m08s   !NEXT!

!* --- Scan from 12h08m52s to 12h11m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=12h11m52s   !NEXT!

!* --- Scan from 12h13m52s to 12h16m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=12h16m52s   !NEXT!

!* --- Scan from 12h17m07s to 12h20m07s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=12h20m07s   !NEXT!

!* --- Scan from 12h20m26s to 12h22m26s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=12h22m26s   !NEXT!

!* --- Scan from 12h22m45s to 12h25m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=12h25m45s   !NEXT!

!* --- Scan from 12h27m45s to 12h30m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=12h30m45s   !NEXT!

!* --- Scan from 12h32m27s to 12h35m27s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=12h35m27s   !NEXT!

!* --- Scan from 12h35m42s to 12h37m42s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=12h37m42s   !NEXT!

!* --- Scan from 12h39m26s to 12h42m26s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=12h42m26s   !NEXT!

!* --- Scan from 12h44m26s to 12h47m26s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=12h47m26s   !NEXT!

!* --- Scan from 12h47m41s to 12h50m41s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=12h50m41s   !NEXT!

!* --- Scan from 12h51m00s to 12h53m00s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=12h53m00s   !NEXT!

!* --- Scan from 12h53m19s to 12h56m19s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=12h56m19s   !NEXT!

!* --- Scan from 12h58m19s to 13h01m19s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=13h01m19s   !NEXT!

!* --- Scan from 13h03m00s to 13h06m00s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=13h06m00s   !NEXT!

!* --- Scan from 13h06m15s to 13h08m15s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=13h08m15s   !NEXT!

!* --- Scan from 13h09m58s to 13h12m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=13h12m58s   !NEXT!

!* --- Scan from 13h14m58s to 13h17m58s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=13h17m58s   !NEXT!

!* --- Scan from 13h18m13s to 13h21m13s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=13h21m13s   !NEXT!

!* --- Scan from 13h21m32s to 13h23m32s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=13h23m32s   !NEXT!

!* --- Scan from 13h23m50s to 13h26m50s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=13h26m50s   !NEXT!

!* --- Scan from 13h28m50s to 13h31m50s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=13h31m50s   !NEXT!

!* --- Scan from 13h33m30s to 13h36m30s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=13h36m30s   !NEXT!

!* --- Scan from 13h36m45s to 13h38m45s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=13h38m45s   !NEXT!

!* --- Scan from 13h40m27s to 13h43m27s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=13h43m27s   !NEXT!

!* --- Scan from 13h45m27s to 13h48m27s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=13h48m27s   !NEXT!

!* --- Scan from 13h48m42s to 13h51m42s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=13h51m42s   !NEXT!

!* --- Scan from 13h52m01s to 13h54m01s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=13h54m01s   !NEXT!

!* --- Scan from 13h54m19s to 13h57m19s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=13h57m19s   !NEXT!

!* --- Scan from 13h59m19s to 14h02m19s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=14h02m19s   !NEXT!

!* --- Scan from 14h03m58s to 14h06m58s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=14h06m58s   !NEXT!

!* --- Scan from 14h07m13s to 14h09m13s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=14h09m13s   !NEXT!

!* --- Scan from 14h10m52s to 14h13m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=14h13m52s   !NEXT!

!* --- Scan from 14h15m52s to 14h18m52s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=14h18m52s   !NEXT!

!* --- Scan from 14h19m08s to 14h22m08s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=14h22m08s   !NEXT!

!* --- Scan from 14h22m26s to 14h24m26s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=14h24m26s   !NEXT!

!* --- Scan from 14h24m45s to 14h27m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=14h27m45s   !NEXT!

!* --- Scan from 14h29m45s to 14h32m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=14h32m45s   !NEXT!

!* --- Scan from 14h34m21s to 14h37m21s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=14h37m21s   !NEXT!

!* --- Scan from 14h37m36s to 14h39m36s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=14h39m36s   !NEXT!

!* --- Scan from 14h41m14s to 14h44m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=14h44m14s   !NEXT!

!* --- Scan from 14h46m14s to 14h49m14s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=14h49m14s   !NEXT!

!* --- Scan from 14h49m29s to 14h52m29s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=14h52m29s   !NEXT!

!* --- Scan from 14h52m48s to 14h54m48s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=14h54m48s   !NEXT!

!* --- Scan from 14h55m06s to 14h58m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=14h58m06s   !NEXT!

!* --- Scan from 15h00m06s to 15h03m06s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=15h03m06s   !NEXT!

!* --- Scan from 15h04m40s to 15h07m40s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=15h07m40s   !NEXT!

!* --- Scan from 15h07m55s to 15h09m55s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=15h09m55s   !NEXT!

!* --- Scan from 15h11m30s to 15h14m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=15h14m30s   !NEXT!

!* --- Scan from 15h16m30s to 15h19m30s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=15h19m30s   !NEXT!

!* --- Scan from 15h19m45s to 15h22m45s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=15h22m45s   !NEXT!

!* --- Scan from 15h23m04s to 15h25m04s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=15h25m04s   !NEXT!

!* --- Scan from 15h25m22s to 15h28m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=15h28m22s   !NEXT!

!* --- Scan from 15h30m22s to 15h33m22s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=15h33m22s   !NEXT!

!* --- Scan from 15h34m54s to 15h37m54s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
stop=15h37m54s   !NEXT!

!* --- Scan from 15h38m09s to 15h40m09s   Sun, 1995 Oct 22 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=15h40m09s   !NEXT!

!* --- Scan from 15h41m41s to 15h44m41s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=15h44m41s   !NEXT!

!* --- Scan from 15h46m41s to 15h49m41s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=15h49m41s   !NEXT!

!* --- Scan from 15h49m56s to 15h52m56s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=15h52m56s   !NEXT!

!* --- Scan from 15h53m04s to 15h55m04s   Sun, 1995 Oct 22 --- *!
sname='0309+411'  ra=03h13m01.962129s  dec= 41d20'01.18341"  qual=  0  calib='V'
stop=15h55m04s   !NEXT!

!* --- Scan from 15h55m13s to 15h58m13s   Sun, 1995 Oct 22 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=  0  calib='V'
stop=15h58m13s   !NEXT!

!* --- Scan from 16h01m13s to 16h06m43s   Sun, 1995 Oct 22 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=  0  calib='V'
extlo = (1,  4.2601000000)
extlosideband = (1,U)
extlo = (2,  4.2101000000)
extlosideband = (2,U)
extlo = (3,  4.2601000000)
extlosideband = (3,U)
extlo = (4,  4.2101000000)
extlosideband = (4,U)
ifchan=(1,B),(2,D),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bbsynth=( 1,625.39),( 2,625.39),( 3,625.39),( 4,625.39)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(7,1510),(8,1510)
stop=16h06m43s   !NEXT!

!* --- Scan from 16h06m58s to 16h12m28s   Sun, 1995 Oct 22 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=  0  calib='V'
extlo = (1,  9.1101000000)
extlosideband = (1,L)
extlo = (2,  9.1601000000)
extlosideband = (2,L)
extlo = (3,  9.1101000000)
extlosideband = (3,L)
extlo = (4,  9.1601000000)
extlosideband = (4,L)
ifchan=(1,A),(2,C),(3,B),(4,D)
sideband=(1,L),(2,L),(3,L),(4,L)
bbsynth=( 1,624.11),( 2,624.11),( 3,624.11),( 4,624.11)
pcalxfreq1=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
pcalxfreq2=(1,10),(2,10),(3,6010),(4,6010),(7,1010),(8,1010)
stop=16h12m28s   !NEXT!
     !QUIT! 
