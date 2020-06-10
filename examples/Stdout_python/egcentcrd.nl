!*  Schedule for VLBA_NL   *!
!*  Experiment egcent   *!
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
!* Observing mode: 6cm 128-4-2 *!
!* Notes: *!
!*  *!
!*  *!
!*  *!
!*  Start at 20h00m00s     Wed, 1995 Jul 05  Day of year  186   *!
program=egcent  

diskformat=mark5c
media=(1,disk)

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 20h00m00s to 20h04m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013937s  dec= 39d02'20.85185"  qual=999  calib='V'
maxcaltime= 120
fe=(1,20cm),(3,20cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,14.6),( 2,-2.4),( 3,14.6)
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
bbsynth=( 1,950.25),( 2,950.25),( 3,755.25),( 4,755.25)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S2),(7,S3),(8,S4)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,M1),(6,M2),(7,M3),(8,M4)
pcalxfreq1=(1,250),(2,250),(3,13250),(4,13250),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,250),(2,250),(3,13250),(4,13250),(5,0),(6,0),(7,0),(8,0)
samplerate=32M
disk=off
  date = 1995Jul05
stop=20h00m00s   !NEXT!        
qual=  0
disk=off
stop=20h04m00s   !NEXT!

!* --- Scan from 20h04m20s to 20h08m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013937s  dec= 39d02'20.85185"  qual=999  calib='V'
disk=off
stop=20h04m20s   !NEXT!        
qual=  0
disk=off
stop=20h08m20s   !NEXT!

!* --- Scan from 20h08m40s to 20h12m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013937s  dec= 39d02'20.85185"  qual=999  calib='V'
disk=off
stop=20h08m40s   !NEXT!        
qual=  0
disk=off
stop=20h12m40s   !NEXT!

!* --- Scan from 20h13m00s to 20h17m00s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013937s  dec= 39d02'20.85185"  qual=999  calib='V'
disk=off
stop=20h13m00s   !NEXT!        
qual=  0
disk=off
stop=20h17m00s   !NEXT!

!* --- Scan from 20h19m20s to 20h23m20s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013937s  dec= 39d02'20.85185"  qual=999  calib='V'
disk=off
stop=20h19m20s   !NEXT!        
qual=  0
disk=off
stop=20h23m20s   !NEXT!

!* --- Scan from 20h23m40s to 20h27m40s   Wed, 1995 Jul 05 --- *!
sname='4C39.25'  ra=09h27m03.013937s  dec= 39d02'20.85185"  qual=999  calib='V'
disk=off
stop=20h23m40s   !NEXT!        
qual=  0
disk=off
stop=20h27m40s   !NEXT!

!* --- Scan from 20h28m00s to 20h32m00s   Wed, 1995 Jul 05 --- *!
sname='J0923+2815'  ra=09h23m51.523412s  dec= 28d15'25.02193"  qual=999  calib='V'
disk=off
stop=20h28m00s   !NEXT!        
qual=  0
disk=off
stop=20h32m00s   !NEXT!

!* --- Scan from 20h32m20s to 20h36m20s   Wed, 1995 Jul 05 --- *!
sname='J0923+2815'  ra=09h23m51.523412s  dec= 28d15'25.02193"  qual=999  calib='V'
disk=off
stop=20h32m20s   !NEXT!        
qual=  0
disk=off
stop=20h36m20s   !NEXT!

!* --- Scan from 20h36m40s to 20h40m40s   Wed, 1995 Jul 05 --- *!
sname='J0923+2815'  ra=09h23m51.523412s  dec= 28d15'25.02193"  qual=999  calib='V'
disk=off
stop=20h36m40s   !NEXT!        
qual=  0
disk=off
stop=20h40m40s   !NEXT!

!* --- Scan from 20h41m00s to 20h45m00s   Wed, 1995 Jul 05 --- *!
sname='J0923+2815'  ra=09h23m51.523412s  dec= 28d15'25.02193"  qual=999  calib='V'
disk=off
stop=20h41m00s   !NEXT!        
qual=  0
disk=off
stop=20h45m00s   !NEXT!

!* --- Scan from 20h45m20s to 20h49m20s   Wed, 1995 Jul 05 --- *!
sname='J0923+2815'  ra=09h23m51.523412s  dec= 28d15'25.02193"  qual=999  calib='V'
disk=off
stop=20h45m20s   !NEXT!        
qual=  0
disk=off
stop=20h49m20s   !NEXT!

!* --- Scan from 20h49m40s to 20h53m40s   Wed, 1995 Jul 05 --- *!
sname='J0923+2815'  ra=09h23m51.523412s  dec= 28d15'25.02193"  qual=999  calib='V'
disk=off
stop=20h49m40s   !NEXT!        
qual=  0
disk=off
stop=20h53m40s   !NEXT!

!* --- Scan from 20h54m00s to 20h58m00s   Wed, 1995 Jul 05 --- *!
sname='J0923+2815'  ra=09h23m51.523412s  dec= 28d15'25.02193"  qual=999  calib='V'
disk=off
stop=20h54m00s   !NEXT!        
qual=  0
disk=off
stop=20h58m00s   !NEXT!

!* --- Scan from 20h58m20s to 21h02m20s   Wed, 1995 Jul 05 --- *!
sname='J0923+2815'  ra=09h23m51.523412s  dec= 28d15'25.02193"  qual=999  calib='V'
disk=off
stop=20h58m20s   !NEXT!        
qual=  0
disk=off
stop=21h02m20s   !NEXT!

!* --- Scan from 21h02m40s to 21h06m40s   Wed, 1995 Jul 05 --- *!
sname='J0923+2815'  ra=09h23m51.523412s  dec= 28d15'25.02193"  qual=999  calib='V'
disk=off
stop=21h02m40s   !NEXT!        
qual=  0
disk=off
stop=21h06m40s   !NEXT!

!* --- Scan from 21h07m00s to 21h17m00s   Wed, 1995 Jul 05 --- *!
sname='J0925-2027'  ra=09h25m11.947405s  dec=-20d27'35.61070"  qual=999  calib='V'
disk=off
stop=21h07m00s   !NEXT!        
qual=  0
disk=off
stop=21h17m00s   !NEXT!
disk=off
stop=21h17m05s   !NEXT!
     !QUIT! 
