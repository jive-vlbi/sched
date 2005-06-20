!*  Schedule for VLBA_NL   *!
!*  Experiment egOH     *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:   6.0  Release: March 2005 *!
!* PI:       PI Name *!
!* Address:  Address (line one) *!
!*           Address (line two) *!
!*           Address (line three) *!
!*  *!
!* Phone:    Telephone number *!
!* EMAIL:    e-mail address *!
!* Fax:      Fax number *!
!* Phone during observation: Tel. during observations *!
!* Observing mode: 18cm spectral line observations *!
!* Notes:    Special instructions *!
!*  *!
!*  *!
!*  *!
!*  Start at 03h08m00s     Sat, 1995 Jun 03  Day of year  154   *!
program=egOH    
autoallocate=on
autoreverse=on

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 03h08m00s to 04h00m00s   Sat, 1995 Jun 03 --- *!
sname='SRC1'  ra=18h53m18.650919s  dec= 01d14'58.08319"  qual=999  calib=' '
maxcaltime= 120
fe=(1,20cm),(3,20cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,15.4),( 2, 2.4),( 3,15.4)
logging=STANDARD
nchan= 8
format=VLBA1:1
barrel=roll_auto
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)
ifchan=(1,A),(2,C),(3,A),(4,C),(5,A),(6,C),(7,A),(8,C)
sideband=(1,L),(2,L),(3,L),(4,L),(5,L),(6,L),(7,L),(8,L)
bits=(1,2),(2,2),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2)
period=(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1)
level=(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,787.78),( 2,787.78),( 3,734.66),( 4,734.66),( 5,732.76),( 6,732.76)
bbsynth=( 7,679.64),( 8,679.64)
bbfilter=(1,250K),(2,250K),(3,250K),(4,250K),(5,250K),(6,250K),(7,250K),(8,250K)
pcal=OFF
pcalxbit1=(1,S1),(2,S2),(3,S3),(4,S4),(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(1,M1),(2,M2),(3,M3),(4,M4),(5,M5),(6,M6),(7,M7),(8,M8)
pcalxfreq1=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
samplerate=2M
tape=(1,STOP) write=(1,off)
  date = 1995Jun03
stop=03h08m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=04h00m00s   !NEXT!

!* --- Scan from 04h08m00s to 05h00m00s   Sat, 1995 Jun 03 --- *!
sname='SRC1'  ra=18h53m18.650919s  dec= 01d14'58.08319"  qual=999  calib=' '
tape=(1,STOP) write=(1,off)
stop=04h08m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h00m00s   !NEXT!

!* --- Scan from 05h02m00s to 05h30m00s   Sat, 1995 Jun 03 --- *!
sname='CAL1'  ra=18h02m32.620579s  dec= 01d00'05.59106"  qual=999  calib=' '
tape=(1,STOP) write=(1,off)
stop=05h02m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h30m00s   !NEXT!

!* --- Scan from 05h30m00s to 05h54m00s   Sat, 1995 Jun 03 --- *!
sname='SRC1'  ra=18h53m18.650919s  dec= 01d14'58.08319"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=05h54m00s   !NEXT!

!* --- Scan from 06h02m00s to 06h54m00s   Sat, 1995 Jun 03 --- *!
sname='SRC1'  ra=18h53m18.650919s  dec= 01d14'58.08319"  qual=999  calib=' '
tape=(1,STOP) write=(1,off)
stop=06h02m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h54m00s   !NEXT!

tape=(1,STOP)    write=(1,off)   dur=0s 
stop=06h54m05s   !NEXT!
     !QUIT! 
