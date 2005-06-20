!*  Schedule for VLBA_SC   *!
!*  Experiment GM031    *!
!* Schedule Version:      99.90 *!
!* Processed by SCHED version:   6.0  Release: March 2005 *!
!* PI:       Huib van Langevelde *!
!* Address:  JIVE, Radiosterrenwacht Dwingeloo *!
!*           Postbus 2, 7990 AA Dwingeloo *!
!*           the Netherlands *!
!*  *!
!* Phone:    +31 521 595 220 *!
!* EMAIL:    huib@jive.nfra.nl *!
!* Fax:      +31 521 597 332 *!
!* Phone during observation: +31 528 221 273 *!
!* Observing mode: VLBA/MKIV *!
!* Notes:    Make sure PHASE CAL is OFF. *!
!*  *!
!*  *!
!*  *!
!*  Start at 12h00m00s     Tue, 1997 Mar 04  Day of year   63   *!
program=GM031   
autoallocate=on
autoreverse=on

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 12h00m00s to 12h22m00s   Tue, 1997 Mar 04 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
maxcaltime= 120
fe=(1,20cm),(3,20cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,15.4),( 2, 2.4),( 3,15.4)
logging=STANDARD
nchan= 4
format=VLBA1:1
barrel=roll_off
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,734.73),( 2,734.73),( 3,732.77),( 4,732.77)
bbfilter=(1,500K),(2,500K),(3,500K),(4,500K)
pcal=OFF
pcalxbit1=(1,S1),(2,S2),(3,S3),(4,S4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(1,M1),(2,M2),(3,M3),(4,M4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxfreq1=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
samplerate=4M
tape=(1,STOP) write=(1,off)
  date = 1997Mar04
stop=12h00m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h22m00s   !NEXT!

!* --- Scan from 12h30m36s to 12h52m36s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=12h30m36s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h52m36s   !NEXT!

!* --- Scan from 12h53m16s to 13h15m16s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=12h53m16s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h15m16s   !NEXT!

!* --- Scan from 13h15m56s to 13h37m56s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=13h15m56s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h37m56s   !NEXT!

!* --- Scan from 13h38m36s to 14h00m36s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=13h38m36s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h00m36s   !NEXT!

!* --- Scan from 14h18m38s to 14h40m38s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=14h18m38s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h40m38s   !NEXT!

!* --- Scan from 14h41m18s to 15h03m18s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=14h41m18s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h03m18s   !NEXT!

!* --- Scan from 15h15m09s to 15h37m09s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
   dra=0.0  ddec=0.0 
tape=(1,STOP) write=(1,off)
stop=15h15m09s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h37m09s   !NEXT!

!* --- Scan from 15h46m43s to 16h08m43s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=15h46m43s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=16h08m43s   !NEXT!

!* --- Scan from 16h09m23s to 16h31m23s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=16h09m23s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=16h31m23s   !NEXT!

!* --- Scan from 16h32m03s to 16h54m03s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=16h32m03s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=16h54m03s   !NEXT!

!* --- Scan from 16h54m43s to 17h16m43s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=16h54m43s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=17h16m43s   !NEXT!

!* --- Scan from 17h17m23s to 17h39m23s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=17h17m23s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=17h39m23s   !NEXT!

!* --- Scan from 17h40m03s to 18h02m03s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=17h40m03s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=18h02m03s   !NEXT!

!* --- Scan from 18h22m51s to 18h44m51s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
   dra=0.0  ddec=0.0 
tape=(1,STOP) write=(1,off)
stop=18h22m51s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=18h44m51s   !NEXT!

!* --- Scan from 18h48m18s to 19h10m18s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=18h48m18s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=19h10m18s   !NEXT!

!* --- Scan from 19h10m58s to 19h32m58s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=19h10m58s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=19h32m58s   !NEXT!

!* --- Scan from 19h33m38s to 19h55m38s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=19h33m38s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=19h55m38s   !NEXT!

!* --- Scan from 19h56m18s to 20h18m18s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=19h56m18s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=20h18m18s   !NEXT!

!* --- Scan from 20h18m58s to 20h40m58s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=20h18m58s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=20h40m58s   !NEXT!

!* --- Scan from 20h41m38s to 21h03m38s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=20h41m38s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=21h03m38s   !NEXT!

!* --- Scan from 21h06m07s to 21h28m07s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
   dra=0.0  ddec=0.0 
tape=(1,STOP) write=(1,off)
stop=21h06m07s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=21h28m07s   !NEXT!

!* --- Scan from 21h30m26s to 21h52m26s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=21h30m26s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=21h52m26s   !NEXT!

!* --- Scan from 21h53m06s to 22h15m06s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=21h53m06s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=22h15m06s   !NEXT!

!* --- Scan from 22h15m46s to 22h37m46s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=22h15m46s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=22h37m46s   !NEXT!

!* --- Scan from 22h38m26s to 23h00m26s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=22h38m26s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=23h00m26s   !NEXT!

!* --- Scan from 23h24m34s to 23h46m34s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=23h24m34s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=23h46m34s   !NEXT!

!* --- Scan from 23h47m14s to 00h09m14s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=23h47m14s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
date=1997Mar05
stop=00h09m14s   !NEXT!

!* --- Scan from 00h10m49s to 00h32m49s   Wed, 1997 Mar 05 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
   dra=0.0  ddec=0.0 
tape=(1,STOP) write=(1,off)
stop=00h10m49s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=00h32m49s   !NEXT!

!* --- Scan from 00h34m22s to 00h56m22s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=00h34m22s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=00h56m22s   !NEXT!

!* --- Scan from 00h57m02s to 01h19m02s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=00h57m02s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=01h19m02s   !NEXT!

!* --- Scan from 01h19m42s to 01h41m42s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=01h19m42s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=01h41m42s   !NEXT!

!* --- Scan from 01h42m22s to 02h04m22s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=01h42m22s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=02h04m22s   !NEXT!

!* --- Scan from 02h05m02s to 02h27m02s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=02h05m02s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=02h27m02s   !NEXT!

!* --- Scan from 02h27m42s to 02h49m42s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=      0.00
tape=(1,STOP) write=(1,off)
stop=02h27m42s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=02h49m42s   !NEXT!

!* --- Scan from 03h09m13s to 03h31m13s   Wed, 1997 Mar 05 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
   dra=0.0  ddec=0.0 
tape=(1,STOP) write=(1,off)
stop=03h09m13s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=03h31m13s   !NEXT!

tape=(1,STOP)    write=(1,off)   dur=0s 
stop=03h31m18s   !NEXT!
     !QUIT! 
