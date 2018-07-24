!*  Schedule for VLBA_SC   *!
!*  Experiment eggl_oh  *!
!* Schedule Version:      99.90 *!
!* Processed by SCHED version:  11.50 *!
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
program=eggl_oh 

diskformat=mark5c
media=(1,disk)

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 12h00m00s to 12h22m00s   Tue, 1997 Mar 04 --- *!
sname='3C454.3'  ra=22h53m57.747938s  dec= 16d08'53.56093"  qual=999  calib='V'
maxcaltime= 120
fe=(1,20cm),(3,20cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,15.4),( 2,-2.6),( 3,15.4)
logging=STANDARD
nchan= 4
format=VLBA1:1
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,L),(2,L),(3,L),(4,L)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,935.00),( 2,935.00),( 3,933.00),( 4,933.00)
bbfilter=(1,1M),(2,1M),(3,1M),(4,1M)
pcal=OFF
pcalxbit1=(1,S1),(2,S2),(3,S3),(4,S4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(1,M1),(2,M2),(3,M3),(4,M4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxfreq1=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
samplerate=2M
disk=off
  date = 1997Mar04
stop=12h00m00s   !NEXT!        
qual=  0
disk=off
stop=12h22m00s   !NEXT!

!* --- Scan from 12h30m32s to 12h52m32s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=12h30m32s   !NEXT!        
qual=  0
disk=off
stop=12h52m32s   !NEXT!

!* --- Scan from 12h53m12s to 13h15m12s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=12h53m12s   !NEXT!        
qual=  0
disk=off
stop=13h15m12s   !NEXT!

!* --- Scan from 13h15m52s to 13h37m52s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=13h15m52s   !NEXT!        
qual=  0
disk=off
stop=13h37m52s   !NEXT!

!* --- Scan from 13h38m32s to 14h00m32s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=13h38m32s   !NEXT!        
qual=  0
disk=off
stop=14h00m32s   !NEXT!

!* --- Scan from 14h15m12s to 14h37m12s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=14h15m12s   !NEXT!        
qual=  0
disk=off
stop=14h37m12s   !NEXT!

!* --- Scan from 14h37m52s to 14h59m52s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=14h37m52s   !NEXT!        
qual=  0
disk=off
stop=14h59m52s   !NEXT!

!* --- Scan from 15h09m46s to 15h31m46s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160094s  dec= 41d30'42.10413"  qual=999  calib='V'
   dra=0.0  ddec=0.0 
disk=off
stop=15h09m46s   !NEXT!        
qual=  0
disk=off
stop=15h31m46s   !NEXT!

!* --- Scan from 15h40m26s to 16h02m26s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=15h40m26s   !NEXT!        
qual=  0
disk=off
stop=16h02m26s   !NEXT!

!* --- Scan from 16h03m06s to 16h25m06s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=16h03m06s   !NEXT!        
qual=  0
disk=off
stop=16h25m06s   !NEXT!

!* --- Scan from 16h25m46s to 16h47m46s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=16h25m46s   !NEXT!        
qual=  0
disk=off
stop=16h47m46s   !NEXT!

!* --- Scan from 16h48m26s to 17h10m26s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=16h48m26s   !NEXT!        
qual=  0
disk=off
stop=17h10m26s   !NEXT!

!* --- Scan from 17h11m06s to 17h33m06s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=17h11m06s   !NEXT!        
qual=  0
disk=off
stop=17h33m06s   !NEXT!

!* --- Scan from 17h33m46s to 17h55m46s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=17h33m46s   !NEXT!        
qual=  0
disk=off
stop=17h55m46s   !NEXT!

!* --- Scan from 18h16m19s to 18h38m19s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160094s  dec= 41d30'42.10413"  qual=999  calib='V'
   dra=0.0  ddec=0.0 
disk=off
stop=18h16m19s   !NEXT!        
qual=  0
disk=off
stop=18h38m19s   !NEXT!

!* --- Scan from 18h41m49s to 19h03m49s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=18h41m49s   !NEXT!        
qual=  0
disk=off
stop=19h03m49s   !NEXT!

!* --- Scan from 19h04m29s to 19h26m29s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=19h04m29s   !NEXT!        
qual=  0
disk=off
stop=19h26m29s   !NEXT!

!* --- Scan from 19h27m09s to 19h49m09s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=19h27m09s   !NEXT!        
qual=  0
disk=off
stop=19h49m09s   !NEXT!

!* --- Scan from 19h49m49s to 20h11m49s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=19h49m49s   !NEXT!        
qual=  0
disk=off
stop=20h11m49s   !NEXT!

!* --- Scan from 20h12m29s to 20h34m29s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=20h12m29s   !NEXT!        
qual=  0
disk=off
stop=20h34m29s   !NEXT!

!* --- Scan from 20h35m09s to 20h57m09s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=20h35m09s   !NEXT!        
qual=  0
disk=off
stop=20h57m09s   !NEXT!

!* --- Scan from 20h59m38s to 21h21m38s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160094s  dec= 41d30'42.10413"  qual=999  calib='V'
   dra=0.0  ddec=0.0 
disk=off
stop=20h59m38s   !NEXT!        
qual=  0
disk=off
stop=21h21m38s   !NEXT!

!* --- Scan from 21h24m05s to 21h46m05s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=21h24m05s   !NEXT!        
qual=  0
disk=off
stop=21h46m05s   !NEXT!

!* --- Scan from 21h46m45s to 22h08m45s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=21h46m45s   !NEXT!        
qual=  0
disk=off
stop=22h08m45s   !NEXT!

!* --- Scan from 22h09m25s to 22h31m25s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=22h09m25s   !NEXT!        
qual=  0
disk=off
stop=22h31m25s   !NEXT!

!* --- Scan from 22h32m05s to 22h54m05s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=22h32m05s   !NEXT!        
qual=  0
disk=off
stop=22h54m05s   !NEXT!

!* --- Scan from 22h54m45s to 23h16m45s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=22h54m45s   !NEXT!        
qual=  0
disk=off
stop=23h16m45s   !NEXT!

!* --- Scan from 23h40m49s to 00h02m49s   Tue, 1997 Mar 04 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=23h40m49s   !NEXT!        
qual=  0
disk=off
date=1997Mar05
stop=00h02m49s   !NEXT!

!* --- Scan from 00h04m21s to 00h26m21s   Wed, 1997 Mar 05 --- *!
sname='3C84'  ra=03h19m48.160094s  dec= 41d30'42.10413"  qual=999  calib='V'
   dra=0.0  ddec=0.0 
disk=off
stop=00h04m21s   !NEXT!        
qual=  0
disk=off
stop=00h26m21s   !NEXT!

!* --- Scan from 00h27m50s to 00h49m50s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=00h27m50s   !NEXT!        
qual=  0
disk=off
stop=00h49m50s   !NEXT!

!* --- Scan from 00h50m30s to 01h12m30s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=00h50m30s   !NEXT!        
qual=  0
disk=off
stop=01h12m30s   !NEXT!

!* --- Scan from 01h13m10s to 01h35m10s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=01h13m10s   !NEXT!        
qual=  0
disk=off
stop=01h35m10s   !NEXT!

!* --- Scan from 01h35m50s to 01h57m50s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=01h35m50s   !NEXT!        
qual=  0
disk=off
stop=01h57m50s   !NEXT!

!* --- Scan from 01h58m30s to 02h20m30s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=01h58m30s   !NEXT!        
qual=  0
disk=off
stop=02h20m30s   !NEXT!

!* --- Scan from 02h21m10s to 02h43m10s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=02h21m10s   !NEXT!        
qual=  0
disk=off
stop=02h43m10s   !NEXT!

!* --- Scan from 02h45m12s to 03h07m12s   Wed, 1997 Mar 05 --- *!
sname='3C84'  ra=03h19m48.160094s  dec= 41d30'42.10413"  qual=999  calib='V'
   dra=0.0  ddec=0.0 
disk=off
stop=02h45m12s   !NEXT!        
qual=  0
disk=off
stop=03h07m12s   !NEXT!

!* --- Scan from 03h26m43s to 03h48m43s   Wed, 1997 Mar 05 --- *!
sname='S_PER'  ra=02h22m51.714267s  dec= 58d35'11.43182"  qual=999  calib=' '
 epochd=1997Mar04 epocht=12h22m00s dra=      0.00 ddec=     -0.00
disk=off
stop=03h26m43s   !NEXT!        
qual=  0
disk=off
stop=03h48m43s   !NEXT!
disk=off
stop=03h48m48s   !NEXT!
     !QUIT! 
