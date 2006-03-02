!*  Schedule for VLA27     *!
!*  Experiment bw099    *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:   6.04  February 2006 *!
!* PI:       J.M. Wrobel *!
!* Address:  NRAO, P.O. Box O *!
!*           Socorro, NM 87801 *!
!*           USA *!
!*  *!
!* Phone:    +1-505-835-7392 *!
!* EMAIL:    jwrobel@nrao.edu (internet) *!
!* Fax:      +1-505-835-7027 *!
!* Phone during observation: VLA *!
!* Observing mode: 128-4-2 *!
!* Notes:    VLBA target                : J1220+29=NGC4278=UGC07386 *** *!
!*           VLBA DR check/VLA phase ref: J1221+28                  *** *!
!*           VLBA FF/manual pcal/bpass  : J1229+02=3C273            *** *!
!*           VLBA amp check, VLA amp std: J1310+32, J1331+30=3C286  *** *!
!*  Start at 06h45m00s     Wed, 1997 Jan 15  Day of year   15   *!
program=bw099   
autoallocate=on
autoreverse=on

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 06h45m00s to 06h57m00s   Wed, 1997 Jan 15 --- *!
sname='J0725-00'  ra=07h25m50.639961s  dec= 00d54'56.54435"  qual=999  calib='V'
maxcaltime= 120
extlo = (3, 21.4101000000)
extlosideband = (3,U)
extlo = (4, 21.8101000000)
extlosideband = (4,U)
logging=STANDARD
nchan= 4
format=VLBA1:4
barrel=roll_auto
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,C),(2,C),(3,D),(4,D)
sideband=(1,U),(2,U),(3,U),(4,U)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,618.39),( 2,626.39),( 3,618.39),( 4,626.39)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S2),(7,S3),(8,S4)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,M1),(6,M2),(7,M3),(8,M4)
pcalxfreq1=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,510),(2,510),(3,6510),(4,6510),(5,0),(6,0),(7,0),(8,0)
samplerate=16M
tape=(1,STOP) write=(1,off)
  date = 1997Jan15
stop=06h45m00s   !NEXT!        
qual=  0
tape=(1,STOP) write=(1,off)
stop=06h57m00s   !NEXT!

!* --- Scan from 06h57m00s to 07h02m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h02m00s   !NEXT!

!* --- Scan from 07h02m00s to 07h05m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=07h05m00s   !NEXT!

!* --- Scan from 07h05m00s to 07h13m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=07h13m00s   !NEXT!

!* --- Scan from 07h13m00s to 07h16m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=07h16m00s   !NEXT!

!* --- Scan from 07h16m00s to 07h24m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=07h24m00s   !NEXT!

!* --- Scan from 07h24m00s to 07h27m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=07h27m00s   !NEXT!

!* --- Scan from 07h27m00s to 07h35m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=07h35m00s   !NEXT!

!* --- Scan from 07h35m00s to 07h38m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=07h38m00s   !NEXT!

!* --- Scan from 07h38m00s to 07h46m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=07h46m00s   !NEXT!

!* --- Scan from 07h46m00s to 07h49m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=07h49m00s   !NEXT!

!* --- Scan from 07h49m00s to 07h57m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=07h57m00s   !NEXT!

!* --- Scan from 07h57m00s to 08h00m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=08h00m00s   !NEXT!

!* --- Scan from 08h00m00s to 08h08m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=08h08m00s   !NEXT!

!* --- Scan from 08h08m00s to 08h11m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=08h11m00s   !NEXT!

!* --- Scan from 08h11m00s to 08h19m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=08h19m00s   !NEXT!

!* --- Scan from 08h19m00s to 08h22m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=08h22m00s   !NEXT!

!* --- Scan from 08h22m00s to 08h30m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=08h30m00s   !NEXT!

!* --- Scan from 08h30m00s to 08h33m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=08h33m00s   !NEXT!

!* --- Scan from 08h33m00s to 08h41m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=08h41m00s   !NEXT!

!* --- Scan from 08h41m00s to 08h44m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=08h44m00s   !NEXT!

!* --- Scan from 08h44m00s to 08h52m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=08h52m00s   !NEXT!

!* --- Scan from 08h52m00s to 08h55m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=08h55m00s   !NEXT!

!* --- Scan from 08h55m00s to 09h03m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=09h03m00s   !NEXT!

!* --- Scan from 09h03m00s to 09h06m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=09h06m00s   !NEXT!

!* --- Scan from 09h06m00s to 09h14m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=09h14m00s   !NEXT!

!* --- Scan from 09h14m00s to 09h17m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=09h17m00s   !NEXT!

!* --- Scan from 09h17m00s to 09h25m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=09h25m00s   !NEXT!

!* --- Scan from 09h25m00s to 09h28m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=09h28m00s   !NEXT!

!* --- Scan from 09h28m00s to 09h36m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=09h36m00s   !NEXT!

!* --- Scan from 09h36m00s to 09h39m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=09h39m00s   !NEXT!

!* --- Scan from 09h39m00s to 09h47m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=09h47m00s   !NEXT!

!* --- Scan from 09h47m00s to 09h50m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=09h50m00s   !NEXT!

!* --- Scan from 09h50m00s to 09h58m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=09h58m00s   !NEXT!

!* --- Scan from 09h58m00s to 10h02m00s   Wed, 1997 Jan 15 --- *!
!*  **** Start 1st calibration sequence.  *!
sname='J1224+03'  ra=12h24m52.421934s  dec= 03d30'50.29295"  qual=  0  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h02m00s   !NEXT!

!* --- Scan from 10h02m00s to 10h07m30s   Wed, 1997 Jan 15 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=10h07m30s   !NEXT!

!* --- Scan from 10h08m30s to 10h14m00s   Wed, 1997 Jan 15 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h08m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h14m00s   !NEXT!

!* --- Scan from 10h14m00s to 10h18m00s   Wed, 1997 Jan 15 --- *!
sname='3C286'  ra=13h31m08.288049s  dec= 30d30'32.95926"  qual=  0  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h18m00s   !NEXT!

!* --- Scan from 10h18m00s to 10h21m00s   Wed, 1997 Jan 15 --- *!
!*  **** End   1st calibration sequence.  *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h21m00s   !NEXT!

!* --- Scan from 10h21m00s to 10h24m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=10h24m00s   !NEXT!

!* --- Scan from 10h24m00s to 10h32m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=10h32m00s   !NEXT!

!* --- Scan from 10h32m00s to 10h35m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=10h35m00s   !NEXT!

!* --- Scan from 10h35m00s to 10h43m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=10h43m00s   !NEXT!

!* --- Scan from 10h43m00s to 10h46m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=10h46m00s   !NEXT!

!* --- Scan from 10h46m00s to 10h54m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=10h54m00s   !NEXT!

!* --- Scan from 10h54m00s to 10h57m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=10h57m00s   !NEXT!

!* --- Scan from 10h57m00s to 11h05m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=11h05m00s   !NEXT!

!* --- Scan from 11h05m00s to 11h08m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=11h08m00s   !NEXT!

!* --- Scan from 11h08m00s to 11h16m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=11h16m00s   !NEXT!

!* --- Scan from 11h16m00s to 11h19m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=11h19m00s   !NEXT!

!* --- Scan from 11h19m00s to 11h27m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=11h27m00s   !NEXT!

!* --- Scan from 11h27m00s to 11h30m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=11h30m00s   !NEXT!

!* --- Scan from 11h30m00s to 11h38m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=11h38m00s   !NEXT!

!* --- Scan from 11h38m00s to 11h41m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=11h41m00s   !NEXT!

!* --- Scan from 11h41m00s to 11h49m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=11h49m00s   !NEXT!

!* --- Scan from 11h49m00s to 11h52m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=11h52m00s   !NEXT!

!* --- Scan from 11h52m00s to 12h00m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=12h00m00s   !NEXT!

!* --- Scan from 12h00m00s to 12h03m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=12h03m00s   !NEXT!

!* --- Scan from 12h03m00s to 12h11m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=12h11m00s   !NEXT!

!* --- Scan from 12h11m00s to 12h14m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=12h14m00s   !NEXT!

!* --- Scan from 12h14m00s to 12h22m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=12h22m00s   !NEXT!

!* --- Scan from 12h22m00s to 12h25m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=12h25m00s   !NEXT!

!* --- Scan from 12h25m00s to 12h33m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=12h33m00s   !NEXT!

!* --- Scan from 12h33m00s to 12h36m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=12h36m00s   !NEXT!

!* --- Scan from 12h36m00s to 12h44m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=12h44m00s   !NEXT!

!* --- Scan from 12h44m00s to 12h47m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=12h47m00s   !NEXT!

!* --- Scan from 12h47m00s to 12h55m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=12h55m00s   !NEXT!

!* --- Scan from 12h55m00s to 12h58m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=12h58m00s   !NEXT!

!* --- Scan from 12h58m00s to 13h06m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=13h06m00s   !NEXT!

!* --- Scan from 13h06m00s to 13h09m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=13h09m00s   !NEXT!

!* --- Scan from 13h09m00s to 13h17m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=13h17m00s   !NEXT!

!* --- Scan from 13h17m00s to 13h20m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=13h20m00s   !NEXT!

!* --- Scan from 13h20m00s to 13h28m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=13h28m00s   !NEXT!

!* --- Scan from 13h28m00s to 13h31m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=13h31m00s   !NEXT!

!* --- Scan from 13h31m00s to 13h39m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=13h39m00s   !NEXT!

!* --- Scan from 13h39m00s to 13h42m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=13h42m00s   !NEXT!

!* --- Scan from 13h42m00s to 13h50m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=13h50m00s   !NEXT!

!* --- Scan from 13h50m00s to 13h54m00s   Wed, 1997 Jan 15 --- *!
!*  **** Start 2nd calibration sequence.  *!
sname='J1224+03'  ra=12h24m52.421934s  dec= 03d30'50.29295"  qual=  0  calib='V'
tape=(1,STOP) write=(1,off)
stop=13h54m00s   !NEXT!

!* --- Scan from 13h54m00s to 13h59m30s   Wed, 1997 Jan 15 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=13h59m30s   !NEXT!

!* --- Scan from 14h00m30s to 14h06m00s   Wed, 1997 Jan 15 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=14h00m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h06m00s   !NEXT!

!* --- Scan from 14h06m00s to 14h10m00s   Wed, 1997 Jan 15 --- *!
sname='3C286'  ra=13h31m08.288049s  dec= 30d30'32.95926"  qual=  0  calib='V'
tape=(1,STOP) write=(1,off)
stop=14h10m00s   !NEXT!

!* --- Scan from 14h10m00s to 14h13m00s   Wed, 1997 Jan 15 --- *!
!*  **** End   2nd calibration sequence.  *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,STOP) write=(1,off)
stop=14h13m00s   !NEXT!

!* --- Scan from 14h13m00s to 14h16m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=14h16m00s   !NEXT!

!* --- Scan from 14h16m00s to 14h24m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=14h24m00s   !NEXT!

!* --- Scan from 14h24m00s to 14h27m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=14h27m00s   !NEXT!

!* --- Scan from 14h27m00s to 14h35m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=14h35m00s   !NEXT!

!* --- Scan from 14h35m00s to 14h38m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=14h38m00s   !NEXT!

!* --- Scan from 14h38m00s to 14h46m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=14h46m00s   !NEXT!

!* --- Scan from 14h46m00s to 14h49m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=14h49m00s   !NEXT!

!* --- Scan from 14h49m00s to 14h57m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=14h57m00s   !NEXT!

!* --- Scan from 14h57m00s to 15h00m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=15h00m00s   !NEXT!

!* --- Scan from 15h00m00s to 15h08m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=15h08m00s   !NEXT!

!* --- Scan from 15h08m00s to 15h11m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=15h11m00s   !NEXT!

!* --- Scan from 15h11m00s to 15h19m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=15h19m00s   !NEXT!

!* --- Scan from 15h19m00s to 15h22m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=15h22m00s   !NEXT!

!* --- Scan from 15h22m00s to 15h30m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=15h30m00s   !NEXT!

!* --- Scan from 15h30m00s to 15h33m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=15h33m00s   !NEXT!

!* --- Scan from 15h33m00s to 15h41m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=15h41m00s   !NEXT!

!* --- Scan from 15h41m00s to 15h44m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=15h44m00s   !NEXT!

!* --- Scan from 15h44m00s to 15h52m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=15h52m00s   !NEXT!

!* --- Scan from 15h52m00s to 15h55m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=15h55m00s   !NEXT!

!* --- Scan from 15h55m00s to 16h03m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=16h03m00s   !NEXT!

!* --- Scan from 16h03m00s to 16h06m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=16h06m00s   !NEXT!

!* --- Scan from 16h06m00s to 16h14m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=16h14m00s   !NEXT!

!* --- Scan from 16h14m00s to 16h17m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=16h17m00s   !NEXT!

!* --- Scan from 16h17m00s to 16h25m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=16h25m00s   !NEXT!

!* --- Scan from 16h25m00s to 16h28m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=16h28m00s   !NEXT!

!* --- Scan from 16h28m00s to 16h36m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=16h36m00s   !NEXT!

!* --- Scan from 16h36m00s to 16h39m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=16h39m00s   !NEXT!

!* --- Scan from 16h39m00s to 16h47m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=16h47m00s   !NEXT!

!* --- Scan from 16h47m00s to 16h50m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=16h50m00s   !NEXT!

!* --- Scan from 16h50m00s to 16h58m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=16h58m00s   !NEXT!

!* --- Scan from 16h58m00s to 17h01m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=17h01m00s   !NEXT!

!* --- Scan from 17h01m00s to 17h09m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=17h09m00s   !NEXT!

!* --- Scan from 17h09m00s to 17h12m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=17h12m00s   !NEXT!

!* --- Scan from 17h12m00s to 17h20m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=17h20m00s   !NEXT!

!* --- Scan from 17h20m00s to 17h23m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=17h23m00s   !NEXT!

!* --- Scan from 17h23m00s to 17h31m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=17h31m00s   !NEXT!

!* --- Scan from 17h31m00s to 17h34m00s   Wed, 1997 Jan 15 --- *!
sname='J1221+28'  ra=12h21m31.690534s  dec= 28d13'58.50011"  qual=  0  calib='V'
tape=(1,+RUN)  write=(1,on)
stop=17h34m00s   !NEXT!

!* --- Scan from 17h34m00s to 17h42m00s   Wed, 1997 Jan 15 --- *!
sname='NGC4278'  ra=12h20m06.824300s  dec= 29d16'50.71600"  qual=  0  calib='M'
tape=(1,+RUN)  write=(1,on)
stop=17h42m00s   !NEXT!

tape=(1,STOP)    write=(1,off)   dur=0s 
stop=17h42m05s   !NEXT!
     !QUIT! 
