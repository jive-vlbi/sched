!*  Schedule for VLBA_SC   *!
!*  Experiment VT002    *!
!* Schedule Version:       6.00 *!
!* Processed by SCHED version:   6.0  Release: March 2005 *!
!* PI:       Rikako OKAYASU *!
!* Address:  Institute of Space and Astronautical Science *!
!*           3-1-1 Yoshinodai *!
!*           Sagamihara, Kanagawa 229 *!
!*           Japan *!
!* Phone:    +81-427-51-3911 *!
!* EMAIL:    okayasu@vsop.isas.ac.jp *!
!* Fax:      +81-427-51-3972 *!
!* Phone during observation: +81-427-51-3911 *!
!* Observing mode: *!
!* Notes:    Please send your tapes to Socorro within 2 days. *!
!*  *!
!*  *!
!*  *!
!*  Start at 06h00m00s     Sun, 1996 Jun 16  Day of year  168   *!
program=VT002   
autoallocate=on
autoreverse=on

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 06h00m00s to 06h06m30s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
maxcaltime= 120
fe=(1,6cm),(3,6cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
logging=STANDARD
nchan= 2
format=VLBA1:4
barrel=roll_off
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2)
ifchan=(1,C),(2,C)
sideband=(1,U),(2,U)
bits=(1,2),(2,2)
period=(1,1),(2,1)
level=(1,-1),(2,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,862.00),( 2,878.00)
bbfilter=(1,16M),(2,16M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S1),(3,S1),(4,S2),(5,S1),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(1,S2),(2,S2),(3,M1),(4,M2),(5,S2),(6,OFF),(7,OFF),(8,OFF)
pcalxfreq1=(1,1000),(2,13000),(3,0),(4,0),(5,2000),(6,0),(7,0),(8,0)
pcalxfreq2=(1,1000),(2,13000),(3,0),(4,0),(5,2000),(6,0),(7,0),(8,0)
samplerate=32M
tape=(1,STOP) write=(1,off)
  date = 1996Jun16
stop=06h00m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h06m30s   !NEXT!

!* --- Scan from 06h07m20s to 06h13m50s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=06h07m20s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h13m50s   !NEXT!

!* --- Scan from 06h18m50s to 06h25m20s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=06h18m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h25m20s   !NEXT!

!* --- Scan from 06h26m10s to 06h32m40s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=06h26m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h32m40s   !NEXT!

!* --- Scan from 06h33m30s to 06h40m00s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=06h33m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h40m00s   !NEXT!

!* --- Scan from 06h40m50s to 06h47m20s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=06h40m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h47m20s   !NEXT!

!* --- Scan from 06h48m10s to 06h54m40s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=06h48m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h54m40s   !NEXT!

!* --- Scan from 06h55m30s to 07h02m00s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=06h55m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h02m00s   !NEXT!

!* --- Scan from 07h02m50s to 07h09m20s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h02m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h09m20s   !NEXT!

!* --- Scan from 07h10m10s to 07h16m40s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h10m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h16m40s   !NEXT!

!* --- Scan from 07h17m30s to 07h24m00s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h17m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h24m00s   !NEXT!

!* --- Scan from 07h24m50s to 07h31m20s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h24m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h31m20s   !NEXT!

!* --- Scan from 07h32m10s to 07h38m40s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h32m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h38m40s   !NEXT!

!* --- Scan from 07h39m30s to 07h46m00s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h39m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h46m00s   !NEXT!

!* --- Scan from 07h51m00s to 07h57m30s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h51m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h57m30s   !NEXT!

!* --- Scan from 07h58m20s to 08h04m50s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=07h58m20s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h04m50s   !NEXT!

!* --- Scan from 08h05m40s to 08h12m10s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h05m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h12m10s   !NEXT!

!* --- Scan from 08h13m00s to 08h19m30s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h13m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h19m30s   !NEXT!

!* --- Scan from 08h20m20s to 08h26m50s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h20m20s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h26m50s   !NEXT!

!* --- Scan from 08h27m40s to 08h34m10s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h27m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h34m10s   !NEXT!

!* --- Scan from 08h35m00s to 08h41m30s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h35m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h41m30s   !NEXT!

!* --- Scan from 08h42m20s to 08h48m50s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h42m20s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h48m50s   !NEXT!

!* --- Scan from 08h49m40s to 08h56m10s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h49m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h56m10s   !NEXT!

!* --- Scan from 08h57m00s to 09h03m30s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=08h57m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h03m30s   !NEXT!

!* --- Scan from 09h04m20s to 09h10m50s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h04m20s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h10m50s   !NEXT!

!* --- Scan from 09h11m40s to 09h18m10s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h11m40s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h18m10s   !NEXT!

!* --- Scan from 09h33m10s to 09h39m40s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h33m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h39m40s   !NEXT!

!* --- Scan from 09h40m30s to 09h47m00s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h40m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h47m00s   !NEXT!

!* --- Scan from 09h47m50s to 09h54m20s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h47m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h54m20s   !NEXT!

!* --- Scan from 09h55m10s to 10h01m40s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=09h55m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h01m40s   !NEXT!

!* --- Scan from 10h02m30s to 10h09m00s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h02m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h09m00s   !NEXT!

!* --- Scan from 10h09m50s to 10h16m20s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h09m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h16m20s   !NEXT!

!* --- Scan from 10h17m10s to 10h23m40s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h17m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h23m40s   !NEXT!

!* --- Scan from 10h24m30s to 10h31m00s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h24m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h31m00s   !NEXT!

!* --- Scan from 10h31m50s to 10h38m20s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h31m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h38m20s   !NEXT!

!* --- Scan from 10h39m10s to 10h45m40s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h39m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h45m40s   !NEXT!

!* --- Scan from 10h46m30s to 10h53m00s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h46m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=10h53m00s   !NEXT!

!* --- Scan from 10h53m50s to 11h00m20s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=10h53m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h00m20s   !NEXT!

!* --- Scan from 11h15m20s to 11h21m50s   Sun, 1996 Jun 16 --- *!
sname='1928+738'  ra=19h27m48.495160s  dec= 73d58'01.57005"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=11h15m20s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h21m50s   !NEXT!

!* --- Scan from 11h36m50s to 11h43m20s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=11h36m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h43m20s   !NEXT!

!* --- Scan from 11h44m10s to 11h50m40s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=11h44m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h50m40s   !NEXT!

!* --- Scan from 11h51m30s to 11h58m00s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=11h51m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=11h58m00s   !NEXT!

!* --- Scan from 11h58m50s to 12h05m20s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=11h58m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h05m20s   !NEXT!

!* --- Scan from 12h06m10s to 12h12m40s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h06m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h12m40s   !NEXT!

!* --- Scan from 12h13m30s to 12h20m00s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h13m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h20m00s   !NEXT!

!* --- Scan from 12h20m50s to 12h27m20s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h20m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h27m20s   !NEXT!

!* --- Scan from 12h28m10s to 12h34m40s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h28m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h34m40s   !NEXT!

!* --- Scan from 12h35m30s to 12h42m00s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h35m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h42m00s   !NEXT!

!* --- Scan from 12h42m50s to 12h49m20s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h42m50s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h49m20s   !NEXT!

!* --- Scan from 12h50m10s to 12h56m40s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h50m10s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=12h56m40s   !NEXT!

!* --- Scan from 12h57m30s to 13h04m00s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=12h57m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h04m00s   !NEXT!

!* --- Scan from 13h19m00s to 13h25m30s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=13h19m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=13h25m30s   !NEXT!

!* --- Scan from 13h40m30s to 14h40m30s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=13h40m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=14h40m30s   !NEXT!

!* --- Scan from 14h42m30s to 15h42m30s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=14h42m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=15h42m30s   !NEXT!

!* --- Scan from 15h44m30s to 16h44m30s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=15h44m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=16h44m30s   !NEXT!

!* --- Scan from 16h46m30s to 17h46m30s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=16h46m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=17h46m30s   !NEXT!

!* --- Scan from 17h48m30s to 18h48m30s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=17h48m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=18h48m30s   !NEXT!

!* --- Scan from 18h50m30s to 19h50m30s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=18h50m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=19h50m30s   !NEXT!

!* --- Scan from 19h52m30s to 20h52m30s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=19h52m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=20h52m30s   !NEXT!

!* --- Scan from 20h54m30s to 21h54m30s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=20h54m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=21h54m30s   !NEXT!

!* --- Scan from 21h56m30s to 22h56m30s   Sun, 1996 Jun 16 --- *!
sname='0552+398'  ra=05h55m30.805614s  dec= 39d48'49.16500"  qual=999  calib='V'
tape=(1,STOP) write=(1,off)
stop=21h56m30s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=22h56m30s   !NEXT!

tape=(2,STOP)    write=(2,off)   dur=0s 
stop=22h56m35s   !NEXT!
     !QUIT! 
