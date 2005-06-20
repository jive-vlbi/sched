!*  Schedule for VLA1      *!
!*  Experiment BD027    *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:   6.02  May 2005 *!
!* PI:       P.J.Diamond *!
!* Address:  NRAO *!
!*           P.O. Box O *!
!*           Socorro, NM 87801, USA *!
!*  *!
!* Phone:    1-505-835-7365 (work) or 1-505-835-2095 (home) *!
!* EMAIL:    pdiamond@nrao.edu (internet) *!
!* Fax:      1-505-835-7027 *!
!* Phone during observation: 1-505-835-7365 (work) or 1-505-835-2095 (home) *!
!* Observing mode: VLBA *!
!* Notes:    At VLA use antenna 27 *!
!*           its the standard VLBI antenna and has good Q band performance *!
!*  *!
!*  *!
!*  Start at 18h02m00s     Fri, 1995 Dec 29  Day of year  363   *!
program=BD027   
autoallocate=off
autoreverse=off

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!
!* ========= New tape starts at: 18h02m00s ========= *!

!* --- Scan from 18h02m00s to 18h13m00s   Fri, 1995 Dec 29 --- *!
sname='1749+096'  ra=17h51m32.818500s  dec= 09d39'00.72800"  qual=999  calib=' '
maxcaltime= 120
extlo = (1, 42.5101000000)
extlosideband = (1,U)
extlo = (3, 42.5101000000)
extlosideband = (3,U)
logging=STANDARD
nchan= 8
format=VLBA1:1
barrel=roll_auto
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)
ifchan=(1,A),(2,C),(3,A),(4,C),(5,A),(6,C),(7,A),(8,C)
sideband=(1,U),(2,U),(3,U),(4,U),(5,U),(6,U),(7,U),(8,U)
bits=(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1)
period=(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1)
level=(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,613.98),( 2,613.98),( 3,617.98),( 4,617.98),( 5,621.98),( 6,621.98)
bbsynth=( 7,625.98),( 8,625.98)
bbfilter=(1,4M),(2,4M),(3,4M),(4,4M),(5,4M),(6,4M),(7,4M),(8,4M)
pcal=OFF
pcalxbit1=(1,S1),(2,S3),(3,S5),(4,S7),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(1,S2),(2,S4),(3,S6),(4,S8),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxfreq1=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
samplerate=8M
track=(1,2),(2,4),(3,6),(4,8),(5,10),(6,12),(7,14),(8,16)
tape=(2,STOP)    write=(2,off) 
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
  date = 1995Dec29
stop=18h02m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=18h13m00s   !NEXT!

!* --- Scan from 18h15m00s to 18h26m00s   Fri, 1995 Dec 29 --- *!
sname='SPER'  ra=02h22m51.725045s  dec= 58d35'11.99044"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
stop=18h15m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=18h26m00s   !NEXT!

!* --- Scan from 18h28m00s to 18h39m00s   Fri, 1995 Dec 29 --- *!
sname='SPER'  ra=02h22m51.725045s  dec= 58d35'11.99044"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=18h28m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=18h39m00s   !NEXT!

!* --- Scan from 18h41m00s to 18h52m00s   Fri, 1995 Dec 29 --- *!
sname='SPER'  ra=02h22m51.725045s  dec= 58d35'11.99044"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=18h41m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=18h52m00s   !NEXT!

!* --- Scan from 18h54m00s to 19h05m00s   Fri, 1995 Dec 29 --- *!
sname='3C454.3'  ra=22h53m57.748200s  dec= 16d08'53.56282"  qual=999  calib=' '
track=(1,18),(2,20),(3,22),(4,24),(5,26),(6,28),(7,30),(8,32)
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
stop=18h54m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=19h05m00s   !NEXT!

!* --- Scan from 19h07m00s to 19h18m00s   Fri, 1995 Dec 29 --- *!
sname='SPER'  ra=02h22m51.725045s  dec= 58d35'11.99044"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
stop=19h07m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=19h18m00s   !NEXT!

!* --- Scan from 19h20m00s to 19h31m00s   Fri, 1995 Dec 29 --- *!
sname='SPER'  ra=02h22m51.725045s  dec= 58d35'11.99044"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=19h20m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=19h31m00s   !NEXT!

!* --- Scan from 19h33m00s to 19h44m00s   Fri, 1995 Dec 29 --- *!
sname='SPER'  ra=02h22m51.725045s  dec= 58d35'11.99044"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=19h33m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=19h44m00s   !NEXT!

!* --- Scan from 19h46m00s to 19h57m00s   Fri, 1995 Dec 29 --- *!
sname='SPER'  ra=02h22m51.725045s  dec= 58d35'11.99044"  qual=999  calib=' '
track=(1,3),(2,5),(3,7),(4,9),(5,11),(6,13),(7,15),(8,17)
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
stop=19h46m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=19h57m00s   !NEXT!

!* --- Scan from 19h59m00s to 20h10m00s   Fri, 1995 Dec 29 --- *!
sname='3C454.3'  ra=22h53m57.748200s  dec= 16d08'53.56282"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
stop=19h59m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=20h10m00s   !NEXT!

!* --- Scan from 20h12m00s to 20h23m00s   Fri, 1995 Dec 29 --- *!
sname='SPER'  ra=02h22m51.725045s  dec= 58d35'11.99044"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=20h12m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=20h23m00s   !NEXT!

!* --- Scan from 20h25m00s to 20h36m00s   Fri, 1995 Dec 29 --- *!
sname='SPER'  ra=02h22m51.725045s  dec= 58d35'11.99044"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=20h25m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=20h36m00s   !NEXT!

!* --- Scan from 20h38m00s to 20h49m00s   Fri, 1995 Dec 29 --- *!
sname='SPER'  ra=02h22m51.725045s  dec= 58d35'11.99044"  qual=999  calib=' '
track=(1,19),(2,21),(3,23),(4,25),(5,27),(6,29),(7,31),(8,33)
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
stop=20h38m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=20h49m00s   !NEXT!

!* --- Scan from 20h51m00s to 21h02m00s   Fri, 1995 Dec 29 --- *!
sname='1749+096'  ra=17h51m32.818500s  dec= 09d39'00.72800"  qual=999  calib=' '
bbsynth=( 1,616.72),( 2,616.72),( 3,620.72),( 4,620.72),( 5,624.72),( 6,624.72)
bbsynth=( 7,628.72),( 8,628.72)
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
stop=20h51m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=21h02m00s   !NEXT!

!* --- Scan from 21h04m00s to 21h15m00s   Fri, 1995 Dec 29 --- *!
sname='UHER'  ra=16h25m47.802614s  dec= 18d53'33.24533"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=21h04m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=21h15m00s   !NEXT!

!* --- Scan from 21h17m00s to 21h28m00s   Fri, 1995 Dec 29 --- *!
sname='UHER'  ra=16h25m47.802614s  dec= 18d53'33.24533"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=21h17m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=21h28m00s   !NEXT!

!* --- Scan from 21h30m00s to 21h41m00s   Fri, 1995 Dec 29 --- *!
sname='UHER'  ra=16h25m47.802614s  dec= 18d53'33.24533"  qual=999  calib=' '
track=(1,2),(2,4),(3,6),(4,8),(5,10),(6,12),(7,14),(8,16)
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
stop=21h30m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
stop=21h41m00s   !NEXT!

!* --- Scan from 21h43m00s to 21h54m00s   Fri, 1995 Dec 29 --- *!
sname='UHER'  ra=16h25m47.802614s  dec= 18d53'33.24533"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
stop=21h43m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
stop=21h54m00s   !NEXT!

!* --- Scan from 21h56m00s to 22h07m00s   Fri, 1995 Dec 29 --- *!
sname='1749+096'  ra=17h51m32.818500s  dec= 09d39'00.72800"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
stop=21h56m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
stop=22h07m00s   !NEXT!

!* --- Scan from 22h09m00s to 22h20m00s   Fri, 1995 Dec 29 --- *!
sname='UHER'  ra=16h25m47.802614s  dec= 18d53'33.24533"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
stop=22h09m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
stop=22h20m00s   !NEXT!

!* --- Scan from 22h22m00s to 22h33m00s   Fri, 1995 Dec 29 --- *!
sname='UHER'  ra=16h25m47.802614s  dec= 18d53'33.24533"  qual=999  calib=' '
track=(1,18),(2,20),(3,22),(4,24),(5,26),(6,28),(7,30),(8,32)
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
stop=22h22m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
stop=22h33m00s   !NEXT!

!* --- Scan from 22h35m00s to 22h46m00s   Fri, 1995 Dec 29 --- *!
sname='UHER'  ra=16h25m47.802614s  dec= 18d53'33.24533"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
stop=22h35m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
stop=22h46m00s   !NEXT!

!* --- Scan from 22h48m00s to 22h59m00s   Fri, 1995 Dec 29 --- *!
sname='1749+096'  ra=17h51m32.818500s  dec= 09d39'00.72800"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
stop=22h48m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
stop=22h59m00s   !NEXT!

!* =====  POSTPASSING DRIVE 1 ==== *!
tape=(1,STOP)    write=(1,off)   dur=0s 
stop=22h59m05s  !NEXT! 
tape=(1,POSTPASS)  stop=22h59m07s
  !NEXT! 
stop=22h59m09s   !NEXT!
     !QUIT! 
