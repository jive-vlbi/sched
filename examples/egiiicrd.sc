!*  Schedule for VLBA_SC   *!
!*  Experiment egiii    *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:   6.0  Release: March 2005 *!
!* PI:       J.M. Wrobel *!
!* Address:  NRAO, P.O. Box O *!
!*           Socorro, NM 87801 *!
!*           USA *!
!*  *!
!* Phone:    +1-505-835-7392 (work) or +1-505-835-3972 (home) *!
!* EMAIL:    jwrobel@nrao.edu (internet) *!
!* Fax:      +1-505-835-3972 *!
!* Phone during observation: +1-505-835-7392 (work) or +1-505-835-3972 (home) *!
!* Observing mode: MkIII mode B double speed 18&4cm Network standards *!
!* Notes:    Please use all available antennas. *!
!*           Fringe finder, manual pulse cal: J1229+02 (3C273) *!
!*           Amplitude check                : J1310+32, J1407+28 (OQ208) *!
!*  *!
!*  Start at 13h03m30s     Sun, 1994 Oct 09  Day of year  282   *!
program=egiii   
autoallocate=off
autoreverse=off

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!
!* ========= New tape starts at: 13h03m30s ========= *!

!* --- Scan from 13h03m30s to 13h10m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
maxcaltime= 120
fe=(2,4cm),(4,4cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1, 7.6),( 2,15.4),( 3,15.4)
logging=STANDARD
nchan=14
format=MARKIII
barrel=roll_off
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,1),(3,2),(4,2),(5,3),(6,3),(7,4),(8,4),(9,5),(10,5),(11,6)
baseband=(12,6),(13,7),(14,7)
ifchan=(1,B),(2,B),(3,B),(4,B),(5,B),(6,B),(7,B),(8,B),(9,B),(10,B),(11,B)
ifchan=(12,B),(13,B),(14,B)
sideband=(1,L),(2,U),(3,L),(4,U),(5,L),(6,U),(7,L),(8,U),(9,L),(10,U),(11,L)
sideband=(12,U),(13,L),(14,U)
bits=(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1),(11,1),(12,1)
bits=(13,1),(14,1)
period=(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1),(11,1)
period=(12,1),(13,1),(14,1)
level=(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1),(9,-1),(10,-1)
level=(11,-1),(12,-1),(13,-1),(14,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,792.99),( 2,792.99),( 3,800.99),( 4,800.99),( 5,808.99),( 6,808.99)
bbsynth=( 7,816.99),( 8,816.99),( 9,824.99),(10,824.99),(11,832.99),(12,832.99)
bbsynth=(13,840.99),(14,840.99)
bbfilter=(1,4M),(2,4M),(3,4M),(4,4M),(5,4M),(6,4M),(7,4M),(8,4M),(9,4M),(10,4M)
bbfilter=(11,4M),(12,4M),(13,4M),(14,4M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S5),(4,S7),(5,S9),(6,S11),(7,S13),(8,S1)
pcalxbit2=(1,S2),(2,S4),(3,S6),(4,S8),(5,S10),(6,S12),(7,S14),(8,S2)
pcalxfreq1=(1,990),(2,990),(3,990),(4,990),(5,990),(6,990),(7,990),(8,2990)
pcalxfreq2=(1,10),(2,10),(3,10),(4,10),(5,10),(6,10),(7,10),(8,3010)
samplerate=8M
track=(1,18),(2,4),(3,20),(4,6),(5,22),(6,8),(7,24),(8,10),(9,26),(10,12)
track=(11,28),(12,14),(13,30),(14,16)
tape=(2,STOP)    write=(2,off) 
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
  date = 1994Oct09
stop=13h03m30s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=13h10m00s   !NEXT!

!* --- Scan from 13h13m30s to 13h20m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,C),(2,C),(3,C),(4,C),(5,C),(6,C),(7,C),(8,C),(9,C),(10,C),(11,C)
ifchan=(12,C),(13,C),(14,C)
sideband=(1,U),(2,L),(3,U),(4,L),(5,U),(6,L),(7,U),(8,L),(9,U),(10,L),(11,U)
sideband=(12,L),(13,U),(14,L)
bbsynth=( 1,757.01),( 2,757.01),( 3,749.01),( 4,749.01),( 5,741.01),( 6,741.01)
bbsynth=( 7,733.01),( 8,733.01),( 9,725.01),(10,725.01),(11,717.01),(12,717.01)
bbsynth=(13,709.01),(14,709.01)
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
stop=13h13m30s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=13h20m00s   !NEXT!

!* --- Scan from 13h34m00s to 13h43m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,B),(3,B),(4,B),(5,B),(6,B),(7,B),(8,B),(9,B),(10,B),(11,B)
ifchan=(12,B),(13,B),(14,B)
sideband=(1,L),(2,U),(3,L),(4,U),(5,L),(6,U),(7,L),(8,U),(9,L),(10,U),(11,L)
sideband=(12,U),(13,L),(14,U)
bbsynth=( 1,792.99),( 2,792.99),( 3,800.99),( 4,800.99),( 5,808.99),( 6,808.99)
bbsynth=( 7,816.99),( 8,816.99),( 9,824.99),(10,824.99),(11,832.99),(12,832.99)
bbsynth=(13,840.99),(14,840.99)
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
stop=13h34m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=13h43m00s   !NEXT!

!* --- Scan from 13h46m00s to 13h50m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=13h46m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=13h50m00s   !NEXT!

!* --- Scan from 13h53m00s to 13h57m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=13h53m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=13h57m00s   !NEXT!

!* --- Scan from 14h01m00s to 14h10m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=14h01m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=14h10m00s   !NEXT!

!* --- Scan from 14h14m00s to 14h23m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,C),(2,C),(3,C),(4,C),(5,C),(6,C),(7,C),(8,C),(9,C),(10,C),(11,C)
ifchan=(12,C),(13,C),(14,C)
sideband=(1,U),(2,L),(3,U),(4,L),(5,U),(6,L),(7,U),(8,L),(9,U),(10,L),(11,U)
sideband=(12,L),(13,U),(14,L)
bbsynth=( 1,757.01),( 2,757.01),( 3,749.01),( 4,749.01),( 5,741.01),( 6,741.01)
bbsynth=( 7,733.01),( 8,733.01),( 9,725.01),(10,725.01),(11,717.01),(12,717.01)
bbsynth=(13,709.01),(14,709.01)
track=(1,19),(2,5),(3,21),(4,7),(5,23),(6,9),(7,25),(8,11),(9,27),(10,13)
track=(11,29),(12,15),(13,31),(14,17)
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
stop=14h14m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=14h23m00s   !NEXT!

!* --- Scan from 14h26m00s to 14h30m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
stop=14h26m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
stop=14h30m00s   !NEXT!

!* --- Scan from 14h44m00s to 14h53m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,B),(3,B),(4,B),(5,B),(6,B),(7,B),(8,B),(9,B),(10,B),(11,B)
ifchan=(12,B),(13,B),(14,B)
sideband=(1,L),(2,U),(3,L),(4,U),(5,L),(6,U),(7,L),(8,U),(9,L),(10,U),(11,L)
sideband=(12,U),(13,L),(14,U)
bbsynth=( 1,792.99),( 2,792.99),( 3,800.99),( 4,800.99),( 5,808.99),( 6,808.99)
bbsynth=( 7,816.99),( 8,816.99),( 9,824.99),(10,824.99),(11,832.99),(12,832.99)
bbsynth=(13,840.99),(14,840.99)
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=14h44m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=14h53m00s   !NEXT!

!* --- Scan from 14h56m00s to 15h00m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=14h56m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=15h00m00s   !NEXT!

!* --- Scan from 15h03m00s to 15h07m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
stop=15h03m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
stop=15h07m00s   !NEXT!

!* --- Scan from 15h11m00s to 15h20m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
track=(1,18),(2,4),(3,20),(4,6),(5,22),(6,8),(7,24),(8,10),(9,26),(10,12)
track=(11,28),(12,14),(13,30),(14,16)
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
stop=15h11m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
stop=15h20m00s   !NEXT!

!* --- Scan from 15h24m00s to 15h33m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,C),(2,C),(3,C),(4,C),(5,C),(6,C),(7,C),(8,C),(9,C),(10,C),(11,C)
ifchan=(12,C),(13,C),(14,C)
sideband=(1,U),(2,L),(3,U),(4,L),(5,U),(6,L),(7,U),(8,L),(9,U),(10,L),(11,U)
sideband=(12,L),(13,U),(14,L)
bbsynth=( 1,757.01),( 2,757.01),( 3,749.01),( 4,749.01),( 5,741.01),( 6,741.01)
bbsynth=( 7,733.01),( 8,733.01),( 9,725.01),(10,725.01),(11,717.01),(12,717.01)
bbsynth=(13,709.01),(14,709.01)
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
stop=15h24m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
stop=15h33m00s   !NEXT!

!* --- Scan from 15h36m00s to 15h40m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
stop=15h36m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
stop=15h40m00s   !NEXT!

!* --- Scan from 15h54m00s to 16h03m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,B),(3,B),(4,B),(5,B),(6,B),(7,B),(8,B),(9,B),(10,B),(11,B)
ifchan=(12,B),(13,B),(14,B)
sideband=(1,L),(2,U),(3,L),(4,U),(5,L),(6,U),(7,L),(8,U),(9,L),(10,U),(11,L)
sideband=(12,U),(13,L),(14,U)
bbsynth=( 1,792.99),( 2,792.99),( 3,800.99),( 4,800.99),( 5,808.99),( 6,808.99)
bbsynth=( 7,816.99),( 8,816.99),( 9,824.99),(10,824.99),(11,832.99),(12,832.99)
bbsynth=(13,840.99),(14,840.99)
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
stop=15h54m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
stop=16h03m00s   !NEXT!

!* --- Scan from 16h06m00s to 16h10m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
stop=16h06m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
stop=16h10m00s   !NEXT!

!* --- Scan from 16h13m00s to 16h17m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
track=(1,19),(2,5),(3,21),(4,7),(5,23),(6,9),(7,25),(8,11),(9,27),(10,13)
track=(11,29),(12,15),(13,31),(14,17)
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
stop=16h13m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
stop=16h17m00s   !NEXT!

!* --- Scan from 16h21m00s to 16h30m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
stop=16h21m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
stop=16h30m00s   !NEXT!

!* --- Scan from 16h34m00s to 16h43m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,C),(2,C),(3,C),(4,C),(5,C),(6,C),(7,C),(8,C),(9,C),(10,C),(11,C)
ifchan=(12,C),(13,C),(14,C)
sideband=(1,U),(2,L),(3,U),(4,L),(5,U),(6,L),(7,U),(8,L),(9,U),(10,L),(11,U)
sideband=(12,L),(13,U),(14,L)
bbsynth=( 1,757.01),( 2,757.01),( 3,749.01),( 4,749.01),( 5,741.01),( 6,741.01)
bbsynth=( 7,733.01),( 8,733.01),( 9,725.01),(10,725.01),(11,717.01),(12,717.01)
bbsynth=(13,709.01),(14,709.01)
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
stop=16h34m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
stop=16h43m00s   !NEXT!

!* --- Scan from 16h46m00s to 16h50m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
stop=16h46m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
stop=16h50m00s   !NEXT!

!* --- Scan from 16h53m30s to 17h00m00s   Sun, 1994 Oct 09 --- *!
sname='J1229+02'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=999  calib='V'
track=(1,18),(2,4),(3,20),(4,6),(5,22),(6,8),(7,24),(8,10),(9,26),(10,12)
track=(11,28),(12,14),(13,30),(14,16)
tape=( 1,STOP) write=( 1,off) head=( 1,-223) 
stop=16h53m30s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
stop=17h00m00s   !NEXT!

!* --- Scan from 17h13m30s to 17h20m00s   Sun, 1994 Oct 09 --- *!
sname='J1229+02'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,B),(3,B),(4,B),(5,B),(6,B),(7,B),(8,B),(9,B),(10,B),(11,B)
ifchan=(12,B),(13,B),(14,B)
sideband=(1,L),(2,U),(3,L),(4,U),(5,L),(6,U),(7,L),(8,U),(9,L),(10,U),(11,L)
sideband=(12,U),(13,L),(14,U)
bbsynth=( 1,792.99),( 2,792.99),( 3,800.99),( 4,800.99),( 5,808.99),( 6,808.99)
bbsynth=( 7,816.99),( 8,816.99),( 9,824.99),(10,824.99),(11,832.99),(12,832.99)
bbsynth=(13,840.99),(14,840.99)
tape=( 1,STOP) write=( 1,off) head=( 1,-223) 
stop=17h13m30s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
stop=17h20m00s   !NEXT!

!* --- Scan from 17h24m00s to 17h33m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1, 127) 
stop=17h24m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
stop=17h33m00s   !NEXT!

!* --- Scan from 17h36m00s to 17h40m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 127) 
stop=17h36m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
stop=17h40m00s   !NEXT!

!* --- Scan from 17h43m00s to 17h47m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
track=(1,19),(2,5),(3,21),(4,7),(5,23),(6,9),(7,25),(8,11),(9,27),(10,13)
track=(11,29),(12,15),(13,31),(14,17)
tape=( 1,STOP) write=( 1,off) head=( 1,-223) 
stop=17h43m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
stop=17h47m00s   !NEXT!

!* --- Scan from 17h51m00s to 18h00m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,-223) 
stop=17h51m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
stop=18h00m00s   !NEXT!

!* --- Scan from 18h04m00s to 18h13m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,C),(2,C),(3,C),(4,C),(5,C),(6,C),(7,C),(8,C),(9,C),(10,C),(11,C)
ifchan=(12,C),(13,C),(14,C)
sideband=(1,U),(2,L),(3,U),(4,L),(5,U),(6,L),(7,U),(8,L),(9,U),(10,L),(11,U)
sideband=(12,L),(13,U),(14,L)
bbsynth=( 1,757.01),( 2,757.01),( 3,749.01),( 4,749.01),( 5,741.01),( 6,741.01)
bbsynth=( 7,733.01),( 8,733.01),( 9,725.01),(10,725.01),(11,717.01),(12,717.01)
bbsynth=(13,709.01),(14,709.01)
tape=( 1,STOP) write=( 1,off) head=( 1, 127) 
stop=18h04m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
stop=18h13m00s   !NEXT!

!* --- Scan from 18h16m00s to 18h20m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 127) 
stop=18h16m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
stop=18h20m00s   !NEXT!

!* --- Scan from 18h34m00s to 18h43m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,B),(3,B),(4,B),(5,B),(6,B),(7,B),(8,B),(9,B),(10,B),(11,B)
ifchan=(12,B),(13,B),(14,B)
sideband=(1,L),(2,U),(3,L),(4,U),(5,L),(6,U),(7,L),(8,U),(9,L),(10,U),(11,L)
sideband=(12,U),(13,L),(14,U)
bbsynth=( 1,792.99),( 2,792.99),( 3,800.99),( 4,800.99),( 5,808.99),( 6,808.99)
bbsynth=( 7,816.99),( 8,816.99),( 9,824.99),(10,824.99),(11,832.99),(12,832.99)
bbsynth=(13,840.99),(14,840.99)
track=(1,18),(2,4),(3,20),(4,6),(5,22),(6,8),(7,24),(8,10),(9,26),(10,12)
track=(11,28),(12,14),(13,30),(14,16)
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
stop=18h34m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
stop=18h43m00s   !NEXT!

!* --- Scan from 18h46m00s to 18h50m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
stop=18h46m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
stop=18h50m00s   !NEXT!

!* --- Scan from 18h53m00s to 18h57m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
stop=18h53m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
stop=18h57m00s   !NEXT!

!* --- Scan from 19h01m00s to 19h10m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1, 175) 
stop=19h01m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
stop=19h10m00s   !NEXT!

!* --- Scan from 19h13m30s to 19h20m00s   Sun, 1994 Oct 09 --- *!
sname='J1229+02'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 175) 
stop=19h13m30s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
stop=19h20m00s   !NEXT!

!* --- Scan from 19h23m30s to 19h30m00s   Sun, 1994 Oct 09 --- *!
sname='J1229+02'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=999  calib='V'
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,C),(2,C),(3,C),(4,C),(5,C),(6,C),(7,C),(8,C),(9,C),(10,C),(11,C)
ifchan=(12,C),(13,C),(14,C)
sideband=(1,U),(2,L),(3,U),(4,L),(5,U),(6,L),(7,U),(8,L),(9,U),(10,L),(11,U)
sideband=(12,L),(13,U),(14,L)
bbsynth=( 1,757.01),( 2,757.01),( 3,749.01),( 4,749.01),( 5,741.01),( 6,741.01)
bbsynth=( 7,733.01),( 8,733.01),( 9,725.01),(10,725.01),(11,717.01),(12,717.01)
bbsynth=(13,709.01),(14,709.01)
track=(1,19),(2,5),(3,21),(4,7),(5,23),(6,9),(7,25),(8,11),(9,27),(10,13)
track=(11,29),(12,15),(13,31),(14,17)
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
stop=19h23m30s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
stop=19h30m00s   !NEXT!

!* --- Scan from 19h34m00s to 19h43m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
stop=19h34m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
stop=19h43m00s   !NEXT!

!* --- Scan from 19h46m00s to 19h50m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 175) 
stop=19h46m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
stop=19h50m00s   !NEXT!

!* --- Scan from 20h04m00s to 20h13m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,B),(3,B),(4,B),(5,B),(6,B),(7,B),(8,B),(9,B),(10,B),(11,B)
ifchan=(12,B),(13,B),(14,B)
sideband=(1,L),(2,U),(3,L),(4,U),(5,L),(6,U),(7,L),(8,U),(9,L),(10,U),(11,L)
sideband=(12,U),(13,L),(14,U)
bbsynth=( 1,792.99),( 2,792.99),( 3,800.99),( 4,800.99),( 5,808.99),( 6,808.99)
bbsynth=( 7,816.99),( 8,816.99),( 9,824.99),(10,824.99),(11,832.99),(12,832.99)
bbsynth=(13,840.99),(14,840.99)
tape=( 1,STOP) write=( 1,off) head=( 1, 175) 
stop=20h04m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
stop=20h13m00s   !NEXT!

!* --- Scan from 20h16m00s to 20h20m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 175) 
stop=20h16m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
stop=20h20m00s   !NEXT!

!* --- Scan from 20h23m00s to 20h27m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
track=(1,18),(2,4),(3,20),(4,6),(5,22),(6,8),(7,24),(8,10),(9,26),(10,12)
track=(11,28),(12,14),(13,30),(14,16)
tape=( 1,STOP) write=( 1,off) head=( 1,-127) 
stop=20h23m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
stop=20h27m00s   !NEXT!

!* --- Scan from 20h31m00s to 20h40m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1,-127) 
stop=20h31m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
stop=20h40m00s   !NEXT!

!* --- Scan from 20h44m00s to 20h53m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,C),(2,C),(3,C),(4,C),(5,C),(6,C),(7,C),(8,C),(9,C),(10,C),(11,C)
ifchan=(12,C),(13,C),(14,C)
sideband=(1,U),(2,L),(3,U),(4,L),(5,U),(6,L),(7,U),(8,L),(9,U),(10,L),(11,U)
sideband=(12,L),(13,U),(14,L)
bbsynth=( 1,757.01),( 2,757.01),( 3,749.01),( 4,749.01),( 5,741.01),( 6,741.01)
bbsynth=( 7,733.01),( 8,733.01),( 9,725.01),(10,725.01),(11,717.01),(12,717.01)
bbsynth=(13,709.01),(14,709.01)
tape=( 1,STOP) write=( 1,off) head=( 1, 223) 
stop=20h44m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
stop=20h53m00s   !NEXT!

!* --- Scan from 20h56m00s to 21h00m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 223) 
stop=20h56m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
stop=21h00m00s   !NEXT!

!* --- Scan from 21h14m00s to 21h23m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,B),(3,B),(4,B),(5,B),(6,B),(7,B),(8,B),(9,B),(10,B),(11,B)
ifchan=(12,B),(13,B),(14,B)
sideband=(1,L),(2,U),(3,L),(4,U),(5,L),(6,U),(7,L),(8,U),(9,L),(10,U),(11,L)
sideband=(12,U),(13,L),(14,U)
bbsynth=( 1,792.99),( 2,792.99),( 3,800.99),( 4,800.99),( 5,808.99),( 6,808.99)
bbsynth=( 7,816.99),( 8,816.99),( 9,824.99),(10,824.99),(11,832.99),(12,832.99)
bbsynth=(13,840.99),(14,840.99)
track=(1,19),(2,5),(3,21),(4,7),(5,23),(6,9),(7,25),(8,11),(9,27),(10,13)
track=(11,29),(12,15),(13,31),(14,17)
tape=( 1,STOP) write=( 1,off) head=( 1,-127) 
stop=21h14m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
stop=21h23m00s   !NEXT!

!* --- Scan from 21h26m00s to 21h30m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-127) 
stop=21h26m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
stop=21h30m00s   !NEXT!

!* --- Scan from 21h33m00s to 21h37m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-127) 
stop=21h33m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
stop=21h37m00s   !NEXT!

!* --- Scan from 21h41m00s to 21h50m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1, 223) 
stop=21h41m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
stop=21h50m00s   !NEXT!

!* --- Scan from 21h54m00s to 22h03m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,C),(2,C),(3,C),(4,C),(5,C),(6,C),(7,C),(8,C),(9,C),(10,C),(11,C)
ifchan=(12,C),(13,C),(14,C)
sideband=(1,U),(2,L),(3,U),(4,L),(5,U),(6,L),(7,U),(8,L),(9,U),(10,L),(11,U)
sideband=(12,L),(13,U),(14,L)
bbsynth=( 1,757.01),( 2,757.01),( 3,749.01),( 4,749.01),( 5,741.01),( 6,741.01)
bbsynth=( 7,733.01),( 8,733.01),( 9,725.01),(10,725.01),(11,717.01),(12,717.01)
bbsynth=(13,709.01),(14,709.01)
tape=( 1,STOP) write=( 1,off) head=( 1, 223) 
stop=21h54m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
stop=22h03m00s   !NEXT!

!* --- Scan from 22h06m00s to 22h10m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
track=(1,18),(2,4),(3,20),(4,6),(5,22),(6,8),(7,24),(8,10),(9,26),(10,12)
track=(11,28),(12,14),(13,30),(14,16)
tape=( 1,STOP) write=( 1,off) head=( 1, -79) 
stop=22h06m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
stop=22h10m00s   !NEXT!

!* --- Scan from 22h24m00s to 22h33m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,B),(3,B),(4,B),(5,B),(6,B),(7,B),(8,B),(9,B),(10,B),(11,B)
ifchan=(12,B),(13,B),(14,B)
sideband=(1,L),(2,U),(3,L),(4,U),(5,L),(6,U),(7,L),(8,U),(9,L),(10,U),(11,L)
sideband=(12,U),(13,L),(14,U)
bbsynth=( 1,792.99),( 2,792.99),( 3,800.99),( 4,800.99),( 5,808.99),( 6,808.99)
bbsynth=( 7,816.99),( 8,816.99),( 9,824.99),(10,824.99),(11,832.99),(12,832.99)
bbsynth=(13,840.99),(14,840.99)
tape=( 1,STOP) write=( 1,off) head=( 1, -79) 
stop=22h24m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
stop=22h33m00s   !NEXT!

!* --- Scan from 22h36m00s to 22h40m00s   Sun, 1994 Oct 09 --- *!
sname='J1310+32'  ra=13h10m28.663849s  dec= 32d20'43.78289"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, -79) 
stop=22h36m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
stop=22h40m00s   !NEXT!

!* --- Scan from 22h43m00s to 22h47m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, -79) 
stop=22h43m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
stop=22h47m00s   !NEXT!

!* --- Scan from 22h51m00s to 23h00m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1, 271) 
stop=22h51m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
stop=23h00m00s   !NEXT!

!* --- Scan from 23h04m00s to 23h13m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,C),(2,C),(3,C),(4,C),(5,C),(6,C),(7,C),(8,C),(9,C),(10,C),(11,C)
ifchan=(12,C),(13,C),(14,C)
sideband=(1,U),(2,L),(3,U),(4,L),(5,U),(6,L),(7,U),(8,L),(9,U),(10,L),(11,U)
sideband=(12,L),(13,U),(14,L)
bbsynth=( 1,757.01),( 2,757.01),( 3,749.01),( 4,749.01),( 5,741.01),( 6,741.01)
bbsynth=( 7,733.01),( 8,733.01),( 9,725.01),(10,725.01),(11,717.01),(12,717.01)
bbsynth=(13,709.01),(14,709.01)
tape=( 1,STOP) write=( 1,off) head=( 1, 271) 
stop=23h04m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
stop=23h13m00s   !NEXT!

!* --- Scan from 23h16m00s to 23h20m00s   Sun, 1994 Oct 09 --- *!
sname='M84'  ra=12h25m03.732000s  dec= 12d53'13.31000"  qual=999  calib=' '
tape=( 1,STOP) write=( 1,off) head=( 1, 271) 
stop=23h16m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
stop=23h20m00s   !NEXT!

!* --- Scan from 23h33m30s to 23h40m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
fe=(2,4cm),(4,4cm)
synth=( 1, 7.6),( 2,15.4)
ifchan=(1,B),(2,B),(3,B),(4,B),(5,B),(6,B),(7,B),(8,B),(9,B),(10,B),(11,B)
ifchan=(12,B),(13,B),(14,B)
sideband=(1,L),(2,U),(3,L),(4,U),(5,L),(6,U),(7,L),(8,U),(9,L),(10,U),(11,L)
sideband=(12,U),(13,L),(14,U)
bbsynth=( 1,792.99),( 2,792.99),( 3,800.99),( 4,800.99),( 5,808.99),( 6,808.99)
bbsynth=( 7,816.99),( 8,816.99),( 9,824.99),(10,824.99),(11,832.99),(12,832.99)
bbsynth=(13,840.99),(14,840.99)
track=(1,19),(2,5),(3,21),(4,7),(5,23),(6,9),(7,25),(8,11),(9,27),(10,13)
track=(11,29),(12,15),(13,31),(14,17)
tape=( 1,STOP) write=( 1,off) head=( 1, -79) 
stop=23h33m30s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
stop=23h40m00s   !NEXT!

!* --- Scan from 23h43m30s to 23h50m00s   Sun, 1994 Oct 09 --- *!
sname='J1407+28'  ra=14h07m00.394413s  dec= 28d27'14.69021"  qual=999  calib='V'
fe=(1,20cm),(3,20cm)
synth=( 1,15.4),( 2, 2.4)
ifchan=(1,C),(2,C),(3,C),(4,C),(5,C),(6,C),(7,C),(8,C),(9,C),(10,C),(11,C)
ifchan=(12,C),(13,C),(14,C)
sideband=(1,U),(2,L),(3,U),(4,L),(5,U),(6,L),(7,U),(8,L),(9,U),(10,L),(11,U)
sideband=(12,L),(13,U),(14,L)
bbsynth=( 1,757.01),( 2,757.01),( 3,749.01),( 4,749.01),( 5,741.01),( 6,741.01)
bbsynth=( 7,733.01),( 8,733.01),( 9,725.01),(10,725.01),(11,717.01),(12,717.01)
bbsynth=(13,709.01),(14,709.01)
tape=( 1,STOP) write=( 1,off) head=( 1, -79) 
stop=23h43m30s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
stop=23h50m00s   !NEXT!

!* =====  POSTPASSING DRIVE 1 ==== *!
tape=(1,STOP)    write=(1,off)   dur=0s 
stop=23h50m05s  !NEXT! 
tape=(1,POSTPASS)  stop=23h50m07s
  !NEXT! 
stop=23h50m09s   !NEXT!
     !QUIT! 
