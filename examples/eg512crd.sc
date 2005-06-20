!*  Schedule for VLBA_SC   *!
!*  Experiment eg512    *!
!* Schedule Version:       2.00 *!
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
!* Notes: *!
!*  *!
!*  *!
!*  *!
!*  Start at 12h00m00s     Tue, 1997 Mar 04  Day of year   63   *!
program=eg512   
autoallocate=off
autoreverse=off

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!
!* ========= New tape starts at: 12h00m00s ========= *!

!* --- Scan from 12h00m00s to 12h22m00s   Tue, 1997 Mar 04 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
maxcaltime= 120
fe=(1,6cm),(3,6cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,15.4),( 2, 4.1),( 3,15.4)
logging=STANDARD
nchan= 8
format=VLBA1:4
barrel=roll_auto
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)
ifchan=(1,A),(2,C),(3,A),(4,C),(5,A),(6,C),(7,A),(8,C)
sideband=(1,U),(2,U),(3,U),(4,U),(5,U),(6,U),(7,U),(8,U)
bits=(1,2),(2,2),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2)
period=(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1)
level=(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,871.49),( 2,871.49),( 3,879.49),( 4,879.49),( 5,887.49),( 6,887.49)
bbsynth=( 7,895.49),( 8,895.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S5),(4,S7),(5,S1),(6,S3),(7,S5),(8,S7)
pcalxbit2=(1,S2),(2,S4),(3,S6),(4,S8),(5,S2),(6,S4),(7,S6),(8,S8)
pcalxfreq1=(1,510),(2,510),(3,510),(4,510),(5,6510),(6,6510),(7,6510),(8,6510)
pcalxfreq2=(1,510),(2,510),(3,510),(4,510),(5,6510),(6,6510),(7,6510),(8,6510)
samplerate=16M
track=(1,2),(2,18),(3,3),(4,19),(5,66),(6,82),(7,67),(8,83)
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
tape=( 2,STOP) write=( 2,off) head=( 2,-319) 
  date = 1997Mar04
stop=12h00m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-319) 
stop=12h22m00s   !NEXT!

!* --- Scan from 12h22m22s to 12h44m22s   Tue, 1997 Mar 04 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
tape=( 2,STOP) write=( 2,off) head=( 2,-319) 
stop=12h22m22s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-319) 
stop=12h44m22s   !NEXT!

!* --- Scan from 12h44m45s to 13h06m45s   Tue, 1997 Mar 04 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
tape=( 2,STOP) write=( 2,off) head=( 2,  31) 
stop=12h44m45s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=13h06m45s   !NEXT!

!* --- Scan from 13h07m07s to 13h29m07s   Tue, 1997 Mar 04 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
tape=( 2,STOP) write=( 2,off) head=( 2,  31) 
stop=13h07m07s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=13h29m07s   !NEXT!

!* --- Scan from 13h29m29s to 13h51m29s   Tue, 1997 Mar 04 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
tape=( 2,STOP) write=( 2,off) head=( 2,-271) 
stop=13h29m29s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=13h51m29s   !NEXT!

!* --- Scan from 13h51m51s to 14h13m51s   Tue, 1997 Mar 04 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
tape=( 2,STOP) write=( 2,off) head=( 2,-271) 
stop=13h51m51s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=14h13m51s   !NEXT!

!* --- Scan from 14h14m14s to 14h36m14s   Tue, 1997 Mar 04 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
tape=( 2,STOP) write=( 2,off) head=( 2,  79) 
stop=14h14m14s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=14h36m14s   !NEXT!

!* --- Scan from 14h36m36s to 14h58m36s   Tue, 1997 Mar 04 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
tape=( 2,STOP) write=( 2,off) head=( 2,  79) 
stop=14h36m36s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=14h58m36s   !NEXT!

!* --- Scan from 15h00m57s to 15h22m57s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-223) 
tape=( 2,STOP) write=( 2,off) head=( 2,-223) 
stop=15h00m57s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=15h22m57s   !NEXT!

!* --- Scan from 15h23m19s to 15h45m19s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-223) 
tape=( 2,STOP) write=( 2,off) head=( 2,-223) 
stop=15h23m19s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=15h45m19s   !NEXT!

!* --- Scan from 15h45m41s to 16h07m41s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 127) 
tape=( 2,STOP) write=( 2,off) head=( 2, 127) 
stop=15h45m41s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=16h07m41s   !NEXT!

!* --- Scan from 16h08m03s to 16h30m03s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 127) 
tape=( 2,STOP) write=( 2,off) head=( 2, 127) 
stop=16h08m03s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=16h30m03s   !NEXT!

!* --- Scan from 16h30m25s to 16h52m25s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
tape=( 2,STOP) write=( 2,off) head=( 2,-175) 
stop=16h30m25s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-175) 
stop=16h52m25s   !NEXT!

!* --- Scan from 16h52m47s to 17h14m47s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
tape=( 2,STOP) write=( 2,off) head=( 2,-175) 
stop=16h52m47s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-175) 
stop=17h14m47s   !NEXT!

!* --- Scan from 17h18m47s to 17h40m47s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 175) 
tape=( 2,STOP) write=( 2,off) head=( 2, 175) 
stop=17h18m47s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=17h40m47s   !NEXT!

!* --- Scan from 17h44m47s to 18h06m47s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 175) 
tape=( 2,STOP) write=( 2,off) head=( 2, 175) 
stop=17h44m47s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=18h06m47s   !NEXT!

!* --- Scan from 18h10m47s to 18h32m47s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-127) 
tape=( 2,STOP) write=( 2,off) head=( 2,-127) 
stop=18h10m47s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=18h32m47s   !NEXT!

!* --- Scan from 18h36m47s to 18h58m47s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-127) 
tape=( 2,STOP) write=( 2,off) head=( 2,-127) 
stop=18h36m47s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=18h58m47s   !NEXT!

!* --- Scan from 18h59m10s to 19h21m10s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 223) 
tape=( 2,STOP) write=( 2,off) head=( 2, 223) 
stop=18h59m10s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=19h21m10s   !NEXT!

!* --- Scan from 19h21m32s to 19h43m32s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 223) 
tape=( 2,STOP) write=( 2,off) head=( 2, 223) 
stop=19h21m32s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=19h43m32s   !NEXT!

!* --- Scan from 19h43m54s to 20h05m54s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, -79) 
tape=( 2,STOP) write=( 2,off) head=( 2, -79) 
stop=19h43m54s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=20h05m54s   !NEXT!

!* --- Scan from 20h06m16s to 20h28m16s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, -79) 
tape=( 2,STOP) write=( 2,off) head=( 2, -79) 
stop=20h06m16s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=20h28m16s   !NEXT!

!* --- Scan from 20h28m39s to 20h50m39s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 271) 
tape=( 2,STOP) write=( 2,off) head=( 2, 271) 
stop=20h28m39s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=20h50m39s   !NEXT!

!* --- Scan from 20h51m01s to 21h13m01s   Tue, 1997 Mar 04 --- *!
sname='3C84'  ra=03h19m48.160119s  dec= 41d30'42.10387"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 271) 
tape=( 2,STOP) write=( 2,off) head=( 2, 271) 
stop=20h51m01s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=21h13m01s   !NEXT!

!* --- Scan from 21h15m39s to 21h37m39s   Tue, 1997 Mar 04 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, -31) 
tape=( 2,STOP) write=( 2,off) head=( 2, -31) 
stop=21h15m39s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -31) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -31) 
stop=21h37m39s   !NEXT!

!* --- Scan from 21h38m00s to 22h00m00s   Tue, 1997 Mar 04 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, -31) 
tape=( 2,STOP) write=( 2,off) head=( 2, -31) 
stop=21h38m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -31) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -31) 
stop=22h00m00s   !NEXT!

!* --- Scan from 22h00m21s to 22h22m21s   Tue, 1997 Mar 04 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 319) 
tape=( 2,STOP) write=( 2,off) head=( 2, 319) 
stop=22h00m21s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 319) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 319) 
stop=22h22m21s   !NEXT!

!* --- Scan from 22h22m42s to 22h44m42s   Tue, 1997 Mar 04 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 319) 
tape=( 2,STOP) write=( 2,off) head=( 2, 319) 
stop=22h22m42s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 319) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 319) 
stop=22h44m42s   !NEXT!

!* ============= Change Tape ============ *!
 !* ======== POSTPASSING DRIVE 1 ========= *! 
 !* ======== POSTPASSING DRIVE 2 ========= *! 
tape=(1,STOP)    write=(1,off) 
tape=(2,STOP)    write=(2,off) 
dur=0  stop=22h44m44s  !NEXT! 
tape=(1,POSTPASS)
dur=0  stop=22h44m46s  !NEXT! 
tape=(2,POSTPASS)
!* ========= New tape starts at: 23h34m42s ========= *!

!* --- Scan from 23h34m42s to 23h56m42s   Tue, 1997 Mar 04 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
bbsynth=( 1,858.49),( 2,858.49),( 3,874.49),( 4,874.49),( 5,890.49),( 6,890.49)
bbsynth=( 7,906.49),( 8,906.49)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M),(5,16M),(6,16M),(7,16M),(8,16M)
pcalxfreq1=(5,13510),(6,13510),(7,13510),(8,13510)
pcalxfreq2=(5,13510),(6,13510),(7,13510),(8,13510)
samplerate=32M
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
tape=( 2,STOP) write=( 2,off) head=( 2,-319) 
stop=23h34m42s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-319) 
stop=23h56m42s   !NEXT!

!* --- Scan from 00h00m42s to 00h22m42s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
tape=( 2,STOP) write=( 2,off) head=( 2,  31) 
  date = 1997Mar05
stop=00h00m42s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=00h22m42s   !NEXT!

!* --- Scan from 00h26m42s to 00h48m42s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
tape=( 2,STOP) write=( 2,off) head=( 2,-271) 
stop=00h26m42s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=00h48m42s   !NEXT!

!* --- Scan from 00h52m42s to 01h14m42s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
tape=( 2,STOP) write=( 2,off) head=( 2,  79) 
stop=00h52m42s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=01h14m42s   !NEXT!

!* --- Scan from 01h18m42s to 01h40m42s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-223) 
tape=( 2,STOP) write=( 2,off) head=( 2,-223) 
stop=01h18m42s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=01h40m42s   !NEXT!

!* --- Scan from 01h44m42s to 02h06m42s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 127) 
tape=( 2,STOP) write=( 2,off) head=( 2, 127) 
stop=01h44m42s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=02h06m42s   !NEXT!

!* --- Scan from 02h10m42s to 02h32m42s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
tape=( 2,STOP) write=( 2,off) head=( 2,-175) 
stop=02h10m42s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-175) 
stop=02h32m42s   !NEXT!

!* --- Scan from 02h33m03s to 02h55m03s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 175) 
tape=( 2,STOP) write=( 2,off) head=( 2, 175) 
stop=02h33m03s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=02h55m03s   !NEXT!

!* --- Scan from 02h55m23s to 03h17m23s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-127) 
tape=( 2,STOP) write=( 2,off) head=( 2,-127) 
stop=02h55m23s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=03h17m23s   !NEXT!

!* --- Scan from 03h17m44s to 03h39m44s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 223) 
tape=( 2,STOP) write=( 2,off) head=( 2, 223) 
stop=03h17m44s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=03h39m44s   !NEXT!

!* --- Scan from 03h40m05s to 04h02m05s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, -79) 
tape=( 2,STOP) write=( 2,off) head=( 2, -79) 
stop=03h40m05s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=04h02m05s   !NEXT!

!* --- Scan from 04h02m26s to 04h24m26s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 271) 
tape=( 2,STOP) write=( 2,off) head=( 2, 271) 
stop=04h02m26s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=04h24m26s   !NEXT!

!* --- Scan from 04h24m47s to 04h46m47s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, -31) 
tape=( 2,STOP) write=( 2,off) head=( 2, -31) 
stop=04h24m47s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -31) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -31) 
stop=04h46m47s   !NEXT!

!* --- Scan from 04h47m07s to 05h09m07s   Wed, 1997 Mar 05 --- *!
sname='J0508+84'  ra=05h08m42.363513s  dec= 84d32'04.54399"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 319) 
tape=( 2,STOP) write=( 2,off) head=( 2, 319) 
stop=04h47m07s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 319) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 319) 
stop=05h09m07s   !NEXT!

!* =====  UNLOADING DRIVE 1 ==== *!
!* =====  UNLOADING DRIVE 2 ==== *!
tape=(1,STOP)    write=(1,off)   dur=0s 
tape=(2,STOP)    write=(2,off)   dur=0s 
stop=05h09m12s  !NEXT! 
tape=(1,UNLOAD)  stop=05h09m14s
  !NEXT! 
tape=(2,UNLOAD)  stop=05h09m16s
  !NEXT! 
stop=05h09m18s   !NEXT!
     !QUIT! 
