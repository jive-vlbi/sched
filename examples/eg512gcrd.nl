!*  Schedule for VLBA_NL   *!
!*  Experiment eg512g   *!
!* Schedule Version:       2.00 *!
!* Processed by SCHED version:   6.0  Release: March 2005 *!
!* PI:       Huib van Langevelde *!
!* Address:  JIVE, Radiosterrenwacht Dwingeloo *!
!*           Postbus 2, 7990 AA Dwingeloo *!
!*           the Netherlands *!
!*  *!
!* Phone:    +31 521 596 515 *!
!* EMAIL:    langevelde@jive.nl *!
!* Fax:      +31 521 597 332 *!
!* Phone during observation: +31 528 221 273 *!
!* Observing mode: VLBA/MKIV *!
!* Notes: *!
!*  *!
!*  *!
!*  *!
!*  Start at 16h00m00s     Tue, 1997 Mar 04  Day of year   63   *!
program=eg512g  
autoallocate=off
autoreverse=off

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!
!* ========= New tape starts at: 16h00m00s ========= *!

!* --- Scan from 16h00m00s to 16h22m00s   Tue, 1997 Mar 04 --- *!
sname='3C48'  ra=01h37m41.299494s  dec= 33d09'35.13382"  qual=999  calib='V'
maxcaltime= 120
fe=(2,1cm),(4,1cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1, 9.1),( 2,15.4),( 3,12.4)
logging=STANDARD
nchan= 8
format=VLBA1:4
barrel=roll_16
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(1,U),(2,U),(3,U),(4,U),(5,U),(6,U),(7,U),(8,U)
bits=(1,2),(2,2),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2)
period=(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1)
level=(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,703.49),( 2,703.49),( 3,719.49),( 4,719.49),( 5,735.49),( 6,735.49)
bbsynth=( 7,751.49),( 8,751.49)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M),(5,16M),(6,16M),(7,16M),(8,16M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S5),(4,S7),(5,S1),(6,S3),(7,S5),(8,S7)
pcalxbit2=(1,S2),(2,S4),(3,S6),(4,S8),(5,S2),(6,S4),(7,S6),(8,S8)
pcalxfreq1=(1,510),(2,510),(3,510),(4,510),(5,13510),(6,13510),(7,13510)
pcalxfreq1=(8,13510)
pcalxfreq2=(1,510),(2,510),(3,510),(4,510),(5,13510),(6,13510),(7,13510)
pcalxfreq2=(8,13510)
samplerate=32M
track=(1,2),(2,18),(3,3),(4,19),(5,66),(6,82),(7,67),(8,83)
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
tape=( 2,STOP) write=( 2,off) head=( 2,-319) 
  date = 1997Mar04
stop=16h00m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-319) 
stop=16h22m00s   !NEXT!

!* --- Scan from 16h22m40s to 16h26m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
tape=( 2,STOP) write=( 2,off) head=( 2,  31) 
stop=16h22m40s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=16h26m40s   !NEXT!

!* --- Scan from 16h26m40s to 16h29m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=16h29m40s   !NEXT!

!* --- Scan from 16h29m40s to 16h32m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=16h32m40s   !NEXT!

!* --- Scan from 16h32m40s to 16h35m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=16h35m40s   !NEXT!

!* --- Scan from 16h35m40s to 16h38m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=16h38m40s   !NEXT!

!* --- Scan from 16h38m40s to 16h41m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=16h41m40s   !NEXT!

!* --- Scan from 16h41m40s to 16h44m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=16h44m40s   !NEXT!

!* --- Scan from 16h45m20s to 16h49m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
tape=( 2,STOP) write=( 2,off) head=( 2,-271) 
stop=16h45m20s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=16h49m20s   !NEXT!

!* --- Scan from 16h49m20s to 16h52m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=16h52m20s   !NEXT!

!* --- Scan from 16h52m20s to 16h55m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=16h55m20s   !NEXT!

!* --- Scan from 16h55m20s to 16h58m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=16h58m20s   !NEXT!

!* --- Scan from 16h58m20s to 17h01m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=17h01m20s   !NEXT!

!* --- Scan from 17h01m20s to 17h04m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=17h04m20s   !NEXT!

!* --- Scan from 17h04m20s to 17h07m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=17h07m20s   !NEXT!

!* --- Scan from 17h08m00s to 17h12m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
tape=( 2,STOP) write=( 2,off) head=( 2,  79) 
stop=17h08m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=17h12m00s   !NEXT!

!* --- Scan from 17h12m00s to 17h15m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=17h15m00s   !NEXT!

!* --- Scan from 17h15m00s to 17h18m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=17h18m00s   !NEXT!

!* --- Scan from 17h18m00s to 17h21m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=17h21m00s   !NEXT!

!* --- Scan from 17h21m00s to 17h24m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=17h24m00s   !NEXT!

!* --- Scan from 17h24m00s to 17h27m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=17h27m00s   !NEXT!

!* --- Scan from 17h27m00s to 17h30m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=17h30m00s   !NEXT!

!* --- Scan from 17h30m40s to 17h34m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-223) 
tape=( 2,STOP) write=( 2,off) head=( 2,-223) 
stop=17h30m40s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=17h34m40s   !NEXT!

!* --- Scan from 17h34m40s to 17h37m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=17h37m40s   !NEXT!

!* --- Scan from 17h37m40s to 17h40m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=17h40m40s   !NEXT!

!* --- Scan from 17h40m40s to 17h43m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=17h43m40s   !NEXT!

!* --- Scan from 17h43m40s to 17h46m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=17h46m40s   !NEXT!

!* --- Scan from 17h46m40s to 17h49m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=17h49m40s   !NEXT!

!* --- Scan from 17h49m40s to 17h52m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=17h52m40s   !NEXT!

!* --- Scan from 17h53m20s to 17h57m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 127) 
tape=( 2,STOP) write=( 2,off) head=( 2, 127) 
stop=17h53m20s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=17h57m20s   !NEXT!

!* --- Scan from 17h57m20s to 18h00m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=18h00m20s   !NEXT!

!* --- Scan from 18h00m20s to 18h03m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=18h03m20s   !NEXT!

!* --- Scan from 18h03m20s to 18h06m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=18h06m20s   !NEXT!

!* --- Scan from 18h06m20s to 18h09m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=18h09m20s   !NEXT!

!* --- Scan from 18h09m20s to 18h12m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=18h12m20s   !NEXT!

!* --- Scan from 18h12m20s to 18h15m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=18h15m20s   !NEXT!

!* --- Scan from 18h30m20s to 18h41m20s   Tue, 1997 Mar 04 --- *!
sname='3C48'  ra=01h37m41.299494s  dec= 33d09'35.13382"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
tape=( 2,STOP) write=( 2,off) head=( 2,-175) 
stop=18h30m20s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-175) 
stop=18h41m20s   !NEXT!

!* --- Scan from 18h45m20s to 18h56m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
tape=( 2,STOP) write=( 2,off) head=( 2,-175) 
stop=18h45m20s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-175) 
stop=18h56m20s   !NEXT!

!* --- Scan from 18h57m00s to 19h01m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 175) 
tape=( 2,STOP) write=( 2,off) head=( 2, 175) 
stop=18h57m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=19h01m00s   !NEXT!

!* --- Scan from 19h01m00s to 19h04m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=19h04m00s   !NEXT!

!* --- Scan from 19h04m00s to 19h07m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=19h07m00s   !NEXT!

!* --- Scan from 19h07m00s to 19h10m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=19h10m00s   !NEXT!

!* --- Scan from 19h10m00s to 19h13m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=19h13m00s   !NEXT!

!* --- Scan from 19h13m00s to 19h16m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=19h16m00s   !NEXT!

!* --- Scan from 19h16m00s to 19h19m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=19h19m00s   !NEXT!

!* --- Scan from 19h19m40s to 19h23m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-127) 
tape=( 2,STOP) write=( 2,off) head=( 2,-127) 
stop=19h19m40s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=19h23m40s   !NEXT!

!* --- Scan from 19h23m40s to 19h26m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=19h26m40s   !NEXT!

!* --- Scan from 19h26m40s to 19h29m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=19h29m40s   !NEXT!

!* --- Scan from 19h29m40s to 19h32m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=19h32m40s   !NEXT!

!* --- Scan from 19h32m40s to 19h35m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=19h35m40s   !NEXT!

!* --- Scan from 19h35m40s to 19h38m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=19h38m40s   !NEXT!

!* --- Scan from 19h38m40s to 19h41m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=19h41m40s   !NEXT!

!* --- Scan from 19h42m20s to 19h46m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 223) 
tape=( 2,STOP) write=( 2,off) head=( 2, 223) 
stop=19h42m20s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=19h46m20s   !NEXT!

!* --- Scan from 19h46m20s to 19h49m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=19h49m20s   !NEXT!

!* --- Scan from 19h49m20s to 19h52m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=19h52m20s   !NEXT!

!* --- Scan from 19h52m20s to 19h55m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=19h55m20s   !NEXT!

!* --- Scan from 19h55m20s to 19h58m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=19h58m20s   !NEXT!

!* --- Scan from 19h58m20s to 20h01m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=20h01m20s   !NEXT!

!* --- Scan from 20h01m20s to 20h04m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=20h04m20s   !NEXT!

!* --- Scan from 20h05m00s to 20h09m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, -79) 
tape=( 2,STOP) write=( 2,off) head=( 2, -79) 
stop=20h05m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=20h09m00s   !NEXT!

!* --- Scan from 20h09m00s to 20h12m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=20h12m00s   !NEXT!

!* --- Scan from 20h12m00s to 20h15m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=20h15m00s   !NEXT!

!* --- Scan from 20h15m00s to 20h18m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=20h18m00s   !NEXT!

!* --- Scan from 20h18m00s to 20h21m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=20h21m00s   !NEXT!

!* --- Scan from 20h21m00s to 20h24m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=20h24m00s   !NEXT!

!* --- Scan from 20h24m00s to 20h27m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=20h27m00s   !NEXT!

!* --- Scan from 20h27m40s to 20h31m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 271) 
tape=( 2,STOP) write=( 2,off) head=( 2, 271) 
stop=20h27m40s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=20h31m40s   !NEXT!

!* --- Scan from 20h31m40s to 20h34m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=20h34m40s   !NEXT!

!* --- Scan from 20h34m40s to 20h37m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=20h37m40s   !NEXT!

!* --- Scan from 20h37m40s to 20h40m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=20h40m40s   !NEXT!

!* --- Scan from 20h40m40s to 20h43m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=20h43m40s   !NEXT!

!* --- Scan from 20h43m40s to 20h46m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=20h46m40s   !NEXT!

!* --- Scan from 20h46m40s to 20h49m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=20h49m40s   !NEXT!

!* ============= Change Tape ============ *!
!*   There were no scheduled tape stops in the last pass.  *!
!*   If it was not interrupted, a POSTPASS is not needed.  *!
 !* ======== UNLOADING DRIVE 1 ========= *! 
 !* ======== UNLOADING DRIVE 2 ========= *! 
tape=(1,STOP)    write=(1,off) 
tape=(2,STOP)    write=(2,off) 
dur=0  stop=20h49m42s  !NEXT! 
tape=(1,UNLOAD)
dur=0  stop=20h49m44s  !NEXT! 
tape=(2,UNLOAD)
!* ========= New tape starts at: 21h04m40s ========= *!

!* --- Scan from 21h04m40s to 21h26m40s   Tue, 1997 Mar 04 --- *!
sname='3C48'  ra=01h37m41.299494s  dec= 33d09'35.13382"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-319) 
tape=( 2,STOP) write=( 2,off) head=( 2,-319) 
stop=21h04m40s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-319) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-319) 
stop=21h26m40s   !NEXT!

!* --- Scan from 21h27m20s to 21h31m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  31) 
tape=( 2,STOP) write=( 2,off) head=( 2,  31) 
stop=21h27m20s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=21h31m20s   !NEXT!

!* --- Scan from 21h31m20s to 21h34m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=21h34m20s   !NEXT!

!* --- Scan from 21h34m20s to 21h37m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=21h37m20s   !NEXT!

!* --- Scan from 21h37m20s to 21h40m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=21h40m20s   !NEXT!

!* --- Scan from 21h40m20s to 21h43m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=21h43m20s   !NEXT!

!* --- Scan from 21h43m20s to 21h46m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=21h46m20s   !NEXT!

!* --- Scan from 21h46m20s to 21h49m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  31) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  31) 
stop=21h49m20s   !NEXT!

!* --- Scan from 21h50m00s to 21h54m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-271) 
tape=( 2,STOP) write=( 2,off) head=( 2,-271) 
stop=21h50m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=21h54m00s   !NEXT!

!* --- Scan from 21h54m00s to 21h57m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=21h57m00s   !NEXT!

!* --- Scan from 21h57m00s to 22h00m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=22h00m00s   !NEXT!

!* --- Scan from 22h00m00s to 22h03m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=22h03m00s   !NEXT!

!* --- Scan from 22h03m00s to 22h06m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=22h06m00s   !NEXT!

!* --- Scan from 22h06m00s to 22h09m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=22h09m00s   !NEXT!

!* --- Scan from 22h09m00s to 22h12m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-271) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-271) 
stop=22h12m00s   !NEXT!

!* --- Scan from 22h12m40s to 22h16m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,  79) 
tape=( 2,STOP) write=( 2,off) head=( 2,  79) 
stop=22h12m40s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=22h16m40s   !NEXT!

!* --- Scan from 22h16m40s to 22h19m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=22h19m40s   !NEXT!

!* --- Scan from 22h19m40s to 22h22m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=22h22m40s   !NEXT!

!* --- Scan from 22h22m40s to 22h25m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=22h25m40s   !NEXT!

!* --- Scan from 22h25m40s to 22h28m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=22h28m40s   !NEXT!

!* --- Scan from 22h28m40s to 22h31m40s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=22h31m40s   !NEXT!

!* --- Scan from 22h31m40s to 22h34m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1,  79) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2,  79) 
stop=22h34m40s   !NEXT!

!* --- Scan from 22h35m20s to 22h39m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-223) 
tape=( 2,STOP) write=( 2,off) head=( 2,-223) 
stop=22h35m20s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=22h39m20s   !NEXT!

!* --- Scan from 22h39m20s to 22h42m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=22h42m20s   !NEXT!

!* --- Scan from 22h42m20s to 22h45m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=22h45m20s   !NEXT!

!* --- Scan from 22h45m20s to 22h48m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=22h48m20s   !NEXT!

!* --- Scan from 22h48m20s to 22h51m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=22h51m20s   !NEXT!

!* --- Scan from 22h51m20s to 22h54m20s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=22h54m20s   !NEXT!

!* --- Scan from 22h54m20s to 22h57m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-223) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-223) 
stop=22h57m20s   !NEXT!

!* --- Scan from 22h58m00s to 23h02m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 127) 
tape=( 2,STOP) write=( 2,off) head=( 2, 127) 
stop=22h58m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=23h02m00s   !NEXT!

!* --- Scan from 23h02m00s to 23h05m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=23h05m00s   !NEXT!

!* --- Scan from 23h05m00s to 23h08m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=23h08m00s   !NEXT!

!* --- Scan from 23h08m00s to 23h11m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=23h11m00s   !NEXT!

!* --- Scan from 23h11m00s to 23h14m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=23h14m00s   !NEXT!

!* --- Scan from 23h14m00s to 23h17m00s   Tue, 1997 Mar 04 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=23h17m00s   !NEXT!

!* --- Scan from 23h17m00s to 23h20m00s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 127) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 127) 
stop=23h20m00s   !NEXT!

!* --- Scan from 23h35m00s to 23h46m00s   Tue, 1997 Mar 04 --- *!
sname='3C48'  ra=01h37m41.299494s  dec= 33d09'35.13382"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
tape=( 2,STOP) write=( 2,off) head=( 2,-175) 
stop=23h35m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-175) 
stop=23h46m00s   !NEXT!

!* --- Scan from 23h46m40s to 23h57m40s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-175) 
tape=( 2,STOP) write=( 2,off) head=( 2,-175) 
stop=23h46m40s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-175) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-175) 
stop=23h57m40s   !NEXT!

!* --- Scan from 23h58m20s to 00h02m20s   Tue, 1997 Mar 04 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 175) 
tape=( 2,STOP) write=( 2,off) head=( 2, 175) 
stop=23h58m20s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
date=1997Mar05
stop=00h02m20s   !NEXT!

!* --- Scan from 00h02m20s to 00h05m20s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=00h05m20s   !NEXT!

!* --- Scan from 00h05m20s to 00h08m20s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=00h08m20s   !NEXT!

!* --- Scan from 00h08m20s to 00h11m20s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=00h11m20s   !NEXT!

!* --- Scan from 00h11m20s to 00h14m20s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=00h14m20s   !NEXT!

!* --- Scan from 00h14m20s to 00h17m20s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=00h17m20s   !NEXT!

!* --- Scan from 00h17m20s to 00h20m20s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 175) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 175) 
stop=00h20m20s   !NEXT!

!* --- Scan from 00h21m00s to 00h25m00s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1,-127) 
tape=( 2,STOP) write=( 2,off) head=( 2,-127) 
stop=00h21m00s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=00h25m00s   !NEXT!

!* --- Scan from 00h25m00s to 00h28m00s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=00h28m00s   !NEXT!

!* --- Scan from 00h28m00s to 00h31m00s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=00h31m00s   !NEXT!

!* --- Scan from 00h31m00s to 00h34m00s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=00h34m00s   !NEXT!

!* --- Scan from 00h34m00s to 00h37m00s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=00h37m00s   !NEXT!

!* --- Scan from 00h37m00s to 00h40m00s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=00h40m00s   !NEXT!

!* --- Scan from 00h40m00s to 00h43m00s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1,-127) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2,-127) 
stop=00h43m00s   !NEXT!

!* --- Scan from 00h43m40s to 00h47m40s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 223) 
tape=( 2,STOP) write=( 2,off) head=( 2, 223) 
stop=00h43m40s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=00h47m40s   !NEXT!

!* --- Scan from 00h47m40s to 00h50m40s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=00h50m40s   !NEXT!

!* --- Scan from 00h50m40s to 00h53m40s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=00h53m40s   !NEXT!

!* --- Scan from 00h53m40s to 00h56m40s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=00h56m40s   !NEXT!

!* --- Scan from 00h56m40s to 00h59m40s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=00h59m40s   !NEXT!

!* --- Scan from 00h59m40s to 01h02m40s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=01h02m40s   !NEXT!

!* --- Scan from 01h02m40s to 01h05m40s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 223) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 223) 
stop=01h05m40s   !NEXT!

!* --- Scan from 01h06m20s to 01h10m20s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, -79) 
tape=( 2,STOP) write=( 2,off) head=( 2, -79) 
stop=01h06m20s   !NEXT!        
qual=  0
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=01h10m20s   !NEXT!

!* --- Scan from 01h10m20s to 01h13m20s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=01h13m20s   !NEXT!

!* --- Scan from 01h13m20s to 01h16m20s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=01h16m20s   !NEXT!

!* --- Scan from 01h16m20s to 01h19m20s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=01h19m20s   !NEXT!

!* --- Scan from 01h19m20s to 01h22m20s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=01h22m20s   !NEXT!

!* --- Scan from 01h22m20s to 01h25m20s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=01h25m20s   !NEXT!

!* --- Scan from 01h25m20s to 01h28m20s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,+RUN)   write=( 1,on)  head=( 1, -79) 
tape=( 2,+RUN)   write=( 2,on)  head=( 2, -79) 
stop=01h28m20s   !NEXT!

!* --- Scan from 01h29m00s to 01h33m00s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=999  calib='V'
tape=( 1,STOP) write=( 1,off) head=( 1, 271) 
tape=( 2,STOP) write=( 2,off) head=( 2, 271) 
stop=01h29m00s   !NEXT!        
qual=  0
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=01h33m00s   !NEXT!

!* --- Scan from 01h33m00s to 01h36m00s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=01h36m00s   !NEXT!

!* --- Scan from 01h36m00s to 01h39m00s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=01h39m00s   !NEXT!

!* --- Scan from 01h39m00s to 01h42m00s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=01h42m00s   !NEXT!

!* --- Scan from 01h42m00s to 01h45m00s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=01h45m00s   !NEXT!

!* --- Scan from 01h45m00s to 01h48m00s   Wed, 1997 Mar 05 --- *!
sname='M31*'  ra=00h42m44.329000s  dec= 41d16'08.42000"  qual=  0  calib=' '
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=01h48m00s   !NEXT!

!* --- Scan from 01h48m00s to 01h51m00s   Wed, 1997 Mar 05 --- *!
sname='J0045+4555'  ra=00h45m00.032407s  dec= 45d55'15.26379"  qual=  0  calib='V'
tape=( 1,-RUN)   write=( 1,on)  head=( 1, 271) 
tape=( 2,-RUN)   write=( 2,on)  head=( 2, 271) 
stop=01h51m00s   !NEXT!

!* =====  UNLOADING DRIVE 1 ==== *!
!* =====  UNLOADING DRIVE 2 ==== *!
tape=(1,STOP)    write=(1,off)   dur=0s 
tape=(2,STOP)    write=(2,off)   dur=0s 
stop=01h51m05s  !NEXT! 
tape=(1,UNLOAD)  stop=01h51m07s
  !NEXT! 
tape=(2,UNLOAD)  stop=01h51m09s
  !NEXT! 
stop=01h51m11s   !NEXT!
     !QUIT! 
