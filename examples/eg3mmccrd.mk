!*  Schedule for VLBA_MK   *!
!*  Experiment eg3mmc   *!
!* Schedule Version:       1.00 *!
!* Processed by SCHED version:   6.0  Release: March 2005 *!
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
!* Notes:    Will use reference pointing. *!
!*  *!
!*  *!
!*  *!
!*  Start at 02h30m00s     Wed, 2000 Jul 05  Day of year  187   *!
program=eg3mmc  
autoallocate=on
autoreverse=on

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 02h30m00s to 02h40m00s   Wed, 2000 Jul 05 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=999  calib='V'
maxcaltime= 120
fe=(1,7mm),(3,7mm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
logging=STANDARD
nchan= 4
format=NONE
barrel=roll_off
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,A),(2,C),(3,A),(4,C)
sideband=(1,U),(2,U),(3,U),(4,U)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,715.99),( 2,715.99),( 3,815.99),( 4,815.99)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
pcal=OFF
pcalxbit1=(1,S1),(2,S2),(3,S3),(4,S4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(1,M1),(2,M2),(3,M3),(4,M4),(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxfreq1=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
tape=(1,STOP) write=(1,off)
  date = 2000Jul05
stop=02h30m00s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=02h40m00s   !NEXT!

!* --- Scan from 02h40m00s to 02h50m00s   Wed, 2000 Jul 05 --- *!
sname='3C273'  ra=12h29m06.699731s  dec= 02d03'08.59814"  qual=  0  calib='V'
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
samplerate=16M
tape=(1,+RUN)  write=(1,on)
stop=02h50m00s   !NEXT!

!* --- Scan from 04h03m13s to 04h04m13s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-V2108OPH'  ra=17h14m19.370000s  dec= 08d56'02.50000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,720.17),( 2,720.17),( 3,820.16),( 4,820.16)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=04h03m13s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h04m13s   !NEXT!

!* --- Scan from 04h04m13s to 04h05m13s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-V2108OPH'  ra=17h14m19.370000s  dec= 08d56'02.50000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h05m13s   !NEXT!

!* --- Scan from 04h06m00s to 04h16m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=04h06m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=04h16m00s   !NEXT!

!* --- Scan from 04h16m00s to 04h26m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=04h26m00s   !NEXT!

!* --- Scan from 04h27m44s to 04h28m44s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-AHSCO'  ra=17h11m16.980000s  dec=-32d19'31.20000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.63),( 2,721.63),( 3,821.63),( 4,821.63)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=04h27m44s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h28m44s   !NEXT!

!* --- Scan from 04h28m44s to 04h29m44s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-AHSCO'  ra=17h11m16.980000s  dec=-32d19'31.20000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h29m44s   !NEXT!

!* --- Scan from 04h30m00s to 04h40m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=04h30m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=04h40m00s   !NEXT!

!* --- Scan from 04h40m00s to 04h50m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=04h50m00s   !NEXT!

!* --- Scan from 04h57m55s to 04h58m55s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-AHSCO'  ra=17h11m16.980000s  dec=-32d19'31.20000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.63),( 2,721.63),( 3,821.63),( 4,821.63)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=04h57m55s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h58m55s   !NEXT!

!* --- Scan from 04h58m55s to 04h59m55s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-AHSCO'  ra=17h11m16.980000s  dec=-32d19'31.20000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h59m55s   !NEXT!

!* --- Scan from 05h00m00s to 05h02m00s   Wed, 2000 Jul 05 --- *!
sname='AHSCO'  ra=17h11m16.980000s  dec=-32d19'31.20000"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,740.60),( 2,740.60),( 3,709.82),( 4,709.82),( 5,681.05),( 6,681.05)
bbsynth=( 7,652.28),( 8,652.28)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=05h00m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h02m00s   !NEXT!

!* --- Scan from 05h03m52s to 05h04m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,722.06),( 2,722.06),( 3,822.07),( 4,822.07)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=05h03m52s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h04m52s   !NEXT!

!* --- Scan from 05h04m52s to 05h05m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h05m52s   !NEXT!

!* --- Scan from 05h06m00s to 05h16m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=05h06m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h16m00s   !NEXT!

!* --- Scan from 05h16m00s to 05h26m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=05h26m00s   !NEXT!

!* --- Scan from 05h27m52s to 05h28m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,722.06),( 2,722.06),( 3,822.07),( 4,822.07)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=05h27m52s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h28m52s   !NEXT!

!* --- Scan from 05h28m52s to 05h29m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h29m52s   !NEXT!

!* --- Scan from 05h30m00s to 05h40m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=05h30m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=05h40m00s   !NEXT!

!* --- Scan from 05h40m00s to 05h50m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=05h50m00s   !NEXT!

!* --- Scan from 05h51m52s to 05h52m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,722.06),( 2,722.06),( 3,822.07),( 4,822.07)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=05h51m52s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h52m52s   !NEXT!

!* --- Scan from 05h52m52s to 05h53m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h53m52s   !NEXT!

!* --- Scan from 05h54m00s to 06h04m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=05h54m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h04m00s   !NEXT!

!* --- Scan from 06h04m00s to 06h14m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=06h14m00s   !NEXT!

!* --- Scan from 06h15m52s to 06h16m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,722.06),( 2,722.06),( 3,822.07),( 4,822.07)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=06h15m52s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=06h16m52s   !NEXT!

!* --- Scan from 06h16m52s to 06h17m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=06h17m52s   !NEXT!

!* --- Scan from 06h18m00s to 06h28m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=06h18m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h28m00s   !NEXT!

!* --- Scan from 06h28m00s to 06h38m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=06h38m00s   !NEXT!

!* --- Scan from 06h39m52s to 06h40m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,722.06),( 2,722.06),( 3,822.07),( 4,822.07)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=06h39m52s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=06h40m52s   !NEXT!

!* --- Scan from 06h40m52s to 06h41m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=06h41m52s   !NEXT!

!* --- Scan from 06h42m00s to 06h52m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=06h42m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=06h52m00s   !NEXT!

!* --- Scan from 06h52m00s to 07h02m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=07h02m00s   !NEXT!

!* --- Scan from 07h05m23s to 07h06m23s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-RUHER'  ra=16h08m08.000000s  dec= 25d12'00.00000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,723.26),( 2,723.26),( 3,823.26),( 4,823.26)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=07h05m23s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h06m23s   !NEXT!

!* --- Scan from 07h06m23s to 07h07m23s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-RUHER'  ra=16h08m08.000000s  dec= 25d12'00.00000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h07m23s   !NEXT!

!* --- Scan from 07h08m00s to 07h18m00s   Wed, 2000 Jul 05 --- *!
sname='3C345'  ra=16h42m58.809968s  dec= 39d48'36.99399"  qual=999  calib='V'
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=07h08m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h18m00s   !NEXT!

!* --- Scan from 07h19m52s to 07h20m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,722.06),( 2,722.06),( 3,822.07),( 4,822.07)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=07h19m52s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h20m52s   !NEXT!

!* --- Scan from 07h20m52s to 07h21m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h21m52s   !NEXT!

!* --- Scan from 07h22m00s to 07h32m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=07h22m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h32m00s   !NEXT!

!* --- Scan from 07h32m00s to 07h42m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=07h42m00s   !NEXT!

!* --- Scan from 07h43m52s to 07h44m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,722.06),( 2,722.06),( 3,822.07),( 4,822.07)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=07h43m52s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h44m52s   !NEXT!

!* --- Scan from 07h44m52s to 07h45m52s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h45m52s   !NEXT!

!* --- Scan from 07h46m00s to 07h56m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=07h46m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=07h56m00s   !NEXT!

!* --- Scan from 07h56m00s to 08h06m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=08h06m00s   !NEXT!

!* --- Scan from 08h07m51s to 08h08m51s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,722.06),( 2,722.06),( 3,822.07),( 4,822.07)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=08h07m51s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h08m51s   !NEXT!

!* --- Scan from 08h08m51s to 08h09m51s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h09m51s   !NEXT!

!* --- Scan from 08h10m00s to 08h20m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=08h10m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h20m00s   !NEXT!

!* --- Scan from 08h20m00s to 08h30m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=08h30m00s   !NEXT!

!* --- Scan from 08h31m51s to 08h32m51s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,722.06),( 2,722.06),( 3,822.07),( 4,822.07)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=08h31m51s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h32m51s   !NEXT!

!* --- Scan from 08h32m51s to 08h33m51s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h33m51s   !NEXT!

!* --- Scan from 08h34m00s to 08h44m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=08h34m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=08h44m00s   !NEXT!

!* --- Scan from 08h44m00s to 08h54m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=08h54m00s   !NEXT!

!* --- Scan from 08h55m50s to 08h56m50s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,722.06),( 2,722.06),( 3,822.07),( 4,822.07)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=08h55m50s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h56m50s   !NEXT!

!* --- Scan from 08h56m50s to 08h57m50s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-OH2.6-0.4'  ra=17h53m18.618404s  dec=-26d56'36.09485"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h57m50s   !NEXT!

!* --- Scan from 08h58m00s to 09h08m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=999  calib=' '
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=08h58m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h08m00s   !NEXT!

!* --- Scan from 09h08m00s to 09h18m00s   Wed, 2000 Jul 05 --- *!
sname='SGRA'  ra=17h45m40.039900s  dec=-29d00'28.13700"  qual=  0  calib=' '
tape=(1,+RUN)  write=(1,on)
stop=09h18m00s   !NEXT!

!* --- Scan from 09h19m45s to 09h20m45s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-RPEG'  ra=23h06m39.157000s  dec= 10d32'36.07000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,722.80),( 2,722.80),( 3,822.81),( 4,822.81)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=09h19m45s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=09h20m45s   !NEXT!

!* --- Scan from 09h20m45s to 09h21m45s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-RPEG'  ra=23h06m39.157000s  dec= 10d32'36.07000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=09h21m45s   !NEXT!

!* --- Scan from 09h22m00s to 09h32m00s   Wed, 2000 Jul 05 --- *!
sname='3C454.3'  ra=22h53m57.747942s  dec= 16d08'53.56086"  qual=999  calib='V'
fe=(2,3mm),(4,3mm)
synth=( 1,12.9),( 2,15.4),( 3,12.1)
nchan= 8
format=VLBA1:2
barrel=roll_auto
baseband=(5,5),(6,6),(7,7),(8,8)
ifchan=(1,B),(2,D),(3,B),(4,D),(5,B),(6,D),(7,B),(8,D)
sideband=(5,U),(6,U),(7,U),(8,U)
bits=(5,2),(6,2),(7,2),(8,2)
bbsynth=( 1,733.49),( 2,733.49),( 3,741.49),( 4,741.49),( 5,749.49),( 6,749.49)
bbsynth=( 7,757.49),( 8,757.49)
bbfilter=(1,8M),(2,8M),(3,8M),(4,8M),(5,8M),(6,8M),(7,8M),(8,8M)
pcalxbit1=(5,S5),(6,S6),(7,S7),(8,S8)
pcalxbit2=(5,M5),(6,M6),(7,M7),(8,M8)
tape=(1,STOP) write=(1,off)
stop=09h22m00s   !NEXT!        
qual=  0
tape=(1,+RUN)  write=(1,on)
stop=09h32m00s   !NEXT!

tape=(1,STOP)    write=(1,off)   dur=0s 
stop=09h32m05s   !NEXT!
     !QUIT! 
