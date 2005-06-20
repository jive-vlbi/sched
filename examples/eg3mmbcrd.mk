!*  Schedule for VLBA_MK   *!
!*  Experiment eg3mmb   *!
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
program=eg3mmb  
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

!* --- Scan from 04h03m38s to 04h04m38s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='NRAO530'  ra=17h33m02.705785s  dec=-13d04'49.54823"  qual=999  calib='V'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,715.99),( 2,715.99),( 3,815.99),( 4,815.99)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=04h03m38s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h04m38s   !NEXT!

!* --- Scan from 04h04m38s to 04h05m38s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='NRAO530'  ra=17h33m02.705785s  dec=-13d04'49.54823"  qual=  0  calib='V'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h05m38s   !NEXT!

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

!* --- Scan from 04h27m35s to 04h28m35s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='NRAO530'  ra=17h33m02.705785s  dec=-13d04'49.54823"  qual=999  calib='V'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,715.99),( 2,715.99),( 3,815.99),( 4,815.99)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=04h27m35s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h28m35s   !NEXT!

!* --- Scan from 04h28m35s to 04h29m35s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='NRAO530'  ra=17h33m02.705785s  dec=-13d04'49.54823"  qual=  0  calib='V'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h29m35s   !NEXT!

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

!* --- Scan from 04h57m43s to 04h58m43s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=04h57m43s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h58m43s   !NEXT!

!* --- Scan from 04h58m43s to 04h59m43s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=04h59m43s   !NEXT!

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

!* --- Scan from 05h03m48s to 05h04m48s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=05h03m48s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h04m48s   !NEXT!

!* --- Scan from 05h04m48s to 05h05m48s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h05m48s   !NEXT!

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

!* --- Scan from 05h27m48s to 05h28m48s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=05h27m48s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h28m48s   !NEXT!

!* --- Scan from 05h28m48s to 05h29m48s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h29m48s   !NEXT!

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

!* --- Scan from 05h51m48s to 05h52m48s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=05h51m48s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h52m48s   !NEXT!

!* --- Scan from 05h52m48s to 05h53m48s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=05h53m48s   !NEXT!

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

!* --- Scan from 06h15m48s to 06h16m48s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=06h15m48s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=06h16m48s   !NEXT!

!* --- Scan from 06h16m48s to 06h17m48s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=06h17m48s   !NEXT!

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

!* --- Scan from 06h39m48s to 06h40m48s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=06h39m48s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=06h40m48s   !NEXT!

!* --- Scan from 06h40m48s to 06h41m48s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=06h41m48s   !NEXT!

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

!* --- Scan from 07h05m55s to 07h06m55s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='3C345'  ra=16h42m58.809968s  dec= 39d48'36.99399"  qual=999  calib='V'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,715.99),( 2,715.99),( 3,815.99),( 4,815.99)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=07h05m55s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h06m55s   !NEXT!

!* --- Scan from 07h06m55s to 07h07m55s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='3C345'  ra=16h42m58.809968s  dec= 39d48'36.99399"  qual=  0  calib='V'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h07m55s   !NEXT!

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

!* --- Scan from 07h19m47s to 07h20m47s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=07h19m47s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h20m47s   !NEXT!

!* --- Scan from 07h20m47s to 07h21m47s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h21m47s   !NEXT!

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

!* --- Scan from 07h43m47s to 07h44m47s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=07h43m47s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h44m47s   !NEXT!

!* --- Scan from 07h44m47s to 07h45m47s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=07h45m47s   !NEXT!

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

!* --- Scan from 08h07m46s to 08h08m46s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=08h07m46s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h08m46s   !NEXT!

!* --- Scan from 08h08m46s to 08h09m46s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h09m46s   !NEXT!

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

!* --- Scan from 08h31m44s to 08h32m44s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=08h31m44s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h32m44s   !NEXT!

!* --- Scan from 08h32m44s to 08h33m44s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h33m44s   !NEXT!

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

!* --- Scan from 08h55m42s to 08h56m42s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=999  calib='L'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,721.22),( 2,721.22),( 3,821.22),( 4,821.22)
bbfilter=(1,2M),(2,2M),(3,2M),(4,2M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=08h55m42s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h56m42s   !NEXT!

!* --- Scan from 08h56m42s to 08h57m42s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='P-VXSGR'  ra=18h08m04.062000s  dec=-22d13'26.10000"  qual=  0  calib='L'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=08h57m42s   !NEXT!

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

!* --- Scan from 09h19m38s to 09h20m38s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='3C446'  ra=22h25m47.259295s  dec=-04d57'01.39083"  qual=999  calib='V'
fe=(1,7mm),(3,7mm)
synth=( 1,15.4),( 2, 7.6),( 3,11.6)
nchan= 4
format=NONE
barrel=roll_off
ifchan=(1,A),(2,C),(3,A),(4,C)
bbsynth=( 1,715.99),( 2,715.99),( 3,815.99),( 4,815.99)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
pcalxbit1=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
pcalxbit2=(5,OFF),(6,OFF),(7,OFF),(8,OFF)
tape=(1,STOP) write=(1,off)
stop=09h19m38s   !NEXT!        
qual=  0
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=09h20m38s   !NEXT!

!* --- Scan from 09h20m38s to 09h21m38s   Wed, 2000 Jul 05 --- *!
!*  Scan added by Sched for reference pointing  *!
sname='3C446'  ra=22h25m47.259295s  dec=-04d57'01.39083"  qual=  0  calib='V'
peakchan =  1
tape=(1,STOP) write=(1,off)
stop=09h21m38s   !NEXT!

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
