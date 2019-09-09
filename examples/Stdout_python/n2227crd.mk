!*  Schedule for VLBA_MK   *!
!*  Experiment n2227    *!
!* Schedule Version:       4.00 *!
!* Processed by SCHED version:  11.50 *!
!* PI:       David Boboltz *!
!* Address:  United States Naval Observatory *!
!*           3450 Massachusetts Ave, NW *!
!*           Washington, DC 20392-5420 *!
!*            U.S.A. *!
!* Phone:    202 762 1488 *!
!* EMAIL:    ut1@nrao.edu *!
!* Fax: *!
!* Phone during observation: 202 762 1488 *!
!* Observing mode: S/X *!
!* Notes:    DELZN-based MK-PT UT1-UTC measurement *!
!*  *!
!*  *!
!*  *!
!*  Start at 14h30m00s     Tue, 2012 Aug 14  Day of year  227   *!
program=n2227   

diskformat=mark5c
media=(1,disk)

!* The first scan is preceeded by a setup scan *!
!* that ends at the start time of the first scan  *!

!* --- Scan from 14h30m04s to 14h30m20s   Tue, 2012 Aug 14 --- *!
sname='0606-223'  ra=06h08m59.686845s  dec=-22d20'20.95671"  qual=999  calib='V'
maxcaltime= 120
fe=(1,13cm),(2,4cm),(3,13cm),(4,4cm)
fexfer=(2,norm)
noise=(1,low-s),(2,low-s),(3,low-s),(4,low-s)
synth=( 1, 7.9),( 2,-2.9),( 3,15.9)
logging=STANDARD
nchan= 4
format=VLBA1:4
ifdistr=(1,0),(2,0),(3,0),(4,0)
baseband=(1,1),(2,2),(3,3),(4,4)
ifchan=(1,A),(2,A),(3,B),(4,B)
sideband=(1,L),(2,L),(3,L),(4,L)
bits=(1,2),(2,2),(3,2),(4,2)
period=(1,1),(2,1),(3,1),(4,1)
level=(1,-1),(2,-1),(3,-1),(4,-1)
azcolim=   0.00  elcolim=   0.00
bbsynth=( 1,808.01),( 2,648.01),( 3,680.01),( 4,872.01)
bbfilter=(1,16M),(2,16M),(3,16M),(4,16M)
pcal=1MHZ
pcalxbit1=(1,S1),(2,S3),(3,S1),(4,S3),(5,S1),(6,S2),(7,S3),(8,S4)
pcalxbit2=(1,S2),(2,S4),(3,S2),(4,S4),(5,M1),(6,M2),(7,M3),(8,M4)
pcalxfreq1=(1,10),(2,10),(3,13010),(4,13010),(5,0),(6,0),(7,0),(8,0)
pcalxfreq2=(1,10),(2,10),(3,13010),(4,13010),(5,0),(6,0),(7,0),(8,0)
samplerate=32M
disk=off
  date = 2012Aug14
stop=14h30m04s   !NEXT!        
qual=  0
disk=off
stop=14h30m20s   !NEXT!

!* --- Scan from 14h31m00s to 14h31m16s   Tue, 2012 Aug 14 --- *!
sname='0405-385'  ra=04h06m59.035342s  dec=-38d26'28.04235"  qual=999  calib='V'
disk=off
stop=14h31m00s   !NEXT!        
qual=  0
disk=off
stop=14h31m16s   !NEXT!

!* --- Scan from 14h33m00s to 14h33m16s   Tue, 2012 Aug 14 --- *!
sname='IIIZW2'  ra=00h10m31.005907s  dec= 10d58'29.50426"  qual=999  calib='V'
disk=off
stop=14h33m00s   !NEXT!        
qual=  0
disk=off
stop=14h33m16s   !NEXT!

!* --- Scan from 14h35m17s to 14h35m33s   Tue, 2012 Aug 14 --- *!
sname='0340+362'  ra=03h43m28.952413s  dec= 36d22'12.42972"  qual=999  calib='V'
disk=off
stop=14h35m17s   !NEXT!        
qual=  0
disk=off
stop=14h35m33s   !NEXT!

!* --- Scan from 14h38m17s to 14h38m33s   Tue, 2012 Aug 14 --- *!
sname='0115-214'  ra=01h17m48.780135s  dec=-21d11'06.63322"  qual=999  calib='V'
disk=off
stop=14h38m17s   !NEXT!        
qual=  0
disk=off
stop=14h38m33s   !NEXT!

!* --- Scan from 14h39m34s to 14h39m50s   Tue, 2012 Aug 14 --- *!
sname='0332-403'  ra=03h34m13.654493s  dec=-40d08'25.39799"  qual=999  calib='V'
disk=off
stop=14h39m34s   !NEXT!        
qual=  0
disk=off
stop=14h39m50s   !NEXT!

!* --- Scan from 14h40m25s to 14h40m41s   Tue, 2012 Aug 14 --- *!
sname='0534-340'  ra=05h36m28.432363s  dec=-34d01'11.46836"  qual=999  calib='V'
disk=off
stop=14h40m25s   !NEXT!        
qual=  0
disk=off
stop=14h40m41s   !NEXT!

!* --- Scan from 14h42m10s to 14h42m26s   Tue, 2012 Aug 14 --- *!
sname='0524+034'  ra=05h27m32.705442s  dec= 03d31'31.51657"  qual=999  calib='V'
disk=off
stop=14h42m10s   !NEXT!        
qual=  0
disk=off
stop=14h42m26s   !NEXT!

!* --- Scan from 14h42m53s to 14h43m09s   Tue, 2012 Aug 14 --- *!
sname='0506+101'  ra=05h09m27.457068s  dec= 10d11'44.60016"  qual=999  calib='V'
disk=off
stop=14h42m53s   !NEXT!        
qual=  0
disk=off
stop=14h43m09s   !NEXT!

!* --- Scan from 14h44m30s to 14h44m46s   Tue, 2012 Aug 14 --- *!
sname='0627-199'  ra=06h29m23.761864s  dec=-19d59'19.72364"  qual=999  calib='V'
disk=off
stop=14h44m30s   !NEXT!        
qual=  0
disk=off
stop=14h44m46s   !NEXT!

!* --- Scan from 14h45m09s to 14h45m25s   Tue, 2012 Aug 14 --- *!
sname='0537-286'  ra=05h39m54.281481s  dec=-28d39'55.94809"  qual=999  calib='V'
disk=off
stop=14h45m09s   !NEXT!        
qual=  0
disk=off
stop=14h45m25s   !NEXT!

!* --- Scan from 14h47m00s to 14h47m16s   Tue, 2012 Aug 14 --- *!
sname='0048-097'  ra=00h50m41.317387s  dec=-09d29'05.21036"  qual=999  calib='V'
disk=off
stop=14h47m00s   !NEXT!        
qual=  0
disk=off
stop=14h47m16s   !NEXT!

!* --- Scan from 14h47m36s to 14h47m52s   Tue, 2012 Aug 14 --- *!
sname='0055-059'  ra=00h58m05.066312s  dec=-05d39'52.27820"  qual=999  calib='V'
disk=off
stop=14h47m36s   !NEXT!        
qual=  0
disk=off
stop=14h47m52s   !NEXT!

!* --- Scan from 14h50m16s to 14h50m32s   Tue, 2012 Aug 14 --- *!
sname='0442+389'  ra=04h46m11.494033s  dec= 39d00'17.09996"  qual=999  calib='V'
disk=off
stop=14h50m16s   !NEXT!        
qual=  0
disk=off
stop=14h50m32s   !NEXT!

!* --- Scan from 14h50m54s to 14h51m10s   Tue, 2012 Aug 14 --- *!
sname='0415+398'  ra=04h19m22.549519s  dec= 39d55'28.97755"  qual=999  calib='V'
disk=off
stop=14h50m54s   !NEXT!        
qual=  0
disk=off
stop=14h51m10s   !NEXT!

!* --- Scan from 14h51m38s to 14h51m54s   Tue, 2012 Aug 14 --- *!
sname='0345+460'  ra=03h49m18.741574s  dec= 46d09'59.65778"  qual=999  calib='V'
disk=off
stop=14h51m38s   !NEXT!        
qual=  0
disk=off
stop=14h51m54s   !NEXT!

!* --- Scan from 14h53m34s to 14h53m50s   Tue, 2012 Aug 14 --- *!
sname='0804+499'  ra=08h08m39.666284s  dec= 49d50'36.53042"  qual=999  calib='V'
disk=off
stop=14h53m34s   !NEXT!        
qual=  0
disk=off
stop=14h53m50s   !NEXT!

!* --- Scan from 14h54m08s to 14h54m24s   Tue, 2012 Aug 14 --- *!
sname='0749+540'  ra=07h53m01.384568s  dec= 53d52'59.63709"  qual=999  calib='V'
disk=off
stop=14h54m08s   !NEXT!        
qual=  0
disk=off
stop=14h54m24s   !NEXT!

!* --- Scan from 14h57m11s to 14h57m27s   Tue, 2012 Aug 14 --- *!
sname='0422-380'  ra=04h24m42.243708s  dec=-37d56'20.78427"  qual=999  calib='V'
disk=off
stop=14h57m11s   !NEXT!        
qual=  0
disk=off
stop=14h57m27s   !NEXT!

!* --- Scan from 14h57m47s to 14h58m03s   Tue, 2012 Aug 14 --- *!
sname='0402-362'  ra=04h03m53.749901s  dec=-36d05'01.91321"  qual=999  calib='V'
disk=off
stop=14h57m47s   !NEXT!        
qual=  0
disk=off
stop=14h58m03s   !NEXT!

!* --- Scan from 15h00m13s to 15h00m29s   Tue, 2012 Aug 14 --- *!
sname='1803+784'  ra=18h00m45.683919s  dec= 78d28'04.01843"  qual=999  calib='V'
disk=off
stop=15h00m13s   !NEXT!        
qual=  0
disk=off
stop=15h00m29s   !NEXT!

!* --- Scan from 15h00m49s to 15h01m05s   Tue, 2012 Aug 14 --- *!
sname='1637+826'  ra=16h32m31.969908s  dec= 82d32'16.39994"  qual=999  calib='V'
disk=off
stop=15h00m49s   !NEXT!        
qual=  0
disk=off
stop=15h01m05s   !NEXT!

!* --- Scan from 15h03m18s to 15h03m34s   Tue, 2012 Aug 14 --- *!
sname='0405-385'  ra=04h06m59.035342s  dec=-38d26'28.04235"  qual=999  calib='V'
disk=off
stop=15h03m18s   !NEXT!        
qual=  0
disk=off
stop=15h03m34s   !NEXT!

!* --- Scan from 15h03m59s to 15h04m15s   Tue, 2012 Aug 14 --- *!
sname='0400-319'  ra=04h02m21.266001s  dec=-31d47'25.94555"  qual=999  calib='V'
disk=off
stop=15h03m59s   !NEXT!        
qual=  0
disk=off
stop=15h04m15s   !NEXT!

!* --- Scan from 15h05m32s to 15h05m48s   Tue, 2012 Aug 14 --- *!
sname='0019+058'  ra=00h22m32.441211s  dec= 06d08'04.26890"  qual=999  calib='V'
disk=off
stop=15h05m32s   !NEXT!        
qual=  0
disk=off
stop=15h05m48s   !NEXT!

!* --- Scan from 15h06m08s to 15h06m24s   Tue, 2012 Aug 14 --- *!
sname='IIIZW2'  ra=00h10m31.005907s  dec= 10d58'29.50426"  qual=999  calib='V'
disk=off
stop=15h06m08s   !NEXT!        
qual=  0
disk=off
stop=15h06m24s   !NEXT!

!* --- Scan from 15h08m43s to 15h08m59s   Tue, 2012 Aug 14 --- *!
sname='0430+289'  ra=04h33m37.829860s  dec= 29d05'55.47703"  qual=999  calib='V'
disk=off
stop=15h08m43s   !NEXT!        
qual=  0
disk=off
stop=15h08m59s   !NEXT!

!* --- Scan from 15h09m32s to 15h09m48s   Tue, 2012 Aug 14 --- *!
sname='0358+210'  ra=04h01m45.166077s  dec= 21d10'28.58696"  qual=999  calib='V'
disk=off
stop=15h09m32s   !NEXT!        
qual=  0
disk=off
stop=15h09m48s   !NEXT!
disk=off
stop=15h09m53s   !NEXT!
     !QUIT! 
