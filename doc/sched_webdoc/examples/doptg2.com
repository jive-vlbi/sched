#!/bin/csh
#
# ========================================================
# =====  Script to make pointing files for the VLBA  =====
# ========================================================
# Based on doptg.com, but using DOSTA to avoid having 2
# full schedules for 3mm and non-3mm.
# Observes the 7ghz band within the 6 cm receiver.

# Clean up from a previous run if these didn't get deleted.

/bin/rm times.par
/bin/rm station.par
/bin/rm ptg_main.par
/bin/rm ptg_all.par

#  Set stations to be processed.
#  Stick to lower case to get expected file names.
#  Examples:
#  set stalist="sc hn nl fd la pt kp ov br mk"
#  Include br to check the offset frequency in 7ghz.
set stalist="pt br hn"
#
#  Set times and experiment code
#
echo Sched environment variable = $SCHED
echo PLANET_DATA environoment variable = $PLANET_DATA
cat <<eofp >! times.par
  year    = 2012
  month   = 6
  day     = 22 
  start   = 0:45:00 
  opdur   = 4:45:00         !  Total project duration
eofp
set  expcode="ptg2"

# =====================================================================
# =====================================================================
# ======                                                         ======
# ======      Changes not normally needed below this line.       ======
# ======                                                         ======
# =====================================================================
# =====================================================================
# Specify the location of the setup files.
#
setenv SETDIR $SCHED/setups
#

# Make most of the key file.
#
cat <<eofs >! ptg_main.par
! ---------------------------------------------------------------------
! SCHED input file setup stuff to be used with pointing proceedure doptg.

overwrit
! -----  Cover information
version  = 1  
expt     = 'STARTUP pointing'
obstype  = PTVLBA
obsmode  = 'Multi-frequency pointing observations.'
piname   = 'Operations'    
phone    = '505 835 7251'
obsphone = '505 835 7251'
fax      = '505 835 7027'
email    = 'vlbaops@nrao.edu'
address1 = 'AOC'

! -----  Catalogs etc.
srcfile = '$SCHED/catalogs/sources.pointing'
stafile = '$SCHED/catalogs/stations.dat'
ephfile = '$PLANET_DATA/JPLEPH.405.2'
!ephfile = '$SCHED/catalogs/JPLEPH.405.2.Linux'
! -----  Schedule instructions
sumitem = EL1, AZ1 ! Start elevation and azimuth in summary.
ptvlba    
ptdur   = 15       ! => scan length = ptslew + 2*ptdur + N*10*ptdur
                   ! The autoleveling scan is 2*ptdur long for pcx.
                   ! Here we have 15+30+2*150 = 345s = 5:45
dwell   = 5:45  
ptslew  = 15 
optmode = SCANS    ! Select scans that are above minimum elevation.
opminel = 20.      ! Minimum elevation.

! -----  Spectral lines
! -----  Set up for 4 channels - 2 on line, 2 off.
lineinit /
 lineset ='H2O'    restfreq=22235.08,22235.08,22285.08,22285.08 /
 lineset ='SiO431' restfreq=43122.03,43122.03,43222.03,43222.03 /
 lineset ='SiO862' restfreq=86243.4, 86243.4, 86343.4, 86343.4  /
endlines /
 
! -----  The detailed schedule
! The frequencies are in an order reasonable for FRM.
!  50/90 cm  CYGA,    TAUA,    3C274
!  20-4 cm   3C454.3, 3C123,   3C274
!  2, 1 cm   DR21,    3C84,    3C274
!  7 mm      RCAS,    RLEO,    WHYA, DR21, 3C84, 3C273, planets
!  3 mm      RCAS,    RLEO,    WHYA, planets

!  Group must equal the number of scans below.
!  (Group*rep) must be less than 2000 (as of 18 Apr 1996). 
!  The 7mm line sources used to have qual=50.  This used to be the way
!     to trigger line processing.  Now CALCODE='L' does it.
!  Note that for a peak=1 scan, no reference pointing will be done
!  if the slew takes longer than 40 seconds.  That is partly why
!  there is a second peak scan.


group=(11*3+8*3) rep=50

stations = sc, hn, nl, fd, la, pt, kp, ov, br, mk    ! all stations.
setup='$SETDIR/pt90cm.set'  source 'CYGA'    nodop   bw=0,0   /
setup='$SETDIR/pt4cm.set'   source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt18cm.set'  source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt6cm.set'   source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt7ghz.set'   source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt13cm.set'  source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt4cmsx.set' source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt1cm.set'   source 'DR21'    nodop   bw=0,0   /
setup='$SETDIR/pt24ghz.set' source 'DR21'    nodop   bw=0,0   /
setup='$SETDIR/pt2cm.set'   source 'DR21'    nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source 'DR21'    nodop   bw=0,0   /

  stations = nl, fd, la, pt, kp, ov, br, mk    ! 3mm stations
  setup='$SETDIR/pt7mm.set'   source 'JUPITER' nodop   bw=0,0 qual=0
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45   /

stations = sc, hn, nl, fd, la, pt, kp, ov, br, mk    ! all stations
setup='$SETDIR/pt7mm.set'   source 'JUPITER' nodop   bw=0,0 qual=0
nopeak ptvlba   dwell = 5:45  /

  stations = nl, fd, la, pt, kp, ov, br, mk    ! 3mm stations
  setup='$SETDIR/pt7mm.set'   source 'RCAS'    doppler bw=2,2 qual=0
  linename='SiO431' 
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45  linename='SiO862'  /

stations = sc, hn, nl, fd, la, pt, kp, ov, br, mk    ! all stations
linename='SiO431' 
setup='$SETDIR/pt7mm.set'   source 'RCAS'    doppler bw=2,2 qual=0
nopeak ptvlba   dwell = 5:45 /
nodop

stations = sc, hn, nl, fd, la, pt, kp, ov, br, mk    ! all stations.
setup='$SETDIR/pt90cm.set'  source 'TAUA'    nodop   bw=0,0   /
setup='$SETDIR/pt4cm.set'   source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt18cm.set'  source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt6cm.set'   source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt7ghz.set'   source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt13cm.set'  source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt4cmsx.set' source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt1cm.set'   source '3C84'    nodop   bw=0,0   /
setup='$SETDIR/pt24ghz.set' source '3C84'    nodop   bw=0,0   /
setup='$SETDIR/pt2cm.set'   source '3C84'    nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source '3C84'    nodop   bw=0,0   /

  stations = nl, fd, la, pt, kp, ov, br, mk    ! 3mm stations
  setup='$SETDIR/pt7mm.set'   source 'RLEO'    doppler bw=2,2 qual=0
  linename='SiO431' 
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45  linename='SiO862' /

stations = sc, hn, nl, fd, la, pt, kp, ov, br, mk    ! all stations.
linename='SiO431' 
setup='$SETDIR/pt7mm.set'   source 'RLEO'    doppler bw=2,2 qual=0
nopeak ptvlba   dwell = 5:45  /

  stations = nl, fd, la, pt, kp, ov, br, mk    ! 3mm stations
  setup='$SETDIR/pt7mm.set'   source 'VENUS'   nodop   bw=0,0 qual=0 
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45   /

stations = sc, hn, nl, fd, la, pt, kp, ov, br, mk    ! all stations.
setup='$SETDIR/pt7mm.set'   source 'VENUS'   nodop   bw=0,0 qual=0 
nopeak ptvlba   dwell = 5:45  /

stations = sc, hn, nl, fd, la, pt, kp, ov, br, mk    ! all stations
setup='$SETDIR/pt90cm.set'  source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt4cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt18cm.set'  source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt6cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt7ghz.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt13cm.set'  source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt4cmsx.set' source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt1cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt24ghz.set' source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt2cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source '3C273'   nodop   bw=0,0   /

  stations = nl, fd, la, pt, kp, ov, br, mk    ! 3mm stations
  setup='$SETDIR/pt7mm.set'   source 'WHYA'    doppler bw=2,2 qual=0 
  linename='SiO431' 
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45  linename='SiO862' /

stations = sc, hn, nl, fd, la, pt, kp, ov, br, mk    ! all stations
setup='$SETDIR/pt7mm.set'   source 'WHYA'    doppler bw=2,2 qual=0 
linename='SiO431' 
nopeak ptvlba   dwell = 5:45  /

  stations = nl, fd, la, pt, kp, ov, br, mk    ! 3mm stations
  setup='$SETDIR/pt7mm.set'   source 'SATURN'  nodop   bw=0,0 qual=0 
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45   /

stations = sc, hn, nl, fd, la, pt, kp, ov, br, mk    ! all stations
setup='$SETDIR/pt7mm.set'   source 'SATURN'  nodop   bw=0,0 qual=0 
nopeak ptvlba  dwell = 5:45  /

! ---------------------------------------------------------------------
eofs

#
# Finally run sched separately for each station.  Give the
# sum and vex files station dependent names so they won't be
# overwritten.
#
foreach station ( $stalist[1*] )
   /bin/rm station.par
   /bin/rm ptg_all.par
   echo " "
   echo " ----------------------------------------------"
   echo " ----------------------------------------------"
   echo " "
   echo " Processing $station"
#
#  Assemble the SCHED input file.
#
   echo dosta=vlba_$station >! station.par
   echo expcode=$expcode >> station.par
   cat  times.par station.par ptg_main.par > ptg_all.par
#
#  Run SCHED
#
   $SCHED/bin/sched < ptg_all.par
   /bin/mv $expcode.sum $expcode.$station.sum
   /bin/mv $expcode.vex $expcode.$station.vex
#
#   Some clean up.
#
#
end
#
#  Clean up par files used for all stations.
#
#/bin/rm times.par
#/bin/rm ptg_main.par
