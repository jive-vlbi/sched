#!/bin/csh
#
#  Try with VEX file
#
# ========================================================
# =====  Script to make pointing files for the VLBA  =====
# ========================================================
#  Set stations to be processed.
#  Note that the case used here will appear in the sum and
#  vex file names, so it is better to use lower.
#  The schedules need to be different depending on whether
#  a 3mm receiver is present.
#  Examples:
#  set stalist_3mm="hn nl fd la pt kp ov mk"
#  set stalist_no3mm="br sc"
#  set stalist_no3mm=""      eg use blank if no stations of this type.
set stalist_3mm="pt hn"
set stalist_no3mm="hn"

#
#  Set times and experiment code
#
echo Sched environment variable = $SCHED
echo PLANET_DATA environoment variable = $PLANET_DATA
cat <<eofp >! times.par
  year    = 1996
  month   = 10
  day     = 25 
  start   = 0:00:00 
  opdur   = 4:00:00         !  Total project duration
eofp
set  expcode="ptg"

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
# Put much of the generic setup information in ptg_setup.par
#
cat <<eofs >! ptg_setup.par
! ---------------------------------------------------------------------
! SCHED input file setup stuff to be used with pointing proceedure doptg.

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

eofs

#  Now make a file with the actual schedule info for 3mm sites.
#  Any imbedded catalogs etc must be here.

cat <<eofs >! ptg_3mm.par
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


group=(10*3+8*3) rep=50

setup='$SETDIR/pt90cm.set'  source 'CYGA'    nodop   bw=0,0   /
setup='$SETDIR/pt4cm.set'   source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt18cm.set'  source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt6cm.set'   source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt13cm.set'  source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt4cmsx.set' source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt1cm.set'   source 'DR21'    nodop   bw=0,0   /
setup='$SETDIR/pt24ghz.set' source 'DR21'    nodop   bw=0,0   /
setup='$SETDIR/pt2cm.set'   source 'DR21'    nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source 'DR21'    nodop   bw=0,0   /

setup='$SETDIR/pt7mm.set'   source 'JUPITER' nodop   bw=0,0 qual=0
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  nopeak ptvlba   dwell = 5:45  /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45   /
setup='$SETDIR/pt7mm.set'   source 'RCAS'    doppler bw=2,2 qual=0
  linename='SiO431' 
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  nopeak ptvlba   dwell = 5:45 /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45  linename='SiO862'  /


setup='$SETDIR/pt90cm.set'  source 'TAUA'    nodop   bw=0,0   /
setup='$SETDIR/pt4cm.set'   source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt18cm.set'  source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt6cm.set'   source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt13cm.set'  source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt4cmsx.set' source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt1cm.set'   source '3C84'    nodop   bw=0,0   /
setup='$SETDIR/pt24ghz.set' source '3C84'    nodop   bw=0,0   /
setup='$SETDIR/pt2cm.set'   source '3C84'    nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source '3C84'    nodop   bw=0,0   /

setup='$SETDIR/pt7mm.set'   source 'RLEO'    doppler bw=2,2 qual=0
  linename='SiO431' 
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  nopeak ptvlba   dwell = 5:45  /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45  linename='SiO862' /
setup='$SETDIR/pt7mm.set'   source 'VENUS'   nodop   bw=0,0 qual=0 
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  nopeak ptvlba   dwell = 5:45  /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45   /

setup='$SETDIR/pt90cm.set'  source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt4cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt18cm.set'  source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt6cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt13cm.set'  source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt4cmsx.set' source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt1cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt24ghz.set' source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt2cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source '3C273'   nodop   bw=0,0   /

setup='$SETDIR/pt7mm.set'   source 'WHYA'    doppler bw=2,2 qual=0 
  linename='SiO431' 
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  nopeak ptvlba   dwell = 5:45  /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45  linename='SiO862' /
setup='$SETDIR/pt7mm.set'   source 'SATURN'  nodop   bw=0,0 qual=0 
  noptvlba peak=1 dwell=01:00  /
  noptvlba peak=1 dwell=01:00  /
  nopeak ptvlba   dwell = 5:45  /
  setup='$SETDIR/pt3mm.set'   nopeak ptvlba dwell = 5:45   /

! ---------------------------------------------------------------------
eofs

#  Now make a file with the actual schedule info for sites without 3mm.
#  Any imbedded catalogs etc must be here.

cat <<eofs >! ptg_no3mm.par
! -----  Spectral lines
! -----  Set up for 4 channels - 2 on line, 2 off.
lineinit /
 lineset ='H2O'    restfreq=22235.08,22235.08,22285.08,22285.08 /
 lineset ='SiO431' restfreq=43122.03,43122.03,43222.03,43222.03 /
 lineset ='SiO862' restfreq=86243.4, 86243.4, 86243.4, 86243.4  /
endlines /

! -----  The detailed schedule
! The frequencies are in an order reasonable for FRM.
!  50/90 cm  CYGA,    TAUA,    3C274
!  20-4 cm   3C454.3, 3C123,   3C274
!  2, 1 cm   DR21,    3C84,    3C274
!  7 mm      RCAS,    RLEO,    WHYA, DR21, 3C84, 3C273, planets
!  Group must equal the number of scans below.
!  (Group*rep) must be less than 2000 (as of 18 Apr 1996).
!  The 7mm line sources used to have qual=50.  This used to be the way
!     to trigger line processing.  Now CALCODE='L' does it.

linename='SiO431' 

group=(12*3) rep=60
setup='$SETDIR/pt90cm.set'  source 'CYGA'    nodop   bw=0,0   /
setup='$SETDIR/pt4cm.set'   source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt18cm.set'  source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt6cm.set'   source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt13cm.set'  source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt4cmsx.set' source '3C454.3' nodop   bw=0,0   /
setup='$SETDIR/pt1cm.set'   source 'DR21'    nodop   bw=0,0   /
setup='$SETDIR/pt24ghz.set' source 'DR21'   nodop   bw=0,0   /
setup='$SETDIR/pt2cm.set'   source 'DR21'    nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source 'DR21'    nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source 'RCAS'    doppler bw=2,2 qual=0 /
setup='$SETDIR/pt7mm.set'   source 'JUPITER' nodop   bw=0,0 qual=0 /

setup='$SETDIR/pt90cm.set'  source 'TAUA'    nodop   bw=0,0   /
setup='$SETDIR/pt4cm.set'   source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt18cm.set'  source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt6cm.set'   source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt13cm.set'  source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt4cmsx.set' source '3C123'   nodop   bw=0,0   /
setup='$SETDIR/pt1cm.set'   source '3C84'    nodop   bw=0,0   /
setup='$SETDIR/pt24ghz.set' source '3C84'   nodop   bw=0,0   /
setup='$SETDIR/pt2cm.set'   source '3C84'    nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source '3C84'    nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source 'RLEO'    doppler bw=2,2 qual=0 /
setup='$SETDIR/pt7mm.set'   source 'VENUS'   nodop   bw=0,0 qual=0 /

setup='$SETDIR/pt90cm.set'  source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt4cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt18cm.set'  source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt6cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt13cm.set'  source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt4cmsx.set' source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt1cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt24ghz.set' source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt2cm.set'   source '3C274'   nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source '3C273'   nodop   bw=0,0   /
setup='$SETDIR/pt7mm.set'   source 'WHYA'    doppler bw=2,2 qual=0 /
setup='$SETDIR/pt7mm.set'   source 'SATURN'  nodop   bw=0,0 qual=0 /
! ---------------------------------------------------------------------
eofs

#
# Now run sched separately for each 3mm station.
#
foreach station ( $stalist_3mm[1*] )
   echo station=vlba_$station >! station.par
   echo expcode=$expcode >> station.par
   $SCHED/bin/sched <<eofst
       @times.par
       @station.par
       @ptg_setup.par
       sch = ptg_3mm.par
       overwrit /
eofst
#
#  Make the sum and vex files station dependent.
#
   /bin/mv ptg.sum ptg.$station.sum
   /bin/mv ptg.vex ptg.$station.vex
end
#
# Finally run sched separately for each no 3mm station.
#
foreach station ( $stalist_no3mm[1*] )
   echo station=vlba_$station >! station.par
   echo expcode=$expcode >> station.par
   $SCHED/bin/sched <<eofst
       @times.par
       @station.par
       @ptg_setup.par
       sch = ptg_no3mm.par
       overwrit /
eofst
#
#  Make the sum and vex files station dependent.
#
   /bin/mv $expcode.sum $expcode.$station.sum
   /bin/mv $expcode.vex $expcode.$station.vex
end
#
#   Some clean up.
#
#/bin/rm times.par
#/bin/rm ptg_setup.par
#/bin/rm ptg_3mm.par
#/bin/rm ptg_no3mm.par
#/bin/rm station.par
#
#   END

