!  Instructions to control reference pointing.

!  This file is maintained in $SCHED/catalogs/Master_NRAO/peak.cmd.
!  The versions in $SCHED/catalogs/ are produced by Update which
!  alters the setup files for each case.

!  This file provides information that allows SCHED to create reference
!  pointing observations with minimal user input, either in response
!  to the POINT command or when using AUTOPEAK.  From this, Update in
!  the catalogs area will create 3 files, one each for use with Mark5a
!  recording, one for the RDBE_PFB and one for the RDBE_DDC.  They only
!  differ in the specified setup file.

!  Not that when POINT or AUTOPEAK are used in RDBE based observations,
!  the CRD parameters such as CRDDOP, CRDBW etc are used so that the
!  legacy BBCs, whose power data are used for pointing, can be set 
!  independently of the RDBE.  This is required for the RDBE_PFB because
!  it cannot use the narrow bandwidths needed for pointing on masers.



!  SOURCE CATALOG:

!  Specify the source catalog file with pointing sources.  These 
!  will be added to the normal source list so the names should be 
!  distinct from sources in the main source list (often sources.vlba).  
!  Note that some sources in the main list can also be used for pointing.

 srcfile = $SCHED/catalogs/sources.pointing ! Reference pointing sources



!  LINE REST FREQUENCIES

! Add the rest frequencies to the list of lines.
! This can also be done in the main schedule inputs.
! For here, use special names that won't conflict with what users
! use in the main program.  This is being done because there are 4
! rest frequencies given, two of which are off the line for use
! with the pointing setup file, which has 4 channels and allows
! on/off line difference power pointing (although that is not yet
! available for reference pointing, it is used for pointing tests.)
! The first two frequencies are the actual line frequency.

lineinit  /
lineset='PSiO431'  restfreq=43122.027, 43122.027, 43222.027, 43222.027 /
lineset='PSiO862'  restfreq=86243.4,   86243.4,   86443.4,   86443.4   /
lineset ='PH2O'    restfreq=22235.08,  22235.08,  22285.08,  22285.08  /
endlines /    



!  CONTROL PARAMETERS

!  The first three parameters are for automatic insertion of peaking 
!  scans only and apply to all 3mm groups.  

minfreq = 60000.0     !  Don't bother for lower frequencies (MHz)
dwell = 1:00          !  Duration of peak scan.
minel = 10.00         !  Minimum elevation for pointing.

!  These also apply to scans for which conversion to pointing is 
!  requested with POINT.  The setup will be modified by Update
!  for the appropriate file for each varient of peak.cmd in the
!  base catalogs directory.  The file choices that replace "pt7mm.set" are:

!     pt7mm.set - for non-rdbe observing.  "peak.cmd"
!     ptd7mm.set - for observing with the RDBE_DDC   "peak_RDBE_DDC.cmd"
!     ptr7mm.set - for observing with the RDBE_PFB   "peak_RDBE_PFB.cmd"
!    For line pointing replace "pt7mm.set" with:
!     pt7mm.set - for non-rdbe observing.  "peak.cmd"
!     ptdl7mm.set - for the RDBE_DDC       "peak_RDBE_DDC.cmd"
!     ptr7mm.set - for the RDBE_PFB        "peak_RDBE_PFB.cmd"
!  These are the same as the continuum for the old systems and for the
!  RDBE.

setup = $SCHED/setups/pt7mm.set
setupl = $SCHED/setups/pt7mm.set
linename = 'PSiO431'



!  SOURCES CHOICES

!  The VLBA pointing for 3mm projects will be done at 7mm.  Essentially all
!  useful sources are variable, so the best sources can vary with time.
!  Keeping only the best sources should be fairly reliable.  The other
!  sources here are shown in case it is important to find something
!  closer to the target.  The scheduler will have to add the desired
!  source(s) to the uncommented list and will have to make the decision
!  on whether the source is expected to be usable.

!! STRONG sources, pretty much guaranteed.  Sources starting with P-
!! are SiO masers.  The others are continuum sources.
!! Vivek Dhawan, 2001 feb12.

sources = 'P-OCeti','P-IkTau','P-TxCam','P-Orinew2','P-RLeo','P-UHer',
          'P-VYCma','P-VXSgr', 'SiO-WHya','SiO-RCas',
          '3C279','1334-127','3C273','3C345','0607-157',
          '3C84'

!  3C446 and NRAO530 used to be in this list, but appear to be too weak
!  as of May 2013 according to the BU Blazar project.
!  3C345's peak is low in the Blazar project, but it's total might be higher.
!  1334-127, 0607-157 are not in the BU project or VLBA regular pointing
!  so their status is unknown
!    This source list need to be revisited.

!! moderately strong sources, but variable so they may not always work..
!! Vivek 2001 Feb12.

!  'P-GXMon','P-RCnc', 'P-RLMi','P-AHSco','P-GYAql','P-ChiCyg','P-NMLCyg',
!  'P-RPeg','P-RAqr'

!! moderately strong continuum sources at 7mm: mostly From BIMA phase-cal list,
!! above about 4Jy at 3mm, expected to be above about 8Jy total at 7mm.  These
!! might be good for pointing.  They need to be tested.

!  '2145+067','2255-282','NRAO150','3C454.3','1921-293','0420-014','4C39.25',
!  'DR21'


!  STATION GROUPS:

!  There can be up to 5 groups of stations that can have separate
!  pointing scans handled automatically (you can do anything you
!  want using full scan specifications).  When pointing is requested,
!  all stations in a group use the same source.  A different source
!  can be picked for other groups, which are typically at other 
!  longitudes.

!  GROUP 1: Eastern VLBA antennas at 3mm.  Station names 
!  or codes can be specified.  These apply to inserted scans 
!  for group 1 or for any scans for which POINT=1 or POINT=0 
!  was specified.

 stations = vlba_sc, vlba_hn, vlba_nl, vlba_fd, vlba_la, 
            vlba_pt, vlba_kp
  /

!  GROUP 2:  Western VLBA stations. 
!  Use most of the same parameters as GROUP 1.

stations = vlba_ov, vlba_br, vlba_mk

  /
