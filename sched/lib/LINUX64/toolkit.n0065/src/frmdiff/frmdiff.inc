C$ Abstract
C
C     Include Section:  FRMDIFF Global Parameters
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Author_and_Institution
C
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    Version 2.1.0, 25-MAR-2014 (BVS).
C
C        Updated version.
C
C-    Version 2.0.0, 27-FEB-2012 (BVS).
C
C        Updated version.
C
C-    Version 1.0.0, 09-DEC-2008 (BVS).
C
C-&

C
C     Program name and version.
C
      CHARACTER*(*)         PGMNAM
      PARAMETER           ( PGMNAM = 'frmdiff' )

      CHARACTER*(*)         PGMVER
      PARAMETER           ( PGMVER ='2.1.0, March 25, 2014')

C
C     Command line keys.
C
      INTEGER               MAXKEY
      PARAMETER           ( MAXKEY = 25 )

      CHARACTER*(*)         KERKEY
      PARAMETER           ( KERKEY = '-k' )

      CHARACTER*(*)         TF1KEY
      PARAMETER           ( TF1KEY = '-t1' )

      CHARACTER*(*)         FF1KEY
      PARAMETER           ( FF1KEY = '-f1' )

      CHARACTER*(*)         CF1KEY
      PARAMETER           ( CF1KEY = '-c1' )

      CHARACTER*(*)         KE1KEY
      PARAMETER           ( KE1KEY = '-k1' )

      CHARACTER*(*)         TF2KEY
      PARAMETER           ( TF2KEY = '-t2' )

      CHARACTER*(*)         FF2KEY
      PARAMETER           ( FF2KEY = '-f2' )

      CHARACTER*(*)         CF2KEY
      PARAMETER           ( CF2KEY = '-c2' )

      CHARACTER*(*)         KE2KEY
      PARAMETER           ( KE2KEY = '-k2' )

      CHARACTER*(*)         AVKEY
      PARAMETER           ( AVKEY  = '-a' )

      CHARACTER*(*)         AVFKEY
      PARAMETER           ( AVFKEY = '-m' )

      CHARACTER*(*)         BEGKEY
      PARAMETER           ( BEGKEY = '-b' )

      CHARACTER*(*)         ENDKEY
      PARAMETER           ( ENDKEY = '-e' )

      CHARACTER*(*)         NSTKEY
      PARAMETER           ( NSTKEY = '-n' )

      CHARACTER*(*)         STPKEY
      PARAMETER           ( STPKEY = '-s' )

      CHARACTER*(*)         FMTKEY
      PARAMETER           ( FMTKEY = '-f' )

      CHARACTER*(*)         TYPKEY
      PARAMETER           ( TYPKEY = '-t' )

      CHARACTER*(*)         ANOKEY
      PARAMETER           ( ANOKEY = '-o' )

      CHARACTER*(*)         ANUKEY
      PARAMETER           ( ANUKEY = '-x' )

      CHARACTER*(*)         DIGKEY
      PARAMETER           ( DIGKEY = '-d' )

      CHARACTER*(*)         USGKEY
      PARAMETER           ( USGKEY = '-usage' )

      CHARACTER*(*)         UKEY
      PARAMETER           ( UKEY   = '-u' )

      CHARACTER*(*)         HLPKEY
      PARAMETER           ( HLPKEY = '-help' )

      CHARACTER*(*)         HKEY
      PARAMETER           ( HKEY   = '-h' )

      CHARACTER*(*)         VKEY
      PARAMETER           ( VKEY   = '-v' )

C
C     Command line key values.
C
      CHARACTER*(*)         YESVAL
      PARAMETER           ( YESVAL = 'yes' )

      CHARACTER*(*)         NOVAL
      PARAMETER           ( NOVAL  = 'no' )

      CHARACTER*(*)         FRMVAL
      PARAMETER           ( FRMVAL = 'from' )

      CHARACTER*(*)         TOVAL
      PARAMETER           ( TOVAL  = 'to' )

      CHARACTER*(*)         ETVAL
      PARAMETER           ( ETVAL  = 'et' )

      CHARACTER*(*)         SCSVAL
      PARAMETER           ( SCSVAL = 'sclk' )

      CHARACTER*(*)         SCDVAL
      PARAMETER           ( SCDVAL = 'sclkd' )

      CHARACTER*(*)         SCTVAL
      PARAMETER           ( SCTVAL = 'ticks' )

      CHARACTER*(*)         BASVAL
      PARAMETER           ( BASVAL = 'basic' )

      CHARACTER*(*)         STSVAL
      PARAMETER           ( STSVAL = 'stats' )

      CHARACTER*(*)         DMVAL
      PARAMETER           ( DMVAL  = 'dumpm' )

      CHARACTER*(*)         DQSVAL
      PARAMETER           ( DQSVAL = 'dumpqs' )

      CHARACTER*(*)         DQOVAL
      PARAMETER           ( DQOVAL = 'dumpqo' )

      CHARACTER*(*)         DEAVAL
      PARAMETER           ( DEAVAL = 'dumpea' )

      CHARACTER*(*)         DAAVAL
      PARAMETER           ( DAAVAL = 'dumpaa' )

      CHARACTER*(*)         DCVAL
      PARAMETER           ( DCVAL  = 'dumpc' )

      CHARACTER*(*)         DGVAL
      PARAMETER           ( DGVAL  = 'dumpg' )

      CHARACTER*(*)         XVAL
      PARAMETER           ( XVAL   = 'x' )

      CHARACTER*(*)         YVAL
      PARAMETER           ( YVAL   = 'y' )

      CHARACTER*(*)         ZVAL
      PARAMETER           ( ZVAL   = 'z' )

C
C     Max and min number orientations that the program can handle.
C
      INTEGER               MAXITR
      PARAMETER           ( MAXITR  = 1000000 )

      INTEGER               MINITR
      PARAMETER           ( MINITR  =       1 )

C
C     Default number orientations.
C
      INTEGER               DEFITR
      PARAMETER           ( DEFITR  = 1000 )

C
C     Maximum number of IDs in a CK or a binary PCK file
C
      INTEGER               MAXIDS
      PARAMETER           ( MAXIDS = 100 )

C
C     Line size parameters.
C
      INTEGER               SWDSIZ
      PARAMETER           ( SWDSIZ = 16 )

      INTEGER               WRDSIZ
      PARAMETER           ( WRDSIZ = 32 )

      INTEGER               LINSIZ
      PARAMETER           ( LINSIZ = 1024 )

      INTEGER               DSPSIZ
      PARAMETER           ( DSPSIZ = 80 )

C
C     Version, help, usage and header display parameters.
C
      INTEGER               VERCNT
      PARAMETER           ( VERCNT = 3 )

      INTEGER               USGCNT
      PARAMETER           ( USGCNT = 33 )

      INTEGER               HLPCNT
      PARAMETER           ( HLPCNT = 28 )

      INTEGER               INFMAX
      PARAMETER           ( INFMAX = 32 )

C
C     DAF descriptor size and component counts.
C
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NICK
      PARAMETER           ( NICK   = 6 )

      INTEGER               NIPCK
      PARAMETER           ( NIPCK  = 5 )

C
C     Cell lower boundary.
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

C
C     Maximum allowed number of coverage windows.
C
      INTEGER               WINSIZ
      PARAMETER           ( WINSIZ = MAXITR )

C
C     Smallest allowed step.
C
      DOUBLE PRECISION      STPTOL
      PARAMETER           ( STPTOL = 1.D-8 )

C
C     Fraction of step to be used as pad at the end of intervals.
C
      DOUBLE PRECISION      STPPAD
      PARAMETER           ( STPPAD = 0.5D0 )

C
C     Default, minimum, and maximum numbers of significant digits
C     allowed for numbers in dump reports.
C
      INTEGER               DEFSDG
      PARAMETER           ( DEFSDG  = 14 )

      INTEGER               MINSDG
      PARAMETER           ( MINSDG  =  6 )

      INTEGER               MAXSDG
      PARAMETER           ( MAXSDG  = 17 )

C
C     End of FRMDIFF parameters.
C

