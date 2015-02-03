C$ Abstract
C
C     Include Section:  SPKDIFF Global Parameters
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
C-    Version 2.0.0, 25-MAR-2014 (BVS).
C
C        Updated for extended functionality (sampling, non-continuous
C        comparison window, coverage and gap display, number of
C        significant digits in the output).
C
C-    Version 1.0.0, 09-DEC-2008 (BVS).
C
C-&

C
C     Program name and version.
C
      CHARACTER*(*)         PGMNAM
      PARAMETER           ( PGMNAM = 'spkdiff' )

      CHARACTER*(*)         PGMVER
      PARAMETER           ( PGMVER = '2.0.0, March 25, 2014')

C
C     Command line keys.
C
      INTEGER               MAXKEY
      PARAMETER           ( MAXKEY = 21 )

      CHARACTER*(*)         SB1KEY
      PARAMETER           ( SB1KEY = '-b1' )

      CHARACTER*(*)         SC1KEY
      PARAMETER           ( SC1KEY = '-c1' )

      CHARACTER*(*)         FR1KEY
      PARAMETER           ( FR1KEY = '-r1' )

      CHARACTER*(*)         KE1KEY
      PARAMETER           ( KE1KEY = '-k1' )

      CHARACTER*(*)         SB2KEY
      PARAMETER           ( SB2KEY = '-b2' )

      CHARACTER*(*)         SC2KEY
      PARAMETER           ( SC2KEY = '-c2' )

      CHARACTER*(*)         FR2KEY
      PARAMETER           ( FR2KEY = '-r2' )

      CHARACTER*(*)         KE2KEY
      PARAMETER           ( KE2KEY = '-k2' )

      CHARACTER*(*)         KERKEY
      PARAMETER           ( KERKEY = '-k' )

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
      CHARACTER*(*)         STSVAL
      PARAMETER           ( STSVAL = 'stats' )

      CHARACTER*(*)         DMPVAL
      PARAMETER           ( DMPVAL = 'dump' )

      CHARACTER*(*)         DVFVAL
      PARAMETER           ( DVFVAL = 'dumpvf' )

      CHARACTER*(*)         BSCVAL
      PARAMETER           ( BSCVAL = 'basic' )

      CHARACTER*(*)         DCVAL
      PARAMETER           ( DCVAL  = 'dumpc' )

      CHARACTER*(*)         DGVAL
      PARAMETER           ( DGVAL  = 'dumpg' )

C
C     Max and min number states that the program can handle.
C
      INTEGER               MAXITR
      PARAMETER           ( MAXITR  = 1000000 )
 
      INTEGER               MINITR
      PARAMETER           ( MINITR  =       2 )
 
C
C     Default number states.
C
      INTEGER               DEFITR
      PARAMETER           ( DEFITR  = 1000 )

C
C     Maximum number of IDs in an SPK file
C
      INTEGER               MAXIDS
      PARAMETER           ( MAXIDS = 50000 )

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
C     Version, help, usage, and header display parameters.
C
      INTEGER               VERCNT
      PARAMETER           ( VERCNT = 3 )

      INTEGER               USGCNT
      PARAMETER           ( USGCNT = 29 )

      INTEGER               HLPCNT
      PARAMETER           ( HLPCNT = 23 )

      INTEGER               INFMAX
      PARAMETER           ( INFMAX = 27 )

C
C     DAF descriptor size and component counts.
C
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

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
C     End of SPKDIFF parameters.
C

