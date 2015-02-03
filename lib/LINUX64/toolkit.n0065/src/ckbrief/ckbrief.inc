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
C     Y.K. Zaiko     (BERC)      
C     B.V. Semenov   (NAIF)      
C
C$ Version
C
C-    Toolkit Version 6.1.0, 27-JUN-2014 (BVS)
C
C        BUG FIX: changed logic to make a combination of -a and an ID
C        specified on the command line work in all cases.
C
C-    CKBRIEF Version 6.0.0, 2014-04-28 (BVS) (NJB)
C
C        Modified to treat all files as a single file (-a).
C
C        Changed SCLKD display format to include 6 decimal 
C        places.
C
C        Increased MAXBOD to 1,000,000 (from 100,000) and CMDSIZ to 
C        50,000 (from 25,000).
C
C        Added support for CK type 6.
C
C-    CKBRIEF Version 5.0.0, 2009-02-11 (BVS)
C
C        Updated version.
C
C-    CKBRIEF Version 4.0.0, 2008-01-13 (BVS)
C
C        Increased MAXBOD to 100,000 (from 10,000).
C
C        Increased CMDSIZ to 25,000 (from 4,000).
C
C        Updated version string and changed its format to 
C        '#.#.#, Month DD, YYYY' (from '#.#.#, YYYY-MM-DD').
C
C-    CKBRIEF Version 3.2.0, 2006-11-02 (BVS)
C
C        Updated version string.
C
C-    CKBRIEF Version 3.1.0, 2005-11-08 (BVS)
C
C        Updated version string.
C
C-    CKBRIEF Version 2.0.0, 2001-05-16 (BVS)
C
C        Increased MAXBOD to 10000 (from 4000). Set LRGWIN to be
C        MAXBOD*2 (was MAXBOD). Changed version string.
C
C-    CKBRIEF Version 1.1.2, 2001-04-09 (BVS)
C
C        Changed version parameter.
C
C-    CKBRIEF Version 1.0.0 beta, 1999-02-17 (YKZ)(BVS)
C
C        Initial release.
C
C-&

C
C     The Version is stored as a string.
C
      CHARACTER*(*)         VERSN
      PARAMETER           ( VERSN = '6.1.0, June 27, 2014' )
      
C
C     The maximum number of segments or interpolation intervals 
C     that can be summarized is stored in the parameter MAXBOD.
C     This is THE LIMIT that should be increased if window 
C     routines called by CKBRIEF fail.
C
      INTEGER               MAXBOD
      PARAMETER           ( MAXBOD = 1000000 )
      
C
C     The largest expected window -- must be twice the size of 
C     MAXBOD for consistency.
C
      INTEGER               LRGWIN
      PARAMETER           ( LRGWIN = MAXBOD*2 )
      
C
C     The longest command line that can be accommodated is
C     given by CMDSIZ.
C
      INTEGER               CMDSIZ
      PARAMETER           ( CMDSIZ = 50000 )
      
C
C     MAXUSE is the maximum number of objects that can be explicitly
C     specified on the command line for ckbrief summaries.
C
      INTEGER               MAXUSE
      PARAMETER           ( MAXUSE = 100 )
      
C
C     Generic line size for all modules.
C
      INTEGER               LINESZ
      PARAMETER           ( LINESZ = 256 )
      
C
C     Time type keys.
C
      CHARACTER*(*)         ET
      PARAMETER           ( ET      = 'ET' )
      
      CHARACTER*(*)         DPSCLK
      PARAMETER           ( DPSCLK  = 'TICKS' )
      
      CHARACTER*(*)         SCLK
      PARAMETER           ( SCLK    = 'SCLK' )
      
      CHARACTER*(*)         UTC
      PARAMETER           ( UTC     = 'UTC' )
      
      CHARACTER*(*)         DOY
      PARAMETER           ( DOY     = 'UTC/DOY' )

C
C     Output time format pictures.
C
      CHARACTER*(*)         PICTDP
      PARAMETER           ( PICTDP = 'xxxxxxxxxxxxxx.xxxxxx' )
      
      CHARACTER*(*)         PICTUT
      PARAMETER           ( PICTUT = 'YYYY-MON-DD HR:MN:SC.###' )

      CHARACTER*(*)         PICTET
      PARAMETER           ( PICTET = 'YYYY-MON-DD HR:MN:SC.### ::TDB' )

      CHARACTER*(*)         PICTDY
      PARAMETER           ( PICTDY = 'YYYY-DOY // HR:MN:SC.###' )
      
      CHARACTER*(*)         NOTIME
      PARAMETER           ( NOTIME = 'NEED LSK AND SCLK FILES' )
      
