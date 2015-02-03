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
C     W.L. Taber     (NAIF)
C     B.V. Semenov   (NAIF)
C
C$ Version
C
C-    BRIEF Version 4.0.0, 08-SEP-2010 (BVS)
C
C        Moved WINRM from main program to this include file.
C
C-    BRIEF Version 3.0.0, 14-JAN-2008 (BVS)
C
C        Increased MAXBOD to 100,000 (from 20,000).
C
C        Increased CMDSIZ to 25,000 (from 4,000).
C
C        Updated version string and changed its format to
C        '#.#.#, Month DD, YYYY' (from '#.#.#').
C
C-    BRIEF Version 1.0.0, 14-MAR-1996 (WLT)
C
C        Initial release.
C
C-&

C
C     The Version is stored as a string.
C
      CHARACTER*(32)        VERSN
      PARAMETER           ( VERSN = '4.0.0, September 8, 2010' )

C
C     MAXUSE is the maximum number of bodies that can be explicitly
C     specified on the command line for brief summaries.
C
      INTEGER               MAXUSE
      PARAMETER           ( MAXUSE = 100 )

C
C     The longest command line that can be accommodated is
C     given by CMDSIZ
C
      INTEGER               CMDSIZ
      PARAMETER           ( CMDSIZ = 25000 )

C
C     The maximum number of bodies that can be summarized is stored
C     in the parameter MAXBOD
C
      INTEGER               MAXBOD
      PARAMETER           ( MAXBOD = 100000 )

C
C     The average number of intervals per body
C
      INTEGER               AVEWIN
      PARAMETER           ( AVEWIN = 20 )

C
C     The largest expected window
C
      INTEGER               LRGWIN
      PARAMETER           ( LRGWIN = 100000 )

C
C     Room in the DP symbol table that holds all windows for all 
C     objects. 
C
      INTEGER               WINRM
      PARAMETER           ( WINRM = MAXBOD*AVEWIN )

C
C     End of inlcude file.
C
