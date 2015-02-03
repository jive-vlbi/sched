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
      SUBROUTINE NSPINT ( VERSN )
 
 
      IMPLICIT NONE
      CHARACTER*(*)         VERSN
 
C
C     This routine handles the task of initializing the various
C     items needed by INSPEKT at startup.
C
C     March 26, 2003  - WLT
C
C     Version 6.0  Added the DELIMITER and QUOTE specification required
C     by the "DELIMITED" report format.
C
C        
C     Version 5.0  Added the override call (BUILTO) that overrides
C     the generic environment reporting capability of the command
C     loop code.
C
C     Version 4.0  Added the default floating and integer formats
C     to be used by Inspekt.
C
C     Version 3.0  Help waiting is set to off by default.  Users
C     can reset it by typing "SET HELP WAIT"
C
C     Version 2.0  The environment variable that points to leapseconds
C     is now called LEAPSECONDS and not LEAPSCND as it was before.
C
 
C
C     Spicelib functions
C
      LOGICAL               EXISTS
 
C
C     Local Parameters.
C
C     FILSIZ   is the number of characters allowed an a filename
C     LEAPNM   is the logical/environment variable that points
C              to a leapsecond kernel.
C     SCLKNM   is the logical/environment variable that points
C              to an sclk kernel.
C
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 127 )
 
      CHARACTER*(FILSIZ)    LEAPS
      CHARACTER*(FILSIZ)    SCLK
 
      CHARACTER*(32)        LEAPNM
      CHARACTER*(32)        SCLKNM
      INTEGER               OFF
      PARAMETER           ( OFF    = 0 )
 
      INTEGER               ASK
      PARAMETER           ( ASK    = 1 )
 
      INTEGER               ON
      PARAMETER           ( ON     = 2 )
 
 
C
C     The first thing we need to do is clear the bulletin board
C     and open a log file.
C
      CALL BBCLR_1
C     CALL TRCOFF
C
C     Set up the default formats, page size etc.
C
      CALL BBPUTI_1 ('POST', 'PAGEHEIGHT',         1,  20              )
      CALL BBPUTI_1 ('POST', 'PAGEWIDTH',          1,  80              )
      CALL BBPUTI_1 ('POST', 'TITLEFREQUENCY',     1,   0              )
      CALL BBPUTI_1 ('POST', 'HEADERFREQUENCY',    1,   0              )
      CALL BBPUTI_1 ('POST', 'REPORTLIMIT',        1, 100              )
      CALL BBPUTI_1 ('POST', 'AUTOADJUST',         1, ASK              )
      CALL BBPUTI_1 ('POST', 'HELPPROMPT',         1,   0              )
 
 
      CALL BBPUTC_1 ('POST', 'FORMAT',             1,
     .                       'FLAGGED PRESERVED'                       )
      CALL BBPUTC_1 ('POST', 'FMTMARK',            1, '>'              )
      CALL BBPUTC_1 ('POST', 'PAGETITLE',          1, 'Inspekt Report' )
      CALL BBPUTC_1 ('POST', 'TITLEJUSTIFICATION', 1, 'LEFT'           )
      CALL BBPUTC_1 ('POST', 'TIMEFMT',            1,
     .               'YYYY MON DD HR:MN:SC::UTC::RND'                  )
      CALL BBPUTC_1 ('POST', 'INTFMT',             1, '###########'    )
      CALL BBPUTC_1 ('POST', 'DPFMT',              1, '#########.####' )
      CALL BBPUTC_1 ('POST', 'CHFMT',              1, '.............'  )
      CALL BBPUTC_1 ('POST', 'QUOTE',              1, '"'              )
      CALL BBPUTC_1 ('POST', 'DELIMITER',          1, 'TAB'            )
C
C     Override the default SHOW ENVIRONMENT command that is
C     available from the command loop code.
C
      CALL BUILTO ( 'ENVIRONMENT' )
 
 
C
C     Initialize the error reporting style used by META/2
C
      CALL M2SERR ( ' /cr(3:3)/cr ' , '/vt...', '.../vt' )
 
 
C
C     Set up the help system.
C
      CALL HLPINT ()
 
C
C     Now look for any environment variables that might be around
C     such as LEAPSECONDS, SCLK, etc.
C
      LEAPNM = 'LEAPSECONDS'
      SCLKNM = 'SCLK'
 
      LEAPS = ' '
      SCLK  = ' '
 
 
      CALL EXPFNM_1 ( LEAPNM, LEAPS )
      CALL EXPFNM_1 ( SCLKNM, SCLK  )
 
      IF ( LEAPS .NE. ' ' ) THEN
         IF ( EXISTS(LEAPS) ) THEN
            CALL LDPOOL ( LEAPS )
            CALL BBPUTC_1 ( 'POST', 'LEAPSECONDS', 1, LEAPS )
         END IF
      END IF
 
      IF ( SCLK .NE. ' ' ) THEN
         IF ( EXISTS(SCLK) ) THEN
            CALL LDPOOL ( SCLK  )
            CALL BBPUTC_1 ( 'POST', 'SCLK', 1, SCLK )
         END IF
      END IF
C
C     Finally present a greeting to the user.
C
      CALL NSPHI(VERSN)
 
      RETURN
      END
