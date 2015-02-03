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
C
C     CHRONOS Include file.
C
 
C
C     CHRONOS Version.
C
      CHARACTER*(*)         VER
      PARAMETER           ( VER = '2.2.0, October 28, 2011' )
 
C
C     Environment variable that contains CHRONOS setup file name.
C
      CHARACTER*(*)         STPENV
      PARAMETER           ( STPENV = 'CHRONOS_SETUP_FILE' )
 
C
C     LST SOL marker.
C
      CHARACTER*(*)         SOLMKR
      PARAMETER           ( SOLMKR = 'SOL' )
 
C
C     Command lines keys and total number of them.
C
      CHARACTER*(*)         STPKEY
      PARAMETER           ( STPKEY = '-SETUP' )
 
      CHARACTER*(*)         FRMKEY
      PARAMETER           ( FRMKEY = '-FROM' )
 
      CHARACTER*(*)         FRTKEY
      PARAMETER           ( FRTKEY = '-FROMTYPE' )
 
      CHARACTER*(*)         TOKEY
      PARAMETER           ( TOKEY  = '-TO' )
 
      CHARACTER*(*)         TOTKEY
      PARAMETER           ( TOTKEY = '-TOTYPE' )
 
      CHARACTER*(*)         FMTKEY
      PARAMETER           ( FMTKEY = '-FORMAT' )
 
      CHARACTER*(*)         TIMKEY
      PARAMETER           ( TIMKEY = '-TIME' )
 
      CHARACTER*(*)         HLPKEY
      PARAMETER           ( HLPKEY = '-HELP' )
 
      CHARACTER*(*)         HKEY
      PARAMETER           ( HKEY   = '-H' )
 
      CHARACTER*(*)         USGKEY
      PARAMETER           ( USGKEY = '-USAGE' )
 
      CHARACTER*(*)         UKEY
      PARAMETER           ( UKEY   = '-U' )
 
      CHARACTER*(*)         TMLKEY
      PARAMETER           ( TMLKEY = '-TEMPLATE' )
 
      CHARACTER*(*)         BATKEY
      PARAMETER           ( BATKEY = '-BATCH' )
 
      CHARACTER*(*)         LBLKEY
      PARAMETER           ( LBLKEY = '-NOLABEL' )
 
      CHARACTER*(*)         TRCKEY
      PARAMETER           ( TRCKEY = '-TRACE' )
 
      CHARACTER*(*)         SIDKEY
      PARAMETER           ( SIDKEY = '-SC' )
 
      CHARACTER*(*)         BODKEY
      PARAMETER           ( BODKEY = '-CENTER' )
 
      CHARACTER*(*)         LSTKEY
      PARAMETER           ( LSTKEY = '-LANDINGTIME' )
 
      CHARACTER*(*)         SOLKEY
      PARAMETER           ( SOLKEY = '-SOL1INDEX' )
 
      INTEGER               MAXKEY
      PARAMETER           ( MAXKEY = 19 )
 
C
C     Setup file variables.
C
      CHARACTER*(*)         LSKKWD
      PARAMETER           ( LSKKWD = 'LEAPSECONDS_FILE' )
 
      CHARACTER*(*)         SCLKWD
      PARAMETER           ( SCLKWD = 'SCLK_FILE' )
 
      CHARACTER*(*)         PCKKWD
      PARAMETER           ( PCKKWD = 'PCK_FILE' )
 
      CHARACTER*(*)         SPKKWD
      PARAMETER           ( SPKKWD = 'SPK_FILES' )
 
      CHARACTER*(*)         CKKWD
      PARAMETER           ( CKKWD  = 'CK_FILES' )
 
      CHARACTER*(*)         FRAKWD
      PARAMETER           ( FRAKWD = 'FRAMES_FILE' )
 
      CHARACTER*(*)         SIDKWD
      PARAMETER           ( SIDKWD = 'SPACECRAFT_ID' )
 
      CHARACTER*(*)         BODKWD
      PARAMETER           ( BODKWD = 'CENTER_ID' )
 
      CHARACTER*(*)         LSTKWD
      PARAMETER           ( LSTKWD = 'LANDING_TIME' )
 
      CHARACTER*(*)         SOLKWD
      PARAMETER           ( SOLKWD = 'LANDING_SOL_INDEX' )
 
      CHARACTER*(*)         FMTKWD
      PARAMETER           ( FMTKWD = 'FORMAT' )
 
C
C     Time system indentifier strings and total number of time
C     systems.
C
      CHARACTER*(*)         UTCSYS
      PARAMETER           ( UTCSYS = 'UTC' )
 
      CHARACTER*(*)         ETSYS
      PARAMETER           ( ETSYS  = 'ET' )
 
      CHARACTER*(*)         SCLSYS
      PARAMETER           ( SCLSYS = 'SCLK' )
 
      CHARACTER*(*)         LSTSYS
      PARAMETER           ( LSTSYS = 'LST' )
 
      INTEGER               MAXSYS
      PARAMETER           ( MAXSYS = 4 )
 
C
C     Time types identitifier strings and total number of time types.
C
      CHARACTER*(*)         SCTTYP
      PARAMETER           ( SCTTYP = 'SCET' )
 
      CHARACTER*(*)         ERTTYP
      PARAMETER           ( ERTTYP = 'ERT' )
 
      CHARACTER*(*)         ETTTYP
      PARAMETER           ( ETTTYP = 'ETT' )
 
      CHARACTER*(*)         LTTYP
      PARAMETER           ( LTTYP  = 'LT' )
 
      CHARACTER*(*)         SECTYP
      PARAMETER           ( SECTYP = 'SECONDS' )
 
      CHARACTER*(*)         SCLTYP
      PARAMETER           ( SCLTYP = 'SCLK' )
 
      CHARACTER*(*)         HEXTYP
      PARAMETER           ( HEXTYP = 'HEX' )
 
      CHARACTER*(*)         TIKTYP
      PARAMETER           ( TIKTYP = 'TICKS' )
 
      CHARACTER*(*)         LSTTYP
      PARAMETER           ( LSTTYP = 'LST' )
 
      CHARACTER*(*)         LSNTYP
      PARAMETER           ( LSNTYP = 'LSUN' )
 
      INTEGER               MAXTYP
      PARAMETER           ( MAXTYP = 10 )
 
C
C     Line size parameters.
C
      INTEGER               WRDSIZ
      PARAMETER           ( WRDSIZ = 32   )
 
      INTEGER               LINSIZ
      PARAMETER           ( LINSIZ = 256  )
 
      INTEGER               LLNSIZ
      PARAMETER           ( LLNSIZ = 2048 )
 
C
C     File name length parameters.
C
      INTEGER               FILEN
      PARAMETER           ( FILEN  =  256 )
 
