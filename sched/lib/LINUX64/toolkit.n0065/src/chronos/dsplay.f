C$Procedure      DSPLAY( Display static descriptive information ) 

      SUBROUTINE DSPLAY( WHAT, ACTION )

C$ Abstract
C
C     Displays CHRONOS usage, help or template information and stops
C     the program if needed.
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
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     None
C
C$ Declarations
 
      IMPLICIT NONE
      
      INCLUDE              'chronos.inc'
      
      CHARACTER*(*)         WHAT
      CHARACTER*(*)         ACTION

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WHAT       I   "What should be displayed" key.
C     ACTION     I   Stop/continue key.
C
C$ Detailed_Input
C
C     WHAT           Key specifying what should be displayed. The value 
C                    can be 'VERSION', 'USAGE', 'HELP' or 'TEMPLATE'.
C
C     ACTION         Key specifying whether program should proceed 
C                    or stop ('STOP', 'PROCEED')
C
C$ Detailed_Output
C
C     None.
C
C     The routine prints requested message to STDOUT and stops (or not)
C     depending on the requested action.
C
C$ Parameters
C
C     See the include file.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     If WHAT value is not one of the values listed in Detailed_Input,
C     then no message is displayed.
C
C     If ACTION value is not 'STOP', then the routine does not stop the
C     program.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     B.V.Semenov      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    CHRONOS Version 2.0.0, 03-JAN-2002 (BVS)
C 
C        Updated usage and template displays to reflect addition of 
C        the new command line switches and FURNSH capability in the
C        calling program.
C
C-    CHRONOS Version 1.2.2, 03-MAY-2001 (BVS)
C
C        Fixed "disappearing" backslashes in template output.
C
C-    CHRONOS Version 1.0.0, 13-MAY-1998 (BVS)
C
C-&
 
C
C     Local parameters.
C
      INTEGER               DSPSIZ
      PARAMETER           ( DSPSIZ = 80 )

      INTEGER               VERCNT
      PARAMETER           ( VERCNT = 4 )
      
      INTEGER               USGCNT
      PARAMETER           ( USGCNT = 26 )
      
      INTEGER               HLPCNT
      PARAMETER           ( HLPCNT = 18 )
      
      INTEGER               TMLCNT
      PARAMETER           ( TMLCNT = 54 )

C
C     Backslash character ord.
C
      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )
            
C
C     Local variables.
C
      CHARACTER*(DSPSIZ)    VERMSG ( VERCNT )
      CHARACTER*(DSPSIZ)    USGMSG ( USGCNT )
      CHARACTER*(DSPSIZ)    HLPMSG ( HLPCNT )
      CHARACTER*(DSPSIZ)    TMLMSG ( TMLCNT )
      
      INTEGER               STDOUT
      
C
C     SPICELIB function.
C
      LOGICAL               RETURN
      
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DSPLAY' )
      END IF

C
C     Version display.
C
      VERMSG(  1 ) = ' '
      VERMSG(  2 ) = 'CHRONOS -- Universal Time Conversion Too'
     .//             'l'
      VERMSG(  3 ) = 'Version ' // VER
      VERMSG(  4 ) = ' '
 
C
C     Usage display.
C
      USGMSG(  1 ) = 'CHRONOS Usage'
      USGMSG(  2 ) = '---------------------------------------'
     .//             '---------------------------'
      USGMSG(  3 ) = ' '
      USGMSG(  4 ) = '   To convert time from one supported s'
     .//             'ystem/type to another:'
      USGMSG(  5 ) = ' '
      USGMSG(  6 ) = '      % CHRONOS # <setup file name OR ' 
     .//             'kernel file name(s)>'
      USGMSG(  7 ) = '                # <"from" time system>'
      USGMSG(  8 ) = '               [# <"from" time type>]'
      USGMSG(  9 ) = '                # <"to" time system>'
      USGMSG( 10 ) = '               [# <"to" time type>]'
      USGMSG( 11 ) = '               [# <output time format pi'
     .//             'cture>]'
      USGMSG( 12 ) = '                # <input time> | #'
      USGMSG( 13 ) = '               [# <sc ID>]'
      USGMSG( 14 ) = '               [# <cental body ID>]'
      USGMSG( 15 ) = '               [# <UTC time of the landing>]'
      USGMSG( 16 ) = '               [# <index of the first SOL>]'
      USGMSG( 17 ) = '               [#]'
      USGMSG( 18 ) = '               [#]'
      USGMSG( 19 ) = ' '
      USGMSG( 20 ) = '   To display usage:               % '
     .//             'CHRONOS [#|#]'
      USGMSG( 21 ) = '   To display help:                % CHRONOS #|#'
      USGMSG( 22 ) = '   To display setup file template: % CHRONOS #'
      USGMSG( 23 ) = ' '
      USGMSG( 24 ) = '   The case of command line switches is'
     .//             ' insignificant. Switches shown within '
      USGMSG( 25 ) = '   square braces [] are optional. See User''s '
     .//             'Guide for details on # usage.'
      USGMSG( 26 ) = ' '
      CALL REPMC ( USGMSG(  6 ), '#', STPKEY, USGMSG(  6 ) )
      CALL REPMC ( USGMSG(  7 ), '#', FRMKEY, USGMSG(  7 ) )
      CALL REPMC ( USGMSG(  8 ), '#', FRTKEY, USGMSG(  8 ) )
      CALL REPMC ( USGMSG(  9 ), '#', TOKEY,  USGMSG(  9 ) )
      CALL REPMC ( USGMSG( 10 ), '#', TOTKEY, USGMSG( 10 ) )
      CALL REPMC ( USGMSG( 11 ), '#', FMTKEY, USGMSG( 11 ) )
      CALL REPMC ( USGMSG( 12 ), '#', TIMKEY, USGMSG( 12 ) )
      CALL REPMC ( USGMSG( 12 ), '#', BATKEY, USGMSG( 12 ) )
      CALL REPMC ( USGMSG( 13 ), '#', SIDKEY, USGMSG( 13 ) )
      CALL REPMC ( USGMSG( 14 ), '#', BODKEY, USGMSG( 14 ) )
      CALL REPMC ( USGMSG( 15 ), '#', LSTKEY, USGMSG( 15 ) )
      CALL REPMC ( USGMSG( 16 ), '#', SOLKEY, USGMSG( 16 ) )
      CALL REPMC ( USGMSG( 17 ), '#', LBLKEY, USGMSG( 17 ) )
      CALL REPMC ( USGMSG( 18 ), '#', TRCKEY, USGMSG( 18 ) )
      CALL REPMC ( USGMSG( 20 ), '#', HLPKEY, USGMSG( 20 ) )
      CALL REPMC ( USGMSG( 20 ), '#', HKEY,   USGMSG( 20 ) )
      CALL REPMC ( USGMSG( 21 ), '#', USGKEY, USGMSG( 21 ) )
      CALL REPMC ( USGMSG( 21 ), '#', UKEY,   USGMSG( 21 ) )
      CALL REPMC ( USGMSG( 22 ), '#', TMLKEY, USGMSG( 22 ) )
      CALL REPMC ( USGMSG( 25 ), '#', BATKEY, USGMSG( 25 ) )
     
C
C     Help display.
C
      HLPMSG(  1 ) = 'CHRONOS Help'
      HLPMSG(  2 ) = '---------------------------------------'
     .//             '---------------------------'
      HLPMSG(  3 ) = ' '
      HLPMSG(  4 ) = 'CHRONOS is a time conversion tool '
     .//             'capable of converting times'
      HLPMSG(  5 ) = 'between the following time systems:'
      HLPMSG(  6 ) = ' '
      HLPMSG(  7 ) = '    # -- Universal Time Coordinated (ty'
     .//             'pes: #, #, #, #)'
      HLPMSG(  8 ) = '    # -- Ephemeris Time (types: #, #, #, #, #)'
      HLPMSG(  9 ) = '    # -- Spacecraft On-board Clock Time'
     .//             ' (types: #, #, #)'
      HLPMSG( 10 ) = '    # -- Local True Solar Time (types: #, #)'
      HLPMSG( 11 ) = ' '
      HLPMSG( 12 ) = 'CHRONOS takes inputs from the command line. '
     .//             'Run CHRONOS with the '
      HLPMSG( 13 ) = '"#" switch to display usage information.'
      HLPMSG( 14 ) = ' '
      HLPMSG( 15 ) = 'Although not required, CHRONOS allows '
     .//             'certain parameters to be '
      HLPMSG( 16 ) = 'provided via a setup file. Run CHRONOS '
     .//             'with the "#" switch '
      HLPMSG( 17 ) = 'to display setup file template. '
      HLPMSG( 18 ) = ' '
      CALL REPMC ( HLPMSG(  7 ), '#', UTCSYS, HLPMSG(  7 ) )
      CALL REPMC ( HLPMSG(  7 ), '#', SCTTYP, HLPMSG(  7 ) )
      CALL REPMC ( HLPMSG(  7 ), '#', ERTTYP, HLPMSG(  7 ) )
      CALL REPMC ( HLPMSG(  7 ), '#', ETTTYP, HLPMSG(  7 ) )
      CALL REPMC ( HLPMSG(  7 ), '#', LTTYP,  HLPMSG(  7 ) )
      CALL REPMC ( HLPMSG(  8 ), '#', ETSYS,  HLPMSG(  8 ) )
      CALL REPMC ( HLPMSG(  8 ), '#', SCTTYP, HLPMSG(  8 ) )
      CALL REPMC ( HLPMSG(  8 ), '#', ERTTYP, HLPMSG(  8 ) )
      CALL REPMC ( HLPMSG(  8 ), '#', ETTTYP, HLPMSG(  8 ) )
      CALL REPMC ( HLPMSG(  8 ), '#', LTTYP,  HLPMSG(  8 ) )
      CALL REPMC ( HLPMSG(  8 ), '#', SECTYP, HLPMSG(  8 ) )
      CALL REPMC ( HLPMSG(  9 ), '#', SCLSYS, HLPMSG(  9 ) )
      CALL REPMC ( HLPMSG(  9 ), '#', SCLTYP, HLPMSG(  9 ) )
      CALL REPMC ( HLPMSG(  9 ), '#', HEXTYP, HLPMSG(  9 ) )
      CALL REPMC ( HLPMSG(  9 ), '#', TIKTYP, HLPMSG(  9 ) )
      CALL REPMC ( HLPMSG( 10 ), '#', LSTSYS, HLPMSG( 10 ) )
      CALL REPMC ( HLPMSG( 10 ), '#', LSTTYP, HLPMSG( 10 ) )
      CALL REPMC ( HLPMSG( 10 ), '#', LSNTYP, HLPMSG( 10 ) )
      CALL REPMC ( HLPMSG( 13 ), '#', USGKEY, HLPMSG( 13 ) )
      CALL REPMC ( HLPMSG( 16 ), '#', TMLKEY, HLPMSG( 16 ) )

C
C     Template diplay
C
      TMLMSG(  1 ) = 'CHRONOS Setup File'
      TMLMSG(  2 ) = '---------------------------------------'
     .//             '---------------------------'
      TMLMSG(  3 ) = ' '
      TMLMSG(  4 ) = 'CHRONOS allows a few parameters to be '
     .//             'provided in a setup file.'
      TMLMSG(  5 ) = 'The setup file format should correspond'
     .//             ' to the SPICE Kernel Text'
      TMLMSG(  6 ) = 'file format specification, i.e. it must'
     .//             ' contain data formatted as'
      TMLMSG(  7 ) = 'a set of KEYWORD=VALUE assignments encl'
     .//             'osed between'
      TMLMSG(  8 ) = ' '
      TMLMSG(  9 ) = '   ' // CHAR(BSLASH) // 'begindata'
      TMLMSG( 10 ) = '   ' // CHAR(BSLASH) // 'begintext'
      TMLMSG( 11 ) = ' '
      TMLMSG( 12 ) = 'markers. Each assignment and marker mus'
     .//             't be on a line by itself.'
      TMLMSG( 13 ) = ' '
      TMLMSG( 14 ) = 'The following parameters may be provid'
     .//             'ed in a setup file:'
      TMLMSG( 15 ) = ' '
      TMLMSG( 16 ) = '   ' // CHAR(BSLASH) // 'begindata'
      TMLMSG( 17 ) = '      # = ''name of a LSK file'''
      TMLMSG( 18 ) = '      # = ''name of a SCLK file for the'
     .//             ' mission'''
      TMLMSG( 19 ) = '      # = ''name of a PCK file'''
      TMLMSG( 20 ) = '      # = ( ''name of '
     .//             'an SPK file'', ''...'' )'
      TMLMSG( 21 ) = '      # = ( ''name of a CK '
     .//             'file'', ''...'' )'
      TMLMSG( 22 ) = '      # = ''name of a frame definitions '
     .//             'file'''
      TMLMSG( 23 ) = '      # = NAIF ID for the spacecraft'
      TMLMSG( 24 ) = '      # = NAIF ID for the center body'
      TMLMSG( 25 ) = '      # = ''UTC time of the landing'''
      TMLMSG( 26 ) = '      # = SOL index of the landing'
      TMLMSG( 27 ) = '   ' // CHAR(BSLASH) // 'begintext'
      TMLMSG( 28 ) = ' '
      TMLMSG( 29 ) = 'Note that either or all of the #, #, #, '
      TMLMSG( 30 ) = 'and # parameters can also be provided '
     .//             'using the command'
      TMLMSG( 31 ) = 'line switches. If done so, the setup '
     .//             'file value corresponding to a command '
      TMLMSG( 32 ) = 'line value is not needed, and, if present, is '
     .//             'ignored by the program.'
      TMLMSG( 33 ) = ' '
      TMLMSG( 34 ) = 'Similarly, the kernels files to be loaded '
     .//             'can be provided using the '
      TMLMSG( 35 ) = 'standard SPICE interface -- with the '
     .//             'KERNELS_TO_LOAD parameter:'
      TMLMSG( 36 ) = ' '
      TMLMSG( 37 ) = '   ' // CHAR(BSLASH) // 'begindata'
      TMLMSG( 38 ) = '      KERNELS_TO_LOAD = ( '
      TMLMSG( 39 ) = '                    ''name of a LSK file'','
      TMLMSG( 40 ) = '                    ''name of a SCLK file '','
      TMLMSG( 41 ) = '                    ''name of a PCK file'','
      TMLMSG( 42 ) = '                    ''name of an SPK file'','
      TMLMSG( 43 ) = '                    ''...'','
      TMLMSG( 44 ) = '                    ''name of a CK file'','
      TMLMSG( 45 ) = '                    ''...'','
      TMLMSG( 46 ) = '                    ''name of an FK file'''
      TMLMSG( 47 ) = '                        )'
      TMLMSG( 48 ) = '   ' // CHAR(BSLASH) // 'begintext'
      TMLMSG( 49 ) = ' '
      TMLMSG( 50 ) = 'or even by simply listing them after the '
     .//             '# command line switch. In'
      TMLMSG( 51 ) = 'either of these two cases, '
     .//             'specifying the #, #, '
      TMLMSG( 52 ) = '#, #, #, and # setup file parameters '      
      TMLMSG( 53 ) = 'is not necessary.'      
      TMLMSG( 54 ) = ' '


      CALL REPMC ( TMLMSG( 17 ), '#', LSKKWD, TMLMSG( 17 ) )
      CALL REPMC ( TMLMSG( 18 ), '#', SCLKWD, TMLMSG( 18 ) )
      CALL REPMC ( TMLMSG( 19 ), '#', PCKKWD, TMLMSG( 19 ) )
      CALL REPMC ( TMLMSG( 20 ), '#', SPKKWD, TMLMSG( 20 ) )
      CALL REPMC ( TMLMSG( 21 ), '#', CKKWD,  TMLMSG( 21 ) )
      CALL REPMC ( TMLMSG( 22 ), '#', FRAKWD, TMLMSG( 22 ) )
      CALL REPMC ( TMLMSG( 23 ), '#', SIDKWD, TMLMSG( 23 ) )
      CALL REPMC ( TMLMSG( 24 ), '#', BODKWD, TMLMSG( 24 ) )
      CALL REPMC ( TMLMSG( 25 ), '#', LSTKWD, TMLMSG( 25 ) )
      CALL REPMC ( TMLMSG( 26 ), '#', SOLKWD, TMLMSG( 26 ) )

      CALL REPMC ( TMLMSG( 29 ), '#', SIDKWD, TMLMSG( 29 ) )
      CALL REPMC ( TMLMSG( 29 ), '#', BODKWD, TMLMSG( 29 ) )
      CALL REPMC ( TMLMSG( 29 ), '#', LSTKWD, TMLMSG( 29 ) )
      CALL REPMC ( TMLMSG( 30 ), '#', SOLKWD, TMLMSG( 30 ) )

      CALL REPMC ( TMLMSG( 50 ), '#', STPKEY, TMLMSG( 50 ) )

      CALL REPMC ( TMLMSG( 51 ), '#', LSKKWD, TMLMSG( 51 ) )
      CALL REPMC ( TMLMSG( 51 ), '#', SCLKWD, TMLMSG( 51 ) )
      CALL REPMC ( TMLMSG( 52 ), '#', PCKKWD, TMLMSG( 52 ) )
      CALL REPMC ( TMLMSG( 52 ), '#', SPKKWD, TMLMSG( 52 ) )
      CALL REPMC ( TMLMSG( 52 ), '#', CKKWD,  TMLMSG( 52 ) )
      CALL REPMC ( TMLMSG( 52 ), '#', FRAKWD, TMLMSG( 52 ) )

C
C     Get STDOUT
C
      CALL STDIO ( 'STDOUT', STDOUT )

C
C     What was requested for display?
C
      IF       ( WHAT .EQ. 'VERSION'  ) THEN
 
C
C        Display version.
C
         CALL WRITLA ( VERCNT, VERMSG, STDOUT )

      ELSE IF  ( WHAT .EQ. 'USAGE' ) THEN
     
C
C        Display usage.
C
         CALL WRITLA ( VERCNT, VERMSG, STDOUT )
         CALL WRITLA ( USGCNT, USGMSG, STDOUT )

      ELSE IF  ( WHAT .EQ. 'HELP' ) THEN
     
C
C        Display help.
C
         CALL WRITLA ( VERCNT, VERMSG, STDOUT )
         CALL WRITLA ( HLPCNT, HLPMSG, STDOUT )
         
      ELSE IF  ( WHAT .EQ. 'TEMPLATE' ) THEN
     
C
C        Display help.
C      
         CALL WRITLA ( VERCNT, VERMSG, STDOUT )
         CALL WRITLA ( TMLCNT, TMLMSG, STDOUT )
            
      END IF
      
C
C     Should we stop the program?
C
      IF ( ACTION .EQ. 'STOP' ) THEN
         STOP
      END IF

      CALL CHKOUT ( 'DSPLAY' )
      RETURN      
      END
