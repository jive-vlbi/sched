C$Procedure      CRONOS ( SPICE Time Conversion Umbrella )

      SUBROUTINE CRONOS ( CMDLIN, NTIMES, INPTIM, OUTTIM )

C$ Abstract
C
C     Convert time between various time systems and types supported by
C     lower level SPICE time conversion subsystems.
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
C     CHRONOS User's Guide
C     TIME
C     SCLK
C
C$ Keywords
C
C     TIME
C     SCLK
C     CONVERSION
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'chronos.inc'

      CHARACTER*(*)         CMDLIN
      INTEGER               NTIMES
      CHARACTER*(*)         INPTIM (*)
      CHARACTER*(*)         OUTTIM (*)

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CMDLIN     I   Command line.
C     NTIMES     I   Number of input/output times.
C     INPTIM     I   Array of input times.
C     OUTTIM     O   Array of output times.
C
C$ Detailed_Input
C
C     CMDLIN     is the command line. See Particulars section for the
C                command line syntax details.
C
C     NTIMES     is the number of input/output times. Must be greater
C                than zero.
C
C     INPTIM     is an array of input times. All elements of this array
C                must be legitimate time tags for the input time
C                system/type as defined by the CMDLIN argument. No blank
C                elements are permitted.
C
C$ Detailed_Output
C
C     OUTTIM     is an array of output times.
C
C$ Parameters
C
C     Essential parameters defined in the include file:
C
C     LLNSIZ     is the maximum number of characters from the input 
C                argument CMDLIN that will be accepted by this routine.
C                All characters past LLNSIZ are truncated.
C
C     LINSIZ     is the maximum number of characters that is taken
C                from each element of the INPTIM and is assigned to 
C                each element of the OUTTIM.
C
C$ Exceptions
C
C     1) TBD
C
C$ Files
C
C     Enough SPICE data to do the requested conversion should be loaded
C     prior to calling this routine.
C
C     An LSK file should be loaded to support any time conversion.
C
C     An SCLK file for the specified spacecraft should be loaded to
C     support any time conversion involving SCLK time system.
C
C     A set of SPK, PCK, and possibly CK and FK files sufficient to
C     compute the specified spacecraft position with respect the
C     specified central body and the Sun should be loaded to support
C     any conversions involving LSK time system.
C
C     A set of SPK, PCK, and possibly CK and FK files sufficient to
C     compute the specified spacecraft position with respect the Earth
C     should be loaded to support any conversions involving ERT, ETT,
C     and LT time types.
C
C$ Particulars
C
C     This section of the header provides a brief overview of the
C     routine's interface. Refer to the CHRONOS User's Guide for the
C     complete specification
C
C     The CMDLIN input argument, specifying the time systems/types from
C     which and to which the conversion should be done along with a set
C     of the other parameters needed to perform the converison,
C     incorporates these specification in a single line, similar to a
C     command line for an executable. The command line has the following
C     syntax:
C
C        'switch <value> switch <value> switch <value> ...'
C
C     The following command line swtches are recognized and used by 
C     the routine:
C
C        switch        optional?  value
C        ------------  ---------  --------------------------------
C        -FROM           no       "from" time system
C        -FROMTYPE       yes      "from" time type
C        -TO             no       "to" time system
C        -TOTYPE         yes      "to" time type
C        -FORMAT         yes      output time format picture
C        -SC             yes(*)   NAIF ID of the spacecraft
C        -CENTER         yes(*)   NAIF ID of the cental body
C        -LANDINGTIME    yes(*)   UTC time of the landing
C        -SOL1INDEX      yes(*)   index of the first SOL
C
C     (*) these inputs are required for some input/output time systems/
C         types and aren't required for the others.
C
C     The following command line switches are recongnized but ignored 
C     by the routine:
C
C        switch   
C        ---------
C        -SETUP
C        -TIME
C        -BATCH
C        -NOLABEL
C        -TRACE
C        -HELP
C        -H
C        -USAGE
C        -U
C        -TEMPLATE
C
C     The recognition of these switches is implemented to provide
C     compatibilty between this routine's and the CHRONOS utility
C     program interfaces, such that a command line setup for the
C     CHRONOS program could be used 'as is' with this routine.
C
C     Any other tokens starting with '-' are not recognized and
C     interpreted as part of the value of the preceding command line
C     switch.
C 
C     The case of command line switches is insignificant.
C
C     This routine supports converison between the following time
C     systems, that can be specified after the -FROM and -TO command
C     line switches:
C
C        System   Description
C        ------   ---------------------------------
C        UTC      Universal Time Coordinated
C        ET       Ephemeris Time
C        SCLK     Spacecraft On-board Clock Time
C        LST      Local True Solar Time
C
C     For each of them it suppports the following input types
C     (specified with the -FROMTYPE switch):
C
C        System   Input Types
C        ------   ---------------------------------
C        UTC      SCET, ERT, ETT 
C        ET       SCET, ERT, ETT, SECONDS
C        SCLK     SCLK, HEX, TICKS
C        LST      LST
C
C     and the following output types (specified with the -TOTYPE
C     switch):
C      
C        System   Input Types
C        ------   ---------------------------------
C        UTC      SCET, ERT, ETT, LT
C        ET       SCET, ERT, ETT, SECONDS, LT
C        SCLK     SCLK, HEX, TICKS
C        LST      LST, LSUN
C
C     where the type identifiers correspond to: 
C
C        Type     Description
C        ------   ---------------------------------
C        SCET     Spacecraft Event Time
C        ERT      Earth Received Time
C        ETT      Earth Transmit Time   
C        LT       One-way Light Time
C        SECONDS  Ephemeris Seconds past epoch J2000
C        SCLK     String Spacecraft Clock
C        HEX      HEX Spacecraft Clock
C        TICKS    Spacecraft Clock Ticks (or encoded SPICE SCLK)
C        LST      Local Solar Time
C        LSUN     Longitude of the Sun
C
C     The input and output type specifications are optional. If either
C     of of the types is absent or set to blank, the default value
C     specified in this table is assumed:
C
C        System   Default Input Type   Default Output Type
C        ------   ------------------   -------------------
C        UTC      SCET                 SCET
C        ET       SCET                 SCET
C        SCLK     SCLK                 SCLK
C        LST      LST                  LST
C
C     The -FORMAT command line switch is used to modify, if applicable,
C     the format of the output time tags. This switch is optional and
C     is applicable only for the following output time system/type
C     combinations:
C
C        System   Type      Default Format
C        ------   -------   --------------------------------
C        UTC      SCET      YYYY-MM-DD HR:MN:SC.### ::RND
C        UTC      ERT       YYYY-MM-DD HR:MN:SC.### ::RND
C        UTC      ETT       YYYY-MM-DD HR:MN:SC.### ::RND
C        UTC      LT        xxxxxxxxxxxx.xxx
C        ET       SCET      YYYY-MM-DD, HR:MN:SC.### ::TDB ::RND
C        ET       ERT       YYYY-MM-DD, HR:MN:SC.### ::TDB ::RND
C        ET       ETT       YYYY-MM-DD, HR:MN:SC.### ::TDB ::RND
C        ET       LT        xxxxxxxxxxxx.xxx
C        ET       SECONDS   xxxxxxxxxxxxxxx.xxx
C        SCLK     TICKS     xxxxxxxxxxxxxxxx       
C        LST      LSUN      xxxxxx.xxx           
C        
C     As with the types, if this switch is not specified or its value
C     is blank, then the default value, given in the right column of
C     the table above, is used.
C
C     The spacecraft ID, specified with the -SC command line switch, is
C     required if the input, output, or both time system is SCLK and if
C     the input, output, or both time type is LST, ERT, ETT, or LT.
C
C     The central body ID, specified with the -CENTER command line
C     switch, is required if the input, output, or both time system is
C     LST.
C
C     The UTC time of the landing, specified with the -LANDINGTIME
C     switch, and the index of the first SOL, specified with the
C     -SOL1INDEX switch, are required if the input or output
C     system/type pair is LST/LST.
C
C$ Examples
C
C     These examples assume that the complete set of kernel files
C     required to support time conversions for MER-A at the landing
C     site EP55A2 has been loaded prior to calling CRONOS routine:
C
C     1) Converting from UTC to ET (note it's sufficient to specify 
C        only -from and -to command line options for this conversion):
C
C           CMDLIN    = '-from UTC -to ET'
C           INTIME(1) = '2004-01-05 01:55:46.175'
C           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME )
C
C        returned value of OUTIME is:
C
C           '2004-01-05, 01:56:50.359'
C
C     2) Converting from SCLK to UTC (note that the s/c ID must be 
C        specified for this conversion):
C     
C           CMDLIN    = '-from SCLK -to UTC -sc -253'
C           INTIME(1) = '0126539810.092'
C           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME )
C     
C        returned value of OUTIME is:
C
C           '2004-01-05 01:55:46.175'
C
C     3) Converting from LST to UTC (note that the IDs for the s/c 
C        and Mars and landing time and first SOL index must be 
C        specified for this conversion as for any other conversion
C        involving LST):
C
C           CMDLIN    = '-from LST '
C          .//          '-to UTC '
C          .//          '-sc -253 '
C          .//          '-center 499 '
C          .//          '-landingtime 2004-01-04 12:00 '
C          .//          '-sol1index 1 '
C           INTIME(1) = 'SOL 2 11:11:11'
C           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME )
C     
C        returned value of OUTIME is:
C
C           '2004-01-05 01:55:46.175'
C
C     4) Computing one-way light time for a given UTC epoch (note
C        that one-way light time expressed in seconds is returned 
C        as a string):
C
C           CMDLIN    = '-from UTC -to UTC -totype LT -sc -253'
C           INTIME(1) = '2004-01-05 01:55:46.175'
C           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME )
C     
C        returned value of OUTIME is:
C
C           '572.658'
C
C     5) Computing Earth Receive Time (ERT) in Pacific Standard 
C        Time (PST) for a given LST (note that the format string 
C        modified ::UTC-8 is used to tell the routine to output 
C        time as PST):
C
C           CMDLIN    = '-from LST '
C          .//          '-to UTC '
C          .//          '-totype ERT '
C          .//          '-sc -253 '
C          .//          '-center 499 '
C          .//          '-landingtime 2004-01-04 12:00 '
C          .//          '-sol1index 1 '
C          .//          '-format YYYY-MM-DD HR:MN:SC.### ::UTC-8 PST '
C           INTIME(1) = 'SOL 2 11:11:11'
C           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME )
C
C        returned value of OUTIME is:
C
C           '2004-01-04 18:05:18.833 PST'
C
C     6) Re-using CHRONOS executable command line (note that the 
C        command line switches -setup, -time, -nolabel and the values
C        that follows them are ignored by the routine):
C
C           CMDLIN    = '-setup chronos.setup '
C          .//          '-from UTC '
C          .//          '-to ET '
C          .//          '-time 2004-01-05 01:55:46.175 '
C          .//          '-nolabel '
C           INTIME(1) = '-time 2004-01-05 01:55:46.175'
C           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME )
C
C        returned value of OUTIME is:
C
C           '2004-01-05, 01:56:50.359'
C
C$ Restrictions
C
C     TBD. Enough SPICE data to do the conversion should be loaded
C     prior to calling this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 28-OCT-2011 (BVS)
C
C        Moved PARCML to support. Updated its calling sequence.
C
C-    SPICELIB Version 1.2.0, 18-AUG-2006 (BVS)
C
C        Fixed the bug that caused incorrect computation of the orbital
C        period used in computing the local second length (the orbital
C        period formula had e**2 instead of e).
C
C-    SPICELIB Version 1.1.0, 07-OCT-2005 (BVS)
C
C        Updated to remove non-standard use of duplicate arguments
C        in subroutine calls (LSTMID and LTIME). Replaced BODVAR with
C        BODVRD.
C
C-    SPICELIB Version 1.0.1, 17-MAR-2004 (BVS)
C
C        Planet ID is now computed from barycenter ID returned by
C        ZZBODBRY rather than one determined by division by a hundred.
C
C-    SPICELIB Version 1.0.0, 13-DEC-2001 (BVS)
C
C        The guts are from CHRONOS Ver 1.2.3 (25-NOV-2001)
C
C-&

C$ Index_Entries
C
C     convert between ET, UTC, SCLK and LST times
C
C-&

C
C     SPICELIB functions.
C
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI
      INTEGER               ISRCHC
      INTEGER               POS
      INTEGER               RTRIM
      LOGICAL               RETURN
      DOUBLE PRECISION      LS
      DOUBLE PRECISION      DPR
      INTEGER               ZZBODBRY

      EXTERNAL              LTIME

C
C     Parameters.
C
      INTEGER               MAXFLD
      PARAMETER           ( MAXFLD = 10 )

      CHARACTER*(*)         SCDLMS
      PARAMETER           ( SCDLMS = ' -.,:' )

      CHARACTER*(*)         LSDLMS
      PARAMETER           ( LSDLMS = ' :' )

C
C     Variables.
C
      CHARACTER*(LLNSIZ)    CMDSAV
      CHARACTER*(LLNSIZ)    CMDTMP
      CHARACTER*(LLNSIZ)    UNPRSD
      CHARACTER*(5)         TYPE
      CHARACTER*(8)         MODIFY ( 5 )
      CHARACTER*(LINSIZ)    CLVALS ( MAXKEY )
      CHARACTER*(LINSIZ)    ERROR
      CHARACTER*(LINSIZ)    HLINE
      CHARACTER*(LINSIZ)    INTIME
      CHARACTER*(LINSIZ)    LINE
      CHARACTER*(LINSIZ)    OUTIME
      CHARACTER*(LINSIZ)    TOFRMT
      CHARACTER*(LINSIZ)    TOFRMH
      CHARACTER*(WRDSIZ)    CLKEYS ( MAXKEY )
      CHARACTER*(WRDSIZ)    CURKEY
      CHARACTER*(WRDSIZ)    DEFTYP ( MAXSYS )
      CHARACTER*(WRDSIZ)    FIELDS ( MAXFLD )
      CHARACTER*(WRDSIZ)    FROMTS
      CHARACTER*(WRDSIZ)    FROMTT
      CHARACTER*(WRDSIZ)    HWD
      CHARACTER*(WRDSIZ)    HWORD
      CHARACTER*(WRDSIZ)    LSTTIM
      CHARACTER*(WRDSIZ)    SYSTMS ( MAXSYS )
      CHARACTER*(WRDSIZ)    TOTS
      CHARACTER*(WRDSIZ)    TOTT
      CHARACTER*(WRDSIZ)    SCSTR
      CHARACTER*(WRDSIZ)    CENSTR
      CHARACTER*(WRDSIZ)    SOLIDS
      CHARACTER*(WRDSIZ)    TYPES  ( MAXTYP )
      CHARACTER*(WRDSIZ*2)  FMTPIC ( MAXSYS, MAXTYP )

      DOUBLE PRECISION      ELTS   ( 8 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETOUT
      DOUBLE PRECISION      ETTEMP
      DOUBLE PRECISION      HDP
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LSECS
      DOUBLE PRECISION      LSTET
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MIDNET
      DOUBLE PRECISION      PMMOTN
      DOUBLE PRECISION      R
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      SCRATE
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      SUNGM
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TVEC   ( 10 )

      INTEGER               COUNT
      INTEGER               BODYID
      INTEGER               FRSIDX
      INTEGER               FRTIDX
      INTEGER               HR
      INTEGER               I
      INTEGER               J
      INTEGER               MN
      INTEGER               N
      INTEGER               NTVEC
      INTEGER               SC
      INTEGER               SCID
      INTEGER               SOLDAY
      INTEGER               SOLZER
      INTEGER               TOSIDX
      INTEGER               TOTIDX
      INTEGER               FRCODE
      INTEGER               NLOOPS

      LOGICAL               BAD
      LOGICAL               CLFLAG ( MAXKEY )
      LOGICAL               FMTTED ( MAXSYS, MAXTYP )
      LOGICAL               FOUND
      LOGICAL               MODS
      LOGICAL               OK
      LOGICAL               SYSTYP ( MAXSYS, MAXTYP )
      LOGICAL               YABBRV
      LOGICAL               FIRST

C
C     Save all variables.
C
      SAVE

C
C     Number of iteration that have been done with each command line is
C     initially set to zero.
C
      DATA  NLOOPS / 0 /

C
C     First call flag.
C
      DATA  FIRST / .TRUE. /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CRONOS' )
      END IF

C
C     The very first thing to check is whether the count of input
C     times make sense?
C
      IF ( NTIMES .LE. 0 ) THEN
         CALL SETMSG ( 'The count of input times was less or '      //
     .                 'equal to zero. This is a NO-NO.'            )
         CALL SIGERR ( 'SPICE(BADTIMECOUNT)'                        )
         CALL CHKOUT ( 'CRONOS'                                     )
         RETURN
      END IF

C
C     If the command line haven't changed from the previous call, there
C     is no need to parse it.
C
      IF ( CMDLIN .NE. CMDSAV ) THEN

C
C        If this is the first call, get parameter arrays -- time
C        systems and types, format pictures and flags, and command line
C        keys.
C
         IF ( FIRST ) THEN
            CALL CRCNST ( SYSTMS, TYPES,  DEFTYP, SYSTYP, FMTTED,
     .                                            FMTPIC, CLKEYS )
            FIRST = .FALSE.
         END IF

C
C        Re-assign command line to a temporary variable and parse it.
C
         CMDTMP = CMDLIN
         CALL PARCML( CMDTMP, MAXKEY, CLKEYS, CLFLAG, CLVALS, FOUND,
     .                UNPRSD )

C
C        Are there any keys on the command line? If not, complain and
C        exit.
C
         IF ( .NOT. FOUND ) THEN
            CALL SETMSG ( 'No recognized tokens were found in the ' //
     .                    'command line argument passed into this ' //
     .                    'routine.'                                )
            CALL SIGERR ( 'SPICE(BLANKCOMMANDLINE)'                 )
            CALL CHKOUT ( 'CRONOS'                                  )
            RETURN
         END IF

C
C        Process command line. First, set all values to blanks.
C
         FROMTS = ' '
         FROMTT = ' '
         TOTS   = ' '
         TOTT   = ' '
         TOFRMT = ' '
         SCSTR  = ' '
         CENSTR = ' '
         LSTTIM = ' '
         SOLIDS = ' '

         DO I = 1, MAXKEY

            IF ( CLFLAG( I ) ) THEN

               CURKEY = CLKEYS(I)

               IF      ( CURKEY .EQ. FRMKEY ) THEN
C
C                 It's "from" time system.
C
                  FROMTS = CLVALS( I )
                  CALL UCASE( FROMTS, FROMTS )

               ELSE IF ( CURKEY .EQ. FRTKEY ) THEN
C
C                 It's "from" time type.
C
                  FROMTT = CLVALS( I )
                  CALL UCASE( FROMTT, FROMTT )

               ELSE IF ( CURKEY .EQ. TOKEY  ) THEN
C
C                 It's "to" time system.
C
                  TOTS = CLVALS( I )
                  CALL UCASE( TOTS, TOTS )

               ELSE IF ( CURKEY .EQ. TOTKEY ) THEN
C
C                 It's "to" time type.
C
                  TOTT = CLVALS( I )
                  CALL UCASE( TOTT, TOTT )

               ELSE IF ( CURKEY .EQ. FMTKEY ) THEN
C
C                 It's "to" time format.
C
                  TOFRMT = CLVALS( I )

               ELSE IF ( CURKEY .EQ. SIDKEY ) THEN
C
C                 It's spacecraft ID; save it in string.
C
                  SCSTR = CLVALS( I )

               ELSE IF ( CURKEY .EQ. BODKEY ) THEN
C
C                 It's body ID; save it in string.
C
                  CENSTR = CLVALS( I )

               ELSE IF ( CURKEY .EQ. LSTKEY ) THEN
C
C                 It's landing time.
C
                  LSTTIM = CLVALS( I )

               ELSE IF ( CURKEY .EQ. SOLKEY ) THEN
C
C                 It's SOL 1 index; save it in string.
C
                  SOLIDS = CLVALS( I )

               END IF

            END IF

         END DO

C
C        Done with parsing. Now, check whether the stuff that we have
C        got makes any sense. We start with systems, types, format and
C        input time. "FROM" time system goes first.
C
         FRSIDX = ISRCHC ( FROMTS, MAXSYS, SYSTMS )
         IF ( FROMTS .NE. ' ' ) THEN
            IF ( FRSIDX .EQ. 0 ) THEN
               CALL SETMSG ( 'Time system ''#'' from which the time'//
     .                       ' must be converted is not one of the '//
     .                       'supported systems.'                   )
               CALL ERRCH  ( '#', FROMTS                            )
               CALL SIGERR ( 'SPICE(BADFROMTIMESYSTEM)'             )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF
         ELSE
            CALL SETMSG ( 'Time system from which the time must '   //
     .                    'be converted wasn''t specified on the '  //
     .                    'command line using # switch.'            )
            CALL ERRCH  ( '#', FRMKEY                               )
            CALL SIGERR ( 'SPICE(NOFROMTIMESYSTEM)'                 )
            CALL CHKOUT ( 'CRONOS'                                  )
            RETURN
         END IF

C
C        "TO" time system goes second.
C
         TOSIDX = ISRCHC ( TOTS, MAXSYS, SYSTMS )
         IF ( TOTS .NE. ' ' ) THEN
            IF ( TOSIDX .EQ. 0 ) THEN
               CALL SETMSG ( 'Time system ''#'' to which the time ' //
     .                       'must be converted is not one of the ' //
     .                       'supported systems.'                   )
               CALL ERRCH  ( '#', TOTS                              )
               CALL SIGERR ( 'SPICE(BADTOTIMESYSTEM)'               )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF
         ELSE
            CALL SETMSG ( 'Time system to which the time must '     //
     .                    'be converted wasn''t specified on the '  //
     .                    'command line using # switch.'            )
            CALL ERRCH  ( '#', TOKEY                                )
            CALL SIGERR ( 'SPICE(NOTOTIMESYSTEM)'                   )
            CALL CHKOUT ( 'CRONOS'                                  )
            RETURN
         END IF

C
C        "FROM" time type goes third.
C
         FRTIDX = ISRCHC ( FROMTT, MAXTYP, TYPES )
         IF ( FRTIDX .NE. 0 ) THEN
            IF ( .NOT. SYSTYP( FRSIDX, FRTIDX  ) ) THEN
               CALL SETMSG ( 'Time type ''#'' is not applicable '   //
     .                       'for time system ''#'' from which '    //
     .                       'the time must be converted.'          )
               CALL ERRCH  ( '#', FROMTT                            )
               CALL ERRCH  ( '#', FROMTS                            )
               CALL SIGERR ( 'SPICE(MISMATCHFROMTIMETYPE)'          )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF
         ELSE
            IF ( FROMTT .NE. ' ' ) THEN
               CALL SETMSG ( 'Time type ''#'' from which the time ' //
     .                       'must be converted is not one of the ' //
     .                       'supported types.'                     )
               CALL ERRCH  ( '#', FROMTT                            )
               CALL SIGERR ( 'SPICE(BADFROMTIMETYPE)'               )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            ELSE
C
C              Set "from" type to default type for that system if
C              the value that we have got is blank.
C
               FROMTT = DEFTYP( FRSIDX )
               FRTIDX = ISRCHC ( FROMTT, MAXTYP, TYPES )
            END IF
         END IF

C
C        "TO" time type goes fourth.
C
         TOTIDX = ISRCHC ( TOTT, MAXTYP, TYPES )
         IF ( TOTIDX .NE. 0 ) THEN
            IF ( .NOT. SYSTYP( TOSIDX, TOTIDX  ) ) THEN
               CALL SETMSG ( 'Time type ''#'' is not applicable '   //
     .                       'for time system ''#'' to which '      //
     .                       'the time must be converted.'          )
               CALL ERRCH  ( '#', TOTT                              )
               CALL ERRCH  ( '#', TOTS                              )
               CALL SIGERR ( 'SPICE(MISMATCHTOTIMETYPE)'            )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF
         ELSE
            IF ( TOTT .NE. ' ' ) THEN
               CALL SETMSG ( 'Time type ''#'' to which the time '   //
     .                       'must be converted is not one of the ' //
     .                       'supported types.'                     )
               CALL ERRCH  ( '#', TOTT                              )
               CALL SIGERR ( 'SPICE(BADTOTIMETYPE)'                 )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            ELSE
C
C              Set "to" type to default type for that system if
C              the value that we have got is blank.
C
               TOTT = DEFTYP( TOSIDX )
               TOTIDX = ISRCHC ( TOTT, MAXTYP, TYPES )
            END IF
         END IF

C
C        Output format string goes next.
C
         IF ( TOFRMT .NE. ' ' ) THEN

C
C           Is formating applicable to time system/type?
C
            IF ( FMTTED( TOSIDX, TOTIDX  ) ) THEN

C
C              For UTC and ET we may need to do some check on the
C              formats. for other systems we will just leave blank
C              cases, maybe check will be added later.
C
               IF ( TOTS .EQ. UTCSYS .OR. TOTS .EQ. ETSYS ) THEN

C
C                 We run TPICTR and use Bill logic -- if it fails, then
C                 provided string is a format picture already, if it
C                 succeeds -- then input was a times string and we have
C                 our picture as the output :).
C
                  CALL TPICTR ( TOFRMT, HLINE, OK, ERROR )

                  IF ( OK ) THEN
                     TOFRMT = HLINE
                  END IF

               ELSE IF ( TOTS .EQ. SCLSYS ) THEN
C
C                 No checks for now.
C

               ELSE IF ( TOTS .EQ. LSTSYS ) THEN
C
C                 No checks for now.
C

               END IF

            ELSE

C
C              Formatting is not applicable to this system/type.
C
               CALL SETMSG ( 'Specification of a format for the  '  //
     .                       'system ''#''/type ''#'' to which the '//
     .                       'time must be converted is not '       //
     .                       'supported.'                           )
               CALL ERRCH  ( '#', TOTS                              )
               CALL ERRCH  ( '#', TOTT                              )
               CALL SIGERR ( 'SPICE(FORMATNOTAPPLICABLE)'           )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN

            END IF

         ELSE

C
C           Format has not been provided. Use default.
C
            TOFRMT = FMTPIC( TOSIDX, TOTIDX )

         END IF

C
C        Process the s/c ID. It is required for all types in the SCLK
C        time system, LST type in the LST time system, and for ERT, ETT
C        and LT types in the UTC and ET systems.
C
         IF ( FROMTS .EQ. SCLSYS .OR. TOTS .EQ. SCLSYS .OR.
     .        FROMTS .EQ. LSTTYP .OR. TOTS .EQ. LSTTYP .OR.
     .        FROMTT .EQ. ERTTYP .OR. TOTT .EQ. ERTTYP .OR.
     .        FROMTT .EQ. ETTTYP .OR. TOTT .EQ. ETTTYP .OR.
     .        FROMTT .EQ. LTTYP  .OR. TOTT .EQ. LTTYP      ) THEN


C
C           Check that the s/c ID has been provided.
C
            IF ( SCSTR .EQ. ' ' ) THEN
               CALL SETMSG ( 'S/c ID required to do conversion '    //
     .                       'between specified time systems and '  //
     .                       'types has not been provided after '   //
     .                       '# switch.'                            )
               CALL ERRCH  ( '#', SIDKEY                            )
               CALL SIGERR ( 'SPICE(NOSPACECRAFTID)'                )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

C
C           Check that s/c ID is an integer.
C
            CALL NPARSI ( SCSTR, SCID, ERROR, N )
            IF ( N .NE. 0 ) THEN
               CALL SETMSG ( 'NAIF ID for the spacecraft, ''#'', ' //
     .                       'is not an integer number.'            )
               CALL ERRCH  ( '#', SCSTR                             )
               CALL SIGERR ( 'SPICE(BADSCID)'                       )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

         END IF

C
C        Process the center body ID. It is required only for  LST
C        time system.
C
         IF ( FROMTS .EQ. LSTTYP .OR. TOTS .EQ. LSTTYP .OR.
     .        FROMTT .EQ. LSNTYP .OR. TOTT .EQ. LSNTYP     ) THEN

C
C           Check that the body ID has been provided.
C
            IF ( CENSTR .EQ. ' ' ) THEN
               CALL SETMSG ( 'Body ID required to do conversion '   //
     .                       'between specified time systems and '  //
     .                       'types has not been provided after '   //
     .                       '# switch.'                            )
               CALL ERRCH  ( '#', BODKEY                            )
               CALL SIGERR ( 'SPICE(NOSPACECRAFTID)'                )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

C
C           Check that center body ID is an integer.
C
            CALL NPARSI ( CENSTR, BODYID, ERROR, N )
            IF ( N .NE. 0 ) THEN
               CALL SETMSG ( 'NAIF ID for the center body, ''#'', ' //
     .                       'is not an integer number.'            )
               CALL ERRCH  ( '#', CENSTR                            )
               CALL SIGERR ( 'SPICE(BADBODYID1)'                    )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

C
C           Check that center body ID is for a planet or a satellite.
C
            IF ( BODYID .LE. 0 .OR. BODYID .GE. 1000 ) THEN
               CALL SETMSG ( 'Center body must be planet or '       //
     .                       'satellite. Provided ID ''#''  '       //
     .                       'designates neither of these.'         )
               CALL ERRINT ( '#', BODYID                            )
               CALL SIGERR ( 'SPICE(BADBODYID2)'                    )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

         END IF

C
C        Check whether SOL index, if provided, is an integer.
C
         IF ( SOLIDS .NE. ' ' ) THEN

            CALL NPARSI ( SOLIDS, SOLZER, ERROR, N )
            IF ( N .NE. 0 ) THEN
               CALL SETMSG ( 'First SOL index, ''#'', is '          //
     .                       'not an integer number.'               )
               CALL ERRCH  ( '#', CENSTR                            )
               CALL SIGERR ( 'SPICE(BADSOLINDEX)'                   )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

         ELSE
            SOLZER = 1
         END IF

C
C        Parsing was successfully completed; re-assign saved command
C        line.
C
         CMDSAV = CMDLIN

C
C        Reset the number of iteration that have been done with this
C        command line
C
         NLOOPS = 0

      END IF

C
C     For LST time we need compute UTC for the first local midnight and
C     the duration of the average local second. Unfortunately, this
C     part relies on the actual loaded data (from all SPICE
C     subsystems), and, therefore, it's hard to determine whether it
C     had to be re-computed even if the command line didn't change.
C
      IF ( FROMTS .EQ. LSTTYP .OR. TOTS .EQ. LSTTYP ) THEN

         IF ( LSTTIM .NE. ' ' ) THEN

C
C           Landing time was provided. So we can compute SOL days and
C           for that we need to get our first midnight time. First, we
C           convert given time to ET.
C
            CALL STR2ET( LSTTIM, LSTET )
            MIDNET = LSTET

C
C           Second we compute longitude of the s/c in the body-fixed
C           rotating frame at the landing time.
C
            CALL CIDFRM( BODYID, FRCODE, HLINE, FOUND )
            IF ( .NOT. FOUND ) THEN
               CALL SETMSG ( 'Cannot determine body-fixed frame '   //
     .                       'name for the body with ID #.'         )
               CALL ERRINT ( '#', BODYID                            )
               CALL SIGERR ( 'SPICE(FRAMENOTFOUND)'                 )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

            CALL SPKEZ( SCID, MIDNET, HLINE, 'NONE', BODYID,
     .                                                     STATE, LT )
            CALL RECLAT( STATE, R, LON, LAT )

C
C           Now we need compute average number of ET seconds in one
C           body second.
C
            HWORD = 'BODY#_PM'
            CALL REPMI ( HWORD, '#', BODYID, HWORD )
            CALL GDPOOL( HWORD, 2, 1, N, PMMOTN, FOUND )

            IF ( PMMOTN .EQ. 0.D0 ) THEN
               CALL SETMSG ( 'Body # prime meridian motion, '       //
     .                       'specified in the PCK keyword #, '     //
     .                       'is zero, which doesn''t make any '    //
     .                       'sense.'                               )
               CALL ERRINT ( '#', BODYID                            )
               CALL ERRCH  ( '#', HLINE                             )
               CALL SIGERR ( 'SPICE(BADPCKVALUE)'                   )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

C
C           If our body is satellite, we compute period of orbit around
C           Sun for the planet which this satellite is orbiting.
C
            I = ZZBODBRY( BODYID )

            IF ( I .GE. 1 .AND. I .LE. 9 ) THEN
               I = I * 100 + 99
            ELSE
               I = BODYID
            END IF

C
C           To compute period of the orbit around the Sun, we need Sun
C           GM.
C
            CALL DTPOOL( 'BODY10_GM', FOUND, N, HWD )

            IF (  .NOT. FOUND             .OR.
     .                  N .LT. 1          .OR.
     .                  HWD(1:1) .NE. 'N'      ) THEN
               CALL SETMSG ( 'GM of the Sun required to compute '   //
     .                       'heliocentric orbit period for '       //
     .                       'the body with NAIF ID # wasn''t '     //
     .                       'found in the pool. Check whether '    //
     .                       'BODY10_GM parameter is present '      //
     .                       'in the loaded PCK files.'             )
               CALL ERRINT ( '#', I                                 )
               CALL SIGERR ( 'SPICE(NOSUNGM)'                       )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

            CALL BODVRD ( '10', 'GM', 1, N, SUNGM )

C
C           Get state of center body relative to the Sun, compute
C           elements and period.
C
            CALL SPKEZ ( I, MIDNET, 'J2000', 'NONE', 10, STATE, LT)

            CALL OSCELT( STATE, MIDNET, SUNGM, ELTS )
            T = TWOPI()*DSQRT((ELTS(1)/(1.D0-ELTS(2)))**3/SUNGM)

            IF ( T .EQ. 0.D0 ) THEN
               CALL SETMSG ( 'Body''s # heliocentric orbital '      //
     .                       'period is computed as zero, '         //
     .                       'which doesn''t make any sense.'       )
               CALL ERRINT ( '#', I                                 )
               CALL SIGERR ( 'SPICE(BADORBITALPERIOD)'              )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

C
C           Compute average local second duration (thanks to Bill
C           for the formula!)
C
            HDP = 1.D0 / ( 360.D0 * SPD() / PMMOTN ) - 1.D0 / T

            IF ( HDP .EQ. 0.D0 ) THEN
               CALL SETMSG ( 'Duration of the local second on the ' //
     .                       'body # is infinity. So, local solar ' //
     .                       'time is always the same there.'       )
               CALL ERRINT ( '#', BODYID                            )
               CALL SIGERR ( 'SPICE(INDEFINITELOCALSECOND)'         )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

            SCRATE = 1.D0 / HDP / SPD()

            IF ( SCRATE .LT. 0.D0 ) THEN
               SCRATE = - SCRATE
            END IF

C
C           Now we compute previous local solar time midnight at this
C           longitude. We will use it as the basic for SOL computation
C           for input and output LST.
C
            CALL LSTMID( MIDNET, BODYID, LON, SCRATE,
     .                   'PREVIOUS', 0, ETTEMP )
            MIDNET = ETTEMP

         END IF

      ELSE

C
C        Landing date knowledge is not needed because neither input nor
C        output system is LST.
C
            LSTTIM = ' '

      END IF

C
C     If speaker is ON, print trace information, but only for the first
C     time around with this command line. (This "hook" to STDOUT is
C     ugly, but we must keep it for backwards compatibility with the
C     old CHRONOS program interface.)
C
C     The "speaker" can be set on by calling SPEKON entry of the SPEAKR
C     umbrella. But it should never be done if this routine used
C     anywhere but in CHRONOS executable.
C
      CALL SPEKST( HWORD )

      IF ( HWORD(1:2) .EQ. 'ON' .AND. NLOOPS .EQ. 0 ) THEN

         CALL SPEAK( ' ' )

         CALL SPEAK( 'Converting time from: ' //
     .                FROMTS(:RTRIM(FROMTS))  // '/'    //
     .                FROMTT(:RTRIM(FROMTT))  )

         CALL SPEAK( '                  to: ' //
     .                TOTS(:RTRIM(TOTS))      // '/'    //
     .                TOTT(:RTRIM(TOTT))      )

         IF ( TOFRMT .NE. ' ' ) THEN
            CALL SPEAK( '  Output time format: ' //
     .                  TOFRMT(:RTRIM(TOFRMT)))
         ELSE
            CALL SPEAK( '  Output time format: NOT APPLICABLE' )
         END IF

         IF ( FROMTS .EQ. SCLSYS .OR. TOTS .EQ. SCLSYS .OR.
     .        FROMTS .EQ. LSTTYP .OR. TOTS .EQ. LSTTYP .OR.
     .        FROMTT .EQ. ERTTYP .OR. TOTT .EQ. ERTTYP .OR.
     .        FROMTT .EQ. ETTTYP .OR. TOTT .EQ. ETTTYP .OR.
     .        FROMTT .EQ. LTTYP  .OR. TOTT .EQ. LTTYP      ) THEN

            HLINE = '  Spacecraft NAIF ID: #'
            CALL REPMI( HLINE, '#', SCID, HLINE )
            CALL SPEAK( HLINE )

         END IF

         IF ( FROMTS .EQ. LSTTYP .OR. TOTS .EQ. LSTTYP .OR.
     .        FROMTT .EQ. LSNTYP .OR. TOTT .EQ. LSNTYP .OR.
     .        FROMTT .EQ. ERTTYP .OR. TOTT .EQ. ERTTYP .OR.
     .        FROMTT .EQ. ETTTYP .OR. TOTT .EQ. ETTTYP .OR.
     .        FROMTT .EQ. LTTYP  .OR. TOTT .EQ. LTTYP      ) THEN

            HLINE = ' Center body NAIF ID: #'
            CALL REPMI( HLINE, '#', BODYID, HLINE )
            CALL SPEAK( HLINE )

         END IF

         IF ( FROMTS .EQ. LSTTYP .OR. TOTS .EQ. LSTTYP ) THEN

            IF ( LSTTIM .NE. ' ' ) THEN

               CALL SPEAK( '   Landing date/time: ' //
     .                     LSTTIM(:RTRIM(LSTTIM)) )
               CALL ET2UTC( MIDNET, 'C', 3, HWORD )

               HLINE = '   Landing SOL index: #'
               CALL REPMI( HLINE, '#', SOLZER, HLINE )
               CALL SPEAK( HLINE )

               CALL SPEAK( 'Landing SOL midnight: ' //
     .                     HWORD(:RTRIM(HWORD)) )

               HLINE =     'Average local second: # ET seconds'
               CALL REPMD ( HLINE, '#', SCRATE, 14, HLINE )
               CALL SPEAK( HLINE )

            ELSE
               CALL SPEAK( '   Landing date/time: NOT SPECIFIED' )
            END IF

         END IF

         CALL SPEAK( ' ' )

      END IF

C
C     Loop for each time in the input times array.
C
      DO COUNT = 1, NTIMES

         INTIME = INPTIM( COUNT )

C
C        Is input time blank?
C
         IF ( INTIME .EQ. ' ' ) THEN
            CALL SETMSG ( 'The #-th input time is blank.'           )
            CALL ERRINT ( '#', COUNT                                )
            CALL SIGERR ( 'SPICE(BLANKINPUTTIME)'                   )
         END IF

C
C        Let's do real work. First we need to convert from whatever
C        "from" system we have got to spacecraft ET. UTC case is first.
C
         IF      ( FROMTS .EQ. UTCSYS ) THEN

C
C           If we have got time string, we need to check whether it's
C           parsable. And if it is, convert it to ET.
C
            IF ( FROMTT .EQ. ERTTYP .OR.
     .           FROMTT .EQ. ETTTYP .OR.
     .           FROMTT .EQ. SCTTYP ) THEN

C
C              Convert UTC to ET. Nothing can be simpler if you have
C              SPICELIB time subsystem. But first check whether there
C              is some key in the input string that will force SPICE
C              time system to interpret it as TDB or TDT instead of
C              UTC.
C
               CALL UCASE( INTIME, LINE )
               IF ( POS ( LINE, 'TDB', 1 ) .NE. 0 .OR.
     .              POS ( LINE, 'TDT', 1 ) .NE. 0       ) THEN
                  CALL SETMSG ( 'Input time which is supposed to '  //
     .                          'be a UTC time contains TDB or TDT '//
     .                          'flag that will make it interpreted '//
     .                          'as Barycentric Dynamical Time '    //
     .                          'system (TDB or ET ) or the '       //
     .                          'Terrestrial Dynamical Time system '//
     .                          '(TDT) time.'                       )
                  CALL SIGERR ( 'SPICE(BADINPUTUTCTIME)'            )
                  CALL CHKOUT ( 'CRONOS'                            )
                  RETURN
               END IF

C
C              Well, let's convert our time to ET using STR2ET. Run
C              TPARTV/TCHECK before it to generate more meaningful
C              message in case if input time cannot be parsed.
C
               CALL TPARTV ( INTIME, TVEC, NTVEC, TYPE, MODIFY,
     .                       MODS, YABBRV, OK, HWORD, ERROR )
               IF ( OK ) THEN
                  CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
               END IF

               IF ( .NOT. OK ) THEN
                  CALL SETMSG ( 'Input time doesn''t seem to '      //
     .                          'represent a UTC time correctly '   //
     .                          'for the following reason: # '      )
                  CALL ERRCH  ( '#', ERROR                          )
                  CALL SIGERR ( 'SPICE(BADINPUTUTCTIME)'            )
                  CALL CHKOUT ( 'CRONOS'                            )
                  RETURN
               END IF

               CALL STR2ET( INTIME, ET )

            END IF

C
C           What else depending on the type do we need to do with it?
C
            IF      ( FROMTT .EQ. ERTTYP ) THEN

C
C              For the ERT type, we need to adjust our ET time to be on
C              the spacecraft instead of the Earth. Call LTIME to do
C              it.
C
               CALL LTIME ( ET, 399, '<-', SCID, ETTEMP, LT )
               ET = ETTEMP

            ELSE IF ( FROMTT .EQ. ETTTYP ) THEN

C
C              For the ETT type, we need to adjust our ET time to be on
C              the spacecraft instead of the Earth. Call LTIME to do it
C              with direction opposite to ERT.
C
               CALL LTIME ( ET, 399, '->', SCID, ETTEMP, LT )
               ET = ETTEMP

            ELSE IF ( FROMTT .EQ. LTTYP ) THEN

C
C              Light time as an input??? Whoever specified it must be
C              crazy!
C
               CALL SETMSG ( 'Input time type ''#'' doesn''t make ' //
     .                       'any sense. Cannot process it.'        )
               CALL ERRCH  ( '#', LTTYP                             )
               CALL SIGERR ( 'SPICE(BADINPUTTYPE)'                  )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN

            ELSE IF ( FROMTT .EQ. SCTTYP ) THEN

C
C              We don't need to do any additional work for SCET type.
C              Therefore, this "if case" is empty.
C

            ELSE

               CALL SETMSG ( 'How did you manage to get to this '   //
     .                       'place in the program?'                )
               CALL SIGERR ( 'SPICE(CHRONOSBUG1)'                   )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN

            END IF

C
C        We are done with UTC case. The next case is ET.
C
         ELSE IF ( FROMTS .EQ. ETSYS  ) THEN

C
C           Convert ET to ET. Silly, huh? :) Maybe not so silly given
C           that ET is provided as a string and quite it's possibly in
C           some calendar format. If we have got time string, we need
C           to check whether it's parsable.
C
            IF ( FROMTT .EQ. ERTTYP .OR.
     .           FROMTT .EQ. ETTTYP .OR.
     .           FROMTT .EQ. SCTTYP       ) THEN

C
C              Add TDB and run TPARTV/TCHECK to see if input ET time is
C              parsable.
C
               CALL UCASE( INTIME, LINE )
               IF ( POS ( LINE, 'TDB', 1 ) .EQ. 0 ) THEN
                  LINE = INTIME
                  LINE = LINE(:RTRIM(LINE)) // ' TDB'
               END IF

               CALL TPARTV ( LINE, TVEC, NTVEC, TYPE, MODIFY,
     .                       MODS, YABBRV, OK, HWORD, ERROR )
               IF ( OK ) THEN
                  CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
               END IF

               IF ( .NOT. OK ) THEN
                  CALL SETMSG ( 'Input time doesn''t seem to '      //
     .                          'represent a ET time correctly for '//
     .                          'the following reason: # '          )
                  CALL ERRCH  ( '#', ERROR                          )
                  CALL SIGERR ( 'SPICE(BADINPUTETTIME)'             )
                  CALL CHKOUT ( 'CRONOS'                            )
                  RETURN
               END IF

               CALL STR2ET( LINE, ET )

            ELSE IF ( FROMTT .EQ. SECTYP ) THEN

C
C              DP number of seconds is stored in the input string. Run
C              NPARSD.
C
               CALL NPARSD ( INTIME, ET, ERROR, N )
               IF ( N .NE. 0 ) THEN
                  CALL SETMSG ( 'Input time doesn''t seem to be a ' //
     .                          'DP number representing a ET '      //
     .                          'seconds past J2000 for '           //
     .                          'the following reason: # '          )
                  CALL ERRCH  ( '#', ERROR                          )
                  CALL SIGERR ( 'SPICE(BADINPUTETTIME)'             )
                  CALL CHKOUT ( 'CRONOS'                            )
                  RETURN
               END IF

            END IF

C
C           What else depending on the type do we need to do with it?
C
            IF      ( FROMTT .EQ. ERTTYP ) THEN

C
C              For the ERT type, we need to adjust our ET time to be on
C              the spacecraft instead of the Earth. Call LTIME to do
C              it.
C
               CALL LTIME ( ET, 399, '<-', SCID, ETTEMP, LT )
               ET = ETTEMP

            ELSE IF ( FROMTT .EQ. ETTTYP ) THEN

C
C              For the ETT type, we need to adjust our ET time to be on
C              the spacecraft instead of the Earth. Call LTIME to do it
C              with direction opposite to ERT.
C
               CALL LTIME ( ET, 399, '->', SCID, ETTEMP, LT )
               ET = ETTEMP


            ELSE IF ( FROMTT .EQ. LTTYP ) THEN

C
C              Light time as an input??? Whoever specified it must be
C              crazy!
C
               CALL SETMSG ( 'Input time type ''#'' doesn''t make ' //
     .                       'any sense. Cannot process it.'        )
               CALL ERRCH  ( '#', LTTYP                             )
               CALL SIGERR ( 'SPICE(BADINPUTTYPE)'                  )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN

            ELSE IF ( FROMTT .EQ. SECTYP ) THEN

C
C              Hey, we don't need to do anything, we already parsed the
C              string containing DP number.
C

            ELSE IF ( FROMTT .EQ. SCTTYP ) THEN

C
C              Also, nothing more to do for SCET type
C

            ELSE
               CALL SETMSG ( 'How did you manage to get to this '   //
     .                       'place in the program?'                )
               CALL SIGERR ( 'SPICE(CHRONOSBUG2)'                   )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

C
C        We are done with ET case also. Now SCLK system
C
         ELSE IF ( FROMTS .EQ. SCLSYS ) THEN

C
C           Convert SCLK to ET. First, convert whatever string we have
C           got to SCLK ticks.
C
            IF      ( FROMTT .EQ. SCLTYP ) THEN

C
C              For string SCLK we just pull it though SCENCD and see
C              what happens.
C
               CALL SCENCD( SCID, INTIME, SCLKDP )

            ELSE IF ( FROMTT .EQ. HEXTYP ) THEN

C
C              Well, for hex we need to convert each field (except
C              partition :) to integer before calling SCENCD. First we
C              determine whether input time contains no more than one
C              slash and that it's not the last char.
C
               CALL LPARSE ( INTIME, '/', MAXFLD, N, FIELDS )

               IF ( N .GT. 2 .OR. FIELDS(N) .EQ. ' ' ) THEN

                  CALL SETMSG ( 'Input HEX SCLK time ''#'' contains '//
     .                          'more than one ''/'' designating '  //
     .                          'partitions and/or it''s the last ' //
     .                          'non-blank character in the time.'  )
                  CALL ERRCH  ( '#', INTIME                         )
                  CALL SIGERR ( 'SPICE(BADINPUTETTIME)'             )
                  CALL CHKOUT ( 'CRONOS'                            )
                  RETURN

               ELSE IF ( N .EQ. 2 ) THEN

C
C                 There is a slash in the input time. Save all before
C                 slash and slash to use a prefix for "integer" SCLK
C                 string that we are going to make.
C
                  HLINE = INTIME(:POS(INTIME, '/', 1))

               ELSE
C
C                 No slash -- no prefix for "integer" SCLK string that
C                 we are going to make.
C
                  HLINE = ' '

               END IF

C
C              Now we parse portion of input time after slash using
C              delimiter list from type 1 SCLK.
C
               CALL LPARSM ( INTIME(POS(INTIME, '/', 1)+1:),
     .                       SCDLMS, MAXFLD, N, FIELDS )

C
C              Convert each field from HEX to integer and "rebuild"
C              "integer" SCLK.
C
               DO I = 1, N

                  CALL HX2INT ( FIELDS(I), J, BAD, ERROR )
                  IF ( BAD ) THEN
                     CALL SETMSG ( '#th field of input HEX SCLK '   //
     .                             'string ''#'' doesn''t represent '//
     .                             'HEX number.'                    )
                     CALL ERRINT ( '#', I                           )
                     CALL ERRCH  ( '#', INTIME                      )
                     CALL SIGERR ( 'SPICE(BADINPUTETTIME)'          )
                     CALL CHKOUT ( 'CRONOS'                         )
                     RETURN
                  END IF
                  CALL INTSTR ( J, HWORD )

                  HLINE = HLINE(:RTRIM(HLINE)) // ' ' // HWORD

               END DO

C
C              Convert SCLK string to ticks.
C
               CALL SCENCD( SCID, HLINE, SCLKDP )

            ELSE IF ( FROMTT .EQ. TIKTYP ) THEN

C
C              For SCLK ticks we just need to convert string to DP.
C
               CALL NPARSD ( INTIME, SCLKDP, ERROR, N )
               IF ( N .NE. 0 ) THEN
                  CALL SETMSG ( 'Input time doesn''t seem to be a ' //
     .                          'DP number representing a SCLK '    //
     .                          'ticks for the following reason: # ')
                  CALL ERRCH  ( '#', ERROR                          )
                  CALL SIGERR ( 'SPICE(BADINPUTETTIME)'             )
                  CALL CHKOUT ( 'CRONOS'                            )
                  RETURN
               END IF

            ELSE
               CALL SETMSG ( 'How did you manage to get to this '   //
     .                       'place in the program?'                )
               CALL SIGERR ( 'SPICE(CHRONOSBUG3)'                   )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN

            END IF

C
C           Here we have encoded SCLK. Simply convert it to ET.
C
            CALL SCT2E( SCID, SCLKDP, ET )

C
C        The last is LST case.
C
         ELSE IF ( FROMTS .EQ. LSTSYS ) THEN

C
C           Again, processing depend on a time type.
C
            IF      ( FROMTT .EQ. LSTTYP ) THEN

C
C              Convert local true solar time to ET. Check if there was
C              landing date or not.
C
               IF ( LSTTIM .NE. ' ' ) THEN

C
C                 Yes, there was. Let's look for SOL day number. It
C                 must be the next word after SOL marker.
C
                  CALL UCASE( INTIME, HLINE )

                  IF ( POS ( HLINE, SOLMKR, 1 ) .EQ. 0  ) THEN
                     CALL SETMSG ( '"#" key is not present in the ' //
     .                             'input LST time string "#". '    //
     .                             'Cannot process this input time. ')
                     CALL ERRCH  ( '#', SOLMKR                      )
                     CALL ERRCH  ( '#', INTIME                      )
                     CALL SIGERR ( 'SPICE(NOSOLMARKER)'             )
                     CALL CHKOUT ( 'CRONOS'                         )
                     RETURN
                  END IF

                  CALL NEXTWD( INTIME(POS(HLINE,SOLMKR,1)
     .                                +RTRIM(SOLMKR):), HWORD, LINE )

                  CALL NPARSI ( HWORD, SOLDAY, ERROR, N )
                  IF ( N .NE. 0 ) THEN
                     CALL SETMSG ( 'SOL day specified after "#" key '//
     .                             'in the input LST time string '  //
     .                             '"#" is not an integer number. ' )
                     CALL ERRCH  ( '#', SOLMKR                      )
                     CALL ERRCH  ( '#', INTIME                      )
                     CALL SIGERR ( 'SPICE(BADSOLDAY)'               )
                     CALL CHKOUT ( 'CRONOS'                         )
                     RETURN
                  END IF

                  IF ( SOLDAY .LT. SOLZER ) THEN
                     CALL SETMSG ( 'SOL day specified after "#" key '//
     .                             'in the input LST time string '  //
     .                             '"#" is less than landing SOL #. ')
                     CALL ERRCH  ( '#', SOLMKR                      )
                     CALL ERRCH  ( '#', INTIME                      )
                     CALL ERRINT ( '#', SOLZER                      )
                     CALL SIGERR ( 'SPICE(BADSOLDAY)'               )
                     CALL CHKOUT ( 'CRONOS'                         )
                     RETURN
                  END IF

C
C                 Now we can compute midnight ET corresponding to this
C                 SOL day. We will iterate to get this ET as good as
C                 possible.
C
                  ET = MIDNET + ( SOLDAY - SOLZER ) * SPD() * SCRATE

C
C                 "Precise" this rough estimate of the local solar
C                 midnight of this day of interest.
C
                  CALL LSTMID( ET, BODYID, LON, SCRATE, 
     .                         'NEAREST', 0, ETTEMP )
                  ET = ETTEMP

C
C                 So, know when was the midnight of this SOL day. so we
C                 just need to add our local HR:MN:SC.### converted to
C                 ET seconds. These HR:MN:SC.### are currently
C                 contained the LINE variable.
C
                  CALL LPARSM ( LINE, LSDLMS, MAXFLD, N, FIELDS )
                  IF ( N .GT. 3 ) THEN
                     CALL SETMSG ( 'Clock part of '                 //
     .                             'the input LST time string "#" ' //
     .                             'contains more than 3 fields.'   )
                     CALL ERRCH  ( '#', INTIME                      )
                     CALL SIGERR ( 'SPICE(BADSOLTIME)'              )
                     CALL CHKOUT ( 'CRONOS'                         )
                     RETURN
                  END IF

                  LSECS = 0.D0
                  DO I = 1, N
                     CALL NPARSD ( FIELDS(I), HDP, ERROR, J )
                     IF ( J .NE. 0 ) THEN
                        CALL SETMSG ( '#th token in the clock part '//
     .                                'of the input LST time string '//
     .                                '"#" is not a number.'        )
                        CALL ERRINT ( '#', I                        )
                        CALL ERRCH  ( '#', INTIME                   )
                        CALL SIGERR ( 'SPICE(BADSOLTIME)'           )
                        CALL CHKOUT ( 'CRONOS'                      )
                        RETURN
                     END IF
                     LSECS = LSECS +  HDP * SCRATE * 60.D0 ** ( 3 - I )
                  END DO

                  IF ( LSECS .LT. 0.D0 .OR.
     .                 LSECS .GE. SPD()*SCRATE ) THEN
                     CALL SETMSG ( 'Number of local seconds '       //
     .                             'represented by the clock part ' //
     .                             'of the input LST time string '  //
     .                             '"#" is negative of greater '    //
     .                             'than 86000.'                    )
                     CALL ERRCH  ( '#', INTIME                      )
                     CALL SIGERR ( 'SPICE(BADSOLTIME)'              )
                     CALL CHKOUT ( 'CRONOS'                         )
                     RETURN
                  END IF

                  ET = ET + LSECS

C
C                 And, as last, we "precise" this estimate of the
C                 computed ET.
C
                  CALL LSTMID( ET, BODYID, LON, SCRATE, 'NEAREST',
     .                               IDNINT( LSECS / SCRATE ), ETTEMP )
                  ET = ETTEMP

               ELSE

C
C                 There were no landing date. Well, we cannot process
C                 LST as input than.
C
                  CALL SETMSG ( 'Since landing time wasn''t '       //
     .                          'provided in the setup # time '     //
     .                          'cannot be used as "from" time '    //
     .                          'system.'                           )
                  CALL SIGERR ( 'SPICE(NOLANDINGTIME)'              )
                  CALL CHKOUT ( 'CRONOS'                            )
                  RETURN

               END IF

            ELSE IF ( FROMTT .EQ. LSNTYP ) THEN

C
C              Longitude of the Sun as an input??? Whoever specified
C              it must be crazy!
C
               CALL SETMSG ( 'Input time type ''#'' doesn''t make ' //
     .                       'any sense. Cannot process it.'        )
               CALL ERRCH  ( '#', LSNTYP                            )
               CALL SIGERR ( 'SPICE(BADINPUTTYPE)'                  )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN

            ELSE
               CALL SETMSG ( 'How did you manage to get to this '   //
     .                       'place in the program?'                )
               CALL SIGERR ( 'SPICE(CHRONOSBUG9)'                   )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

         ELSE
            CALL SETMSG ( 'How did you manage to get to this '      //
     .                    'place in the program?'                   )
            CALL SIGERR ( 'SPICE(CHRONOSBUG4)'                      )
            CALL CHKOUT ( 'CRONOS'                                  )
            RETURN
         END IF

C
C        Congratulations!!! 1/2 is done: we have ET time corresponding
C        to our input time (whatever it was:). Let do the second half
C        and convert this ET to the output system/type. UTC case goes
C        first (again).
C

         IF      ( TOTS .EQ. UTCSYS ) THEN

C
C           Convert UTC to ET. Do we need any computation depending on
C           type?
C
            IF      ( TOTT .EQ. ERTTYP ) THEN
C
C              For the ERT type, we need to adjust our ET time to be on
C              the Earth instead of the spacecraft. Call LTIME to do
C              it.
C
               CALL LTIME ( ET, SCID, '->', 399, ETTEMP, LT )
               ET = ETTEMP

            ELSE IF ( TOTT .EQ. ETTTYP ) THEN

C
C              For the ETT type, we need to adjust our ET time to be on
C              the Earth instead of the spacecraft. Call LTIME to do it
C              but use light direction opposite to ERT.
C
               CALL LTIME ( ET, SCID, '<-', 399, ETTEMP, LT )
               ET = ETTEMP

            ELSE IF ( TOTT .EQ. LTTYP ) THEN
C
C              For the LT type, we need to compute light time. Call
C              LTIME for that.
C
               CALL LTIME ( ET, SCID, '->', 399, ETTEMP, LT )
               ET = ETTEMP


            ELSE IF ( TOTT .EQ. SCTTYP ) THEN

C
C              Nothing for SCET, thank you.
C
            ELSE
               CALL SETMSG ( 'How did you manage to get to this '   //
     .                       'place in the program?'                )
               CALL SIGERR ( 'SPICE(CHRONOSBUG5)'                   )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN

            END IF

C
C           Do we need any additional formatting depending on type?
C
            IF ( TOTT .EQ. SCTTYP .OR.
     .           TOTT .EQ. ETTTYP .OR.
     .           TOTT .EQ. ERTTYP       ) THEN

C
C              For SCET and ERT output is a time string. So we need a
C              stupidity check for output format. What if somebody put
C              ::TDB, ::TDT or some other strange subsystem
C              specification in it? (No need to uppercase when check --
C              it was already done by TPICTR.) It's not clear whether
C              time zone token must be checked also. So, we don't check
C              it, because it leave more flexibility for output while
C              still filters completely erroneous sys-spec tokens.
C
               IF ( POS ( TOFRMT, '::TDB',  1 ) .NE. 0 .OR.
     .              POS ( TOFRMT, '::GCAL', 1 ) .NE. 0 .OR.
     .              POS ( TOFRMT, '::JCAL', 1 ) .NE. 0 .OR.
     .              POS ( TOFRMT, '::MCAL', 1 ) .NE. 0 .OR.
     .              POS ( TOFRMT, '::TDT',  1 ) .NE. 0       ) THEN
                  CALL SETMSG ( 'Output format picture ''#'' which '//
     .                       'is supposed to be a suitable for '    //
     .                       'formating time as UTC contains TDB, ' //
     .                       'TDT, GCAL, JCAL or MCAL token which ' //
     .                       'tells SPICE time '                    //
     .                       'subsystem to output time in the '     //
     .                       'corresponding non-UTC time system.'   )
                  CALL SIGERR ( 'SPICE(MISMATCHOUTPUTFORMAT)'       )
                  CALL CHKOUT ( 'CRONOS'                            )
                  RETURN
               END IF

               CALL  TIMOUT ( ET, TOFRMT, OUTIME )

            ELSE IF ( TOTT .EQ. LTTYP ) THEN

C
C              For LT output is a DP number ... but still stored in
C              string.
C
               CALL DPFMT ( LT, TOFRMT, OUTIME )

            END IF

C
C        Second case is ET.
C
         ELSE IF ( TOTS .EQ. ETSYS  ) THEN

C
C           Convert ET to ET. Silly, huh? :) Not at all :) Sometime we
C           even need to compute something depending on type.
C
            IF      ( TOTT .EQ. ERTTYP ) THEN
C
C              For the ERT type, we need to adjust our ET time to be on
C              the Earth instead of the spacecraft. Call LTIME to do
C              it.
C
               CALL LTIME ( ET, SCID, '->', 399, ETTEMP, LT )
               ET = ETTEMP

            ELSE IF ( TOTT .EQ. ETTTYP ) THEN
C
C              For the ETT type, we need to adjust our ET time to be on
C              the Earth instead of the spacecraft. Call LTIME to do it
C              but use light direction opposite to ERT.
C
               CALL LTIME ( ET, SCID, '<-', 399, ETTEMP, LT )
               ET = ETTEMP

            ELSE IF ( TOTT .EQ. LTTYP ) THEN
C
C              For the LT type, we need to compute it. Call LTIME for
C              that.
C
               CALL LTIME ( ET, SCID, '->', 399, ETTEMP, LT )
               ET = ETTEMP

            ELSE IF ( TOTT .EQ. SECTYP ) THEN
C
C              Nothing for the seconds.
C

            ELSE IF ( TOTT .EQ. SCTTYP ) THEN

C
C              And also nothing for ET-SCET.
C
            ELSE
               CALL SETMSG ( 'How did you manage to get to this '   //
     .                       'place in the program?'                )
               CALL SIGERR ( 'SPICE(CHRONOSBUG6)'                   )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF

C
C           What about formatting output?
C
            IF ( TOTT .EQ. SCTTYP .OR.
     .           TOTT .EQ. ETTTYP .OR.
     .           TOTT .EQ. ERTTYP      ) THEN

C
C              SCET, ERT and ETT are to be formatted as time string.
C
C              Same story as for output UTC above -- check format for
C              stupid errors and go ahead with TIMOUT.
C
               IF ( POS ( TOFRMT, '::UTC',  1 ) .NE. 0 .OR.
     .              POS ( TOFRMT, '::GCAL', 1 ) .NE. 0 .OR.
     .              POS ( TOFRMT, '::JCAL', 1 ) .NE. 0 .OR.
     .              POS ( TOFRMT, '::MCAL', 1 ) .NE. 0 .OR.
     .              POS ( TOFRMT, '::TDT',  1 ) .NE. 0       ) THEN
                  CALL SETMSG ( 'Output format picture ''#'' which '//
     .                       'is supposed to be a suitable for '    //
     .                       'formating time as ET contains UTC, '  //
     .                       'TDT, GCAL, JCAL or MCAL token which ' //
     .                       'will tell SPICE time '                //
     .                       'subsystem to output time in the '     //
     .                       'corresponding non-ET time system.'    )
                  CALL SIGERR ( 'SPICE(MISMATCHOUTPUTFORMAT)'       )
                  CALL CHKOUT ( 'CRONOS'                            )
                  RETURN
               END IF

               IF ( POS ( TOFRMT, '::TDB', 1 ) .EQ. 0 ) THEN
                  TOFRMH = TOFRMT(:RTRIM(TOFRMT)) // ' ::TDB'
               ELSE
                  TOFRMH = TOFRMT(:RTRIM(TOFRMT))
               END IF

               CALL  TIMOUT ( ET, TOFRMH, OUTIME )

            ELSE IF ( TOTT .EQ. LTTYP ) THEN

C
C              Store DP number in output string for LT.
C
               CALL DPFMT ( LT, TOFRMT, OUTIME )

            ELSE IF ( TOTT .EQ. SECTYP ) THEN

C
C              Do the same as for ET seconds.
C
               CALL DPFMT ( ET, TOFRMT, OUTIME )

            END IF

C
C        SCLK case goes third.
C
         ELSE IF ( TOTS .EQ. SCLSYS ) THEN

C
C           Convert SCLK to ET. Any additional computations/
C           formatting?
C
            IF      ( TOTT .EQ. SCLTYP ) THEN

C
C              Output is plain SCLK. This is the easiest one.
C
               CALL SCE2T ( SCID, ET, SCLKDP )
               CALL SCDECD( SCID, SCLKDP, OUTIME )

            ELSE IF ( TOTT .EQ. HEXTYP ) THEN

C
C              It's a little more headache with outputting HEX. But
C              it's definitely doable :)
C
               CALL SCE2T ( SCID, ET, SCLKDP )
               CALL SCDECD( SCID, SCLKDP, OUTIME )

C
C              Slash is always these and in the right place, no checks
C              for that. Just go ahead -- parse, convert to HEX and
C              rebuild the string
C
               CALL LPARSM ( OUTIME(POS(OUTIME, '/', 1)+1:),
     .                       SCDLMS, MAXFLD, N, FIELDS )

               HLINE = OUTIME(:POS(OUTIME, '/', 1))

               DO I = 1, N

                  CALL NPARSI ( FIELDS(I), J, ERROR, N )
                  CALL INT2HX ( J, HWORD, N )

                  IF ( I .EQ. 1 ) THEN
                     HLINE = HLINE(:RTRIM(HLINE)) // HWORD
                  ELSE
                     HLINE = HLINE(:RTRIM(HLINE)) // '.' // HWORD
                  END IF
               END DO

               OUTIME = HLINE


            ELSE IF ( TOTT .EQ. TIKTYP ) THEN

C
C              Ticks is easy too. But let's do them as integer.
C
               CALL SCE2C  ( SCID, ET, SCLKDP )
               CALL DPFMT  ( SCLKDP, TOFRMT, OUTIME )

            ELSE
               CALL SETMSG ( 'How did you manage to get to this '   //
     .                       'place in the program?'                )
               CALL SIGERR ( 'SPICE(CHRONOSBUG7)'                   )
            END IF

C
C        At last, output LST case.
C
         ELSE IF ( TOTS .EQ. LSTSYS ) THEN

C
C           Any additional computation depending on type?
C
            IF      ( TOTT .EQ. LSTTYP ) THEN

C
C              Convert LST to ET.
C
               IF ( LSTTIM .NE. ' ' .AND. ET .GE. LSTET ) THEN

C
C                 We can compute SOL day number if we know landing time
C                 and our ET after the landing time. We find midnight,
C                 previous to this ET.
C
                  CALL LSTMID( ET, BODYID, LON, SCRATE,
     .                         'PREVIOUS', 0, ETOUT )

C
C                 How many days since first SOL midnight?
C
                  HDP = ( ETOUT - MIDNET ) / ( SPD() * SCRATE )

                  IF ( DMOD( HDP, 1.D0 ) .GT.  0.5D0 ) THEN
                     SOLDAY = INT( HDP ) + SOLZER + 1
                  ELSE
                     SOLDAY = INT( HDP ) + SOLZER
                  END IF

C
C                 Put SOL day number into the output string.
C
                  OUTIME = '# # #'
                  CALL REPMC ( OUTIME, '#', SOLMKR, OUTIME )
                  CALL REPMI ( OUTIME, '#', SOLDAY, OUTIME )

               ELSE

C
C                 No SOL day ... how pity :). Let prepare all things to
C                 compute at least relative LST is sub-spacecraft
C                 point.
C
                  CALL BODC2N( BODYID, HWORD, FOUND )
                  IF ( .NOT. FOUND ) THEN
                     CALL SETMSG ( 'Cannot recognize body ID #.'    )
                     CALL ERRINT ( '#', BODYID                      )
                     CALL SIGERR ( 'SPICE(BADBODYID)'               )
                     CALL CHKOUT ( 'CRONOS'                         )
                     RETURN
                  END IF

                  HLINE = 'IAU_' // HWORD

                  CALL SPKEZ(SCID, ET, HLINE, 'LT+S', BODYID, STATE,LT)
                  CALL RECLAT( STATE, R, LON, LAT )

C
C                 And set initial string for output time.
C
                  OUTIME = '#'

               END IF

C
C              Now, get 24 hours clock readout from ET2LST.
C
               CALL ET2LST( ET, BODYID, LON, 'PLANETOCENTRIC',
     .                               HR, MN, SC, HWORD, HLINE )

C
C              Final touch -- inserting local clock to output string
C              :).
C
               CALL REPMC ( OUTIME, '#', HWORD, OUTIME )

            ELSE IF ( TOTT .EQ. LSNTYP ) THEN

C
C              We need to compute longitude if the Sun -- measure of
C              the seasons on a planet. We just call Nat's routine to
C              do that.
C
               HDP = LS( BODYID, ET, 'LT+S' ) * DPR()

C
C              Store DP longitude of the Sun in output string.
C
               CALL DPFMT ( HDP, TOFRMT, OUTIME )

            ELSE
               CALL SETMSG ( 'How did you manage to get to this '   //
     .                       'place in the program?'                )
               CALL SIGERR ( 'SPICE(CHRONOSBUG10)'                  )
               CALL CHKOUT ( 'CRONOS'                               )
               RETURN
            END IF


         ELSE
            CALL SETMSG ( 'How did you manage to get to this '      //
     .                    'place in the program?'                   )
            CALL SIGERR ( 'SPICE(CHRONOSBUG8)'                      )
            CALL CHKOUT ( 'CRONOS'                                  )
            RETURN
         END IF

C
C        Conversion is done. Assign corresponding element of the output
C        times array.
C
         OUTTIM( COUNT ) = OUTIME

C
C        Increment the number of iterations with this command line.
C
         NLOOPS = NLOOPS + 1

C
C        End of the main loop processing input times.
C
      END DO

C
C     All done.
C
      CALL CHKOUT( 'CRONOS' )
      RETURN

      END

