C$Procedure      CMLARG ( Get command line arguments )

      SUBROUTINE CMLARG ( CMDFIL, INPFN, OUTFN, APPFLG )

C$ Abstract
C
C     This routine is a module of the MKSPK program. It parses 
C     command line and returns setup, input, and output files names
C     and a flag indicating that appending to an existing output
C     file was requested, or displays help, usage or template 
C     information if either of them was requested using corresponding 
C     command line switch.
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
C     None.
C
C$ Declarations
 
      IMPLICIT              NONE

      INCLUDE               'mkspk.inc'  

      CHARACTER*(*)         CMDFIL
      CHARACTER*(*)         INPFN
      CHARACTER*(*)         OUTFN
      LOGICAL               APPFLG
      
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CMDFIL     O   Setup filename
C     INPFN      O   Input filename
C     OUTFN      O   Output filename
C     APPFLG     O   Append flag
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     CMDFIL      is the setup file name provided on command line after 
C                 corresponding switch or interactively in response 
C                 to the prompt.
C                    
C     INPFN       is the input file name provided on command line after 
C                 corresponding switch. If it wasn't provided, it's set 
C                 to blank.
C      
C     OUTFN       is the output file name provided on command line 
C                 after corresponding switch. If it wasn't provided, 
C                 it's set to blank.
C
C     APPFLG      is the flag indicating that appending to an 
C                 existing output file was requested. If appropriate
C                 command line switch was present, this flag is set
C                 to .TRUE. Otherwise, it's set to .FALSE.
C
C$ Parameters
C
C     Command line keys are declared as parameters in the include 
C     file.
C
C$ Exceptions
C
C     1) If input data type specified on command line after 
C        -t/-template command line option is not recognized, then
C        the error 'SPICE(UNRECOGNDATATYPE)' will be signaled.
C
C     2) If only input data type specified on the command line after 
C        -t/-template command line option, then the error 
C        'SPICE(MISSINGARGUMENTS)' will be signaled.
C
C     3) If output SPK type provided on the command line after 
C        -t/-template command line option is not an integer number
C        is not one of the supported types, then the error 
C        'SPICE(BADOUTPUTSPKTYPE)' will be signaled.
C
C     4) If output SPK type and input data types provided after 
C        -t/-template command line option mismatch, then the error
C        'SPICE(TYPESMISMATCH)' will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     If setup filename was not provided on the command line after 
C     corresponding switch, this routine will prompt for it. It will 
C     not prompt for input and output file names if they weren't 
C     provided.
C
C     If usage, help or template display was requested, the routine 
C     displays it and stops.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.G. Khavenson (IKI RAS, Russia)
C     B.V. Semenov   (NAIF, JPL)
C
C$ Version
C
C-    Version 2.1.0, 13-FEB-2012 (BVS)
C
C        Updated complete and TLE-specific template displays for new
C        TLE keywords.
C
C-    Version 2.0.0, 20-MAR-2001 (BVS)
C
C        Changed calling sequence to also return a flag indicating 
C        that appending to an existing output file was requested. 
C        Added processing of the command line parameter '-append'. 
C        Changed usage and help displays to show the command
C        line option '-append' and corresponding setup file 
C        keyword APPEND_TO_OUTPUT.
C
C-    Version 1.0.8, 28-JAN-2000 (BVS)
C
C        Fixed TLE/10 template to contain producer ID.
C
C-    Version 1.0.7, 20-JAN-2000 (BVS)
C
C        Added code to recognize output SPK types 12 and 13.
C
C-    Version 1.0.6, 18-JAN-2000 (BVS)
C
C        Added a few more redundant error checks (for more polite
C        and clear complaints :-). Replaced straight string 
C        comparisons with calls to EQSTR. Corrected help display to 
C        mention two-line elements input.
C
C-    Version 1.0.5, 25-NOV-1999 (NGK)
C
C        Added template display for two-line element processing.
C
C-    Version 1.0.4, 29-SEP-1999 (NGK)
C
C        Added template displays depending of input data type and
C        output SPK type.
C
C-    Version 1.0.3, 09-SEP-1999 (NGK)
C
C        Added parameters and special processing for \begintext and 
C        \begindata tokens to ensure that they are printed correctly
C        on all platforms. 
C
C-    Version 1.0.2, 29-MAR-1999 (NGK)
C
C        Added comments.
C
C-    Version 1.0.1, 18-MAR-1999 (BVS)
C
C        Added usage, help and template displays. Corrected comments.
C
C-    Version 1.0.0, 8-SEP-1998 (NGK)
C
C-&
 
C$ Index_Entries
C
C     Get command line arguments                  
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               POS 
      INTEGER               NBLEN
      INTEGER               RTRIM
      LOGICAL               RETURN
      LOGICAL               EQSTR

C
C     Local parameters. Size SHRTLN declared in the include file.
C
      INTEGER               MSGSIZ
      PARAMETER           ( MSGSIZ = 50 )

      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )

      CHARACTER*(SHRTLN)    DAT
      PARAMETER           ( DAT    = 'begindata' )

      CHARACTER*(SHRTLN)    TXT
      PARAMETER           ( TXT    = 'begintext' )

      INTEGER               MRKLEN
      PARAMETER           ( MRKLEN = 11 )

C
C     Local variables
C
C     Generic line variable. Size LINLEN declared in the include file.
C
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    REST 
 
      CHARACTER*(LINLEN)    ERROR         
      CHARACTER*(SHRTLN)    MESSGE (MSGSIZ)

      CHARACTER*(MRKLEN)    BEGDAT
      CHARACTER*(MRKLEN)    BEGTXT 

      CHARACTER*(MRKLEN)    INPTYP
      CHARACTER*(MRKLEN)    OUTTYP 
      CHARACTER*(MRKLEN)    CMLTMG 
             
      INTEGER               PTR    
      INTEGER               IOUTYP    

      INTEGER               I

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CMLARG' )
      END IF 

C
C     Form the terms that must enclose setup assignments
C
      BEGDAT = CHAR(BSLASH) // DAT
      BEGTXT = CHAR(BSLASH) // TXT

C
C     Read command line. Add one space at the beginning and one the 
C     end of it to be able to search for command line keys surrounded
C     by spaces.
C
      CALL GETCML ( LINE )
      REST = ' ' // LINE(:RTRIM(LINE)) // ' '
      LINE = REST

C
C     Display usage and stop. Flags CMLUSS and CMLUSL are declared 
C     in the include file.
C
      IF (  POS ( LINE, ' ' // CMLUSS // ' ', 1 ) .NE. 0 .OR.
     .      POS ( LINE, ' ' // CMLUSL // ' ', 1 ) .NE. 0      ) THEN
     
         MESSGE(  1 ) = '     Program usage:'
         MESSGE(  2 ) = ' '
         MESSGE(  3 ) = '           > mkspk [-setup <setup fi'
     .   //             'le name>]'
         MESSGE(  4 ) = '                   [-input <input da'
     .   //             'ta file name>]'
         MESSGE(  5 ) = '                   [-output <output '
     .   //             'SPK file name>]'
         MESSGE(  6 ) = '                   [-append]'
         MESSGE(  7 ) = '                   [-u|-usage]'
         MESSGE(  8 ) = '                   [-h|-help]'
         MESSGE(  9 ) = '                   [-t|-template]'
     .   //             ' [<input data type> <output spk type>]'

         MESSGE( 10 ) = ' '
         MESSGE( 11 ) = '     If a setup file name isn''t pro'
     .   //             'vided on the command line, the progr'
     .   //             'am'
         MESSGE( 12 ) = '     will prompt for it. It will not'
     .   //             ' prompt for the input or output file'
         MESSGE( 13 ) = '     names; these file names must be'
     .   //             ' provided on the command line or in '
     .   //             'the'
         MESSGE( 14 ) = '     setup file. If input and output'
     .   //             ' file names are provided on the comm'
     .   //             'and'
         MESSGE( 15 ) = '     line, any file names assigned u'
     .   //             'sing setup keywords are ignored. The'
         MESSGE( 16 ) = '     input file must already exist a'
     .   //             'nd, if -append key or corresponding '
         MESSGE( 17 ) = '     setup file keyword is not specified,'
     .   //             ' the output file must be a new file.'
         MESSGE( 18 ) = ' '
         
         DO I = 1, 18
            CALL TOSTDO( MESSGE(I) )
         END DO
         
         STOP
         
      END IF

C
C     Display help and stop. Flags CMLHLS and CMLHLL are declared 
C     in the include file.
C
      IF (  POS ( LINE, ' ' // CMLHLS // ' ', 1 ) .NE. 0 .OR.
     .      POS ( LINE, ' ' // CMLHLL // ' ', 1 ) .NE. 0       ) THEN
     
         MESSGE(  1 ) = '     MKSPK is a NAIF Toolkit utility'
     .   //             ' program that generates a spacecraft'
     .   //             ' or'
         MESSGE(  2 ) = '     target body''s ephemeris file i'
     .   //             'n SPICE SPK format using one of the'
         MESSGE(  3 ) = '     following SPK types: 5, 8, 9, 1'
     .   //             '0, 12, 13, 15, 17. This SPK file is'
         MESSGE(  4 ) = '     a binary file constructed according '
     .   //             'to the DAF (Double precision Array'
         MESSGE(  5 ) = '     File) architecture, containing one or'
     .   //             ' more SPK data segments.'
         MESSGE(  6 ) = ' '
         MESSGE(  7 ) = '     The MKSPK program accepts one A'
     .   //             'SCII text file containing various'
         MESSGE(  8 ) = '     descriptions of input data (set'
     .   //             'up file) and a second ASCII text fil'
     .   //             'e'
         MESSGE(  9 ) = '     (input file) containing the eph'
     .   //             'emeris data to be processed.'
         MESSGE( 10 ) = ' '
         MESSGE( 11 ) = '     Input data could be time ordere'
     .   //             'd lists of states (positions and'
         MESSGE( 12 ) = '     velocities), or sets of conic e'
     .   //             'lements, or a set of equinoctial'
         MESSGE( 13 ) = '     elements, or sets of two-line e'
     .   //             'lements. All input data must'
         MESSGE( 14 ) = '      be defined in a reference frame'
     .   //             ' and relative to a body center, '
         MESSGE( 15 ) = '     both'
     .   //             ' of which are specified in the setup'
         MESSGE( 16 ) = '     file.'
         MESSGE( 17 ) = ' '
         MESSGE( 18 ) = '     The program also allows the use'
     .   //             'r to optionally specify some'
         MESSGE( 19 ) = '     descriptive text in a separate '
     .   //             'file (comment file) to be placed int'
     .   //             'o'
         MESSGE( 20 ) = '     the ``comment area'''' of the N'
     .   //             'AIF SPK ephemeris file.'
         MESSGE( 21 ) = ' '
         MESSGE( 22 ) = '     For documentation purposes cont'
     .   //             'ent of the MKSPK setup file is'
         MESSGE( 23 ) = '     automatically placed at the end'
     .   //             ' of the ``comment area'''' of the SP'
     .   //             'K'
         MESSGE( 24 ) = '     file.'
         MESSGE( 25 ) = ' '
         MESSGE( 26 ) = '     Run the program with '//CMLUSL//' com'
     .   //             'mand line key to see usage informati'
     .   //             'on.'
         MESSGE( 27 ) = ' '

         DO I = 1, 27
            CALL TOSTDO( MESSGE(I) )
         END DO
         
         STOP
         
      END IF

C
C     Display template depending of input/output type and stop. 
C     Flags CMLTMS and CMLTML and template keywords are declared 
C     in the include file.
C
      IF (  POS ( LINE, ' ' // CMLTMS // ' ', 1 ) .NE. 0 .OR.
     .      POS ( LINE, ' ' // CMLTML // ' ', 1 ) .NE. 0      ) THEN

C
C        Two top lines in the message are left blank because they will
C        be set depending on whether complete or input/output type 
C        specific template will be displayed.
C
         MESSGE(  1 ) = ' '
         MESSGE(  2 ) = ' '
         
         MESSGE(  3 ) = ' '
         MESSGE(  4 ) = BEGDAT
         MESSGE(  5 ) = '   '//KINDTP//'   = '''//INSTTP//''''
     .   //             ' or '''//INELTP//''' or '''//INEQTP//''''
     .   //             ' or '''//INTLTP//''''
         MESSGE(  6 ) = '   '//KOSPTP//'   = 5 or 8 or '
     .   //             '9 or 10 or 12 or 13 or 15 or 17'
         MESSGE(  7 ) = '   '//KOBJID//'         = NAIF '
     .   //             'numeric code of the object or TLE '
     .   //             's/c code'
         MESSGE(  8 ) = ' or'
         MESSGE(  9 ) = '   '//KOBJNM//'       = ''NAIF sup'
     .   //             'ported object name'''
         MESSGE( 10 ) = '   '//KCENID//'         = NAIF '
     .   //             'numeric code of the central body'
         MESSGE( 11 ) = ' or'
         MESSGE( 12 ) = '   '//KCENNM//'       = ''NAIF sup'
     .   //             'ported body name'''
         MESSGE( 13 ) = '   '//KRFRNM//'    = ''referenc'
     .   //             'e frame name'''
         MESSGE( 14 ) = '   '//KPROID//'       = ''producer'
     .   //             ' identifier'''
         MESSGE( 15 ) = '   '//KDATOR//'        = ''ordered '
     .   //             'list of input parameter names'''
         MESSGE( 16 ) = '   '//KDATDL//'    = ''delimite'
     .   //             'r separating input data items'''
         MESSGE( 17 ) = '   '//LSKFIL//'  = ''leapseco'
     .   //             'nds file name'''
         MESSGE( 18 ) = '   '//KINDFN//'   = ''input da'
     .   //             'ta file name'''
         MESSGE( 19 ) = '   '//KOSPFN//'   = ''output S'
     .   //             'PK file name'''
         MESSGE( 20 ) = '   '//PCKFIL//'          = ( ''PCK_1 '
     .   //             'file name'' ''PCK_2 file name'' ... '
     .   //             ')'
         MESSGE( 21 ) = '   '//FRKFIL//'    = ''frame de'
     .   //             'finition file name'''
         MESSGE( 22 ) = '   '//CMTFIL//'      = ''comment '
     .   //             'file name'''
         MESSGE( 23 ) = '   '//KINUNI//'  = ( ''ANGLES'
     .   //             ' = ang. unit'' ''DISTANCES= dist. un'
     .   //             'it'' )'
         MESSGE( 24 ) = '   '//KELENG//'  = length of '
     .   //             'epoch string'
         MESSGE( 25 ) = '   '//KIGNFL//' = number of '
     .   //             'initial file lines to be ignored'
         MESSGE( 26 ) = '   '//KLNPRC//'  = number o'
     .   //             'f lines in one input record'
         MESSGE( 27 ) = '   '//KTIMWR//'      = ''# time w'
     .   //             'rapper'''
         MESSGE( 28 ) = '   '//KSTATM//'        = ''start ti'
     .   //             'me'''
         MESSGE( 29 ) = '   '//KSTOTM//'         = ''stop tim'
     .   //             'e'''
         MESSGE( 30 ) = '   '//KPRTYP//'   = ''NO PRECE'
     .   //             'SSION'' or'
         MESSGE( 31 ) = '                       ''APSIDE P'
     .   //             'RECESSION ONLY'' or'
         MESSGE( 32 ) = '                       ''NODE PRE'
     .   //             'CESSION ONLY'' or'
         MESSGE( 33 ) = '                       ''APSIDE A'
     .   //             'ND NODE PRECESSION'''
         MESSGE( 34 ) = '   '//KPLDEG//'    = polynomial'
     .   //             ' degree of Lagrange or Hermite interpolation'
         MESSGE( 35 ) = '   '//KCENGM//'         = center GM '
     .   //             'value'
         MESSGE( 36 ) = '   '//KCEPRA//'    = the right '
     .   //             'ascension of the center''s north pol'
     .   //             'e'
         MESSGE( 37 ) = '   '//KCPDEC//'   = the declin'
     .   //             'ation of the center''s north pole'
         MESSGE( 38 ) = '   '//KCENJ2//'         = center''s '
     .   //             'J2 value'
         MESSGE( 39 ) = '   '//KCNEQR//'  = center''s '
     .   //             'equatorial radius'
         MESSGE( 40 ) = '   '//KSEGID//'        = ''segment '
     .   //             'identifier'''
         MESSGE( 41 ) = '   '//KAPPND//'  = '''//YESVAL//''' or '''
     .   //              NOVAL//''''
         MESSGE( 42 ) = BEGTXT
         MESSGE( 43 ) = ' '
         MESSGE( 44 ) = '   '//KTLTID//'  = code of the object to '
     .   //             'look for in the input TLE file'
         MESSGE( 45 ) = '   '//KTLSID//'    = NAIF ID to use in the '
     .   //             'output TLE-based SPK file'
         MESSGE( 46 ) = '   '//KTLBPD//'     = ''duration units'''
         MESSGE( 47 ) = '   '//KTLEPD//'      = ''duration units'''

C
C        Check whether type of input data and output SPK file were 
C        specified after ``display-template'' command line option.
C
         IF      ( POS ( LINE, ' ' // CMLTMS // ' ', 1 ) .NE. 0 ) THEN
            CMLTMG = CMLTMS 
         ELSE IF ( POS ( LINE, ' ' // CMLTML // ' ', 1 ) .NE. 0 ) THEN
            CMLTMG = CMLTML
         END IF

C
C        First, extract input data type.
C
         CALL NEXTWD ( LINE ( ( POS (LINE, CMLTMG(:RTRIM(CMLTMG)), 1)
     .                 +  NBLEN ( CMLTMG ) ) : ), INPTYP, REST )
   
         IF ( INPTYP .EQ. ' ' ) THEN

C
C           No input/output type specified on command line. Full
C           template display.
C          
            MESSGE(  1 ) = 'Complete MKSPK Setup File Template:  '
            
            DO I = 1, 9
               CALL TOSTDO( MESSGE(I) )
            END DO
         
            CALL TOSTDO( MESSGE (8)  )
            CALL TOSTDO( MESSGE (44) )
            CALL TOSTDO( MESSGE (45) )

            DO I = 10, 28
               CALL TOSTDO( MESSGE(I) )
            END DO

            CALL TOSTDO( MESSGE (8)  )
            CALL TOSTDO( MESSGE (46) )
            CALL TOSTDO( MESSGE (29) )
            CALL TOSTDO( MESSGE (8)  )
            CALL TOSTDO( MESSGE (47) )
         
            DO I = 30, 43
               CALL TOSTDO( MESSGE(I) )
            END DO
         
            STOP  
         
         ELSE

C
C           Check whether we can recognize input data type.
C            
            IF ( .NOT. EQSTR( INPTYP, INSTTP ) .AND.
     .           .NOT. EQSTR( INPTYP, INELTP ) .AND.
     .           .NOT. EQSTR( INPTYP, INEQTP ) .AND.
     .           .NOT. EQSTR( INPTYP, INTLTP )       ) THEN

               CALL SETMSG ( 'Input data type ''#'' specified on '  //
     .                       'command line after ''#''/''#'' '      //
     .                       'option is not recognized. Supported ' //
     .                       'input data types are #, #, # and #.'   )
               CALL ERRCH  ( '#', INPTYP                             )
               CALL ERRCH  ( '#', CMLTMS                             )
               CALL ERRCH  ( '#', CMLTML                             )
               CALL ERRCH  ( '#', INSTTP                             )
               CALL ERRCH  ( '#', INELTP                             )
               CALL ERRCH  ( '#', INEQTP                             )
               CALL ERRCH  ( '#', INTLTP                             )
               CALL SIGERR ( 'SPICE(UNRECOGNDATATYPE)'               )

            END IF

         END IF

C
C        Now extract output SPK type.
C
         CALL NEXTWD ( LINE ( ( POS (LINE, INPTYP(:RTRIM(INPTYP)), 1)
     .                 + NBLEN ( INPTYP ) ) : ), OUTTYP, REST )

         IF ( OUTTYP .EQ. ' ' ) THEN  

            CALL SETMSG ( 'Both input/output types must be '         //
     .                    'specified on command line for ''#'' '     // 
     .                    'option; only input type was provided.'    )
            CALL ERRCH  ( '#',  CMLTMG                               )
            CALL SIGERR ( 'SPICE(MISSINGARGUMENTS)'                  )

         END IF
         
C
C        Convert it to integer and check whether it's a supported 
C        type.
C
         CALL NPARSI( OUTTYP, IOUTYP, ERROR, PTR )
         IF ( PTR .NE. 0 ) THEN  

            CALL SETMSG ( 'Output SPK type ''#'' provided on the '   //
     .                    'command line is not an integer number.'   )
            CALL ERRCH  ( '#', OUTTYP                                )
            CALL SIGERR ( 'SPICE(BADOUTPUTSPKTYPE)'                  )

         END IF

         IF ( IOUTYP .NE. 5
     .        .AND.
     .        IOUTYP .NE. 8
     .        .AND.
     .        IOUTYP .NE. 9
     .        .AND.
     .        IOUTYP .NE. 10
     .        .AND.
     .        IOUTYP .NE. 12
     .        .AND.
     .        IOUTYP .NE. 13
     .        .AND.
     .        IOUTYP .NE. 15 
     .        .AND.
     .        IOUTYP .NE. 17 ) THEN

            CALL SETMSG ( 'Output SPK type ''#'' provided on the '   //
     .                    'command line is not supported. Supported '//
     .                    'SPK types are: 5, 8, 9, 10, 12, 13, 15 '  //
     .                    'and 17. '                                 )
            CALL ERRCH  ( '#', OUTTYP                                )
            CALL SIGERR ( 'SPICE(BADOUTPUTSPKTYPE)'                  )

         END IF

C
C        Input/output type were specified on the command line. Display
C        template depending of input/output type.
C
         CALL UCASE( INPTYP, INPTYP )
         MESSGE (1) = 'MKSPK Setup File Template for input data type '
     .   //           INPTYP (: RTRIM(INPTYP)) // ' and output '
         MESSGE (2) = 'SPK type ' // OUTTYP
         MESSGE (5) = '   '//KINDTP//'   = '''
     .   //           INPTYP(:RTRIM(INPTYP))//''''
         MESSGE (6) = '   '//KOSPTP//'   = '//OUTTYP
         MESSGE (7) = '   '//KOBJID//'         = NAIF '
     .   //           'numeric code of the object'

         IF ( EQSTR( INPTYP, INSTTP ) .AND. IOUTYP .EQ. 5 ) THEN

C       
C           Input type is STATES, output type is 5.
C
            DO I = 1, 27
               CALL TOSTDO( MESSGE(I) )
            END DO
         
            CALL TOSTDO( MESSGE (35) )

         ELSE IF ( EQSTR( INPTYP, INSTTP ) .AND. IOUTYP .EQ. 8 ) THEN

C       
C           Input type is STATES, output type is 8.
C        
            MESSGE( 34 ) = '   '//KPLDEG//'    = polynomial'
     .      //             ' degree of Lagrange interpolation'
            DO I = 1, 27
               CALL TOSTDO( MESSGE(I) )
            END DO
        
            CALL TOSTDO( MESSGE (34) )

         ELSE IF ( EQSTR( INPTYP, INSTTP ) .AND. IOUTYP .EQ. 9 ) THEN

C       
C           Input type is STATES, output type is 9.
C
            MESSGE( 34 ) = '   '//KPLDEG//'    = polynomial'
     .      //             ' degree of Lagrange interpolation'
            DO I = 1, 27
               CALL TOSTDO( MESSGE(I) )
            END DO

            CALL TOSTDO( MESSGE (34) )

         ELSE IF ( EQSTR( INPTYP, INSTTP ) .AND. IOUTYP .EQ. 12 ) THEN

C       
C           Input type is STATES, output type is 12.
C        
            MESSGE( 34 ) = '   '//KPLDEG//'    = polynomial'
     .      //             ' degree of Hermite interpolation '
            DO I = 1, 27
               CALL TOSTDO( MESSGE(I) )
            END DO
        
            CALL TOSTDO( MESSGE (34) )

         ELSE IF ( EQSTR( INPTYP, INSTTP ) .AND. IOUTYP .EQ. 13 ) THEN

C       
C           Input type is STATES, output type is 13.
C
            MESSGE( 34 ) = '   '//KPLDEG//'    = polynomial'
     .      //             ' degree of Hermite interpolation '
            DO I = 1, 27
               CALL TOSTDO( MESSGE(I) )
            END DO

            CALL TOSTDO( MESSGE (34) )

         ELSE IF ( EQSTR( INPTYP, INSTTP ) .AND. IOUTYP .EQ. 15 ) THEN

C       
C           Input type is STATES, output type is 15.
C
            DO I = 1, 33
               CALL TOSTDO( MESSGE(I) )
            END DO

            DO I = 35, 39
               CALL TOSTDO( MESSGE(I) )
            END DO

         ELSE IF ( EQSTR( INPTYP, INSTTP ) .AND. IOUTYP .EQ. 17 ) THEN

C       
C           Input type is STATES, output type is 17.
C
            DO I = 1, 29
               CALL TOSTDO( MESSGE(I) )
            END DO

            DO I = 35, 37
               CALL TOSTDO( MESSGE(I) )
            END DO

         ELSE IF ( EQSTR( INPTYP, INELTP ) .AND. IOUTYP .EQ. 5 ) THEN

C       
C           Input type is ELEMENTS, output type is 5.
C
            DO I = 1, 27
               CALL TOSTDO( MESSGE(I) )
            END DO

            CALL TOSTDO( MESSGE (35) )

         ELSE IF ( EQSTR( INPTYP, INELTP ) .AND. IOUTYP .EQ. 8 ) THEN

C       
C           Input type is ELEMENTS, output type is 8.
C
            MESSGE( 34 ) = '   '//KPLDEG//'    = polynomial'
     .      //             ' degree of Lagrange interpolation'
            DO I = 1, 27
               CALL TOSTDO( MESSGE(I) )
            END DO

            CALL TOSTDO( MESSGE (34) )
            CALL TOSTDO( MESSGE (35) )

         ELSE IF ( EQSTR( INPTYP, INELTP ) .AND. IOUTYP .EQ. 9 ) THEN

C       
C           Input type is ELEMENTS, output type is 9.
C
            MESSGE( 34 ) = '   '//KPLDEG//'    = polynomial'
     .      //             ' degree of Lagrange interpolation'
            DO I = 1, 27
               CALL TOSTDO( MESSGE(I) )
            END DO

            CALL TOSTDO( MESSGE (34) )
            CALL TOSTDO( MESSGE (35) )

         ELSE IF ( EQSTR( INPTYP, INELTP ) .AND. IOUTYP .EQ. 12 ) THEN

C       
C           Input type is ELEMENTS, output type is 12.
C
            MESSGE( 34 ) = '   '//KPLDEG//'    = polynomial'
     .      //             ' degree of Hermite interpolation'
            DO I = 1, 27
               CALL TOSTDO( MESSGE(I) )
            END DO

            CALL TOSTDO( MESSGE (34) )
            CALL TOSTDO( MESSGE (35) )

         ELSE IF ( EQSTR( INPTYP, INELTP ) .AND. IOUTYP .EQ. 13 ) THEN

C       
C           Input type is ELEMENTS, output type is 13.
C
            MESSGE( 34 ) = '   '//KPLDEG//'    = polynomial'
     .      //             ' degree of Hermite interpolation'
            DO I = 1, 27
               CALL TOSTDO( MESSGE(I) )
            END DO

            CALL TOSTDO( MESSGE (34) )
            CALL TOSTDO( MESSGE (35) )

         ELSE IF ( EQSTR( INPTYP, INELTP ) .AND. IOUTYP .EQ. 15 ) THEN

C       
C           Input type is ELEMENTS, output type is 15.
C
            DO I = 1, 33
               CALL TOSTDO( MESSGE(I) )
            END DO

            DO I = 35, 39
               CALL TOSTDO( MESSGE(I) )
            END DO

         ELSE IF ( EQSTR( INPTYP, INELTP ) .AND. IOUTYP .EQ. 17 ) THEN

C       
C           Input type is ELEMENTS, output type is 17.
C
            DO I = 1, 29
               CALL TOSTDO( MESSGE(I) )
            END DO
   
            DO I = 35, 37
               CALL TOSTDO( MESSGE(I) )
            END DO

         ELSE IF ( EQSTR( INPTYP, INEQTP ) .AND. IOUTYP .EQ. 17 ) THEN

C       
C           Input type is EQ_ELEMENTS, output type is 17.
C
            DO I = 1, 29
               CALL TOSTDO( MESSGE(I) )
            END DO

            DO I = 36, 37
               CALL TOSTDO( MESSGE(I) )
            END DO

         ELSE IF ( EQSTR( INPTYP, INTLTP ) .AND. IOUTYP .EQ. 10 ) THEN

C       
C           Input type is TL_ELEMENTS, output type is 10.
C
            MESSGE (7) = '   '//KOBJID//'         = '
     .      //                  'TLE satellite code '        
            MESSGE (10) =  MESSGE  (10)( :RTRIM(MESSGE(10)) )
     .      //            ' (399)'
            MESSGE (12) = MESSGE (12)( :RTRIM(MESSGE(12)) )
     .      //            ' (''EARTH'')'
            MESSGE (13) = MESSGE (13)( :RTRIM(MESSGE(13))  )
     .      //            ' (''J2000'')'
            MESSGE (20) = '   '//PCKFIL//'          = '
     .      //                   '''Geophysical constants file name'''
       

            DO I = 1, 9
               CALL TOSTDO( MESSGE(I) )
            END DO
         
            CALL TOSTDO( MESSGE (8)  )
            CALL TOSTDO( MESSGE (44) )
            CALL TOSTDO( MESSGE (45) )

            DO I = 10, 14
               CALL TOSTDO( MESSGE(I) )
            END DO

            DO I = 17, 20
               CALL TOSTDO( MESSGE(I) )
            END DO

            CALL TOSTDO( MESSGE(22)  )
            CALL TOSTDO( MESSGE (28) )
            CALL TOSTDO( MESSGE (8)  )
            CALL TOSTDO( MESSGE (46) )
            CALL TOSTDO( MESSGE (29) )
            CALL TOSTDO( MESSGE (8)  )
            CALL TOSTDO( MESSGE (47) )
         
         ELSE

C
C           Well, input/output types mismatch - notify the user.
C
            CALL SETMSG ( 'You cannot generate type # SPK from '  //
     .                    '''#'' input. See MKSPK User''s Guide ' //
     .                    'for valid input/output type combinations.')
            CALL ERRINT ( '#', IOUTYP                                )
            CALL ERRCH  ( '#', INPTYP                                )
            CALL SIGERR ( 'SPICE(TYPESMISMATCH)'                     )

         END IF
         
C
C        Whatever template we just displayed, we need to display last 
C        three lines of it and stop the program.
C
         DO I = 40, 43
            CALL TOSTDO( MESSGE(I) )
         END DO   
         
         STOP

      END IF

C
C     Get setup name from command line if it is present. Flag 
C     CMLSET is declared in the include file.
C
      IF ( POS ( LINE, ' ' // CMLSET // ' ', 1 ) .EQ. 0 ) THEN

         CALL TOSTDO ( ' '                         )
         CALL PROMPT ( 'SETUP FILE NAME> ', CMDFIL )
        
      ELSE

         CALL NEXTWD ( LINE ( ( POS ( LINE, CMLSET, 1 ) +
     .                 NBLEN ( CMLSET ) ) : ), CMDFIL, REST )

      END IF

C
C     Get input file name from command line if it is present. 
C     Flag CMLINP is declared in the include file. 
C
      IF ( POS ( LINE, ' ' // CMLINP // ' ', 1 ) .EQ. 0 ) THEN

         INPFN = ' ' 
      
      ELSE

         CALL NEXTWD ( LINE ( ( POS ( LINE, CMLINP, 1 ) + 
     .                 NBLEN( CMLINP) ) : ), INPFN, REST  ) 
                   
      END IF

C
C     Get output file name from command line if it is present. Flag 
C     CMLOUT declared in the include file.
C
      IF ( POS ( LINE, ' ' // CMLOUT // ' ', 1 ) .EQ. 0 ) THEN

         OUTFN = ' ' 
       
      ELSE

         CALL NEXTWD ( LINE ( ( POS ( LINE, CMLOUT, 1 ) +
     .                 NBLEN ( CMLOUT ) ) : ), OUTFN, REST )

      END IF

C
C     Check for presence of the flag indicating that appending to
C     an existing output file was requested. CMLAPP is declared in 
C     the include file.
C
      IF ( POS ( LINE, ' ' // CMLAPP // ' ', 1 ) .NE. 0 ) THEN
         APPFLG = .TRUE. 
      ELSE
         APPFLG = .FALSE. 
      END IF

      CALL CHKOUT ( 'CMLARG' )
       
      RETURN
      
      END
