C$Procedure      SETUP ( Get setup file keyword values )

      SUBROUTINE SETUP ( FLAG, CHVAL, INTVAL, DPVAL, FOUND )

C$ Abstract
C
C     This routine is a module of the MKSPK program. It returns 
C     values of the setup file keywords loaded into the POOL.
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
C     MKSPK User's Guide
C
C$ Keywords
C
C     None.
C
C$ Declarations

      IMPLICIT              NONE
      
      INCLUDE               'mkspk.inc'

      CHARACTER*(*)         FLAG
      CHARACTER*(*)         CHVAL
      INTEGER               INTVAL
      DOUBLE PRECISION      DPVAL
      LOGICAL               FOUND

C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     FLAG       I   SETUPC, SETUPI, SETUPD, SETUPA
C     CHVAL      O   SETUPC
C     INTVAL     O   SETUPI
C     DPVAL      O   SETUPD
C     FOUND      O   SETUPC, SETUPI, SETUPD, SETUPA
C     ALLDLM     P   Complete list of input data delimiters.
C
C$ Detailed_Input
C
C     See the ENTRY points for a discussion of their arguments.
C
C$ Detailed_Output
C
C     See the ENTRY points for a discussion of their arguments.
C
C$ Parameters
C
C     ALLDLM    is a complete list of supported input data delimiters.
C
C$ Exceptions
C
C     1) If input file name was not provided neither on the
C        command line nor as a value of the setup file keyword,
C        then the error 'SPICE(NOINPUTFILENAME)' will be signaled.
C        
C     2) If input file name is blank, then the error
C        'SPICE(BLANKINPUTFILENAME)' will be signaled.
C        
C     3) If input file does not exist, then the error
C        'SPICE(INPUTFILENOTEXIST)' will be signaled.
C        
C     4) If output SPK file name was not provided neither on
C        the command line nor as a value of the setup file keyword,
C        then the error 'SPICE(NOOUTPUTFILENAME)' will be signaled.
C        
C     5) If output file name is blank, then the error
C        'SPICE(BLANKOUTPTFILENAME)' will be signaled.
C        
C     6) If append flag specified in the setup file keyword 
C        is not recognized the error 'SPICE(UNRECOGNAPPFLAG)'
C        will be signaled.
C        
C     7) If type of the input data was not specified in the
C        setup file keyword, then the error 'SPICE(NOINPUTDATATYPE)'
C        will be signaled.
C        
C     8) If input data type specified in the setup file keyword
C        is not recognized, then the error 'SPICE(UNRECOGNDATATYPE)'
C        will be signaled.
C        
C     9) If the name of the reference frame with respect to
C        which the input trajectory data are provided was not
C        specified in the setup file keyword, then the error
C        'SPICE(NOFRAMENAME)' will be signaled.
C        
C     10) If the reference frame is not recognized and the
C        name of a frames definition kernel file which could
C        contain a definition of this frame was not provided via
C        the setup file keyword, then the error 
C        'SPICE(NOFRAMESKERNELNAME)' will be signaled.
C        
C     11) If the reference frame is not recognized, then the
C        error 'SPICE(FRAMENOTRECOGNIZED)' will be signaled.
C        
C     12) If string containing producer name was not specified
C        in the setup file keyword, then the error 
C        'SPICE(NOPRODUCERID)' will be signaled.
C        
C     13) If time wrapper string specified in the setup file
C        keyword didn't contain special character indicating
C        location at which time string from the input records
C        should be inserted, then the error 'SPICE(NO#INTIMEWRAPPER)'
C        will be signaled.
C        
C     14) If the file containing comments to be inserted into
C        the comment of the output SPK file, specified in the
C        setup file keyword doesn't exist, then the error
C        'SPICE(COMMFILENOTEXIST)' will be signaled.
C        
C     15) If the order parameters in input data records was
C        not specified in the setup file keyword, then the error
C        'SPICE(NODATAORDER)' will be signaled.
C        
C     16) If character separating parameters in the input data
C        records was not specified in the setup file keyword,
C        then the error 'SPICE(NODELIMCHARACTER)' will be signaled.
C        
C     17) If delimiter specified in the setup file keyword is
C        not recognized, then the error 'SPICE(UNRECOGNDELIMITER)'
C        will be signaled.
C        
C     18) If neither object ID nor object name was specified
C        in the setup file using corresponding keywords, then
C        the error 'SPICE(NOOBJECTIDORNAME)' will be signaled.
C        
C     19) If object name specified in the setup file keyword
C        cannot be translated to NAIF ID, then the error
C        'SPICE(BADOBJECTNAME)' will be signaled.
C        
C     20) If neither center ID nor center name was specified
C        in the setup file using corresponding keywords, then
C        the error 'SPICE(NOCENTERIDORNAME)' will be signaled.
C        
C     21) If center name specified in the setup file keyword
C        cannot be translated to NAIF ID, then the error
C        'SPICE(BADCENTERNAME)' will be signaled.
C        
C     22) If output SPK file type was not specified in the
C        setup file keyword, then the error 'SPICE(NOOUTPUTSPKTYPE)'
C        will be signaled.
C        
C     23) If output SPK type specified in the setup file
C        keyword is not supported in this version of the program,
C        then the error 'SPICE(SPKTYPENOTSUPPORTD)' will be
C        signaled.
C        
C     24) If number of lines which a single input file record
C        occupies was not specified in the setup file keyword
C        as an integer value, then the error
C        'SPICE(NOLINESPERRECCOUNT)' will be signaled.
C        
C     25) If number of lines, which a single input file record
C        occupies, specified in the setup file keyword is not a
C        positive integer number or word, then the error
C        'SPICE(BADLINEPERRECCOUNT)' will be signaled.
C        
C     26) If polynomial degree was not specified in the setup
C        file keyword, then the error 'SPICE(NOPOLYNOMIALDEGREE)'
C        will be signaled.
C        
C     27) If orbit precession type was not specified in the
C        setup file keyword, then the error 'SPICE(NOPRECESSIONTYPE)'
C        will be signaled.
C        
C     28) If precession type specified in the setup file
C        keyword is not recognized, then the error
C        'SPICE(UNRECOGNPRECTYPE)' will be signaled.
C        
C     29) If leapsecond file name was not specified in the
C        setup file keyword, then the error 'SPICE(NOLSKFILENAME)'
C        will be signaled.
C        
C     30) If PCK file specified in the setup file keyword is
C        not a text or binary PCK file, then the error
C        'SPICE(NOTAPCKFILE)' will be signaled.
C        
C     31) In case of a programming error by NAIF, a fail-safe error
C        in the form 'SPICE(MKSPKBUGSETUP#)' will be signaled.
C
C     32) If the first item in a TLE coverage pad keyword value is not
C        a number, then the error 'SPICE(BADTLECOVERAGEPAD2)' will be
C        signaled.
C
C     33) If a TLE coverage pad value is not exactly two items, a
C        number and a word, then the error 'SPICE(BADTLECOVERAGEPAD)'
C        will be signaled.
C
C     34) If the duration in a TLE coverage pad keyword value is 
C        negative, then the error 'SPICE(BADTLECOVERAGEPAD3)' will be
C        signaled.
C
C$ Files
C
C     See the ENTRY points for a details.
C
C$ Particulars
C
C     SETUP should never be called directly, but should instead be
C     accessed only through its entry points. If SETUP is called 
C     directly a 'SPICE(BOGUSENTRY)' error will be signaled. 
C
C     The purpose of this routine is to get the values of the setup 
C     file keywords that were loaded into the POOL. The following 
C     entry points should be used to get, process and return
C     specified value to the main program:
C
C           SETUPC   Return a string values 
C
C           SETUPI   Return an integer values
C
C           SETUPD   Return a d.p. value  
C
C           SETUPA   Get LSK and PCK file names and load its 
C                    contents.
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
C     E.D. Wright    (NAIF)
C
C$ Version
C
C-    Version 1.2.0, 24-FEB-2012 (BVS)
C
C        Added processing of TLE coverage and ID keywords.
C
C-    Version 1.1.0, 08-APR-2004 (EDW)
C
C        Replaced all LDPOOL calls with FURNSH.
C
C-    Version 1.0.8, 21-MAR-2001 (BVS)
C
C        Added loading of the frames kernel for the object and 
C        center cases. FK may contain id-name mapping that is
C        needed for resolving the object/center names. Added 
C        processing of 'APPEND FLAG' case to SETUPC entry point.
C        Also, in SETUPC, eliminated check for existence of 
C        output SPK. It's now done in the calling program.
C
C-    Version 1.0.7, 19-JAN-2000 (BVS)
C
C        Added code to recognize SPK types 12 and 13. 
C
C-    Version 1.0.6, 25-NOV-1999 (NGK)
C
C        Added input/output types processing for case of two-line 
C        elements. 
C
C-    Version 1.0.5, 29-MAR-1999 (NGK)
C
C        Corrected comments.
C
C-    Version 1.0.4, 21-MAR-1999 (BVS)
C
C        Removed all warning messages.
C
C-    Version 1.0.3, 19-MAR-1999 (BVS)
C
C        Changed type of the LINES_PER_RECORD value in entry point 
C        SETUPI to integer.
C
C-    Version 1.0.2, 15-MAR-1999 (BVS)
C
C        Corrected comments. Added "bogus entry" error message if 
C        SETUP is called directly.
C   
C-    Version 1.0.1, 13-JAN-1999 (BVS)
C
C        Modified error and warning messages.
C   
C-    Version 1.0.0, 08-SEP-1998 (NGK)
C   
C-&

C$ Index_Entries
C
C     Return a value of a MKSPK setup file keyword
C
C-&

C
C     SPICELIB functions
C
      INTEGER               POS
      INTEGER               LASTNB
      INTEGER               RTRIM
      INTEGER               ISRCHC
      INTEGER               WDCNT
      LOGICAL               EXISTS
      LOGICAL               RETURN

C
C     Local parameters
C
      CHARACTER*(*)         NOPREC
      PARAMETER           ( NOPREC = 'NO PRECESSION' )

      CHARACTER*(*)         APSPRC
      PARAMETER           ( APSPRC = 'APSIDE PRECESSION ONLY' )

      CHARACTER*(*)         NODPRC
      PARAMETER           ( NODPRC = 'NODE PRECESSION ONLY' )

      CHARACTER*(*)         APNOPR
      PARAMETER           ( APNOPR = 'APSIDE AND NODE PRECESSION' )

      CHARACTER*(*)         EOLMRK
      PARAMETER           ( EOLMRK = '#' ) 

C     
C
C     Local variables
C
C
C     Input and output file names length.
C
      INTEGER               FNMLEN
      
C
C     Generic line variables. Sizes LINLEN, FILSIZ, SHRTLN, VALUEL 
C     declared in the include file.
C          
      CHARACTER*( LINLEN )  WRKCHR
      CHARACTER*( FILSIZ )  LSKFN
      CHARACTER*( FILSIZ )  FRKFN
      CHARACTER*( SHRTLN )  OBJNVL
      CHARACTER*( SHRTLN )  CENNVL
      CHARACTER*( SHRTLN )  ANGUNT
      CHARACTER*( SHRTLN )  DSTUNT
      CHARACTER*( SHRTLN )  STTMVL
      CHARACTER*( SHRTLN )  SPTMVL 
      CHARACTER*( SHRTLN )  PRCTVL
      CHARACTER*( SHRTLN )  ARCH
      CHARACTER*( SHRTLN )  TYPE
      CHARACTER*( SHRTLN )  PADVL
      CHARACTER*( SHRTLN )  CHRDP
      CHARACTER*( SHRTLN )  KEYWRD
      CHARACTER*( SHRTLN )  UNITS
      CHARACTER*( VALUEL )  VALUE
      
C
C     Array of all reserved delimiter that may appear in input
C     data. Size and dimension declared in the include file.
C
      CHARACTER*( DLMSIZ )  ALLDLM ( ALLNMB )

C
C     Binary PCK  handling
C
      INTEGER               HANDLE

C
C     The remaining local variables
C
      DOUBLE PRECISION      WRKDP
      LOGICAL               FLOADD
      LOGICAL               EXIST
      INTEGER               REFCOD
      INTEGER               N
      INTEGER               I

C
C     Assign  all value chose as delimiter 
C
      SAVE                  ALLDLM
      SAVE                  FLOADD

      DATA ALLDLM         / 'TAB' , 'EOL' , ',' , ' ' , ';' /
      DATA FLOADD         / .FALSE. /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SETUP' )
      END IF
 
C
C     This routine should never be called. If this routine is called,
C     an error is signaled.
C
      CALL SETMSG ( 'SETUP: You have called an entry which '      //
     .              'performs performs no run-time function. '    //
     .              'This may indicate a bug'                     ) 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'SETUP' )
      RETURN


C$Procedure   SETUPC ( Return string values to main program )

      ENTRY SETUPC ( FLAG, CHVAL, FOUND )
      
C$ Abstract
C
C     Return character string value of a particular setup file 
C     keyword.
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
C     KERNEL
C
C$ Keywords
C
C     KERNEL
C     POOL
C
C$ Declarations
C      
C     CHARACTER*(*)         FLAG
C     CHARACTER*(*)         CHVAL
C     LOGICAL               FOUND
C      
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FLAG       I   Flag of keyword
C     CHVAL      O   Returned string value
C     FOUND      O   True if variable is in pool.
C
C$ Detailed_Input
C
C     FLAG        is the string containing text identifier for the 
C                 keyword of interest.
C                
C$ Detailed_Output
C
C     CHVAL       is string value of the keyword
C
C     FOUND       is TRUE if the value was obtained from the loaded 
C                 setup file keyword(s).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     See main routine header for complete list of exceptions.   
C           
C$ Files
C
C     None.
C     
C$ Particulars
C
C     None.
C      
C$ Examples
C      
C     None
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
C-    Version 1.0.7, 21-MAR-2001 (BVS) 
C
C        Added processing of 'APPEND FLAG' case. Changed FK
C        loading logic in 'REFERENCE FRAME NAME' case. Eliminated
C        check for existence of output SPK. It's now done in the 
C        calling program.
C
C-    Version 1.0.6, 25-NOV-1999 (NGK)
C
C        Added input type processing for case of two-line elements.
C
C-    Version 1.0.5, 29-MAR-1999 (NGK)
C
C        Corrected comments.
C
C-    Version 1.0.4, 21-MAR-1999 (BVS)
C
C        Removed all warning messages.
C
C-    Version 1.0.2, 15-MAR-1999 (BVS)
C
C        Corrected comments.
C
C-    Version 1.0.1, 13-JAN-1999 (BVS)
C
C        Modified error and warning messages.
C   
C-    Version 1.0.0, 8-SEP-1998 (NGK)
C   
C-&

C$ Index_Entries
C
C     Return a string value of a MKSPK setup file keyword
C
C-&  


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SETUPC' )
      END IF

      IF ( FLAG .EQ. 'INPUT FILE NAME' ) THEN

C
C        Retrieve INPUT_DATA_FILE value (if it wasn't provided
C        on command line and therefore wasn't assigned to CHVAL).
C        Parameter KINDFN declared in include file.
C
         IF ( CHVAL .EQ. ' ' ) THEN

            CALL GCPOOL ( KINDFN, 1, 1, N, CHVAL,  FOUND )

            IF ( .NOT. FOUND ) THEN

               CALL SETMSG ( 'Input file name was not provided '     //
     .                       'neither on the command line nor as '   //
     .                       'a value of the setup file keyword '    //
     .                       '''#''.'                                )
               CALL ERRCH  ( '#', KINDFN                             )
               CALL SIGERR ( 'SPICE(NOINPUTFILENAME)'                )

            END IF

         END IF

         CALL LJUST ( CHVAL, CHVAL )
         FNMLEN = LASTNB ( CHVAL )

         IF ( FNMLEN .EQ. 0 ) THEN

            CALL SETMSG ( 'Input file name is blank.'          )
            CALL SIGERR ( 'SPICE(BLANKINPUTFILENAME)'          )

         END IF

         IF( .NOT. EXISTS (CHVAL ( : FNMLEN ) ) ) THEN
   
            CALL SETMSG ( 'Input file ''#'' does not exist. '  )
            CALL ERRCH  ( '#', CHVAL                           )
            CALL SIGERR ( 'SPICE(INPUTFILENOTEXIST)'           )

         END IF

      ELSE IF ( FLAG .EQ. 'OUTPUT FILE NAME' ) THEN

C
C        Retrieve OUTPUT_SPK_FILE value (if it wasn't provided on
C        the command line and therefore wasn't assigned to CHVAL)
C        Parameter KOSPFN declared in include file.
C
         IF ( CHVAL .EQ. ' ' ) THEN

            CALL GCPOOL ( KOSPFN, 1, 1, N, CHVAL,  FOUND )

            IF ( .NOT. FOUND ) THEN

               CALL SETMSG ( 'Output SPK file name was not '         //
     .                       'provided neither on the command '      //
     .                       'line nor as a value of the setup '     //
     .                       'file keyword ''#''.'                   )
               CALL ERRCH  ( '#', KOSPFN                             )
               CALL SIGERR ( 'SPICE(NOOUTPUTFILENAME)'               )

            END IF

         END IF

         CALL LJUST ( CHVAL, CHVAL)
         FNMLEN = LASTNB ( CHVAL )

         IF ( FNMLEN .EQ. 0 ) THEN

            CALL SETMSG ( 'Output file name is blank.' )
            CALL SIGERR ( 'SPICE(BLANKOUTPTFILENAME)'  )

         END IF

      ELSE IF ( FLAG .EQ. 'INPUT DATA TYPE' ) THEN

C
C        Retrieve INPUT_DATA_TYPE value and check it.
C        Parameter KINDTP declared in include file.
C
         CALL GCPOOL ( KINDTP, 1, 1, N, CHVAL,  FOUND )
         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Type of the input data was not '          //
     .                    'specified in the setup file keyword '     //
     .                    '''#''.'                                   )
            CALL ERRCH  ( '#', KINDTP                                )
            CALL SIGERR ( 'SPICE(NOINPUTDATATYPE)'                   )

         END IF

         CALL UCASE ( CHVAL, CHVAL )
         CALL LJUST ( CHVAL, CHVAL )

         IF ( CHVAL( : RTRIM ( CHVAL )) .NE. INSTTP
     .        .AND.
     .        CHVAL( : RTRIM ( CHVAL )) .NE. INELTP
     .        .AND.
     .        CHVAL( : RTRIM ( CHVAL )) .NE. INEQTP 
     .        .AND.
     .        CHVAL( : RTRIM ( CHVAL )) .NE. INTLTP  ) THEN

            CALL SETMSG ( 'Input data type ''#'' specified in the '  //
     .                    'setup file keyword ''#'' is not '         //
     .                    'recognized. Refer to the User''s Guide '  //
     .                    'for the program for the list of '         //
     .                    'supported input data types.'              )
            CALL ERRCH  ( '#', CHVAL                                 )
            CALL ERRCH  ( '#', KINDTP                                )
            CALL SIGERR ( 'SPICE(UNRECOGNDATATYPE)'                  )

         END IF

      ELSE IF ( FLAG .EQ. 'REFERENCE FRAME NAME' ) THEN

C
C        Retrieve REF_FRAME_NAME value.
C        Parameter KRFRNM declared in include file.
C
         CALL GCPOOL ( KRFRNM, 1, 1, N, CHVAL, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'The name of the reference frame with '    //
     .                    'respect to which the input trajectory '   //
     .                    'data are provided was not specified in '  //
     .                    'the setup file keyword ''#''.'            )
            CALL ERRCH  ( '#', KRFRNM                                )
            CALL SIGERR ( 'SPICE(NOFRAMENAME)'                       )

         END IF

         CALL LJUST ( CHVAL, CHVAL )

C
C        Get the NAIF integer code for the reference frame.
C
         CALL  NAMFRM ( CHVAL, REFCOD )

         IF ( REFCOD .EQ. 0 ) THEN

C
C           Frame name was not recognized. See if frames file has 
C           already been loaded. 
C
            IF ( .NOT. FLOADD ) THEN

C
C              It hasn't. Retrieve FRAME_DEFINITION file name and 
C              load it. Keyword FRKFIL declared in include file.
C
               CALL GCPOOL ( FRKFIL, 1, 1, N, FRKFN,  FOUND )

               IF ( .NOT. FOUND ) THEN

                  CALL SETMSG ( 'The reference frame ''#'' is not '  //
     .                       'recognized and the name of a frames '  //
     .                       'definition kernel file which could '   //
     .                       'contain a definition for this frame '  //
     .                       'was not provided via the setup file '  //
     .                       'keyword ''#''.'                        )
                  CALL ERRCH  ( '#', CHVAL                           )
                  CALL ERRCH  ( '#', FRKFIL                          )
                  CALL SIGERR ( 'SPICE(NOFRAMESKERNELNAME)'          )

               END IF

               CALL LJUST  ( FRKFN, FRKFN )
               CALL FURNSH ( FRKFN )
               FLOADD = .TRUE.

C
C              Try to resolve frame ID again.
C
               CALL NAMFRM ( CHVAL, REFCOD )

            END IF

C
C           Has loading FK file helped? If not, signal an error.
C
            IF ( REFCOD .EQ. 0 ) THEN

               CALL SETMSG ( 'The reference frame ''#'' is not '     //
     .                       'recognized. Check whether the frames ' //
     .                       'definition kernel file ''#'' '         //
     .                       'specified in the setup file '          //
     .                       'keyword ''#'' contains definition '    //
     .                       'for this frame.'                       )
               CALL ERRCH  ( '#', CHVAL                              )
               CALL ERRCH  ( '#', FRKFN                              )
               CALL ERRCH  ( '#', FRKFIL                             )
               CALL SIGERR ( 'SPICE(FRAMENOTRECOGNIZED)'             )

            END IF

         END IF

      ELSE IF ( FLAG .EQ. 'PRODUCER ID' ) THEN

C
C        Retrieve PRODUCER_ID value. Parameter KPROID declared in 
C        include file.
C
         CALL GCPOOL ( KPROID , 1, 1, N, CHVAL, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'String containing producer name was not ' //
     .                    'specified in the setup file keyword '     //
     .                    '''#''.'                                   )
            CALL ERRCH  ( '#', KPROID                                )
            CALL SIGERR ( 'SPICE(NOPRODUCERID)'                      )

         END IF

         CALL LJUST ( CHVAL, CHVAL )
         
      ELSE IF ( FLAG .EQ. 'TIME WRAPPER' ) THEN

C
C        Retrieve TIME_WRAPPER value. Parameter KTIMWR declared in 
C        include file. 
C
         CALL GCPOOL ( KTIMWR , 1, 1, N, CHVAL, FOUND )

         IF ( FOUND ) THEN 
         
            IF ( POS ( CHVAL, '#', 1 ) .EQ. 0 ) THEN
            
               CALL SETMSG ( 'Time wrapper string specified in '     //
     .                       'the setup file keyword ''#'' didn''t ' //
     .                       'contain special character ''#'' '      //
     .                       'indicating location at which time '    //
     .                       'string from the input records '        //
     .                       'should be inserted.'                   )
               CALL ERRCH  ( '#', KTIMWR                             )
               CALL SIGERR ( 'SPICE(NO#INTIMEWRAPPER)'               )
            
            END IF
            
         ELSE
            
            CHVAL = '#'

         END IF

         CALL LJUST ( CHVAL, CHVAL )

      ELSE IF ( FLAG .EQ. 'SEGMENT ID' ) THEN

C
C        Retrieve SEGMENT_ID value. Parameter KSEGID declared in 
C        include file.
C
         CALL GCPOOL ( KSEGID , 1, 1, N, CHVAL, FOUND )

         IF ( FOUND ) THEN

            CALL LJUST ( CHVAL, CHVAL )

         END IF

      ELSE IF ( FLAG .EQ. 'COMMENT FILE NAME' ) THEN

C
C        Retrieve COMMENT_FILE name value. Parameter CMTFIL declared
C        in include file.
C
         CALL GCPOOL (CMTFIL , 1, 1, N, CHVAL, FOUND )

         IF ( FOUND ) THEN

            CALL LJUST ( CHVAL, CHVAL )
            FNMLEN = LASTNB ( CHVAL )

C
C           Check if the comment file exists.
C
            IF ( .NOT. EXISTS ( CHVAL ( : FNMLEN ) ) ) THEN

               CALL SETMSG ( 'The file ''#'' containing comments '   //
     .                       'to be inserted into the comment '      //
     .                       'of the output SPK file, specified '    //
     .                       'in the setup file keyword ''#'' '      //
     .                       'doesn''t exist.'                       )
               CALL ERRCH  ( '#', CHVAL                              )
               CALL ERRCH  ( '#', CMTFIL                             )
               CALL SIGERR ( 'SPICE(COMMFILENOTEXIST)'               )

            END IF

         END IF 
         
      ELSE IF ( FLAG .EQ. 'DATA ORDER' ) THEN
      
C
C        Retrieve DATA_ORDER value. Parameter KDATOR declared
C        in include file.  
C
         CALL GCPOOL ( KDATOR , 1, 1, N, CHVAL, FOUND )
      
         IF ( .NOT. FOUND ) THEN 
      
            CALL SETMSG ( 'The order of parameters in input data '   //
     .                    'file records was not specified in the '   //
     .                    'setup file keyword ''#''.'                )
            CALL ERRCH  ( '#', KDATOR                                )
            CALL SIGERR ( 'SPICE(NODATAORDER)'                       ) 
         
         END IF 
      
         CALL LJUST ( CHVAL, CHVAL ) 
         CALL UCASE ( CHVAL, CHVAL )

      ELSE IF ( FLAG .EQ. 'DATA DELIMITER' ) THEN 
        
C
C        Retrieve DATA_DELIMITER value and check it. Parameter KDATDL
C        declared in include file. 
C
         CALL GCPOOL (KDATDL , 1, 1, N, VALUE, FOUND )
         
         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Character separating parameters in '      //
     .                    'the input data records was not '          //
     .                    'specified in the setup file keyword '     //
     .                    '''#''.'                                   )  
            CALL ERRCH  ( '#', KDATDL                                )
            CALL SIGERR ( 'SPICE(NODELIMCHARACTER)'                  )

         END IF 

         CALL LJUST ( VALUE, VALUE )
         CALL UCASE ( VALUE, VALUE )

         IF ( ISRCHC ( VALUE, 5, ALLDLM ) .EQ. 0 ) THEN

C
C           Delimiter is not allowed. Error will be signaled.
C        
            CALL SETMSG ( 'Delimiter ''#'' specified in the setup '  //
     .                    'file keyword ''#'' is not recognized. '  //
     .                    'Refer to the User''s Guide for the '      //
     .                    'program for the list of supported '       //
     .                    'input data delimiters.'                   )
            CALL ERRCH  ( '#', VALUE                                 )
            CALL ERRCH  ( '#', KDATDL                                )
            CALL SIGERR ( 'SPICE(UNRECOGNDELIMITER)'                 )
         
         END IF
       
         CHVAL = VALUE  

C
C        Delimiter is 'EOL'. It will be replaced with EOLMRK 
C        marker on buffer lines.
C                 
         IF ( VALUE .EQ. ALLDLM (2) ) THEN
       
            CHVAL = EOLMRK  
         
         END IF
      
C
C        Delimiter is 'TAB'. It will be replaced with symbol CHAR (9) 
C        on buffer lines.
C                 
         IF ( VALUE .EQ. ALLDLM (1) ) THEN
       
            CHVAL = CHAR (9)  
         
         END IF

      ELSE IF ( FLAG .EQ. 'APPEND FLAG' ) THEN

C
C        Retrieve APPEND_TO_OUTPUT value and check it.
C        Parameter KAPPND declared in the include file.
C
         CALL GCPOOL ( KAPPND, 1, 1, N, CHVAL, FOUND )

         IF ( FOUND ) THEN

            CALL UCASE ( CHVAL, CHVAL )
            CALL LJUST ( CHVAL, CHVAL )

            IF ( CHVAL( : RTRIM ( CHVAL )) .NE. YESVAL
     .           .AND.
     .           CHVAL( : RTRIM ( CHVAL )) .NE. NOVAL  ) THEN

               CALL SETMSG ( 'Append flag ''#'' specified in the '   //
     .                       'setup file keyword ''#'' is not '      //
     .                       'recognized. Refer to the User''s '     //
     .                       'Guide for the program for the list '   //
     .                       'of supported values.'                  )
               CALL ERRCH  ( '#', CHVAL                              )
               CALL ERRCH  ( '#', KAPPND                             )
               CALL SIGERR ( 'SPICE(UNRECOGNAPPFLAG)'                )

            END IF

         ELSE

C
C           Well, if it wasn't provided, then it's 'NO' :-)
C
            CHVAL = NOVAL
            FOUND = .TRUE.

         END IF

      ELSE

         CALL SIGERR ( 'SPICE(MKSPKBUGSETUP1)' )

      END IF

      CALL CHKOUT ( 'SETUPC' )

      RETURN


C$Procedure   SETUPI ( Return integer values to main program )

      ENTRY SETUPI ( FLAG, INTVAL, FOUND )
      
C$ Abstract
C
C     Return integer value of a particular setup file keyword.
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
C     KERNEL
C
C$ Keywords
C
C     KERNEL
C     POOL
C
C$ Declarations
C      
C     CHARACTER*(*)         FLAG
C     CHARACTER*(*)         INTVAL
C     LOGICAL               FOUND
C      
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FLAG       I   Flag of keyword
C     INTVAL     O   Returned integer value
C     FOUND      O   True if variable is in pool.
C
C$ Detailed_Input
C
C     FLAG        is the string containing text identifier for the 
C                 keyword of interest.
C                
C$ Detailed_Output
C
C     INTVAL      is integer value of the keyword
C
C     FOUND       is TRUE if the value was obtained from the loaded 
C                 setup file keyword(s).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     See main routine header for complete list of exceptions.   
C           
C$ Files
C
C     None.
C     
C$ Particulars
C
C     None.
C      
C$ Examples
C      
C     None
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
C-    Version 1.0.9, 12-FEB-2012 (BVS)
C
C        Added fetching input and output IDs for TLEs. 
C
C-    Version 1.0.8, 21-MAR-2001 (BVS)
C
C        Added FK loading to object and center ID cases. 
C
C-    Version 1.0.7, 19-JAN-2000 (BVS)
C
C        Added code to recognize SPK types 12 and 13. 
C
C-    Version 1.0.6, 25-NOV-1999 (NGK)
C
C        Added output type processing for case of two-line element.
C
C-    Version 1.0.5, 29-MAR-1999 (NGK)
C
C        Corrected comments.
C
C-    Version 1.0.4, 21-MAR-1999 (BVS)
C
C        Removed all warning messages.
C
C-    Version 1.0.3, 19-MAR-1999 (BVS)
C
C        Changed type of the LINES_PER_RECORD value to integer.
C
C-    Version 1.0.2, 15-MAR-1999 (BVS)
C
C        Corrected comments.
C
C-    Version 1.0.1, 13-JAN-1999 (BVS)
C
C        Modified error and warning messages.
C   
C-    Version 1.0.0, 8-SEP-1998 (NGK)
C   
C-&

C$ Index_Entries
C
C      Return an integer value of a MKSPK setup file keyword
C
C-&  


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SETUPI' )
      END IF

      IF      ( FLAG .EQ. 'OBJECT ID CHECK' ) THEN 

C
C        Check if object ID is provided.
C
         CALL GIPOOL ( KOBJID, 1, 1, N, INTVAL, FOUND )

      ELSE IF ( FLAG .EQ. 'OBJECT NAME CHECK' ) THEN 

C
C        Check if object name is provided and can be resolved to ID.
C
         CALL GCPOOL ( KOBJNM, 1, 1, N, OBJNVL, FOUND )

         IF ( FOUND ) THEN

            CALL BODN2C ( OBJNVL, INTVAL, FOUND )

            IF ( .NOT. FOUND ) THEN

C
C              If FK was not loaded, load it and try to resolve the
C              name again.
C
               IF ( .NOT. FLOADD ) THEN

                  CALL GCPOOL ( FRKFIL, 1, 1, N, FRKFN, FOUND )

                  IF ( FOUND ) THEN
                     CALL LJUST  ( FRKFN, FRKFN )
                     CALL FURNSH ( FRKFN )
                     FLOADD = .TRUE.
                  END IF

                  CALL BODN2C ( OBJNVL, INTVAL, FOUND )

               END IF

            END IF

         END IF

      ELSE IF ( FLAG .EQ. 'OBJECT ID' ) THEN

C
C        Retrieve OBJECT_ID value or retrieve OBJECT_NAME value and
C        convert them to object ID code. Parameters KOBJID and KOBJNM
C        declared in include file.
C
         CALL GIPOOL ( KOBJID, 1, 1, N, INTVAL, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL GCPOOL ( KOBJNM, 1, 1, N, OBJNVL, FOUND )

            IF ( .NOT. FOUND ) THEN

               CALL SETMSG ( 'Neither object ID nor object name '    //
     .                       'was specified in the setup file '      //
     .                       'using ''#'' or ''#'' keyword.'          )
               CALL ERRCH  ( '#', KOBJID                              )
               CALL ERRCH  ( '#', KOBJNM                              )
               CALL SIGERR ( 'SPICE(NOOBJECTIDORNAME)'                )

            END IF

            CALL LJUST  ( OBJNVL, OBJNVL )
            CALL BODN2C ( OBJNVL, INTVAL, FOUND )

            IF ( .NOT. FOUND ) THEN

C
C              We couldn't resolve the name. What if name-id mapping
C              is provided in the frames kernel that we haven't loaded 
C              yet?
C
               IF ( .NOT. FLOADD ) THEN

                  CALL GCPOOL ( FRKFIL, 1, 1, N, FRKFN, FOUND )

                  IF ( FOUND ) THEN
                     CALL LJUST  ( FRKFN, FRKFN )
                     CALL FURNSH ( FRKFN )
                     FLOADD = .TRUE.
                  END IF

C
C                 If FK was provided, we loaded it; can the object name 
C                 be resolved now?
C
                  CALL BODN2C ( OBJNVL, INTVAL, FOUND )

               END IF

C
C              Check found flag again to see if loading FK helped.
C
               IF ( .NOT. FOUND ) THEN

                  CALL SETMSG ( 'Object name ''#'' specified in the '//
     .                          'setup file keyword ''#'' cannot be '//
     .                          'translated to NAIF ID.'             )
                  CALL ERRCH  ( '#', OBJNVL                          )
                  CALL ERRCH  ( '#', KOBJNM                          )
                  CALL SIGERR ( 'SPICE(BADOBJECTNAME)'               )

               END IF
 
            END IF

         END IF
 
      ELSE IF ( FLAG .EQ. 'CENTER ID' ) THEN

C
C        Retrieve CENTER_ID value or retrieve CENTER_NAME value and
C        convert them to center ID code. Parameters KCENID and KCENNM
C        declared in include file.
C
         CALL GIPOOL ( KCENID, 1, 1, N, INTVAL, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL GCPOOL ( KCENNM, 1, 1, N, CENNVL, FOUND )

            IF ( .NOT. FOUND ) THEN

               CALL SETMSG ( 'Neither center ID nor center name '    //
     .                       'was specified in the setup file '      //
     .                       'using ''#'' or ''#'' keyword.'         )
               CALL ERRCH  ( '#', KCENID                             )
               CALL ERRCH  ( '#', KCENNM                             )
               CALL SIGERR ( 'SPICE(NOCENTERIDORNAME)'               )

            END IF

            CALL LJUST  ( CENNVL, CENNVL )
            CALL BODN2C ( CENNVL, INTVAL, FOUND )

            IF ( .NOT. FOUND ) THEN

C
C              We couldn't resolve the name. What if name-id mapping
C              is provided in the frames kernel that we haven't loaded 
C              yet?
C
               IF ( .NOT. FLOADD ) THEN

                  CALL GCPOOL ( FRKFIL, 1, 1, N, FRKFN, FOUND )

                  IF ( FOUND ) THEN
                     CALL LJUST  ( FRKFN, FRKFN )
                     CALL FURNSH ( FRKFN )
                     FLOADD = .TRUE.
                  END IF

C
C                 If FK was provided, we loaded it; can the center name 
C                 be resolved now?
C
                  CALL BODN2C ( CENNVL, INTVAL, FOUND )

               END IF

C
C              Check found flag again to see if loading FK helped.
C
               IF ( .NOT. FOUND ) THEN

                  CALL SETMSG ( 'Center name ''#'' specified in the '//
     .                          'setup file keyword ''#'' cannot be '//
     .                          'translated to NAIF ID.'             )
                  CALL ERRCH  ( '#', CENNVL                          )
                  CALL ERRCH  ( '#', KCENNM                          )
                  CALL SIGERR ( 'SPICE(BADCENTERNAME)'               )

               END IF
 
            END IF

         END IF 
         
      ELSE IF ( FLAG .EQ. 'OUTPUT SPK TYPE' ) THEN

C
C        Retrieve OUTPUT_SPK_TYPE  value and check it. Parameter KOSPTP
C        declared in include file.
C
         CALL GIPOOL ( KOSPTP, 1, 1, N, INTVAL, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Output SPK file type was not specified '  //
     .                    'in the setup file keyword ''#''.'         )
            CALL ERRCH  ( '#', KOSPTP                                )
            CALL SIGERR ( 'SPICE(NOOUTPUTSPKTYPE)'                   )

         END IF

         IF ( INTVAL .NE. 5
     .        .AND.
     .        INTVAL .NE. 8
     .        .AND.
     .        INTVAL .NE. 9
     .        .AND.
     .        INTVAL .NE. 10
     .        .AND.
     .        INTVAL .NE. 12
     .        .AND.
     .        INTVAL .NE. 13
     .        .AND.
     .        INTVAL .NE. 15 
     .        .AND.
     .        INTVAL .NE. 17 ) THEN

            CALL SETMSG ( 'Output SPK type ''#'' specified in the '  //
     .                    'setup file keyword ''#'' is not '         //
     .                    'supported in this version of the '        //
     .                    'program. '                                )
            CALL ERRINT ( '#', INTVAL                                )
            CALL ERRCH  ( '#', KOSPTP                                )
            CALL SIGERR ( 'SPICE(SPKTYPENOTSUPPORTD)'                )

         END IF

      ELSE IF ( FLAG .EQ. 'IGNORE FIRST LINES' ) THEN 

C
C        Retrieve IGNORE_FIRST_LINES value.Parameter KIGNFL declared 
C        in include file. 
C
         CALL GIPOOL ( KIGNFL, 1, 1, N, INTVAL, FOUND )

      ELSE IF ( FLAG .EQ. 'LINES PER RECORD' ) THEN 

C
C        Retrieve LINES_PER_RECORD value. Parameter KLNPRC declared 
C        in include file.  
C
         CALL GIPOOL ( KLNPRC, 1, 1, N, INTVAL, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Number of lines which a single input '    //
     .                    'data file record occupies was not '       //
     .                    'specified in the setup file keyword '     //
     .                    '''#'' as an integer value.'               )
            CALL ERRCH  ( '#', KLNPRC                                )
            CALL SIGERR ( 'SPICE(NOLINESPERRECCOUNT)'                )

         ELSE

            IF ( INTVAL .LE. 0 ) THEN 

C
C              Provided number of lines per record is less than
C              or equal to zero: how are we supposed to deal with
C              this???
C                     
               CALL SETMSG ( 'Number of lines, which a single '   //
     .                       'input file record occupies ''#'', ' //
     .                       'specified in the setup file '       //
     .                       'keyword ''#'' is not a positive '   //
     .                       'integer number . '                  )
               CALL ERRCH  ( '#', WRKCHR                          )  
               CALL ERRCH  ( '#', KLNPRC                          )  
               CALL SIGERR ( 'SPICE(BADLINEPERRECCOUNT)'          )
                  
            END IF 
         
         END IF
            
      ELSE IF ( FLAG .EQ. 'TIME STRING LENGTH' ) THEN 

C
C        Retrieve EPOCH_STR_LENGTH value. Parameter KELENG declared 
C        in include file. 
C
         CALL GIPOOL ( KELENG , 1, 1, N, INTVAL, FOUND )

      ELSE IF ( FLAG .EQ. 'POLYNOMIAL DEGREE' ) THEN
      
C
C        Retrieve POLYNOM_DEGREE value. Parameter KPLDEG declared 
C        in include file.  
C
         CALL GIPOOL ( KPLDEG, 1, 1, N, INTVAL, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Polynomial degree was not specified '     //
     .                    'in the setup file keyword ''#''.'         )
            CALL ERRCH  ( '#', KPLDEG                                )
            CALL SIGERR ( 'SPICE(NOPOLYNOMIALDEGREE)'                )

         END IF

      ELSE IF ( FLAG .EQ. 'TLE INPUT OBJECT ID' ) THEN 

C
C        Retrieve TLE_INPUT_OBJECT_ID value. Parameter KTLTID declared 
C        in the include file. 
C
         CALL GIPOOL ( KTLTID, 1, 1, N, INTVAL, FOUND )

      ELSE IF ( FLAG .EQ. 'TLE SPK OBJECT ID' ) THEN 

C
C        Retrieve TLE_SPK_OBJECT_ID value. Parameter KTLSID declared 
C        in the include file. 
C
         CALL GIPOOL ( KTLSID, 1, 1, N, INTVAL, FOUND )

      ELSE

         CALL SIGERR ( 'SPICE(MKSPKBUGSETUP2)')

      END IF

      CALL CHKOUT ( 'SETUPI' )

      RETURN


C$Procedure   SETUPD ( Return d.p. values to main program )

      ENTRY SETUPD ( FLAG, DPVAL, FOUND )
      
C$ Abstract
C
C     Return double precision value of a particular setup file 
C     keyword.
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
C     KERNEL
C
C$ Keywords
C
C     KERNEL
C     POOL
C
C$ Declarations
C      
C     CHARACTER*(*)         FLAG
C     CHARACTER*(*)         DPVAL
C     LOGICAL               FOUND
C      
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FLAG       I   Flag of keyword
C     DPVAL      O   Returned d.p. value
C     FOUND      O   True if variable is in pool.
C
C$ Detailed_Input
C
C     FLAG        is the string containing text identifier for the 
C                 keyword of interest.
C                
C$ Detailed_Output
C
C     DPVAL       is d.p. value of the keyword
C
C     FOUND       is TRUE if the value was obtained from the loaded 
C                 setup file keyword(s).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     See main routine header for complete list of exceptions.   
C           
C$ Files
C
C     None.
C     
C$ Particulars
C
C     None.
C      
C$ Examples
C      
C     None
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
C-    Version 1.1.0, 08-FEB-2012 (BVS)
C
C        Added fetching TLE start and stop pads.
C
C-    Version 1.0.5, 29-MAR-1999 (NGK)
C
C        Corrected comments.
C
C-    Version 1.0.4, 21-MAR-1999 (BVS)
C
C        Removed all warning messages.
C
C-    Version 1.0.2, 15-MAR-1999 (BVS)
C
C        Corrected comments.
C
C-    Version 1.0.1, 13-JAN-1999 (BVS)
C
C        Modified error and warning messages.
C   
C-    Version 1.0.0, 8-SEP-1998 (NGK)
C   
C-&

C$ Index_Entries
C
C      Return a d.p. value of a MKSPK setup file keyword
C
C-&  


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SETUPD' )
      END IF

      IF ( FLAG .EQ. 'DISTANCE COEFFICIENT' ) THEN

C
C        Retrieve DISTANCE unit value. If this exists we put
C        its coefficient instead of default assigned unit (km).
C        Parameter KINUNI declared in include file.
C
         EXIST = .FALSE.
         I = 1
         CALL GCPOOL (KINUNI , I, 1, N, WRKCHR , FOUND ) 

         IF ( FOUND ) THEN

            DO WHILE ( N .NE. 0 )

               CALL UCASE ( WRKCHR, WRKCHR )
               CALL ASTRIP ( WRKCHR, ' ', ' ', WRKCHR)
                               
               IF ( POS ( WRKCHR, 'DISTANCES=', 1 ) .NE. 0 ) THEN

C
C                 Return distance units
C
                  DSTUNT = WRKCHR ( POS ( WRKCHR, '=', 1) + 1 :
     .                              LASTNB ( WRKCHR ) )
                  EXIST = .TRUE. 

               END IF

               I = I + 1
               CALL GCPOOL (KINUNI , I, 1, N, WRKCHR , FOUND )
               
            END DO

         END IF

         IF ( EXIST ) THEN

C
C           Coefficient that converts distance unit to KM.
C
            CALL CONVRT ( 1.D0, DSTUNT, 'KM', DPVAL )

         ELSE

            FOUND = .FALSE.
            
         END IF

      ELSE IF ( FLAG .EQ. 'ANGLE COEFFICIENT' ) THEN

C
C        Retrieve ANGLE unit value. If this exists we put
C        its coefficient instead of default coefficient.
C        Parameter KINUNI declared in include file.
C 
         EXIST = .FALSE.
         I = 1
         CALL GCPOOL (KINUNI , I, 1, N, WRKCHR , FOUND )

         IF ( FOUND ) THEN

            DO WHILE ( N .NE. 0 )

               CALL UCASE ( WRKCHR, WRKCHR )
               CALL ASTRIP ( WRKCHR, ' ', ' ', WRKCHR)

               IF ( POS ( WRKCHR, 'ANGLES=', 1 ) .NE. 0 ) THEN

C
C                 Return angle units
C
                  ANGUNT = WRKCHR ( POS ( WRKCHR, '=', 1) + 1 :
     .                              LASTNB ( WRKCHR ) )
                  EXIST = .TRUE.

               END IF

               I = I + 1
               CALL GCPOOL (KINUNI , I, 1, N, WRKCHR , FOUND )

            END DO

         END IF

         IF ( EXIST ) THEN

C
C           Coefficient that converts angle unit to radian.
C
            CALL CONVRT ( 1.D0, ANGUNT, 'RADIANS', DPVAL )

         ELSE

            FOUND = .FALSE.

         END IF

      ELSE IF ( FLAG .EQ. 'START TIME' ) THEN

C
C        Retrieve START_TIME value. Parameter KSTATM declared 
C        in include file.
C
         CALL GCPOOL (KSTATM , 1, 1, N, STTMVL, FOUND )

         IF ( FOUND ) THEN

            CALL STR2ET ( STTMVL, DPVAL )

         END IF

      ELSE IF ( FLAG .EQ. 'STOP TIME' ) THEN

C
C        Retrieve STOP_TIME value. Parameter KSTOTM declared 
C        in include file.
C
         CALL GCPOOL (KSTOTM , 1, 1, N, SPTMVL, FOUND )

         IF ( FOUND ) THEN

            CALL STR2ET ( SPTMVL, DPVAL )

         END IF

      ELSE IF ( FLAG .EQ. 'CENTER BODY GM' ) THEN 
      
C
C        Retrieve CENTER_GM value. Parameter KCENGM declared 
C        in include file.   
C
         CALL GDPOOL ( KCENGM , 1, 1, N, DPVAL, FOUND )

      ELSE IF ( FLAG .EQ. 'J2' ) THEN

C
C        Retrieve CENTER_J2 value. Parameter KCENJ2 declared 
C        in include file.   
C
         CALL GDPOOL ( KCENJ2 , 1, 1, N, DPVAL, FOUND )

      ELSE IF ( FLAG .EQ. 'EQUATORIAL RADIUS' ) THEN 
      
C
C        Retrieve CENTER_EQ_RADIUS value. Parameter KCNEQR declared 
C        in include file.   
C
         CALL GDPOOL ( KCNEQR , 1, 1, N, DPVAL, FOUND )

      ELSE IF ( FLAG .EQ. 'PRECESSION TYPE' ) THEN
      
C
C        Retrieve PRECESSION_TYPE value and set J2 flags
C        according to it. Parameter KPRTYP declared in include file.
C
         CALL GCPOOL (KPRTYP, 1, 1, N, PRCTVL, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Orbit precession type was not specified ' //
     .                    'in the setup file keyword ''#''.'         )
            CALL ERRCH  ( '#', KPRTYP                                )
            CALL SIGERR ( 'SPICE(NOPRECESSIONTYPE)'                  )

         END IF

         CALL LJUST ( PRCTVL, PRCTVL )
         CALL UCASE ( PRCTVL, PRCTVL ) 
         
         IF ( PRCTVL ( : RTRIM ( PRCTVL ) ) .EQ. NOPREC ) THEN
      
            DPVAL = 3.D0
         
         ELSE IF ( PRCTVL ( : RTRIM ( PRCTVL ) ).EQ. APSPRC ) THEN
      
            DPVAL= 2.D0 
         
         ELSE IF ( PRCTVL ( : RTRIM ( PRCTVL ) ).EQ. NODPRC ) THEN
      
            DPVAL = 1.D0
         
         ELSE IF ( PRCTVL ( : RTRIM ( PRCTVL ) ).EQ. APNOPR ) THEN
      
            DPVAL = 0.D0
         
         ELSE

            CALL SETMSG ( 'Precession type ''#'' specified in the '  //
     .                    'setup file keyword ''#'' is not '         //
     .                    'recognized. Refer to the User''s Guide ' //
     .                    'for the program for the list of '         //
     .                    'supported precession types.'              )
            CALL ERRCH  ( '#', PRCTVL                                )
            CALL ERRCH  ( '#', KPRTYP                                )
            CALL SIGERR ( 'SPICE(UNRECOGNPRECTYPE)'                  )

         END IF

      ELSE IF ( FLAG .EQ. 'POLE RA' ) THEN
      
C
C        Retrieve CENTER_POLE_RA value. Parameter KCEPRA declared
C        in include file.
C
         CALL GDPOOL ( KCEPRA , 1, 1, N, DPVAL, FOUND )

      ELSE IF ( FLAG .EQ. 'POLE DEC' ) THEN
      
C
C        Retrieve CENTER_POLE_DEC value. Parameter KCPDEC declared
C        in include file.
C
         CALL GDPOOL ( KCPDEC , 1, 1, N, DPVAL, FOUND )

      ELSE IF ( FLAG .EQ. 'TLE COVERAGE START PAD' .OR. 
     .          FLAG .EQ. 'TLE COVERAGE STOP PAD'       ) THEN

C
C        Retrieve TLE coverage value. Set keyword name based on whether
C        the start pad or the stop pad is requested.
C
         IF      ( FLAG .EQ. 'TLE COVERAGE START PAD' ) THEN

            KEYWRD = KTLBPD

         ELSE IF ( FLAG .EQ. 'TLE COVERAGE STOP PAD'  ) THEN

            KEYWRD = KTLEPD

         ELSE

            CALL SIGERR ( 'SPICE(MKSPKBUGSETUP5)' )

         END IF

         CALL GCPOOL ( KEYWRD, 1, 1, N, PADVL, FOUND )

         IF ( FOUND ) THEN

C
C           Check that the pad value consists of two items, number and
C           units.
C
            IF ( WDCNT( PADVL ) .EQ. 2 ) THEN

C
C              Split the value string and convert the first item to a
C              DP number.
C
               CALL NEXTWD ( PADVL, CHRDP, UNITS )

               CALL NPARSD ( CHRDP, WRKDP, WRKCHR, I )

               IF ( I .EQ. 0 ) THEN

C
C                 Check that the pad is not negative.
C
                  IF ( WRKDP .LT. 0.D0 ) THEN
                     CALL SETMSG ( 'The value ''#'' specified as ' //
     .                             'the first item in the value '  //
     .                             'of the setup file keyword '    //
     .                             '''#'' is negative. Negative '  //
     .                             'pads are not allowed.'          )
                     CALL ERRCH  ( '#', CHRDP                       )
                     CALL ERRCH  ( '#', KEYWRD                      )
                     CALL SIGERR ( 'SPICE(BADTLECOVERAGEPAD3)'      )
                  END IF

C
C                 Convert pad to seconds.
C
                  CALL CONVRT ( WRKDP, UNITS, 'SECONDS', DPVAL )

               ELSE

                  CALL SETMSG ( 'The value ''#'' specified as the ' //
     .                          'first item in the value of the '   //
     .                          'setup file keyword ''#'' is not '  //
     .                          'a number.'                         )
                  CALL ERRCH  ( '#', CHRDP                          )
                  CALL ERRCH  ( '#', KEYWRD                         )
                  CALL SIGERR ( 'SPICE(BADTLECOVERAGEPAD2)'         )

               END IF

            ELSE

               CALL SETMSG ( 'The TLE coverage pad ''#'' '    //
     .                       'specified in the setup file keyword ' //
     .                       '''#'' does not consists of exactly '  //
     .                       'two items, a number and a word '      //
     .                       'representing time units.'              )
               CALL ERRCH  ( '#', PADVL                              )
               CALL ERRCH  ( '#', KEYWRD                             )
               CALL SIGERR ( 'SPICE(BADTLECOVERAGEPAD)'              )

            END IF

         END IF

      ELSE

         CALL SIGERR ( 'SPICE(MKSPKBUGSETUP3)')

      END IF

      CALL CHKOUT ( 'SETUPD' )

      RETURN


C$Procedure   SETUPA ( Get LSK and PCK file names and load its content )

      ENTRY SETUPA ( FLAG, FOUND )
      
C$ Abstract
C
C     Load LSK and PCK files provided in a setup file. 
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
C     KERNEL
C     PCK
C
C$ Keywords
C
C     KERNEL
C     POOL
C
C$ Declarations
C      
C     CHARACTER*(*)         FLAG
C     LOGICAL               FOUND
C      
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FLAG       I   Flag of keyword
C     FOUND      O   True if variable is in pool.
C
C$ Detailed_Input
C
C     FLAG        is the string containing text identifier for the 
C                 keyword of interest.
C                
C$ Detailed_Output
C
C     FOUND       is TRUE if the files were found and loaded.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     See main routine header for complete list of exceptions.   
C           
C$ Files
C
C     This routine requires LSK and PCK files specified in the 
C     corresponding setup file keywords to exist and be legitimate 
C     SPICE kernel file.
C     
C$ Particulars
C
C     This routine does not return the value of LSK and PCK 
C     file names but loads corresponding files using the pool or
C     binary loaders. 
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
C-    Version 1.0.5, 29-MAR-1999 (NGK)
C
C        Corrected comments.
C
C-    Version 1.0.4, 21-MAR-1999 (BVS)
C
C        Removed all warning messages.
C
C-    Version 1.0.2, 15-MAR-1999 (BVS)
C
C        Corrected comments.
C
C-    Version 1.0.1, 13-JAN-1999 (BVS)
C
C        Modified error and warning messages.
C   
C-    Version 1.0.0, 8-SEP-1998 (NGK)
C   
C-&

C$ Index_Entries
C
C      Load LSK and PCK files provided in a MKSPK setup file
C
C-&  


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SETUPA' )
      END IF

      IF ( FLAG .EQ. 'LOAD LSK FILE' ) THEN

C
C        Retrieve LEAPSECONDS file name and load it. Parameter LSKFIL
C        declared in include file.
C
         CALL GCPOOL ( LSKFIL, 1, 1, N, LSKFN, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Leapsecond file name was not specified '  //
     .                    'in the setup file keyword ''#''.'         )
            CALL ERRCH  ( '#', LSKFIL                                )
            CALL SIGERR ( 'SPICE(NOLSKFILENAME)'                     )

         END IF

         CALL LJUST ( LSKFN, LSKFN )
         CALL FURNSH ( LSKFN )

      ELSE IF ( FLAG .EQ. 'LOAD PCK FILE' ) THEN

C
C        Retrieve PCK_FILE names value. Parameter PCKFIL declared in 
C        include file.
C
C        Check if keyword exists and get number of its values.
C
         CALL DTPOOL ( PCKFIL, FOUND, N, TYPE )

         IF ( FOUND .AND. TYPE .EQ. 'C' ) THEN

            DO I = 1, N

C
C              Get next file name, left-justify it and get file's
C              type and architecture
C
               CALL GCPOOL ( PCKFIL, I, 1, N, WRKCHR , FOUND )
               CALL LJUST  ( WRKCHR, WRKCHR )
               CALL GETFAT ( WRKCHR, ARCH, TYPE )

               IF   ( TYPE .EQ. 'PCK' .AND. ARCH .EQ. 'DAF' ) THEN

C
C                 It's a binary PCK file. Use binary loader PCKLOF.
C
                  CALL PCKLOF ( WRKCHR, HANDLE )

               ELSE IF ( TYPE .EQ. 'PCK' .AND. ARCH .EQ. 'KPL' ) THEN

C
C                 Text PCK. Call FURNSH.
C
                  CALL FURNSH ( WRKCHR )

               ELSE

C
C                 Unknown combination of TYPE/ARCHITECTURE. Complain.
C
                  CALL SETMSG ( 'The file ''#'' specified in the '   //
     .                          'setup file keyword ''#'' is not '   //
     .                          'a text or binary PCK file.'         )
                  CALL ERRCH  ( '#', WRKCHR                          )
                  CALL ERRCH  ( '#', PCKFIL                          )
                  CALL SIGERR ( 'SPICE(NOTAPCKFILE)'                 )

               END IF

            END DO

         END IF

      ELSE

         CALL SIGERR ( 'SPICE(MKSPKBUGSETUP4)' )

      END IF
      
      CALL CHKOUT ( 'SETUPA' )
      
      RETURN
                              
      END
