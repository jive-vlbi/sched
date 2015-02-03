C$Procedure      EDTCOM (Edit a command)
 
      SUBROUTINE EDTCOM ( DELIM, PROMPT, COMMND, SOURCE )
 
C$ Abstract
C
C     This entry point allows the user of a program to fetch
C     previously entered commands, review them, re-execute the commands
C     or edit and re-execute the command.
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
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(1)         DELIM
      CHARACTER*(*)         PROMPT
      CHARACTER*(*)         COMMND
      INTEGER               SOURCE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     DELIM      I   is the character used to delimit the command ends
C     COMMND    I/O  is a command to process
C     SOURCE    I/O  indicates the source of the command
C
C$ Detailed_Input
C
C     DELIM        is the character used to delimit input commands.
C                  A command begins at the first non-blank character
C                  of COMMND and continues until the last non-blank
C                  character or the first occurrence of DELIM which
C                  ever is first.
C
C
C     COMMND       is a string that indicates some action the program
C                  should take.  The only commands that have meaning
C                  to this routine are those of the form:
C
C                    RECALL @int(1:20)
C
C                    RECALL ALL
C
C                    DO     @int(1:20)
C
C                    EDIT   @int(1:20)
C
C                  all other commands are ignored by this routine.
C                  (See the META/2 language specification language
C                  for a more detailed description of the meaning
C                  of the syntax specifications given above.)
C
C     SOURCE       is an integer indicating where the input command
C                  came from.  Unless SOURCE has a value of 2 meaning
C                  the command was typed interactively, no action
C                  is taken by this routine.
C
C$ Detailed_Output
C
C     COMMND       if the input command is recognized by this routine
C                  COMMND will be set to all blank characters.
C                  Otherwise, COMMND will remain unchanged.
C
C     SOURCE       if the input command is recognized by this routine
C                  SOURCE will be set to zero indicating that there
C                  is no longer a potential command in the string
C                  COMMND.  SOURCE will remain unchanged.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     In the case of a command of the form EDIT @int(1:20) this routine
C     must be able to create a file that the editor can edit.
C
C     If this cannot be done one of the following errors will be
C     signalled.
C
C     1)   If the program cannot create a new file name that
C          could hold the command to be edited, the error
C          COMLOOP(NOFILECREATION) will be signalled.
C
C     2)   If a new file name could be created but the file could
C          not be opened, the error COMLOOP(COMMANDEDITFAILED)
C          will be signalled.
C
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is provided so that command line programs
C     may obtain a history of commands that have been entered
C     into the program and possible re-execute or edit and execute
C     the previous commands.  This is meant to be integrated
C     with the basic command loop software available for
C     constructing command driven programs.  See the routine
C     CMLOOP to see how this fits into the general sequence of
C     command processing.
C
C$ Examples
C
C     See CMLOOP for the intended use of this routine.
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
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.29.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 1.28.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 1.27.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 1.26.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 1.24.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 1.23.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 1.22.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 1.21.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.14.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 1.13.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 1.12.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 1.11.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 1.10.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 1.9.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 1.8.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 1.7.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 1.6.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 1.5.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 1.4.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 1.4.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 1.4.3, 20-SEP-1999 (NJB)
C
C        CSPICE and PC-LINUX environment lines were added.  Some
C        typos were corrected.
C
C-    SPICELIB Version 1.4.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 1.4.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.4.0, 9-JAN-1997 (WLT)
C
C        Added minimal support for the MAC version. When the user
C        requests EDIT, the routine displays the matching item
C        that should be editted.  This is the only option available
C        at the moment on the MAC.  When something better comes along
C        we'll do something more creative.
C
C-    SPICELIB Version 1.3.0, 5-Dec-1995 (WLT)
C
C        Fixed the bug that occured if you typed RECALL x and
C        there was no matching command (probably should have done
C        thins in version 1.2.0) EDTCOM now pronounces that this
C        is an error.
C
C-    SPICELIB Version 1.2.0, 11-SEP-1995 ( WLT )
C
C        Fixed the bug that occurred if you type EDIT x or
C        DO x and there was no matching command in the history
C        list.  EDTCOM no pronounces that this is an error.
C
C-    SPICELIB Version 1.1.0, 1-JUN-1995 (HAN)
C
C        Created the master source file for VAX/VMS, Alpha/OpenVMS,
C        Sun (Sun OS 4.1.x and Solaris), PC(Microsoft Fortran), HP,
C        and NeXT.
C
C-    Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C        This is the configured version of the Command Loop
C        software as of May 4, 1994
C
C-    SPICELIB Version 1.0.0, 18-APR-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Recall Re-execute or edit and re-execute a command
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 1-JUN-1995 (HAN)
C
C        Created the master source file for VAX/VMS, Alpha/OpenVMS,
C        Sun (Sun OS 4.1.x and Solaris), PC(Microsoft Fortran), HP,
C        and NeXT.
C
C-&
 
C
C     Spicelib Functions
C
      INTEGER               RTRIM
      INTEGER               CARDC
      LOGICAL               MATCH
      LOGICAL               FAILED
      LOGICAL               HAVE
 
C
C     Meta/2 functions
C
      LOGICAL               M2WMCH
 
 
C
C     Below are the various sources from which
C     commands might come.
C
C     NONE
C     COMBUF
C     KEYBRD
C     INPFIL
C
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )
 
      INTEGER               COMBUF
      PARAMETER           ( COMBUF = NONE   + 1 )
 
      INTEGER               KEYBRD
      PARAMETER           ( KEYBRD = COMBUF + 1 )
 
      INTEGER               INPFIL
      PARAMETER           ( INPFIL = KEYBRD + 1 )
 
C
C     Local parameters used for allocating space and controlling loop
C     execution.
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 132 )
 
      INTEGER               BSIZE
      PARAMETER           ( BSIZE  = 80  )
 
      INTEGER               INTLEN
      PARAMETER           ( INTLEN = 3   )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5  )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32  )
 
C
C     Local Variables.
C
      CHARACTER*(1)         SPACE
      CHARACTER*(1)         TAB
      CHARACTER*(1760)      REST
      CHARACTER*(INTLEN)    DSTRNG
      CHARACTER*(LNSIZE)    BUFFER ( LBCELL:BSIZE)
      CHARACTER*(LNSIZE)    EDITOR
      CHARACTER*(LNSIZE)    ERROR  ( 2 )
      CHARACTER*(LNSIZE)    ERRSTR
      CHARACTER*(LNSIZE)    NAME
      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(LNSIZE)    PATTRN
      CHARACTER*(WDSIZE)    FRSTWD
      CHARACTER*(WDSIZE)    SCNDWD
 
      INTEGER               B1
      INTEGER               B2
      INTEGER               COMNUM
      INTEGER               DEPTH
      INTEGER               E1
      INTEGER               E2
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               PTR
      INTEGER               R
      INTEGER               UNIT
 
      LOGICAL               FIRST
      LOGICAL               LSTAT  ( 3 )
      LOGICAL               SSTAT  ( 3 )
      LOGICAL               STATUS ( 3 )
      LOGICAL               SVSTAT ( 3 )
      LOGICAL               GOTONE
 
 
      SAVE
 
      DATA     EDITOR    / 'emacs' /
      DATA     FIRST     / .TRUE.  /
 
C
C     The only time an EDIT/RECALL/DO command can have any meaning
C     is when it comes from the keyboard.
C
      IF ( SOURCE .NE. KEYBRD ) THEN
         RETURN
      END IF
 
C
C     Initialize the syntax for the preprocessing commands
C
      IF ( FIRST ) THEN
         FIRST = .FALSE.
 
         TAB   = CHAR(9)
         SPACE = ' '
 
      END IF
 
C
C     Next we take apart the command and see if it is one of the
C     preprocessing commands.
C
      CALL NEXTWD ( COMMND,  FRSTWD,  REST )
      CALL NEXTWD ( REST,    SCNDWD,  REST )
 
C
C     We probably don't have any of the pathologies below, but they
C     are easy to check so we handle them here.
C
      IF ( REST .NE. ' ' ) THEN
         RETURN
      END IF
 
      IF ( FRSTWD .EQ. ' ' ) THEN
         RETURN
      END IF
 
      B1 = 1
      B2 = 1
      E1 = RTRIM  ( FRSTWD )
      E2 = RTRIM  ( SCNDWD )
 
      IF (       SCNDWD .EQ. ' '
     .     .AND.
     .     .NOT. M2WMCH ( FRSTWD, B1, E1, 'RECALL' ) ) THEN
         RETURN
      END IF
 
 
C
C     We need the beginning and endings of the words we've extracted.
C
      B1 = 1
      B2 = 1
      E1 = RTRIM  ( FRSTWD )
      E2 = RTRIM  ( SCNDWD )
 
 
      IF    (         M2WMCH ( FRSTWD, B1, E1, 'RECALL' )
     .         .AND. (SCNDWD .EQ. ' ' )      ) THEN
 
 
C
C        We don't want the RECALL command to show up in the
C        output.
C
         CALL DMPBUF ()
 
C
C        We don't write the output of a RECALL command to the
C        log file.
C
         CALL NSPGST ( 'LOG', STATUS )
         CALL NSPIOH ( 'LOG'         )
C
C        Determine the depth of the command line buffer.
C
         CALL GETBSZ ( DEPTH )
C
C        Fetch each paragraph and display it.
C
         DO WHILE ( DEPTH .GT. 0 )
 
            CALL SSIZEC ( BSIZE,  BUFFER )
            CALL INTSTR ( DEPTH,  DSTRNG )
            CALL LJUST  ( DSTRNG, DSTRNG )
            CALL GETBUF ( DEPTH,  BUFFER )
 
            LINE = DSTRNG(1:3) // BUFFER(1)
 
            CALL NSPWLN ( LINE )
 
            DO I = 2, CARDC(BUFFER)
               LINE = '   ' // BUFFER(I)
               CALL NSPWLN ( LINE )
            END DO
 
            DEPTH = DEPTH - 1
 
         END DO
C
C        Reset the status of the LOG file back to whatever it
C        was before we started dumping old commands.  Finally
C        set the command to a blank.
C
         CALL NSPPST ( 'LOG', STATUS )
         COMMND = ' '
         SOURCE  = NONE
 
         RETURN
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'RECALL'     )
     .         .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN
C
C        We don't write the output of a RECALL command to the
C        log file.
C
         CALL NSPGST ( 'LOG', STATUS )
         CALL NSPIOH ( 'LOG'         )
C
C        Find out the depth of the command to fetch.
C
         CALL NPARSI (SCNDWD, DEPTH, ERRSTR, PTR )
C
C        Get rid of the top command (it's the RECALL command).
C
         CALL DMPBUF ()
         CALL SSIZEC ( BSIZE,  BUFFER )
         CALL GETBUF ( DEPTH,  BUFFER )
 
 
         DO I = 1, CARDC(BUFFER)
            LINE = '   ' // BUFFER(I)
            CALL NSPWLN ( LINE )
         END DO
C
C        Reset the status of the LOG file back to whatever it
C        was before we started dumping old commands.  Finally
C        set the command to a blank.
C
         CALL NSPPST ( 'LOG', STATUS )
         COMMND = ' '
         SOURCE  = NONE
         RETURN
 
      ELSE IF ( M2WMCH ( FRSTWD, B1, E1, 'RECALL' ) ) THEN
 
 
C
C        Find out the depth of the command to fetch.
C
         CALL GETBSZ (         DEPTH  )
         CALL SUFFIX ( '*', 0, SCNDWD )
 
         COMNUM = 2
         GOTONE = .FALSE.
 
         DO WHILE ( COMNUM .LE. DEPTH )
 
            CALL SSIZEC( BSIZE,  BUFFER )
            CALL GETBUF( COMNUM, BUFFER )
 
            IF  (       CARDC ( BUFFER  ) .GT.  0
     .            .AND. MATCH ( BUFFER(1), SCNDWD ) ) THEN
 
C
C              We don't write the output of a RECALL command to the
C              log file.
C
               CALL NSPGST ( 'LOG', STATUS )
               CALL NSPIOH ( 'LOG'         )
C
C              Dump the top command as it is just the recall command.
C
               CALL DMPBUF ()
               DO I = 1, CARDC(BUFFER)
                  LINE = '   ' // BUFFER(I)
                  CALL NSPWLN ( LINE )
               END DO
 
               COMNUM = DEPTH
               GOTONE = .TRUE.
               COMMND = ' '
               SOURCE = NONE
 
               CALL NSPPST ( 'LOG', STATUS )
 
            END IF
 
            COMNUM = COMNUM + 1
 
         END DO
C
C        Reset the status of the LOG file back to whatever it
C        was before we started dumping old commands.
C
         IF ( .NOT. GOTONE ) THEN
 
            ERROR(1) = 'There is no command in the command '
     .      //         'history list that matches ''#''. '
 
            CALL REPMC ( ERROR(1), '#', SCNDWD, ERROR(1) )
            COMMND = ' '
            SOURCE = NONE
            CALL SETMSG (  ERROR              )
            CALL SIGERR ( 'EDTCOM(NOMATCH)'   )
            RETURN
 
         END IF
 
 
         RETURN
 
 
 
 
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'EDIT'       )
     .         .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN
 
         CALL NPARSI (SCNDWD, DEPTH, ERRSTR, PTR )
         CALL DMPBUF ()
         CALL SSIZEC ( BSIZE, BUFFER )
         CALL GETBUF ( DEPTH, BUFFER )
C
C        Open the utility port to receive the contents of BUFFER.
C
         PATTRN = 'edt{0-z}{0-z}{0-z}{0-z}{0-z}.tmp'
 
         CALL NEWFIL ( PATTRN, 'UTILITY', NAME )
 
         IF ( FAILED() ) THEN
            CALL RESET
            CALL CHKIN  ( 'EDTCOM' )
            CALL SETMSG ( 'The program was unable to open a file '
     .      //            'that could be used with the '
     .      //            'editor. '
     .      //            'Command editing cannot be '
     .      //            'performed at this time. ' )
            CALL SIGERR ( 'CMLOOP(COMMANDEDITFAILED)' )
            CALL CHKOUT ( 'EDTCOM' )
            RETURN
         END IF
C
C        We have at this point succeeded in opening a file
C        into which we can write the last command.  But we
C        don't want to write to the screen, log file or save
C        file if there is one. Inhibit writing to any port
C        but the utility port.
C
         CALL NSPGST ( 'LOG',     LSTAT  )
         CALL NSPGST ( 'SCREEN',  SSTAT  )
         CALL NSPGST ( 'SAVE',    SVSTAT )
 
         CALL NSPIOH ( 'LOG'     )
         CALL NSPIOH ( 'SCREEN'  )
         CALL NSPIOH ( 'SAVE'    )
 
         DO I = 1, CARDC ( BUFFER )
            CALL NSPWLN ( BUFFER(I) )
         END DO
 
         CALL NSPIOC ( 'UTILITY' )
C
C        Activate the editor
C
         CALL EDTCMD ( EDITOR(1:RTRIM(EDITOR)),
     .                 NAME(1:RTRIM(NAME)) )
 
         ERROR(1) = ' '
         ERROR(2) = ' '
 
         IF ( HAVE(ERROR) ) THEN
            CALL NSPPST ( 'LOG',     LSTAT  )
            CALL NSPPST ( 'SCREEN',  SSTAT  )
            CALL NSPPST ( 'SAVE',    SVSTAT )
            COMMND = ' '
            SOURCE = NONE
            CALL SETMSG (  ERROR                   )
            CALL SIGERR ( 'SPICE(FILEREADERROR)'   )
            RETURN
         END IF
C
C        Read the first command from the edited file.
C
 
         CALL PRSTRT ( NAME, ERROR )
 
         IF ( HAVE(ERROR) ) THEN
            CALL NSPPST ( 'LOG',     LSTAT  )
            CALL NSPPST ( 'SCREEN',  SSTAT  )
            CALL NSPPST ( 'SAVE',    SVSTAT )
            COMMND = ' '
            SOURCE = NONE
            CALL PREXIT
            CALL SETMSG (  ERROR                   )
            CALL SIGERR ( 'SPICE(FILEREADERROR)'   )
            RETURN
         END IF
 
         CALL PRREAD ( DELIM,  COMMND )
         CALL PUTCOM ( COMMND, KEYBRD )
         CALL PREXIT ( )
 
C
C        Finally, delete the file we used with the editor.
C
         CALL TXTOPR ( NAME, UNIT )
         CLOSE  ( UNIT   =  UNIT,
     .            STATUS = 'DELETE',
     .            IOSTAT = IOSTAT )
 
         CALL SSIZEC ( BSIZE, BUFFER )
         CALL GETBUF ( 1,     BUFFER )
 
         CALL NSPIOA ( 'SCREEN' )
 
         R = RTRIM ( PROMPT ) + 2
 
         DO I = 1, CARDC(BUFFER)
            IF ( I .EQ. 1 ) THEN
 
               LINE = PROMPT
               CALL SUFFIX ( BUFFER(I), 1, LINE )
            ELSE
               LINE     = ' '
               LINE(R:) = BUFFER(I)
            END IF
 
            CALL NSPWLN ( LINE(1:RTRIM(LINE)) )
         END DO
 
C
C        Reset the writing to all other ports.
C
         CALL NSPPST ( 'LOG',     LSTAT  )
         CALL NSPPST ( 'SCREEN',  SSTAT  )
         CALL NSPPST ( 'SAVE',    SVSTAT )
 
         COMMND = ' '
         SOURCE = NONE
 
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'EDIT' )
     .         .AND. SCNDWD .NE. ' '                     ) THEN
 
         GOTONE = .FALSE.
         COMNUM = 2
         CALL GETBSZ ( DEPTH          )
         CALL SUFFIX ( '*', 0, SCNDWD )
 
         DO WHILE ( COMNUM .LE. DEPTH )
 
            CALL SSIZEC( BSIZE,  BUFFER )
            CALL GETBUF( COMNUM, BUFFER )
 
            IF  (       CARDC ( BUFFER  ) .GT.  0
     .            .AND. MATCH ( BUFFER(1), SCNDWD ) ) THEN
 
               GOTONE = .TRUE.
               CALL DMPBUF ()
 
C
C              Open the utility port to receive the contents of BUFFER.
C
               PATTRN = 'edt{0-z}{0-z}{0-z}{0-z}{0-z}.tmp'
 
               CALL NEWFIL ( PATTRN, 'UTILITY', NAME )
 
               IF ( FAILED() ) THEN
                  CALL RESET
                  CALL CHKIN  ( 'EDTCOM' )
                  CALL SETMSG  ( 'The program was unable to '
     .            //             'open a file that could be used '
     .            //             'with the editor. Command '
     .            //             'editing cannot be performed at '
     .            //             'this time. ')
 
                  CALL SIGERR ( 'CMLOOP(COMMANDEDITFAILED)' )
                  CALL CHKOUT ( 'EDTCOM' )
                  RETURN
               END IF
 
C
C              We have at this point succeeded in opening a file
C              into which we can write the last command.  But we
C              don't want to write to the screen, log file or save
C              file if there is one. Inhibit writing to any port
C              but the utility port.
C
               CALL NSPGST ( 'LOG',     LSTAT  )
               CALL NSPGST ( 'SCREEN',  SSTAT  )
               CALL NSPGST ( 'SAVE',    SVSTAT )
 
               CALL NSPIOH ( 'LOG'     )
               CALL NSPIOH ( 'SCREEN'  )
               CALL NSPIOH ( 'SAVE'    )
 
               DO I = 1, CARDC ( BUFFER )
                  CALL NSPWLN ( BUFFER(I) )
               END DO
 
               CALL NSPIOC ( 'UTILITY' )
 
C
C              Activate the editor
C
               CALL EDTCMD ( EDITOR(1:RTRIM(EDITOR)),
     .                        NAME(1:RTRIM(NAME)) )
 
               ERROR(1) = ' '
               ERROR(2) = ' '
 
               IF ( HAVE(ERROR) ) THEN
                  CALL NSPPST ( 'LOG',     LSTAT  )
                  CALL NSPPST ( 'SCREEN',  SSTAT  )
                  CALL NSPPST ( 'SAVE',    SVSTAT )
                  COMMND = ' '
                  SOURCE = NONE
                  CALL SETMSG (  ERROR                   )
                  CALL SIGERR ( 'SPICE(FILEREADERROR)'   )
                  RETURN
               END IF
 
C
C              Read the first command from the edited file.
C
 
               CALL PRSTRT ( NAME, ERROR )
 
               IF ( HAVE(ERROR) ) THEN
                  CALL NSPPST ( 'LOG',     LSTAT  )
                  CALL NSPPST ( 'SCREEN',  SSTAT  )
                  CALL NSPPST ( 'SAVE',    SVSTAT )
                  COMMND = ' '
                  SOURCE = NONE
                  CALL PREXIT
                  CALL SETMSG (  ERROR                   )
                  CALL SIGERR ( 'SPICE(FILEREADERROR)'   )
                  RETURN
               END IF
 
               CALL PRREAD ( DELIM,  COMMND )
               CALL PUTCOM ( COMMND, KEYBRD )
               CALL PREXIT ( )
 
C
C              Finally, delete the file we used with the editor.
C
               CALL TXTOPR ( NAME, UNIT )
               CLOSE  ( UNIT   =  UNIT,
     .                  STATUS = 'DELETE',
     .                  IOSTAT = IOSTAT )
 
               CALL SSIZEC ( BSIZE, BUFFER )
               CALL GETBUF ( 1,     BUFFER )
 
               CALL NSPIOA ( 'SCREEN' )
 
               R = RTRIM ( PROMPT ) + 2
 
               DO I = 1, CARDC(BUFFER)
                  IF ( I .EQ. 1 ) THEN
 
                     LINE = PROMPT
                     CALL SUFFIX ( BUFFER(I), 1, LINE )
                  ELSE
                     LINE     = ' '
                     LINE(R:) = BUFFER(I)
                  END IF
 
                  CALL NSPWLN ( LINE(1:RTRIM(LINE)) )
               END DO
 
C
C              Reset the writing to all other ports.
C
               CALL NSPPST ( 'LOG',     LSTAT  )
               CALL NSPPST ( 'SCREEN',  SSTAT  )
               CALL NSPPST ( 'SAVE',    SVSTAT )
 
               COMMND = ' '
               SOURCE = NONE
 
               COMNUM = DEPTH
 
            END IF
 
            COMNUM = COMNUM + 1
 
         END DO
 
 
         IF ( .NOT. GOTONE ) THEN
 
            ERROR(1) = 'There is no command in the command '
     .      //         'history list that matches ''#''. '
 
            CALL REPMC ( ERROR(1), '#', SCNDWD, ERROR(1) )
            COMMND = ' '
            SOURCE = NONE
            CALL SETMSG (  ERROR              )
            CALL SIGERR ( 'EDTCOM(NOMATCH)'   )
            RETURN
 
         END IF
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'DO'         )
     .         .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN
 
         CALL NPARSI ( SCNDWD, DEPTH, ERRSTR, PTR )
         CALL DMPBUF ()
         CALL SSIZEC ( BSIZE, BUFFER )
         CALL GETBUF ( DEPTH, BUFFER )
 
         CALL NSPGST ( 'SCREEN', SSTAT )
         CALL NSPIOA ( 'SCREEN'        )
 
         R = RTRIM ( PROMPT ) + 2
C
C        Reset the paragraph buffer so it can receive another
C        paragraph. (This is where we buffer commands and we
C        need to buffer this one.)
C
         CALL RSTBUF ()
 
         DO I = 1, CARDC(BUFFER)
 
            CALL PUTBUF(BUFFER(I))
 
            IF ( I .EQ. 1 ) THEN
               LINE = PROMPT
               CALL SUFFIX ( BUFFER(I), 1, LINE )
            ELSE
               LINE     = ' '
               LINE(R:) = BUFFER(I)
            END IF
 
            CALL NSPWLN ( LINE(1:RTRIM(LINE)) )
         END DO
 
         CALL NSPPST ( 'SCREEN',  SSTAT  )
 
         COMMND = ' '
 
         COMMND = BUFFER(1)
 
         DO I = 2, CARDC ( BUFFER )
            CALL SUFFIX ( BUFFER(I), 1, COMMND )
         END DO
 
         I = INDEX ( COMMND, DELIM )
 
         IF ( I .GT. 0 ) THEN
            CALL PUTCOM ( COMMND(1:I-1), KEYBRD )
         ELSE
            CALL PUTCOM ( COMMND,        COMBUF )
         END IF
 
         COMMND = ' '
         SOURCE = NONE
 
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'DO' )
     .         .AND.  SCNDWD .NE. ' '                ) THEN
C
C        This is basically the same as the last case, but
C        we look for a pattern match before doing anything.
C
         GOTONE = .FALSE.
         CALL GETBSZ ( DEPTH )
         CALL SUFFIX ( '*', 0, SCNDWD )
         COMNUM = 2
 
         DO WHILE ( COMNUM .LE. DEPTH )
 
            CALL SSIZEC ( BSIZE,  BUFFER )
            CALL GETBUF ( COMNUM, BUFFER )
 
            IF  (       CARDC ( BUFFER  ) .GT.  0
     .            .AND. MATCH ( BUFFER(1), SCNDWD ) ) THEN
 
               GOTONE = .TRUE.
               CALL DMPBUF ()
 
               CALL NSPGST ( 'SCREEN', SSTAT )
               CALL NSPIOA ( 'SCREEN'        )
 
               R = RTRIM ( PROMPT ) + 2
C
C              Reset the paragraph buffer so it can receive another
C              paragraph. (This is where we buffer commands and we
C              need to buffer this one.)
C
               CALL RSTBUF ()
 
               DO I = 1, CARDC(BUFFER)
                  CALL PUTBUF( BUFFER(I) )
                  IF ( I .EQ. 1 ) THEN
                     LINE = PROMPT
                     CALL SUFFIX ( BUFFER(I), 1, LINE )
                  ELSE
                     LINE     = ' '
                     LINE(R:) = BUFFER(I)
                  END IF
 
                  CALL NSPWLN ( LINE(1:RTRIM(LINE)) )
               END DO
 
               CALL NSPPST ( 'SCREEN',  SSTAT  )
 
               COMMND = ' '
 
               COMMND = BUFFER(1)
 
               DO I = 2, CARDC ( BUFFER )
                  CALL SUFFIX ( BUFFER(I), 1, COMMND )
               END DO
 
               I = INDEX ( COMMND, DELIM )
 
               IF ( I .GT. 0 ) THEN
                  CALL PUTCOM ( COMMND(1:I-1), KEYBRD )
               ELSE
                  CALL PUTCOM ( COMMND,        COMBUF )
               END IF
 
               COMMND = ' '
               SOURCE = NONE
 
               COMNUM = DEPTH
 
            END IF
 
            COMNUM = COMNUM + 1
 
         END DO
 
         IF ( .NOT. GOTONE ) THEN
 
            ERROR(1) = 'There is no command in the command '
     .      //         'history list that matches ''#''. '
 
            CALL REPMC ( ERROR(1), '#', SCNDWD, ERROR(1) )
            COMMND = ' '
            SOURCE = NONE
            CALL SETMSG (  ERROR              )
            CALL SIGERR ( 'EDTCOM(NOMATCH)'   )
            RETURN
 
         END IF
 
      END IF
 
 
 
      RETURN
 
      ENTRY SETEDT ( COMMND )
 
      EDITOR = COMMND
      RETURN
 
      ENTRY GETEDT ( COMMND )
      COMMND = EDITOR
      RETURN
 
      END
