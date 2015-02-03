C$Procedure NSPIO (Inspekt I/O Manager)
 
      SUBROUTINE NSPIO ( LINE, PORT, NAME, STATUS, OK )
 
C$ Abstract
C
C    Manage file and screen logging information for Inspekt.
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
C     TEXT
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         LINE
      CHARACTER*(*)         PORT
      CHARACTER*(*)         NAME
      LOGICAL               STATUS ( 3 )
      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LINE       I   NSPWLN
C     PORT       I   NSPOPN, NSPIOH, NSPIOA, NSPGST, NSPPST, NSPIOC
C                    NSPIOS, NSPIOR, NSPPFL
C     NAME      I/O  NSPOPN, NSPPFL
C     STATUS    I/O  NSPGST, NSPPST
C     OK         O   NSPIOR
C
C$ Detailed_Input
C
C     LINE       is a string of text that is to be written to all the
C                open, active, non-suspended ports.
C
C     PORT       is a string that indicates the name of a port on which
C                to perform an operation.  Acceptable values are:
C
C                 Standard Output Port:
C
C                   'SCREEN'
C
C                 File Based Ports:
C
C                   'LOG'
C                   'SAVE'
C                   'UTILITY'
C                   'ERROR'
C                   'AUX1'
C                   'AUX2'
C                   'AUX3'
C
C     NAME       The name of a file to create and attach to a file
C                based port.
C
C     STATUS     An array of logicals that configures the status of
C                a port.  Acceptable values are as follows:
C
C                   STATUS(1) - Activity Status:
C                               .TRUE.  - the port is active
C                               .FALSE. - the port is inactive
C
C                   STATUS(2) - Open Status:
C                               .TRUE.  - the port is open
C                               .FALSE. - the port is closed
C
C                   STATUS(3) - Suspend Status:
C                               .TRUE.  - I/O on this port is suspended
C                               .FALSE. - I/O can proceed on this port
C
C$ Detailed_Output
C
C     NAME       The name of a file attached to a file based port.
C
C     STATUS     An array of logicals that describes the status of
C                a port.  A description of the values follows:
C
C                   STATUS(1) - Activity Status:
C                               .TRUE.  - the port is active
C                               .FALSE. - the port is inactive
C
C                   STATUS(2) - Open Status:
C                               .TRUE.  - the port is open
C                               .FALSE. - the port is closed
C
C                   STATUS(3) - Suspend Status:
C                               .TRUE.  - I/O on this port is suspended
C                               .FALSE. - I/O can proceed on this port
C
C     OK         is a logical that indicates whether the attempt to
C                reopen a suspended port succeeded.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     1) This umbrella may be configured to simultaneously access
C        NPORTS number of files.  They are all opened using the
C        SPICELIB routine TXTOPN.
C
C$ Exceptions
C
C     1) If the bogus entry point NSPIO is called directly, then the
C        error NSPIO(BOGUSENTRY) is signalled.
C
C     2) See entry points «Entry Points» for exceptions specific to
C        them.
C
C$ Particulars
C
C     NSPIO is an umbrella that functions as an I/O manager.  It
C     is capable of interfacing with STDOUT as well as several
C     files at once.  To accomplish these management tasks, the
C     following entry points are provided:
C
C        NSPOPN - Open a port.
C
C        NSPIOH - Inhibit access to a port.
C        NSPIOA - Activate an inhibited port.
C
C        NSPGST - Get the status of a port.
C        NSPPST - Put the status of a port.
C
C        NSPIOS - Suspend access to a port.
C        NSPIOR - Reopen a suspended port.
C
C        NSPWLN - Write a line of text to all accessible ports.
C
C        NSPEND - Close all ports and reset the state of the I/O
C                 manager to the default.
C
C        NSPPFL - Retrieve the name of the file associated with a port.
C
C        NSPIOC - Close a port.
C
C     The following ports are provided for usage:
C
C        Standard Output Port:
C
C          'SCREEN'
C
C        File Based Ports:
C
C          'LOG'
C          'SAVE'
C          'UTILITY'
C          'ERROR'
C          'AUX1'
C          'AUX2'
C          'AUX3'
C
C     By default the SCREEN port is open and ready to receive lines
C     of text.  All of the file based ports are closed until opened
C     with NSPOPN.
C
C     NSPEND is provided to close all open ports and reset the I/O
C     manager back to its default state.  If the SCREEN port is
C     accessible for writing, then when the LOG, SAVE, and ERROR ports
C     are closed a message indicating where they may be found is
C     written to the screen port.  The ERROR port is a special case,
C     since if it was unsuccessfully opened, when NSPEND attempts to
C     close this port it writes a brief diagnostic indicating the
C     open failure.
C
C     The suspend and reopen entry points are provided for backwards
C     compatibility and should not be used in developing new code.
C
C$ Examples
C
C     See INSPEKT for examples.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 01-FEB-2000 (FST)
C
C        Added headers and ports ERROR, AUX1, AUX2, and AUX3.
C
C-    NSPIO Version 1.0.0, 15-ARP-1996 (WLT)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Other Functions
C
      INTEGER               ZZNSPPOK
 
C
C     Local Parameters
C
C
C     Error File Port Integer Code.
C
      INTEGER               EFILE
      PARAMETER           ( EFILE  = 5 )
C
C     Log File Port Integer Code.
C
      INTEGER               LOG
      PARAMETER           ( LOG    = 2 )
 
C
C     The number of total ports supported by this version of NSPIO.
C
      INTEGER               NPORTS
      PARAMETER           ( NPORTS = 8 )
 
C
C     The logical unit that is associated with STDOUT.
C
      INTEGER               SCREEN
      PARAMETER           ( SCREEN = 6 )
 
C
C     The maximum filename string length.
C
      INTEGER               SIZFIL
      PARAMETER           ( SIZFIL = 255 )
 
C
C     The maximum length of a message.
C
      INTEGER               SIZMSG
      PARAMETER           ( SIZMSG = 400 )
 
C
C     The maximum length of a word.
C
      INTEGER               SIZWRD
      PARAMETER           ( SIZWRD = 32 )
 
C
C     Spool Port Integer Code.
C
      INTEGER               SPOOL
      PARAMETER           ( SPOOL  = 4 )
 
C
C     Screen Port Integer Code.
C
      INTEGER               STDOUT
      PARAMETER           ( STDOUT = 1 )
 
C
C     Local Variables
C
      CHARACTER*(SIZFIL)    FILES  ( NPORTS )
      CHARACTER*(SIZMSG)    MESSGE
      CHARACTER*(SIZWRD)    PORTS  ( NPORTS )
 
      INTEGER               ID
      INTEGER               R
      INTEGER               TO
      INTEGER               UNITS  ( NPORTS )
 
      LOGICAL               ACTIVE ( NPORTS )
      LOGICAL               ERROPF
      LOGICAL               OPEN   ( NPORTS )
      LOGICAL               OPENOK
      LOGICAL               SUSPND ( NPORTS )
 
C
C     Save all local variables.
C
      SAVE
 
C
C     Initialize the PORT configuration arrays.
C
      DATA   PORTS   / 'SCREEN', 'LOG',   'SAVE',  'UTILITY',
     .                 'ERROR',  'AUX1',  'AUX2',  'AUX3'     /
 
      DATA   FILES   / ' ',      ' ',     ' ',     ' ',
     .                 ' ',      ' ',     ' ',     ' '        /
 
      DATA   UNITS   / SCREEN,   0,       0,       0,
     .                 0,        0,       0,       0          /
 
      DATA   ACTIVE  / .TRUE.,   .FALSE., .FALSE., .FALSE.,
     .                 .FALSE.,  .FALSE., .FALSE., .FALSE.    /
 
      DATA   OPEN    / .TRUE.,   .FALSE., .FALSE., .FALSE.,
     .                 .FALSE.,  .FALSE., .FALSE., .FALSE.    /
 
      DATA   SUSPND  / .FALSE.,  .FALSE., .FALSE., .FALSE.,
     .                 .FALSE.,  .FALSE., .FALSE., .FALSE.    /
 
      DATA   ERROPF  / .FALSE. /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIO' )
         CALL SIGERR( 'NSPIO(BOGUSENTRY)' )
         CALL CHKOUT( 'NSPIO' )
      END IF
 
      RETURN
 
 
C$Procedure NSPOPN ( Inspekt I/O Manager -- Open Port )
 
      ENTRY NSPOPN ( PORT, NAME )
 
C$ Abstract
C
C     Open a new port.
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
C     TEXT
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         PORT
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   String specifying which port to open.
C     NAME       I   The name of the file to open and attach to PORT.
C
C$ Detailed_Input
C
C     PORT       is a string that indicates the name of a port on which
C                to perform an operation.  Acceptable values are:
C
C                 Standard Output Port:
C
C                   'SCREEN'
C
C                 File Based Ports:
C
C                   'LOG'
C                   'SAVE'
C                   'UTILITY'
C                   'ERROR'
C                   'AUX1'
C                   'AUX2'
C                   'AUX3'
C
C     NAME       The name of a file to create and attach to a file
C                based port.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See NSPIO.
C
C$ Files
C
C     1) If PORT is a file-based port then this routine will open
C        a file with the SPICE routine TXTOPN.
C
C     2) If PORT is already attached to a file, then this file
C        is closed before PORT is attached to a new file.
C
C$ Exceptions
C
C     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
C        is signaled by ZZNSPPOK.  Note, in this case, the status of
C        all ports remains the same.
C
C     2) If PORT is file based and already open, then NSPOPN closes
C        the file attached to PORT and opens the requested new one.
C
C     3) If PORT is 'SCREEN', then this entry point does nothing.
C
C     4) If PORT is 'ERROR', then if an error occurs opening the
C        file, this routine simply leaves the port unopen and
C        returns.
C
C     4) Any errors that occur in opening the files not associated with
C        the 'SCREEN' and 'ERROR' ports are processed by TXTOPN.
C
C$ Particulars
C
C     See NSPIO.
C
C$ Examples
C
C     See NSPIO.
C
C$ Restrictions
C
C     1) NAME should point to a non-existant file that can be opened
C        for write access.
C
C     2) NAME should be a string of less than SIZFIL characters in
C        length.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 01-FEB-2000 (FST)
C
C
C-&
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPOPN' )
      END IF
 
C
C     Find the integer associated with PORT.
C
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )
 
C
C     See if an error has been signaled. If so, do nothing
C     further and return.
C
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'NSPOPN' )
         RETURN
 
      END IF
 
C
C     First check to see whether we are dealing with the SCREEN
C     port.  If we are return, do nothing and return.
C
      IF ( ID .EQ. STDOUT ) THEN
 
         CALL CHKOUT ( 'NSPOPN' )
         RETURN
 
      END IF
 
C
C     Now at this point we have a request to open a file based
C     port.  Check first to see if it is already open.
C
      IF ( OPEN(ID) ) THEN
 
C
C        If the file attached to PORT is already open, close it
C        before attaching this new file to it.
C
         CLOSE( UNIT = UNITS(ID) )
 
C
C        Now reset PORT's status.
C
         ACTIVE(ID) = .FALSE.
         OPEN  (ID) = .FALSE.
         SUSPND(ID) = .FALSE.
         FILES (ID) = ' '
 
      END IF
 
C
C     Check to see if we are opening the ERROR port.  We treat
C     this port differently from the other file based ports, since
C     if an error occurs opening the file, no error is signaled.
C     The port is simply not opened.
C
      IF ( ID .EQ. EFILE ) THEN
 
C
C        Assume there is will be no error opening the file.
C
         ERROPF = .FALSE.
 
C
C        Attempt to open the file.
C
         R = RTRIM(NAME)
         CALL ZZTXTOPN ( NAME(1:R), UNITS(ID), OPENOK )
 
C
C        If the OPEN process failed, then clear the status of the
C        port and return.
C
         IF ( .NOT. OPENOK ) THEN
 
            ACTIVE(ID) = .FALSE.
            OPEN  (ID) = .FALSE.
            SUSPND(ID) = .FALSE.
 
C
C           Leave FILES(ID) set, so that the name of the file can
C           be reported.
C
 
            FILES (ID) = NAME
 
C
C           Before returning, set ERROPF to .TRUE., since
C           this will facilitate the creation of the warning
C           message when NSPEND is invoked.
C
            ERROPF = .TRUE.
 
            CALL CHKOUT ( 'NSPOPN' )
            RETURN
 
         END IF
 
C
C     Consider all other file based ports.  For these ports we will
C     signal errors if TXTOPN is incapable of opening the file.
C
      ELSE
C
C        Open the new file.
C
         R = RTRIM(NAME)
         CALL TXTOPN ( NAME(1:R), UNITS(ID) )
 
C
C        Check FAILED(). If an error has occurred, clear PORT status,
C        check out and return.
C
         IF ( FAILED() ) THEN
 
            ACTIVE(ID) = .FALSE.
            OPEN  (ID) = .FALSE.
            SUSPND(ID) = .FALSE.
            FILES (ID) = ' '
            CALL CHKOUT ( 'NSPOPN' )
            RETURN
 
         END IF
 
      END IF
 
C
C     If we made it this far, then the file was opened successfully.
C     Set PORT status to reflect successful open.
C
      ACTIVE(ID) = .TRUE.
      OPEN  (ID) = .TRUE.
      SUSPND(ID) = .FALSE.
      FILES (ID) = NAME
 
      CALL CHKOUT( 'NSPOPN' )
      RETURN
 
 
 
 
C$Procedure NSPIOH ( Inspekt I/O Manager -- Inhibit Port )
 
      ENTRY NSPIOH ( PORT )
 
C$ Abstract
C
C     Inhibit output to a port.
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
C     TEXT
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         PORT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   String specifying which port to inhibit.
C
C$ Detailed_Input
C
C     PORT       is a string that indicates the name of a port on which
C                to perform an operation.  Acceptable values are:
C
C                 Standard Output Port:
C
C                   'SCREEN'
C
C                 File Based Ports:
C
C                   'LOG'
C                   'SAVE'
C                   'UTILITY'
C                   'ERROR'
C                   'AUX1'
C                   'AUX2'
C                   'AUX3'
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See NSPIO.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
C        is signaled by ZZNSPPOK.
C
C     2) If PORT is already inhibited, then it remains inhibited.
C
C$ Particulars
C
C     See NSPIO.
C
C$ Examples
C
C     See NSPIO.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 01-FEB-2000 (FST)
C
C
C-&
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIOH' )
      END IF
 
C
C     Find the integer associated with PORT.
C
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )
 
C
C     Inhibit I/O to the port, if no error was signaled. Note - if
C     the port is already inhibited, then this does not change it's
C     state.
C
      IF ( .NOT. FAILED() ) THEN
 
         ACTIVE(ID) = .FALSE.
 
      END IF
 
      CALL CHKOUT ( 'NSPIOH' )
      RETURN
 
 
 
 
C$Procedure NSPIOA ( Inspekt I/O Manager -- Activate Port )
 
      ENTRY NSPIOA ( PORT )
 
C$ Abstract
C
C     Activate a port.
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
C     TEXT
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         PORT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   String specifying which port to activate.
C
C$ Detailed_Input
C
C     PORT       is a string that indicates the name of a port on which
C                to perform an operation.  Acceptable values are:
C
C                 Standard Output Port:
C
C                   'SCREEN'
C
C                 File Based Ports:
C
C                   'LOG'
C                   'SAVE'
C                   'UTILITY'
C                   'ERROR'
C                   'AUX1'
C                   'AUX2'
C                   'AUX3'
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See NSPIO.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
C        is signaled by ZZNSPPOK.
C
C     2) If PORT is already active, then PORT remains active.
C
C$ Particulars
C
C     See NSPIO.
C
C$ Examples
C
C     See NSPIO.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 02-FEB-2000 (FST)
C
C
C-&
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIOA' )
      END IF
 
C
C     Find the integer associated with PORT.
C
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )
 
C
C     Activate the port, if no error was signaled. Note - if PORT was
C     already activated, then it will remain activated.
C
      IF ( .NOT. FAILED() ) THEN
 
         ACTIVE(ID) = .TRUE.
 
      END IF
 
      CALL CHKOUT ( 'NSPIOA' )
      RETURN
 
 
 
C$Procedure NSPGST ( Inspekt I/O Manager -- Get Port Status )
 
      ENTRY NSPGST ( PORT, STATUS )
 
C$ Abstract
C
C     Get the current status of a port.
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
C     TEXT
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         PORT
C     LOGICAL               STATUS ( 3 )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   String specifying which port to fetch status.
C     STATUS     O   An array of logicals that indicates port status.
C
C$ Detailed_Input
C
C     PORT       is a string that indicates the name of a port on which
C                to perform an operation.  Acceptable values are:
C
C                 Standard Output Port:
C
C                   'SCREEN'
C
C                 File Based Ports:
C
C                   'LOG'
C                   'SAVE'
C                   'UTILITY'
C                   'ERROR'
C                   'AUX1'
C                   'AUX2'
C                   'AUX3'
C
C$ Detailed_Output
C
C     STATUS     An array of logicals that describes the status of
C                a port.  A description of the values follows:
C
C                   STATUS(1) - Activity Status:
C                               .TRUE.  - the port is active
C                               .FALSE. - the port is inactive
C
C                   STATUS(2) - Open Status:
C                               .TRUE.  - the port is open
C                               .FALSE. - the port is closed
C
C                   STATUS(3) - Suspend Status:
C                               .TRUE.  - I/O on this port is suspended
C                               .FALSE. - I/O can proceed on this port
C
C
C$ Parameters
C
C     See NSPIO.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
C        is signaled by ZZNSPPOK.  In the event this happens
C        the routine does not alter the contents of STATUS.
C
C$ Particulars
C
C     See NSPIO.
C
C$ Examples
C
C     See NSPIO.
C
C$ Restrictions
C
C     1) STATUS must be an array with space for 3 logicals.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 03-FEB-2000 (FST)
C
C
C-&
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPGST' )
      END IF
 
C
C     Find the integer associated with PORT.
C
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )
 
C
C     Return the status of the port if no error was signaled.
C
      IF ( .NOT. FAILED() ) THEN
 
         STATUS(1) = ACTIVE(ID)
         STATUS(2) = OPEN  (ID)
         STATUS(3) = SUSPND(ID)
 
      END IF
 
      CALL CHKOUT ( 'NSPGST' )
      RETURN
 
 
 
 
C$Procedure NSPPST ( Inspekt I/O Manager -- Put Port Status )
 
      ENTRY NSPPST ( PORT, STATUS )
 
C$ Abstract
C
C     Put the status of a port.
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
C     TEXT
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         PORT
C     LOGICAL               STATUS ( 3 )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   String specifying which port to receive status.
C     STATUS     O   An array of logicals that indicates port status.
C
C$ Detailed_Input
C
C     PORT       is a string that indicates the name of a port on which
C                to perform an operation.  Acceptable values are:
C
C                 Standard Output Port:
C
C                   'SCREEN'
C
C                 File Based Ports:
C
C                   'LOG'
C                   'SAVE'
C                   'UTILITY'
C                   'ERROR'
C                   'AUX1'
C                   'AUX2'
C                   'AUX3'
C
C     STATUS     An array of logicals that describes the status of
C                a port.  A description of the values follows:
C
C                   STATUS(1) - Activity Status:
C                               .TRUE.  - the port is active
C                               .FALSE. - the port is inactive
C
C                   STATUS(2) - Open Status:
C                               .TRUE.  - the port is open
C                               .FALSE. - the port is closed
C
C                   STATUS(3) - Suspend Status:
C                               .TRUE.  - I/O on this port is suspended
C                               .FALSE. - I/O can proceed on this port
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See NSPIO.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
C        is signaled by ZZNSPPOK.  In the event this happens
C        the routine does not alter the status of any PORT.
C
C$ Particulars
C
C     See NSPIO.
C
C$ Examples
C
C     See NSPIO.
C
C$ Restrictions
C
C     1) The STATUS array must provide at least 3 logicals.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 03-FEB-2000 (FST)
C
C
C-&
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPPST' )
      END IF
 
C
C     Find the integer associated with PORT.
C
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )
 
C
C     Set the status of the port if no error was signaled.
C
      IF ( .NOT. FAILED() ) THEN
 
         ACTIVE(ID) = STATUS(1)
         OPEN  (ID) = STATUS(2)
         SUSPND(ID) = STATUS(3)
 
      END IF
 
      CALL CHKOUT ( 'NSPPST' )
      RETURN
 
 
 
C$Procedure NSPIOC ( Inspekt I/O Manager -- Close Port )
 
      ENTRY NSPIOC ( PORT )
 
C$ Abstract
C
C    Close a port.
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
C     TEXT
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         PORT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   String specifying which port to close.
C
C$ Detailed_Input
C
C     PORT       is a string that indicates the name of a port on which
C                to perform an operation.  Acceptable values are:
C
C                 Standard Output Port:
C
C                   'SCREEN'
C
C                 File Based Ports:
C
C                   'LOG'
C                   'SAVE'
C                   'UTILITY'
C                   'ERROR'
C                   'AUX1'
C                   'AUX2'
C                   'AUX3'
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See NSPIO.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
C        is signaled by ZZNSPPOK.  In the event this happens
C        the routine does not alter the contents of STATUS.
C
C     2) If PORT is already closed, then this routine does nothing,
C        and simply returns.
C
C     3) Attempting to "close" the screen port will have no effect.
C
C$ Particulars
C
C     See NSPIO.
C
C$ Examples
C
C     See NSPIO.
C
C$ Restrictions
C
C     1) PORT must refer to a file based port.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 03-FEB-2000 (FST)
C
C
C-&
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIOC' )
      END IF
 
C
C     Find the integer associated with PORT.
C
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )
 
C
C     Check FAILED() to see if an error was signaled, or if
C     ID refers to the SCREEN port.  In either case, return without
C     doing anything.
C
      IF ( FAILED() .OR. ( ID .EQ. STDOUT ) ) THEN
 
         CALL CHKOUT ( 'NSPIOC' )
         RETURN
 
      END IF
 
C
C     Now check to see if the port is currently closed or if the
C     requested port to close is the SCREEN port.
C
      IF ( ( .NOT. OPEN(ID) ) .OR. ( ID .EQ. STDOUT ) ) THEN
 
         CALL CHKOUT ( 'NSPIOC' )
         RETURN
 
      END IF
 
C
C     If we make it this far, then we were given an open file
C     based port.  Close the port and reset its status.
C
      CLOSE ( UNITS(ID) )
      ACTIVE(ID) = .FALSE.
      OPEN  (ID) = .FALSE.
      SUSPND(ID) = .FALSE.
      FILES (ID) = ' '
 
C
C     If we have closed the error file, then clear ERROPF.
C
      IF ( ID .EQ. EFILE ) THEN
 
         ERROPF = .FALSE.
 
      END IF
 
      CALL CHKOUT ( 'NSPIOC' )
      RETURN
 
 
 
 
C$Procedure NSPIOS ( Inspekt I/O Manager -- Suspend Port )
 
      ENTRY NSPIOS ( PORT )
 
C$ Abstract
C
C     Suspend a port.
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
C     TEXT
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         PORT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   String specifying which port to suspend.
C
C$ Detailed_Input
C
C     PORT       is a string that indicates the name of a port on which
C                to perform an operation.  Acceptable values are:
C
C                 Standard Output Port:
C
C                   'SCREEN'
C
C                 File Based Ports:
C
C                   'LOG'
C                   'SAVE'
C                   'UTILITY'
C                   'ERROR'
C                   'AUX1'
C                   'AUX2'
C                   'AUX3'
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See NSPIO.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
C        is signaled by ZZNSPPOK.  In the event this happens
C        the routine does not alter the contents of STATUS.
C
C     2) If PORT is already has it's I/O suspended, then it will
C        remain suspended.
C
C$ Particulars
C
C     See NSPIO.
C
C$ Examples
C
C     See NSPIO.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 08-FEB-2000 (FST)
C
C
C-&
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIOS' )
      END IF
 
C
C     Find the integer associated with PORT.
C
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )
 
C
C     Suspend I/O on the port, if no error was signaled. Note - if
C     PORT was already suspended, then it will remain suspended.
C
      IF ( .NOT. FAILED() ) THEN
 
C
C        Suspend I/O on PORT.
C
         SUSPND (ID) = .TRUE.
 
      END IF
 
      CALL CHKOUT ( 'NSPIOS' )
      RETURN
 
 
 
 
C$Procedure NSPIOR ( Inspekt I/O Manager -- Reopen Port )
 
      ENTRY NSPIOR ( PORT, OK )
 
C$ Abstract
C
C     Reopen a suspended port.
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
C     TEXT
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         PORT
C     LOGICAL               OK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   String specifying which port to re-open.
C     OK         O   logical that indicates a successful re-open.
C
C$ Detailed_Input
C
C     PORT       is a string that indicates the name of a port on which
C                to perform an operation.  Acceptable values are:
C
C                 Standard Output Port:
C
C                   'SCREEN'
C
C                 File Based Ports:
C
C                   'LOG'
C                   'SAVE'
C                   'UTILITY'
C                   'ERROR'
C                   'AUX1'
C                   'AUX2'
C                   'AUX3'
C
C$ Detailed_Output
C
C     OK         is a logical that indicates whether the attempt to
C                reopen a suspended port succeeded.
C
C$ Parameters
C
C     See NSPIO.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
C        is signaled by ZZNSPPOK.
C
C     2) If PORT is already not suspended, then PORT remains so and
C        OK is returned as .FALSE.
C
C$ Particulars
C
C     See NSPIO.
C
C$ Examples
C
C     See NSPIO.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 08-FEB-2000 (FST)
C
C
C-&
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIOR' )
      END IF
 
C
C     Find the integer associated with PORT.
C
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )
 
C
C     See if an error has been signaled. If so, do nothing
C     further and return.
C
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'NSPIOR' )
         RETURN
 
      END IF
 
C
C     Check to see if PORT is currently suspended.
C
      IF ( .NOT. SUSPND(ID) ) THEN
 
C
C        If it's not, then set OK to .FALSE. and return
C
         OK = .FALSE.
         CALL CHKOUT( 'NSPIOR' )
         RETURN
 
      END IF
 
C
C     Suspend I/O to this port.
C
      SUSPND(ID) = .FALSE.
 
      CALL CHKOUT( 'NSPIOR' )
      RETURN
 
 
 
 
C$Procedure NSPWLN ( Inspekt I/O Manager -- Write Line )
 
      ENTRY NSPWLN ( LINE )
 
C$ Abstract
C
C     Write a line to all open and active ports.
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
C     TEXT
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         LINE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LINE       I   is a line of text to be written to available ports.
C
C$ Detailed_Input
C
C     LINE       is a string of text that is to be written to all the
C                open, active, non-suspended ports.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See NSPIO.
C
C$ Files
C
C     1) This routine will write to any files associated with ports
C        that are open, active, and not suspended when NSPWLN is
C        called.
C
C$ Exceptions
C
C     1) If an error occurs writing the line to a particular port,
C        then this routine closes that port, resets its status, and
C        continues writing LINE to the other ports.
C
C     2) Any errors are signaled by routines in the call tree of
C        NSPWLN.
C
C$ Particulars
C
C     See NSPIO.
C
C$ Examples
C
C     See NSPIO.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 08-FEB-2000 (FST)
C
C
C-&
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPWLN' )
      END IF
 
C
C     Write to all the open, active, and non-suspended ports.
C
      DO ID = 1, NPORTS
 
         IF (  ( .NOT. SUSPND(ID) )  .AND.
     .                ACTIVE(ID)     .AND.
     .                OPEN  (ID)           ) THEN
 
C
C           Write the line to this port.
C
            TO = UNITS(ID)
            CALL WRITLN ( LINE, TO )
 
C
C           Check for and process any errors.
C
            IF ( ( ID .NE. STDOUT ) .AND.
     .           ( FAILED()       )       ) THEN
 
C
C              If we have encountered an error then close the
C              file and reset the port status.  Note we do not
C              need to reset error status to continue, since
C              WRITLN does not check RETURN().
C
               CLOSE ( UNIT = UNITS(ID) )
               ACTIVE(ID) = .FALSE.
               OPEN  (ID) = .FALSE.
               SUSPND(ID) = .FALSE.
               FILES (ID) = ' '
 
            END IF
 
         END IF
 
      END DO
 
      CALL CHKOUT ( 'NSPWLN' )
      RETURN
 
 
 
 
C$Procedure NSPEND ( Inspekt I/O Manager -- Finished with I/O )
 
      ENTRY NSPEND
 
C$ Abstract
C
C     The final entry point handles closing files and informing
C     the user of the location of these files.
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
C     TEXT
C     UTILITY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See NSPIO.
C
C$ Files
C
C     1) This routine closes the files attached to all open ports.
C
C$ Exceptions
C
C     1) If the SCREEN port is not open, it simply closes the port
C        and does not write any notifications.
C
C$ Particulars
C
C     See NSPIO.
C
C$ Examples
C
C     See NSPIO.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 09-FEB-2000 (FST)
C
C
C-&
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPEND' )
      END IF
 
C
C     If the LOG port is open, then notify the user about it's
C     location, and close it.
C
      IF ( OPEN(LOG) ) THEN
 
         CALL TRNLAT ( 'LOGFILWRITTENTO', MESSGE )
 
         IF (  ( .NOT. SUSPND(STDOUT) )  .AND.
     .                 ACTIVE(STDOUT)    .AND.
     .                 OPEN  (STDOUT)           ) THEN
 
C
C           Write the message.
C
            CALL WRITLN ( ' ',             SCREEN )
 
            R = RTRIM(MESSGE)
            CALL WRITLN ( MESSGE(1:R),     SCREEN )
 
            R = RTRIM(FILES(LOG))
            CALL WRITLN ( FILES(LOG)(1:R), SCREEN )
 
         END IF
 
      END IF
 
C
C     If the SAVE port is open, then notify the user about it's
C     location, and close it.
C
      IF ( OPEN(SPOOL) ) THEN
 
         CALL TRNLAT ( 'SAVFILWRITTENTO', MESSGE )
 
         IF (  ( .NOT. SUSPND(STDOUT) )  .AND.
     .                 ACTIVE(STDOUT)    .AND.
     .                 OPEN  (STDOUT)           ) THEN
 
C
C           Write the message.
C
            CALL WRITLN ( ' ',               SCREEN )
 
            R = RTRIM(MESSGE)
            CALL WRITLN ( MESSGE(1:R),       SCREEN )
 
            R = RTRIM(FILES(SPOOL))
            CALL WRITLN ( FILES(SPOOL)(1:R), SCREEN )
 
         END IF
 
      END IF
 
C
C     If the ERROR port is open, then notify the user about it's
C     location, and close it.
C
      IF ( OPEN(EFILE) ) THEN
 
         CALL TRNLAT ( 'ERRFILWRITTENTO', MESSGE )
 
         IF (  ( .NOT. SUSPND(STDOUT) )  .AND.
     .                 ACTIVE(STDOUT)    .AND.
     .                 OPEN  (STDOUT)           ) THEN
 
C
C           Write the message.
C
            CALL WRITLN ( ' ',               SCREEN )
 
            R = RTRIM(MESSGE)
            CALL WRITLN ( MESSGE(1:R),       SCREEN )
 
            R = RTRIM(FILES(EFILE))
            CALL WRITLN ( FILES(EFILE)(1:R), SCREEN )
 
         END IF
 
      ELSE IF ( ERROPF ) THEN
 
         CALL TRNLAT ( 'ERRFILWRITEFAIL', MESSGE )
 
         IF (  ( .NOT. SUSPND(STDOUT) )  .AND.
     .                 ACTIVE(STDOUT)    .AND.
     .                 OPEN  (STDOUT)           ) THEN
 
C
C           Write the message.
C
            CALL WRITLN ( ' ',               SCREEN )
 
            R = RTRIM(MESSGE)
            CALL WRITLN ( MESSGE(1:R),       SCREEN )
 
            R = RTRIM(FILES(EFILE))
            CALL WRITLN ( FILES(EFILE)(1:R), SCREEN )
 
         END IF
 
      END IF
 
C
C     Close all ports and restore NSPIO status to it's uninitialized
C     state.  First handle the screen port, since it's an exception.
C
      ACTIVE(STDOUT) = .TRUE.
      OPEN  (STDOUT) = .TRUE.
      SUSPND(STDOUT) = .FALSE.
 
C
C     Now reset the file based ports.
C
      DO ID = 2, NPORTS
 
C
C        Close the file associated with the port if it's open.
C
         IF ( OPEN(ID) ) THEN
 
            CLOSE ( UNIT = UNITS(ID) )
 
         END IF
C
C        Restore original port status.
C
         UNITS (ID) = 0
         FILES (ID) = ' '
         ACTIVE(ID) = .FALSE.
         OPEN  (ID) = .FALSE.
         SUSPND(ID) = .FALSE.
 
 
      END DO
 
      CALL CHKOUT ( 'NSPEND' )
      RETURN
 
 
 
 
C$Procedure NSPPFL ( Inspekt I/O Manager -- Fetch file name )
 
      ENTRY NSPPFL ( PORT, NAME )
 
C$ Abstract
C
C     Get the name of the file associated with a port.
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
C     TEXT
C     UTILITY
C
C$ Declarations
C
C
C     CHARACTER*(*)         PORT
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   String that indicates the name of the port.
C     NAME       O   String holding the filename associated with PORT.
C
C$ Detailed_Input
C
C     PORT       is a string that indicates the name of a port on which
C                to perform an operation.  Acceptable values are:
C
C                 Standard Output Port:
C
C                   'SCREEN'
C
C                 File Based Ports:
C
C                   'LOG'
C                   'SAVE'
C                   'UTILITY'
C                   'ERROR'
C                   'AUX1'
C                   'AUX2'
C                   'AUX3'
C
C$ Detailed_Output
C
C     NAME       The name of a file attached to a file based port.
C
C$ Parameters
C
C     See NSPIO.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
C        is signaled by ZZNSPPOK and NAME is set to ' '.
C
C     2) If PORT is 'SCREEN' then NSPPFL sets NAME to ' '.
C
C     3) If PORT is INACTIVE, SUSPENDED, or CLOSED, then NAME is
C        returned as ' '.
C
C$ Particulars
C
C     See NSPIO.
C
C$ Examples
C
C     See NSPIO.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 09-FEB-2000 (FST)
C
C
C-&
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPPFL' )
      END IF
 
C
C     Find the integer associated with PORT.
C
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )
 
C
C     See if an error has been signaled. If so, clear NAME
C     and return.
C
      IF ( FAILED() ) THEN
 
         NAME = ' '
         CALL CHKOUT ( 'NSPPFL' )
         RETURN
 
C
C     If the ID refers to an active, open, non-suspended port, then
C     set NAME to the name of the file.  Note: in the case when PORT
C     is 'SCREEN', the corresponding entry in the FILES array is ' '.
C
      ELSE IF (  ( .NOT. SUSPND(ID) )  .AND.
     .                   ACTIVE(ID)    .AND.
     .                   OPEN  (ID)           ) THEN
 
         NAME = FILES(ID)
 
C
C     If PORT is inactive, suspended or closed, set NAME to ' '.
C
      ELSE
 
         NAME = ' '
 
      END IF
 
      CALL CHKOUT ( 'NSPPFL' )
      RETURN
 
 
      END
