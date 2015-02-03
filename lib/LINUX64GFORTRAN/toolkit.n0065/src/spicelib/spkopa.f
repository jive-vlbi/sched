C$Procedure      SPKOPA ( SPK open for addition )
 
      SUBROUTINE SPKOPA ( FILE, HANDLE )
 
C$ Abstract
C
C    Open an existing SPK file for subsequent write.
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
C     SPK
C
C$ Keywords
C
C     SPK
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         FILE
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILE       I   The name of an existing SPK file.
C     HANDLE     O   A handle attached to the SPK file opened for write.
C
C$ Detailed_Input
C
C     FILE       is the name of an existing SPK file to which
C                you wish to append additional SPK segments.
C
C$ Detailed_Output
C
C     HANDLE     is the DAF handle attached to the file required
C                by any of the SPK writing routines.  If any exceptions
C                arise that prevent opening of the specified file for
C                writing, HANDLE will be returned with the value 0.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1)  If the file specified does not exist the error
C         'SPICE(FILENOTFOUND)' will be signalled.
C
C     2)  If the file specified is not an SPK file, the error
C         'SPICE(FILEISNOTSPK)' will be signalled.
C
C     All other exceptions are determined by routines in the
C     call tree of this routine.
C
C$ Particulars
C
C     This file provides an interface for opening existing SPK
C     files for the addition of SPK segments.  If you need
C     to open an new SPK file for writing, call the routine SPKOPN.
C
C$ Examples
C
C     Suppose you have collected data for a type 05 spk segment and
C     wish to place the new segment in an existing SPK file.  The
C     code fragment below shows one set of calls that you could perform
C     to make the addition.  (Note that you could add segments of
C     other data types by replacing the call to SPKW05 with a suitably
C     modified call to another SPKWxx routine.)
C
C     We assume that the following variables have already been
C     assigned the proper values:
C
C        BODY   (integer)  Body code for ephemeris object.
C        CENTER (integer)  Body code for the center of motion
C                          of the body.
C        FRAME  (string)   The reference frame of the states.
C        FIRST  (d.p.)     First valid time for which states can be
C                          computed in seconds past 2000.
C        LAST   (d.p.)     Last valid time for which states can
C                          be computed in seconds past 2000.
C        GM     (d.p.)     Gravitational mass of central body.
C        N      (integer)  Number of states and epochs.
C        STATES (d.p.)     Array of states (x,y,z,dx,dy,dz).
C        EPOCHS (d.p.)     Array of epochs (seconds past 2000.)
C        SEGID  (string)   Segment identifier
C
C
C     Begin by opening the file.
C
C     CALL SPKOPA ( FILE, HANDLE )
C
C     Now add the collected data as a new segment.
C
C     CALL SPKW05 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, SEGID,
C    .              GM,    N,      STATES, EPOCHS      )
C
C     Finally, close the file.
C
C     CALL SPKCLS ( HANDLE )
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 10-MAR-1999 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Open an existing SPK file for adding segments
C
C-&

C
C     SPICELIB Functions
C
 
      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN
 
 
      INTEGER               SMWDSZ
      PARAMETER           ( SMWDSZ = 8 )
 
      CHARACTER*(SMWDSZ)    ARCH
      CHARACTER*(SMWDSZ)    TYPE
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SPKOPA')
 
C
C     Until we get a legitimate handle we set HANDLE to zero.
C
      HANDLE = 0
 
C
C     First make sure the file exists.
C
      IF ( .NOT. EXISTS(FILE) ) THEN
 
         CALL SETMSG ( 'The file ''#'' is not recognized as an '
     .   //            'existing file. ' )
         CALL ERRCH  ( '#', FILE )
         CALL SIGERR ( 'SPICE(FILENOTFOUND)'  )
         CALL CHKOUT ( 'SPKOPA' )
 
         RETURN
      END IF
 
C
C     Next make sure it is an SPK file.
C
      CALL GETFAT ( FILE, ARCH, TYPE )
 
      IF ( FAILED() )THEN
         CALL CHKOUT ( 'SPKOPA' )
         RETURN
      END IF
 
      IF ( ARCH .NE. 'DAF' .OR. TYPE .NE. 'SPK' ) THEN
 
         CALL SETMSG ( 'The file ''#'' was not an SPK file.  The '
     .   //            'architecture and type of the file were '
     .   //            'found to be ''#'' and ''#'' respectively. ' )
 
         CALL ERRCH  ( '#', FILE )
         CALL ERRCH  ( '#', ARCH )
         CALL ERRCH  ( '#', TYPE )
         CALL SIGERR ( 'SPICE(FILEISNOTSPK)'  )
         CALL CHKOUT ( 'SPKOPA' )
         RETURN
      END IF
 
C
C     That's the limit of the checks performed here.  We let DAFOPW
C     handle the remaining checks.
C
      CALL DAFOPW ( FILE, HANDLE )
 
      IF ( FAILED() ) THEN
         HANDLE = 0
      END IF
 
      CALL CHKOUT ( 'SPKOPA' )
      RETURN
 
      END
