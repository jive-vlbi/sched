C$Procedure      RDKER ( Read a kernel file )
 
      SUBROUTINE RDKER ( KERNEL, LINE, NUMBER, EOF )
 
C$ Abstract
C
C     Open and read the contents of a SPICE ASCII kernel file.
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
C     FILES
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         KERNEL
      CHARACTER*(*)         LINE
      INTEGER               NUMBER
      LOGICAL               EOF
 
C$ Brief_I/O
C
C     VARIABLE  I/O  ENTRY
C     --------  ---  --------------------------------------------------
C     KERNEL     I   RDKNEW
C     LINE       O   RDKDAT
C     NUMBER     O   RDKLIN
C     EOF        O   RDKDAT
C
C$ Detailed_Input
C
C     All input is through entry RDKNEW.
C
C$ Detailed_Output
C
C     All output is through entry RDKDAT.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If RDKER is called directly, the error SPICE(BOGUSENTRY) is
C        signalled.
C
C$ Files
C
C     The SPICE ASCII kernel file KERNEL is opened by RDKNEW and read
C     by RDKDAT.  The entry point RDKLIN is available for reporting
C     the name of the open file and the number of the last line that
C     was read from that file.
C
C$ Particulars
C
C     RDKER should never be called directly, but should instead be
C     accessed only through its entry points, RDKNEW, RDKDAT and
C     RDKLIN.
C
C$ Examples
C
C     In the following example, RDKNEW and RDKDAT are used to read
C     the contents of a kernel file.
C
C     Let the file KERNEL contain the following lines.
C
C        =============================================================
C
C        DELTA_T_A is defined to be 32.184 seconds, and should not
C        be changed except under the most unusual circumstances.
C
C        \begindata
C
C        DELTA_T_A       =   32.184
C
C        \begintext
C
C        The next three items determine the relativistic correction
C        in the difference ET - TAI. To turn the correction off,
C        just set K to zero.
C
C        \begindata
C
C        K               =    1.657D-3
C        ORBIT_ECC       =    1.671D-2
C        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
C
C        =============================================================
C
C     Then the code fragment
C
C        CALL RDKNEW ( KERNEL )
C        CALL RDKDAT ( LINE, EOF )
C
C        DO WHILE ( (.NOT. EOF) .AND. ( .NOT. FAILED () ) )
C           WRITE (6,*) LINE
C           CALL RDKDAT ( LINE, EOF )
C        END DO
C
C     prints the following lines.
C
C        =============================================================
C        DELTA_T_A       =   32.184
C        K               =    1.657D-3
C        ORBIT_ECC       =    1.671D-2
C        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
C        =============================================================
C
C$ Restrictions
C
C     The input file must be opened and initialized by RDKNEW prior
C     to the first call to RDKDAT.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     H.A. Neilan     (JPL)
C     M.J. Spencer    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.6.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 3.5.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 3.4.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 3.3.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 3.2.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 3.1.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 3.0.0, 11-FEB-2008 (NJB)
C
C        Entry points RDKNEW and RDKDAT have been updated so as to be
C        able to parse text kernel lines containing tab characters.
C
C-    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 2.3.0, 14-NOV-2005 (BVS)
C
C        Reinstated HP_C environment.
C
C-    SPICELIB Version 2.2.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
C
C        File rdker.f made a master file so as to
C        add the ZZSETNNREAD call. This call will exist
C        only in FORTRAN source intended for conversion
C        to C by the f2c utility.
C
C        The ZZSETNNREAD call activates and deactivates
C        the non-native text line read capability for the
C        CSPICE toolkit.
C
C-     SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
C
C        Corrected ENDIF to END IF.
C
C-     SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
C
C         The entry point RDKLIN was added.
C
C-     SPICELIB Version 1.3.0, 22-SEP-1993 (NJB)
C
C         Updated for port to NeXT.  The "previous kernel" is now closed
C         only if there actually was a previous kernel.
C
C-     SPICELIB Version 1.2.0, 01-JUN-1992 (MJS)
C
C         RDKER now initializes the variables BEGDAT and BEGTXT
C         in a portable way. On the first valid entry to this routine,
C         the backslash character in the form CHAR(92) is concatenated
C         individually to 'begindata' and 'begintext'.
C
C-     SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.1.0, 7-DEC-1990 (HAN)
C
C         The declarations for BEGDAT and BEGTXT were changed from
C         CHARACTER*10 to CHARACTER*(*).
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     read a kernel file
C
C-&
 
 
 
 
C$ Revisions
C
C-     SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
C
C         The entry point RDKLIN was added.
C
C-     SPICELIB Version 1.3.0, 22-SEP-1993 (NJB)
C
C         Updated for port to NeXT.  The "previous kernel" is now closed
C         only if there actually was a previous kernel.
C
C         In the last version of this routine, on the first entry into
C         the routine, the variable FILE, which records the name of
C         the last kernel accessed, was passed to CLTEXT.  CLTEXT
C         executed an INQUIRE statement using this name, which was
C         not initialized.  On the NeXT, this caused the INQUIRE
C         statement to fail.
C
C
C-     SPICELIB Version 1.2.0, 01-JUN-1992 (MJS)
C
C         RDKER now initializes the variables BEGDAT and BEGTXT
C         in a portable way. On the first valid entry to this routine,
C         the backslash character in the form CHAR(92) is concatenated
C         individually to 'begindata' and 'begintext'. As a result of
C         this change, this module is no longer considered environment
C         specific. All references in the header to the previous method
C         of initialization were removed.
C
C         FILE is now initialized to ' '. Before this modification, if
C         a call to RDKDAT was performed prior to RDKNEW, RDTEXT
C         would have printed out garbage (on some machines) in its
C         error message when notifiying the user that it couldn't read
C         from FILE.
C
C-     SPICELIB Version 1.1.0, 7-DEC-1990 (HAN)
C
C         The declarations for BEGDAT and BEGTXT were changed from
C         CHARACTER*10 to CHARACTER*(*). The fixed length of 10 was
C         not long enough.
C
C-     Beta Version 1.1.0, 9-MAR-1989 (HAN)
C
C         Moved the declaration of the parameters BEGDAT and
C         BEGTXT from the code to the Declarations section.
C         Filled out the Brief I/O and Parameters sections.
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
 
      LOGICAL               RETURN
      LOGICAL               FAILED
 
C
C     Local parameters
C
C
C     Because some environments (such as the SUN) treat the backslash
C     character as a special character, some gyrations are needed to
C     put it into a variable in a "portable" way. This is the reason
C     for the following block of declarations. Admittedly this is
C     bizarre, but it works.
C
      CHARACTER*(*)         DAT
      PARAMETER           ( DAT = 'begindata' )
 
      CHARACTER*(*)         TXT
      PARAMETER           ( TXT = 'begintext' )
 
      CHARACTER*(*)         BLANK
      PARAMETER           ( BLANK = ' ' )
 
C
C     The ASCII decimal code for the tab character is 9.
C
      INTEGER               TABCDE
      PARAMETER           ( TABCDE = 9 )
 
      INTEGER               MRKLEN
      PARAMETER           ( MRKLEN = 10 )
 
      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )
 
      INTEGER               INTEXT
      PARAMETER           ( INTEXT = 1 )
 
      INTEGER               INDATA
      PARAMETER           ( INDATA = INTEXT + 1 )
 
      INTEGER               ENDOFF
      PARAMETER           ( ENDOFF = INDATA + 1 )
 
C
C     Local variables
C
      CHARACTER*(MRKLEN)    BEGDAT
      CHARACTER*(MRKLEN)    BEGTXT
      CHARACTER*(FILSIZ)    FILE
      CHARACTER*(LNSIZE)    FIRST
 
      INTEGER               I
      INTEGER               R
      INTEGER               STATUS
      INTEGER               LINNUM
 
      LOGICAL               END
      LOGICAL               FRSTIM
 
C
C     Save EVERYTHING.
C
      SAVE
 
C
C     Initial values
C
      DATA                  FRSTIM / .TRUE. /
      DATA                  FILE   / ' '    /
      DATA                  LINNUM /  0     /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RDKER' )
      END IF
 
C
C     Calling RDKER directly is a serious breach of protocol.
C     If RDKER is called, an error is signalled.
C
 
      CALL SETMSG ( 'RDKER: You have called an entry which '   //
     .              'performs no run-time function. This '     //
     .              'may indicate a bug. Please check the '    //
     .              'documentation for the subroutine RDKER.'     )
 
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
 
      CALL CHKOUT ( 'RDKER' )
      RETURN
 
 
 
C$Procedure RDKNEW ( Open and initialize a new kernel file )
 
      ENTRY RDKNEW ( KERNEL )
 
C$ Abstract
C
C     Open and initialize a SPICE ASCII kernel file.
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
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         KERNEL
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     KERNEL     I   Kernel file.
C
C$ Detailed_Input
C
C     KERNEL      is the name of the SPICE ASCII kernel file to be
C                 opened and initialized.
C
C$ Detailed_Output
C
C      None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     The SPICE ASCII kernel file KERNEL is opened by RDKNEW and read
C     by RDKDAT.
C
C$ Particulars
C
C     RDKNEW should be called prior to the first call to RDKDAT.
C     RDKNEW opens the kernel file and RDKDAT reads the lines of
C     data in the file.
C
C$ Examples
C
C     In the following example, RDKNEW and RDKDAT are used to read
C     the contents of a kernel file.
C
C     Let the file KERNEL contain the following lines.
C
C        =============================================================
C
C        DELTA_T_A is defined to be 32.184 seconds, and should not
C        be changed except under the most unusual circumstances.
C
C        \begindata
C
C        DELTA_T_A       =   32.184
C
C        \begintext
C
C        The next three items determine the relativistic correction
C        in the difference ET - TAI. To turn the correction off,
C        just set K to zero.
C
C        \begindata
C
C        K               =    1.657D-3
C        ORBIT_ECC       =    1.671D-2
C        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
C
C        =============================================================
C
C     Then the code fragment
C
C        CALL RDKNEW ( KERNEL )
C        CALL RDKDAT ( LINE, EOF )
C
C        DO WHILE ( (.NOT. EOF) .AND. ( .NOT. FAILED () ) )
C           WRITE (6,*) LINE
C           CALL RDKDAT ( LINE, EOF )
C        END DO
C
C     prints the following lines.
C
C        =============================================================
C        DELTA_T_A       =   32.184
C        K               =    1.657D-3
C        ORBIT_ECC       =    1.671D-2
C        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
C        =============================================================
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
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 11-FEB-2008 (NJB)
C
C        This entry point has been updated so as to be
C        able to parse text kernel lines containing tab
C        characters.
C
C-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
C
C        File rdker.f made a master file so as to
C        add the ZZSETNNREAD call. This call will exist
C        only in FORTRAN source intended for conversion
C        to C by the f2c utility.
C
C        The ZZSETNNREAD call activates and deactivates
C        the non-native text line read capability for the
C        CSPICE toolkit.
C
C-     SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
C
C         The entry point RDKLIN was added.
C
C-     SPICELIB Version 1.2.0, 01-JUN-1992 (MJS)
C
C         RDKER now initializes the variables BEGDAT and BEGTXT
C         in a portable way. On the first valid entry to this routine,
C         the backslash character in the form CHAR(92) is concatenated
C         individually to 'begindata' and 'begintext'.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     open and initialize a new kernel file
C
C-&
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RDKNEW' )
      END IF
 
C
C     Initialize the data delimiters if it hasn't been done already.
C
      IF ( FRSTIM ) THEN
 
         BEGDAT = CHAR(BSLASH) // DAT
         BEGTXT = CHAR(BSLASH) // TXT
 
         FRSTIM = .FALSE.
 
      ELSE
C
C        Close the previous file, if it hasn't been closed already.
C
         CALL CLTEXT ( FILE )
 
      END IF
 
C
C     Close the new file, too, in case they are the same. No sense
C     burning up logical units.
C
      CALL CLTEXT ( KERNEL )
 
C
C     Read the first line of the file. It can't possibly be a data
C     line, since data must be preceded by a \begindata marker, so
C     we needn't take any pains to save it.
C
C     We also initialize LINNUM to 1 so we know
C     the line number of the last line read and can return this
C     information from RDKLIN.
C
 
 
      CALL RDTEXT ( KERNEL, FIRST, END )
 
 
C
C     Replace any tab characters with blanks.
C
      R = RTRIM ( FIRST )
 
      DO I = 1, R
 
         IF ( ICHAR(FIRST(I:I)) .EQ. TABCDE ) THEN
 
            FIRST(I:I) = ' '
 
         END IF
 
      END DO
 
 
      CALL LJUST ( FIRST, FIRST )
      LINNUM =  1
 
C
C     The first line is enough to set the status for subsequent
C     calls to RDKDAT.
C
      IF ( END ) THEN
 
         STATUS = ENDOFF
         CALL CLTEXT ( KERNEL )
 
      ELSE IF ( FIRST .EQ. BEGDAT ) THEN
         STATUS = INDATA
 
      ELSE
         STATUS = INTEXT
      END IF
 
C
C     Save the name of the file for future reference.
C
      FILE = KERNEL
 
 
      CALL CHKOUT ( 'RDKNEW' )
      RETURN
 
 
 
C$Procedure RDKDAT ( Read the next data line from a kernel file )
 
      ENTRY RDKDAT ( LINE, EOF )
 
C$ Abstract
C
C     Read the next line of data from a SPICE ASCII kernel file.
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
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         LINE
C     LOGICAL               EOF
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LINE       O   Next line of kernel data.
C     EOF        O   End of file indicator.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     LINE        is the next line of data from the kernel file
C                 most recently opened by NEWKER. Data lines are
C                 non-blank lines which lie between \begindata
C                 and \begintext markers. Lines are returned left
C                 justified.
C
C     EOF         is true when the end of the kernel file has been
C                 reached, and is false otherwise. The kernel file
C                 is closed automatically when the end of the file
C                 is reached.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     The SPICE ASCII kernel file KERNEL is opened by RDKNEW and read
C     by RDKDAT.
C
C$ Particulars
C
C     RDKDAT is used internally by RDKVAR to retrieve successive lines
C     of data from the current kernel file. It exists primarily to
C     relieve RDKVAR of the responsibility of dealing with comment
C     blocks and blank lines.
C
C$ Examples
C
C     In the following example, RDKNEW and RDKDAT are used to read
C     the contents of a kernel file.
C
C     Let the file KERNEL contain the following lines.
C
C        =============================================================
C
C        DELTA_T_A is defined to be 32.184 seconds, and should not
C        be changed except under the most unusual circumstances.
C
C        \begindata
C
C        DELTA_T_A       =   32.184
C
C        \begintext
C
C        The next three items determine the relativistic correction
C        in the difference ET - TAI. To turn the correction off,
C        just set K to zero.
C
C        \begindata
C
C        K               =    1.657D-3
C        ORBIT_ECC       =    1.671D-2
C        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
C
C        =============================================================
C
C     Then the code fragment
C
C        CALL RDKNEW ( KERNEL )
C        CALL RDKDAT ( LINE, EOF )
C
C        DO WHILE ( (.NOT. EOF) .AND. ( .NOT. FAILED () ) )
C           WRITE (6,*) LINE
C           CALL RDKDAT ( LINE, EOF )
C        END DO
C
C     prints the following lines.
C
C        =============================================================
C        DELTA_T_A       =   32.184
C        K               =    1.657D-3
C        ORBIT_ECC       =    1.671D-2
C        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
C        =============================================================
C
C$ Restrictions
C
C     The input file must be opened and initialized by NEWKER prior
C     to the first call to RDKDAT.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     H.A. Neilan     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 11-FEB-2008 (NJB)
C
C        This entry point has been updated so as to be
C        able to parse text kernel lines containing tab
C        characters.
C
C-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
C
C        File rdker.f made a master file so as to
C        add the ZZSETNNREAD call. This call will exist
C        only in FORTRAN source intended for conversion
C        to C by the f2c utility.
C
C        The ZZSETNNREAD call activates and deactivates
C        the non-native text line read capability for the
C        CSPICE toolkit.
C
C-    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
C
C        Corrected ENDIF to END IF.
C
C-    SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
C
C        The entry point RDKLIN was added.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     read the next data line from a kernel file
C
C-&
 
C$ Revisions
C
C-     Beta Version 2.0.0, 23-OCT-1989 (HAN)
C
C        A FAILED test was added to the DO-loop which reads
C        lines in the kernel file.
C
C        If the error action was set to 'RETURN' an infinite loop
C        could have resulted if RDTEXT failed and the loop conditions
C        were satisfied.
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RDKDAT' )
      END IF
 
C
C     If the previous call detected the end of the file,
C     this one should do the same.
C
      IF ( STATUS .EQ. ENDOFF ) THEN
         EOF = .TRUE.
         CALL CHKOUT ( 'RDKDAT' )
         RETURN
      END IF
 
C
C     Well, at least we can try to read a line. Adjust the status as
C     needed, return if appropriate, read another line if necessary.
C     Basically, we're looking for a non-blank line in a data segment.
C
C     Note that after every read, we increment LINNUM so we know
C     the line number of the last line read and can return this
C     information from RDKLIN.
C
      LINE = BLANK
 
      DO WHILE ( ( .NOT. FAILED() )          .AND.
     .           ( STATUS .EQ. INTEXT  .OR.  LINE .EQ. BLANK ) )
 
 
         CALL RDTEXT ( FILE, LINE, EOF )
 
 
C
C        Replace any tab characters with blanks.
C
         R = RTRIM ( LINE )
 
         DO I = 1, R
 
            IF (  ICHAR(LINE(I:I)) .EQ. TABCDE ) THEN
 
               LINE(I:I) = ' '
 
            END IF
 
         END DO
 
 
         CALL LJUST ( LINE, LINE )
         LINNUM = LINNUM + 1
 
 
         IF ( EOF ) THEN
            STATUS = ENDOFF
            CALL CLTEXT ( FILE )
            CALL CHKOUT ( 'RDKDAT' )
            RETURN
 
         ELSE IF ( LINE .EQ. BEGTXT ) THEN
            STATUS = INTEXT
 
         ELSE IF ( LINE .EQ. BEGDAT ) THEN
            STATUS = INDATA
            LINE   = BLANK
         END IF
 
      END DO
 
 
      CALL CHKOUT ( 'RDKDAT' )
      RETURN
 
 
C$Procedure      RDKLIN ( Reading kernel at line number )
 
      ENTRY      RDKLIN ( KERNEL, NUMBER )
 
C$ Abstract
C
C     Return the name of file and line number of the last line read by
C     the entry point RDKDAT.
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
C      None.
C
C$ Keywords
C
C       UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         FILE
C     INTEGER               NUMBER
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      KERNEL     O   The name of the current file that is being read
C      NUMBER     O   The line number of the last line read in the file
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     KERNEL      is the name of the last file supplied via a call
C                 to RDKNEW.  If no call to RDKNEW have been made
C                 KERNEL is returned as a blank.  If KERNEL is not
C                 sufficiently long to hold th name of the file, the
C                 file name will be truncated on the right.
C
C     NUMBER      is the number of the last line in KERNEL returned by
C                 a call to RDKDAT.  If no call to RDKNEW or RDKDAT
C                 have been made NUMBER is returned with the value 0.
C
C
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If no calls to RDKNEW have been made, KERNEL is returned as
C        a blank and NUMBER is returned with the value 0.
C
C     2) If no calls to RDKDAT have been made but RDKNEW has been
C        called NUMBER is returned with the value 1.
C
C     3) If KERNEL is not sufficiently long to hold the name of the
C        file being read, the name will be truncated on the right.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This routine is a utility to aid in determining the last
C     line read in a text file that is being read via RDKDAT.
C
C     It is particular useful in pointing out the location of
C     an error in an input file.
C
C$ Examples
C
C     Suppose that you are processing a file and have detected an
C     error in the syntax in the file.  The following code fragment
C     illustrates how you can use this routine to inform a user of
C     the location of the error in the file.
C
C        CALL RDKLIN ( FILE, NUMBER )
C        R =  RTRIM  ( FILE )
C
C        WRITE (*,*) 'An error occurred while reading line ', NUMBER
C        WRITE (*,*) 'of the file ''', FILE(1:R), ''''
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
C
C        File rdker.f made a master file so as to
C        add the ZZSETNNREAD call. This call will exist
C        only in FORTRAN source intended for conversion
C        to C by the f2c utility.
C
C        The ZZSETNNREAD call activates and deactivates
C        the non-native text line read capability for the
C        CSPICE toolkit.
C
C-    SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Determine the last line read from a kernel file.
C
C-&
 
C
C     Not much to do here.  Just copy the information and return.
C
      KERNEL = FILE
      NUMBER = LINNUM
 
      RETURN
      END
