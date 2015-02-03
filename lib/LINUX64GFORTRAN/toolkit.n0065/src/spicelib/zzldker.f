C$Procedure ZZLDKER ( Load a kernel )
 
      SUBROUTINE ZZLDKER ( FILE, NOFILE, FILTYP, HANDLE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine the architecture and type of a file and load
C     the file into the appropriate SPICE subsystem
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
C      PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         FILE
      CHARACTER*(*)         NOFILE
      CHARACTER*(*)         FILTYP
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILE       I   The name of a file to be loaded.
C     NOFILE     I   A message to issue if FILE cannot be located
C     FILTYP     O   The type of kernel.
C     HANDLE     O   The handle associated with the loaded kernel.
C
C$ Detailed_Input
C
C     FILE       is the name of a file that is anticipated to
C                be a SPICE kernel.
C
C     NOFILE     is a template for the message that should be created
C                with SETMSG if a problem is identified with FILE. The
C                message should have the form: "[text] '#' [text] #" The
C                first octothorpe ('#') will be replaced by the name of
C                the file. The second by a descriptive message.
C
C$ Detailed_Output
C
C     FILTYP     is the type of the kernel as determined by the
C                SPICE file record of the file or by various
C                heuristics.  Possible return values are:
C
C                  TEXT   ---  if FILE is interpreted as a text kernel
C                              suitable for loading via LDPOOL.  No
C                              attempt is made to distinguish between
C                              different types of text kernels.
C                  SPK   |
C                  CK    |
C                  PCK   |---  if FILE is a binary PCK file.
C                  EK    |
C
C                If a failure occurs during the attempt to load
C                the FILE, FILTYP will be returned as the blank string.
C
C     HANDLE     is the DAF or DAS handle that is associated with the
C                file.  If the FILTYP of the file is 'TEXT', HANDLE
C                will be set to zero.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the specified file does not exist, the error
C        SPICE(NOSUCHFILE) will be signaled.
C
C     2) If the specified file can be identified as unloadable
C        because it is a transfer format file, the error
C        SPICE(TRANSFERFILE) will be signaled.
C
C     3) If the specified file can be identified as unloadable
C        because it is an obsolete text E-kernel, the error
C        SPICE(TYPE1TEXTEK) will be signaled.
C
C     4) If the specified file can be recognized as a DAF/DAS file
C        but is not one of the currently recognized binary kernel
C        types, the error SPICE(UNKNOWNKERNELTYPE) will be signaled.
C
C     5) FILTYP is not sufficiently long to hold the full text of the
C        type of the kernel, the value returned will be the truncation
C        of the value.  As currently implemented this truncated type is
C        sufficient to distinguish between the various types of
C        kernels.
C
C     6) If the FILE cannot be loaded, HANDLE will be set to zero.
C
C     7) All other problems associated with the loading of FILE
C        are diagnosed by the routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is intended as a supporting routine for the
C     SPICE routine FURNSH.  It handles the task of loading
C     an arbitrary kernel without the caller having to specify
C     the type of the kernel.
C
C$ Examples
C
C     None.  (After all it's a private routine)
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
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.17.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 1.16.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 1.15.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 1.14.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.10.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 1.9.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.8.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 1.7.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 1.6.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 1.3.0, 03-OCT-2005 (EDW)
C
C        Source file zzldker.f converted to master file.
C        Modification occurred to prevent f2c's versions
C        from making the zzascii test. CSPICE now
C        includes coed to allow reading of non native text files.
C
C-    SPICELIB Version 1.2.0, 17-FEB-2004 (EDW) (BVS)
C
C        Added the ZZASCII terminator test for text files. Used a
C        working line length of 132 characters (maximum text kernel
C        line size.)
C
C-    SPICELIB Version 1.1.0, 24-JUN-2002 (EDW)
C
C        Added a call to ZZBODKIK to run the
C        NAIF_BODY_NAME/CODE read/check routine
C        whenever a text kernel loads.
C
C-    SPICELIB Version 1.0.0, 04-JUN-1999 (WLT)
C
C
C-&
C
C     SPICELIB Functions
C
      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN
 
 
C
C     Local Variables.
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               EOLTSZ
      PARAMETER           ( EOLTSZ = 5 )
 
      INTEGER               KLINSZ
      PARAMETER           ( KLINSZ = 132 )
 
      CHARACTER*(WDSIZE)    VERSN
      CHARACTER*(WDSIZE)    ARCH
      CHARACTER*(WDSIZE)    MYTYPE
      CHARACTER*(EOLTSZ)    TERMIN
      CHARACTER*(KLINSZ)    KERLIN
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN  ( 'ZZLDKER' )
 
      IF ( .NOT. EXISTS( FILE ) ) THEN
 
         CALL SETMSG ( NOFILE )
         CALL ERRCH  ( '#', FILE )
         CALL ERRCH  ( '#', 'could not be located.' )
         CALL SIGERR ( 'SPICE(NOSUCHFILE)'  )
         CALL CHKOUT ( 'ZZLDKER' )
         RETURN
 
      END IF
 
 
      CALL GETFAT ( FILE, ARCH, MYTYPE )
 
C
C     Possible values for the architecture are:
C
C        DAF -- The file is based on the DAF architecture.
C        DAS -- The file is based on the DAS architecture.
C        XFR -- The file is in a SPICE transfer file format.
C        DEC -- The file is an old SPICE decimal text file.
C        ASC -- An ASCII text file.
C        KPL -- Kernel Pool File (i.e., a text kernel)
C        TXT -- An ASCII text file.
C        TE1 -- Text E-Kernel type 1.
C         ?  -- The architecture could not be determined.
C
C     Some of these are obviously losers.
C
      IF (      ARCH .EQ. 'XFR'
     .     .OR. ARCH .EQ. 'DEC' ) THEN
 
         CALL SETMSG ( NOFILE )
         CALL ERRCH  ( '#', FILE )
         CALL ERRCH  ( '#', 'is a transfer format file. '
     .   //                 'Transfer format files cannot be '
     .   //                 'loaded. ' )
         CALL SIGERR ( 'SPICE(TRANSFERFILE)'  )
         CALL CHKOUT ( 'ZZLDKER' )
         RETURN
 
      ELSE IF ( ARCH .EQ. 'TE1' ) THEN
 
         CALL SETMSG ( NOFILE )
         CALL ERRCH  ( '#', FILE )
         CALL ERRCH  ( '#', 'is a type 1 text E-kernel.  These '
     .   //                 'files are obsolete and cannot be '
     .   //                 'loaded. ' )
         CALL SIGERR ( 'SPICE(TYPE1TEXTEK)' )
         CALL CHKOUT ( 'ZZLDKER' )
         RETURN
 
      END IF
 
C
C     That takes care of the obvious errors.  Try loading the
C     kernel.
C
      HANDLE = 0
      FILTYP = ' '
 
      IF ( ARCH .EQ. 'DAF' )      THEN
 
         IF      ( MYTYPE .EQ. 'SPK' ) THEN
 
            CALL SPKLEF ( FILE, HANDLE )
 
         ELSE IF ( MYTYPE .EQ. 'CK' ) THEN
 
            CALL CKLPF ( FILE, HANDLE )
 
         ELSE IF ( MYTYPE .EQ. 'PCK' ) THEN
 
            CALL PCKLOF ( FILE, HANDLE )
 
         ELSE
 
            CALL TKVRSN ( 'TOOLKIT', VERSN )
            CALL SETMSG ( NOFILE )
            CALL ERRCH  ( '#', FILE )
            CALL ERRCH  ( '#', 'is a "#" DAF file. This kind of '
     .      //                 'binary file is not supported in '
     .      //                 'version # of the SPICE toolkit. '
     .      //                 'Check with NAIF to see if your '
     .      //                 'toolkit version is up to date. ' )
            CALL ERRCH  ( '#', MYTYPE  )
            CALL ERRCH  ( '#', VERSN )
            CALL SIGERR ( 'SPICE(UNKNOWNKERNELTYPE)' )
            CALL CHKOUT ( 'ZZLDKER' )
            RETURN
 
         END IF
 
         FILTYP = MYTYPE
 
      ELSE IF ( ARCH .EQ. 'DAS' ) THEN
 
         IF ( MYTYPE .EQ. 'EK' ) THEN
            CALL EKLEF ( FILE, HANDLE )
         ELSE
 
            CALL TKVRSN ( 'TOOLKIT', VERSN )
            CALL SETMSG ( NOFILE )
            CALL ERRCH  ( '#', FILE )
            CALL ERRCH  ( '#', 'is a "#" DAS file.  This kind of '
     .      //                 'binary file is not supported in '
     .      //                 'version # of the SPICE toolkit. '
     .      //                 'Check with NAIF to see if your '
     .      //                 'toolkit version is up to date. ' )
            CALL ERRCH  ( '#', MYTYPE  )
            CALL ERRCH  ( '#', VERSN )
            CALL SIGERR ( 'SPICE(UNKNOWNKERNELTYPE)' )
            CALL CHKOUT ( 'ZZLDKER' )
            RETURN
 
         END IF
 
         FILTYP = MYTYPE
 
      ELSE
 
 
 
 
C
C        Check for line terminator compatibility with this platform.
C        .TRUE. means that ZZASCII will compare terminator
C        detected with the one native to this platform and will
C        stop if they don't match.
C
         CALL ZZASCII( FILE, KERLIN, .TRUE., TERMIN )
 
 
C
C        Load the file using the text file loader.
C
         CALL LDPOOL ( FILE )
 
         IF ( .NOT. FAILED() ) THEN
            FILTYP = 'TEXT'
C
C           Cause the kernel pool mechanism to perform
C           the standard error checks on the pool
C           data.
C
            CALL ZZBODKIK()
         END IF
 
      END IF
 
      CALL CHKOUT ( 'ZZLDKER' )
      RETURN
      END
