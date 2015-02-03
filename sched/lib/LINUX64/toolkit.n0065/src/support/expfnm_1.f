C$Procedure      EXPFNM_1 ( Expand a filename )
 
      SUBROUTINE EXPFNM_1 ( INFIL, OUTFIL )
 
C$ Abstract
C
C     Given a filename, expand it to be a full filename.
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
C     FILES
C     UTILITY
C
C$ Declarations
 
 
      CHARACTER*(*)         INFIL
      CHARACTER*(*)         OUTFIL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INFIL      I   The filename to be expanded.
C     OUTFIL     O   The expanded filename.
C
C$ Detailed_Input
C
C     INFIL      is the filename to be expanded.
C
C$ Detailed_Output
C
C     OUTFIL     is the expanded filename. If no expansion could be
C                done, the value of OUTFIL is equal to the value of
C                INFIL. OUTFIL may not overwrite INFIL.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input filename is blank, begins with blank characters,
C        or has embedded blanks in it, the error SPICE(BADFILENAME)
C        is signalled.
C
C     2) If the expanded filename is too long to fit into the
C        output string, the error SPICE(STRINGTOOSMALL) is signalled.
C
C     3) The output string may not overwrite the input string.
C
C     4) If no expansion of the input filename can be done, the
C        output filename is assigned the value of the input filename.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The input filename may not be blank, begin with blank characters,
C     nor may it it contain embedded blanks. As a general rule,
C     SPICELIB routines do not allow blank characters as part of a
C     filename.
C
C     Unix platforms:
C
C     On the Unix platforms, a filename containing an environment
C     variable must be expanded completely before FORTRAN can do
C     anything with it. FORTRAN interacts directly with the kernel, and
C     as a result does not pass input filenames through the shell
C     for expansion of environment variables.
C
C     VAX/VMS, Alpha/OpenVMS platforms:
C
C     The operating system does filname expansion itself, so this
C     routine currently does not expand the name.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     Unix platforms:
C
C     This routine cannot be used to expand a file name whose form
C     is '~xxx', where xxx is an account name.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     H.A. Neilan    (JPL)
C
C$ Version
C
C-    Beta Version 3.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    Beta Version 3.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    Beta Version 3.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    Beta Version 3.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    Beta Version 3.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    Beta Version 3.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    Beta Version 3.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    Beta Version 3.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    Beta Version 3.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    Beta Version 3.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    Beta Version 3.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    Beta Version 3.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    Beta Version 3.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    Beta Version 3.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    Beta Version 3.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    Beta Version 3.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    Beta Version 3.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    Beta Version 3.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    Beta Version 3.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    Beta Version 3.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    Beta Version 3.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    Beta Version 3.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    Beta Version 3.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    Beta Version 3.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    Beta Version 3.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    Beta Version 3.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    Beta Version 3.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are WIN-NT
C
C-    Beta Version 3.0.3, 21-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    Beta Version 3.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    Beta Version 3.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    Beta Version 3.0.0, 05-APR-1998 (NJB)
C
C        Added references to the PC-LINUX environment.
C
C-    Beta Version 2.1.0, 5-JAN-1995 (HAN)
C
C        Removed Sun Solaris environment since it is now the same
C        as the Sun OS 4.1.x environment.
C        Removed DEC Alpha/OpenVMS environment since it is now the
C        same as the VAX environment.
C
C-    Beta Version 2.0.0, 08-JUL-1994 (HAN)
C
C        The capability of resolving a Unix filename that contains
C        an environment variable directory specificiation plus a
C        filename has been added.
C
C-    Beta Version 1.0.0, 06-APR-1992 (HAN)
C
C-&
 
C$ Index_Entries
C
C     expand a filename
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               POS
      INTEGER               RTRIM
 
C
C     Parameters
C
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 255 )
 
 
C
C     Local variables
C
      INTEGER               BLANK
      INTEGER               SLASH
 
      CHARACTER*(MAXLEN)    WORD
      CHARACTER*(MAXLEN)    DIR
 
      INTEGER               INLEN
      INTEGER               WRDLEN
      INTEGER               DIRLEN
      INTEGER               OUTLEN
      INTEGER               KEEP
      INTEGER               NEED
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EXPFNM_1' )
      END IF
 
 
C
C     If the input filename is blank, that's an error.
C
      IF ( INFIL .EQ. ' ' ) THEN
         CALL SETMSG ( 'The input filename ''#'' was blank.' )
         CALL ERRCH  ( '#', INFIL                            )
         CALL SIGERR ( 'SPICE(BADFILENAME)'                  )
         CALL CHKOUT ( 'EXPFNM_1'                            )
         RETURN
      END IF
 
C
C     If there are blanks anywhere in the filename, SPICELIB
C     considers the filename to be invalid.
C
      BLANK = POS( INFIL(1:RTRIM(INFIL)), ' ', 1 )
 
      IF ( BLANK .NE. 0 ) THEN
         CALL SETMSG ( 'The input filename ''#'' had blank '         //
     .                 'characters in it.'                            )
         CALL ERRCH  ( '#', INFIL                                     )
         CALL SIGERR ( 'SPICE(BADFILENAME)'                           )
         CALL CHKOUT ( 'EXPFNM_1'                                     )
         RETURN
      END IF
 
C
C     Look for a slash in the filename.
C
      SLASH = POS( INFIL, '/', 1 )
 
C
C     If we found a slash in a position other than the first
C     character position, we want to examine the word that
C     comes before it just in case it is an environment
C     variable.
C
      IF ( SLASH .GT. 1 ) THEN
 
         WORD   = ( INFIL(1: SLASH - 1 ) )
 
         CALL GETENV ( WORD, DIR  )
 
C
C        If the word was an environment variable, then construct
C        the expanded filename. If it wasn't, just return the original
C        input filename.
C
         IF ( DIR .NE. ' ' ) THEN
 
            OUTFIL = INFIL
 
            INLEN  = RTRIM( INFIL  )
            WRDLEN = RTRIM( WORD   )
            DIRLEN = RTRIM( DIR    )
            OUTLEN = LEN( OUTFIL )
            KEEP   = INLEN - WRDLEN
            NEED   = KEEP + DIRLEN
 
C
C           If the output filename length is not long enough for
C           the substitution, signal an error. Otherwise, substitute
C           in the new value.
C
            IF ( NEED .GT. OUTLEN ) THEN
 
               CALL SETMSG ( 'The expanded filename for the input '   //
     .                       'filename ''#'' exceeded the length '    //
     .                       'of the output filename. The expanded '  //
     .                       'name was # characters too long.'         )
               CALL ERRCH  ( '#', INFIL                                )
               CALL ERRINT ( '#', NEED - OUTLEN                        )
               CALL SIGERR ( 'SPICE(STRINGTOOSMALL)'                   )
               CALL CHKOUT ( 'EXPFNM_1'                                )
               RETURN
 
            ELSE
 
               CALL REPSUB ( INFIL, 1, SLASH - 1,
     .                       DIR(1:RTRIM(DIR)), OUTFIL )
 
            END IF
 
         ELSE
 
            OUTFIL = INFIL
 
         END IF
 
      ELSE
 
C
C        No slashes are in the filename, so it's just an easy case.
C
C        It's possible that the entire filename is an environment
C        variable. If it's not, then just return the input filename.
C
 
         CALL GETENV ( INFIL, OUTFIL )
 
         IF ( OUTFIL .EQ. ' ' ) THEN
            OUTFIL = INFIL
         END IF
 
      END IF
 
      CALL CHKOUT ( 'EXPFNM_1' )
      RETURN
      END
