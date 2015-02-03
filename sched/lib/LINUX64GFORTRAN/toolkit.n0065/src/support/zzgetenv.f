C$Procedure      ZZGETENV ( Get environment variable value. )
 
      SUBROUTINE ZZGETENV ( ENVVAR, VALUE )
 
C$ Abstract
C
C     Get the value of a specified environment variable or VAX DCL
C     symbol, if it exists.
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
 
      CHARACTER*(*)         ENVVAR
      CHARACTER*(*)         VALUE
 
C
C     Length of an environment variable or DCL symbol name.
C
      INTEGER               ENVLEN
      PARAMETER           ( ENVLEN = 32 )
C
C     Length of an environment variable or DCL symbol value.
C
      INTEGER               VALLEN
      PARAMETER           ( VALLEN = 255 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ENVVAR     I   The name of the environment variable or symbol.
C     VALUE      O   The value of the environment variable or symbol.
C     ENVLEN     P   Maximum length of an environemt variable or symbol.
C     VALLEN     P   Maximum length of a value.
C
C$ Detailed_Input
C
C     ENVVAR   This is the name of the environment variable, or DCL
C              symbol, whose value is desired. The significant, i.e.,
C              nonblank, portion of the environment variable, or DCL
C              symbol, name may be at most ENVLEN characters in length
C              and may not contain embedded blanks.
C
C              A standard convention used for naming environment
C              variables is to use only the upper case characters
C              'A' - 'Z', the digits '0' - '9', and the underscore
C              character '_', in the names. We do not enforce this
C              convention but we strongly recommend its use for
C              interface consistency across heterogeneous computing
C              environments.
C
C              For a particular operating system and compiler the
C              maximum allowed length of an environment variable name
C              may be less than ENVLEN. Consult the appropriate
C              operating system and/or compiler manuals for details.
C
C$ Detailed_Output
C
C     VALUE    This is the value obtained for the environment variable
C              ENVVAR if it is defined. The result will be left
C              justified on output.
C
C              If any of the following are true:
C
C                 1) a value for the environment variable cannot be
C                    obtained,
C
C                 2) the significant portion of ENVVAR contains
C                    embedded blanks,
C
C                 3) the input ENVVAR is blank,
C
C                 4) The input ENVVAR contains characters other than
C                    the upper case characters 'A' - 'Z', the digits
C                    '0' - '9', and the underscore '_',
C
C                 5) The value for the environment variable is too long
C                    to fit in the available space,
C
C              then VALUE will be blank.
C
C$ Parameters
C
C     ENVLEN   The maximum allowed length of an environment variable
C              or DCL symbol name.
C
C     VALLEN   The maximum allowed length of an environment variable
C              or DCL symbol value.
C
C$ Exceptions
C
C     None.
C
C     1) If a value for the environment variable cannot be obtained,
C        a blank string will be returned.
C
C     2) If the significant portion of ENVVAR contains embedded blanks,
C        a blank string will be returned.
C
C     3) If the input ENVVAR is blank, a blank string will be returned.
C
C     4) If the value for the environment variable is too long to fit
C        in the available space, a blank string will be returned.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Call the subroutine 'GETENV( ENVVAR, VALUE )', provided for
C     UNIX compatibility. Given the name of an environment variable,
C     this subroutine storing in VALUE the value of the specified
C     environment variable or a blank string if an error occurs.
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
C     K.R. Gehringer     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 2.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 2.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 2.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 2.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 2.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 2.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 2.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 2.0.3, 21-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    Beta Version 2.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    Beta Version 2.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    Beta Version 2.0.0, 05-APR-1998 (NJB)
C
C        Added the PC-LINUX environment.
C
C-    Beta Version 1.0.0, 31-MAY-1996 (KRG)
C
C-&
 
C$ Index_Entries
C
C      get environment variable value
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
 
      LOGICAL               RETURN
C
C     Local variables
C
      CHARACTER*(VALLEN)    MYVALU
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZGETENV' )
      END IF
C
C     We do three things:
C
C        1) Check to see if the input is blank.
C        2) Attempt to get the value.
C        3) If we got a nonblank value, see if it will fit in the
C           space provided.
C
      IF ( ENVVAR .EQ. ' ' ) THEN
 
         MYVALU = ' '
 
      ELSE
 
         CALL GETENV ( ENVVAR, MYVALU )
 
         IF ( MYVALU .NE. ' ' ) THEN
 
            IF ( RTRIM (MYVALU) .GT. LEN (VALUE) ) THEN
               MYVALU = ' '
            END IF
 
         END IF
 
      END IF
 
      VALUE = MYVALU
 
      CALL CHKOUT ( 'ZZGETENV' )
      RETURN
      END
