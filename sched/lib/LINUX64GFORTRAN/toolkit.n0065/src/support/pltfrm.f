C$Procedure      PLTFRM ( Get platform attributes )
 
      SUBROUTINE PLTFRM ( ROOM, N, ATTR )
C~  NEXT
C*    IMPLICIT NONE
C~~
 
C$ Abstract
C
C     Return platform id and various attributes of the platform
C     environment
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
 
      INTEGER               ROOM
      INTEGER               N
      CHARACTER*(*)         ATTR ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ROOM       I   amount of room available for returning attributes
C     N          O   number of attributes returned
C     ATTR       O   string values of various attributes
C
C$ Detailed_Input
C
C     ROOM        is the amount of space available in the character
C                 string array ATTR for returning platform attributes.
C
C$ Detailed_Output
C
C     N           is the actual number of attributes returned.  N will
C                 always be less than or equal to ROOM.
C
C     ATTR        is an array of attributes about the platform
C                 and environment on which this routine is running.
C
C                 ATTR will contain in the following order
C
C                 1) machine name    :   HP, NEXT, PC, SGI, etc.
C                 2) fortran compiler:   HP , ABSOFT, etc.
C                 3) Operating System
C
C                 Other items may be added later.  Check your local
C                 listing for details.
C
C                 If a value is not available it will be returned
C                 with the value '<unavailable>'
C
C                 The routine that calls this should declare
C                 ATTR to be at least CHARACTER*(32).
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
C     Error free.
C
C     1)  If ROOM is less than or equal to zero, the N will be
C         returned with a value of zero and ATTR will not be
C         changed from it's input state.
C
C$ Particulars
C
C     This routine serves to identify the platforma and compiler
C     used in creating SPICELIB.  It is provided so that routines
C     and programs can make run-time decisions based upon the
C     ambient fortran environment.
C
C$ Examples
C
C     This routine could be used so that a single routine
C     can be written that translates the meaning of IOSTAT values
C     that depend upon the machine and compiler.  At run time
C     the routine can look up the appropriate message to associate
C     with an IOSTAT value.
CC
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
C-    Support Version 1.4.0, 28-JUL-1999 (WLT)
C
C        Changed routine to call new SPICE private routine ZZPLATFM
C        The routine is no longer environment specific.
C
C-    Inspekt Version 1.3.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    Inspekt Version 1.3.0, 05-APR-1998 (NJB)
C
C        Added the PC-LINUX environment.
C
C-    Inspekt Version 1.2.0, 12-AUG-1996 (WLT)
C
C        Added the DEC-OSF1 environment.
C
C-    Inspekt Version 1.1.0, 16-JUN-1995 (WLT)
C
C        Master version of original machine dependent collection.
C        Copyright notice added.
C
C-    Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C        This is the configured version of the Command Loop
C        software as of May 4, 1994
C
C
C-    SPICELIB Version 1.0.0, 21-APR-1994 (WLT)
C
C
C-&
 
 
C$ Index_Entries
C
C     Determine the machine, OS and fortran version.
C
C-&
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               NATTR
      PARAMETER           ( NATTR  =  3 )
 
      CHARACTER*(WDSIZE)    ITEM ( NATTR )
 
      INTEGER               LIMIT
      INTEGER               I
 
      ITEM(1) = 'SYSTEM'
      ITEM(2) = 'COMPILER'
      ITEM(3) = 'O/S'
 
      LIMIT = MAX( 0, MIN ( NATTR, ROOM ) )
 
      DO I = 1, LIMIT
         CALL ZZPLATFM ( ITEM(I), ATTR(I) )
      END DO
 
      N = LIMIT
 
      RETURN
      END
