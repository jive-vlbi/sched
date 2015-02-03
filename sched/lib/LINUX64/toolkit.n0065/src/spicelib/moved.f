C$Procedure  MOVED  ( Move a double precision array to another )
 
      SUBROUTINE MOVED ( ARRFRM, NDIM, ARRTO )
 
C$ Abstract
C
C     Copy the elements of one double precision array into another
C     array.
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
C     ARRAY
C
C$ Declarations
 
      DOUBLE PRECISION      ARRFRM ( * )
      INTEGER               NDIM
      DOUBLE PRECISION      ARRTO  ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O              DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ARRFRM     I     Double precision array to be moved.
C     NDIM       I     Number of elements to copy, i.e. the dimension
C                      of ARRFRM and ARRTO.
C     ARRTO      O     Destination array.
C
C$ Detailed_Input
C
C     ARRFRM     Array from which to copy items.
C
C     NDIM       Number of items to copy.
C
C$ Detailed_Output
C
C     ARRTO      Array to which items should be copied.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If NDIM < 1, the output array is returned unchanged.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is simply shorthand for the following 3 lines of
C     code.
C
C        DO I = 1, NDIM
C           ARRTO(I) = ARRFRM(I)
C        END DO
C
C$ Examples
C
C     Often one needs to make a temporary copy of an array so that
C     it can be manipulated without altering the original array.
C     As pointed out in particulars, you could just do this within
C     the code that needs the copy.  However, if you have several
C     arrays to copy, you can cut the number of lines of code that
C     are needed by a third.
C
C     For example:
C
C        DO I = 1, 19
C           TEMPA(I) = A(I)
C        END DO
C
C        DO I = 1, 38
C           TEMPB(I) = B(I)
C        END DO
C
C     Can be rewritten as
C
C        CALL MOVED ( A, 19, TEMPA )
C        CALL MOVED ( B, 38, TEMPB )
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
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C     W.M. Owen       (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.16.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 2.15.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 2.14.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 2.13.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 2.10.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.9.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 2.8.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.7.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 2.6.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 2.5.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 2.3.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 2.1.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 2.1.0, 09-NOV-2001 (NJB)
C
C        Bug fix:  code for PC-LINUX and PC-DIGITAL environments
C        was changed to prevent corruption of bit patterns inserted
C        into double precision numbers via EQUIVALENCE statements.
C        Now the routine memmove is called to effect the double
C        precision assignments performed by this routine.
C
C-    SPICELIB Version 2.0.3, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 08-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.  Added
C        Copyright section to header.
C
C-    SPICELIB Version 1.1.0, 21-FEB-1996 (KRG)
C
C        This subroutine was turned into a machine dependent master
C        file due to problems with its use with arrays containing
C        INTEGERs that had been EQUIVALENCEd to DOUBLE PRECISION
C        numbers on the NeXT with the Absoft Fortran compiler, V3.2.
C
C        The DO loop has been replaced with a call to the C memmove
C        function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&
 
C$ Index_Entries
C
C     move a d.p. array to another d.p. array
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 09-NOV-2001 (NJB)
C
C        Bug fix:  code for PC-LINUX and PC-DIGITAL environments
C        was changed to prevent corruption of bit patterns inserted
C        into double precision numbers via EQUIVALENCE statements.
C        Now the routine memmove is called to effect the double
C        precision assignments performed by this routine.
C
C-    SPICELIB Version 2.0.0, 08-APR-1998 (NJB)
C
C        Module was updated for the PC-LINUX platform.  Added
C        Copyright section to header.
C
C-    Beta Version 1.0.1, 4-FEB-1989 (WLT)
C
C     Header fully filled out.
C
C-&
 
 
      CALL MOVEI ( ARRFRM, 2*NDIM, ARRTO )
 
 
      RETURN
      END
 
