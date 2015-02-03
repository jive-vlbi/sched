C
C$ Abstract
C
C     Parameter declarations for the DAF/DAS handle manager.
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
C     DAF, DAS
C
C$ Keywords
C
C     PRIVATE
C
C$ Particulars
C
C     This include file contains parameters defining limits and
C     integer codes that are utilized in the DAF/DAS handle manager
C     routines.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner       (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 2.5.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 2.4.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 2.3.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 2.2.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.1.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 2.0.0, 12-APR-2012 (BVS)
C
C        Increased FTSIZE (from 1000 to 5000).
C
C-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 1.0.1, 17-JUL-2002
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 1.0.0, 07-NOV-2001
C
C-&
C
C     Unit and file table size parameters.
C
C     FTSIZE     is the maximum number of files (DAS and DAF) that a
C                user may have open simultaneously.
C
      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 5000 )
 
C
C     RSVUNT     is the number of units protected from being locked
C                to a particular handle by ZZDDHHLU.
C
      INTEGER               RSVUNT
      PARAMETER           ( RSVUNT = 2 )
 
C
C     SCRUNT     is the number of units protected for use by scratch
C                files.
C
      INTEGER               SCRUNT
      PARAMETER           ( SCRUNT = 1 )
 
C
C     UTSIZE     is the maximum number of logical units this manager
C                will utilize at one time.
C
      INTEGER               UTSIZE
      PARAMETER           ( UTSIZE = 20 + SCRUNT + RSVUNT )
 
C
C     Access method enumeration.  These parameters are used to
C     identify which access method is associated with a particular
C     handle.  They need to be synchronized with the STRAMH array
C     defined in ZZDDHGSD in the following fashion:
C
C        STRAMH ( READ   ) = 'READ'
C        STRAMH ( WRITE  ) = 'WRITE'
C        STRAMH ( SCRTCH ) = 'SCRATCH'
C        STRAMH ( NEW    ) = 'NEW'
C
C     These values are used in the file table variable FTAMH.
C
      INTEGER               READ
      PARAMETER           ( READ   = 1 )
 
      INTEGER               WRITE
      PARAMETER           ( WRITE  = 2 )
 
      INTEGER               SCRTCH
      PARAMETER           ( SCRTCH = 3 )
 
      INTEGER               NEW
      PARAMETER           ( NEW    = 4 )
 
      INTEGER               NUMAMH
      PARAMETER           ( NUMAMH = 4 )
 
C
C     Binary file format enumeration.  These parameters are used to
C     identify which binary file format is associated with a
C     particular handle.  They need to be synchronized with the STRBFF
C     array defined in ZZDDHGSD in the following fashion:
C
C        STRBFF ( BIGI3E ) = 'BIG-IEEE'
C        STRBFF ( LTLI3E ) = 'LTL-IEEE'
C        STRBFF ( VAXGFL ) = 'VAX-GFLT'
C        STRBFF ( VAXDFL ) = 'VAX-DFLT'
C
C     These values are used in the file table variable FTBFF.
C
      INTEGER               BIGI3E
      PARAMETER           ( BIGI3E = 1 )
 
      INTEGER               LTLI3E
      PARAMETER           ( LTLI3E = 2 )
 
      INTEGER               VAXGFL
      PARAMETER           ( VAXGFL = 3 )
 
      INTEGER               VAXDFL
      PARAMETER           ( VAXDFL = 4 )
 
      INTEGER               NUMBFF
      PARAMETER           ( NUMBFF = 4 )
 
C
C     Some random string lengths... more documentation required.
C     For now this will have to suffice.
C
 
      INTEGER               STRSIZ
      PARAMETER           ( STRSIZ = 8 )
 
      INTEGER               STRLEN
      PARAMETER           ( STRLEN = (STRSIZ+1)*NUMBFF )
 
 
C
C     Architecture enumeration.  These parameters are used to identify
C     which file architecture is associated with a particular handle.
C     They need to be synchronized with the STRARC array defined in
C     ZZDDHGSD in the following fashion:
C
C        STRARC ( DAF ) = 'DAF'
C        STRARC ( DAS ) = 'DAS'
C
C     These values will be used in the file table variable FTARC.
C
      INTEGER               DAF
      PARAMETER           ( DAF = 1 )
 
      INTEGER               DAS
      PARAMETER           ( DAS = 2 )
 
      INTEGER               NUMARC
      PARAMETER           ( NUMARC = 2 )
 
C
C     For the following environments, record length is measured in
C     characters (bytes) with eight characters per double precision
C     number.
C
C     Environment: Sun, Sun FORTRAN
C     Source:      Sun Fortran Programmer's Guide
C
C     Environment: PC, MS FORTRAN
C     Source:      Microsoft Fortran Optimizing Compiler User's Guide
C
C     Environment: Macintosh, Language Systems FORTRAN
C     Source:      Language Systems FORTRAN Reference Manual,
C                  Version 1.2, page 12-7
C
C     Environment: PC/Linux, g77
C     Source:      Determined by experiment.
C
C     Environment: PC, Lahey F77 EM/32 Version 4.0
C     Source:      Lahey F77 EM/32 Language Reference Manual,
C                  page 144
C
C     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
C     Source:      FORTRAN/9000 Reference-Series 700 Computers,
C                  page 5-110
C
C     Environment: NeXT Mach OS (Black Hardware),
C                  Absoft Fortran Version 3.2
C     Source:      NAIF Program
C
      INTEGER               RECL
      PARAMETER           ( RECL  = 1024 )
 
C
C     The following parameter defines the size of a string used
C     to store a filenames on this target platform.
C
      INTEGER               FILEN
      PARAMETER           ( FILEN  =  255 )
 
C
C     The following parameter controls the size of the character record
C     buffer used to read data from non-native files.
C
      INTEGER               CBFSIZ
      PARAMETER           ( CBFSIZ = 1024 )
