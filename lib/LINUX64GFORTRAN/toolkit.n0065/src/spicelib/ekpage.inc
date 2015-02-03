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
C
C     Include Section:  EK Das Paging Parameters
C
C        ekpage.inc  Version 4    25-AUG-1995 (NJB)
C
C
C
C     The EK DAS paging system makes use of the integer portion
C     of an EK file's DAS address space to store the few numbers
C     required to describe the system's state.  The allocation
C     of DAS integer addresses is shown below.
C
C
C                       DAS integer array
C
C        +--------------------------------------------+
C        |            EK architecture code            |  Address = 1
C        +--------------------------------------------+
C        |      Character page size (in DAS words)    |
C        +--------------------------------------------+
C        |        Character page base address         |
C        +--------------------------------------------+
C        |      Number of character pages in file     |
C        +--------------------------------------------+
C        |   Number of character pages on free list   |
C        +--------------------------------------------+
C        |      Character free list head pointer      |  Address = 6
C        +--------------------------------------------+
C        |                                            |  Addresses =
C        |           Metadata for d.p. pages          |    7--11
C        |                                            |
C        +--------------------------------------------+
C        |                                            |  Addresses =
C        |         Metadata for integer pages         |    12--16
C        |                                            |
C        +--------------------------------------------+
C                              .
C                              .
C                              .
C        +--------------------------------------------+
C        |                                            |  End Address =
C        |                Unused space                |  integer page
C        |                                            |  end
C        +--------------------------------------------+
C        |                                            |  Start Address =
C        |             First integer page             |  integer page
C        |                                            |  base
C        +--------------------------------------------+
C                              .
C                              .
C                              .
C        +--------------------------------------------+
C        |                                            |
C        |              Last integer page             |
C        |                                            |
C        +--------------------------------------------+
C
C     The following parameters indicate positions of elements in the
C     paging system metadata array:
C
C
      INTEGER               EPARCH
      PARAMETER           ( EPARCH = 1 )
 
C
C     Number of metadata items per data type:
C
      INTEGER               EPNIPT
      PARAMETER           ( EPNIPT = 5 )
 
C
C     Character metadata indices:
C
      INTEGER               EPPSZC
      PARAMETER           ( EPPSZC = EPARCH + 1 )
 
      INTEGER               EPBASC
      PARAMETER           ( EPBASC = EPPSZC + 1 )
 
      INTEGER               EPNPC
      PARAMETER           ( EPNPC  = EPBASC + 1 )
 
      INTEGER               EPNFPC
      PARAMETER           ( EPNFPC = EPNPC  + 1 )
 
      INTEGER               EPFPC
      PARAMETER           ( EPFPC  = EPNFPC + 1 )
 
C
C     Double precision metadata indices:
C
      INTEGER               EPPSZD
      PARAMETER           ( EPPSZD = EPPSZC + EPNIPT )
 
      INTEGER               EPBASD
      PARAMETER           ( EPBASD = EPPSZD + 1 )
 
      INTEGER               EPNPD
      PARAMETER           ( EPNPD  = EPBASD + 1 )
 
      INTEGER               EPNFPD
      PARAMETER           ( EPNFPD = EPNPD  + 1 )
 
      INTEGER               EPFPD
      PARAMETER           ( EPFPD  = EPNFPD + 1 )
 
C
C     Integer metadata indices:
C
      INTEGER               EPPSZI
      PARAMETER           ( EPPSZI = EPPSZD + EPNIPT )
 
      INTEGER               EPBASI
      PARAMETER           ( EPBASI = EPPSZI + 1 )
 
      INTEGER               EPNPI
      PARAMETER           ( EPNPI  = EPBASI + 1 )
 
      INTEGER               EPNFPI
      PARAMETER           ( EPNFPI = EPNPI  + 1 )
 
      INTEGER               EPFPI
      PARAMETER           ( EPFPI  = EPNFPI + 1 )
 
C
C     Size of metadata area:
C
      INTEGER               EPMDSZ
      PARAMETER           ( EPMDSZ = 1 + 3*EPNIPT )
 
C
C     Page sizes, in units of DAS words of the appropriate type:
C
      INTEGER               PGSIZC
      PARAMETER           ( PGSIZC = 1024 )
 
      INTEGER               PGSIZD
      PARAMETER           ( PGSIZD = 128 )
 
      INTEGER               PGSIZI
      PARAMETER           ( PGSIZI = 256 )
 
C
C     Default page base addresses:
C
      INTEGER               PGBASC
      PARAMETER           ( PGBASC = 0 )
 
      INTEGER               PGBASD
      PARAMETER           ( PGBASD = 0 )
 
      INTEGER               PGBASI
      PARAMETER           ( PGBASI = 256 )
 
 
C
C     End Include Section:  EK Das Paging Parameters
C
