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
C     Include Section:  EK Data Page Parameters
C
C        ekfilpar.inc  Version 1  03-APR-1995 (NJB)
C
C     These parameters apply to EK files using architecture 4.
C     These files use a paged DAS file as their underlying file
C     structure.
C
C     In paged DAS EK files, data pages are structured:  they contain
C     metadata as well as data.  The metadata is located in the last
C     few addresses of each page, so as to interfere as little as
C     possible with calculation of data addresses.
C
C     Each data page belongs to exactly one segment.  Some bookkeeping
C     information, such as record pointers, is also stored in data
C     pages.
C
C     Each page contains a forward pointer that allows rapid lookup
C     of data items that span multiple pages.  Each page also keeps
C     track of the current number of links from its parent segment
C     to the page.  Link counts enable pages to `know' when they
C     are no longer in use by a segment; unused pages are deallocated
C     and returned to the free list.
C
C     The parameters in this include file depend on the parameters
C     declared in the include file ekpage.inc.  If those parameters
C     change, this file must be updated.  The specified parameter
C     declarations we need from that file are:
C
C        INTEGER               PGSIZC
C        PARAMETER           ( PGSIZC = 1024 )
C
C        INTEGER               PGSIZD
C        PARAMETER           ( PGSIZD = 128 )
C
C        INTEGER               PGSIZI
C        PARAMETER           ( PGSIZI = 256 )
C
C
C
C     Character pages use an encoding mechanism to represent integer
C     metadata.  Each integer is encoded in five consecutive
C     characters.
C
C
C     Character data page parameters:
C
C
C     Size of encoded integer:
C
      INTEGER               ENCSIZ
      PARAMETER           ( ENCSIZ = 5 )
 
C
C     Usable page size:
C
      INTEGER               CPSIZE
      PARAMETER           ( CPSIZE = 1014 )
 
C
C     Location of character forward pointer:
C
      INTEGER               CFPIDX
      PARAMETER           ( CFPIDX = CPSIZE + 1)
 
 
C
C     Location of character link count:
C
      INTEGER               CLCIDX
      PARAMETER           ( CLCIDX = CFPIDX + ENCSIZ )
 
 
C
C     Double precision data page parameters:
C
C     Usable page size:
C
      INTEGER               DPSIZE
      PARAMETER           ( DPSIZE = 126 )
 
C
C     Location of d.p. forward pointer:
C
      INTEGER               DFPIDX
      PARAMETER           ( DFPIDX = DPSIZE + 1)
 
 
C
C     Location of d.p. link count:
C
      INTEGER               DLCIDX
      PARAMETER           ( DLCIDX = DFPIDX + 1 )
 
 
C
C     Integer data page parameters:
C
C     Usable page size:
C
      INTEGER               IPSIZE
      PARAMETER           ( IPSIZE = 254 )
 
C
C     Location of integer forward pointer:
C
      INTEGER               IFPIDX
      PARAMETER           ( IFPIDX = IPSIZE + 1)
 
 
C
C     Location of integer link count:
C
      INTEGER               ILCIDX
      PARAMETER           ( ILCIDX = IFPIDX + 1 )
 
 
C
C     End Include Section:  EK Data Page Parameters
C
