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
C     Include Section:  EK Segment Descriptor Parameters
C
C        eksegdsc.inc  Version 8  06-NOV-1995 (NJB)
C
C
C     All `base addresses' referred to below are the addresses
C     *preceding* the item the base applies to.  This convention
C     enables simplied address calculations in many cases.
C
C     Size of segment descriptor.  Note:  the include file ekcoldsc.inc
C     must be updated if this parameter is changed.  The parameter
C     CDOFF in that file should be kept equal to SDSCSZ.
C
      INTEGER               SDSCSZ
      PARAMETER           ( SDSCSZ = 24  )
 
C
C     Index of the segment type code:
C
      INTEGER               EKTIDX
      PARAMETER           ( EKTIDX = 1 )
 
C
C     Index of the segment's number.  This number is the segment's
C     index in the list of segments contained in the EK to which
C     the segment belongs.
C
      INTEGER               SNOIDX
      PARAMETER           ( SNOIDX = EKTIDX  + 1 )
 
C
C     Index of the DAS integer base address of the segment's integer
C     meta-data:
C
      INTEGER               IMDIDX
      PARAMETER           ( IMDIDX = SNOIDX  + 1 )
 
C
C     Index of the DAS character base address of the table name:
C
      INTEGER               TNMIDX
      PARAMETER           ( TNMIDX = IMDIDX  + 1 )
 
C
C     Index of the segment's column count:
C
      INTEGER               NCIDX
      PARAMETER           ( NCIDX  = TNMIDX  + 1 )
 
C
C     Index of the segment's record count:
C
      INTEGER               NRIDX
      PARAMETER           ( NRIDX  = NCIDX + 1 )
 
C
C     Index of the root page number of the record tree:
C
      INTEGER               RTIDX
      PARAMETER           ( RTIDX  = NRIDX + 1 )
 
C
C     Index of the root page number of the character data page tree:
C
      INTEGER               CPTIDX
      PARAMETER           ( CPTIDX = RTIDX  + 1 )
 
C
C     Index of the root page number of the double precision data page
C     tree:
C
      INTEGER               DPTIDX
      PARAMETER           ( DPTIDX = CPTIDX + 1 )
 
C
C     Index of the root page number of the integer data page tree:
C
      INTEGER               IPTIDX
      PARAMETER           ( IPTIDX = DPTIDX + 1 )
 
C
C     Index of the `modified' flag:
C
      INTEGER               MFLIDX
      PARAMETER           ( MFLIDX = IPTIDX + 1 )
 
C
C     Index of the `initialized' flag:
C
      INTEGER               IFLIDX
      PARAMETER           ( IFLIDX = MFLIDX + 1 )
 
C
C     Index of the shadowing flag:
C
      INTEGER               SHDIDX
      PARAMETER           ( SHDIDX = IFLIDX + 1 )
 
C
C     Index of the companion file handle:
C
      INTEGER               CFHIDX
      PARAMETER           ( CFHIDX = SHDIDX + 1 )
 
C
C     Index of the companion segment number:
C
      INTEGER               CSNIDX
      PARAMETER           ( CSNIDX = CFHIDX + 1 )
 
C
C     The next three items are, respectively, the page numbers of the
C     last character, d.p., and integer data pages allocated by the
C     segment:
C
      INTEGER               LCPIDX
      PARAMETER           ( LCPIDX = CSNIDX + 1 )
 
      INTEGER               LDPIDX
      PARAMETER           ( LDPIDX = LCPIDX + 1 )
 
      INTEGER               LIPIDX
      PARAMETER           ( LIPIDX = LDPIDX + 1 )
 
C
C     The next three items are, respectively, the page-relative
C     indices of the last DAS word in use in the segment's
C     last character, d.p., and integer data pages:
C
      INTEGER               LCWIDX
      PARAMETER           ( LCWIDX = LIPIDX + 1 )
 
      INTEGER               LDWIDX
      PARAMETER           ( LDWIDX = LCWIDX + 1 )
 
      INTEGER               LIWIDX
      PARAMETER           ( LIWIDX = LDWIDX + 1 )
 
C
C     Index of the DAS character base address of the column name list:
C
      INTEGER               NMLIDX
      PARAMETER           ( NMLIDX = LIWIDX  + 1 )
 
C
C     The last descriptor element is reserved for future use.  No
C     parameter is defined to point to this location.
C
 
C
C     End Include Section:  EK Segment Descriptor Parameters
C
