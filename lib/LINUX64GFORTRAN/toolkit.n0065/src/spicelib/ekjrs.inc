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
C     Include Section:  EK Join Row Set Parameters
C
C        ekjrs.inc  Version 1    07-FEB-1995 (NJB)
C
C
C     Maximum number of join row sets in a join row set union:
C
      INTEGER               MXJRS
      PARAMETER           ( MXJRS  = 200 )
 
 
C
C     The layout of a join row set in the EK scratch area is shown
C     below:
C
C        +--------------------------------------------+
C        |              join row set size             |  1 element
C        +--------------------------------------------+
C        |    number of row vectors in join row set   |  1 element
C        +--------------------------------------------+
C        |               table count (TC)             |  1 element
C        +--------------------------------------------+
C        |          segment vector count (SVC)        |  1 element
C        +--------------------------------------------+
C        |               segment vector 1             |  TC elements
C        +--------------------------------------------+
C                              .
C                              .
C                              .
C        +--------------------------------------------+
C        |               segment vector SVC           |  TC elements
C        +--------------------------------------------+
C        |   segment vector 1 row set base address    |  1 element
C        +--------------------------------------------+
C        |      segment vector 1 row count (RC_1)     |  1 element
C        +--------------------------------------------+
C                              .
C                              .
C                              .
C        +--------------------------------------------+
C        |  segment vector SVC row set base address   |  1 element
C        +--------------------------------------------+
C        |   segment vector SVC row count (RC_SVC)    |  1 element
C        +--------------------------------------------+
C        | Augmented row vectors for segment vector 1 |  (TC+1)*RC_1
C        +--------------------------------------------+  elements
C                              .
C                              .
C                              .
C        +--------------------------------------------+
C        |Augmented row vectors for segment vector SVC|  (TC+1)*RC_SVC1
C        +--------------------------------------------+  elements
C
C
C     The following parameters indicate positions of elements in the
C     join row set structure:
C
C
C     Base-relative index of join row set size
C
      INTEGER               JSZIDX
      PARAMETER           ( JSZIDX = 1 )
 
C
C     Index of row vector count
C
      INTEGER               JRCIDX
      PARAMETER           ( JRCIDX = 2 )
 
C
C     Index of table count
C
      INTEGER               JTCIDX
      PARAMETER           ( JTCIDX = 3 )
 
C
C     Index of segment vector count
C
      INTEGER               JSCIDX
      PARAMETER           ( JSCIDX = 4 )
 
C
C     Base address of first segment vector
C
      INTEGER               JSVBAS
      PARAMETER           ( JSVBAS = 4 )
C
C
C     End Include Section:  EK Join Row Set Parameters
C
