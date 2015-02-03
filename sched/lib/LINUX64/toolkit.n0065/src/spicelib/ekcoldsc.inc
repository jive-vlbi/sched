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
C     Include Section:  EK Column Descriptor Parameters
C
C        ekcoldsc.inc Version 6    23-AUG-1995 (NJB)
C
C
C     Note:  The column descriptor size parameter CDSCSZ  is
C     declared separately in the include section CDSIZE$INC.FOR.
C
C     Offset of column descriptors, relative to start of segment
C     integer address range.  This number, when added to the last
C     integer address preceding the segment, yields the DAS integer
C     base address of the first column descriptor.  Currently, this
C     offset is exactly the size of a segment descriptor.  The
C     parameter SDSCSZ, which defines the size of a segment descriptor,
C     is declared in the include file eksegdsc.inc.
C
      INTEGER               CDOFF
      PARAMETER           ( CDOFF  = 24 )
 
 
C
C     Size of column descriptor
C
      INTEGER               CDSCSZ
      PARAMETER           ( CDSCSZ = 11 )
 
 
C
C     Indices of various pieces of column descriptors:
C
C
C     CLSIDX is the index of the column's class code.  (We use the
C     word `class' to distinguish this item from the column's data
C     type.)
C
      INTEGER               CLSIDX
      PARAMETER           ( CLSIDX = 1 )
 
C
C     TYPIDX is the index of the column's data type code (CHR, INT, DP,
C     or TIME).  The type is actually implied by the class, but it
C     will frequently be convenient to look up the type directly.
C
C
      INTEGER               TYPIDX
      PARAMETER           ( TYPIDX = CLSIDX + 1 )
 
C
C     LENIDX is the index of the column's string length value, if the
C     column has character type.  A value of IFALSE in this element of
C     the descriptor indicates that the strings have variable length.
C
      INTEGER               LENIDX
      PARAMETER           ( LENIDX = TYPIDX + 1 )
 
C
C     SIZIDX is the index of the column's element size value.  This
C     descriptor element is meaningful for columns with fixed-size
C     entries.  For variable-sized columns, this value is IFALSE.
C
      INTEGER               SIZIDX
      PARAMETER           ( SIZIDX = LENIDX + 1 )
 
C
C     NAMIDX is the index of the base address of the column's name.
C
      INTEGER               NAMIDX
      PARAMETER           ( NAMIDX = SIZIDX + 1 )
 
C
C     IXTIDX is the data type of the column's index.  IXTIDX
C     contains a type value only if the column is indexed. For columns
C     that are not indexed, the location IXTIDX contains the boolean
C     value IFALSE.
C
      INTEGER               IXTIDX
      PARAMETER           ( IXTIDX = NAMIDX + 1 )
 
C
C     IXPIDX is a pointer to the column's index.  IXTPDX contains a
C     meaningful value only if the column is indexed.  The
C     interpretation of the pointer depends on the data type of the
C     index.
C
      INTEGER               IXPIDX
      PARAMETER           ( IXPIDX = IXTIDX + 1 )
 
C
C     NFLIDX is the index of a flag indicating whether nulls are
C     permitted in the column.  The value at location NFLIDX is
C     ITRUE if nulls are permitted and IFALSE otherwise.
C
      INTEGER               NFLIDX
      PARAMETER           ( NFLIDX = IXPIDX + 1 )
 
C
C     ORDIDX is the index of the column's ordinal position in the
C     list of columns belonging to the column's parent segment.
C
      INTEGER               ORDIDX
      PARAMETER           ( ORDIDX = NFLIDX + 1 )
 
C
C     METIDX is the index of the column's integer metadata pointer.
C     This pointer is a DAS integer address.
C
      INTEGER               METIDX
      PARAMETER           ( METIDX = ORDIDX + 1 )
 
C
C     The last position in the column descriptor is reserved.  No
C     parameter is defined to point to this location.
C
 
C
C     End Include Section:  EK Column Descriptor Parameters
C
