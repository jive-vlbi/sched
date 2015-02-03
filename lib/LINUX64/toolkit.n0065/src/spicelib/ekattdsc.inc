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
C     Include Section:  EK Column Attribute Descriptor Parameters
C
C        ekattdsc.inc Version 1    23-AUG-1995 (NJB)
C
C
C     This include file declares parameters used in EK column
C     attribute descriptors.  Column attribute descriptors are
C     a simplified version of column descriptors:  attribute
C     descriptors describe attributes of a column but do not contain
C     addresses or pointers.
C
C
C     Size of column attribute descriptor
C
      INTEGER               ADSCSZ
      PARAMETER           ( ADSCSZ = 6 )
 
 
C
C     Indices of various pieces of attribute descriptors:
C
C
C     ATTSIZ is the index of the column's class code.  (We use the
C     word `class' to distinguish this item from the column's data
C     type.)
C
      INTEGER               ATTCLS
      PARAMETER           ( ATTCLS = 1 )
 
C
C     ATTTYP is the index of the column's data type code (CHR, INT, DP,
C     or TIME).  The type is actually implied by the class, but it
C     will frequently be convenient to look up the type directly.
C
C
      INTEGER               ATTTYP
      PARAMETER           ( ATTTYP = ATTCLS + 1 )
 
C
C     ATTLEN is the index of the column's string length value, if the
C     column has character type.  A value of IFALSE in this element of
C     the descriptor indicates that the strings have variable length.
C
      INTEGER               ATTLEN
      PARAMETER           ( ATTLEN = ATTTYP + 1 )
 
C
C     ATTSIZ is the index of the column's element size value.  This
C     descriptor element is meaningful for columns with fixed-size
C     entries.  For variable-sized columns, this value is IFALSE.
C
      INTEGER               ATTSIZ
      PARAMETER           ( ATTSIZ = ATTLEN + 1 )
 
C
C     ATTIDX is the location of a flag that indicates whether the column
C     is indexed.  The flag takes the value ITRUE if the column is
C     indexed and otherwise takes the value IFALSE.
C
      INTEGER               ATTIDX
      PARAMETER           ( ATTIDX = ATTSIZ + 1 )
 
C
C     ATTNFL is the index of a flag indicating whether nulls are
C     permitted in the column.  The value at location NFLIDX is
C     ITRUE if nulls are permitted and IFALSE otherwise.
C
      INTEGER               ATTNFL
      PARAMETER           ( ATTNFL = ATTIDX + 1 )
 
C
C     End Include Section:  EK Column Attribute Descriptor Parameters
C
