
C$ Abstract
C
C     Include file sclk.inc
C
C     SPICE private file intended solely for the support of SPICE
C     routines.  Users should not include this file directly due
C     to the volatile nature of this file
C
C     The parameters below define sizes and limits used by 
C     the SCLK system.
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
C$ Parameters
C
C     See the declaration section below.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 24-MAY-2010 (NJB)
C
C        Increased value of maximum coefficient record count 
C        parameter MXCOEF from 10K to 50K.
C
C-    SPICELIB Version 1.0.0, 11-FEB-2008 (NJB)
C
C-&

 
C
C     Number of supported SCLK field delimiters:
C
      INTEGER               NDELIM
      PARAMETER           ( NDELIM = 5 )

C
C     Supported SCLK string field delimiters:
C  
      CHARACTER*(NDELIM)    DELIMS
      PARAMETER           ( DELIMS = '.:-, ' )

C
C     Maximum number of partitions:    
C     
      INTEGER               MXPART
      PARAMETER           ( MXPART = 9999 )

C
C     Partition string length.
C
C     Since the maximum number of partitions is given by MXPART is
C     9999, PRTSTR needs at most 4 characters for the partition number
C     and one character for the slash.
C
      INTEGER               PARTLN
      PARAMETER           ( PARTLN = 5 )

C
C     Maximum number of coefficient records:
C     
      INTEGER               MXCOEF
      PARAMETER           ( MXCOEF = 50000 )

C
C     Maximum number of fields in an SCLK string:
C
      INTEGER               MXNFLD
      PARAMETER           ( MXNFLD = 10 )

C
C     Length of strings used to represent D.P. 
C     numbers:
C
      INTEGER               DPLEN
      PARAMETER           ( DPLEN = 30 )

C
C     Maximum number of supported parallel time systems:
C
      INTEGER               MXTSYS
      PARAMETER           ( MXTSYS = 2 )

C
C     End of include file sclk.inc
C
