
C$ Abstract
C
C     Include file zzabcorr.inc
C
C     SPICE private file intended solely for the support of SPICE
C     routines.  Users should not include this file directly due
C     to the volatile nature of this file
C
C     The parameters below define the structure of an aberration
C     correction attribute block.
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
C     An aberration correction attribute block is an array of logical
C     flags indicating the attributes of the aberration correction
C     specified by an aberration correction string.  The attributes
C     are:
C
C        - Is the correction "geometric"?
C
C        - Is light time correction indicated?
C
C        - Is stellar aberration correction indicated?
C
C        - Is the light time correction of the "converged 
C          Newtonian" variety?
C
C        - Is the correction for the transmission case?
C
C        - Is the correction relativistic?
C
C    The parameters defining the structure of the block are as 
C    follows:
C
C       NABCOR    Number of aberration correction choices.
C
C       ABATSZ    Number of elements in the aberration correction
C                 block.   
C
C       GEOIDX    Index in block of geometric correction flag.
C
C       LTIDX     Index of light time flag.
C
C       STLIDX    Index of stellar aberration flag.
C
C       CNVIDX    Index of converged Newtonian flag.
C
C       XMTIDX    Index of transmission flag.
C
C       RELIDX    Index of relativistic flag.
C
C    The following parameter is not required to define the block
C    structure, but it is convenient to include it here:
C    
C       CORLEN    The maximum string length required by any aberration
C                 correction string
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
C-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
C
C-&
 
 
C     Number of aberration correction choices:
C     
      INTEGER               NABCOR
      PARAMETER           ( NABCOR = 15 )

C
C     Aberration correction attribute block size
C     (number of aberration correction attributes):
C
      INTEGER               ABATSZ
      PARAMETER           ( ABATSZ = 6 )

C
C     Indices of attributes within an aberration correction
C     attribute block:
C
      INTEGER               GEOIDX
      PARAMETER           ( GEOIDX = 1 )

      INTEGER               LTIDX
      PARAMETER           ( LTIDX  = GEOIDX + 1 )     

      INTEGER               STLIDX
      PARAMETER           ( STLIDX = LTIDX  + 1 ) 

      INTEGER               CNVIDX
      PARAMETER           ( CNVIDX = STLIDX + 1 )     

      INTEGER               XMTIDX
      PARAMETER           ( XMTIDX = CNVIDX + 1 ) 

      INTEGER               RELIDX
      PARAMETER           ( RELIDX = XMTIDX + 1 )

C
C     Maximum length of an aberration correction string:
C
      INTEGER               CORLEN
      PARAMETER           ( CORLEN = 5 )

C
C     End of include file zzabcorr.inc
C
