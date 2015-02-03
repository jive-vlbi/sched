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
C     Include Section:  Private FTP Validation String Parameters
C
C        zzftprms.inc Version 1    01-MAR-1999 (FST)
C
C     This include file centralizes the definition of string sizes
C     and other parameters that are necessary to properly implement
C     the FTP error detection scheme for binary kernels.
C
C     Before making any alterations to the contents of this file,
C     refer to the header of ZZFTPSTR for a detailed discussion of
C     the FTP validation string.
C
C     Size of FTP Test String Component:
C
      INTEGER               SIZSTR
      PARAMETER           ( SIZSTR = 16 )
 
C
C     Size of Maximum Expanded FTP Validation String:
C
C      (This indicates the size of a buffer to hold the test
C       string sequence from a possibly corrupt file. Empirical
C       evidence strongly indicates that expansion due to FTP
C       corruption at worst doubles the number of characters.
C       So take 3*SIZSTR to be on the safe side.)
C
      INTEGER               SIZEXP
      PARAMETER           ( SIZEXP = (3 * SIZSTR) )
 
C
C     Size of FTP Validation String Brackets:
C
      INTEGER               SIZEND
      PARAMETER           ( SIZEND = 6 )
 
C
C     Size of FTP Validation String:
C
      INTEGER               SIZFTP
      PARAMETER           ( SIZFTP = ( SIZSTR + 2*SIZEND ) )
 
C
C     Size of DELIM.
C
      INTEGER               SIZDLM
      PARAMETER           ( SIZDLM = 1 )
 
C
C     Number of character clusters present in the validation string.
C
      INTEGER               NUMTST
      PARAMETER           ( NUMTST = 6 )
 
C
C     End Include Section:  Private FTP Validation String Parameters
C
