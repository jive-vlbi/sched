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
C     Include Section:  EK File Metadata Parameters
C
C        ekfilpar.inc  Version 1  28-MAR-1995 (NJB)
C
C     These parameters apply to EK files using architecture 4.
C     These files use a paged DAS file as their underlying file
C     structure.
C
C     The metadata for an architecture 4 EK file is very simple:  it
C     consists of a single integer, which is a pointer to a tree
C     that in turn points to the segments in the EK.  However, in the
C     interest of upward compatibility, one integer page is reserved
C     for the file's metadata.
C
C
C     Size of file parameter block:
C
      INTEGER               FPARSZ
      PARAMETER           ( FPARSZ = 1 )
 
C
C     All offsets shown below are relative to the beginning of the
C     first integer page in the EK.
C
C
C     Index of the segment pointer tree---this location contains the
C     root page number of the tree:
C
      INTEGER               SGTIDX
      PARAMETER           ( SGTIDX  =  1 )
 
C
C     End Include Section:  EK File Metadata Parameters
C
