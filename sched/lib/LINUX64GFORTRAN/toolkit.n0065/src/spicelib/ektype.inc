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
C     Include Section:  EK Data Types
C
C        ektype.inc Version 1  27-DEC-1994 (NJB)
C
C
C     Within the EK system, data types of EK column contents are
C     represented by integer codes.  The codes and their meanings
C     are listed below.
C
C     Integer codes are also used within the DAS system to indicate
C     data types; the EK system makes no assumptions about compatibility
C     between the codes used here and those used in the DAS system.
C
C
C     Character type:
C
      INTEGER               CHR
      PARAMETER           ( CHR  = 1 )
 
C
C     Double precision type:
C
      INTEGER               DP
      PARAMETER           ( DP   = 2 )
 
C
C     Integer type:
C
      INTEGER               INT
      PARAMETER           ( INT  = 3 )
 
C
C     `Time' type:
C
C     Within the EK system, time values are represented as ephemeris
C     seconds past J2000 (TDB), and double precision numbers are used
C     to store these values.  However, since time values require special
C     treatment both on input and output, and since the `TIME' column
C     has a special role in the EK specification and code, time values
C     are identified as a type distinct from double precision numbers.
C
      INTEGER               TIME
      PARAMETER           ( TIME = 4 )
 
C
C     End Include Section:  EK Data Types
C
