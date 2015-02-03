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
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C     The following parameters are the system parameters required
C     by PERCY.  Be sure to read any notes before adjusting these
C
C
C     The maximum number of commands that can be buffered is
C     determined by the value of MAXCOM.  This parameter is
C     used primarily by NXTCOM.
C
      INTEGER               MAXCOM
      PARAMETER           ( MAXCOM = 20 )
 
 
C
C     The parameter FILEN is the maximum length of a file name
C     on a particular system.
C
      INTEGER               FILEN
      PARAMETER           ( FILEN = 128 )
 
C
C     The parameter COMSIZ is the maximum length allowed for a
C     command.
C
      INTEGER               COMSIZ
      PARAMETER           ( COMSIZ = 1024 )
 
C
C     The parameter ERRSIZ is the maximum length allowed for
C     error messages.
C
      INTEGER               ERRSIZ
      PARAMETER           ( ERRSIZ = 1760 )
C
C     The parameter STYSIZ is the maximum length expected for
C     a NICEPR style string.
C
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )
C
