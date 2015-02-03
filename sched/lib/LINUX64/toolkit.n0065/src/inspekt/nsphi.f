      SUBROUTINE NSPHI ( VERSN )
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
C$ Version
C
C-    Inspekt Version 2.1.0, 11-NOV-2005 (BVS)
C
C        Removed copyright statement from the banner.
C
C-    Inspekt Version 2.0.0, 21-JUL-1997 (WLT)
C
C        Added the line that gives the version of SPICELIB this
C        program was linked against.
C
C-    Inspekt Version 1.1.0, 15-JUN-1995 (WLT)
C
C        Added Copyright notice.
C
C-&
      INTEGER               RTRIM
      INTEGER               LTRIM
 
      CHARACTER*(*)         VERSN
 
      LOGICAL               STATUS ( 3 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    TKVER
      CHARACTER*(LNSIZE)    TKV
 
 
 
      INTEGER               LT
      INTEGER               LV
      INTEGER               MV
      INTEGER               RV
      INTEGER               SIZET
 
      LV = LTRIM ( VERSN )
      RV = RTRIM ( VERSN )
      MV = (RV + LV) / 2
 
 
      CALL TKVRSN ( 'TOOLKIT', TKV )
 
      CALL PREFIX ( '(SPICE Toolkit', 1, TKV )
      CALL SUFFIX ( ')',              0, TKV )
 
      SIZET      =  RTRIM(TKV)
      LT         =  MV - SIZET/2
      TKVER      = ' '
      TKVER(LT:) = TKV
 
 
 
      CALL NSPGST ( 'LOG', STATUS )
      CALL NSPIOH ( 'LOG'         )
 
      CALL NSPWLN ( ' ' )
      CALL NSPWLN ( VERSN )
      CALL NSPWLN ( TKVER )
      CALL NSPWLN ( ' ' )
      CALL NSPWLN ( '       A NAIF program for inspecting the '
     .//                   'contents of E-kernels.'                   )
      CALL NSPWLN ( ' ' )
      CALL NSPWLN ( '                         by Bill Taber '         )
      CALL NSPWLN ( '                    with assistance from'        )
      CALL NSPWLN ( '                  Hester Neilan and Nat Bachman' )
      CALL NSPWLN ( ' ' )
      CALL NSPWLN ( '   Please note: ' )
      CALL NSPWLN ( ' ' )
      CALL NSPWLN ( '   1) Commands may extend over multiple lines but')
      CALL NSPWLN ( '      ALL commands must end with a semi-colon.')
      CALL NSPWLN ( ' ' )
      CALL NSPWLN ( '   2) To leave the program type "EXIT;". ' )
      CALL NSPWLN ( ' ' )
      CALL NSPWLN ( '   3) To get a summary of commands type '
     .//                  '"HELP;" or "help;". ')
      CALL NSPWLN ( ' ' )
 
      CALL NSPPST ( 'LOG', STATUS )
 
      RETURN
      END
