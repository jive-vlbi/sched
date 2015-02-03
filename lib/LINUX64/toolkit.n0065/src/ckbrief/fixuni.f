C$Procedure      FIXUNI ( Temporary fix for UNITIM bug. )
 
      SUBROUTINE  FIXUNI

C$ Abstract
C
C     This routine sets POOL watch for LSK variables that is not 
C     set by UNITIM when it fails the first time it's called.
C     This (or equivalent) fix will be made in UNITIM for N0049
C     delivery.
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
C$ Author_and_Institution
C
C     B.V. Semenov     (NAIF)      
C
C$ Version
C
C-    CKBRIEF Beta Version 1.0.0, 11-AUG-1998 (BVS)
C
C-&

C
C     All variables, parameters and actual code in this routine 
C     were "borrowed" from SPICELIB's UNITIM.
C

C
C     NEEDED is the number of kernel pool variables needed by this
C     routine.
C
      INTEGER               NEEDED
      PARAMETER           ( NEEDED = 4 )
 
C
C     VARS holds names of these variables. VARS are to be saved.
C
      CHARACTER*(32)        VARS   ( NEEDED )
      
      SAVE                  VARS
      
C
C     Initial values of VARS correspond to those in UNITIM.
C
      DATA                  VARS     / 'DELTET/DELTA_T_A',
     .                                 'DELTET/K',
     .                                 'DELTET/EB',
     .                                 'DELTET/M'              /

C
C     The only thing this routine has to do is to set watchers 
C     to fix the problem in UNITIM.
C
      CALL SWPOOL ( 'UNITIM', NEEDED, VARS )
      
C
C     All done. Bye-bye.
C
      RETURN
      END
