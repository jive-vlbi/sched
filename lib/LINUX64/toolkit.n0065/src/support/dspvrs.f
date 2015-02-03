C$Procedure      DSPVRS ( Display Version )
 
      SUBROUTINE DSPVRS ( PNAME, VRSN )
 
C$ Abstract
C
C    This routine displays the name of a program as well as its
C    version and the version of SPICELIB that the calling
C    program has been linked with.
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
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     UTITITY
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         PNAME
      CHARACTER*(*)         VRSN
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PNAME      I   The name of the calling program
C     VRSN       I   The version number of the calling program
C
C$ Detailed_Input
C
C     PNAME      is the name of the calling program
C
C     VRSN       is the version number of the calling program
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This is a utility routine for printing the name and
C     version number of a program as well as the identifier
C     of the SPICELIB library that was used in linking
C     the program.
C
C     The following template is filled out and then displayed
C     at standard output.
C
C       <pname> --- Version <vrsn>,  SPICE Toolkit <tkvrsn>
C
C$ Examples
C
C     Suppose you are creating an program called "DoIt"
C     and you would like to have the name and current version
C     of the program displayed along with the linked version
C     of SPICELIB at some point in the execution of the program,
C     Here is how you can use this routine to perform the
C     version display function.
C
C        CALL DSPVRS ( 'DoIt', '1.0.0' )
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 26-SEP-1997 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Print a version line
C
C-&
 
 
      INTEGER               TKVSIZ
      PARAMETER           ( TKVSIZ = 8 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(TKVSIZ)    TKV
 
 
      CALL TKVRSN ( 'toolkit', TKV )
 
      LINE = PNAME
 
      CALL SUFFIX ( 'Version',         1, LINE )
      CALL SUFFIX (  VRSN,             1, LINE )
      CALL SUFFIX ( ', SPICE Toolkit', 0, LINE )
      CALL SUFFIX (  TKV,              1, LINE )
 
      CALL TOSTDO ( LINE )
 
      RETURN
      END
