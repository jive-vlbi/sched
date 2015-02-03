C$Procedure ZZDDHGSD ( Private --- DDH Get String Definitions )
 
      SUBROUTINE ZZDDHGSD ( CLASS, ID, LABEL )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Retrieve string definitions for various CLASS and ID pairs.
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
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      CHARACTER*(*)         CLASS
      INTEGER               ID
      CHARACTER*(*)         LABEL
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CLASS      I   String indicating the class of label requested.
C     ID         I   Integer ID desired for the decoding.
C     LABEL      O   Corresponding label for the CLASS/ID pair.
C
C$ Detailed_Input
C
C     CLASS      is the class of the input ID code.  Acceptable
C                values are:
C
C                   'METHOD' - File access method
C                   'ARCH'   - File architecture
C                   'BFF'    - Binary file format
C
C     ID         is a integer value that is defined in the include
C                file 'zzddhman.inc'.
C
C$ Detailed_Output
C
C     LABEL      is the 'decoded' ID value that describes a particular
C                CLASS/ID pair. In the event that CLASS and ID do not
C                correspond to parameters in the include file, the
C                routine simply returns a blank string.
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
C     1) If the CLASS/ID pair is not known, then this routine
C        simply returns a blank label.
C
C$ Particulars
C
C     This routine is designed to consolidate label definitions for
C     parameters in the include file 'zzddhman.inc'.  These labels
C     are to be used in error messages and user level subroutines.
C
C$ Examples
C
C     See ZZDDHMAN for sample usage.
C
C$ Restrictions
C
C     This routine must be kept compatible with the contents of
C     'zzddhman.inc' as its functionality is heavily dependent
C     on its contents.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 11-OCT-2005 (NJB)
C
C        Removed embedded blanks from reference to parameter ARCH.
C        Previous spelling was 'ARC  H'.  This was not a problem
C        for Fortran compilers but was irritating for human readers.
C
C-    SPICELIB Version 1.0.0, 07-JUN-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               ISRCHC
 
C
C     Local Parameters
C
      INTEGER               LOCSIZ
      PARAMETER           ( LOCSIZ = 8 )
 
C
C     Number of classes of label/ID pairs of which this routine is
C     aware.
C
      INTEGER               NUMCLS
      PARAMETER           ( NUMCLS = 3 )
 
C
C     Integer codes for the currently supported classes.
C
      INTEGER               METHOD
      PARAMETER           ( METHOD = 1 )
 
      INTEGER               ARCH
      PARAMETER           ( ARCH   = METHOD + 1 )
 
      INTEGER               BFF
      PARAMETER           ( BFF    = ARCH   + 1 )
 
C
C     Local Variables
C
      CHARACTER*(LOCSIZ)    CLSLST ( NUMCLS )
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(LOCSIZ)    TMPSTR
 
      INTEGER               CLSID
 
C
C     Saved Variables
C
      SAVE                  CLSLST
      SAVE                  STRAMH
      SAVE                  STRARC
      SAVE                  STRBFF
 
C
C     Data Statements
C
      DATA                  CLSLST / 'METHOD', 'ARCH', 'BFF' /
 
      DATA                  STRAMH / 'READ',     'WRITE',
     .                               'SCRATCH',  'NEW'       /
 
      DATA                  STRARC / 'DAF',      'DAS'       /
 
      DATA                  STRBFF / 'BIG-IEEE', 'LTL-IEEE',
     .                               'VAX-GFLT', 'VAX-DFLT'  /
 
C
C     Left justify and convert the input to upper case.
C
      CALL LJUST ( CLASS,  TMPSTR )
      CALL UCASE ( TMPSTR, TMPSTR )
 
      CLSID = ISRCHC ( TMPSTR, NUMCLS, CLSLST )
 
C
C     Initialize LABEL to the default response.
C
      LABEL = ' '
 
C
C     Branch on CLSID and return the appropriate label as requested
C     by ID.
C
      IF      (       ( CLSID .EQ. METHOD )
     .          .AND. (    ID .GE. 1      )
     .          .AND. (    ID .LE. NUMAMH ) ) THEN
 
         LABEL = STRAMH ( ID )
 
      ELSE IF (       ( CLSID .EQ. ARCH   )
     .          .AND. (    ID .GE. 1      )
     .          .AND. (    ID .LE. NUMARC ) ) THEN
 
         LABEL = STRARC ( ID )
 
      ELSE IF (       ( CLSID .EQ. BFF    )
     .          .AND. (    ID .GE. 1      )
     .          .AND. (    ID .LE. NUMBFF ) ) THEN
 
         LABEL = STRBFF ( ID )
 
      END IF
 
      RETURN
      END
