C$ Procedure     CRCNST ( CHRONOS Parameter Arrays )

      SUBROUTINE CRCNST ( SYSTMS, TYPES,  DEFTYP, SYSTYP, 
     .                    FMTTED, FMTPIC, CLKEYS )

C$ Abstract
C
C     Returns arrays with parameterized time systems/types, output
C     system/type formats, and command line keys used by CRONOS routine
C     and CHRONOS executable.
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
C     None.
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'chronos.inc'

      CHARACTER*(WRDSIZ)    SYSTMS ( MAXSYS )
      CHARACTER*(WRDSIZ)    TYPES  ( MAXTYP )
      CHARACTER*(WRDSIZ)    DEFTYP ( MAXSYS )
      LOGICAL               SYSTYP ( MAXSYS, MAXTYP )
      LOGICAL               FMTTED ( MAXSYS, MAXTYP )
      CHARACTER*(WRDSIZ*2)  FMTPIC ( MAXSYS, MAXTYP )
      CHARACTER*(WRDSIZ)    CLKEYS ( MAXKEY )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SYSTMS     O   Array of supported time systems.
C     TYPES      O   Array of supported time types.
C     DEFTYP     O   Array of default time types.
C     SYSTYP     O   Time system-type matrix.
C     FMTTED     O   Output format applicability matrix.
C     FMTPIC     O   Array of default output formats.
C     CLKEYS     O   Array of recongnized command line keys.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     SYSTMS         is the array of supported time systems.
C
C     TYPES          is the array of supported time types.
C
C     DEFTYP         is the array of default time types.
C
C     SYSTYP         is the time system-type matrix.
C
C     FMTTED         is the output format applicability matrix.
C
C     FMTPIC         is the array of default output formats.
C
C     CLKEYS         is the array of recongnized command line keys.
C
C$ Parameters
C
C     See CHRONOS include file.
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
C     This routine provides consistent parameter arrays to CRONOS
C     routine and CHRONOS main module. In essence, it's just an
C     augmentation to the CHRONOS include file.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    CHRONOS Version 1.0.0, 17-DEC-2001 (BVS)
C
C-&

C
C     Local array declarations.
C
      CHARACTER*(WRDSIZ)    LSYSTM ( MAXSYS )
      CHARACTER*(WRDSIZ)    LTYPES ( MAXTYP )
      CHARACTER*(WRDSIZ)    LDEFTY ( MAXSYS )
      LOGICAL               LSYSTY ( MAXSYS, MAXTYP )
      LOGICAL               LFMTTE ( MAXSYS, MAXTYP )
      CHARACTER*(WRDSIZ*2)  LFMTPI ( MAXSYS, MAXTYP )
      CHARACTER*(WRDSIZ)    LCLKEY ( MAXKEY )

      INTEGER               I
      INTEGER               J

C
C     Save all local arrays.
C
      SAVE

C
C     The data.
C

C
C     Time systems
C
      DATA ( LSYSTM(I), I = 1, MAXSYS )
     .     /
     .      UTCSYS,
     .      ETSYS,
     .      SCLSYS,
     .      LSTSYS
     .     /

C
C     Types within systems.
C
      DATA ( LTYPES(I), I =  1, MAXTYP )
     .     /
     .      SCTTYP,
     .      ERTTYP,
     .      SCLTYP,
     .      HEXTYP,
     .      TIKTYP,
     .      LTTYP,
     .      SECTYP,
     .      LSTTYP,
     .      ETTTYP,
     .      LSNTYP
     .     /

C
C     Default types for the systems.
C
      DATA ( LDEFTY(I), I =  1, MAXSYS )
     .     /
     .      SCTTYP,
     .      SCTTYP,
     .      SCLTYP,
     .      LSTTYP
     .     /

C
C     This array defines whether TYPE is applicable for a system.
C
      DATA ( ( LSYSTY(I,J), I = 1, MAXSYS), J = 1, MAXTYP )
     .     /
     .      .TRUE.,  .TRUE.,  .FALSE., .FALSE.,
     .      .TRUE.,  .TRUE.,  .FALSE., .FALSE.,
     .      .FALSE., .FALSE., .TRUE.,  .FALSE.,
     .      .FALSE., .FALSE., .TRUE.,  .FALSE.,
     .      .FALSE., .FALSE., .TRUE.,  .FALSE.,
     .      .TRUE.,  .TRUE.,  .FALSE., .FALSE.,
     .      .FALSE., .TRUE.,  .FALSE., .FALSE.,
     .      .FALSE., .FALSE., .FALSE., .TRUE.,
     .      .TRUE.,  .TRUE.,  .FALSE., .FALSE.,
     .      .FALSE., .FALSE., .FALSE., .TRUE.
     .     /

C
C     This array defines whether an output of SYSTEM/TYPE can be
C     formatted.
C
      DATA ( ( LFMTTE(I,J), I =  1, MAXSYS), J = 1, MAXTYP )
     .     /
     .      .TRUE.,  .TRUE.,  .FALSE., .FALSE.,
     .      .TRUE.,  .TRUE.,  .FALSE., .FALSE.,
     .      .FALSE., .FALSE., .FALSE., .FALSE.,
     .      .FALSE., .FALSE., .FALSE., .FALSE.,
     .      .FALSE., .FALSE., .TRUE.,  .FALSE.,
     .      .TRUE.,  .TRUE.,  .FALSE., .FALSE.,
     .      .FALSE., .TRUE.,  .FALSE., .FALSE.,
     .      .FALSE., .FALSE., .FALSE., .FALSE.,
     .      .TRUE.,  .TRUE.,  .FALSE., .FALSE.,
     .      .FALSE., .FALSE., .FALSE., .TRUE.
     .     /

C
C     This array defines default picture for output format
C     for a combination of SYSTEM/TYPE.
C
      DATA ( ( LFMTPI(I,J), I =  1, MAXSYS), J = 1, MAXTYP )
     .     /
     .      'YYYY-MM-DD HR:MN:SC.### ::RND',
     .      'YYYY-MM-DD, HR:MN:SC.### ::TDB ::RND', ' ', ' ',
     .      'YYYY-MM-DD HR:MN:SC.### ::RND',
     .      'YYYY-MM-DD, HR:MN:SC.### ::TDB ::RND', ' ', ' ',
     .      ' ', ' ', ' ', ' ',
     .      ' ', ' ', ' ', ' ',
     .      ' ', ' ', 'xxxxxxxxxxxxxxxx', ' ',
     .      'xxxxxxxxxxxx.xxx', 'xxxxxxxxxxxx.xxx', ' ', ' ',
     .      ' ', 'xxxxxxxxxxxxxxx.xxx', ' ', ' ',
     .      ' ', ' ', ' ', ' ',
     .      'YYYY-MM-DD HR:MN:SC.### ::RND',
     .      'YYYY-MM-DD, HR:MN:SC.### ::TDB ::RND', ' ', ' ',
     .      ' ', ' ', ' ', 'xxxxxx.xxx'
     .     /

C
C     Command line keys.
C
      DATA ( LCLKEY(I), I =  1, MAXKEY )
     .     /
     .      STPKEY,
     .      FRMKEY,
     .      FRTKEY,
     .      TOKEY,
     .      TOTKEY,
     .      FMTKEY,
     .      TIMKEY,
     .      HLPKEY,
     .      HKEY,
     .      USGKEY,
     .      UKEY,
     .      TMLKEY,
     .      BATKEY,
     .      LBLKEY,
     .      TRCKEY,
     .      SIDKEY,
     .      BODKEY,
     .      LSTKEY,
     .      SOLKEY
     .     /

C
C     Copy local arrays to outputs and return.
C
      DO I = 1, MAXSYS 
         SYSTMS ( I ) = LSYSTM ( I )
         DEFTYP ( I ) = LDEFTY ( I )
         DO J = 1, MAXTYP
            SYSTYP ( I, J ) = LSYSTY ( I, J )
            FMTTED ( I, J ) = LFMTTE ( I, J )
            FMTPIC ( I, J ) = LFMTPI ( I, J )
         END DO
      END DO

      DO J = 1, MAXTYP
         TYPES  ( J ) = LTYPES ( J )
      END DO

      DO I = 1, MAXKEY
         CLKEYS ( I ) = LCLKEY ( I )
      END DO

      RETURN

      END
