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
C$Procedure      HLPTXT ( Text for a help system )
 
      SUBROUTINE HLPTXT ( N, BUFFER )
 
C     This routine maintains a collection of text messages
C     that are intended for use in a program's help system.
C
C     The program was built with version 1.0 of the
C     SPICE program BuildHelp.
C
C     There are two entry points.
C
C     HLPTXT  returns buffers of text by numeric index
C     HLPSIZ  returns the total number of help messages
C             available.
C
C     The source files used to create this program were:
C
C      AboutHelp.hlp
C      Autoadjust.hlp
C      CollectingCommands.hlp
C      Column.hlp
C      ColumnTableAbbrev.hlp
C      Columns.hlp
C      CombiningTables.hlp
C      ConditionalOperators.hlp
C      CurrentSettings.hlp
C      CustomFormats.hlp
C      DefaultFloatingFormat.hlp
C      DefaultIntegerFormat.hlp
C      DefaultTimeFormat.hlp
C      DelimitedFormat.hlp
C      DelugeWarning.hlp
C      DisplayArea.hlp
C      EchoingTranslatedCommands.hlp
C      EditingCommands.hlp
C      EnvironmentVariables.hlp
C      Errors.hlp
C      ExampleTimeFormats.hlp
C      ExampleWhereClause.hlp
C      FlaggedFormat.hlp
C      FromClause.hlp
C      GettingTooMuchData.hlp
C      Glossary.hlp
C      Headers.hlp
C      Help.hlp
C      Kernels.hlp
C      Limits.hlp
C      LookingatData.hlp
C      MakingHelpWait.hlp
C      NumericFormats.hlp
C      OrderBy.hlp
C      OtherSettings.hlp
C      PatternMatching.hlp
C      Patterns.hlp
C      ProblemsSuggestions.hlp
C      Query.hlp
C      Reports.hlp
C      SamplingData.hlp
C      SavingWork.hlp
C      SelectClause.hlp
C      SetColumn.hlp
C      SetFormat.hlp
C      SetFormatMark.hlp
C      SetHeader.hlp
C      SetPage.hlp
C      SetTime.hlp
C      SetTitle.hlp
C      SettingTheEditor.hlp
C      SettingUpInspekt.hlp
C      ShortCuttoTopics.hlp
C      ShowColumn.hlp
C      ShowComments.hlp
C      ShowEnvironment.hlp
C      ShowFormat.hlp
C      ShowIndexed.hlp
C      ShowKernels.hlp
C      ShowPage.hlp
C      ShowSummary.hlp
C      SpecialSymbolsQueries.hlp
C      SpecifyingStrings.hlp
C      SpecifyingTimes.hlp
C      Symbol.hlp
C      SyntaxDescriptionLanguage.hlp
C      SyntaxSummaries.hlp
C      Table.hlp
C      TabularFormat.hlp
C      TabularFormatMark.hlp
C      TimeFormats.hlp
C      Titles.hlp
C      TypingCommands.hlp
C      UsingSymbols.hlp
C      VerbatimFormat.hlp
C      WhereClause.hlp
C
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               N
      CHARACTER*(*)         BUFFER ( LBCELL : * )
 
C     Spicelib Functions
 
      INTEGER               SIZEC
 
 
C     Local Variables
 
      INTEGER               NTXT
      PARAMETER           ( NTXT = 76 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 3056 )
 
      CHARACTER*(LNSIZE)    TEXT  ( TXTSIZ )
 
      INTEGER               I
      INTEGER               J
      INTEGER               SIZE
      INTEGER               BEGIN ( NTXT )
      INTEGER               FINISH( NTXT )
 
      LOGICAL               FIRST
      SAVE
 
      DATA                  FIRST / .TRUE. /
 
C
C     Reset the text buffer
C
      SIZE = SIZEC  (       BUFFER )
      CALL   SSIZEC ( SIZE, BUFFER )
 
      IF (        N .LT. 1
     .      .OR.  N .GT. NTXT )  THEN
 
C
C        There is no help for the value of N, simply
C        return with nothing in the buffer.
C
         RETURN
 
      END IF
 
 
      IF ( .NOT. FIRST ) THEN
 
C
C        Look up the help for this value of N
C
         J = 0
 
         DO I = BEGIN(N), FINISH(N)
            J         = J + 1
            BUFFER(J) = TEXT(I)
         END DO
         CALL SCARDC( J, BUFFER )
         RETURN
 
      END IF
 
      FIRST = .FALSE.
      CALL ZZHLP000 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP001 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP002 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP003 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP004 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP005 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP006 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP007 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP008 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP009 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP010 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP011 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP012 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP013 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP014 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP015 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP016 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP017 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP018 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP019 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP020 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP021 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP022 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP023 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP024 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP025 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP026 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP027 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP028 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP029 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP030 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP031 ( BEGIN, FINISH, TEXT )
      CALL ZZHLP032 ( BEGIN, FINISH, TEXT )
C
C     Look up the help for this value of N
C
 
      J = 0
 
      DO I = BEGIN(N), FINISH(N)
         J         = J + 1
         BUFFER(J) = TEXT(I)
      END DO
 
 
      CALL SCARDC ( J, BUFFER )
      RETURN
 
C     Return the total number of text messages.
 
      ENTRY HLPSIZ ( N )
 
      N = NTXT
      RETURN
      END
