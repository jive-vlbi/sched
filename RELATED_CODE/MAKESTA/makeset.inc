C     Include file for makesetup
C
C     Array sizes etc.
C
      INTEGER    MF, MM, MLF, MLM, MW
      PARAMETER  (MF=1)       ! Maximum number of output files.
      PARAMETER  (MM=1000)    ! Maximum number of "modes".
      PARAMETER  (MLM=20)     ! Maximum number of lines in a mode.
      PARAMETER  (MLF=5000)   ! Maximum number of lines in a file.
      PARAMETER  (MW=30)      ! Maximum number of words in a line.
C
C     The file names and data.
C
      CHARACTER  INFILE*80
      CHARACTER  FILE(MF)*40, FILETEXT(MLF,MF)*120
      CHARACTER  MODE(MM)*40, MODETEXT(MLM,MM)*120
C
      INTEGER    NF, NM, NUMLINE
      INTEGER    FILEL(MF), MODEL(MM)
C
C     The COMMONS.
C
      COMMON / MSN / NF, NM, NUMLINE, FILEL, MODEL
      COMMON / MSC / INFILE, FILE, FILETEXT, MODE, MODETEXT
C ---------------------------------------------------------------------
