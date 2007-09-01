      PROGRAM INSERTTEX
C
C     Insert text into a tex (or latex) file.  This is meant for
C     inserting the examples into the SCHED manual after testing.
C
C     In the latex file, start a line with "% RCWINCLUDE" followed
C     by the name of the file to be inserted.  End that section with
C     "% RCWEND"
C
C     This is crude, but effective.
C
      INTEGER    LEN1, IER, VLBOPE
      LOGICAL    ININC
      CHARACTER  LINE*200, INTEX*80, OUTTEX*80, INCFILE*80
      CHARACTER  CBUFF*256
C ------------------------------------------------------------------
C
C     Open input data file.
C
  10  WRITE(*,'(A)') ' Input tex file.'
      READ(*,'(A)') INTEX
      IER = VLBOPE( 8, INTEX, 'TEXT', 'OLD', CBUFF )
      IF( IER .NE. 1 ) GO TO 10
C
C     Open AIPS output file.
C
  20  WRITE(*,'(A)') ' Output tex file.'
      READ(*,'(A)') OUTTEX
      IER = VLBOPE( 9, OUTTEX, 'TEXT', 'NEW', CBUFF )
      IF( IER .NE. 1 ) GO TO 20
C
C     Begin reading.
C
      ININC = .FALSE.
  100 CONTINUE
         READ(8,'(A)',END=300) LINE
C
C        Want to write both kinds of flag lines.
C
         IF( LINE(1:8) .EQ. '% RCWEND' ) THEN
            ININC = .FALSE.
         END IF
         IF( .NOT. ININC ) THEN
            WRITE(9,'(A)') LINE(1:LEN1(LINE))
         END IF
C
C        Now see if in include.
C
         IF( LINE(1:12) .EQ. '% RCWINCLUDE' ) THEN
            ININC = .TRUE.
C
C           Open the include file.
C
            INCFILE = LINE(14:94)
            IER = VLBOPE( 10, INCFILE, 'TEXT', 'OLD', CBUFF )
            IF( IER .NE. 1 ) THEN
               WRITE(*,*) ' Cannot open include file: ',
     1              INCFILE(1:LEN1(INCFILE))
               STOP
            END IF
            WRITE(*,*) 'Opened ', INCFILE(1:LEN1(INCFILE))
C
C           Transfer it.
C
            WRITE(9,'(A)') '\begin{verbatim}'
  200       CONTINUE
               READ(10,'(A)',END=220) LINE
               WRITE(9,'(A)') LINE(1:LEN1(LINE))
               GO TO 200
  220       CONTINUE
            WRITE(9,'(A)') '\end{verbatim}'
         END IF
C
C        Get next line
C
         GO TO 100
C
  300 CONTINUE
C
      STOP
      END
