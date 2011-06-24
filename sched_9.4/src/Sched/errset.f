      SUBROUTINE ERRSET( KS )
C
C     Subroutine for SCHED used by various setup file checking and
C     defaulting routines when there is an error.  It writes the
C     name of the offending setup file and the first station name
C     to the screen, and then aborts the program.
C
C     It also writes the setup file details as far as they are known
C     to this poine.
C
C     This used to be called SETERR, but there is a routine by that 
C     name in the NAIF software so I had to change it when spacecraft
C     tracking was added.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER        KS, LEN1
C ---------------------------------------------------------------------
      IF( SDEBUG ) CALL WLOG( 0, 'ERRSET starting.' )
      CALL WLOG( 0, ' ' )
      CALL WLOG( 0, ' ' )  
      CALL WLOG( 0, '======================== Setup Error' //
     1              ' ========================' )
C
      IF( KS .LE. 0 .OR. KS .GT. MSET ) THEN
         CALL WLOG( 0, 'The line(s) above and/or in the logfile ' //
     1         'describe a fatal setup error.' ) 
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I5, A )' ) 
     1         'The program thinks the error is in setup group ',
     1         KS, ' which is not a valid index.'
         CALL WLOG( 0, MSGTXT )
         CALL WLOG( 0, 'This could be either a user or programming ' //
     1                 'error.' )
         CALL WLOG( 0, 'Please report it to cwalker@nrao.edu' )
         CALL WLOG( 0, '====================================' //
     1              '=========================' )
         CALL ERRLOG( ' ' )
      ELSE
         CALL WLOG( 0, ' ' )
         CALL WLOG( 0, 'The preceeding lines here and/or lines in the'
     1         // ' terminal output describe a ' )
         CALL WLOG( 0, 'fatal setup error in file:'  )
         CALL WLOG( 0, ( SETNAME(KS)(1:LEN1(SETNAME(KS))) ) )
         CALL WLOG( 0, 'Station: ' // SETSTA(1,KS) )
         CALL WLOG( 0, '====================================' //
     1              '=========================' )
         CALL WLOG( 0, 'To help debug the problem, the setup ' //
     1       'information as determined so far is:' )
C        
C        Use the standard setup printing routine to dump the setup.
C        
         CALL PRTSET( KS, ILOG )
C        
         CALL WLOG( 1, ' ' )
         CALL WLOG( 0, ' ' )
         CALL WLOG( 0, 'End of setup as determined before the error '
     1        // 'was found.' )
         CALL WLOG( 0, '====================================' //
     1              '=========================' )
         CALL PUTOUT( '   There is more information about the bad ' //
     1          'setup in ' // LOGFILE(1:LEN1(LOGFILE)) )
         CALL ERRLOG( ' ' )
C
      END IF
C
      RETURN
      END
