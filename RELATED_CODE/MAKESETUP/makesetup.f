      PROGRAM MAKESETUP
C
C     This file reads a master file of setup information and writes
C     out setup files for use by SCHED.  The intent is to make 
C     maintenance of setup files easier.
C
C     Items in the master file:
C
C     Modes:  Specified between #mode <name> and #end.
C             Inserted as new lines wherever @mode <name> appears 
C             later in file. Modes are meant to be blocks of lines
C     Files:  Specified between #file <name> and #end
C             Causes an output file to be written.
C     Variables: Specified with #<name>.  Everything else on that
C             line up to a ";" or the end of the line is the value.  
C             Other things can be earlier on the line or after the
C             ";".  If the value ends with a comma, the whole next 
C             line will also be included (limit - one extra line).
C             The line or lines will be substituted wherever @<name> 
C             appears.  Anything else on the line with the 
C             $name will be preserved.
C             The variable definition must be specified in the #file 
C             or the modes included in the #file.  However its 
C             position relative to its use does not matter.  Functions
C             may be included in variables.  Of course, #mode and 
C             #file are reserved for invoking modes and files.
C     "if"    ?<name> to ?end is only included if the named variable   
C             is not blank.  These can be anywhere on their lines.
C             and many lines can be included.
C
C     Functions: name ends with (.  Argument list ends with ) and items
C     are separated with blanks.
C     There are a few built in:
C
C        bbc(    Requires a value for NCHAN have been set.
C                5 arguments:  a, bbc(i),i=1,4
C                gives:  bbc(i) = bbc(mod(i,4)+1) + int((i+1)/4)*a
C                Anything beyond the *bbc spec in the line will be lost.
C                Note that SCHED has good BBC defaulting so this is
C                probably no longer needed.
C        side(   Set the sidebands.  Takes 4 arguments such as 
C                'L L U U'.
C                Will repeat that pattern to the number of channels. 
C        foff(   Set the frequency offsets.  3 or 4 arguments.  First 
C                is number of times to repeat each one.  Second is 
C                multiplier of bandwidth for the increment.  Third is
C                the lowest offset.  The optional fourth is the offset
C                of the nchan/2+1 channel (for s/x).
C        freq(   Set the reference frequencies.  If there is one
C                argument, it is all that is given (used for all
C                channels.  If a second is given (eg s/x), it is
C                used for the second half of the channels.
C
C     Lines not in a mode or file will be ignored.
C     Substitution of modes happens whenever the mode request is
C     found.
C
C     The #mode and #file sections can be in any order.  Everything
C     will be read in before anything is written out so any #mode
C     sections will be available for expansion by the time it is
C     needed.  There can be only one instance of each mode or file.
C     Variables can change at any time and the most recently seen
C     one after expansion of the modes will be used.
C
C
      INCLUDE 'makeset.inc'
C
C ---------------------------------------------------------------------
C
C     Read all #modes and #files into memory.
C
      CALL GETDAT 
C
C     Expand out the modes.
C
      CALL EXPFIL
C
C     Fill in the variables and act on the ? lines.
C
      CALL DOVAR
C
C     Deal with functions.
C
      CALL FUNCT
C
C     Write out the files.
C
      CALL OUTPUT
C
      WRITE(*,*) 'Got to end of MAKESETUP'
C
      STOP
      END
