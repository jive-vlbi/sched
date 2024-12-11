      SUBROUTINE PLHELP
C
C     Write a description of sliders usage in the SCHED startup
C     terminal
C
      WRITE( *, '( 1X, /, A, 1X, /, 3( /, A ) )' )
     1   'HELP on setting scales in the AXIS Panel:', 
     2   '  Plot scales may be adjusted either with the mouse or by ' //
     3     'typing in new values.',
     4   '  The scale may be reset to the default value by clicking ' //
     5     'the ''x'' button to the',
     6   '  right of the field.'
C
      WRITE( *, '( 1X, 6( /, A ) )' )
     1   '  To change a scale using the mouse, place the cursor in ' //
     2     'the field and LEFT', 
     3   '  click to increase the value or RIGHT click to decrease ' //
     4     'it.  The amount of',
     5   '  change per click is equal to 10**N, where N is the ' //
     6     'number in the upper button',
     7   '  to the right of the field.  That number may be increased '//
     8     'by LEFT clicking on',
     9   '  it or typing ''+'' and may be decreased by RIGHT '//
     a     'clicking on it or by hitting',
     b   '  any other key while pointing to it.'
C
      WRITE( *, '( 1X, 4( /, A ) )' )
     1   '  To edit a field from the keyboard, place the cursor in ' //
     2     'the box and type.',
     3   '  While editing, use ESC to restore the old value and ' //
     4     'quit, ENTER to finish',
     5   '  setting a new value, and either BACKSPACE or DELETE to ' //
     6     'delete the last',
     7   '  character entered.'
C
      WRITE( *, '( 1X, 2( /, A ) )' )
     1   '  Some time fields include a day field.  This is the day ' //
     2     'relative to the first',
     3   '  day of the observation, which is day 0.'
C
      WRITE( *, '( 1X, 5( /, A ) )' )
     1    '  In the FILES Panel, fields may be edited from the ' //
     2      'keyboard.  Click or',
     3    '  type a character while the cursor is in the field to ' //
     4      'begin editing.  Use',
     5    '  ENTER to finish editing and ESC to restore the original' //
     6      ' value and quit.',
     7    '  DEL and a few basic emacs editing commands, such as ' //
     8      '<CTL>F, <CTL>B, <CTL>A,',
     9    '  <CTL>E, and <CTL>D, can be used.'
C
      RETURN
      END
