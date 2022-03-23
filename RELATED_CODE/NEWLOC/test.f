      real              num4
      double precision  num8
      write(*,*) 'a many-digit number to 15 dig'
      read(*,'(F15.3)' ) num8
      write(*,'(F15.3)') num8
      write(*,*) 'a many-digit number to 15 dig'
      read(*,'(F15.3)' ) num4
      num8 = num4
      write(*,'(F15.3,F15.3)' ) num4, num8
      stop
      end
