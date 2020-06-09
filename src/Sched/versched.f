      SUBROUTINE VERSCHED( VERNUM, VERSION )
C
C     Set a version number for the SCHED main code.  
C     Note that a coarser, date based version is set in sched.f
C
      REAL VERNUM
      CHARACTER  VERSION*(*)
C ----------------------------------------------------------------------
C      VERNUM = 5.18    !  Oct 2001 release
C      VERNUM = 5.19    !  Early 2002 development
C      VERNUM = 5.20    !  02 July 2002 release.
C      VERNUM = 5.21    !  Post 02 July 2002 release development
C      VERNUM = 5.22    !  15 Sept. 2003 release.
C      VERNUM = 5.23    !  Post 15 Sept. 2003 release.
C      VERNUM = 5.24    !  Trial release of 30 July 2004
C      VERNUM = 5.25    !  Internal NRAO release of 4 October 2004
C      VERNUM = 5.26    !  Development after 4 October 2004
C
C      Change version scheme to be mainly numerical in Feb 2005.
C
C      VERNUM = 6.0     !  Released March 8, 2005
C      VERNUM = 6.01    !  Released March 30, 2005.  Bug fixes.
C      VERNUM = 6.02    !  Released July 29, 2005.
C      VERNUM = 6.03    !  Mostly development versions
C      VERNUM = 6.04    !  Mar. 2006
C      VERNUM = 6.05    !  June 2006
C      VERNUM = 7.0     !  Development > Dec 2006
C
C      Now even versions are releases, odd versions are development.
C      Not really sticking to that.
C
C      VERNUM = 8.0     !  Release Dec. 2008
C      Version 8.1 is in an svn branch.  Taken from this on Feb 18, 2009
C      VERNUM = 9.0      !  Beta version of Oct 14, 2009
C      VERSION = 'Beta version released Oct. 14, 2009'
C      VERSION = 9.1    !  Development version before 9.2
C      VERNUM  = 9.2
C      VERSION = 'Release of March 8, 2010'
C      VERNUM  = 9.3
C      VERSION = 'Release March 23, 2010 with Apr. 8 bug fix.'
C      VERNUM  = 9.4
C      VERSION = 'Release Jan. 13, 2011.'
C       VERNUM  = 10.0
C       VERSION = 'Release Sept. 15, 2011'
C       VERNUM  = 10.1
C       VERSION = 'Development version after Sept. 21, 2011'
C       VERSION = 'Beta version of Dec. 7, 2011'
C       VERSION = 'Development version after Dec. 7, 2011'
C       VERSION = 'Beta version of Feb. 7, 2012'
C       VERSION = 'Development version after Feb. 7, 2012'
C       VERSION = 'Release of about Feb. 14, 2012'
C       VERNUM  = 10.2
C       VERSION = 'Development version after beta of May 4, 2012'
C       VERSION = 'Development version after beta of June 25, 2012'
C       VERSION = 'Release of about July 12, 2012'
C       VERNUM  = 10.3
C       VERSION = 'Beta version of Sept. 12, 2012'
C                 There was a beta of Sept. 20.
C       VERSION = 'Beta version of Nov. 5, 2012'
C       VERNUM  = 11.0
C       VERSION = 'sched_temp version of Jan. 24, 2013'
C       VERSION = 'Release of Feb. 15, 2013'
C       VERNUM  = 11.1
C       VERSION = 'Version 11.1 Beta of about April 26, 2013'
C       VERSION = 'Version 11.1 Beta of June 19, 2013'    
C       VERSION = 'Development version 11.1 after June 19, 2013'    
C       VERSION = 'Version 11.1 Beta  Jan. 2, 2014' 
C       VERNUM  = 11.2
C       VERSION = 'Release of Jan. 22, 2014 - update 1'
C       VERSION = 'Development version for 11.3'
C       VERNUM  = 11.3
C       VERSION = 'Release of April 4, 2014'
C       VERSION = 'Version 11.3 update 1.  April 14, 2014'
C       VERNUM  = 11.4
C       VERSION = 'Release 11.4.  March 14, 2015'
        VERNUM  = 11.6
        VERSION = 'Release 11.6; Feburary 2020'
C
      RETURN
      END
