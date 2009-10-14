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
C
C      VERNUM = 8.0     !  Release Dec. 2008
C      version 8.1 is in an svn branch.  Taken from this on Feb 18, 2009
      VERNUM = 9.0      !  Beta version of Oct 14, 2009
      VERSION = 'Beta version released Oct. 14, 2009'
C     VERNUM  = 9.1
C     VERSION = 'Development version after Oct. 14, 2009'
C
      RETURN
      END
