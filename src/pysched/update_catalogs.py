import os

# git import will raise an exception if it cannot find the executable,
# this can be suppressed with the environment variable GIT_PYTHON_REFRESH
_var = "GIT_PYTHON_REFRESH"
_old_value = os.environ.get(_var)
os.environ[_var] = "quiet"
import git
if _old_value is not None:
    os.environ[_var] = _old_value

import schedlib as s

import atexit
import sys

checkout_dir = os.path.join(os.path.expanduser("~"), ".pysched")
git_repository = "https://github.com/jive-vlbi/sched.git"

class Spinner(git.RemoteProgress):
    cursor_chars = ["|", "/", "-", "\\"]
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        sys.stdout.write(" ")
        sys.stdout.flush()
        self.index = 0
    
    def update(self, op_code, cur_count, max_count=None, message=""):
        sys.stdout.write("\r")
        sys.stdout.write(self.cursor_chars[self.index])
        sys.stdout.flush()
        self.index = (self.index + 1) % len(self.cursor_chars)

def update_catalogs():
    try:
        if not os.path.exists(checkout_dir):
            s.wlog(1, "Downloading catalogs to {}".format(checkout_dir))
            # clone a shallow, single branch version to reduce data usage
            repo = git.Repo.clone_from(git_repository, checkout_dir, 
                                       depth=1, branch="data_files", 
                                       progress=Spinner())
            print()
        else:
            repo = git.Repo(checkout_dir)

        repo.git.checkout("data_files")

        # point $SCHED to the checkout if not explicitly set, restore at exit
        if "SCHED" not in os.environ:
            os.environ["SCHED"] = checkout_dir
            def restore():
                del os.environ["SCHED"]
            atexit.register(restore)

        s.wlog(1, "Updating catalogs in {}".format(checkout_dir))
        repo.git.pull()
        s.wlog(1, "Catalogs in {} are up-to-date.".format(checkout_dir))

    except git.exc.GitError as e:
        s.wlog(1, "Warning, failed to update catalogs.")
        s.wlog(0, "Update failed with error: {}".format(e))
