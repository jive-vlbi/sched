import warnings
import logging
from contextlib import contextmanager

# using toolmanager prints warning messages since it's considered experimental
# since version 1.5 (at least up till version 3.3)
@contextmanager
def shut_up_mpl():
    warnings.filterwarnings("ignore")
    tool_manager_logger = logging.getLogger("matplotlib.backend_managers")
    filter_all = lambda record: False
    tool_manager_logger.addFilter(filter_all)
    yield
    tool_manager_logger.removeFilter(filter_all)
    warnings.resetwarnings()

