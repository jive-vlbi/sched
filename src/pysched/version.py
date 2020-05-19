import requests
from distutils.version import LooseVersion

import pkg_resources

try:
    pysched_version = pkg_resources.get_distribution("pythonSCHED").version
except:
    pysched_version = "unknown"

def get_latest_version():
    response = requests.get("https://pypi.org/pypi/pythonSCHED/json")
    response.raise_for_status()
    return response.json()["info"]["version"]

def check_version():
    try:
        latest_version = get_latest_version()
        if LooseVersion(pysched_version) < LooseVersion(latest_version):
            print(f"Version {latest_version} of pySCHED is available through "
                  f"pip,\n you are running version {pysched_version}.")
    except Exception:
        pass
