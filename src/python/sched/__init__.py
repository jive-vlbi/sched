# there is a potential problem with cyclic imports here 
# (eg. input calls schfiles), but so far I haven't found a fortran call cycle,
# so with the right order, exposing the module functions like this works fine
from .stread_module   import stread
from .srread_module   import srread, srread_open
from .getfreq_module  import getfreq
from .getsta_module   import getsta
from .rfreq_module    import rfreq
from .rdpeak_module   import rdpeak, rdpeak_open
from .satini_module   import satini
from .pcread_module   import pcread
from .rdset_module    import rdset
from .schfiles_module import schfiles
from .gettim_module   import gettim
from .gintent_module  import gintent
from .toggle_module   import toggle
from .infdb_module    import infdb
from .invla_module    import invla
from .scndup_module   import scndup
from .schrep_module   import schrep
from .getcov_module   import getcov
from .getcor_module   import getcor
from .times_module    import times
from .sttant_module   import sttant
from .schin_module    import schin
from .getset_module   import getset
from .input_module    import input_