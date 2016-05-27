# test_ftable.py
#
# David Lampert (djlampert@gmail.com)
#
# shows how to create an ftable for a dammed reach

from pyhspf.core.ftable import lake_ftable
from pyhspf.core        import Dam

# data for Lake Belva on German Creek in Keokuk County, IA, NID IA003399

q = 3.4     # avg flow (ft3/s)
v = 0.854   # avg velocity (ft/s)
L = 7.7     # length (km)
s = 0.00504 # channel slope

dam = Dam('IA003399',
          'Lake Belva',
          -92,
          41,
          'German Creek',
          'Keokuk',
          'RE',
          'RO',
          2001,
          55,
          8100,
          3666,
          247,
          )

ftable = lake_ftable(q, v, L, s, dam)

for row in ftable:
    print('{:6.2f}, {:6.2f}, {:6.2f}, {:6.2f}'.format(*row))
