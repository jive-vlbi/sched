import numpy as np
import util

class Catalog(object):
    """
    Base class to help read f2py common blocks in python objects
    and write back
    """
    class CatalogEntry(object):
        def __init__(self, **kwargs):
            self.attributes = list(kwargs.keys())
            for key, value in kwargs.items():
                setattr(self, key, value)

        def set_keyin_values(self, values, attribute_to_key):
            for attribute in self.attributes:
                if attribute_to_key.get(attribute) in values:
                    setattr(self, attribute, 
                            values[attribute_to_key[attribute]])
           

    def __init__(self, nr_elements, block_items):
        self.nr_elements = nr_elements
        self.block_items = block_items
        self.attributes = sum(block_items.values(), [])
    
    def prime(self):
        """
        Prepares the catalog with the values from common blocks to overwrite
        with keyin values before calling self.write().
        Where the common blocks contain fixed length arrays,
        set the length of the entries using the variable defining the length.
        """
        # gather a copy (because .T returns a view) of all arrays in C-order
        vector_func = np.vectorize(util.f2str, otypes=[object])
        arrays = {item: 
                  vector_func(getattr(block, item).T)
                  if getattr(block, item).dtype.kind == "S" else 
                  getattr(block, item).T.copy()
                  for block, items in self.block_items.items()
                  for item in items}
        # create an entry for each index of the arrays
        self.entries = [
            self.CatalogEntry(
                **{key: value[i] for key, value in arrays.items()})
            for i in range(self.nr_elements)]
        return self.entries

    def read(self):
        """
        Read the catalog entries from the common blocks.
        Only return entries that have defined values.
        """
        return self.prime()

    def write(self, indices=None):
        """
        Write the entries to common blocks.
        Where the common blocks contain fixed length arrays,
        set the variable defining the length from 
        the length of the entries (the inverse process of prime). 
        
        indices: write only these entries to the catalog.
        """
        if indices is None:
            indices = range(self.nr_elements)
        for block, items in self.block_items.items():
            for item in items:
                dtype=getattr(block, item).dtype
                vector_func = np.vectorize(
                    lambda x: x.ljust(dtype.itemsize),
                    otypes=[dtype])
                for i in indices:
                    new_value = np.array(
                        getattr(self.entries[i], item), dtype=dtype)
                    if dtype.kind == "S":
                        # fill string with spaces fortran/sched style
                        new_value = vector_func(new_value)
                    # if the new value is smaller, embed it
                    dest_slice = (slice(i, i+1), ) + \
                                 tuple(map(slice, new_value.shape))
                    getattr(block, item).T[dest_slice] = new_value
