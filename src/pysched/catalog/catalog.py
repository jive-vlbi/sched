import numpy as np
from .. import util

def get_arrays(block_items):
    # gather a copy (because .T returns a view) of all arrays in C-order
    vector_func = np.vectorize(util.f2str, otypes=[object])
    arrays = {item: 
              vector_func(getattr(block, item).T)
              if getattr(block, item).dtype.kind == "S" else 
              getattr(block, item).T.copy()
              for block, items in block_items.items()
              for item in items}
    return arrays

def write(block_items, entries, indices):
    for block, items in block_items.items():
        for item in items:
            dtype=getattr(block, item).dtype
            vector_func = np.vectorize(
                lambda x: x.ljust(dtype.itemsize),
                otypes=[dtype])
            for i in indices:
                new_value = np.array(
                    getattr(entries[i], item), dtype=dtype)
                if dtype.kind == "S":
                    # fill string with spaces fortran/sched style
                    new_value = vector_func(new_value)
                # if the new value is smaller, embed it
                dest_slice = (slice(i, i+1), ) + \
                             tuple(map(slice, new_value.shape))
                getattr(block, item).T[dest_slice] = new_value

class Catalog(object):
    """
    Base class to help read f2py common blocks in python objects
    and write back.
    Instances of subclasses share the state with each other.
    """

    # FIX: replace indexing member to other catalog entries 
    #      with actual catalog entry

    class CatalogEntry(object):
        def __init__(self, **kwargs):
            self.__dict__.update(kwargs)
        
        def set_keyin_values(self, values, attribute_to_key):
            for attribute, key in attribute_to_key.items():
                if key in values:
                    setattr(self, attribute, values[key])

        def __str__(self):
            return str(self.__dict__)
           

    def __init__(self, nr_elements, block_items, extended_attributes=set()):
        """
        nr_elements: the maximum number of catalog entries
        block_items: {schedlib block: [attribute of block]}
        extended_attributes: {name of attribute not represented in 
                              Fortran common blocks}
        """
        try:
            self.__dict__ = type(self)._shared_state
            assert(self.nr_elements == nr_elements)
            assert(self.block_items == block_items)
            assert(self.extended_attributes == extended_attributes)
        except AttributeError:
            # no shared state for this class yet, create and assign
            self.__dict__ = type(self)._shared_state = {}
            self.nr_elements = nr_elements
            self.block_items = block_items
            self.attributes = sum(block_items.values(), [])
            self.extended_attributes = extended_attributes
            self.prime()

    def adjust_lengths(self, entries):
        """
        Where the common blocks contain fixed length arrays,
        set the length of the entries using the variable defining the length.
        """
        pass
    
    def prime(self):
        """
        Prepares the catalog with the values from common blocks to overwrite
        with keyin values before calling self.write().
        """
        arrays = get_arrays(self.block_items)
        # create an entry for each index of the arrays
        self.entries = [
            self.CatalogEntry(
                **{key: value[i] for key, value in arrays.items()})
            for i in range(self.nr_elements)]
        for entry in self.entries:
            for attribute in self.extended_attributes:
                setattr(entry, attribute, None)
        self.adjust_lengths(self.entries)
        return self.entries

    def read(self, selection_slice=None):
        """
        Read the catalog entries from the common blocks.
        Only read and return entries that sliced by selection_slice, 
        applied to self.entries.
        If selection_slice is None, self.scheduled_slice() is used.
        """
        if selection_slice is None:
            selection_slice = self.scheduled_slice()
        arrays = get_arrays(self.block_items)
        entries = self.entries[selection_slice]
        for i, entry in zip(range(*selection_slice.indices(len(self.entries))), 
                            entries):
            entry.__dict__.update(
                {key: value[i] for key, value in arrays.items()})
        self.adjust_lengths(entries)
        return entries

    def scheduled_slice(self):
        """
        Return the slice of entries that are scheduled.
        """
        return slice(None)
    
    def scheduled(self):
        """
        Return entries that have defined values.
        """
        return self.entries

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
        write(self.block_items, self.entries, indices)
