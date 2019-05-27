
class NoMatchingPatch(ValueError):
    pass

# parsed from DBBC documentation
channel_map = \
{
    ('VSI1', 'LBA'): ['1US', '1UM', '2US', '2UM', '5US', '5UM', '6US', '6UM', '3US', '3UM', '4US', '4UM', '7US', '7UM', '8US', '8UM', '1LS', '1LM', '2LS', '2LM', '5LS', '5LM', '6LS', '6LM', '3LS', '3LM', '4LS', '4LM', '7LS', '7LM', '8LS', '8LM'], 
    ('VSI2', 'LBA'): ['1US', '1UM', '2US', '2UM', '5US', '5UM', '6US', '6UM', '3US', '3UM', '4US', '4UM', '7US', '7UM', '8US', '8UM', '1LS', '1LM', '2LS', '2LM', '5LS', '5LM', '6LS', '6LM', '3LS', '3LM', '4LS', '4LM', '7LS', '7LM', '8LS', '8LM'], 
    ('VSI2', 'WASTRO'): ['9US', '9UM', '10US', '10UM', '11US', '11UM', '12US', '12UM', '13US', '13UM', '14US', '14UM', '15US', '15UM', '16US', '16UM', '9LS', '9LM', '10LS', '10LM', '11lS', '11LM', '12LS', '12LM', '13LS', '13LM', '14LS', '14LM', '15LS', '15LM', '16LS', '16LM'], 
    ('VSI2', 'GEO'): ['1US', '1UM', '2US', '2UM', '3US', '3UM', '4US', '4UM', '5US', '5UM', '6US', '6UM', '7US', '7UM', '8US', '8UM', '1LS', '1LM', '8LS', '8LM', '9US', '9UM', '10US', '10UM', '11US', '11UM', '12US', '12UM', '13US', '13UM', '14US', '14UM'], 
    ('VSI1', 'WASTRO'): ['1US', '1UM', '2US', '2UM', '3US', '3UM', '4US', '4UM', '5US', '5UM', '6US', '6UM', '7US', '7UM', '8US', '8UM', '1LS', '1LM', '2LS', '2LM', '3LS', '3LM', '4LS', '4LM', '5LS', '5LM', '6LS', '6LM', '7LS', '7LM', '8LS', '8LM'], 
    ('VSI2', 'ASTRO'): ['1US', '1UM', '2US', '2UM', '3US', '3UM', '4US', '4UM', '5US', '5UM', '6US', '6UM', '7US', '7UM', '8US', '8UM', '1LS', '1LM', '2LS', '2LM', '3LS', '3LM', '4LS', '4LM', '5LS', '5LM', '6LS', '6LM', '7LS', '7LM', '8LS', '8LM'], 
    ('VSI1', 'ASTRO'): ['1US', '1UM', '2US', '2UM', '3US', '3UM', '4US', '4UM', '5US', '5UM', '6US', '6UM', '7US', '7UM', '8US', '8UM', '1LS', '1LM', '2LS', '2LM', '3LS', '3LM', '4LS', '4LM', '5LS', '5LM', '6LS', '6LM', '7LS', '7LM', '8LS', '8LM'], 
    ('VSI1', 'ASTRO3'): ['1US', '1UM', '3US', '3UM', '5US', '5UM', '7US', '7UM', '9US', '9UM', '11US', '11UM', '13US', '13UM', '15US', '15UM', '1LS', '1LM', '3LS', '3LM', '5LS', '5LM', '7LS', '7LM', '9LS', '9LM', '11LS', '11LM', '13LS', '13LM', '15LS', '15LM'], 
    ('VSI1', 'ASTRO2'): ['1US', '1UM', '2US', '2UM', '3US', '3UM', '4US', '4UM', '9US', '9UM', '10US', '10UM', '11US', '11UM', '12US', '12UM', '1LS', '1LM', '2LS', '2LM', '3LS', '3LM', '4LS', '4LM', '9LS', '9LM', '10LS', '10LM', '11LS', '11LM', '12LS', '12LM'], 
    ('VSI2', 'ASTRO2'): ['1US', '1UM', '2US', '2UM', '3US', '3UM', '4US', '4UM', '9US', '9UM', '10US', '10UM', '11US', '11UM', '12US', '12UM', '1LS', '1LM', '2LS', '2LM', '3LS', '3LM', '4LS', '4LM', '9LS', '9LM', '10LS', '10LM', '11LS', '11LM', '12LS', '12LM'], 
    ('VSI1', 'GEO'): ['1US', '1UM', '2US', '2UM', '3US', '3UM', '4US', '4UM', '5US', '5UM', '6US', '6UM', '7US', '7UM', '8US', '8UM', '1LS', '1LM', '8LS', '8LM', '9US', '9UM', '10US', '10UM', '11US', '11UM', '12US', '12UM', '13US', '13UM', '14US', '14UM']
}

def get_input_bitstreams(channel_data):
    """
    Returns a list of VSI-H input bitstreams given a iterable of 
    (sideband, BBC number, sign/mag) tuples
    """
    channel_string = ["{}{}{}".format(d[1], 
                                      d[0].upper(), 
                                      "S" if d[2] == "sign" else "M")
                      for d in channel_data]
    # commented VSI2 attempts which are the same as their VSI1 counterpart
    # also, VSI1, WASTRO == VSI, ASTRO
    attempt_order = [
        ('VSI1', 'ASTRO3'),
        ('VSI1', 'ASTRO2'),
        ('VSI1', 'ASTRO'),
        ('VSI1', 'LBA'),
        # ('VSI2', 'LBA'),
        ('VSI2', 'WASTRO'),
        # ('VSI2', 'GEO'),
        # ('VSI1', 'WASTRO'),
        # ('VSI2', 'ASTRO'),
        # ('VSI2', 'ASTRO2'),
        ('VSI1', 'GEO')
    ]
    for patch_key in attempt_order:
        patch_attempt = channel_map[patch_key]
        try:
            return [patch_attempt.index(s) for s in channel_string]
        except ValueError:
            pass
    raise NoMatchingPatch()
