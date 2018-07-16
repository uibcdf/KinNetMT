from os.path import basename as _basename

form_name=_basename(__file__).split('.')[0].split('_')[-1]

is_form = {
    'gpickle': form_name,
    'gpickle.gz': form_name
    }

def to_native_Network(item):
    pass

def to_native_KineticNetwork(item):
    pass

def to_native_PotentialEnergyNetwork(item):
    pass

def to_networkx_Graph(item):

    pass
