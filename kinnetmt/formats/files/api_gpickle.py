from os.path import basename as _basename

form_name=_basename(__file__).split('.')[0].split('_')[-1]

is_form = {
    'gpickle': form_name,
    'gpickle.gz': form_name
    }

def to_native_Network(item):
    from ..classes.api_networkx_Graph import to_native_Network as _networkx_Graph_to_native_Network
    tmp_form_aux = to_networkx_Graph(item)
    tmp_form = _networkx_Graph_to_native_Network(tmp_form_aux)
    del(_networkx_Graph_to_native_Network,tmp_form_aux)
    return tmp_form

def to_native_KineticNetwork(item):
    pass

def to_native_PotentialEnergyNetwork(item):
    from ..classes.api_networkx_Graph import to_native_PotentialEnergyNetwork as \
    _networkx_Graph_to_native_PotentialEnergyNetwork
    tmp_form_aux = to_networkx_Graph(item)
    tmp_form = _networkx_Graph_to_native_PotentialEnergyNetwork(tmp_form_aux)
    del(_networkx_Graph_to_native_PotentialEnergyNetwork,tmp_form_aux)
    return tmp_form


def to_networkx(item):
    return to_networkx_Graph(item)

def to_networkx_Graph(item):
    from networkx import read_gpickle as _read_gpickle
    tmp_form = _read_gpickle(item)
    del(_read_gpickle)
    return tmp_form
