from os.path import basename as _basename
from networkx import Graph as _networkx_Graph

form_name=_basename(__file__).split('.')[0].replace('api_','').replace('_','.')

is_form={
    'native.Network': form_name,
    _networkx_Graph : form_name
}


def to_native_Network(item):
    from kinnetmt.native import Network as _native_Network
    return _native_Network(item=item)

def to_native_KineticNetwork(item):
    pass

def to_native_PotentialEnergyNetwork(item):
    from kinnetmt.native import PotentialEnergyNetwork as _native_PotentialEnergyNetwork
    return _native_PotentialEnergyNetwork(item=item)
    pass
