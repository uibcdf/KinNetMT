from os.path import basename as _basename
from kinnetmt.native import Network as _native_Network

form_name=_basename(__file__).split('.')[0].replace('api_','').replace('_','.')

is_form={
    'native.Network': form_name,
    _native_Network : form_name
}


def to_networkx_Graph(item):
    pass

