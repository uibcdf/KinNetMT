## Classes
from .formats.classes import dict_is_form as _dict_classes_is_form, \
    list_forms as _list_classes_forms, \
    dict_converter as _dict_classes_converter

## Files
from .formats.files import dict_is_form as _dict_files_is_form, \
    list_forms as _list_files_forms, \
    dict_converter as _dict_files_converter

_dict_is_form = {**_dict_classes_is_form, **_dict_files_is_form}
_dict_converter = {**_dict_classes_converter, **_dict_files_converter}

_dict_from_to = {}
_dict_to_from = {}

for in_form in _dict_converter.keys():
    _dict_from_to[in_form]=_dict_converter[in_form].keys()

for in_form, out_forms in _dict_from_to.items():
    for out_form in out_forms:
        try:
            _dict_to_from[out_form].append(in_form)
        except:
            _dict_to_from[out_form]=[]
            _dict_to_from[out_form].append(in_form)

for in_form in _dict_from_to.keys():
    _dict_from_to[in_form]=sorted(_dict_from_to[in_form])

for out_form in _dict_to_from.keys():
    _dict_to_from[out_form]=sorted(_dict_to_from[out_form])


def extract():

    pass

def get_form(item=None):

    if type(item)==str:
        if ':' in item:
            item=item.split(':')[0]+":id"
        else:
            item=item.split('.')[-1]

    try:
        return _dict_is_form[type(item)]
    except:
        try:
            return _dict_is_form[item]
        except:
            raise NotImplementedError("This item's form has not been implemented yet")

def load(item=None,form='native.Network'):

    return convert(item,form)

def convert(item=None,form=None):

    in_form  = get_form(item)
    out_form = form
    out_file = None

    if type(out_form)==str:
        if out_form.split('.')[-1] in _list_files_forms:
            out_form=form.split('.')[-1]
            out_file=form

    if out_file is not None:
        return _dict_converter[in_form][out_form](item,out_file)
    else:
        return _dict_converter[in_form][out_form](item)


def info_forms(engines=True,classes=True,files=True,verbose=True):

    tmp_dict={
        'classes':_list_classes_forms,
        'files':_list_files_forms
        }

    return tmp_dict

def info_load(from_form=None,to_form=None,verbose=True):

    return info_convert(from_form,to_form,verbose)

def info_convert(from_form=None,to_form=None,verbose=True):

    tmp_output=None

    if from_form is not None:
        if to_form is not None:
            if to_form in _dict_from_to[from_form]:
                tmp_output= True
            else:
                tmp_output= False
        else:
            tmp_output=_dict_from_to[from_form]
    elif to_form is not None:
        tmp_output=_dict_to_from[to_form]
    else:
        if verbose:
            print('From... to...')
            for key in _dict_from_to.keys():
                print(key,': ',_dict_from_to[key])
            print('\nTo... from...')
            for key in _dict_to_from.keys():
                print(key,': ',_dict_to_from[key])
            pass

    if (tmp_output is not None) and verbose:
        print(tmp_output)
    else:
        pass


