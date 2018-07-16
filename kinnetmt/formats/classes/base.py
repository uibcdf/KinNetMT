from os import listdir as _listdir
from os.path import dirname as _dirname
from importlib import import_module as _import_module

base_package = __name__.replace('.base','')

def _not_implemented_conversion(item):
    raise NotImplementedError("This conversion has not been implemented yet")

list_api_forms=[filename.split('.')[0] for filename in _listdir(_dirname(__file__)) if filename.startswith('api')]

dict_api_forms={}
list_forms=[]
dict_converter={}
dict_is_form={}

for api_form in list_api_forms:
    module_api_form=_import_module('.'+api_form,base_package)
    form_name=module_api_form.form_name
    list_forms.append(form_name)
    dict_api_forms[form_name]=module_api_form
    dict_is_form.update(module_api_form.is_form)

for form_name in list_forms:
    dict_converter[form_name]= {}
    for method in dict_api_forms[form_name].__dict__.keys():
        if method.startswith('to_'):
            out_form_name=method.replace('to_','').replace('_','.')
            dict_converter[form_name][out_form_name]= getattr(dict_api_forms[form_name],method)

list_forms=sorted(list_forms)

if 'out_form_name' in globals():
    del(out_form_name)
if 'syntaxis_name' in globals():
    del(syntaxis_name)

del(api_form, list_api_forms, form_name, method, module_api_form, base_package)
