from setuptools import find_packages

from numpy.distutils.core import setup
from numpy.distutils.extension import Extension


ext_potential_energy_network = Extension(
    name = 'kinnetmt.lib.potential_energy',
    #extra_compile_args = ["-fcheck=all"],
    extra_compile_args = [],
    libraries = [],
    language = 'f90',
    sources = ['kinnetmt/lib/potential_energy.f90'],
)


setup(
    name='kinnetmt',
    version='0.0.1',
    author='UIBCDF Lab',
    author_email='uibcdf@gmail.com',
    package_dir={'kinnetmt': 'kinnetmt'},
    packages=find_packages(),
    package_data={'kinnetmt': []},
    ext_modules=[ext_potential_energy_network],
    scripts=[],
    url='http://uibcdf.org',
    download_url ='https://github.com/uibcdf/KinNetMT',
    license='MIT',
    description="---",
    long_description="---",
)
