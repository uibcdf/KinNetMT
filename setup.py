import distutils.extension
from setuptools import setup, find_packages

setup(
    name='kinnetmt',
    version='0.0.1',
    author='UIBCDF Lab',
    author_email='uibcdf@gmail.com',
    package_dir={'kinnetmt': 'kinnetmt'},
    packages=find_packages(),
    package_data={'kinnetmt': []},
    scripts=[],
    url='http://uibcdf.org',
    download_url ='https://github.com/uibcdf/KinNetMT',
    license='MIT',
    description="---",
    long_description="---",
)
