from setuptools import find_packages, setup

setup(
    name="advent",
    packages=find_packages(where="src"),
    package_dir={"advent": "src/advent"},
    package_data={"advent": ["data/*/*"]},
)
