import pathlib
from setuptools import setup

# The directory containing this file
HERE = pathlib.Path(__file__).parent

# The text of the README file
README = (HERE / "README.md").read_text()

# This call to setup() does all the work
setup(
    name="copybook",
    version="1.0.7",
    description="python copybook parser",
    long_description=README,
    
    long_description_content_type="text/markdown",
    url="https://github.com/zalmane/copybook",
    author="Oren Elias",
    author_email="support@datayoga.io",
    license="MIT",
    classifiers=[
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
    ],
    packages=["copybook"],
    include_package_data=True,
    install_requires=['pyparsing'],
    setup_requires=['pytest-runner'],
    tests_require=['pytest'],
)