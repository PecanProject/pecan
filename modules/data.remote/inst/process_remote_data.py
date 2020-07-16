#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
process_remote_data controls functions which perform further computation on the data.

Requires Python3

Author: Ayush Prasad
"""

from importlib import import_module


def process_remote_data(aoi_name, process_data, outdir, algorithm):
    """
    uses processing functions to perform computation on input data
    
    Parameters
    ----------
    aoi_name (str) -- name of the AOI.

    process_data (str) -- the type of output variable

    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.

    algorithm (str) -- name of the algorithm used to perform computation.

    Returns
    -------
    Nothing:
            output netCDF is saved in the specified directory.
    """

    # locate the input file
    input_file = "".join([outdir, "/", aoi_name, "_", "bands", ".nc"])
    # extract the computation which is to be done
    output = process_data
    # construct the function name
    func_name = "".join(["bands", "2", output, "_", algorithm])
    # import the module
    module = import_module(func_name)
    # import the function from the module
    func = getattr(module, func_name)
    # call the function
    func(input_file, outdir)
