# -*- coding: utf-8 -*-
"""
Created on Mon May 11 14:34:08 2020

@author: Olli Nevalainen (olli.nevalainen@fmi.fi),
 Finnish Meteorological Institute)

Olli's python implementation of ESA SNAP s2toolbox biophysical processor and
computation of vegetation indices.
See ATBD at https://step.esa.int/docs/extra/ATBD_S2ToolBox_L2B_V1.1.pdf
And java source code at
https://github.com/senbox-org/s2tbx/tree/main/s2tbx-biophysical/src/main/java/org/esa/s2tbx/biophysical

Caveats
Currently changes out of bounds inputs and outputs to nan (or min or max value
if output wihtin tolerance). Maybe output flagging information as well ( i.e.
diffferent flags input and output out of bounds).

Convex hull input checking currently disabled. It's computationally slow and
 not sure of its benefits. Better to filter out bad data based on L2A quality
 info/classification\
    and hope averaging removes some bad pixels.
"""

import requests
import io
import numpy as np
import xarray as xr

# url to Sentinel 2 Toolbox's auxdata
# This base_url points towards the original toolbox(not the one created by Olli)
base_url = "https://raw.githubusercontent.com/senbox-org/s2tbx/main/s2tbx-biophysical/src/main/resources/auxdata/2_1/{}/{}"


def get_fromurl(var, pattern):
    """
    Fetches the contents of a text file from the base url and stores it in a ndarray.

    Author: Ayush Prasad

    Parameters
    ----------
    var (str) -- type of the product, one of FAPAR, FCOVER, LAI, LAI_Cab and LAI_Cw.
    pattern (str) -- name of the file excluding the initial variable part.

    Returns
    -------
    ndarray -- loaded with the contents of the text file.
    """
    # attach variable and file name to the base url
    res_url = base_url.format(var, str(var) + "%s" % str(pattern))
    # make a GET request to the url to fetch the data.
    res_url = requests.get(res_url)
    # check the HTTP status code to see if any error has occured.
    res_url.raise_for_status()
    # store the contents of the url in an in-memory buffer and use it to load the ndarray.
    return np.loadtxt(io.BytesIO(res_url.content), delimiter=",")


# Read SNAP Biophysical processor neural network parameters
nn_params = {}
for var in ["FAPAR", "FCOVER", "LAI", "LAI_Cab", "LAI_Cw"]:
    norm_minmax = get_fromurl(var, "_Normalisation")
    denorm_minmax = get_fromurl(var, "_Denormalisation")
    layer1_weights = get_fromurl(var, "_Weights_Layer1_Neurons")
    layer1_bias = get_fromurl(var, "_Weights_Layer1_Bias").reshape(-1, 1)
    layer2_weights = get_fromurl(var, "_Weights_Layer2_Neurons").reshape(1, -1)
    layer2_bias = get_fromurl(var, "_Weights_Layer2_Bias").reshape(1, -1)
    extreme_cases = get_fromurl(var, "_ExtremeCases")

    if var == "FCOVER":
        nn_params[var] = {
            "norm_minmax": norm_minmax,
            "denorm_minmax": denorm_minmax,
            "layer1_weights": layer1_weights,
            "layer1_bias": layer1_bias,
            "layer2_weights": layer2_weights,
            "layer2_bias": layer2_bias,
            "extreme_cases": extreme_cases,
        }
    else:
        defdom_min = get_fromurl(var, "_DefinitionDomain_MinMax")[0, :].reshape(-1, 1)
        defdom_max = get_fromurl(var, "_DefinitionDomain_MinMax")[1, :].reshape(-1, 1)
        defdom_grid = get_fromurl(var, "_DefinitionDomain_Grid")
        nn_params[var] = {
            "norm_minmax": norm_minmax,
            "denorm_minmax": denorm_minmax,
            "layer1_weights": layer1_weights,
            "layer1_bias": layer1_bias,
            "layer2_weights": layer2_weights,
            "layer2_bias": layer2_bias,
            "defdom_min": defdom_min,
            "defdom_max": defdom_max,
            "defdom_grid": defdom_grid,
            "extreme_cases": extreme_cases,
        }


def _normalization(x, x_min, x_max):
    x_norm = 2 * (x - x_min) / (x_max - x_min) - 1
    return x_norm


def _denormalization(y_norm, y_min, y_max):
    y = 0.5 * (y_norm + 1) * (y_max - y_min)
    return y


def _input_ouf_of_range(x, variable):
    x_copy = x.copy()
    x_bands = x_copy[:8, :]

    # check min max domain
    defdom_min = nn_params[variable]["defdom_min"][:, 0].reshape(-1, 1)
    defdom_max = nn_params[variable]["defdom_max"][:, 0].reshape(-1, 1)
    bad_input_mask = (x_bands < defdom_min) | (x_bands > defdom_max)
    bad_vector = np.any(bad_input_mask, axis=0)
    x_bands[:, bad_vector] = np.nan

    # convex hull check, currently disabled due to time consumption vs benefit
    # gridProject = lambda v: np.floor(10 * (v - defdom_min) / (defdom_max - defdom_min) + 1 ).astype(int)
    # x_bands = gridProject(x_bands)
    # isInGrid = lambda v: any((v == x).all() for x in nn_params[variable]['defdom_grid'])
    # notInGrid = ~np.array([isInGrid(v) for v in x_bands.T])
    # x[:,notInGrid | bad_vector] = np.nan

    x_copy[:, bad_vector] = np.nan
    return x_copy


def _output_ouf_of_range(output, variable):
    new_output = np.copy(output)
    tolerance = nn_params[variable]["extreme_cases"][0]
    output_min = nn_params[variable]["extreme_cases"][1]
    output_max = nn_params[variable]["extreme_cases"][2]

    new_output[output < (output_min + tolerance)] = np.nan
    new_output[(output > (output_min + tolerance)) & (output < output_min)] = output_min
    new_output[(output < (output_max - tolerance)) & (output > output_max)] = output_max
    new_output[output > (output_max - tolerance)] = np.nan
    return new_output


def _compute_variable(x, variable):

    x_norm = np.zeros_like(x)
    x = _input_ouf_of_range(x, variable)
    x_norm = _normalization(
        x,
        nn_params[variable]["norm_minmax"][:, 0].reshape(-1, 1),
        nn_params[variable]["norm_minmax"][:, 1].reshape(-1, 1),
    )

    out_layer1 = np.tanh(
        nn_params[variable]["layer1_weights"].dot(x_norm)
        + nn_params[variable]["layer1_bias"]
    )
    out_layer2 = (
        nn_params[variable]["layer2_weights"].dot(out_layer1)
        + nn_params[variable]["layer2_bias"]
    )
    output = _denormalization(
        out_layer2,
        nn_params[variable]["denorm_minmax"][0],
        nn_params[variable]["denorm_minmax"][1],
    )[0]
    output = _output_ouf_of_range(output, variable)
    output = output.reshape(1, np.shape(x)[1])
    return output


def run_snap_biophys(dataset, variable):
    """Compute specified variable using the SNAP algorithm.

    See ATBD at https://step.esa.int/docs/extra/ATBD_S2ToolBox_L2B_V1.1.pdf

    Parameters
    ----------
    dataset : xr dataset
        xarray dataset.
    variable : str
        Options 'FAPAR', 'FCOVER', 'LAI', 'LAI_Cab' or 'LAI_Cw'

    Returns
    -------
    xarray dataset
        Adds the specified variable array to dataset (variable name in
        lowercase).

    """
    # generate view angle bands/layers
    vz = (
        np.ones_like(dataset.band_data[:, 0, :, :]).T
        * np.cos(np.radians(dataset.view_zenith)).values
    )
    vz = vz[..., np.newaxis]
    vzarr = xr.DataArray(
        vz,
        coords=[dataset.y, dataset.x, dataset.time, ["view_zenith"]],
        dims=["y", "x", "time", "band"],
    )

    sz = (
        np.ones_like(dataset.band_data[:, 0, :, :]).T
        * np.cos(np.radians(dataset.sun_zenith)).values
    )
    sz = sz[..., np.newaxis]
    szarr = xr.DataArray(
        sz,
        coords=[dataset.y, dataset.x, dataset.time, ["sun_zenith"]],
        dims=["y", "x", "time", "band"],
    )

    raz = (
        np.ones_like(dataset.band_data[:, 0, :, :]).T
        * np.cos(np.radians(dataset.sun_azimuth - dataset.view_azimuth)).values
    )
    raz = raz[..., np.newaxis]
    razarr = xr.DataArray(
        raz,
        coords=[dataset.y, dataset.x, dataset.time, ["relative_azimuth"]],
        dims=["y", "x", "time", "band"],
    )

    newarr = xr.concat([dataset.band_data, vzarr, szarr, razarr], dim="band")
    newarr = newarr.stack(xy=("x", "y"))
    arr = xr.apply_ufunc(
        _compute_variable,
        newarr,
        input_core_dims=[["band", "xy"]],
        output_core_dims=[["xy"]],
        kwargs={"variable": variable},
        vectorize=True,
    ).unstack()
    return dataset.assign({variable.lower(): arr})
