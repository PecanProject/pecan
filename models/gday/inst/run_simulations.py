#!/usr/bin/env python

"""
Test wrapper script to run GDAY at fluxnet site

(i) Spin-up using 20 years of recycled met data: n-cycle off.
(ii) Run simulation using met forcing years.

Obviously parameters will need to be set for this PFT, for the moment I'm just
going to use vaguely made up forest paramaters.
"""

import numpy as np
import os
import shutil
import sys
import subprocess

__author__  = "Martin De Kauwe, Tony Gardella"
__version__ = "1.0 (03.08.2016)"
__email__   = "mdekauwe@gmail.com"


sys.path.append("@PATH_SCRIPTS@")
import adjust_gday_param_file as ad

def main(experiment_id, latitude, albedo, topsoil_type,
         rootsoil_type, finesoil, SPIN_UP=False, RUN_SIM=False):
    
    gday_exe = "@PATHTOGDAY@"
    GDAY_SPIN = gday_exe + " -s -p "
    GDAY = gday_exe + " -p "

    # dir names
    base_param_name = "base_start"
    base_dir = os.getcwd()
    base_param_dir = "@PATH_PARAMS@"
    param_dir = "@RUNDIR@"
    met_dir = "@SITE_MET@"
    run_dir = "@RUNDIR@"

    if SPIN_UP == True:

        # copy base files to make two new experiment files
        shutil.copy(os.path.join(base_param_dir, base_param_name + ".cfg"),
                    os.path.join(param_dir, "%s_model_spinup.cfg" % \
                                                (experiment_id)))

        # Run model to equilibrium assuming forest, growing C pools from
        # effectively zero
        itag = "%s_model_spinup" % (experiment_id)
        otag = "%s_model_spunup" % (experiment_id)
        mtag = "%s.daily_spin.csv" % (experiment_id)
        out_fn = itag + "_equilib.out"
        out_param_fname = os.path.join(param_dir, otag + ".cfg")
        cfg_fname = os.path.join(param_dir, itag + ".cfg")
        met_fname = os.path.join(met_dir, mtag)
        out_fname = os.path.join(run_dir, out_fn)
        replace_dict = {
                        # files
                        "out_param_fname": "%s" % (out_param_fname),
                        "cfg_fname": "%s" % (cfg_fname),
                        "met_fname": "%s" % (met_fname),
                        "out_fname": "%s" % (out_fname),

                        # state - default C:N 25.
                         "age": "0.0",
                         "canht": "20.0", # Made up
                         "activesoil": "0.001",
                         "activesoiln": "0.00004",
                         "age": "0.0",
                         "branch": "0.001",
                         "branchn": "0.00004",
                         "cstore": "0.001",
                         "inorgn": "0.00004",
                         "metabsoil": "0.0",
                         "metabsoiln": "0.0",
                         "metabsurf": "0.0",
                         "metabsurfn": "0.0",
                         "nstore": "0.00004",
                         "passivesoil": "0.001",
                         "passivesoiln": "0.0004",
                         "prev_sma": "-999.9",
                         "root": "0.001",
                         "root_depth": "-9999.9",
                         "rootn": "0.00004",
                         "sapwood": "0.001",
                         "shoot": "0.001",
                         "shootn": "0.00004",
                         "slowsoil": "0.001",
                         "slowsoiln": "0.00004",
                         "stem": "0.001",
                         "stemn": "0.00004",
                         "stemnimm": "0.00004",
                         "stemnmob": "0.0",
                         "structsoil": "0.001",
                         "structsoiln": "0.00004",
                         "structsurf": "0.001",
                         "structsurfn": "0.00004",
                         "croot": "0.0",   # don't simulate coarse roots
                         "crootn": "0.0",  # don't simulate coarse roots

                         # parameters
                         #"fix_lai": "3.0",
                         "latitude": "%f" % (latitude),
                         "albedo": "%f" % (albedo),
                         "finesoil": "%f" % (finesoil),
                         "intercep_frac": "0.15",
                         "max_intercep_lai": "3.0",
                         "slamax": "4.4",     # made up [m2 kg-1 DW]
                         "sla": "4.4",        # made up [m2 kg-1 DW]
                         "slazero": "4.4",    # made up [m2 kg-1 DW]
                         "cfracts": "0.5",
                         "lai_closed": "0.5",  # I am effectively turning this feature off by setting it so low

                         "c_alloc_fmax": "0.25",
                         "c_alloc_fmin": "0.25",
                         "c_alloc_rmax": "0.05",
                         "c_alloc_rmin": "0.05",
                         "c_alloc_bmax": "0.1",
                         "c_alloc_bmin": "0.1",
                         "c_alloc_cmax": "0.0", # turn off coarse roots!


                         #"c_alloc_fmax": "0.35",
                         #"c_alloc_fmin": "0.15",
                         #"c_alloc_rmax": "0.35",
                         #"c_alloc_rmin": "0.05",
                         #"c_alloc_bmax": "0.1",
                         #"c_alloc_bmin": "0.1",
                         #"c_alloc_cmax": "0.0", # turn off coarse roots!


                         "fretrans": "0.5",
                         "rretrans": "0.0",
                         "bretrans": "0.0",
                         "wretrans": "0.0",
                         "ncwnewz": "0.003",
                         "ncwnew": "0.003",
                         "ncwimmz": "0.003",
                         "ncwimm": "0.003",
                         "ncbnewz": "0.003",
                         "ncbnew": "0.003",
                         "ncrfac": "0.8",
                         "ncmaxfyoung": "0.04",
                         "ncmaxfold": "0.04",
                         "ncmaxr": "0.03",
                         "retransmob": "0.0",
                         "fdecay": "0.59988",      # Protocol  [years-1]
                         "fdecaydry": "0.59988",   # Protocol
                         "rdecay": "0.33333",      # Protocol
                         "rdecaydry": "0.33333",   # Protocol
                         "bdecay": "0.02",         # No data, assuming 50 years
                         "wdecay": "0.02",
                         "crdecay": "0.00",  # turn off coarse roots!
                         "watdecaydry": "0.0",
                         "watdecaywet": "0.1",
                         "ligshoot": "0.24",      # Based on White et al. 2000 for ENF
                         "ligroot": "0.22",       # Based on White et al. 2000
                         "rateuptake": "1.9",           # set somewhat (very) arbitarly to get an LAI ~ 4.
                         "rateloss": "0.3",

                         "topsoil_depth": "50.0",
                         "rooting_depth": "2000.0",
                         "topsoil_type": topsoil_type,
                         "rootsoil_type": rootsoil_type,
                         "ctheta_topsoil": "-999.9",     # Derive based on soil type
                         "ntheta_topsoil": "-999.9",     # Derive based on soil type
                         "ctheta_root": "-999.9",        # Derive based on soil type
                         "ntheta_root": "-999.9",        # Derive based on soil type
                         "measurement_temp": "25.0",
                         "dz0v_dh": "0.075", # However I have used value from Jarvis, quoted in Jones 1992, pg. 67. Produces a value within the bounds of 3.5-1.1 mol m-2 s-1 Drake, 2010, GCB for canht=17
                         "displace_ratio": "0.78",
                         "g1": "2.74",


                         #"jmax": "60.0",
                         #"vcmax": "30.0",
                         "jmaxna": "60.0",
                         "jmaxnb": "0.0",
                         "vcmaxna": "30.0",
                         "vcmaxnb": "0.0",


                         "measurement_temp": "25.0",
                         "heighto": "4.826",
                         "htpower": "0.35",
                         "height0": "5.0",
                         "height1": "30.0",
                         "leafsap0": "4000.0",
                         "leafsap1": "2700.0",
                         "branch0": "5.61",
                         "branch1": "0.346",
                         "croot0": "0.34",
                         "croot1": "0.84",
                         "targ_sens": "0.5",
                         "density": "480.0",
                         "sapturnover": "0.1",

                         "prescribed_leaf_NC": "0.03",

                         # control
                         "adjust_rtslow": "false",  # priming, off
                         "alloc_model": "fixed",
                         "assim_model": "mate",
                         "calc_sw_params": "true",   #false=use fwp values, true=derive them
                         "deciduous_model": "false",
                         "disturbance": "false",
                         "exudation": "false",
                         "fixed_stem_nc": "true",
                         "fixleafnc": "false",
                         "fixed_lai": "false",
                         "grazing": "false",
                         "gs_model": "medlyn",
                         "model_optroot": "false",
                         "modeljm": "1",
                         "ncycle": "false",
                         "nuptake_model": "1",
                         "passiveconst": "false",
                         "print_options": "end",
                         "ps_pathway": "c3",
                         "respiration_model": "fixed",
                         "strfloat": "0",
                         "sw_stress_model": "1",  # Sands and Landsberg
                         "water_stress": "true",

        }
        ad.adjust_param_file(cfg_fname, replace_dict)
        os.system(GDAY_SPIN + cfg_fname)


    if RUN_SIM == True:

        # dir names
        param_dir = os.path.join("@RUNDIR@")
        met_dir = os.path.join("@SITE_MET@")
        run_dir = os.path.join("@RUNDIR@")
        
        if SPIN_UP == True:
            shutil.copy(os.path.join(param_dir, "%s_model_spunup.cfg" % (experiment_id)),
                        os.path.join(param_dir, "%s_model_spunup_adj.cfg" % (experiment_id)))

        itag = "%s_model_spunup_adj" % (experiment_id)
        otag = "%s_simulation" % (experiment_id)
        mtag = "%s.daily_run.csv" % (experiment_id)
        out_fn = "%s_simulation.csv" % (experiment_id)
        out_param_fname = os.path.join(param_dir, otag + ".cfg")
        cfg_fname = os.path.join(param_dir, itag + ".cfg")
        met_fname = os.path.join(met_dir, mtag)
        out_fname = os.path.join(run_dir, "gday_out.csv")
        replace_dict = {

                         # files
                         "out_param_fname": "%s" % (out_param_fname),
                         "cfg_fname": "%s" % (cfg_fname),
                         "met_fname": "%s" % (met_fname),
                         "out_fname": "%s" % (out_fname),

                         # control
                         "print_options": "daily",
                         "sub_daily": "false",

                        }
        ad.adjust_param_file(cfg_fname, replace_dict)
        os.system(GDAY + cfg_fname)



if __name__ == "__main__":

    experiment_id = "@SITE@"

    latitude=@LATITUDE@
    main(experiment_id, latitude=latitude, albedo=0.2,
         topsoil_type="silty_clay_loam", rootsoil_type="silty_clay_loam",
         finesoil=0.5, SPIN_UP=True, RUN_SIM=True)
