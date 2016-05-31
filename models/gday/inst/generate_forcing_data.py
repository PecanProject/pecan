#!/usr/bin/env python

""" 
Create a G'DAY spinup & driving met forcing given a series of flux netcdf files.
NB. Ndep & CO2 (most likely) are missing from the flux files. In this scenario
CO2 is being set to 400 ppm and Ndep to -999.9. This means you will need to
run the model with the N-cycle off.

r_interface_wrapper is the function to interface to R

That's all folks.
"""
__author__ = "Martin De Kauwe"
__version__ = "1.0 (13.05.2016)"
__email__ = "mdekauwe@gmail.com"

import sys
import glob
import os
import csv
import math
import numpy as np
from datetime import date
import calendar
import pandas as pd
import datetime as dt
import netCDF4 as nc

def r_interface_wrapper(site, outfile_tag, fpath=None, file_list=None):
    # Option to supply the path to where the files are in which case the code
    # will attempt to be smart and grab all the files. This could be a problem
    # if there is a gap in the year sequence. So there is also the option to
    # supply the exact years you want, i.e. a smaller subset.
    if file_list is None:
        file_list = glob.glob(os.path.join(fpath, "%s.*.nc" % site))

    C = CreateMetData(site, file_list, outfile_tag)
    C.main()


class CreateMetData(object):

    def __init__(self, site, file_list, outfile_tag):

        self.file_list = file_list
        self.site = site
        self.spinup_ofname = "%s.spin.csv" % (outfile_tag)
        self.forcing_ofname = "%s.run.csv" % (outfile_tag)
        self.ovar_names = ['#year', 'doy', 'hod', 'rain', 'par', 'tair',
                           'tsoil', 'vpd', 'co2', 'ndep', 'wind', 'press']
        self.ounits = ['#--', '--', '--', 'mm/30min', 'umol/m2/s','degC',
                       'degC', 'kPa', 'ppm', 't/ha/30min', 'm/s','kPa']
        self.lat = -999.9
        self.lon = -999.9

        # unit conversions
        self.SEC_TO_HFHR = 60.0 * 30.0 # sec/half hr
        self.PA_TO_KPA = 0.001
        self.J_TO_MJ = 1.0E-6
        self.K_to_C = 273.15
        self.G_M2_to_TONNES_HA = 0.01
        self.MM_S_TO_MM_HR = 3600.0
        self.SW_2_PAR = 2.3


    def main(self):

        frames = []
        for fname in self.file_list:
            df_yr = self.read_nc_file(fname)
            frames.append(df_yr)
        df = pd.concat(frames)

        # Create spinup file
        start_yr = df.index.year[0]
        end_yr = df.index.year[-1]

        num_yrs = 20
        yr_sequence = self.get_random_year_sequence(start_yr, end_yr, num_yrs,
                                                    preserve_leap=False)

        ndep = -999.9
        co2 = 285.0
        self.write_spinup_file(df, yr_sequence, vary_co2=False, co2_data=co2,
                               vary_ndep=False, ndep_data=ndep)
        self.write_met_file(df)

    def write_spinup_file(self, df, yr_sequence, vary_co2=False, co2_data=None,
                          vary_ndep=False, ndep_data=None):

        start_sim = yr_sequence[0]
        end_sim = yr_sequence[-1]
        year = str(start_sim)

        try:
            ofp = open(self.spinup_ofname, 'wb')
            wr = csv.writer(ofp, delimiter=',', quoting=csv.QUOTE_NONE,
                            escapechar=None, dialect='excel')
            wr.writerow(['# %s 30-min met forcing' % (site)])
            wr.writerow(['# Data from %s-%s' % (start_sim, end_sim)])
            wr.writerow(['# Created by Martin De Kauwe: %s' % date.today()])
            wr.writerow([var for i, var in enumerate(self.ounits)])
            wr.writerow([var for i, var in enumerate(self.ovar_names)])

        except IOError:
            raise IOError('Could not write spinup file: %s' % \
                          self.spinup_ofname)

        for i, yr in enumerate(yr_sequence):
            days = np.unique(df[df.year == yr].doy)
            for j, doy in enumerate(days):
                days_data = df[(df.year == yr) & (df.doy == doy)]

                # otherwise we can't index 0-47 for HOD
                days_data = days_data.reset_index()
                for hod in xrange(len(days_data)):

                    # mm/sec -> mm/30 min
                    # need to split the rain in two to spread over 2 half hrs
                    rain = days_data.rain[hod] * self.MM_S_TO_MM_HR / 2.0
                    par = days_data.sw[hod] * self.SW_2_PAR
                    if par < 0.0:
                        par = 0.0
                    tair = days_data.tair[hod] - self.K_to_C
                    tsoil = np.mean(days_data.tair) - self.K_to_C
                    qair = days_data.qair[hod]

                    # co2 -> [ppm] Daily mean value
                    if vary_co2:
                        co2 = days_data.co2[hod]
                    else:
                        co2 = co2_data

                    # g/m2/30 min -> t/ha/30min
                    if vary_ndep:
                        ndep = ndep_data[hod]
                        ndep *= G_M2_to_TONNES_HA
                    else:
                        ndep = ndep_data

                    wind = days_data.wind[hod]
                    press = days_data.press[hod] * self.PA_TO_KPA
                    vpd = self.qair_to_vpd(qair, tair, press)
                    if vpd < 0.05:
                        vpd = 0.05

                    wr.writerow([yr, doy, hod, rain, par, tair, tsoil, vpd, \
                                 co2, ndep, wind, press])

        ofp.close()

    def write_met_file(self, df):

        yr_sequence = np.unique(df.year)
        start_sim = yr_sequence[0]
        end_sim = yr_sequence[-1]
        year = str(start_sim)

        try:
            ofp = open(self.forcing_ofname, 'wb')
            wr = csv.writer(ofp, delimiter=',', quoting=csv.QUOTE_NONE,
                            escapechar=None, dialect='excel')
            wr.writerow(['# %s 30-min met forcing' % (self.site)])
            wr.writerow(['# Data from %s-%s' % (start_sim, end_sim)])
            wr.writerow(['# Created by Martin De Kauwe: %s' % date.today()])
            wr.writerow([var for i, var in enumerate(self.ounits)])
            wr.writerow([var for i, var in enumerate(self.ovar_names)])

        except IOError:
            raise IOError('Could not write met file: "%s"' % \
                            self.forcing_ofname)

        for i, yr in enumerate(yr_sequence):
            days = np.unique(df[df.year == yr].doy)
            for j, doy in enumerate(days):
                days_data = df[(df.year == yr) & (df.doy == doy)]

                # otherwise we can't index 0-47 for HOD
                days_data = days_data.reset_index()
                for hod in xrange(len(days_data)):

                    # mm/sec -> mm/30 min
                    # need to split the rain in two to spread over 2 half hrs
                    rain = days_data.rain[hod] * self.MM_S_TO_MM_HR / 2.0
                    par = days_data.sw[hod] * self.SW_2_PAR
                    if par < 0.0:
                        par = 0.0
                    tair = days_data.tair[hod] - self.K_to_C
                    tsoil = np.mean(days_data.tair) - self.K_to_C
                    qair = days_data.qair[hod]
                    co2 = days_data.co2[hod]
                    ndep = -999.9

                    wind = days_data.wind[hod]
                    press = days_data.press[hod] * self.PA_TO_KPA
                    vpd = self.qair_to_vpd(qair, tair, press)
                    if vpd < 0.05:
                        vpd = 0.05

                    # Need to write two rows as the data is hourly and we want
                    # 30 min
                    wr.writerow([yr, doy, hod*2., rain, par, tair, tsoil, vpd, \
                                 co2, ndep, wind, press])
                    wr.writerow([yr, doy, hod*2.+0.5, rain, par, tair, tsoil, \
                                 vpd, co2, ndep, wind, press])

        ofp.close()

    def read_nc_file(self, fname):
        """ Build a DF from the netcdf outputs """

        f = nc.Dataset(fname)
        #times = f.variables['time']
        #t = nc.num2date(times[:], times.units)
        #for i in t:
        #    print i
        #sys.exit()

        times = f.variables['time']
        date_time = nc.num2date(times[:], times.units)
        # Round time units, so that we don't introduce any weirdness due to
        # floats
        date_time = [self.round_minutes(i) for i in date_time]

        self.lat = f.variables['latitude'][0,0]
        self.lon = f.variables['longitude'][0,0]

        df = pd.DataFrame(f.variables['surface_downwelling_shortwave_flux_in_air'][:,0,0], columns=['sw']) # W/m^2
        df['tair'] = f.variables['air_temperature'][:,0,0]              # deg K
        df['rain'] = f.variables['precipitation_flux'][:,0,0]           # mm/s
        df['qair'] = f.variables['specific_humidity'][:,0,0]            # kg/kg
        df['wind'] = f.variables['wind_speed'][:,0,0]                   # m/s
        df['press'] = f.variables['air_pressure'][:,0,0]                # Pa
        #df['co2'] = f.variables['CO2air'][:,0,0]                       # ppmv

        try:
            df['co2'] = f.variables['mole_fraction_of_carbon_dioxide_in_air'][:,0,0]
        except:
            df['co2'] = np.ones(len(df['tair'])) * 400.0

        # adding correct datetime information
        df['dates'] = date_time
        df = df.set_index('dates')
        df['year'] = df.index.year
        df['doy'] = df.index.dayofyear

        return df

    def get_random_year_sequence(self, start_yr, end_yr, out_yrs,
                                 preserve_leap=False):

        # Set the seed so we can repeat this if required
        np.random.seed(42)
        yrs = np.arange(start_yr, end_yr+1)

        if preserve_leap:

            # preserve leap yrs, so find them first
            leapyrs = np.zeros(0)
            for yr in yrs:
                if calendar.isleap(yr):
                    leapyrs = np.append(leapyrs, yr)


            # However we don't want the leapyrs in the sequence, so exclude them
            yrs = np.array([yrs[i] for i, yr in enumerate(yrs) \
                                    if yr not in leapyrs])

            shuff_years = self.randomise_array(out_yrs, yrs)
            shuff_years_leap = self.randomise_array(out_yrs, leapyrs)

            sequence = []
            i = 0
            for yr in np.arange(start_yr, end_yr+1):

                if i == 0:
                    prev_yr_leap = 1666 # anything not in the sequence
                    prev_yr = 1666 # anything not in the sequence

                if calendar.isleap(yr):
                    out_yr = shuff_years_leap[i]

                    # Make sure we don't get the same year twice
                    while prev_yr_leap == int(out_yr):
                        i += 1
                        out_yr = shuff_years_leap[i]

                    sequence.append(out_yr)
                    prev_yr_leap = shuff_years_leap[i]
                else:
                    out_yr = shuff_years[i]

                    # Make sure we don't get the same year twice
                    while prev_yr == int(out_yr):
                        i += 1
                        out_yr = shuff_years[i]

                    sequence.append(out_yr)
                    prev_yr = shuff_years[i]

                i += 1
        else:
            yrs = np.array([yrs[i] for i, yr in enumerate(yrs)])
            shuff_years = self.randomise_array(out_yrs, yrs)
            yr_list = np.random.uniform(start_yr, end_yr,
                                        out_yrs).astype(np.int)
            sequence = []
            i = 0
            for yr in yr_list:

                if i == 0:
                    prev_yr = 1666 # anything not in the sequence

                out_yr = shuff_years[i]

                # Make sure we don't get the same year twice
                while prev_yr == int(out_yr):
                    i += 1
                    out_yr = shuff_years[i]

                sequence.append(out_yr)
                prev_yr = shuff_years[i]

                i += 1

        return sequence

    def randomise_array(self, out_yrs, yrs):
        # make a sequence longer than the number of years we actually want
        num_seq = np.ones(out_yrs * len(yrs))
        num_years = len(yrs) * len(num_seq)
        shuff_years = (yrs * num_seq[:,None]).reshape(num_years)
        np.random.shuffle(shuff_years)

        return shuff_years

    def qair_to_vpd(self, qair, tair, press):

        # convert back to Pa
        press /= self.PA_TO_KPA

        # saturation vapor pressure
        es = 100.0 * 6.112 * np.exp((17.67 * tair) / (243.5 + tair))

        # vapor pressure
        ea = (qair * press) / (0.622 + (1.0 - 0.622) * qair)

        vpd = (es - ea) * self.PA_TO_KPA

        return vpd

    def round_minutes(self, t): # t is a datetime object
        return (t - dt.timedelta(minutes = t.minute - round(t.minute, -1),
                seconds = t.second, microseconds = t.microsecond))



if __name__ == "__main__":

    site = "US-NR1"
    fpath = "/fs/data2/tonygard/testgday/testmet2model.gday/Ameriflux_CF_gapfill_site_0-772" % (os.getlogin())
    outfile_tag = "gdaytestmet"
    r_interface_wrapper(site, outfile_tag, fpath)
