#!/usr/bin/env python
"""
Create a G'DAY spinup & driving met forcing given a series of flux netcdf files.

If (as is most likely) N deposition, N fixation & CO2 information are missing
from the flux files, CO2 is set to 400 ppm and Ndep/Nfix to -999.9. This means
you will need to run the model with the N-cycle off.

If you wish to use the N-cycle you will need to precscribe the N deposition, see
the README on the github for more information.

r_interface_wrapper is the function to interface to R and thus talk to PECAN

NB. this script has been edited to work with Python 3, although I think it
should work fine with python 2.7 as well.

That's all folks.
"""
__author__ = "Martin De Kauwe"
__version__ = "1.0 (03.08.2016)"
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

def r_interface_wrapper(site, outfile_tag, sub_daily=True, tsoil_run_mean=True,
                        fpath=None, file_list=None):
    # Option to supply the path to where the files are in which case the code
    # will attempt to be smart and grab all the files. This could be a problem
    # if there is a gap in the year sequence. So there is also the option to
    # supply the exact years you want, i.e. a smaller subset.
    if file_list is None:
        file_list = glob.glob(os.path.join(fpath, "%s.*.nc" % site))

    C = CreateMetData(site, file_list, outfile_tag, sub_daily, tsoil_run_mean)
    C.main()


class CreateMetData(object):

    def __init__(self, site, file_list, outfile_tag, sub_daily=True,
                 tsoil_run_mean=True):

        self.file_list = sorted(file_list)
        self.site = site
        self.sub_daily = sub_daily
        # 7-day running mean vs. Tair 24 avg
        self.tsoil_run_mean = tsoil_run_mean

        if self.sub_daily:
            self.spinup_ofname = "%s.30min_spin.csv" % (outfile_tag)
            self.forcing_ofname = "%s.30min_run.csv" % (outfile_tag)
            self.ovar_names = ['#year', 'doy', 'hod', 'rain', 'par', 'tair',
                               'tsoil', 'vpd', 'co2', 'ndep', 'nfix', 'wind',
                               'press']
            self.ounits = ['#--', '--', '--', 'mm/30min', 'umol/m2/s','degC',
                           'degC', 'kPa', 'ppm', 't/ha/30min', 't/ha/30min',
                           'm/s','kPa']
        else:
            self.spinup_ofname = "%s.daily_spin.csv" % (outfile_tag)
            self.forcing_ofname = "%s.daily_run.csv" % (outfile_tag)
            self.ovar_names = ['#year', 'doy', 'tair', 'rain', 'tsoil',
                               'tam', 'tpm', 'tmin', 'tmax', 'tday', 'vpd_am',
                               'vpd_pm', 'co2', 'ndep', 'nfix', 'wind', 'pres',
                               'wind_am', 'wind_pm', 'par_am', 'par_pm']
            self.ounits = ['#--', '--', 'degC', 'mm/d', 'degC','degC', 'degC',
                           'degC','degC', 'degC', 'kPa', 'kPa', 'ppm', 't/ha/d',
                           't/ha/d','m/s', 'kPa', 'm/s', 'm/s', 'mj/m2/d',
                           'mj/m2/d']
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
        self.J_TO_UMOL = 4.57
        self.UMOL_TO_J = 1.0 / self.J_TO_UMOL

    def main(self):

        # read all the flux netcdf files into a single dataframe
        frames = []
        for fname in self.file_list:
            df_yr = self.read_nc_file(fname)
            frames.append(df_yr)
        df = pd.concat(frames)

        # Create a GDAY spinup file
        start_yr = df.index.year[0]
        end_yr = df.index.year[-1]
        ndep = -999.9
        nfix = -999.9
        co2 = 285.0
        num_yrs = 20
        yr_sequence = self.get_random_year_sequence(start_yr, end_yr, num_yrs,
                                                    preserve_leap=False)
        self.write_spinup_file(df, yr_sequence, vary_co2=False,
                               co2_data=co2, vary_ndep=False,
                               ndep_data=ndep, nfix=nfix)

        # Create a GDAY forcing file
        all_years = np.arange(start_yr, end_yr+1)
        self.write_met_file(df, all_years, vary_co2=True, co2_data=None,
                            vary_ndep=False, ndep_data=ndep, nfix=nfix)

    def write_spinup_file(self, df, yr_sequence, vary_co2=False, co2_data=None,
                          vary_ndep=False, ndep_data=None, nfix=None):

        start_sim = yr_sequence[0]
        end_sim = yr_sequence[-1]
        year = str(start_sim)
        (ofp, wr) = self.write_hdr(yr_sequence, spinup=True)
        if self.sub_daily:
            self.write_30min_data(df, yr_sequence, ofp, wr, vary_co2, co2_data,
                                  vary_ndep, ndep_data, nfix)
        else:
            self.write_daily_data(df, yr_sequence, ofp, wr, vary_co2, co2_data,
                                  vary_ndep, ndep_data, nfix)

    def write_met_file(self, df, yr_sequence, vary_co2=False, co2_data=None,
                       vary_ndep=False, ndep_data=None, nfix=None):

        start_sim = yr_sequence[0]
        end_sim = yr_sequence[-1]
        year = str(start_sim)
        (ofp, wr) = self.write_hdr(yr_sequence, spinup=False)
        if self.sub_daily:
            self.write_30min_data(df, yr_sequence, ofp, wr, vary_co2, co2_data,
                                  vary_ndep, ndep_data, nfix)
        else:
            self.write_daily_data(df, yr_sequence, ofp, wr, vary_co2, co2_data,
                                  vary_ndep, ndep_data, nfix)

    def write_hdr(self, yr_sequence, spinup=True):
        start_sim = yr_sequence[0]
        end_sim = yr_sequence[-1]
        year = str(start_sim)

        if spinup:
            tag = "spinup"
            ofname = self.spinup_ofname
        else:
            tag = "forcing"
            ofname = self.forcing_ofname

        try:
            ofp = open(ofname, 'w')
            wr = csv.writer(ofp, delimiter=',', quoting=csv.QUOTE_NONE,
                            escapechar=None, dialect='excel')
            wr.writerow(['# %s daily met %s' % (self.site, tag)])
            wr.writerow(['# Data from %s-%s' % (start_sim, end_sim)])
            wr.writerow(['# Created by Martin De Kauwe: %s' % date.today()])
            wr.writerow([var for i, var in enumerate(self.ounits)])
            wr.writerow([var for i, var in enumerate(self.ovar_names)])
        except IOError:
            raise IOError('Could not write met file: "%s"' % ofname)

        return (ofp, wr)

    def generate_soil_temp_data(self, yr_sequence, df):
        # There is no Tsoil data, so we are going to use the day average of air
        # temperature and a 7-day running mean to remove some of the Tair=Tsoil
        tsoil = []
        dates = []
        for i, yr in enumerate(yr_sequence):
            days = np.unique(df[df.year == yr].doy)
            for j, doy in enumerate(days):
                days_data = df[(df.year == yr) & (df.doy == doy)]
                tsoil.append( np.mean(days_data["tair"]-self.K_to_C) )

        # for the spinup stuff the years not being in order will mess the dates
        # up, so make up some random date series. It doesn't really matter as
        # we aren't using the actual date for anything other than to interface
        # with the pandas lib
        st = dt.datetime.strptime("01/01/80 00:00:00", '%d/%m/%y %H:%M:%S')
        nintervals = len(tsoil)
        dates = pd.date_range(st, periods=nintervals, freq='D')

        D = pd.Series(tsoil, dates)
        window_size = 7
        d_mva = D.rolling(window_size).mean()

        # The first few values will be nans, so we will use the 24-hr tair
        # values as replacements here
        for i in range(window_size-1):
            d_mva[i] = tsoil[i]
        tsoil_data = d_mva.values

        return (tsoil_data)

    def write_30min_data(self, df, yr_sequence, ofp, wr, vary_co2, co2_data,
                          vary_ndep, ndep_data, nfix):

        if self.tsoil_run_mean:
            tsoil_data = self.generate_soil_temp_data(yr_sequence, df)

        cnt = 0
        for i, yr in enumerate(yr_sequence):
            days = np.unique(df[df.year == yr].doy)
            for j, doy in enumerate(days):
                days_data = df[(df.year == yr) & (df.doy == doy)]


                # otherwise we can't index 0-47 for HOD
                days_data = days_data.reset_index()
                for hod in range(len(days_data)):
                    # mm/sec -> mm/30 min
                    # need to split the rain in two to spread over 2 half hrs
                    rain = days_data.rain[hod] * self.MM_S_TO_MM_HR / 2.0
                    if days_data.par[hod] < 0.0:
                        par = 0.0
                    else:
                        par = days_data.par[hod]

                    tair = days_data.tair[hod] - self.K_to_C
                    if self.tsoil_run_mean:
                        tsoil = tsoil_data[cnt]
                    else:
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
                    if wind <= 0.0:
                        wind = 0.1 # set v.small speed but not zero

                    press = days_data.press[hod] * self.PA_TO_KPA
                    vpd = self.qair_to_vpd(qair, tair, press)
                    if vpd < 0.05:
                        vpd = 0.05

                    wr.writerow([yr, doy, hod, rain, par, tair, tsoil, vpd,\
                                 co2, ndep, nfix, wind, press])
                    cnt += 1

        ofp.close()

    def write_daily_data(self, df, yr_sequence, ofp, wr, vary_co2, co2_data,
                         vary_ndep, ndep_data, nfix):

        if self.tsoil_run_mean:
            tsoil_data = self.generate_soil_temp_data(yr_sequence, df)

        cnt = 0
        for i, yr in enumerate(yr_sequence):
            days = np.unique(df[df.year == yr].doy)
            for j, doy in enumerate(days):
                days_data = df[(df.year == yr) & (df.doy == doy)]
                days_data = days_data.reset_index()

                morning = days_data.iloc[0:24][days_data.par >= 5.0]
                afternoon = days_data.iloc[24:48][days_data.par >= 5.0]
                day_light = days_data[days_data.par >= 5.0]

                tair = np.mean(day_light["tair"]-self.K_to_C)

                tam = np.mean(morning["tair"]-self.K_to_C)
                tpm = np.mean(afternoon["tair"]-self.K_to_C)
                #tsoil = np.mean(days_data["Tair"]-self.K_to_C)
                if self.tsoil_run_mean:
                    tsoil = tsoil_data[cnt]
                else:
                    tsoil = np.mean(days_data.tair) - self.K_to_C

                # daytime min/max temp
                tmin = np.min(days_data["tair"]-self.K_to_C)
                tmax = np.max(days_data["tair"]-self.K_to_C)
                tday = np.mean(days_data["tair"]-self.K_to_C)

                if len(morning) == 0:
                    vpd_am = 0.05
                else:
                    qair_am = np.mean(morning["qair"])
                    press_am = np.mean(morning["press"]) * self.PA_TO_KPA
                    vpd_am = self.qair_to_vpd(qair_am, tam, press_am)
                    if vpd_am < 0.05:
                        vpd_am = 0.05

                if len(afternoon) == 0:
                    vpd_pm = 0.05
                else:
                    qair_pm = np.mean(afternoon["qair"])
                    press_pm = np.mean(afternoon["press"]) * self.PA_TO_KPA
                    vpd_pm = self.qair_to_vpd(qair_pm, tpm, press_pm)
                    if vpd_pm < 0.05:
                        vpd_pm = 0.05


                # convert PAR [umol m-2 s-1] -> mj m-2 30min-1
                conv = self.UMOL_TO_J * self.J_TO_MJ * self.SEC_TO_HFHR
                par_am = np.sum(morning.par * conv)
                par_pm = np.sum(afternoon.par * conv)

                # rain -> mm
                # coversion 1800 seconds to half hours and summed gives day value.
                # Going to use the whole day including the night data
                rain = max(0.0, np.sum(days_data.rain * 1800.))

                # wind speed -> m/s
                wind = np.mean(day_light.wind)
                if wind <= 0.0:
                    wind = 0.1 # set v.small speed but not zero

                # odd occasions when there is no data, so set a very small
                # wind speed.
                wind_am = np.mean(morning.wind)
                if len(morning.wind) == 0:
                    wind_am = 0.1

                wind_pm = np.mean(afternoon.wind)
                if len(afternoon.wind) == 0:
                    wind_pm = 0.1

                # air pressure -> kPa
                press = np.mean(day_light.press * self.PA_TO_KPA)

                # co2 -> [ppm]
                # Need to take the whole day mean for CO2 to get the correct
                # forcing data, not the daylight only data.
                if vary_co2:
                    co2 = np.mean(days_data.co2)
                else:
                    co2 = co2_data

                if vary_ndep:
                    # Need to take the whole day sum for NDEP to get the
                    # correct forcing data, not the daylight only data.
                    ndep = np.sum(days_data.ndep)
                    ndep *= self.G_M2_to_TONNES_HA
                else:
                    ndep = ndep_data

                wr.writerow([yr, doy, tair, rain, tsoil, tam, tpm, tmin, tmax,\
                             tday, vpd_am, vpd_pm, co2, ndep, nfix, wind, \
                             press, wind_am, wind_pm, par_am, par_pm])

                cnt += 1
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

        df = pd.DataFrame(f.variables['surface_downwelling_shortwave_flux_in_air'][:,0,0],
                          columns=['sw']) # W/m^2
        df['par'] = df.sw * self.SW_2_PAR
        df['tair'] = f.variables['air_temperature'][:,0,0]              # deg K
        df['rain'] = f.variables['precipitation_flux'][:,0,0]           # mm/s
        df['qair'] = f.variables['specific_humidity'][:,0,0]            # kg/kg
        df['wind'] = f.variables['wind_speed'][:,0,0]                   # m/s
        df['press'] = f.variables['air_pressure'][:,0,0]                # Pa
        #df['co2'] = f.variables['CO2air'][:,0,0]                       # ppmv

        try:
            df['co2'] = f.variables['mole_fraction_of_carbon_dioxide_in_air'][:,0,0]*1E6
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

    if len(sys.argv) != 6:
        sys.exit()

    site = sys.argv[1]
    fpath = sys.argv[2]
    outfile_tag = sys.argv[3]

    if str(sys.argv[4]).upper() == "FALSE":
        sub_daily = False      # Make 30-min file vs. Day, stick with day for now
    else:
        sub_daily = True      # Make 30-min file vs. Day, stick with day for now
    if str(sys.argv[5]).upper() == "FALSE":
        tsoil_run_mean = False      # Make 30-min file vs. Day, stick with day for now
    else:
        tsoil_run_mean = True      # Make 30-min file vs. Day, stick with day for now

    """
    site = "US-NR1"
    fpath = "met_data"
    outfile_tag = "US-NR1"
    sub_daily = False
    tsoil_run_mean = False # 7-day running mean vs. Tair 24 avg
    """

    r_interface_wrapper(site, outfile_tag, sub_daily, tsoil_run_mean, fpath)
