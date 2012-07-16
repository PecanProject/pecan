#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#==========================================================================================#
#==========================================================================================#
# Time conversion units                                                                    #
#------------------------------------------------------------------------------------------#
yr.day  <<- 365.2425         # # of days in a year                              [   day/yr]
day.sec <<- 86400.           # # of seconds in a day                            [    s/day]
day.min <<- 1440.            # # of minutes in a day                            [  min/day]
day.hr  <<- 24.              # # of hours in a day                              [   hr/day]
hr.sec  <<- 3600.            # # of seconds in an hour                          [     s/hr]
hr.min  <<- 60.              # # of minutes in an hour                          [   min/hr]
min.sec <<- 60.              # # of seconds in a minute                         [    s/min]
yr.sec  <<- yr.day * day.sec # # of seconds in a year                           [     s/yr]
#==========================================================================================#