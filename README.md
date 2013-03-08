Facilitating feedbacks between field measurements and ecosystem models
======================================================================

### Development and Use

See the [PEcAn wiki](https://github.com/PecanProject/pecan/wiki) for documentation.

### Project Overview

The Predictive Ecosystem Analyzer (PEcAn) is an integrated ecological bioinformatics toolbox (LeBauer et al, 2013) which consists of: 1) a scientific workflow system to manage the immense amounts of publicly-available environmental data and 2) a Bayesian data assimilation system to synthesize this information within state-of-the-art ecosystems models. This project is motivated by the fact that many of the most pressing questions about global change are not necessarily limited by the need to collect new data as much as by our ability to synthesize existing data. This project seeks to improve this ability by developing a framework for integrating multiple data sources in a sensible manner.

The output of the data assimilation system will be a regional-scale high-resolution estimate of both the terrestrial carbon cycle and plant biodiversity based on the best available data and with a robust accounting of the uncertainties involved. The workflow system will allow ecosystem modeling to be more reproducible, automated, and transparent in terms of operations applied to data, and thus ultimately more comprehensible to both peers and the public. It will reduce the redundancy of effort among modeling groups, facilitate collaboration, and make models more accessible the rest of the research community.

PEcAn is not itself an ecosystem model, and it can be used to with a variety of different ecosystem models; integrating a model involves writing a wrapper to convert inputs and outputs to and from the standards used by PEcAn. Currently, PEcAn supports the Ecosystem Demography model (ED2, Medvigy et al 2009), SIPNET (Sacks et al., 2005), and BioCro (Miguez et al, 2012).


### Demonstration

This system allows you to experiment and create simulations using PEcAn, ED, SIPNET, BioCro and BETYdb. The first page will allow for selection of a site where the simulation will run at. The BETY database contains data for both the local system as well as data on the illinois ebi servers (ebi-cluster and ebi-forecast). Only those sites with data on the local system can be run. After selecting a site and entering the date range for the simulation, PEcAn will take over and set up and execute the selected model. Once the model is finished it will allow you to create graphs with the results of the simulation as well as download the results. It is also possible to see all past experiments and simulations.

This machine uses the BETY database (LeBauer et al, 2010) for the trait data and the PEcAn analyzer.

For any questions about the research behind this demonstration or about the system itself, please contact Michael Dietze at dietze(at)bu.edu or David LeBauer at dlebauer@illinois.edu

### Acknowledgements

This material is based upon work supported by the Energy Biosciences Institute and the National Science Foundation under Grant No. 1062547 and 1062204. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation. PEcAn is a collaboration among research groups at the Energy Biosciences Institute at the University of Illinois, the Department of Earth And Environment at Boston University, the Image Spatial Data Analysis group at NCSA, and the Department of Atmospheric & Oceanic Sciences at the University Wisconsin-Madison.

BETY-db is a product of the Energy Biosciences Institute at the University of Illinois at Urbana-Champaign. We gratefully acknowledge the great effort of other researchers who generously made their own data available for further study.

### References

*    LeBauer, D., Wang, D., Richter, K., Davidson, C., & Dietze, M. C. (2012). Facilitating feedbacks between field measurements and ecosystem models. Ecological Monographs.
*    Medvigy, D., S. C. Wofsy, J. W. Munger, D. Y. Hollinger, and P. R. Moorcroft. 2009. "Mechanistic scaling of ecosystem function and dynamics in space and time: Ecosystem Demography model version 2". Journal of Geophysical Research 114:121.
*    LeBauer, D., D. Wang, M. Dietze, 2010. "Biofuel Ecophysiological Traits and Yields Database Version 1.0.", Energy Biosciences Institute, Urbana, IL
*    Miguez, F. E., Maughan, M., Bollero, G. A., & Long, S. P. (2012). Modeling spatial and dynamic variation in growth, yield, and yield stability of the bioenergy crops Miscanthus x giganteus and Panicum virgatum across the conterminous United States. GCB Bioenergy.
*    Sacks, W. J., Schimel, D. S., Monson, R. K., & Braswell, B. H. (2005). Modelâ€data synthesis of diurnal and seasonal CO2 fluxes at Niwot Ridge, Colorado. Global Change Biology, 12(2), 240-259.

