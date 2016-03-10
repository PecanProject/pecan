
## What is PEcAn?

[![Join the chat at https://gitter.im/PecanProject/pecan](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/PecanProject/pecan?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

The Predictive Ecosystem Analyzer (PEcAn) (see [pecanproject.org](http://pecanproject.org)) is an integrated ecological bioinformatics toolbox (LeBauer et al, 2013) that consists of: 1) a scientific workflow system to manage the immense amounts of publicly-available environmental data and 2) a Bayesian data assimilation system to synthesize this information within state-of-the-art ecosystems models. This project is motivated by the fact that many of the most pressing questions about global change are not necessarily limited by the need to collect new data as much as by our ability to synthesize existing data. This project seeks to improve this ability by developing a framework for integrating multiple data sources in a sensible manner.

The output of the data assimilation system will be a regional-scale high-resolution estimate of both the terrestrial carbon cycle and plant biodiversity based on the best available data and with a robust accounting of the uncertainties involved. The workflow system will allow ecosystem modeling to be more reproducible, automated, and transparent in terms of operations applied to data, and thus ultimately more comprehensible to both peers and the public. It will reduce the redundancy of effort among modeling groups, facilitate collaboration, and make models more accessible the rest of the research community.

PEcAn is not itself an ecosystem model, and it can be used to with a variety of different ecosystem models; integrating a model involves writing a wrapper to convert inputs and outputs to and from the standards used by PEcAn. Currently, PEcAn supports the Ecosystem Demography model (Medvigy et al 2009), SIPNET (Sacks et al 2005), and BioCro (Miguez et al 2009).

## Getting Started

See ["Getting Started"](https://pecan.gitbooks.io/pecan-documentation/content/users_guide/basic_users_guide/Getting-started.html) on the PEcAn 

### Installation

Complete instructions on how to install PEcAn can be found in the [Gitbook Installation Page](https://pecan.gitbooks.io/pecan-documentation/content/developers_guide/Installing-PEcAn.html). To get PEcAn up and running you will need to have [R](http://www.r-project.org) as well as [PostgreSQL](http://www.postgresql.org) installed. You can also [download a Virtual Machine](http://opensource.ncsa.illinois.edu/projects/artifacts.php?key=PECAN) which has all the components as well as PEcAn installed. To run this Virtual Machine you will need to have [VirtualBox](http://virtualbox.org) installed

### Website

Visit our [webage](https://pecanproject.github.io) to keep up with latest news, version, and information about the PEcAn Project

#### Web Interface demo
The fastest way to begin modeling ecosystems is through the PEcAn web interface.  
We have a [demo website](http://pecan.ncsa.illinois.edu/pecan) that runs the current version of PEcAn. Using this instance you can perform a run using either ED or SIPNET at any of the predefined sites.

The demo instance only allows for runs at pecan.ncsa.illinois.edu. Once you have set up the run it will execute on our server; depending on the number of people executing a model and the model selected this can take between a few seconds and a few minutes to finish. Once it's finished, you see the results of the execution and can plot the outputs of the model. Complete examples of a few executions can be found in our online [tutorials](http://pecanproject.github.io/tutorials.html).

## Publications

* LeBauer, D.S., D. Wang, K. Richter, C. Davidson, and M.C. Dietze (2013). Facilitating feedbacks between field measurements and ecosystem models. Ecological Monographs. [doi:10.1890/12-0137.1](http://dx.doi.org/10.1890/12-0137.1)
* Wang, D, D.S. LeBauer, and M.C. Dietze (2013). Predicting yields of short-rotation hybrid poplar (Populus spp.) for the contiguous US through model-data synthesis. Ecological Applications [doi:10.1890/12-0854.1](http://dx.doi.org/10.1890/12-0854.1)
* Dietze, M.C., D.S LeBauer, and R. Kooper (2013). On improving the communication between models and data. Plant, Cell, & Environment [doi:10.1111/pce.12043](http://dx.doi.org/10.1111/pce.12043)
* Dietze, Michael C., Shawn P. Serbin, Carl Davidson, Ankur R. Desai, Xiaohui Feng, Ryan Kelly, Rob Kooper et al. "A quantitative assessment of a terrestrial biosphere model's data needs across North American biomes." Journal of Geophysical Research: Biogeosciences 119, no. 3 (2014): 286-300.
* Viskari, Toni, Brady Hardiman, Ankur R. Desai, and Michael C. Dietze. "Model-data assimilation of multiple phenological observations to constrain and predict leaf area index." (2015) [doi:10.1890/14-0497.1](http://dx.doi.org/10.1890/14-0497.1)


## Other References

* Medvigy, D., S. C. Wofsy, J. W. Munger, D. Y. Hollinger, and P. R. Moorcroft (2009). "Mechanistic scaling of ecosystem function and dynamics in space and time: Ecosystem Demography model version 2". Journal of Geophysical Research 114:121.
* Miguez, F. E., Maughan, M., Bollero, G. A., & Long, S. P. (2012). Modeling spatial and dynamic variation in growth, yield, and yield stability of the bioenergy crops Miscanthus x giganteus and Panicum virgatum across the conterminous United States. GCB Bioenergy.
* Sacks, W. J., Schimel, D. S., Monson, R. K., & Braswell, B. H. (2005). Model data synthesis of diurnal and seasonal CO2 fluxes at Niwot Ridge, Colorado. Global Change Biology, 12(2), 240-259.

## Acknowledgements

This material is based upon work supported by the Energy Biosciences Institute, the National Science Foundation under Grants No. 1062547 and 1062204, and the Department of Energy. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation. PEcAn is a collaboration among research groups at the Carl Woese Institute for Genomic Biology at the University of Illinois, the Department of Earth And Environment at Boston University, the Image Spatial Data Analysis group at the National Center for Supercomputing Applications, and the Department of Atmospheric & Oceanic Sciences at the University Wisconsin-Madison.

BETYdb is a product of the Energy Biosciences Institute at the University of Illinois at Urbana-Champaign. We gratefully acknowledge the great effort of other researchers who generously made their own data available for further study.

## License

University of Illinois/NCSA Open Source License

Copyright (c) 2012, University of Illinois, NCSA.  All rights reserved.

PEcAn project
www.pecanproject.org

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the  "Software"), to deal with the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimers.
- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimers in the documentation and/or other materials provided with the distribution.
- Neither the names of University of Illinois, NCSA, nor the names of its contributors may be used to endorse or promote products derived from this Software without specific prior written permission.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON INFRINGEMENT. IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
