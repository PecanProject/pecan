
## What is PEcAn

The Predictive Ecosystem Analyzer (PEcAn) (see [http://pecanproject.org](http://pecanproject.org)) is an integrated ecological bioinformatics toolbox [2] which consists of: 1) a scientific workflow system to manage the immense amounts of publicly-available environmental data and 2) a Bayesian data assimilation system to synthesize this information within state-of-the-art ecosystems models. This project is motivated by the fact that many of the most pressing questions about global change are not necessarily limited by the need to collect new data as much as by our ability to synthesize existing data. This project seeks to improve this ability by developing a framework for integrating multiple data sources in a sensible manner.

The output of the data assimilation system will be a regional-scale high-resolution estimate of both the terrestrial carbon cycle and plant biodiversity based on the best available data and with a robust accounting of the uncertainties involved. The workflow system will allow ecosystem modeling to be more reproducible, automated, and transparent in terms of operations applied to data, and thus ultimately more comprehensible to both peers and the public. It will reduce the redundancy of effort among modeling groups, facilitate collaboration, and make models more accessible the rest of the research community.

PEcAn is not itself an ecosystem model, and it can be used to with a variety of different ecosystem models; integrating a model involves writing a wrapper to convert inputs and outputs to and from the standards used by PEcAn. Currently, PEcAn supports the Ecosystem Demography model [1], SIPNET [4], and BioCro [3].

## Installation

Complete instructions on how to install PEcAn can be found in the [WIKI Installation Page](https://github.com/PecanProject/pecan/wiki/Installing-PEcAn). To get PEcAn up and running you will need to have [R](http://www.r-project.org) as well as [PostgreSQL](http://www.postgresql.org) installed. You can also [download a Virtual Machine](http://isda.ncsa.illinois.edu/download/index.php?project=PEcAn&sort=category) which has all the components as well as PEcAn installed. To run this Virtual Machine you will need to have [VirtualBox](http://virtualbox.org) installed

## Demo

We have a [demo instance](http://pecan.ncsa.illinois.edu/pecan) running the current version of PEcAn. At this instance you can perform a run using either ED or SIPNET at any of the predefined sites.

The demo instance only allows for runs at pecan.ncsa.illinois.edu. Once you have setup the run it will run our server, depending on the number of people executing a model and the model selected this can take between a few seconds and a few minutes to finish. Once finished you see the results of the execution and can plot the outputs of the model. Complete examples of a few executions can be found in our online [tutorials](http://pecanproject.github.io/tutorials.html).

## Publications

1. LeBauer, D.S., D. Wang, K. Richter, C. Davidson, & M.C. Dietze. (2013). Facilitating feedbacks between field measurements and ecosystem models. Ecological Monographs. [doi:10.1890/12-0137.1](http://dx.doi.org/10.1890/12-0137.1)
2. Wang, D, D.S. LeBauer, and M.C. Dietze(2013) Predicting yields of short-rotation hybrid poplar (Populus spp.) for the contiguous US through model-data synthesis. Ecological Applications [doi:10.1890/12-0854.1](http://dx.doi.org/10.1890/12-0854.1)
3. Dietze, M.C., D.S LeBauer, R. Kooper (2013) On improving the communication between models and data. Plant, Cell, & Environment [doi:10.1111/pce.12043](http://dx.doi.org/10.1111/pce.12043)

## References

1. Medvigy, D., S. C. Wofsy, J. W. Munger, D. Y. Hollinger, and P. R. Moorcroft. 2009. "Mechanistic scaling of ecosystem function and dynamics in space and time: Ecosystem Demography model version 2". Journal of Geophysical Research 114:121.
2. LeBauer, David; Dietze, Michael; Kooper, Rob; Long, Steven; Mulrooney, Patrick; Rohde, Gareth Scott; Wang, Dan; (2010): Biofuel Ecophysiological Traits and Yields Database (BETYdb); Energy Biosciences Institute, University of Illinois at Urbana-Champaign. [doi:10.13012/J8H41PB9](http://dx.doi.org/10.13012/J8H41PB9)
3. Miguez, F. E., Maughan, M., Bollero, G. A., & Long, S. P. (2012). Modeling spatial and dynamic variation in growth, yield, and yield stability of the bioenergy crops Miscanthus x giganteus and Panicum virgatum across the conterminous United States. GCB Bioenergy.
4. Sacks, W. J., Schimel, D. S., Monson, R. K., & Braswell, B. H. (2005). Model data synthesis of diurnal and seasonal CO2 fluxes at Niwot Ridge, Colorado. Global Change Biology, 12(2), 240-259.

## Acknowledgements

This material is based upon work supported by the Energy Biosciences Institute and the National Science Foundation under Grant No. 1062547 and 1062204. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation. PEcAn is a collaboration among research groups at the Energy Biosciences Institute at the University of Illinois, the Department of Earth And Environment at Boston University, the Image Spatial Data Analysis group at NCSA, and the Department of Atmospheric & Oceanic Sciences at the University Wisconsin-Madison.

BETY-db is a product of the Energy Biosciences Institute at the University of Illinois at Urbana-Champaign. We gratefully acknowledge the great effort of other researchers who generously made their own data available for further study.

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
