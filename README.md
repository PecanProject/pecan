[![GitHub Actions CI](https://github.com/PecanProject/pecan/workflows/CI/badge.svg)](https://github.com/PecanProject/pecan/actions)
[![Slack](https://img.shields.io/badge/slack-login-green.svg)](https://pecanproject.slack.com/)
[![Slack](https://img.shields.io/badge/slack-join_chat-green.svg)](https://join.slack.com/t/pecanproject/shared_invite/enQtMzkyODUyMjQyNTgzLWEzOTM1ZjhmYWUxNzYwYzkxMWVlODAyZWQwYjliYzA0MDA0MjE4YmMyOTFhMjYyMjYzN2FjODE4N2Y4YWFhZmQ)
[![DOI](https://zenodo.org/badge/4469/PecanProject/pecan.svg)](https://zenodo.org/badge/latestdoi/4469/PecanProject/pecan)

## Our Vision

#### Ecosystem science, policy, and management informed by the best available data and models

## Our Mission

#### Develop and promote accessible tools for reproducible ecosystem modeling and forecasting

## What is PEcAn?

The Predictive Ecosystem Analyzer (PEcAn) (see [pecanproject.org](http://pecanproject.org)) is an integrated ecological bioinformatics toolbox (Dietze et al 2013, LeBauer et al, 2013) that consists of: 1) a scientific workflow system to manage the immense amounts of publicly-available environmental data and 2) a Bayesian data assimilation system to synthesize this information within state-of-the-art ecosystems models. This project is motivated by the fact that many of the most pressing questions about global change are not necessarily limited by the need to collect new data as much as by our ability to synthesize existing data. This project seeks to improve this ability by developing a accessibe framework for integrating multiple data sources in a sensible manner.

The PEcAn workflow system allows ecosystem modeling to be more reproducible, automated, and transparent in terms of operations applied to data, and thus ultimately more comprehensible to both peers and the public. It reduces the redundancy of effort among modeling groups, facilitate collaboration, and makes models more accessible the rest of the research community.

PEcAn is not itself an ecosystem model, and it can be used to with a variety of different ecosystem models; integrating a model involves writing a wrapper to convert inputs and outputs to and from the standards used by PEcAn. Currently, PEcAn supports over a dozen ecosystem models, with more being added all the time (see the _models_ folder for the most up-to-date list)

## Documentation

Consult documentation of the PEcAn Project; either the [latest stable development](https://pecanproject.github.io/pecan-documentation/develop/) branch, the latest [release](https://pecanproject.github.io/pecan-documentation/latest/). Documentation from [earlier releases is here](https://pecanproject.github.io/documentation.html).

## Getting Started

See our ["Tutorials Page"](https://pecanproject.github.io/tutorials.html) that provides self-guided tutorials, links to vignettes, and an overview presentation.

### Installation

Complete instructions on how to install PEcAn can be found in the [documentation here](https://pecanproject.github.io/pecan-documentation/develop/pecan-manual-setup.html). To get PEcAn up and running you can use one of three methods:

1. Run a [Virtual Machine](https://pecanproject.github.io/pecan-documentation/develop/install-vm.html#install-vm). This is recommended for students and new users, and provides a consistent, tested environment for each release.

2. Use [Docker](https://pecanproject.github.io/pecan-documentation/develop/install-docker.html#install-docker). This is recommended, especially for development and production deployment.

3. Install all of the PEcAn R packages on your own Linux or MacOS computer or server. This can be done by [installing from r-universe](https://pecanproject.github.io/pecan-documentation/develop/r-universe.html):

```R
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.all in R
install.packages('PEcAn.all')
```

This, however, may have limited functionality without also installing other components of PEcAn, in particular [BETYdb](https://pecanproject.github.io/pecan-documentation/develop/osinstall.html#install-bety).

### Website

Visit our [webpage](https://pecanproject.github.io) to keep up with latest news, version, and information about the PEcAn Project

#### Web Interface demo

The fastest way to begin modeling ecosystems is through the PEcAn web interface.  
We have a [demo website](http://pecan.ncsa.illinois.edu/pecan/01-introduction.php) that runs the current version of PEcAn. Using this instance you can perform a run using either ED or SIPNET at any of the predefined sites.

The demo instance only allows for runs at pecan.ncsa.illinois.edu. Once you have set up the run it will execute on our server; depending on the number of people executing a model and the model selected this can take between a few seconds and a few minutes to finish. Once it's finished, you see the results of the execution and can plot the outputs of the model. Complete examples of a few executions can be found in our online [tutorials](http://pecanproject.github.io/tutorials.html).

## Publications

* LeBauer, D.S., D. Wang, K. Richter, C. Davidson, and M.C. Dietze (2013). Facilitating feedbacks between field measurements and ecosystem models. Ecological Monographs. [doi:10.1890/12-0137.1](https://doi.org/10.1890/12-0137.1)
* Wang, D, D.S. LeBauer, and M.C. Dietze (2013). Predicting yields of short-rotation hybrid poplar (Populus spp.) for the contiguous US through model-data synthesis. Ecological Applications [doi:10.1890/12-0854.1](https://doi.org/10.1890/12-0854.1)
* Dietze, M.C., D.S LeBauer, and R. Kooper (2013). On improving the communication between models and data. Plant, Cell, & Environment [doi:10.1111/pce.12043](https://doi.org/10.1111/pce.12043)
* Dietze, Michael C., Shawn P. Serbin, Carl Davidson, Ankur R. Desai, Xiaohui Feng, Ryan Kelly, Rob Kooper et al. "A quantitative assessment of a terrestrial biosphere model's data needs across North American biomes." Journal of Geophysical Research: Biogeosciences 119, no. 3 (2014): 286-300.
* Viskari, Toni, Brady Hardiman, Ankur R. Desai, and Michael C. Dietze. "Model-data assimilation of multiple phenological observations to constrain and predict leaf area index." (2015) [doi:10.1890/14-0497.1](https://doi.org/10.1890/14-0497.1)
* Shiklomanov. A, MC Dietze, T Viskari, PA Townsend, SP Serbin. 2016 "Quantifying the influences of spectral resolution on uncertainty in leaf trait estimates through a Bayesian approach to RTM inversion" Remote Sensing of the Environment 183: 226-238
* LeBauer, David, Rob Kooper, Patrick Mulrooney, Scott Rohde, Dan Wang, Stephen P. Long, and Michael C. Dietze. "BETYdb: a yield, trait, and ecosystem service database applied to second‐generation bioenergy feedstock production." GCB Bioenergy (2017).

A extensive list of publications that apply PEcAn or are informed by our work on [Google Scholar](https://scholar.google.com/citations?hl=en&user=HWhxBY4AAAAJ).

## Acknowledgements

The PEcAn project is supported by the National Science Foundation (ABI #1062547, ABI #1458021, DIBBS #1261582, ARC #1023477, EF #1318164, EF #1241894, EF #1241891), NASA Terrestrial Ecosystems, the Energy Biosciences Institute, Department of Energy (ARPA-E awards #DE-AR0000594 and DE-AR0000598), and an Amazon AWS in Education Grant.

Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation, NASA, or other federal agencies. PEcAn is a collaboration among research groups at the Department of Earth And Environment at Boston University, the Carl Woese Institute for Genomic Biology at the University of Illinois, the Image Spatial Data Analysis group at the National Center for Supercomputing Applications, the Department of Atmospheric & Oceanic Sciences at the University Wisconsin-Madison, and the Terrestrial Ecosystem Science & Technology group at Brookhaven National Lab.

BETYdb is a product of the Energy Biosciences Institute at the University of Illinois at Urbana-Champaign. We gratefully acknowledge the great effort of other researchers who generously made their own data available for further study.

## License

University of Illinois/NCSA Open Source License

Copyright (c) 2012, University of Illinois, NCSA.  All rights reserved.

PEcAn project
<www.pecanproject.org>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the  "Software"), to deal with the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimers.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimers in the documentation and/or other materials provided with the distribution.
* Neither the names of University of Illinois, NCSA, nor the names of its contributors may be used to endorse or promote products derived from this Software without specific prior written permission.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON INFRINGEMENT. IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.

## Activities

![Alt](https://repobeats.axiom.co/api/embed/9d39b0af80fbfa979e349a529c05f21bbac9f858.svg "Repobeats analytics image")
