\name{plot.variance.decomposition}
\alias{plot.variance.decomposition}
\title{Variance Decomposition Plots}
\usage{
  Simple usage:
    plot.variance.decomposition(sensitivity.results$variance.decomposition.output)

  Multiple variance decompositions with differing colors and shapes:
    plot.variance.decomposition(prior.results$variance.decomposition.output,
                                literature.results$variance.decomposition.output,
                                field.results$variance.decomposition.output,
                                assimilation.results$variance.decomposition.output,
                                col=c('grey', 'blue', 'green', 'red'),
                                pch=c(1, 1, 16, 16),
                                order.plot.input = field.results$variance.decomposition.output, 
                                ticks.plot.input = field.results$variance.decomposition.output)

  Advanced presentation options:
    plot.variance.decomposition(sensitivity.results$variance.decomposition.output, 
                                exclude=c('plant_min_temp'), 
                                convert.var=sqrt, var.label='Std Deviation (Mg/ha)',
                                main='Graminoid PFT',
                                col='purple', pch=16)
}
\arguments{
  \item{...}{Output from any number of sensitivity analyses. Output must be of the form given by sensitivity.results$variance.decomposition.output in model output}

  \item{all.plot.inputs}{Optional argument allowing output from sensitivity analyses to be specified in a list}

  \item{exclude}{vector of strings specifying parameters to omit from the variance decomposition graph}

  \item{convert.var}{function transforming variances to the value displayed in the graph}

  \item{var.label}{label to displayed over variance column}

  \item{order.plot.input}{Output from a sensitivity analysis that is to be used to order parameters. Parameters are ordered by variance. Defaults to the first sensitivity analysis output given}

  \item{ticks.plot.input}{Output from a sensitivity analysis that is to be used. Defaults to the first sensitivity analysis output given}

  \item{col}{Color of each sensitivity analysis. Equivalent to col parameter of the plot function.}

  \item{pch}{Shape of each sensitivity analysis. Equivalent to pch parameter of the plot function.}

  \item{main}{Plot title. Useful for multi-pft variance decompositions.}

  \item{fontsize}{list specifying the font size of the titles and axes of the graph}
}
\description{
  Plot results of variance decomposition
}
\details{
  Plots variance decomposition tryptich
}
\author{
  David LeBauer
}

