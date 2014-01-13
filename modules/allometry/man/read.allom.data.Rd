\name{read.allom.data}
\alias{read.allom.data}
\title{read.allom.data}
\usage{
  read.allom.data(pft.data, component, field, parm,
    nsim = 10000)
}
\arguments{
  \item{pft.data}{PFT dataframe \itemize{
  \item{acronym}{USDA species acronym, used with FIELD
  data} \item{spcd}{USFS species code, use with TALLY data}
  }}

  \item{component}{allometry ID, Jenkins table 5}

  \item{field}{raw field data}

  \item{parm}{allometry equation file, Jenkins table 3}
}
\value{
  \item{name field} \item{name parm}
}
\description{
  read.allom.data
}

