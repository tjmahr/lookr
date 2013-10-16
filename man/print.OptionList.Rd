\name{print.OptionList}
\alias{print.OptionList}
\title{Print an option list}
\usage{
  \method{print}{OptionList} (option_list, ...)
}
\arguments{
  \item{...}{parameters passed to the Option List's
  \code{$get(...)} internal method. If blank, get
  everything. If a specific slot is named, get that. If
  \code{default = TRUE}, get the defaults.}
}
\description{
  This function just wraps a call to \code{$get(...)}.
  \code{print(lwl_opts)} gives the same results as
  \code{lwl_opts$get()}
}

