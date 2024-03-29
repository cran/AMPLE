% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hcr_funcs.R
\name{constant}
\alias{constant}
\title{Evaluates a constant harvest control rule}
\usage{
constant(mp_params, ...)
}
\arguments{
\item{mp_params}{The HCR / management procedure parameters used to evaluate the HCR (as a list).}

\item{...}{Unused}
}
\description{
Evaluates a constant harvest control rule, i.e. one that ignores the stock status and just returns the constant level (catch or effort).
Used by the \code{hcr_op} function.
}
