% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_sleeplog.R
\name{convert_sleeplog}
\alias{convert_sleeplog}
\title{convert_sleeplog}
\usage{
convert_sleeplog(sleeplog = c(), part2resultsfile = c(), sep = ",")
}
\arguments{
\item{sleeplog}{path to sleeplog (diary) file kept by the study (xlsx) assumed to have a column BEGINDAT with the date of recording and at least a column bed1 to indicate the time of going to bed on night 1.}

\item{part2resultsfile}{Path to GGIR part 2 person level summary file (csv).}

\item{sep}{separator used by csv files stored by GGIR}
}
\value{
no object is returned, only a new file is created in the same folder as the activitylog
}
\description{
convert_sleeplog
}
