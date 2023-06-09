% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{This function gives the summary of results per year as a list of years for
each data set imputed into it.}
\usage{
fars_summarize_years(years)
}
\description{
@param years A vector or list containing the years of the files. Must provide the
 years of data that are available.
}
\details{
@inheritParams fars_read_years

 @importFrom dyplyr bind_rows, group_by, summarise

 @importFrom tidyr spread

 @return a dataframe containing the sample size by dataset summarising by year
 and month with years as columns and months as rows

 @examples
 \dontrun{
 fars_summarize_years(c(2013,2014,2015))
 }

 @export
}
