% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic_gg_barplot.R
\name{generic_barplot}
\alias{generic_barplot}
\title{Create a generic barplot using ggplot.}
\usage{
generic_barplot(
  data,
  group_by,
  values_in = NULL,
  fill_by = NULL,
  percentaged = FALSE
)
}
\arguments{
\item{data}{A data frame containing the data for plotting.}

\item{group_by}{The name of the column in `data` that contains the group 
labels. Each unique value in this column corresponds to a distinct group, 
and a bar will be generated for each group. The column must be of type 
character or factor.}

\item{values_in}{(Optional) The name of the column in `data` that contains
the values used to calculate the bar heights. If not provided, the bar
heights will be calculated based on the counts, i.e., the number of rows in
`data` that belong to each group as defined by the values in column
`group_by`.}

\item{fill_by}{(Optional) The name of the column in `data` that defines
sub-groups. Each sub-group will be represented by a different fill colour,
and the bars will consist of stacked, filled rectangles. This parameter is
useful when visualising sub-groups within each group.}

\item{percentaged}{If set to `TRUE`, all bars will be stretched to extend to
the full height of the plot. This option is only applicable when `fill_by`
is specified where each bar is represented by a stack of rectangles each of
which is filled in a different colour. The heights of the rectangles will
then reflect the proportions of each sub-group (count or sum of
corresponding rows or values in the `values_in` column) within a group. The
default value is `FALSE`.}
}
\value{
A barplot visualising the data using ggplot.
}
\description{
This function generates a barplot using ggplot, providing flexibility in 
customising the plot according to your data. The barplot can represent 
different groups and sub-groups, with the option to calculate bar heights 
based on counts or specific values.
}
\details{
Note: This description was written by the help of ChatGPT.
}
\examples{
# Basic usage
my_data <- data.frame(
 group = c("A", "A", "A", "B", "B", "C", "C"),
 value = c(10, 15, 3, 8, 12, 5, 20),
 subgroup = c("X", "Y", "X", "X", "Y", "X", "Y")
)

generic_barplot(
  data = my_data, 
  group_by = "group", 
  values_in = "value", 
  fill_by = "subgroup"
)

# Percentage-based bar heights
generic_barplot(
  data = my_data, 
  group_by = "group", 
  values_in = "value", 
  fill_by = "subgroup", 
  percentaged = TRUE
)
}