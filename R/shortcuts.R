#' Character vectors
#'
#' This is a wrapper for \code{as.character()}.
#' @param x A vector
#' @return The object \code{x} coerced to an object of type "character".
#' @export
#' @examples
#' char(c(1, 2, 3))
char <- function(x){ as.character(x) }

#' Numeric vectors
#'
#' This is a wrapper for \code{as.numeric()}.
#' @param x A vector
#' @return The object \code{x} coerced to an object of type "numeric".
#' @export
#' @examples
#' num(c(1, 2, 3))
num <- function(x){ as.numeric(char(x)) }

#' Standardized variables
#'
#' This function standardizes variables by taking the difference between each value and the mean and dividing by the standard deviation.
#' @param x An object of type numeric, logical, date, date-time, or time interval.
#' @return The object \code{x} coerced to an object of type "numeric".
#' @export
stdz <- function(x){ ( x-mean(x) )/sd(x) }

#' Median
#'
#' This is a wrapper for \code{median(x, na.rm = TRUE)}.
#' @param x An object for which a method has been defined, or a numeric vector containing the values whose median is to be computed.
#' @return Returns a length-one object of the same type as \code{x}, except when x is logical or integer of even length, when the result will be double.
#' @export
medNA <- function(x){ median(x, na.rm=TRUE) }


#' Remove elements of a vector matching a pattern from a string
#'
#' Vectorised over \code{string} but not over \code{pattern}.
#' @param string Input vector. either a character vector or something coercible to one.
#' @param pattern Pattern to look for.
#' @return A vector of the same type as \code{string}, but without any elements in which \code{pattern} was detected.
#' @export
str_delete <- function(string, pattern){string[!str_detect(string, pattern)]}

#' Common column names
#'
#' Get the common column names between two dataframes.
#' @param df1 First input dataframe.
#' @param df2 Second input dataframe.
#' @return A character vector containing all of the column names that \code{df1} and \code{df2} have in common.
#' @export
comNames <- function(df1, df2){intersect(names(df1), names(df2))}

#' Even numbers
#'
#' Determine whether values are odd or even.
#' @param x An object for which a method has been defined, or a numeric vector.
#' @return A logical object with the same length as \code{x} indicating whether each element in x is odd or even
#' @export
even <- function(x){ x %% 2 == 0 }

#' Object size
#'
#' Provides an estimate of the memory that is being used to store an \code{R} object. This is a wrapper for \code{print(object.size(x), units=unit)}.
#' @param x An \code{R} object.
#' @param unit A string containing the units to be used. Defaults to "Mb". See \code{?object.size} for more options.
#' @return An estimate of the size of the object in the units specified by \code{unit}. Estimates are not always accurate, see \code{?object.size} for details.
#' @export
size <- function(x, unit = 'Mb'){ print(object.size(x), units = unit)}

#' NA elements
#'
#' Counts the number of NA values in a vector.
#' @param x An input vector.
#' @return A length-one integer object displaying the number of NAs in \code{x}.
#' @export
nas <- function(x){length(which(is.na(x)))}

#' Variable classes
#'
#' Display the \code{class()} of each variable in a dataframe.
#' @param df An input dataframe.
#' @return A named character vector where names are the column names in \code{df} and values are the corresponding \code{class()} for each column.
#' @export
classes <- function(df){sapply(names(df), function(name){class(df[,name])})}

#' Length of object dimensions
#'
#' A common function to compute the number of elements (in vectors and lists) or the number of vectors in each dimension (dataframes, matrices, and arrays) of common types of \code{R} objects.
#' @param x An input object whose dimensions will be computed.
#' @return Returns a length-\code{n} integer vector where \code{n} is the number of dimensions in \code{x} (e.g. \code{n=1} for atomic vectors and lists, \code{n=2} for matrices and dataframes, and \code{n} may be more than 2 for some arrays). Each element in the output is the number of elements in that dimension of \code{x} (e.g. if \code{n=1}, the number of elements in the list or vector, if \code{n=2}, first the number of rows in the dataframe or matrix, then the number of columns).
#' @export
d <- function(x){
  if(!is.vector(x) & !is.factor(x) & !is.matrix(x) & !is.data.frame(x) & !'Date' %in% class(x)){
    stop('Requires data frame, matrix, list, array, or vector!')
  }
  if(is.vector(x) | is.factor(x) | 'Date' %in% class(x)){
    length(x)
  } else {
    dim(x)
  }
}

#' Change column names of a dataframe to lowercase
#'
#' Allows this operation to be done in a magrittr chain
#' @param df An input dataframe whose column names will be pushed to lowercase
#' @return Returns the input dataframe, but with the names in lower case
#' @export
namesLower <- function(df){
  names(df) <- tolower(names(df))
  return(df)
}

#' Load .rdata files and assign them to an object of your choosing
#'
#' This allows you to read files with `load()` without needing to keep the original object name
#' @param rdata A filepath to a file to be read with `load()`
#' @return An R object or objects read in with `load()`, without the original object names attached
#' @export
assignLoad <- function(rdata){
  load(rdata)
  return(get(ls()[ls() != 'rdata']))
}
