pull_rng <- function(rng, var = -1, name = NULL) {
  
  .data <- rng %>% as.data.frame()
  
  #code copied directly from dplyr::pull
  var <- tidyselect::vars_pull(names(.data), !!rlang::enquo(var))
  name <- rlang::enquo(name)
  if (rlang::quo_is_null(name)) {
    return(.data[[var]])
  }
  name <- tidyselect::vars_pull(names(.data), !!name)
  rlang::set_names(.data[[var]], nm = .data[[name]])

}

#' Extract a single column of (meta)data from a Ranges object by name or position
#'
#' @param .data a `Ranges` object
#' @param var One column name, either from the core GRanges object or its metadata. If empty will return the last column in mdata.
#' @param name An optional parameter that specifies the column to be used as names for a named vector. Specified in a similar manner as var.
#' @param ... Not used in this method.
#' @return A vector the same size as the number of ranges in .data.
#' @seealso [dplyr::pull()]
#' @importFrom dplyr pull
#' @examples
#' df <- data.frame(start = 1:10, width = 5,  seqnames = "seq1",
#' strand = sample(c("+", "-", "*"), 10, replace = TRUE), gc = runif(10), counts = rpois(10, 2))
#' rng <- as_granges(df)
#' pull(rng, gc)
#' pull(rng, counts)
#' pull(rng, start)

#' @rdname ranges-pull
#' @method pull Ranges
#' @export
pull.Ranges <- function(.data, var = -1, name = NULL) {
  pull_rng(rng = .data, var = {{ var }}, name = {{ name }} )
}

#' @method pull DelegatingGenomicRanges
#' @export
pull.DelegatingGenomicRanges <- function(.data, var = -1, name = NULL, ...) {
  pull_rng(.data@delegate, var = var, name = name)

}

#' @method pull DelegatingIntegerRanges
#' @export
pull.DelegatingIntegerRanges <- pull.DelegatingGenomicRanges