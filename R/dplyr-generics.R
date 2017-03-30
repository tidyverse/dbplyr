# These shims are necessary to avoid an R CMD check NOTE due to buggy
# S3 generic lookup. They shouldn't affect user experience because
# dbplyr will not normally be attached.

#' @export
dplyr::union

#' @export
dplyr::intersect

#' @export
dplyr::setdiff

#' @export
dplyr::filter
