# nocov start
.onLoad <- function(...) {
  methods::setOldClass(c("ident_q", "ident", "character"), ident_q())
  methods::setOldClass(c("ident", "character"), ident())
  methods::setOldClass(c("sql", "character"), sql())

  base_scalar$`%>%` <- magrittr::`%>%`
}

# Silence R CMD check note:
# ** checking whether the namespace can be loaded with stated dependencies ... NOTE
# Warning in .undefineMethod("initialize", Class, classWhere) :
#   no generic function 'initialize' found
#
# I'm not sure why this is necessary, but I suspect it's due to the use of
# setOldClass onLoad
#' @importFrom methods initialize
NULL

# nocov end
