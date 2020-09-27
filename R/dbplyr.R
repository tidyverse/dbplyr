#' @importFrom assertthat assert_that
#' @importFrom assertthat is.flag
#' @importFrom stats setNames update
#' @importFrom utils head tail
#' @importFrom glue glue
#' @importFrom dplyr n
#' @importFrom blob is_blob as_blob
#' @import rlang
#' @import DBI
#' @import tibble
#' @keywords internal
"_PACKAGE"

# Generics that really should live in dbplyr
#' @importFrom dplyr db_analyze
#' @importFrom dplyr db_create_index
#' @importFrom dplyr db_desc
#' @importFrom dplyr db_explain
#' @importFrom dplyr db_query_fields
#' @importFrom dplyr db_query_rows
#' @importFrom dplyr db_save_query
#' @importFrom dplyr db_write_table
#' @importFrom dplyr sql_join
#' @importFrom dplyr sql_select
#' @importFrom dplyr sql_semi_join
#' @importFrom dplyr sql_set_op
#' @importFrom dplyr sql_subquery
#' @importFrom dplyr sql_translate_env
NULL
