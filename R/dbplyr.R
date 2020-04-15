#' @importFrom assertthat assert_that
#' @importFrom assertthat is.flag
#' @importFrom stats setNames update
#' @importFrom utils head tail
#' @importFrom glue glue
#' @importFrom methods setOldClass
#' @importFrom dplyr n
#' @import rlang
#' @import DBI
#' @import tibble
#' @keywords internal
"_PACKAGE"

# Generics that really should live in dbplyr
#' @importFrom dplyr db_analyze
#' @importFrom dplyr db_begin
#' @importFrom dplyr db_commit
#' @importFrom dplyr db_create_index
#' @importFrom dplyr db_create_indexes
#' @importFrom dplyr db_create_table
#' @importFrom dplyr db_data_type
#' @importFrom dplyr db_desc
#' @importFrom dplyr db_drop_table
#' @importFrom dplyr db_explain
#' @importFrom dplyr db_has_table
#' @importFrom dplyr db_insert_into
#' @importFrom dplyr db_list_tables
#' @importFrom dplyr db_query_fields
#' @importFrom dplyr db_query_rows
#' @importFrom dplyr db_rollback
#' @importFrom dplyr db_save_query
#' @importFrom dplyr db_write_table
#' @importFrom dplyr sql_escape_ident
#' @importFrom dplyr sql_escape_string
#' @importFrom dplyr sql_join
#' @importFrom dplyr sql_select
#' @importFrom dplyr sql_semi_join
#' @importFrom dplyr sql_set_op
#' @importFrom dplyr sql_subquery
#' @importFrom dplyr sql_translate_env
NULL
