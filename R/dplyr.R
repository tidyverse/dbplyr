# called by .onLoad()
forward_old_dplyr_generics <- function(env) {
  defaults <- lapply(dplyr_generics, forward_default, pkg = "dplyr")
  names(defaults) <- dplyr_generics

  for (generic in dplyr_generics) {
    register_s3_method("dbplyr", generic, "default", defaults[[generic]])
  }
}

dplyr_generics <- c(
  "db_analyze",
  "db_begin",
  "db_commit",
  "db_create_index",
  "db_create_indexes",
  "db_create_table",
  "db_data_type",
  "db_desc",
  "db_drop_table",
  "db_explain",
  "db_has_table",
  "db_insert_into",
  "db_list_tables",
  "db_query_fields",
  "db_query_rows",
  "db_rollback",
  "db_save_query",
  "db_write_table",
  "sql_escape_ident",
  "sql_escape_string",
  "sql_join",
  "sql_select",
  "sql_semi_join",
  "sql_set_op",
  "sql_subquery",
  "sql_translate_env"
)


forward_default <- function(pkg, fun) {
  generic_args <- formals(get(fun))

  cross_call <- call2(
    call2("::", sym(pkg), sym(fun)),
    !!!lapply(set_names(names(generic_args)), sym)
  )

  x <- NULL # silence R CMD check note

  body <- expr({
    if (identical(getOption("cross_package_default"), TRUE)) {
      # Previous generics didn't have default method so simulate error message
      stop(
        "No applicable method for `", .Generic, "()` ",
        "applied to an object of class \"", class(x)[[1]], "\"",
        call. = FALSE
      )
    } else {
      old <- options(cross_package_default = TRUE)
      on.exit(options(old), add = TRUE)
      !!cross_call
    }
  })

  new_function(generic_args, body, parent.frame())
}
