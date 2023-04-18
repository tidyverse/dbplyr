#' Build a SQL string.
#'
#' This is a convenience function that should prevent sql injection attacks
#' (which in the context of dplyr are most likely to be accidental not
#' deliberate) by automatically escaping all expressions in the input, while
#' treating bare strings as sql. This is unlikely to prevent any serious
#' attack, but should make it unlikely that you produce invalid sql.
#'
#' This function should be used only when generating `SELECT` clauses,
#' other high level queries, or for other syntax that has no R equivalent.
#' For individual function translations, prefer [sql_expr()].
#'
#' @param ... input to convert to SQL. Use [sql()] to preserve
#'   user input as is (dangerous), and [ident()] to label user
#'   input as sql identifiers (safe)
#' @param .env the environment in which to evaluate the arguments. Should not
#'   be needed in typical use.
#' @param con database connection; used to select correct quoting characters.
#' @keywords internal
#' @export
#' @examples
#' con <- simulate_dbi()
#' build_sql("SELECT * FROM TABLE", con = con)
#' x <- "TABLE"
#' build_sql("SELECT * FROM ", x, con = con)
#' build_sql("SELECT * FROM ", ident(x), con = con)
#' build_sql("SELECT * FROM ", sql(x), con = con)
#'
#' # http://xkcd.com/327/
#' name <- "Robert'); DROP TABLE Students;--"
#' build_sql("INSERT INTO Students (Name) VALUES (", name, ")", con = con)
build_sql <- function(..., .env = parent.frame(), con = sql_current_con()) {
  check_con(con)

  escape_expr <- function(x, con) {
    # If it's a string, leave it as is
    if (is.character(x)) return(x)

    val <- eval_bare(x, .env)
    # Skip nulls, so you can use if statements like in paste
    if (is.null(val)) return("")

    escape(val, con = con)
  }

  pieces <- purrr::map_chr(enexprs(...), escape_expr, con = con)
  sql(paste0(pieces, collapse = ""))
}

glue_sql2 <- function(.con,
                      ...,
                      .sep = "",
                      .envir = parent.frame(),
                      .open = "{",
                      .close = "}",
                      .na = DBI::SQL("NULL"),
                      .null = "",
                      .comment = "#",
                      .literal = FALSE,
                      .trim = TRUE) {
  sql(glue(
    ...,
    .sep = .sep,
    .envir = .envir,
    .open = .open,
    .close = .close,
    .na = .na,
    .null = .null,
    .comment = .comment,
    .literal = .literal,
    .transformer = sql_quote_transformer(.con),
    .trim = .trim
  ))
}

sql_quote_transformer <- function(connection) {
  function(text, envir) {
    collapse_regex <- "[*][[:space:]]*$"
    should_collapse <- grepl(collapse_regex, text)
    if (should_collapse) {
      text <- sub(collapse_regex, "", text)
    }

    type_regex <- "^\\.(tbl|sql|col|name|from|kw|val) (.*)"
    m <- regexec(type_regex, text)
    is_quoted <- any(m[[1]] != -1)
    if (is_quoted) {
      matches <- regmatches(text, regexec(type_regex, text))[[1]]

      type <- matches[[2]]
      value <- matches[[3]]
    } else {
      value <- text
      type <- "raw"
    }
    value <- eval(parse(text = value, keep.source = FALSE), envir)
    glue_check_collapse(type, should_collapse)

    if (type == "tbl") {
      if (is_bare_character(value)) {
        value <- ident(value)
      }
    } else if (type == "from") {
      # TODO maybe this could call dbplyr_sql_subquery()
      if (is_bare_character(value)) {
        value <- ident(value)
      }
    } else if (type == "col") {
      if (is_bare_character(value)) {
        value <- ident(value)
      }
    } else if (type == "name") {
      if (is_bare_character(value)) {
        value <- ident(value)
      }
    } else if (type == "kw") {
      value <- sql(style_kw(value))
    } else if (type == "val") {
      # keep as is
    }

    if (!type %in% c("sql", "raw")) {
      value <- escape(value, collapse = NULL, parens = FALSE, con = connection)
    }

    if (should_collapse) {
      value <- paste0(unclass(value), collapse = ", ")
    }

    # TODO use `vctrs::vec_check_size(value, size = 1L)`
    if (length(value) != 1) {
      cli_abort("{.arg value} must have size 1, not {length(value)}.")
    }

    unclass(value)
  }
}

glue_check_collapse <- function(type, collapse) {
  # collapse is only allowed for type `col`
  if (type %in% c("col", "val")) {
    return()
  }

  if (collapse) {
    cli_abort("Collapsing is only allowed for {.val col}, not for {.val {type}}.")
  }
}
