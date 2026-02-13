# generates valid sql for all directions

    Code
      tidyr::fill(lf_asc, n1, .direction = "up")
    Output
      <SQL>
      SELECT
        "id",
        "group",
        LAST_VALUE("n1" IGNORE NULLS) OVER (ORDER BY "id" DESC ROWS UNBOUNDED PRECEDING) AS "n1"
      FROM "df"
    Code
      tidyr::fill(lf_desc, n1, .direction = "up")
    Output
      <SQL>
      SELECT
        "id",
        "group",
        LAST_VALUE("n1" IGNORE NULLS) OVER (ORDER BY "id" ROWS UNBOUNDED PRECEDING) AS "n1"
      FROM "df"
    Code
      tidyr::fill(lf_asc, n1, .direction = "updown")
    Output
      <SQL>
      SELECT
        "id",
        "group",
        LAST_VALUE("n1" IGNORE NULLS) OVER (ORDER BY "id" ROWS UNBOUNDED PRECEDING) AS "n1"
      FROM (
        SELECT
          "id",
          "group",
          LAST_VALUE("n1" IGNORE NULLS) OVER (ORDER BY "id" DESC ROWS UNBOUNDED PRECEDING) AS "n1"
        FROM "df"
      ) AS "q01"
    Code
      tidyr::fill(lf_asc, n1, .direction = "downup")
    Output
      <SQL>
      SELECT
        "id",
        "group",
        LAST_VALUE("n1" IGNORE NULLS) OVER (ORDER BY "id" DESC ROWS UNBOUNDED PRECEDING) AS "n1"
      FROM (
        SELECT
          "id",
          "group",
          LAST_VALUE("n1" IGNORE NULLS) OVER (ORDER BY "id" ROWS UNBOUNDED PRECEDING) AS "n1"
        FROM "df"
      ) AS "q01"

# fill() respects grouping

    Code
      tidyr::fill(window_order(group_by(lf, group), id), n1)
    Output
      <SQL>
      SELECT
        "id",
        "group",
        LAST_VALUE("n1" IGNORE NULLS) OVER (PARTITION BY "group" ORDER BY "id" ROWS UNBOUNDED PRECEDING) AS "n1"
      FROM "df"

# can generate variant SQL

    Code
      tidyr::fill(window_order(lf, id), n1)
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        MAX(`n1`) OVER (PARTITION BY `..dbplyr_partition_1`) AS `n1`
      FROM (
        SELECT
          *,
          SUM(CASE WHEN ((`n1` IS NULL)) THEN 0 ELSE 1 END) OVER (ORDER BY `id` ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partition_1`
        FROM `df`
      ) AS `q01`
    Code
      tidyr::fill(window_order(lf, desc(id)), n1)
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        MAX(`n1`) OVER (PARTITION BY `..dbplyr_partition_1`) AS `n1`
      FROM (
        SELECT
          *,
          SUM(CASE WHEN ((`n1` IS NULL)) THEN 0 ELSE 1 END) OVER (ORDER BY `id` DESC ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partition_1`
        FROM `df`
      ) AS `q01`
    Code
      tidyr::fill(window_order(group_by(lf, group), id), n1)
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        MAX(`n1`) OVER (PARTITION BY `group`, `..dbplyr_partition_1`) AS `n1`
      FROM (
        SELECT
          *,
          SUM(CASE WHEN ((`n1` IS NULL)) THEN 0 ELSE 1 END) OVER (PARTITION BY `group` ORDER BY `id` ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partition_1`
        FROM `df`
      ) AS `q01`

# fill errors on unsorted data

    Code
      tidyr::fill(df)
    Condition
      Error in `tidyr::fill()`:
      x `.data` does not have explicit order.
      i Please use `dbplyr::window_order()` to make order explicit.

# fill() errors on attempted rename

    Code
      tidyr::fill(lf, y = x)
    Condition
      Error in `tidyr::fill()`:
      ! Arguments in `...` must be passed by position, not name.
      x Problematic argument:
      * y = x

# fill() produces nice error messages

    Code
      tidyr::fill(lf, non_existent)
    Condition
      Error in `tidyr::fill()`:
      ! Can't select columns that don't exist.
      x Column `non_existent` doesn't exist.

