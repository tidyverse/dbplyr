# up-direction works

    Code
      df_lazy_ns %>% window_order(id) %>% tidyr::fill(n1, .direction = "up")
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        MAX(`n1`) OVER (PARTITION BY `..dbplyr_partition_1`) AS `n1`
      FROM (
        SELECT
          `df`.*,
          SUM(CASE WHEN ((`n1` IS NULL)) THEN 0 ELSE 1 END) OVER (ORDER BY `id` DESC ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partition_1`
        FROM `df`
      ) AS `q01`

---

    Code
      df_lazy_std %>% window_order(id) %>% tidyr::fill(n1, .direction = "up")
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        LAST_VALUE(`n1` IGNORE NULLS) OVER (ORDER BY `id` DESC ROWS UNBOUNDED PRECEDING) AS `n1`
      FROM `df`

---

    Code
      df_lazy_std %>% window_order(id) %>% tidyr::fill(n1, .direction = "updown")
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        LAST_VALUE(`n1` IGNORE NULLS) OVER (ORDER BY `id` ROWS UNBOUNDED PRECEDING) AS `n1`
      FROM (
        SELECT
          `id`,
          `group`,
          LAST_VALUE(`n1` IGNORE NULLS) OVER (ORDER BY `id` DESC ROWS UNBOUNDED PRECEDING) AS `n1`
        FROM `df`
      ) AS `q01`

---

    Code
      df_lazy_std %>% window_order(id) %>% tidyr::fill(n1, .direction = "downup")
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        LAST_VALUE(`n1` IGNORE NULLS) OVER (ORDER BY `id` DESC ROWS UNBOUNDED PRECEDING) AS `n1`
      FROM (
        SELECT
          `id`,
          `group`,
          LAST_VALUE(`n1` IGNORE NULLS) OVER (ORDER BY `id` ROWS UNBOUNDED PRECEDING) AS `n1`
        FROM `df`
      ) AS `q01`

# up-direction works with descending

    Code
      df_lazy_ns %>% window_order(desc(id)) %>% tidyr::fill(n1, .direction = "up")
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        MAX(`n1`) OVER (PARTITION BY `..dbplyr_partition_1`) AS `n1`
      FROM (
        SELECT
          `df`.*,
          SUM(CASE WHEN ((`n1` IS NULL)) THEN 0 ELSE 1 END) OVER (ORDER BY `id` ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partition_1`
        FROM `df`
      ) AS `q01`

---

    Code
      df_lazy_std %>% window_order(desc(id)) %>% tidyr::fill(n1, .direction = "up")
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        LAST_VALUE(`n1` IGNORE NULLS) OVER (ORDER BY `id` ROWS UNBOUNDED PRECEDING) AS `n1`
      FROM `df`

# groups are respected

    Code
      group_by(df_lazy_ns, group) %>% window_order(id) %>% tidyr::fill(n1)
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        MAX(`n1`) OVER (PARTITION BY `group`, `..dbplyr_partition_1`) AS `n1`
      FROM (
        SELECT
          `df`.*,
          SUM(CASE WHEN ((`n1` IS NULL)) THEN 0 ELSE 1 END) OVER (PARTITION BY `group` ORDER BY `id` ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partition_1`
        FROM `df`
      ) AS `q01`

---

    Code
      group_by(df_lazy_std, group) %>% window_order(id) %>% tidyr::fill(n1)
    Output
      <SQL>
      SELECT
        `id`,
        `group`,
        LAST_VALUE(`n1` IGNORE NULLS) OVER (PARTITION BY `group` ORDER BY `id` ROWS UNBOUNDED PRECEDING) AS `n1`
      FROM `df`

# fill errors on unsorted data

    Code
      (expect_error(df_db %>% tidyr::fill(n1)))
    Output
      <error/rlang_error>
      Error in `tidyr::fill()`:
      x `.data` does not have explicit order.
      i Please use `dbplyr::window_order()` to make order explicit.

# fill() produces nice error messages

    Code
      lazy_frame(x = 1) %>% tidyr::fill(non_existent)
    Condition
      Error in `tidyr::fill()`:
      ! Can't select columns that don't exist.
      x Column `non_existent` doesn't exist.

