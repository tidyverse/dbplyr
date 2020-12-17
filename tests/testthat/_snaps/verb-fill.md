# up-direction works

    Code
      dbplyr_fill(df_lazy_ns, n1, order_by = id, .direction = "up")
    Output
      <SQL>
      SELECT `id`, `group`, MAX(`n1`) OVER (PARTITION BY `..dbplyr_partion_1`) AS `n1`
      FROM (SELECT `id`, `group`, `n1`, SUM(CASE WHEN (((`n1`) IS NULL)) THEN (0) WHEN NOT(((`n1`) IS NULL)) THEN (1) END) OVER (ORDER BY -`id` ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partion_1`
      FROM `df`)

---

    Code
      dbplyr_fill(df_lazy_std, n1, order_by = id, .direction = "up")
    Output
      <SQL>
      SELECT `id`, `group`, LAST_VALUE(`n1` IGNORE NULLS) OVER (ORDER BY -`id`) AS `n1`
      FROM `df`

# up-direction works with descending

    Code
      dbplyr_fill(df_lazy_ns, n1, order_by = desc(id), .direction = "up")
    Output
      <SQL>
      SELECT `id`, `group`, MAX(`n1`) OVER (PARTITION BY `..dbplyr_partion_1`) AS `n1`
      FROM (SELECT `id`, `group`, `n1`, SUM(CASE WHEN (((`n1`) IS NULL)) THEN (0) WHEN NOT(((`n1`) IS NULL)) THEN (1) END) OVER (ORDER BY -`id` DESC ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partion_1`
      FROM `df`)

---

    Code
      dbplyr_fill(df_lazy_std, n1, order_by = desc(id), .direction = "up")
    Output
      <SQL>
      SELECT `id`, `group`, LAST_VALUE(`n1` IGNORE NULLS) OVER (ORDER BY -`id` DESC) AS `n1`
      FROM `df`

# groups are respected

    Code
      dbplyr_fill(group_by(df_lazy_ns, group), n1, order_by = id)
    Output
      <SQL>
      SELECT `group`, `id`, MAX(`n1`) OVER (PARTITION BY `group`, `..dbplyr_partion_1`) AS `n1`
      FROM (SELECT `id`, `group`, `n1`, SUM(CASE WHEN (((`n1`) IS NULL)) THEN (0) WHEN NOT(((`n1`) IS NULL)) THEN (1) END) OVER (PARTITION BY `group` ORDER BY `id` ROWS UNBOUNDED PRECEDING) AS `..dbplyr_partion_1`
      FROM `df`)

---

    Code
      dbplyr_fill(group_by(df_lazy_std, group), n1, order_by = id)
    Output
      <SQL>
      SELECT `id`, `group`, LAST_VALUE(`n1` IGNORE NULLS) OVER (PARTITION BY `group` ORDER BY `id`) AS `n1`
      FROM `df`

