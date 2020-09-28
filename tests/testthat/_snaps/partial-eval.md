# across() translated to individual components

    Code
      lf %>% summarise(across(everything(), "log"))
    Output
      <SQL>
      SELECT LN(`a`) AS `a`, LN(`b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(everything(), log))
    Output
      <SQL>
      SELECT LN(`a`) AS `a`, LN(`b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(everything(), list(log)))
    Output
      <SQL>
      SELECT LN(`a`) AS `a`, LN(`b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(everything(), "log", base = 2))
    Output
      <SQL>
      SELECT LOG(2.0, `a`) AS `a`, LOG(2.0, `b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(everything(), c("log", "exp")))
    Output
      <SQL>
      SELECT LN(`a`) AS `a_log`, EXP(`a`) AS `a_exp`, LN(`b`) AS `b_log`, EXP(`b`) AS `b_exp`
      FROM `df`

---

    Code
      lf %>% summarise(across(everything(), c("log", "exp"), .names = "{.fn}_{.col}"))
    Output
      <SQL>
      SELECT LN(`a`) AS `log_a`, EXP(`a`) AS `exp_a`, LN(`b`) AS `log_b`, EXP(`b`) AS `exp_b`
      FROM `df`

