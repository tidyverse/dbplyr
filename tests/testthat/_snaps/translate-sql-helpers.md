# output of print method for sql_variant is correct

    Code
      sql_variant(sim_trans, sim_trans, sim_trans)
    Output
      <sql_variant>
      scalar:    +
      aggregate: +
      window:    +

# win_rank() is accepted by the sql_translator

    Code
      sql_variant(sql_translator(test = win_rank("test")))
    Output
      <sql_variant>
      scalar:    test

