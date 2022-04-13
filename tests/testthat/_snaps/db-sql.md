# 2nd edition uses sql methods

    Code
      expect_error(dbplyr_analyze(con), "db_method")
    Condition
      Warning:
      <Test> uses an old dbplyr interface
      i Please install a newer version of the package or contact the maintainer
      This warning is displayed once every 8 hours.

