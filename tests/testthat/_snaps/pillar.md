# custom header

    Code
      # Number of rows is shown
      x <- filter(memdb_frame(a = 1:3), a > 0)
      setup <- pillar::tbl_format_setup(x)
      tbl_format_header(x, setup)[[1]]
    Output
      [1] "# Source:   SQL [3 x 1]"
    Code
      # Number of rows still can't be shown if above 20
      x <- filter(memdb_frame(a = 1:21), a > 0)
      setup <- pillar::tbl_format_setup(x)
      tbl_format_header(x, setup)[[1]]
    Output
      [1] "# Source:   SQL [?? x 1]"

