# ungroup() produces nice error messages

    Code
      pull(memdb_frame(x = 1), non_existent)
    Condition
      Error in `pull()`:
      Caused by error:
      ! object 'non_existent' not found
    Code
      pull(memdb_frame(x = 1), "non_existent")
    Condition
      Error in `pull()`:
      ! Can't extract columns that don't exist.
      x Column `non_existent` doesn't exist.
    Code
      pull(memdb_frame(x = 1), 1000)
    Condition
      Error in `pull()`:
      ! Can't extract columns past the end.
      i Location 1000 doesn't exist.
      i There is only 1 column.
    Code
      pull(memdb_frame(x = 1), x, "name_non_existent")
    Condition
      Error in `pull()`:
      ! Can't extract columns that don't exist.
      x Column `name_non_existent` doesn't exist.

