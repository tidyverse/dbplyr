#********************************************************************#
#************************* Utility functions ************************#
#********************************************************************#

checkCounters <- function(pool) {
  expect_gte(pool$counters$free, 0)
  expect_gte(pool$counters$taken, 0)
}

checkCounts <- function(pool, free, taken) {
  checkCounters(pool)
  if (!missing(free)) {
    expect_identical(pool$counters$free, free)
  }
  if (!missing(taken)) {
    expect_identical(pool$counters$taken, taken)
  }
}

#********************************************************************#
#**************************** Sample data ***************************#
#********************************************************************#

# first 10 rows of nycflights13::flights (except `time_hour` col)
flights <- tibble::tibble(
  year = c(2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 2013L),
  month = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
  day = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
  dep_time = c(517L, 533L, 542L, 544L, 554L, 554L, 555L, 557L, 557L, 558L),
  sched_dep_time = c(515L, 529L, 540L, 545L, 600L, 558L, 600L, 600L, 600L, 600L),
  dep_delay = c(2, 4, 2, -1, -6, -4, -5, -3, -3, -2),
  arr_time = c(830L, 850L, 923L, 1004L, 812L, 740L, 913L, 709L, 838L, 753L),
  sched_arr_time = c(819L, 830L, 850L, 1022L, 837L, 728L, 854L, 723L, 846L, 745L),
  arr_delay = c(11, 20, 33, -18, -25, 12, 19, -14, -8, 8),
  carrier = c("UA", "UA", "AA", "B6", "DL", "UA", "B6", "EV", "B6", "AA"),
  flight = c(1545L, 1714L, 1141L, 725L, 461L, 1696L, 507L, 5708L, 79L, 301L),
  tailnum = c("N14228", "N24211", "N619AA", "N804JB", "N668DN", "N39463", "N516JB", "N829AS", "N593JB", "N3ALAA"),
  origin = c("EWR", "LGA", "JFK", "JFK", "LGA", "EWR", "EWR", "LGA", "JFK", "LGA"),
  dest = c("IAH", "IAH", "MIA", "BQN", "ATL", "ORD", "FLL", "IAD", "MCO", "ORD"),
  air_time = c(227, 227, 160, 183, 116, 150, 158, 53, 140, 138),
  distance = c(1400, 1416, 1089, 1576, 762, 719, 1065, 229, 944, 733),
  hour = c(5, 5, 5, 5, 6, 5, 6, 6, 6, 6),
  minute = c(15, 29, 40, 45, 0, 58, 0, 0, 0, 0)
)

