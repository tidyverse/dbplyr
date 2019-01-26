context("test-backend-oracle.R")

test_that("custom scalar functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_oracle())
  }

  expect_equal(trans(as.character(x)), sql("CAST(`x` AS VARCHAR2(255))"))
  expect_equal(trans(as.integer64(x)), sql("CAST(`x` AS NUMBER(19))"))
  expect_equal(trans(as.double(x)),    sql("CAST(`x` AS NUMBER)"))
  expect_equal(trans(as.Date(x)),    sql("DATE `x`"))
  #expect_equal(trans(as.POSIXct(x)),    sql("DATE `x`"))
  expect_equal(trans(as.Date(x, format = '%Y-%b-%d %X')),    sql("TO_DATE(`x`, 'YYYY-MON-DD HH24:MI:SS')"))
  expect_equal(trans(today()), sql("TRUNC(SYSDATE)"))
  expect_equal(trans(now()), sql("SYSDATE"))

  expect_equal(trans(today() %>% `month<-`(3)), sql("to_date('03'||to_char(TRUNC(SYSDATE),'DDYYYYHH24MISS'), 'MMDDYYYYHH24MISS')"))
  expect_equal(trans(today() %>% `month<-`('Mar')), sql("to_date('03'||to_char(TRUNC(SYSDATE),'DDYYYYHH24MISS'), 'MMDDYYYYHH24MISS')"))
  expect_equal(trans(today() %>% `month<-`('October')), sql("to_date('10'||to_char(TRUNC(SYSDATE),'DDYYYYHH24MISS'), 'MMDDYYYYHH24MISS')"))

})

test_that("queries translate correctly", {
  mf <- lazy_frame(x = 1, src = simulate_oracle())

  expect_match(
    mf %>% head() %>% sql_render(simulate_oracle()),
    sql("^SELECT [*] FROM [(]SELECT [*]\nFROM [(]`df`[)] [)] `[^`]*` WHERE ROWNUM [<][=] 6")
  )
})

test_that("paste and paste0 translate correctly", {
  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_oracle(), window = FALSE)
  }

  expect_equal(trans(paste(x, y)), sql("`x` || ' ' || `y`"))
  expect_equal(trans(paste0(x, y)), sql("`x` || `y`"))
})
