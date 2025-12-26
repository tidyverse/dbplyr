# Cache and retrieve an `src_sqlite` of the Lahman baseball database.

This creates an interesting database using data from the Lahman baseball
data source, provided by [Sean Lahman](http://seanlahman.com), and made
easily available in R through the Lahman package by Michael Friendly,
Dennis Murphy and Martin Monkman. See the documentation for that package
for documentation of the individual tables.

## Usage

``` r
lahman_sqlite(path = NULL)

lahman_postgres(dbname = "lahman", host = "localhost", ...)

lahman_mysql(dbname = "lahman", ...)

copy_lahman(con, ...)

has_lahman(type, ...)

lahman_srcs(..., quiet = NULL)
```

## Arguments

- ...:

  Other arguments passed to `src` on first load. For MySQL and
  PostgreSQL, the defaults assume you have a local server with `lahman`
  database already created. For `lahman_srcs()`, character vector of
  names giving srcs to generate.

- type:

  src type.

- quiet:

  if `TRUE`, suppress messages about databases failing to connect.

## Examples

``` r
# Connect to a local sqlite database, if already created
# \donttest{
library(dplyr)

if (has_lahman("sqlite")) {
  lahman_sqlite()
  batting <- tbl(lahman_sqlite(), "Batting")
  batting
}
#> Creating table: AllstarFull
#> Creating table: Appearances
#> Creating table: AwardsManagers
#> Creating table: AwardsPlayers
#> Creating table: AwardsShareManagers
#> Creating table: AwardsSharePlayers
#> Creating table: Batting
#> Creating table: BattingPost
#> Creating table: CollegePlaying
#> Creating table: Fielding
#> Creating table: FieldingOF
#> Creating table: FieldingOFsplit
#> Creating table: FieldingPost
#> Creating table: HallOfFame
#> Creating table: HomeGames
#> Creating table: LahmanData
#> Creating table: Managers
#> Creating table: ManagersHalf
#> Creating table: Parks
#> Creating table: People
#> Creating table: Pitching
#> Creating table: PitchingPost
#> Creating table: Salaries
#> Creating table: Schools
#> Creating table: SeriesPost
#> Creating table: Teams
#> Creating table: TeamsFranchises
#> Creating table: TeamsHalf
#> # A query:  ?? x 22
#> # Database: sqlite 3.51.1 [/tmp/Rtmpu1Pzuj/lahman.sqlite]
#>    playerID  yearID stint teamID lgID      G    AB     R     H   X2B
#>    <chr>      <int> <int> <chr>  <chr> <int> <int> <int> <int> <int>
#>  1 aardsda01   2004     1 SFN    NL       11     0     0     0     0
#>  2 aardsda01   2006     1 CHN    NL       45     2     0     0     0
#>  3 aardsda01   2007     1 CHA    AL       25     0     0     0     0
#>  4 aardsda01   2008     1 BOS    AL       47     1     0     0     0
#>  5 aardsda01   2009     1 SEA    AL       73     0     0     0     0
#>  6 aardsda01   2010     1 SEA    AL       53     0     0     0     0
#>  7 aardsda01   2012     1 NYA    AL        1     0     0     0     0
#>  8 aardsda01   2013     1 NYN    NL       43     0     0     0     0
#>  9 aardsda01   2015     1 ATL    NL       33     1     0     0     0
#> 10 aaronha01   1954     1 ML1    NL      122   468    58   131    27
#> # ℹ more rows
#> # ℹ 12 more variables: X3B <int>, HR <int>, RBI <int>, SB <int>,
#> #   CS <int>, BB <int>, SO <int>, IBB <int>, HBP <int>, SH <int>,
#> #   SF <int>, GIDP <int>

# Connect to a local postgres database with lahman database, if available
if (has_lahman("postgres")) {
  lahman_postgres()
  batting <- tbl(lahman_postgres(), "Batting")
}
#> Error: connection to server at "localhost" (::1), port 5432 failed: Connection refused
#>  Is the server running on that host and accepting TCP/IP connections?
#> connection to server at "localhost" (127.0.0.1), port 5432 failed: Connection refused
#>  Is the server running on that host and accepting TCP/IP connections?
# }
```
