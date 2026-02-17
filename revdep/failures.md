# vegdata (1.9.15)

* Email: <mailto:florian.jansen@uni-rostock.de>
* GitHub mirror: <https://github.com/cran/vegdata>

Run `revdepcheck::cloud_details(, "vegdata")` for more info

## In both

*   checking whether package ‘vegdata’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/vegdata/new/vegdata.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘vegdata’ ...
** this is package ‘vegdata’ version ‘1.9.15’
** package ‘vegdata’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
...
Warning in dir.create(self$cache_path_get(), recursive = TRUE) :
  cannot create dir '/root/.cache/R', reason 'No such file or directory'
Error: package or namespace load failed for ‘vegdata’:
 .onLoad failed in loadNamespace() for 'vegdata', details:
  call: file.copy(from = list.files(s, full.names = TRUE), to = target, 
  error: more 'from' files than 'to' files
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/vegdata/new/vegdata.Rcheck/vegdata’


```
### CRAN

```
* installing *source* package ‘vegdata’ ...
** this is package ‘vegdata’ version ‘1.9.15’
** package ‘vegdata’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
...
Warning in dir.create(self$cache_path_get(), recursive = TRUE) :
  cannot create dir '/root/.cache/R', reason 'No such file or directory'
Error: package or namespace load failed for ‘vegdata’:
 .onLoad failed in loadNamespace() for 'vegdata', details:
  call: file.copy(from = list.files(s, full.names = TRUE), to = target, 
  error: more 'from' files than 'to' files
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/vegdata/old/vegdata.Rcheck/vegdata’


```
