# bigrquery

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/bigrquery
* URL: https://github.com/rstats-db/bigrquery
* BugReports: https://github.com/rstats-db/bigrquery/issues
* Date/Publication: 2019-07-02 05:20:57 UTC
* Number of recursive dependencies: 63

Run `revdep_details(,"bigrquery")` for more info

</details>

## In both

*   checking whether package ‘bigrquery’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/bigrquery/new/bigrquery.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bigrquery’ ...
** package ‘bigrquery’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DSTRICT_R_HEADERS -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/rapidjsonr/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BqField.cpp -o BqField.o
In file included from BqField.cpp:6:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include/RProgress.h:6:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: unknown type name 'uuid_t'
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from BqField.cpp:6:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include/RProgress.h:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from BqField.cpp:6:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include/RProgress.h:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from BqField.cpp:6:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include/RProgress.h:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from BqField.cpp:6:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include/RProgress.h:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [BqField.o] Error 1
ERROR: compilation failed for package ‘bigrquery’
* removing ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/bigrquery/new/bigrquery.Rcheck/bigrquery’

```
### CRAN

```
* installing *source* package ‘bigrquery’ ...
** package ‘bigrquery’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DSTRICT_R_HEADERS -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/rapidjsonr/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BqField.cpp -o BqField.o
In file included from BqField.cpp:6:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include/RProgress.h:6:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: unknown type name 'uuid_t'
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from BqField.cpp:6:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include/RProgress.h:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from BqField.cpp:6:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include/RProgress.h:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from BqField.cpp:6:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include/RProgress.h:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from BqField.cpp:6:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigrquery/progress/include/RProgress.h:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [BqField.o] Error 1
ERROR: compilation failed for package ‘bigrquery’
* removing ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/bigrquery/old/bigrquery.Rcheck/bigrquery’

```
# bigsnpr

<details>

* Version: 1.3.0
* Source code: https://github.com/cran/bigsnpr
* URL: https://privefl.github.io/bigsnpr
* BugReports: https://github.com/privefl/bigsnpr/issues
* Date/Publication: 2020-03-09 20:00:02 UTC
* Number of recursive dependencies: 126

Run `revdep_details(,"bigsnpr")` for more info

</details>

## In both

*   checking whether package ‘bigsnpr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/bigsnpr/new/bigsnpr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bigsnpr’ ...
** package ‘bigsnpr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigsnpr/bigstatsr/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/Rcpp/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigsnpr/RcppArmadillo/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigsnpr/rmio/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -DARMA_64BIT_WORD=1 -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘bigsnpr’
* removing ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/bigsnpr/new/bigsnpr.Rcheck/bigsnpr’

```
### CRAN

```
* installing *source* package ‘bigsnpr’ ...
** package ‘bigsnpr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigsnpr/bigstatsr/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/Rcpp/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigsnpr/RcppArmadillo/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/bigsnpr/rmio/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -DARMA_DONT_PRINT_OPENMP_WARNING -DARMA_DONT_PRINT_CXX11_WARNING -DARMA_64BIT_WORD=1 -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘bigsnpr’
* removing ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/bigsnpr/old/bigsnpr.Rcheck/bigsnpr’

```
# dplyr

<details>

* Version: 0.8.5
* Source code: https://github.com/cran/dplyr
* URL: http://dplyr.tidyverse.org, https://github.com/tidyverse/dplyr
* BugReports: https://github.com/tidyverse/dplyr/issues
* Date/Publication: 2020-03-07 12:20:02 UTC
* Number of recursive dependencies: 94

Run `revdep_details(,"dplyr")` for more info

</details>

## In both

*   checking whether package ‘dplyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/dplyr/new/dplyr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dplyr’ ...
** package ‘dplyr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DRCPP_DEFAULT_INCLUDE_CALL=false -DCOMPILING_DPLYR -DRCPP_USING_UTF8_ERROR_STRING -DRCPP_USE_UNWIND_PROTECT -DBOOST_NO_AUTO_PTR  -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/plogr/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from RcppExports.cpp:4:
In file included from ./../inst/include/dplyr.h:4:
In file included from ../inst/include/dplyr/main.h:6:
In file included from ../inst/include/dplyr/workarounds/static_assert.h:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config.hpp:57:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config/detail/posix_features.hpp:18:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: unknown type name 'uuid_t'
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from RcppExports.cpp:4:
In file included from ./../inst/include/dplyr.h:4:
In file included from ../inst/include/dplyr/main.h:6:
In file included from ../inst/include/dplyr/workarounds/static_assert.h:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config.hpp:57:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from RcppExports.cpp:4:
In file included from ./../inst/include/dplyr.h:4:
In file included from ../inst/include/dplyr/main.h:6:
In file included from ../inst/include/dplyr/workarounds/static_assert.h:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config.hpp:57:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from RcppExports.cpp:4:
In file included from ./../inst/include/dplyr.h:4:
In file included from ../inst/include/dplyr/main.h:6:
In file included from ../inst/include/dplyr/workarounds/static_assert.h:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config.hpp:57:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from RcppExports.cpp:4:
In file included from ./../inst/include/dplyr.h:4:
In file included from ../inst/include/dplyr/main.h:6:
In file included from ../inst/include/dplyr/workarounds/static_assert.h:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config.hpp:57:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from RcppExports.cpp:5:
In file included from ./../inst/include/dplyr_types.h:4:
In file included from ../inst/include/dplyr/data/GroupedDataFrame.h:8:
In file included from ../inst/include/tools/SymbolMap.h:4:
In file included from ../inst/include/tools/hash.h:10:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/unordered_map.hpp:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/unordered/unordered_map.hpp:19:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/move/move.hpp:30:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/move/iterator.hpp:27:
/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/move/detail/iterator_traits.hpp:29:1: warning: inline namespaces are a C++11 feature [-Wc++11-inline-namespace]
BOOST_MOVE_STD_NS_BEG
^
/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/new/BH/include/boost/move/detail/std_ns_begin.hpp:18:34: note: expanded from macro 'BOOST_MOVE_STD_NS_BEG'
   #define BOOST_MOVE_STD_NS_BEG _LIBCPP_BEGIN_NAMESPACE_STD
                                 ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__config:866:53: note: expanded from macro '_LIBCPP_BEGIN_NAMESPACE_STD'
#define _LIBCPP_BEGIN_NAMESPACE_STD namespace std { inline namespace _LIBCPP_ABI_NAMESPACE {
                                                    ^
1 warning and 5 errors generated.
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘dplyr’
* removing ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/dplyr/new/dplyr.Rcheck/dplyr’

```
### CRAN

```
* installing *source* package ‘dplyr’ ...
** package ‘dplyr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DRCPP_DEFAULT_INCLUDE_CALL=false -DCOMPILING_DPLYR -DRCPP_USING_UTF8_ERROR_STRING -DRCPP_USE_UNWIND_PROTECT -DBOOST_NO_AUTO_PTR  -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/plogr/include" -I"/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from RcppExports.cpp:4:
In file included from ./../inst/include/dplyr.h:4:
In file included from ../inst/include/dplyr/main.h:6:
In file included from ../inst/include/dplyr/workarounds/static_assert.h:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config.hpp:57:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config/detail/posix_features.hpp:18:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: unknown type name 'uuid_t'
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from RcppExports.cpp:4:
In file included from ./../inst/include/dplyr.h:4:
In file included from ../inst/include/dplyr/main.h:6:
In file included from ../inst/include/dplyr/workarounds/static_assert.h:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config.hpp:57:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from RcppExports.cpp:4:
In file included from ./../inst/include/dplyr.h:4:
In file included from ../inst/include/dplyr/main.h:6:
In file included from ../inst/include/dplyr/workarounds/static_assert.h:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config.hpp:57:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from RcppExports.cpp:4:
In file included from ./../inst/include/dplyr.h:4:
In file included from ../inst/include/dplyr/main.h:6:
In file included from ../inst/include/dplyr/workarounds/static_assert.h:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config.hpp:57:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from RcppExports.cpp:4:
In file included from ./../inst/include/dplyr.h:4:
In file included from ../inst/include/dplyr/main.h:6:
In file included from ../inst/include/dplyr/workarounds/static_assert.h:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config.hpp:57:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from RcppExports.cpp:5:
In file included from ./../inst/include/dplyr_types.h:4:
In file included from ../inst/include/dplyr/data/GroupedDataFrame.h:8:
In file included from ../inst/include/tools/SymbolMap.h:4:
In file included from ../inst/include/tools/hash.h:10:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/unordered_map.hpp:17:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/unordered/unordered_map.hpp:19:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/move/move.hpp:30:
In file included from /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/move/iterator.hpp:27:
/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/move/detail/iterator_traits.hpp:29:1: warning: inline namespaces are a C++11 feature [-Wc++11-inline-namespace]
BOOST_MOVE_STD_NS_BEG
^
/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/dbplyr/old/BH/include/boost/move/detail/std_ns_begin.hpp:18:34: note: expanded from macro 'BOOST_MOVE_STD_NS_BEG'
   #define BOOST_MOVE_STD_NS_BEG _LIBCPP_BEGIN_NAMESPACE_STD
                                 ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__config:866:53: note: expanded from macro '_LIBCPP_BEGIN_NAMESPACE_STD'
#define _LIBCPP_BEGIN_NAMESPACE_STD namespace std { inline namespace _LIBCPP_ABI_NAMESPACE {
                                                    ^
1 warning and 5 errors generated.
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘dplyr’
* removing ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/dplyr/old/dplyr.Rcheck/dplyr’

```
# grasp2db

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/grasp2db
* Number of recursive dependencies: 100

Run `revdep_details(,"grasp2db")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented code objects:
      ‘GRASP2’ ‘checkAnti’ ‘getJoinCompatible’
    Undocumented data sets:
      ‘mml10p_nox’ ‘uniqueGexNames2.0’ ‘uniquePPDnames2.0’
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking data for non-ASCII characters ... WARNING
    ```
      Warning: found non-ASCII string
      'Beh<e7>et's disease' in object 'uniquePPDnames2.0'
    ```

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      
      Note: significantly better compression could be obtained
            by using R CMD build --resave-data
                            old_size new_size compress
      mml10p_nox.rda           7.1Mb    2.8Mb       xz
      uniquePPDnames2.0.rda     17Kb     15Kb    bzip2
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘AnnotationHubData’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   7.1Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    License components with restrictions not permitted:
      Artistic-2.0 + file LICENSE
    ```

*   checking R code for possible problems ... NOTE
    ```
    .grasp2ToAnnotationHub: no visible global function definition for
      ‘outputFile’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/db_AnnotationHub.R:39)
    .grasp2ToAnnotationHub: no visible global function definition for
      ‘outputFile’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/db_AnnotationHub.R:40)
    checkAnti: no visible binding for global variable ‘chr_hg19’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/doanti.R:19-20)
    getJoinCompatible: no visible binding for global variable ‘gwrngs19’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/doanti.R:7)
    Undefined global functions or variables:
      chr_hg19 gwrngs19 outputFile
    ```

# msPurity

<details>

* Version: 
* Source code: ???
* URL: https://dbplyr.tidyverse.org/, https://github.com/tidyverse/dbplyr
* BugReports: https://github.com/tidyverse/dbplyr/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There is a binary version available but the source version is later:
     binary  source needs_compilation
Rcpp  1.0.4 1.0.4.6              TRUE

  Binaries will be installed


installing the source package ‘msPurityData’



```
### CRAN

```

  There is a binary version available but the source version is later:
     binary  source needs_compilation
Rcpp  1.0.4 1.0.4.6              TRUE

  Binaries will be installed


installing the source package ‘msPurityData’



```
# Organism.dplyr

<details>

* Version: 1.14.0
* Source code: https://github.com/cran/Organism.dplyr
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 118

Run `revdep_details(,"Organism.dplyr")` for more info

</details>

## In both

*   R CMD check timed out
    

# pleiades

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/pleiades
* URL: https://github.com/ropensci/pleiades
* BugReports: https://github.com/ropensci/pleiades/issues
* Date/Publication: 2017-06-15 17:49:51 UTC
* Number of recursive dependencies: 71

Run `revdep_details(,"pleiades")` for more info

</details>

## In both

*   R CMD check timed out
    

# RClickhouse

<details>

* Version: 0.5.2
* Source code: https://github.com/cran/RClickhouse
* URL: https://github.com/IMSMWU/RClickhouse
* BugReports: https://github.com/IMSMWU/RClickhouse/issues
* Date/Publication: 2020-03-06 15:30:02 UTC
* Number of recursive dependencies: 40

Run `revdep_details(,"RClickhouse")` for more info

</details>

## In both

*   R CMD check timed out
    

# resourcer

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/resourcer
* BugReports: https://github.com/obiba/resourcer
* Date/Publication: 2020-04-02 15:50:02 UTC
* Number of recursive dependencies: 85

Run `revdep_details(,"resourcer")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

# taxizedb

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/taxizedb
* URL: https://github.com/ropensci/taxizedb
* BugReports: https://github.com/ropensci/taxizedb/issues
* Date/Publication: 2017-06-20 23:54:44 UTC
* Number of recursive dependencies: 60

Run `revdep_details(,"taxizedb")` for more info

</details>

## In both

*   R CMD check timed out
    

# valr

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/valr
* URL: http://github.com/rnabioco/valr, http://rnabioco.github.io/valr
* BugReports: https://github.com/rnabioco/valr/issues
* Date/Publication: 2019-01-03 16:20:04 UTC
* Number of recursive dependencies: 118

Run `revdep_details(,"valr")` for more info

</details>

## In both

*   R CMD check timed out
    

