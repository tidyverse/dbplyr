library("devtools")

install.packages("RPostgreSQL", lib = getOption("devtools.revdep.libpath"))
install.packages("bit64", lib = getOption("devtools.revdep.libpath"))
install.packages("rJava", lib = getOption("devtools.revdep.libpath"))

revdep_check()
revdep_check_save_summary()
revdep_check_print_problems()
