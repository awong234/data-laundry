if (! require("pak")) install.packages("pak", type = "binary")
pak::pkg_install(c(
    "DBI",
    "odbc",
    "rvest",
    "stringr",
    "dplyr",
    "dbplyr",
    "janitor",
    "glue",
    "bench"
))
