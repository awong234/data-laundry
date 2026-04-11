library(rvest)
library(dplyr)
library(stringr)
library(DBI)

remove_non_characters = function(string, keep_chars = "") {
    r = regex(sprintf(r"{[^a-zA-Z %s]}", keep_chars))
    str_remove_all(string, pattern = r)
}

split_members = function(str){
    # First remove brackets and numbers
    str = str_replace_all(str, "é", "e")
    str = str_remove_all(str, r"{\[\w+\]}")
    str = str_remove_all(str, r"{(^.*Inducted\:)}")
    str = str_replace_all(str, ", Jr.", " Jr.")
    str = remove_non_characters(str, keep_chars = ",0-9")
    str = str_split(str, pattern = regex(r"{, and |,and | and |, |\s{2}}"))
    str
}

count_prior_nominations = function(noms) {
    if (grepl("First nomination", noms) | grepl("Inaugural class", noms)) {
        return(1L)
    }
    extr = str_extract(noms, r"{(\d+) \(.*\)}", group = 1)
    noms = extr[1]
    return(as.integer(noms))
}


# ------

url = "https://en.wikipedia.org/wiki/List_of_Rock_and_Roll_Hall_of_Fame_inductees"

html = rvest::read_html(url)
tables = rvest::html_table(html)
performers = tables[[1]]

names(performers) = names(performers) |> remove_non_characters()
performers = janitor::clean_names(performers)
performers = performers %>%
    mutate(
        name = str_remove_all(name, r"{\[\d+\]}"),
        name = str_remove_all(name, r"{\[N\d]}"),
        name = remove_non_characters(name, keep_chars = "&0-9\\-"),
        n_nominations = sapply(prior_nominations, count_prior_nominations),
        inducted_members = split_members(inducted_members),
        induction_presenter = split_members(induction_presenter)
    )


# Normalize tables --------

inducted_groups = performers %>%
    select(year, name, n_nominations) %>%
    rename(
        inductee_year = year,
        inductee_group = name
        )

inductees = performers %>% select(year, name, inducted_members) %>%
    tidyr::unnest(cols=c(inducted_members)) %>%
    mutate(inducted_members = ifelse(inducted_members == "", name, inducted_members)) %>%
    rename(
        inductee_year = year,
        inductee_group = name,
        inductee_name = inducted_members)

presenters = performers %>%
    select(year, induction_presenter, name) %>%
    tidyr::unnest(cols=c(induction_presenter)) %>%
    rename(
        presenter_name = induction_presenter,
        presenter_year = year,
        presented  = name
        )

presenters %>%
    filter(presenter_name == "Paul McCartney") %>%
    left_join(inducted_groups, by = c("presenter_year" = "inductee_year", "presented" = "inductee_group")) %>%
    left_join(inductees, by = c("presenter_year" = "inductee_year", "presented" = "inductee_group"), relationship = 'many-to-many') %>%
    print(n=Inf)

presenters %>%
    filter(presenter_year == 1996) %>%
    print(n = Inf)

# Write to db ------

con = dbConnect(
    odbc::odbc(),
    driver = "ODBC Driver 18 for SQL Server",
    server=Sys.getenv("GCP_SERVER_ADDRESS"),
    database="crug",
    uid=Sys.getenv("GCP_SERVER_ADMIN"),
    pwd=Sys.getenv("GCP_PASS"),
    TrustServerCertificate="yes",
)

tables_to_write = list(
    "inducted_groups" = inducted_groups,
    "inductees" = inductees,
    "presenters" = presenters
)

Map(\(name, table) {
    dbWriteTable( con, name, table , overwrite = TRUE)
}, name = names(tables_to_write), table = tables_to_write)
