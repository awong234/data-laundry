library(DBI)
library(dplyr)
library(glue)

# Interesting cases:
# Bruce Springstein has a repeating cycle of presenters (Bruce Springsteen presented U2, Bono of U2 Presented Bruce Springsteen)
# John Illsley of Dire Straits presents Dire Straits -- self referential node
# Longest cycle is 6 presenters presenting presentees who present presentees who ...!

con = dbConnect(
    odbc::odbc(),
    driver = "ODBC Driver 18 for SQL Server",
    server="34.9.50.190",
    database="crug",
    uid="crug-member",
    pwd="*u+u?fpfrZ]xA|c",
    TrustServerCertificate="yes",
)

# For each group presented, find the names of the people in the group, then find who they presented.
get_next = function(p, level=0) {
    # When we join inductees table we're lining up the presented group with the inductee group
    join = setNames(c("inductee_group", "inductee_year"), c(glue("presented_{level}"), glue("presenter_year_{level}")))
    # When we join the presenters table, we are lining up the
    join2 = setNames(c("presenter_name"), glue("presenter_name_{level+1}"))
    p %>%
        inner_join(inductees, by = join, relationship='many-to-many') %>%
        rename("presenter_name_{level+1}" := inductee_name) %>%
        inner_join(presenters %>% select(presenter_year, presenter_name, presented), by = join2, relationship = 'many-to-many') %>%
        rename("presenter_year_{level+1}" := presenter_year) %>%
        rename("presented_{level+1}" := presented)
}

get_next_r = function(p, level=0, all_presenters = vector(mode="character")) {
    next_group = get_next(p, level)
    # Avoid cycles -- the next presenter should not be the same as any preceding first
    if (level > 0) {
        curr_presenter = as.symbol(glue("presenter_name_{level}"))
        last_presenter = as.symbol(glue("presenter_name_{level-1}"))
        previous_presenters = next_group %>% distinct({{last_presenter}}) %>% collect()
        all_presenters = c(all_presenters, previous_presenters[[1]])
        if (length(all_presenters)) next_group = next_group %>% filter(! {{curr_presenter}} %in% all_presenters)
    }
    n_presenters_in_next = collect(count(next_group))[[1]]
    # If there are no presentees who themselves presented an award.
    if (n_presenters_in_next == 0) {
        return(list(
            length = level,
            chain = p
        ))
    } else {
        get_next_r(next_group, level+1, all_presenters)
    }
}

prep_init = function(p) {
    p %>%
        rename(
            presenter_year_0 = presenter_year,
            presenter_name_0 = presenter_name,
            presented_0 = presented
            )
}

# In-memory --------

presenters = dbReadTable(con, "presenters")
inductees = dbReadTable(con, "inductees")

p = presenters %>% filter(presenter_name == "Paul McCartney", presented == "James Taylor") %>%
    prep_init()

p %>% get_next(0) %>% get_next(1) %>% get_next(2)
get_next_r(p)

# bench::mark(
#     get_next(p, 0) %>% get_next(1) %>% get_next(2),
#     get_next_r(p),
#     check = FALSE,
#     iterations = 50
# )

results_list = list(
    presenter = vector(mode="character"),
    presented = vector(mode="character"),
    chain = vector(mode="list"),
    length = vector(mode="numeric")
)


for (i in 1:nrow(presenters)) {
    presenter = presenters[i, "presenter_name"]
    presented = presenters[i, "presented"]
    message(glue("{presenter} presenting {presented}: {i}"))
    result = presenters[i, ] %>% prep_init() %>% get_next_r()
    results_list$chain[[i]] = result$chain
    results_list$length[i] = result$length
    message(glue("Chain length: {result$length}"))
}

biggest_chain = which.max(results_list$length)
results_list$chain[[biggest_chain]]

# Longest unbroken chain of presenters presenting inductees --------

inductees = tbl(con, "inductees")
groups = tbl(con, "inducted_groups")
presenters = tbl(con, "presenters")

p = presenters %>%
    filter(presenter_name == "Bruce Springsteen") %>%
    prep_init()

p %>% get_next(0) %>% get_next(1) %>% get_next(2)

p = presenters %>%
    filter(presenter_name == "Paul McCartney", presented == "James Taylor") %>%
    prep_init()

p %>% get_next(0) %>% get_next(1) %>% get_next(2)

presenters %>%
    filter(presenter_name == "U2", presented == "The Who") %>%
    prep_init() %>%
    get_next_r()

results_list = list(
    presenter = vector(mode="character"),
    presenting = vector(mode="character"),
    chain = vector(mode="list"),
    length = vector(mode="numeric")
)

unique_presenters = presenters %>% distinct(presenter_year, presenter_name, presented) %>% collect()

for (i in 1:nrow(unique_presenters)) {
    presenter = unique_presenters[i, ]
    presenter_remote = presenters %>%
        inner_join(presenter, copy = TRUE) %>%
        prep_init()
    message(glue("{i}: {presenter[\"presenter_name\"]} presenting {presenter['presented']}"))
    result = presenter_remote %>% get_next_r()
    message(glue(": chain length = {result$length}"))
    results_list$presenter[i]   = presenter$presenter_name
    results_list$presenting[i]  = presenter$presented
    results_list$chain[[i]]  = result$chain
    results_list$length[i] = result$length
}

biggest_chain = which.max(results_list$length)
results_list$chain[[biggest_chain]]
