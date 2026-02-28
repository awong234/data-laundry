# A live interactive group demo where participants input their names, some
# chocolate brands and items, and created an intermediate table mapping a
# person to a favorite candy.

# A temporary GCP SQL Server cloud instance was created for this demo, to which 
# participants were able to join with the correct credentials (not included).

# This script is for display and reference only, it will not run unless you
# have a SQL Server instance you can connect to, and then you will have to
# modify the connection information for your setup.

library(DBI)
library(glue)
library(purrr)

# # # # # # # # # #
# LOGIN ###########
# # # # # # # # # #

con = DBI::dbConnect(
    odbc::odbc(),
    driver = "ODBC Driver 18 for SQL Server",
    server = "", # IP address -- server no longer exists.
    uid = "participants",
    pwd = Sys.getenv("PWD"),
    database = "crug",
    trustservercertificate = "yes"
)

# # # # # # # # # # # # # # # # # # # # # # # #
# CREATE TABLES IF THEY DON'T EXIST ###########
# # # # # # # # # # # # # # # # # # # # # # # #

tables = dbListTables(con)

setup_table_names = c(
    "participants",
    "chocolate_brands",
    "favorites"
)

# Here are the queries that define the table structure
setup_table_definitions = list(
    glue(r"{
    CREATE TABLE "dbo".participants (
        auto_id INT NOT NULL IDENTITY(1,1) PRIMARY KEY,
        first_name varchar(max),
        last_name varchar(max),
        attend_date date
    );
    }"),
    glue(r"{
    CREATE TABLE "dbo".chocolate_brands (
        auto_choco_id INT NOT NULL IDENTITY(1,1) PRIMARY KEY,
        brand varchar(max),
        name varchar(max)
    )
    }"),
    glue(r"{
    CREATE TABLE "dbo".favorites (
        person_id INT,
        chocolate_id INT
    )
    }")
)

names(setup_table_definitions) = setup_table_names

create_table_if_not_exists = function(name, query) {
    tables = dbListTables(con)
    if (!name %in% tables) {
        dbExecute(con, query)
        message(glue("Table {name} created!"))
    } else {
        message(glue("Table {name} already exists!"))
    }
}

# For each table definition and table name, if the table doesn't exist, create
# it.
purrr::pwalk(
    .f = create_table_if_not_exists,
    .l = list(name = setup_table_names, query = setup_table_definitions)
)

# e.g. this is what it's doing; it's a concise way to write a for loop
# create_table_if_not_exists(setup_table_names[1], setup_table_definitions[1])
# create_table_if_not_exists(setup_table_names[2], setup_table_definitions[2])
# create_table_if_not_exists(setup_table_names[3], setup_table_definitions[3])
# create_table_if_not_exists(setup_table_names[4], setup_table_definitions[4])

# # # # # # # # # # # # # # # #
# Upload some data! ###########
# # # # # # # # # # # # # # # #

## Update participants table -- add your name! -----------

dbListTables(con, schema="dbo") # List the tables in the database

str(dbGetQuery(con, "select top(100) * from participants")) # Get an idea of what the column types are

# Create a data frame that matches the schema of the table
# No need to make the id column
me = data.frame(
    first_name = "Alec",
    last_name = "Wong",
    attend_date = as.Date("2026-02-28")
)

# Write the data to the table. append=TRUE, overwrites are disabled!
dbWriteTable(con, "participants", me, append = TRUE)

# Read the entire table
dbReadTable(con, "participants")

## Update chocolates database ------------

str(dbGetQuery(con, "select * from chocolate_brands where 1=0"))

chocolate = data.frame(
    brand = "ferrero",
    name = "ferrero-rocher"
)

dbWriteTable(con, "chocolate_brands", chocolate, append=TRUE)

dbReadTable(con, "chocolate_brands")

## What's your favorite chocolate? -------------------

str(dbGetQuery(con, "select * from favorites where 1=0"))

dbGetQuery(con, glue("select * from participants where first_name = 'Alec'"))
dbGetQuery(con, glue("select * from chocolate_brands"))

favorite = data.frame(
    person_id = 1,    # My person id
    chocolate_id = 1  # My designated favorite chocolate id
)

dbWriteTable(con, "favorites", favorite, append=TRUE)
dbReadTable(con, "favorites")

# # # # # # # # # # # # # # # # # # # #
# Perform a join #######################
# # # # # # # # # # # # # # # # # # # #

# String interpolation with SQL

custom = "Alec"

# glue_sql will automatically and very conveniently format inputs depending on
# their type to be valid for the SQL database -- I don't have to fuss about 
# quotes
join_query = glue_sql(
    "select *
    from favorites f
    left join participants p on
        f.person_id = p.auto_id
    left join chocolate_brands c on
        f.chocolate_id = c.auto_choco_id
    where p.first_name = {custom}
    ", .con = con
)

join_query

dbGetQuery(con, join_query)

# # # # # # # # # # # # # # # #
# DO it using DBPLYR ##########
# # # # # # # # # # # # # # # #

library(dplyr)
# A separate package you need -- technically don't need to load this explicitly
# once installed though.
library(dbplyr)

tbl_participants = tbl(con, "participants")
tbl_chocolates   = tbl(con, "chocolate_brands")
tbl_favorites    = tbl(con, "favorites")

tbl_favorites %>%
    left_join(tbl_participants, by = c("person_id" = "auto_id")) %>%
    left_join(tbl_chocolates, by = c("chocolate_id" = "auto_choco_id")) %>%
    collect()
