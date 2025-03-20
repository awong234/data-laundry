# Library & Packages -----------------------------

dir.create('lib', showWarnings = FALSE)
.libPaths('lib')
if (! 'pak' %in% .packages(all = TRUE)) install.packages("pak", type = "binary")

pak::repo_add(CRAN = 'https://packagemanager.posit.co/cran/2025-03-09')
pak::pkg_install(c(
    'readxl',
    'dplyr',
    'stringr',
    'tidyr',
    'ggplot2'
))

# Setup --------------------------------------------

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Cleaning -----------------------------------------

file = 'RollData.xlsx'

data = list(
    cleric  = read_excel(file, sheet = 1, range = cell_limits(c(2, 1), c(NA, 4)), col_names = TRUE),
    paladin = read_excel(file, sheet = 1, range = cell_limits(c(2, 6), c(NA, 8)), col_names = TRUE),
    shaman  = read_excel(file, sheet = 1, range = cell_limits(c(2, 10), c(NA, 12)), col_names = TRUE),
    hunter  = read_excel(file, sheet = 1, range = cell_limits(c(2, 14), c(NA, 16)), col_names = TRUE),
    centipede  = read_excel(file, sheet = 1, range = cell_limits(c(2, 18), c(NA, 22)), col_names = TRUE),
    giantfly  = read_excel(file, sheet = 1, range = cell_limits(c(2, 24), c(NA, 27)), col_names = TRUE)
)

clean = function(df) {
    # index non-missing records
    df$all_missing = apply(X = df, MARGIN = 1, FUN = \(x) all(is.na(x)))
    # Fix damage as data
    available_columns = colnames(df)
    damage_col = grep('damage', available_columns, ignore.case = TRUE)
    damage_col_name = available_columns[damage_col]
    if (length(damage_col) > 1) {
        df$Damage = coalesce(!!!df[, damage_col_name])
    } else {
        df$Damage = df[[damage_col_name]]
    }
    # Initialize roll column as 1d20
    df$roll = '1d20'
    # For each damage column type figure out what the die size is and fill as data
    for (i in seq_along(damage_col)) {
        roll_type = str_extract(available_columns[damage_col[i]], '\\d+d\\d+')
        roll_characteristic = str_split(roll_type, '')[[1]]
        nrolls = roll_characteristic[1] |> as.integer()
        die_size = roll_characteristic[3] |> as.integer()
        value_present = !is.na(df[, damage_col_name[i]])
        df = df |>
            mutate(
                roll = case_when(value_present ~ roll_type, .default = roll),
                nrolls = case_when(value_present ~ nrolls, .default = 1),
                die_size = case_when(value_present ~ die_size, .default = 20)
            )
        # Make damage columns character
        df[[damage_col_name[i]]] = as.character(df[[damage_col_name[i]]])
    }

    # Fix crits, remove x2 from the damage portion. Assuming damage roll is exactly what is show after x2.
    df$crit = str_detect(df$Damage, 'x')
    df$Damage = as.character(df$Damage)
    df$Damage[which(df$crit)] = sapply(str_split(df$Damage, ' ', ), \(x) tail(x, 1))[which(df$crit)]
    df$Damage = as.integer(df$Damage)

    # Add a time index, remove rows where no data were present
    df = df |> mutate( tindex = row_number() )
    df = df |> filter(!all_missing)
    df
}

data = lapply(data, clean)
data = bind_rows(data, .id = 'Role')

# Define "us"

data = data |> mutate(Party = !Role %in% c("centipede", "giantfly"))

data |> select(tindex, Role, Party, Attack, Damage, Skill, Initiative, roll, nrolls, die_size) |> arrange(tindex)

data_long = data |>
    select(Party, Role, Attack, Damage, Skill, Initiative, roll, nrolls, die_size, crit, tindex) |>
    tidyr::pivot_longer(c(Attack, Damage, Skill, Initiative), names_to = 'action_type', values_to = 'roll_face_value') |>
    filter(!is.na(roll_face_value))
data_long$action_type = factor(data_long$action_type, levels = c("Skill", "Initiative", "Attack", "Damage"))

# Reproduce plot ------------------------

data_long |>
    filter(action_type %in% c("Skill", "Initiative", "Attack")) |>
    ggplot() +
    geom_histogram(aes(x = roll_face_value, fill = action_type), color = 'black', bins = 20) +
    scale_fill_manual(values = c("green4", "orange", "dodgerblue")) +
    facet_wrap(~Party, labeller = label_both) +
    theme_bw()

pl1 = data_long |>
    ggplot() +
    geom_histogram(aes(x = roll_face_value, fill = Party), color = 'black', bins = 20, position = position_dodge()) +
    facet_wrap(~Party, labeller = label_both)

pl2 = pl1 + facet_grid(action_type ~ Party, labeller = labeller(action_type = label_value, Party = label_both))

pl1
pl2

# For later
d20_rolls = data_long |> filter(roll == '1d20')

# Statistical properties of truly random dice rolls ------------------

sample_dice_roll = function(n) {
    x = sample(1:20, size = n, replace = TRUE)
    x = factor(x, levels = seq(1,20))
    p = chisq.test(table(x))
    p$p.value
}

reps = 5000
p = vector('numeric', reps)
for (i in 1:reps) {
    cat(i, '\r')
    p[i] = sample_dice_roll(nrow(d20_rolls))
}
hist(p)

# Statistical properties of dice rolls -----------------

x = d20_rolls$roll_face_value
table(x)
hist(x, breaks = seq(0,20))
prop.table(table(x))
p = chisq.test(table(x))
p$p.value

# By action type
split(d20_rolls, d20_rolls$action_type) |>
    Filter(function(x) nrow(x) > 0, x = _) |>
    lapply(\(df) {
        rolls = df$roll_face_value |> factor(levels = seq(1,20))
        chisq.test(table(rolls))
    })

# By party
split(d20_rolls, d20_rolls$Party) |>
    Filter(function(x) nrow(x) > 0, x = _) |>
    lapply(\(df) {
        rolls = df$roll_face_value |> factor(levels = seq(1,20))
        chisq.test(table(rolls))
    })

