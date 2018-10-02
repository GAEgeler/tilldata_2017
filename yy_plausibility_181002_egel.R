# check for plausibility

# status: 2.10.18 // egel

# load data see script 04_load_data

set.seed(17)

df <- df_7 %>%
    group_by()


library(skimr)
skim(df)
describe(df$member)
describe(df$price_descript)
describe(df$article)

### validate exploration
skim(sample)
describe(sample$member)
describe(sample$price_descript)
