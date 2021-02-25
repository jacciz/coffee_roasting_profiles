pool <- pool::dbPool(RSQLite::SQLite(), dbname = "data-raw/roasting_profiles.db")
# pool <- pool::dbPool(RSQLite::SQLite(), dbname = "C:/W_shortcut/coffeeroastingprofiles/data-raw/roasting_profiles.db") # TEST LINK
# NON Pool
# pool <- dbConnect(RSQLite::SQLite(), dbname = "data/vaccine_inventory.db")
# pool <- dbConnect(RSQLite::SQLite(), dbname = "C:/W_shortcut/vaccine_distribution/data/vaccine_inventory.db")
# allocations <-dbReadTable(pool, "contacts")
# dbDisconnect(pool)
# dbListFields(pool, "jabs_master")
# poolClose(pool)
