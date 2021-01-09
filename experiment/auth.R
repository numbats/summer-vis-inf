library(googlesheets4)

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")

# check the value of the option, if you like
gargle::gargle_oauth_cache()

# trigger auth on purpose --> store a token in the specified cache
# a broswer will be opened
googlesheets4::sheets_auth()

# see your token file in the cache, if you like
list.files(".secrets/")

# deauth
sheets_deauth()

# sheets reauth with specified token and email address
sheets_auth(
  cache = ".secrets",
  email = "kjin7@student.monash.edu",
  token = "data/authentication.rds"
)

saveRDS(sheets_auth(), "data/authentication.rds")
