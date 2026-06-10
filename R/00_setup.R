# =============================================================================
# 00_setup.R
# Run this once before the session to make sure everything works.
# =============================================================================

# --- 1. Packages -------------------------------------------------------------

needed <- c("httr", "sf", "terra", "dplyr", "tidyr", "ggplot2", "leaflet")
missing <- setdiff(needed, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)
invisible(lapply(needed, library, character.only = TRUE))

# --- 2. Credentials ----------------------------------------------------------
# These must live in ~/.Renviron, not in this script. To set them:
#   1) Run: usethis::edit_r_environ()      # opens ~/.Renviron
#   2) Add two lines:
#        SH_CLIENT_ID=your-client-id-here
#        SH_CLIENT_SECRET=your-client-secret-here
#   3) Save, restart R.

client_id     <- Sys.getenv("SH_CLIENT_ID")
client_secret <- Sys.getenv("SH_CLIENT_SECRET")

if (client_id == "" || client_secret == "") {
  stop("SH_CLIENT_ID / SH_CLIENT_SECRET not found in ~/.Renviron. ",
       "See instructions at the top of this file.")
}

# --- 3. Test authentication --------------------------------------------------

get_token <- function(client_id, client_secret) {
  response <- POST(
    "https://services.sentinel-hub.com/oauth/token",
    encode = "form",
    body = list(
      grant_type    = "client_credentials",
      client_id     = client_id,
      client_secret = client_secret
    )
  )
  if (http_status(response)$category != "Success") {
    stop("Authentication failed — check your Sentinel Hub client ID/secret.")
  }
  content(response, "parsed")$access_token
}

bearer_token <- get_token(client_id, client_secret)
message("✅ Authenticated. Token length: ", nchar(bearer_token))
message("✅ All packages loaded. You're ready for the session.")
