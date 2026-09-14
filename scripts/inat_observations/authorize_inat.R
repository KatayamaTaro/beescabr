# =============================================================
# inat_observations/authorize_inat.R
# beescabr pipeline -- ONE-TIME SETUP: connect this computer to your iNaturalist account.
#
#   source("scripts/inat_observations/authorize_inat.R")
#
# Run this once, on a new machine, BEFORE the cleaning pipeline. iNaturalist hides the
# true coordinates of sensitive species from the public; signed in as yourself you get
# the real ones for your own records, and that is what the park needs for its maps.
# This script signs you in, saves the token, and then checks that the real coordinates
# actually come through, so a coordinate problem is found now and not in a map later.
# =============================================================

source("scripts/config.R")
source("scripts/inat_observations/engine/api/inat_auth.R")

# The signed-in username out of a users/me response. A pure function because
# `me$results[[1]]$login` threw "subscript out of bounds" on an empty response --
# [[1]] fails before %||% can supply a fallback -- so an odd but harmless API
# answer ended this script in a raw R error with no instruction attached.
.auth_login_of <- function(me) {
  r <- if (is.null(me)) NULL else me$results
  if (is.null(r) || !length(r)) return("")
  r[[1]]$login %||% ""
}

if (!exists("AUTHORIZE_INAT_SOURCED_FOR_HELPERS")) {

if (!inat_auth_enabled()) {
  message("")
  message("  STOP -- this computer has no iNaturalist app registered yet.")
  message("")
  message("  WHAT IS MISSING")
  message("    Three values that let this pipeline sign in as you. They belong in")
  message("      ", INAT_SECRETS_FILE)
  message("    one per line, as INAT_CLIENT_ID=..., INAT_CLIENT_SECRET=... and")
  message("    INAT_REDIRECT_URI=...")
  message("")
  message("  HOW TO GET THEM -- about two minutes")
  message("    1. Sign in as THE PARK'S iNaturalist account. This matters: observers")
  message("       grant coordinate trust to one specific account and it does not carry")
  message("       over, so a brand-new account gets blurred coordinates for everything.")
  message("    2. Open https://www.inaturalist.org/oauth/applications/new")
  message("    3. Name it something recognizable, e.g. officialbeescabr")
  message("    4. Callback URL -- type exactly:  http://localhost:3000/beescabr")
  message("    5. Click Create. The page then shows an Application ID and a Secret:")
  message("       those two are INAT_CLIENT_ID and INAT_CLIENT_SECRET, and the callback")
  message("       URL you just typed is INAT_REDIRECT_URI.")
  message("")
  message("  The same steps, with the reasoning, are in dev-docs/PIPELINE_GUIDE.md.")
  message("  These credentials stay in data/secrets/, which is never committed and is")
  message("  not part of a data handoff. Do not share them.")
  stop("no iNaturalist credentials yet -- see the steps above.", call. = FALSE)
}

message("")
message("  STEP 1 of 3 -- signing in to iNaturalist")
message("    A browser window opens. Sign in, then click Authorize. The page then")
message("    bounces back to this computer on its own -- there is nothing to copy.")
message("    Already done this here? Nothing opens; the saved token is reused.")
jwt <- inat_auth_token(force = TRUE)
message("    signed in.")

auth_get <- function(path, query = list())
  request(paste0(INAT_BASE_URL, path)) |>
    req_url_query(!!!query) |>
    req_headers(Authorization = paste("Bearer", jwt)) |>
    req_user_agent(INAT_USER_AGENT) |>
    req_perform() |> resp_body_json()

message("")
message("  STEP 2 of 3 -- which account is this?")
who <- .auth_login_of(auth_get("users/me"))
if (!nzchar(who)) {
  message("    iNaturalist accepted the sign-in but did not say who you are.")
  message("    Run this script again. If it happens twice, the app registration in")
  message("    ", INAT_SECRETS_FILE, " is probably for a different account.")
  stop("could not read the signed-in username.", call. = FALSE)
}
message("    ", who)
message("    Everything the pipeline downloads is pulled as this account, and blurred")
message("    coordinates are released only to the account each observer trusted. If that")
message("    is not this one, the park's bees will come back blurred -- register the app")
message("    again on the park's account (see dev-docs/PIPELINE_GUIDE.md) before running")
message("    the pipeline.")

message("")
message("  STEP 3 of 3 -- do the true coordinates come through?")
message("    iNaturalist blurs the location of sensitive species -- rare bees among them --")
message("    to roughly a 20 km box for everyone except the person who made the record.")
message("    Signed in as yourself you should see the exact spot on YOUR OWN records.")
message("    That is what the next few lines check.")

# The blur happens two different ways and they are separate fields on the record:
# an observer can set `geoprivacy` by hand, and iNaturalist sets `taxon_geoprivacy`
# on its own for a sensitive species. Querying geoprivacy="obscured" therefore MISSED
# every automatically-obscured record -- which is nearly all of them, and exactly the
# ones this check exists for -- and reported "0" to accounts that own dozens.
mine <- auth_get("observations",
                 list(user_login = who, per_page = 50, order_by = "id",
                      taxon_geoprivacy = "obscured"))
hits <- Filter(function(o) isTRUE(o$obscured), mine$results %||% list())

message("")
if (!length(hits)) {
  message("    You own no blurred records, so there is nothing to check here.")
  message("    That is fine -- it usually just means none of your own bees are")
  message("    sensitive species. The token is saved and the pipeline can run.")
} else {
  message("    ", length(hits), " of your records are blurred for the public. On each one,")
  message("    'public' is the blurred point everybody sees and 'private' is the real spot:")
  for (o in utils::head(hits, 5))
    message(sprintf("      %s  https://www.inaturalist.org/observations/%s",
                    o$taxon$name %||% "?", o$id %||% "?"),
            "\n        public  ", o$location %||% "(none)",
            "\n        private ", o$private_location %||% "(none)")
  ok <- vapply(hits, function(o) nzchar(o$private_location %||% ""), logical(1))
  message("")
  if (all(ok)) {
    message("    Every one shows a private coordinate. That is the answer we want:")
    message("    the park's maps will use real locations, not blurred ones.")
  } else {
    message("    ", sum(!ok), " of them show no private coordinate. Sign in at")
    message("    https://www.inaturalist.org and check that these are YOUR records --")
    message("    the real spot is only ever released to the person who made the record.")
  }
}

message("")
message("  Setup is done. The token is saved on this computer, so you will not be asked")
message("  again. Next step -- the pipeline itself:")
message("    source(\"scripts/run_data_cleaning_pipeline.R\")")
message("")

}
