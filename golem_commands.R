.pkg <- "Rminor" # Change to the repo name
golem::fill_desc(
pkg_name = .pkg, # The Name of the package containing the App
pkg_title = .pkg, # The Title of the package containing the App
pkg_description = "An open-source HMIS custom reporting Shiny app", # The Description of the package containing the App
author_first_name = "Genelle", # Your First Name
author_last_name = "Denzin",  # Your Last Name
author_email = "genelledenzin@cohhio.org",      # Your Email
repo_url = "https://github.com/COHHIO/Rminor" # The (optional) URL of the GitHub Repo
)
golem::set_golem_options()
usethis::use_agpl3_license("Genelle Denzin")
usethis::use_readme_rmd( open = FALSE )
# Copy MD Content from REPO to this Rmd
usethis::use_lifecycle_badge( "Stable" )
usethis::use_news_md( open = FALSE )
golem::use_recommended_tests()
golem::use_recommended_deps()
golem::remove_favicon()
golem::use_favicon("https://cohhio.org/wp-content/uploads/2020/08/favicon.png")
golem::use_utils_ui()
golem::use_utils_server()
capture.output(sinew::pretty_namespace(fs::path("..", .pkg, "ui", ext = ".R"), force = list(filter = "dplyr::filter", layout = "plotly::layout", group_by = "dplyr::group_by", box = "shinydashboard::box")), file = "ui.R")
capture.output(sinew::pretty_namespace(fs::path("..", .pkg, "server", ext = ".R"), force = list(filter = "dplyr::filter", box = "shinydashboard::box",  layout = "plotly::layout", group_by = "dplyr::group_by", as.data.frame = "base::as.data.frame")), file = "server.R")
# might need to manually change git2r::as.data.frame back to base::as.data.frame in server.R if it changes this incorrectly
#ui.R and server.R need to be copied to the respective locations in R/app_ui.R & R/app_server.R - see those files for examples if need be.
sinew::makeImport("R/app_server.R", cut = 10, desc_loc = "DESCRIPTION") # This doesnt seem to actually update description
# Copy the output to the bottom of the roxygen comments in R/app_ui.R
sinew::makeImport("R/app_ui.R", cut = 10, desc_loc = "DESCRIPTION")
# Copy the output to the bottom of the roxygen comments in R/app_server.R
devtools::document() # This will update description and namespace
usethis::use_data_raw() # Creates the data-raw folder. The file that opens can have the line of code removed. This is where data will be saved to from COHHIO_HMIS
fs::dir_copy(fs::path("..", .pkg, "data"),"data", overwrite = TRUE)
fs::file_copy(fs::path("..", .pkg, "global", ext = ".R"), "R/global.R")
fs::dir_copy(fs::path("..", .pkg, "www"), "inst/app/")
golem::add_resource_path("www", "inst/app/www")
golem::add_utils("helpers")