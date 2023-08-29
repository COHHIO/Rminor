context("golem tests")

library(golem)

test_that("app ui", {
  ui <- app_ui(data_ui = data_ui)
  expect_shinytaglist(ui)
})


test_that("app server", {
  server <- app_server
  expect_is(server, "function")
})

# Configure this test to fit your need

test_that(
  "app launches",{
    skip_on_cran()
    skip_on_travis()
    skip_on_appveyor()
    .fun <- function(find_path)({
      Sys.setenv('TESTTHAT' = 'true')
      pkgload::load_all(find_path('^Rminor$'))
      message(paste0(search(), collapse = ","))
      run_app()
    })
    x <- callr::r_bg(.fun,
                     #, stdout = normalizePath("~/R/Contributor_Repos/COHHIO/tests.log")
                     #, stderr = "2>&1",
                     args = list(find_path = find_path)
    )
    Sys.sleep(5)
    expect_true(x$is_alive())
    x$kill()
  }
)
