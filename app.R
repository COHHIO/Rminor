# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

install.packages(".", repos = NULL, type = "source")
options(golem.app.prod = TRUE)
# golem::add_resource_path("www", "inst/app/www")
Rminor::run_app() # add parameters here (if any)
