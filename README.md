
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rminor

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

An open-source HMIS custom reporting Shiny app created by Ohio Balance
of State CoC.

This repository would be helpful to any HUD-designated Continuum of Care
looking for a way to get more out of their HMIS data.

### About

This is an open source project released under the GNU AGPLv3 license.
See LICENSE for more details or visit the official GNU page at
<http://www.gnu.org/licenses/agpl-3.0.html>.

All the code in this repository is written using R and R Studio. Please
consult the book [R for Data Science](https://r4ds.had.co.nz/) for help
getting started with R and R Studio.

### Data Source

This app takes the .Rdata files from the COHHIO\_HMIS project (see that
repo for details) via a symbolic link. This way, processing happens
once, somewhere else, and not so much in the app.

If you clone or fork this, create a data folder in your R project and
set up symbolic links to the .RData files in your COHHIO\_HMIS/images
directory.

### Security

No HMIS data is ever included in this repository. To make this code
work, you will need to supply your own HMIS data. You are responsible
for securing your HUD CSV export on your computer and ensuring that it
is not compromised using whatever security measures you use for that
locally.
