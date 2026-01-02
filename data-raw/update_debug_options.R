# This script can be used to extract and update the list of available debug
# options of the LifeInsureR package. It scans all .R files for
# getoption("LIC.debug.*", extracts the option names and stores them as
# a package data set. This should be done whenever an option is added or renamed.
#
# This file also updates the list of available options that will be included in
# the package's vignette.
#
# The LIC_debug_options are used when installed bye the LIC_debug method that lets
# the "user" (developer of code using the LifeInsureR package) select which
# debug methods should be turned on.

library(here)

LIC_debug_options = extract_lic_debug_options(pkg_path = ".")
usethis::use_data(LIC_debug_options, internal = TRUE, overwrite = TRUE)

lines = c(
  "### Available debug options",
  "",
  paste0("- `", LIC_debug_options, "`")
)
writeLines(lines, "vignettes/debug-options.md")


message("Wrote internal index to R/sysdata.rda and vignette fragment to vignettes/debug-options.md")
