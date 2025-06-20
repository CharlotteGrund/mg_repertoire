
# Run all scripts for results on individual repertoire size and gesturing rate section ---- 

# clear environment
rm(list = ls())

# set path
script_folder <- "03_scripts/03_irs_igr"

# list all R scripts in alphabetical order, excluding this script
scripts <- list.files(
  path = script_folder,
  pattern = "\\.R$",
  full.names = TRUE
)

# exclude run all script
this_script <- normalizePath("03_scripts/03_irs_igr/00_run_all.R")
scripts <- scripts[normalizePath(scripts) != this_script]
# sort scripts
scripts <- sort(scripts)

# run each script (plots are printed)
for (script in scripts) {
  message("Running: ", script)
  tryCatch({
    withVisible(source(script, echo = TRUE))
  }, error = function(e) {
    message("Error in script ", script, ": ", e$message)
  })
}
