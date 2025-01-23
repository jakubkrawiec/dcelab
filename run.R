#' DCE Experiment Preparation Script
#'
#' @description
#' Prepares DCE experiment for deployment
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-12-24

library(yaml)
library(idefix)

source("R/design.R")
source("R/helpers.R")

# Select experiment ID
exp_id <- "example_experiment"

# Set up paths
exp_path <- file.path("experiments", exp_id)
deploy_path <- "app"
resources_path <- file.path(deploy_path, "www")
data_path <- file.path("data/raw", exp_id)

# Create directories
dir.create(resources_path, recursive = TRUE, showWarnings = FALSE)
dir.create(data_path, recursive = TRUE, showWarnings = FALSE)

# Load and validate config
config <- yaml::read_yaml(file.path(exp_path, "config.yaml"))
validate_config(config)

# Process attributes
atts <- read.csv(
  file.path(exp_path, "attributes.csv"), 
  na.strings = c(""), 
  check.names = FALSE, 
  encoding = "UTF-8"
  )

atts_lvls <- as.numeric(lengths(lapply(atts, function(x) x[!is.na(x)])))
atts_coding <- rep("E", length(atts_lvls))

# Generate design
profiles <- Profiles(lvls = atts_lvls, coding = atts_coding)
priors <- generate_priors(profiles)
design <- generate_design(
  lvls = atts_lvls,
  coding = atts_coding,
  n_sets = config$design$n_sets,
  n_alts = config$design$n_alts,
  par_draws = priors$draws,
  alt_cte = config$design$alt_cte
)

# Save design
design_file <- file.path(resources_path, "design.rds")
saveRDS(design, file = design_file)

cat("\nDesign generated and saved")
cat("\nExperiment preparation complete")
cat("\n- Files copied to: ", resources_path)
cat("\n- Data will be saved to: ", data_path)
cat("\n- D-error: ", round(design$error, 4))

# Copy resources
resource_files <- list.files(exp_path, recursive = TRUE, include.dirs = FALSE)
file.copy(
  file.path(exp_path, resource_files),
  file.path(resources_path, resource_files),
  overwrite = TRUE
)

# Deploy to shinyapps.io if configured
if (!is.null(config$deployment) && !is.null(config$deployment$enabled) && config$deployment$enabled) {
  cat("\nDeploying to shinyapps.io...")
  
  # Set up authentication
  if (!is.null(config$deployment$account$token)) {
    rsconnect::setAccountInfo(
      name = config$deployment$account$name,
      token = config$deployment$account$token,
      secret = config$deployment$account$secret
    )
  }
  
  # Set app name if provided (default is exp_id)
  app_name <- if (!is.null(config$deployment$appname)) config$deployment$appname else exp_id
  
  # Deploy application
  rsconnect::deployApp(
    appDir = deploy_path,
    appName = app_name,
    account = config$deployment$account$name,
    launch.browser = FALSE
  )
  
  cat(sprintf("\nDeployed to: https://%s.shinyapps.io/%s",
              config$deployment$account$name,
              app_name))
}
