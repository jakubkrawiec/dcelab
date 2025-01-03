#' DCE Experiment Preparation Script
#'
#' @description
#' Prepares DCE experiment for deployment by:
#' - Creating deployment directory
#' - Copying required files
#' - Generating experimental design
#' - Setting up data paths
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-12-24

# Load packages
library(yaml)
library(idefix)

# Local source
source("R/design.R")

# Set experiment ID
exp_id <- "exp1"  # Specify experiment identifier

# Set up paths
exp_path <- file.path("experiments", exp_id)
deploy_path <- "app"
resources_path <- file.path(deploy_path, "resources")
data_path <- file.path("data/raw", exp_id)

# Create directories (if they don't exist)
dir.create(resources_path, recursive = TRUE, showWarnings = FALSE)
dir.create(data_path, recursive = TRUE, showWarnings = FALSE)

# Load config and attributes
config <- yaml::read_yaml(file.path(exp_path, "config.yaml"))
atts <- read.csv(file.path(exp_path, "attributes.csv"), na.strings = c(""), check.names = FALSE, encoding = "UTF-8")

# Define experiment resources
exp_files <- c("droptoken.rds", "config.yaml", "attributes.csv", "intro.txt", "outro.txt")

# Copy resources
for (file in exp_files) {
  file.copy(
    file.path(exp_path, file),
    file.path(resources_path, file),
    overwrite = TRUE
  )
}

# Process attributes
atts_lvls <- as.numeric(lengths(lapply(atts, function(x) x[!is.na(x)])))
atts_coding <- rep("E", length(atts_lvls))

# Generate design
profiles <- Profiles(lvls = atts_lvls, coding = atts_coding)
priors <- generate_priors(profiles)
des <- generate_design(
  lvls = atts_lvls,
  coding = atts_coding,
  n_sets = config$design$n_sets,
  n_alts = config$design$n_alts,
  par_draws = priors$draws,
  alt_cte = config$design$alt_cte
)

# Save design to resources path
des_file <- file.path(resources_path, "design.rds")
saveRDS(des, file = des_file)

cat("\nDesign generated and saved")
cat("\nExperiment preparation complete")
cat("\n- Files copied to: ", resources_path)
cat("\n- Data will be saved to: ", data_path)
cat("\n- D-error: ", round(des$error, 4))
