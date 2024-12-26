# Toolkit for building and deploying Discrete Choice Experiments (DCE) with Shiny

Configurable framework for running Discrete Choice Experiments (DCE) using R Shiny.

## Project Structure

```
.
├── app/                    # Deployment directory
│   ├── app.R               # Main Shiny application
│   ├── utils.R             # Helper functions
│   └── resources/          # Generated experiment resources
├── data/                   # Data directory
│   ├── raw/                # Raw experiment data
│   └── processed/          # Processed results
├── experiments/            # Experiment definitions
│   └── exp1/               # Specific experiment
│       ├── config.yaml     # Experiment configuration
│       ├── droptoken.rds   # Dropbox authentication
│       ├── attributes.csv  # Attributes definition
│       ├── intro.txt       # Opening text
│       └── outro.txt       # Closing text
├── R/                      # Core functions
│   └── design.R            # Design generation functions
└── run.R                   # Experiment preparation script
```

## Requirements

- R >= 4.0.0
- Required packages:

```r
install.packages(c(
  "shiny",
  "shinyjs",
  "tidyverse",
  "idefix",
  "rdrop2",
  "tmvtnorm",
  "yaml"
))
```

## Features

- Dynamic choice experiment interface
- D-optimal experimental design generation
- Binary and multinomial choice options
- Automatic data storage to Dropbox
- Progress tracking and completion redirect
- Configurable attributes and levels

## Getting Started

1. Clone the repository
2. Install required packages
3. Create your experiment directory following `exp1` template
4. Configure Dropbox authentication
5. Run preparation script
6. Launch the Shiny app

## Setup

1. Configure Dropbox authentication:

```r
token <- rdrop2::drop_auth()
saveRDS(token, "experiments/exp1/droptoken.rds")
```

2. Configure your experiment in `experiments/exp1/config.yaml`:

```yaml
# Unique identifier for the experiment
exp_id: "exp1"

# Experimental design parameters
design:
  n_sets: 35             # Total number of choice sets
  n_total: 35            # Number of sets to present
  n_alts: 2              # Number of alternatives
  alternatives:          # Alternative labels
    - "Option A"
    - "Option B"
  alt_cte: [0, 0]       # Alternative-specific constants
  no_choice: null       # No-choice option (null = disabled)

# User interface settings
ui:
  buttons_text: "Which option do you prefer?"

# Data storage configuration
storage:
  dropbox:
    base_path: "Projects/dce-experiments"  # Base path in Dropbox
    data_path: "data/base"                 # Directory for experiment data

# Post-experiment settings
completion:
  url: "https://psychodpt.fra1.qualtrics.com/jfe/form/SV_5omLD5XXcDF59pc"  # Redirect URL
```

3. Define attributes in `attributes.csv` with labels for:
   - Monthly savings contribution
   - Number of participating peers
   - Employer contribution
   - Salary increase provisions
   - Current account balance
   - Government contribution

## Deployment

1. Prepare the experiment:

```r
source("run.R")
```

This script will:
- Create necessary directories
- Copy configuration files
- Generate D-optimal design (if needed)
- Set up data paths

2. Run the application:

```r
shiny::runApp("app")
```

## Data Storage

Response data is automatically saved to Dropbox in two formats:
- Numeric data: Binary choice responses with design matrix (`{timestamp}_num_data.txt`)
- Character data: Text responses with attribute levels (`{timestamp}_char_data.txt`)

## Experiment Preparation

To set up a new experiment:

1. Create a new experiment directory in `experiments/` (e.g., `experiments/exp1/`) containing:

- `config.yaml`: Experiment configuration
- `attributes.csv`: Attribute definitions
- `intro.txt`: Introduction text
- `outro.txt`: Closing text
- `droptoken.Rds`: Dropbox authentication token

2. Run the preparation script `run.R`:

The script will:

- Create necessary directories (`app/resources/`, `data/raw/exp1/`)
- Copy experiment resources to the deployment directory
- Generate and save the experimental design (if not exists)
- Set up data paths

The design generation uses helper functions from `R/design.R` and will only occur if `design.Rds` doesn't exist in the resources directory.

> Check `run.R` for implementation details and `R/design.R` for design generation helper functions.

## License

This project is licensed under the MIT License - see [LICENSE](LICENSE) for details.

## Authors

Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>\
Jakub Krawiec, PhD <krawiecjm@gmail.com>
