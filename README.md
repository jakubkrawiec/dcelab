# Toolkit for building and deploying Discrete Choice Experiments (DCE) with Shiny

Configurable framework for running Discrete Choice Experiments (DCE) using R Shiny.
Built upon the [idefix](https://github.com/traets/idefix) R package for generating
D-efficient designs, it adds custom decision attributes, experiment configuration, 
web deployment infrastructure, and automated data collection.

## Project Structure

```
.
├── app/                      # Deployment directory
│   ├── app.R                 # Main Shiny application
│   ├── utils.R               # Helper functions
│   └── www/                  # Experiment resources
├── data/                     # Data directory
│   ├── raw/                  # Raw experiment data
│   └── processed/            # Processed results
├── experiments/              # Experiment definitions
│   └── example_experiment/   # Specific experiment
│       ├── config.yaml       # Experiment configuration
│       ├── attributes.csv    # Attributes definition
│       ├── custom.csv        # Custom attribute functions
│       ├── intro.txt         # Opening text
│       └── outro.txt         # Closing text
├── R/                        # Core functions
│   ├── design.R              # Design generation functions
│   └── helpers.R             # Helper functions
└── run.R                     # Experiment preparation script
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
  "aws.s3",
  "withr",
  "yaml",
  "rsconnect"
))
```

## Features

- Dynamic choice experiment interface
- D-optimal experimental design generation
- Binary and multinomial choice options
- Configurable data storage to cloud (AWS S3)
- Progress tracking and completion redirect
- Configurable attributes and levels
- Local and shinyapps.io deployment options

## Getting Started

1. Clone the repository
2. Install required packages
3. Create your experiment directory following `example_experiment` template
4. Configure cloud authentication
5. Configure shinyapps.io deployment (optional)
6. Run preparation script
7. Deploy the Shiny app (locally or to shinyapps.io)

## Setup

### 1. Configure AWS S3 storage:

Set up your S3 bucket and obtain AWS credentials. Then configure the storage section in your `config.yaml`:

```r
# Data storage configuration
# Available providers: s3
storage:
  s3:
    bucket: bucket-name                         # S3 bucket name
    prefix: data/raw                            # Path prefix for data storage
    region: your-region                         # AWS region
    access_key: your-access-key                 # AWS access key
    secret_key: your-secret-key                 # AWS secret key
```

### 2. Configure your experiment in `experiments/example_experiment/config.yaml`:

```yaml
# DCE Experiment Configuration
#
# YAML Formatting Rules:
# - Quote text with spaces/special chars: "Option A", "$5/month"
# - Leave unquoted: numbers, booleans (true/false), null, simple-text
#
# Structure:
# 1. exp_id             - Experiment identifier
# 2. design             - Core experiment settings
# 3. ui                 - Interface settings
# 4. storage            - Data management
# 5. completion         - Post-experiment handling
# Optional:
# - custom_attributes   - Display functions
# - deployment          - shinyapps.io settings

# Unique identifier for this experiment
exp_id: example_experiment

# Core experimental design configuration
design:
  n_sets: 30                                    # Total number of choice sets in the design
  n_total: 30                                   # Number of sets each participant will see
  n_alts: 2                                     # Number of alternatives per choice set
  alternatives:                                 # Labels shown for each alternative
    - "Option A"                                # First alternative label
    - "Option B"                                # Second alternative label
  alt_cte: [0, 0]                               # Alternative-specific constants (0 = no constant)
  no_choice: null                               # Optional no-choice option

# User interface customization
ui:
  buttons_text: "Which option do you prefer?"   # Text shown above choice buttons
  shuffle_attributes: false                     # Whether to randomize attribute order

# Custom attribute definitions (optional)
custom_attributes:
  display_image:
    function_name: "display_image"              # Function name in custom.R
    attribute_label: "Displayed image"          # Label shown in the interface

  display_text:
    function_name: "display_majority_choice"    # Function name in custom.R
    attribute_label: " "                        # Label shown in the interface

# Data storage configuration (optional)
# Available providers: s3
storage:
  s3:
    bucket: bucket-name                         # S3 bucket name
    prefix: data/raw                            # Path prefix for data storage
    region: your-region                         # AWS region
    access_key: your-access-key                 # AWS access key
    secret_key: your-secret-key                 # AWS secret key

# Post-experiment settings
completion:
  url: "your-redirect-url"                      # Redirect URL after completion

# Deployment configuration
deployment:
  enabled: false                                # Whether to auto-deploy
  appname: null                                 # Custom application name
  account:
    name: your-account                          # Account name
    token: your-token                           # Account token
    secret: your-secret                         # Account secret
```

### 3. Define attributes (columns) and their levels (rows) in `attributes.csv`

#### Attributes Configuration

Define experiment attributes in `attributes.csv`:
- Each column represents an attribute
- Each row represents possible levels for that attribute
- Empty cells are ignored
- UTF-8 encoding required

Example:

```csv
How much would I save monthly from my salary?,Number of your friends participating in savings program,What amount would the employer contribute?,Is there a raise planned in the employment contract?,Current savings in your account,What amount would the State contribute?,Displayed image
1%,None of your friends currently uses the savings program,1% of monthly salary,No planned raise,Less than 1000,0% of monthly salary (No State participation),1
2%,10% of your friends currently uses the savings program,2% of monthly salary,Planned raise of 10% salary after 3-month probation period,1000-5000,1% of monthly salary,2
5%,30% of your friends currently uses the savings program,5% of monthly salary,Planned raise of 10% salary after 6-month probation period,5000-10000,2% of monthly salary,3
```

#### Custom Attributes

This toolkit supports custom attribute display functions configured in `config.yaml`:

```yaml
custom_attributes:
  display_image:
    function_name: "display_image"
    attribute_label: "Displayed image"
  display_text:
    function_name: "display_majority_choice"
    attribute_label: " "
```

> `attribute_label` must match attribute label (column) defined in `attributes.csv` for design-dependent custom attributes.
> `function_name` must match the corresponding function name defined in `custom.R`.

Custom attribute display functions can be defined in `experiments/{exp_id}/custom.R`. Each function receives a `context` object containing:

- `choice_set`: The current choice set matrix
- `col_index`: Current column being processed
- `config`: Configuration parameters
- `alternatives`: Available alternatives

> Custom attribute functions must return a character string (HTML allowed)

Example custom function:
```r
display_image <- function(context) {
  level <- as.numeric(context$choice_set["Displayed image", context$col_index])
  if (!is.numeric(level) || level < 1 || level > 3) return("")
  sprintf('<img src="image%d.svg" style="width:48px;height:48px;">', level)
}
```

## Deployment

### Local Deployment

Run the preparation script and launch the application locally:

```r
source("run.R")                               # Prepare experiment
shiny::runApp("app")                          # Run locally
```

### shinyapps.io Deployment

1. Set up shinyapps.io account and obtain credentials from the dashboard
2. Configure deployment in `config.yaml`:
   - Set `deployment.enabled: true`
   - Add account credentials
   - Optionally specify custom app name
3. Run the preparation script:

```r
   source("run.R")                            # Will prepare and deploy if enabled
```

The preparation script will:
- Create necessary directories
- Copy configuration files
- Generate D-optimal design
- Set up data paths
- Deploy to shinyapps.io (if enabled)

## Data Storage

Response data is automatically saved to the configured storage provider. When using AWS S3,
data is saved as text files containing responses with attribute levels, using the format
`{experiment_id}_{timestamp}.txt`. The storage location within the S3 bucket follows the
pattern `{prefix}/{experiment_id}/{filename}`. By default, the prefix is set to
`data/raw` in the example configuration, but you can adjust this path in the storage
settings to match your needs.

## Experiment Preparation

To set up a new experiment:

1. Create a new experiment directory in `experiments/` (e.g., `experiments/example_experiment/`) containing:
   - `config.yaml`: Experiment configuration
   - `attributes.csv`: Attribute definitions
   - `intro.txt`: Introduction text
   - `outro.txt`: Closing text
   - `custom.R`: Custom attribute display functions (if using custom attributes)

2. Run the preparation script `run.R`:
   - Creates necessary directories (`app/www/`, `data/raw/example_experiment/`)
   - Copies experiment resources to the deployment directory
   - Generates and saves the experimental design
   - Sets up data paths
   - Deploys to shinyapps.io (if configured)

The design generation uses helper functions from `R/design.R`.

> Check `run.R` for implementation details and `R/design.R` for design generation helper functions.

## License

This project is licensed under the GNU General Public License v3.0 - see [LICENSE](LICENSE) for details.

## Authors

Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>

Jakub Krawiec, PhD <krawiecjm@gmail.com>
