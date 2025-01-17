#' Discrete Choice Experiment Shiny Application
#'
#' @description
#' Main Shiny application for running DCE experiments
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-12-24

library(shiny)
library(shinyjs)
library(aws.s3)
library(tidyverse)
library(idefix)
library(tmvtnorm)

source("utils.R")

# Load configuration and resources
resources_path <- "www"
config <- yaml::read_yaml(file.path(resources_path, "config.yaml"))
atts <- read.csv(file.path(resources_path, "attributes.csv"),
                 na.strings = c(""),
                 check.names = FALSE,
                 encoding = "UTF-8")
design <- readRDS(file.path(resources_path, "design.rds"))
drop_token <- readRDS(file.path(resources_path, "droptoken.rds"))

Sys.setenv(
  AWS_ACCESS_KEY_ID = config$storage$s3$access_key,
  AWS_SECRET_ACCESS_KEY = config$storage$s3$secret_key,
  AWS_DEFAULT_REGION = config$storage$s3$region
)

# Load custom functions if configured
custom_funcs <- NULL
if (!is.null(config$custom_attributes)) {
  custom_funcs <- load_custom_functions(config, resources_path)
}

# Process attributes
atts_names <- colnames(atts)
atts_labs <- lapply(atts, function(x) x[!is.na(x)])
atts_lvls <- as.numeric(lengths(atts_labs))
names(atts_lvls) <- atts_names
atts_coding <- rep("E", length(atts))
n_atts <- length(atts_labs)

# Initialize design parameters
bs <- seq(1, (nrow(design$design) - config$design$n_alts + 1), config$design$n_alts)
es <- c((bs - 1), nrow(design$design))[-1]

# Set app name if provided (default is exp_id)
app_name <- if (!is.null(config$deployment$appname)) config$deployment$appname else config$exp_id

# Define UI
ui <- fluidPage(
  title = app_name,
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .choice-table td {
        vertical-align: middle !important;
        text-align: center !important;
      }
      .choice-table th:first-child {
        text-align: left !important;
      }
      .choice-table td:first-child {
        text-align: left !important;
      }
    "))
  ),
  column(12, align = "center",
         style = "padding-top:200px; padding-bottom:5px",
         textOutput("set_nr")),
  column(12, align = "center",
         tableOutput("choice_set")),
  column(12, align = "center",
         uiOutput("buttons")),
  column(12, align = "center",
         style = "padding-bottom:10px",
         htmlOutput("intro")),
  column(12, align = "center",
         style = "padding-bottom:10px",
         htmlOutput("end")),
  column(12, align = "center",
         actionButton("OK", "OK"))
)

# Define server
server <- function(input, output, session) {
  # Initialize reactive values
  V <- reactiveValues(
    sn = 0,
    fulldes = design$design,
    choice_set = NA,
    choice_sets = matrix(NA, nrow = (config$design$n_sets * config$design$n_alts),
                         ncol = n_atts),
    custom_funcs = custom_funcs,
    sdata = list(),
    survey_data = list(),
    y_bin = numeric(),
    resp = character(),
    atts_labs = atts_labs
  )

  # Display intro text
  output$intro <- renderText(
    readLines(file.path(resources_path, "intro.txt"), encoding = "UTF-8")
  )

  # Enable OK button after choice
  observeEvent(input$survey, {
    enable("OK")
  })

  # Handle OK button clicks
  observeEvent(input$OK, {
    disable("OK")

    # Update set counter
    V$sn <- V$sn + 1

    # Clear intro text after first click
    if (V$sn == 1) {
      output$intro <- renderText(NULL)
    }

    # Update set number display
    if (V$sn <= config$design$n_total) {
      output$set_nr <- renderText(
        paste("Choice:", V$sn, "/", config$design$n_total)
      )
    } else {
      output$set_nr <- renderText(NULL)
    }

    # Handle choice sets
    if (V$sn <= config$design$n_total) {
      current_set <- select_choice_set(
        V = V,
        design = design$design,
        bs = bs,
        es = es,
        atts = atts,
        atts_lvls = atts_lvls,
        atts_coding = atts_coding,
        n_atts = n_atts,
        config = config
      )

      output$choice_set <- renderTable(
        current_set,
        rownames = TRUE,
        sanitize.text.function = function(x) x,
        class = "choice-table",
        align = paste0('l', paste0(rep('c', ncol(current_set)), collapse = ''))
      )

      if (V$sn == 1) {
        V$choice_sets <- current_set
      } else {
        V$choice_sets <- rbind(V$choice_sets, current_set)
      }

    } else {
      output$choice_set <- renderTable(NULL)
      if (V$sn == (config$design$n_total + 1)) {
        output$end <- renderText(
          readLines(file.path(resources_path, "outro.txt"), encoding = "UTF-8")
        )
        shinyjs::delay(100, enable("OK"))
      }
    }

    # Store response data
    if (V$sn > 1 && V$sn <= (config$design$n_total + 1)) {
      V$resp <- c(V$resp, input$survey)
      V$y_bin <- convert_responses(
        V$resp,
        config$design$alternatives,
        config$design$n_alts
      )
      V$sdata$bin_responses <- V$y_bin
      V$sdata$responses <- V$resp
      V$sdata$design <- V$fulldes
      V$sdata$survey <- V$choice_sets
      V$survey_data <- V$sdata
    }

    # Save final data
    if (V$sn == (config$design$n_total + 1)) {
      save_experiment_data(
        data = V$survey_data,
        config = config,
        exp_id = config$exp_id,
        n_atts = n_atts
      )
    }

    # Handle completion
    if (V$sn > (config$design$n_total + 1)) {
      shinyjs::runjs(sprintf("window.location.href='%s'", config$completion$url))
      stopApp()
    }
  })

  # Display choice buttons
  output$buttons <- renderUI({
    if (V$sn > 0 && V$sn <= config$design$n_total) {
      radioButtons(
        "survey",
        config$ui$buttons_text,
        config$design$alternatives,
        inline = TRUE,
        selected = "None"
      )
    }
  })
}

# Run application
shinyApp(ui = ui, server = server)
