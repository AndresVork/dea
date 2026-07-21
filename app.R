library(shiny)
library(Benchmarking)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)
library(DT)
library(purrr)

# Load data
load(file = "dfdata.RData")

# Available variables
input_choices <- dfdata |>
  filter(type == "input") |>
  pull(measure) |>
  unique() |>
  sort()

output_choices <- dfdata |>
  filter(type == "output") |>
  pull(measure) |>
  unique() |>
  sort()

year_choices <- dfdata |>
  pull(obstime) |>
  unique() |>
  sort()


# Variable descriptions used in dropdown menus and sidebar
variable_descriptions <- c(
  "LFEXP" = "Life expectancy",
  "HALE" = "Healthy life expectancy",
  "TRTM" = "Treatable mortality (inverse)",
  "AVM" = "Avoidable mortality (inverse)",
  "PREVM" = "Preventable mortality (inverse)",
  "INM" = "Infant mortality (inverse)",
  "MATM" = "Maternal mortality per 100 000 (inverse)",
  "MINU" = "Nurses per 1000",
  "PHYS" = "Doctors per 1000",
  "VQ" = "Health and social workers",
  "HB" = "Hospital beds per 1000",
  "EXP_HEALTH" = "Health expenditure, USD PPP pc"
)

# Function to show both code and description in dropdown,
# but keep only the code as the selected value
make_variable_choices <- function(x) {
  
  labels <- ifelse(
    x %in% names(variable_descriptions),
    paste0(x, " - ", variable_descriptions[x]),
    x
  )
  
  setNames(x, labels)
}

input_choices_labeled <- make_variable_choices(input_choices)
output_choices_labeled <- make_variable_choices(output_choices)


# Default selections
default_inputs <- intersect(c("EXP_HEALTH"), input_choices)
default_outputs <- intersect(c("LFEXP"), output_choices)
default_year <- ifelse(2022 %in% year_choices, 2022, max(year_choices))

# Helper for model-comparison sidebar rows
model_selector_ui <- function(i) {
  tags$details(
    open = if (i == 1) "open" else NULL,
    tags$summary(strong(paste("Model", i))),
    
    checkboxInput(
      inputId = paste0("compare_use_", i),
      label = paste("Use model", i),
      value = i == 1
    ),
    
    selectizeInput(
      inputId = paste0("compare_inputs_", i),
      label = "Input variables",
      choices = input_choices_labeled,
      selected = if (i == 1) default_inputs else character(0),
      multiple = TRUE
    ),
    
    selectizeInput(
      inputId = paste0("compare_outputs_", i),
      label = "Output variables",
      choices = output_choices_labeled,
      selected = if (i == 1) default_outputs else character(0),
      multiple = TRUE
    ),
    
    hr()
  )
}

ui <- navbarPage(
  title = "DEA efficiency analysis",
  
  # ==========================================================
  # TAB 1: Original single-model analysis
  # ==========================================================
  tabPanel(
    title = "Single model",
    
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput(
          inputId = "year",
          label = "Choose year",
          choices = year_choices,
          selected = default_year
        ),
        
        selectizeInput(
          inputId = "inputs",
          label = "Choose input variables",
          choices = input_choices_labeled,
          selected = default_inputs,
          multiple = TRUE
        ),
        
        selectizeInput(
          inputId = "outputs",
          label = "Choose output variables",
          choices = output_choices_labeled,
          selected = default_outputs,
          multiple = TRUE
        ),
        
        radioButtons(
          inputId = "orientation",
          label = "DEA orientation",
          choices = c(
            "Output-oriented" = "out",
            "Input-oriented" = "in"
          ),
          selected = "out"
        ),
        
        actionButton(
          inputId = "run",
          label = "Run DEA analysis",
          class = "btn-primary"
        ),
        
        helpText("Note: DEA outputs should usually be variables where higher is better."),
        
        hr(),
        
        h4("Variable descriptions"),
        helpText("Note: not all variables have all years"),
        
        tags$table(
          class = "table table-sm",
          
          tags$tr(
            tags$th("Code"),
            tags$th("Description")
          ),
          
          tags$tr(tags$td("LFEXP"), tags$td("Life expectancy")),
          tags$tr(tags$td("HALE"), tags$td("Healthy life expectancy")),
          tags$tr(tags$td("TRTM"), tags$td("Treatable mortality (inverse)")),
          tags$tr(tags$td("AVM"), tags$td("Avoidable mortality (inverse)")),
          tags$tr(tags$td("PREVM"), tags$td("Preventable mortality (inverse)")),
          tags$tr(tags$td("INM"), tags$td("Infant mortality (inverse)")),
          tags$tr(tags$td("MATM"), tags$td("Maternal mortality per 100 000 (inverse)")),
          tags$tr(tags$td("MINU"), tags$td("Nurses per 1000")),
          tags$tr(tags$td("PHYS"), tags$td("Doctors per 1000")),
          tags$tr(tags$td("VQ"), tags$td("Health and social workers")),
          tags$tr(tags$td("HB"), tags$td("Hospital beds per 1000")),
          tags$tr(tags$td("EXP_HEALTH"), tags$td("Health expenditure, USD PPP pc"))
        )
      ),
      
      mainPanel(
        
        h3("Efficiency ranking graph"),
        plotOutput("eff_plot", height = "700px"),
        br(),
        
        wellPanel(
          h4("Meaning of table abbreviations"),
          
          tags$p(
            tags$b("eff_vrs"),
            " - efficiency score under variable returns to scale."
          ),
          
          tags$p(
            tags$b("eff_crs"),
            " - efficiency score under constant returns to scale."
          ),
          
          tags$p(
            tags$b("scale_eff"),
            " - scale efficiency, calculated as eff_crs / eff_vrs."
          ),
          
          tags$p(
            tags$b("eff_vrs_raw and eff_crs_raw"),
            " - raw efficiency scores under variable and constant returns to scale. ",
            "They are equal to eff_vrs and eff_crs in input-oriented models, ",
            "and their inverse is used in output-oriented models."
          )
        ),
        
        h3("Efficiency ranking table"),
        DTOutput("eff_table")
        
      )
    )
  ),
  
  # ==========================================================
  # TAB 2: New model-comparison tab
  # ==========================================================
  tabPanel(
    title = "Model comparison",
    
    sidebarLayout(
      
      sidebarPanel(
        
        h4("General model settings"),
        
        selectInput(
          inputId = "compare_year",
          label = "Choose year",
          choices = year_choices,
          selected = default_year
        ),
        
        radioButtons(
          inputId = "compare_orientation",
          label = "DEA orientation",
          choices = c(
            "Output-oriented" = "out",
            "Input-oriented" = "in"
          ),
          selected = "out"
        ),
        
        hr(),
        
        h4("Choose DEA models"),
        helpText("Select up to 10 input/output combinations. Each selected row is one VRS DEA model."),
        
        do.call(
          tagList,
          lapply(1:10, model_selector_ui)
        ),
        
        actionButton(
          inputId = "run_compare",
          label = "Run model comparison",
          class = "btn-primary"
        ),
        
        hr(),
        
        helpText("The comparison uses VRS DEA. Rank 1 means the country is closest to the efficient frontier.")
      ),
      
      mainPanel(
        
        h3("Country ranking heatmap"),
        
        helpText(
          "Rows are countries. Columns are DEA models. ",
          "Cells show the country rank within each model. ",
          "Green = best rank, white = middle rank, red = lowest rank."
        ),
        
        plotOutput("compare_heatmap", height = "750px"),
        
        br(),
        
        h3("Country ranking table"),
        DTOutput("compare_rank_table"),
        
        br(),
        
        h3("Model definitions"),
        DTOutput("compare_model_definitions"),
        
        br(),
        
        h3("Model messages"),
        verbatimTextOutput("compare_messages")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ==========================================================
  # Helper function: run one DEA model
  # ==========================================================
  
  run_one_dea_model <- function(model_id, inputs, outputs, myyear, orientation) {
    
    if (length(inputs) < 1) {
      stop("Choose at least one input variable.")
    }
    
    if (length(outputs) < 1) {
      stop("Choose at least one output variable.")
    }
    
    dfmodel <- dfdata |>
      filter(measure %in% c(inputs, outputs)) |>
      filter(obstime == myyear) |>
      pivot_wider(
        id_cols = c(ref_area, obstime),
        names_from = measure,
        values_from = obsvalue
      ) |>
      filter(if_all(all_of(c(inputs, outputs)), ~ !is.na(.x)))
    
    if (nrow(dfmodel) == 0) {
      stop("No complete country observations for this selection.")
    }
    
    if (nrow(dfmodel) <= length(inputs) + length(outputs)) {
      stop("Too few countries for the number of selected input and output variables.")
    }
    
    X <- as.matrix(dfmodel[, inputs, drop = FALSE])
    Y <- as.matrix(dfmodel[, outputs, drop = FALSE])
    
    if (any(X < 0, na.rm = TRUE)) {
      stop("Input variables contain negative values.")
    }
    
    if (any(Y < 0, na.rm = TRUE)) {
      stop("Output variables contain negative values.")
    }
    
    dea_vrs <- dea(
      X,
      Y,
      RTS = "vrs",
      ORIENTATION = orientation
    )
    
    eff_vrs_raw <- efficiencies(dea_vrs)
    
    # Convert to intuitive 0-1 efficiency score
    # Input-oriented scores are already 0-1.
    # Output-oriented scores are usually >= 1, so inverse is used.
    if (orientation == "out") {
      eff_vrs <- 1 / eff_vrs_raw
    } else {
      eff_vrs <- eff_vrs_raw
    }
    
    dfmodel |>
      mutate(
        model = paste0("Model ", model_id),
        model_id = model_id,
        eff_vrs = eff_vrs,
        eff_vrs_raw = eff_vrs_raw
      ) |>
      arrange(desc(eff_vrs), ref_area) |>
      mutate(
        rank_vrs = min_rank(desc(eff_vrs))
      ) |>
      select(
        model,
        model_id,
        ref_area,
        obstime,
        rank_vrs,
        eff_vrs,
        eff_vrs_raw,
        all_of(inputs),
        all_of(outputs)
      )
  }
  
  # ==========================================================
  # TAB 1: Original single-model server logic
  # ==========================================================
  
  dea_results <- eventReactive(input$run, {
    
    req(input$year)
    req(input$inputs)
    req(input$outputs)
    req(input$orientation)
    
    inputs <- input$inputs
    outputs <- input$outputs
    myyear <- input$year
    
    shiny::validate(
      need(length(inputs) >= 1, "Choose at least one input variable."),
      need(length(outputs) >= 1, "Choose at least one output variable.")
    )
    
    dfmodel <- dfdata |>
      filter(measure %in% c(inputs, outputs)) |>
      filter(obstime == myyear) |>
      pivot_wider(
        id_cols = c(ref_area, obstime),
        names_from = measure,
        values_from = obsvalue
      ) |>
      filter(if_all(all_of(c(inputs, outputs)), ~ !is.na(.x)))
    
    shiny::validate(
      need(nrow(dfmodel) > 0, "No complete country observations for this selection."),
      need(
        nrow(dfmodel) > length(inputs) + length(outputs),
        "Too few countries for the number of selected input and output variables."
      )
    )
    
    X <- as.matrix(dfmodel[, inputs, drop = FALSE])
    Y <- as.matrix(dfmodel[, outputs, drop = FALSE])
    
    shiny::validate(
      need(all(X >= 0), "Input variables contain negative values."),
      need(all(Y >= 0), "Output variables contain negative values.")
    )
    
    # CRS DEA
    dea_crs <- dea(
      X,
      Y,
      RTS = "crs",
      ORIENTATION = input$orientation
    )
    
    # VRS DEA
    dea_vrs <- dea(
      X,
      Y,
      RTS = "vrs",
      ORIENTATION = input$orientation
    )
    
    # Raw efficiency scores from Benchmarking
    eff_crs_raw <- efficiencies(dea_crs)
    eff_vrs_raw <- efficiencies(dea_vrs)
    
    # Convert to intuitive 0-1 efficiency scores
    if (input$orientation == "out") {
      eff_crs <- 1 / eff_crs_raw
      eff_vrs <- 1 / eff_vrs_raw
    } else {
      eff_crs <- eff_crs_raw
      eff_vrs <- eff_vrs_raw
    }
    
    scale_eff <- eff_crs / eff_vrs
    
    eff_table <- dfmodel |>
      mutate(
        eff_vrs = eff_vrs,
        eff_crs = eff_crs,
        scale_eff = scale_eff,
        eff_vrs_raw = eff_vrs_raw,
        eff_crs_raw = eff_crs_raw
      ) |>
      arrange(desc(eff_vrs), desc(ref_area)) |>
      mutate(rank_vrs = row_number()) |>
      select(
        rank_vrs,
        ref_area,
        obstime,
        eff_vrs,
        eff_crs,
        scale_eff,
        eff_vrs_raw,
        eff_crs_raw,
        all_of(inputs),
        all_of(outputs)
      )
    
    eff_table
  })
  
  output$eff_table <- renderDT({
    
    req(input$run)
    
    dea_results() |>
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 25,
          scrollX = TRUE
        )
      ) |>
      formatRound(
        columns = c("eff_vrs", "eff_crs", "scale_eff", "eff_vrs_raw", "eff_crs_raw"),
        digits = 3
      )
  })
  
  output$eff_plot <- renderPlot({
    
    req(input$run)
    
    eff_table <- dea_results() |>
      mutate(
        label_y = pmin(eff_vrs + 0.025, 1.06),
        label_text = percent(eff_vrs, accuracy = 0.1)
      )
    
    orientation_title <- ifelse(
      input$orientation == "out",
      "Output-oriented",
      "Input-oriented"
    )
    
    ggplot(
      eff_table,
      aes(
        x = fct_reorder(ref_area, eff_vrs),
        y = eff_vrs
      )
    ) +
      geom_col() +
      geom_text(
        aes(
          y = label_y,
          label = label_text
        ),
        hjust = 0,
        size = 3
      ) +
      coord_flip() +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        limits = c(0, 1.10),
        breaks = seq(0, 1, by = 0.25),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        x = NULL,
        y = "VRS efficiency score",
        title = paste0(orientation_title, " DEA efficiency by country, ", input$year),
        subtitle = "VRS model; 1 = efficient frontier"
      ) +
      theme_minimal()
  })
  
  # ==========================================================
  # TAB 2: Model-comparison server logic
  # ==========================================================
  
  compare_results <- eventReactive(input$run_compare, {
    
    req(input$compare_year)
    req(input$compare_orientation)
    
    myyear <- input$compare_year
    orientation <- input$compare_orientation
    
    selected_models <- tibble(
      model_id = 1:10,
      use_model = map_lgl(
        1:10,
        ~ isTRUE(input[[paste0("compare_use_", .x)]])
      ),
      inputs = map(
        1:10,
        ~ input[[paste0("compare_inputs_", .x)]]
      ),
      outputs = map(
        1:10,
        ~ input[[paste0("compare_outputs_", .x)]]
      )
    ) |>
      filter(use_model)
    
    shiny::validate(
      need(nrow(selected_models) >= 1, "Select at least one model.")
    )
    
    results_list <- list()
    messages <- character(0)
    
    withProgress(
      message = "Running DEA models",
      value = 0,
      {
        
        for (j in seq_len(nrow(selected_models))) {
          
          model_id <- selected_models$model_id[j]
          inputs <- selected_models$inputs[[j]]
          outputs <- selected_models$outputs[[j]]
          
          incProgress(
            amount = 1 / nrow(selected_models),
            detail = paste("Running model", model_id)
          )
          
          model_result <- tryCatch(
            {
              run_one_dea_model(
                model_id = model_id,
                inputs = inputs,
                outputs = outputs,
                myyear = myyear,
                orientation = orientation
              )
            },
            error = function(e) {
              messages <<- c(
                messages,
                paste0("Model ", model_id, ": ", e$message)
              )
              NULL
            }
          )
          
          if (!is.null(model_result)) {
            results_list[[paste0("Model ", model_id)]] <- model_result
            messages <- c(
              messages,
              paste0("Model ", model_id, ": OK")
            )
          }
        }
      }
    )
    
    shiny::validate(
      need(length(results_list) >= 1, "No model could be estimated. Check selected variables.")
    )
    
    all_results <- bind_rows(results_list)
    
    rank_table <- all_results |>
      select(ref_area, model, rank_vrs) |>
      pivot_wider(
        names_from = model,
        values_from = rank_vrs
      ) |>
      arrange(ref_area)
    
    model_definitions <- selected_models |>
      mutate(
        model = paste0("Model ", model_id),
        inputs = map_chr(inputs, ~ paste(.x, collapse = ", ")),
        outputs = map_chr(outputs, ~ paste(.x, collapse = ", "))
      ) |>
      select(model, inputs, outputs)
    
    list(
      all_results = all_results,
      rank_table = rank_table,
      model_definitions = model_definitions,
      messages = messages,
      year = myyear,
      orientation = orientation
    )
  })
  
  output$compare_rank_table <- renderDT({
    
    req(input$run_compare)
    
    compare_results()$rank_table |>
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 30,
          scrollX = TRUE
        )
      )
  })
  
  output$compare_model_definitions <- renderDT({
    
    req(input$run_compare)
    
    compare_results()$model_definitions |>
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "t"
        )
      )
  })
  
  output$compare_messages <- renderText({
    
    req(input$run_compare)
    
    paste(compare_results()$messages, collapse = "\n")
  })
  
  output$compare_heatmap <- renderPlot({
    
    req(input$run_compare)
    
    heatmap_data <- compare_results()$all_results |>
      select(ref_area, model, rank_vrs) |>
      group_by(ref_area) |>
      mutate(mean_rank = mean(rank_vrs, na.rm = TRUE)) |>
      ungroup()
    
    max_rank <- max(heatmap_data$rank_vrs, na.rm = TRUE)
    
    ggplot(
      heatmap_data,
      aes(
        x = model,
        y = fct_reorder(ref_area, mean_rank, .desc = TRUE),
        fill = rank_vrs
      )
    ) +
      geom_tile(color = "grey80") +
      geom_text(
        aes(label = rank_vrs),
        size = 3
      ) +
      scale_fill_gradient2(
        low = "green3",
        mid = "white",
        high = "red3",
        midpoint = (max_rank + 1) / 2,
        limits = c(1, max_rank),
        name = "Rank"
      ) +
      labs(
        x = NULL,
        y = NULL,
        title = paste0("Country ranks across VRS DEA models, ", compare_results()$year),
        subtitle = ifelse(
          compare_results()$orientation == "out",
          "Output-oriented models; rank 1 = highest VRS efficiency",
          "Input-oriented models; rank 1 = highest VRS efficiency"
        )
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}

shinyApp(ui = ui, server = server)