library(shiny)
library(Benchmarking)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)
library(DT)

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

ui <- fluidPage(
  
  titlePanel("DEA efficiency analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId = "year",
        label = "Choose year",
        choices = year_choices,
        selected = ifelse(2022 %in% year_choices, 2022, max(year_choices))
      ),
      
      selectizeInput(
        inputId = "inputs",
        label = "Choose input variables",
        choices = input_choices,
        selected = intersect(c("MINU", "EXP_HEALTH", "PHYS"), input_choices),
        multiple = TRUE
      ),
      
      selectizeInput(
        inputId = "outputs",
        label = "Choose output variables",
        choices = output_choices,
        selected = intersect(c("LFEXP", "TRTM"), output_choices),
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
    
    tags$table(
      class = "table table-sm",
      
      tags$tr(
        tags$th("Code"),
        tags$th("Description")
      ),
      
      tags$tr(tags$td("LFEXP"), tags$td("Life expectancy")),
      tags$tr(tags$td("TRTM"), tags$td("Treatable mortality (inverse)")),
      tags$tr(tags$td("AVM"), tags$td("Avoidable mortality (inverse)")),
      tags$tr(tags$td("PREVM"), tags$td("Preventable mortality (inverse)")),
      tags$tr(tags$td("MINU"), tags$td("Nurses")),
      tags$tr(tags$td("PHYS"), tags$td("Doctors")),
      tags$tr(tags$td("VQ"), tags$td("Health and social workers")),
      tags$tr(tags$td("EXP_HEALTH"), tags$td("Health expenditure, USD PPP pc"))
    )
    ),
    
    mainPanel(
      h3("Efficiency ranking table"),
      DTOutput("eff_table"),
      
      br(),
      
      h3("Efficiency ranking graph"),
      plotOutput("eff_plot", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  dea_results <- eventReactive(input$run, {
    
    req(input$year)
    req(input$inputs)
    req(input$outputs)
    req(input$orientation)
    
    inputs <- input$inputs
    outputs <- input$outputs
    myyear <- input$year
    
    validate(
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
    
    validate(
      need(nrow(dfmodel) > 0, "No complete country observations for this selection."),
      need(nrow(dfmodel) > length(inputs) + length(outputs),
           "Too few countries for the number of selected input and output variables.")
    )
    
    X <- as.matrix(dfmodel[, inputs])
    Y <- as.matrix(dfmodel[, outputs])
    
    validate(
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
    # Input-oriented scores are already 0-1.
    # Output-oriented scores are usually >= 1, so take inverse.
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
      arrange(desc(eff_vrs)) |>
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
  
}

shinyApp(ui = ui, server = server)