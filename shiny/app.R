library(shiny)
library(dplyr)
library(plotly)
library(stringr)
library(shinyWidgets)
library(leaflet)
library(terra)
library(tidyterra)
library(ggplot2)
library(htmltools)  
library(bslib)
library(tools)
library(scales)
#setwd("output/shiny")
# data prep ---------------------------------------------------------------

# load data 

# load data 
data_type <- readRDS("data_LUH2_GCB2025.rds")
saveRDS(data_type,"data_LUH2_GCB2025.rds", compress = "xz")

?saveRDS
cols <- c(
  # Abandoned land
  "Secondary land" = "#DCD4D0",
  "High" = "#01665e",
  "Medium" = "#35978f",
  "Low"  = "#80cdc1",
  "Cropland" = "#8c510a",
  "Pasture" = "#dfc27d",
  "Plantation" = "#bf812d",
  "Abandoned" = "#DCD4D0",
  "Brazil" =  "#900000", 
  "Peru" = "#ee2400",
  "Other South America" = "#FFB2B2",
  "Mexico" = "#74add1",
  "North America" =  "#4575b4",
  "Indonesia"="#762a83", 
  "China" = "#9970ab", 
  "Other Asia and Pacific" = "#c2a5cf", 
  "DR Congo" = "#6B990F",
  "Tanzania" = "#A3CC51", 
  "Other Africa" = "#E5FFB2", 
  "Europe" = "#E69F60",
  "bananas" = "#e3b505",                     # golden yellow
  "leguminous crops" = "#A7BBEC",            # olive  
  "maize" = "#8CD790",                       # medium purple 
  "oilpalm" = "#D81E5B",                     # deep red  
  "other cereals" = "#9BDEAC",               # cyan-blue  
  "other crops" = "#7f7f7f",                  # dark grey
  "other fruits and nuts" = "#FFA420",       # light yellow-beige  
  "other oilseed crops" = "#F0544F",         # light coral  
  "rice" = "#588157",                        # viol et  
  "soybeans" = "#BE6C77",                    # brown  
  "sugar beverage and spice crops" = "#B3EBF2", # pink-magenta  
  "vegetables, melons and root/tuber crops" = "#AD8A64"
)

# Load once in data.R
raster_lookup <- readRDS("raster_lookup.rds")

coolwarm_hcl <-   c("#002F70","#1A4F97","#336BB8","#4F87D0","#6AA1DE",
                    "#86B9E8","#A3CDED","#BDDDF0","#D4E8F2","#DCE6ED",
                    "lightgrey", # neutral color for zero
                    "#FCFDBFFF", "#FDE992" ,"#FEC98DFF","#FD9567FF","#FA815FFF",
                    "#F4685CFF" ,"#E85362FF", "#D6456CFF" ,"#C03A76FF" ,"#AB337CFF")




###
# UI ----------------------------------------------------------------------
###
ui <- page_sidebar(
  
  title = "Biodiversity Impact Timeseries Explorer",
  
  sidebar = sidebar(
    
    h4("Map filters"),
    
    pickerInput(
      "map_landuse",
      "Land use:",
      choices = c("All", "Cropland", "Plantation", "Pasture"),
      selected = "All"
    ),
    
    pickerInput(
      "map_intensity",
      "Intensity:",
      choices = c("All", "Low", "Medium", "High"),
      selected = "All"
    ),
    
    pickerInput(
      "map_crop_group",
      "Crop group:",
      choices = c(
        "All",
        sort(setdiff(unique(data_type$crop_group),
                     c("Abandoned", "Pasture")))
      ),
      selected = "All"
    ),
    
    actionButton("update_map", "Apply map filters"),
    
    hr(),
    
    h4("Timeseries filters"),
    
    pickerInput(
      "intensity",
      "Intensity:",
      choices = c("All", sort(unique(data_type$intensity))),
      selected = "All",
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    
    pickerInput(
      "region",
      "Region:",
      choices = c("All", sort(unique(data_type$country_group))),
      selected = "All",
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    
    pickerInput(
      "country",
      "Country:",
      choices = c("All", sort(unique(data_type$country))),
      selected = "All",
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    
    pickerInput(
      "landuse",
      "Land use:",
      choices = c("All", sort(unique(data_type$LU_type))),
      selected = "All",
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    
    pickerInput(
      "crop_group",
      "Crop group:",
      choices = c(
        "All",
        sort(setdiff(unique(data_type$crop_group),
                     c("Abandoned", "Pasture")))
      ),
      selected = "All",
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    
    pickerInput(
      "crop_type",
      "Crop type:",
      choices = c(
        "All",
        sort(setdiff(unique(data_type$crop_type),
                     c("Abandoned", "Pasture")))
      ),
      selected = "All",
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    
    selectInput(
      "value_type",
      "Value:",
      choices = c(
        "Total impact",
        "Absolute change since 2000",
        "Relative change since 2000 (%)"
      )
    ),
    
    sliderInput(
      "year_range",
      "Year range:",
      min = min(data_type$year),
      max = max(data_type$year),
      value = c(min(data_type$year), max(data_type$year))
    )
  ),
  div(
    #style = "height: 100vh; overflow-y: auto;",
    
    layout_column_wrap(
      width = 1,
      
      card(
        card_header("Spatial impact"),
        plotlyOutput("impact_map", height = 250)
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(plotlyOutput("plot_intensity", height = 300)),
        card(plotlyOutput("plot_landuse", height = 300)),
        card(plotlyOutput("plot_region", height = 300)),
        card(plotlyOutput("plot_croptype", height = 300))
      )
    )
  ),
  fillable = FALSE,
  theme = bs_theme(preset = "minty")
)


# Environment to cache loaded RDS
raster_df_cache <- new.env(parent = emptyenv())

get_raster_df <- function(rds_path) {
  if (exists(rds_path, envir = raster_df_cache)) {
    return(get(rds_path, envir = raster_df_cache))
  }
  df <- readRDS(rds_path)
  assign(rds_path, df, envir = raster_df_cache)
  df
}

# server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- data_type
    if (!("All" %in% input$region))     df <- df %>% filter(country_group %in% input$region)
    if (!("All" %in% input$intensity))  df <- df %>% filter(intensity %in% input$intensity)
    if (!("All" %in% input$landuse))    df <- df %>% filter(LU_type %in% input$landuse)
    if (!("All" %in% input$crop_group)) df <- df %>% filter(crop_group %in% input$crop_group)
    if (!("All" %in% input$country))   df <- df %>% filter(country %in% input$country)
    if (!("All" %in% input$crop_type)) df <- df %>% filter(crop_type %in% input$crop_type)
    df %>% filter(year >= input$year_range[1], year <= input$year_range[2])
  })
  
  compute_metrics <- function(df, group_name) {
    df_sum <- df %>%
      group_by(.data[[group_name]], year) %>%
      summarise(impact = sum(impact, na.rm = TRUE), .groups = "drop") %>%
      rename(!!group_name := .data[[group_name]])
    
    base <- df_sum %>% filter(year == 2000) %>% select(all_of(group_name), impact_2000 = impact)
    
    df_sum %>%
      left_join(base, by = group_name) %>%
      mutate(
        abs_change = impact - coalesce(impact_2000, 0),
        rel_change = ifelse(is.na(impact_2000) | impact_2000 == 0, NA, (impact / impact_2000 - 1) * 100)
      )
  }
  
  render_group_plot <- function(group_name, title) {
    renderPlotly({
      df <- compute_metrics(filtered_data(), group_name)
      if (nrow(df) == 0) return(NULL)
      
      yvar <- switch(input$value_type,
                     "Total impact" = "impact",
                     "Absolute change since 2000" = "abs_change",
                     "Relative change since 2000 (%)" = "rel_change")
      
      if (input$value_type == "Relative change since 2000 (%)") {
        p <- ggplot(df, aes(x = year, y = .data[[yvar]], color = .data[[group_name]], group = .data[[group_name]])) +
          geom_line(size = 1) +
          scale_color_manual(values = cols, name = NULL) +
          labs(title = title, y = "Relative change (%)", x = NULL) +
          theme(legend.title = element_blank())
      } else {
        p <- ggplot(df, aes(x = year, y = .data[[yvar]], fill = .data[[group_name]])) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = cols, name = NULL) +
          labs(title = title, y = input$value_type, x = NULL)
      }
      ggplotly(p)  %>%
        layout(
          # legend = list(
          #   orientation = "v",
          #   x = 0,
          #   y = 1)
           )
    })
  }
  
  output$plot_intensity <- render_group_plot("intensity_f", "By Intensity")
  output$plot_landuse <- render_group_plot("lu_type_f", "By Land Use Type")
  output$plot_region <- render_group_plot("region_f", "By Region")
  output$plot_croptype <- render_group_plot("crop_group_f", "By Crop Group")
  # Conditional crop-type plot
  
  # Bei region filter nur jeweilige Countries in den regions anzeigen
  observeEvent(input$region, {
    # Wenn ALL gewählt ist → alle Länder anzeigen
    if ("All" %in% input$region) {
      updatePickerInput(
        session,
        inputId = "country",
        choices = c("All", sort(unique(data_type$country))),
        selected = "All"
      )
      return()
    }
    
    # Welche Länder gehören zu den gewählten Regionen?
    valid_countries <- data_type %>%
      filter(country_group %in% input$region) %>%
      pull(country) %>%
      unique() %>%
      sort()
    
    updatePickerInput(
      session,
      inputId = "country",
      choices = c("All", valid_countries),
      selected = "All"
    )
  })
  
  # Same for crop types 
  observeEvent(input$crop_group, {
    # Wenn ALL gewählt ist → alle Länder anzeigen
    if ("All" %in% input$crop_group) {
      updatePickerInput(
        session,
        inputId = "crop_type",
        choices = c("All", sort(unique(data_type$crop_type))),
        selected = "All"
      )
      return()
    }
    
    # Welche Länder gehören zu den gewählten Regionen?
    valid_crops <- data_type %>%
      filter(crop_group %in% input$crop_group) %>%
      pull(crop_type) %>%
      unique() %>%
      sort()
    
    updatePickerInput(
      session,
      inputId = "crop_type",
      choices = c("All", valid_crops),
      selected = "All"
    )
  })

  # plot maps --------------------------------------------------------------------

  
  # Default filters (before any button click)
  default_filters <- reactive({
    tibble(
      landuse    = "all",
      crop_group = "all",
      intensity  = "all"
    )
  })
  
  # Event-reactive that updates only when button is clicked
  filter_map <- eventReactive(input$update_map, {
    tibble(
      landuse    = tolower(input$map_landuse),
      crop_group = tolower(gsub(" ", "_", input$map_crop_group)),
      intensity  = tolower(input$map_intensity)
    )
  }, ignoreNULL = FALSE) # <-- important to allow first use
  
  # A helper that uses default if eventReactive hasn't fired yet
  current_filters <- reactive({
    if (is.null(filter_map())) {
      default_filters()
    } else {
      filter_map()
    }
  })
  
  selected_raster_df <- reactive({
    f <- raster_lookup |>
      filter(
        landuse == filter_map()$landuse,
        crop_group == filter_map()$crop_group,
        intensity == filter_map()$intensity
      ) |>
      pull(file)
    cat("DEBUG: raster_lookup returned:\n")
    print(f)
    print(paste("landuse", filter_map()$landuse, 
          "cropgroup ", filter_map()$crop_group,
          "intensity ", filter_map()$intensity))
    
    req(length(f) == 1)
    
    # full path in the downsampled folder
    rds_path <- file.path("croptype_df", f)
    
    cat("DEBUG: Trying to load RDS:\n")
    print(rds_path)
    cat("File exists?", file.exists(rds_path), "\n")
    
    req(file.exists(rds_path))
    
    get_raster_df(rds_path)
  })
  
  
  # define breaks 
  breaks = c(-1e-09,-1e-10,-1.0e-11,
             0.000e+00,
             1e-11, 1e-10, 1e-09)
  
  output$impact_map <- renderPlotly({
    df_r <- selected_raster_df()
    if (is.null(df_r)) {
      # Create a dummy data.frame for the message
      df_r <- data.frame(
        x = 0, y = 0, impact = 0, 
        message = paste0("No raster for this combination: Land use: ", input$map_landuse,
                            " | Intensity: ", input$map_intensity,
                            " | Crop group: ", input$map_crop_group)
      )
      p <- ggplot(df_r, aes(x, y)) +
        geom_text(aes(label = message), size = 10, color = "red") +
        theme_void()
    } else {
      
    
      map_r <- ggplot(df_r, aes(x, y, fill = impact)) +
        geom_tile(color = NA) +
        scale_fill_gradientn(
          colors = coolwarm_hcl,
          na.value = "white",
          trans = pseudo_log_trans(sigma = 1e-13),
          name = expression(Delta*PSL["glo"]),
          breaks = breaks
        ) +
        labs(title = paste0("Biodiversity impact change 2000-2019 \n",
                            "Land use: ", input$map_landuse,
                            " | Intensity: ", input$map_intensity,
                            " | Crop group: ", input$map_crop_group),
             y = NULL, x = NULL) +
        theme(
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        )
      
      ggplotly(map_r)
    }
  })
  
}


# Run the App -------------------------------------------------------------
shinyApp(ui = ui, server = server)
