library(shiny)
library(tidyverse)
library(here)
library(crosstalk)
library(plotly)
library(janitor)
library(sf)
library(DT)
# Read data
# fgdb = "OregonStateParks_20181010.gdb"

# List all feature classes in a file geodatabase
# st_layers(fgdb)


dtw_pfix <- readRDS(file = here('data','hydro','dtw_pfix.RDS')) %>% filter(Year > 1985)
parcels <- readRDS(file = here('data','hydro','parcels.RDS'))

# %>%



  # filter(Parcel != "FSP004_BGP188")
# filter(Parcel %in% c("BGP162",
#                      "BLK021",
#                      "BLK075",
#                      "BLK094",
#                      "BLK099",
#                      "BLK142",
#                      "FSL044",
#                      "FSL054",
#                      "FSP006",
#                      "IND021",
#                      "IND026",
#                      "IND029",
#                      "IND139",
#                      "LAW035",
#                      "LAW043",
#                      "LAW052",
#                      "LAW070",
#                      "LAW072",
#                      "LAW082",
#                      "LAW085",
#                      "LNP045",
#                      "MAN037",
#                      "PLC007",
#                      "TIN050",
#                      "TIN053",
#                      "TIN064",
#                      "TIN068"))

rs_pfix <- readRDS(file = here('data','hydro','rs_pfix.RDS')) %>% filter(Year > 1985)
num_parcels <- length(unique(parcels$Parcel))
# baseline <- read_csv(here("data","tbl_DWPtrans_data.csv"))
# species <- read_csv(here("data","species.csv"))

parcels_aug <- read_csv(here("data","parcel_augment2.csv")) %>% clean_names()
# parcels_aug
# parcels_aug %>% nrow() #2087
# parcels_aug %>% distinct(pcl) %>% nrow()#1899
# 188 duplicates?
parcel_dupes <- parcels_aug %>% get_dupes(pcl)
# %>% distinct(pcl)

dremovepar <- parcels_aug %>% anti_join(parcel_dupes)
#1841 left over

pdupes_to1 <- parcel_dupes %>% group_by(pcl) %>%
  top_n(1, abs(acres))
# add these back in after removing dupes

parcels_augment <- bind_rows(dremovepar,pdupes_to1)



# UI
ui <- fluidPage(
  titlePanel("Groundwater-Dependent Ecosystems: Depth to water, field-measured cover, NDVI, precipitation"),
  fluidRow(
    column(4, selectInput("parcel", "Choose Parcel", choices = unique(parcels$Parcel))),
    column(4, actionButton("prevParcel", "Previous Parcel")),
    column(4, actionButton("nextParcel", "Next Parcel"))
  ),
  htmlOutput("parcel_info"),  # New widget to display parcel information
  plotlyOutput("dtw_plot", height = "150px"),
  plotlyOutput("cover_plot", height = "150px"),
  plotlyOutput("ndvi_plot", height = "150px"),
  plotlyOutput("precipitation_plot", height = "150px")
)

# Server
server <- function(input, output, session) {
  # Read data
  dtw_pfix <- readRDS(file = here('data','hydro','dtw_pfix.RDS')) %>% filter(Year > 1985)
  parcels <- readRDS(file = here('data','hydro','parcels.RDS')) %>% filter(Parcel != "FSP004_BGP188")
  rs_pfix <- readRDS(file = here('data','hydro','rs_pfix.RDS')) %>% filter(Year > 1985)

  # Define the number of parcels
  num_parcels <- length(unique(parcels$Parcel))

  # Reactive value for current parcel index
  currentParcelIndex <- reactiveVal(1)

  # Function to update parcel based on index
  updateParcel <- function(index) {
    selected_parcel <- parcels$Parcel[index]
    updateSelectInput(session, "parcel", selected = selected_parcel)
  }


  # Update dropdown selection and current index when selecting from dropdown
  observeEvent(input$parcel, {
    currentParcelIndex(which(parcels$Parcel == input$parcel))
  })



  # Advance to the next parcel when right arrow is clicked
  observeEvent(input$nextParcel, {
    current_index <- currentParcelIndex()
    next_index <- current_index %% num_parcels + 1
    updateParcel(next_index)
  })

  # Go to the previous parcel when left arrow is clicked
  observeEvent(input$prevParcel, {
    current_index <- currentParcelIndex()
    prev_index <- ifelse(current_index == 1, num_parcels, current_index - 1)
    updateParcel(prev_index)
  })


  # Create a shared data frame
  shared_data <- reactive({
    selected_parcel <- parcels$Parcel[currentParcelIndex()]
    list(
      dtw_plot = dtw_pfix %>% filter(Parcel == selected_parcel),
      cover_plot = parcels %>%
        filter(Parcel == selected_parcel) %>%
        mutate(annual = TLC - Cover) %>%
        select(NominalYear, Grass, Shrub, annual) %>%
        pivot_longer(cols = -NominalYear, names_to = "Cover_Type", values_to = "Cover_Value") %>%
        mutate(Cover_Type = factor(Cover_Type, levels = c("annual", "Shrub", "Grass"))),
      ndvi_plot = rs_pfix %>% filter(Parcel == selected_parcel),
      precipitation_plot = rs_pfix %>% filter(Year == 1986, Parcel == selected_parcel)
    )
  })

  # Render the dtw_plot based on the current selected parcel
  output$dtw_plot <- renderPlotly({
    dtw_plot <- shared_data()$dtw_plot
    hline_value <- dtw_plot %>% filter(Year == 1986) %>% pull(DTW)

    # dtw_plot$flag <- ifelse(dtw_plot$DTW < hline_value + 1, "Flagged", "Not Flagged")

    p <- ggplot(dtw_plot, aes(x = Year, y = DTW )) +#text = paste("Flag:", flag)
      geom_line(color = "darkturquoise") +
      geom_hline(yintercept = hline_value, color = "black", size = 0.1, linetype = "dashed") +
      geom_segment(aes(x = Year, xend = Year, y = DTW, yend = DTW-.5), color = ifelse(dtw_plot$DTW < hline_value, "cornflowerblue", "transparent"), size = 1.5) +  # Draw red line segments below the hline
      # geom_text(aes(label = ifelse(DTW < hline_value, "+", "")), vjust = 1, hjust = 0.5, color = "blue", size = 1) +  # Add text annotation below the hline
      labs(y = "DTW (ft bgs)", x = NULL) +
      theme_minimal() +
      scale_y_reverse() +
      scale_x_continuous(breaks = seq(from = 1986, to = 2026, by = 2))

    return(plotly::ggplotly(p))
  })

  output$ndvi_plot <- renderPlotly({
    selected_parcel <- parcels$Parcel[currentParcelIndex()]

    ndvi_data <- rs_pfix %>% filter(Parcel == selected_parcel)

    hline_value <- ndvi_data %>% filter(Year == 1986) %>% pull(NDVI_SUR)

    # Count the number of times NDVI value is above the hline
    flagged_count <- sum(ndvi_data$NDVI_SUR > hline_value - 0.01)

    # Calculate fraction of years with flagged NDVI value
    fraction_flagged <- flagged_count * 100 / nrow(ndvi_data)

    p <- ggplot(ndvi_data, aes(x = Year, y = NDVI_SUR)) +
      geom_bar(stat = "identity", fill = "darkseagreen", alpha = 0.7) +
      geom_hline(yintercept = hline_value, color = "black", size = 0.1, linetype = "dashed") +
      geom_segment(aes(x = Year, xend = Year, y = NDVI_SUR, yend = NDVI_SUR + 0.01), color = ifelse(ndvi_data$NDVI_SUR > hline_value - 0.01, "cyan4", "transparent"), size = 1.5) +  # Draw red line segments above the hline
      # geom_text(aes(label = ifelse(NDVI_SUR > hline_value, "", "")), vjust = -1, hjust = 0.5, color = "cyan4", size = 3) +  # Add text annotation above the hline
      # geom_segment(data = filter(ndvi_data, NDVI_SUR > hline_value), aes(x = Year - 0.3, xend = Year + 0.3, y = NDVI_SUR, yend = NDVI_SUR), color = "cyan", size = 2) +  # Add geom_segment flags for values above the hline
      annotate("text", x = 2000, y = max(ndvi_data$NDVI_SUR), label = paste("Years reaching 1986:", flagged_count, "/", nrow(ndvi_data), "=", round(fraction_flagged, 0), "%"), color = "blue") +  # Annotation for flagged count as fraction of total years
      labs(y = "NDVI", x = NULL) +
      theme_minimal() +
      scale_y_continuous(labels = function(x) round(x, 1)) +  # Round to one decimal place
      coord_cartesian(ylim = c(0, NA)) +  # Setting y-axis limits to start from 0 and auto-adjust the upper limit
      scale_x_continuous(breaks = seq(from = 1986, to = 2026, by = 2))

    return(plotly::ggplotly(p))
  })

  output$parcel_info <- renderUI({
    selected_parcel <- input$parcel
    parcel_info <- parcels_augment %>% filter(pcl == selected_parcel)
    HTML(paste("<p><strong>Ecological Site:</strong>", parcel_info$ecologic_3, "<strong>Greenbook Type:</strong> ", parcel_info$type, " ", parcel_info$comm_name, "</p>",
               # "<p><strong>Type:</strong>", parcel_info$type, "</p>",
               # "<p><strong>Holland Type:</strong>", parcel_info$comm_name, "</p>",
               "<p><strong>Wellfield (I=in,O=out):</strong>", parcel_info$wellfield, "</p>"), sep = "")
  })



  # Render the cover_plot based on the current selected parcel
  output$cover_plot <- renderPlotly({
    cover_plot <- shared_data()$cover_plot

    hline_cover <- cover_plot %>%
      filter(NominalYear == 1986, Cover_Type %in% c("Grass", "Shrub")) %>%
      pull(Cover_Value) %>%
      sum()

    hline_grass <- cover_plot %>%
      filter(NominalYear == 1986, Cover_Type %in% c("Grass")) %>%
      pull(Cover_Value) %>%
      sum()


    p <- ggplot(cover_plot, aes(x = NominalYear, y = Cover_Value, fill = Cover_Type)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
      geom_hline(yintercept = hline_cover, color = "black", size = 0.1, linetype = "dashed") +
      geom_hline(yintercept = hline_grass, color = "green", size = 0.1, linetype = "dashed") +
      labs(y = "Cover", x = NULL) +
      scale_fill_manual(values = c("Grass" = "chartreuse3", "Shrub" = "burlywood", "annual" = "deeppink2")) +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_cartesian(ylim = c(0, NA)) +
      scale_x_continuous(breaks = seq(from = 1986, to = 2026, by = 2))

    return(plotly::ggplotly(p))
  })

  # Render the precipitation_plot based on the current selected parcel
  output$precipitation_plot <- renderPlotly({
    selected_parcel <- parcels$Parcel[currentParcelIndex()]

    p <- ggplot(rs_pfix %>% filter(Parcel == selected_parcel), aes(x = Year, y = PPT * 0.0393701)) +
      geom_bar(stat = "identity", fill = "cadetblue1", alpha = 0.7) +
      geom_hline(yintercept = rs_pfix %>% filter(Year == 1986, Parcel == selected_parcel) %>% pull(PPT) * 0.0393701, color = "black", size = 0.1, linetype = "dashed") +
      labs(x = "Year", y = "PPT (inches)") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(from = 1986, to = 2026, by = 2))

    return(plotly::ggplotly(p))
  })

}

# Run the app
shinyApp(ui = ui, server = server)
