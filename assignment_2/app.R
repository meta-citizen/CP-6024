library(ggplot2)
library(dplyr)
library(shiny)

#Set Environment


ga_counties <- read.csv('a01-gareis2003.txt')
ga_proj <- read.csv('a40-gaprojected.txt')

analysis_level <- data.frame("FIPS" = unique(ga_counties$FIPS), "AREANAME" = unique(ga_counties$AREANAME))
analysis_type <-  data.frame("LINECODE" = unique(ga_counties$LINECODE), "LINETITLE" = unique(ga_counties$LINETITLE))
analysis_years <- data.frame(unique(ga_counties$YEAR))

#
ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "base_year",
                    label = "Analysis Range",
                    value = c(1969,2003), min = 1969, max = 2003),
        selectInput(inputId = "a_level",
                    label = "Analysis Level",
                    choices = analysis_level$AREANAME,
                    selected = "Appling, Georgia [13001]",
                    multiple = FALSE),
        selectInput(inputId = "a_type",
                    label = "Analysis Type",
                    choices = analysis_type$LINETITLE),
        selectInput(inputId = "proj_year", 
                    label = "Projection Year",
                    choices = c("2010",
                                "2020"),
                    selected = "2010"),
        selectInput(input = "proj_line", label = "Projection Line",
                    choices = c("Simple Linear", "Simple Geometric"))
        ),
      mainPanel(
        plotOutput(outputId = "yv_scatter")
      )
  ),
    dataTableOutput(outputId = "proj_table"),  
    dataTableOutput(outputId = "pop_table")
  )


server <- function(input, output) {
  source('proj_src.R')
  
  output$yv_scatter <- renderPlot({
      p_x = subset(ga_counties, AREANAME == input$a_level & LINETITLE == input$a_type & YEAR >= input$base_year[1])$YEAR
      p_y = subset(ga_counties, AREANAME == input$a_level & LINETITLE == input$a_type & YEAR >= input$base_year[1])$VALUE
    plot(
        x = p_x,
        y = p_y,
        xlim = range(input$base_year[1]:input$base_year[2]),
        ylim = range(min(p_y):max(p_y)),
        xlab = "Year",
        ylab = input$a_type)})
  output$pop_table = renderDataTable({
      subset(ga_counties, AREANAME == input$a_level & LINETITLE == input$a_type & YEAR >= input$base_year[1])
  }, options = list(lengthMenu = c(5,10,15), pageLength = 5))
  output$proj_table = renderDataTable({

    if (input$proj_year == "2010") { var <- 2010 } else {var <- 2020 }
    
    p <- subset(ga_counties, AREANAME == input$a_level & LINETITLE == input$a_type & between(YEAR, input$base_year[1], input$base_year[2]))$VALUE
    t <- subset(ga_counties, AREANAME == input$a_level & LINETITLE == input$a_type & between(YEAR, input$base_year[1], input$base_year[2]))$YEAR-1969
    
    #Establish i and j table frames
    i_base <- subset(ga_counties, AREANAME == input$a_level & LINETITLE == input$a_type & YEAR == input$base_year[1])
    i_launch <- subset(ga_counties, AREANAME == input$a_level & LINETITLE == input$a_type & YEAR == input$base_year[2])
    
    j_base <- subset(ga_counties, LINECODE == 20 & YEAR == input$base_year[1] & FIPS == 13000)
    j_launch <- subset(ga_counties, LINECODE == 20 & YEAR == input$base_year[2] & FIPS == 13000)
    j_proj <- subset(ga_proj, DataType == 'Population' & Year == input$proj_year)
    
    tmp <- data.frame(rbind(smp_linear(i_base$VALUE, 
                                 i_launch$VALUE, 
                                 input$base_year[2]-input$base_year[1], 
                                 var-input$base_year[2]),
                      smp_geom(i_base$VALUE, 
                               i_launch$VALUE, 
                               input$base_year[2]-input$base_year[1], 
                               var-input$base_year[2]),
                      smp_exp(i_base$VALUE, 
                              i_launch$VALUE, 
                              input$base_year[2]-input$base_year[1], 
                              var-input$base_year[2]),
                      line_proj(p, 
                                i_launch$VALUE, 
                                t, 
                                var-input$base_year[2]),
                      quad_proj(p, 
                                i_launch$VALUE, 
                                t, 
                                var-input$base_year[2]),
                      cube_proj(p, 
                                i_launch$VALUE, 
                                t, 
                                var-input$base_year[2]),
                      geometric(i_base$VALUE, 
                                i_launch$VALUE, 
                                input$base_year[2]-input$base_year[1], 
                                var-input$base_year[2]),
                      mod_exp(i_base$VALUE,
                              i_launch$VALUE,
                              input$base_year[2]-input$base_year[1],
                              var-input$base_year[2]),
                      logistic(i_base$VALUE, 
                               i_launch$VALUE, 
                               input$base_year[2]-input$base_year[1], 
                               var-input$base_year[2]),
                      c_share( j_proj$Value,
                               j_launch$VALUE,
                               i_launch$VALUE),
                      s_share(j_base$VALUE,
                              j_launch$VALUE,
                              j_proj$Value,
                              i_base$VALUE,
                              i_launch$VALUE,
                              input$base_year[2]-input$base_year[1],
                              var-input$base_year[2]),
                      g_share(j_base$VALUE,
                              j_launch$VALUE,
                              j_proj$Value,
                              i_base$VALUE,
                              i_launch$VALUE)
                        )
                      )
    
    out_tab <- data.frame(PROJECTION_TYPE = c("Simple Linear","Simple Geometric", "Simple Exponential", "Linear Model", "Quadratic Model",
                                              "Cubic Model", "Geometric", "Modified Exponential", "Logistic", "Constant Share", 
                                              "Shift Share", "Growth Share"),
                          PROJECTION = tmp$population,
                          RATE = tmp$rate
    )
    

  })
}

#TO DO: Integrate data loading and manipulation into source (.R) files to be called later
#       Run projections into out_tab
#       If there is time plot each projection inorder to create a smooth curve using supsmu
#       with interface for changing projection type

shinyApp(ui = ui, server = server)