#Setup Environment
library(ggplot2)
library(dplyr)
library(shiny)
library(nls2)

ga_counties <- read.csv('a01-gareis2003.txt')
ga_proj <- read.csv('a40-gaprojected.txt')

analysis_level <- data.frame("FIPS" = unique(ga_counties$FIPS), "AREANAME" = unique(ga_counties$AREANAME))
analysis_type <-  data.frame("LINECODE" = unique(ga_counties$LINECODE), "LINETITLE" = unique(ga_counties$LINETITLE))
analysis_years <- data.frame("YEAR" = unique(ga_counties$YEAR))
prj_methods <- data.frame(methods = rbind("Linear","Quadratic","Cubic","Geometric","Simple Expoential","Modified Exponential",
                                      "Logistic","Constant Share","Shift Share","Growth Shart"))

# Begin Shiny
ui <- fluidPage(
  titlePanel("CP-6024: Assignment 2, Population Projections and Forcasts"),
  plotOutput("pop_graph"),
  fluidRow(
      column(3,
              h4("Projection Controls"),
              sliderInput("year_ctrl", "Set Years",
                        min = min(analysis_years$YEAR), max = max(analysis_years$YEAR),
                        value = c(min(analysis_years$YEAR), max(analysis_years$YEAR)),
                        step = 10,
                        sep = ""
                        ),
             selectInput("area_ctrl", "Set County", analysis_level$AREANAME, selected = "Appling, Georgia [13001]"),
             selectInput("analysis_type", "Variable (Y)", analysis_type$LINETITLE, selected = "Population (persons)")
            ),
      column(4,
             h4("Projection Type"),
              selectInput("proj_type", "Projection Method", prj_methods$methods),
              tableOutput("prj_out")
            ),
      column(5,
              h4("Error Measurement"),
              tableOutput("err_out")
             )
          ),
  h4("Projetions Table"),
  dataTableOutput("proj_table")
  )

server <- function (input, output) {
  #Call Function Source
  source("proj_methods.R")
  
  site )- 
  output$pop_graph <- renderPlot({


      ggplot(a, aes(a$year+1968, a$pop)) + geom_point(aes(size = a$pop), show.legend = FALSE) +
      geom_smooth(color = "black", linetype = "dashed", size = 0.5, alpha = 0) +
      xlim(input$year_ctrl[1]-1,2010) +
      ylim(min(a$pop),max(a$pop)+(max(a$pop)*0.05)) +
      xlab("Year") + ylab(input$analysis_type)
  })
  output$proj_table <- ({
    df.10
  })
}


#Call Shiny
shinyApp(ui = ui, server = server)