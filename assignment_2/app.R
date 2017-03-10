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
                                      "Logistic","Constant Share","Shift Share","Growth Share"))

# Begin Shiny
ui <- fluidPage(
  titlePanel("CP-6024: Assignment 2, Population Projections and Forcasts"),
  plotOutput("pop_graph"),
  fluidRow(
      column(3,
              h4("Projection Controls"),
              sliderInput("year_ctrl", "Set Years",
                        min = min(analysis_years$YEAR), max = max(analysis_years$YEAR),
                        value = c(min(analysis_years$YEAR-1968), max(analysis_years$YEAR-1968)),
                        step = 1,
                        sep = ""
                        ),
             selectInput("area_ctrl", "Set County", analysis_level$AREANAME, selected = "Appling, Georgia [13001]"),
             selectInput("analysis_type", "Variable (Y)", analysis_type$LINETITLE, selected = "Population (persons)")
            ),
      column(4,
             h4("Projection Type"),
              selectInput("proj_type", "Projection Method", c("Linear","Quadratic","Cubic","Exponential")),
              tableOutput("prj_out")
            ),
      column(5,
             textOutput("lvl_out")
            )
          ),
  h4("Projetions Table"),
  dataTableOutput("proj_table")
  )

server <- function (input, output) {
  #Call Function Source
  source("proj_methods.R")

  a <- reactive({ n <- data.frame( pop = subset(ga_counties, AREANAME == input$area_ctrl & LINETITLE == input$analysis_type)$VALUE,
                   year = subset(ga_counties, AREANAME == input$area_ctrl & LINETITLE == input$analysis_type)$YEAR-1968)
                  n <- n[with(n, order(year)),]
                   return(n)})
  a_j <- reactive({ n <- data.frame( pop = subset(ga_counties, FIPS == 13000 & LINETITLE == input$analysis_type)$VALUE,
                     year = subset(ga_counties, FIPS == 13000 & LINETITLE == input$analysis_type)$YEAR-1968)
                    n <- n[with(n, order(year)),]
                    return(n)})
  
  b.10 <- data.frame(subset(ga_proj, Year == 2010 & DataType == 'Population'))
  b.20 <- data.frame(subset(ga_proj, Year == 2020 & DataType == 'Population'))
  
  tmp.mod <- reactive({ 

      lp <- linear_proj(a()$year, a()$pop, input$year_ctrl[2]-1968, 2010-input$year_ctrl[2])
      qp <- quad_proj(a()$year, a()$pop, input$year_ctrl[2]-1968, 2010-input$year_ctrl[2])
      cp <- cube_proj(a()$year, a()$pop, input$year_ctrl[2]-1968, 2010-input$year_ctrl[2])
      sg <- s_g(a()$year, a()$pop, input$year_ctrl[2]-1968, 2010-input$year_ctrl[2])
      sep <- s_exp(a()$year, a()$pop, input$year_ctrl[2]-1968, 2010-input$year_ctrl[2])
      me <- m_e(a()$year, a()$pop, input$year_ctrl[2]-1968, 2010-input$year_ctrl[2])
      l <- lo(a()$year, a()$pop, input$year_ctrl[2]-1968, 2010-input$year_ctrl[2])
      
      p.10 <- data.frame(rbind(predict(lp, newdata = data.frame( x = (input$year_ctrl[2]-1968)+7)),
                              predict(qp, newdata = data.frame( x = (input$year_ctrl[2]-1968)+7)),
                              predict(cp, newdata = data.frame( x = (input$year_ctrl[2]-1968)+7)),
                              predict(sg, newdata = data.frame( x = (input$year_ctrl[2]-1968)+7)),
                              predict(sep, newdata = data.frame( x = (input$year_ctrl[2]-1968)+7)),
                              predict(me, newdata = data.frame( x = (input$year_ctrl[2]-1968)+7)),
                              predict(l, newdata = data.frame( x = (input$year_ctrl[2]-1968)+7)),
                              c_s(a(), a_j(), b.10$Value),
                              s_s(a(), a_j(), b.10$Value, input$year_ctrl[2]-1968+7),
                              s_gr(a(), a_j(), b.10$Value)
                  )
                )
      
      names(p.10) <- c("Population (2010)")
      
      p.20 <- data.frame(rbind(predict(lp, newdata = data.frame( x = (input$year_ctrl[2]-1968)+17)),
                              predict(qp, newdata = data.frame( x = (input$year_ctrl[2]-1968)+17)),
                              predict(cp, newdata = data.frame( x = (input$year_ctrl[2]-1968)+17)),
                              predict(sg, newdata = data.frame( x = (input$year_ctrl[2]-1968)+17)),
                              predict(sep, newdata = data.frame( x = (input$year_ctrl[2]-1968)+17)),
                              predict(me, newdata = data.frame( x = (input$year_ctrl[2]-1968)+17)),
                              predict(l, newdata = data.frame( x = (input$year_ctrl[2]-1968)+17)),
                              c_s(a(), a_j(), b.20$Value),
                              s_s(a(), a_j(), b.20$Value, input$year_ctrl[2]-1968+17),
                              s_gr(a(), a_j(), b.20$Value)
                  )
                )
      

      names(p.20) <- c("Population (2020)")
      p_m = prj_methods
      p <- bind_cols(p_m,p.10,p.20)
      
    return(p)})

  output$pop_graph <- renderPlot({
      y = a()$pop
      x = a()$year
      
      ggplot(a(), aes(a()$year+1968, a()$pop)) + geom_point(aes(size = a()$pop), show.legend = FALSE) +
      geom_smooth(color = "black", linetype = "dashed", size = 0.5, alpha = 0) +
      geom_smooth(method = "lm", formula = y ~ x, 
                  color = if (input$proj_type == "Linear"){ "blue" } else { "grey" },
                  linetype = "solid", 
                  size = if (input$proj_type == "Linear"){ 1 } else { 0.5 }, alpha = 0) +
      geom_smooth(method = "lm", formula = y ~ poly(x,2), 
                  color = if (input$proj_type == "Quadratic"){ "blue" } else { "grey" }, 
                  linetype = "solid", 
                  size = if (input$proj_type == "Quadratic"){ 1 } else { 0.5 }, alpha = 0) +
      geom_smooth(method = "lm", formula = y ~ poly(x,3), 
                  color = if (input$proj_type == "Cubic"){ "blue" } else { "grey" },
                  linetype = "solid",
                  size = if (input$proj_type == "Cubic"){ 1 } else { 0.5 }, alpha = 0) +
      geom_smooth(method = "lm", formula =  log(y) ~ x,
                  color = if (input$proj_type == "Exponential"){ "blue" } else { "grey" }, 
                  linetype = "solid",
                  size = if (input$proj_type == "Exponential"){ "blue" } else { "grey" }, alpha = 0) +
      xlim(input$year_ctrl[1]-1,2010) +
      ylim(min(a()$pop),max(a()$pop)+(max(a()$pop)*0.05)) +
      xlab("Year") + ylab(input$analysis_type)
  })
  
  output$prj_out <- renderDataTable({
    print(subset(tmp.mod(), methods == input$proj_type))
    df_out <- data.frame( rbind(subset(tmp.mod(), methods == input$proj_type)[2],
                                subset(tmp.mod(), methods == input$proj_type)[3])
    )
  })
  output$proj_table <- renderDataTable({
        tmp.mod()
  })
  
  output$lvl_out <- renderText({
    input$area_ctrl
  })
}


#Call Shiny
shinyApp(ui = ui, server = server)