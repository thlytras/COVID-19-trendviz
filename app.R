load("dat.RData")
source("include.R")

library(shiny)
library(shinyWidgets)
library(markdown)

library(Cairo)
options(shiny.usecairo=TRUE)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "mine.css"),
    tags$link(rel = "shortcut icon", href = "logo-small.png")
  ),
  titlePanel("COVID-19 (2019-nCoV) trends visualizer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cnt", "Country/Region", 
        choices=c(unique(dat$cnt)[1], sort(unique(dat$cnt)[-1])), multiple=TRUE),
      fluidRow(
        column(6, actionButton("cnt_selAll", "Select all countries", style="margin-bottom: 1em; width:100%")),
        column(6, actionButton("cnt_clear", "Clear countries", style="margin-bottom: 1em; width:100%"))      
      ),
      selectInput("prov", "Province/State",
        choices=sort(unique(dat$prov)), multiple=TRUE),
      fluidRow(
        column(6, actionButton("prov_selAll", "Select all provinces", style="margin-bottom: 1em; width:100%")),
        column(6, actionButton("prov_clear", "Clear provinces", style="margin-bottom: 1em; width:100%"))      
      ),
      fluidRow(
        column(6, 
          sliderInput("pltThick", "Line/bar thickness", 
            min=-1.6, max=1.6, value=0, step=0.1, ticks=FALSE)
        ),
        column(6, 
          awesomeCheckbox("plotConf", "Plot confirmed cases", value=TRUE),
          awesomeCheckbox("plotDead", "Plot deaths", value=TRUE),
          awesomeCheckbox("plotRecov", "Plot recovered cases", value=TRUE)
        )
      ),
      img(src='eody.png', width=164*2/3, height=189*2/3, 
          style="display: block; margin-left: auto; margin-right: auto;"),
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cumulative cases",
          plotOutput("cumCases"),
          downloadButton("save_cumPlot", "Save plot")
        ),
        tabPanel("Incident cases",
          plotOutput("incCases"),
          selectInput("plotTypeInc", "Plot type", choices=c("Bar chart" = 0, "Line chart" = 1)), 
          downloadButton("save_incPlot", "Save plot")
        ),
        tabPanel("Second derivative",
          plotOutput("secDerivative"),
          downloadButton("save_secDerPlot", "Save plot")
        ),
        tabPanel("About", includeMarkdown("README.md"))
      )
    )
  )
)


server <- function(input, output, session) {

  # Selectable provinces based on the countries selected
  provChoices <- reactive({
    # if no country selected, choose all provinces
    if (is.null(input$cnt)) {
      return(sort(unique(dat$prov)))
    } else { # or else, chose only provinces found in the country
      return(sort(unique(subset(dat, cnt %in% input$cnt)$prov)))
    }
  })
  
  observe({
    sel <- isolate(input$prov)
    sel <- sel[sel %in% provChoices()]
    updateSelectInput(session, "prov", choices=provChoices(), selected=sel)
  })


  selData <- reactive({
    selData <- dat
    if (!is.null(input$cnt)) {
      selData <- subset(selData, cnt %in% input$cnt)
    }
    if (!is.null(input$prov)) {
      selData <- subset(selData, prov %in% input$prov)
    }
    return(selData)
  })
  
  aggrData <- reactive({
    aggregate(selData()[,c("conf","dead","recov")], selData()[,"datetime",drop=FALSE], sum, na.rm=TRUE)
  })

  # Returns aggregate data **by the end of the day**
  aggrDataEOD <- reactive({
    a <- aggrData()
    a$date <- as.Date(a$datetime)
    a[rev(!duplicated(rev(a$date))),]
  })

  
  output$cumCases <- renderPlot({
    if (nrow(aggrData())>0) {
      par(mar=c(5,4,2,2))
      plotCumCases(aggrData(), 
        plot.conf=input$plotConf, plot.recov=input$plotRecov, plot.dead=input$plotDead,
        thk=exp(input$pltThick))
    }
  })

  output$save_cumPlot <- downloadHandler(
    filename = function() {
      "cumulative_cases.png"
    },
    content = function(file) {
      png(file, width=1000, height=600, res=115)
      par(mar=c(5,4,2,2))
      if (nrow(aggrData())>0) {
        plotCumCases(aggrData(), plot.conf=input$plotConf, 
          plot.recov=input$plotRecov, plot.dead=input$plotDead,
          thk=exp(input$pltThick))
      }
      dev.off()
    }
  )
  
  output$incCases <- renderPlot({
    if (nrow(aggrData())>0) {
      par(mar=c(5,4,2,2))
      plotIncCases(aggrDataEOD(), 
        plot.conf=input$plotConf, plot.recov=input$plotRecov, plot.dead=input$plotDead,
        thk=exp(input$pltThick), line=as.logical(as.integer(input$plotTypeInc)))
    }
  })

  output$save_incPlot <- downloadHandler(
    filename = function() {
      "new_cases.png"
    },
    content = function(file) {
      png(file, width=1000, height=600, res=115)
      par(mar=c(5,4,2,2))
      if (nrow(aggrData())>0) {
        plotIncCases(aggrDataEOD(), plot.conf=input$plotConf, 
          plot.recov=input$plotRecov, plot.dead=input$plotDead,
          thk=exp(input$pltThick), line=as.logical(as.integer(input$plotTypeInc)))
      }
      dev.off()
    }
  )

  output$secDerivative <- renderPlot({
    if (nrow(aggrData())>0) {
      par(mar=c(5,4,2,2))
      plotSecDer(aggrDataEOD(), plot.conf=input$plotConf, 
        plot.recov=input$plotRecov, plot.dead=input$plotDead,
        thk=exp(input$pltThick))
    }
  })

  output$save_secDerPlot <- downloadHandler(
    filename = function() {
      "second_derivative.png"
    },
    content = function(file) {
      png(file, width=1000, height=600, res=115)
      par(mar=c(5,4,2,2))
      if (nrow(aggrData())>0) {
        plotSecDer(aggrDataEOD(), plot.conf=input$plotConf, 
          plot.recov=input$plotRecov, plot.dead=input$plotDead, 
          thk=exp(input$pltThick))
      }
      dev.off()
    }
  )

  observeEvent(input$prov_clear, {
    updateSelectInput(session, "prov", selected=character())
  })
  
  observeEvent(input$prov_selAll, {
    updateSelectInput(session, "prov", selected=provChoices())
  })
  
  observeEvent(input$cnt_clear, {
    updateSelectInput(session, "cnt", selected=character())
  })
  
  observeEvent(input$cnt_selAll, {
    all <- c(unique(dat$cnt)[1], sort(unique(dat$cnt)[-1]))
    all <- all[all!="Others"]
    updateSelectInput(session, "cnt", 
        selected=all)
  })
  
}

shinyApp(ui = ui, server = server)
