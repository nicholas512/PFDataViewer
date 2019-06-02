shinyUI(pageWithSidebar(
  headerPanel("Permafrost Viewer"),
  sidebarPanel(
    fileInput('GT_primary', 'Choose primary dataset ',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),    
    fileInput('GT_secondary', 'Choose secondary dataset',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    radioButtons('readtype', "Data Type", choices=c("GEOtop", "Geoprecision", "Custom"), selected="Custom"),
    uiOutput("n.skip"),
    uiOutput("Date.name"),
    uiOutput("Date.fmt"),
    uiOutput("Depth.mult"),
    tags$hr(),
    uiOutput("choose_year"),
    uiOutput("choose_columns"),
    
    width = 3

  ),
  mainPanel(
     tabsetPanel(
     tabPanel("Info",
        h2("Description"),
        p("An R-driven GUI to speed up data previewing and model vs. observations comparisons"),
        h2("Instructions"),
        p("Load a ground temperature dataset (csv) using the panel on the left.  Then select the appropriate parameters
        to read the csv.  Then, enjoy beautiful time series and temperature envelope plots by using the tabbed navigation at the top.
         If a plot fails to load, you may have to add a dataset, or fiddle with the reading parameters"),
        h3("Input settings:"),
        p(strong(a(href="http://geotopmodel.github.io/geotop/","GEOtop:")),"skip=0, name =", em("Date, "),"format = %d/%m/%Y %H:%M "),
        p(strong(a(href="http://www.geoprecision.com/en/","Geoprecision"))," (mini-logger): skip=2, name =", em("Time, "),"format = %d.%m.%Y %H:%M:%S")),     
     tabPanel("Plot Primary", plotlyOutput("plot1"), tableOutput("contents")),
     tabPanel("Trumpet Curve", 
            fluidRow(
             column(12, plotlyOutput("trumpet1")),
             column(12, p("Ground temperature envelope (trumpet curve) for primary dataset")),
             column(12, tableOutput("ttab"))
         )),
     tabPanel("Plot Secondary", plotlyOutput("plot2")),
    tabPanel("Comparison", fluidRow(
             column(12, plotlyOutput("comparison")),
             column(12, p("Solid line corresponds to primary dataset, dashed line corresponds to secondary datset"),
             p("If plot does not load, it may be because depths are selected that do not exist in both data sets"))
         ))

     
     )
     )))