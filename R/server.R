format.data <- function(input.data, columns, datefmt, Date.name, linetype="solid", ...) {
    dat <- read.timeseries(input.data, datefmt = datefmt, datecol=Date.name,  ...)
    plot.cols = columns[columns %in% names(dat)]
    if (length(plot.cols)  < 2)
     return(NA)
    dat <- dat[, plot.cols, drop = FALSE]
    plot.data <- melt(dat, id = Date.name, variable_name = c("Depth"))    
    names(plot.data)[3] = "Temperature"
    plot.data$Linetype = linetype
    
    return(plot.data)
    }

prms <- function(input) {
if (input$readtype == "GEOtop"){
  skip = 0; fmt = " %d/%m/%Y %H:%M"; datnam = "Date"; depthmult = 1000
}
if (input$readtype == "Geoprecision"){
  skip = 2; fmt = " %d.%m.%Y %H:%M:%S"; datnam = "Time"; depthmult = 1
}
else{
  skip = input$n.skip; fmt = input$Date.fmt; datnam = input$Date.name; depthmult = input$Depth.mult
}
    return(list(skip=skip, fmt=fmt, date=datnam, depthmult = depthmult))
    }
    
shinyServer(function(input, output) {

# Check boxes
    output$choose_columns <- renderUI({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$GT_primary))
    return()
      
    # Get the data set with the appropriate name
    
    dat <- read.timeseries(input$GT_primary$datapath, datefmt = prms(input)$fmt, skip=prms(input)$skip, datecol = prms(input)$date, div.by=as.numeric(prms(input)$depthmult))
    colnames <- names(dat)
    
    # If second data set is loaded, add column names
    if(!is.null(input$GT_secondary)){
        dat2 <- read.timeseries(input$GT_secondary$datapath, datefmt = prms(input)$fmt, skip=prms(input)$skip, datecol = prms(input)$date, div.by=as.numeric(prms(input)$depthmult))
        colnames <- unique(c(colnames,names(dat2)))
        
        }
    dateIndex <- which(colnames == prms(input)$date)
    colnames <- colnames[c(dateIndex, 1+order(as.numeric(colnames[-dateIndex])))]    
    # Create the checkboxes and select first two by default
    checkboxGroupInput("columns", "Choose columns", 
                        choices  = colnames,
                        selected = colnames[1:2],
                        inline = TRUE)
  })
  
  # Year Select - TODO: this could be a range slider
    output$choose_year <- renderUI({
    
    # If missing input, return to avoid error later in function
    if (is.null(input$GT_primary))
    return()
      
    # Get the data set with the appropriate name
    dat <- read.timeseries(input$GT_primary$datapath, skip=prms(input)$skip, datefmt = prms(input)$fmt, datecol = prms(input)$date, div.by=as.numeric(prms(input)$depthmult))
    years <- unique(format(dat[,prms(input)$date], "%Y"))
    if (length(years) == 0)
    return()
    
    # Create the checkboxes and select first two by default
    selectInput('choose_year',
      label = "Choose year for trumpet",
      choices = years,
      selected = years[1]
    )
  })

  # data read parameters
   output$n.skip <- renderUI({
   if (input$readtype !="Custom"){
     return()}
    numericInput('n.skip', "Number of rows to skip", value=0)
     })
   
   output$n.skip <- renderUI({
   if (input$readtype !="Custom"){
     return()}
    numericInput('n.skip', "Number of rows to skip", value=0)
     })
   
   output$Date.name <- renderUI({
   if (input$readtype !="Custom"){
     return()}
    textInput('Date.name', "Name of date column", value="Date")
     })
     
   output$Date.fmt <- renderUI({
   if (input$readtype !="Custom"){
     return()}
    textInput('Date.fmt',  "Format of date column", value="%d/%m/%Y %H:%M")
   })
   
   output$Depth.mult <- renderUI({
   if (input$readtype !="Custom"){
     return()}
    radioButtons("Depth.mult", "Convert depth from:", choiceNames=c("m", "mm"), choiceValues=c(1,1000), selected=1000)
   })
# Output the data
  
  output$contents <- renderTable({
    # If missing input, return to avoid error later in function
    if(is.null(input$GT_primary))
      return()

    # Get the data set
    dat <- read.timeseries(input$GT_primary$datapath, convertDate=FALSE, skip=prms(input)$skip, datefmt = prms(input)$fmt, div.by=as.numeric(prms(input)$depthmult))

    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    if (is.null(input$columns))
      return()
    plot.cols = input$columns[input$columns %in% names(dat)]
    
    # Keep the selected columns
    dat <- dat[, plot.cols, drop = FALSE]
    
    # Return all rows
    head(dat, 20)
  })
  
  output$plot1 <- renderPlotly({
    if(is.null(input$GT_primary) || length(input$columns) < 2 || !prms(input)$date %in% input$columns)
      return()
    
    plot.data <- format.data(input$GT_primary$datapath, input$columns, prms(input)$fmt, 
    prms(input)$date, skip=prms(input)$skip, div.by=as.numeric(prms(input)$depthmult))
    
    plot.data %>%
    group_by(Depth) %>%
    plot_ly(x = plot.data[,prms(input)$date], y= ~Temperature, color = ~Depth) %>%
    add_lines()%>%
    layout(legend = list(orientation = 'h'))

  })
  
    output$plot2 <- renderPlotly({
    if(is.null(input$GT_secondary) || length(input$columns) < 2 || !prms(input)$date %in% input$columns)
      return()
    plot.data <- format.data(input$GT_secondary$datapath, input$columns, prms(input)$fmt, 
        prms(input)$date, skip=prms(input)$skip, div.by=as.numeric(prms(input)$depthmult))

    plot.data %>%
    group_by(Depth) %>%
    plot_ly(x = plot.data[,prms(input)$date], y= ~Temperature, color = ~Depth) %>%
    add_lines()%>%
    layout(legend = list(orientation = 'h'))
  })
  
output$comparison <- renderPlotly({
    if(is.null(input$GT_secondary) || length(input$columns) < 2 ||
    !prms(input)$date %in% input$columns || is.null(input$GT_primary) ||is.null(input$columns))
      return()
       
    plot.data1 <- format.data(input$GT_primary$datapath, input$columns, prms(input)$fmt, 
        prms(input)$date, skip=prms(input)$skip, div.by=as.numeric(prms(input)$depthmult))
    plot.data2 <- format.data(input$GT_secondary$datapath, input$columns, prms(input)$fmt, 
        prms(input)$date, linetype="twodash", skip=prms(input)$skip, div.by=as.numeric(prms(input)$depthmult))
    
    plot.data = rbind(plot.data1, plot.data2)
    
    plot.data %>%
    group_by(Depth) %>%
    plot_ly(x = plot.data[,prms(input)$date], y= ~Temperature, color = ~Depth, linetype = ~Linetype) %>%
    add_lines()   %>%
    layout(legend = list(orientation = 'h'))

  })

  output$trumpet1 <- renderPlotly({
    
    # Load and subset data 
    data.all <- read.timeseries(input$GT_primary$datapath, datefmt = prms(input)$fmt, skip=prms(input)$skip, div.by=as.numeric(prms(input)$depthmult))
    datamax <- as.numeric(lapply(data.all[,-which(names(data.all)==prms(input)$date)], function(x) max(x, na.rm=TRUE)))
    datamin <- as.numeric(lapply(data.all[,-which(names(data.all)==prms(input)$date)], function(x) min(x, na.rm=TRUE)))
    
    data.year <- data.all[as.numeric(format(data.all[,prms(input)$date],"%Y")) == as.numeric(input$choose_year),]
    temperature <- data.year[,-which(names(data.year)==prms(input)$date)]
    
    # build table    
    PFstat <- build.trumpet(temperature, melt=TRUE)    
    
    p <- ggplot(PFstat, aes_string(x="value", y="depths", color="variable")) +
    xlim(min(datamin), max(datamax)) +
    geom_path()
    p
    
  })
  
    output$ttab <- renderTable({
    
    # Load and subset data 
    data.all <- read.timeseries(input$GT_primary$datapath, datefmt = prms(input)$fmt, skip=prms(input)$skip, div.by=as.numeric(prms(input)$depthmult))
    data.year <- data.all[as.numeric(format(data.all[,prms(input)$date],"%Y")) == as.numeric(input$choose_year),]
    temperature <- data.year[,-which(names(data.year)==prms(input)$date)]
    
    # build table    
    PFstat <- build.trumpet(temperature) 
    PFstat
})

})

