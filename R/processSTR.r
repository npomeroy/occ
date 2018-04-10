processSTR = function(file) {

  # Set environment variables to UTC
  Sys.setenv(TZ = 'UTC')

  #Load data
  str = read.csv(file)

  #Concatenate date and time
  str$DateTime = ymd_hms(paste0(str$Date, str$Time), tz = "UTC")

  #Define function for "Close Window" button in JavaScript
  jscode = 'shinyjs.closeWindow = function() { window.close(); }'

  # Create user interface. This bit of code defines what will appear on the UI.
  ui <- fluidPage(mainPanel(
    br(),
    actionButton("close", "Stop app"),

    # Paste file name at top of page
    h2(paste0(file_path_sans_ext(basename(
      file
    )))),

    # "Be patient" text...these datasets are huge!
    h6("(Be patient...the plots may take a minute to load!)"),
    br(),

    h5("Here is the raw STR time series:"),

    # Display the raw time series
    plotlyOutput("whole.ts", height = "200px"),

    h5(
      "The plots below show the the first and last month of the STR time series. Click on the time series to select start and end points."
    ),

    # Disploy the start and end plots of the time series
    fixedRow(column(
      6, plotlyOutput("start.plot", height = "400px")
    ),
    column(
      6, plotlyOutput("end.plot", height = "400px")
    ),
    style = 'padding:40px;'),

    # Display the selected start and end times
    fixedRow(
      column(6, verbatimTextOutput("start.select")),
      column(6, verbatimTextOutput("end.select"))
    ),

    h5("Here's what the clean time series looks like:"),

    # Display the trimmed time series
    plotlyOutput("cut.ts", height = "200px"),

    h5(
      "If you are happy with the clean time series, select the 'Save as CDP' button. Otherwise, continue to adjust start and end dates."
    ),

    h5(
      "Once you have saved the CDP file, select 'Close Window' to exit the app."
    ),

    # Create a"Save as CDP" button
    actionButton("save", "Save as CDP"),

    useShinyalert(),

    # Create a "Close Window" button
    useShinyjs(),

    extendShinyjs(text = jscode, functions = c("closeWindow")),

    actionButton("close", "Stop app"),

    br(),
    br(),
    br(),
    br(),
    br()
  )
  )

  # This code creates the shiny server (i.e. the code that the app is actually running)
  server <- function(input, output) {

    # Create start and end time variables that change based on user input
    vals = reactiveValues(start.time = NULL,
                          end.time = NULL)

    # Plot the raw time series
    output$whole.ts = renderPlotly({
      plot_ly(
        str,
        x = ~ DateTime,
        y = ~ Temperature,
        mode = 'lines',
        type = 'scatter'

      )
    })

    # Plot the first month of the time series
    output$start.plot = renderPlotly({
      plot_ly(
        str,
        x = ~ DateTime,
        y = ~ Temperature,
        mode = 'lines',
        type = 'scatter',
        source = 'S', # Connect this plot to the selection of start time
        hoverinfo = 'none' # Remove the default hover boxes
      ) %>%
        layout(
          title = 'Select START point',
          xaxis = list(
            rangeslider = list(type = "date"), # Add a range slider at the bottom of the plot
            spikemode = "across", # Add crosshairs
            spikethickness = 0.5, # Crosshair thickness
            spikecolor = "black", # Crosshair color
            spikedash = "line", # Make crosshairs a solid line
            showspikes = TRUE,
            range = c(min(str$DateTime), (min(str$DateTime) + months(1))) # Define x axis range
          ),
          yaxis = list(
            spikemode = "across", #Same crosshair properties but for y axis
            spikemode = "across",
            spikethickness = 0.5,
            spikecolor = "black",
            spikedash = "line",
            showspikes = TRUE
          )
        ) %>% onRender(
        "function(el, x) {
        Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')} "
      )

    })
    output$start.select = renderPrint ({
      vals$start.time = event_data("plotly_click", source = 'S')

      if (length(vals$start.time) == 0) {
        "Select a start time"
      } else {
        cat("Selected start time is \n")
        return(print(ymd_hms(vals$start.time$x, tz = 'UTC')))
      }
    })

    output$end.plot = renderPlotly({
      plot_ly(
        str,
        x = ~ DateTime,
        y = ~ Temperature,
        mode = 'lines',
        type = 'scatter',
        source = 'E',
        hoverinfo = 'none'
      ) %>%
        onRender(
          "function(el, x) {
          Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')} "
        ) %>%
        layout(
          title = 'Select END point',
          xaxis = list(
            rangeslider = list(type = "date"),
            spikemode = "across",
            spikethickness = 0.5,
            spikecolor = "black",
            spikedash = "line",
            showspikes = TRUE,
            range = c(max(str$DateTime) - months(1), (max(str$DateTime)))
          ),
          yaxis = list(
            spikemode = "across",
            spikemode = "across",
            spikethickness = 0.5,
            spikecolor = "black",
            spikedash = "line",
            showspikes = TRUE
          )
        )
    })

    output$end.select = renderPrint ({
      vals$end.time = event_data("plotly_click", source = 'E')

      if (length(vals$end.time) == 0) {
        "Select an end time"
      } else {
        cat("Selected end time is \n")
        return(print(ymd_hms(vals$end.time$x, tz = 'UTC')))
      }
    })

    observeEvent(c(vals$start.time, vals$end.time), {
      str.subset = subset(str,
                          DateTime >= vals$start.time$x &
                            DateTime <= vals$end.time$x)

      output$cut.ts = renderPlotly({
        plot_ly(
          str.subset,
          x = ~ DateTime,
          y = ~ Temperature,
          mode = 'lines',
          type = 'scatter'
        ) %>%
          layout(xaxis = list(
            range = c(vals$start.time$x, vals$end.time$x)
          ))
      })
    })


    observe({
      if (input$save > 0) {
        str.subset = subset(str,
                            DateTime >= vals$start.time$x &
                              DateTime <= vals$end.time$x)
        str.df = str.subset[c('DateTime', 'Temperature')]
        colnames(str.df) = c('UTCDateTime', 'Temperature')
        str.df$UTCDateTime = ymd_hms(str.df$UTCDateTime, tz = "UTC")

        write.csv(str.df,
                  paste0(
                    dirname(file),
                    "/",
                    file_path_sans_ext(basename(file)),
                    ".cdp"
                  ),
                  row.names = FALSE)
      }
    })

    observeEvent(input$save, {
      shinyalert("File saved",
                 type = 'success')
    })

    observeEvent(input$close, {
      js$closeWindow()
      stopApp()
    })
  }


  # Run the shiny app
  shinyApp(ui, server)
  }
