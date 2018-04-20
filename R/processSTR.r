processSTR = function(file,
                      write.csv = FALSE) {
  # Set environment variables to UTC
  Sys.setenv(TZ = 'UTC')

  #Load data
  test = read.csv(file, header = FALSE)[1:20 ,]

  #Find number of rows to skip in header
  skip = min(which(test == "Date")) - 1

  #Skip header
  str = read.csv(file, skip = skip)

  #Concatenate date and time
  str$DateTime = ymd_hms(paste0(str$Date, str$Time), tz = "UTC")

  #Define function for "Close Window" button in JavaScript
  jscode = 'shinyjs.closeWindow = function() { window.close(); }'

  # Create user interface. This bit of code defines what will appear on the UI.
  ui <- fluidPage(
    mainPanel(
      br(),

      # Paste file name at top of page
      h2(paste0(file_path_sans_ext(basename(
        file
      )))),

      # "Be patient" text...these datasets are huge!
      h6("(Be patient...the plots may take a minute to load!)"),
      br(),

      h5("1. Review the entire raw time series."),

      # Display the raw time series
      plotlyOutput("whole.ts", height = "200px"),

      br(),

      h5(
        "2. Click on time series below to select in situ start and end points. Gray scroll bars at bottom of plots adjust the time windows."
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

      h5(
        "3. Review the resulting trimmed time series based on start and end time selected."
      ),

      # Display the trimmed time series
      plotlyOutput("cut.ts", height = "200px"),

      h5(
        "4. If you are happy with the trimmed time series, click the 'Save' button. A 'File saved' message will appear when file has saved successfully."
      ),

      # Create a save button
      actionButton("save", "Save"),

      useShinyalert(),

      h5(
        "5. Once you have saved the file, click the 'Stop' button to stop the app."
      ),

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
        source = 'S',
        # Connect this plot to the selection of start time
        hoverinfo = 'none' # Remove the default hover boxes
      ) %>%
        layout(
          title = 'Select START point',
          xaxis = list(
            rangeslider = list(type = "date"),
            # Add a range slider at the bottom of the plot
            spikemode = "across",
            # Add crosshairs
            spikethickness = 0.5,
            # Crosshair thickness
            spikecolor = "black",
            # Crosshair color
            spikedash = "line",
            # Make crosshairs a solid line
            showspikes = TRUE,
            range = c(min(str$DateTime), (min(str$DateTime) + months(1))) # Define x axis range
          ),
          yaxis = list(
            spikemode = "across",
            #Same crosshair properties but for y axis
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
          layout(xaxis = list(range = c(
            vals$start.time$x, vals$end.time$x
          )))
      })
    })


    observe({
      if (input$save > 0) {
        str.subset = subset(str,
                            DateTime >= vals$start.time$x &
                              DateTime <= vals$end.time$x)
        str.subset$UTCDateTime = ymd_hms(str.subset$DateTime, tz = "UTC")
        str.subset$Year = year(str.subset$UTCDateTime)
        str.subset$Month = month(str.subset$UTCDateTime)
        str.subset$Day = day(str.subset$UTCDateTime)
        str.subset$Hour = hour(str.subset$UTCDateTime)
        str.subset$Minute = minute(str.subset$UTCDateTime)
        str.subset$Second = second(str.subset$UTCDateTime)

        str.df = str.subset[c("Year",
                              "Month",
                              "Day",
                              "Hour",
                              "Minute",
                              "Second",
                              "Temperature")]



        output.file = file(paste0(
          dirname(file),
          "/",
          file_path_sans_ext(basename(file)),
          ".cdp"
        ),
        "wb")

        write.table(
          str.df,
          file = output.file,
          row.names = FALSE,
          col.names = FALSE,
          sep = "\t",
          eol = "\n"
        )

        close(output.file)
        
        plot = ggplot(data = str.subset) +
          geom_line(aes(x = UTCDateTime, y = Temperature), col = 'dodgerblue') +
          theme_bw() +
          scale_x_datetime(
            breaks = date_breaks("4 months"),
            labels = date_format("%m/%y")
          ) +
          ylab("Temp (deg C)") +
          theme(
            axis.title.x = element_blank()
          )
        
        ggsave(filename = paste0(dirname(file),"/",file_path_sans_ext(basename(file)), ".png"), plot = plot, width = 12, height = 3)

        if (write.csv == TRUE) {
          write.csv(
            str.subset[c("UTCDateTime", "Temperature")],
            paste0(
              dirname(file),
              "/",
              file_path_sans_ext(basename(file)),
              "_processed.csv"
            ),
            row.names = FALSE
          )
        }
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
