server <- function(input, output) {

  options(shiny.maxRequestSize=30*1024^2)

  data <- reactive({
    req(input$csv_input)
    df <- read.csv(input$csv_input$datapath)
    df$datetime <- as.POSIXct(df$datetime)  # Convert datetime column to POSIXct object
    df$error <- abs(df$observation - df$mean)
    return(df)
    })

  observeEvent(data(),{
    site_id_choices <- c(unique(data()$site_id))
    start_date_choices <- c(unique(data()$reference_datetime))
    updateSelectInput(inputId = "site_id", choices = site_id_choices)
    updateSelectInput(inputId = "start_date", choices = start_date_choices)
  })

  site_id <- eventReactive(input$run_button,input$site_id)
  start_date <- eventReactive(input$run_button,input$start_date)

  # NEE Forecast Plot
  forecast_plot <- function(forecast_data, input_site, start_date, input_variable){
    forecast_data<- filter(forecast_data, variable == input_variable & site_id == input_site & reference_datetime == start_date & observation!="NA")

    ggplot(forecast_data, aes(x = datetime)) +
      geom_ribbon(
        aes(ymin = quantile02.5, ymax = quantile97.5, fill = "95% Confidence Interval"),
        alpha = 0.4
      ) +
      geom_line(aes(y = mean, color = "Predicted")) +
      geom_line(aes(y = observation, color = "Observed Data"), size = 1) +
      ggtitle(paste0("Forecast Horizon for ", input_site)) +
      scale_color_manual(
        name = "Legend",
        labels = c("Observed Data", "Predicted"),
        values = c("Observed Data" = "firebrick4", "Predicted" = "skyblue1")
      ) +
      scale_fill_manual(
        labels = c("95% Confidence Interval"),
        values = c("95% Confidence Interval" = "blue1")
      ) +
      scale_y_continuous(name = "NEE (kg C m-2 s-1)") +
      scale_x_datetime(
        name = "Date and Time",
        date_labels = "%Y-%m-%d",
        breaks = unique(as.POSIXct(as.Date(forecast_data$datetime))),
        labels = format(unique(as.POSIXct(as.Date(forecast_data$datetime))), "%Y-%m-%d"),
        guide = guide_axis(n.dodge = 1)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12)
      )
  }

  scatter_plot<- function(scatter_data, input_site, start_date, input_variable) {
    scatter_data<- filter(scatter_data, variable == input_variable & site_id == input_site & reference_datetime == start_date & observation!="NA")
    scatter_data$E <- scatter_data$mean
    scatter_data$O <- scatter_data$observation
    all <- c(scatter_data$E, scatter_data$O)
    RMSE <- sqrt(mean((scatter_data$E - scatter_data$O)^2, na.rm = TRUE))
    Bias <- mean(scatter_data$E - scatter_data$O, na.rm = TRUE)

    # Predicted vs Observed Scatter + 1:1 line + regression
    ggplot(scatter_data, aes(x = E, y = O)) +
      geom_point(size = 3) +
      geom_line(data = data.frame(x = c(min(all, na.rm = TRUE), max(all, na.rm = TRUE)),
                                  y = c(min(all, na.rm = TRUE), max(all, na.rm = TRUE))),
                aes(x = x, y = y), color = "darkgrey", size = 2, linetype = "solid") +
      labs(x = "Predicted", y = "Observed",
           title = paste0("Scatter Plot for ", input_site)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      annotate("text", x = 0.05, y = 0.9, label = paste0("RMSE = ", formatC(RMSE, format = "e", digits = 2)),
               xref = "paper", yref = "paper")
  }

  error_plot <- function(error_data, input_site, start_date, input_variable) {
    error_data<- filter(error_data, variable == input_variable & site_id == input_site & reference_datetime == start_date & observation!="NA")
    ggplot(error_data, aes(x = datetime, y = error, group = 1)) +
      geom_point(aes(color = datetime), size = 3) +
      geom_hline(yintercept = 0, color = "black") +
      xlab("Date") +
      ylab("LE Error (kg C m-2 s-1)") +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  }

  #NEE Forecast Plot
  nee_ft_plot <- eventReactive(input$run_button,{
    forecast_plot(data(), site_id(), start_date(), "nee")
  })


  output$nee_ft_plot <- renderPlotly(nee_ft_plot())

  #NEE Scatter PLot
  nee_sct_plot <- eventReactive(input$run_button, {
    scatter_plot(data(), site_id(), start_date(), "nee")
  })

  output$nee_sct_plot <- renderPlotly(nee_sct_plot())

  #NEE Error plot
  nee_err_plot <- eventReactive(input$run_button,{
    error_plot(data(), site_id(), start_date(), "nee")
  })


  output$nee_err_plot <- renderPlotly(nee_err_plot())

  # LE Forecast Plot
  le_ft_plot <- eventReactive(input$run_button,{
    forecast_plot(data(), site_id(), start_date(), "le")
  })


  output$le_ft_plot <- renderPlotly(le_ft_plot())

  # LE Scatter Plot
  le_sct_plot <- eventReactive(input$run_button, {
    scatter_plot(data(), site_id(), start_date(), "le")
  })

  output$le_sct_plot <- renderPlotly(le_sct_plot())

  # LE Error Plot
  le_err_plot <- eventReactive(input$run_button,{
    error_plot(data(), site_id(), start_date(), "le")
  })


  output$le_err_plot <- renderPlotly(le_err_plot())

}
