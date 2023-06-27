server <- function(input, output) {
  
  # Filter the data based on the selected site_id, start_date and the variable
  data()$datetime <- as.POSIXct(data()$datetime)
  data()$error <- abs(data()$observation - data()$mean)
  data
  filtered_data_nee <- reactive({
    filter(data, variable == "nee" & observation != "NA" & site_id == input$site_id & reference_datetime == input$start_date)
  })
  filtered_data_le <- reactive({
    filter(data, variable == "le"  & site_id == input$site_id & reference_datetime == input$start_date)
  })
  
  # NEE Forecast Plot
  output$nee_ft_plot <- renderPlotly({
    p <- ggplot(filtered_data_nee(), aes(x = datetime)) +
      geom_ribbon(
        aes(ymin = quantile02.5, ymax = quantile97.5, fill = "95% Confidence Interval"),
        alpha = 0.4
      ) +
      geom_line(aes(y = mean, color = "Predicted")) +
      geom_line(aes(y = observation, color = "Observed Data"), size = 1) +
      ggtitle(paste0("Forecast Horizon for ", input$site_id)) +
      scale_color_manual(
        name = "Legend",
        labels = c("Observed Data", "Predicted"),
        values = c("Observed Data" = "firebrick4", "Predicted" = "skyblue1")
      ) +
      scale_fill_manual(
        labels = c("95% Confidence Interval"),
        values = c("95% Confidence Interval" = "blue1")
      ) +
      scale_y_continuous(name = "NEE Error (kg C m-2 s-1)") +
      scale_x_datetime(
        name = "Date and Time",
        date_labels = "%Y-%m-%d",
        breaks = unique(as.POSIXct(as.Date(filtered_data_nee()$datetime))),
        labels = format(unique(as.POSIXct(as.Date(filtered_data_nee()$datetime))), "%Y-%m-%d"),
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
    
    ggplotly(p)
  })
  
  # NEE Scatter Plot
  output$nee_sct_plot <- renderPlotly({
    E <- filtered_data_nee()$mean
    O <- filtered_data_nee()$observation
    all <- c(E, O)
    RMSE <- sqrt(mean((E - O)^2, na.rm = TRUE))
    Bias <- mean(E - O, na.rm = TRUE)
    
    # Predicted vs Observed Scatter + 1:1 line + regression
    plot_ly(x = E, y = O, type = "scatter", mode = "markers", marker = list(size = 3)) %>%
      add_trace(x = c(min(all, na.rm = TRUE), max(all, na.rm = TRUE)),
                y = c(min(all, na.rm = TRUE), max(all, na.rm = TRUE)),
                type = "scatter", mode = "lines", line = list(color = "darkgrey", width = 2)) %>%
      layout(
        xaxis = list(title = "Predicted"),
        yaxis = list(title = "Observed"),
        title = paste0("Scatter Plot for ", input$site_id),
        showlegend = FALSE,
        annotations = list(
          x = 0.05, y = 0.9,
          text = paste0("RMSE = ", formatC(RMSE, format = "e", digits = 2)),
          xref = "paper", yref = "paper",
          showarrow = FALSE
        )
      )
  })
  
  #NEE Error plot
  output$nee_err_plot <- renderPlotly({
    ggplot(filtered_data_nee(), aes(x = datetime, y = error, group = 1)) + 
      geom_point(aes(color = datetime), size = 3) + 
      geom_hline(yintercept = 0, color = "black") + 
      xlab("Date") + 
      ylab("NEE Error (kg C m-2 s-1)") + 
      theme_minimal() + 
      theme(
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  })
  
  # LE Forecast Plot
  output$le_ft_plot <- renderPlotly({
    le_p <- ggplot(filtered_data_le(), aes(x = datetime)) +
      geom_ribbon(
        aes(ymin = quantile02.5, ymax = quantile97.5, fill = "95% Confidence Interval"),
        alpha = 0.4
      ) +
      geom_line(aes(y = mean, color = "Predicted")) +
      geom_line(aes(y = observation, color = "Observed Data"), size = 1) +
      ggtitle(paste0("Forecast Horizon for ", input$site_id)) +
      scale_color_manual(
        name = "Legend",
        labels = c("Observed Data", "Predicted"),
        values = c("Observed Data" = "firebrick4", "Predicted" = "skyblue1")
      ) +
      scale_fill_manual(
        labels = c("95% Confidence Interval"),
        values = c("95% Confidence Interval" = "blue1")
      ) +
      scale_y_continuous(name = "LE Error (kg C m-2 s-1)") +
      scale_x_datetime(
        name = "Date and Time",
        date_labels = "%Y-%m-%d",
        breaks = unique(as.POSIXct(as.Date(filtered_data_le()$datetime))),
        labels = format(unique(as.POSIXct(as.Date(filtered_data_le()$datetime))), "%Y-%m-%d"),
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
    
    ggplotly(le_p)
  })
  
  # LE Scatter Plot
  output$le_sct_plot <- renderPlotly({
    E <- filtered_data_le()$mean
    O <- filtered_data_le()$observation
    all <- c(E, O)
    RMSE <- sqrt(mean((E - O)^2, na.rm = TRUE))
    Bias <- mean(E - O, na.rm = TRUE)
    
    # Predicted vs Observed Scatter + 1:1 line + regression
    plot_ly(x = E, y = O, type = "scatter", mode = "markers", marker = list(size = 3)) %>%
      add_trace(x = c(min(all, na.rm = TRUE), max(all, na.rm = TRUE)),
                y = c(min(all, na.rm = TRUE), max(all, na.rm = TRUE)),
                type = "scatter", mode = "lines", line = list(color = "darkgrey", width = 2)) %>%
      layout(
        xaxis = list(title = "Predicted"),
        yaxis = list(title = "Observed"),
        title = paste0("Scatter Plot for ", input$site_id),
        showlegend = FALSE,
        annotations = list(
          x = 0.05, y = 0.9,
          text = paste0("RMSE = ", formatC(RMSE, format = "e", digits = 2)),
          xref = "paper", yref = "paper",
          showarrow = FALSE
        )
      )
  })
  
  # LE Error Plot
  output$le_err_plot <- renderPlotly({
    ggplot(filtered_data_le(), aes(x = datetime, y = error, group = 1)) + 
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
  })
  
}
