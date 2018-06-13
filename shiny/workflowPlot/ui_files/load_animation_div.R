div(
  id = "plot-container",
  div(
    class = "plotlybars-wrapper",
    div(
      class = "plotlybars",
      div(class = "plotlybars-bar b1"),
      div(class = "plotlybars-bar b2"),
      div(class = "plotlybars-bar b3"),
      div(class = "plotlybars-bar b4"),
      div(class = "plotlybars-bar b5"),
      div(class = "plotlybars-bar b6"),
      div(class = "plotlybars-bar b7")
    ),
    div(class = "plotlybars-text",
        p("Updating the plot. Hold tight!"))
  ),
  plotlyOutput(plot_var)
)