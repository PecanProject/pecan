bs_accordion_sidebar(id = "documentation",
                     spec_side = c(width = 3, offset = 0),
                     spec_main = c(width = 9, offset = 0)) %>%
  bs_append(
    title_side = "App Documentation", 
    content_side = NULL,
    content_main = withMathJax(includeMarkdown("markdown/app_documentation.Rmd"))
  ) %>%
  bs_append(
    title_side = "Setup Page", 
    content_side = NULL,
    content_main = withMathJax(includeMarkdown("markdown/setup_page.Rmd"))
  ) %>%
  bs_append(
    title_side = "History Runs", 
    content_side = NULL,
    content_main = HTML("
                        <p>This page is for seaching history runs.</p>
                        <p>If you don\'t know the workflow Id to select in the first panel, use this page to explore all the runs at all sites.
                        <br>Select the one you wish to explore using the explore button.</p>
                        ")
  ) %>%
  bs_append(
    title_side = "Exploratory Plots", 
    content_side = NULL,
    content_main = withMathJax(includeMarkdown("markdown/exploratory_plot.Rmd"))
  ) %>%
  bs_append(
    title_side = "Benchmarking", 
    content_side = NULL,
    content_main = 
      bs_accordion_sidebar(id = "benchmarking") %>%
      bs_append(
        title_side = "Settings",
        content_side = NULL,
        content_main = withMathJax(includeMarkdown("markdown/benchmarking_setting.Rmd"))
      ) %>%
      bs_append(
        title_side = "Scores",
        content_side = NULL,
        content_main = withMathJax(includeMarkdown("markdown/benchmarking_scores.Rmd"))
      ) %>% 
      bs_append(
        title_side = "Plots",
        content_side = NULL,
        content_main = withMathJax(includeMarkdown("markdown/benchmarking_plots.Rmd"))
      )
  )
