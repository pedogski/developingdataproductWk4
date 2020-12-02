server <- function(input, output, session) {
  output$d3 <- renderD3({
    Biopsy %>%
      mutate(label = !!sym(input$var)) %>%
      group_by(label) %>%
      tally() %>%
      arrange(desc(n)) %>%
      mutate(
        y = n,
        ylabel = prettyNum(n, big.mark = ","),
        fill = ifelse(label != input$val, "#E69F00", "red"),
        mouseover = "#0072B2"
      ) %>%
      r2d3(r2d3_file)
  })
  observeEvent(input$bar_clicked, {
    updateTextInput(session, "val", value = input$bar_clicked)
  })
  output$table <- renderDataTable({
    Biopsy %>%
      filter(!!sym(input$var) == input$val) %>%
      datatable()
  })
}

