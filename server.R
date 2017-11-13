function(input, output) {

  output$crazy_plot1 <- renderPlot({
    crazy_map()
  })
  output$crazy_plot2 <- renderPlot({
    crazy_map()
  })
  output$crazy_plot3 <- renderPlot({
    crazy_map()
  })
  output$crazy_plot4 <- renderPlot({
    crazy_map()
  })
  output$crazy_plot5 <- renderPlot({
    crazy_map()
  })
  output$crazy_plot6 <- renderPlot({
    crazy_map()
  })
  output$crazy_plot7 <- renderPlot({
    crazy_map()
  })

  output$crazy_plot8 <- renderPlot({
    crazy_map()
  })



  output$plot1 <- renderPlot({
    hist(rnorm(n = input$slider))
    title(main = 'Some title')
  })

  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })

  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
}
