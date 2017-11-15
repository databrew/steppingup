library(shiny)
library(leaflet)
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

  # Leaflet
  output$demo_leaflet <- renderLeaflet({

      df <- birthplace %>%
        filter(year == input$demo_year,
               age_group == input$demo_age,
               sex == input$demo_sex)
      if(!input$by_birthplace){
        df <- df %>%
          group_by(geography) %>%
          summarise(value = sum(value, na.rm = TRUE))
      } else {
        df <- df %>%
          group_by(geography) %>%
          summarise(value = sum(value[place == 'abroad'], na.rm = TRUE) /
                      sum(value[place == 'anywhere'], na.rm = TRUE) * 100)
      }


    leaf(x = df,
         tile = input$tile,
         palette = input$palette,
         show_legend = input$show_legend)
  })


  # # Demo selector
  # output$demo_selector <- renderUI({
  #   the_choices <- names(si)[2:ncol(si)]
  #   selectInput(inputId = 'demo_selection', label = 'Indicator', choices = the_choices)
  # })
  # Demo plot
  output$demo_plot <- renderPlot({
    require(ggplot2)
    # if(is.null(input$demo_selection)){
      var <- input$demo_selection
    # } else {
    #   var <- names(si)[5]
    # }
    the_data <- si[,c(1, which(names(si) == var))]
    names(the_data) <- c('Group', 'Indicator')
    the_data <- the_data[3:nrow(the_data),]
    the_data <- the_data[1:14,]
    the_data <- data.frame(the_data)
    the_data[,2] <- as.numeric(the_data[,2])
    the_data <- the_data %>% filter(!Group %in% c('Total visible minority population',
                                                  'Multiple visible minority'))
    ggplot(data = the_data,
           aes(x = Group,
               y = Indicator)) +
      theme_databrew() +
      geom_bar(stat = 'identity',
               fill = 'darkblue',
               alpha = 0.6,
               color = 'black',
               lwd = 0.1) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(y = 'People',
           title = var)
  })

}
