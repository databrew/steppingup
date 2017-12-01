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

  # Create a reactive dataframe for leaflet mapping
  leaflet_data <- reactive({
    df <- census_all %>%
      filter(year == input$demo_year)
    
    # Filter age or keep them all
    if(!input$demo_age == '15 to 29 years'){
      df <- df %>% 
        filter(age == input$demo_age)
    }
    
    # Filter sex or keep them all
    if(!input$demo_sex == 'Both'){
      df <- df %>% filter(sex == input$demo_sex)
    }
    
    # Filter pob or keep them all
    if(!input$demo_pob == 'Born anywhere'){
      df <- df %>% filter(pob == input$demo_pob)
    }
    
    # Filter vm or keep them all
    if(!input$demo_vm == 'All ethnicities'){
      df <- df %>% filter(vm == input$demo_vm)
    }
    
    # Require special indicators
    df <- df %>% filter(si == input$demo_si)
    
    # Aggregate
    df <- df %>%
      group_by(geography) %>%
      summarise(value = sum(value, na.rm = TRUE))
    df
  })
  
  # Test table
  output$test <- renderTable({
    leaflet_data()
  })
  
  # Leaflet
  output$demo_leaflet <- renderLeaflet({
    
    df <- leaflet_data()

    leaf(x = df,
         tile = input$tile,
         palette = input$palette,
         show_legend = input$show_legend)
  })
  
  # Table below leaflet plot
  output$demo_table <- DT::renderDataTable({
    x <- leaflet_data() %>%
      filter(geography != '3500') %>%
      mutate(percent = value / sum(value) * 100)
    prettify(x, download_options = TRUE)
  })

}
