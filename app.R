library(shiny)
library(googleVis)
library(DT)
library(leaflet)
library(RColorBrewer)
library(networkD3)
options(gvis.plot.tag = 'chart')
library(shinyBS)
library(shinyLP)
library(ggplot2)
library(shinythemes)

source('global.R')

ui <- dashboardPage(skin = 'blue',
                    
                    
                    dashboardHeader(
                      title = "Ontario Youth Compass",
                      titleWidth = 300
                    ),
                    
                    dashboardSidebar(width = 300,
                                     
                                     sidebarMenu(
                                       menuItem('Welcome',
                                                icon = icon('users'),
                                                tabName = 'welcome'),
                                       menuItem('Explore census data',
                                                icon = icon('address-book-o'),
                                                tabName = 'census'),
                                       menuItem("Explore data by theme",
                                                tabName = "theme",
                                                icon = icon("dashboard")),
                                       menuItem("Download raw data",
                                                tabName = "download",
                                                icon = icon("suitcase")),
                                       menuItem("About",
                                                icon = icon('folder-open'),
                                                tabName = "about"),
                                       menuItem("Widgets",
                                                icon = icon("th"),
                                                tabName = "widgets",
                                                badgeLabel = "placeholder",
                                                badgeColor = "green"))),
                    dashboardBody(
                      #   tags$head(
                      #     tags$link(rel = "stylesheet", type = "text/css", href = "assets/css/main.css")
                      #   ),
                      #   includeHTML("www/head.html"),
                      tabItems(
                        tabItem(tabName = 'welcome',
                                jumbotron("Welcome!", "The Ontario Youth Compass tracks the wellbeing of youth across the province using data from a variety trusted sources. This web app allows for easy exploration, visualization, and access to data about youth in Ontario.",
                                          button = FALSE
                                          # buttonLabel = "Explore!"
                                ),
                                fluidRow(
                                  column(6, panel_div(class_type = "primary", panel_title = "App Directions",
                                                      content = "How to use the app<br>One day soon<br>There will be instructions here.")),
                                  column(6, panel_div("success", "App Maintenance",
                                                      "Email us: <a href='mailto:xing@databrew.cc?Subject=Stepping%20Up%20App' target='_top'>Xing Brew</a>"))
                                ),  # end of fluidRow
                                fluidRow(
                                  column(6, panel_div("info", "App Status", "Include text with status, version and updates")),
                                  column(6, panel_div("danger", "App Design and Support", "Copyright 2017 <a href='http://databrew.cc'>DataBrew Consulting Services</a>")),
                                  
                                  #### FAVICON TAGS SECTION ####
                                  tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))#,
                                  
                                  #### JAVASCRIPT TAGS SECTION #### - ENABLE WHEN READY
                                  # tags$head(tags$script(src='pl.js')), # comment this out?
                                  
                                  # bsModal("modalExample", "Instructional Video", "tabBut", size = "large" ,
                                  #         p("Additional text and widgets can be added in these modal boxes. Video plays in chrome browser"),
                                  #         iframe(width = "560", height = "315", url_link = "https://www.youtube.com/embed/X192OYoqiiM")
                                  # )
                                  
                                )),
                        tabItem(tabName = "census",
                                h2('Explore census data'),
                                helpText('I\'m looking for data about:'),
                                fluidRow(column(3,
                                                selectInput('category',
                                                            'Category',
                                                            choices = category_choices)),
                                         column(3, 
                                                uiOutput("sub_category")),
                                         column(6,
                                                uiOutput("variable"))),
                                fluidRow(column(3,
                                                radioButtons('percent',
                                                             'View as percentage or raw number',
                                                             choices = c('Percentage' = TRUE, 
                                                                         'Raw numbers' = FALSE))),
                                         column(3,
                                                checkboxGroupInput('years',
                                                                   'Year',
                                                                   choices = c('2001', '2006', '2011'),
                                                                   selected = '2011'))),
                                fluidRow(column(12,
                                                strong('Examine by sub-group(s):'))),
                                fluidRow(column(2, 
                                                checkboxInput('age',
                                                              'Age Group',
                                                              value = TRUE)),
                                         column(2,
                                                checkboxInput('sex',
                                                              'Sex',
                                                              value = TRUE)),
                                         column(2,
                                                checkboxInput('pob',
                                                              'Place of birth')),
                                         column(2,
                                                checkboxInput('vm',
                                                              'Visible minority')),
                                         column(2,
                                                checkboxInput('geography',
                                                              'Geography')),
                                         column(2)),
                                fluidRow(column(2,
                                                uiOutput('age_filter')),
                                         column(2,
                                                uiOutput('sex_filter')),
                                         column(2,
                                                uiOutput('pob_filter')),
                                         column(2,
                                                uiOutput('vm_filter')),
                                         column(2,
                                                uiOutput('geography_filter'))),
                                
                                tabsetPanel(
                                  tabPanel('Table',
                                           fluidRow(column(12,
                                                           # tableOutput('test')
                                                           DT::dataTableOutput('xing_table')
                                           ))),
                                  tabPanel('Map',
                                           textOutput('map_text'),
                                           fluidRow(column(3,
                                                           checkboxInput('show_legend',
                                                                         'Show legend?')),
                                                    column(4,
                                                           selectInput('palette', 'Palette',
                                                                       choices = c('Yellow-red' = 'YlOrRd',
                                                                                   'Spectral' = 'Spectral',
                                                                                   'Blues' = 'Blues',
                                                                                   'Greens' = 'Greens',
                                                                                   'Purples' = 'Purples',
                                                                                   'Reds' = 'Reds',
                                                                                   'Black and white' = 'Greys'))),
                                                    column(5,
                                                           selectInput('tile',
                                                                       'Background',
                                                                       choices = c('OSM - Mapnik' = 'OpenStreetMap.Mapnik',
                                                                                   'OSM - TopoMap' = 'OpenTopoMap',
                                                                                   'Stamen - Simple BW' = 'Stamen.Toner',
                                                                                   'Stamen - Watercolor' = 'Stamen.Watercolor',
                                                                                   'Stamen - Terrain' = 'Stamen.Terrain',
                                                                                   'ESRI - Satellite' = 'Esri.WorldImagery',
                                                                                   'ESRI - Nat Geo' = 'Esri.NatGeoWorldMap')))),
                                           leafletOutput('the_map')),
                                  tabPanel('Plot', 
                                            plotOutput('bar_plot')))),
                        tabItem(tabName = "theme",
                                h2('Explore data by theme'),
                                p('In 2013, the Government of Ontario adopted Stepping Up as the province’s evidence-based framework for improving youth outcomes. As an evidence-based framework, Stepping Up aims to consolidate and harmonize decision-making and program planning in Ontario’s youth-serving sectors to support youth wellbeing. This framework has guided both the development and implementation of youth initiatives by specifying seven themes for youth wellbeing.'),
                                p('You can explore various data sets under each of the Stepping Up themes below.'),
                                tabsetPanel(id = "tabs",
                                  tabPanel(title = 'Health and wellness'),
                                  tabPanel(title = 'Supportive families'),
                                  tabPanel(title = 'Education'),
                                  tabPanel(title = 'Employment'),
                                  tabPanel(title = 'Civic engagement'),
                                  tabPanel(title = 'Diversity'),
                                  tabPanel(title = 'Communities')
                                ),
                                fluidRow(column(6,
                                                uiOutput('theme_var')),
                                         column(6,
                                                uiOutput('theme_var_2'))),
                                fluidRow(column(12,
                                                checkboxInput('want_another_var',
                                                              'Compare with a second variable?',
                                                              value = FALSE))),
                                # fluidRow(textOutput('fake_text')),
                                tabsetPanel(
                                  tabPanel('Table',
                                           fluidRow(column(12,
                                                           DT::dataTableOutput('theme_table')
                                           ))),
                                  tabPanel('Plot',
                                           fluidRow(column(12,
                                                           plotOutput('theme_plot')))))
                                ),
                        tabItem(tabName = "download",
                                h2("Data download"),
                                br(),
                                h3('Census data'),
                                p('Click below to download the entire census dataset (processed, formatted, and filtered by Databrew). Warning: This file is nearly 30MB large; depending on your internet connection speed, this download can be slow.'),
                                downloadButton('downloadData', 'Download'),
                                h3('Survey data'),
                                p('Click below to download the entire survey dataset (processed, formatted, and filtered by Databrew). This is just a placeholder - data not yet available for download.'),
                                downloadButton('downloadSurvey', 'Download'),
                                br()),
                        tabItem(tabName = "about",
                                h2("About"),
                                h4('A collaboration with www.databrew.cc')),
                        tabItem(tabName = "widgets",
                                h2("Widgets tab content"),
                                # infoBoxes with fill=FALSE
                                fluidRow(
                                  # A static infoBox
                                  infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                                  # Dynamic infoBoxes
                                  infoBoxOutput("progressBox"),
                                  infoBoxOutput("approvalBox")
                                ),
                                # infoBoxes with fill=TRUE
                                fluidRow(
                                  infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
                                  infoBoxOutput("progressBox2"),
                                  infoBoxOutput("approvalBox2")
                                ),
                                
                                fluidRow(
                                  # Clicking this will increment the progress amount
                                  box(width = 4, actionButton("count", "Increment progress"))
                                ),
                                fluidRow(
                                  box(title = "Box title", "Box content"),
                                  box(status = "warning", "Box content")
                                ),
                                
                                fluidRow(
                                  box(
                                    title = "Title 1", width = 4, solidHeader = TRUE, status = "primary",
                                    "Box content"
                                  ),
                                  box(
                                    title = "Title 2", width = 4, solidHeader = TRUE,
                                    "Box content"
                                  ),
                                  box(
                                    title = "Title 1", width = 4, solidHeader = TRUE, status = "warning",
                                    "Box content"
                                  )
                                ),
                                
                                fluidRow(
                                  box(
                                    width = 4, background = "black",
                                    "A box with a solid black background"
                                  ),
                                  box(
                                    title = "Title 5", width = 4, background = "light-blue",
                                    "A box with a solid light-blue background"
                                  ),
                                  box(
                                    title = "Title 6",width = 4, background = "maroon",
                                    "A box with a solid maroon background"
                                  )
                                ),
                                
                                fluidRow(
                                  column(width = 4,
                                         box(
                                           title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
                                           "Box content"
                                         ),
                                         box(
                                           width = NULL, background = "black",
                                           "A box with a solid black background"
                                         )
                                  ),
                                  
                                  column(width = 4,
                                         box(
                                           title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
                                           "Box content"
                                         ),
                                         box(
                                           title = "Title 5", width = NULL, background = "light-blue",
                                           "A box with a solid light-blue background"
                                         )
                                  ),
                                  
                                  column(width = 4,
                                         box(
                                           title = "Title 2", width = NULL, solidHeader = TRUE,
                                           "Box content"
                                         ),
                                         box(
                                           title = "Title 6", width = NULL, background = "maroon",
                                           "A box with a solid maroon background"
                                         )
                                  )
                                )
                                
                        ))
                      
                      
                    )
)


# Define server 
server <- function(input, output) {
  
  # reactive object for theme
  theme_code <- reactive({
    input$tabs # just run to refresh
    x <- theme_dictionary %>% filter(long_name == input$tabs)
    x <- x$short_name
    return(x)
  })
  
  # reactive for choosing themes
  theme_choices <- reactive({
    x <- survey_dictionary
    x <- x %>% filter(theme_name == theme_code())
    x$new_variable
  })
  theme_choices_labels <- reactive({
    x <- survey_dictionary
    x <- x %>% filter(theme_name == theme_code())
    x$display_name
  })
  
  output$theme_var <- renderUI({
    input$tabs # just run to refresh
    x <- theme_choices()
    names(x) <- theme_choices_labels()
    selectInput('theme_var',
                'Choose a variable to explore',
                choices = x)
  })
  
  # reactive data set NAME based on the input$theme_var
  theme_data_name <- reactive({
    if(!is.null(input$theme_var)){
      var1 <- input$theme_var
      x <- var_summary
      x$data_set <- unlist(lapply(strsplit(x$new_variable, '_'), function(x) x[2]))
      x <- x %>% filter(new_variable == var1)
      return(x$data_set[1])
    } else {
      return(NULL)
    }
  })
  
  # reactive dataset based on the theme_data_name
  theme_data <- reactive({
    the_name <- theme_data_name()
    if(is.null(the_name)){
      NULL
    } else {
      full_name <- dataset_dictionary %>%
        filter(short_name == the_name) %>%
        .$long_name
      x <- survey[[which(names(survey) == full_name)]]
      x
    }
  })
  
  # reactive object for second choice 
  theme_choices_2 <- reactive({
    input$tabs # just for refreshing
    x <- survey_dictionary
    x <- x %>% filter(short_name == theme_data_name())
    out <- x$new_variable
    dd <- theme_data()
    out <- out[out %in% names(dd)]
    
    if(length(out) == 0){
      return(NULL)
    } else{
      return(out)
    }
  })

  theme_choices_labels_2 <- reactive({
    x <- survey_dictionary
    x <- x %>% filter(short_name == theme_data_name())
    out <- x$display_name
    original_var_name <- x$new_variable
    dd <- theme_data()
    out <- out[original_var_name %in% names(dd)]
    if(length(out) == 0){
      return(NULL)
    } else{
      return(out)
    }
  })
  
  output$theme_var_2 <- renderUI({
    input$tabs # just run to refresh
    input$want_another_var # just run to refresh
    if(is.null(input$theme_var) | !input$want_another_var) {
      return(NULL)
    } else {
      x <- theme_choices_2()
      if(is.null(x)){
        return(NULL)
      } else {
        names(x) <- theme_choices_labels_2()
        selectInput('theme_var_2',
                    'Choose a variable to compare',
                    choices = x)  
      }
    }
  })
  
  # output$fake_text <- renderText({
  #   paste0('Theme data name is ', theme_data_name(), '\n',
  #          'var 1 is ', input$theme_var, '\n',
  #          'var 2 is ', input$theme_var_2)
  # })
  
  output$theme_plot <- renderPlot({
    input$tabs # just run to refresh
    df <- theme_data()
    v1 <- input$theme_var
    v2 <- input$theme_var_2
    has_two <- input$want_another_var & !is.null(input$theme_var_2)
    # Subset to only include the variables we want
    keep_vars <- v1
    if(!is.null(df)){
      if(has_two){
        keep_vars <- c(keep_vars, v2)
        # Keep only the relevant variables
        df <- df[,names(df) %in% keep_vars]  
      }
      if(!is.data.frame(df)){
        return(NULL)
      } else {
        # All operations go here
        # Keep only the relevant variables
        df <- df[,names(df) %in% keep_vars]
        
        if(has_two){
          names(df) <- c('v1', 'v2')
          type_1 <- class(df$v1)
          type_2 <- class(df$v2)
          type_2_numeric <- type_2 %in% c('integer', 'numeric')
          type_1_numeric <- type_1 %in% c('integer', 'numeric')
          if(type_1_numeric & type_2_numeric){
            g <- ggplot(data = df,
                        aes(x = v1,
                            y = v2)) +
              geom_point()
          }
          if(type_1_numeric & !type_2_numeric){
            g <- ggplot(data = df,
                        aes(x = v1,
                            group = v2,
                            fill = v2)) +
              geom_density(alpha = 0.3)
          }
          if(!type_1_numeric & type_2_numeric){
            g <- ggplot(data = df,
                        aes(x = v1,
                            y = v2,
                            group = v1)) +
              geom_jitter(alpha = 0.3) +
              geom_violin()
          }
          if(!type_1_numeric & !type_2_numeric){
            g <- ggplot(data = df,
                        aes(x = v1,
                            group = v2,
                            fill = v2)) +
              geom_bar(position = 'dodge')
          }
          g <- g + theme_databrew() +
            labs(title = v1,
                 subtitle = v2,
                 x = '',
                 y = '')
          
        } else {
          df <- data.frame(v1 = df)
          type_1 <- class(df$v1)
          type_1_numeric <- type_1 %in% c('integer', 'numeric')
          if(type_1_numeric){
            g <- ggplot(data = df,
                        aes(x = v1)) +
              geom_density(fill = 'darkorange',
                           alpha = 0.6)
          } else {
            g <- ggplot(data = df,
                        aes(x = v1)) +
              geom_bar(fill = 'darkorange',
                       alpha = 0.6) 
          }
          g <- g +
            theme_databrew() +
            labs(title = theme_choices_labels(),
                 x = '',
                 y = '')
        }
        return(g)
        
      }
    } else{
      NULL
    }
  })
  
  output$theme_table <- renderDataTable({
    input$tabs # just run to refresh
    df <- theme_data()
    v1 <- input$theme_var
    v2 <- input$theme_var_2
      has_two <- input$want_another_var & !is.null(input$theme_var_2)
      # Subset to only include the variables we want
      keep_vars <- v1
      if(!is.null(df)){
        if(has_two){
          keep_vars <- c(keep_vars, v2)
          # Keep only the relevant variables
          df <- df[,names(df) %in% keep_vars]  
        }
        if(!is.data.frame(df)){
          return(NULL)
        } else {
          # All operations go here
          v1 <- input$theme_var
          v2 <- input$theme_var_2
          input$tabs # just run to refresh
          has_two <- input$want_another_var & !is.null(input$theme_var_2)
          # Subset to only include the variables we want
          keep_vars <- v1
          if(has_two){
            keep_vars <- c(keep_vars, v2)
          }
          # Keep only the relevant variables
          df <- df[,names(df) %in% keep_vars]
          head(df)
          
          if(has_two){
            names(df) <- c('v1', 'v2')
            type_1 <- class(df$v1)
            type_2 <- class(df$v2)
            type_2_numeric <- type_2 %in% c('integer', 'numeric')
            type_1_numeric <- type_1 %in% c('integer', 'numeric')
            if(type_1_numeric & type_2_numeric){
              out <- data.frame(a = 1, b = 2)
            }
            if(type_1_numeric & !type_2_numeric){
              out <- data.frame(a = 1, b = 2)
            }
            if(!type_1_numeric & type_2_numeric){
              out <- data.frame(a = 1, b = 2)
            }
            if(!type_1_numeric & !type_2_numeric){
              out <- data.frame(a = 1, b = 2)
            }
            names(out) <- c(v1, v2)
            
          } else {
            df <- data.frame(v1 = df)
            type_1 <- class(df$v1)
            type_1_numeric <- type_1 %in% c('integer', 'numeric')
            if(type_1_numeric){
              out <- data.frame(z = 1)
            } else {
              out <- data.frame(z = 1)
            }
            names(out) <- v1
          }
          return(out)
        }
      } else{
        NULL
      }
  })
  

  # Reactive census object
  censified <- reactive({
    choices <- unique(census_dict$sub_category[census_dict$category == input$category])
    
    if(length(choices) == 1) {
      sc <- input$category
    } else {
      sc <- input$sub_category 
    }
    
    x <- censify(df = census, dict = census_dict, 
            age = input$age, 
            sex = input$sex,
            pob = input$pob,
            vm = input$vm,
            geo_code = input$geography,
            years = input$years,
            sc = sc,
            percent = input$percent)
    
    if(input$age & !is.null(input$age_filter)) {
      if(input$age_filter != 'All') {
        x <- x %>% filter(`Age group` == input$age_filter)
      }
    }
    
    if(input$sex & !is.null(input$sex_filter)) {
      if(input$sex_filter != 'All') {
        x <- x %>% filter(Sex == input$sex_filter)
      }
    }
    
    if(input$pob & !is.null(input$pob_filter)) {
      if(input$pob_filter != 'All') {
        x <- x %>% filter(`Place of birth` == input$pob_filter)
      }
    }
    
    if(input$vm & !is.null(input$vm_filter)) {
      if(input$vm_filter != 'All') {
        x <- x %>% filter(`Visible minority` == input$vm_filter)
      }
    }
    
    if(input$geography & !is.null(input$geography_filter)) {
      if(input$geography_filter != 'All') {
        x <- x %>% filter(Geography == input$geography_filter)
      }
    }
    return(x)
  })
  
  # Misc approval box
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  # Misc approval box
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  # Age filter
  output$age_filter <- renderUI({
    if(input$age){
      choices <-  sort(unique(census$`Age group`))
      choices <- c('All', choices)
      choices <- choices[!grepl('Total', choices)]
      selectInput('age_filter',
                  'Filter',
                  choices = choices)
    }
  })
  
  # barplot 
  output$bar_plot <- renderPlot({
    if(input$category == 'income'){
      ggplot() +
        theme_databrew() +
          labs(title = 'Income variables are not visualizable yet.')
    } else {
      if(is.null(input$variable) | length(input$variable) == 0){
        ggplot() +
          theme_databrew() +
          labs(title = 'You must select a variable to plot')
      } else {
        plotter(censified(), variable = input$variable)
      }
    }
  })
  # Download table
  output$downloadData <- downloadHandler(
    filename = function() { paste('databrew', '.csv', sep='') },
    content = function(file) {
      write.csv(census, file)})
  # Geography filter
  output$geography_filter <- renderUI({
    if(input$geography){
      choices <-  sort(unique(census$Geography))
      choices <- c('All', choices)
      choices <- choices[!grepl('Total', choices)]
      selectInput('geography_filter',
                  'Filter',
                  choices = choices)
    }
  })
  # Download survey
  output$downloadSurvey <- downloadHandler(
    filename = function() { paste('databrew_survey', '.csv', sep='') },
    content = function(file) {
      write.csv(data.frame(a = 'placeholer',
                           b = 'no',
                           c = 'data',
                           d = 'yet'), file)})
  # Leaflet
  output$map_text <- renderText({
    make_map <- FALSE
    if(input$geography &
       !input$age &
       !input$sex &
       !input$pob &
       !input$vm &
       length(input$years) == 1){
      make_map <- TRUE
    }
    if(!make_map){
      paste0('To generate a map, check the "geography" box above, select only one year, and uncheck all the others.')
    }
  })
  # Place of birth filter
  output$pob_filter <- renderUI({
    if(input$pob){
      choices <-  sort(unique(census$`Place of birth`))
      choices <- c('All', choices)
      choices <- choices[!grepl('Total', choices)]
      selectInput('pob_filter',
                  'Filter',
                  choices = choices)
    }
  })
  # Progress box
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  # Progress box
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  # Sex filter
  output$sex_filter <- renderUI({
    if(input$sex){
      choices <-  sort(unique(census$Sex))
      choices <- c('All', choices)
      choices <- choices[!grepl('Total', choices)]
      selectInput('sex_filter',
                  'Filter',
                  choices = choices)
    }
  })
  # Sub category UI
  output$sub_category <- renderUI({
    choices <- unique(census_dict$sub_category[census_dict$category == input$category])
    if(length(choices) == 1) {
      return(NULL)
    } else {
      names(choices) <- Hmisc::capitalize(gsub('_', ' ', choices))
      radioButtons('sub_category',
                   'Sub Category',
                   choices = choices,
                   selected = choices[1])}})
  
  output$the_map <- renderLeaflet({
    make_map <- FALSE
    if(input$geography &
       !input$age &
       !input$sex &
       !input$pob &
       !input$vm &
       length(input$years) == 1){
      make_map <- TRUE
    }
    if(make_map){
      df <- censified()
      if(length(input$variable) == 1){
        which_var <- which(names(df) == input$variable)
        val <- as.numeric(unlist(df %>% dplyr::select_(which_var)))
        if(all(is.na(val))){
          return(NULL)
        } else {
          df <- df %>%
            dplyr::select(geo_code)
          df$value <- val
          leaf(x = df,
               tile = input$tile,
               palette = input$palette,
               show_legend = input$show_legend)
        }
      } else {
        NULL
      }
      
    } else {
      NULL
      # leaflet() %>%
      #   addTiles()
    }
  })
  # Variable selection
  variable_choices <- reactive({
    x <- censified()
    out <- input$variable
    x <- names(x)
    x <- x[!x %in% head_vector]
    x <- x[!grepl('Total', x)]
    if(all(out %in% x)){
      out <- out
    } else {
      out <- x[1]
    }
    return(out)
  })
  output$variable <- renderUI({
    x <- censified()
    x <- names(x)
    x <- x[!x %in% head_vector]
    x <- x[!grepl('Total', x)]
    selectInput('variable', 
                'Variable',
                choices = x,
                selected = variable_choices(),
                # selected = x[1],
                multiple = TRUE)
  })
  # Visible minority filter
  output$vm_filter <- renderUI({
    if(input$vm){
      choices <-  sort(unique(census$`Visible minority`))
      choices <- c('All', choices)
      choices <- choices[!grepl('Total', choices)]
      selectInput('vm_filter',
                  'Filter',
                  choices = choices)
    }
  })
  # Main table
  output$xing_table <- renderDataTable({
    x <- censified()
    x <- x[, names(x) %in% c(head_vector, input$variable)]
    if(length(input$variable) == 0) {
      DT::datatable(data_frame())
    } else {
      prettify(x, download_options = TRUE) 
    }
  })
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

