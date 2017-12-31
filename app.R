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
                                                tabName = "about"))),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
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
                                                uiOutput("variable"),
                                                h4(strong(textOutput('variable_text'))))),
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
                                                checkboxInput('ai',
                                                              'Aboriginal identity')),
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
                                                uiOutput('ai_filter')),
                                         column(2,
                                                uiOutput('geography_filter'))),
                                
                                tabsetPanel(
                                  tabPanel('Table',
                                           fluidRow(column(12,
                                                           textOutput('xing_text'),
                                                           DT::dataTableOutput('xing_table')
                                           ))),
                                  tabPanel('Map',
                                           textOutput('map_text'),
                                           
                                           h3(textOutput('map_title')),
                                           leafletOutput('the_map')),
                                  tabPanel('Plot', 
                                           checkboxInput('show_labels',
                                                         'Show values on charts?',
                                                         TRUE),
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
                                fluidRow(column(6,
                                                checkboxInput('want_another_var',
                                                              'Compare with a second variable?',
                                                              value = FALSE)),
                                         column(6,
                                                helpText(textOutput('compare_text')))),
                                fluidRow(
                                  column(3,
                                         uiOutput('theme_gender')),
                                  column(3,
                                         uiOutput('theme_race')),
                                  column(6)
                                ),
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
                                p('This application uses many different surveys. Select a survey below, and then click download to get entire survey dataset (processed, formatted, and filtered by Databrew) in raw form.'),
                                fluidRow(column(12,
                                                selectInput('survey_download',
                                                            'Choose a survey dataset to download',
                                                            choices = survey_download_choices))),

                                downloadButton('downloadSurvey', 'Download'),
                                br()),
                        tabItem(tabName = "about",
                                h2("About"),
                                h4('A collaboration with www.databrew.cc'))
                        
                      )))
                    



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
  
  output$compare_text <-
    renderText({
      if(input$want_another_var){
        'Note that variables can only be compared from within the same dataset. So, the list of comparison variables (to the right) depends on the exploratory variable chosen (on the left).'
      } else {
        NULL
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
  
  has_race_gender <- reactive({
    # Get the theme data name
    x <- theme_data_name()
    y <- theme_code()
    full_name <- NULL
    if(!is.null(x)){
      # Use the dataset dictionary to convert to a fuller name
      full_name <- dataset_dictionary %>%
        filter(short_name == x) 
      if(nrow(full_name) == 1){
        full_name <- full_name$long_name
      } else {
        full_name <- NULL
      }
    } 
    if(!is.null(full_name)){
      race_gender <- race_gender_dictionary %>%
        filter(data_folder == full_name)
      full_name <- race_gender
      race <- full_name %>% dplyr::filter(category == 'race') %>% .$variable_name
      gender <- full_name %>% dplyr::filter(category == 'gender') %>% .$variable_name 
      full_name <- data.frame('race' = ifelse(is.na(race), NA, race), 
                              'gender' = ifelse(is.na(gender), NA, gender))
    }
    # full_name <- paste0(x, ' ', y)
    return(full_name)
  })
  
  output$theme_gender <- renderUI({
    x <- has_race_gender()
    out <- NULL
    if(!is.null(x)){
      if(!is.na(x$gender)){
        out <- checkboxInput('theme_gender',
                             'Group by gender')
      }
    }
    return(out)
  })
  output$theme_race <- renderUI({
    x <- has_race_gender()
    out <- NULL
    if(!is.null(x)){
      if(!is.na(x$race)){
        out <- checkboxInput('theme_race',
                             'Group by race')
      }
    }
    return(out)
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
    df_full <- df
    v1 <- input$theme_var
    v2 <- input$theme_var_2
    has_two <- input$want_another_var & !is.null(input$theme_var_2)
    
    # Deal with grouping by gender and race
    by_gender <- FALSE
    by_race <- FALSE
    
    if(!is.null(df)){
      if(!is.null(input$theme_gender)){
        if(input$theme_gender){
          by_gender <- TRUE
        }
      }
      if(!is.null(input$theme_race)){
        if(input$theme_race){
          by_race <- TRUE
        }
      }
    } else {
      return(NULL)
    }
    
    
    # Get the label names of our variables
    if(!is.null(v1)){
      v1_label <- survey_dictionary %>% filter(new_variable == v1) %>% .$display_name %>% strsplit(' (', fixed = TRUE) %>% lapply(function(x){x[1]}) %>% unlist
    } else {
      v1_label <- ''
    }
    if(!is.null(v2)){
      v2_label <- survey_dictionary %>% filter(new_variable == v2) %>% .$display_name %>% strsplit(' (', fixed = TRUE) %>% lapply(function(x){x[1]}) %>% unlist
    } else {
      v2_label <- ''
    }
    
    # Subset to only include the variables we want
    keep_vars <- v1
    if(!is.null(df)){
      if(has_two){
        keep_vars <- c(keep_vars, v2)
      }
      
      # Deal with gender and race grouping
      if(by_gender){
        keep_vars <- c(keep_vars, 'gender')
      }
      if(by_race){
        keep_vars <- c(keep_vars, 'race')
      }
      
      
      if(!is.data.frame(df)){
        return(NULL)
      } else {
        # All operations go here
        # Keep only the relevant variables
        df <- df[,names(df) %in% keep_vars, drop = FALSE]
        print(head(df))
        
        if(has_two & ncol(df) >= 2){
          names(df)[1:2] <- c('v1', 'v2')
          type_1 <- class(df$v1)
          type_2 <- class(df$v2)
          type_2_numeric <- type_2 %in% c('integer', 'numeric')
          type_1_numeric <- type_1 %in% c('integer', 'numeric')
          if(type_1_numeric & type_2_numeric){
            g <- ggplot(data = df,
                        aes(x = v1,
                            y = v2)) +
              geom_point() +
              labs(x = v1_label,
                   y = v2_label)
          }
          if(type_1_numeric & !type_2_numeric){
            g <- ggplot(data = df,
                        aes(x = v1,
                            group = v2,
                            fill = v2)) +
              geom_density(alpha = 0.3) +
              labs(x = v1_label)
          }
          if(!type_1_numeric & type_2_numeric){
            g <- ggplot(data = df,
                        aes(x = v1,
                            y = v2,
                            group = v1)) +
              geom_jitter(alpha = 0.3) +
              geom_violin() +
              labs(x = v1_label,
                   y = v2_label)
          }
          
          if(!type_1_numeric & !type_2_numeric){
            cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(df$v2)))
            g <- ggplot(data = df,
                        aes(x = v1,
                            group = v2,
                            fill = v2)) +
              geom_bar(position = 'dodge') +
              labs(x = v2_label) +
              scale_fill_manual(name = v1_label,
                                values = cols) 
          }
          g <- g + theme_databrew() +
            labs(title = v1_label,
                 subtitle = v2_label) 
          
        } else {
          if(!is.data.frame(df)){ # this means there is no gender / race, it's just one vector
            df <- data.frame(v1 = df)
          } else {
            names(df)[1] <- 'v1'
          }
          type_1 <- class(df$v1)
          type_1_numeric <- type_1 %in% c('integer', 'numeric')
          if(type_1_numeric){
            g <- ggplot(data = df,
                        aes(x = v1)) +
              geom_density(fill = 'darkorange',
                           alpha = 0.6) +
              labs(x = v1_label)
          } else {
            g <- ggplot(data = df,
                        aes(x = v1)) +
              geom_bar(fill = 'darkorange',
                       alpha = 0.6) +
              labs(x = v1_label)
          }
          g <- g +
            theme_databrew() +
            labs(title = v1_label)
        }
        if(by_gender & !by_race){
          if('gender' %in% names(df)){
            g <- g + facet_wrap(~gender)
          }
        } else if(!by_gender & by_race){
          if('race' %in% names(df)){
            g <- g + facet_wrap(~race)
          }
        } else if(by_gender & by_race){
          if('race' %in% names(df) & 'gender' %in% names(df)){
            g <- g + facet_grid(~gender+race)
          }
        } 
        g <- g +
          theme(axis.text.x = element_text(angle = 90))
        return(g)
        
      }
    } else{
      NULL
    }
  })
  
  output$theme_table <- renderDataTable({
    
    # Deal with grouping by gender and race
    by_gender <- FALSE
    by_race <- FALSE
    demo_keepers <- c()
    has_two <- FALSE
    df <- NULL
    v1 <- NULL
    v2 <- NULL
    
    input$tabs # just run to refresh
    df <- theme_data()
    v1 <- input$theme_var
    v2 <- input$theme_var_2
    has_two <- input$want_another_var & !is.null(input$theme_var_2)
    

    
    if(!is.null(df)){
      if(!is.null(input$theme_gender)){
        if(input$theme_gender){
          by_gender <- TRUE
        }
      }
      if(!is.null(input$theme_race)){
        if(input$theme_race){
          by_race <- TRUE
        }
      }
    } else {
      return(NULL)
    }
    
    
    # Get the label names of our variables
    if(!is.null(v1)){
      v1_label <- survey_dictionary %>% filter(new_variable == v1) %>% .$display_name %>% strsplit(' (', fixed = TRUE) %>% lapply(function(x){x[1]}) %>% unlist
    } else {
      v1_label <- ''
    }
    if(!is.null(v2)){
      v2_label <- survey_dictionary %>% filter(new_variable == v2) %>% .$display_name %>% strsplit(' (', fixed = TRUE) %>% lapply(function(x){x[1]}) %>% unlist
    } else {
      v2_label <- ''
    }
    
    # Subset to only include the variables we want
    keep_vars <- v1
    if(!is.null(df)){
      if(has_two){
        keep_vars <- c(keep_vars, v2)
      }
      if(!is.data.frame(df)){
        return(NULL)
      } else {
        # All operations go here
        
        # Deal with gender and race grouping
        
        if(by_gender){
          keep_vars <- c(keep_vars, 'gender')
          demo_keepers <- c(demo_keepers, 'gender')
        }
        if(by_race){
          keep_vars <- c(keep_vars, 'race')
          demo_keepers <- c(demo_keepers, 'race')
        }
        
        # Keep only the relevant variables
        df <- df[,names(df) %in% unique(c(demo_keepers, keep_vars)), drop = FALSE] 

        if(is.null(df)){
          return(NULL)
        }
        
        if(has_two & ncol(df) >= 2){
          names(df)[1:2] <- c('v1', 'v2')
          type_1 <- class(df$v1)
          type_2 <- class(df$v2)
          type_2_numeric <- type_2 %in% c('integer', 'numeric')
          type_1_numeric <- type_1 %in% c('integer', 'numeric')
          if(type_1_numeric & type_2_numeric){
            if(length(demo_keepers) > 0){
              a <- df %>%
                group_by_at(demo_keepers) %>%
                summarise(average = mean(v1, na.rm = TRUE),
                          maximum = max(v1, na.rm = TRUE),
                          minimum = min(v1, na.rm = TRUE),
                          IQR = paste0(quantile(v1, c(0.25, 0.75), na.rm = TRUE), collapse = ' to '),
                          observations = length(v1),
                          NAs = length(which(is.na(v1))))
              b <- df %>%
                group_by_at(demo_keepers) %>%
                summarise(average = mean(v2, na.rm = TRUE),
                          maximum = max(v2, na.rm = TRUE),
                          minimum = min(v2, na.rm = TRUE),
                          IQR = paste0(quantile(v2, c(0.25, 0.75), na.rm = TRUE), collapse = ' to '),
                          observations = length(v2),
                          NAs = length(which(is.na(v2))))
            } else {
              a <- df %>%
                summarise(average = mean(v1, na.rm = TRUE),
                          maximum = max(v1, na.rm = TRUE),
                          minimum = min(v1, na.rm = TRUE),
                          IQR = paste0(quantile(v1, c(0.25, 0.75), na.rm = TRUE), collapse = ' to '),
                          observations = length(v1),
                          NAs = length(which(is.na(v1))))
              b <- df %>%
                summarise(average = mean(v2, na.rm = TRUE),
                          maximum = max(v2, na.rm = TRUE),
                          minimum = min(v2, na.rm = TRUE),
                          IQR = paste0(quantile(v2, c(0.25, 0.75), na.rm = TRUE), collapse = ' to '),
                          observations = length(v2),
                          NAs = length(which(is.na(v2))))
            }
            
            out <- bind_rows(
              cbind(data.frame(variable = v1_label), a),
              cbind(data.frame(variable = v2_label), b)
            )
            
          }
          if(type_1_numeric & !type_2_numeric){
            if(length(demo_keepers) > 0){
              out <- df %>%
                group_by_at(c('v2', demo_keepers)) %>%
                summarise(average = mean(v1, na.rm = TRUE),
                          maximum = max(v1, na.rm = TRUE),
                          minimum = min(v1, na.rm = TRUE),
                          IQR = paste0(quantile(v1, c(0.25, 0.75), na.rm = TRUE), collapse = ' to '),
                          observations = length(v1),
                          NAs = length(which(is.na(v1))))
            } else {
              out <- df %>%
                group_by(v2) %>%
                summarise(average = mean(v1, na.rm = TRUE),
                          maximum = max(v1, na.rm = TRUE),
                          minimum = min(v1, na.rm = TRUE),
                          IQR = paste0(quantile(v1, c(0.25, 0.75), na.rm = TRUE), collapse = ' to '),
                          observations = length(v1),
                          NAs = length(which(is.na(v1))))
            }
            names(out)[1] <- v2_label
          }
          if(!type_1_numeric & type_2_numeric){
            if(length(demo_keepers) > 0){
              out <- df %>%
                group_by_at(c('v1', demo_keepers)) %>%
                summarise(average = mean(v2, na.rm = TRUE),
                          maximum = max(v2, na.rm = TRUE),
                          minimum = min(v2, na.rm = TRUE),
                          IQR = paste0(quantile(v2, c(0.25, 0.75), na.rm = TRUE), collapse = ' to '),
                          observations = length(v2),
                          NAs = length(which(is.na(v2))))
            } else {
              out <- df %>%
                group_by(v1) %>%
                summarise(average = mean(v2, na.rm = TRUE),
                          maximum = max(v2, na.rm = TRUE),
                          minimum = min(v2, na.rm = TRUE),
                          IQR = paste0(quantile(v2, c(0.25, 0.75), na.rm = TRUE), collapse = ' to '),
                          observations = length(v2),
                          NAs = length(which(is.na(v2))))
            }
            names(out)[1] <- v1_label
          }
          if(!type_1_numeric & !type_2_numeric){
            # Both are categorical
            if(length(demo_keepers) > 0){
              out <- df %>%
                group_by_at(c('v1', 'v2', demo_keepers)) %>% tally
            } else {
              out <- df %>%
                group_by(v1, v2) %>% tally
            }
            
            names(out)[1:2] <- c(v1_label, v2_label)
          }
        } else {
          if(!is.data.frame(df)){ # this means there is no gender / race, it's just one vector
            df <- data.frame(v1 = df)
          } else {
            names(df)[1] <- 'v1'
          }
          type_1 <- class(df$v1)
          type_1_numeric <- type_1 %in% c('integer', 'numeric')
          if(type_1_numeric){
            if(length(demo_keepers) > 0){
              out <- df %>%
                group_by_at(demo_keepers) %>%
                summarise(average = mean(v1, na.rm = TRUE),
                          maximum = max(v1, na.rm = TRUE),
                          minimum = min(v1, na.rm = TRUE),
                          IQR = paste0(quantile(v1, c(0.25, 0.75), na.rm = TRUE), collapse = ' to '),
                          observations = length(v1),
                          NAs = length(which(is.na(v1))))
            } else {
              out <- df %>%
                summarise(average = mean(v1, na.rm = TRUE),
                          maximum = max(v1, na.rm = TRUE),
                          minimum = min(v1, na.rm = TRUE),
                          IQR = paste0(quantile(v1, c(0.25, 0.75), na.rm = TRUE), collapse = ' to '),
                          observations = length(v1),
                          NAs = length(which(is.na(v1))))
            }
          } else {
            if(length(demo_keepers) > 0){
              out <- df %>%
                group_by_at(c('v1', demo_keepers)) %>%
                summarise(observations = n()) %>%
                ungroup %>%
                mutate(percentage = round(observations / sum(observations) * 100, digits = 2))
            } else {
              out <- df %>%
                group_by(v1) %>%
                summarise(observations = n()) %>%
                ungroup %>%
                mutate(percentage = round(observations / sum(observations) * 100, digits = 2))
            }
            names(out)[1] <- v1_label
          }
        }
        return(prettify(out, download_options = TRUE))
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
                 ai = input$ai,
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
    
    if(input$ai & !is.null(input$ai_filter)) {
      if(input$ai_filter != 'All') {
        x <- x %>% filter(`Aboriginal identity` == input$ai_filter)
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
        plotter(censified(), variable = input$variable, show_labels = input$show_labels)
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
    filename = function() { paste(paste0('databrew_survey_',
                                         input$survey_download,
                                         collapse = ''), 
                                  '.csv', sep='') },
    content = function(file) {
      the_data <- survey[[which(names(survey) == input$survey_download)]]
      write.csv(the_data, file)})
  
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
      paste0('To generate a map with data, check the "geography" box above, select only one year, and uncheck all the others.')
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
  
  make_the_map <- reactive({
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
          make_map <- FALSE
        } else {
          make_map <- TRUE
          
        }
      } else {
        make_map <- FALSE
      }
      
    } else {
      make_map <- FALSE
    }
    return(make_map)
  })
  
  output$map_title <- renderText({
    make_it <- make_the_map()
    if(make_it){
      paste0('Map of ', tolower(input$variable))
    } else {
      NULL
    }
  })
  
  output$the_map <- renderLeaflet({
    make_it <- make_the_map()
    if(make_it){
      n <- 3
      the_title <- input$variable
      withProgress(message = 'Making map', value = 0, {
        incProgress(1/n, detail = paste("Doing part", i))
        df <- censified()
        incProgress(1/n, detail = paste("Doing part", i))
        which_var <- which(names(df) == input$variable)
        val <- as.numeric(unlist(df %>% dplyr::select_(which_var)))
        df <- df %>%
          dplyr::select(geo_code)
        incProgress(1/n, detail = paste("Doing part", i))
        df$value <- val
        leaf(x = df)#,
        # tile = input$tile,
        # palette = input$palette,
        # show_legend = input$show_legend)
      })
    } else {
      # NULL
      leaf_basic()
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
  # Variable reactive text
  output$variable_text <- renderText({
    if(is.null(input$variable) | length(input$variable) == 0){
      '(Pick at least one variable)'
    } else {
      NULL
    }
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
  # Aboriginal identity filter
  output$ai_filter <- renderUI({
    if(input$ai){
      choices <-  sort(unique(census$`Aboriginal identity`))
      choices <- c('All', choices)
      choices <- choices[!grepl('Total', choices)]
      selectInput('ai_filter',
                  'Filter',
                  choices = choices)
    }
  })
  # Main table
  no_go <- reactive({
    no_go <- FALSE
    x <- censified()
    x <- x[, names(x) %in% c(head_vector, input$variable)]
    if(is.null(input$variable)){
      no_go <- TRUE
    } else {
      non_head <- x[,!names(x) %in% head_vector]
      if(all(is.na(non_head))){
        no_go <- TRUE
      }
    }
    return(no_go)
  })
  output$xing_text <- renderText({
    ng <- no_go()
    if(ng){
      'No data available for the parameters chosen.'
    } else {
      NULL
    }
  })
  output$xing_table <- renderDataTable({
    out <- DT::datatable(data_frame())
    x <- censified()
    ng <- no_go()
    x <- x[, names(x) %in% c(head_vector, input$variable)]
    if(length(input$variable) == 0 | ng) {
      out
    } else {
      prettify(x, download_options = TRUE) 
    }
  })
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

