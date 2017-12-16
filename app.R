library(shiny)
library(googleVis)
library(DT)
library(leaflet)
library(RColorBrewer)
library(networkD3)
options(gvis.plot.tag = 'chart')
library(shinyBS)
library(shinyLP)
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
                                      menuItem("Download",
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
                               fluidRow(column(4,
                                               selectInput('category',
                                                           'Category',
                                                           choices = category_choices)),
                                        column(4, 
                                               uiOutput("sub_category")),
                                        column(4,
                                               uiOutput("variable"))),
                               fluidRow(column(6,
                                               radioButtons('percent',
                                                            'View as percentage or raw number',
                                                            choices = c('Percentage' = TRUE, 
                                                                        'Raw numbers' = FALSE))),
                                        column(6,
                                               checkboxGroupInput('years',
                                                                  'Year',
                                                                  choices = c('2001', '2006', '2011'),
                                                                  selected = '2001'))),
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
                                          p('To be finished on December 16'),
                                                    
                                                    leafletOutput('demo_leaflet'),
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
                                                                                            'ESRI - Nat Geo' = 'Esri.NatGeoWorldMap'))))),
                                          tabPanel('Plot',
                                                   fluidRow(column(12,
                                                                   p('To be finished on Dec 16')))))),
                       tabItem(tabName = "theme",
                               h2('Explore data by theme'),
                               p('In 2013, the Government of Ontario adopted Stepping Up as the province’s evidence-based framework for improving youth outcomes. As an evidence-based framework, Stepping Up aims to consolidate and harmonize decision-making and program planning in Ontario’s youth-serving sectors to support youth wellbeing. This framework has guided both the development and implementation of youth initiatives by specifying seven themes for youth wellbeing.'),
                               p('You can explore various data sets under each of the Stepping Up themes below.'),
                               tabsetPanel(
                                 tabPanel(title = 'Health and wellness'),
                                 tabPanel(title = 'Supportive families'),
                                 tabPanel(title = 'Education'),
                                 tabPanel(title = 'Employment'),
                                 tabPanel(title = 'Civic engagement'),
                                 tabPanel(title = 'Diversity'),
                                 tabPanel(title = 'Communities'),
                                 tabPanel(title = 'All themes')
                               )),
                       tabItem(tabName = "download",
                               h2("Data download"),
                               br(),
                               downloadButton('downloadData', 'Download'),
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


# Define server logic for random distribution app ----
server <- function(input, output) {
  
  censified <- reactive({
    choices <- unique(census_dict$sub_category[census_dict$category == input$category])
    
    if(length(choices) == 1) {
      sc <- input$category
    } else {
      sc <- input$sub_category 
    }
    
  censify(df = census, dict = census_dict, 
          age = input$age, 
                 sex = input$sex,
                 pob = input$pob,
                 vm = input$vm,
                 geo_code = input$geography,
                 years = input$years,
                 sc = sc,
                 percent = input$percent)
    
  })
  
  output$xing_table <- renderDataTable({
    x <- censified()
    x <- x[, names(x) %in% c(head_vector, input$variable)]
    if(length(input$variable) == 0) {
      DT::datatable(data_frame())
    } else {
      prettify(x, download_options = TRUE) 
    }
  })
  output$sub_category <- renderUI({
    choices <- unique(census_dict$sub_category[census_dict$category == input$category])
    if(length(choices) == 1) {
      return(NULL)
    } else {
      names(choices) <- Hmisc::capitalize(gsub('_', ' ', choices))
      radioButtons('sub_category',
                   'Sub Category',
                   choices = choices,
                   selected = choices[1])
    }
    
  })
  
  output$variable <- renderUI({
    x <- censified()
    x <- names(x)
    x <- x[!x %in% head_vector]
    x <- x[!grepl('Total', x)]
    selectInput('variable', 
                'Variable',
                choices = x,
                selected = x[1],
                multiple = TRUE)
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
    
    # x <- censified()
    # if(!'geo_code' %in% names(x) |
    #    'Age group' %in% names(x) |
    #    'Sex' %in% names(x) |
    #    'Place of birth' %in% names(x) |
    #    'Visible minority' %in% names(x)){
    #   return(NULL)
    # } else {
    #   
    # }
    
    leaflet() %>%
      addTiles()
    # df <- leaflet_data()
    # 
    # leaf(x = df,
    #      tile = input$tile,
    #      palette = input$palette,
    #      show_legend = input$show_legend)
  })
  
  # Table below leaflet plot
  output$demo_table <- DT::renderDataTable({
    x <- leaflet_data()
    prettify(x, download_options = TRUE)
  })
  
  # Download table
  output$downloadData <- downloadHandler(
    filename = function() { paste('databrew', '.csv', sep='') },
    content = function(file) {
      write.csv(census, file)})
  
  output$age_filter <- renderUI({
    if(input$age){
      selectInput('age_filter',
                  'Filter',
                  choices = sort(unique(censified()$`Age group`)))
    }
  })
  
  output$sex_filter <- renderUI({
    if(input$sex){
      selectInput('sex_filter',
                  'Filter',
                  choices = sort(unique(censified()$`Sex`)))
    }
  })
  
  output$pob_filter <- renderUI({
    if(input$pob){
      selectInput('pob_filter',
                  'Filter',
                  choices = sort(unique(censified()$`Place of birth`)))
    }
  })
  
  output$vm_filter <- renderUI({
    if(input$vm){
      selectInput('vm_filter',
                  'Filter',
                  choices = sort(unique(censified()$`Visible minority`)))
    }
  })
  
  output$geography_filter <- renderUI({
    if(input$geography){
      selectInput('geography_filter',
                  'Filter',
                  choices = sort(unique(censified()$Geography)))
    }
  })
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

