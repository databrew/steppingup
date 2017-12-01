library(shiny)
library(leaflet)
dashboardPage(skin = 'blue',
              dashboardHeader(title = "Ontario Youth Compass",
                              titleWidth = 300),
              dashboardSidebar(width = 300,

                sidebarMenu(
                  menuItem('Geographic explorer',
                           icon = icon('pencil'),
                           tabName = 'demo'),
                  menuItem('Health and wellness',
                           icon = icon('stethoscope'),
                           tabName = 'haw'),
                  menuItem('Strong, Supportive Friends and Families',
                           icon = icon('address-book-o'),
                           tabName = 'ssfaf'),
                  menuItem('Education, Training & Apprenticeships',
                           icon = icon('pencil'),
                           tabName = 'etaa'),
                  menuItem('Employment & Entrepreneurship',
                           icon = icon('suitcase'),
                           tabName = 'eae'),
                  menuItem('Diversity, Social Inclusion & Safety',
                           icon = icon('users'),
                           tabName = 'dsias'),
                  menuItem('Civic Engagement & Youth Leadership',
                           icon = icon('globe'),
                           tabName = 'ceayl'),
                  menuItem('Coordinated & Youth-Friendly Communities',
                           icon = icon('handshake-o'),
                           tabName = 'cayfc'),
                  menuItem("About",
                           icon = icon('folder-open'),
                           tabName = "about"),
                  menuItem("Download",
                           tabName = "download",
                           icon = icon("dashboard")),
                  menuItem("Widgets",
                           icon = icon("th"),
                           tabName = "widgets",
                           badgeLabel = "placeholder",
                           badgeColor = "green"))),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "demo",
                          helpText('Select a sex, age group, and year to visualize an interactive population map.'),
                          fluidRow(column(2,
                                          selectInput('demo_sex', 'Sex',
                                                      choices = c('Both', 
                                                                  'Female', 
                                                                  'Male'))),
                                   column(2,
                                          selectInput('demo_age', 'Age group',
                                                      choices = c('15 to 29 years',
                                                                  '15 to 19 years',
                                                                  '20 to 24 years',
                                                                  '25 to 29 years'))),
                                   column(2,
                                          selectInput('demo_year', 'Year',
                                                      choices = c(2001, 2006, 2011),
                                                      selected = 2011)),
                                   column(3,
                                          selectInput('palette', 'Palette',
                                                      choices = c('Yellow-red' = 'YlOrRd',
                                                                  'Spectral' = 'Spectral',
                                                                  'Blues' = 'Blues',
                                                                  'Greens' = 'Greens',
                                                                  'Purples' = 'Purples',
                                                                  'Reds' = 'Reds',
                                                                  'Black and white' = 'Greys'))),
                                   column(3,
                                          selectInput('tile',
                                                      'Background',
                                                      choices = c('OSM - Mapnik' = 'OpenStreetMap.Mapnik',
                                                                  'OSM - TopoMap' = 'OpenTopoMap',
                                                                  'Stamen - Simple BW' = 'Stamen.Toner',
                                                                  'Stamen - Watercolor' = 'Stamen.Watercolor',
                                                                  'Stamen - Terrain' = 'Stamen.Terrain',
                                                                  'ESRI - Satellite' = 'Esri.WorldImagery',
                                                                  'ESRI - Nat Geo' = 'Esri.NatGeoWorldMap')))),
                          fluidRow(column(3,
                                          selectInput('demo_pob',
                                                      'Place of birth',
                                                      choices = c('Born anywhere',
                                                                  'Born in Canada',
                                                                  'Born outside of Canada'))),
                                   column(4,
                                          selectInput('demo_vm',
                                                      'Visible minorities',
                                                      choices = c("All ethnicities","Aboriginal identity","All others","Arab","Arab/West Asian","Black","Chinese","Filipino","Japanese","Korean","Latin American","Multiple visible minorities","Multiple visible minority","Non-Aboriginal identity","South Asian","Southeast Asian","Visible minority, n.i.e.","West Asian"))),
                                   column(5,
                                          selectInput('demo_si',
                                                      'Special indicators',
                                                      choices = c("All classes of worker","Apprenticeship or trades certificate or diploma","Attended school","Attended school full time","Attended school part time","Average household income $","Average household income (before tax) $","Average household income before tax $","Bachelor's degree","Both English and French","Certificate, diploma or degree","Children","Children in couple families","Children in lone parent families","Class of worker - not applicable","Class of worker - Not applicable","College, CEGEP or other non-university certificate or diploma","College, CEGEP or other non-university certificate or diploma from a program of 3 months or more","Degree in medicine, dentistry, veterinary medicine or optometry","Did not attend school","Divorced","Earned doctorate","Employed","Employee","Employment/population ratio %","Employment rate %","Employment to population ratio %","English only","External migrants","French only","High school diploma or equivalency certificate","High school or equivalent","Internal migrants","Interprovincial migrants","In the labour force","Intraprovincial migrants","Legally married and not separated","Legally married and separated","Living alone","Living in band housing","Living in owner occupied dwelling","Living in owner-occupied dwelling","Living in rented dwelling","Living with non-relatives","Living with relatives","Lone parents","Low income (LICO after tax)","Low income (LICO before tax)","Low income (LICO before tax only)","Master's degree","Migrants","Movers","Neither English nor French","Never married (single) 15 years and over","No certificate, diploma or degree","None","None_1","None_2","Non-migrants","Non-movers","Non-subsidized housing","Not in labour force","Not in the labour force","Not low income (LICO after tax)","Not low income (LICO before tax)","Not low income (LICO before tax only)","Paid workers","Participation rate %","Person not in census families","Persons in census families","Persons not in census families","Population 15 and over by hours of unpaid care to seniors","Population 15 and over by hours of unpaid childcare","Population 15 and over by hours of unpaid housework","Population 15 years and over by labour force activity","Population 15 years and over by place of residence 5 years ago","Population 15 years and over by Place of Residence 5 years ago","Population - concept not applicable","Population - Concept not applicable","Population for the low income status variable","Self employed incorporated","Self-employed (incorporated)","Self employed unincorporated","Self-employed (unincorporated)","Some","Some_1","Some_2","Spouses and common law partners","Spouses and common-law partners","Standard error of average household income $","Standard error of average household income before tax $","Subsidized housing","Unemployed","Unemployment rate %","University certificate above bachelor level","University certificate below bachelor level","University certificate, diploma or degree","University certificate, diploma or degree at bachelor level or above","University certificate, diploma or degree at bachelor's or above","University certificate or diploma below bachelor level","Unpaid family workers","Widowed","Working for wages, salary, tips or commission")))),
                          tabsetPanel(tabPanel('Map',
                                               
                                               leafletOutput('demo_leaflet'),
                                               fluidRow(column(6,
                                                               checkboxInput('show_legend',
                                                                             'Show legend?')),
                                                        column(6,
                                                               checkboxInput('placeholder',
                                                                              'This is just a placeholder')))),
                                      tabPanel('Table',
                                               fluidRow(column(12,
                                                               # tableOutput('test')
                                                               DT::dataTableOutput('demo_table')
                                                               ))))),
                  tabItem(tabName = "haw",
                          h2("Health and wellness"),
                          plotOutput('crazy_plot2')),
                  tabItem(tabName = 'ssfaf',
                          h2('Strong, Supportive Friends and Families'),
                          plotOutput('crazy_plot3')),
                  tabItem(tabName = 'etaa',
                          h2('Education, Training & Apprenticeships'),
                          plotOutput('crazy_plot4')),
                  tabItem(tabName = 'eae',
                          h2('Employment & Entrepreneurship'),
                          plotOutput('crazy_plot5')),
                  tabItem(tabName = 'dsias',
                          h2('Diversity, Social Inclusion & Safety'),
                          plotOutput('crazy_plot6')),
                  tabItem(tabName = 'ceayl',
                          h2('Civic Engagement & Youth Leadership'),
                          plotOutput('crazy_plot7')),
                  tabItem(tabName = 'cayfc',
                          h2('Coordinated & Youth-Friendly Communities'),
                          plotOutput('crazy_plot8')),
                  tabItem(tabName = "about",
                          h2("About"),
                          h4('A collaboration with www.databrew.cc')),
                  tabItem(tabName = "download",
                          h2("Data download")),
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
                          ),
                          fluidRow(
                            box(plotOutput("plot1")),

                            box(
                              "Box content here", br(), "More box content",
                              sliderInput("slider", "Slider input:", 1, 100, 50),
                              textInput("text", "Text input:")
                            )
                          )
                  ))


              )
)

