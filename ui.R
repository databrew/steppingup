dashboardPage(skin = 'blue',
              dashboardHeader(title = "Stepping Up",
                              titleWidth = 300),
              dashboardSidebar(width = 300,

                sidebarMenu(
                  menuItem('Demo',
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
                          h2("Demo"),
                          fluidRow(column(4,
                                          selectInput('demo_sex', 'Sex', 
                                                      choices = c('total', 'male', 'female'))),
                                   column(4,
                                          selectInput('demo_age', 'Age group', 
                                                      choices = c('15 to 19' = '15_19', 
                                                                                           '15 to 24' = '15_24', '15 and older' = '15_up', '20 to 24' = '20_24', '25 to 29' = '25_29'))),
                                   column(4,
                                          selectInput('demo_year', 'Year', 
                                                      choices = c(2001, 2006, 2011),
                                                      selected = 2011))),
                          leafletOutput('demo_leaflet'),
                          plotOutput('crazy_plot1')),
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

