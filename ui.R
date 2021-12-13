shinyUI(
  navbarPage("Aetna",
             
             tabPanel("Executive Summary", 
                      
                      includeCSS(paste0(getwd(), "/www/styles.css")),
                      includeCSS(paste0(getwd(), "/www/AdminLTE.css")),
                      includeCSS(paste0(getwd(), "/www/shinydashboard.css")),
                      includeScript(paste0(getwd(), "/www/app.js")), # 
                      
                      div(tabsetPanel(
                        tabPanel("Overview", 
                                 fluidRow(
                                   column(2,
                                          dropdownButton(
                                            uiOutput('product_overview'), # selectInput("promo20p", "Product", choices = c("MAPD","PDP","DSNP"), selected = "MAPD"),
                                            uiOutput('market_overview'),
                                            uiOutput('submarket_overview'),
                                            uiOutput('county_overview'),
                                            uiOutput('channeltype_overview'),
                                            uiOutput('channel_overview'),
                                            uiOutput('year_overview'), # selectInput("promo20y", "Year", choices = c(2020,2021), selected = 2021),
                                            uiOutput('dates_overview'),
                                            uiOutput('submit_overview'), # actionButton("actButOverview","Submit")
                                            
                                            circle = FALSE,
                                            status = "default",
                                            size = "default",
                                            icon = icon("filter"),
                                            label = "Filter",
                                            tooltip = FALSE,
                                            width = NULL,
                                            margin = "10px",
                                            inline = FALSE,
                                            inputId = NULL
                                          )
                                   )
                                 ),
                                 br(),
                                 fluidRow(
                                   # column(2,
                                   #        wellPanel(
                                   #          uiOutput('product_overview'), # selectInput("promo20p", "Product", choices = c("MAPD","PDP","DSNP"), selected = "MAPD"),
                                   #          uiOutput('market_overview'),
                                   #          uiOutput('submarket_overview'),
                                   #          uiOutput('county_overview'),
                                   #          uiOutput('channeltype_overview'),
                                   #          uiOutput('channel_overview'),
                                   #          uiOutput('year_overview'), # selectInput("promo20y", "Year", choices = c(2020,2021), selected = 2021),
                                   #          uiOutput('dates_overview'),
                                   #          uiOutput('submit_overview'), # actionButton("actButOverview","Submit")
                                   #        )
                                   #  ),
                                   column(12,
                                          fluidRow(
                                            # tableOutput('funnel') # Funnel data table
                                            column(12, 
                                                   shinydashboard::box(
                                                     dataTableOutput('funnel'),
                                                     solidHeader = T, collapsible = T, title = "Data Table", status = "primary", width = 12)
                                                   # dataTableOutput('funnel')
                                            ) # Funnel data table
                                            
                                          ),
                                          # hr(),
                                          br(),
                                          fluidRow(
                                            column(6,
                                                   shinydashboard::box(
                                                     uiOutput("abs_overview"), 
                                                     br(),
                                                     plotlyOutput("map"),
                                                     h4("Note: Based on absolute/normalized volume, at market, sub-market and county level", style = 'font-size:12px;color:#757575;'),
                                                     solidHeader = T, collapsible = T, title = "Sample Plot 1", status = "primary", width = 12)
                                                   # uiOutput("abs_overview"), # selectInput("abs_overview", "", choices = c("Absolute", "Normalized"), selected = "Normalized", width="20%"),
                                                   # br(),
                                                   # plotlyOutput("map"),
                                                   # h4("Note: Based on absolute/normalized volume, at market, sub-market and county level", style = 'font-size:12px;color:#757575;')
                                            ),
                                            column(6,
                                                   shinydashboard::box(
                                                     br(), br(), 
                                                     plotlyOutput("map_digital"),
                                                     h4("Note: Only Purchases through website or calls", style = 'font-size:12px;color:#757575;'),
                                                     br(), br(),
                                                     solidHeader = T, collapsible = T, title = "Sample Plot 2", status = "primary", width = 12)
                                                   # # div(plotOutput("map_digital"), style ="height:450px;"), 
                                                   # br(), br(), br(), br(),
                                                   # plotlyOutput("map_digital"),
                                                   # h4("Note: Only Purchases through website or calls", style = 'font-size:12px;color:#757575;')
                                            )
                                          ),
                                          # hr(),
                                          br(),
                                          fluidRow(
                                            column(6,
                                                   shinydashboard::box(
                                                     fluidRow(
                                                       column(4, uiOutput("eff_overview")),
                                                       column(8, uiOutput("eff_overview_y")),
                                                       br(),
                                                       column(12,
                                                              # div(plotlyOutput("map_effective"),style ="height:400px;"), 
                                                              br(),
                                                              plotlyOutput("map_effective"),
                                                              h4("Note: Effectiveness is defined as Volume/Spend", style = 'font-size:12px;color:#757575;')
                                                       )
                                                     ),
                                                     solidHeader = T, collapsible = T, title = "Sample Plot 3", status = "primary", width = 12)
                                                   
                                                   # fluidRow(
                                                   #   column(4, uiOutput("eff_overview")),
                                                   #   column(8, uiOutput("eff_overview_y")),
                                                   #   br(),
                                                   #   column(12,
                                                   #          # div(plotlyOutput("map_effective"),style ="height:400px;"), 
                                                   #          br(),
                                                   #          plotlyOutput("map_effective"),
                                                   #          h4("Note: Effectiveness is defined as Volume/Spend", style = 'font-size:12px;color:#757575;')
                                                   #   )
                                                   # )
                                            ),
                                            column(6,
                                                   # div(plotOutput("map_spend"), style ="height:450px;"), 
                                                   shinydashboard::box(
                                                     br(), br(),
                                                     plotlyOutput("map_spend"),
                                                     h4("Note: Spend is total spend so far. Remaining is budget less spend so far", style = 'font-size:12px;color:#757575;'),
                                                     br(), br(),
                                                     solidHeader = T, collapsible = T, title = "Sample Plot 4", status = "primary", width = 12)
                                                   # br(), br(), br(), br(),
                                                   # plotlyOutput("map_spend"),
                                                   # h4("Note: Spend is total spend so far. Remaining is budget less spend so far", style = 'font-size:12px;color:#757575;')
                                            )
                                          ),
                                          br(),
                                          fluidRow(
                                            column(12,
                                                   shinydashboard::box(
                                                     fluidRow(
                                                       column(2, uiOutput("selinpproduct")),
                                                       column(2, uiOutput("selinpyear")),
                                                       column(2, uiOutput("selinpRow")),
                                                       column(2, uiOutput("selinpValues")),
                                                       column(2, uiOutput("actButOverviewPivot")),
                                                       br()
                                                     ),
                                                     fluidRow(
                                                       column(12, DTOutput("dtoOverviewPivot"))
                                                     ),
                                                     solidHeader = T, collapsible = T, title = "Sample Plot 3", status = "primary", width = 12)
                                            )
                                          )
                                   )
                                 ),
                                 
                                 hr(),
                                 fluidRow(),
                                 br(), br(), br(), br(), br(), br(), br(),
                                 br(), br(), br(), br(), br(), br(), br(),
                                 br(), br(), br(), br(), br(), br(), br(),
                                 br(), br(), br(), br(), br(), br(), br(),
                                 hr(),
                                 br(), br(), br(), br(), br(), br(), br(),
                                 br(), br(), br(), br(), br(), br(), br(),
                                 br(), br(), br(), br(), br(), br(), br(),
                                 hr(),
                                 br(), br(), br(), br(), br(), br(), br(),
                                 br(), br(), br(), br(), br(), br(), br(),
                                 hr(),
                                 br(), br(), br(), br(), br(), br(), br(),
                                 br(), br(), br(), br(), br(), br(), br()
                        ),
                        tabPanel("Weekly Change"
                        ), 
                        tabPanel("Historical"
                        ), 
                        tabPanel("Goal"
                        ),
                      ), class = "planning-tabs")
             ),
             
             
             tabPanel("Channel Summary", 
                      includeCSS(paste0(getwd(), "/www/styles.css"))
             )
  )
)

