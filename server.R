# server <- function(session, input, output) {
shinyServer(function(session, input, output) {
  
  # whoami()
  # getwd()
  # hive.connect()
  
  #Panel Selection############################
  
  # input_data <- reactive({
  #   
  #   if(input$promo20p == "MAPD"){
  #     dt_input <- as.data.table(
  #       read.hive("select geo1 AS geo1,geo2 AS geo2, geo3 AS geo3, year as year
  #     from
  #     (select geo1,geo2,geo3,year from dev_ca_medicare_enc.consolidated_mapd_data_zs where year=2021 union all
  #     select geo1,geo2,geo3,year from dev_ca_medicare_enc.consolidated_mapd_data_zs where year=2020)temp
  #     group by geo1, geo2, geo3,year")
  #     )
  #   }
  #   
  #   if(input$promo20p == "DSNP"){
  #     dt_input <- as.data.table(
  #     read.hive("select geo1 AS geo1,geo2 AS geo2, geo3 AS geo3, year as year from
  #     (select geo1,geo2,geo3,year from dev_ca_medicare_enc.consolidated_dsnp_data_zs where year=2021 union all
  #     select geo1,geo2,geo3,year from dev_ca_medicare_enc.consolidated_dsnp_data_zs where year=2020)temp
  #               group by geo1, geo2, geo3,year"))
  #   }
  #   
  #   if(input$promo20p == "PDP"){
  #     dt_input <- as.data.table(
  #     read.hive("select geo1 AS geo1,geo2 AS geo2, geo3 AS geo3, year as year from
  #     (select geo1,geo2,geo3,year from dev_ca_medicare_enc.consolidated_pdp_data_zs where year=2021 union all
  #     select geo1,geo2,geo3,year from dev_ca_medicare_enc.consolidated_pdp_data_zs where year=2020)temp
  #               group by geo1, geo2, geo3,year"))
  #   }
  #   dt_input
  #   
  # })
  
  # input_data_sec <- reactive({
  #   
  #   if(input$promo20p == "MAPD"){
  #     dt_input <- as.data.table(
  #     read.hive("select channel as channel, channel_type as channel_type, start_date_of_week as start_date, year as year
  #     from (select start_date_of_week,channel, channel_type, year from dev_ca_medicare_enc.consolidated_mapd_data_zs
  #     where year = 2021 union all select start_date_of_week,channel, channel_type, year from dev_ca_medicare_enc.consolidated_mapd_data_zs
  #               where year = 2020)temp group by start_date_of_week, channel, channel_type, year"))
  #     dt_input$start_date <- as.Date(dt_input$start_date)
  #   }
  #   
  #   if(input$promo20p == "DSNP"){
  #     dt_input <- as.data.table(
  #     read.hive("select channel as channel, channel_type as channel_type, start_date_of_week as start_date, year as year
  #     from (select start_date_of_week,channel, channel_type, year from dev_ca_medicare_enc.consolidated_dsnp_data_zs
  #     where year = 2021 union all select start_date_of_week,channel, channel_type, year from dev_ca_medicare_enc.consolidated_dsnp_data_zs
  #               where year = 2020)temp group by start_date_of_week, channel, channel_type, year"))
  #     dt_input$start_date <- as.Date(dt_input$start_date)
  #   }
  #   
  #   if(input$promo20p == "PDP"){
  #     dt_input <- as.data.table(
  #     read.hive("select channel as channel, channel_type as channel_type, start_date_of_week as start_date, year as year
  #     from (select start_date_of_week,channel, channel_type, year from dev_ca_medicare_enc.consolidated_pdp_data_zs
  #     where year = 2021 union all select start_date_of_week,channel, channel_type, year from dev_ca_medicare_enc.consolidated_pdp_data_zs
  #               where year = 2020)temp group by start_date_of_week, channel, channel_type, year"))
  #     dt_input$start_date <- as.Date(dt_input$start_date)
  #   }
  #   dt_input
  # })
  
  
  output$product_overview <- renderUI({
    selectInput("promo20p", "Product", choices = c("MAPD","PDP","DSNP"), selected = "MAPD")
  })
  
  output$market_overview <- renderUI({
    # selectInput("promo20", "Market", choices = as.character(sort(unique(input_data()$geo1[input_data()$year == input$promo20y]))), selected = NULL, multiple = TRUE)
    selectInput("test02", "test input 2", choices = c("a","b","c"), selected = "a")
  })
  
  output$submarket_overview <- renderUI({
    # selectInput("promo20s", "Sub-Market", choices = as.character(sort(unique(input_data()$geo2[input_data()$geo1 == input$promo20]))), selected = NULL, multiple = TRUE)
    selectInput("test03", "test input 3", choices = c("a","b","c"), selected = "a")
  })
  
  output$county_overview <- renderUI({   
    # selectInput("promo20c", "County", choices = as.character(sort(unique(input_data()$geo3[input_data()$geo2 == input$promo20s & input_data()$geo1 == input$promo20]))), selected = NULL, multiple = TRUE)
    selectInput("test04", "test input 4", choices = c("a","b","c"), selected = "a")
  })
  
  output$channeltype_overview <- renderUI({
    # if(input$tabselected == 12){
    #   NULL
    # }else{
    #   selectInput("promo20off", "Offline/Online", choices = unique(input_data_sec()$channel_type), selected = NULL, multiple = TRUE)
    # }
    selectInput("test05", "test input 5", choices = c("a","b","c"), selected = "a")
  })
  
  output$channel_overview <- renderUI({
    # if(input$tabselected == 12){
    #   selectInput("promo20ch", "Channel", choices = c("",unique(input_data_sec()$channel[input_data_sec()$year == input$promo20y])), selected = NULL, multiple = FALSE)
    # }else{
    #   selectInput("promo20ch", "Channel", choices = unique(input_data_sec()$channel[input_data_sec()$year == input$promo20y]), selected = NULL, multiple = TRUE)
    # }
    selectInput("test06", "test input 6", choices = c("a","b","c"), selected = "a")
  })
  
  output$year_overview <- renderUI({
    selectInput("promo20y", "Year", choices = c(2020,2021), selected = 2021)
  })
  
  output$dates_overview <- renderUI({
    # sliderInput("promo20d", "Timeframe", min = min(input_data_sec()$start_date[input_data_sec()$year == 2021]), max = max(input_data_sec()$start_date[input_data_sec()$year == 2021]),
    #             value = range(input_data_sec()$start_date[input_data_sec()$year == 2021]), step = 7, width = "1000px", dragRange = TRUE)
    sliderInput("test07", "test input 7", min = 0, max = 100, value = 50)
  })
  
  output$submit_overview <- renderUI({
    actionButton("actButOverview","Submit")
  })
  
  
  
  
  # output$funnel <- function() {
  #   shinyjs::runjs(paste0("console.log('funnelhere');
  #                           setInterval(function(){
  #                             console.log('funnelfunction');
  #                             var table = document.getElementById('funnel');
  #                             var finaltable = table.getElementsByTagName('table')[0];
  #                             var tbody = finaltable.getElementsByTagName('tbody')[0];
  #                             var rows = tbody.getElementsByTagName('tr');
  #                             for (var i=0, len=rows.length; i<len; i++){
  #                               var cell = rows[i].getElementsByTagName('td');
  #                               var cellContent = cell[4].innerText;
  #                               console.log(cellContent);
  #                               var cellfinal = cellContent.split('%')[0]
  # 
  #                               if(cellfinal > 0){
  #                                 cell[4].style.color = 'green';
  #                               } else {
  #                                 cell[4].style.color = 'red';
  #                               }
  #                             }
  #                           }, 3000)"))
  #   
  #   data_funnel_kable() %>%
  #     kable("html", align = c('l', 'c', 'c', 'c', 'c'), escape = F, col.names = NULL) %>%
  #     kable_styling("striped", full_width = TRUE, font_size = 18, position = "center", stripe_color = "gray!6") %>%
  #     row_spec(c(1,3), background = "lavenderblush", color = "black", extra_css = "vertical-align:middle;") %>%
  #     row_spec(c(2,4), background = "white", color = "black", extra_css = "vertical-align:middle;") %>%
  #     column_spec(1,  image = spec_image(c("awareness_snip.PNG", "Consideration_snip.PNG", "Lead Pic.PNG", "purchase.PNG"), 500, 300)) %>%
  #     add_header_above(c(" ", "General Statistics" = 1, "Funnel Progression" = 1, "Weekly Funnel Progression" = 1, "WoW Change" = 1), bold=TRUE, color = "purple", align = "c")
  # }
  output$funnel <- renderDataTable({
    datatable(mtcars)
  })
  
  
  output$abs_overview <- renderUI({
    selectInput("abs_overview", "", choices = c("Absolute", "Normalized"), selected = "Normalized", width="20%")
  })
  
  
  # data_fips <- reactive({
  #   setDT(dt_date())[, lapply(.SD, sum, na.rm = TRUE), .SDcols = 4:ncol(dt_date()), by = fips]
  # })
  # 
  # dt_fips <- reactive({
  #   d <- data_fips()
  # 
  #   if (input$abs_overview == "Normalized"){
  #     d[["interaction_sales"]] <- log(d[["interaction_sales"]])
  #     d <- d[!is.infinite(d[["interaction_sales"]]), ]
  #   }
  #   d
  # })
  # 
  # url <- "geojson-counties-fips.json"
  # counties <- rjson :: fromJSON(file = url)
  # 
  # g <- list(scope= "usa",
  #           projection = list(type = 'albers usa'),
  #           showlakes = TRUE,
  #           lakecolor = toRGB('white'))
  
  output$map <- renderPlotly({
    # fig <- plot_ly()
    # fig <- fig %>% add_trace(type= "choropleth", geojson = counties, locations = dt_fips()$fips,
    #                        z = dt_fips()$interaction_sales, color = dt_fips()$interaction_sales, colors = magma(50, alpha = 0.2, begin = 0, end = 0.65, direction = -1), marker= list(line=list(width=0)))
    # fig <- fig %>% layout(geo = g, title = "<b>Geographic Marketing Driven Sales Volume</b>", legend= list(title=list(text= "Sales Volume"))) %>%
    #   colorbar(title = "<b>Sales Volume</b>")
    # fig
    plot_ly(data = mtcars, x = ~mpg, y = ~disp, type = "scatter", mode = "markers", marker = list(size = 10, colorbar = list(title = "Number of cylinders"), color = ~cyl, colorscale='Viridis', reversescale =T)) %>% 
      layout(title = "Miles per gallon versus displacement", xaxis = list(title = "Miles per gallon"), 
             yaxis = list(title = "Displacement"))
  })
  
  
  
  
  
  # data_channel <- reactive({
  #   setDT(dt_date())[, lapply(.SD, sum, na.rm = TRUE), .SDcols = 4:ncol(dt_date()), by = channel]
  # })
  # 
  # dt_channel <- reactive({
  #   d1 <- data_channel()[, c("channel","web_visits_sales")]
  #   setnames(d1, "web_visits_sales", "n")
  #   d1[ , `:=` (metric = "Web Driven Sales")]
  #   d2 <- data_channel()[, c("channel","tfn_sales")]
  #   setnames(d2, "tfn_sales", "n")
  #   d2[ , `:=` (metric = "Call Driven Sales")]
  #   d_f <- rbind(d1,d2)
  #   d_f
  # })
  # 
  # output$map_digital <- renderPlotly({
  #   g <- ggplotly(
  #     ggplot(data = dt_channel(), aes(x = channel, y=n, fill=metric)) +
  #       geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)  +
  #       geom_text(aes(label = n), vjust = -0.5,
  #                 position = position_dodge(.9), size = 4) +
  #       scale_fill_manual(values = c("#800080", "#D8BFD8"))+
  #       theme(panel.background = element_rect(fill = 'white', color = 'white'),
  #             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #             axis.text.x = element_text(angle = 75, hjust=1),
  #             plot.title = element_text(hjust = 0.5, size = 14))+
  #       ggtitle("Digital/Telephonic Sales Volume")+ labs(y="Sales")+
  #       theme(plot.title = element_text(hjust = 0.5, size = 22),
  #             axis.title.x = element_blank(),
  #             axis.title.y = element_text(size = 15),
  #             legend.title = element_blank(),
  #             legend.text = element_text(size = 15),
  #             legend.position = "bottom",
  #             axis.ticks.x = element_blank(),
  #             axis.text = element_text(size = 15),
  #             axis.ticks.y = element_blank())
  #   )
  #   g
  #   }, height = 450)
  
  
  output$map_digital <- renderPlotly({
    plot_ly(data = mtcars, x = ~mpg, y = ~disp, type = "scatter", mode = "markers", marker = list(size = 10, colorbar = list(title = "Number of cylinders"), color = ~cyl, colorscale='Viridis', reversescale =T)) %>% 
      layout(title = "Miles per gallon versus displacement", xaxis = list(title = "Miles per gallon"), 
             yaxis = list(title = "Displacement"))
  })
  
  
  output$eff_overview <- renderUI({
    selectInput("eff_overview", "X-Axis", choices = c("Volume", "Effectiveness" ,"Lead Attainment"), selected = "Volume", width="100%")
  })
  
  output$eff_overview_y <- renderUI({
    selectInput("eff_overview_y", "Y-Axis", choices = c("Volume", "Effectiveness" ,"Lead Attainment"), selected = "Effectiveness", width="50%")
  })
  
  # effective <- reactive({
  #   d1 <- data_channel()[, c("channel","Effectiveness","Lead Attainment", "interaction_sales")]
  #   setnames(d1, "interaction_sales", "Volume")
  #   d1
  # })
  # 
  # output$map_effective <- renderPlotly({
  #   eff_table <- effective()
  #   eff_table$xx <- eff_table[[input$eff_overview]]
  #   
  #   plot_ly(eff_table, x = ~xx, y = ~yy,
  #           color = ~`channel`, type = "scatter",
  #           mode="markers",
  #           marker = list(symbol = 'circle', size = 10,sizemode = 'diameter', line = list(width = 1))) %>%
  #     layout(title = "Channel Performance",
  #            xaxis = list(title =input$eff_overview),
  #            yaxis = list(title =input$eff_overview_y),
  #            annotations = list(text="Click channel names\n to select/deselect", showarrow=FALSE, xref="paper", yref = "paper", xanchor="right", yanchor = "top", x=1.1, y=0.5))
  # })
  output$map_effective <- renderPlotly({
    plot_ly(data = mtcars, x = ~mpg, y = ~disp, type = "scatter", mode = "markers", marker = list(size = 10, colorbar = list(title = "Number of cylinders"), color = ~cyl, colorscale='Viridis', reversescale =T)) %>% 
      layout(title = "Miles per gallon versus displacement", xaxis = list(title = "Miles per gallon"), 
             yaxis = list(title = "Displacement"))
  })
  
  
  # spend_data_overview_map <- reactive({
  #   d <- data_channel()[, c("channel","spend","budget")]
  #   d[ , `:=` (remaining = budget-spend)]
  #   d1 <- d[, c("channel","spend")]
  #   setnames(d1, "spend", "n")
  #   d1[ , `:=` (metric = "Spend")]
  #   d2 <- d[, c("channel","remaining")]
  #   setnames(d2, "remaining", "n")
  #   d2[ , `:=` (metric = "Remaining")]
  #   d_f <- rbind(d1,d2)
  #   d_f
  # })
  # 
  # output$map_spend <- renderPlotly({
  #   g <- ggplotly(
  #     ggplot(data = spend_data_overview_map(), aes(fill=metric, x=channel, y = (n/1000000))) +
  #       geom_bar(stat = 'identity', position = "stack")+
  #       geom_text(data = spend_data_overview_map(), 
  #                 aes(x=channel, y = (n/1000000), group = metric,
  #                     label = format(round(n/1000000), big.mark=","), vjust=-.4), size = 4)+
  #       theme(panel.background = element_rect(fill = 'white', color = 'white'),
  #             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #             axis.text.x = element_text(angle = 75, hjust=1),
  #             plot.title = element_text(hjust = 0.5, size = 14))+
  #       ggtitle("Spend Usage")+
  #       labs(y = "Spend $million")+
  #       scale_fill_manual(values = c("#DDA0DD","#800080"))+
  #       scale_y_continuous(labels = scales::comma)+
  #       theme(plot.title = element_text(size=22), legend.title=element_blank(),
  #             axis.title.x = element_blank(), legend.position = "bottom",
  #             axis.text.x = element_text(size = 15), legend.text=element_text(size=15),
  #             axis.line.y = element_blank(), axis.text.y = element_text(size = 15),
  #             axis.ticks = element_blank(), axis.title.y= element_text(size = 15))
  #   )
  #   g
  #   }, height = 450)
  
  output$map_spend <- renderPlotly({
    plot_ly(data = mtcars, x = ~mpg, y = ~disp, type = "scatter", mode = "markers", marker = list(size = 10, colorbar = list(title = "Number of cylinders"), color = ~cyl, colorscale='Viridis', reversescale =T)) %>% 
      layout(title = "Miles per gallon versus displacement", xaxis = list(title = "Miles per gallon"), 
             yaxis = list(title = "Displacement"))
  })
  
  
  
  
  output$selinpproduct <- renderUI({
    selectInput("selinpproduct","Product",choices = c("MAPD","DSNP","PDP"),selected = c("MAPD"),multiple = F)
  })
  
  output$selinpyear <- renderUI({
    selectInput("selinpyear","Year",choices = c(2021,2020),selected = 2021,multiple = F)
  })
  
  output$selinpRow <- renderUI({
    vec_choice <- c("market", "other")
    
    # selectInput("selinpRow","Granularity Level",choices = vec_choice,selected = c("market"),multiple = T)
    selectInput("selinpRow","Granularity Level",choices = vec_choice,selected = c("market"),multiple = T)
  })
  
  output$selinpValues <- renderUI({
    vec_choice_num <- c(1,2,3,4,5,6,7,8,9,10)
    
    # selectInput("selinpValues","Metrics",choices = vec_choice_num,selected = NULL,multiple = T)
    selectInput("selinpValues","Metrics",choices = vec_choice_num,selected = NULL,multiple = T)
  })
  
  output$actButOverviewPivot <- renderUI({
    actionButton("actButOverviewPivot","Submit")
  })
  
  output$dtoOverviewPivot <- renderDT({
    datatable(mtcars)
  })
  
  
  
})
