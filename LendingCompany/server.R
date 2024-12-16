
server <- function(input, output, session) { 
  
  output$box1 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box1", width = NULL, height = 450,    #CAMBIO
              tabPanel(title = "All Acounts Balances ($)", value = "T1",
                       div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                       div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                       withSpinner(plotlyOutput("balances_plot", height = 350)) ), 
              tabPanel( title = "Interest Rate", value = "T2",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("interest_rates_plot", height = 350))),
              tabPanel( title = "Princi
               + Interests", value = "T3",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("princ_rates", height = 350)))
              
      )
    )
  }
  )    
  
  
  output$box2 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box2", width = NULL, height = 400,
              tabPanel( title = "Borrower's Financial Status", value = "T4",
                        div( style = "position:absolute;left:0.5em;bottom: 0.5em;", ),
                        withSpinner(plotlyOutput("public_plot",  height = 350)) )  
      )
    )
  }
  )    
  
  output$box3 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box3", width = NULL, height = 400,
              tabPanel( title = "Credit Destination by Ownership", value = "T5",
                        div( style = "position:absolute;left:0.5em;bottom: 0.5em;", ),
                        withSpinner(plotlyOutput("credits", height = 350)) 
              )  
      )
    )
  }
  )    
  
  output$box4 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box4", width = NULL, height = 400,
              tabPanel( title = "Account Status ($)", value = "T6",
                        div( style = "position:absolute;left:0.5em;bottom: 0.5em;", ),
                        withSpinner(plotlyOutput("loan_status_plot", height = 350)) 
              )  
      )
    )
  }
  )   
  
  output$box5 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box5", width = NULL, height = 450,
              tabPanel( title = "Loan Amount vs Interest Rate", value = "T7",
                        div( style = "position:absolute;left:0.5em;bottom: 0.5em;", ),
                        withSpinner(plotlyOutput("loans_rate", height = 350)) ),
              tabPanel( title = "Loan Grade vs Interest Rate", value = "T9",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("grades_rate", height = 350)))
      )
    )
  }
  )   
  
  
  output$box6 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box6", width = NULL, height = 400,
              tabPanel( title = "Loan's Grade Status", value = "T5",
                        div( style = "position:absolute;left:0.5em;bottom: 0.5em;", ),
                        withSpinner(plotlyOutput("public", height = 350)) 
              )  
      )
    )
  }
  )   
  
  
  output$box7 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box7", width = NULL, height = 350,
              tabPanel( title = "Training set", value = "T11",
                        div( style = "position:absolute;left:0.5em;bottom: 0.5em;", ),
                        withSpinner(plotlyOutput("roc_t", height = 300)) ),
              tabPanel( title = "Test set", value = "T12",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("roc_te", height = 300)))
      )
    )
  }
  )  
  
  
  output$box8 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box8", width = NULL, height = 350,
              tabPanel( title = "Training set", value = "T13",
                        div( style = "position:absolute;left:0.5em;bottom: 0.5em;", ),
                        withSpinner(plotlyOutput("pra_t", height = 300)) ),
              tabPanel( title = "Test set", value = "T14",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("pra_te", height = 300)))
      )
    )
  }
  )
  

  output$box88 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box88.1", width = NULL, height = 350,
              tabPanel(
                title = "Major Contribution Features (Top 10)",
                withSpinner(
                  plotlyOutput("ice", height = 300)  ,
                  type = 5,
            
                  size = 0.5
                )
              )
              
      )
    )
  }
  ) 
  
  
  output$box99 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box99.0", width = NULL, height = 350,
              tabPanel(
                title = "Variance Explanation",
                div(
                  style = "position: absolute; left: 0.5em; bottom: -2.5em;  z-index: 100;background-color: #ECF0F5;",
                  dropdown(
                    radioGroupButtons(
                      inputId = "box99.1",
                      label = "Feature:", 
                      width = "300px",
                      choiceNames = num_vars,
                      choiceValues = num_vars, 
                      selected = "int_rate", 
                      direction = "horizontal",
                      size = "xs", checkIcon = list("yes" = icon("check")),
                      individual = TRUE
                    ),
                    size = "xs",animate = FALSE,
                    label = "Feature",
                
                    up = TRUE
                  )
                ),
                
                withSpinner(
                  plotOutput("feature_variance", height = 300)  ,
                  type = 5,
             
                  size = 0.5
                )
              ) ,
              
              tabPanel(
                title = "Individual & Average Dependency",
                div(
                  style = "position: absolute; left: 0.5em; bottom: -2.5em;  z-index: 100;background-color: #ECF0F5;",
                  dropdown(
                    radioGroupButtons(
                      inputId = "box99.2",
                      label = "Feature:", 
                      width = "300px",
                      choiceNames = num_vars,
                      choiceValues = num_vars, 
                      selected = "int_rate", 
                      direction = "horizontal",
                      size = "xs", checkIcon = list("yes" = icon("check")),
                      individual = TRUE
                    ),
                    size = "xs",animate = FALSE,
                    label = "Feature",
                   
                  )
                ),
                
                withSpinner(
                  plotOutput ("feature_ice", height = 300),  
                  type = 5,
                  
                  size = 0.5
                )
              )
              

              
      )
    )
  }
  ) 
  
  
  

  
  output$box9 <- renderUI({
    div(
      style = "position: relative; background-color: #ffffff ;",
      tabBox(
        id = "box9",
        width = NULL,
        height = 340,
        tabPanel(
          title = "Overall Model Metrics",
         
          withSpinner(
            DT::dataTableOutput("table_metrics", height = "300px"),
            type = 4,
            color = "#d33724",
            size = 0.1
          )
        )
        
        
      )
    )
    
  }
  )  
  
  
  output$box10 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box10", width = NULL, height = 350,
              tabPanel( title = "Feature Importance", value = "T15",
                        div( style = "position:relative;left:0.5em;bottom: 0.5em; ", ),
                        withSpinner(plotlyOutput("feature_imp", height = 300)) 
              )  
      )
    )
  }
  )  
  
  
  output$box11 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box11", width = NULL, height = 350,
              tabPanel( title = "Partial Dependency Plots", value = "T151",
                        div( style = "position: absolute; left: 0.5em; bottom: -2.5em;  z-index: 100;",
                             dropdown(
                               radioGroupButtons (
                                 inputId = "box111",
                                 label = "Choose feature", 
                                 choiceNames = all_vars, 
                                 choiceValues = all_vars,
                                 selected = "last_pymnt_amnt",
                                 direction = "vertical", size = "xs"
                               ),
                               size = "xs", animate = TRUE,
                               label = "Feature",
                           
                               up = TRUE
                             )
                             
                        ),
                        withSpinner(plotlyOutput("partial_dep", height = 300),
                                    type = 4,
                                    color = "#d33724", 
                                    size = 0.7
                                    
                        ) 
              )  
      )
    )
  }
  )  
  
  
  
  
  
  
  ## render plotlys
  
  output$interest_rates_plot <- renderPlotly(
    { ggplotly(t1, tooltip = NULL) %>% layout(legend = list(orientation = "h", x = 0.35, y = 1, font = list(size = 8))) %>%  partial_bundle()  } 
  )
  
  output$balances_plot <- renderPlotly(
    { ggplotly(t2, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.55, y = 1.1, font = list(size = 8)))  } 
  )
  

public_plot_r <- reactive ({
  
  
  temp3$issue_d_y <- as.character(temp3$issue_d_y)
 
  temp3 <- temp3 [issue_d_y == input$year_1 & term == input$term_1 & application_type == input$app_1 & issue_d_y >= 2015]  
  
  temp3 <- select(temp3, pub_rec, pub_rec_bankruptcies)

  
  
  temp3$ID <- seq.int(nrow(temp3))
  temp3 <- temp3 %>%  reshape2::melt(id.vars = "ID", variable.name = "feature", value.name = "Total") %>% select(-ID) %>% group_by(feature) %>% summarize(Total = sum(Total), .groups = "drop")
  temp3$feature <- as.factor(temp3$feature)
  levels(temp3$feature) <- c("Pub. Records", "Bankruptcies")  
  
  temp3 %>% ungroup()
  
})  
    
  
  output$public_plot <- renderPlotly(
    { 
      
      
      
      plot_ly(public_plot_r(), x = ~reorder(feature,-1*Total), y = ~Total, color = ~feature, type = 'bar'  , textposition = 'inside', insidetextanchor = "middle",
              colors = c("#AF1E2D","#E8112D" ), texttemplate="%{value:,f}", hoverinfo = 'text', insidetextfont = list(color = '#FFFFFF', size = 10), 
              text = ~paste0("Total Count: ", Total), textfont = list(color = 'rgb(0,0,0)')) %>%
        layout(yaxis = list(title = FALSE, showticklabels = FALSE, showgrid = FALSE), xaxis = list(showgrid = FALSE, title = FALSE, showline = FALSE, font = list(size = 8)), 
               legend = list(orientation = 'h', title = FALSE, x = 0.02, y = 1) ) %>%  partial_bundle()
    } 
  )
  
public_r <- reactive({
  
  
  
  temp8$issue_d_y <- as.character(temp8$issue_d_y)
  

    temp8 <- temp8[issue_d_y == input$year_1 & term == input$term_1 & application_type == input$app_1] 

  temp8 <- temp8 %>% select(grade, pub_rec_bankruptcies, tax_liens, pub_rec) %>% group_by(grade) %>%
    
    summarize(`Bankruptcies` = sum(pub_rec_bankruptcies), `Public Records` = sum(pub_rec), `Tax Liens` = sum(tax_liens), .groups = "drop" ) %>% ungroup() %>% as.data.frame() %>%
    reshape::melt(id.vars = "grade", variable.name = "feature", value.name = "value")
  temp8$variable <- as.factor(temp8$variable)
  temp8 %>% ungroup()
  
})
  
    
  output$public <- renderPlotly(
    { 
      plot_ly(public_r(), x = ~grade, y = ~ value, color = ~variable, type = 'bar'  , textposition = FALSE, insidetextanchor = FALSE,
              colors = c("#AF1E2D","#E8112D", "#FC5E72" ), texttemplate="%{value:,f}", hoverinfo = 'text', insidetextfont = list(color = '#FFFFFF', size = 10), 
              text = ~paste0("Total value: ", value), textfont = list(color = 'rgb(0,0,0)')) %>%
        layout(yaxis = list(title = FALSE, showticklabels = TRUE, showgrid = TRUE), xaxis = list(showgrid = FALSE, title = FALSE, showline = FALSE), 
               legend = list(orientation = 'h', title = FALSE, x = 0.02, y = 1) ) %>%  partial_bundle() 
    } 
  )
  
  
  
  
  output$princ_rates <- renderPlotly(
    { ggplotly(t4, tooltip = "text") %>% layout(legend = list(orientation = "v", x = 0.06, y = 1)) %>%  partial_bundle() }
  )
  

credits_r <- reactive({
  

  temp$issue_d_y <- as.character(temp$issue_d_y)

  temp <- temp [issue_d_y == input$year_1 & term == input$term_1 & application_type == input$app_1 & home_ownership %in% c("RENT", "OWN", "MORTGAGE", "ANY") ]  
  
  tempc1 <- temp 
  rows_t <- nrow(temp)
  tempc1 <- tempc1 %>%  select(title) %>% group_by(title) %>% summarize(count = n(), .groups = "drop") %>% mutate(prop = 100 * count /rows_t) %>% arrange(desc(prop)) %>% top_n(n = 2) %>% ungroup()
  topc1 <- tempc1$title
  temp$title <- ifelse(temp$title %ni% topc1, "Other", temp$title)
  tempc1 <- temp  %>% select(title, home_ownership, grade) %>% group_by(title, home_ownership, grade) %>% summarize(count = n(), .groups = "drop_last") 
  tempc1$title <- as.factor(tempc1$title)
  tempc1$grade <- as.factor(tempc1$grade)
  
  tempc1$home_ownership <- as.factor(tempc1$home_ownership)
 
  value <- tempc1  %>% select(title,home_ownership,count) %>% summarize(count = sum(count),  .groups = "drop") 
  value$home_ownership <- as.factor(value$home_ownership)
  value$title <- as.factor(value$title)
  levels(value$title) <- c("CC Refinan.", "Consolid.", "Other")  
  value$rows_t <- rows_t
  value %>% ungroup()
  
 })  


  output$credits <- renderPlotly(
    { 

      if (nrow(credits_r()  ) != 0 ) {  
         
         
         
         plot_ly(credits_r() , x = ~title, y = ~count/rows_t , color = ~home_ownership,type = 'bar', textposition = 'inside', insidetextanchor = "middle",
              colors = c("#C9E8DD","#004438", "#00AF99","#008270"),
              texttemplate="%{value:.1%f,s}", hoverinfo = 'text',
              insidetextfont = list(color = '#FFFFFF', size = 10), text = ~ round(100 * credits_r()$count/rows_t,1) , textfont = list(color = 'rgb(0,0,0)') ) %>%
             layout(yaxis = list(title = FALSE, showticklabels = FALSE, showgrid = FALSE), xaxis = list(showgrid = FALSE, title = FALSE, showline = FALSE),
             legend = list(orientation = 'h', title = FALSE, xanchor  = "top", x = 0.02, y = 1) ,barmode = "stack") %>%  partial_bundle()
      
        }  else { 
        
      empty_df <-  ggplot() +
          theme_void() +
          geom_text(aes(0,0,label='N/A')) +
          xlab(NULL) 
        
        ggplotly(empty_df) %>%  partial_bundle()
        
        }
        
        } 
  )
  

  

loans_rate_r <- reactive ({
  
  
  temp6$issue_d_y <- as.character(temp6$issue_d_y)
  

  temp6 <- temp6 [issue_d_y == input$year_1 & term == input$term_1 & application_type == input$app_1]  
  
  temp6 <- select(temp6, loan_amnt, int_rate)
  
   b <- c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000 )
  temp6$bin <- cut(temp6$loan_amnt, breaks = b, dig.lab = 10)
  temp6 <- temp6 %>% select (bin, int_rate) %>% group_by(bin) %>% summarize(count = n(), int_rate = mean(int_rate), prop = round(100 * count/nrow(temp6),1), .groups = "drop")
  
  temp6 <- ungroup(temp6)
  
  
  t6 <- ggplot(temp6) + geom_bar(aes(x = bin, y = prop, text = paste0("Loan Amount bin ($): ", bin )), stat='identity', fill= "#009CDE"  )  + 
    labs(title="") + ylab("Proportion(%)") + xlab("Loan Amount $") +
    geom_line(aes(x=bin, y=int_rate, text = paste0("Average Interest Rate (%): ", round(int_rate,1) ) ) , size = 1, color="#CA3604", group = 1) +   
    scale_y_continuous(sec.axis = sec_axis(~., name = "Average Interest rates (%)", breaks = seq(0,30,5))) +
    theme_bw() + theme(plot.title = element_text(hjust=0.5)) + theme(legend.title=element_blank()) +
    geom_text(aes(label=round(prop,1), x=bin, y=0.5 * prop), colour="white") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))  + 
    scale_x_discrete(breaks=c("(0,5000]","(15000,20000]","(35000,40000]")) +
    theme_text_border
  
  t6 
})  
  
    
  output$loans_rate <- renderPlotly(
    { 
      ggplotly(loans_rate_r(), tooltip = "text" ) %>%  partial_bundle()
    } 
  )

  
grades_rate_r <- reactive ({
  
 
  temp7$issue_d_y <- as.character(temp7$issue_d_y)
  
 
  
  temp7 <- temp7 [issue_d_y == input$year_1 & term == input$term_1 & application_type == input$app_1]  
  
  temp7 <- temp7 %>% select(grade, int_rate)  %>% group_by(grade) %>% summarize(int_rate = mean(int_rate), count = n() ) %>% ungroup() %>%  as.data.frame()
  
  
  temp7$prop <- 100 * temp7$count /sum(temp7$count) 
  
 
  
  t7 <- ggplot(temp7) + geom_bar(aes(x = grade, y = prop, text = paste0("Loan grade: ", grade, "</br></br>Proportion(%): ", round(prop,1) )), stat='identity', fill="#009CDE")  + 
    labs(title="") + ylab("Proportion(%)") + xlab("Grade") +
    geom_line(aes(x=grade, y=int_rate, text = paste0("Average Interest Rate (%): ", round(int_rate,1) ) ) , size = 1, color="#CA3604", group = 1) +   
    scale_y_continuous( sec.axis = sec_axis(~., name = "Average Interest rate (%)", breaks = seq(0,30,5))) +
    theme_bw() + theme(plot.title = element_text(hjust=0.5)) + theme(legend.title=element_blank()) +
    geom_text(aes(label=round(prop,1), x=grade, y=0.5 * prop), colour="white") +
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme_text_border
  
  t7  
})
    
    
  output$grades_rate <- renderPlotly(
    { 
      ggplotly(grades_rate_r(), tooltip = "text") %>%  partial_bundle()
    } 
  )
  
  
  
  
  output$loan_status_plot <- renderPlotly(
    { ggplotly(t10) %>%  partial_bundle() }
  )
  
  
  #roc curves
  
  output$roc_t <- renderPlotly(
    { plot_ly(roc_t) %>%  add_trace(x = ~fpr, y = ~tpr, line = list(color = "#00249C"), type = "scatter", mode = "lines", 
                                    showlegend = FALSE) %>% add_trace(x = ~fpr, y = ~fpr , name = 'trace 2', type = "scatter", mode = "lines", 
                                                                      line = list(color = '#221C35', dash="dot"), showlegend = FALSE) %>% layout(annotations = a_t) %>% 
        layout(yaxis = ay) %>% layout(xaxis = ax) %>% layout(title = "Receiver Operating Characteristics (ROC)", titlefont=list(size=12, color = '#000000') ) }
  )
  
  
  output$roc_te <- renderPlotly(
    { plot_ly(roc_te) %>%  add_trace(x = ~fpr, y = ~tpr, line = list(color = "#00249C"), type = "scatter", mode = "lines", 
                                     showlegend = FALSE) %>% add_trace(x = ~fpr, y = ~fpr , name = 'trace 2', type = "scatter", mode = "lines", 
                                                                       line = list(color = '#221C35', dash="dot"), showlegend = FALSE) %>% layout(annotations = a_te) %>% 
        layout(yaxis = ay) %>% layout(xaxis = ax) %>% layout(title = "Receiver Operating Characteristics (ROC)", titlefont=list(size=12, color = '#000000') ) }
  )
  
  #precision, recall, accuracy curves
  
  output$pra_t <- renderPlotly(
    { ggplotly(p_r_a_t_plot, tooltip = c("text")) %>% layout(yaxis = by) %>% layout(xaxis = bx) %>% 
        layout(legend = list(orientation = "h", x = 0.20, y = 0.5, font = list(size = 8))) %>% layout(title = "Precision + Recall + Accuracy", titlefont=list(size=12) )
    }
  )
  
  
  output$pra_te <- renderPlotly(
    { ggplotly(p_r_a_te_plot, tooltip = c("text")) %>% layout(yaxis = by) %>% layout(xaxis = bx) %>% 
        layout(legend = list(orientation = "h", x = 0.20, y = 0.5, font = list(size = 8))) %>% layout(title = "Precision + Recall + Accuracy", titlefont=list(size=12) )
    }
  )
  
  observeEvent(input$predict, {
    cat("Showing")
  })
  
  
  new_prediction <- eventReactive(input$predict, {
    
    if ( is.numeric(input$ivalue_true)) { 
      
      data_org[input$ivalue_true,]
      
    } else {paste("not valid")}
    
  })
  
  
  #### delete
  
  
  plot_ice<- function()
  { 

    new_pred <- new_prediction() 
    shp.explain = Shapley$new(predictor.rf, x.interest = new_pred[,x], sample.size = 150) ## just one time
    predicted <- shp.explain$y.hat.interest$p0
    shp_df_p0 <- shp.explain$results %>% filter(class == "p0") %>%  mutate(phi = round(phi * 100,1)) %>% 
      arrange(-desc(abs(phi.var))) %>%  top_n(10, wt = abs(phi.var)) 
    colfunc <- colorRampPalette(c("#6CACE4", "#002D72"))
    pal <- colfunc(10)
    shp_df_p0
    
    
  }
  
  
  output$ice <- renderPlotly(
    { 
    
    colfunc <- colorRampPalette(c("#6CACE4", "#002D72"))
    pal <- colfunc(10)
    
    plot_ly(plot_ice(), x = ~phi, y = ~feature.value, type = 'bar', orientation = 'h', 
              marker = list(color = ~pal)) %>%
        layout(yaxis = list(categoryorder = "array", categoryarray = ~abs(phi.var))) %>% layout(yaxis = fy) %>% layout(xaxis = fx) 
      
    }
  )
  
  ####
  
  output$feature_variance <- renderPlot(
    { 
      
      
      
      plot_sd <- ggplot(data = data_sd_deriv%>% filter(feature == input$box99.1), aes(x = value, y = sd)) +
        geom_line(colour="#00249C") + 
        labs(title = "In which points the current feature interacts with all other features?") +
        theme_bw() + xlab(paste("Feature: ", input$box99.1)) + ylab("Explanation variability") + 
        theme(legend.position = "bottom", 
              plot.title = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
      plot_sd
      
    }
  )
  
  
  output$feature_ice <- renderPlot(
    { 
      
      
      plot_dice <- ggplot(data = data_c_ice %>% filter(feature == input$box99.2), aes(x = value, y = y_var)) +
        geom_path(aes(group = obs_id ),
                  alpha = 0.5, col = "#00249C") +
        geom_point(data=data_c_observations%>% filter(feature == input$box99.2), aes(x= value, y=y),
                   colour = "black", pch = 21, fill = "gray20") +
        geom_line(data = data_d_pdp%>% filter(feature == input$box99.2), aes(x = value, y = y),
                  colour = "#D6001C", size = 1.2) +
        labs(title = "Individual Conditional Explanation (blue) + Average Explanation (red)") +
        theme_bw() + xlab(paste("Feature: ", input$box99.2)) + ylab("Explanation variability") + 
        theme(legend.position = "bottom", 
              plot.title = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
      
      plot_dice
      #ggplotly(plot_dice)
      
    }
  )
  
  output$table_metrics <- DT::renderDT({
    DT::datatable(metrics_total, height = "500px" , caption = '    Threshold selected: 54.9%', options = list(lengthMenu = c(5, 20, 15, 20), 
                                                                                                              pageLength = 5, style = "bootstrap", 
                                                                                                              dom = 'rpt') ) 
  })
  
  
  output$feature_imp <- renderPlotly(
    { plot_ly(vimp_df, x = ~scaled_importance, y = ~variable, type = 'bar', orientation = 'h', marker = list(color = ~pal_)) %>%
        layout(yaxis = list(categoryorder = "array", categoryarray = ~scaled_importance))  %>% layout(yaxis = cy) %>% layout(xaxis = cx)
      
    }
  )
  
  
  output$partial_dep <- renderPlotly(
    { 
      
      
      if (input$box111 %in%  num_vars  ) {
        
        dy <- list( title = "Average Prediction" , tickfont = list(size = 8, color = "black"),
                    titlefont = list(size = 10)); dx <- list( title = input$box111 , 
                                                              tickfont = list(size = 8, color = "black"), titlefont = list(size = 10))
                    
                    partial_num <- plot_ly(var_effects_num %>% filter(`_vname_` == input$box111 ) ) %>% 
                      add_trace(x = ~`_x_`, y = ~`_yhat_`, 
                                line = list(color = "#9A3324"), type = "scatter", mode = "lines", 
                                showlegend = FALSE) %>% layout(yaxis = dy) %>% layout(xaxis = dx)
                    partial_num
                    
      } else {
        
        ey <- list( title = input$box111 , tickfont = list(size = 8, color = "black"),
                    titlefont = list(size = 10)); ex <- list( title = "Average Prediction" , 
                                                              tickfont = list(size = 8, color = "black"), titlefont = list(size = 10))
                    
                    partial_cat <- plot_ly(var_effects_cat %>% filter(`_vname_` == input$box111 ), x = ~`_yhat_`, y = ~`_x_`, 
                                           marker = list(color = "#9A3324"),
                                           type = 'bar', orientation = 'h') %>% layout(yaxis = ey) %>% layout(xaxis = ex)
                    
                    partial_cat
      }
      
    }
  )
  
  
  
  observeEvent(input$loanstatus== "FALSE", { 
    updateSelectizeInput(session, "loanstatus_true", label = NULL,
                         choices = names(table(as.character(data_org$loan_status))), selected = "Current") 
  })
  
  
  observeEvent(input$loanterm=="FALSE", {
    updateSelectizeInput(session, "loanterm_true", label = NULL,
                         choices = names(table(as.character(data_org$term))), selected = "36 months" ,
                         options = list(placeholder = "")) 
  })
  
  observeEvent(input$loangrade=="FALSE", { 
    updateSelectizeInput(session, "loangrade_true", label = NULL,
                         choices = names(table(as.character(data_org$grade))), selected = names(table(as.character(data_org$grade))),
                         options = list(placeholder = "")) 
  })
  
  observeEvent(input$apptype=="FALSE", { 
    updateSelectizeInput(session, "apptype_true", label = NULL,
                         choices = names(table(as.character(data_org$apptype))), selected = "Individual",
                         options = list(placeholder = "")) 
  })
  
  observeEvent(input$imonth=="FALSE", { #for HBs
    updateSelectizeInput(session, "imonth_true", label = NULL,
                         choices = names(table(as.character(data_org$imonth))), selected = "Dec",
                         options = list(placeholder = "")) 
  })
  


  filter_table <- reactive ( {
    
    
    
    if (!is.null(input$loanstatus_true)) { 
      filtered_data <- data_org %>% 
        filter(loan_status %in% input$loanstatus_true) %>% 
        filter(term %in% input$loanterm_true) %>%
        filter(grade %in% input$loangrade_true) %>%
        filter(application_type %in% input$apptype_true) %>% 
    
        filter(issue_d_m %in% input$imonth_true) %>% filter(ifelse(is.null(input$code), addr_state %in% names(table(as.character(data_org$addr_state))),
                                                                   addr_state %in% input$code))
      
    } 
    
    if (!is.null(input$loanterm_true)) { 
      filtered_data <- data_org %>% 
        filter(term %in% input$loanterm_true) %>% 
        filter(loan_status %in% input$loanstatus_true) %>%
        filter(grade %in% input$loangrade_true) %>%
        filter(application_type %in% input$apptype_true) %>%
  
        filter(issue_d_m %in% input$imonth_true)  %>% filter(ifelse(is.null(input$code), addr_state %in% names(table(as.character(data_org$addr_state))),
                                                                    addr_state %in% input$code))
    }
    #                
    if (!is.null(input$loangrade_true)) { 
      filtered_data <- data_org %>% 
        filter(grade %in% input$loangrade_true) %>% 
        filter(term %in% input$loanterm_true)  %>% 
        filter(loan_status %in% input$loanstatus_true) %>%
        filter(application_type %in% input$apptype_true) %>% 
     
        filter(issue_d_m %in% input$imonth_true) %>% filter(ifelse(is.null(input$code), addr_state %in% names(table(as.character(data_org$addr_state))),
                                                                   addr_state %in% input$code))
    }
    
    if (!is.null(input$apptype_true)) { 
      filtered_data <- data_org %>% 
        filter(application_type %in% input$apptype_true) %>%
        filter(loan_status %in% input$loanstatus_true) %>% 
        filter(term %in% input$loanterm_true) %>%
        filter(grade %in% input$loangrade_true) %>% 
       
        filter(issue_d_m %in% input$imonth_true) %>% filter(ifelse(is.null(input$code), addr_state %in% names(table(as.character(data_org$addr_state))),
                                                                   addr_state %in% input$code))
      
    }
    
    
    
    if (!is.null(input$imonth_true)) { 
      filtered_data <- data_org %>% 
        filter(issue_d_m %in% input$imonth_true) %>%
        filter(addr_state %in% input$code) %>%
        filter(application_type %in% input$apptype_true) %>%
        filter(loan_status %in% input$loanstatus_true) %>% 
        filter(term %in% input$loanterm_true) %>%
        filter(grade %in% input$loangrade_true) %>% filter(ifelse(is.null(input$code), addr_state %in% names(table(as.character(data_org$addr_state))),
                                                                  addr_state %in% input$code))
      
      
      
    }
    
    
    if (!is.null(input$code)) { 
      filtered_data <- data_org %>% 
        filter(addr_state %in% input$code) %>%
        filter(application_type %in% input$apptype_true) %>%
        filter(loan_status %in% input$loanstatus_true) %>% 
        filter(term %in% input$loanterm_true) %>%
        filter(grade %in% input$loangrade_true) %>%
        filter(issue_d_m %in% input$imonth_true) 
      
    } else {
      
      filtered_data <- data_org %>% 
        filter(addr_state %in% names(table(as.character(data_org$addr_state)))) %>%
        filter(application_type %in% input$apptype_true) %>%
        filter(loan_status %in% input$loanstatus_true) %>% 
        filter(term %in% input$loanterm_true) %>%
        filter(grade %in% input$loangrade_true) %>%
        filter(issue_d_m %in% input$imonth_true) 
      
      
    }
    
 
    
    table <- filtered_data %>% select(ID, `Probability(%)` = Prob, loan_status, term, grade, sub_grade, application_type, issue_d, issue_d_m, addr_state,
                                      
                                      emp_title, title, emp_length, desc, purpose, disbursement_method,
                                      home_ownership,  `annual_inc($)`  = annual_inc, verification_status, `int_rate(%)` = int_rate, `tot_hi_cred_lim($)` = tot_hi_cred_lim, open_acc, 
                                      `loan_amnt($)` = loan_amnt, `total_rec_prncp($)` = total_rec_prncp, `total_pymnt($)` = total_pymnt, `total_rec_int($)` = total_rec_int, `recoveries($)` = recoveries, `pub_rec($)` = pub_rec, `pub_rec_bankruptcies($)` = pub_rec_bankruptcies,
                                      `tot_cur_bal($)` = tot_cur_bal, `dti($)` = dti, `all_util($)` = all_util, `il_util($)` = il_util, `total_bal_il($)` = total_bal_il, `revol_bal($)` = revol_bal, `revol_util($)` = revol_util,total_acc, `annual_inc_joint($)` = annual_inc_joint, `dti_joint($)` = dti_joint, mort_acc, verification_status_joint,
                                      acc_now_delinq,
                                      last_credit_pull_d,last_pymnt_d,last_pymnt_amnt, next_pymnt_d,  earliest_cr_line, mths_since_last_delinq,
                                      debt_settlement_flag,settlement_amount,settlement_term, inq_last_6mths, open_rv_24m, total_rec_late_fee
    )
    table
 
    
  })
  
  
  output$table_filtered <- DT::renderDataTable({
    
    DT::datatable(filter_table(), height = "700px", class = 'cell-border stripe', 
                  style = 'bootstrap', rownames = FALSE, options = list(dom = 'tp', 
                                                                        lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                                                                        
                                                                                                           
                                                                        language = list(
                                                                          zeroRecords = "No records found matching your selection - 
                    have you selected a geography and at least one indicator/domain/profile? 
                    See 'Indicator definitions' under the Info tab for Geographies available.") )   
 
    ) %>% formatStyle( 0, target= 'row', fontWeight ='bold', lineHeight='10%')
  })
  
output$table_dictionary <- DT::renderDataTable({
    
    DT::datatable(dictionary, height = "600px", class = 'compact', 
                  style = 'bootstrap', rownames = FALSE, options = list(dom = 'tp'
                                                
                                                                       )   

    ) %>% formatStyle( 0, target= 'row', fontWeight ='bold', lineHeight='10%')
  })
  
  
  
  output$loan_map <- renderLeaflet({
    basemap %>% setView(-99.066067, 39.390897, zoom = 4)   
    
  })
  
  
fi_map_plot <- reactive ( {
    
    theme_text_border <- theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), 
                              axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8), 
                             panel.border = element_blank(), panel.grid.minor.x  = element_blank(), 
                            panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(), plot.margin = margin(5, 12, 5, 5) )  
    
    t1_map <- reactive_status %>% filter(as.character(State_Area) %in% as.character(input$region_s)  ) %>% filter(as.character(issue_d_y) %in% as.character(input$year_s) ) %>% select(issue_d_m, Current, `Fully Paid`,Default, `Charged Off`, `Late (16-30 days)`, `Late (31-120 days)`) %>% 
      reshape2::melt(id.var = c("issue_d_m")) %>% group_by(issue_d_m, variable) %>% summarize(Total = sum(value))
    
   
  })
  
 
  
  output$plot1 <- renderPlotly(
    { 
      gy <- list( title = "Loan Amount ($)", tickfont = list(size = 8),
                  titlefont = list(size = 10)); gx <- list( title = "", tickfont = list(size = 8),
                                                            titlefont = list(size = 10))
  
      t1_graph <- ggplot(fi_map_plot() %>% filter(as.character(variable) %in% c("Current", "Fully Paid")  ), aes(x=
                                                                                                                 factor(issue_d_m,
                                                                                                  levels = c(
                                                                                                  "Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec") )
                                                                                                                 
                                                                                                                 , y=Total, col=variable, group = variable,
                                                  text = paste0("Loan Status: ", variable))) + geom_line()  + theme_minimal(base_size = 8)  + 
      scale_color_manual(values = c("#025F1D", "#279F00"))  + theme(legend.title=element_blank()) + scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3)) + 
      scale_x_discrete(breaks = c("Jan","Jun", "Dec")) + 
      labs(y="Loan Amount ($)", x = "") +  theme(plot.title = element_text(hjust=0.5, size = 10, face = "bold")) + theme_text_border
                  
                              
      ggplotly(t1_graph, tooltip = c("text")) %>% layout(yaxis = gy) %>% layout(xaxis = gx) %>% hide_legend()
      #ggplotly(plot_dice)
      
    }
  )
  
  output$plot2 <- renderPlotly(
    { 
      
      gy <- list( title = "Loan Amount ($)", tickfont = list(size = 8),
                  titlefont = list(size = 10)); gx <- list( title = "", tickfont = list(size = 8),
                                                            titlefont = list(size = 10))
      
     t2_graph <- ggplot(fi_map_plot() %>% filter(variable %in% c("Charged Off", "Late (31-120 days)") ), 
           aes(x=factor(issue_d_m,
                        levels = c(
                          "Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec") ), y=Total, col=variable, group = variable,text = paste0("Loan Status: ", variable))) + geom_line()  + theme_minimal(base_size = 8)  + 
           scale_color_manual(values = c("#E10600", "#EF6A00"))  + theme(legend.title=element_blank()) + scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3)) + 
           scale_x_discrete(breaks = c("Jan","Jun", "Dec")) + 
           labs(y="Loan Amount ($)", x = "") +  theme(plot.title = element_text(hjust=0.5, size = 10, face = "bold")) + theme_text_border
                  
      ggplotly(t2_graph, tooltip = c("text")) %>% layout(yaxis = gy) %>% layout(xaxis = gx) %>% hide_legend()

      
    }
  )
  

  
observeEvent(input$year_s, {
    

  new_reactive <- reactive_status %>% filter(as.character(issue_d_y) %in% input$year_s) %>% select(latitude,longitude,name, Current, `Fully Paid`, Default, `Charged Off`, `Late (16-30 days)`, `Late (31-120 days)`) %>% 
  group_by(name, latitude, longitude) %>% summarize(Current = sum(Current), `Fully Paid` = sum(`Fully Paid`), Default = sum(Default),
                                                      `Charged Off` = sum(`Charged Off`), `Late (16-30 days)` = sum(`Late (16-30 days)`), `Late (31-120 days)` = sum(`Late (31-120 days)`), .groups = "drop" ) %>% ungroup() 
 
    
    leafletProxy("loan_map") %>%  
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons   , stroke = TRUE, color = "#808080", weight = 1,
                  smoothFactor = 0.3, fillOpacity = 0.15, fillColor = ~cv_pal(big_states$Total)) %>% 
      
      addCircles(data = new_reactive, lat = ~ latitude, lng = ~ longitude, 
                 weight = 1, radius = ~(Current)^(1/4) * 900, 
                 fillOpacity = 0.15, color = current, group = "Current",
                 label = sprintf("<strong>%s</strong><br/>Current Total Amount (Million$): %g<br/>", new_reactive$name, round(new_reactive$Current/1000000,1)) %>% 
                   lapply(htmltools::HTML), labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = current),
                     textsize = "15px", direction = "auto")) %>%
      
      addCircles(data = new_reactive , lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(`Fully Paid`)^(1/4) * 900, 
                 fillOpacity = 0.15, color = fully, group = "Fully Paid",
                 label = sprintf("<strong>%s</strong><br/>Fully Paid Total Amount (Million $): %g<br/>", new_reactive$name, round(new_reactive$`Fully Paid`/1000000,1) ) %>% 
                   lapply(htmltools::HTML), labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" =  fully ),
                     textsize = "15px", direction = "auto")) %>%
      
      addCircles(data = new_reactive, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Default)^(1/4) * 5000, 
                 fillOpacity = 0.15, color = default, group = "Default",
                 label = sprintf("<strong>%s</strong><br/>Default Total Amount (Thousand $): %g<br/>", new_reactive$name, round(new_reactive$Default/1000,1 )) %>% 
                   lapply(htmltools::HTML), labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" =  default ),
                     textsize = "15px", direction = "auto")) %>%
      
      addCircles(data = new_reactive , lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(`Charged Off`)^(1/4) * 800, 
                 fillOpacity = 0.15, color = charge, group = "Charged Off",
                 label = sprintf("<strong>%s</strong><br/>Charged Off Total Amount (Million $): %g<br/>", new_reactive$name, round(new_reactive$`Charged Off`/1000000,1) ) %>% 
                   lapply(htmltools::HTML), labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" =  charge ),
                     textsize = "15px", direction = "auto")) %>%
      
      
      addCircles(data = new_reactive , lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(`Late (16-30 days)`)^(1/4) * 1000, 
                 fillOpacity = 0.15, color = late1, group = "Late (16-30 days)",
                 label = sprintf("<strong>%s</strong><br/>Late (16-30 days) Total Amount (Million $): %g<br/>", new_reactive$name, round(new_reactive$`Late (16-30 days)`/1000000,1)) %>% 
                   lapply(htmltools::HTML), labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" =  late1 ),
                     textsize = "15px", direction = "auto")) %>%
      
      addCircles(data = new_reactive , lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(`Late (31-120 days)`)^(1/4) * 1000, 
                 fillOpacity = 0.15, color = late2, group = "Late (31-120 days)",
                 label = sprintf("<strong>%s</strong><br/>Late (31-120 days) Total Amount (Million $): %g<br/>", new_reactive$name, round(new_reactive$`Late (31-120 days)`/1000000,1 )) %>% 
                   lapply(htmltools::HTML), labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" =  late2 ),
                     textsize = "15px", direction = "auto"))
  }
  
  )
  
  
  
  sars_mod_date = reactive({   
    format(as.Date(input$sars_plot_date, format="%d %b %y"), "%Y-%m-%d")
  })
  
  
  
  sars_reactive_db = reactive({     
    sars_cases %>% filter(date == sars_mod_date())
  })
  
  
  output$sars_reactive_case_count <- renderText({
    paste0(sum(sars_reactive_db()$cases), " cases")
  })
  
  output$sars_reactive_death_count <- renderText({
    paste0(sum(sars_reactive_db()$deaths), " deaths")
  })
  
  
  output$sars_reactive_country_count <- renderText({
    paste0(length(unique(sars_reactive_db()$country_group)), " countries/territories affected")
  })
  
  
  
}



