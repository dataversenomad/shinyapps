


ui <- fluidPage( 
  
  style = "background-color: #ECF0F5", 
  

  tags$style("
  
   .nav-tabs-custom .nav-tabs li.active a { color: 	#000000; 
   border-color: transparent; border-top-color: #686F84;}
   .nav-tabs-custom .nav-tabs li a {color: #d1351be6;font-size: 12px; background-color: #ffffff; color:#000000; border-bottom: 1px solid #BBC7D6;  border-right:1px solid #BBC7D6;}
   .nav-tabs-custom .nav-tabs li.active a {font-size: 12px; border-bottom: 1px solid #BBC7D6; border-right:1px solid #BBC7D6;}
   
   
   th {
	background-color: #000000;
	color: white; font-size: 10px;
   }
   
   th, td {
    font-size: 10px;
  }
            "),
  
  navbarPage(theme = shinytheme("cerulean"), collapsible = TRUE, "LENDING COMPANY", id ="nav",
             
             
             tabPanel( "Dashboard" ,icon = icon("dashboard"), 
                       sidebarLayout( 
                         sidebarPanel(
                           
                           tags$h5("FILTER PANEL"),
                           
                           
                           pickerInput(
                             inputId = "year_1",
                             label = "Loan issued year", 
                             choices = c("2015", "2016", "2017", "2018"),
                             multiple = FALSE,
                             selected = "2018",
                             options = list(
                               size = 5)
                           ),
                           pickerInput(
                             inputId = "term_1",
                             label = "Loan term", 
                             choices = c("36 months", "60 months"),
                             multiple = FALSE,
                             selected = "36 months" ,
                             options = list(
                               size = 5)
                           ),
                           pickerInput(
                             inputId = "app_1",
                             label = "Application type", 
                             choices = c("Individual", "Joint App"),
                             multiple = FALSE,
                             selected = "Individual" ,
                             options = list(
                               size = 5)
                           ), 
                           
                           
                           width = 2
                         ),
                         
                         mainPanel(
                           
                           fluidRow( div( id = "row1",column(width = 6,  uiOutput("box1") ), column(width = 6, uiOutput("box5") )  )),
                           br(), #### CAMBIO
                           fluidRow( div( id = "row2",column(width = 4,  uiOutput("box3") ), column(width = 3, uiOutput("box2") ), column(width = 5, uiOutput("box6")  )))
                           
                           ,width = 10
                           
                         )
                         
                       )
             ), 
             
             tabPanel( "Loan Mapper" ,icon = icon("map-marked-alt"),
                       div(class="outer",
                           tags$head(includeCSS("styles.css")),
                           leafletOutput("loan_map", width="100%", height="100%"),
                           
                           absolutePanel(id = "controls", class = "panel panel-default",
                                         top = 75, left = 55, width = 250, fixed=TRUE,
                                         draggable = TRUE, height = "auto",
                                         
                                         span(tags$i(h6("Explore the total loan amount trend by its status. You can use the filters below to select origin state and issued year. Panel on the bottom right helps to explore loan status/amount on the map as well.")), style="color:#00249C"),
                                         #h3(textOutput("reactive_case_count"), align = "right"),
                                         #h4(textOutput("reactive_death_count"), align = "right"),
                                         #span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                         #span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                         #h6(textOutput("clean_date_reactive"), align = "right"),
                                         #h6(textOutput("reactive_country_count"), align = "right"),
                                         #tags$i(h6("Updated once daily. For more regular updates, refer to: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard."))),
                                         plotlyOutput("plot1", height="130px", width="100%"),
                                         plotlyOutput("plot2", height="130px", width="100%"),
                                         
                                         
                                         pickerInput(
                                           inputId = "region_s",
                                           label = "Select by address code", 
                                           choices = names(table(as.character(data_r$addr_state))) ,
                                           multiple = FALSE,
                                           
                                           selected = "AK",
                                           options = list(
                                             size = 5)
                                         )
                                         
                                         ,
                                         pickerInput(
                                           inputId = "year_s",
                                           label = "Select by issued year", 
                                           choices = names(table(as.character(reactive_status$issue_d_y))),
                                           multiple = FALSE,
                                           
                                           selected = "2018",
                                           options = list(
                                             size = 5)
                                         )
                                         
                                         
                                         
                           )
                       )
             ),
             
             ##### MODEL DESCRIPTION
             
             tabPanel("Model Description", icon = icon("book"),
                      
                      mainPanel(width=12,
                                fluidRow(p(h3("Machine Learning Model: Random Forest"),
                                           
                                           h4("BACKGROUND", style = "color:black;"), 
                                           h5("Random forest is an ensemble-based learning algorithm which is comprised of n collections of de-correlated decision trees. It is built off the idea of bootstrap aggregation, which is a method for resampling with replacement each particular subsets of data in order to reduce variance (errors between predictions). ", style = "color:black;"),
                                           h5("Random Forest uses multiple trees to average (regression) or compute majority votes (classification) in the terminal leaf nodes when making a prediction. Built off the idea of decision trees, random forest models have resulted in significant improvements in prediction accuracy as compared to a single tree by growing 'n' number of trees; each tree in the training set is sampled randomly without replacement. Decision trees consist simply of a tree-like structure where the top node is considered the root of the tree that is recursively split at a series of decision nodes from the root until the terminal node or decision node is reached.", style = "color:black;"),
                                           h5("As illustrated in the tree structure from the next figure, the decision tree algorithm is a top down 'greedy' approach that partitions the dataset into smaller subsets. ", style = "color:black;"),
                                           
                                           br(),
                                           
                                           tags$img(src="d9.png",height="15%", width="15%"),
                                           
                                           br(),
                                           
            h5("This is an example of a decision tree with “age” as root node; “gender” and “last_r” are two decision nodes.
            Greedy algorithms are those that take the simplest solution rather than the most optimal solution. To determine which feature to split on at each node, the entropy is computed. Entropy measures the homogeneity of the subset data.", style = "color:black;"),
            h5("Advantages of using a tree-like learning algorithm allow for training models on large datasets in addition to quantitative and qualitative input variables. Additionally, tree-based models can be immune to redundant variables or variables with high correlation which may lead to overfitting (modeling error that occurs when a function is too closely fit to a limited set of data points) in other learning algorithms.", style = "color:black;"),
            h5("Trees also have very few parameters to tune for when training the model and performs relatively well with outliers or missing values in a dataset. However, trees are prone to poor prediction performance; decision trees themselves are prone to overfitting noise in a training set which ultimately leads to results with high variance. In other words, this means the model could accurately predict the same data it was trained on but may not possess the same performance on datasets without the similar patterns and variations in the training set. Even fully-grown decision trees are notorious for overfitting and do not generalize well to unseen data; random forest solves the overfitting conundrum by using a combination or 'ensemble' of decision trees where the values in the tree are a random, independent, sample.", style = "color:black;"),
            h5("Randomly sampling with replacement is known as bagging and this results in a different tree being generated to train on; averaging the results from the 'n' number of trees will result in decreasing the variance and establishing a
smoother decision boundary. For instance, while using random forest for classification, each tree will give an estimate of the probability of the class label, the probabilities will be averaged over the 'n' trees and the highest yields the predicted class label
", style = "color:black;"),
                                           
                                           br(),
                                           
                                           tags$img(src="d10.png",height="25%", width="25%"),
                                           
                                           br(),
                                           
            h5("In addition to bagging or bootstrap aggregation, in order to further reduces the variance in the decision boundary further, the trees must be completely uncorrelated, and the method of bootstrapping alone is not enough. To cope with this situation, we need to randomly sample 'm' number of features at each decision split in the tree as a way to de-correlate the trees in the random forest algorithm", style = "color:black;"),
            
            br(),
                                           h4("MODEL SUMMARY",
                                              style = "color:black;"),
                                           
                                           
                                           br(),
                                           
            h5("Number of data registries: 2,260,668", style = "color:black;"),
            h5("Number of data columns (features): 149", style = "color:black;"),
            h5("Number of data columns selected (features): 94", style = "color:black;"),
            h5("Target feature: Loan Status", style = "color:black;"),
            h5("Positive Class: Fully Paid", style = "color:black;"),
            h5("Negative Class: Charged Off/Default", style = "color:black;"),
            h5("Software: R", style = "color:black;"),
            h5("R Packages (Most used): H2O, Leaflet, Ggplot2, Plotly, Shinydashboard, Lime, Dalex, IceBox, Maps.", style = "color:black;"),
            h5("Machine Learning Algorithm name: Random Forest", style = "color:black;"),
            h5("Machine Learning Algorithm type: Classification (binomial)", style = "color:black;"),
            h5("Imbalanced classes: Yes", style = "color:black;"),
            h5("Number of Folds: 10", style = "color:black;"),
            h5("Number of trees: 5", style = "color:black;"),
            h5("Number of internal trees: 5", style = "color:black;"),
            h5("Minimum depth: 20", style = "color:black;"),
            h5("Maximum depth: 20", style = "color:black;"),
            h5("Mean depth: 20", style = "color:black;"),
            h5("Minimum of leaves: 2586", style = "color:black;"),
            h5("Maximum of leaves: 4744", style = "color:black;"),
            h5("Mean of leaves: 3273.20", style = "color:black;"),
            h5("Mean Per-Class Error:  0.01442578", style = "color:black;"),
            h5("AUC:  0.9926648", style = "color:black;"),
            h5("AUC Precision/Recall:  0.987395", style = "color:black;"),
            h5("Gini:  0.9853295", style = "color:black;"),
                                           style = "font-size:20px")),
                                
                      )#main panel bracket
             ),
             
             tabPanel( icon = icon("bar-chart-o"),  title="Model Performance", 
                       mainPanel( 
                         fluidRow(br(), div( id = "row7",
                                             column(width = 4,  uiOutput("box7") ) , 
                                             column(width = 4, uiOutput("box8") ), 
                                             column(width = 4, div(htmlOutput("box9")) )  )  ),
                         
                         fluidRow(br(), div( id = "row15",column(width = 6,  uiOutput("box10") )
                                             , column(width = 6, uiOutput("box11"))  
                         )), fluidRow(br(), br(),br())
                         
                         ,width = 12
                         
                       )
                       
             ),
             
             tabPanel( "Model Evaluation" ,icon = icon("chart-line"),
                       #div(class="outer",
                       #      tags$table (includeCSS("new_style.css"))
                       #  )
                       # ,
                       mainPanel(
                         width = 12, style="margin-left:0.5%; margin-right:0.5%",
                         #Row 1 for intro  
                         
                         
                         fluidRow(
                           
                           column(2,
                                  p("Filter data by: ", style = "font-weight: bold; color: black;"),
                                  # Scotland selections
                                  
                                  # Panel for health board selections
                                  #awesomeCheckbox("loanstatus",label = "Loan Status", value = FALSE, status = "primary"),
                                  prettyCheckbox(inputId = "loanstatus", label = "Loan Status", icon = icon("check")),
                                  conditionalPanel(
                                    condition = "input.loanstatus == true",
                                    selectInput("loanstatus_true", label = NULL, 
                                                choices = names(table(as.character(data_org$loan_status))), selected = FALSE, multiple=FALSE)) ,
                                  
                                  #awesomeCheckbox("loanterm",label = "Term", value = FALSE),
                                  prettyCheckbox(inputId = "loanterm", label = "Term", icon = icon("check")),
                                  conditionalPanel(
                                    condition = "input.loanterm == true",
                                    selectInput("loanterm_true", label = NULL,
                                                choices = names(table(as.character(data_org$term))), selected = FALSE, multiple=TRUE)),
                                  #awesomeCheckbox("loangrade",label = "Loan Grade", value = FALSE),
                                  prettyCheckbox(inputId = "loangrade", label = "Loan Grade", icon = icon("check")),
                                  conditionalPanel(
                                    condition = "input.loangrade == true",
                                    selectInput("loangrade_true", label = NULL,
                                                choices = names(table(as.character(data_org$grade))), selected = FALSE, multiple=TRUE)),
                                  #awesomeCheckbox("apptype",label = "Application Type", value = FALSE),
                                  prettyCheckbox(inputId = "apptype", label = "Application Type", icon = icon("check")),
                                  conditionalPanel(
                                    condition = "input.apptype == true",
                                    selectInput("apptype_true", label = NULL,
                                                choices = names(table(as.character(data_org$application_type))), selected = "Individual", multiple=FALSE)),
                                  prettyCheckbox(inputId = "imonth", label = "Issued Month", icon = icon("check")),
                                  conditionalPanel(
                                    condition = "input.imonth == true",
                                    selectInput("imonth_true", label = NULL,
                                                choices = names(table((data_org$issue_d_m))), selected = "Dec", multiple=TRUE)),   
                                  
                                  selectizeInput("code", label = NULL, choices = names(table(as.character(data_org$addr_state))),
                                                 options = list(placeholder = 'Search by region code'), 
                                                 multiple=TRUE, selected = "")
                                  
                                  
                                  
                           ), # column bracket
                           column(2, p("What is the contribution of each feature on the probability of payment?", style = "font-weight: bold; color: black;"),
                                  
                                  # Panel for HSC partnership selections
                                  #awesomeCheckbox("ivalue",label = "Feature", value = FALSE),
                                  
                                  #     prettyCheckbox(inputId = "ivalue", label = "Select ID: ", icon = icon("check")),
                                  #       conditionalPanel(
                                  #          condition = "input.ivalue== true",
                                  
                                  #  numericInput("ivalue_true", label = NULL, value = 1, min = 1, max = max(data_org$ID), step = NA,
                                  #           width = NULL))
                                  
                                  
                                  numericInput("ivalue_true", label = "ID: ", value = 1, min = 1, max = max(data_org$ID), step = FALSE,
                                  ) ,   #)#, 
                                  actionButton("predict", icon("refresh"), width = '50%'),
                                  helpText(p("Enter a valid record ID from the table,",
                                             "then click on refresh buttom to see the feature impact on a single record ID" ,"It takes about 30 seconds to build the overall relationships and create the graph (It's not recommended to interact with any filters or navigate through other tabs). Results are displayed to the right:" , 
                                             style = "font-weight: bold; color: black; font-size:10px; ")  
                                  ) #,      
                                  #verbatimTextOutput("value")                          
                                  #selectInput("hscp_true", label = NULL, choices = c("Indicator", "Domain", "Profile"), 
                                  #           selected = NULL, multiple=TRUE))
                                  #selectInput("ivalue_true", label = NULL, choices = as.numeric(names(table((data_org$ID)))) , 
                                  #          selected = 1, multiple=FALSE))
                           ),
                           
                           column(width = 4,  uiOutput("box88") ),
                           column(width = 4,  uiOutput("box99") )
                           
                           
                         ),  ## here
                         
                         fluidRow(  
                           p("   Lending Company Data (2018)", style = "font-weight: bold; color: black;"), 
                           column(12, div(
                             
                             
                             DT::dataTableOutput("table_filtered", height="400px"),
                             style = "height:600px; font-size: 80%; width: 100%"))
                         )
                         
                         
                         
                       )
                       
             ),
             
             ##############NavBar Menu----
             ###############################################.
             #Starting navbarMenu to have tab with dropdown list
             navbarMenu("Info", icon = icon("info-circle"),
                        ###############################################.
                        ## About ----
                        ###############################################.
                        tabPanel("About", value = "about",
                                 
                                 mainPanel(width=8,
                                           h4("About this site", style = "color:black;"),  p("This application is intended to show how machine learning can be used in several industries to solve real problems and take decisions. Digital lending industries and FINTECHS are some examples, and the goal of this application is put an algorithm in production called Random Forest that can help to estimate what is the probability that a borrower is likely to pay a loan. In other words, how likely is that borrowers fail to pay back a debt according to an initial arrangement."),
                                            p("This application was designed using the open source software “R” and Shiny dashboard, which is one package that can be attached to it. Other packages such as H2O and Leaflet were used."),
                                            p("All the data used on this site was collected on Kaggle web page. You can follow the next link to get the dataset used for this dashboard: ", tags$a(href="https://www.kaggle.com/wendykan/lending-club-loan-data", "Kaggle", 
                                                          class="externallink"), ". Kaggle provides a huge repository of datasets for data scientists or anyone who wants to do exploratory data analysis or create visualizations using these datasets. Several companies across the globe are willing to share their information to be used as a competition for developing machine learning models."  ),
                                           p("You can click on “Quick tour” to see a brief description of each tab and the functionally of this application.",
                                          "."), p("If you have any questions or suggestions, you can reach me out at: james31982@gmail.com") 
                                          ) ,
                                 br()
                        ),#Tab panel
     
                        tabPanel("Basic terminology", value = "about",
                                 
                                 mainPanel(width=12,
                                           h2("Basic terminology used on this site", style = "color:black;"),
                                           br(),
                                           h4("Machine Learning", style = "color:black;"),  
                                           p("Machine learning is a subfield of computer science that is concerned with building algorithms
                                              which, to be useful, rely on a collection of examples of some phenomenon. These examples can come from nature, be handcrafted by humans or generated by another algorithm. Machine learning can also be defined as the process of solving a practical problem by 1) gathering a dataset, and 2) algorithmically building a statistical model based on that dataset. That statistical model is assumed to be used somehow to solve the practical problem."),
                                           h4("Training Data:", style = "color:black;"),  
                                           p("Training data is the main and most important data which helps machines to learn and make the predictions. This data set is used by machine learning engineer to develop your algorithm and more than 70% of your total data used in the project. A huge quantity of datasets are used to train the model at best level to get the best results."),
                                           h4("Validation Data:", style = "color:black;"),  
                                           p("This is the second type of data set used to validate the machine learning model before final delivery of project. ML model validation is important to ensure the accuracy of model prediction to develop a right application. Using this type of data helps to know whether model can correctly identify the new examples or not."),
                                           h4("Test Data ", style = "color:black;"),  
                                           p("This is the final and last type of data helps to check the prediction level of machine learning and AI model. Its is similar to validation data in testing the model accuracy but don’t help to improve the prediction level. It is basically used to test the model whether it will work well in real-life use and final test in the moment of truth for the model, if it works perfectly."),
                                           h4("Receiver Operating Characteristics (ROC)", style = "color:black;"),  
                                           p("AUC (area under the curve) - ROC curve is a performance measurement for classification problem at various thresholds settings. ROC is a probability curve and AUC represents degree or measure of separability. It tells how much model is capable of distinguishing between classes. Higher the AUC, better the model is at predicting 0s as 0s and 1s as 1s. By analogy, Higher the AUC, better the model is at distinguishing between default / no default borrowers"),
                                           h4("Performance Metrics:", style = "color:black;"),  
                                           p("Performance metrics tell us about how well the machine learning model was able to capture the predictions correctly. Usually, they are given as percentage."),
                                           h4("True Positive Rate (TPR):", style = "color:black;"),  
                                           p("It measures how many observations out of all positive observations have we classified as positive. It tells us how many “Discharged” transactions (Default or unpaid) we recalled from all “Discharged” transactions."),
                                           h4("False Positive Rate (FPR): ", style = "color:black;"),  
                                           p("When we predict something, when it isn’t, we are contributing to the false positive rate. You can think of it as a fraction of false alerts that will be raised based on your model predictions. In our case, it represents “Discharged” transactions classified as “Fully Paid” transactions."),
                                           h4("True Negative Rate (TNR): ", style = "color:black;"),  
                                           p("It measures how many observations out of all negative observations have we classified as negative. In our loan application, it tells us how many transactions, out of all “Fully Paid” transactions, we marked as “Fully Paid”."),
                                           h4("False Negative Rate (FNR): ", style = "color:black;"),  
                                           p("When we don’t predict something, when it is, we are contributing to the false negative rate. We can think of it as a “Fully Paid” transactions that the model lets through."),
                                           h4("Precision: ", style = "color:black;"),  
                                           p("It measures how many observations predicted as positive are in fact positive. Taking our loan application, it tells us what is the ratio of transactions that were correctly classified as “Charged or Default”."),
                                           h4("Recall: ", style = "color:black;"),  
                                           p("Applying the same understanding, Recall shall be the model metric we use to select our best model when there is a high cost associated with False Negative."),
                                           h4("Accuracy: ", style = "color:black;"),  
                                           p("It measures how many observations, both positive and negative, were correctly classified."),
                                           h4("Specificity: ", style = "color:black;"),  
                                           p("Specificity is defined as the proportion of actual negatives, which got predicted as the negative (or true negative)."),
                                           h4("Threshold: ", style = "color:black;"),  
                                           p("Represents the cut-off (in percentage) value where precision and recall meet. Threshold zero means lowest precision, highest recall; Threshold one means highest precision, lowest recall. "),
                                           h4("F2:", style = "color:black;"),  
                                           p("It’s a metric that combines precision and recall, putting 2x emphasis on recall."),
                                           h4("Feature:", style = "color:black;"),  
                                           p("In machine learning and pattern recognition, a feature is an individual measurable property or characteristic of a phenomenon being observed. In other words, corresponds to a particular column on the dataset."),
                                           h4("Feature Importance: ", style = "color:black;"),  
                                           p("Feature importance refers to techniques that assign a score to input features based on how useful they are at predicting a target variable (Probability of payment)"),
                                           h4("Partial Dependency Plot: ", style = "color:black;"),  
                                           p("The partial dependence plot (short PDP or PD plot) shows the marginal effect one or two features have on the predicted outcome of a machine learning model."),
                                           h4("Individual Conditional Explanation: ", style = "color:black;"),  
                                           p("While partial dependence plots provide the average effect of a feature, Individual Conditional Expectation (ICE) plots are a method to disaggregate these averages. ICE plots visualize the functional relationship between the predicted response and the feature separately for each instance. In other words, a PDP averages the individual lines of an ICE plot")
                                           
                                           
                                 ) ,
                                 br()
                        ),#Tab pane
                        
                        ###############################################.
                        tabPanel("Data dictionary", value = "definitions",
                                 #Sidepanel for filtering data
                                 fluidRow(style = "width:80%; margin-left: 2%; min-width: 350px",
                                          h4("Data dictionary (code book)", style = "color:black;"),
                                          h5(style = "color:black", 
                                             "On the next table, you can find all information about the features (data columns) used on this site:"),

                                          column(12, div(
                                            DT::dataTableOutput("table_dictionary", height="380"),
                                            style = "height:600px; font-size: 80%; width: 100%")))
                                 
                        ), #tab panel
                        ###############################################.             

                        ###############################################.             
           
                        ############## Tour of the tool----    
                        ###############################################.
                        tabPanel("Quick tour", value = "tour",
                                 
                                 mainPanel(width=10,
                                           fluidRow(p(h4("Welcome to the Lending Company Application"),
                                                      h5("This interactive application helps a lending company to monitor the distribution of loan payments and accounts performance managed across the U.S. country. It also helps to keep tracking of all borrowers financial status and credit purposes.", style = "color:black;"),
                                                      h5("Basically the application is divided in four parts: Dashboard, Loan Mapper, Model Performance and Model Evaluation. In some tabs there are specific slicers that help to filter the data according to some criteria.", style = "color:black;"),
                                                      br(),
                                                      h4("DASHBOARD - Tab",
                                                         style = "color:black;"),
                                                      tags$img(src="d1.png",height="50%", width="50%"),
                                                      br(),
                                                      br(),
                                                      h5("From left to right (top 2 graphs): The first graph is intended to show the total balance of the company split by each managed account. We can play with the tabs to extract information about the interest rate trend and received principal and interests by each year. Second graph shows the loan amount distribution split by proportion and grade type managed by the company; the red line represents the average interest rate",
                                                         style = "color:black;"),
                                                     
                                                      h5("From left to right (buttom graphs): The last 3 graphs help to monitor the borrowers financial status. For example, in the first graph we can monitor the proportion that has been acquired by the borrowers ownership (that is: rent, own property, mortgage, etc.) and the destination of the credit. We can track the amount the borrowers performance according to their financial status like public records, tax liens and bankruptcies. ",
                                                       style = "color:black;"),
                                                      br(),
                                                      h4("LOAN MAPPER - Tab",
                                                         style = "color:black;"),
                                                      tags$img(src="d2.png",height="75%", width="75%"),
                                                      br(),
                                                      br(),
                                                      h5("This tab allows the user to monitor the loan amount distribution by each state across the country. The highlighted states are the ones with most of loan amount proportion across the years 2015-2018 (those color will permanently stay either if we interact with any of the filters on this page). We can interact with the bottom right slicer to mark (circles) the countries by their loan status amount. It’s recommended to play with only two filters at a time to clearly see the amount if you close the mouse pointer to each circle.",
                                                         style = "color:black;"),
                                                      
                                                      h5("The trend graphs shown on the left help the user to monitor the loan amount across the years, with the capacity to filter by a specific state. NOTE: any changes on the filters from this tab will also filter the data on the map and consequently, changing the circles size drawn on the map.  ",
                                                         style = "color:black;"),
                                                      
                                                      br(),
                                                      h4("MODEL PERFORMANCE - Tab",
                                                         style = "color:black;"),
                                                      tags$img(src="d3.png",height="75%", width="75%"),
                                                      br(),
                                                      br(),
                                                      h5("This tab shows how the model is performing after running the machine learning algorithm “Random Forest” on the backend, over the training data and the test data. For further details on the nomenclature and metrics shown on this tab you can go to info -> Basic terminology from the top menu.",
                                                         style = "color:black;"),
                                                      
                                                      h5("Receiver Operating Characteristics (ROC): The ROC curve is marked in blue and it’s showing the true positive rate versus the false positive rate. The Area Under the Curve (which is showing 0.994) helps to have a global view of the model performance. Values near 1 or 100% tell that the model has a good performance in predicting the probability of payment of each borrower.",
                                                         style = "color:black;"),
                                                      
                                                      h5("Precision + Recall + Accuracy: Here we are showing these plots in one single view. For this specific application the selected threshold is 0.549 (54.9%) which permitted to achieve a higher precision. In our application, high precision means to capture charged off (or default) borrowers effectively, in other words, not making a mistake of incorrectly classifying a “Charged off” transaction as a “Fully Paid” transaction. Overall performance metrics are displayed on the table from the right.",
                                                         style = "color:black;"),
                                                      
                                                      h5("Feature Importance & Partial Dependency Plots: Both graphs highlight which features are important the most during the machine learning development. The difference between both, is that Partial Dependency Plots focus on the average prediction (probability of payment) across each value of the selected feature.",
                                                         style = "color:black;"),
                                                      
                                                      br(),
                                                      h4("MODEL EVALUATION - Tab",
                                                         style = "color:black;"),
                                                      
                                                      br(),
                                        
                                                      h5("This tab is focused on studying and exploring the dataset as of 2018. The user can filter the data by using the slicers of Loan Status, Term, Loan Grade, Application Type and Issued Month (for further details about each filter, you can go to “Data dictionary” from the “Info” tab.",
                                                         style = "color:black;"),
                                                      
                                                      tags$img(src="d4.png",height="65%", width="65%"),
                                                      
                                                      h5("The key features (columns) of the dataset correspond to “Probability (%)” and “loan_status”. The “Probability (%)” column shows a percentage estimated by the Machine Learning Algorithm “Random Forest”, that represents how likely is that a specific borrower (each registry) is going to pay the current loan. In other words, “what is the probability of payment for a specific borrower”. ",
                                                         style = "color:black;"),
                                                      
                                                      h5("The middle section corresponds to estimate the feature contribution (given as probability) for a specific ID on the data table. Here we can see the top 10 features that are having the higher impact on estimating the probability (%) of payment. By selecting a specific ID from the table below and clicking on the refresh bottom the results will be displayed (it takes about 30 – 40 seconds to build all relationships and update the current graph). ",
                                                         style = "color:black;"),
                                                      br(),
                                                      tags$img(src="d5.png",height="65%", width="65%"),
                                                      
                                                      br(),
                                                      br(),
                                                      h5("The size of the bars represents amount of probability contributed by a specific value:",
                                                         style = "color:black;"),
                                                      
                                                      tags$img(src="d7.png",height="40%", width="40%"),
                                                      
                                                      h5("Lastly, we can see a graph right aside from the previous section. It corresponds to the explanation of the model by a selected feature. That means, how this feature is interacting with other features in specific points. For example, the feature “int_rate” is having several up and downs in almost all the intervals:",
                                                         style = "color:black;"),
                                                      
                                                      tags$img(src="d8.png",height="25%", width="25%"),
                                                      
                                                      
                                                     style = "font-size:20px")),

                                 )#main panel bracket
                        ), #tab panel bracket
                        ###############################################.             
                        ##############Other profiles----    
                        ###############################################.
                        tabPanel("Resources", value = "others",
                                 
                                 mainPanel(
                                   h4("Useful resources for this application", style = "color:black;"),
                                   p(""),
                                   tags$ul( 
                                     #Link to GCPH
                                     tags$li(class= "li-custom", tags$a(href="https://www.r-project.org/",
                                                                        "R Project for Statistical Computing",  class="externallink")), 
                                     #Link to GCPH
                                     tags$li(class= "li-custom", tags$a(href="https://shiny.rstudio.com/",
                                                                        "Shiny Dashboard R",  class="externallink")), 
                                     #Link to Fife
                                     tags$li(class= "li-custom", tags$a(href="https://www.h2o.ai/",
                                                                        "H2O",  class="externallink")), 
                                     #Link to IS
                                     tags$li(class= "li-custom", tags$a(href="https://medium.com/@williamkoehrsen/random-forest-simple-explanation-377895a60d2d",
                                                                        "Random Forest algorithm explained",  class="externallink")), 
                                     #Link to NRS
                                     tags$li(class= "li-custom", tags$a(href="https://www.kaggle.com/", 
                                                                        "Kaggle",  class="externallink")), 
                                     #Link to stats.gov.scot
                                     tags$li(class= "li-custom", tags$a(href="https://medium.com/datadriveninvestor/what-is-machine-learning-55028d8bdd53", 
                                                                        "What is Machine Learning?",  class="externallink")), 
                                     #Link to Scottish nation
                                     tags$li(class= "li-custom", tags$a(href="https://medium.com/@MohammedS/performance-metrics-for-classification-problems-in-machine-learning-part-i-b085d432082b", 
                                                                        "Performance Metrics for Classification Problems in Machine Learning",  class="externallink"))
                                   ), #Bullet point list bracket
                                   br()
                                 ) # mainPanel bracket
                        ) #tabPanel bracket
             )# NavbarMenu bracket
             
             
  )
  
)




