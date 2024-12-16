
rm(list=ls())  #cleaning variables

####### LIBRARIES ----------------------------------------------------------------------------------------------------------------------------------------

library(readr) ; library(dplyr) ; library(lubridate) ; library(plotly) ; library(tidyverse)
library(leaflet); library(DT); library(shiny); library(rmarkdown); library(data.table); library(naniar)
library(knitr); library(repr); library(lattice); library(GGally); library(scales); library(purrr)
library(gridExtra); library(FSA); library(caTools); library(vcd); library(RJDBC); library(shinydashboard); library(shiny)
library(ggmosaic); library(ggplot2) ; library(devtools);  library(shinythemes) ; library(shinycssloaders)
library(maps); library(RColorBrewer) ; library(leaflet); library(sf) ; library(geojsonio); library(readxl)
library(h2o); library(purrr); library(dplyr); library(DALEX); library(lime) ; library(iml); library(future); library(ICEbox)
library(ingredients); library(localModel); library(knitr); library(kableExtra); library(ceterisParibus); library(ingredients); library(shinyWidgets)




######## 1. GETTING THE DATA -----------------------------------------------------------------------------------------------------------------------------

#setwd("/srv/shiny-server/LendingCompany")

data <- fread("loan.csv", sep = ",", header = TRUE)
dictionary <- fread("Dictionary.csv", sep = ",", header = TRUE)

#useful functions

'%ni%' <- Negate('%in%')

theme1 <-  theme_minimal(base_size = 8)  +  theme_grey(base_size = 8) + theme(legend.text = element_text(size = 6), legend.box.background = element_rect(fill = "transparent"), legend.background = element_rect(fill = "transparent"))  + theme_minimal(base_size = 8) + theme(panel.border = element_rect(colour = "gray85", fill=NA, size=1), legend.box.background = element_rect(fill = "transparent"), legend.background = element_rect(fill = "transparent", linetype = "transparent", color = NA), legend.box.margin = "transparent", panel.grid.major.x = element_blank()  )
theme2 <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
theme_text_border <- theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8), panel.border = element_blank(), panel.grid.minor.x  = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank() )  


####### 2. DATA CLEANING / TRANSFORMATION / MANIPULATION ---------------------------------------------------------------------------------------------------------------------------

##### DATES MAPPING #############

## issued date
data$issue_d_y <- as.integer((sub(".*-","",data$issue_d)))
a <- strsplit(data$issue_d, "[-]")  # extract months on a list
data$issue_d_m <- unlist(lapply(a, `[[`, 1))  #unlist values
data$issue_d <- paste0(data$issue_d_y,"-", data$issue_d_m)


#last payment date

validation <- names(table(data$last_pymnt_d))[2:136]
data$last_pymnt_d  <- ifelse(data$last_pymnt_d %ni% validation  , "Jan-2021", data$last_pymnt_d) ## there were some empty characters, replacing with other 
data$last_pymnt_d_y <- as.integer((sub(".*-","",data$last_pymnt_d)))
a <- strsplit(data$last_pymnt_d, "[-]")  # extract months on a list
data$last_pymnt_d_m <- unlist(lapply(a, `[[`, 1))  #unlist values
data$last_pymnt_d <- paste0(data$last_pymnt_d_y,"-", data$last_pymnt_d_m)

######## MISSING VALUES #########

missing_data <- miss_var_summary(data)
missing_data$pct_miss <- round(missing_data$pct_miss, 0)
#View(filter(missing_data, pct_miss !=0 ))  # There are some potential columns with NA values
#We have a lot of missing data. First all we need to get rid of the features with 100% of missingness, they don't add value information whatsoever: 
data <- as.data.frame(data) ; keep_feat <- subset(missing_data, pct_miss <= 99)$variable ; data_r <- data[, names(data) %in% keep_feat ]

missing_data <- subset(missing_data, pct_miss <= 99) #updated

replace_1 <- c("settlement_amount", "settlement_percentage", "settlement_term")
data_r[, names(data_r) %in% replace_1 ][is.na(data_r[, names(data_r) %in% replace_1])] <- 0
replace_2 <- c("sec_app_inq_last_6mths", "sec_app_mort_acc", "sec_app_open_acc", "sec_app_open_act_il", "sec_app_num_rev_accts",
               "sec_app_chargeoff_within_12_mths", "sec_app_collections_12_mths_ex_med")    

data_r$is_sec_app <- as.factor(ifelse(is.na(data_r$sec_app_revol_util), 0, 1))  #picking any of these would do the job
data_r[, names(data_r) %in% replace_2 ][is.na(data_r[, names(data_r) %in% replace_2])] <- 0
replace_3 <- c("open_act_il", "open_il_12m", "open_il_24m", "total_bal_il", "open_rv_12m",
               "open_rv_24m", "max_bal_bc", "inq_fi", "all_util", "open_acc_6m", "total_cu_tl", "inq_last_12m" )    

data_r[, names(data_r) %in% replace_3 ][is.na(data_r[, names(data_r) %in% replace_3])] <- 0
replace_4 <- c("num_tl_120dpd_2m", "num_rev_accts", "num_accts_ever_120_pd", "num_tl_90g_dpd_24m", "num_tl_op_past_12m", "delinq_2yrs", "open_acc",
               "num_actv_bc_tl", "num_actv_rev_tl", "num_bc_tl", "num_il_tl", "num_op_rev_tl", "num_tl_30dpd", "num_rev_tl_bal_gt_0", "pub_rec","acc_now_delinq",
               "num_bc_sats", "num_sats", "acc_open_past_24mths", "mort_acc", "pub_rec_bankruptcies", "chargeoff_within_12_mths", "tax_liens", "inq_last_6mths") 

data_r[, names(data_r) %in% replace_4 ][is.na(data_r[, names(data_r) %in% replace_4])] <- 0

replace_5 <- c("mths_since_last_record", "mths_since_recent_bc_dlq", "mths_since_last_major_derog", "mths_since_recent_revol_delinq", "mths_since_last_delinq",
               "mths_since_rcnt_il", "mths_since_recent_inq", "mths_since_recent_bc", "collections_12_mths_ex_med", "sec_app_mths_since_last_major_derog",
               "mo_sin_old_il_acct", "mo_sin_old_rev_tl_op", "mo_sin_rcnt_rev_tl_op", "mo_sin_rcnt_tl" ) 

data_r[, names(data_r) %in% replace_5 ][is.na(data_r[, names(data_r) %in% replace_5])] <- 0

replace_6 <- c("tot_coll_amt", "tot_cur_bal", "tot_hi_cred_lim", "total_rev_hi_lim", "total_il_high_credit_limit", "annual_inc", 
               "total_bal_ex_mort", "total_bc_limit", "total_acc", "revol_bal_joint", "annual_inc_joint","bc_open_to_buy", "acc_now_delinq", "delinq_amnt" ) 

data_r[, names(data_r) %in% replace_6 ][is.na(data_r[, names(data_r) %in% replace_6])] <- 0

replace_7 <- c("sec_app_revol_util", "dti_joint", "il_util", "bc_util", "pct_tl_nvr_dlq", "percent_bc_gt_75", 
               "avg_cur_bal", "revol_util", "dti" ) 

data_r[, names(data_r) %in% replace_7 ][is.na(data_r[, names(data_r) %in% replace_7])] <- 0


#t <- names(which(sapply(data_r, function(x) any(x == ""  )  ) ))
data_r$emp_title  <- ifelse(data_r$emp_title == "", "Missing", data_r$emp_title)
data_r$emp_title  <- ifelse(is.na(data_r$emp_title) , "Missing", data_r$emp_title)
data_r$title  <- ifelse(is.na(data_r$title) , "Missing", data_r$title)
data_r$title  <- ifelse(data_r$title == "", "Missing", data_r$title)
data_r$desc  <- ifelse(data_r$desc == "", "Missing", data_r$desc)
data_r$zip_code  <- ifelse(data_r$zip_code == "", "Missing", data_r$zip_code)
data_r$earliest_cr_line  <- ifelse(data_r$earliest_cr_line == "", "Missing", data_r$earliest_cr_line)
data_r$last_pymnt_d  <- ifelse(data_r$last_pymnt_d == "", "Missing", data_r$last_pymnt_d)
data_r$next_pymnt_d  <- ifelse(data_r$next_pymnt_d == "", "Missing", data_r$next_pymnt_d)
data_r$last_credit_pull_d  <- ifelse(data_r$last_credit_pull_d == "", "Missing", data_r$last_credit_pull_d)
data_r$verification_status_joint  <- ifelse(data_r$verification_status_joint == "", "Missing", data_r$verification_status_joint)
data_r$sec_app_earliest_cr_line  <- ifelse(data_r$sec_app_earliest_cr_line == "", "Missing", data_r$sec_app_earliest_cr_line)
data_r$hardship_type  <- ifelse(data_r$hardship_type == "", "Missing", data_r$hardship_type)
data_r$hardship_reason  <- ifelse(data_r$hardship_reason == "", "Missing", data_r$hardship_reason)
data_r$hardship_status  <- ifelse(data_r$hardship_status == "", "Missing", data_r$hardship_status)
data_r$hardship_start_date  <- ifelse(data_r$hardship_start_date == "", "Missing", data_r$hardship_start_date)
data_r$hardship_end_date  <- ifelse(data_r$hardship_end_date == "", "Missing", data_r$hardship_end_date)
data_r$payment_plan_start_date  <- ifelse(data_r$payment_plan_start_date == "", "Missing", data_r$payment_plan_start_date)
data_r$hardship_loan_status  <- ifelse(data_r$hardship_loan_status == "", "Missing", data_r$hardship_loan_status)

data_r$revol_bal <- as.numeric(data_r$revol_bal)

## DATA 2018 #####
data_2018 <- data_r %>% filter(issue_d_y == "2018")  #only data from 2018

data <- NULL
a <- NULL


########### 3. DASHBOARD GRAPHS  ----------------------------------------------------------------------------------------------------------------------------

temp <- as.data.table(data_r)
temp <- select(temp, issue_d_y,term,application_type,home_ownership,title, home_ownership, grade)
 


### 1. tabset Interest rates ######

temp1 <- data_r %>% select(issue_d_y, int_rate) %>% 
  group_by(issue_d_y) %>% 
  summarise(int_rate = mean(int_rate)) 

temp1 <- as.data.frame(temp1)
px <- pretty(temp1$issue_d_y)
py <- pretty(temp1$int_rate)

temp1 <- as.data.table(temp1)

t1 <- temp1 %>% 
  ggplot(aes(x=issue_d_y,y=int_rate,color= "tomato4",fill="tomato4" ) )  +geom_line() + 
  ylab("Average (%)") + 
  scale_x_continuous(breaks=px, limits=range(px)) + geom_point(size=1) + scale_y_continuous(breaks=py, limits=range(py)) +
  theme_bw() +  theme(plot.title = element_text(hjust=0.5)) + theme(legend.title=element_blank()) + theme(legend.title=element_blank()) + 
  theme_text_border + theme(legend.position = "none") +  theme(axis.title.x=element_blank())


#### 2. tabset Balance

temp2 <- data_r %>% filter(issue_d_y >= 2015) %>% select(Year = issue_d , tot_cur_bal, revol_bal, total_bal_ex_mort, total_bal_il) %>% 
  mutate (total_mort = tot_cur_bal - total_bal_ex_mort  ) %>% select(-total_bal_ex_mort) %>% group_by(Year) %>% 
  summarise(`Current` = sum(tot_cur_bal), `Revolving` = sum(revol_bal), `Installment` = sum(total_bal_il),
            `Mortgage` = sum(total_mort)) %>% reshape2::melt(id.vars = "Year", variable.name = "Account", value.name = "Amount")

temp2 <- as.data.table(temp2)

t2 <- temp2 %>% 
  ggplot(aes(x=Year,y=Amount, color=Account, text = paste0("Amount (Billion): ", round(Amount/1000000000,2), "</br></br>Year: ", Year ), group = Account ) ) +   geom_line() + 
  ylab("Total Balance $") + scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9)) + 
   theme(legend.title=element_blank()) +   theme_bw() + 
  scale_color_manual(values=c("#279989", "#E35205", "#485CC7", "#5F249F")) + scale_x_discrete(breaks = c("2015-Jan","2016-Jan","2017-Jan","2018-Jan")) + 
  theme_text_border + theme(axis.title.x=element_blank())


#### 3. Public assessment

temp3 <- as.data.table(data_r)
temp3 <- select(temp3, issue_d_y, term, application_type,issue_d_y, pub_rec, pub_rec_bankruptcies )


## 4. Interest and principal received

temp4 <- data_r %>% filter(issue_d_y >= 2015) %>%select(Year = issue_d , total_rec_prncp, total_rec_int ) %>%
  group_by(Year) %>% 
  summarise(`Principal` = sum(total_rec_prncp), `Interests` = sum(total_rec_int)) %>%
  reshape2::melt(id.vars = "Year", variable.name = "Account", value.name = "Amount")

temp4 <- as.data.table(temp4)

t4 <- temp4 %>% 
  ggplot(aes(x=Year,y=Amount,color=Account, text = paste0("Amount (Million): ", round(Amount/1000000000,2), "</br></br>Year: ", Year ), group = Account )) +
  geom_line() + ylab("Total Amount $") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) + theme1 + theme(legend.title=element_blank()) + 
  scale_color_manual(values=c("#101820", "#53565A")) + scale_x_discrete(breaks = c("2015-Jan","2016-Jan","2017-Jan","2018-Jan")) + theme_text_border +
  theme(axis.title.x=element_blank())


# 5. credit

temp_e<- as.data.table(data_2018)



tempc1_e <- temp_e %>% select(title) %>% group_by(title) %>% summarise(count = n()) %>% mutate(prop = 100 * count /nrow(data_2018) ) %>% arrange(desc(prop)) %>% top_n(n = 2)
topc1_e <- tempc1_e$title
temp_e$title <- ifelse(temp_e$title %ni% topc1_e, "Other", temp_e$title)
tempc1_e <- temp_e  %>% select(title, home_ownership, grade) %>% group_by(title, home_ownership, grade) %>% summarise(count = n()) 
tempc1_e$title <- as.factor(tempc1_e$title)
tempc1_e$grade <- as.factor(tempc1_e$grade)
tempc1_e$home_ownership <- as.factor(tempc1_e$home_ownership)
value_e <- tempc1_e  %>% select(title,home_ownership,count) %>% summarise(count = sum(count))
value_e$home_ownership <- as.factor(value_e$home_ownership)
value_e$title <- as.factor(value_e$title)
levels(value_e$title) <- c("CC Refinan.", "Consolid.", "Other")
value_e$rows_t <- 1

value_e$count <- 0
## 6 loan amount  vs interest rate

temp6 <- as.data.table(data_r)
temp6 <- select(temp6,issue_d_y,term,application_type,loan_amnt,int_rate )
  
## 7. Loan Amount Vsinterest Rate

temp7 <- as.data.table(data_r)
temp7 <- select(temp7, issue_d_y,term,application_type,grade,int_rate)

temp8 <- as.data.table(data_r)
temp8 <- select(temp8,issue_d_y,term,application_type,grade,pub_rec_bankruptcies,tax_liens,pub_rec )




### loan status

data_2018 <- as.data.table(data_2018)

temp10 <- data_2018 %>% select(loan_status) %>% group_by(loan_status) %>% summarise(count = n()) %>%
  mutate(prop = 100 * count /nrow(data_2018) ) %>% arrange(desc(prop)) 

temp10 <- as.data.table(temp10)

t10 <- ggplot(temp10, aes(x=reorder(loan_status, prop), y=prop)) + geom_bar(stat='identity', fill='steelblue3')  + 
  ylab("Proportion(%)") + xlab("") +
  theme_bw() + theme(plot.title = element_text(hjust=0.5)) + theme(legend.title=element_blank())





############# 4. MAP #####################################################################################################

#back

#back <- data_r
#data_r <- back

#states country
states = geojson_read("us-states.json", what = "sp")  #spacial class


loan_data <- data_r %>% select(issue_d_y, issue_d_m, application_type, term, zip_code, loan_status, loan_amnt)
coord <- read.csv("states.csv")
zipcodes <- read_excel("zipcodes.xlsx")
names(zipcodes)[2] <- "State_Area"

zip_code_table <- loan_data  ### copy of loan data

loan_data$zip_code <- substr(loan_data$zip_code, start = 1, stop = 3)
loan_data <- loan_data %>% group_by(issue_d_y, issue_d_m, application_type, term, zip_code, loan_status) %>% summarize(Total_Amount = sum(loan_amnt))

m1 <- loan_data
m1 <- filter(m1, zip_code %ni% "Missing")
m2 <- select(zipcodes, Prefix, State_Area)
merge1 <- merge(x = m1, y = m2, by.x = "zip_code", by.y = "Prefix", all.x = TRUE)
merge1$Total_Amount <- as.numeric(merge1$Total_Amount)

loan_data <- merge1 %>% group_by(State_Area, application_type, term, loan_status, issue_d_y, issue_d_m ) %>% summarize(Total = sum(Total_Amount))
merge2 <- merge(x = loan_data, y = coord, by.x = "State_Area", by.y = "state", all.y = TRUE)
loan_data <- merge2
loan_data$loan_status <- ifelse(loan_data$loan_status == "Does not meet the credit policy. Status:Charged Off", "Charged Off",
                                ifelse(loan_data$loan_status == "Does not meet the credit policy. Status:Fully Paid","Fully Paid", loan_data$loan_status) )


###obtaining list of zip code cities ####

zip_code_table$zip <- substr(zip_code_table$zip_code, start = 1, stop = 3)
zip_code_table <- zip_code_table %>% select(zip_code, zip) %>% filter(zip_code %ni% "Missing")
data_r <- data_r %>% filter(zip_code %ni% "Missing")
zip_code_table <- merge(x = zip_code_table, y = m2, by.x = "zip", by.y = "Prefix", all.x = TRUE)
data_r$zip_code <- zip_code_table$State_Area

#ph 1



#reactive_db = cv_cases %>% filter(date == plot_date)
reactive_db <-  loan_data    
big_states_data =  loan_data %>% select(name, Total) %>% group_by(name) %>% summarize(Total = sum(Total))
big_states_data = big_states_data[order(big_states_data$Total, decreasing = TRUE),]
big_states = big_states_data[1:10,]
big_states = big_states[order(big_states$name, decreasing = FALSE), ]

reactive_db_large = reactive_db %>% filter(name %in% big_states$name)

reactive_polygons = states[states$name %in% reactive_db_large$name, ]  ## only polygons

#reactive_db_last24h = cv_cases %>% filter(date == plot_date & new_cases>0)
reactive_db_last24h =  loan_data %>% filter(Total >0)

current = "#6F7BD4"
fully = "#279989"
default = "#F4364C"
charge = "#F4364C"
late1 = "#FF9800"
late2 = "#E56916"

#bins = c(0,500000, 1000000,10000000, 100000000,500000000,1000000000, 2500000000)
bins = c(1000000000, 2000000000,3000000000, 4000000000,5000000000)

#cv_pal <- colorBin("Blues", domain = cv_large_countries$per100k, bins = bins)   #defines color of the fill
cv_pal <- colorBin("Oranges", domain = big_states$Total, bins = bins)   #defines color of the fill


plot_map <- states[states$name %in% loan_data$name, ]  #essencial map

#formula to map data on map
reactive_status <- reshape2::dcast(reactive_db_last24h, 
                                   State_Area + issue_d_y  + issue_d_m +  application_type + term + latitude + longitude + name ~ loan_status, 
                                   value.var = "Total", fun.aggregate = sum) %>% filter(issue_d_y  %in% c(2015,2016,2017,2018) ) 

### function must start here

new_reactive <- reactive_status %>% select(latitude,longitude,name, Current, `Fully Paid`, Default, `Charged Off`, `Late (16-30 days)`, `Late (31-120 days)`) %>% 
  group_by(name, latitude, longitude) %>% summarize(Current = sum(Current), `Fully Paid` = sum(`Fully Paid`), Default = sum(Default),
                                                    `Charged Off` = sum(`Charged Off`), `Late (16-30 days)` = sum(`Late (16-30 days)`), `Late (31-120 days)` = sum(`Late (31-120 days)`), .groups = "drop" )  %>% ungroup()

basemap = leaflet(plot_map) %>% 
  addTiles() %>%  
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Current", "Fully Paid", "Default", "Charged Off", "Late (16-30 days)", "Late (31-120 days)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  #hideGroup(c("Current", "Fully Paid", "Default", "Charged Off",  "Late (16-30 days)", "Late (31-120 days)"))  %>%
  hideGroup(c("Current", "Fully Paid", "Default", "Charged Off",  "Late (16-30 days)", "Late (31-120 days)"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  fitBounds(~-100,-50,~80,80) %>%
  addLegend("bottomright", pal = cv_pal, values = ~big_states$Total,
            title = "<small>  Overall Loan Amount 2015-2018 ($) </small>") %>%  clearShapes() %>% 
  #fills the country #according to fill color count
  addPolygons(data = reactive_polygons   , stroke = TRUE, color = "#808080", weight = 1,
              smoothFactor = 0.3, fillOpacity = 0.15, fillColor = ~cv_pal(big_states$Total)) %>% 
  
  addCircles(data = new_reactive, lat = ~ latitude, lng = ~ longitude, 
             weight = 1, radius = ~(Current)^(1/4) * 900, 
             fillOpacity = 0.15, color = current, group = "Current",
             label = sprintf("<strong>%s</strong><br/>Current Total Amount (Million $): %g<br/>", new_reactive$name,  round(new_reactive$Current/1000000,1)) %>% 
               lapply(htmltools::HTML), labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = current),
                 textsize = "15px", direction = "auto")) %>%
  
  addCircles(data = new_reactive , lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(`Fully Paid`)^(1/4) * 900, 
             fillOpacity = 0.15, color = fully, group = "Fully Paid",
             label = sprintf("<strong>%s</strong><br/>Fully Paid Total Amount (Million $): %g<br/>", new_reactive$name, round(new_reactive$`Fully Paid`/1000000,1) ) %>% 
               lapply(htmltools::HTML), labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px", "color" =  fully ),
                 textsize = "15px", direction = "auto")) %>%
  
  addCircles(data = new_reactive , lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Default)^(1/4) * 5000, 
             fillOpacity = 0.15, color = default, group = "Default",
             label = sprintf("<strong>%s</strong><br/>Default Total Amount (Thousand $): %g<br/>", new_reactive$name, round(new_reactive$Default/1000),1) %>% 
               lapply(htmltools::HTML), labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px", "color" =  default ),
                 textsize = "15px", direction = "auto")) %>%
  
  addCircles(data = new_reactive , lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(`Charged Off`)^(1/4) * 800, 
             fillOpacity = 0.15, color = charge, group = "Charged Off",
             label = sprintf("<strong>%s</strong><br/>Charged Off Total Amount (Million $): %g<br/>", new_reactive$name, round(new_reactive$`Charged Off`/1000000,1 ) ) %>% 
               lapply(htmltools::HTML), labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px", "color" =  charge ),
                 textsize = "15px", direction = "auto")) %>%
  
  
  addCircles(data = new_reactive , lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(`Late (16-30 days)`)^(1/4) * 1000, 
             fillOpacity = 0.15, color = late1, group = "Late (16-30 days)",
             label = sprintf("<strong>%s</strong><br/> Late (16-30 days) Total Amount (Million $): %g<br/>", new_reactive$name, round(new_reactive$`Late (16-30 days)`/1000000,1)  ) %>% 
               lapply(htmltools::HTML), labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px", "color" =  late1 ),
                 textsize = "15px", direction = "auto")) %>%
  
  addCircles(data = new_reactive , lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(`Late (31-120 days)`)^(1/4) * 1000, 
             fillOpacity = 0.15, color = late2, group = "Late (31-120 days)",
             label = sprintf("<strong>%s</strong><br/>Late (31-120 days) Total Amount (Million $): %g<br/>", new_reactive$name, round(new_reactive$`Late (31-120 days)`/1000000,1 )) %>% 
               lapply(htmltools::HTML), labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px", "color" =  late2 ),
                 textsize = "15px", direction = "auto"))



##################### 5. DATA PREPARATION -------------------------------------------------------------------------------------------------


data_m <- data_r %>% filter(loan_status %in% c("Charged Off", "Fully Paid", "Default", "Does not meet the credit policy. Status:Charged Off", 
                                               "Does not meet the credit policy. Status:Fully Paid" ))

data_m$target <- as.factor(ifelse(data_m$loan_status %in% c("Fully Paid", 
                                                            "Does not meet the credit policy. Status:Fully Paid"),
                                  0, 1))



#"Next variables are categorical"
data_m$policy_code <- as.character(data_m$policy_code)
num <- data_m[, sapply(data_m, class) %in% c("numeric", "integer")]

#"Attaching feature"
data_mn <- num  # new data frame of numerical data
data_mn$target <- data_m$target  ## attaching target feature
data_mn$date <- data_m$issue_d
data_mn$year <- data_mn$issue_d_y

a <- strsplit(data_mn$date, "[-]")  # extract months on a list
data_mn$month <- unlist(lapply(a, `[[`, 1))  #unlist values
#data_mn$date <- paste0(data_mn$year,"-", data_mn$month)

#"Taking Data from 2015 to 2018"

data_mn <- as.data.frame(data_mn)  #nuevos

data_simple <- data_mn %>% filter(year %in% c(2015, 2016,2017, 2018))

#"High Correlation Features" (after deep analysis)

high_corr <- c("funded_amnt","funded_amnt_inv","installment","total_pymnt_inv","num_sats","num_op_rev_tl","total_rev_hi_lim",
               "revol_util","out_prncp_inv","collection_recovery_fee","avg_cur_bal","tot_hi_cred_lim","open_il_12m","open_rv_12m",
               "il_util","total_bc_limit","settlement_percentage","num_actv_rev_tl","num_rev_tl_bal_gt_0","num_rev_accts",
               "num_op_rev_tl","total_il_high_credit_limit","sec_app_num_rev_accts")

data_simple_v2 <-  data_simple[, names(data_simple) %ni% high_corr]   #new numerical with created categories

#####"ANALIZING CATEGORICAL DATA"

cat <- data_m[, sapply(data_m, class) %in% c("factor", "character")]
cat$target <- data_mn$target
cat$year <- data_mn$year
cat$date <- data_mn$date
data_simple_c <- cat %>% filter(year %in% c(2015, 2016,2017, 2018))

# list of excluded categories No.1

exc_cat <- c( "loan_status", "year", "date", "desc", "emp_title", "pymnt_plan", "policy_code", "verification_status_joint",
              "hardship_flag", "hardship_type","hardship_reason", "hardship_status", "hardship_start_date", "hardship_end_date",
              "hardship_loan_status", "payment_plan_start_date", "debt_settlement_flag_date", "settlement_status", "settlement_date",
              "issue_d_m", "last_pymnt_d_m")

data_simple_c <- data_simple_c[, names(data_simple_c) %ni% exc_cat]

#categorical conversions

data_simple_c$purpose <- ifelse(data_simple_c$purpose %ni% c("credit_card", "debt_consolidation", "home_improvement", "house", "car" ),
                                "Other", data_simple_c$purpose)

data_simple_c$title <- ifelse(data_simple_c$title %ni% c("Debt consolidation", "Credit card refinancing", "home_improvement", 
                                                         "Major purchase", "Medical expenses",
                                                         "Car financing",  "Business"),
                              "Other", data_simple_c$title )


data_simple_c$ID <- seq.int(nrow(data_simple_c))

data_simple_v2$target <- NULL
data_simple_v2$month <- NULL
data_simple_v2$last_pymnt_d_y <- NULL
data_simple_v2$ID <- NULL
data_simple_v2$year <- NULL
data_simple_c$ID <- NULL

data_simple_v2$ID <- seq.int(nrow(data_simple_v2))   #making indices
data_simple_c$ID <- seq.int(nrow(data_simple_c))

#joining tables

a <- data.table(data_simple_v2, key = "ID") 
b <- data.table(data_simple_c, key = "ID") 
data_all <- a[b] 
data_all <- as.data.frame(data_all)
data_all$ID <- NULL
data_all <- filter(data_all, data_all$issue_d_y >= 2015)
data_all$issue_d_y <- NULL

########### 6. MACHINE LEARNING MODEL -----------------------------------------------------------------------

good <- colnames(data_all)
good <- good[good %ni% c("sec_app_earliest_cr_line","earliest_cr_line", "next_pymnt_d", "zip_code")]
all_features <- c(good,"issue_d_y", "loan_status")
all_features <- all_features[all_features %ni% c("date")]

#### original data ####

#data_org <- data_r[, names(data_r) %in% all_features]
data_org <- data_r %>% filter(issue_d_y >= 2018)
data_org$issue_d_m <- as.factor(data_org$issue_d_m)
data_org$issue_d_m = factor(data_org$issue_d_m,
                            levels = c(
                              "Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec") )

data_org$ID  <- rownames(data_org)
data_org$ID <- as.numeric(data_org$ID)


#data_org <- filter(data_org, issue_d_y >= 2015)
data_org$purpose <- ifelse(data_org$purpose %ni% c("credit_card", "debt_consolidation", "home_improvement", "house", "car" ),
                           "Other", data_org$purpose)

data_org$title <- ifelse(data_org$title %ni% c("Debt consolidation", "Credit card refinancing", "home_improvement", 
                                               "Major purchase", "Medical expenses",
                                               "Car financing",  "Business"),
                         "Other", data_org$title )



####### Train data for the model

train_data <- data_all[, names(data_all) %in% good]
train_data$date <- NULL
train_data$term <- as.factor(train_data$term)
train_data$grade <- as.factor(train_data$grade)
train_data$term <- as.factor(train_data$term)
train_data$grade <- as.factor(train_data$grade)
train_data$verification_status <- as.factor(train_data$verification_status)
train_data$issue_d <- as.factor(train_data$issue_d)
train_data$purpose <- as.factor(train_data$purpose)
train_data$last_pymnt_d <- as.factor(train_data$last_pymnt_d)
train_data$emp_length <- as.factor(train_data$emp_length)
train_data$loan_amnt <- as.numeric(train_data$loan_amnt)
train_data %>% map_if(is.character, as.factor) %>% as.data.frame() -> train_data


###################

data_simple_c <- NULL
a <- NULL
b  <- NULL
cat  <- NULL
data_simple_v2 <- NULL
data_all  <- NULL
data_simple  <- NULL
num  <- NULL
data_mn  <- NULL
data_m <- NULL

merge2   <- NULL           
merge1  <- NULL                
m1  <- NULL


##################


#########H2O #################################


h2o.init(ip = "localhost", nthreads = 8, max_mem_size = "30g", port = 55322)


### level categorization

levels(train_data$target) <- c(0, 1)
h_train_data <- as.h2o(train_data) 
h_train_data$target <- as.factor(h_train_data$target)

#Train and Test data

h_split <- h2o.splitFrame(h_train_data, ratios = 0.80, seed = 1234)

train_h2o <- h_split[[1]] # 80% for modelling
test_h2o <- h_split[[2]] # 20% for evaluation

y <- "target"
x <- colnames(train_data)[1:93]

### MODEL 
#Train & Cross-validate a RF
best_model <- h2o.randomForest(x = x, y = y, balance_classes = TRUE,
                              training_frame = train_h2o, validation_frame = test_h2o,
                             ntrees = 5,
                            nfolds = 10,
                           fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                         seed = 1234)

######## MODEL METRICS: TRAINNING SET

perf_t <- h2o.performance(best_model, train_h2o) 
metrics_fit_t <- as.data.frame(h2o.metric(perf_t))
saveRDS(metrics_fit_t, "./metrics_fit_t.rds")

perf_te <- h2o.performance(best_model, test_h2o) 
metrics_fit_te <- as.data.frame(h2o.metric(perf_te))
saveRDS(metrics_fit_te, "./metrics_fit_te.rds")


#####

metrics_fit_t <- readRDS("./metrics_fit_t.rds")
metrics_fit_te <- readRDS("./metrics_fit_te.rds")

perf <- h2o.performance(best_model, train_h2o) 
auc_t <- round(h2o.auc(best_model, train = TRUE),3)
auc_te <- round(h2o.auc(best_model, valid = TRUE),3)
aucpr_t <- round(h2o.aucpr(best_model, train = TRUE),3)
aucpr_te <- round(h2o.aucpr(best_model, valid = TRUE),3)


#### GRAPHS

##### ROC CURVE #shiny1


#train

a_t <- list(
  x = 0.4,
  y = 0.6,
  text = paste0("AUC (Area Under The Curve): ", auc_t),
  xref = "x",
  yref = "y",
  showarrow = FALSE,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

a_te <- list(
  x = 0.3,
  y = 0.6,
  text = paste0("AUC (Area Under The Curve): ", auc_te),
  xref = "x",
  yref = "y",
  showarrow = FALSE,
  arrowhead = 7,
  ax = 20,
  ay = -40
)


ay <- list( title = "True Positive Rate (TPR)", tickfont = list(size = 8, color = "black"),
            titlefont = list(size = 10)); ax <- list( title = "False Positive Rate (FPR)", tickfont = list(size = 8, color = "black"),
                                                      titlefont = list(size = 10))


#train
roc_t <- metrics_fit_t %>% select(fpr, tpr)
new_d <- data.frame(0.00, 0.00 ) 
names(new_d) <- c("fpr", "tpr")
roc_t <- new_d %>% rbind(roc_t)

#test

roc_te <- metrics_fit_te %>% select(fpr, tpr)
new_d <- data.frame(0.00, 0.00 ) 
names(new_d) <- c("fpr", "tpr")
roc_te <- new_d %>% rbind(roc_te)



### PRECISION AND RECALL, Accuracy

#train

by <- list( title = "Rate", tickfont = list(size = 8),
            titlefont = list(size = 10)); bx <- list( title = "Threshold", tickfont = list(size = 8),
                                                      titlefont = list(size = 10))

p_r_a_t <- metrics_fit_t %>% select(threshold, precision, recall, accuracy) %>% reshape2::melt(id.var = "threshold")
p_r_a_t_plot <- ggplot(p_r_a_t, aes(x=threshold, y=value, col=variable, group = variable,
                                    text = paste0("Value(%): ", (100 * round(value,2)), "</br></br>Variable: ", variable,
                                                  "</br>Threshold(%): ", (100 * round(threshold,2)))
)) + geom_line()  + theme_minimal(base_size = 8)  + 
  scale_color_manual(values = c("#00966C", "#10069F", "#DA291C"))  + theme(legend.title=element_blank()) + 
  labs(y="Rate", x = "Threshold") +  theme(plot.title = element_text(hjust=0.5, size = 10, face = "bold"))


#test

p_r_a_te <- metrics_fit_te %>% select(threshold, precision, recall, accuracy) %>% reshape2::melt(id.var = "threshold")
p_r_a_te_plot <- ggplot(p_r_a_te, aes(x=threshold, y=value, col=variable, group = variable,
                                      text = paste0("Value(%): ", (100 * round(value,2)), "</br></br>Variable: ", variable,
                                                    "</br>Threshold(%): ", (100 * round(threshold,2)))
)) + geom_line()  + theme_minimal(base_size = 8)  + 
  scale_color_manual(values = c("#00966C", "#10069F", "#DA291C"))  + theme(legend.title=element_blank()) + 
  labs(y="Rate", x = "Threshold") +  theme(plot.title = element_text(hjust=0.5, size = 10, face = "bold"))


### PREDICTIONS

#Train
pred_t <- h2o.predict(object = best_model, train_h2o) 
pred_train <- pred_t
my_th <- 0.1969974  
pred_train$prediction <- ifelse(pred_train[, "p1"] >= my_th ,1, 0)
pred_train <- as.data.frame(pred_train)

#test
pred <- h2o.predict(object = best_model, test_h2o) 
pred_test <- pred
my_th <- 0.1969974  
pred_test$prediction <- ifelse(pred_test[, "p1"] >= my_th,1, 0)
pred_test <- as.data.frame(pred_test)

#data_org


pred <- h2o.predict(object = best_model, as.h2o(data_org)) 
pred_org <- pred
my_th <- 0.55    #0.1969974
pred_org$prediction <- ifelse(pred_org[, "p0"] >= my_th,0, 1)
pred_org <- as.data.frame(pred_org)
pred_org$p0 <- round(100 * pred_org$p0,1)
data_org$Prob <- pred_org$p0
data_org$Prediction <- pred_org$prediction



# Overall Metrics Training Set
metrics_th <- metrics_fit_t %>% filter(threshold <= 0.55)

metrics_th <- metrics_th[1,]
metrics_th_1 <- metrics_th %>% select(threshold, f2, accuracy, specificity, precision, recall, tpr, tnr, fpr, fnr) %>% sapply(function(x) round(x,3))

metrics_t <- reshape2::melt(metrics_th_1, variable.name = "Metric", value.name = "Rate (Training)")


# Overall Metrics Test Set
metrics_teh <- metrics_fit_te %>% filter(threshold <= 0.55)
metrics_teh <- metrics_teh[1,]
metrics_teh_1 <- metrics_teh %>% select(threshold, f2, accuracy, specificity, precision, recall, tpr, tnr, fpr, fnr) %>% sapply(function(x) round(x,3))

metrics_te <- reshape2::melt(metrics_teh_1, variable.name = "Metric", value.name = "Rate (Test)")

metrics_total <- cbind(metrics_t, metrics_te)


############

vimp_df <-  h2o.varimp(best_model) %>% as.data.frame()
vimp_df <- vimp_df %>% arrange(-desc(scaled_importance)) %>%  top_n(25, wt = scaled_importance ) 
vimp_df$scaled_importance <- round(vimp_df$scaled_importance * 100,0)


cy <- list( title = "", tickfont = list(size = 8, color = "black"),
            titlefont = list(size = 10)); cx <- list( title = "Feature Contribution (%)", tickfont = list(size = 8, color = "black"),
                                                      titlefont = list(size = 10))


colfunc <- colorRampPalette(c("#6CACE4", "#002D72"))
pal_ <- colfunc(25)



# Descriptive Machine Learning EXplanations (DALEX)

# Custom Predict Function

custom_predict <- function(model, newdata) {
  newdata_h2o <- as.h2o(newdata)
  res <- as.data.frame(h2o.predict(model, newdata_h2o))
  return( as.numeric(res$p0)) # round the probabil
}





custom_pred <- function(object, newdata)  {
  
  results <- as.vector(h2o.predict(object, as.h2o(newdata)))
  return(results)
}


features <- x  #features to analize

#converting to dataframes
test_data <- as.data.frame(test_h2o)
test_data$target <- as.vector(as.numeric(as.character(test_data$target)))
train_data <- as.data.frame(train_h2o)
train_data$target <- as.vector(as.numeric(as.character(train_data$target)))

#### Converting next variables to numeric

train_data$loan_amnt <- as.numeric(train_data$loan_amnt )
train_data$settlement_term <- as.numeric(train_data$settlement_term)
train_data$inq_last_6mths <- as.numeric(train_data$inq_last_6mths)
train_data$open_rv_24m <- as.numeric(train_data$open_rv_24m)
train_data$all_util  <- as.numeric(train_data$all_util )
###

#custom_predict(best_model, test_data) %>% head()

explainer_RF <- DALEX::explain(model = best_model, 
                               data = train_data[, features],
                               y = train_data$target ,
                               predict_function = custom_predict,
                               label = "H2O RF Model")



num_vars <- c("last_pymnt_amnt","total_rec_prncp", "recoveries", "total_pymnt", "loan_amnt", "total_rec_int", "int_rate",
              "settlement_amount", "settlement_term", "inq_last_6mths","open_rv_24m", "total_rec_late_fee", "dti", "all_util")

cat_vars <- c("sub_grade", "debt_settlement_flag", "term", "home_ownership", "emp_length", "verification_status", "grade", "addr_state",
              "last_credit_pull_d", "last_pymnt_d", "issue_d" )



all_vars <- c(num_vars,cat_vars)


effect_list <- list()



var_effects_num <- readRDS( "./var_effects_num.rds")
var_effects_cat <- readRDS( "./var_effects_cat.rds")






new_pred <- data_org[1,]

############# CETERIS PARIBUS (What-If Plots.) ################

x <- colnames(train_data)[1:93]
features <- as.data.frame(test_h2o) %>% select(-target)



predictor.rf <- Predictor$new(
  model = best_model, 
  data = train_data[,x], 
  y = train_data$target, 
  predict.fun = custom_predict, type = "prob"
)


####################### SHAPLEY

new_pred <- data_org[1,]


fy <- list( title = "", tickfont = list(size = 8),
            titlefont = list(size = 8)); fx <- list( title = "Probability contribution", tickfont = list(size = 8),
                                                     titlefont = list(size = 8))


###### ICE PLOTS

# restoring Object

data_ice_curves <- readRDS( "./ice/data_ice_curves.rds")
data_ice_obs <-    readRDS( "./ice/data_ice_obs.rds")
data_ice_pdp <-    readRDS( "./ice/data_ice_pdp.rds")

data_c_ice <- readRDS("./ice/data_c_ice.rds")
data_c_observations <- readRDS("./ice/data_c_observations.rds")
data_d_pdp <- readRDS("./ice/data_d_pdp.rds")

data_sd_deriv <- readRDS("./ice/data_sd_deriv.rds")













