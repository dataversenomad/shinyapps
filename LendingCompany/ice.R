
#rm(list=ls())  #cleaning variables

####### LIBRARIES ----------------------------------------------------------------------------------------------------------------------------------------

library(readr) ; library(dplyr) ; library(lubridate) ; library(plotly) ; library(tidyverse)
library(leaflet); library(DT); library(shiny); library(rmarkdown); library(data.table); library(naniar)
library(knitr); library(repr); library(lattice); library(scales); library(purrr)
library(gridExtra); library(FSA); library(caTools); library(vcd); library(RJDBC); library(shinydashboard); library(shiny)
library(ggmosaic); library(ggplot2) ;  library(shinythemes) ; library(shinycssloaders)
library(maps); library(RColorBrewer) ; library(leaflet); library(sf) ; library(geojsonio); library(readxl)
library(h2o); library(purrr); library(dplyr); library(DALEX); library(lime) ; library(iml); library(future); library(ICEbox)
library(ingredients); library(localModel); library(knitr); library(kableExtra); library(ceterisParibus); library(ingredients)


######## 1. GETTING THE DATA -----------------------------------------------------------------------------------------------------------------------------

#setwd("/srv/shiny-server/LendingClub/")

data <- fread("loan.csv", sep = ",", header = TRUE)


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
#data_mn$issue_d <- data_m$issue_d
#data_mn$year <- as.integer((sub(".*-","",data_mn$issue_d)))
a <- strsplit(data_mn$date, "[-]")  # extract months on a list
data_mn$month <- unlist(lapply(a, `[[`, 1))  #unlist values
#data_mn$date <- paste0(data_mn$year,"-", data_mn$month)

#"Taking Data from 2015 to 2018"

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
train_data$inq_last_6mths <- as.numeric(train_data$inq_last_6mths)
train_data$open_rv_24m <- as.numeric(train_data$open_rv_24m)
train_data$all_util <- as.numeric(train_data$all_util)

train_data %>% map_if(is.character, as.factor) %>% as.data.frame() -> train_data




########## H2O #################################


h2o.init(ip = "localhost", nthreads = 8, max_mem_size = "30g")


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


#custom_predict(best_model, test_data) %>% head()

explainer_RF <- DALEX::explain(model = best_model, 
                               data = train_data[, features],
                               y = train_data$target ,
                               predict_function = custom_predict,
                               label = "H2O RF Model")

##### VARIABLE EFFECTS (Partial Dependency), can be also:  accumulated_dependency

num_vars <- c("last_pymnt_amnt","total_rec_prncp", "recoveries", "total_pymnt", "loan_amnt", "total_rec_int", "int_rate",
              "settlement_amount", "settlement_term", "inq_last_6mths","open_rv_24m", "total_rec_late_fee", "dti", "all_util")




XX <- train_data
XX$issue_d <- as.character(XX$issue_d)
XX <- filter(XX, grepl(c("2018"), issue_d, fixed = TRUE))
#XX <- filter(XX, grepl(c("Sep"), issue_d, fixed = TRUE)
 #            | grepl(c("Oct"), issue_d, fixed = TRUE)
  #           | grepl(c("Nov"), issue_d, fixed = TRUE)
   #         | grepl (c("Aug"), issue_d, fixed = TRUE)
    #        )

regions <- names(table(XX$issue_d))

x <- colnames(train_data)[1:93]

### IML OBJECT

predictor.rf <- Predictor$new(
  model = best_model, 
  data = train_data[,x], 
  y = train_data$target, 
  predict.fun = custom_predict, type = "prob"
)



i = 1
j = 1

data_ice_curves <- list()
data_ice_obs <- list()
data_ice_pdp <- list()
data_dice_curves <- list()
data_c_ice <- list()
data_c_observations <- list()
data_d_pdp <- list()
data_sd_deriv <- list()

for(i in 1:length(num_vars) )  {
  
  variable_selected <- num_vars[i]
  bhd.ice = ice(object = explainer_RF, X = XX , y = XX$target, predictor = variable_selected,
              frac_to_build = 0.25, logodds = FALSE) 

  predictor = bhd.ice$predictor
  
  data_ice_curves[[i]] <- bhd.ice$ice_curves %>% as.data.frame() %>% mutate(obs_id = rownames(bhd.ice$Xice) )  %>%  
  gather(key ="value", value = "y", -obs_id) %>% mutate(feature = predictor) 

  data_ice_obs[[i]] <- bhd.ice$xj %>%
  as.data.frame() %>%
  rename(value = !!sym(".")) %>% 
  mutate(y = bhd.ice$actual_prediction,
         obs_id = rownames(bhd.ice$Xice))  %>% mutate(feature = predictor)

  data_ice_pdp[[i]] <- bhd.ice$pdp %>%
  as.data.frame() %>%
  rownames_to_column(var = "value" ) %>%
  rename(y := !!sym(".")) %>%  mutate(feature = predictor)
  
  data_ice_curves[[i]][2]$value <- as.numeric(data_ice_curves[[i]][2]$value )
  data_ice_curves[[i]][3]$y <- as.numeric(data_ice_curves[[i]][3]$y)

  data_ice_obs[[i]][1]$value <- as.numeric(data_ice_obs[[i]][1]$value) 
  data_ice_obs[[i]][2]$y <- as.numeric(data_ice_obs[[i]][2]$y)

  data_ice_pdp[[i]][1]$value <- as.numeric(data_ice_pdp[[i]][1]$value)
  data_ice_pdp[[i]][2]$y <- as.numeric(data_ice_pdp[[i]][2]$y)
  
  #plot <- ggplot(data = data_ice_curves[[i]], aes(x = value , y = y)) +  
  #  geom_point(data=data_ice_obs[[i]], aes(x = value, y=y),
   #            colour = "black", pch = 21, fill = "gray20") +
  #  geom_line(data = data_ice_pdp[[i]], aes(x = value, y = y),
  #            colour = "red", size = 1.2) +
  #  labs(title = paste("Curvas ICE:", predictor)) +
  #  theme_bw() +
  #  theme(legend.position = "bottom")
  
  #ggplotly(plot)
  
  
  ##DICE

  bhd.dice <- dice(ice_obj = bhd.ice)

  data_dice <- bhd.dice$d_ice_curves %>% as.data.frame() %>% mutate(obs_id = rownames(bhd.dice$Xice)) %>%
    gather(key = "value", value = "y", -obs_id) %>% mutate(feature = predictor) 

  min_by_curve <- data_ice_curves[[i]] %>% as.data.frame() %>% 
    group_by(obs_id) %>% 
    summarize(minum = y[which.min(value)])
  
  data_c_ice[[i]] <- data_ice_curves[[i]]  %>%
  left_join(min_by_curve, by = "obs_id") %>%
  mutate(y1 = y - minum) %>% mutate(y_var = data_dice$y)
 
  data_c_observations[[i]] <- bhd.dice$xj %>%
  as.data.frame() %>%
  rename(value = !!sym(".")) %>%
  mutate(y = bhd.dice$actual_deriv,
  obs_id = rownames(bhd.dice$Xice)) %>% mutate(feature = predictor) 
  
  data_d_pdp[[i]] <- data.frame(y = bhd.dice$dpdp) %>%
  mutate(value = bhd.dice$gridpts) %>% mutate(feature = predictor) 
  
  #plot_dice <- ggplot(data = data_c_ice[[i]], aes(x = value, y = y_var)) +
   # geom_path(aes(group = obs_id ),
    #          alpha = 0.5, col = "blue") +
    #geom_point(data=data_c_observations[[i]], aes(x= value, y=y),
    #           colour = "black", pch = 21, fill = "gray20") +
    #geom_line(data = data_d_pdp[[i]], aes(x = value, y = y),
    #          colour = "red", size = 1.2) +
    #labs(title = paste("Curvas ICE centradas:", predictor)) +
    #theme_bw() +
    #theme(legend.position = "bottom")
  
  #ggplotly(plot_dice)

  ### STANDARD DEVIATION
  
    data_sd_deriv[[i]] <- data.frame(sd = bhd.dice$sd_deriv) %>%
    mutate(value = bhd.dice$gridpts) %>% mutate(feature = predictor) 
  
    
}

#ICE
data_ice_curves <- do.call(rbind, data_ice_curves)
data_ice_obs <- do.call(rbind, data_ice_obs)
data_ice_pdp <- do.call(rbind, data_ice_pdp)

saveRDS(data_ice_curves, "./ice/data_ice_curves2.rds")
saveRDS(data_ice_obs, "./ice/data_ice_obs2.rds")
saveRDS(data_ice_pdp, "./ice/data_ice_pdp2.rds")


#DICE
data_c_ice <- do.call(rbind, data_c_ice)
data_c_observations <- do.call(rbind, data_c_observations)
data_d_pdp <- do.call(rbind, data_d_pdp)

saveRDS(data_c_ice, "./ice/data_c_ice2.rds")
saveRDS(data_c_observations, "./ice/data_c_observations2.rds")
saveRDS(data_d_pdp, "./ice/data_d_pdp2.rds")

##SD
data_sd_deriv <- do.call(rbind, data_sd_deriv)
saveRDS(data_sd_deriv, "./ice/data_sd_deriv2.rds")






