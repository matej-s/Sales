#' ## 1 Introduction  
#' 
#' The aim of the project is to develop a system that predicts  
#' the success of sales by using existing company sales data. 
#'   
#' ## 2 Method and Analysis  
#' ### 2.1 Download Data and Generate Data Sets  
# to find total processing time
startTime_ALL_2 <- Sys.time()

# Install packages and call library for the project
inst_pack_load_lib <- function(need_pkg)
{ install_pkg <- need_pkg[!(need_pkg %in% installed.packages()[,"Package"])]
  if(length(install_pkg)) 
  { install.packages(install_pkg,  repos = "http://cran.us.r-project.org")}
  for(package_name in need_pkg)
  {    library(package_name,character.only=TRUE,quietly=TRUE)}
}

# required packages list  
need_pkg <- c("tidyverse", "jjb", "caret" , "rpart",  "e1071", 
              "kableExtra", "reshape2", "ggcorrplot", "knitr" , "gridExtra",  
              "ggridges", "ggplot2",  "gtable", "grid", "egg" , 
              "lemon",  "ggpubr", "huxtable", "scales", "ggpubr", 
              "naivebayes" , "fastAdaboost",  "ada", "precrec")

# install and load missing packages
inst_pack_load_lib(need_pkg);


# make directory dataset to save data set 
mkdir("dataset")
# make directory figs to save figures
mkdir("figs")

# Download data
# WA_Fn-UseC_-Sales-Win-Loss dataset
# project location on GitHub 
# https://github.com/matej-s/Sales
# link used to download data 
# "https://raw.githubusercontent.com/matej-s/Sales/
#main/dataset/WA_Fn-UseC_-Sales-Win-Loss.csv.zip"
zip_url <- "https://raw.githubusercontent.com/matej-s/Sales/
main/dataset/WA_Fn-UseC_-Sales-Win-Loss.csv.zip"
zip_file <- "WA_Fn-UseC_-Sales-Win-Loss.csv.zip"
zip_dest <- as.character(paste(getwd(), zip_file, sep = "/dataset/"))
download.file(zip_url, destfile = zip_dest)
#unzip in  working directory
unzip(zip_dest)

# move unzip data from working directory to /dataeet project directory
path_to_original_file <- file.path(getwd(), "WA_Fn-UseC_-Sales-Win-Loss.csv")
path_to_move_to <- file.path(getwd(), "dataset/WA_Fn-UseC_-Sales-Win-Loss.csv")
file.copy(from = path_to_original_file, to  = path_to_move_to)
file.remove(path_to_original_file)

# Dataset
# load sales_dataset with downloaded data 
sales_dataset <- read_csv(path_to_move_to)

#' ### 2.1.1 Initial Exploration  
#Initial data exploration 
str(sales_dataset)
dim(sales_dataset)
head(sales_dataset)
glimpse(sales_dataset)
# this data set  covers sales activities

#is there empty data 
sum(is.na(sales_dataset))
#[1] 0
#no empty data in data set

#is there duplicate Ids - Opportunity Number
duplicated(sales_dataset$`Opportunity Number`)
no_unique <- sales_dataset$`OpportunityNumber`[
                    duplicated(sales_dataset$`Opportunity Number`)]
length(no_unique)
#[1] 196
#there are 196 duplicates 

#Remove duplicated rows on Opportunity Number
sales_dataset <- sales_dataset %>% 
                 distinct(sales_dataset$`Opportunity Number`, .keep_all = TRUE)
nrow(sales_dataset)
#[1] 77829

# data preparation - column names, column format

# make column names without spaces
names(sales_dataset) <- make.names(names(sales_dataset))

# remove duplicate columns sales_dataset$`Opportunity Number`
sales_dataset = subset(sales_dataset, select = 
                    -c(Opportunity.Number, sales_dataset..Opportunity.Number.)) 
str(sales_dataset)

# check column type and format columns
levels(as.factor(sales_dataset$Supplies.Subgroup))
# [1] "Batteries & Accessories" "Car Electronics"         "Exterior Accessories"   
# [4] "Garage & Car Care"       "Interior Accessories"    "Motorcycle Parts"       
# [7] "Performance Parts"       "Replacement Parts"       "Shelters & RV"          
#[10] "Tires & Wheels"          "Towing & Hitches"  

# make as factor 
sales_dataset$Supplies.Subgroup=factor(sales_dataset$Supplies.Subgroup)

levels(as.factor(sales_dataset$Supplies.Group))
#[1] "Car Accessories"        "Car Electronics"        "Performance & Non-auto"
#[4] "Tires & Wheels"

# make as factor
sales_dataset$Supplies.Group=factor(sales_dataset$Supplies.Group)

levels(as.factor(sales_dataset$Region))
#[1] "Mid-Atlantic" "Midwest"      "Northeast"    "Northwest"    "Pacific"     
#[6] "Southeast"    "Southwest"

# make as factor
sales_dataset$Region=factor(sales_dataset$Region)

levels(as.factor(sales_dataset$Route.To.Market))
#[1] "Fields Sales" "Other"  "Reseller"  "Telecoverage"  "Telesales"   
# make as factor
# set levels "Fields Sales" "Reseller"  "Telecoverage"  "Telesales"  "Other"
sales_dataset$Route.To.Market <- factor(sales_dataset$Route.To.Market, levels=
           c("Fields Sales", "Reseller", "Telecoverage", "Telesales", "Other"))  

levels(as.factor(sales_dataset$Opportunity.Result))
#[1] "Loss" "Won" 
# make as factor
# make base level for Opportunity.Result to Won
sales_dataset$Opportunity.Result <- factor(sales_dataset$Opportunity.Result,
                                           levels=c("Won", "Loss"))
# move Opportunity.Result column to the start 
sales_dataset <- sales_dataset %>% select(Opportunity.Result, everything())

levels(as.factor(sales_dataset$Competitor.Type))
#[1] "Known"   "None"    "Unknown"
# make as factor
sales_dataset$Competitor.Type=factor(sales_dataset$Competitor.Type)

levels(as.factor(sales_dataset$Deal.Size.Category))
#[1] "1" "2" "3" "4" "5" "6" "7"
# make as factor
sales_dataset$Deal.Size.Category=factor(sales_dataset$Deal.Size.Category)

levels(as.factor(sales_dataset$Client.Size.By.Revenue))
#[1] "1" "2" "3" "4" "5"
# make as factor
sales_dataset$Client.Size.By.Revenue=factor(sales_dataset$Client.Size.By.Revenue)

levels(as.factor(sales_dataset$Client.Size.By.Employee.Count))
#[1] "1" "2" "3" "4" "5"
# make as factor
sales_dataset$Client.Size.By.Employee.Count=
    factor(sales_dataset$Client.Size.By.Employee.Count)

levels(as.factor(sales_dataset$Revenue.From.Client.Past.Two.Years))
#[1] "0" "1" "2" "3" "4"
# make as factor
sales_dataset$Revenue.From.Client.Past.Two.Years=
  factor(sales_dataset$Revenue.From.Client.Past.Two.Years)

# structure of data set 
str(sales_dataset)
#tibble [77,829 x 18] (S3: tbl_df/tbl/data.frame)
# $ Opportunity.Result                     : Factor w/ 2 levels "Won","Loss": 1 2 1 2 2 2 1 2 2 2 ...
# $ Supplies.Subgroup                      : Factor w/ 11 levels "Batteries & Accessories",..: 3 3 6 9 3 9 4 3 1 3 ...
# $ Supplies.Group                         : Factor w/ 4 levels "Car Accessories",..: 1 1 3 3 1 3 1 1 1 1 ...
# $ Region                                 : Factor w/ 7 levels "Mid-Atlantic",..: 4 5 5 2 5 5 5 5 4 5 ...
# $ Route.To.Market                        : Factor w/ 5 levels "Fields Sales",..: 1 2 2 2 2 2 1 1 1 2 ...
# $ Elapsed.Days.In.Sales.Stage            : num [1:77829] 76 63 24 16 69 89 111 82 68 18 ...
# $ Sales.Stage.Change.Count               : num [1:77829] 13 2 7 5 11 3 12 6 8 7 ...
# $ Total.Days.Identified.Through.Closing  : num [1:77829] 104 163 82 124 91 114 112 70 156 50 ...
# $ Total.Days.Identified.Through.Qualified: num [1:77829] 101 163 82 124 13 0 112 70 156 50 ...
# $ Opportunity.Amount.USD                 : num [1:77829] 0 0 7750 0 69756 ...
# $ Client.Size.By.Revenue                 : Factor w/ 5 levels "1","2","3","4",..: 5 3 1 1 1 5 4 1 1 1 ...
# $ Client.Size.By.Employee.Count          : Factor w/ 5 levels "1","2","3","4",..: 5 5 1 1 1 1 5 1 5 1 ...
# $ Revenue.From.Client.Past.Two.Years     : Factor w/ 5 levels "0","1","2","3",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Competitor.Type                        : Factor w/ 3 levels "Known","None",..: 3 3 3 1 3 3 3 1 2 3 ...
# $ Ratio.Days.Identified.To.Total.Days    : num [1:77829] 0.696 0 1 1 0 ...
# $ Ratio.Days.Validated.To.Total.Days     : num [1:77829] 0.114 1 0 0 0.141 ...
# $ Ratio.Days.Qualified.To.Total.Days     : num [1:77829] 0.154 0 0 0 0 ...
# $ Deal.Size.Category                     : Factor w/ 7 levels "1","2","3","4",..: 1 1 1 1 4 5 2 6 6 4 ...

#' ### 2.1.2 Project Data Sets  

# preparing data sets for train, test and final validation 

# final validation set (validation_set)  will be 10% of the entire sales_dataset
set.seed(211120, sample.kind = "Rounding")
test_index <- createDataPartition(y = sales_dataset$Opportunity.Result, 
                                  times = 1, 
                                  p = 0.1, 
                                  list = FALSE)
# final validation sets
sales_set <- sales_dataset[-test_index,]
validation_set  <- sales_dataset[test_index,]

# split sales_set (80/20) to train and test algorithms
set.seed(211120, sample.kind = "Rounding")
test2_index <- createDataPartition(y = sales_set$Opportunity.Result, 
                                   times = 1, 
                                   p = 0.2, 
                                   list = FALSE)

train_sales_set <- sales_set[-test2_index,]
test_sales_set <- sales_set[test2_index,]

# data set used in projects 
part1 <- nrow(sales_set) / nrow(sales_dataset)
part2 <- nrow(validation_set) / nrow(sales_dataset)

table_stes <- tibble(sales_dataset_split="sales_set", 
                     rows=format(nrow(sales_set), big.mark= ',' ), 
                     percentage = percent(part1, accuracy=1)     )

table_stes <- bind_rows(table_stes, tibble(sales_dataset_split="validation_set",
                        rows=format(nrow(validation_set), big.mark= ',' ), 
                        percentage = percent(part2, accuracy=1)) )


part3 <- nrow(train_sales_set) / nrow(sales_set)
part4 <- nrow(test_sales_set) / nrow(sales_set)
table_sets_b <- tibble(sales_set_split="train_sales_set", 
                       rows=format(nrow(train_sales_set), big.mark= ',' ), 
                       percentage = percent(part3, accuracy=1) )

table_sets_b <- bind_rows(table_sets_b, tibble(sales_set_split="test_sales_set",
                          rows=format(nrow(test_sales_set), big.mark= ',' ),
                          percentage = percent(part4, accuracy=1) ))

kable( list(table_stes, table_sets_b), caption = 'Data sets', 
            booktabs = TRUE, valign = 't')  %>% 
            kable_styling(latex_options = "HOLD_position", font_size = 8)

#' ### 2.2 Exploration and Vizualization  

# all predictors, all columns except Opportunity.Result
pred_col <- c("Supplies.Subgroup", 
              "Supplies.Group",  
              "Region",
              "Route.To.Market", 
              "Elapsed.Days.In.Sales.Stage", 
              "Sales.Stage.Change.Count", 
              "Total.Days.Identified.Through.Closing",
              "Total.Days.Identified.Through.Qualified", 
              "Opportunity.Amount.USD",
              "Client.Size.By.Revenue" , 
              "Client.Size.By.Employee.Count",
              "Revenue.From.Client.Past.Two.Years", 
              "Competitor.Type",
              "Ratio.Days.Identified.To.Total.Days", 
              "Ratio.Days.Validated.To.Total.Days",
              "Ratio.Days.Qualified.To.Total.Days", 
              "Deal.Size.Category")

# categorical variables
categ_col <- c("Opportunity.Result",
               "Supplies.Subgroup", 
               "Supplies.Group",  
               "Region",
               "Route.To.Market",
               "Competitor.Type",
               "Deal.Size.Category",
               "Client.Size.By.Revenue" , 
               "Client.Size.By.Employee.Count", 
               "Revenue.From.Client.Past.Two.Years" )

# continuous variables
cont_col <- c( "Elapsed.Days.In.Sales.Stage", 
               "Sales.Stage.Change.Count", 
               "Total.Days.Identified.Through.Closing",
               "Total.Days.Identified.Through.Qualified", 
               "Opportunity.Amount.USD",
               "Ratio.Days.Identified.To.Total.Days", 
               "Ratio.Days.Validated.To.Total.Days",
               "Ratio.Days.Qualified.To.Total.Days")

#' ### 2.2.1 Correlation  

# heatmap
# set for the correlation  
cor_sales_set = subset(sales_set, 
                select = -c(Opportunity.Result, Supplies.Subgroup, 
                            Supplies.Group,  Region, Route.To.Market, 
                            Competitor.Type, Deal.Size.Category, 
                            Client.Size.By.Revenue,
                            Client.Size.By.Employee.Count, 
                            Revenue.From.Client.Past.Two.Years  ))
# correlation matrix 
cor_mat_sales <- round(cor(cor_sales_set),2)

# create the correlation heatmap 
# library(reshape2)
melted_cor_mat_sales <- melt(cor_mat_sales)

# save qq plot in figures
png(file="figs/corr_1.png", width=480, height=240)
# show the correlation matrix
ggcorrplot(cor_mat_sales, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE, lab_size = 2.5) +
   theme(axis.text.x = element_text(size = 8)) +
   theme(axis.text.y = element_text(size = 8)) 
dev.off()

# call heatmap graph in report
include_graphics("figs/corr_1.png", 
                 auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE), dpi=NA)

# drop Total.Days.Identified.Through.Closing from data sets 
sales_set = subset(sales_set, select = -c(Total.Days.Identified.Through.Closing))
train_sales_set = subset(train_sales_set, select = 
                           -c(Total.Days.Identified.Through.Closing))
test_sales_set = subset(test_sales_set, select = 
                          -c(Total.Days.Identified.Through.Closing))
validation_set = subset(validation_set, select = 
                          -c(Total.Days.Identified.Through.Closing))
pred_col <- pred_col[pred_col != "Total.Days.Identified.Through.Closing"]
cont_col <- cont_col[cont_col != "Total.Days.Identified.Through.Closing"]

#' ### 2.2.2 Near Zero Variance Predictors  
# identification of near zero variance predictors using nearZeroVar 
kable(nearZeroVar(sales_set[, pred_col], 
    saveMetrics = TRUE), caption = 'nearZeroVar', digits = 4, booktabs=TRUE) %>% 
    kable_styling(latex_options = "HOLD_position", font_size = 8)

# count values for Revenue.From.Client.Past.Two.Years
tbl1 <- sales_set %>% count(Revenue.From.Client.Past.Two.Years)
kable( tbl1, digits = 3, booktabs = TRUE, align = c("r", "r"))  %>% 
      kable_styling(latex_options = "HOLD_position", font_size = 8)

 
#' ### 2.2.3 Statistics Summary  
# data set variables summary statistics
#summary(sales_set)
temp <- sales_set[ , 1:5]
kable( summary(temp), booktabs = TRUE)  %>% 
  kable_styling(latex_options = "scale_down", font_size = 8)
temp <- sales_set[ , 6:9]
kable( summary(temp), booktabs = TRUE)  %>% 
  kable_styling(latex_options = "scale_down", font_size = 8)
temp <- sales_set[ , 10:13]
kable( summary(temp), booktabs = TRUE)  %>% 
  kable_styling(latex_options = "scale_down", font_size = 8)
temp <- sales_set[ , 14:17]
kable( summary(temp), booktabs = TRUE)  %>%   
  kable_styling(latex_options = "scale_down", font_size = 8)

#' ### 2.2.4 QQ Plot  
# save qq plot in figures
png(file="figs/qq_1.png", width=480, height=270)
# QQ plots to look up if the feature is normally distributed
qq_grid <- lapply(cont_col, FUN=function(var) {
  sales_set %>% 
    dplyr::select(all_of(var)) %>%
    ggplot(data = ., aes(sample = scale(.))) + 
    stat_qq() +
    stat_qq_line(colour = "red") +
    theme(axis.text.x = element_text(hjust = 1)) +
    ggtitle(var)+
    theme(title =element_text(size=8), 
          axis.text=element_text(size=9), 
          axis.title=element_text(size=9))
})
do.call(grid.arrange, args=c(qq_grid, list(ncol=3)))
dev.off()

# call qq plots in report
include_graphics("figs/qq_1.png", 
                 auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE), dpi=NA)

#' ### 2.2.5 Density Plot  
# save density plot in figures
png(file="figs/density_grid_1.png", width=480, height=330)
# grid of density plots
dens_grid <- lapply(cont_col, FUN=function(var) {
  # Build the plots
  ggplot(sales_set) + 
    geom_density(aes_string(x = var, fill = "Opportunity.Result"), alpha = 0.5) +
    ggtitle(var)+
    theme(title =element_text(size=8), 
          axis.text=element_text(size=6), 
          axis.title=element_text(size=8), axis.title.x=element_blank())
})
do.call(grid_arrange_shared_legend, args=c(dens_grid, nrow = 3, ncol = 3))
dev.off()

# call density plots in report
include_graphics("figs/density_grid_1.png", 
                 auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE), dpi=NA)

#' ### 2.2.6 Prevalence  
# distribution of Won Loss Opportunity.Result 
sales_set_won <- sales_set[which(sales_set$Opportunity.Result == "Won"),]
sales_set_loss <- sales_set[which(sales_set$Opportunity.Result == "Loss"),]
part1 <- nrow(sales_set_won) / nrow(sales_set)
part2 <- nrow(sales_set_loss) / nrow(sales_set)

table_stes <- tibble(Opportunity.Result="Won", 
                     rows=format(nrow(sales_set_won), big.mark= ',' ),
                     percentage = percent(part1, accuracy=1) )
table_stes <- bind_rows(table_stes, tibble(Opportunity.Result="Loss", 
                    rows=format(nrow(sales_set_loss), big.mark= ',' ) , 
                    percentage =   percent(part2, accuracy = 1) ))

kable( table_stes, booktabs = TRUE, valign = 't')  %>% 
  kable_styling(latex_options = "hold_position", font_size = 8)

#' ### 2.2.7 Class Distribution  
# save categorical plots in figures
png(file="figs/class_1.png", width=480, height=300)
# categorical variables class distribution 
p1 <- ggplot(data = sales_set) + geom_bar(aes(x=Supplies.Subgroup, 
            fill=Opportunity.Result)) + labs(title = "Supplies.Subgroup") +
  theme(title =element_text(size=9), axis.text.x=element_blank(), 
        axis.title.x = element_blank()) +
  theme(legend.position = "none")  + ylab("Proportion")

p2 <- ggplot(data = sales_set) + geom_bar(aes(x=Supplies.Group, 
            fill=Opportunity.Result)) + labs(title = "Supplies.Group") +
  theme(title =element_text(size=9), axis.text.x=element_blank(), 
        axis.title.x=element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = "none")  + ylab("Proportion")

p3 <- ggplot(data = sales_set) + geom_bar(aes(x=Region, 
             fill=Opportunity.Result)) + labs(title = "Region") +
  theme(title =element_text(size=9), axis.text.x=element_blank(),  
        axis.title.x=element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = "none")  + ylab("Proportion")

p4 <- ggplot(data = sales_set) + geom_bar(aes(x=Route.To.Market, 
            fill=Opportunity.Result)) + labs(title = "Route.To.Market") +
  theme(title =element_text(size=9), axis.text.x=element_blank(), 
        axis.title.x=element_blank()) +
  theme(legend.position = "none")  + ylab("Proportion")

p5 <- ggplot(data = sales_set) + geom_bar(aes(x=Competitor.Type, 
            fill=Opportunity.Result)) + labs(title = "Competitor.Type") +
  theme(title =element_text(size=9), axis.text.x=element_blank(), 
        axis.title.x=element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = "none")  + ylab("Proportion")

p6 <- ggplot(data = sales_set) + geom_bar(aes(x=Deal.Size.Category, 
            fill=Opportunity.Result)) + labs(title = "Deal.Size.Category") +
  theme(title =element_text(size=9), axis.text.x=element_blank(), 
        axis.title.x=element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = "none")  + ylab("Proportion")

p7 <- ggplot(data = sales_set) + geom_bar(aes(x=Client.Size.By.Revenue, 
            fill=Opportunity.Result)) + labs(title = "Client.Size.By.Revenue") +
  theme(title =element_text(size=9), axis.text.x=element_blank(), 
        axis.title.x=element_blank()) +
  theme(legend.position = "none")  + ylab("Proportion")

p8 <-ggplot(data = sales_set) + geom_bar(aes(x=Client.Size.By.Employee.Count, 
            fill=Opportunity.Result)) + labs(title = "Client.Size.By.Revenue") +
  theme(title =element_text(size=9), axis.text.x=element_blank(), 
        axis.title.x=element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = "none")  + ylab("Proportion")

p9 <- ggplot(data = sales_set) + geom_bar(aes(x=Revenue.From.Client.Past.Two.Years,
     fill=Opportunity.Result))+labs(title = "Revenue.From.Client.Past.Two.Years") +
  theme(title =element_text(size=9), axis.text.x=element_blank(), 
        axis.title.x=element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = "none")  + ylab("Proportion")

ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow=3, ncol=3, 
          common.legend = TRUE, legend="bottom")
dev.off()

# call class plots in report
include_graphics("figs/class_1.png", 
                 auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE), dpi=NA)

#' ### 2.2.8 Feature Selections  
# variable importance for Opportunity.Result
filterVarImp(x = sales_set[,pred_col], 
             y = sales_set$Opportunity.Result)

# variable importance table
tbl_varimp <- tibble(Feature = 
              rownames(filterVarImp(x = sales_set[,pred_col], 
                                    y = sales_set$Opportunity.Result)),
               Won = filterVarImp(x = sales_set[,pred_col], 
                                  sales_set$Opportunity.Result)$Won,
               Loss = filterVarImp(x = sales_set[,pred_col], 
                                   sales_set$Opportunity.Result)$Loss) %>%  
              arrange(desc(Won))

kable( tbl_varimp, digits = 3, caption='variable importance', 
       booktabs = TRUE, valign = 't' )  %>% 
       kable_styling(latex_options = "HOLD_position", font_size = 8)


#' ### 2.3 Modeling Approach  
#' The goal is to successfully predict successful sale (Won) 
#' what we are trying to achieve by testing different algorithms.
 
#' For algorithm testing we use train_sales_set sets and perform cross validation to train algorithm ten times. 
#' Then with best training result algorithms performance are evaluates according to test_sales_set 
#' and resulting measures are calculated using confusionMatrix function. 
#' As a basic measure, we use a baseline model.


 
#' ## 3 Results  

#' ### 3.1 Baseline Model  
# baseline model
set.seed(211120, sample.kind = "Rounding")
# guess with equal probability of Opportunity.Result Won, Loss
baseline <- sample(c("Loss", "Won"), nrow(train_sales_set), replace = TRUE)
baseline <- factor(baseline, levels = c("Won", "Loss"))

# use confusionMatrix to view the results 
cm_baseline_1 <- confusionMatrix(data = factor(baseline), 
                reference = factor(train_sales_set$Opportunity.Result))
cm_baseline_2 <- confusionMatrix(data = factor(baseline), 
                reference = factor(train_sales_set$Opportunity.Result), 
                mode = "prec_recall", positive="Won")
# results table for baseline 
baseline_results <- tibble(Model = "baseline",
                        Accuracy = cm_baseline_1$overall["Accuracy"],
                        Sensitivity = cm_baseline_1$byClass["Sensitivity"],
                        Specificity = cm_baseline_1$byClass["Specificity"],
                        Precision  = cm_baseline_2$byClass["Precision"],
                        F1_Score = cm_baseline_2$byClass["F1"])
#baseline_results
kable( baseline_results, caption = "baseline model result" )  %>% 
       kable_styling(latex_options = "HOLD_position", font_size = 8)


#' ### 3.2 Generalized Linear Model  
# glm model 
set.seed(211120, sample.kind = "Rounding")
# train glm model 
train_glm_all <- train(Opportunity.Result ~ ., 
                       method = "glm", 
                       data = train_sales_set,
                       trControl = trainControl(method = "cv", number = 10, p = 0.9))
# predict on test data set 
glm_all_preds <- predict(train_glm_all, test_sales_set)

## use confusionMatrix to view the results
cm_glm_1 <- confusionMatrix(data = factor(glm_all_preds), 
                            reference = factor(test_sales_set$Opportunity.Result))
cm_glm_2 <-  confusionMatrix(data = factor(glm_all_preds), 
                             reference = factor(test_sales_set$Opportunity.Result), 
                             mode = "prec_recall", positive="Won")

# results table for glm model 
glm_results <- tibble(Model = "glm",
                        Accuracy = cm_glm_1$overall["Accuracy"],
                        Sensitivity = cm_glm_1$byClass["Sensitivity"],
                        Specificity = cm_glm_1$byClass["Specificity"],
                        Precision  = cm_glm_2$byClass["Precision"],
                        F1_Score = cm_glm_2$byClass["F1"])
kable( glm_results, caption = "glm model result" )  %>% 
       kable_styling(latex_options = "HOLD_position", font_size = 8)


#' ### 3.3 Naive Bayes Model  
# naive_bayes model
set.seed(211120, sample.kind = "Rounding")
# train naive_bayes model 
train_nb_all <- train(Opportunity.Result ~ .  ,
                  data = train_sales_set,
                  method = "naive_bayes",
                  trControl = trainControl(method = "cv", number = 10, p = 0.9))
# predict on test data set 
nb_preds_all <- predict(train_nb_all, test_sales_set)

## use confusionMatrix to view the results
cm_nb_1 <- confusionMatrix(data = factor(nb_preds_all), reference =
                             factor(test_sales_set$Opportunity.Result))
cm_nb_2 <- confusionMatrix(data = factor(nb_preds_all), reference =
                             factor(test_sales_set$Opportunity.Result), 
            mode = "prec_recall", positive="Won")
# results table for naivebayes 
naivebayes_results <- tibble(Model = "naivebayes",
                        Accuracy = cm_nb_1$overall["Accuracy"],
                        Sensitivity = cm_nb_1$byClass["Sensitivity"],
                        Specificity = cm_nb_1$byClass["Specificity"],
                        Precision  = cm_nb_2$byClass["Precision"],
                        F1_Score = cm_nb_2$byClass["F1"])
kable( naivebayes_results, caption = "naive bayes model result" )  %>% 
       kable_styling(latex_options = "HOLD_position", font_size = 8)



#' ### 3.4 K-Nearest Neighbors  

# train i test set normalization for knn
# function to normalize values
normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}
#normalization train set n_ 
n_train_sales_set <- train_sales_set

#$ Opportunity.Result                     : Factor w/ 2 levels "Won","Loss": 1 2 2 2 2 2 2 2 2 2 ...
n_train_sales_set$Supplies.Subgroup <- as.numeric(
       factor(n_train_sales_set$Supplies.Subgroup))
n_train_sales_set$Supplies.Group <- as.numeric(
       factor(n_train_sales_set$Supplies.Group))
n_train_sales_set$Region <-  as.numeric(
       factor(n_train_sales_set$Region))
n_train_sales_set$Route.To.Market <-  as.numeric(
       factor(n_train_sales_set$Route.To.Market))
#$ Elapsed.Days.In.Sales.Stage            : num [1:56035] 76 69 89 82 68 18 35 16 81 83 ...
#Sales.Stage.Change.Count               : num [1:56035] 13 11 3 6 8 7 6 5 10 13 ...
#$ Total.Days.Identified.Through.Qualified: num [1:56035] 101 13 0 70 156 50 31 208 138 130 ...
#$ Opportunity.Amount.USD                 : num [1:56035] 0 69756 232522 450000 25
n_train_sales_set$Client.Size.By.Revenue <-  as.numeric(
       factor(n_train_sales_set$Client.Size.By.Revenue))
n_train_sales_set$Client.Size.By.Employee.Count <- as.numeric(
       factor(n_train_sales_set$Client.Size.By.Employee.Count))
n_train_sales_set$Revenue.From.Client.Past.Two.Years <-  as.numeric(
       factor(n_train_sales_set$Revenue.From.Client.Past.Two.Years))
n_train_sales_set$Competitor.Type <-  as.numeric(
       factor(n_train_sales_set$Competitor.Type))
#$ Ratio.Days.Identified.To.Total.Days    : num [1:56035] 0.696 0 0 0.264 0 ...
#$ Ratio.Days.Validated.To.Total.Days     : num [1:56035] 0.113985 0.141125 0.000877 0.73639 0.562821 ...
#$ Ratio.Days.Qualified.To.Total.Days     : num [1:56035] 0.154 0 0 0 0.437 ...
n_train_sales_set$Deal.Size.Category <-  as.numeric(
       factor(n_train_sales_set$Deal.Size.Category))

n_train_sales_set <- data.frame(n_train_sales_set[1],
                                lapply(n_train_sales_set[2:17], normalize) )

#normalization test set n_ normalization
n_test_sales_set <- test_sales_set
#$ Opportunity.Result                     : Factor w/ 2 levels "Won","Loss": 1 2 2 2 2 2 2 2 2 2 ...
n_test_sales_set$Supplies.Subgroup <- as.numeric(
      factor(n_test_sales_set$Supplies.Subgroup))
n_test_sales_set$Supplies.Group <- as.numeric(
      factor(n_test_sales_set$Supplies.Group))
n_test_sales_set$Region <-  as.numeric(
      factor(n_test_sales_set$Region))
n_test_sales_set$Route.To.Market <-  as.numeric(
      factor(n_test_sales_set$Route.To.Market))
#$ Elapsed.Days.In.Sales.Stage            : num [1:56035] 76 69 89 82 68 18 35 16 81 83 ...
#Sales.Stage.Change.Count               : num [1:56035] 13 11 3 6 8 7 6 5 10 13 ...
#$ Total.Days.Identified.Through.Qualified: num [1:56035] 101 13 0 70 156 50 31 208 138 130 ...
#$ Opportunity.Amount.USD                 : num [1:56035] 0 69756 232522 450000 25
n_test_sales_set$Client.Size.By.Revenue <-  as.numeric(
      factor(n_test_sales_set$Client.Size.By.Revenue))
n_test_sales_set$Client.Size.By.Employee.Count <-  as.numeric(
      factor(n_test_sales_set$Client.Size.By.Employee.Count))
n_test_sales_set$Revenue.From.Client.Past.Two.Years <-  as.numeric(
      factor(n_test_sales_set$Revenue.From.Client.Past.Two.Years))
n_test_sales_set$Competitor.Type <-  as.numeric(
      factor(n_test_sales_set$Competitor.Type))
#$ Ratio.Days.Identified.To.Total.Days    : num [1:56035] 0.696 0 0 0.264 0 ...
#$ Ratio.Days.Validated.To.Total.Days     : num [1:56035] 0.113985 0.141125 0.000877 0.73639 0.562821 ...
#$ Ratio.Days.Qualified.To.Total.Days     : num [1:56035] 0.154 0 0 0 0.437 ...
n_test_sales_set$Deal.Size.Category <-  as.numeric(
      factor(n_test_sales_set$Deal.Size.Category))

n_test_sales_set <- data.frame(n_test_sales_set[1], 
                               lapply(n_test_sales_set[2:17], normalize))


#normalize train i test set 
# n_train_sales_set
# n_test_sales_set

# knn model 
set.seed(211120, sample.kind = "Rounding")
# train knn model
train_knn_bp <- train(Opportunity.Result ~ Total.Days.Identified.Through.Qualified + 
                      Deal.Size.Category + Ratio.Days.Qualified.To.Total.Days + 
                      Revenue.From.Client.Past.Two.Years + 
                      Ratio.Days.Identified.To.Total.Days + Opportunity.Amount.USD,
                      method = "knn",
                      data = n_train_sales_set,
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
# predict on test data set 
knn_preds_bp <- predict(train_knn_bp, n_test_sales_set)

### use confusionMatrix to view the results
cm_knn_1  <- confusionMatrix(data = factor(knn_preds_bp), reference = 
                               factor(n_test_sales_set$Opportunity.Result))
cm_knn_2 <- confusionMatrix(data = factor(knn_preds_bp), reference = 
                              factor(n_test_sales_set$Opportunity.Result), 
                              mode = "prec_recall", positive="Won")
# results table for knn 
knn_results <- tibble(Model = "knn",
                        Accuracy = cm_knn_1$overall["Accuracy"],
                        Sensitivity = cm_knn_1$byClass["Sensitivity"],
                        Specificity = cm_knn_1$byClass["Specificity"],
                        Precision  = cm_knn_2$byClass["Precision"],
                        F1_Score = cm_knn_2$byClass["F1"])
kable( knn_results, caption = "knn model result" )  %>% 
       kable_styling(latex_options = "HOLD_position", font_size = 8)



#' ### 3.5 Quadratic Discriminant Analysis  
# qda model
set.seed(211120, sample.kind = "Rounding")
# train qda model
train_qda_bp <- train(Opportunity.Result ~ Total.Days.Identified.Through.Qualified + 
                      Deal.Size.Category + Ratio.Days.Qualified.To.Total.Days + 
                      Revenue.From.Client.Past.Two.Years + 
                      Ratio.Days.Identified.To.Total.Days + Opportunity.Amount.USD,
                      data = train_sales_set,
                      method = "qda",
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
# predict on test data set
qda_preds_bp <- predict(train_qda_bp, test_sales_set)

### use confusionMatrix to view the results
cm_qda_1 <- confusionMatrix(data = factor(qda_preds_bp), reference =
                              factor(test_sales_set$Opportunity.Result))
cm_qda_2 <- confusionMatrix(data = factor(qda_preds_bp), reference =
                              factor(test_sales_set$Opportunity.Result), 
                              mode = "prec_recall", positive="Won")
# results table for qda 
qda_results <- tibble(Model = "qda",
                        Accuracy = cm_qda_1$overall["Accuracy"],
                        Sensitivity = cm_qda_1$byClass["Sensitivity"],
                        Specificity = cm_qda_1$byClass["Specificity"],
                        Precision  = cm_qda_2$byClass["Precision"],
                        F1_Score = cm_qda_2$byClass["F1"])
kable( qda_results, caption = "qda model result")  %>% 
       kable_styling(latex_options = "HOLD_position", font_size = 8)


#' ### 3.6 Random Forest  
# !!!this  can take long execution time
# rf model
set.seed(211120, sample.kind = "Rounding")
# train rf model
train_rf_all <- train(Opportunity.Result ~ . , 
                  data = train_sales_set,
                  method = "rf",
                  ntree = 100,    #ntree = 100
                  tuneGrid = data.frame(mtry = seq(1:10)),
                  trControl = trainControl(method = "cv", number = 10, p = 0.9))
# predict on test data set
rf_preds_all <- predict(train_rf_all, test_sales_set)

# use confusionMatrix to view the results 
cm_RF_1 <- confusionMatrix(data = factor(rf_preds_all), reference = 
                             factor(test_sales_set$Opportunity.Result))
cm_RF_2 <- confusionMatrix(data = factor(rf_preds_all), reference = 
                             factor(test_sales_set$Opportunity.Result), 
                             mode = "prec_recall", positive="Won")
# results table for rf
rf_results <- tibble(Model = "rf",
                        Accuracy = cm_RF_1$overall["Accuracy"],
                        Sensitivity = cm_RF_1$byClass["Sensitivity"],
                        Specificity = cm_RF_1$byClass["Specificity"],
                        Precision  = cm_RF_2$byClass["Precision"],
                        F1_Score = cm_RF_2$byClass["F1"])
kable( rf_results, caption = "rf model result" )  %>%
       kable_styling(latex_options = "HOLD_position", font_size = 8)


#' ### 3.7 AdaBoost  
# !!!this  can take very long execution time
# adaboost model
set.seed(211120, sample.kind = "Rounding")
# train adaboost model
train_adab_bp <- train(Opportunity.Result ~ Total.Days.Identified.Through.Qualified + 
                      Deal.Size.Category + Ratio.Days.Qualified.To.Total.Days + 
                      Revenue.From.Client.Past.Two.Years + 
                      Ratio.Days.Identified.To.Total.Days + Opportunity.Amount.USD,
                      data = train_sales_set,
                      method = "adaboost",
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
# predict on test data set
adab_preds_bp <- predict(train_adab_bp, test_sales_set)

# use confusionMatrix to view the results 
cm_adab_1 <- confusionMatrix(data = factor(adab_preds_bp), reference = 
                               factor(test_sales_set$Opportunity.Result))
cm_adab_2 <- confusionMatrix(data = factor(adab_preds_bp), reference = 
                               factor(test_sales_set$Opportunity.Result), 
                               mode = "prec_recall", positive="Won")
# results table for adaboost
adaboost_results <- tibble(Model = "adaboost",
                        Accuracy = cm_adab_1$overall["Accuracy"],
                        Sensitivity = cm_adab_1$byClass["Sensitivity"],
                        Specificity = cm_adab_1$byClass["Specificity"],
                        Precision  = cm_adab_2$byClass["Precision"],
                        F1_Score = cm_adab_2$byClass["F1"])
kable( adaboost_results, caption = "adaboost model result" )  %>% 
       kable_styling(latex_options = "HOLD_position", font_size = 8)


#' ### 3.8 Ensemble Model  
# ensemble for alggorithms "glm", "naive_bayes", "knn", "qda", "rf", "adaboost"
# define algorithms for ensemble model
models <- c( "glm", "naive_bayes", "knn", "qda", "rf", "adaboost") 
set.seed(211120, sample.kind = "Rounding")
# make predictions using already trained algorithms  
preds <- sapply(models, function(model){ 
  if (model == "glm") {
  # glm use all predictors 
    fit <- train_glm_all
  } else if (model == "naive_bayes") {
  # naive bayes uuse all predictors
    fit <- train_nb_all
  }else if (model == "knn") {
  # knn use set of most important predictors and normalization data
    fit <- train_knn_bp
  } else if (model == "qda") {
  # qda use set of most important predictors
    fit <- train_qda_bp
  } else if (model == "rf") {
  # Random forest use all predictors
    fit <- train_rf_all
  } else if (model == "adaboost") {
  # adaboost use set of most important predictors 
    fit <- train_adab_bp
  }
# predictions and predictions for knn
  if (model == "knn") {
    # knn use data sets with normalization
    pred <- predict(object = fit, newdata = n_test_sales_set)  
  }else {
    pred <- predict(object = fit, newdata = test_sales_set) 
  }
# these predictions will be used for ensemble
  return(pred)
})
# Combine all models, use votes method to ensemble the predictions
votes <- rowMeans(preds == "Won")
ens_preds <- factor(ifelse(votes > 0.5, "Won", "Loss"))
ens_preds <- factor(ens_preds, levels=c("Won", "Loss"))

# use confusionMatrix to view the results
cm_ens_1 <- confusionMatrix(data = factor(ens_preds), reference = 
                              factor(test_sales_set$Opportunity.Result))
cm_ens_2 <- confusionMatrix(data = factor(ens_preds), reference = 
                              factor(test_sales_set$Opportunity.Result), 
                              mode = "prec_recall", positive="Won")
# results table for Ensemble
ensemble_results <- tibble(Model = "ensemble",
                        Accuracy = cm_ens_1$overall["Accuracy"],
                        Sensitivity = cm_ens_1$byClass["Sensitivity"],
                        Specificity = cm_ens_1$byClass["Specificity"],
                        Precision  = cm_ens_2$byClass["Precision"],
                        F1_Score = cm_ens_2$byClass["F1"])
kable(ensemble_results, caption = "ensemble model result")  %>%  
      kable_styling(latex_options = "HOLD_position", font_size = 8)



#' ### 3.9 Best Model Selection  
# all model results in table model_results
# results baseline 
model_results <- tibble(Model = "baseline",
                        Accuracy = cm_baseline_1$overall["Accuracy"],
                        Sensitivity = cm_baseline_1$byClass["Sensitivity"],
                        Specificity = cm_baseline_1$byClass["Specificity"],
                        Precision  = cm_baseline_2$byClass["Precision"],
                        F1_Score = cm_baseline_2$byClass["F1"])
# glm
model_results <- model_results %>% add_row(Model = "glm",
                        Accuracy = cm_glm_1$overall["Accuracy"],
                        Sensitivity = cm_glm_1$byClass["Sensitivity"],
                        Specificity = cm_glm_1$byClass["Specificity"],
                        Precision = cm_glm_2$byClass["Precision"],
                        F1_Score = cm_glm_2$byClass["F1"])
# naive_bayes
model_results <- model_results %>% add_row(Model = "naive_bayes",
                        Accuracy = cm_nb_1$overall["Accuracy"],
                        Sensitivity = cm_nb_1$byClass["Sensitivity"],
                        Specificity = cm_nb_1$byClass["Specificity"],
                        Precision = cm_nb_2$byClass["Precision"],
                        F1_Score = cm_nb_2$byClass["F1"])
# knn
model_results <- model_results %>% add_row(Model = "knn",
                       Accuracy = cm_knn_1$overall["Accuracy"],
                       Sensitivity = cm_knn_1$byClass["Sensitivity"],
                       Specificity = cm_knn_1$byClass["Specificity"],
                       Precision = cm_knn_2$byClass["Precision"],
                       F1_Score = cm_knn_2$byClass["F1"])
# qda
model_results <- model_results %>% add_row(Model = "qda",
                       Accuracy = cm_qda_1$overall["Accuracy"],
                       Sensitivity = cm_qda_1$byClass["Sensitivity"],
                       Specificity = cm_qda_1$byClass["Specificity"],
                       Precision = cm_qda_2$byClass["Precision"],
                       F1_Score = cm_qda_2$byClass["F1"])
# rf
model_results <- model_results %>% add_row(Model = "rf",
                       Accuracy = cm_RF_1$overall["Accuracy"],
                       Sensitivity = cm_RF_1$byClass["Sensitivity"],
                       Specificity = cm_RF_1$byClass["Specificity"],
                       Precision = cm_RF_2$byClass["Precision"],
                       F1_Score = cm_RF_2$byClass["F1"])
# adaboost
model_results <- model_results %>% add_row(Model = "adaboost",
                       Accuracy = cm_adab_1$overall["Accuracy"],
                       Sensitivity = cm_adab_1$byClass["Sensitivity"],
                       Specificity = cm_adab_1$byClass["Specificity"],
                       Precision = cm_adab_2$byClass["Precision"],
                       F1_Score = cm_adab_2$byClass["F1"])
#ensemble
model_results <- model_results %>% add_row(Model = "ensemble",
                       Accuracy = cm_ens_1$overall["Accuracy"],
                       Sensitivity = cm_ens_1$byClass["Sensitivity"],
                       Specificity = cm_ens_1$byClass["Specificity"],
                       Precision  = cm_ens_2$byClass["Precision"],
                       F1_Score = cm_ens_2$byClass["F1"])
#model_results
kable( model_results, caption = "Model results", digits = 4 )  %>% 
       kable_styling(latex_options = "HOLD_position", font_size = 8) %>% 
       row_spec(0,bold=TRUE)


# Show ROC and Precision-Recall curves for all models, using precrec package 
# glm  scores and labels for precrec package
scores_glm <-  str_replace_all(glm_all_preds, 'Won', '1') 
scores_glm <-  str_replace_all(scores_glm, 'Loss', '0') 
scores_glm <- as.numeric(as.character(scores_glm))

labels_glm <- str_replace_all(test_sales_set$Opportunity.Result, 'Won', '1')
labels_glm <- str_replace_all(labels_glm, 'Loss', '0')
labels_glm <- as.numeric(as.character(labels_glm))

# naive bayes scores and labels for precrec package
scores_nb <-  str_replace_all(nb_preds_all, 'Won', '1') 
scores_nb <-  str_replace_all(scores_nb, 'Loss', '0') 
scores_nb <- as.numeric(as.character(scores_nb))

labels_nb <- str_replace_all(test_sales_set$Opportunity.Result, 'Won', '1')
labels_nb <- str_replace_all(labels_nb, 'Loss', '0')
labels_nb <- as.numeric(as.character(labels_nb))

# knn scores and labels for precrec package
scores_knn <-  str_replace_all(knn_preds_bp, 'Won', '1') 
scores_knn <-  str_replace_all(scores_knn, 'Loss', '0') 
scores_knn <- as.numeric(as.character(scores_knn))

#knn use set with normalization
labels_knn <- str_replace_all(n_test_sales_set$Opportunity.Result, 'Won', '1')  
labels_knn <- str_replace_all(labels_knn, 'Loss', '0')
labels_knn <- as.numeric(as.character(labels_knn))

# qda scores and labels for precrec package
scores_qda <-  str_replace_all(qda_preds_bp, 'Won', '1') 
scores_qda <-  str_replace_all(scores_qda, 'Loss', '0') 
scores_qda <- as.numeric(as.character(scores_qda))

labels_qda <- str_replace_all(test_sales_set$Opportunity.Result, 'Won', '1')
labels_qda <- str_replace_all(labels_qda, 'Loss', '0')
labels_qda <- as.numeric(as.character(labels_qda))

# rf scores and labels for precrec package
scores_rf <-  str_replace_all(rf_preds_all, 'Won', '1') 
scores_rf <-  str_replace_all(scores_rf, 'Loss', '0') 
scores_rf <- as.numeric(as.character(scores_rf))

labels_rf <- str_replace_all(test_sales_set$Opportunity.Result, 'Won', '1')
labels_rf <- str_replace_all(labels_rf, 'Loss', '0')
labels_rf <- as.numeric(as.character(labels_rf))

# adaboost scores and labels for precrec package
scores_adab <-  str_replace_all(adab_preds_bp, 'Won', '1') 
scores_adab <-  str_replace_all(scores_adab, 'Loss', '0') 
scores_adab <- as.numeric(as.character(scores_adab))

labels_adab <- str_replace_all(test_sales_set$Opportunity.Result, 'Won', '1')
labels_adab <- str_replace_all(labels_adab, 'Loss', '0')
labels_adab <- as.numeric(as.character(labels_adab))

# ensemble scores and labels for precrec package
scores_ens <-  str_replace_all(ens_preds, 'Won', '1') 
scores_ens <-  str_replace_all(scores_ens, 'Loss', '0') 
scores_ens <- as.numeric(as.character(scores_ens))

labels_ens <- str_replace_all(test_sales_set$Opportunity.Result, 'Won', '1')
labels_ens <- str_replace_all(labels_ens, 'Loss', '0')
labels_ens <- as.numeric(as.character(labels_ens))

# save results ROC, PRC plots in figures
png(file="figs/result_plot_1.png", width=480, height=180)

# one plot for "glm", "naive_bayes", "knn", "qda", "rf", "adaboost" and  ensemble
# join score vectors
scores1 <-  join_scores(scores_glm, scores_nb, scores_knn, scores_qda, 
                        scores_rf, scores_adab, scores_ens)
# join label vectors
labels1 <- join_labels(labels_glm, labels_nb, labels_knn, labels_qda,  
                       labels_rf, labels_adab, labels_ens)
# specify model names and test data set names
mmmdat1 <- mmdata(scores1, labels1, modnames= 
          c("glm", "naive_bayes", "knn", "qda", "rf", "adaboost", "ensemble" ), 
          dsids = c(1, 2, 3, 4, 5, 6, 7))
# calculate curves for multiple models and multiple test datasets
mmcurves <- evalmod(mmmdat1)
# show average ROC curves
p1 <-  autoplot(mmcurves, "ROC") + labs(title="ROC")
# show average Precision-Recall curves
p2 <- autoplot(mmcurves, "PRC") + labs(title="Precision-Recall")
ggarrange(p1, p2,  nrow=1, ncol=2, common.legend = TRUE, legend="right" )
dev.off()


# call results plots in report
include_graphics("figs/result_plot_1.png", 
                 auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE), dpi=NA)


#' ### 3.10 Final Validation  
# !!!this can take long execution time
# final model rf, sales_set and validation set  
set.seed(211120, sample.kind = "Rounding")
# train final rf model on sales_set
train_final_rf_all <- train(Opportunity.Result ~ . , 
                      data = sales_set,
                      method = "rf",
                      ntree = 100,    #ntree = 100
                      tuneGrid = data.frame(mtry = seq(1:10)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
# predict for final rf model on validation set
final_rf_preds_all <- predict(train_final_rf_all, validation_set)


# use confusionMatrix to view the results
final_cm_RF_1 <- confusionMatrix(data = factor(final_rf_preds_all), 
                          reference = factor(validation_set$Opportunity.Result))
final_cm_RF_2 <- confusionMatrix(data = factor(final_rf_preds_all), 
                          reference = factor(validation_set$Opportunity.Result), 
                          mode = "prec_recall", positive="Won")

# final validation results table for  rf model on validation set 
final_val_results <- tibble(Model = "final validation rf",
                        Accuracy = final_cm_RF_1$overall["Accuracy"],
                        Sensitivity = final_cm_RF_1$byClass["Sensitivity"],
                        Specificity = final_cm_RF_1$byClass["Specificity"],
                        Precision  = final_cm_RF_2$byClass["Precision"],
                        F1_Score = final_cm_RF_2$byClass["F1"])
kable(final_val_results, caption = "rf final model results")  %>%  
  kable_styling(latex_options = "HOLD_position", font_size = 8)



# end Time ALL
endTime_All_2 <-  Sys.time()
# total time ALL
endTime_All_2 - startTime_ALL_2


# end.
