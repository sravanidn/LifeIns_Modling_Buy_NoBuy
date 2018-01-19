rm(list=ls())
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

df = read.csv("C:/Users/srava/Desktop/Capstone BuyerNonBuyer_Updated_V6.0.csv", colClasses = "character", stringsAsFactors = F)

df = apply(df, MARGIN = 2, function(x) ifelse(x == "#NULL!", NA, x))
df = data.frame(df)

'
Open-eneded -- 

1. What are the features of wealthy people/employed not purchasing the insurance?
2. Who are the people that will definitely purchase insurance?
3. Who are the people that will definitely not purchase insurance?

Concrete ---
2. Demographics/education/salary - how each factor affects purchasing behaviour?
3. Retired people contribution to purchasing? 
4. Demographics - Age, Marital, Geneder, Salary, Ethnicity, houesold count, insurance, information sources, financial agent, major event
5. Conversion rate by age ?
6. Most effective advertising channels?
7. Type of life insurance people are buying?
9. Difference between segment 4 and 5? Any particular interest around financial agent?
10. Impact of different financial professionals?
'


## Data manipulation

# Marital Status
df$Q2_Marital.Status = as.character(df$Q2_Marital.Status)
df$Q2_Marital.Status[df$Q2_Marital.Status == "Single, never married"] = "Single"
df$Q2_Marital.Status[df$Q2_Marital.Status == "Married or Domestic Partner"] = "Married"

# Annual Household income
df$Q8_Annual_Household_Income = as.character(df$Q8_Annual_Household_Income)
df$Q8_Annual_Household_Income[str_detect(df$Q8_Annual_Household_Income, pattern = "^\\$100")] = 100000
df$Q8_Annual_Household_Income[str_detect(df$Q8_Annual_Household_Income, pattern = "^\\$150")] = 150000
df$Q8_Annual_Household_Income[str_detect(df$Q8_Annual_Household_Income, pattern = "^\\$200")] = 200000
df$Q8_Annual_Household_Income[str_detect(df$Q8_Annual_Household_Income, pattern = "^\\$25,")] = 25000
df$Q8_Annual_Household_Income[str_detect(df$Q8_Annual_Household_Income, pattern = "^\\$250,")] = 300000
df$Q8_Annual_Household_Income[str_detect(df$Q8_Annual_Household_Income, pattern = "^\\$50,")] = 50000
df$Q8_Annual_Household_Income[str_detect(df$Q8_Annual_Household_Income, pattern = "^\\$75,")] = 75000
df$Q8_Annual_Household_Income[str_detect(df$Q8_Annual_Household_Income, pattern = "Less")] = 20000

df$Q8_Annual_Household_Income = as.integer(df$Q8_Annual_Household_Income)

# Employment

df$Q10_Employment_Status = as.character(df$Q10_Employment_Status)

df$Q10_Employment_Status[str_detect(df$Q10_Employment_Status, "Full-time")] = "Full_Time"
df$Q10_Employment_Status[str_detect(df$Q10_Employment_Status, "Part-time")] = "Part_Time"
df$Q10_Employment_Status[str_detect(df$Q10_Employment_Status, "Retired")] = "Retired"
df$Q10_Employment_Status[str_detect(df$Q10_Employment_Status, "Not working")] = "Not_Working"
df$Q10_Employment_Status[str_detect(df$Q10_Employment_Status, "Self")] = "Self_Employed"


df$Q11_Partner_Employment_Status = as.character(df$Q11_Partner_Employment_Status)

df$Q11_Partner_Employment_Status[str_detect(df$Q11_Partner_Employment_Status, "Full-time")] = "Full_Time"
df$Q11_Partner_Employment_Status[str_detect(df$Q11_Partner_Employment_Status, "Part-time")] = "Part_Time"
df$Q11_Partner_Employment_Status[str_detect(df$Q11_Partner_Employment_Status, "Retired")] = "Retired"
df$Q11_Partner_Employment_Status[str_detect(df$Q11_Partner_Employment_Status, "Not working")] = "Not_Working"
df$Q11_Partner_Employment_Status[str_detect(df$Q11_Partner_Employment_Status, "Self")] = "Self_Employed"


df$Q23c1_TotalCoverageWithSpouse_IndivisualIns = df$Q23c1_TotalCoverageWithSpouse_IndivisualIns %>% 
        as.character() %>% 
        str_replace_all(pattern = ",", "") %>% 
        str_extract_all(pattern = "[[:digit:]]+", simplify = T) %>% 
        data.frame() %>% 
        mutate(X1 = as.numeric(as.character(X1)), X2 = as.numeric(as.character(X2))) %>% 
        mutate(X3 = ifelse(is.na(X2 + X1) , yes = X1, (X1 + X2)/2)) %>% 
        select(X3) %>% 
        as.list() %>% 
        unlist() %>% 
        as.integer()


df$Q23c2_TotalCoverageWithSpouse_GroupIns = df$Q23c2_TotalCoverageWithSpouse_GroupIns %>% 
        as.character() %>% 
        str_replace_all(pattern = ",", "") %>% 
        str_extract_all(pattern = "[[:digit:]]+", simplify = T) %>% 
        data.frame() %>% 
        mutate(X1 = as.numeric(as.character(X1)), X2 = as.numeric(as.character(X2))) %>% 
        mutate(X3 = ifelse(is.na(X2 + X1) , yes = X1, (X1 + X2)/2)) %>% 
        select(X3) %>% 
        as.list() %>% 
        unlist() %>% 
        as.integer()


df$Q24_Total_Investable_Assets = df$Q24_Total_Investable_Assets %>% 
        as.character() %>% 
        str_replace_all(pattern = ",", "") %>% 
        str_extract_all(pattern = "[[:digit:]]+", simplify = T) %>% 
        data.frame() %>% 
        mutate(X1 = as.numeric(as.character(X1)), X2 = as.numeric(as.character(X2))) %>% 
        mutate(X3 = ifelse(is.na(X2 + X1) , yes = X1, (X1 + X2)/2)) %>% 
        select(X3) %>% 
        as.list() %>% 
        unlist() %>% 
        as.integer()

df$Q25_Total_Household_Debt_Estimate = df$Q25_Total_Household_Debt_Estimate %>% 
        as.character() %>% 
        str_replace_all(pattern = ",", "") %>% 
        str_extract_all(pattern = "[[:digit:]]+", simplify = T) %>% 
        data.frame() %>% 
        mutate(X1 = as.numeric(as.character(X1)), X2 = as.numeric(as.character(X2))) %>% 
        mutate(X3 = ifelse(is.na(X2 + X1) , yes = X1, (X1 + X2)/2)) %>% 
        select(X3) %>% 
        as.list() %>% 
        unlist() %>% 
        as.integer()


#-------------------------
## Conversion by Age

### 1. Contribution by age to each segment
df %>% 
        select(AgeBins_5, Segment) %>% 
        ggplot(aes(AgeBins_5, fill = Segment)) +
        geom_histogram(stat = "count",position = "fill") # People below 35 have a higher tendency to purchase life insurance

### Difference in insured vs un-insured people
df %>% 
        select(AgeBins_5, Segment, Q20_Insured) %>% 
        ggplot(aes(AgeBins_5, fill = Segment)) +
        geom_histogram(stat = "count",position = "fill") +
        facet_grid(~Q20_Insured) # If already insured, people below 35 have the highest propensity to buy another life insurance. Do these people have high income/education?


### Difference by income if people are already insured
df %>% 
        filter(Q20_Insured == "1") %>% 
        select(AgeBins_5, Segment, Q8_Annual_Household_Income) %>% 
        ggplot(aes(AgeBins_5, fill = Segment)) +
        geom_histogram(stat = "count",position = "fill") +
        facet_grid(~Q8_Annual_Household_Income) # Insured People below 35 with income between 100k-250k have highest propensity to purchase life insurance. All people below 35 with income between 150k - 200k and >250k who received quote purchased the insurance. All insured people below 35 with who felt a need made a purchase

### Do People who do not felt a need received any information?

df %>% 
        filter(Q20_Insured == "1") %>% 
        mutate(Information = ifelse(Q28_31_Information_Sources =="0","0","1")) %>% 
        select(Segment, AgeBins_5, Information, Q30_Event) %>% 
        ggplot(aes(Information, fill = Segment)) +
        geom_bar(stat = "count", position = "fill") +
        facet_wrap(~AgeBins_5) # Even if information was provided to users through 1 channel, the chances of purchasing insurance increases significantly. This effect is most pronounced in people with age 35-44. Most of the people who do not felt a need never received information about life insurance


df %>% 
        filter(Q20_Insured == "0") %>% 
        mutate(Information = ifelse(Q28_31_Information_Sources =="0","0","1")) %>% 
        select(Segment, AgeBins_5, Information, Q30_Event) %>% 
        ggplot(aes(Information, fill = Segment)) +
        geom_bar(stat = "count", position = "fill") +
        facet_wrap(~AgeBins_5) # Even if information was provided to uninsured usersthe chances of purchasing insurance do not increase.


#-------------------------
## Features of wealthy people not buying insurance

df_w = df %>% 
        filter(Q8_Annual_Household_Income > 100000 | Q24_Total_Investable_Assets > 499999)

table(df_w$Segment) %>% prop.table()*100 # 62% do not feel a need

table(df_w$Segment, df_w$Q20_Insured) %>% prop.table()*100 # Only 23% of  all wealthy people are not already insured. Lets look at only uninsured people



df_w = df_w[df_w$Buyers_vs_NonBuyers=="Non-Buyers", ]
df_w %>% dim() # 1000 people fall in this category

df_w %>% 
        mutate(Information = ifelse(Q28_31_Information_Sources =="0","0","1")) %>%
        group_by(Information, Segment) %>% 
        summarise(cnt = n())


df_w %>% head(20) %>% View()

table(df_w$Segment) %>% prop.table() # 80% do not feel a need. Are they insured?
table(df_w$Segment, df_w$Q20_Insured) %>% prop.table() # 27% of uninsured people do not feel the need. Are they even being informed?

df_w %>% 
        filter(Q20_Insured == "0" & Segment == "Segment 1 - No Need") %>% 
        mutate(Information = ifelse(Q28_31_Information_Sources =="0","0","1")) %>%
        select(Information, AgeBins_5) %>% 
        ggplot(aes(AgeBins_5, fill = Information)) +
        geom_bar(stat = "count", position = "fill") # Over 60% of uninsured wealthy people are not receiving any information about life insurance products. Whats preventing informed people from buying insurance?

## from a separate analysis - of all the reasons given by reached out wealthy uninsured people in Segment 1 -
### 37% of reasons were - dont trust insurance companies/agents/to avoid high pressure sales tactics
### 20% reasons - not sure how to pick right amount, or what insurance to purchase - potential customers

df_w %>% 
        filter(Q20_Insured == "0" & Segment == "Segment 1 - No Need") %>% 
        mutate(Information = ifelse(Q28_31_Information_Sources =="0","0","1")) %>%
        select(Information, AgeBins_5) %>% 
        ggplot(aes(AgeBins_5, fill = Information)) +
        geom_bar(stat = "count", position = "fill")


### ---------------------------------
## Clustering users based on numerical variables

df_c = df

## Variables -- Chr -> Factor
### Marital status, Q10/11 - Employment Status

df$Q2_Marital.Status = as.factor(df$Q2_Marital.Status)
df$Q10_Employment_Status = as.factor(df$Q10_Employment_Status)
df$Q11_Partner_Employment_Status = as.factor(df$Q11_Partner_Employment_Status)

## Variables -- Factor -> integer
### Q1_age, Q4_familyoccupaton, Q26_people, Q30_events, Q28_31_Information_sources
df$Q1_Age = as.integer(as.character(df$Q1_Age))
df$Q4_FamilyOccupation = as.integer(as.character(df$Q4_FamilyOccupation))
df$Q26_People = as.integer(as.character(df$Q26_People))
df$Q30_Event = as.integer(as.character(df$Q30_Event))
df$Q28_31_Information_Sources = as.integer(as.character(df$Q28_31_Information_Sources))

## Variables -- dummy variables
library(caret)
dmy = dummyVars(formula = "~.", data = df[2:31])
df_trnsf = as.data.frame(predict(object = dmy, newdata = df[2:31]))

df_trnsf = df_trnsf %>% 
        select(- starts_with("Weight"))

## Centering and scaling
df_trnsf_num = df_trnsf[c("Q1_Age", "Q8_Annual_Household_Income", "Q23c1_TotalCoverageWithSpouse_IndivisualIns", "Q23c2_TotalCoverageWithSpouse_GroupIns", "Q24_Total_Investable_Assets", "Q25_Total_Household_Debt_Estimate")]

df_numproc_temp = preProcess(x = df_trnsf_num, method = c("center", "scale"))

df_trnsf_num_proc = predict(object = df_numproc_temp, newdata = df_trnsf_num)

df_trnsf = df_trnsf[!(colnames(df_trnsf) %in% colnames(df_trnsf_num))]
df_trnsf = cbind(df_trnsf, df_trnsf_num_proc)

df_trnsf %>% colnames()
df_trnsf_identifiers = df_trnsf[1:9]
#df_trnsf = df_trnsf[- c(1:9)]

## Imputing missing values
df_trnsf[is.na(df_trnsf)] = 0

df_clust5 = kmeans(x = df_trnsf, centers = 5, nstart = 50)
df_clust5$tot.withinss

table(df_clust5$cluster)
df_trnsf$clust = df_clust5$cluster
#df_trnsf = cbind(df_trnsf, df_trnsf_identifiers)

df_trnsf = df_trnsf %>% 
        gather(key = Segment, value = segment_flag, `Segment.Segment 1 - No Need`, `Segment.Segment 2 - No information`, `Segment.Segment 3 - No Options`, `Segment.Segment 4 - No Purchase`, `Segment.Segment 5 - Purchase`) %>% 
        filter(segment_flag == "1")

df_trnsf %>% 
        ggplot(aes(clust, fill  = Segment)) +
        geom_histogram(stat = "count", position = "fill") +
        labs(title = "Clutser vs Segment Distribution")

df_trnsf$Customer_ID = df$Customer_ID ### It would be interesting to look at characterstics of people falling in cluster 5 who havent purchased.

## Looping for value of k
vec = c()

for (i in 2:20) {
        df_clust = kmeans(x = df_trnsf, centers = i, nstart = 50)
        vec = append(vec, (df_clust$tot.withinss))
        #clust_smry = rbind(clust_smry, c(i, df_clust$tot.withinss))
} ## WTF, should SS decrease with increase in number of segments? Yeah, that does makes sense


### Modeling

df = df_c
library(caret)

df$Q23c1_TotalCoverageWithSpouse_IndivisualIns[is.na(df$Q23c1_TotalCoverageWithSpouse_IndivisualIns)] = 0
df$Q23c2_TotalCoverageWithSpouse_GroupIns[is.na(df$Q23c2_TotalCoverageWithSpouse_GroupIns)] = 0
df$Q25_Total_Household_Debt_Estimate[is.na(df$Q25_Total_Household_Debt_Estimate)] = 0
df$Q11_Partner_Employment_Status[df$Q11_Partner_Employment_Status == ""] = "NA"

df[1:40] %>% str()
df$Q2_Marital.Status = as.factor(df$Q2_Marital.Status)
df$Q10_Employment_Status = as.factor(df$Q10_Employment_Status)
df$Q11_Partner_Employment_Status = as.factor(df$Q11_Partner_Employment_Status)

library(randomForest)
colnames(df[1:40])
df_prediction = cbind(df[4], df[9:31])
df_prediction %>% str()
df_prediction$Q1_Age = as.integer(as.character(df_prediction$Q1_Age))
df_prediction$Q23c1_TotalCoverageWithSpouse_IndivisualIns = NULL
df_prediction$Q23c2_TotalCoverageWithSpouse_GroupIns = NULL
df_prediction$Q28_31_Information_Sources = as.integer(as.character(df_prediction$Q28_31_Information_Sources))
df_prediction$Q26_People = as.integer(as.character(df_prediction$Q26_People))
df_prediction$Q30_Event = as.integer(as.character(df_prediction$Q30_Event))
df_prediction$Quote_vs_NonBuyers = as.character(df_prediction$Quote_vs_NonBuyers)
df_prediction$Quote_vs_NonBuyers = ifelse(df_prediction$Quote_vs_NonBuyers == "Quote",1, 0)
df_prediction$Quote_vs_NonBuyers = factor(df_prediction$Quote_vs_NonBuyers)

df_prediction$Region4 = NULL

test = sample(1:nrow(df_prediction), size = 1000, replace = F)
df_prediction_test = df_prediction[test,]
df_prediction_train = df_prediction[-test,]

## Assumption
### Cost of missing out customer who receive a quote = 5 times
cost_mat = as.data.frame(rbind(c(0,0,0), c(1, 0, 5), c(0, 1, 1), c(1,1,0)))

## Random Forest
rf1 = randomForest(Quote_vs_NonBuyers~., data = df_prediction_train)
varImpPlot(rf1, sort = T, main = "Quote vs No Quote", n.var = 6)

pred_rf1 = predict(object = rf1, newdata = df_prediction_test, type = "prob")
pred_rf1 = as.data.frame(pred_rf1)

pred_rf_table = data.frame(table(df_prediction_test$Quote_vs_NonBuyers, ifelse(pred_rf1$`1` >= 0.5, 1,0)))
pred_rf_table = apply(pred_rf_table, MARGIN = 2, function(x) as.integer(as.character(x)))
pred_rf_table = data.frame(pred_rf_table)

cost_fn = function(cost_matrix = cost_mat, pred_table = pred_rf_table){
        cost = pred_table %>% 
                left_join(y = cost_mat, by = c("Var1" = "V1", "Var2" = "V2")) %>% 
                summarise(cost = sum(Freq*V3))
        print(cost)
}

cost_fn()

pred_rf_table
metrics_fn = function(pred_tbl = pred_rf_table){
        tp = pred_tbl$Freq[pred_tbl$Var1 == 1 & pred_tbl$Var2 == 1]
        tn = pred_tbl$Freq[pred_tbl$Var1 == 0 & pred_tbl$Var2 == 0]
        fp = pred_tbl$Freq[pred_tbl$Var1 == 0 & pred_tbl$Var2 == 1]
        fn = pred_tbl$Freq[pred_tbl$Var1 == 1 & pred_tbl$Var2 == 0]
        sens = tp/(tp + fn)
        spec = tn/(tn + fp)
        accuracy = (tp+tn)/sum(tp + tn + fp + fn)
        print(paste0( "Sensitivity -", sens))
        print(paste0( "Specificity -", spec))
        print(paste0( "Accuarcy -", accuracy))
}

metrics_fn()

## Logistic Regression
logistic1 = glm(Quote_vs_NonBuyers~., data = df_prediction_train, family = binomial)
pred_logistic1 = predict(logistic1, newdata = df_prediction_test, type = "response")
summary(logistic1)

pred_log1 = as.data.frame(table(df_prediction_test$Quote_vs_NonBuyers, ifelse(pred_logistic1 >= 0.5, 1,0)))
pred_log1 = apply(pred_log1, MARGIN = 2, function(x) as.integer(as.character(x)))
pred_log1 = data.frame(pred_log1)
cost_fn(pred_table = pred_log1)

metrics_fn(pred_log1)

logistic2 = glm(Quote_vs_NonBuyers~ Q1_Age + Q28_31_Information_Sources + Q24_Total_Investable_Assets + Q25_Total_Household_Debt_Estimate + Q20_Insured + Q8_Annual_Household_Income + Q26_People + Q30_Event + Q12_Education_Level, family = "binomial", data = df_prediction_train)

pred_logistic2 = predict(logistic2, df_prediction_test, type = "response")

pred_log2 = data.frame(table(df_prediction_test$Quote_vs_NonBuyers, ifelse(pred_logistic2 >= 0.5, 1,0)))

pred_log2 = apply(pred_log2, MARGIN = 2, function(x) as.integer(as.character(x)))
pred_log2 = data.frame(pred_log2)
cost_fn(pred_table = pred_log2)


## Ensemble Model
df_prediction_train$rf1 = predict(object = rf1, type = "prob")[,2]
df_prediction_train$logistic = predict(logistic1, type = "response")

df_prediction_test$rf1 = pred_rf1$`1`
df_prediction_test$logistic = pred_logistic1

objControl = trainControl(method = "repeatedcv",number = 5, repeats = 3)

rf2 = train(Quote_vs_NonBuyers~ ., data = df_prediction_train, method = "rf", trControl = objControl)

varImp(rf2)

pred_rf2 = predict(object = rf2, newdata = df_prediction_test)
pred_rf2_table = data.frame(table(df_prediction_test$Quote_vs_NonBuyers, pred_rf2))

pred_rf2_table$Var2 = pred_rf2_table$pred_rf2
pred_rf2_table$pred_rf2 = NULL
pred_rf2_table = apply(pred_rf2_table, MARGIN = 2, function(x) as.integer(as.character(x)))
pred_rf2_table = data.frame(pred_rf2_table)
cost_fn(pred_table = pred_rf2_table)

metrics_fn(pred_tbl = pred_rf2_table)


### GAMS
library(gam)

gam1 = gam(Quote_vs_NonBuyers~ s(Q1_Age) + s(Q28_31_Information_Sources) + s(Q24_Total_Investable_Assets) + s(Q25_Total_Household_Debt_Estimate) + Q20_Insured + s(Q8_Annual_Household_Income), family = "binomial", data = df_prediction_train)
plot.gam(gam1,rugplot = F, col = "deepskyblue3")

pred_gam = predict(gam1, newdata = df_prediction_test, type = "response")
pred_gam_table = data.frame(table(df_prediction_test$Quote_vs_NonBuyers, ifelse(pred_gam >= 0.5, 1,0)))

pred_gam_table = apply(pred_gam_table, MARGIN = 2, function(x) as.integer(as.character(x)))
pred_gam_table = data.frame(pred_gam_table)
cost_fn(pred_table = pred_gam_table)
metrics_fn(pred_gam_table)



