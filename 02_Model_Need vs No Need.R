### Modeling  - Need vs No Need -- Bseline - 34.5%

df = df_c
library(caret)
library(randomForest)

df$Q23c1_TotalCoverageWithSpouse_IndivisualIns[is.na(df$Q23c1_TotalCoverageWithSpouse_IndivisualIns)] = 0
df$Q23c2_TotalCoverageWithSpouse_GroupIns[is.na(df$Q23c2_TotalCoverageWithSpouse_GroupIns)] = 0
df$Q25_Total_Household_Debt_Estimate[is.na(df$Q25_Total_Household_Debt_Estimate)] = 0
df$Q11_Partner_Employment_Status[df$Q11_Partner_Employment_Status == ""] = "NA"

df[1:40] %>% str()
df$Q2_Marital.Status = as.factor(df$Q2_Marital.Status)
df$Q10_Employment_Status = as.factor(df$Q10_Employment_Status)
df$Q11_Partner_Employment_Status = as.factor(df$Q11_Partner_Employment_Status)


colnames(df[1:40])
df_prediction = cbind(df[2], df[9:31])
df_prediction %>% str()
df_prediction$Q1_Age = as.integer(as.character(df_prediction$Q1_Age))
df_prediction$Q23c1_TotalCoverageWithSpouse_IndivisualIns = NULL
df_prediction$Q23c2_TotalCoverageWithSpouse_GroupIns = NULL
df_prediction$Q28_31_Information_Sources = as.integer(as.character(df_prediction$Q28_31_Information_Sources))
df_prediction$Q26_People = as.integer(as.character(df_prediction$Q26_People))
df_prediction$Q30_Event = as.integer(as.character(df_prediction$Q30_Event))
df_prediction$Region4 = NULL

df_prediction$Segment = as.character(df_prediction$Segment)
df_prediction$Need = ifelse(df_prediction$Segment == "Segment 1 - No Need",0, 1)
df_prediction$Segment = NULL
df_prediction$Need = factor(df_prediction$Need, levels = c(0, 1))

test = sample(1:nrow(df_prediction), size = 1000, replace = F)
df_prediction_test = df_prediction[test,]
df_prediction_train = df_prediction[-test,]

## Assumption
### Cost of missing out customer who need = 5 times
cost_mat = as.data.frame(rbind(c(0,0,0), c(1, 0, 5), c(0, 1, 1), c(1,1,0)))

## Random Forest
rf1 = randomForest(Need~., data = df_prediction_train)
varImpPlot(rf1, sort = T, main = "Need vs No Need", n.var = 6)

pred_rf1 = predict(object = rf1, newdata = df_prediction_test, type = "prob")
pred_rf1 = as.data.frame(pred_rf1)

pred_rf_table = data.frame(table(df_prediction_test$Need, ifelse(pred_rf1$`1` >= 0.5, 1,0)))
pred_rf_table = apply(pred_rf_table, MARGIN = 2, function(x) as.integer(as.character(x)))
pred_rf_table = data.frame(pred_rf_table)

#### Cost function
cost_fn = function(cost_matrix = cost_mat, pred_table = pred_rf_table){
        cost = pred_table %>% 
                left_join(y = cost_mat, by = c("Var1" = "V1", "Var2" = "V2")) %>% 
                summarise(cost = sum(Freq*V3))
        print(cost)
}

cost_fn()

#### Metrics Function
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
logistic1 = glm(Need~., data = df_prediction_train, family = binomial)
pred_logistic1 = predict(logistic1, newdata = df_prediction_test, type = "response")
summary(logistic1)

pred_log1 = as.data.frame(table(df_prediction_test$Need, ifelse(pred_logistic1 >= 0.5, 1,0)))
pred_log1 = apply(pred_log1, MARGIN = 2, function(x) as.integer(as.character(x)))
pred_log1 = data.frame(pred_log1)
cost_fn(pred_table = pred_log1)

metrics_fn(pred_log1)

logistic2 = glm(Need~ Q1_Age + Q28_31_Information_Sources + Q24_Total_Investable_Assets + Q25_Total_Household_Debt_Estimate + Q20_Insured + Q8_Annual_Household_Income + Q26_People + Q30_Event + Q12_Education_Level, family = "binomial", data = df_prediction_train)

pred_logistic2 = predict(logistic2, df_prediction_test, type = "response")

pred_log2 = data.frame(table(df_prediction_test$Need, ifelse(pred_logistic2 >= 0.5, 1,0)))

pred_log2 = apply(pred_log2, MARGIN = 2, function(x) as.integer(as.character(x)))
pred_log2 = data.frame(pred_log2)
cost_fn(pred_table = pred_log2)


## Ensemble Model
df_prediction_train$rf1 = predict(object = rf1, type = "prob")[,2]
df_prediction_train$logistic = predict(logistic1, type = "response")

df_prediction_test$rf1 = pred_rf1$`1`
df_prediction_test$logistic = pred_logistic1

objControl = trainControl(method = "repeatedcv",number = 5, repeats = 3)

rf2 = train(Need~ ., data = df_prediction_train, method = "rf", trControl = objControl)

varImp(rf2)

pred_rf2 = predict(object = rf2, newdata = df_prediction_test)
pred_rf2_table = data.frame(table(df_prediction_test$Need, pred_rf2))

pred_rf2_table$Var2 = pred_rf2_table$pred_rf2
pred_rf2_table$pred_rf2 = NULL
pred_rf2_table = apply(pred_rf2_table, MARGIN = 2, function(x) as.integer(as.character(x)))
pred_rf2_table = data.frame(pred_rf2_table)
cost_fn(pred_table = pred_rf2_table)

metrics_fn(pred_tbl = pred_rf2_table)


### GAMS
library(gam)

gam1 = gam(Need~ s(Q1_Age) + s(Q28_31_Information_Sources) + s(Q24_Total_Investable_Assets) + s(Q25_Total_Household_Debt_Estimate) + Q20_Insured + s(Q8_Annual_Household_Income), family = "binomial", data = df_prediction_train)
plot.gam(gam1,rugplot = F, col = "deepskyblue3")

pred_gam = predict(gam1, newdata = df_prediction_test, type = "response")
pred_gam_table = data.frame(table(df_prediction_test$Need, ifelse(pred_gam >= 0.5, 1,0)))

pred_gam_table = apply(pred_gam_table, MARGIN = 2, function(x) as.integer(as.character(x)))
pred_gam_table = data.frame(pred_gam_table)
cost_fn(pred_table = pred_gam_table)
metrics_fn(pred_gam_table)



