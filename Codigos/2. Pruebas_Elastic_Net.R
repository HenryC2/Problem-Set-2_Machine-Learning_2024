#------------------------------------------------------------------------------#
#----------------------------- Pruebas - Elastic Net  -------------------------#
#------------------------------------------------------------------------------#

# 0. Carga de informacion  -----------------------------------------------------
setwd(paste0(wd,"/Base"))
Train <- import(file = "Train_hogares_final.rds")
Test <- import(file = "Test_hogares_final.rds")

# 1. Re-balanceo ---------------------------------------------------------------



#2. model1 <- train(Pobre~.,
data=train,
metric = "F",
method = "glmnet",
trControl = ctrl,
family="binomial",
tuneGrid=expand.grid(
  alpha = seq(0,1,by=.5),
  lambda =10^seq(-1, -3, length = 10)
)

)


predictSample <- test   %>% 
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)   



name<- paste0("EN_lambda_", "00046", "_alpha_" , "05", ".csv") 

write.csv(predictSample,name, row.names = FALSE)


