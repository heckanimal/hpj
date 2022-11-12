
#These global objects are for testing only. comment these before saving
#alpha<-.05
#x<-lo_hist$pts100
#y<-lo_hist$date
#by<-lo_hist$team_abv
#groups<-20
#reps<-3
#deg = 1

#define function to output squared errors from loess sampling
lo_rmse<-function(alpha,x,y,by,groups,reps,deg){

t0 <- Sys.time()

#generate list of unique values from each
each <- unique(by)
#remove any NA's from each
each <- each[which(!is.na(each))]
#order each
each <- each[order(each)]
#use function parameters to create data frame to be used within function.
#put by group, dependent, and independent variables in data frame
fun_data.1<-data.frame(by,x,y)

for(r in 1:reps){

  #for(i in 1:2){
  for(i in 1:length(each)){
    
    #filter data by each
    fun_data.1 %>%
      filter(by == each[i]) %>%
      #remove obsolete "by" column
      #select(-by) %>%
      #order by independent variable
      arrange(y) %>%
      #add index column "y"
      mutate(y_ind = 1:nrow(.)) %>%
      #create variable assigning sample groups to each observation
      mutate(lo_group = sample(1:groups,nrow(.),replace = TRUE)) -> fun_data
    
    #add index column "y"
    #fun_data %>% 
    #  mutate(y_ind = 1:nrow(fun_data)) %>%
    #  select(by,x,y,y_ind,lo_group) -> fun_data
    
    #create training data frame
    fun_data_train <- fun_data
    #create test data frame for y variable
    fun_data_test_y <- fun_data
    #create test data frame for x variable
    fun_data_test_x <- fun_data
    
    #create global object referring to the column positions to be added to train and test data frames
    c = 1:groups + 5
    
    
    for(k in 1:groups){
      #create k number of train and test samples for k-fold cross validation
      fun_data_train[c[k]] <- ifelse(fun_data_train$lo_group == k,NA,fun_data_train$x)
      fun_data_test_y[c[k]] <- ifelse(fun_data_test_y$lo_group == k,fun_data_test_y$y_ind,NA)
      fun_data_test_x[c[k]] <- ifelse(fun_data_test_x$lo_group == k,fun_data_test_x$x,NA)
    }
    
    #retrieve names of training variables from training data frame
    train_cols <- colnames(fun_data_train[c])
    #create data frame of test observations from test y values
    test_y_cols <- fun_data_test_y[c]
    #create data frame of test observations from test x values
    test_x_cols <- fun_data_test_x[c]
    
    #run loess function on each training sample, producing a list of k number of regression outputs
    #loess specifications set by function parameters
    lo_list <- lapply(train_cols,function(traincol)
      loess(as.formula(paste0(traincol,' ~ y_ind')),
            data = fun_data_train,
            span = alpha,
            degree = deg))
    
    #predict the outcomes of each of the k models created in the previous step and store in prediction matrix
    predict_mat <- as.data.frame(mapply(function(model,test_sample){
      predict(model,newdata = test_sample)},
      model=lo_list,test_sample=test_y_cols))
    
    #subtract predicted value from observed x values and store in temp residual matrix
    resid_mat <- test_x_cols - predict_mat
    #add columns back to residual matrix
    data.frame(fun_data,r,resid_mat) %>%
    #reduce residual columns to single column
    #pivot to long form
      pivot_longer(cols = c+1,names_to = 'k_group',values_to = 'resid') %>%
      #keep only one row per game
      group_by(by,y) %>%
      arrange(by,y,resid) %>%
      slice(1) %>%
      ungroup() %>%
      select(-k_group) -> resid_temp
    
    #add id and name columns to residual matrix
    #resid_temp %>%
    #  mutate(rep = r,by = each[i]) %>%
    #  select(rep,by,1:k) -> resid_temp
    #change column names
    #colnames(resid_temp) <- c('rep','by',paste0('r',1:k))
    
    #if first in "each" then store temp data frame as permanent resid data frame
    if(i == 1){
      resid_df_r <- resid_temp
    #otherwise bind the temp data frame to the permanent resid data frame
    } else {
      resid_df_r <- rbind(resid_df_r,resid_temp)
    }
    
  }
  
  if(r == 1){
    resid_df <- resid_df_r
  } else {
    resid_df <- rbind(resid_df,resid_df_r)
  }
  
  resid_df %>% arrange(by,y,r) -> resid_df
  
  
  print(paste0('rep ',r,' complete ',Sys.time() - t0))
  
}
  
resid_df


#resid_mat <- as.matrix(resid_df[3:(k+2)])
#rmse <-   mean(sqrt(resid_mat^2),na.rm = TRUE)

#if(r == 1){
#  RMSEs<-rmse
#}else{
#  RMSEs[r]<-rmse
#}

#print(paste0('rep ',r,' complete'))
#}

#error <- mean(RMSEs)

#out <- list(error,RMSEs)

#out

#t2 <- Sys.time()
#time <- t2-t1

#out <- list(error,time)

#out[[1]]

}


