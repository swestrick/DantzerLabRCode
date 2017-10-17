## combine all the files into one R dataframe "odbaData"

folder <- "~/Desktop/2016AXYdata/ODBA-Output/Lactation/"  ## change to ODBA folder needed
file_list <- list.files(path = folder, pattern = "*.csv")
odbaData <-
  do.call("rbind",
          lapply(file_list,
                 function(x)
                   read.csv(paste(folder, x, sep= ""),
                            stringsAsFactors = FALSE)))
odbaData$trt <-as.factor(odbaData$trt)
odbaData$SqID <- as.factor(odbaData$SqID)
odbaData$datetime <- as.POSIXct(odbaData$datetime)

# test simple model with temporal autocorrelation to test for differences between groups
# model: activity ~ treatment + temporal autocorrelation correction
# final model goal: activity ~ individualID (dummy variables) + treatment + time of day + temporal autocorrelation correction 

m <- lme(odba ~ trt,
            data= odbaData, method="REML",
            random = ~ 1|datetime, 
            correlation=corCAR1(form=~datetime))
summary(m)

