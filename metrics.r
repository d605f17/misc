makeTestdata <- function(filename){
  testdata <- read_delim(paste(getwd(), "/ml-100k/", filename, ".test", sep = ""),
                         "\t", escape_double = FALSE, trim_ws = TRUE, 
                         col_names = c("userId", "movieId", "rating", "timestamp"),
                         col_types = cols(
                           userId = col_integer(),
                           movieId = col_integer(),
                           rating = col_integer(),
                           timestamp = col_integer()
                         )
  );
  return(as.matrix(testdata))
}

RMSE <- function(predictions, testData){
  squaredError <- 0
  
  for(row in 1:nrow(testData)){
    prediction <- predictions[as.numeric(testData[row, 1]), as.numeric(testData[row, 2])]
    
    squaredError <- squaredError + (as.numeric(testData[row, 3]) - prediction)^2
  }
  
  return(sqrt(1/nrow(testData) * squaredError))
}

MAE <- function(predictions, testData){
  absError <- 0
  
  for(row in 1:nrow(testData)){
    prediction <- predictions[as.numeric(testData[row, 1]), as.numeric(testData[row, 2])]
    
    absError <- absError + abs(as.numeric(testData[row, 3]) - prediction)
  }
  
  return(1/nrow(testData) * absError)
}

precision <- function(predictions, testData, threshhold){
  numberOfUsers <- nrow(predictions)
  
  tp <- 0
  fp <- 0
  
  for(u in 1:numberOfUsers){
    known <- testData[which(testData[, 1] == u), ]
    
    for(k in 1:10){
      prediction <- predictions[u, known[k, 2]]
      actualRating <- known[k, 3]
      
      if(prediction >= threshhold & actualRating >= threshhold) #Recommended & Used
        tp <- tp + 1
      if(prediction >= threshhold & actualRating < threshhold) #Recommended & Unused
        fp <- fp + 1
    }
  }
  return(tp/(tp + fp))
}

recall <- function(predictions, testData, threshhold){
  numberOfUsers <- nrow(predictions)
  
  tp <- 0
  fn <- 0
  
  for(u in 1:numberOfUsers){
    known <- testData[which(testData[, 1] == u), ]
    
    for(k in 1:10){
      prediction <- predictions[u, known[k, 2]]
      actualRating <- known[k, 3]
      
      if(prediction >= threshhold & actualRating >= threshhold) #Recommended & Used
        tp <- tp + 1
      if(prediction < threshhold & actualRating >= threshhold) #Not recommended & Used
        fn <- fn + 1
    }
  }
  return(tp/(tp + fn))
}

fmeasure <- function(precision, recall){
  return((2*precision*recall)/(precision + recall))
}