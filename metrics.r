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

NDCG <- function(predictions, M, testData, ratingMatrix){
  numberOfUsers <- 943
  totalNDCG <- 0
  for(user in 1:numberOfUsers){
    pi <- order(predictions[user, ], decreasing = TRUE)
    rel <- order(ratingMatrix[user, ], decreasing = TRUE)
    consideredItems <- 0
    DCG <- 0
    IDCG <- 0
    knownItems <- as.matrix(testData[which(testData[, 1] == user), 2])
    
    for(item in pi){
      if(item %in% knownItems){
        consideredItems <- consideredItems + 1
        consumed <- 0
        rating <- testData[which(testData[, 1] == user & testData[, 2] == item), 3]
        predictedRating <- predictions[user, item]
        if(rating >= 4 & predictedRating >= 4){
          consumed <- 1
        }
        DCG <- DCG + ((2^consumed) - 1)/(log(item + 1))
        if(consideredItems >= M){
          break
        }
      }
    }
    
    consideredItems <- 0
    for(item in rel){
      if(item %in% knownItems){
        consideredItems <- consideredItems + 1
        rating <- testData[which(testData[, 1] == user & testData[, 2] == item), 3]
        consumed <- 0
        if(rating >= 4){
          consumed <- 1
        }
        IDCG <- IDCG + ((2^consumed) - 1)/log(item + 1)
        if(consideredItems >= M){
          break
        }
      }
    }
    if(!is.na(DCG/IDCG)){
      totalNDCG <- totalNDCG + DCG/IDCG
    }
  }
  NDCGk <- totalNDCG/numberOfUsers
  return(NDCGk)
}

printResults <- function(predictions, testData, threshold, ratingMatrix, M) {
  print("MAE, RMSE, REC3, REC4, PREC3, PREC4, F3, F4, NDCG")
  print(paste(
    MAE(predictions, testData),
    RMSE(predictions, testData),
    recall(predictions, testData, 3),
    recall(predictions, testData, 4),
    precision(predictions, testData, 3),
    precision(predictions, testData, 4),
    fmeasure(
      precision(predictions, testData, 3),
      recall(predictions, testData, 3)
    ),
    fmeasure(
      precision(predictions, testData, 4),
      recall(predictions, testData, 4)
    ),
    NDCG(predictions, M, testData, ratingMatrix)
    ),
    sep = ","
  )
}
