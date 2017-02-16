my_vec <- c(3,1,2)
sort(my_vec)
sort(my_vec,decreasing=FALSE)

my.sort <- function(input_vector,decreasing=FALSE){
  for(i in 1:(length(input_vector)-1)){
    for( j in(i+1):length(input_vector)){
      temp <-  input_vector[i]
      if(input_vector[i] > input_vector[j] & decreasing==FALSE){
        input_vector[i] <- input_vector[j]
        input_vector[j] <-  temp
      }else{
          temp <-  input_vector[i]
          if(input_vector[i] < input_vector[j] & decreasing==T){
            input_vector[i] <- input_vector[j]
            input_vector[j] <-  temp
          }
      }
    }
  }
  
return(input_vector)
  }

set.seed(87)
rand_vector <- ceiling(runif(10) * 100)
rand_vector


my.sort(rand_vector,decreasing=F)




#method1
my.sd <- function(input_vector) {
  sum2 <- 0
  count <- 0
  for (i in input_vector) {
    sum2 <- sum2 + (i-mean(input_vector))^2
  } 
  for (i in input_vector) {
    count <- count + 1
  }
  return(sqrt(sum2 /(count -1)))
}
my_vector <- 1:10
another_vector <- 1:100

my.sd(my_vector)
my.sd(another_vector)

#method2
my.sd <- function(input_vector) {
  sum2 <- 0
  for (i in input_vector) {
    sum2 <- sum2 + (i-mean(input_vector))^2
  } 
  return(sqrt(sum2 /((length(input_vector)) -1)))
}
my_vector <- 1:10
another_vector <- 1:100

my.sd(my_vector)
my.sd(another_vector)


