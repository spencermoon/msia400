library(dplyr)

myanova <- function(data, label){
  ### This function manually calculates the F statistic
  
  k = length(unique(label))
  n = length(data)
  
  # Manipulate inputs for calculations
  sstdf = data.frame(data = data, label = label) %>% 
    group_by(label) %>% 
    summarise(sum = sum(data), count = n()) %>%
    mutate(sst = sum^2/count)
  
  totalss = sum(data^2) - (sum(data)^2)/length(data)
  sst = sum(sstdf$sst) - (sum(data)^2)/length(data)
  sse = totalss - sst
  
  mst = sst/(k-1)
  mse = sse/(n-k)
  
  f = mst/mse
  p = 1-pf(f, df1 = (k-1), df2 = (n-k))
  
  result = ifelse(p < 0.01,
                  paste('Reject null hypothesis at a = 0.01 with p-value =', p),
                  paste('Do not reject null hypothesis at a = 0.01 with p-value =', p))
  
  print(result)
  return(f)
}


# Run an example using Tensile dataset
tensile <- read.table("Tensile.txt", header=T)
resp <- c(t(as.matrix(tensile)))
treats <- c("HC5", "HC10", "HC15", "HC20")
k <- 4
n <- 6
tm <- gl(k, 1, n*k, factor(treats))

myanova(resp, tm)