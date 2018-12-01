
rm(list=ls())

fun <- function(x,seed){
  seed <- as.numeric(seed)
  set.seed(seed); seed2 <- sample(0:9)
  y <- apply(outer(as.numeric(strsplit(paste(x),'')[[1]]),seed2,'=='),1,which)-1
  list(y=log(as.numeric(paste(y,collapse='')) * seed),seed=nchar(x)+10^(-nchar(seed))*seed)
}

nuf <- function(y,seed){
  x.len <- floor(seed)
  seed <- (seed-x.len) * 10^(nchar(seed)-nchar(x.len)-1)
  y <- round(exp(y)/seed,0)
  y <- paste(c(rep(0,x.len - nchar(y)),y),collapse='')
  set.seed(seed); seed2 <- sample(0:9)
  x <- seed2[as.numeric(strsplit(y,'')[[1]])+1]
  as.numeric(paste(x,collapse=''))
}

rand <- function(i){
  as.numeric(paste(sample(0:9,i,T),collapse=''))
}

test <- function(i){
  testseed <- rand(5)
  testx <- rand(11)
  testy <- fun(testx,testseed)$y
  testseed2 <- fun(testx,testseed)$seed
  testx2 <- nuf(testy,testseed2)
  c(seed=testseed,seed2=testseed2,x=testx,y=testy,x2=testx2,val=(testx==testx2))
}

t(sapply(1:10,test))

#######################

x <- 92700512287
seed <-  98481
fun(x,seed)
y <- fun(x,seed)$y
seed <- fun(x,seed)$seed
