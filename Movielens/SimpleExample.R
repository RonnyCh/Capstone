
## Simple example to resolve this movielen issue
library(dplyr)
library(tidyverse)
#### understanding matrix sweep
user1 <- c(4,4,5,4)
user2 <- c(4,4,4,4)
mymat <- rbind(user1,user2)




## convert to dataframe
mydf <- data.frame(mymat)
mydf$user <- rownames(mydf)
mydf <- gather(mydf,movie,rating,-user)

## movie avg
mu = mean(mydf$rating)
movieAvg <- mydf %>% group_by(movie) %>% summarise(b_m = mean(rating-mu))
userAvg <- mydf %>% left_join(movieAvg,by='movie') %>% group_by(user) %>% summarise(b_u = mean(rating-b_m-mu))

# prediction
pred <- mydf %>% left_join(movieAvg,by='movie') %>% left_join(userAvg,by='user') %>% mutate(mu=mu,prediction=mu+b_u+b_m, residual=rating-prediction)



# working out residual using matrix factorisation
t <- mymat

t <- sweep(t,2,colMeans(t))
t <- sweep(t,1,rowMeans(t))
t


# compare result from method 1 vs matrix factorisatio showing same residual
# for example below, both predicting user 1 movie1
pred %>% filter(user=='user1' & movie=='X2')
t[1,2]


# so I resolve two methods are giving me same residual.
# now why can't we have zero residual??
pred %>% arrange(user,movie)
hist(pred$residual)

# test lambda as a mapping of pq
pq <- pred %>% select(user,movie,res=residual)

# apply residual
pred %>% left_join(pq,by=c('user','movie')) %>% mutate(final=prediction + res)



# testing clustering user

user10 <- edx %>% filter(userId==10) 
user2 <- edx %>% filter(userId==2) 
user3 <- edx %>% filter(userId==3) 
user4 <- edx %>% filter(userId==4)
all <- rbind(user2,user3,user4,user10)


all <- all %>% select(userId,movieId,rating) %>% spread(movieId,rating) %>% as.matrix()
rownames(all)<- all[,1]
all <- all[,-1]


all[is.na(all)] <- 0

k <- kmeans(all, centers = 2, nstart=5)
groups <- k$cluster
split(names(groups), groups)




## now i know how to put residual... how do we put this to big data.... 
# probably need to split like... residual on a movie first and see if it works...

library(dplyr)
mytrain %>% group_by(userId) %>% summarize(mean(rating))

