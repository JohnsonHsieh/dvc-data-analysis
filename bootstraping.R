library(dplyr)

dat <- data.frame(count=sample(1:7, 4000, replace = TRUE), x1=rnorm(4000), x2=rnorm(4000))

dat$count_plus <- cut(dat$count, breaks=c(1,2,3,Inf), right = F, labels = c(1,2,3))

set.seed(123)
dat1 <- dat[which(dat$count_plus=="1") %>% sample(., 200, replace=TRUE),]
dat2 <- dat[which(dat$count_plus=="2") %>% sample(., 200, replace=TRUE),]
dat3 <- dat[which(dat$count_plus=="3") %>% sample(., 200, replace=TRUE),]
out <- rbind(dat1, dat2, dat3)
