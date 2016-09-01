library(magrittr) # 用 %>% 寫R code，詳見 https://goo.gl/vP6qW2
library(dplyr) # 資料處理的套件，詳見 vignette("introduction", package="dplyr")
library(reshape2) # 資料處理套，詳見 http://seananderson.ca/2013/10/19/reshape.html

# 讀取檔案
out7 <- read.csv("受害者村里地圖_0831版.csv")

# 把6個案大類換成4類
out <- filter(out7, !is.na(lat)) %>%
  mutate(location=paste0(T_Name, V_Name)) %>%
  mutate(class=factor(個案大類, 
                          levels = c("兒少保護", "家暴加害人處遇", "老人保護", "其他家虐", "親密關係", "兄弟姊妹間暴力"),
                          labels = c("兒少保護", "其他家虐", "老人保護", "其他家虐", "親密關係", "其他家虐")) %>% as.character)

# 計算各里 (location) x 個案分類 (class) 下的案件數
dat <- group_by(out, location, class) %>%
  summarise(count=n()) %>% 
  group_by() %>% arrange(-count)

# 把 dat 物件轉換成 wide table
dat.summ <- dcast(dat, formula = location~class, fun.aggregate = sum, value.var = "count")

colSums(dat.summ[,-1]) # 檢查各類總數

rownames(dat.summ) <- dat.summ[,1]
hc <- dist(dat.summ[,-1]) %>% hclust()

par(family="STHeiti") # Mac繪圖指定中文字型
plot(hc, cex=0.5)

heatmap(as.matrix(dat.summ[,-1])) # hratmap

gp <- cutree(hc, 5) # 把階層分群樹橫切一刀分成5群

baseline <- dat.summ[,-1] %>% colMeans() # 每一個里的平均案件數

# a1~a5 每一群的案件數odds (各群平均案件數/總平均案件數）
a1 <- dat.summ[which(gp==1),-1] %>% {colMeans(.)/baseline} %>% round(3)
a2 <- dat.summ[which(gp==2),-1] %>% {colMeans(.)/baseline} %>% round(3)
a3 <- dat.summ[which(gp==3),-1] %>% {colMeans(.)/baseline} %>% round(3)
a4 <- dat.summ[which(gp==4),-1] %>% {colMeans(.)/baseline} %>% round(3)
a5 <- dat.summ[which(gp==5),-1] %>% {colMeans(.)/baseline} %>% round(3)

# b1~b5 找出個群代表性最高的里 (總案件數最多)
b1 <- subset(dat.summ, gp==1, -1) %>% rowSums() %>% which.max %>% names
b2 <- subset(dat.summ, gp==2, -1) %>% rowSums() %>% which.max %>% names
b3 <- subset(dat.summ, gp==3, -1) %>% rowSums() %>% which.max %>% names
b4 <- subset(dat.summ, gp==4, -1) %>% rowSums() %>% which.max %>% names
b5 <- subset(dat.summ, gp==5, -1) %>% rowSums() %>% which.max %>% names

# 展示結果
mat <- rbind(a1, a2, a3, a4, a5) %>% cbind(count=table(gp)) %>%
  data.frame(., typical=rbind(b1,b2,b3,b4,b5))

mat

# Final output
#    兒少保護 老人保護 其他家虐 親密關係 count    typical
# a1    0.743    0.832    0.664    0.701   277     不能說
# a2    1.320    1.303    1.617    1.278   129     不可說
# a3    2.045    2.111    2.119    2.164    20     不得說
# a4    1.156    0.437    0.650    1.720    29     不要說
# a5    5.365    2.533    2.263    3.844     1     不行說

