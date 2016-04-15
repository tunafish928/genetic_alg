# list of core inc ids with tdd > 15
# SELECT trader, count(DISTINCT(DATE(modified))) AS td_day FROM LTS4INC_Global_FX_S3.CHILD_ORDER_AUDIT WHERE trader NOT LIKE '%-R201%'
# GROUP BY trader HAVING td_day >15;
core_inc_apr5 <- read.csv("~/incubatee/core_inc_apr5 - Sheet1.csv", header = T, sep = ",") # 127 x 2, april 5
curr_inc_apr1 <- read.table("~/incubatee/inctb_ord.xls", header = T, sep="\t") # 364x10, march 25 
## 127 / 364 core inc perf
core_inc_apr5_perf <- merge(core_inc_apr5, curr_inc_apr1, by.x = "trader", by.y = "USER_ID", all = F)
core_inc_apr5_perf <- core_inc_apr5_perf[ ,-3]
curr_inc_apr1 <- curr_inc_apr1[,-1]


## 1.
## map inc_id to sim_id for all current inc 
map <- read.table("~/incubatee/av_incsim_map.xls", header = T, sep="\t") 
map <- map[,2:4] # 208 x 3
map[,1] <- as.character(map[,1])
map[,2] <- as.character(map[,2])
map[,3] <- as.character(map[,3])
core_inc_apr5_perf <- merge(core_inc_apr5_perf, map, by.x = "trader", by.y = "inc_id", all = F)
## 104 out of 127 all core inc got mapped


## 2.
## remove inc who never traded
core_inc_apr5_perf <- subset(core_inc_apr5_perf, ACCOUNT_VALUE!=10000)
# none got removed

## 3.
## curr_inc_apr1 includes fdt_selected inc.
fdt_sel_inc <- read.csv("~/incubatee/fdt_choose_inc.csv", header = T, sep = ",") # 105 x 2, feb04_ch, feb29_global
fdt_sel_inc_traded <- merge(fdt_sel_inc, core_inc_apr5_perf, by.x = "ID", by.y = "sim_id", all = F) 
# 7 out of 105 fdt selected inc are core inc (inc tdd > 15)
fdt_sel_inc_traded[,1] <- as.character(fdt_sel_inc_traded[,1])
fdt_sel_idx <- sapply(1:length(fdt_sel_inc_traded[,1]), function(i) {
  which(core_inc_apr5_perf[,11]==fdt_sel_inc_traded[i,1])
})
## label the 7 fdt selected inc in core_inc_apr5_perf
core_inc_apr5_perf$status="prev_sel"
core_inc_apr5_perf$status[fdt_sel_idx] <- "fdt_sel"

colnames(core_inc_apr5_perf) <- c("inc_id", "tdd", "inc_alltime_pnl", "inc_account_value", 
                             "inc_weekly_pnl", "inc_alltime_roi", "inc_alltime_rank",
                             "inc_5d_pnlR", "inc_alltime_sharpe", "inc_weekly_trcnt",
                             "sim_id", "region", "inc_status")
## 105 core inc as of apr5


## 4a. prepare global data
## Run inc_ft.py file from AWS ~/incubatee/ on AWS python
gfx_inc_ftscores_229 <- read.csv("~/incubatee/gfx_inc_ftscores_229.csv", header = T, sep = "\t")
gfx_inc_ftscores_229 <- gfx_inc_ftscores_229[!duplicated(gfx_inc_ftscores_229$user_id), ] # 51 x 23
gfx_inc_ftscores_229 <- gfx_inc_ftscores_229[-4, ]
gfx_inc_ftscores_229[41,23] <- 86.05 # rushlee = 86.05
gfx_inc_ftscores_229[40,23] <- 82.1

## 4b. prepare china data
## Run chfx_inc_ftscores204.py file from aliyun ~/jack/workspace/risk_prof/chfx/ on alilyun python
## saved as "chfx_inc_ftscores_204.csv" on aliyun ~/jack/workspace/risk_prof/chfx/
## also saved as "chfx_inc_ftscores_204.csv" on AWS ~/sainan/incubatee/
chfx_inc_ftscores_204 <- read.csv("~/incubatee/chfx_inc_ftscores_204.csv", header = T, sep = "\t") # 44 x 23
inc_ftscores_feb <- rbind(chfx_inc_ftscores_204, gfx_inc_ftscores_229) # (44+51) x 23 = 95 x 23
inc_ftscores_feb <- inc_ftscores_feb[,-1] # 95 x 22
colnames(inc_ftscores_feb) <- c("user_id","ft_avg_ngt_op","ft_std_op_qty","ft_streak",       
                                "ft_bgw","ft_mdd","ft_tdlf","ft_bgl","ft_df_qty",        
                                "ft_cp_streak","ft_std_op_dr","ft_avg_dr","ft_std_neg_pnlr","ft_tmdd",          
                                "ft_login_cnt_pday","ft_avg_max_op","ft_roll_price","ft_shp_rt","ft_tdd",          
                                "ft_trade_cnt_pday","sim_activity","sim_fdt")                          


#######################################################
## 105 core inc as of apr5
## only 59 core inc had corresonding fdt scores in feb
all_incsim_info1 <- merge(inc_ftscores_feb, core_inc_apr5_perf, by.x = "user_id", by.y = "sim_id", all = F) # 59 x 34

length(which(all_incsim_info1[,34]=="fdt_sel")) 
length(which(all_incsim_info1[,34]=="fdt_sel" & all_incsim_info1[32]=="china"))
# 7 are fdt_selected, 7 from sim china_fx, 0 from inc global_fx

write.table(all_incsim_info1, file="~/incubatee/core_incsim_info_feb.xls", sep = "\t", col.names = NA)



## 5. run genetic algorithm
## try 1
## all fts = X, sim_fdt = y
mydata1 <- cbind(all_incsim_info1[,2:20], all_incsim_info1[,22], all_incsim_info1[,28]) 
colnames(mydata1) <- c("ft_avg_ngt_op","ft_std_op_qty","ft_streak","ft_bgw",
                       "ft_mdd","ft_tdlf","ft_bgl","ft_df_qty",            
                       "ft_cp_streak","ft_std_op_dr","ft_avg_dr","ft_std_neg_pnlr",      
                       "ft_tmdd","ft_login_cnt_pday","ft_avg_max_op","ft_roll_price",        
                       "ft_shp_rt","ft_tdd","ft_trade_cnt_pday","sim_fdt", "inc_roi")
inc_roi <- all_incsim_info1[,28]
## fit linear model
## range of the search space can be obtained from a preliminary OLS estimation of coef and their standard errors
# pwts <- c( -0.25,0.5,0.05,
# 0.1,0.2,0.3,0.1,0.25,0.02,
# 0.5,0.05,0.1,0.2,
# 0.2,-0.25,0.25,
# 0.25,0.2,0.3)
ols <- glm(inc_roi ~ ft_avg_ngt_op + ft_std_op_qty + ft_streak 
            + ft_bgw + ft_mdd + ft_tdlf + ft_bgl + ft_df_qty + ft_cp_streak
            + ft_std_op_dr + ft_avg_dr + ft_std_neg_pnlr + ft_tmdd
            + ft_login_cnt_pday + ft_avg_max_op + ft_roll_price
            + ft_shp_rt + ft_tdd + ft_trade_cnt_pday, data = mydata1)
y <- model.response(model.frame(ols))
X <- model.matrix(ols)
se.coef <- sqrt(diag(vcov(ols)))
min <- coef(ols) - 3 * se.coef
max <- coef(ols) + 3 * se.coef
## define fitness function to maximize
rob <- function(b)
  sum(cor(inc_roi, X %*% b, method = "kendall")) 
# kendall give neg cor
# where inc_roi = roi during inc, X %*% b = new_fdt_score computed from ols1 weights
GA <- ga(type = "real-valued", fitness = rob, min = min, max = max,
          popSize = 500, pmutation = 0.05, maxiter = 200, run = 100)
summary(GA)
plot(GA)

GA_wts <- summary(GA)[11][[1]][108,]
GA_wts <- sapply(1:length(GA_wts), function (i) {
  GA_wts[i][[1]]
})
GA_wts <- GA_wts[-1]

GA_fdtscores <- X[,2:20] %*% GA_wts
cor(inc_roi, GA_fdtscores, method = "kendall")
cor.test(inc_roi, GA_fdtscores, method = "kendall")

all_incsim_info1 <- cbind(all_incsim_info1, GA_fdtscores)



panel.cor <- function(x, y, digits=2, cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, method = "kendall")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y, method = "kendall")
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  text(0.5, 0.25, paste("r=",txt))
  text(.5, .75, Signif)
}

panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}

panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE, breaks = 50)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}


# fdt score vs roi raw
pairs(all_incsim_info1[,c(35,28)],
      lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist) 












rob2 <- function(b)
  -sum((X %*% b - inc_roi)^2)/59 
# kendall give neg cor
# where inc_roi = roi during inc, X %*% b = new_fdt_score computed from ols1 weights
GA2 <- ga(type = "real-valued", fitness = rob2, min = min, max = max,
         popSize = 500, pmutation = 0.05, maxiter = 400, run = 100)
summary(GA2)
plot(GA2)

GA2_wts <- summary(GA2)[11][[1]][1,]
GA2_wts <- sapply(1:length(GA2_wts), function (i) {
  GA2_wts[i][[1]]
})
GA2_wts <- GA2_wts[-1]

GA2_fdtscores <- X[,2:20] %*% GA2_wts
cor(inc_roi, GA2_fdtscores, method = "kendall") # 0.3325541
cor.test(inc_roi, GA2_fdtscores, method = "kendall") # 0.0001985

all_incsim_info1 <- cbind(all_incsim_info1, GA2_fdtscores)

# fdt score vs roi raw
pairs(all_incsim_info1[,c(36,28)],
      lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist) 



### transform GA weighted (simple linear comb of wts) FDT scores onto [0,100] scale
## y = ax+b, where x=original value, y=newly mapped value in [0,100]
## min=0, max=100, low=min(GA2_fdtscores), high=max(GA2_fdtscores)
score_map.fun <- function (min, max, low, high, x) {
  a = (max - min) / (high - low)
  b = (min*high - max*low) / (high - low)
  y = a*x + b
  options(scipen=999)
  df = data.frame(cbind(x, y))
  return(df)
}
max = 100; min = 0
GA2_fdtscores_mapped <- score_map.fun(min = min, max = max, 
                                      high = max(GA2_fdtscores), 
                                      low = min(GA2_fdtscores), 
                                      x = GA2_fdtscores)[,2] 
cor(inc_roi, GA2_fdtscores_mapped, method = "kendall") # 0.3325541
cor.test(inc_roi, GA2_fdtscores_mapped, method = "kendall") # 0.0001985

all_incsim_info1 <- cbind(all_incsim_info1, GA2_fdtscores_mapped)

# fdt score vs roi raw
pairs(all_incsim_info1[,c(37,28)],
      lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist) 











### change rob, parameters
rob3 <- function(b)
  sum(cor(inc_roi, X %*% b, method = "kendall")) 
# where inc_roi = roi during inc, X %*% b = new_fdt_score computed from ols1 weights
GA3 <- ga(type = "real-valued", fitness = rob3, min = min, max = max,
          popSize = 500, pmutation = 0.05, maxiter = 400, run = 100)
summary(GA3)
plot(GA3)

GA3_wts <- summary(GA3)[11][[1]][51,]
GA3_wts <- sapply(1:length(GA3_wts), function (i) {
  GA3_wts[i][[1]]
})
GA3_wts <- GA3_wts[-1]

GA3_fdtscores <- X[,2:20] %*% GA3_wts
cor(inc_roi, GA3_fdtscores, method = "kendall") # 0.4436
cor.test(inc_roi, GA3_fdtscores, method = "kendall") # z = 4.9635, p-value = 0.0000006924

max = 100; min = 0
GA3_fdtscores_mapped <- score_map.fun(min = min, max = max, 
                                      high = max(GA3_fdtscores), 
                                      low = min(GA3_fdtscores), 
                                      x = GA3_fdtscores)[,2] 
cor(inc_roi, GA3_fdtscores_mapped, method = "kendall") # 0.4436
cor.test(inc_roi, GA3_fdtscores_mapped, method = "kendall") # z = 4.9635, p-value = 0.0000006924
all_incsim_info1 <- cbind(all_incsim_info1, GA3_fdtscores_mapped)

# fdt score vs roi raw
pairs(all_incsim_info1[,c(38,28)],
      lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist) 







rob3 <- function(b)
  sum(cor(inc_roi, X %*% b, method = "kendall")) 
# where inc_roi = roi during inc, X %*% b = new_fdt_score computed from ols1 weights
GA4 <- ga(type = "real-valued", fitness = rob3, min = min, max = max,
          popSize = 500, pmutation = 0.05, pcrossover=0.8, maxiter = 400, run = 100)
summary(GA4)
plot(GA4)

GA4_wts <- summary(GA4)[11][[1]][77,]
GA4_wts <- sapply(1:length(GA4_wts), function (i) {
  GA4_wts[i][[1]]
})
GA4_wts <- GA4_wts[-1]

GA4_fdtscores <- X[,2:20] %*% GA4_wts
cor(inc_roi, GA4_fdtscores, method = "kendall") # 0.4646
cor.test(inc_roi, GA4_fdtscores, method = "kendall") # z = 5.1989, p-value = 0.0000002005

max = 100; min = 0
GA4_fdtscores_mapped <- score_map.fun(min = min, max = max, 
                                      high = max(GA4_fdtscores), 
                                      low = min(GA4_fdtscores), 
                                      x = GA4_fdtscores)[,2] 
cor(inc_roi, GA4_fdtscores_mapped, method = "kendall") # 0.4646
cor.test(inc_roi, GA4_fdtscores_mapped, method = "kendall") # z = 5.1989, p-value = 0.0000002005
all_incsim_info1 <- cbind(all_incsim_info1, GA4_fdtscores_mapped)
# fdt score vs roi raw
pairs(all_incsim_info1[,c(39,28)],
      lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist) 


# pcrossover: prob of crossover between pairs of chromosomes. Typically it is big, default=0.8
# elitism: # of best fitness individuals to survive at each generation, default top 5% individuals will survive at each iteration.
# elitism = max(1, round(popSize * 0.05))
# ga output results:
# best: best fitness value of 1 individual at each iteration of the GA search
# mean: avg fitness value of all individuals at each iteration of the GA search
# fitnessValue: best fitness value found by the GA search. At convergence=fitness evaluated at the solution string(s)
# solution: 
# A matrix of solution strings, with as many rows as the number of solutions found
# and as many columns as the number of decision variables.






rob3 <- function(b)
  sum(cor(inc_roi, X %*% b, method = "kendall")) 
# where inc_roi = roi during inc, X %*% b = new_fdt_score computed from ols1 weights
GA5 <- ga(type = "real-valued", fitness = rob3, min = min, max = max,
          popSize = 500, pmutation = 0.15, pcrossover=0.7, maxiter = 400, run = 100)
summary(GA5)
plot(GA5)

GA5_wts <- summary(GA5)[11][[1]][25,]
GA5_wts <- sapply(1:length(GA5_wts), function (i) {
  GA5_wts[i][[1]]
})
GA5_wts <- GA5_wts[-1]

GA5_fdtscores <- X[,2:20] %*% GA5_wts
cor(inc_roi, GA5_fdtscores, method = "kendall") # 0.495
cor.test(inc_roi, GA5_fdtscores, method = "kendall") # z = 5.5389, p-value = 0.00000003043

max = 100; min = 0
GA5_fdtscores_mapped <- score_map.fun(min = min, max = max, 
                                      high = max(GA5_fdtscores), 
                                      low = min(GA5_fdtscores), 
                                      x = GA5_fdtscores)[,2] 
cor(inc_roi, GA5_fdtscores_mapped, method = "kendall") # 0.4646
cor.test(inc_roi, GA5_fdtscores_mapped, method = "kendall") # z = 5.1989, p-value = 0.0000002005
plot(inc_roi, GA5_fdtscores_mapped)





############## max cor for >1 response (ie. inc_roi + sharpe ratio) ######################
inc_sharpe <- all_incsim_info1[,31]
a1 = 0.5; a2=0.5
rob4 <- function(b)
  (a1*sum(cor(inc_roi, X %*% b, method = "kendall"))) + (a2*sum(cor(inc_sharpe, X %*% b, method = "kendall")))
# where inc_roi = roi during inc, X %*% b = new_fdt_score computed from GA weights
GA6 <- ga(type = "real-valued", fitness = rob4, min = min, max = max,
          popSize = 500, pmutation = 0.15, pcrossover=0.7, maxiter = 400, run = 100)
summary(GA6)
plot(GA6)

GA6_wts <- summary(GA6)[11][[1]][30,]
GA6_wts <- sapply(1:length(GA6_wts), function (i) {
  GA6_wts[i][[1]]
})
GA6_wts <- GA6_wts[-1]

aa=data.frame(cbind(colnames(X[,2:20]),GA6_wts))
aa <- aa[order(aa[,2], decreasing = T), ]
row.names(aa) <- NULL

### fdtscores vs inc_roi
GA6_fdtscores <- X[,2:20] %*% GA6_wts
cor(inc_roi, GA6_fdtscores, method = "kendall") # 0.48
cor.test(inc_roi, GA6_fdtscores, method = "kendall") # z = 5.3558, p-value = 8.516e-08
plot(inc_roi, GA6_fdtscores)
### fdtscores vs inc_sharpe
cor(inc_sharpe, GA6_fdtscores, method = "kendall") # 0.35
cor.test(inc_sharpe, GA6_fdtscores, method = "kendall") # z = 3.891, p-value = 9.983e-05

max = 100; min = 0
GA6_fdtscores_mapped <- score_map.fun(min = min, max = max, 
                                      high = max(GA6_fdtscores), 
                                      low = min(GA6_fdtscores), 
                                      x = GA6_fdtscores)[,2] 
plot(inc_roi, GA6_fdtscores_mapped)
plot(inc_sharpe, GA6_fdtscores_mapped)





########### define initial random normal population ##########
pwts <- matrix(c(-3, -0.25,0.5,0.05,
0.1,0.2,0.3,0.1,0.25,0.02,
0.5,0.05,0.1,0.2,
0.2,-0.25,0.25,
0.25,0.2,0.3), ncol = 20)

gareal_pop_norm <- function (object, ...) {
  min <- object@min
  max <- object@max
  nvars <- length(min)
  population <- matrix(as.double(NA), nrow = object@popSize,
                       ncol = nvars)
  for (j in 1:nvars) {
    population[ ,j] <- rnorm(n=object@popSize)
  }
  return(population)
} 

inc_sharpe <- all_incsim_info1[,31]
a1 = 0.5; a2=0.5
rob4 <- function(b)
  (a1*sum(cor(inc_roi, X %*% b, method = "kendall"))) + (a2*sum(cor(inc_sharpe, X %*% b, method = "kendall")))
# where inc_roi = roi during inc, X %*% b = new_fdt_score computed from GA weights
GAtest <- ga(type = "real-valued", fitness = rob4, min = min, max = max,
          popSize = 500, pmutation = 0.15, pcrossover=0.7, maxiter = 400, run = 100,
          suggestions = pwts, population = gareal_pop_norm, 
          names = c( "intercept","mdd", "streak", "tdd", "trade_cnt_pday", "df_qty",
                    "shr_rt", "std_op_dr", "tdlf", "avg_ngt_op", "std_neg_pnlr",
                    "tmdd", "bgw", "std_op_qty", "cp_streak", "roll_price", 
                    "login_cnt_pday", "blg", "avg_max_op", "avg_dr"))
summary(GAtest)
plot(GAtest)
GAtest_wts <- summary(GAtest)[11][[1]][33,]
GAtest_wts <- sapply(1:length(GAtest_wts), function (i) {
  GAtest_wts[i][[1]]
})
GAtest_wts <- GAtest_wts[-1]
aa=data.frame(cbind(colnames(X[,2:20]),GAtest_wts))
aa <- aa[order(aa[,2], decreasing = T), ]
row.names(aa) <- NULL

### fdtscores vs inc_roi
GAtest_fdtscores <- X[,2:20] %*% GAtest_wts
cor(inc_roi, GAtest_fdtscores, method = "kendall") # 0.47
cor.test(inc_roi, GAtest_fdtscores, method = "kendall") # z = 5.2251, p-value = 0.0000001741
plot(inc_roi, GAtest_fdtscores)
### fdtscores vs inc_sharpe

cor(inc_sharpe, GAtest_fdtscores, method = "kendall") # 0.36
cor.test(inc_sharpe, GAtest_fdtscores, method = "kendall") # z = 4.0479, p-value = 0.00005167
plot(inc_sharpe, GAtest_fdtscores)














################################################
######### DEoptim ##############################
# performs evolutionary optimization via differential evolution algorithm
all_incsim_info1 <- read.table(file="~/incubatee/core_incsim_info_feb.xls", sep = "\t", header=T)
mydata1 <- cbind(all_incsim_info1[,3:21], all_incsim_info1[,23], all_incsim_info1[,29]) 
colnames(mydata1) <- c("ft_avg_ngt_op","ft_std_op_qty","ft_streak","ft_bgw",
                       "ft_mdd","ft_tdlf","ft_bgl","ft_df_qty",            
                       "ft_cp_streak","ft_std_op_dr","ft_avg_dr","ft_std_neg_pnlr",      
                       "ft_tmdd","ft_login_cnt_pday","ft_avg_max_op","ft_roll_price",        
                       "ft_shp_rt","ft_tdd","ft_trade_cnt_pday","sim_fdt", "inc_roi")
inc_roi <- mydata1$inc_roi
ols <- glm(inc_roi ~ ft_avg_ngt_op + ft_std_op_qty + ft_streak 
           + ft_bgw + ft_mdd + ft_tdlf + ft_bgl + ft_df_qty + ft_cp_streak
           + ft_std_op_dr + ft_avg_dr + ft_std_neg_pnlr + ft_tmdd
           + ft_login_cnt_pday + ft_avg_max_op + ft_roll_price
           + ft_shp_rt + ft_tdd + ft_trade_cnt_pday, data = mydata1)
y <- model.response(model.frame(ols))
X <- model.matrix(ols)
se.coef <- sqrt(diag(vcov(ols)))
min <- coef(ols) - 3 * se.coef
max <- coef(ols) + 3 * se.coef

library(DEoptim)
rob3 <- function(b)
  sum(cor(inc_roi, X %*% b, method = "kendall")) 
res <- DEoptim(rob3, min, max)
res$optim
res$optim$bestmem[2:20]


GAtest_fdtscores <- X[,2:20] %*% res$optim$bestmem[2:20]
cor(inc_roi, GAtest_fdtscores, method = "kendall") # 0.47
cor.test(inc_roi, GAtest_fdtscores, method = "kendall") # z = 5.2251, p-value = 0.0000001741
plot(inc_roi, GAtest_fdtscores)





