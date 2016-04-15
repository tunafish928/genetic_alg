library(genalg)
library(ggplot2)
library(doParallel)
registerDoParallel(cores = 32) # Registrer a parallel backend for train
getDoParWorkers() # check that there are 4 workers
library(GA)


f <- function(x) abs(x) + cos(x)
min <- -20
max <- +20
curve(f, min, max)

fitness <- function(x) - f(x)
GA <- ga(type = "real-valued", fitness = fitness, min = min, max = max)
plot(GA)
summary(GA)

monitor <- function(obj) {
  curve(f, min, max, main = paste("iteration =", obj@iter), font.main = 1)
  points(obj@population, -obj@fitness, pch = 20, col = 2)
  rug(obj@population, col = 2)
  Sys.sleep(0.2)
}

GA <- ga(type = "real-valued", fitness, min = min, max = max,
         monitor = monitor)





f <- function(x) (x^2 + x) * cos(x)
min <- -10
max <- 10
curve(f, min, max)
# For the maximization of this function we may use f directly as the tness function:
GA <- ga(type = "real-valued", fitness = f, min = min, max = max)
plot(GA)
summary(GA)

opt.sol <- optimize(f, lower = min, upper = max, maximum = TRUE)
nlm.sol <- nlm(function(...) -f(...), 0, typsize = 0.1)
curve(f, min, max)
points(GA@solution, GA@fitnessValue, col = 2, pch = 20)
points(opt.sol$maximum, opt.sol$objective, col = 3, pch = 8)
points(nlm.sol$estimate, -nlm.sol$minimum, col = 4, pch = 17)
legend(x = -5, y = -70, legend = c("ga", "optimize", "nlm"),
       title = "Solutions", pch = c(20,8,17), col = 2:4)



AndrewsSineFunction <- function(x, a = 1.5)
  ifelse(abs(x) > pi * a, 2 * a^2, a^2 * (1 - cos(x/a)))
rob <- function(b, s = 1)
  -sum(AndrewsSineFunction((y - X %*% b)/s))
## rob = function (wt = weight_vector) -sum(corr())
## y = sim_fdt_score
#rob <- function(b, s = 1)
#  -sum((y - X %*% b)/s)

rob1 <- function(b)
  -sum(cor(y, X %*% b))

data("stackloss", package = "datasets")

OLS <- lm(stack.loss ~ ., data = stackloss)
y <- model.response(model.frame(OLS))
X <- model.matrix(OLS)
se.coef <- sqrt(diag(vcov(OLS)))
min <- coef(OLS) - 3 * se.coef
max <- coef(OLS) + 3 * se.coef

GA <- ga(type = "real-valued", fitness = rob1, min = min, max = max,
            popSize = 100, pmutation = 0.2, maxiter = 100, run = 200)
summary(GA)
plot(GA)













## prior weights
## cd /home/caochen/keystone/settings/weights/
## fsw_sim_default.xml, current fdt scores, no neg weights 
## fsw_inc_w_op_dr.xml, fdt score of inc in feb when they got selected, has neg weights


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
## map inc_id to sim_id for all current inc (not core inc, b/c fdt_selected are not yet core inc: tdd>15)
map <- read.table("~/incubatee/av_incsim_map.xls", header = T, sep="\t") 
map <- map[,2:4] # 208 x 3
map[,1] <- as.character(map[,1])
map[,2] <- as.character(map[,2])
map[,3] <- as.character(map[,3])
curr_inc_apr1 <- merge(curr_inc_apr1, map, by.x = "USER_ID", by.y = "inc_id", all = F)
curr_inc_apr1 <- curr_inc_apr1[-207,]
## 206 out of 364 all current inc got mapped


## 2.
## remove inc who never traded
curr_inc_apr1 <- subset(curr_inc_apr1, ACCOUNT_VALUE!=10000)
## 157 out of 206 mapped have traded >= 1, account value !=10000


## 3.
## curr_inc_apr1 includes fdt_selected inc.
fdt_sel_inc <- read.csv("~/incubatee/fdt_choose_inc.csv", header = T, sep = ",") # 105 x 2, feb04_ch, feb29_global
fdt_sel_inc_traded <- merge(fdt_sel_inc, curr_inc_apr1, by.x = "ID", by.y = "sim_id", all = F) 
# 13 out of 105 fdt selected inc have traded
fdt_sel_inc_traded[,1] <- as.character(fdt_sel_inc_traded[,1])
fdt_sel_idx <- sapply(1:length(fdt_sel_inc_traded[,1]), function(i) {
  which(curr_inc_apr1[,10]==fdt_sel_inc_traded[i,1])
})
## label the 13 fdt selected inc from curr_inc_apr1
curr_inc_apr1$status="prev_sel"
curr_inc_apr1$status[fdt_sel_idx] <- "fdt_sel"

colnames(curr_inc_apr1) <- c("inc_id", "inc_alltime_pnl", "inc_account_value", 
                             "inc_weekly_pnl", "inc_alltime_roi", "inc_alltime_rank",
                             "inc_5d_pnlR", "inc_alltime_sharpe", "inc_weekly_trcnt",
                             "sim_id", "region", "inc_status")



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

all_incsim_info <- merge(inc_ftscores_feb, curr_inc_apr1, by.x = "user_id", by.y = "sim_id", all = F) # 95 x 33
colnames(all_incsim_info) <- c("sim_id","ft_avg_ngt_op","ft_std_op_qty","ft_streak",        
                               "ft_bgw","ft_mdd","ft_tdlf","ft_bgl",            
                               "ft_df_qty","ft_cp_streak","ft_std_op_dr","ft_avg_dr",         
                               "ft_std_neg_pnlr","ft_tmdd","ft_login_cnt_pday","ft_avg_max_op",    
                               "ft_roll_price","ft_shp_rt","ft_tdd","ft_trade_cnt_pday", 
                               "sim_activity","sim_fdt","inc_id","inc_alltime_pnl",   
                               "inc_account_value","inc_weekly_pnl","inc_alltime_roi","inc_alltime_rank",  
                               "inc_5d_pnlR","inc_alltime_sharpe","inc_weekly_trcnt","sim_region",            
                               "inc_status")

length(which(all_incsim_info[,33]=="fdt_sel")) 
length(which(all_incsim_info[,33]=="fdt_sel" & all_incsim_info[32]=="china"))
# 13 are fdt_selected, 11 from sim china_fx, 2 from inc global_fx

write.table(all_incsim_info, file="~/incubatee/all_incsim_info_feb.xls", sep = "\t", col.names = NA)



## 5. run genetic algorithm
# data <- read.table("~/incubatee/all_incsim_info_feb.xls", header = T, sep = "\t")

## try 1
## all fts = X, sim_fdt = y
mydata1 <- cbind(all_incsim_info[,2:20], all_incsim_info[,22]) 
colnames(mydata1) <- c("ft_avg_ngt_op","ft_std_op_qty","ft_streak","ft_bgw",
                       "ft_mdd","ft_tdlf","ft_bgl","ft_df_qty",            
                       "ft_cp_streak","ft_std_op_dr","ft_avg_dr","ft_std_neg_pnlr",      
                       "ft_tmdd","ft_login_cnt_pday","ft_avg_max_op","ft_roll_price",        
                       "ft_shp_rt","ft_tdd","ft_trade_cnt_pday","sim_fdt")
inc_roi <- all_incsim_info[,27]
## fit linear model
## range of the search space can be obtained from a preliminary OLS estimation of coef and their standard errors
ols1 <- glm(sim_fdt ~ ft_avg_ngt_op + ft_std_op_qty + ft_streak 
           + ft_bgw + ft_mdd + ft_tdlf + ft_bgl + ft_df_qty + ft_cp_streak
           + ft_std_op_dr + ft_avg_dr + ft_std_neg_pnlr + ft_tmdd
           + ft_login_cnt_pday + ft_avg_max_op + ft_roll_price
           + ft_shp_rt + ft_tdd + ft_trade_cnt_pday, data = mydata1)
y <- model.response(model.frame(ols1))
X <- model.matrix(ols1)
se.coef <- sqrt(diag(vcov(ols1)))
min <- coef(ols1) - 3 * se.coef
max <- coef(ols1) + 3 * se.coef
## define fitness function to maximize
rob <- function(b)
  sum(cor(inc_roi, X %*% b, method = "spearman")) 
# kendall give neg cor
# where inc_roi = roi during inc, X %*% b = new_fdt_score computed from ols1 weights
GA1 <- ga(type = "real-valued", fitness = rob, min = min, max = max,
         popSize = 500, pmutation = 0.05, maxiter = 200, run = 100)
summary(GA1)
plot(GA1)

GA1_wts <- summary(GA1)[11][[1]]
GA1_wts <- sapply(1:length(GA1_wts), function (i) {
  GA1_wts[i][[1]]
})
GA1_wts <- GA1_wts[-1]

GA1_fdtscores <- X[,2:20] %*% GA1_wts
cor(inc_roi, GA1_fdtscores, method = "spearman")
cor.test(inc_roi, GA1_fdtscores, method = "spearman")

all_incsim_info <- cbind(all_incsim_info, GA1_fdtscores)


## plot GA1_fdtscores vs inc_roi from April1, using Spearman's rank cor
panel.cor <- function(x, y, digits=2, cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, method = "spearman")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y, method = "spearman")
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


m5$Colour="black"
m5$Colour[m5$region=="china"]="red"
m5$Colour[m5$region=="global"]="blue" 

# fdt score vs roi raw
pairs(all_incsim_info[,c(34,27)],
      lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist) 
plot(m5$fdt_Feb, m5$roi_3.17, col=m5$Colour,
     main = "trading FDT SELECTED INC: fdt score vs roi raw",
     xlab = "fdt score", ylab = "roi")
textxy(m5$fdt_Feb, m5$roi_3.17, paste(m5$inc_id, m5$sim_id), cex = 0.7)














