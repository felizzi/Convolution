## loader of cool data
install.packages("dplyr")
install.packages("flexsurv")
require(dplyr)
require(flexsurv)

dset <- read.csv("Datasets/test (version 1).csv")


## roll time back from death
dset <- dset %>% 
  dplyr::mutate(timeutil = timemth - osdur) %>% 
  dplyr::filter(!is.na(timemth)) %>% dplyr::filter(!is.na(pfsinv)) %>% 
  dplyr::filter(age > 5)

## create flags for observation times that are pre-PFS, between PFS and OS
dset <- dset %>% dplyr::mutate(STATE = ifelse(timemth <= pfsinv, "pre-PROGRESSION","post-PROGRESSION"))

#take averages of qol pre and post progression 
means_qol <- dset %>% group_by(STATE) %>% summarize(mean_util = mean(utility, na.rm = T))

loess50 <- loess(utility ~ timeutil, data=dset %>% filter(!is.na(utility)), span=.9)
smooth50 <- predict(loess50,newdata = data.frame(timeutil = seq(-52,0))) 

dset_red <- dset %>% filter(!is.na(utility))

plot(dset$timeutil, dset$utility, pch=19, main='Loess Regression Models')


qol_pred <- predict(loess50, newdata = data.frame(timeutil = seq(-52,0)))
lines(smooth50, x=seq(-52,0) , col='red', lwd = 7)

fsr_os <- flexsurvreg(Surv(osdur, 1-oscnsr) ~ 1, data = dset_red, dist = "weibull")
fsr_pfs <- flexsurvreg(Surv(pfsinv, 1- pfsinvcn) ~ 1, data = dset_red, dist = "weibull")

time_v <- seq(0,1500)

f_t_os <- do.call(fsr_os$dfns$d, args = c(as.list(fsr_os$res[,'est']),
                                    list (x = time_v, log = FALSE)))

                                    
  S_t_os <- do.call(fsr_os$dfns$p, args = c(as.list(fsr_os$res[,'est']),
                                list(q = time_v, lower.tail = FALSE))
  )       
               
plot(fsr_os, xlim = c(0,500))
lines(time_v, S_t_os)

S_t_pfs <- do.call(fsr_pfs$dfns$p, args = c(as.list(fsr_pfs$res[,'est']),
                                          list(q = time_v, lower.tail = FALSE))
)
lines(time_v, S_t_pfs, col = "blue")
#calculate the "integral" for QoL

area_surv(time_v, S_t_os)
area_surv(time_v, S_t_pfs)


## integral for our purposes 
QALY_qol <- sum(1-cumsum(f_t_os*v_qol_res)) / 12