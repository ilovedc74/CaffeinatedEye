

# One between variable and two within variables
##########---------------/----------------------
# comparing pre-post caffeine manipulation (exp vs control)
aov.bww <- aov(respcorr ~ exp*eccent*delay*prepost + Error(idn/(eccent*delay*prepost)) + exp, data=datanal[delay!=500,])
# comparing post caffeine manipulation (exp vs control)
aov.bww3 <- aov(respcorr ~ caffeine*eccent*delay + Error(idn/(eccent*delay)) + caffeine, data=datanal[prepost==1,])
aov.arousal = aov(arousal3 ~ caffeine, data = datanal)

##### Overview
overview = datanal[, list(avgArouse1 = mean(arousal1),
                          avgArouse2 = mean(arousal2),
                          avgArouse3 = mean(arousal3),
                          avgArouse4 = mean(arousal4),
                          avgRespcorr = mean(respcorr),
                          avgSacLen = mean(saccade_len),
                          avgFixDur = mean(crit_fix),
                          avgQuestion = mean(QUESTIONRESP, na.rm = TRUE)), by = c("eccent","delay","prepost","exp")]

overview_short = datanal[, list(avgArouse1 = mean(arousal1),
                                avgArouse2 = mean(arousal2),
                                avgArouse3 = mean(arousal3),
                                avgArouse4 = mean(arousal4),
                                avgRespcorr = mean(respcorr),
                                avgSacLen = mean(saccade_len),
                                avgFixDur = mean(crit_fix),
                                avgQuestion = mean(QUESTIONRESP, na.rm = TRUE)), by = c("prepost","exp")]

##### Probe effect Check

ezANOVA(datanal, dv = crit_fix, wid = idn, 
        within = list(delay), 
        return_aov = TRUE) # temporal has effect on fixation duration

datanal_R = datanal[eccent==6,]
datanal_RNC = datanal_R[caffeine==0,]
datanal_L = datanal[eccent==-6,]
datanal_LNC = datanal_L[caffeine==0,]

ezANOVA(datanal_R[caffeine==0,], dv = crit_fix, wid = idn, 
        within = list(delay)
        , return_aov = TRUE) # temporal has effect on fixation duration
TukeyHSD(aov(datanal_RNC$crit_fix~datanal_RNC$delay), conf.level=.95)
pairwise.t.test(datanal_RNC$crit_fix, datanal_RNC$delay, p.adjust="bonferroni", pool.sd = T)


ezANOVA(datanal_L[caffeine==0,], dv = crit_fix, wid = idn, 
        within = list(delay)
        , return_aov = TRUE) # temporal has effect on fixation duration
TukeyHSD(aov(datanal_LNC$crit_fix~datanal_LNC$delay), conf.level=.95)
pairwise.t.test(datanal$crit_fix, datanal$delay, p.adjust="bonferroni", pool.sd = T)


summary(aov(datanal$crit_fix~datanal$delay))
TukeyHSD(aov(datanal$crit_fix~datanal$delay), conf.level=.95)
pairwise.t.test(datanal$crit_fix, datanal$delay, p.adjust="bonferroni", pool.sd = T)
avg500 = datanal[, mean(crit_fix), by = c("delay","eccent")]

ezANOVA(datanal, dv = saccade_len, wid = idn, 
        within = list(delay)) # no effect on saccade length
summary(aov(datanal$saccade_len~datanal$delay))


##### Manipulation check
########--------------/-------------------
ezANOVA(datanal[delay!=500 & prepost==1,], 
             dv = respcorr, 
             wid = idn, 
             within_full = list(eccent, delay, idn), 
             # within = list(eccent, delay, ),
             between = exp) # no effect on manipulation

dat.kar = datanal[,c("idn","eccent","delay","prepost","exp", "respcorr")]
dat.kar[, mrespcorr := mean(respcorr), by = c("idn","eccent","delay","prepost","exp")]
dat.k = dat.kar[,c("idn","eccent","delay","prepost","exp","mrespcorr")]
dat.k = unique(dat.k)
dp = dat.kar[prepost==1,]
ezANOVA(data = datanal,
        dv = respcorr,
        wid = idn,
        within_full  = .(eccent, delay,prepost), 
        # within = .(eccent, delay,prepost),
             # within = list(eccent, delay, ),
        between = exp) 
# no effect on manipulation

aov.bww <- aov(respcorr ~ exp*eccent*delay*prepost + Error(idn/(eccent*delay*prepost)) + exp, data=datanal)

ezANOVA(datanal[prepost ==0 &delay!=500,], dv = respcorr, wid = idn, 
        within_full = list(eccent, delay), 
        between = exp) 
# Both group has similar pre test

ezANOVA(datanal[prepost ==1&delay!=500,], dv = respcorr, wid = idn, 
        within_full = list(eccent, delay), 
        between = exp) 
# Both group do not have similar post test, but not significant

ezANOVA(datanal[exp==1 & delay!=500,], dv = respcorr, wid = idn, 
        within = list(eccent, delay, prepost)) # there's effect on delay

## This is sig. and interaction
# ---only pre-test data---
predat = ezANOVA(datanal[delay!="500"&prepost=="0",], dv = respcorr, wid = idn, 
        within = list(eccent,delay), between=exp) 
# replication effect

# TukeyHSD(x = predat$aov, conf.level=.95)

#----------/--------difference post-pre altogether //data  = dtall 

dt1 = datanal[,c("idn","eccent","delay","prepost2","exp","respcorr")]

######## prepare 4 conditions dt

dtall = dt1[delay!=500,] # exclude no probe control trials 

# #DTALL

## long to wide
dtall = dcast(
  dtall, 
  delay + eccent + exp ~ prepost2, 
  value.var = "respcorr", 
  fun.aggregate = c(mean,length,sd))

dtall[, mean_diff := (
  respcorr_mean_post - respcorr_mean_pre),] # mean difference

# change labels for graph
dtall[, `:=` (LR = ifelse(eccent==-6, "Left Probe", "Right Probe"), 
              EXP= ifelse(exp==0, "Control group", "Experiment group")),]

### Pooled SE functions
var.pooled <- function(df1,df2,SD1,SD2){
  (df1*SD1^2 + df2*SD2^2)/(df1+df2)
}
SE.diff <- function(var.pool, n1,n2){
  sqrt(var.pool*(1/n1 + 1/n2))
}

var.pool <-var.pooled(df1 = dtall$respcorr_length_post-1,
                      df2 = dtall$respcorr_length_pre-1,
                      SD1 = dtall$respcorr_sd_post,
                      SD2 = dtall$respcorr_sd_pre)
se.dif <- SE.diff(var.pool,
                  n1 = dtall$respcorr_length_post,
                  n2 = dtall$respcorr_length_pre)
dtall = cbind(dtall,var.pool, se.dif)

#------------------
dtall.phase = dtall[, c("LR","delay", "EXP", 
                        "respcorr_mean_post","respcorr_mean_pre",
                        "mean_diff")]

New.dtall.phase = 
  dcast(dtall.phase, 
        LR + EXP ~ delay,
        value.var = 'mean_diff',
        fun.aggregate = mean
  )
New.dtall.phase[, `:=` (phase1 = (`10`+`40`)/2,
                        phase2 = (`110`+`220`)/2),]

# ----------/--------difference post-pre for each P-----------

Newtest = datanal[, c("idn","prepost2","eccent","delay", "exp", "respcorr","wbound","cis_binary")]
Newtest2 = 
  dcast(Newtest, 
                 idn + eccent + delay + exp + cis_binary ~ prepost2,
                 value.var = 'respcorr',
                 fun.aggregate = mean
      )
Newtest2[, postprediff:= post-pre, ]

Newtest2 = na.omit(Newtest2)

ezANOVA(Newtest2[delay!="500"&(delay=="110"|delay=="220"),], dv = postprediff, wid = idn, 
        within = list(eccent,delay,cis_binary)) # checking only ORIENTING post-pre

ezANOVA(Newtest2[delay!="500"&(delay=="10"|delay=="40"),], dv = postprediff, wid = idn, 
        within = list(eccent,delay), between=exp) # checking only Engagement post-pre

Newtest2[, .(m = mean(postprediff),sd = sd(postprediff)), by = c("eccent","exp")]

#----
# NO effect is found for CIS survey
ezANOVA(datanal[delay!="500"&(delay=="110"|delay=="220"),], dv = respcorr, wid = idn, 
        within_full = list(eccent,delay), between = cis_binary) # checking only ORIENTING post-pre

ezANOVA(datanal[delay!="500"&(delay=="10"|delay=="40"),], dv = respcorr, wid = idn, 
        within = list(eccent,delay), between = cis_binary) # checking only Engagement post-pre

# CHECK word boundary before pooling------
WB = Newtest[delay!="500"&eccent=="6" & prepost2 == "pre"  ,] # change phases here & (delay=="10"|delay=="40"),or & (delay=="110"|delay=="220")
WB1 = WB[wbound==1,] # 425 (all)->sig, 209 (orient), 216
WB0 = WB[wbound==0,] # 414 (all), 175 (orient), 239
WB0 = WB0[sample(nrow(WB0), 190), ]
WBALL = rbind(WB1, WB0)

t.test(respcorr ~ wbound, data = WBALL, paired = T)

ezANOVA(Newtest[ delay!="500" & (delay=="110"|delay=="220"), ], dv = respcorr, wid = idn, 
        within = list(eccent,delay), between = wbound ) # checking only ORIENTING post-pre

ezANOVA(Newtest2[delay!="500" & (delay=="10"|delay=="40"), ], dv = postprediff, wid = idn, 
        within = list(eccent,delay), between=exp) # checking only Engagement post-pre

#-----------//-//-//-//-//-//-------------------

# Phase 10+40 110+220 mean suggestion from here - data - Newtest2

# or 4 time points.

NewtestN = dcast(Newtest2, 
                 idn + eccent + exp ~ delay,
                 value.var = 'postprediff')

NewtestN[, `:=` (engage = (`10`+`40`)/2, orient = (`110`+`220`)/2),]

NewtestN = melt(NewtestN, id.vars = c("idn", "eccent","exp"),
                variable.name = "phase",
                value.name = "postprediff")

ezANOVA(NewtestN[phase == "engage" | phase == "orient" ], 
        dv = postprediff, 
        wid = idn, 
        within = list(eccent,phase), 
        between=exp) # checking only post-pre

ezANOVA(NewtestN[phase != "engage" & phase != "orient" ], 
        dv = postprediff, 
        wid = idn, 
        within = list(eccent,phase), 
        between=exp) # checking only post-pre
#-----------//-//-//-//-//-//-------------------
#----check the test of averaged SOA

# prepare collapse from raw data

Newtest3 =
  dcast(Newtest,
        idn + eccent +exp~prepost2+delay,
        value.var = 'respcorr',
        fun.aggregate = mean
  )

Newtest3 = na.omit(Newtest3)
# Unbalanced data
# Newtest5 = Newtest3[, `:=` (post.engage = (post_10+post_40+post_110)/3,
#                             pre.engage = (pre_10+pre_40+pre_110)/3,
#                             post.orient = (post_220),
#                             pre.orient = (pre_220))]
# Newtest5[, `:=` (phase_O = post.orient- pre.orient,
#                  phase_E = post.engage - pre.engage), ]
# Newtest5 = Newtest5[, c("idn", "eccent","exp","phase_O","phase_E"),]
# Newtest5 = melt(Newtest5, id.vars = c("idn", "eccent","exp"),
#                 variable.name = "phase",
#                 value.name = "diff.resp")
# ezANOVA(Newtest5, dv = diff.resp, wid = idn,
#         within = list(eccent,phase), between=exp) # checking only post-pre


#----
Newtest3 = Newtest3[, `:=` (post.engage = (post_10+post_40)/2,
                            pre.engage = (pre_10+pre_40)/2,
                            post.orient = (post_110+post_220)/2,
                            pre.orient = (pre_110+pre_220)/2)]
Newtest3[, `:=` (phase_O = post.orient- pre.orient,
                 phase_E = post.engage - pre.engage), ]

Newtest4 = Newtest3[, c("idn", "eccent","exp","phase_O","phase_E"),]
Newtest4 = melt(Newtest4, id.vars = c("idn", "eccent","exp"),
            variable.name = "phase", 
            value.name = "diff.resp")

ezANOVA(Newtest4, dv = diff.resp, wid = idn, 
        within = .(eccent,phase)) # checking only post-pre

class(Newtest4$eccent)

plttest = 
  ezPlot(Newtest4, dv = diff.resp, wid = idn, 
         within = .(eccent, phase), 
         between = exp,
         x = phase,
         do_lines = TRUE, 
         do_bars = TRUE,
         col = eccent,
         row = exp,
         x_lab = "temporal offsets",
         y_lab = "correct probe response"
  )
# prepare data:
soa = 
  dcast(Newtest2, 
        idn + eccent + exp ~ delay,
        value.var = 'postprediff'
        # fun.aggregate = mean
  )
soa[, `:=` (engage = (`10`+`40`)/2, orient = (`110`+`220`)/2),]

ezANOVA(soa[delay!="500",], dv = postprediff, wid = idn, 
        within = list(eccent,delay), between=exp) # checking only post-pre

soa2 = soa[, c("idn", "eccent","exp","orient","engage")]

soa2 = melt(soa2, id.vars = c("idn", "eccent","exp"),
     variable.name = "phase", 
     value.name = "m.diff.respcor")

ezANOVA(soa2, dv = m.diff.respcor, wid = idn, 
        within = list(phase,eccent), between=exp) # checking averaged phase

# ---only post-test data
ezANOVA(datanal[delay!="500"&prepost=="1",], dv = respcorr, wid = idn, 
        within = list(eccent,delay), between=exp) # replication effect

ezANOVA(datanal[delay!="500",], dv = respcorr, wid = idn, 
        within_full= list(eccent, delay, prepost), between=exp) # replication effect 


ezANOVA(datanal[delay!=500,], dv = respcorr, wid = idn, 
        within = list(eccent)) # spatial effect 

ezANOVA(datanal[delay!=500,], dv = respcorr, wid = idn, 
        within_full = list(delay, prepost),
        between = exp)

ezDesign(datanal, delay, respcorr, row = prepost, col = exp)

ezANOVA(datanal, dv = respcorr, wid = idn, 
        within_full = list(delay, eccent), 
        between = exp)
#----

# delete 4, 8, 11
bal_data = datanal[id!="04" & 
                     id!="08" & 
                     id!="11",
                   c("idn", "eccent","exp","prepost","delay","respcorr")]

ezANOVA(bal_data, dv = respcorr, wid = idn, 
        within = list(delay, eccent), 
        between = exp)

# RIGHT, Error rates
Right_Probe = 
  ezPlot(datanal[eccent == 6 & delay != 500,], dv = respcorr, wid = idn, 
         within = list(delay, prepost), 
         between = exp,
         x = delay,
         do_lines = TRUE, 
         do_bars = TRUE,
         col = exp,
         row = prepost,
         x_lab = "temporal offsets",
         y_lab = "correct probe response",
         print_code = TRUE
  )
# print_code = TRUE
# adding error bar
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}



####################
# SACCADE LENGTHS ANOVA

ezANOVA(datanal,
        dv = saccade_len, 
        wid = idn,
        within_full = prepost,
        between = exp)
ezDesign(datanal,
         prepost,
         saccade_len, 
         col = exp)

######### RESULTS

# $ANOVA
# Effect DFn DFd        F          p p<.05       ges
# 1    exp   1  14 7.149557 0.01816213     * 0.3380476
# 
# $`Levene's Test for Homogeneity of Variance`
# DFn DFd       SSn      SSd         F         p p<.05
# 1   1  14 0.7426743 10.90886 0.9531192 0.3454954   

####################
# FIXATION DURATION ANOVA

ezANOVA(datanal,
        dv = crit_fix, 
        wid = idn,
        within_full = prepost,
        between = exp)
####### RESULTS
# $ANOVA
# Effect DFn DFd        F          p p<.05      ges
# 1    exp   1  14 8.829486 0.01010748     * 0.386758
# 
# $`Levene's Test for Homogeneity of Variance`
# DFn DFd      SSn      SSd         F         p p<.05
# 1   1  14 373.2247 9292.078 0.5623227 0.4657417

####################
# PROBE DETECTION CORRECT ANOVA

ezANOVA(datanal,
        dv = saccade_len, 
        wid = idn,
        within_full = prepost,
        between = exp)
ezDesign(datanal,
         prepost,
         saccade_len, 
         col = exp)

####################
# LEFT, Error rates
# ezPlot(datanal[eccent == -6,], dv = respcorr, wid = idn, 
#        within = .(delay, prepost), 
#        between = .(exp),
#        x = .(delay),
#        do_lines = TRUE, 
#        do_bars = TRUE,
#        col = exp,
#        row = prepost,
#        x_lab = "temporal offsets",
#        y_lab = "correct probe response"
# )

#SACCADE LENGTH
Saccade_Length = 
  ezPlot(datanal, dv = saccade_len, wid = idn, 
         within = .(prepost), 
         between = exp,
         x = prepost,
         do_lines = TRUE, 
         do_bars = TRUE,
         col = exp,
         # row = prepost,
         x_lab = "before and after (caffeine for col 1)",
         y_lab = "Saccade Length"
  )
# FIXATION DURATION
FixDuration = 
  ezPlot(datanal, dv = crit_fix, wid = idn, 
         within = .(prepost), 
         between = exp,
         x = prepost,
         do_lines = TRUE, 
         do_bars = TRUE,
         col = exp,
         # row = prepost,
         x_lab = "before and after (caffeine for col 1)",
         y_lab = "Fixation Duration",
         # print_code = TRUE
  )
ggplot(data=sebb, aes(x=variable, y=value, group=Fluency, color = Fluency)) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1) +
  geom_line()+
  geom_point() +
  theme_classic()



ezANOVA(datanal, dv = crit_fix, wid = idn, 
        within = list(delay, eccent, prepost), 
        between = list(exp), return_aov = TRUE)

ezANOVA(datanal, dv = saccade_len, wid = idn, 
        within = list(delay, eccent, prepost), 
        between = list(exp), return_aov = TRUE)

ezDesign(datanal, delay, saccade_len, row = prepost, col = caffeine)

# two groups baseline should be same
ezANOVA(datanal[prepost==0,], dv = respcorr, wid = idn, 
        within = list(delay, eccent), 
        between = list(exp))

ezPlot()

