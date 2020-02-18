
# TODO: check the four Ps are same or not, plot and range, mean----
# 
## b1 = pilot (exp/con sep)
## dv = 1. saccade length, 2. fixation duration, 3. respcorr

datanal[, pilot :=(ifelse(idn=="02"|idn=="03"|idn=="04"|idn=="05", 1, 0)),]
datanal[, range(c(crit_fix,saccade_len)), by="pilot"]
datanal[, list(fix = mean(crit_fix), sac = mean(saccade_len)), by="pilot"]
datanal[, list(fix = mean(crit_fix), sac = mean(saccade_len)),]

d1 = datanal[prepost=="1",]
ggplot(datanal,  aes(x=saccade_len, y=crit_fix, color = c("prepost","exp")))+
  geom_point()
ggplot(d1,  aes(x=saccade_len, y=crit_fix, color = exp))+
  geom_point()

t = dat_final[, mean(QUESTIONRESP, na.rm = TRUE), by = c("prepost","caffeine")]
Qshort = dat_final[, c("idn","prepost2","exp2","QUESTIONRESP")]
Qshort = na.omit(Qshort)
Qwide = dcast(Qshort, idn + exp2 ~ prepost2, value.var = "QUESTIONRESP", 
              fun.aggregate = c(mean,length,sd))
Qwide = Qwide[, Qmeandiff := QUESTIONRESP_mean_post-QUESTIONRESP_mean_pre, ]

res <- t.test(Qmeandiff ~ exp2, data = Qwide, var.equal = TRUE)
res
datanal[, list(mean(crit_fix),  mean(saccade_len)), by=c("caffeine")]

# Checking arousal effect test & plot
# TODO: make sure arousal effect t-test and plot-------

## b1=exp
## dv=arousal_post-arousal_pre
## t test

datanal[, `:=` (pre_arouse = ((Pre1 + pre2)/2), 
                    post_arouse = ((post1 + post2)/2)),]
datanal[, `:=` (md_arouse = post_arouse-pre_arouse),]

ds = datanal[, c("idn", "exp", "md_arouse")]
ds = ds[!duplicated(ds), ]
res <- t.test(md_arouse ~ exp, data = ds, var.equal = TRUE)
res
ds[, sd(md_arouse), by = "exp"]

#--------------/-------------plot arousal

datanal[, arouse.plot:= ifelse(prepost2=="pre", pre_arouse, post_arouse),]

library(ggplot2)
d.arouse = unique(datanal[,c("idn", "exp", "arouse.plot", "prepost2")])
# install.packages("lattice")
library(Rmisc)

sum = summarySE(d.arouse, 
                measurevar="arouse.plot", 
                groupvars=c("exp","prepost2"))

names(sum)[names(sum)=="exp"]  <- "Group" # change label for legend exp
levels(sum$Group)[levels(sum$Group)=="0"] <- "Control group" # change label for legend exp
levels(sum$Group)[levels(sum$Group)=="1"] <- "Experiment group" # change label for legend exp

pd = position_dodge(.2) 
ggplot(sum, aes(x=prepost2, 
                y=arouse.plot, 
                color=Group,
                group=Group)) + 
  geom_errorbar(aes(ymin=arouse.plot-se, 
                    ymax=arouse.plot+se), 
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  geom_line(position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_y_continuous(name="Arousal") +
  scale_color_manual(values = c("#999999", "#E69F00"))+ 
  scale_x_discrete(name="",
                   limits = rev(levels(d.arouse$prepost2)),
                   breaks=c("pre", "post"),
                   labels=c("Before treatment", "After treatment"))+
  # scale_fill_discrete(name="Experimental\nCondition",
  #                     breaks=c("0", "1"),
  #                     labels=c("Control group", "Experiment group"))+
  theme(legend.title=element_blank()) +
    # theme(strip.text.x = element_text(size=14),
    #       strip.text.y = element_text(size=14))+
    theme(legend.text = element_text(size=16))+
    theme(axis.title.x = element_text(size=16, face = "bold"),    # x,y axis title
          axis.text.x  = element_text(vjust=0.5, size=18),  # x,y axis value
          axis.title.y = element_text(size=18, face = "bold"),   # x,y axis title
          axis.text.y  = element_text(vjust=0.5, size=16))  # x,y axis value
  
datanal[, mean(saccade_len), by = c("exp","prepost2")]
# fixation se
fixx = datanal[,c("idn", "exp", "crit_fix", "prepost2")]
sum = summarySE(fixx, 
                measurevar="crit_fix", 
                groupvars=c("exp","prepost2"))

sacc = datanal[,c("idn", "exp", "saccade_len", "prepost2")]
sum = summarySE(sacc, 
                measurevar="saccade_len", 
                groupvars=c("exp","prepost2"))

# #------------------- Plot arousal2, with all 4 measures
# 
# 
# datanal[, arouse.plot:= ifelse(prepost2=="pre", pre_arouse, post_arouse),]
# 
# library(ggplot2)
# d.arouse = unique(datanal[,c("idn", "exp", "arouse.plot", "prepost2")])
# # install.packages("lattice")
# library(Rmisc)
# 
# sum = summarySE(d.arouse, 
#                 measurevar="arouse.plot", 
#                 groupvars=c("exp","prepost2"))
# 
# names(sum)[names(sum)=="exp"]  <- "Group" # change label for legend exp
# levels(sum$Group)[levels(sum$Group)=="0"] <- "Control group" # change label for legend exp
# levels(sum$Group)[levels(sum$Group)=="1"] <- "Experiment group" # change label for legend exp
# 
# pd = position_dodge(.2) 
# ggplot(sum, aes(x=prepost2, 
#                 y=arouse.plot, 
#                 color=Group,
#                 group=Group)) + 
#   geom_errorbar(aes(ymin=arouse.plot-se, 
#                     ymax=arouse.plot+se), 
#                 width=.2, size=0.7, position=pd) +
#   geom_point(shape=15, size=4, position=pd) +
#   geom_line(position=pd) +
#   theme_bw() +
#   theme(
#     axis.title.y = element_text(vjust= 1.8),
#     axis.title.x = element_text(vjust= -0.5),
#     axis.title = element_text(face = "bold")) +
#   scale_y_continuous(name="Arousal") +
#   scale_color_manual(values = c("#999999", "#E69F00"))+ 
#   scale_x_discrete(name="",
#                    limits = rev(levels(d.arouse$prepost2)),
#                    breaks=c("pre", "post"),
#                    labels=c("Before treatment", "After treatment"))+
#   # scale_fill_discrete(name="Experimental\nCondition",
#   #                     breaks=c("0", "1"),
#   #                     labels=c("Control group", "Experiment group"))+
#   theme(legend.title=element_blank()) +
#   # theme(strip.text.x = element_text(size=14),
#   #       strip.text.y = element_text(size=14))+
#   theme(legend.text = element_text(size=16))+
#   theme(axis.title.x = element_text(size=16, face = "bold"),    # x,y axis title
#         axis.text.x  = element_text(vjust=0.5, size=18),  # x,y axis value
#         axis.title.y = element_text(size=18, face = "bold"),   # x,y axis title
#         axis.text.y  = element_text(vjust=0.5, size=16))  # x,y axis value
# 


# TODO: check reading is same without probe (check manuscript)----------------------

############## prepare for t test mean difference

datanal$seeprobe = as.factor(datanal$probeoccur_durfix)

dt.wide = datanal[,c("idn","crit_fix","saccade_len","prepost2","exp","seeprobe")]
dt.wide = dcast(
  dt.wide, 
  idn + seeprobe + exp ~ prepost2, 
  value.var = c("crit_fix","saccade_len"), 
  fun.aggregate = c(mean,length,sd))
dt.wide = na.omit(dt.wide)


# This is the control trials SOA=500 check #############

#----- 1 fixation duration (without probe

## model 1 ezanova
ezANOVA(datanal, 
        crit_fix, 
        idn, 
        within = c("seeprobe"), 
        between = exp)
plot(x=datanal$exp, y=datanal$crit_fix, 
     xlab="Groups", ylab="Fixation durations",
     xaxt="n")
     axis(1, at=1:2, labels = c("control group","caffeine group"))
## model 2 aov

# 2x2 mixed:
# IV between: exp
# IV within:  seeprobe, prepost
# DV:         crit_fix
aov_probeeffect_fix <- aov(
  crit_fix ~ exp*seeprobe + Error(idn/(seeprobe)) + exp, 
  data=datanal)
summary(aov_probeeffect_fix)
# model.tables(aov_probeeffect_fix, "means")

## model T Test

  #--saccade mean diff 
    dt.wide[, sac.mean.diff := (
      saccade_len_mean_post - saccade_len_mean_pre),] # mean difference
  # t test
    t.sac = t.test(dt.wide[seeprobe==1,sac.mean.diff],dt.wide[seeprobe==2,sac.mean.diff])
  # boxplot
    ggplot(dt.wide, aes(x = seeprobe, y = sac.mean.diff)) +
      geom_boxplot()

  #--fixation mean diff
    dt.wide[, fix.mean.diff := (
      crit_fix_mean_post - crit_fix_mean_pre),] # mean difference
  # t test
    t.fix = t.test(dt.wide[seeprobe==1,fix.mean.diff],dt.wide[seeprobe==2,fix.mean.diff])
  # boxplot
    ggplot(dt.wide, aes(x = seeprobe, y = fix.mean.diff)) +
      geom_boxplot()  # t test

#  ## long to wide
# dtall = dcast(dtall, delay + eccent + exp ~ prepost2, value.var = "respcorr", fun.aggregate = c(mean,length,sd))
# dtall[, mean_diff := (respcorr_mean_post - respcorr_mean_pre),] # mean difference


#----- 2 saccade length (without probe

## 
# 2x2 mixed:
# IV between: exp
# IV within:  seeprobe, prepost
# DV:         saccade_len
aov_probeeffect_sac <- aov(
  saccade_len ~ exp*seeprobe + Error(idn/(seeprobe)) + exp, 
  data=datanal)
summary(aov_probeeffect_sac)


ez.probeffect.sac = ezANOVA(datanal, 
                            saccade_len,
                            idn,
                            within_full = c("seeprobe"),
                            between = exp)
ez.probeffect.sac

# plot
plot(x=datanal$exp, y=datanal$saccade_len, 
     xlab="Groups", ylab="saccade lengths",
     xaxt="n")
axis(1, at=1:2, labels = c("control group","caffeine group"))

#----- 3 Respcorr (without probe
## 
# 2x2 mixed:
# IV between: exp
# IV within:  seeprobe, prepost
# DV:         respcorr
aov_probeeffect_respcorr <- aov(
  respcorr ~ exp*seeprobe*prepost + Error(idn/(seeprobe*prepost)) + exp, 
  data=datanal)
summary(aov_probeeffect_respcorr)

ez.probeffect.resp = ezANOVA(datanal, 
                            respcorr,
                            idn,
                            within = c("seeprobe"),
                            between = exp)
ez.probeffect.resp
# Error: idn
#                       Df Sum Sq Mean Sq F value Pr(>F)  
# exp:seeprobe          1  1.693  1.6930   3.636  0.093 .








# TODO: main effect and manipulation on spatial and temporal effects)----------------------

# too much uncorrected t test Part1---------
# ----------FINAL "m.resp.short2.wide"
mean.resp.short = datanal[delay!=500, c("idn","eccent","delay","prepost2","exp","respcorr")]
m.resp.short = mean.resp.short[, m.rc := mean(respcorr), by = c("eccent","delay","idn","prepost2")]
m.resp.short2 = unique(m.resp.short)


m.resp.short2.wide =na.omit(
  dcast(m.resp.short2, 
        idn + eccent + delay + exp ~ prepost2, 
        value.var = 'm.rc',
        fun.aggregate = c(mean))
)

m.resp.short2.wide[,`:=` ( m.rc.diff = post - pre ),]
t.test( m.rc.diff ~ exp, data = m.resp.short2.wide )
# too much uncorrected t test Part2-------
# mean diff pre-post of delay and spatial

# m.resp.short3 =na.omit(
#   dcast(m.resp.short2, 
#         idn+eccent+delay+exp~prepost2, 
#         value.var = 'mean.resp',
#         fun.aggregate = c(mean))
# )
# m.resp.short3[,`:=` (mean.diff.resp = post-pre),]
# 

# too much uncorrected t test Part3 -----
res.all = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide, var.equal = TRUE)
res.all

res.220.all = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="220",], var.equal = TRUE)
res.220.all
res.220.L = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="220"&eccent==-6,], var.equal = TRUE)
res.220.L
res.220.R = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="220"&eccent==6,], var.equal = TRUE)
res.220.R

res.110.all = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="110",], var.equal = TRUE)
res.110.all 
res.110.L = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="110"&eccent==-6,], var.equal = TRUE)
res.110.L
res.110.R = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="110"&eccent==6,], var.equal = TRUE)
res.110.R

res.40.all = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="40",], var.equal = TRUE)
res.40.all
res.40.L = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="40"&eccent==-6,], var.equal = TRUE)
res.40.L
res.40.R = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="40"&eccent==6,], var.equal = TRUE)
res.40.R

res.10.all = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="10",], var.equal = TRUE)
res.10.all
res.10.L = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="10"&eccent==-6,], var.equal = TRUE)
res.10.L
res.10.R = t.test(m.rc.diff ~ exp, data = m.resp.short2.wide[delay=="10"&eccent==6,], var.equal = TRUE)
res.10.R

# too much uncorrected t test Part4 -----

mean.resp.short = datanal[delay!=500, c("idn","eccent","delay","prepost2","exp","respcorr")]
m.resp.short = mean.resp.short[, mean.resp := mean(respcorr), by = c("eccent","delay","idn")]
m.resp.short2 = unique(m.resp.short)


m.resp.short3 =na.omit(
  dcast(m.resp.short2, 
        eccent+delay+exp~prepost2, 
        value.var = 'mean.resp',
        fun.aggregate = c(mean, sd))
)

m.resp.short3[,`:=` (mean.diff.resp = mean.resp_mean_post-mean.resp_mean_pre),]
t.test(mean.diff.resp~exp, data = m.resp.short3)

res.all = t.test(mean.resp ~ exp, data = m.resp.short2, var.equal = TRUE)
res.all

res.220.all = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="220",], var.equal = TRUE)
res.220.all
res.220.L = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="220"&eccent==-6,], var.equal = TRUE)
res.220.L
res.220.R = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="220"&eccent==6,], var.equal = TRUE)
res.220.R

res.110.all = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="110",], var.equal = TRUE)
res.110.all 
res.110.L = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="110"&eccent==-6,], var.equal = TRUE)
res.110.L
res.110.R = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="110"&eccent==6,], var.equal = TRUE)
res.110.R

res.40.all = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="40",], var.equal = TRUE)
res.40.all
res.40.L = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="40"&eccent==-6,], var.equal = TRUE)
res.40.L
res.40.R = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="40"&eccent==6,], var.equal = TRUE)
res.40.R

res.10.all = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="10",], var.equal = TRUE)
res.10.all
res.10.L = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="10"&eccent==-6,], var.equal = TRUE)
res.10.L
res.10.R = t.test(mean.resp ~ exp, data = m.resp.short2[delay=="10"&eccent==6,], var.equal = TRUE)
res.10.R
## 
# 2x2 mixed:
# IV between: exp
# IV within:  delay, eccent
# DV:         mean.resp

aov.mresp <- aov(mean.resp ~ exp*eccent + Error(idn/(eccent)) + exp, 
  data=mean.resp.short)
summary(aov.mresp)


## 
# 2x2 mixed:
# IV between: exp
# IV within:  delay, eccent, prepost
# DV:         respcorr
datanal.probe = datanal[delay!=500,]
aov_probeeffect_respcorr <- aov(
  respcorr ~ exp*delay*eccent + Error(idn/(delay*eccent)) + exp, 
  data=mean.resp.short)
summary(aov_probeeffect_respcorr)
# res <- t.test(weight ~ group, data = my_data, var.equal = TRUE)
# res1 = t.test(mean_diff ~ exp, data = dtall)

# 2x2 mixed: only 220 ms
# IV between: exp
# IV within:  delay, eccent, prepost
# DV:         respcorr







# TODO: check caffeinated eye effects)----------------------)---------------------








# prepare data--------------/-----------
datanal[,`:=` (word_len = trigger2-trigger1-1,
                          saccade_len = next_pos - (crit_pos+4)),] 
datanal[, `:=` (fix_avg = (crit_fix+prev_fix+next_fix)/3),]
# 'crit_fix' for just fix Wn
datanal[, `:=` (sac2 = (crit_pos-prev_pos)),]
datanal[, `:=` (sac_avg = (saccade_len+sac2)/2),] 
# 'saccade_len' for just fix Wn


## USE THIS data table
basic = datanal[,c("idn","prepost2","exp","fix_avg","sac_avg","saccade_len","crit_fix")]
basic2 = datanal[,c("idn","prepost2","exp","saccade_len","crit_fix","caffeine")]

# -----------saccade len (caffeinated eye effects) ------


# Saccade length way 1 by mean diff NO EFFECTS ------

# (1) all three saccade_len measures
dbsac = na.omit(
  dcast(
    basic,
    idn + exp ~ prepost2,
    value.var = "sac_avg",
    fun.aggregate = c(mean,length,sd)))
dbsac[,`:=` (sac.mean.diff = sac_avg_mean_post-sac_avg_mean_pre),]
t.test(sac.mean.diff~exp, data = dbsac)

# find out how much difference prepost

dbsac[, .(mean_sacdiff = mean(sac.mean.diff),
          sd_sacdiff = sd(sac.mean.diff)), by = c("exp")]


# (2) only crit saccade_len measure
dbsac2 = na.omit(
  dcast(
    basic, 
    idn + exp ~ prepost2, 
    value.var = "saccade_len", 
    fun.aggregate = c(mean,length,sd)))
dbsac2[,`:=` (sac.mean.diff = saccade_len_mean_post-saccade_len_mean_pre),]
t.test( sac.mean.diff ~ exp, data = dbsac2 )



# saccade len way 2 (no effect, just skip) ------
# 2x2 mixed:
# IV between: exp
# IV within:  prepost
# DV:         sac_avg


##### saccade length
# (aov)
aov.msac <- aov(sac_avg ~ exp*prepost2 + Error(idn/(prepost2)) + exp, 
                 data=basic)
summary(aov.msac) # no effect

# (ez)
ez.msac = ezANOVA(basic, 
                  dv = saccade_len, 
                  wid = idn, 
                  within = prepost2, 
                  between = exp)
ez.msac # no effect




# -----------fixation duration 510 ----

# not controlling pre test, purely group difference
t.test(fix_avg ~ exp, data = datanal) # p-value < 2.2e-16
t.test(crit_fix ~ exp, data = datanal) # p-value < 2.2e-16 

# by mean diff # p-value = 0.2356
db = na.omit(
  dcast(
  basic, 
  idn + exp ~ prepost2, 
  value.var = "crit_fix", 
  fun.aggregate = c(mean,length,sd)))
db[, `:=` ( fix.mean.diff = crit_fix_mean_post-crit_fix_mean_pre ),]
t.test( fix.mean.diff ~ exp, data = db )

# ---no probe test on fixdur----
basic = datanal[,c("idn","prepost2","exp","fix_avg","sac_avg",
                   "saccade_len","crit_fix","caffeine", "probeoccur_durfix")] #take needed columns
db = na.omit(
  dcast(
    basic, 
    idn + exp + probeoccur_durfix ~ prepost2, 
    value.var = "crit_fix", 
    fun.aggregate = c(mean)))

db[, `:=` ( fix.mean.diff = post-pre ),]

t.test(fix.mean.diff ~ probeoccur_durfix, db)

#-----------------------------way 2 annova ----

##### RESULTS: Trend of shorter fix duration
ez.mfix = ezANOVA(basic, 
                  fix_avg, 
                  wid = idn, 
                  within_full = prepost2, 
                  between = exp)
ez.mfix # only shows group difference

basic[,mean(fix_avg),by = c("prepost2","exp")]
basic2[,mean(crit_fix),by = c("prepost2","exp")]

# db2 = na.omit(
#   dcast(
#     basic2[exp==1,], 
#     idn ~ prepost2, 
#     value.var = "crit_fix", 
#     fun.aggregate = c(mean,length,sd)))
# db2[, `:=` ( fix.mean.diff = crit_fix_mean_post - crit_fix_mean_pre ),]
# t.test(fix.mean.diff ~ exp, data = db2)
# 
# t.test(db2$crit_fix_mean_pre, db2$crit_fix_mean_post, paired = T)


#-----------Reading comprehension -----

Qres = datanal[, mean(QUESTIONRESP, na.rm = TRUE), by = c("exp","prepost")]


# TODO: check 220ms statistical sig.? Y and not only

#-----------Respcorr ----

## RESULTS: main effect of delat and eccent / interaction between exp and delay
ez.all = ezANOVA(datanal[delay!=500,], 
        respcorr, 
        wid = idn, 
        within = c("delay", "eccent","prepost2"),
        between = exp)
ez.all

ez.all2 = ezANOVA(m.resp.short, 
                  mean.resp, 
                  wid = idn, 
                  within = c("delay", "eccent"),
                  between = exp)
ez.all2

## Post hoc analysis 

# since mauchly's sphericity test is violated: correction F(1.9018227,26.6255178)


# TODO: reading speed

dat_final[, `:=` (phase = ( ifelse( delay!="220" & delay!="500", "engage", ifelse(
  delay!="10" & delay!="40" & delay!="500", "orient", "noprobe")))),]


# USE this
# check usable trials
eng.pre = dat_final[phase == "engage" & prepost == "0" & include==1, ]

dat_final[phase == "engage" & prepost == "0", .N] # this is the total trials
dat_final[phase == "engage" & prepost == "0" & include==1, .N] # this is the filtered trials
print("percentage :")
dat_final[phase == "engage" & prepost == "0" & include==1, .N]/(dat_final[phase == "engage" & prepost == "0", .N])

# check usable trials
ori.pre = dat_final[phase == "orient" & prepost == "0" & include==1,]

dat_final[phase == "orient" & prepost == "0", .N] # this is the total trials
dat_final[phase == "orient" & prepost == "0" & include==1, .N] # this is the filtered trials
print("percentage :")
dat_final[phase == "orient" & prepost == "0" & include==1, .N]/(dat_final[phase == "orient" & prepost == "0", .N])

# per condition per Ps------
ori.pre[, `:=` (count = .N), by = c("delay","eccent","idn","exp")]
probe_output = ori.pre[, list(meanN = mean(count),  # samples per condition per participant
                                        semN = std.error(count)),] # SEM
eng.pre[, `:=` (count = .N), by = c("delay","eccent","idn","exp")]
eng.output = eng.pre[, list(meanN = mean(count),  # samples per condition per participant
                                          semN = std.error(count)),] # SEM


# Main analysis for replication----
ezANOVA(ori.pre, 
        dv = respcorr, 
        wid = idn, 
        within = .(eccent,delay))
ori.pre[,.(mean = mean(respcorr),sd = sd(respcorr)),by= "delay"]

ezANOVA(eng.pre, 
        dv = respcorr, 
        wid = idn, 
        within = .(eccent,delay))
eng.pre[,.(mean = mean(respcorr),sd = sd(respcorr)),by= "delay"]



# ---probe effect db
db2 = db[(probeoccur_durfix==1 & exp==1),] # change phases here & (delay=="10"|delay=="40"),or & (delay=="110"|delay=="220")
WB1 = WB[wbound==1,] # 425 (all)->sig, 209 (orient), 216
WB0 = WB[wbound==0,] # 414 (all), 175 (orient), 239
WB0 = WB0[sample(nrow(WB0), 190), ]
WBALL = rbind(WB1, WB0)

# > unique(datanal[prepost ==1 & exp==1,.N])
# [1] 531
# > unique(datanal[prepost ==1 & exp==0,.N])
# [1] 494
 e = datanal[prepost ==1 & exp==0,]
 e = eq[sample(nrow(eq), 494), ]
 en  = datanal[prepost ==1 & exp==1,]
 en= eqnorm[sample(nrow(eqnorm), 494), ]
 eq.postbtwn = rbind(eq, eqnorm)