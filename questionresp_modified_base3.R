rm(list = ls())
setwd("/Users/whitneyhung/Documents/panc-local/thesis/whitneydata")
# setwd("/Users/mbpr/Downloads/tmp/")
#install.packages("data.table")
# install.packages("stringr")
# install.packages("ez")
library(ez)
library(data.table)
library(stringr)
library(dplyr)
library(stringr)
library(ggplot2)
# install.packages("gridExtra")
library(gridExtra)
library(plyr); library(dplyr)
# install.packages("plotrix")
library(plotrix)
library("ggsci")



#prepare datafile
dat = read.csv("data2.csv", header = TRUE, sep = ',')
dat = as.data.table(dat)

# ----

# give warning if any of the length of each id Txxx is not equal to it's corr. length in big_qt
warning_check <- function(xfile, yfile){
  x_id_lengths <- xfile %>% group_by(id) %>% count()
  y_id_lengths <- yfile %>% group_by(id) %>% count()
  big_xy <- merge(x_id_lengths, y_id_lengths, all.x=TRUE, by = "id" )
  rm(x_id_lengths, y_id_lengths)
  for (row in 1:nrow(big_xy)){
    id <- levels(big_xy$id)[row]
    nx <- big_xy[row, "n.x"]
    ny <- big_xy[row, "n.y"]
    if ( is.na( ny ) ){
      cat("Warning 001: The id ", id, "has NA length in yfile. Please check!\n")
    }
    else if ( nx != ny ){
      cat("Warning 002: The id ", id, " has different lengths in x(", nx, ") and y(", 
          ny, "). Please check!\n")
    }
  }
  return( big_xy) 
}
# loop codes
infiles = dir(pattern='T\\d+Q\\.csv')

changefiles = function(file, target_file){
  
  Qcsv = read.csv(file, header = TRUE, sep = ',')
  
  QcsvTrim = Qcsv[,c(1,2,32)] # 3 columns needed
  
  QcsvTrim = as.data.table(QcsvTrim)
  
  colnames(QcsvTrim) = c("id","num","MSG") # same as datafile
  
  QcsvTrim[, paste0("MSG", 1:2) := tstrsplit(MSG, " ", keep = 1:2)] # split column, keep 1:2 columns only
  
  b = QcsvTrim[ (MSG1=="QUESTIONRESP" | MSG1=="TRIALID"),c(1,2,4,5)] # select QuestionResp & trialID
  
  qt = dcast(b, id + num ~ MSG1, value.var = "MSG2")
  
  return(qt)
}

# concat all files into one table: big_qt
qtlist = list()
for (i in 1:length(infiles)){
  qtlist[[i]]<- changefiles(infiles[i], dat)
}
big_qt <- data.table::rbindlist(qtlist)

# generate warnings if necessary
count_sum_table <- warning_check(dat, big_qt)
print(count_sum_table)

# all.x=TRUE: keep all rows of x in the table even if no matching from y
dat <- merge(dat, big_qt, all.x=TRUE, by = c("id", "num"))

dat[, caffeine:=(ifelse(exp==1 & prepost==1, 1,0)),]

write.csv(dat, file = "data_output.csv", sep = ",", row.names = FALSE)

dat[, mean(respcorr),by="id"]
rm(big_qt, qtlist, i, count_sum_table, demo_info)

###############################################################

d_tmp = copy(dat)
d_tmp = data.frame(d_tmp)
demo_info = as.data.frame(read.csv("demographics.csv")) 
####TODO
demo_info <- rename(demo_info, arousal1 = Pre1, arousal2 = pre2, 
                    arousal3 = post1, arousal4 = post2, cis = CIS_SUM)
demo_info <- subset(demo_info, select = -c(X, X.1))
demo_info[, 7:11] <- sapply(demo_info[, 7:11], function(x) as.numeric(as.character(x)))
d_tmp[, colnames(demo_info)[-1]] <- NA # exclude id and build empty columns
d_tmp[, c(26:36)] <- sapply(d_tmp[, c(26:36)], function(x) as.numeric(as.character(x)))


for (idnum in demo_info$id){
  d_tmp[d_tmp$id == idnum, colnames(demo_info)[-1]] <- demo_info[demo_info$id == idnum, -1]
}



##############
# adding same id for same participant

d_tmp = as.data.table(d_tmp)

d_tmp[, idn := str_sub(id,-2),]
d_tmp$idn = factor(d_tmp$idn) # change into factor
##############



#############  filter data ############ ** optional
dat_final = as.data.table(d_tmp)
# dat_final[, prepost2 := ifelse(prepost==1,"post", "pre"),]
# dat_final[, exp2 := ifelse(exp==1,"exp", "con"),] 
# dat_final[, caff2 := ifelse(caffeine==1,"caff", "noncaff"),] 
# dat_final = as.data.table(d_tmp)
nrow(dat_final)

# (i) the target word (here, Wn) was fixated; 
dat_final[,  `:=` (fix = ifelse(trigger1 <= crit_pos+4 & crit_pos+4 <= trigger2,1,0), 
                   skip = ifelse(crit_pos>trigger2,1,0)),]
nrow(dat_final[fix==1,]) # fixated using rule 1
nrow(dat_final[skip==0,]) # didn't skip using rule 2

# (2) probe- onset occurred before the termination of the first fixation on Wn;
dat_final[, probeoccur_durfix := ifelse(delay <= crit_fix,1,ifelse(delay==500,2,0)),]
# (3) the first fixation on Wn endured for at least 100 ms and
dat_final[, fix_duration := ifelse(crit_fix > 100,1,0),]
# (4) was followed by a forward saccade;
##### use fixdir==1

# (5) the probe did not occur on a blank space.
dat_final[, prob_onblank := ifelse(grepl("[a-z]", probech),1,0),]

# (6) participants with two or more sessions generating < 60 % accuracy in probe discrimination were excluded
dat_final[, probe_percent := mean(respcorr),by ="id"]

# (7) participants with reading comprehension < 85% were excluded
dat_final$QUESTIONRESP = as.numeric(dat_final$QUESTIONRESP)
dat_final[, QR85 := mean(QUESTIONRESP, na.rm = TRUE),by ="id"]

# (8) forward fixation:
dat_final[, fixforward := ifelse(next_pos-crit_pos>=0, 1, 0),]

############  Additional Filter -- after discussion with Karina 2019.08.13
# **(9) only correct question reponse trials
dat_final[, Qcorr := ifelse(QUESTIONRESP == 1,1,0), ]

dat_final[, Group := ifelse(exp==1, "experiment group", "control group")]
# **(10) exclude first few participants ----------------------------------------
# dat_final = dat_final[idn!="02" & idn!="03" & idn!="04"& idn!="05",]

dat_final[, `:=` (saccade_len = next_pos - crit_pos, word_len = trigger2-trigger1-1),]
dat_final[, prepost2 := ifelse(prepost==1,"post", "pre"),] # remane prepost values for dcast
dat_final$prepost2 = as.factor(dat_final$prepost2)
# (11) check word boundary:
dat_final[, wbound := ifelse( saccade_len > word_len, 1, 0 ),]
dat_final[, rspeed := ((next_pos-prev_pos)/(crit_fix)),]
dat_final[, cis_binary := ifelse( CIS_SUM > 20, 1, 0),]

dat_final$cis_binary = as.factor(dat_final$cis_binary)
dat_final$wbound = as.factor(dat_final$wbound)
dat_final$prepost = as.factor(dat_final$prepost)
dat_final$eccent = as.factor(dat_final$eccent)
dat_final$delay = as.factor(dat_final$delay)
dat_final$exp = as.factor(dat_final$exp)
dat_final$prepost = as.factor(dat_final$prepost)
dat_final$caffeine = as.factor(dat_final$caffeine)
dat_final$seeprobe = as.factor(dat_final$probeoccur_durfix)


nrow(dat_final)
class(dat_final$idn)

# -----
# set include variable
dat_final[,include := ifelse( delay !=500 &
                                fixforward==1 & #--direction
                                fix==1 &  #--fixate
                                skip==0 &  #--fixate
                               probeoccur_durfix !=0 & #--saw the probe
                               fix_duration==1 & #--fixate over 100 ms
                               prob_onblank==1 & #--probe not on blank
                               (probe_percent>=0.6 & probe_percent<=0.9) & #--filter whole participant
                               # probe_percent>=0.6 &
                               QR85>=0.85,   #--filter whole participant
                             1,ifelse(delay==500 & 
                                        fix_duration==1 & #--fixate over 100 ms
                                        fix==1 &  #--fixate
                                        skip==0 & #--fixate
                                        fixforward==1 & #--direction
                                        QR85>=0.85 &  #--filter whole participant
                                        (probe_percent>=0.6 & probe_percent<=0.9 #--filter whole participant
                                         ), 1, 0)),]

dat_final[,includespss := ifelse( delay !=500 &
                                fixforward==1 & #--direction
                                fix==1 &  #--fixate
                                skip==0 &  #--fixate
                                probeoccur_durfix !=0 & #--saw the probe
                                fix_duration==1 & #--fixate over 100 ms
                                prob_onblank==1 & #--probe not on blank
                                (probe_percent>=0.6) & #--filter whole participant
                                # probe_percent>=0.6 &
                                QR85>=0.85,   #--filter whole participant
                              1,0),]

dat_final[,incl.fix := ifelse( delay !=500 &
                                # fixforward==1 & #--direction
                                fix==1 &  #--fixate
                                skip==0 &  #--fixate
                                fix_duration==1 & #--fixate over 100 ms
                                prob_onblank==1 & #--probe not on blank
                                (probe_percent>=0.6) & #--filter whole participant
                                # probe_percent>=0.6 &
                                QR85>=0.85,   #--filter whole participant
                              1,ifelse(delay==500 & 
                                         fix_duration==1 & #--fixate over 100 ms
                                         fix==1 &  #--fixate
                                         skip==0 & #--fixate
                                         # fixforward==1 & #--direction
                                         QR85>=0.85 &  #--filter whole participant
                                         (probe_percent>=0.6  #--filter whole participant
                                         ), 1, 0)),]

# delete fix dir to check for regression
dat_final[,include3 := ifelse( delay !=500 &
                                # fixforward==1 & #--direction
                                fix==1 &  #--fixate
                                skip==0 &  #--fixate
                                # probeoccur_durfix !=0 & #--saw the probe
                                fix_duration==1 & #--fixate over 100 ms
                                prob_onblank==1 & #--probe not on blank
                                (probe_percent>=0.6) & #--filter whole participant
                                # probe_percent>=0.6 &
                                QR85>=0.85,   #--filter whole participant
                              1,ifelse(delay==500 & 
                                         fix_duration==1 & #--fixate over 100 ms
                                         fix==1 &  #--fixate
                                         skip==0 & #--fixate
                                         # fixforward==1 & #--direction
                                         QR85>=0.85 &  #--filter whole participant
                                         (probe_percent>=0.6  #--filter whole participant
                                         ), 1, 0)),]

# allow question response to be less than 85%
dat_final[,include4 := ifelse( delay !=500 &
                                 # fixforward==1 & #--direction
                                 # fix==1 &  #--fixate
                                 # skip==0 &  #--fixate
                                 # probeoccur_durfix!=0 & #--saw the probe
                                 # fix_duration==1 & #--fixate over 100 ms
                                 # prob_onblank==1 & #--probe not on blank
                                 (probe_percent>=0.6 ), #--filter whole participant
                                 # probe_percent>=0.6 &
                                 # QR85>=0.85,   #--filter whole participant
                               1,ifelse(delay==500 & 
                                          fix_duration==1 & #--fixate over 100 ms
                                          fix==1 &  #--fixate
                                          skip==0 & #--fixate
                                          # fixforward==1 & #--direction
                                          # QR85>=0.85 &  #--filter whole participant
                                          (probe_percent>=0.6  #--filter whole participant
                                          ), 1, 0)),]


dat_final[include==0,.N, by="id"] #T004, (T006, T106), T108, (T010, T110), T011, (T013, T113), (T015,T115), (T021, T121)

dat_final[, `:=` (phase = ( ifelse( delay!="110" & delay!="220" & delay!="500", "engage", ifelse( 
  delay!="10" & delay!="40" & delay!="500", "orient", "noprobe")))),]
############


nrow(dat_final[include==1,]) #-----delete this later & Qcorr==1
# [1] 2103 ## can be analysed
nrow(dat_final)
# [1] 2998 ## originally
nrow(dat_final[delay==500 & include==1,])
nrow(dat_final[delay!=500 & include==1,])

print("percentage valid:")
print(nrow(dat_final[include==1,])/nrow(dat_final)) #-----delete this later & Qcorr==1
print(nrow(dat_final[include==1,])/nrow(dat_final[idn!="06"|idn!="15",])) # rule out ps earlier

## use this data.table to analyse
datanal = dat_final[include==1,]



# datanal = dat_final[includespss==1,]

# datanal = dat_final[incl.fix==1,] #-------------DELETE LATER

for (i in c(10,40,110,220,500)) {
  print("delayed :")
  print(i)
  print("sample :")
  print(datanal[delay==i, .N])
}
datanal[delay!=500, `:=` (count = .N), by = c("delay","eccent","idn","exp")]
probe_output = datanal[delay!=500, list(meanN = mean(count),  # samples per condition per participant
                 semN = std.error(count)),] # SEM
datanal[delay==500, `:=` (count = .N), by = c("delay","eccent","idn","exp")]
noprobe_output = datanal[delay==500, list(meanN = mean(count),  # samples per condition per participant
                                    semN = std.error(count)),] # SEM

datanal[, `:=` (count = .N), by = c("delay","eccent","idn","exp")]
all_output = datanal[, list(meanN = mean(count),  # samples per condition per participant
                                          semN = std.error(count)),] # SEM


overview = datanal[, list(sac.mean = mean(saccade_len), 
                          sac.sd = sd(saccade_len), 
                          critfix.mean = mean(crit_fix),
                          critfix.sd = sd(crit_fix)),]



# -------
################## plot main effect

# mean(post)-mean(pre) by each time point, and by (right, left) X (con, exp)

# datanal[, exp2 := ifelse(exp==1,"exp","con"),] # rename exp values for dcast
# datanal$exp2 = as.factor(datanal$exp2)

dt1 = datanal[,c("idn","eccent","delay","prepost2","exp","respcorr")]

######## prepare 4 conditions dt

dtall = dt1[delay!=500,] # exclude no probe control trials 

# #------------/--------------/----

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





### Mean diff
dtallplot = ggplot(dtall, 
                   aes(x = delay, y = mean_diff)) + 
  geom_point( size = 3, stroke = 1, color = "#B2182B") #, color="#1380A1" 

# PLOT: 4 grid:  "exp_con" vertical, "left right" horizontal
dtallplot + facet_grid(EXP ~ LR)+ 
  # ggtitle("Mean difference before and after caffeine manipulation")+ #graph title
  theme(plot.title = element_text(lineheight=.8, face="bold", size =20)) + 
  geom_errorbar(aes(ymin=mean_diff-se.dif, ymax=mean_diff+se.dif), width=.2, color = "#B2182B" ) + #, color = "#1380A1"
    scale_x_discrete(name="Time Delay (ms)") +
    scale_y_continuous(name="Probe discrimination difference between before and after manipulation")+
                       # labels=c("-40%", "-20%", "0%", "20%","40%"))+
  geom_hline(yintercept=0, color="grey50", linetype="dashed", size=.8, alpha=.4)+ #, linetype="dashed", color = "#FAAB18", #2166AC
  # geom_text(aes(label=round(mean_diff,2)), hjust = 1.5, size=3.5)+ # this is the number labels
  # scale_colour_manual("#1380A1")+ 
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  theme(legend.text = element_text(size=16))+
        # , plot.margin = margin(2, 2, 2, 2, "cm"))+ 
  theme(axis.line = element_line(size = 3, colour = "grey80"))+
  theme(strip.text.x = element_text(size=16),    # with caffeine and left right probe
        strip.text.y = element_text(size=16)) +
  theme(axis.title.x = element_text(size=18, face = "bold"),    # x,y axis title
        axis.text.x  = element_text(vjust=0.5, size=14),  # x,y axis value
        axis.title.y = element_text(size=18, face = "bold"),   # x,y axis title
        axis.text.y  = element_text(vjust=0.5, size=14))  # x,y axis value
  # scale_colour_manual(values = c("#1380A1"))
  # bbc_style()
# -------






#----------/---------------Original plot

##----PREPARE dtall (Additional)

dt1 = datanal[,c("idn","eccent","delay","prepost2","exp","respcorr")]

######## prepare 4 conditions dt

dtall = dt1[delay!=500,] # exclude no probe control trials 


# #------------/--------------/----

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


##----PREPARE dtall

### wide to long format---------------
dtallshort = dtall[,c(1,4,5,8,9,11,12)] # concise data
dtall_rawdat = melt(dtallshort, id.vars = c("delay", "LR","EXP",
                                            "respcorr_sd_pre",
                                            "respcorr_sd_post"))
# change label
levels(dtall_rawdat$variable)[levels(
  dtall_rawdat$variable)=="respcorr_mean_post"] <- "After treatment" # change label for legend exp
levels(dtall_rawdat$variable)[levels(
  dtall_rawdat$variable)=="respcorr_mean_pre"] <- "Before treatment" # change label for legend exp



  dtallplot2 = ggplot(dtall_rawdat, 
                    aes(x = delay, y = value, color = variable, group = variable)) + 
    geom_point(size = 3, stroke=0.3) + geom_line()
# PLOT: 4 grid with "exp_con" vertical, "left right" horizontal
  dtallplot2 + facet_grid(EXP ~ LR)+
    ggtitle("Before and after caffeine manipulation comparison")+ 
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    theme(plot.title = element_text(lineheight=.8, face="bold")) + 
    geom_errorbar(aes(ymin=value-se.dif, ymax=value+se.dif), width=.2)+ 
    scale_x_discrete(name="Time Delay (ms)") +
    scale_y_continuous(name="Correct probe discrimination")+
    # geom_text(aes(label=round(value,2)), hjust = 1.5, vjust = 2, size=3.5) +
    # scale_colour_manual(values = c("#0072B2", "#D55E00"))+ # use manual pallette
    scale_color_jco()+ # use pallette here: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
    theme(strip.text.x = element_text(size=16),
          strip.text.y = element_text(size=16))+
    theme(legend.title=element_blank()) +
    theme(legend.text = element_text(size=16))+
    theme(axis.title.x = element_text(size=16, face = "bold"),    # x,y axis title
          axis.text.x  = element_text(vjust=0.5, size=14),  # x,y axis value
          axis.title.y = element_text(size=16, face = "bold"),   # x,y axis title
          axis.text.y  = element_text(vjust=0.5, size=14))  # x,y axis value
  # strip.background = element_rect(fill="#FFFFCC"))
  
# devtools::install_github('bbc/bbplot')
# if(!require(pacman))install.packages("pacman")

# pacman::p_load('dplyr', 'tidyr', 'gapminder',
#                'ggplot2',  'ggalt',
#                'forcats', 'R.utils', 'png', 
#                'grid', 'ggpubr', 'scales',
#                'bbplot')

write.csv(datanal, file = "datanal.csv")

# Big raw data table
# datanal[delay!=500, mean(respcorr), by = c("eccent","exp","prepost","delay")]







# Draw ORIENTING phase------/-------------/------------
  ### Mean diff
  dtallplot = ggplot(dtall[(delay!=10&delay!=40), ], 
                   aes(x = delay, y = mean_diff)) + 
    geom_point( size = 3, stroke = 1, color = "#0072B2")  #, color="#1380A1" 
  

  # PLOT: 4 grid:  "exp_con" vertical, "left right" horizontal
  dtallplot + facet_grid(EXP ~ LR)+ 
    ggtitle("Disengagement and orienting phase")+ #graph title
    theme(plot.title = element_text(lineheight=.8, face="bold", size =20)) + 
    geom_errorbar(aes(ymin=mean_diff-se.dif, ymax=mean_diff+se.dif), width=.2, color = "#0072B2" ) + #, color = "#1380A1"
    scale_x_discrete(name="Time Delay (ms)") +
    scale_y_continuous(name="Probe discrimination difference between before and after manipulation")+
    # labels=c("-40%", "-20%", "0%", "20%","40%"))+
    geom_hline(yintercept=0, color="grey50", linetype="dashed", size=.8, alpha=.4)+ #, linetype="dashed", color = "#FAAB18", 
    # geom_text(aes(label=round(mean_diff,2)), hjust = 1.5, size=3.5)+ # this is the number labels
    # scale_colour_manual("#1380A1")+ 
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    theme(legend.text = element_text(size=16))+
    # , plot.margin = margin(2, 2, 2, 2, "cm"))+ 
    theme(axis.line = element_line(size = 3, colour = "grey80"))+
    theme(strip.text.x = element_text(size=16),    # with caffeine and left right probe
          strip.text.y = element_text(size=16)) +
    theme(axis.title.x = element_text(size=18, face = "bold"),    # x,y axis title
          axis.text.x  = element_text(vjust=0.5, size=14),  # x,y axis value
          axis.title.y = element_text(size=18, face = "bold"),   # x,y axis title
          axis.text.y  = element_text(vjust=0.5, size=14))  # x,y axis value
  
  
  
  
  
  # Draw ENGAGEMENT  phase------/-------------/------------
  ### Mean diff
  dtallplot = ggplot(dtall[(delay!=220), ], # delay!=110 & 
                     aes(x = delay, y = mean_diff)) + 
    geom_point( size = 3, stroke = 1, color = "#0072B2")  #, color="#1380A1" 
  
  
  # PLOT: 4 grid:  "exp_con" vertical, "left right" horizontal
  dtallplot + facet_grid(EXP ~ LR)+ 
    ggtitle("Engagement phase")+ #graph title
    theme(plot.title = element_text(lineheight=.8, face="bold", size =20)) + 
    geom_errorbar(aes(ymin=mean_diff-se.dif, ymax=mean_diff+se.dif), width=.2, color = "#0072B2" ) + #, color = "#1380A1"
    scale_x_discrete(name="Time Delay (ms)") +
    scale_y_continuous(name="Probe discrimination difference between before and after manipulation")+
    # labels=c("-40%", "-20%", "0%", "20%","40%"))+
    geom_hline(yintercept=0, color="grey50", linetype="dashed", size=.8, alpha=.4)+ #, linetype="dashed", color = "#FAAB18", 
    # geom_text(aes(label=round(mean_diff,2)), hjust = 1.5, size=3.5)+ # this is the number labels
    # scale_colour_manual("#1380A1")+ 
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    theme(legend.text = element_text(size=16))+
    # , plot.margin = margin(2, 2, 2, 2, "cm"))+ 
    theme(axis.line = element_line(size = 3, colour = "grey80"))+
    theme(strip.text.x = element_text(size=16),    # with caffeine and left right probe
          strip.text.y = element_text(size=16)) +
    theme(axis.title.x = element_text(size=18, face = "bold"),    # x,y axis title
          axis.text.x  = element_text(vjust=0.5, size=14),  # x,y axis value
          axis.title.y = element_text(size=18, face = "bold"),   # x,y axis title
          axis.text.y  = element_text(vjust=0.5, size=14))  # x,y axis value
  