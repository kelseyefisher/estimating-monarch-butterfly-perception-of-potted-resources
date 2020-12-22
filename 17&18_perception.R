setwd("C:/Users/kefisher/Box/Publications/17&18 Perception Distance/Data Analysis")

summary<-read.csv("1718_trialsummary_1to5dayscaptive_5255075_10mph.csv", header=TRUE)
names(summary)
winddirection <- data.frame("winddirection" = c("N","NNE", "NE", "NEE", "E", "SEE", "SE", "SSE", "S", "SSW", "SW", "SWW", "W", "NWW", "NW", "NNW"), "windbearing" = c(0, 22.5, 45, 67.5, 90, 112.5, 135, 157.5, 180, 202.5, 225, 247.5, 270, 292.5, 315, 337.5))
summary$windbearing <- winddirection$windbearing[match(summary$winddirection, winddirection$winddirection)]
resource <- read.csv("1718_releasetoresourcecentroid_GMEoutput.csv", header=TRUE)
summary$resourcebearing <- resource$resourcebearing[match(summary$monarchrunid, resource$monarchrunid)]


#is there a difference in the number of duds based on tag, shamtag, or untag?
table(summary$attachment, summary$fly)
chisq.test(summary$attachment, summary$fly, correct=FALSE)

#dayscaptive?
table(summary$dayscaptive, summary$fly)
chisq.test(summary$dayscaptive, summary$fly, correct=FALSE)

#release distance?
table(summary$distance, summary$fly)
chisq.test(summary$distance, summary$fly, correct=FALSE)



#all data, circular bearing analyses
#correct for resources or wind to be at a bearing of zero
library(circular)

bear<- subset(summary, select=c("monarchrunid", "distance", "year", "temp", "cloud", "windkph", "euclidianbearing", "euclidiandistance", "resourcebearing", "windbearing", "firststep", "firstbearing"))
bear<-na.omit(bear)

bear$rescor <- (bear$euclidianbearing - bear$resourcebearing)
bear$adjeucbear <- ifelse(bear$rescor <0, bear$rescor +360, 
                          ifelse(bear$rescor >360, bear$rescor -360, bear$rescor)) 
bear$adjeucbeardeg<-circular(bear$adjeucbear, units="degrees", template="geographics")


bear$rescor1 <- (bear$firstbearing - bear$resourcebearing)
bear$adjfirstbear <- ifelse(bear$rescor1 <0, bear$rescor1 +360, 
                          ifelse(bear$rescor1 >360, bear$rescor1 -360, bear$rescor1)) 
bear$adjfirstbeardeg<-circular(bear$adjfirstbear, units="degrees", template="geographics")


names(bear)



#########################################################################
#do monarch orient to the resources at each distance?

library(circular)
library(dplyr)

bear5<-bear %>% filter(distance == 5)
rayleigh.test(bear5$adjeucbeardeg)
#is it unimodal toward the resources?
bear5$adjres<-c(0)
bear5$adjres<-circular(bear5$adjres, units="degrees", template="geographics")
#watson.two.test(bear5$adjeucbeardeg, bear5$adjres)
data <- list(
  x5 = circular(bear5$adjeucbeardeg, units="degrees", template="geographics"),
  xres = circular(bear5$adjres, units="degrees", template="geographics")
)
watson.williams.test(data)


bear25<-bear %>% filter(distance == 25)
rayleigh.test(bear25$adjeucbeardeg)
#is it unimodal toward the resources?
bear25$adjres<-c(0)
bear25$adjres<-circular(bear25$adjres, units="degrees", template="geographics")
#watson.two.test(bear25$adjeucbeardeg, bear25$adjres)
data <- list(
  x25 = circular(bear25$adjeucbeardeg, units="degrees", template="geographics"),
  xres = circular(bear25$adjres, units="degrees", template="geographics")
)
watson.williams.test(data)


bear50<-bear %>% filter(distance == 50)
rayleigh.test(bear50$adjeucbeardeg)
#is it unimodal toward the resources?
bear50$adjres<-c(0)
bear50$adjres<-circular(bear50$adjres, units="degrees", template="geographics")
#watson.two.test(bear25$adjeucbeardeg, bear25$adjres)
data <- list(
  x50 = circular(bear50$adjeucbeardeg, units="degrees", template="geographics"),
  xres = circular(bear50$adjres, units="degrees", template="geographics")
)
watson.williams.test(data)


bear75<-bear %>% filter(distance == 75)
rayleigh.test(bear75$adjeucbeardeg)




#is 5 orienting with the wind?

#for comparison analysis
bear5$windcor <- (bear5$euclidianbearing - bear5$windbearing)
bear5$adjeucbearwind <- ifelse(bear5$windcor <0, bear5$windcor +360, 
                               ifelse(bear5$windcor >360, bear5$windcor -360, bear5$windcor)) 
# #for image
bear5$windcor2 <- (bear5$adjeucbearwind - 180)
bear5$adjeucbearwind2 <- ifelse(bear5$windcor2 <0, bear5$windcor2 +360, 
                                ifelse(bear5$windcor2 >360, bear5$windcor2 -360, bear5$windcor2))

bear5$adjeucbearwinddeg<-circular(bear5$adjeucbearwind, units="degrees", template="geographics")
bear5$adjeucbearwinddeg2<-circular(bear5$adjeucbearwind2, units="degrees", template="geographics")


rayleigh.test(bear5$adjeucbearwinddeg2)
#is it unimodal toward the resources?
bear5$adjwind<-c(180)
bear5$adjwind<-circular(bear5$adjwind, units="degrees", template="geographics")
#watson.two.test(bear5$adjeucbearwinddeg2, bear5$adjwind)
data <- list(
  x5 = circular(bear5$adjeucbearwinddeg2, units="degrees", template="geographics"),
  xwind = circular(bear5$adjwind, units="degrees", template="geographics")
)
watson.williams.test(data)


#is 25 orienting with the wind?

#for comparison analysis
bear25$windcor <- (bear25$euclidianbearing - bear25$windbearing)
bear25$adjeucbearwind <- ifelse(bear25$windcor <0, bear25$windcor +360, 
                                ifelse(bear25$windcor >360, bear25$windcor -360, bear25$windcor)) 
# #for image
bear25$windcor2 <- (bear25$adjeucbearwind - 180)
bear25$adjeucbearwind2 <- ifelse(bear25$windcor2 <0, bear25$windcor2 +360, 
                                 ifelse(bear25$windcor2 >360, bear25$windcor2 -360, bear25$windcor2))

bear25$adjeucbearwinddeg<-circular(bear25$adjeucbearwind, units="degrees", template="geographics")
bear25$adjeucbearwinddeg2<-circular(bear25$adjeucbearwind2, units="degrees", template="geographics")


rayleigh.test(bear25$adjeucbearwinddeg2)
#is it unimodal toward the resources?
bear25$adjwind<-c(180)
bear25$adjwind<-circular(bear25$adjwind, units="degrees", template="geographics")
#watson.two.test(bear25$adjeucbearwinddeg2, bear25$adjwind)
data <- list(
  x25 = circular(bear25$adjeucbearwinddeg2, units="degrees", template="geographics"),
  xwind = circular(bear25$adjwind, units="degrees", template="geographics")
)
watson.williams.test(data)


#is 50 orienting with the wind?

#for comparison analysis
bear50$windcor <- (bear50$euclidianbearing - bear50$windbearing)
bear50$adjeucbearwind <- ifelse(bear50$windcor <0, bear50$windcor +360, 
                                ifelse(bear50$windcor >360, bear50$windcor -360, bear50$windcor)) 
# #for image
bear50$windcor2 <- (bear50$adjeucbearwind - 180)
bear50$adjeucbearwind2 <- ifelse(bear50$windcor2 <0, bear50$windcor2 +360, 
                                 ifelse(bear50$windcor2 >360, bear50$windcor2 -360, bear50$windcor2))

bear50$adjeucbearwinddeg<-circular(bear50$adjeucbearwind, units="degrees", template="geographics")
bear50$adjeucbearwinddeg2<-circular(bear50$adjeucbearwind2, units="degrees", template="geographics")


rayleigh.test(bear50$adjeucbearwinddeg2)
#is it unimodal toward the resources?
bear50$adjwind<-c(180)
bear50$adjwind<-circular(bear50$adjwind, units="degrees", template="geographics")
#watson.two.test(bear50$adjeucbearwinddeg2, bear50$adjwind)
data <- list(
  x50 = circular(bear50$adjeucbearwinddeg2, units="degrees", template="geographics"),
  xwind = circular(bear50$adjwind, units="degrees", template="geographics")
)
watson.williams.test(data)



#pretty graph

cirmean5<-mean(bear5$adjeucbeardeg)
cirmean25<-mean(bear25$adjeucbeardeg)
cirmean50<-mean(bear50$adjeucbeardeg)
cirmean75<-mean(bear75$adjeucbeardeg)
cirmeanres<-mean(bear5$adjres)

plot.circular(bear5$adjeucbeardeg, stack=TRUE, col="cornflowerblue", shrink = 1.3)
points.circular(bear25$adjeucbeardeg, stack=TRUE, col="chocolate")
points.circular(bear50$adjeucbeardeg, stack=TRUE, col="darkslategrey")
#points.circular(bear75$adjeucbeardeg, stack=TRUE, col="tan4")
arrows.circular(cirmean5, col="cornflowerblue", lwd=2)
arrows.circular(cirmean25, col="chocolate", lwd=2)
arrows.circular(cirmean50, col="darkslategrey", lwd=2)
#arrows.circular(cirmean75, col="tan4", lwd=2)
arrows.circular(cirmeanres, col="palegreen4", lwd=3, lty="twodash")
legend("bottom", legend=c("Resources", "5 m", "25 m", "50 m"), col=c("palegreen4", "cornflowerblue", "chocolate", "darkslategrey"), lwd=2, title="Mean Bearing", lty=c("twodash", "solid", "solid", "solid"), horiz = TRUE)




#########################################################################################
#FIRST STEP

#########################################################################
#do monarch orient to the resources at each distance?


rayleigh.test(bear5$adjfirstbeardeg)
rayleigh.test(bear25$adjfirstbeardeg)
rayleigh.test(bear50$adjfirstbeardeg)
rayleigh.test(bear75$adjfirstbeardeg)

#pretty graph?

cirmean5b<-mean(bear5$adjfirstbeardeg)
cirmean25b<-mean(bear25$adjfirstbeardeg)
cirmean50b<-mean(bear50$adjfirstbeardeg)
cirmean75b<-mean(bear75$adjfirstbeardeg)
cirmeanresb<-0
cirmeanresb<-circular(cirmeanres, units="degrees", template="geographics")

plot.circular(bear5$adjfirstbeardeg, stack=TRUE, col="cornflowerblue", shrink = 1.3)
points.circular(bear25$adjfirstbeardeg, stack=TRUE, col="chocolate")
points.circular(bear50$adjfirstbeardeg, stack=TRUE, col="darkslategrey")
#points.circular(bear75$adjfirstbeardeg, stack=TRUE, col="tan4")
arrows.circular(cirmean5b, col="cornflowerblue", lwd=2)
arrows.circular(cirmean25b, col="chocolate", lwd=2)
arrows.circular(cirmean50b, col="darkslategrey", lwd=2)
#arrows.circular(cirmean75b, col="tan4", lwd=2)
arrows.circular(cirmeanresb, col="palegreen4", lwd=3, lty="twodash")
legend("bottom", legend=c("Resources", "5 m", "25 m", "50 m"), col=c("palegreen4", "cornflowerblue", "chocolate", "darkslategrey"), lwd=2, title="Mean Bearing", lty=c("twodash", "solid", "solid", "solid"), horiz = TRUE)




#################
#how many directed "toward resource +/- 45"

bear5o<-bear5 %>% filter((adjeucbeardeg >=315) | (adjeucbeardeg <=45))
bear25o<-bear25 %>% filter((adjeucbeardeg >=315) | (adjeucbeardeg <=45))
bear50o<-bear50 %>% filter((adjeucbeardeg >=315) | (adjeucbeardeg <=45))
bear75o<-bear75 %>% filter((adjeucbeardeg >=315) | (adjeucbeardeg <=45))

nrow(bear5)
nrow(bear5o)
nrow(bear25)
nrow(bear25o)
nrow(bear50)
nrow(bear50o)
nrow(bear75)
nrow(bear75o)

##had a step toward resources

fly<-read.csv("1718_locationsonly_forflightsteps.csv", header=TRUE)
steps<-merge(fly, summary, by="monarchrunid")
names(steps)



steps$rescorstep <- (steps$bearing - steps$resourcebearing)
steps$adjstepbear <- ifelse(steps$rescorstep <0, steps$rescorstep +360, 
                          ifelse(steps$rescorstep >360, steps$rescorstep -360, steps$rescorstep)) 
steps$adjstepbear<-circular(steps$adjstepbear, units="degrees", template="geographics")

names(steps)

steps5<-steps %>% filter(distance == 5)
steps25<-steps %>% filter(distance == 25)
steps50<-steps %>% filter(distance == 50)
steps75<-steps %>% filter(distance == 75)

steps5o<-steps5 %>% filter((adjstepbear >=315) | (adjstepbear <=45))
steps25o<-steps25 %>% filter((adjstepbear >=315) | (adjstepbear <=45))
steps50o<-steps50 %>% filter((adjstepbear >=315) | (adjstepbear <=45))
steps75o<-steps75 %>% filter((adjstepbear >=315) | (adjstepbear <=45))



steps5o$stepbear<-format(steps5o$adjstepbear, scientific=FALSE)
steps25o$stepbear<-format(steps25o$adjstepbear, scientific=FALSE)
steps50o$stepbear<-format(steps50o$adjstepbear, scientific=FALSE)
steps75o$stepbear<-format(steps75o$adjstepbear, scientific=FALSE)


nrow(steps5o)
nrow(steps5)
nrow(steps25o)
nrow(steps25)
nrow(steps50o)
nrow(steps50)
nrow(steps75o)
nrow(steps75)

orient<-read.csv("1718_orientland.csv", header=TRUE)
chisq.test(orient$distance, orient$steps, correct=FALSE)
chisq.test(orient$distance, orient$effective, correct=FALSE)
chisq.test(orient$distance, orient$land, correct=FALSE)

min(steps5o$steplength)
max(steps5o$steplength)
min(steps25o$steplength)
max(steps25o$steplength)
min(steps50o$steplength)
max(steps50o$steplength)
min(steps75o$steplength)
max(steps75o$steplength)


###relationship between wind speed, or temperature with direction


bear$toward<-ifelse((bear$adjeucbeardeg >=315) | (bear$adjeucbeardeg <=45), "toward", "away")

sum(bear$toward == "toward")
sum(bear$toward == "away")



names(bear)


t.test(bear$temp~bear$toward)
t.test(bear$windkph~bear$toward)


towardwind<-subset(bear, toward == "toward")
write.csv(towardwind, "towardwind.csv")
awaywind<-subset(bear, toward == "away")
write.csv(awaywind, "awaywind.csv")

mean(towardwind$windkph)
sd(towardwind$windkph)

mean(awaywind$windkph)
sd(awaywind$windkph)



#######################

mean(bear$euclidiandistance)
sd(bear$euclidiandistance)
max(bear$euclidiandistance)
nrow(bear)

library(ggplot2)
ggplot(data=bear, aes(x=euclidiandistance)) +
  geom_histogram(breaks=seq(0, 1500, by=25),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("") +
  ylab("")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,65))+
  scale_x_continuous(expand=c(0,0), limits=c(0,1550))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 25))



totaldist<-aggregate(steplength ~ monarchrunid, steps, sum)
names(totaldist)
nrow(steps)

mean(totaldist$steplength)
sd(totaldist$steplength)
max(totaldist$steplength)

library(ggplot2)
ggplot(data=totaldist, aes(x=steplength)) +
  geom_histogram(breaks=seq(0, 1500, by=25),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("") +
  ylab("")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,65))+
  scale_x_continuous(expand=c(0,0), limits=c(0,1550))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 25))





mean(steps$steplength)
sd(steps$steplength)
max(steps$steplength)








##turn angles

turn<-read.csv("1718_locationsonly.csv", header=TRUE)
names(turn)
turn<-na.omit(turn)


mean(turn$turnangle)
sd(turn$turnangle)

library(ggplot2)
ggplot(data=turn, aes(x=turnangle)) +
  geom_histogram(breaks=seq(-180, 180, by=25),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Turn Angle (degrees)") +
  ylab("Count")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,65))+
  scale_x_continuous(expand=c(0,0), limits=c(-180,180))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 25))






#steplengths
library(ggplot2)
ggplot(data=steps, aes(x=steplength)) +
  geom_histogram(breaks=seq(0, 2000, by=10),
                 col="black",
                 fill="black",
                 alpha = .2) +
  xlab("Flight Step Length (m)") +
  ylab("Count")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,180))+
  scale_x_continuous(expand=c(0,0), limits=c(0,900))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 25))
