

library(tidyverse)

df.reg<-read.csv("df_reg.csv")


#histogram
df.ph<-df.reg%>%select(nchange,nsense,stateWD,word)%>%
  pivot_longer(cols=c("nchange","nsense"),names_to="dependent",values_to="value")%>%
  mutate(dependent=ifelse(dependent=="nchange","number of changes","number of senses"))


ph.all<-ggplot(df.ph,aes(x=value))+
  theme_bw()+
  scale_y_continuous(trans='log10')+
  scale_x_continuous()+
  facet_wrap(stateWD~dependent,scales="free_x",ncol=4)+
  labs(x="")+
  geom_histogram(binwidth=1,color="black",fill="white")
ph.all



#point graph
pp.lc<-ggplot(df.reg,aes(x=lifespan,y=nchange))+
  theme_bw()+
  geom_point(size=2,alpha = 0.1)+
  facet_wrap(.~stateWD,scales="free_y")+
  labs(x="lifespan",y="number of changes")
pp.lc



#glm for obs +nob
a0<-glm(nchange ~ lifespan*stateWD, family = "poisson", data = df.reg)
summary(a0)

as<-glm(nsense ~ lifespan*stateWD, family = "poisson", data = df.reg)
summary(as)

#glm for obs
dobs.c<-df.reg%>%filter(stateWD=="obsolete words")

a1<-glm(nchange ~ lifespan, family = "poisson", data = dobs.c)
summary(a1)

a1s<-glm(nsense ~ lifespan, family = "poisson", data = dobs.c)
summary(a1s)


#glm for nob (with freq)
df.nob.frq<-read.csv("df_nob_frq.csv")

a2<-glm(nchange ~ lifespan*freqBand, family = "poisson", data = df.nob.frq)
summary(a2)

a2s<-glm(nsense ~ lifespan*freqBand, family = "poisson", data = df.nob.frq)
summary(a2s)




#glm with Berdicevskis (2020)'s data

d.a = read.delim("adjectives.csv",header=T,sep="\t")
d.n = read.delim("nouns.csv",header=T,sep="\t")
d.v = read.delim("verbs.csv",header=T,sep="\t")

all <- rbind(d.a, d.v, d.n)

all<-all[all$year >=951,]
#Set intercept to 1
all$year2 <- all$year - 950

all.c<-all%>%mutate(nchange=nmeanings+2*ncrosses)
all.c$pos <- as.factor(all.c$pos)


###glm
all.c$pos<-factor(all.c$pos,levels=c("v","adj","n"))
ba.cv<-glm(nchange ~ year2*freq*pos, family = "poisson", data = all.c)
summary(ba.cv)

ba.sv<-glm(total_nmeanings ~ year2*freq*pos, family = "poisson", data = all.c)
summary(ba.sv)

#predicted values
y.ba.cv<-predict(ba.cv,newdata=expand.grid(year2=seq(0,1100,10),freq=seq(0,8,1),pos=c("v","adj","n")),type="response")

fitted.df.cv<-cbind(expand.grid(year2=seq(0,1100,10),freq=seq(0,8,1),pos=c("v","adj","n")),y.ba.cv)%>%
  rename(predicted=y.ba.cv)


fitted.df.cv$freq<-as.factor(fitted.df.cv$freq)

pp.b.lc.prd<-ggplot(fitted.df.cv,aes(x=year2,y=predicted,group=freq))+
  theme_bw()+
  scale_y_continuous()+
  scale_x_continuous()+
  geom_point(size=0.02,aes(shape=freq,color=freq))+
  scale_shape_manual(values=seq(0,8,1))+
  facet_grid(.~pos,scales="free_y",
             labeller=labeller(pos = c(v="verb",adj="adjective",n="noun")))+
  guides(size = "none",#)+#,
         shape = guide_legend(override.aes = list(size = 5),title = "frequency band",ncol=2),
         color=guide_legend(title = "frequency band",ncol=2))+
  labs(x="year of entry",y="predicted number of changes")
pp.b.lc.prd

