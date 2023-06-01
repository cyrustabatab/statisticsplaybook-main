
library(tidyverse)
library(patchwork)
library(networkD3)


draft <- read_csv('draft.csv')


head(draft)

draft %>% 
  dim()


draft %>% 
  select(-c(3,4,16:24)) %>% 
  slice(-c(90,131)) -> draft







draft %>% 
  summarize(across(everything(), ~n_distinct(.x)))  %>% 
  t() %>% 
  as_tibble(rownames = 'Col')  %>% 
  filter(V1 <= 35) %>% 
  pull(Col) -> column_names


draft %>% 
  mutate(across(all_of(column_names),as.factor)) -> draft



draft %>% 
  mutate(Born2=ifelse(Born == 'us','USA','World') %>% as.factor())-> draft

draft %>% glimpse()


draft %>% 
  mutate(College2=ifelse(is.na(College),0,1) %>% as.factor()) -> draft


draft %>% 
  count(College2)

draft %>% 
  pull(Pos) %>% 
  levels()


draft %>% 
  mutate(Pos2=case_when(Pos == 'G' ~ 'Guard',
                         Pos == 'F' ~ 'Forward',
                         Pos == 'C' ~ 'Center',
                         Pos %in% c('F-C','C-F') ~ 'Big',
                         T ~ 'Swingman')) -> draft

draft %>% 
  select(-ncol(.)) -> draft

draft %>%
  select(Pos,Pos2)


draft %>% 
  head() %>% 
  View()

draft %>% 
  summary(0)

library(sqldf)


sqldf('select max(age),player,tm,pk,year FROM draft')



draft %>% 
  ggplot(aes(x=WS))





s


saveRDS(draft,'draft.rds')  

draft <- readRDS('draft.rds')

draft %>% 
  ggplot(aes(x=WS)) + 
  geom_histogram(fill='royalblue3',bins=8,color='white') + 
  labs(title='Carerr Win Shares Distribution of NBA first round selections') + 
  theme(plot.title=element_text(face='bold'))


draft %>% 
  mutate(win_share_cat=ifelse(WS >=75,'high','low')) %>% 
  count(win_share_cat)


ggplot(draft,aes(x=College2,y=WS)) + 
  geom_boxplot(color='orange4',fill='orange1') + 
  stat_summary(fun=mean,geom='point',color='white') +
  facet_wrap(~Born2) + 
  scale_x_discrete(breaks=0:1,labels=c("No COllege",'COllege'))


glimpse(draft)
ggplot(draft,aes(x=Year,y=WS)) + 
  geom_boxplot(fill='dodgerblue4',color='dodgerblue')  +
  stat_summary(fun=mean,geom='point',color='white',fill='white',size=8,shape=20)



draft %>% 
  group_by(Year) %>% 
  tally(WS >= 75)


draft %>%
  filter(Year == 2000) %>% 
  arrange(desc(WS)) %>% 
  select(Player,Pk)



draft %>% 
filter(WS >= 100)   %>% 
  group_by(Year) %>% 
  summarize(med=median(Pk),mean=mean(Pk))



draft %>% 
  select(6,12:15) %>% 
  cor() %>% 
  as_tibble(rownames='Var1') %>% 
  pivot_longer(-Var1,names_to='Var2')  %>% 
  ggplot(aes(x=Var1,y=Var2,fill=value)) + 
  geom_tile()  + 
  scale_fill_gradient2(midpoint=0.5,mid='grey84',limits=c(-1,1)) + 
  geom_text(aes(x=Var1,y=Var2,label=round(value,2)))





draft %>% 
  ggplot(aes(x=Born2,y=WS)) + 
  stat_summary(fun=mean,geom='bar') + 
  geom_text(aes(label=trunc(mean(WS))))


draft %>% 
  group_by(Born2) %>% 
  summarize(meanWS=mean(WS),
            medianWS=median(WS)) -> t


t %>% 
  ggplot(aes(x=Born2,y=meanWS)) + 
  geom_col() + 
  ylim(0,35) + 
  theme(plot.title=element_text(face='bold')) -> p1


t %>% 
  pivot_longer(-Born2) %>% 
  ggplot(aes(x=Born2,y=value,fill=name)) + 
  geom_col() + 
  facet_wrap(~name,labeller=labeller(name=c(meanWS='Mean Win Shares',medianWS='Median Win Shares'))) + 
  geom_text(aes(label=trunc(value)),vjust=-0.3) + 
  scale_fill_manual(values=c(meanWS='darkorchid4',medianWS='sienna1')) +   
  ylim(0,35) + 
  theme(legend.position='none') + 
  labs(x='Born',y='Win Shares')


draft %>% 
  group_by(Pos2,Born2,College2)  %>% 
  summarize(mean=mean(WS)) %>% 
  ggplot(aes(x=Pos2,y=mean)) + 
  geom_col() + 
  labs(title='Average Win Shares by Place of Birth',y='Win Shares',subtitle='2000-2009 NBA Drafts') + 
  facet_grid(Born2 ~ College2,labeller=labeller(College2=c("0"="No College","1"="College"))) -> p1


draft %>% 
  group_by(Pos2,Born2,College2)  %>% 
  summarize(mean=median(WS)) %>% 
  ggplot(aes(x=Pos2,y=mean)) + 
  geom_col(fill='darkorchid4') + 
  labs(title='Average Win Shares by Place of Birth',y='',subtitle='2000-2009 NBA Drafts') + 
  facet_grid(Born2 ~ College2,labeller=labeller(College2=c("0"="No College","1"="College"))) -> p2

draft %>% 
  names()
  


p1 + p2


draft %>% 
  mutate(Pk2=as.integer(Pk)) %>% 
  filter(WS >= 75) %>% 
  summarize(mean(Pk2),median(Pk2))


draft %>% 
  pull(WS) %>% 
  summary()


draft %>% 
  mutate(Pk2=as.integer(Pk),above_avg=WS >= quantile(WS,.75)) %>% 
  group_by(above_avg) %>% 
  summarize(mean(Pk2),median(Pk2))

draft %>% 
  mutate(Pk2=as.integer(Pk)) %>% 
  mutate(draft_cat=case_when(Pk2 <= 10 ~ 'Top 10',
                             Pk2 <= 20 ~ '10-20',
                             T ~ '20-30') %>% as.factor())  %>% 
  ggplot(aes(x=draft_cat,y=WS)) + 
  geom_boxplot()


draft %>% 
  mutate(Pk2=as.integer(Pk)) %>% 
  mutate(cat=cut(Pk2,breaks =seq(1,31,by=5),include.lowest=T)) %>% 
  ggplot(aes(x=cat,y=WS)) + 
  geom_boxplot()


?cut


n <- 10000
replicate(n, {
  
  prize_door <- sample(1:3,size=1) 
  
  
  
  
 
cut(draft2$Pk,breaks=seq(1,31,5 ),right = F,include.lowest = T,labels=c('1-5','6-10','11-15','16-20','21-25','26-30'))  
  
  
  
  
draft2 <- read_csv("draft2.csv")  

draft2 %>% 
  mutate(Pk2=cut(draft2$Pk,breaks=seq(1,31,5 ),right = F,include.lowest = T,labels=c('1-5','6-10','11-15','16-20','21-25','26-30')) %>% as.factor()  ) %>% 
  
  summarize(meanGames=mean(G),
            medianGames=median(G),
            pctGames=sum(G)/sum(draft2$G)) %>% 
  ggplot(aes(x=Pk2,y=meanGames)) + 
  geom_col(fill='coral') + 
  geom_text(aes(label=trunc(meanGames)),vjust=-0.1) + 
  geom_label(aes(label=trunc(pctGames * 100)),vjust=1.5)

draft2 %>% 
  mutate(Pk2=cut(draft2$Pk,breaks=seq(1,31,5 ),right = F,include.lowest = T,labels=c('1-5','6-10','11-15','16-20','21-25','26-30')) %>% as.factor()  ) %>% 
  group_by(Pk2) %>% 
  summarize(meanGames=mean(G),
            medianGames=median(G),
            pctGames=sum(G)/sum(draft2$G)) -> t 


t %>% 
  pivot_longer(-c(Pk2,pctGames)) %>% 
  ggplot(aes(x=Pk2,y=value,fill=name)) + 
  facet_wrap(~name)+ 
  geom_col() + 
  geom_text(aes(label=trunc(value)),vjust=-0.1) + 
  geom_label(aes(label=trunc(pctGames * 100)),vjust=1.5)+ 
  scale_fill_brewer(palette='Set1',name='stat')



draft2 %>% 
  mutate(Pk2=cut(draft2$Pk,breaks=seq(1,31,5 ),right = F,include.lowest = T,labels=c('1-5','6-10','11-15','16-20','21-25','26-30')) %>% as.factor()  ) -> picks


names(draft2 )

picks %>% 
  ggplot(aes(x=Pk2,y=MP)) + 
  stat_summary(fun='mean',geom='bar') +
  geom_label(='mean')


picks %>% 
  group_by(Pk2) %>% 
  summarize(meanMPG=mean(WS,na.rm=T),medianMPG=median(WS,na.rm=T),pct=sum(WS,na.rm=T)/sum(.$WS,na.rm=T)) %>% 
  pivot_longer(-c(Pk2,pct),names_to='stat') %>% 
  group_split(stat) %>%  
  map2(c('Mean','Median'),~.x %>% ggplot(aes(x=Pk2,y=value)) +
        geom_col(color='deepskyblue4',fill='deepskyblue3') + ylim(0,80) +
        geom_text(aes(label=round(value,2)),vjust=-0.15) + 
        geom_label(aes(label=trunc(pct * 100)),vjust=1) + 
        labs(x='Pick Category',y='WS',title=paste(.y,' Career Win Shares'),subtitle='FOr draft picks between 2000-2009 ',caption='Regular Season Games only') + 
        theme(plot.title=element_text(face='bold'))) -> x


  x[[1]] %>% 
    ggplot(aes(x=Pk2,y=value)) + 
    geom_col()

  x[[1]]
  
  
  
  
  
draft2 
  
draft2 %>% 
  count(Born2,College2)



draft2 %>% 
  count(Born2,College2) %>% 
  select(n) -> counts


draft2 %>% 
  summarize(n_distinct(Born2)) %>% pull() -> num_born

draft2 %>% 
  summarize(n_distinct(College2)) %>% pull() -> num_college


to_links <- 0:(num_born - 1) + num_college


expand.grid(x=0:(num_born - 1),y=to_links) %>% 
  arrange(x) %>% 
  bind_cols(counts) %>% 
  
  as.matrix() -> x


install.packages('networkD3')


draft %>% 
  pull(Born2) %>% 
  levels() -> x


draft2 %>% 
  distinct(College2) %>% 
  pull() %>% 
  sort()  -> y


nodes <- data.frame(name=c(x,y))

nodes

nodes
links <- x

nodes


links <- as.data.frame(links)

names(links) <- c("source",'target','value')

links

nodes
links

sankeyNetwork(Links=links,Nodes=nodes,
              Source='source',Target='target',
              Value='value',NodeID='name',
              fontSize=12,nodeWidth=30) %>% 
saveNetwork('test.html')

?sankeyNetwork

ggsave('test.png')
  

draft2 %>% 
  group_by(Pk) %>% 
  summarize(meanWS=mean(WS)) -> x


dist(x)

x %>% 
  filter(Pk %in% 7:8) %>% 
  summarize(diff(meanWS))



draft2 %>% 
  mutate(designation=case_when(WS >= 100 ~ 'superstar',
                               WS >= 75 ~ 'star',
                               WS >= 50 ~ 'starter',
                               WS >= 25 ~ 'reserve',
                               T ~ 'marginal')) -> draft2

draft2 %>% 
  filter(Pk <= 5) %>% 
  count(designation) %>% 
  mutate(n =n/sum(n))

draft2 %>% 
  filte
  group_by(designation) %>% 
  summarize(median(WS))
  
  
draft2 %>% 
  mutate(draft_pick=if_else(Pk <= 5,'top five','other')) -> draft2

draft2 %>% 
  group_by(draft_pick,designation) %>% 
  summarize(n=n(),median=median(WS)) %>% 
  mutate(n=n/sum(n)) %>% 
  summarize(sum(n * median))
 
# HW 5: Due 6/12/2023

# 11.24 (a)-(f)
# 11.25 (a)

clus <- hclust(dist(x),method='complete')

# 11.30 (a) (b)
bg = par(bg = "darkseagreen1")
plot(as.dendrogram(clus, cex = 0.6, hang = -1),
     main = "Cluster Dendrogram: Win Shares by First-Round Selection",
     xlab = "First-Round Selection Number\n2000-2009 NBA Drafts",
     ylab = "Height (aka Euclidian Distance")
rect.hclust(clus, k = 2)


free_agent <- read_csv("free_agents.csv")


free_agent %>% 
  summary()

options(scipen=999)

ggplot(free_agent,aes(x=annual_salary)) + 
  geom_density(alpha=.3,fill='salmon') + 
  geom_vline(aes(xintercept=mean(annual_salary)),linetype='longdash',size=.8) + 
  geom_vline(aes(xintercept=median(annual_salary)),linetype='longdash',size=.8)  + 
  scale_x_continuous(labels=scales::comma)+
  theme(plot.title=element_text(face='bold')) + 
  annotate("text",x=18e6,y=.000000025,label='Mean',color='black',angle=90) + 
  annotate("text",x=13e6,y=.000000025,label='Median',color='black',angle=90)



free_agent %>% 
  arrange(position2) -> free_agent_sort


free_agent_sort %>% 
  select(player,age,position2,annual_salary,win_shares) -> free_agent_sort


free_agent_sort %>% 
  mutate(center=str_starts(position2,'C'),
         pf=str_starts(position2,'PF'),
         pg=str_starts(position2,'PG'),
         sf=str_starts(position2,'SF'),
         sg=str_starts(position2,'SG')) -> free_agent_sort
  

free_agent_sort  %>% 
  select(center:sg) %>% 
  mutate(across(everything(),as.numeric))  %>% 
  t() %>% 
  cbind(2) %>% 
  as_tibble()  %>% 
  uncount(V25) %>% 
  as.matrix() -> constraint_matrix


constraint_matrix <- rbind(constraint_matrix,rep(1,24),free_agent_sort$annual_salary,free_agent_sort$age)

dim(constraint_matrix)
  

dimnames(constraint_matrix) <-
  list(c("OneCenterMax",
         "OneCenterMin",
         "OnePowerForwardMax",
         "OnePowerForwardMin",
         "OnePointGuardMax",
         "OnePointGuardMin",
         "OneShootingForwardMax",
         "OneShootingForwardMin",
         "OneShootingGuardMax",
         "OneShootingGuardMin",
         "fivePlayerMax",
         "salaryMax",
         "ageMax"),
       free_agent_sort$position2)



constraint_matrix

install.packages('lpSolve')

lpSolve::lp(direction='max',objective.in = free_agent_sort$win_shares,const.mat=constraint_matrix,
            const.rhs=c(rep(1,10),5,90e6,150),
            const.dir = c(rep(c("<=",">="),times=5),'<=','<=','<='),int.vec = 1:24,all.bin = T)-> values

free_agent_sort[as.logical(values$solution),]

free_agent_sort
dim(constraint_matrix)
            

rep(c("<=",">="),times=5)                            
