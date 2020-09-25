#import libraries 
library(rvest)
library(readr)
library(dplyr)
library(ggplot2)
library(BayesFactor)
library(pgirmess)
library(infer)

#import csv file 
exit0=read.csv("exit0.csv")
exit1=exit0[complete.cases(exit0),]
levels(exit1$player_name) 

#summary statistics 
avg_launch=ddply(exit1, .(player_name), summarize,  launch_speed=mean(launch_speed))
avg_launch[order(avg_launch$launch_speed,avg_launch$player_name),]
table(exit1$player_name)

#top launch_speed (mph) 
top_launch <- exit1 %>%
  select(launch_speed, player_name) %>% 
  transmute(launch_speed=launch_speed, player_name = player_name) %>%
  filter(player_name %in% c("Albert Pujols", "Shohei Ohtani", "Jefry Marte",
                            "Andrelton Simmons", "Justin Upton", "Kole Calhoun",
                            "Jonathan Lucroy","Tommy La Stella","Mike Trout",
                            "Luis Valbuena","Justin Bour","Peter Bourjos",
                            "Rene Rivera","Zack Cozart")) %>%
  mutate_if(is.character, as.factor) %>%
  droplevels

top_launch$player_name <- forcats::fct_reorder(top_launch$player_name, top_launch$launch_speed)

top_launch %>%
  group_by(player_name) %>%
  summarise(mean_launch_spd=mean(launch_speed)) %>%
  arrange(mean_launch_spd)

top_launch %>%
  group_by(player_name) %>%
  summarise(median_launch_spd=median(launch_speed)) %>%
  arrange(median_launch_spd)

#parametric mean testing unequal variances
t.test(top_launch$launch_speed)

# MANOVA test (differences in launch speed between players?)
exit_diff<- aov(launch_speed~player_name, data = top_launch)
tuk=TukeyHSD(exit_diff)
plot(tuk)

#dot plot launch speed 
ggplot(top_launch, aes(x=player_name, y=launch_speed)) + 
  geom_violin(trim = FALSE)+
  geom_dotplot(binaxis='y', stackdir='center')

#non-parametric rank testing 
kruskal.test(launch_speed~player_name,data=top_launch) #p<0.0008

#bayesian testing (differences between means) 
levels(top_launch$player_name)
summary(aov(launch_speed~player_name,data=top_launch)) #anova 
bf=anovaBF(launch_speed~player_name,data=top_launch)
plot(bf) 

#quality check player launch speed metrics 
top_launch1=exit1 %>%
  mutate(launch_spd=exit1$launch_speed)
top_launch1

n_distinct(top_launch1$player_name) #43

cnt=top_launch1 %>%
  group_by(player_name) %>%
  summarise(n_rec = n()) %>%
  arrange(desc(n_rec))
cnt 

cnt1=as.data.frame(cnt)
dirty=merge(cnt1,avg_launch,by="player_name")
dirty=subset(dirty,n_rec>=200)
write.csv(dirty,file="rumm.csv")

#distribution of At bats by player
ggplot(cnt, aes(n_rec)) +
  geom_histogram(binwidth = 50, fill = "lightblue", colour = "black") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Distribution of number of at bats by Angels players") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 0.5)
  )

cnt1=top_launch %>%
  inner_join(
    select(filter(cnt, n_rec >= 200),player_name ),
    by = c("player_name" = "player_name")
  )
cnt1 

ggplot(cnt1, aes(launch_speed)) +
  geom_density() +
  facet_wrap(~player_name, nrow = 4, ncol = 4) +
  xlab("Launch Speed (mph)") +
  ylab(NULL) +
  ggtitle("Distribution of launch speed by Angels players") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 0.5)
  )

med_exit<- cnt1 %>%
  group_by(player_name) %>%
  summarise(med_launch = median(launch_speed)) %>%
  arrange(med_launch)
med_exit 

cnt2<-cnt1 %>%
  filter(player_name %in% c("Albert Pujols","Shohei Ohtani",
                            "Andrelton Simmons","Tommy La Stella",
                            "Kole Calhoun","Justin Upton",
                            "Jefry Marte","Mike Trout","Luis Valbuena",
                            "Zack Cozart","Ian Kinsler",
                            "David Fletcher","Martin Maldonado"))


#kruskall-wallis test 
cnt2$player_name=as.factor(cnt2$player_name)
kt=kruskal.test(launch_speed~player_name,data=cnt2)
kt #p>0.05 (no hitters have sig diff launch speed?)

ktph=kruskalmc(cnt2$launch_speed,cnt2$player_name)
ktph$dif.com 

#bayes factor 
set.seed(323)
bf=anovaBF(launch_speed~player_name,data=cnt2) #0.029 bayes factor
bf 

exp(0.029) #1.03 negligible evidence  

compare_hitters_bf <- function(df, hitter1, hitter2) {
  
  ds <- df %>%
    filter(player_name %in% c(hitter1, hitter2)) %>%
    droplevels %>% 
    as.data.frame
  zzz <- ttestBF(formula = launch_speed ~ player_name, data = ds)
  yyy <- extractBF(zzz)
  xxx <- paste0("The evidence provided by the data corresponds to odds of ", 
                round(yyy$bf,0), 
                ":1 that ", 
                hitter1, 
                " hitters harder than ",
                hitter2 )
  return(xxx)
}

compare_hitters_bf(cnt2,"Albert Pujols","Tommy La Stella")
compare_hitters_bf(cnt2,"Mike Trout","Albert Pujols")
compare_hitters_bf(cnt2,"Mike Trout","Zack Cozart")
compare_hitters_bf(cnt2,"Andrelton Simmons","Zack Cozart")
compare_hitters_bf(cnt2,"Shohei Ohtani","Zack Cozart")

#frequentist test 
justtwo <- cnt2 %>%
  filter(player_name %in% c("Albert Pujols", "Zach Cozart")) %>%
  droplevels %>% 
  as.data.frame
t.test(formula = launch_speed ~ player_name, data = justtwo)

#bayesian version t-test
justtwo1 <- cnt2 %>%
  filter(player_name %in% c("Albert Pujols", "Zach Cozart")) %>%
  droplevels %>% 
  as.data.frame
ttestBF(formula = launch_speed ~ player_name, data = justtwo,
        rescale="wide") #medium bf=1940, 

#directional hypothesis 
justtwo2 <- cnt2 %>%
  filter(player_name %in% c("Albert Pujols", "Zack Cozart")) %>%
  droplevels %>% 
  as.data.frame
# notice these two just return the same answer in a different order
ttestBF(formula = launch_speed~player_name, data = justtwo2, nullInterval = c(0, Inf))

#histogram overlap 
king <- cnt2 %>%
  filter(player_name %in% c("Albert Pujols", "Zack Cozart"))

ggplot(king, aes(launch_speed, fill = player_name)) + 
  geom_histogram(bins=45,alpha = 0.5, aes(y = ..density..), position = 'identity')+
  xlab("Launch Speed")+ylab("density")
