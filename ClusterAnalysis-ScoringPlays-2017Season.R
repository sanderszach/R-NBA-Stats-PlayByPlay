### Updated 20170428
### Cluster analysis of 2016-2017 NBA Games
### Compares games based on the scoring margin over the course of the game
### Uses kmeans to cluster 

library(reshape)

###--- Filter for Scoring Plays only ---###
scoringPlays <- gameDetailFrame %>% 
  filter(!is.na(SCORE),    #only scoring plays
         as.numeric(PERIOD)<=4) %>%    #excludes overtime portion of games
  select(GAME_ID,SCORE,PCTIMESTRING,PERIOD) %>% 
  separate(SCORE,into = c('vst_score','hm_score')) %>% 
  separate(PCTIMESTRING, into = c('mins','secs')) %>% 
  mutate(real_time = as.numeric(mins)+as.numeric(secs)/60) %>% 
  mutate(total_time = (as.numeric(PERIOD)-1)*12+(12-real_time) ) %>% 
  mutate(vst_score=as.numeric(vst_score),hm_score=as.numeric(hm_score)) %>% 
  mutate(score_delta=hm_score-vst_score) %>% 
  select(GAME_ID,vst_score,hm_score,score_delta,total_time) %>% 
  unique()

###--- Run linear interpolation of the data ---###
scoringPlaysLinear <- lapply(unique(scoringPlays$GAME_ID),function(X){
  df <- filter(scoringPlays,GAME_ID==X)
  cbind.data.frame(
    total_time=seq(1,max(df$total_time),length=200), 
    sapply(df[,c("GAME_ID","vst_score","hm_score","score_delta")], function(T) approxfun(df$total_time, T)(seq(1,max(df$total_time),length=200)))
  )
})
scoringPlaysLinearFrame <- do.call(rbind.data.frame, scoringPlaysLinear)
scoringPlaysLinearFrame <- filter(scoringPlaysLinearFrame,!is.na(GAME_ID))
#save(scoringPlaysLinearFrame,file='scoringPlaysLinearFrame.Rda')

###--- Plot Some Games ---###
ggplot(scoringPlaysLinearFrame[1:(200*20),],aes(total_time,score_delta))+
  geom_point()+
  facet_wrap(~GAME_ID,ncol = 4)+
  geom_hline(yintercept = 0)+
  ylab('score margin (home score - away score)') +
  xlab('Time (mins)')


################################
###--- Run Kmeans on data ---###
################################

###--- Cast Matrix with score margin at every point in time of game ---###
mat <- as.data.frame(cast(scoringPlaysLinearFrame,GAME_ID~total_time,sum,value = 'score_delta'))
mat[,1] <- NULL

###--- Select Number of Clusters ---###
mydata <- mat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

###--- Run Kmeans ---###
km <- kmeans(as.data.frame(mat),6) #use k clusters from above plot

###--- Join Clusters Back to data frame ---###
km_results <- data.frame(GAME_ID=seq(1:length(km$cluster)),km$cluster)
scoringPlaysLinearFrame_cluster <- merge(scoringPlaysLinearFrame,km_results,by='GAME_ID',all.x = T)

###--- Graph Clusters ---###
game_count <- km_results %>% group_by(km.cluster) %>% summarise(cnt = paste0('n = ',length(unique(GAME_ID))))

#First 30 games 
ggplot(scoringPlaysLinearFrame_cluster[1:(200*30),],aes(total_time,score_delta,group=GAME_ID))+ #[1:10000,]
  geom_line()+
  facet_wrap(~km.cluster,ncol = 3)+
  geom_hline(yintercept = 0) +
  ylab('score margin (home score - away score)') +
  xlab('Time (mins)') +
  ggtitle('Scoring Margin Trends - Cluster Analysis')

#All games overlayed density plots
ggplot(scoringPlaysLinearFrame_cluster,aes(total_time,score_delta))+ #[1:10000,]
  geom_hex()+
  facet_wrap(~km.cluster,ncol = 3)+
  geom_hline(yintercept = 0) +
  geom_text(data=game_count, aes(x=5, y=35, label=cnt), 
            colour="black", inherit.aes=FALSE, parse=FALSE) +
  ylab('score margin (home score - away score)') +
  xlab('Time (mins)') +
  ggtitle('Scoring Margin Trends - Cluster Analysis')
