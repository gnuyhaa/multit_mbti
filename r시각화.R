library(colorspace)
zodiac <- read.csv('zodiac_mbti.csv')
View(zodiac)


##################별자리데이터 mbti 분포###################
zo_mbtii <- zodiac %>%
  group_by(mbti) %>%
  summarise(counts=n()) %>%
  mutate(pct=counts/sum(counts)*100)
view(zo_mbtii)

ggplot(zo_mbtii, aes(reorder(mbti, -counts),counts, fill=mbti)) +
  geom_col() +
  theme_classic() +
  theme(legend.position = 'none', 
        plot.title = element_text(face='bold',hjust = 0.5, size=18),
        axis.title = element_text(face='bold', size=13))+
  labs(title='Reddit 별자리게시판 이용자들의 MBTI 분포', 
       x='MBTI 타입', y='글 수')+
  coord_cartesian(ylim=c(0,350)) 


########### 별자리별 mbti 빈도 히트맵 (leo, libra 포함) ###########
zo_mbti <- zodiac %>%
  group_by(type, mbti) %>%
  summarise(n=n()) %>%
  mutate(pct=n/sum(n)*100)
view(zo_mbti)

zod_heat <- zo_mbti %>% 
  ggplot(aes(type, mbti, fill=pct)) +
  theme_classic()+
  geom_tile() +
  geom_text(aes(label=paste0(round(pct),'%')))+
  scale_fill_gradient(low = "#F5F9D6", high = "#53AD89")+
  theme(plot.title = element_text(face='bold',hjust = 0.5, size=18),
        axis.title = element_text(face='bold', size=14))+
  labs(title='별자리별 MBTI 분포', 
       x='별자리', y='MBTI 유형', 
       fill='percent (%)')+
  scale_y_discrete(limits=c('estp','estj','entp','enfj','infp','entj','enfp',
                            'intj','intp','esfp','esfj','istp','isfp','isfj',
                            'infj','istj'))
zod_heat


########### 별자리별 mbti 빈도 히트맵 (leo, libra 제외) ###########
zo_mbti1 <- zo_mbti %>% filter(! type == 'Leo') %>% filter(! type == 'Libra')
View(zo_mbti1)  

zod_heat2 <- zo_mbti1 %>% 
  ggplot(aes(type, mbti, fill=pct)) +
  theme_classic()+
  geom_tile() +
  geom_text(aes(label=paste0(round(pct),'%')))+
  scale_fill_gradient(low = "#F5F9D6", high = "#53AD89")+
  theme(plot.title = element_text(face='bold',hjust = 0.5, size=18),
        axis.title = element_text(face='bold', size=14))+
  labs(title='별자리별 MBTI 분포', 
       x='별자리', y='MBTI 유형', 
       fill='percent (%)')+
  scale_y_discrete(limits=c('estp','estj','entp','enfj','infp','entj','enfp',
                            'intj','intp','esfp','esfj','istp','isfp','isfj',
                            'infj','istj'))
zod_heat2



#######################################
### 웹소설 장르별 MBTI 분포 히트맵 ###
#######################################
webnv <- nvl_data %>%
  group_by(genre, mbti) %>%
  summarise(n=n()) %>%
  mutate(pct=n/sum(n)*100)
view(webnv)

hea <- webnv %>% 
  ggplot(aes(genre, mbti, fill=pct)) +
  theme_classic()+
  geom_tile() +
  geom_text(aes(label=paste0(round(pct),'%')))+
  scale_fill_gradient(low = "#E1F5FE", high = "#03A9F4")+
  theme(plot.title = element_text(face='bold',hjust = 0.5, size=18),
        axis.title = element_text(face='bold', size=14))+
  labs(title='장르별 MBTI 분포 히트맵', 
       x='장르', y='MBTI 유형', 
       fill='percent (%)')
hea