library(tidyverse)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


d = bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
  read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  filter(experiment %in% c("spentLondon", "storeBought")) %>%
  select(experiment, adj, num, noun, adjclass, numclass, nounclass, starts_with("goodness")) %>%
  gather(variable, value, -experiment, -adj, -num, -noun, -adjclass, -numclass, -nounclass)

# this shows the predicted adjective effects very nicely
adjs = filter(d, numclass == "num-low") %>%
  group_by(variable, experiment, adjclass) %>%
  filter(variable == "goodness-sent") %>%
  summarise(m=mean(value, na.rm=T),
            n=n(),
            l = mean(value, na.rm=T) - 1.96 * sd(value, na.rm=T)/sqrt(n()),
            u = mean(value, na.rm=T) + 1.96 * sd(value, na.rm=T)/sqrt(n()))
adjs$experiment = ifelse(adjs$experiment == 'spentLondon', 
                         "temporal noun",
                         "common noun")
adjs$adjclass = gsub("adj-", "", adjs$adjclass)
ggplot(adjs, aes(x=adjclass, y=m,
                 ymin=l, ymax=u)) + 
  geom_bar(stat='identity', position=position_dodge(width=1), fill="gray") + 
  geom_errorbar(position=position_dodge(width=1), width=.5) + 
  theme_classic(20) +
  theme(#legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle=90, hjust=1)) + 
  facet_grid(. ~ experiment, scales="free_x", drop=T) + 
  scale_fill_manual(values = c("gray", "gray",
                               "blue", "lightblue",
                               "darkblue")) +
  xlab("") + ylab("mean probability of good")
ggsave("pngs/adjs.png", width=6, height=4)

# high-odd are weirdly high, the rest as expected
group_by(d, variable, numclass) %>%
  summarise(m=mean(value, na.rm=T)) %>%
  spread(numclass, m)

# measure best. The common nouns and common measure nouns are no good.
group_by(d, variable, nounclass) %>%
  summarise(m=mean(value, na.rm=T)) %>%
  spread(nounclass, m)

# pick a really good one and test the overall pattern
# split by adjective
filter(d, numclass == "num-low",
       nounclass == "noun-measure") %>%
  group_by(variable, adjclass) %>%
  summarise(m=mean(value)) %>%
  spread(adjclass, m)

filter(d, numclass == "num-low",
       nounclass == "noun-common") %>%
  group_by(variable, adjclass) %>%
  summarise(m=mean(value)) %>%
  spread(adjclass, m)


group_by(d, experiment) %>%
  summarise(m=mean(value, na.rm=T)) 

d.2 = bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
               read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  filter(experiment %in% c("spentLondon", "storeBought"),
         numclass == "num-low",
         nounclass == "noun-measure") %>%
  group_by(adjclass) %>%
  summarise(a.helps = mean(`goodness-sent` > `goodness-sent_no_a`),
            mod.helps = mean(`goodness-sent` > `goodness-sent_no_mod`),
            order.helps = mean(`goodness-sent` > `goodness-sent_reverse_mods`),
            singular.helps = mean(`goodness-sent` > `goodness-sent_no_plural`))

d.2.common = bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
                 read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  filter(experiment %in% c("spentLondon", "storeBought"),
         numclass == "num-low",
         nounclass == "noun-common") %>%
  group_by(adjclass) %>%
  summarise(a.helps = mean(`goodness-sent` > `goodness-sent_no_a`),
            mod.helps = mean(`goodness-sent` > `goodness-sent_no_mod`),
            order.helps = mean(`goodness-sent` > `goodness-sent_reverse_mods`),
            singular.helps = mean(`goodness-sent` > `goodness-sent_no_plural`))
d.2.common


d.3 = bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
                 read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  filter(experiment %in% c("spentLondon", "storeBought"),
         numclass == "num-low",
         nounclass == "noun-measure") %>%
  mutate(adjclass = ifelse(adjclass == "adj-neg" | adjclass == "adj-pos",
                           "adj",
                           adjclass)) %>%
  rename( `a ADJ five days`=(`goodness-sent`),
            `five ADJ days`=(`goodness-sent_default`),
            `a five days`=(`goodness-sent_no_mod`),
            `a five ADJ days`=(`goodness-sent_reverse_mods`),
            `a ADJ five day` = (`goodness-sent_no_plural`),
           `ADJ five days` = (`goodness-sent_no_a`)) %>%
  select(adjclass, adj, starts_with("a ADJ"), starts_with("ADJ"),
         starts_with("a five"), starts_with("five")) %>%
  group_by(adjclass) %>%
  mutate(firstadj=first(adj)) %>%
  ungroup() %>%
  pivot_longer(cols=c(`a ADJ five days`,`a ADJ five day`, `a five days`,
               `a five ADJ days`, `five ADJ days`, `ADJ five days`)) %>%
  na.omit() %>%
  group_by(adjclass, name, firstadj) %>%
  summarise(m=mean(value),
            n=n(),
            s=sd(value),
            l=mean(value) - 1.96 * sd(value, na.rm=T)/sqrt(n),
            u=mean(value) + 1.96 * sd(value, na.rm=T)/sqrt(n),
           )

d.3$name = factor(d.3$name, levels = c("a ADJ five days",
                                       "five ADJ days",
                                       "a five ADJ days",
                                       "a five days",
                                       "a ADJ five day",
                                       "ADJ five days"))
  

sum(d.3$n)

################################
# breakout hungry
hungry = bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
           read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  filter(experiment == "servedDinnerTo") %>%
  select(sent, `goodness-sent`, adjclass, numclass, nounclass, adj, num, noun) %>%
  mutate(agents = ifelse(grepl("arrived", sent), "agents", "not_agents"))

group_by(hungry, agents, adjclass, adj) %>%
  summarise(m=mean(`goodness-sent`)) %>%
  spread(agents, m)

# shows the right pattern, but it says it's good overall. Even the one we think is bad

##################################

filter(d, experiment == "twoAdjs",
       variable %in% c("goodness-sentadjorder1", "goodness-sentadjorder2")) %>%
  group_by(variable) %>%
  summarise(m=mean(value))

############################### 

# doesn't get this one
doublea = bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
               read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  filter(experiment == "twoAdjs") %>%
  rename(goodness1 = `goodness-sentadjorder1`,
         goodness2 = `goodness-sentadjorder2`) %>%
  select( experiment, adj, num, noun, adjclass, numclass, nounclass,
          sentadjorder1, sentadjorder2,
    goodness1, goodness2) %>%
  mutate(expected.dir = goodness1 - goodness2)

select(doublea, `sentadjorder1`, `sentadjorder2`, expected.dir)
mean(doublea$expected.dir > 0)
group_by(doublea, noun) %>%
  summarise(mean(expected.dir))
group_by(doublea, adj) %>%
  summarise(mean(expected.dir))
group_by(doublea, num) %>%
  summarise(mean(expected.dir))

group_by(doublea) %>%
  summarise(mean(`goodness1`), mean(`goodness2`))


##################################

agree = read_csv("gpt3_data/sents_adjs_agree.csv")

agree$nounclass = factor(agree$nounclass,
                          levels = c("noun-temporal",
                                     "noun-unit_like",
                                     "noun-objects",
                                     "noun-human",
                                     "noun-distance",
                                     "noun-art"))

l = lmer(data=agree,
         `goodness-sent` ~ singplur * 
           nounclass +
           (1 + singplur|noun) + 
           (1|adj) + 
           (1|template))
summary(l)

t.stat <- function(x) fixef(x)/sqrt(diag(vcov(x)))

ls = data.frame(cbind(fixef(l), (t.stat(l)))) 
names(ls) = c("beta", "t-value")
ls$`p<.05` = ifelse(abs(ls$`t-value`) > 1.96, "*", "")

ls %>%
  xtable()


singplur = group_by(agree, nounclass, singplur) %>%
  summarise(m=mean(`goodness-sent`),
            l=m - 1.96 * sd(`goodness-sent`)/sqrt(n()),
            u=m + 1.96 * sd(`goodness-sent`)/sqrt(n())) %>%
  mutate(nounclass = gsub("noun-", "", nounclass))

spread(singplur, singplur, m) %>% 
  mutate(prefer.singular = singular - plural) %>%
  arrange(-prefer.singular) 

ggplot(singplur, aes(x=nounclass, y=m, fill=singplur,
                     ymin=l, ymax=u)) +
  geom_bar(stat="identity", position=position_dodge(width=NULL)) +
  scale_y_continuous(breaks = c(0, 1), limits=c(0, 1.2)) +
  geom_bar(stat='identity', position=position_dodge(width=1)) + 
  geom_errorbar(position=position_dodge(width=1), width=.5) + 
  theme_classic(7) +
  theme(#legend.position = "bottom",
    #theme(legend.position = c(0.87, 0.25))
    legend.position=c(.15, .95), legend.title=element_blank(),
    legend.direction = "horizontal") + 
  #axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_fill_manual(values=cbbPalette) + 
  ylab("acceptability") + xlab("noun class") 

ggsave("pngs/agreement.png", width=3, height=1.7)




# all the right patterns, but some strong preferences
agree = bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
                     read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  filter(experiment == "agreement" | experiment == "agreementHuman") %>%
  select(experiment, adj, num, noun, adjclass, numclass, nounclass,
         sent, sent_default, 
         `goodness-sent`, `goodness-sent_default`)
agree$singular = ifelse(grepl("wants", agree$sent) | grepl("seems", agree$sent), "sing", "plural")

singplur = group_by(agree, noun, singular) %>%
  summarise(m=mean(`goodness-sent`)) 
singplur

spread(singplur, singular, m) %>% 
  mutate(prefer.singular = sing - plural) %>%
  arrange(-prefer.singular)

singplur_default = group_by(agree, noun, singular) %>%
  summarise(m=mean(`goodness-sent_default`))
singplur_default

spread(singplur_default, singular, m) %>% 
  mutate(prefer.singular = sing - plural) %>%
  arrange(-prefer.singular)


############# this generates the huamn sentences to run on mturk for main exp

human.sents = d %>%
  bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
             read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  filter(experiment %in% c("spentLondon", "storeBought"),
       numclass == "num-low",
       nounclass == "noun-measure") %>%
  select(sent, adj, num, noun, sent_default, sent_no_mod, sent_no_a,
         sent_reverse_mods, sent_no_plural) %>%
  na.omit() %>%
  gather(variable, value, -adj, -num, -noun) %>%
  mutate(temp = substr(value, 1, 10))

write_csv(human.sents, "humansents.csv")

spread(human.sents, variable, value)

human.sents2 = bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
               read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  filter(experiment %in% c("spentLondon", "storeBought"),
         numclass == "num-low") %>%
  select(sent, adj, num, noun) %>%
  na.omit() %>%
  gather(variable, value, -adj, -num, -noun)


humans = bind_rows(human.sents, human.sents2) %>%
  unique()

mturk = filter(human.sents, num == "three", noun %in% c("days", "weeks", "months")) %>%
  group_by(temp) %>%
  mutate(list=sample(1:n())) %>%
  ungroup() %>%
  mutate(id = paste(adj,num,noun,variable,temp, sep="-")) %>%
  select(id, value, list)



fillers = tibble(value= c("Flosa has often seen Marn.",
                                       "Chardon sees often Kuru.",
                                       "Bob walk.",
                                       "Malevolent floral candy is delicious.",
                                       "The bone chewed the dog.",
                                       "The bone dog the chewed.",
                                       "I wonder you ate how much.",
                                       "The fragrant orangutan sings loudest at Easter."),
                 good=c(1, 0, 0, 1, 1, 0, 0, 1),
                 s=1:length(c(1, 0, 0, 1, 1, 0, 0, 1)),
                 id=paste("filler", good, s, sep="_"))
                 
fillers_ex = expand_grid(fillers, list=1:max(mturk$list)) %>%
  select(id, value, list)

m = bind_rows(fillers_ex, mturk)

m = group_by(m, list) %>%
  mutate(randorder = sample(1:n()))

out_turk = pivot_wider(m, id_cols=list, values_from=c(value, id), names_from=randorder) 
out_turk = out_turk[,order(colnames(out_turk))] 
out_turk = ungroup(out_turk)

a = bind_cols(select(out_turk, list), select(out_turk, -list))
write_csv(a, file="mturk_generation/runturk_main.csv")


########################## get adj ones
adjs = bind_rows(read_csv("gpt3_data/sents_adjs_20221004.csv"),
                 read_csv("gpt3_data/sents_adjs_20221004_2.csv")) %>%
  rename(value=`goodness-sent`) 
# get human sents
human.sents = adjs %>%
  select(sent, adj, num, noun, template, experiment) %>%
  na.omit() %>%
  gather(variable, value, -adj, -num, -noun, -template, -experiment) 

mturk = filter(human.sents) %>%
  group_by(template, experiment) %>%
  mutate(list=sample(1:n())) %>%
  ungroup() %>%
  mutate(id = paste(adj,num,noun,variable,experiment, template, sep="-")) %>%
  select(id, value, list)


FILLERS2 = c("Flosa has often seen Marn.",
  "Chardon sees often Kuru.",
  "Bob walk.",
  "Malevolent floral candy is delicious.",
  "The bone chewed the dog.",
  "The bone dog the chewed.",
  "I wonder you ate how much.",
  "The fragrant orangutan sings loudest at Easter.",
  
  "The boy chased the ball.",
  "There seems to be three policitians in the park.",
  "Taiwanese bankers played in the snow.",
  "She arrived last night.",
  "Here comes the sun.",
  
  "There seem to be three politicians in the park.",
  "We bought chair at the store.",
  "Here come a lovely chair.",
  "Book the book read I.",
  "Is the woman who sings is happy?"
  
  )
FILLERS2_RATINGS = c(1, 0, 0, 1, 1, 0, 0, 1,
                     1, 1, 1, 1, 1,
                     0, 0, 0, 0, 0)
fillers = tibble(value= FILLERS2,
                 good=FILLERS2_RATINGS,
                 s=1:length(FILLERS2),
                 id=paste("filler", good, s, sep="_"))

fillers_ex = expand_grid(fillers, list=1:max(mturk$list)) %>%
  select(id, value, list)

m = bind_rows(fillers_ex, mturk)

m = group_by(m, list) %>%
  mutate(randorder = sample(1:n()))

out_turk = pivot_wider(m, id_cols=list, values_from=c(value, id), names_from=randorder) 
out_turk = out_turk[,order(colnames(out_turk))] 
out_turk = ungroup(out_turk)

a = bind_cols(select(out_turk, list), select(out_turk, -list))
write_csv(a[1:300, ], file="runturk_adjs.csv")
x = tibble(x=as.matrix(a[1, ]))

################################################
# turk for adj order
doublea = bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
                     read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  filter(experiment == "twoAdjs") %>%
  rename(goodness1 = `goodness-sentadjorder1`,
         goodness2 = `goodness-sentadjorder2`)
  
# get human sents
human.sents = doublea %>%
  select(sentadjorder1, sentadjorder2, adj, num, noun, template, experiment) %>%
  na.omit() %>%
  gather(variable, value, -adj, -num, -noun, -template, -experiment) 

mturk = filter(human.sents) %>%
  group_by(template, experiment) %>%
  mutate(list=sample(1:n())) %>%
  ungroup() %>%
  mutate(id = paste(adj,num,noun,variable,experiment, template, sep="-")) %>%
  select(id, value, list)


FILLERS3 = c("Flosa has often seen Marn.",
             "Chardon sees often Kuru.",
             "Bob walk.",
             "Malevolent floral candy is delicious.",
             "The bone chewed the dog.",
             "The bone dog the chewed.",
             "I wonder you ate how much.",
             "The fragrant orangutan sings loudest at Easter."
             )
FILLERS3_RATINGS = c(1, 0, 0, 1, 1, 0, 0, 1)
                     
fillers = tibble(value= FILLERS3,
                 good=FILLERS3_RATINGS,
                 s=1:length(FILLERS3),
                 id=paste("filler", good, s, sep="_"))

fillers_ex = expand_grid(fillers, list=1:max(mturk$list)) %>%
  select(id, value, list)

m = bind_rows(fillers_ex, mturk)

m = group_by(m, list) %>%
  mutate(randorder = sample(1:n()))

out_turk = pivot_wider(m, id_cols=list, values_from=c(value, id), names_from=randorder) 
out_turk = out_turk[,order(colnames(out_turk))] 
out_turk = ungroup(out_turk)

a = bind_cols(select(out_turk, list), select(out_turk, -list))
write_csv(a, file="twoadjs_turk.csv")
x = tibble(x=as.matrix(a[1, ]))

################ analyze numerals
dnum = filter(d, variable == "goodness-sent") %>%
  group_by(numclass) %>%
  summarize(example=first(num),
    m=mean(value, na.rm=T)) 

print(xtable(dnum), include.rownames=FALSE)

