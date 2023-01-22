library(tidyverse)
library(jsonlite)
library(viridis)
library(lme4)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# commented code is how we anonymize the original turk files.
# we make only anonymous files public.
# e1 = read.csv("mturk_data/main_exp_2022_10_06.csv") %>%
#    mutate(batch=1)
# 
# e2 = read.csv("mturk_data/main_exp_2022_10_11.csv") %>%
#    mutate(batch=2)
# 
# e = bind_rows(e1, e2)
# 
# e$WorkerId = paste0("mainS", as.numeric(as.factor(e$WorkerId)))
# e$HITId = paste0("mainH", as.numeric(as.factor(e$HITId)))
# d = select(e, HITId, batch, WorkerId, Input.list,
#           starts_with("Input")) %>%
#  select(HITId, WorkerId, batch, starts_with(("Input.id"))) %>%
#  gather(variable, value, -HITId, -WorkerId, -batch)
# 
# answers = e$Answer.taskAnswers %>%
#   map_df(function(z) {(fromJSON(z) %>% as.data.frame())}) %>%
#   bind_cols(select(e, HITId, batch))
# 
# answers$English = answers$English$yes
# answers$country = answers$country$USA
# answers[, 769] = answers[, 769]$yes
# 
# a = gather(answers, value, answer, -batch, -English, -country, -HITId) %>%
#   mutate(value=gsub("howMuch_", "", value))
# 
# d = left_join(d, a)

#write_csv(d, file="mturk_data/mainexp_turk.csv")

d = read_csv("mturk_data/mainexp_turk.csv")
fillers = filter(d, grepl("filler", value))
fillers$good = grepl("filler_1", fillers$value)
filler.mean = group_by(fillers, WorkerId, good) %>%
  summarise(m=mean(answer), n=n()) %>%
  spread(good, m) %>%
  mutate(diff=`TRUE` - `FALSE`) 

badp = filter(filler.mean, diff < 1 | n > 4)
fillers = filter(fillers, WorkerId %in% badp$WorkerId == F)
d = filter(d, !grepl("filler", value))
d = filter(d, WorkerId %in% fillers$WorkerId)

turk = separate(d, value, into=c("adj", "num", "noun", "sent", "temp"), sep="-")
turk = mutate(turk, sent = case_when(sent == "sent"~ "a ADJ five days",
                                     sent == "sent_default"~"five ADJ days",
                                     sent == "sent_no_mod"~ "a five days",
                                     sent == "sent_reverse_mods"~ "a five ADJ days",
                                     sent == "sent_no_plural"~ "a ADJ five day",
                                     sent == "sent_no_a"~ "ADJ five days")) %>%
  rename(name=sent)
              
turk = filter(turk, English == T)
length(unique(turk$WorkerId))  

####################3

g = bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
                 read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  filter(experiment %in% c("spentLondon"),
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
  mutate(temp=substr(sent, 1, 10)) %>%
  select(adjclass, adj, nounclass, noun, numclass, num, temp, starts_with("a ADJ"), starts_with("ADJ"),
         starts_with("a five"), starts_with("five")) %>%
  group_by(adjclass) %>%
  mutate(firstadj=first(adj)) %>%
  ungroup() %>%
  pivot_longer(cols=c(`a ADJ five days`,`a ADJ five day`, `a five days`,
                      `a five ADJ days`, `five ADJ days`, `ADJ five days`)) %>%
  na.omit() 

g.turk = right_join(g, turk)

cor(g.turk$value, g.turk$answer, method="spearman")

group_by(turk, adj, num, noun, temp, name) %>%
  summarise(n=n()) %>%
  filter(n > 1)


select(g.turk, adjclass, adj, noun, num, temp, name) %>%
  distinct() %>%
  nrow()

group_by(g.turk, adjclass, adj, noun, num, temp, name, value, answer) %>%
  summarise(n=n())

d.3 = select(g.turk, adjclass, name, firstadj, answer, value) %>%
  mutate(answer=answer/10) %>%
  rename(gpt3=value, 
         humans=answer) %>%
  gather(variable, value, -adjclass, -name, -firstadj)

#%>%
d.3 = d.3 %>%
  group_by(adjclass, name, firstadj, variable) %>%
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

d.3 = mutate(d.3, adjclass = case_when(adjclass == "adj" ~ "qual.",
                                       adjclass == "adj-ambig" ~ "qual./quant.",
                                       adjclass == "adj-quant" ~ "quant.")) 
ggplot(d.3, aes(x=adjclass, y=m, fill=variable,
                group=variable, ymin=l, ymax=u)) +
  geom_bar(stat="identity", position=position_dodge(width=NULL)) +
  geom_errorbar(position=position_dodge(width=NULL)) + 
  theme_classic(10) +
  theme(legend.position = c(.8, .8),
    axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
    legend.title = element_blank()) + 
  xlab("") + ylab("mean probability of good") + 
  facet_grid(. ~ name) +
  scale_fill_viridis(discrete=T)
ggsave("pngs/full-human-gpt.png", width=7, height=4)

d.3 = select(g.turk, adjclass, name, firstadj, answer, value) %>%
  mutate(answer=answer/10) %>%
  rename(gpt3=value, 
         humans=answer) %>%
  gather(variable, value, -adjclass, -name, -firstadj) %>%
  group_by(name, variable) %>%
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

d.3$good = ifelse(d.3$name %in% c("a ADJ five days", "five ADJ days"), 
                  "Reported Acceptable",
                  "Reported Unacceptable")
ggplot(filter(d.3, variable == "gpt3"), aes(x=name, y=m, fill=good,
                group=variable, ymin=l, ymax=u)) +
  geom_bar(stat="identity", position=position_dodge(width=NULL)) +
  geom_errorbar(position=position_dodge(width=NULL)) + 
  theme_classic(10) +
  theme(legend.position = c(.8, .9),
        legend.key.size = unit(.1, 'cm'),
    axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
    legend.title = element_blank()) + 
  xlab("") + ylab("acceptability") + 
  #scale_fill_viridis(discrete=T) +
  scale_fill_manual(values=cbbPalette) + 
  geom_point(data=filter(d.3, variable == "humans"), aes(x=name, y=m,
                                                         group=variable),
             colour=cbbPalette[3],
             shape=17,
             size=4)
ggsave("pngs/gpt3-humans.png", width=5.5, height=3)

library(xtable)
d.3.table = select(d.3, name, variable, m) %>%
  spread(variable, m) %>%
  mutate(name=paste("The family spent", gsub("ADJ", "beautiful", name), "in London.")) %>%
  rename(`Ex. Sentence` = name) 
print(xtable(d.3.table), include.rownames=FALSE)

#######
g.turk$name = factor(g.turk$name, levels = c("a ADJ five days",
                                       "five ADJ days",
                                       "a five ADJ days",
                                       "a five days",
                                       "a ADJ five day",
                                       "ADJ five days"))

l = lmer(value ~ name + (1|adjclass) + (1|adj) + 
           (1 | temp) , data=g.turk)
summary(l)

as.data.frame(fixef(l)) %>%
  xtable()

l.human = lmer(answer ~ name + (1|adjclass) + (1|adj) + 
           (1 | temp) + (1 |WorkerId), data=g.turk)
summary(l.human)

as.data.frame(fixef(l.human)) %>%
  xtable()

