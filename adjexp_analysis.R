library(tidyverse)
library(lme4)
library(xtable)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

############## read in turk data

# e1 = read.csv("mturk_data/adjs_turk_results_20221013.csv") %>%
#   mutate(batch=1) 
# 
# e = bind_rows(e1)
# 
# e$WorkerId = paste0("adjS", as.numeric(as.factor(e$WorkerId)))
# e$HITId = paste0("adjH", as.numeric(as.factor(e$HITId)))
# 
# d = select(e, HITId, batch, WorkerId, Input.list,
#            starts_with("Input")) %>%
#   select(HITId, WorkerId, batch, starts_with(("Input.id"))) %>%
#   gather(variable, value, -HITId, -WorkerId, -batch)
# 
# answers = e$Answer.taskAnswers %>%
#   map_df(function(z) {(fromJSON(z) %>% as.data.frame())}) %>%
#   bind_cols(select(e, HITId, batch))
# 
# answers$English = answers$English$yes
# answers$country = answers$country$USA
# answers[, 39] = answers[, 39]$yes
# 
# a = gather(answers, value, answer, -batch, -English, -country, -HITId) %>%
#   mutate(value=gsub("howMuch_", "", value))
# 
# d = left_join(d, a)
# write_csv(d, file="mturk_data/adjexp_turk.csv")

# read in anonymized public turk data
d = read_csv("mturk_data/adjexp_turk.csv")


fillers = filter(d, grepl("filler", value))
fillers$good = grepl("filler_1", fillers$value)
filler.mean = group_by(fillers, WorkerId, good) %>%
  summarise(m=mean(answer), n=n()) %>%
  spread(good, m) %>%
  mutate(diff=`TRUE` - `FALSE`) 

# filter participants with not a big enough diff 
# between good and bad fillers or who did it more than once
badp = filter(filler.mean, diff < 1 | n > max(n)) 

fillers = filter(fillers, WorkerId %in% badp$WorkerId == F)

bad = filter(fillers, value == "filler_0_3") 
hist(bad$answer)


###############################
d = filter(d, !grepl("filler", value))

d = filter(d, WorkerId %in% fillers$WorkerId)

d$value = gsub("record-s", "record s", d$value)
turk = separate(d, value, into=c("adj", "num", "noun", "sent", "adjtype", "template"), sep="-")
turk = mutate(turk, sent = case_when(sent == "sent"~ "a ADJ five days",
                                     sent == "sent_default"~"five ADJ days",
                                     sent == "sent_no_mod"~ "a five days",
                                     sent == "sent_reverse_mods"~ "a five ADJ days",
                                     sent == "sent_no_plural"~ "a ADJ five day",
                                     sent == "sent_no_a"~ "ADJ five days")) %>%
  rename(name=sent)
turk = filter(turk, English == T)

print(length(unique(turk$WorkerId)))

###########

# read in gpt3 data
g = bind_rows(read_csv("gpt3_data/sents_adjs_20221004.csv"),
          read_csv("gpt3_data/sents_adjs_20221004_2.csv")) 
g$template = as.character(g$template)
g$adj = gsub("record-s", "record s", g$adj)

# merge, keeping only sentences also in turk
g.turk = right_join(g, turk)


g.turk$answer = g.turk$answer/10
g.turk$value = g.turk$`goodness-sent`

g.turk = mutate(g.turk, adjclass = ifelse(adjclass %in% c("adj-neg", "adj-pos"), "qual.", adjclass))
d.3 = select(g.turk, adjclass, nounclass, name, answer, value) %>%
  rename(gpt3=value, 
         humans=answer) %>%
  gather(variable, value, -adjclass, -nounclass, -name)

d.3 = d.3 %>%
  group_by(adjclass, nounclass, name, variable) %>%
  summarise(m=mean(value),
            n=n(),
            s=sd(value),
            l=mean(value) - 1.96 * sd(value, na.rm=T)/sqrt(n),
            u=mean(value) + 1.96 * sd(value, na.rm=T)/sqrt(n),
  )


ggplot(d.3, aes(x=adjclass, y=m, fill=variable,
                group=variable, ymin=l, ymax=u)) +
  geom_bar(stat="identity", position=position_dodge(width=NULL)) +
  geom_errorbar(position=position_dodge(width=NULL)) + 
  theme_classic(10) +
  theme(legend.position = c(.8, .8),
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.title = element_blank()) + 
  xlab("") + ylab("mean probability of good") + 
  facet_grid(name ~ nounclass, drop=T, scales="free") 


d.3 = mutate(d.3, adjclass = gsub("adj-", "", adjclass),
             nounclass = gsub("noun-", "", nounclass))
d.3$adjclass = factor(d.3$adjclass, levels=c("quant", "ambig",
                                             "qual.", "human",
                                             "color", "stubborn"))

ggplot(filter(d.3, variable == "gpt3"), aes(x=adjclass, y=m,
                group=variable, ymin=l, ymax=u, fill=adjclass)) +
  geom_bar(stat="identity", position=position_dodge(width=NULL)) +
  geom_errorbar(position=position_dodge(width=NULL)) + 
  theme_classic(10) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.title = element_blank()) + 
  xlab("") + ylab("acceptability") + 
  facet_grid(. ~ nounclass, drop=T, scales="free") +
  scale_fill_manual(values=cbbPalette) + 
  geom_point(data=filter(d.3, variable == "humans"), aes(x=adjclass, y=m,
                                       group=variable),
             colour=cbbPalette[8],
             shape=17)
ggsave("pngs/adjs_turk.png", width=4, height=2.5)

####################
# run regressions
g.turk = mutate(g.turk, adjclass = ifelse(adjclass == "adj-color", "adj-stubborn", adjclass))
g.turk$adjclass = factor(g.turk$adjclass,
                         levels = c("adj-human",
                                    "adj-quant",
                                    "adj-stubborn",
                                    "adj-qual.",
                                    "adj-ambig"))

g.turk$nounclass = factor(g.turk$nounclass,
                         levels = c("noun-temporal",
                                    "noun-unit_like",
                                    "noun-objects",
                                    "noun-human",
                                    "noun-distance",
                                    "noun-art"))



l = lmer(value ~ adjclass * nounclass + (1|adj) + (1|noun) + 
           (1|num) + 
           (1 |template) , 
         data=g.turk)
summary(l)

t.stat <- function(x) fixef(x)/sqrt(diag(vcov(x)))

ls = data.frame(cbind(fixef(l), (t.stat(l)))) 
names(ls) = c("beta", "t-value")
ls$`p<.05` = ifelse(abs(ls$`t-value`) > 1.96, "*", "")

ls %>%
  xtable()

l.human = lmer(answer ~ adjclass * nounclass + (1|adj) + (1|noun) + 
           (1|num) + 
           (1 |template) +
             (adjclass + nounclass | WorkerId), 
         data=g.turk)
summary(l.human)


ls = data.frame(cbind(fixef(l.human), (t.stat(l.human)))) 
names(ls) = c("beta", "t-value")
ls$`p<.05` = ifelse(abs(ls$`t-value`) > 1.96, "*", "")

ls %>%
  xtable()

