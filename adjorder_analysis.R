library(tidyverse)
library(jsonlite)
cbbPalette <- c("gray", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# e = read.csv("mturk_data/adjorder_2022_10_06.csv")
# e$WorkerId = paste0("ordS", as.numeric(as.factor(e$WorkerId)))
# e$HITId = paste0("ordH", as.numeric(as.factor(e$HITId)))
# 
# d = select(e, HITId, WorkerId, Input.list,
#            starts_with("Input")) %>%
#   select(HITId, WorkerId, starts_with(("Input.id"))) %>%
#   gather(variable, value, -HITId, -WorkerId)
# 
# answers = e$Answer.taskAnswers %>%
#   map_df(function(z) {(fromJSON(z) %>% as.data.frame())}) %>%
#   bind_cols(select(e, HITId))
# 
# answers$English = answers$English$yes
# answers$country = answers$country$USA
# answers$onetime = answers$onetime$yes
# 
# a = gather(answers, value, answer, -onetime,  -English, -country, -HITId) %>%
#   mutate(value=gsub("howMuch_", "", value))
# 
# d = left_join(d, a)
# 
# write_csv(d, file="mturk_data/adjorder_turk.csv")

d = read_csv("mturk_data/adjorder_turk.csv")

fillers = filter(d, grepl("filler", value))
fillers$good = grepl("filler_1", fillers$value)
filler.mean = group_by(fillers, WorkerId, good) %>%
  summarise(m=mean(answer), n=n()) %>%
  spread(good, m) %>%
  mutate(diff=`TRUE` - `FALSE`)

badp = filter(filler.mean, diff < 1 | n > max(n)) 

fillers = filter(fillers, WorkerId %in% badp$WorkerId == F)

d = filter(d, !grepl("filler", value))
d = filter(d, WorkerId %in% fillers$WorkerId)

turk = separate(d, value, into=c("adj", "num", "noun", "cond", "twoadjs", "template"), sep="-")
turk = mutate(turk, sent = case_when(cond == "sentadjorder1"~ "a beautiful mere five days",
                                     cond == "sentadjorder2"~"a mere beautiful five days")) %>%
              rename(name=sent)
              
turk = filter(turk, English == T)

length(unique(turk$WorkerId))
####################3

g = bind_rows( read_csv("gpt3_data/sents_20221004.csv"),
                 read_csv("gpt3_data/sents_20221004_2.csv")) %>%
  rename( `a beautiful mere five days`=(`goodness-sentadjorder1`),
          `a mere beautiful five days`=(`goodness-sentadjorder2`)) %>%
  select(adjclass, adj, nounclass, noun, numclass, num, template, `a beautiful mere five days`,
         `a mere beautiful five days`) %>%
  pivot_longer(cols=c(`a mere beautiful five days`, `a beautiful mere five days`)) %>%
  na.omit() 

g$template = as.character(g$template)
g.turk = right_join(g, turk)

cor(g.turk$value, g.turk$answer, method="spearman")

d.3 = select(g.turk, adjclass, name, answer, value) %>%
  mutate(answer=answer/10) %>%
  rename(gpt3=value, 
         humans=answer) %>%
  gather(variable, value, -adjclass, -name)

#%>%
d.3.sum = d.3 %>%
  group_by(adjclass, name, variable) %>%
  summarise(m=mean(value),
            n=n(),
            s=sd(value),
            l=mean(value) - 1.96 * sd(value, na.rm=T)/sqrt(n),
            u=mean(value) + 1.96 * sd(value, na.rm=T)/sqrt(n),
  )


ggplot(d.3.sum, aes(x=adjclass, y=m, fill=variable,
                group=variable, ymin=l, ymax=u)) +
  geom_bar(stat="identity", position=position_dodge(width=NULL)) +
  geom_errorbar(position=position_dodge(width=NULL)) + 
  theme_classic(20) +
  theme(#legend.position = "bottom",
    axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
    legend.title = element_blank()) + 
  xlab("") + ylab("mean probability of good") + 
  facet_grid(. ~ name) +
  scale_fill_manual(values=cbbPalette)
######################

d.3.2 = d.3 %>%
  group_by(name, variable) %>%
  summarise(m=mean(value),
            n=n(),
            s=sd(value),
            l=mean(value) - 1.96 * sd(value, na.rm=T)/sqrt(n),
            u=mean(value) + 1.96 * sd(value, na.rm=T)/sqrt(n),
  )


d.3.2$variable = ifelse(d.3.2$variable == "gpt3", "GPT-3", d.3.2$variable)
ggplot(d.3.2, aes(x=variable, y=m, fill=name,
                group=name, ymin=l, ymax=u)) +
  geom_bar(stat="identity", position=position_dodge(width=NULL)) +
  geom_errorbar(position=position_dodge(width=NULL)) + 
  theme_classic(10) +
  theme(legend.position = c(.75, .9),
    legend.title = element_blank()) + 
  xlab("") + ylab("acceptability") + 
  scale_fill_manual(values=cbbPalette) +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) 
ggsave("pngs/adjorder.png", width=4, height=2)


nrow(g.turk)

group_by(g.turk, adj, num, noun, template, adjclass, name) %>%
  summarize(n=n()) %>%
  arrange(n)

group_by(g.turk, adj, num, noun, template, adjclass, name) %>%
  summarize(n=n()) %>%
  arrange(-n)


l = lmer(value ~ name + (1|adj)  +
           (1|num) + 
           (1 |template) , 
         data=g)
summary(l)

t.stat <- function(x) fixef(x)/sqrt(diag(vcov(x)))

ls = data.frame(cbind(fixef(l), (t.stat(l)))) 
names(ls) = c("beta", "t-value")
ls$`p<.05` = ifelse(abs(ls$`t-value`) > 1.96, "*", "")


l.human = lmer(answer ~ name + (1|adj)  +
           (1|num) + 
           (1 |template) +
            (name | WorkerId), 
         data=g.turk)
summary(l.human)

t.stat <- function(x) fixef(x)/sqrt(diag(vcov(x)))

ls = data.frame(cbind(fixef(l.human), (t.stat(l.human)))) 
names(ls) = c("beta", "t-value")
ls$`p<.05` = ifelse(abs(ls$`t-value`) > 1.96, "*", "")


