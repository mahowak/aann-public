# -*- coding: utf-8 -*-

from collections import defaultdict
from unicodedata import name
from cola import get_judgment, getcolaprompt

import argparse
import itertools
import pandas as pd
import re


def uppercasefirst(s):
  """"""
  return re.sub('([a-zA-Z])', lambda x: x.groups()[0].upper(), s, 1)

def a_an(nextword):
  """Return a or an depending on the next word."""
  if nextword[0] in "aeiou":
    return "an"
  return "a"

def get_sents_dict():
  """Get a dictionary of sentences to rate."""
  sents = []
  for p in list(PREFIXDICT.keys()):
    for tempnum, template in enumerate(PREFIXDICT[p]):
      working_d = aann.loc[aann[p] == 1]
      d = defaultdict(list)
      for i in range(working_d.shape[0]):
        x = working_d.iloc[i, :]
        d[x["cat"]] += [(x["word"], x["cat"] + "-" + x["info"])]
      d["quant"] = [(None, None)]
      if p in PREFIXDICTTWOADJ:
        d["quant"] = [i for i in d["adj"] if "quant" in i[1]]
        d["adj"] = [i for i in d["adj"] if "quant" not in i[1]]
      for adj, num, noun, quant in itertools.product(*[d["adj"], d["num"], d["noun"], d["quant"]]):    
          newd = {            
                  "experiment": p,
                  "template": str(tempnum),
                  "adj": adj[0],
                  "num": num[0],
                  "noun": noun[0],
                  "adjclass": adj[1],
                  "numclass": num[1],
                  "nounclass": noun[1]}
          if p not in PREFIXDICTTWOADJ:
            newd["sent"] = uppercasefirst(
                f"{template[0]} {a_an(adj[0])} {adj[0]} {num[0]} {noun[0]}{template[1]}.".lstrip(" "))
            if len(template) > 2:
              newd["singplur"] = template[2]

            if p not in ONLYEXP:
                newd["sent_default"] = uppercasefirst(
                f"{template[0]} {num[0]} {adj[0]} {noun[0]}{template[1]}.".lstrip(" "))
          else:
            newd["sentadjorder1"] = uppercasefirst(f"{template[0]} {a_an(adj[0])} {adj[0]} {quant[0]} {num[0]} {noun[0]}{template[1]}.".lstrip(" "))
            newd["sentadjorder2"] = uppercasefirst(f"{template[0]} {a_an(quant[0])} {quant[0]} {adj[0]} {num[0]} {noun[0]}{template[1]}.".lstrip(" "))

          if p in DOALL:
            newd["sent_no_plural"] = uppercasefirst(
                f"{template[0]} {a_an(adj[0])} {adj[0]} {num[0]} {noun[0][:-1]}{template[1]}.".lstrip(" "))
            newd["sent_no_mod"] = uppercasefirst(
                f"{template[0]} {a_an(num[0])} {num[0]} {noun[0]}{template[1]}.".lstrip(" "))
            newd["sent_default_nomod"] = uppercasefirst(
                f"{template[0]} {num[0]} {noun[0]}{template[1]}.".lstrip(" "))
            newd["sent_no_a"] = uppercasefirst(
                f"{template[0]} {adj[0]} {num[0]} {noun[0]}{template[1]}.".lstrip(" "))
            newd["sent_reverse_mods"] = uppercasefirst(
                f"{template[0]} {a_an(num[0])} {num[0]} {adj[0]} {noun[0]}{template[1]}.".lstrip(" "))
          sents += [newd]
  return sents

def get_ratings(sents, outfile, startnum):
  """Get ratings from GPT-3,
  write a dataframe with the ratings."""
  print("getting ratings for: " +  str(len(sents)))
  ratedsents = []
  sentstorate = []
  for sentnum, sent in enumerate(sents):
    if sentnum < startnum:
      continue
    newsent = sent
    for i in list(sent.keys()):
      if i.startswith("sent"):
        sentstorate += [sent[i]]
        rating =get_judgment(getcolaprompt(sent[i]))
        newsent["rating-" + i] = rating[0]
        if "good" in rating[0]:
          newsent["goodness-" + i] = rating[1]
        else:
          newsent["goodness-" + i] = 1 - rating[1]
    ratedsents += [newsent]
    if sentnum % 100 == 0:
      print(sentnum)
      pd.DataFrame(ratedsents).to_csv(outfile)

  pd.DataFrame(ratedsents).to_csv(outfile)


if __name__ == "__main__":

  parser = argparse.ArgumentParser() 
  parser.add_argument("--outfile", type=str)
  parser.add_argument("--numsents", type=int, default=-1)
  parser.add_argument("--start", type=int, default=0, help="start at this sentence number")
  parser.add_argument("--adj", type=str, default="adj", adj="run only the adjective experiment")
  args = parser.parse_args()

  if args.adj == "adj":
    aann = pd.read_csv("generate_sentence_templates/adj_exp.csv")
    templates = pd.read_csv("generate_sentence_templates/templates_adj.csv", keep_default_na=False)
    PREFIXDICT = defaultdict(list)
    for i in range(templates.shape[0]):
        x = templates.loc[i, :]
        PREFIXDICT[x["type"]] += [(x["first"], x["second"])]
  elif args.adj == "adj_agree":
    aann = pd.read_csv("generate_sentence_templates/adj_exp_agree.csv")
    templates = pd.read_csv("generate_sentence_templates/templates_adj_agree.csv", keep_default_na=False)
    PREFIXDICT = defaultdict(list)
    for i in range(templates.shape[0]):
        x = templates.loc[i, :]
        second = " " + x["second"]
        PREFIXDICT[x["type"]] += [(x["first"], second, x["singplur"])]
        
  else:
    aann = pd.read_csv("generate_sentence_templates/aann_main.csv")

    PREFIXDICT = {"spentLondon": [("The family spent", " in London"),
                                  ("The diplomat worked for", " in Nairobi"),
                                  ("The tourist passed", " in Papua New Guinea")],
                  "storeBought": [("She bought", ""),
                                  ("They discovered", ""),
                                  ("Someone saw", "")],
                  "servedDinnerTo": [("We served dinner to", ""),
                                    ("", " arrived and asked for dinner")],
                  "agreement": [("", " seem reasonable to me"),
                                ("", " seems reasonable to me")],
                  "agreementHuman": [("", " want dinner immediately"),
                                    ("", " wants dinner immediately")],
                  "twoAdjs": [("The family spent", " in London"),
                              ("The diplomat worked for", " in Nairobi"),
                              ("The tourist passed", " in Papua New Guinea")]
                  }

  PREFIXDICTTWOADJ = set(["twoAdjs"])
  DOALL = set(["spentLondon", "storeBought"])
  ONLYEXP = set(["temporal", "objects", "human", "art", "unitlike", "distance"])
  sents = get_sents_dict()
  #print(sents)

  if args.numsents > 0:
    sents = sents[:args.numsents]
  get_ratings(sents, args.outfile, args.start)
