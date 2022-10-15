"""# Code for AANN Project

# Python files
- aann_experiments.py: this file reads generate_sentence_templates files as input and collects GPT-3 ratings. You need an OpenAI key code to use it.
- cola.py: this contains the prompt and contains code to test the prompt on the COLA benchmark set

# R analysis files
- mainexp_analysis_gpt3.R: this file reads the output of aann_experiments.py and analyzes the results for the main epxeriment (comparing the AANN construction against the default construction). It also generates the mturk input csvs for this experiment
- mainexp_mturk_analysis.R: this file reads the output of the mturk experiment and analyzes the results, along with the GPT3 ratings
- adjexp_analysis.R: this file reads the output of aann_experiments.py and analyzes the results for the adjective experiment (comparing the AANN construction against the default construction). It also generates the mturk input csvs for this experiment
- adjorder_analysis.R: this file reads the output of aann_experiments.py and analyzes the results for the adjective order experiment (comparing the AANN construction against the default construction). It also generates the mturk input csvs for this experiment

# generate_sentence_templates files
These files contain template files read by aaan_experiments.py and used to generate sentences, based on templates.
They are in the format of a csv file, with columns for different sub-experiment templates. Each row is a particular word.
A 1 in a particular cell indicates that the indicated word should be used for a particular experiment.
If it is 0 or empty, it will not be used.
For the "main_exp" experiment, the templates are in aann.py, but for the rest there are separate template files.

# gpt3_data
These files are the output of aann_experiments.py for the GPT3 rating studies.

# mturk_data files
These are MTurk results for various conditions.

# mturk_generation
These are the files needed to re-run MTurk experiments. They include the html templates, as well as input csvs for each of the 3 sub-experiments.
"""
