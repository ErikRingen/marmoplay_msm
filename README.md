## Title: Common marmosets use body posture as multi-functional signal to solicit, maintain, and modify social play

## Summary

This study investigated whether common marmosets (Callithrix jacchus) use body posture as signal to regulate play. We recorded play within three captive common marmoset family groups. Three distinct signals (i.e. supine, hide, stalk) and six distinct play types (i.e. wrestle, chase, pounce, touch, catch, pull) were identified. We used a multi-state time-to-event model to analyze the sequences of play, including short- and long-term transitions between different states (i.e. signal, play, or rest/nothing). Our data-driven approach accounted for uncertainty in the duration of play bouts, using probabilistic classification rather than arbitrary bout thresholds. The resulting classifications allowed us to assess the social function of signals by comparing play behavior to a resting state baseline. We found that the presence of a signal: (1) increases the probability to play; (2) extends the duration of play; (3) leads to more diverse play; and (4) increases the probability of play fighting. Marmosets also show turn-taking of signaling and initiating subsequential play. These results show that marmosets use postures as communicative signals to initiate and change play dynamics, and thereby establish a mutual understanding of the joint action.

## Code version: 1.0

## Project Directory Structure

This is the directory structure for our analysis project:

``` markdown
Project/
├── R/                                    # R scripts
│   ├── functions.R                       # Smaller functions
│   ├── fig_classifier.R                  # plot pr(rest)
│   ├── fig_ctmc.R                        # plot rates of transition
│   ├── fig_duration_play.R               # plot inferred duration of play
│   ├── fig_pr_rest.R                     # plot pr(rest | signal)
│   ├── fig_segment_example.R             # example of classified sequence
│   ├── fig_4_behavior.R                  # plot pr(behavior | signal)
│   ├── pred_check.R                      # posterior predictive check
│   ├── segment_behavior.R                # sample of classified sequences
│   ├── standata_msm.R                    # prepare data for stan
│   ├── stanfit_play_focal.R              # fit role type stan model
│   ├── stanfit_play.R                    # fit other stan models
│   ├── time_series_expand.R              # timestamps -> behavior durations
│
├── stan/                                  # Stan model files
│   ├── behavior_model.stan                # type of play ~ signal
│   ├── diversity_model_ord_duration.stan  # diversity of play ~ signal + duration
│   ├── diversity_model_ord.stan           # diversity of play ~ signal
│   ├── duration_model_gamma.stan          # duration of play ~ signal
│   ├── msm_classifier_zi.stan             # multi-state model
│   ├── ndyad_model_ord.stan               # ndyads ~ signal
│   ├── roles_model.stan                   # role type ~ signal  
|
├── raw_data/                              # Raw study data file
│   ├── rawplay.csv          
|
├── _targets.R                             # targets() file to run analysis pipeline
├── README.md                              # Project overview and instructions
```

## Computational Environment

To run this code, you will need to [install R](https://www.r-project.org/) and the following R packages:

```         
install.packages(
c("tidyverse",
"lubridate",
"posterior",
"cmdstanr",
"tidygraph",
"ggraph",
"igraph",
"patchwork",
"Matrix",
"wesanderson",
"bayesplot",
"posterior")
)
```

You will also require the `rethinking` package, which you can install with the following code (see [here](https://github.com/rmcelreath/rethinking) for more details):

```         
# from https://github.com/rmcelreath/rethinking
install.packages(c("coda", "mvtnorm", "devtools", "loo", "dagitty"))
devtools::install_github("rmcelreath/rethinking")
```

Analyses from this manuscript were run in R version 4.3.2., with Stan models fit using cmdstan version 2.35.0.

### Executing code

1.  Set your working directory in R to this code repository
2.  Load the `targets` package with `library(targets)`
3.  To run all analyses, run `tar_make()`
4.  To load individual targets into your environment, run `tar_load(targetName)`

To visualize how the various code and functions work together in the pipeline, run `targets::tar_visnetwork()`.
