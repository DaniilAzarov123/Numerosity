# Numerosity Estimation: Wisdom of the Crowd and Wisdom of the Inner Crowd

This repository contains the experiments and analyses for a project investigating the **Wisdom of the Crowd (WoC)** and **Wisdom of the Inner Crowd (WoIC)** effects in numerosity estimation. We examine how aggregating multiple estimates — either across individuals or within a single individual — improves judgment accuracy, and what factors modulate this benefit.

## Project Structure

```
Numerosity/
├── Exp1/
│   ├── Analysis/           # R scripts and processed data for Experiment 1
│   │   ├── E1_DataPreparation.R
│   │   ├── E1_Analysis.R
│   │   ├── data/           # Raw JSON files (one per participant)
│   │   ├── data_processed/ # Processed CSVs (output of DataPreparation.R)
│   │   └── plots/          # Saved figures
│   └── Exp/                # Experiment files
│       ├── exp1_run.html   # Main experiment HTML
│       └── src/
│	    ├── consent.html
│           ├── assets/     # Stimuli images
│           ├── plugins/    # jsPsych plugins including custom numerosity plugin
│           └── php/        # PHP script for saving participant data to the lab server
├── Exp2/
│   ├── Analysis/           # R scripts and processed data for Experiment 2
│   │   ├── E2_DataPreparation.R
│   │   ├── E2_Analysis.R
│   │   ├── data/           # Raw JSON files (one per participant)
│   │   ├── data_processed/ # Processed CSVs (output of DataPreparation.R)
│   │   └── plots/          # Saved figures
│   └── Exp/                # Experiment files
│       ├── exp2_run.html   # Main experiment HTML
│       └── src/
│	    ├── consent.html
│           ├── assets/     # Stimuli images
│           └── plugins/    # jsPsych plugins including custom numerosity plugin
└── .gitignore
```

## Experiments

Both experiments were built with [jsPsych 7.3](https://www.jspsych.org/7.3/) (de Leeuw et al., 2023) and run online via SONA.

**Experiment 1** — Participants estimated the number of objects in a display. Each display was shown twice, either back-to-back (*paired*) or separated in time (*unpaired*). We compared the accuracy of individual estimates vs their geometric mean.

**Experiment 2** — Participants estimated the average number of objects across 8 rapidly presented displays. Displays varied in object identity (same vs different categories) and spatial configuration (same vs different positions) in a 2 × 2 factorial design. Each trial was presented twice, always separated in time (corresponds to the unpaired condition in Exp 1).

### Running the experiments

Both experiments can be run directly in the browser via GitHub Pages:

* **Experiment 1:** https://DaniilAzarov123.github.io/Numerosity/Exp1/Exp/exp1_run.html
* **Experiment 2:** https://DaniilAzarov123.github.io/Numerosity/Exp2/Exp/exp2_run.html

Note that **data will not be saved** without the original server setup — the experiments will run normally but responses will not be written to disk.

## Analysis

Analysis is split into two scripts per experiment:

1. **`E1_DataPreparation.R` / `E2_DataPreparation.R`** — Load raw JSON files, exclude poor-performing participants (top 5% by mean absolute error), log-transform responses, compute geometric means and absolute errors on both log and raw scales, sample random partners for the Wisdom of the Crowd analysis, and save processed CSVs to `data_processed/`.
2. **`E1_Analysis.R` / `E2_Analysis.R`** — Read processed CSVs and run all analyses: regression to the mean, similarity between estimates, WoIC effect, WoIC magnitude as a function of numerosity, WoC effect, direction of the second estimate, metacognition, and supplementary (permutation test of correlations of individual estimates and fatigue effect).

### Requirements

R packages: `jsonlite`, `dplyr`, `tidyverse`, `ggplot2`, `lme4`, `lmerTest`, `car`, `performance`, `effectsize`, `emmeans`, `glmmTMB`, `rstatix`

Install all at once:

```r
install.packages(c("jsonlite", "dplyr", "tidyverse", "ggplot2", "lme4",
                   "lmerTest", "car", "performance", "effectsize",
                   "emmeans", "glmmTMB", "rstatix"))
```

To run the analysis, open the relevant `.Rproj` file in RStudio, then run `DataPreparation.R` first, followed by `Analysis.R`.
