# Residual diagnostics for mixed models

This is the repo for a 2020-2021 summer project to conduct a visual inference study on appropriate residual diagnostics for multi-level models. 

* Based on work by [Kaiwen Jin Master's thesis](https://github.com/kaiwenjanet/master).
* The Shiny app for the experiment is available [here](https://kaiwen-jin.shinyapps.io/experiment/).

# Overall goal

* Build from Kaiwen's thesis

1. Clean up shiny app
2. Check metric underlying is correct
3. Run experiment 

# Milestone

## 2020/12/07

* Read Kaiwen's thesis and go through Shiny app code 

## 2020/12/11: 

* Read [these papers](https://paperpile.com/shared/dxNYN7). Might be best to start off with Graphical inference for infovis paper. 
* Note: Singer et al. paper is hard to read. 
* **Write a summary as you read.** 
* Commit summaries and other additions regularly to this repo.
* Over summer break, get the R Shiny experiment working and embed your own lineup.

## 2021/01/07:

* [carry over] get the R Shiny experiment working and embed your own lineup
* Work through the code by Kaiwen to get the confounded conditional residual.

## 2021/01/14:

* Work through the simulation code by Kaiwen (focus just on sleep data for the moment)
* Change the simulation setup so that there is more difference between null and data plots. 
* Think about how you would set up the experiment to test the 
 difference between least confounded residual vs. conditional residual. 
 
## 2021/01/21:

* Read https://style.tidyverse.org/ and follow the style guidelines there to improve the legibility of the code. You might want to use [`styler`](https://github.com/r-lib/styler) for some easy restyling.
* Documentation of the code
