# High Lonesome 2022 Lottery
An interactive, transparent, and reproducible design for the 2022 High Lonesome 100 lottery. Code by Garret Christensen, volunteering in exchange for whiskey. Thanks to Caleb for a great race and wanting to do things transparently. If any other RDs are interested in running their lotteries transparently, I'd love to talk!

The lottery is implemented via R Shiny apps [here](https://garretchristensen.shinyapps.io/Lottery2022/).

The code for it is all in this repository that you're looking at.
Note that if you clone this repo, you can run the code on your own machine and get the same results, you may just have to change the data file name since I'm sharing a version here with no e-mail addresses.

* Lottery.Rmd implements the lottery as a Shiny App website.
* 2022HL100_lottteryentrants_final_noemails.csv is the complete final data, with emails removed for privacy.
* AddEmailtoCSV.R is code with the same basic code as the Shiny App, but it exports a CSV that the RD can upload to RunSignUp using runner e-mails.
* odds.txt is runner odds of being selected. It's calculated in AddEmailtoCSV.R, and I upload it here. (The raw file is linked as a hyperlink from the Shiny App website.)

Questions, comments, pull requests welcome!
