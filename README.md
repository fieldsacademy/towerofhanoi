Code to implement the Tower of Hanoi puzzle in R, and create R shiny app. \
Play the game here: https://fieldsacademy.shinyapps.io/towerofhanoi/

This code and app were developed for the Fields Academy at The Fields Institute for Research in Mathematical Sciences. \
Author: Emma Kroell, https://www.emmakroell.ca \
Copyright: The Fields Institute for Research in Mathematical Sciences, http://www.fields.utoronto.ca/activities/academy

# Game rules
The Tower of Hanoi is a puzzle where your goal is to move the blocks from spot 1 to spot 3, in the same order. However, you must follow three rules: 
1. You can only move one block at a time. 
2. You can only move the top block in each stack. 
3. You can only stack smaller blocks on top of large ones.

The goal is to have the user attempt to determine the winning algorithm, i.e., the algorithm which solves the puzzle in the least amount of moves, as well as the minimal number of moves needed to solve the puzzle with a given number of blocks.

# References 
Attali, Dean (2016). Need any more reason to love R-Shiny? Here: you can even use Shiny to create simple games!
   https://deanattali.com/blog/shiny-game-lightsout/. \
Attali, Dean (2021). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 2.1.0.
   https://CRAN.R-project.org/package=shinyjs \
Chang, Winston, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff Allen, Jonathan McPherson, Alan Dipert
   and Barbara Borges (2021). shiny: Web Application Framework for R. R package version 1.7.1. https://CRAN.R-project.org/package=shiny \
Wickham, Hadley (2020). Mastering Shiny. https://mastering-shiny.org/index.html.
