# Does-a-Basketball-Player-in-his-prime-Play-More-Than-Others-
In this project we attempt to estimate the age in which a professional male basketball player
reaches the prime of his career. Throughout this project we are only concerned with players 
within the National Basketball Association (NBA) whom were active members of the
NBA in the 2019-2020 regular season. An active member of NBA refers to a player under
contract who had playing time on the court sometime during the season. Regular season
refers to the portion of the season before the playoff tournament. Thus we will refer to such
players simply as players. The goal of this project is to estimate which players in the future
regular season we expect to see an improvement in their minutes and which ones we expect
to experience a downfall based on their age. Since we desire to make mathematical based
prediction about future seasons we used data on players from the 2019-2020 regular season.
Data collected from the NBA's website.
In this project we have chosen to quantify the quality of a player by the amount of time
they actually play in a game. In particular we have utilized the mean minutes played per
game (MPG) throughout the season by each player. Since we are making inferences about
the mean MPG of a player based on their age we have divided all players into six distinct
categories based on their age. The NBA had exactly 529 players in the 2019-2020 season,
from ages of 19 to 43 years old (as of the beginning of the 2019-2020 season). Let N denote
the total sample size, i.e., the total number of active players within NBA in the 2019-2020
season: N = 529. These age categories were chosen based on age and for convenience based
on the number of samples in each group being similar. The goal of similar sample sizes was
not to achieve a balanced design, but to reduce the likelihood of having outliers.

First, in order to determine if a player in his prime plays more than others, we conducted
a one-way ANOVA test on mean MPG by age group. Then we conducted a Scheffe post
hoc test to find out which groups were different. This was followed by performing pooled
sample t-tests on the unequal means to fnd out which age groups had more or less MPG.
Next, to come up with a predictor equation for future NBA season, we performed multiple
regression to determine the effect of points per game (PTS), field goals made (FGM), assists
per game (AST), and steals (STL) on MPG. After coming up with the regression equation,
we used the best subsets method to see if we could come up with a better predictor equation.

The R code was written myself. The LaTeX file was written by Shakila Cham. And the report/analysis was a collaborative effort between myself, Thomas Connolly, and Shakila Cham.

This project was a requirement of the course AMS 572: Data Analysis I at Stony Brook University.
