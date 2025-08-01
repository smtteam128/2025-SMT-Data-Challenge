# Being Kelly Leak - A Statistical Analysis of Outfielder Intent in Professional Baseball
### Team 128 - 2025 SMT Data Challenge Submission

This ReadMe will guide you through our code, the order to run everything, and what each script entails.

## Script 1 - Data Wrangling for Center Fielders

This is the script where we get player position and ball tracking data on plays where the center fielder touches the ball. Keep in mind that we are also getting the positioning of left fielders and right fielders as well. The output will be a dataset of around 6000-7000 observations that contains the player positioning for the three outfielders between when the ball was hit off the bat and when the ball lands.

## Script 2 - Data Wrangling for Left Fielders

We do the same thing as script 1 except for plays where the left fielder touches the ball. We’ll get a similar output although slightly less observations than for the center fielders.

## Script 3 - Data wrangling for Right Fielders

Same thing as the previous scripts except for plays where right fielders caught the ball.

## Script 4 - Player speeds

This is a Python script containing information and code on how we calculated player maximum speeds on the base path and player 60-yard dash times. Save the output as a .csv and import it into script 5 (code is already written out).

## Script 5 - Modeling and applications

This contains our Random Forest and GAM models as well as TRS and other applications.

## Script 6 - Visualizations
Using the variables from script 5, we have conducted a few customized visualizations that appear throughout the paper.
