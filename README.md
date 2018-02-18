R-Shiny app to plot the price of the Euro against other currencies - Nevan O'Keeffe.
This is my first GitHub repository so is unlikely to be an example of best practice.

Euro prices are pulled from https://www.ecb.europa.eu/stats/eurofxref
Each time the script is run it will pull the latest prices from a zip file available at the above link.

The script is available in this repository as finalScript.R.
The plotly and shiny libraries will need to be installed in R for the script to run, that can be done with the following 2 lines:
install.packages('Shiny')
install.packages('Plotly')


To do list:
- Work on aesthetics of plots (research equivalents for ggthemes, colorbrewer).
- Tidy up x axis labelling.
