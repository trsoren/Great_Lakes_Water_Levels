    This project is about analyzing the relative
movements of depth gauges in the Great Lakes over time.
These movements occur due to the uneven movement of the
earths crust. Specifically, we are interested in how water
levels from a Master Gauge that has existed since 1860 differ
from the lakewide average water level taken from a collection 
of many other gauges built after 1920. If we can find a
pattern in how the Master Gauge compares to the lakewide
average after 1920, then we can extrapolate that pattern
back to 1860 accounting for errors in the Master Gauge.

Lake_plot.pdf (outdated):
    This plots the monthly mean water level
data for the gauges and the difference between the 
master gauge and the lakewide average for each month. 
This graph is very noisy due to cycles in water level
over a year, so Lake_monthly_plot is much more useful.

Lake_monthly_plot.pdf:
    This splits up each months water levels and plots
the difference between the master gauge and lakewide
average. 

Lake_Yearly_plot.pdf:
    This takes the yearly average water levels instead
of the monthly. Also, this plot contains a linear regression
model with a prediction interval, and the extrapolated values
for lakewide average water levels before 1920. Eventually this
model will be created for each month of Lake_monthly_plot as well. 