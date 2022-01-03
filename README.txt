The purpose of this project is to analyze the relative
movements of depth gauges in the Great Lakes over time.
These depth gauges are constanly rising/sinking over time
due to movements in the earths crust, however they don't all
shift at the same rate. For this project specifically, we are interested in how water
level readings from a Master Gauge that was built in 1860 differ
from the lakewide average water level taken from a collection 
of many other gauges built after 1920. Then by finding a linear
relationship in the difference between the Master Gauge and the lakewide
average after 1920, we can extrapolate that relationship
back to 1860 and use the Master Gauge data to more accuratly "predict" 
the actual water levels between 1860 and 1920.

extrapolated_yearly_water_levels contains the results of this analysis 
for each of the Great Lakes (Michigan and Huron are counted as 1 lake).
This provides lake water level data in a yearly format.

monthly_plots plots the difference between the Master Gauge and the 
Lakewide Average split across each month. In the future, these will be
used to provide extrapolated water level data in a monthly format instead of
yearly (each month must be treated independently from the others due to 
seasonal differences in rainfall, ice cover, etc). 

erie_monthly_plots plots the difference between several gauges all 
around the lake against the lakewide average. I wanted to visualize 
this after noticing that some gauges were consistently above/below
the lakewide average during the winter months, suggesting a seasonal 
"tilting" of Lake Erie. This has never been scientifically documented 
before and my team hopes to eventually publish a paper on this subject. 