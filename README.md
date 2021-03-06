# COVID-19 Cases per Mio

The short scripts `r/cases_per_mio.R` and `py/cases_per_mio.ipynb`

1. download data from [European Centre for Disease Prevention and Control](https://www.ecdc.europa.eu/en),

2. select a couple of countries and do the necessary transformations, and

3. plots the cases per mio inhabitants including a scatter plot smoother.

The resulting plot of the Python script as per July 31, 2020:

![Some curves](cases_per_mio.png)

(note that the data structure of the required data has been changed from daily to weekly numbers. You will need to adapt the codes accordingly.)

# Weekly COVID-19 deaths per Mio

The short scripts `r/deaths_per_mio.R` and `py/deaths_per_mio.ipynb` are a version of above script, with focus on the more reliable numbers on death counts.

The resulting plot of the Python script is per January 15, 2021.

![Some curves](deaths_per_mio.png)

# COVID-19 number of positive and negative tests in Switzerland

The script `r/tests_switzerland.R` downloads data from the Federal Office of Public Health BAG and visualizes the number of positive and negative cases over time, aggregated per week.

![Tests](tests_Switzerland.png)
