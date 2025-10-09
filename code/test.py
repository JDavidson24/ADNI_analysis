# Install if needed: pip install rpy2
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri

# Activate conversion between R and pandas
pandas2ri.activate()

# Import the ADNIMERGE package in R
ro.r('library(ADNIMERGE)')
ro.r('data("ADNIMERGE")')

# Pull the R dataset into Python as a pandas DataFrame
adni = pandas2ri.rpy2py(ro.r('ADNIMERGE'))

# Now adni is a pandas DataFrame
print(adni.head())
print(adni.columns)
