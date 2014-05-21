Negtracker
==========

Writeup, code, and data for Nordmeyer & Frank, "The role of context in young children’s comprehension of negation."

Writeup
-------

Contains submitted draft of the paper. 

Materials
---------

Has as subdirectories:
- analysis
- annotations
- data
- demographics

The analysis subdirectory contains a number of scripts to reproduce the figures and statistical analyses in the paper. In order to reproduce these, you must:

1. Run negtracker_makeLongData.R - this script will process the many raw data files from the eye-tracker. It will take a substantial amount of time (on the order of 30 minutes). 
2. Run negtracker_exclusionCriteria.R - this script implements the planned exclusions described in the manuscript. 

Subsequent analyses will then be run on a single datafile that is created in this process (but is too large to host on git). 

The annotations directory contains timing information for the experiment, and the demographics directory contains the demographic information used for analysis and exclusion. 
 