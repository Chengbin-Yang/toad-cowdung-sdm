Code and data for "Of cows and frogs: cow dung concentration impacts amphibian early life stages"
====================================================================================================

This package is anonymised for peer review. No author names, affiliations, e-mail addresses
or local file paths are included.


CONTENTS
--------
data/     input data
code/     analysis code, one file per figure or processing step
results/  tables and figures produced by the code, kept for checking the numbers in the paper


HOW TO RUN
----------
Run from inside the code/ directory. Each script reads from ../data/ and writes to ../results/.

  1  cattle_density_buffer.py        python3   adjusted cattle density, Eq. (1)      -> Supplementary Figure 4
  2  concentration_calc.py           python3   concentration = 0.3 x mass / runoff
  3  concentration_classified_plot.py python3  preview of the concentration classes
  4  fig6a_pixel_stats.R             Rscript   medians and class areas                -> Figure 6a
  5  fig6a_violin.R                  Rscript   violin plots with Dunn tests           -> Figure 6a
  6  fig6b_class_area.R              Rscript   area by concentration class            -> Figure 6b
  7  fig3_temporal_dynamics.R        Rscript   stage trajectories and 50% days        -> Figure 3, Table 1
  8  fig4_metamorphosis.R            Rscript   box plots, forest plot, Cox models     -> Figure 4, Table 1, Table S6

The three scripts in code/archive/ are earlier versions of the experiment analysis, kept so that
every number reported in the paper can be traced back to a specific run.


WHAT IS INCLUDED AND WHAT IS NOT
---------------------------------
Every input needed to run the analysis scripts in code/ is now included, except the
environmental layers behind the species distribution model. All rasters are EPSG:4326.

Included and runnable end to end:

  Figure 3, Figure 4, Table 1            from 01_experiment_daily_records.xlsx
  Figure 6a                              from 04_pixel_habitat_concentration.csv
  Figure 6b                              from 08_class_area_data.csv (see the note below)
  cattle density and concentration       the four rasters 09 to 12 are the actual inputs,
                                         and code/09 to 11 rebuild them from GPW, GLC_FCS30D
                                         and the grazing mask

Not included:

  the species distribution model. 13_habitat_suitability_1km_utm53n.tif is the suitability
  surface used for the overlap analysis, produced by a collaborator at 1 km in UTM 53N.
  The MaxEnt and ENMeval scripts that produced it were run outside this project and are not
  part of this package, so Figures 5 and Supplementary Figures 2 and 3 cannot be rebuilt here.

  the drawings of Figures 1, 2 and 7, which are layout files rather than data.

NOTE ON 08_class_area_data.csv
------------------------------
This table was reconstructed from 04_pixel_habitat_concentration.csv and is the only file in
the package that is not an original output. Pixels were counted per habitat class and
concentration class, then multiplied by the ground area of one 10 m cell at 42.9 degrees N
(73.1 m2). That gives 41.7 km2 in total across the 570,004 pixels. The area of suitable
habitat quoted in Section 3.3 (1,663.19 km2, of which 299.64 km2 highly suitable) implies an
area per pixel of roughly 3,500 m2, about 48 times larger. The original area convention
therefore differs from the one used here and should be confirmed before Figure 6b is
regenerated from this file.

FILES IN results/
-----------------
results/fig4/            Figure 4 source figures (png, pdf) and 15 analysis tables
results/*.csv            primary and exploratory tables, assumption tests, Cox models,
                         and the descriptive statistics used in Tables 1, S2, S3, S4 and S6
results/fig4_v2/         output directory used by code/archive/experiment_analysis_v2.R


COMPUTING ENVIRONMENT
---------------------
R 4.1.2 with tidyverse, readxl, survival, DescTools, patchwork, FSA, eoffice
Python 3.11 with rasterio, numpy, matplotlib

eoffice writes figures into PowerPoint and needs a local Office installation; without it the
scripts still write png and pdf for Figure 4 but not for Figure 6a and 6b.


DATA NOTES
----------
01_experiment_daily_records.xlsx  61 sheets, Day-0 to Day-60, five treatments x 15 containers.
                                  Columns are eggs, tadpoles, limb development, froglets, adults
                                  and deaths, counted per container on each day.
04_pixel_habitat_concentration.csv  570,004 pixels with cow dung concentration and habitat
                                  suitability class (low, moderate, high).
07_village_points_grazing.csv     villages associated with free-ranging cattle grazing, used as
                                  the starting points for the 3.5 km buffers (Table S1).
08_class_area_data.csv            reconstructed, see the note above.
