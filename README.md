# SI_Staehlke_MDPI_Cells
Supporting information for the paper by Staehlke, Susanne; Haack, Fiete; Waldner, Anna-Christin; Koczan, Dirk; Moerke, Caroline; Müller, Petra et al. (2020): 
"ROS Dependent Wnt/β-Catenin Pathway and Its Regulation on Defined Micro-Pillars—A Combined In Vitro and In Silico Study." In: Cells 9 (8), S. 1784. 
DOI: 10.3390/cells9081784.

This README briefly explains the content of the subdirectories and how to replicate all experiments and obtain the figure shown in the paper mentioned above. 
The scripts and README were created with support of Kai Budde.  

## Requirements for Replicating all Experiments:

    Install Maven.
    Install R (and, optionally, RStudio).
    Run "ExecuteSesslandPlotResults_Replications.R". (Source the content of the active "ExecuteSesslandPlotResults_Replications.R" document in RStudio. 
    If an error message shows up, try to resource the script. 
    All requirements, such as Sessl and ML-Rules, should be automatically downloaded. (This is currently working under Linux (Centos 7) and Windows 10.))
    All plots are created within the new directory "/experiments/Plots/".

### Subdirectory data:

This directory contains reference data.


### Subdirectory experiments:

This directory contains subdirectories with all in silico experiments. 

### Subdirectory models:

This directory contains the simulation model. 
