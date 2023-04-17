# e120plantcluster

![e120 plot](FureyGN_e120plot.jpeg)

## Description

The project applies a functional trait approach to a long-term plant biodiversity experiment (Biodiversity II). The Cedar Creek Ecosystem Science Reserve (CCESR) experiment name is e120 "Biodiversity II:  Effects of Plant Biodiversity on Population and Ecosystem Processes".  

## Reproducing results 

To reproduce the analyses, the scripts are labelled for each figure and table in order. Cleaning scripts process raw data into derived data used for analyses. Derived data, figures and tables are all created using the targets pipeline. Files within the "Data" directory are not modified. To run the targets pipeline, install targets and then run tar_make(). Each target uses a function to either clean a data file, run an analyses or create a figure, table or summary statistic. 

## Analyses pipeline description

The analyses are run on species trait data and dried aboveground biomass estimates for the e120 plots. The analyses also require a phylogenetic tree for the species set. The current version uses a phylogeny generate using R package V-phylomaker (Jin & Qian 2019). The analyses are truncated into separate functions that run an operation on sets of data. I have tried to make logical breaks in the pipeline to check for errors or to use the output in another function. In the _targets.R file, one can examine the source for each output and the arguments for each function. Analyses use either the mean trait for each species, or the full set of values for each species. The analyses should be reproducible to alternative trees, to new sources of trait data, or to probe for errors. The targets pipeline can be visualized to see dependancies using tar_visnetwork() or open the dependency file "e120plantcluster_tar_visnetwork.html". Each file is described below by its role in the analyses.


## Files  

### Make file
_targets.R  
This file manages the data cleaning and analyses  

### Cleaning files  

0_e120plantcluster_specieslist.R  
Generate species list for e120 experiment  
1.1_cleanfile_e120plantcluster_get_aafe120.R  
Get local trait data used in Cadotte 2009  
1.2_cleanfile_e120plantcluster_aafe120.R  
Clean local trait data   
2.1_cleanfile_e120plantcluster_tryrequest.R  
Get try trait names   
2.2_cleanfile_e120plantcluster_try_alltraits.R  
Clean try trait data   
3_cleanfile_e120plantcluster_kothari_tree.R  
Load in Kothari 2018 tree - not used   
4_cleanfile_e120plantcluster_vphylomaker_generate_tree.R  
Generate phylomaker tree   
5_cleanfile_e120plantcluster_cadotte_tree.R  
Clean Cadotte et al. 2009 tree - not used  
6.1_cleanfile_e120plantcluster_tissuechem_get.R
Pull trait data from EDI DOI
6.2_cleanfile_e120plantcluster_tissuechem.R
Clean local tissue chemistry data  
7_cleanfile_e120plantcluster_generate_mergedata.R  
Generate merge mean trait matrix   
8_cleanfile_e120plantcluster_traitnames.R  
Clean trait name   
9.1_cleanfile_e120plantcluster_e120biomass_EDI_get.R  
Get e120 plot scale biomass   
9.2_cleanfile_e120plantcluster_e120biomass_EDI.R  
Clean and calculate plot level means for aboveground biomass  
### Analyses scripts  
  
1_e120plantcluster_generate_phylosignal_SuppTable3.R    
Generate phylogenetic signal for Supplemental Table 3    
2_e120plantcluster_generate_clusters.R    
Generate trait x phylogeny clusters using cophentic correlation      
3.1_e120plantcluster_generate_SupplementalFigure1.R    
Generate data for Supplemental Figure 1      
3.2_e120plantcluster_plot_SupplementalFigure1.R    
Plot Supplemental Figure 1   
3.3_e120plantcluster_topcor.R    
Examine the top correlation per category      
4_e120plantcluster_plot_dendro_Figure1.R    
Plot Figure 1       
5.1_e120plantcluster_generate_freq_Figure2.R    
Generate frequency occurrence of traits where cor > 0.8    
5.2_e120plantcluster_plot_freq_Figure2.R    
Plot frequency occurrence figure    
5.3_e120plantcluster_generate_fgmean_Figure2.R    
Generate mean and standard errors for traits with a phylogenetic signal and create a plot    
5.4_e120plantcluster_comboplot_freq_Figure2.R    
Combine all panels for Figure 2    
6.1_e120plantcluster_generate_rank_SuppFig2.R    
Generate ranking of various combinations of trait runs for Supplemental Figure 2    
6.2_e120plantcluster_plot_rank_SuppFig2.R    
Plot Supplemental Figure 2    
6.3_e120plantcluster_generate_rank_results.R    
Report rankings for caption for Supplemental Figure 2      
7_e120plantcluster_generate_ANPP_Figure3.R  
Run linear regression and plot Figure 3  
8.1_e120plantcluster_generate_volume.R  
Generate (hyper)volumes using hyper volume package    
8.2_e120plantcluster_plot_volume_Figure3b.R  
Plot Figure 3 panel b    
8.3_e120plantcluster_volume_tests.R  
Generate objects for intersection tests     
8.4_e120plantcluster_volume_CI.R  
Run intersection tests     
8.5_e120plantcluster_volume_examine.R  
Examine tests that are reported in results     
9_e120plantcluster_generate_PCA_Figure4.R  
Generate and plot PCA in figure 4  
10_e120plantcluster_generate_Supplementalfigure3.R  
Generate and plot mean and standard error for C3 vs. C4 difference  
ELEFUREYSA3.Rmd    
Show TRY 5.0 data processing and cite trait packages  
ELEFUREYSA3.Rmd    
Show extra details on how (hyper)volumes were calculated    
userMatrix.rds  
Custom orientation of Figure 3b    
windowRect.rds  
Custom orientation of Figure 3b    
zoom.rds  
Custom orientation of Figure 3b    

### Important files to reproduce analyses  
  
To reproduce Figure 1  
Data_derived/data_e120plantcluster_meantraits.csv
Data_derived/phylomakertree_new_tree.rds  
To reproduce Figure 2  
Tables/table_e120plantcluster_Clusters_phylomaker.csv
Tables/table_e120plantcluster_SupplementalTable3_phylomaker.csv
Data_derived/data_e120plantcluster_meantraits_long.csv  
To reproduce Figure 3a  
Data_derived/data_e120plantcluster_e120biomass.csv  
To reproduce Figure 3b  
Data_derived/data_e120plantclust_tissuechemistry.csv
Data_derived/e120plantcluster_Asterids_hv_silverman.rds
Data_derived/e120plantcluster_Fabaceae_hv_silverman.rds
Data_derived/e120plantcluster_Poaceae_hv_silverman.rds  
To reproduce Figure 4  
Data_derived/data_e120plantcluster_meantraits.csv

### Raw data DOI
1.1_cleanfile_e120plantcluster_get_aafe120.R  
Tilman, D. (2018). Plant traits: Biodiversity II: Effects of Plant Biodiversity on Population and Ecosystem Processes ver 8. Environmental Data Initiative. https://doi.org/10.6073/pasta/67d59b1ad2739791ced5606d7f512ae9.  
6.1_cleanfile_e120plantcluster_tissuechem_get.R
Furey, G.N. & Tilman, D. (2023). Species trait tissue chemistry: Biodiversity II: Effects of Plant Biodiversity on Population and Ecosystem Processes ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/c5955cf005ea393fe680f574d1eed200.  
9.1_cleanfile_e120plantcluster_e120biomass_EDI_get.R  
Tilman, D. (2021). Plant aboveground biomass data: Biodiversity II: Effects of Plant Biodiversity on Population and Ecosystem Processes ver 10. Environmental Data Initiative. https://doi.org/10.6073/pasta/7ef2de3865062d7352f7b20753ecd39b.  


---