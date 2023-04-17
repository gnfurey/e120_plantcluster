#####
#Targets workflow for project e120plantcluster
####
library(targets)
library(tarchetypes)
tar_option_set(packages = c("tidyverse"))
#####
# helping functions
####
source("Functions/00_e120plantcluster_functions.R")
source("Functions/999_saveandreturn.R")
#####
# cleaning files
####
#get species taxonomy and metadata
source("Cleaningfiles/0_e120plantcluster_specieslist.R")
# clean cadotte 2009 trait data 
source("Cleaningfiles/1.1_cleanfile_e120plantcluster_get_aafe120.R")
source("Cleaningfiles/1.2_cleanfile_e120plantcluster_aafe120.R")
# get try data 
source("Cleaningfiles/2.1_cleanfile_e120plantcluster_tryrequest.R")
# clean try trait data
source("Cleaningfiles/2.2_cleanfile_e120plantcluster_try_alltraits.R")
# read in kothari 2018 tree - not used in the present analyses as Asteraceae not accurate 
source("Cleaningfiles/3_cleanfile_e120plantcluster_kothari_tree.R")
# generate phylomaker tree
source("Cleaningfiles/4_cleanfile_e120plantcluster_vphylomaker_generate_tree.R")
# read in cadotte 2009 tree - not used as it was not accurate using the species presented
source("Cleaningfiles/5_cleanfile_e120plantcluster_cadotte_tree.R")
# accession script to get e120 plant chemical traits from EDI
source("Cleaningfiles/6.1_cleanfile_e120plantcluster_tissuechem_get.R")
# clean chemical traits 
source("Cleaningfiles/6.2_cleanfile_e120plantcluster_tissuechem.R")
# merge together clean data
source("Cleaningfiles/7_cleanfile_e120plantcluster_generate_mergedata.R")
# get clean names
source("Cleaningfiles/8_cleanfile_e120plantcluster_traitnames.R")
# get e120 biomass
source("Cleaningfiles/9.1_cleanfile_e120plantcluster_e120biomass_EDI_get.R")
source("Cleaningfiles/9.2_cleanfile_e120plantcluster_e120biomass_EDI.R")
#####
# Analysis
####
# Table 1
# tests for the phylogenetic signal
source("1_e120plantcluster_generate_phylosignal_SuppTable3.R")
####
# generate data for clustering figures
source("2_e120plantcluster_generate_clusters.R")
####
# Supplemental Figure 1 
# get top runs
source("3.1_e120plantcluster_generate_SupplementalFigure1.R")
# plot Supplemental Figure 1 
source("3.2_e120plantcluster_plot_SupplementalFigure1.R")
#examine top correlations - numbers in main text and supplement
#source("3.3_e120plantcluster_topcor.R)
####
# Figure 1
# dendrogram
source("4_e120plantcluster_plot_dendro_Figure1.R")
####
# Figure 2
####
# count occurrences of each cluster
source("5.1_e120plantcluster_generate_freq_Figure2.R")
# plot occurrence
source("5.2_e120plantcluster_plot_freq_Figure2.R")
# generate and plot means by clade
source("5.3_e120plantcluster_generate_fgmean_Figure2.R")
# generate final combination plot for figure 2
source("5.4_e120plantcluster_comboplot_freq_Figure2.R")
####
# Supplemental Figure 2
####
# generate ranks
source("6.1_e120plantcluster_generate_rank_SuppFig2.R")
# plot
source("6.2_e120plantcluster_plot_rank_SuppFig2.R")
# stats for the caption
# source("6.3_e120plantcluster_generate_rank_results.R")
####
# Figure 3a
# generate analyses and figure
source("7_e120plantcluster_generate_ANPP_Figure3.R")
####
# Figure 3b
# hypervolume
# calculate volume
source("8.1_e120plantcluster_generate_volume.R")
# plot using RGL
source("8.2_e120plantcluster_plot_volume_Figure3b.R")
# test for volume size
source("8.3_e120plantcluster_volume_tests.R")
# run intersection tests
source("8.4_e120plantcluster_volume_CI.R")
# examine the volumes
# source("8.5_e120plantcluster_volume_examine.R")
# system("open 8.5_e120plantcluster_volume_examine.R")
####
# PCA
# Figure 4
source("9_e120plantcluster_generate_PCA_Figure4.R")
####
#Fig S3 C3/C4 difference
source("10_e120plantcluster_generate_Supplementalfigure3.R")
#####
#Targets
####
list(
  #####
  # generic files
  ####
  # species list
  # Supplemental Table 1
  tar_target(
    e120species_meta,
    "Data/e120_species_meta.csv",
    format = "file",
  ),
  tar_target(
    e120_species,
    get_e120species(e120species_meta=e120species_meta),
    format = "file"
    # system(command = "open Cleaningfiles/0_e120plantcluster_specieslist.R")
    # system(command= "open Data_derived/data_e120plantcluster_e120species.csv")
  ),
  #####
  # Trait data
  ####
  # local trait data
  ####
  # cadotte trait data
  tar_target(
    aafe_raw,
    get_aaf_e120(1),
    format = "file",
    # system(command = "open Cleaningfiles/1.1_cleanfile_e120plantcluster_get_aafe120.R")
    # system(command= "open Data/data_e120plantcluster_RAW_aafe120.csv")
    # Package ID: knb-lter-cdr.414.8 Cataloging System:https://pasta.edirepository.org.
  ),
  tar_target(
    aafe_meta,
    "Data/aafe120_Traitmeta.csv",
    format = "file"
    # https://www.cedarcreek.umn.edu/research/data/dataset?aafe120
  ),
  # get data file
  tar_target(
    aafe120,
    cleanaafe120(aafe_raw,
      datatype = "data",
      aafe_meta1 = aafe_meta
    ),
    format = "file"
    # system(command= "open Cleaningfiles/1.2_cleanfile_e120plantcluster_aafe120.R")
    # system(command= "open Data_derived/data_e120plantcluster_aafe120.csv")
  ),
  # get names from cadotte file
  tar_target(
    aafe120_name,
    cleanaafe120(aafe_raw,
      datatype = "names",
      aafe_meta1 = aafe_meta
    ),
    format = "file"
    # system(command= "open Cleaningfiles/1.2_cleanfile_e120plantcluster_aafe120.R")
    # system(command= "open Data_derived/aafe120_Traitmeta_clean.csv")
  ),
  ####
  tar_target(
    e120plantcluster_chemtraits_source,
    get_tissuechem_e120(1),
    format = "file"
    # system(command = "open Cleaningfiles/6.1_cleanfile_e120plantcluster_tissuechem_get.R")
    # system(command = "open Data/data_e120plantclust_tissuetrait_meta_edi.csv")
    # Package ID: knb-lter-cdr.715.3 Cataloging System:https://pasta.edirepository.org.
  ),
  tar_target(
    e120chem,
    clean_planttraits(e120plantcluster_chemtraits_source),
    format = "file"
    # system(command = "open Cleaningfiles/6.2_cleanfile_e120plantcluster_tissuechem.R")
    # system(command = "open Data_derived/data_e120plantclust_tissuechemistry.csv")
  ),
  ####
  # trydata
  # try trait list
  tar_target(
    try_trait_list,
    "Data/traitID.csv",
    format = "file"
  ),
  tar_target(
    try_request,
    try_request_fun(try_trait_list),
    format = "file"
    # system(command = "open Cleaningfiles/2.1_cleanfile_e120plantcluster_tryrequest.R")
    # system(command = "open Data/Alltrytraits.txt")
  ),
  # source try data base request 19226 FUREY
  tar_target(
    trydata_raw,
    "Data/19226_27012022110447/19226.txt",
    format = "file"
    # source Try DB 5.0 request portal George N. Furey
    # user furey034@umn.edu
  ),
  tar_target(
    trydata_clean,
    tryclean(trydata_raw, datatype = "data"),
    format = "file"
    # system(command= "open Cleaningfiles/2.2_cleanfile_e120plantcluster_try_alltraits.R")
    # system(command= "open Data_derived/data_e120plantcluster_tryclean.csv")
  ),
  # trynames
  tar_target(
    trydata_names,
    tryclean(trydata_raw, datatype = "names"),
    # system(command= "open Cleaningfiles/2.2_cleanfile_e120plantcluster_try_alltraits.R")
    # system(command= "open Data_derived/data_e120plantcluster_datanames_try.csv")
    format = "file"
  ),
  # long data file for checking distribution of trait values
  tar_target(
    trydata_long,
    tryclean(trydata_raw, datatype = "long"),
    # system(command= "open Cleaningfiles/2.2_cleanfile_e120plantcluster_try_alltraits.R")
    # system(command= "open Data_derived/data_e120plantcluster_longtry.csv")
    format = "file"
  ),
  #####
  # trees
  ####
  # store Kothari's tree
  tar_target(
    shantree,
    "Data/Shantree.txt",
    format = "file"
    # source https://static-content.springer.com/esm/art%3A10.1007%2Fs11099-018-0777-9/MediaObjects/11099_2018_777_MOESM4_ESM.pdf
  ),
  # store Cadotte's tree
  tar_target(
    cadotte_tree,
    "Data/cadotte_tree.txt",
    format = "file"
    # source https://doi.org/10.1371/journal.pone.0005695.s001
  ),
  # read in Kothari's tree
  tar_target(
    shantree_dat,
    readtree(shantree, datatype = "tree")
    # system(command = "open Cleaningfiles/3_cleanfile_e120plantcluster_kothari_tree.R")
  ),
  # read in Kothari's tree as RDS
  tar_target(
    shantree_file,
    readtree(shantree, datatype = "file"),
    # system(command = "open Cleaningfiles/3_cleanfile_e120plantcluster_kothari_tree.R")
    # return("Data_derived/shantree.rds")
    format = "file"
  ),
  # read in cadotte tree
  tar_target(
    cadotte_tree_dat,
    readtree_cadotte(cadotte_tree, datatype = "tree")
    # system(command = "open Cleaningfiles/5_cleanfile_e120plantcluster_cadotte_tree.R")
  ),
  # read in cadotte tree as RDS
  tar_target(
    cadotte_tree_file,
    readtree_cadotte(cadotte_tree, datatype = "file"),
    format = "file"
    # system(command = "open Cleaningfiles/5_cleanfile_e120plantcluster_cadotte_tree.R")
    # return("Data_derived/cadotte_tree.rds")
  ),
  # run Vphylomaker as targets object
  tar_target(
    phylomaker_tree_dat,
    phylomaker_tree(datatype = "tree", choice = "",
                    e120_species=e120_species)
    # system(command = "open Cleaningfiles/4_cleanfile_e120plantcluster_vphylomaker_generate_tree.R")
  ),
  # run Vphylomaker as RDS
  tar_target(
    phylomaker_tree_file,
    phylomaker_tree(datatype = "file", choice = "",
                    e120_species=e120_species),
    format = "file"
    # system(command = "open Cleaningfiles/4_cleanfile_e120plantcluster_vphylomaker_generate_tree.R")
    # Data_derived/phylomakertree_tree.rds
  ),
  #####
  #naming files
  ####
  # get common name table for Supplemental Table 2
  tar_target(
    nametable_csv,
    namemake(
      e120chem1 = e120chem,
      aafenames = aafe120_name,
      trynames = trydata_names,
      datatype = "csv"
    ),
    # system(command=" open Cleaningfiles/8_cleanfile_e120plantcluster_traitnames.R")
    # system(command= "open Tables/table_e120plantcluster_Traitnames.csv")
    format = "file"
  ),
  tar_target(
    nametable_word,
    namemake(
      e120chem = e120chem,
      aafenames = aafe120_name,
      trynames = trydata_names,
      datatype = "word"
    ),
    # system(command= "open Cleaningfiles/8_cleanfile_e120plantcluster_traitnames.R")
    # system(command= "open Tables/table_e120plantcluster_SupplementalTable2.docx")
    format = "file"
  ),
  #####
  #merging data
  ####
  # all mean traits together in one N x P matrix
  tar_target(
    meantraitdat,
    mergefunction(
      e120_species=e120_species,
      chem = e120chem,
      cadotte = aafe120,
      trydat = trydata_clean,
      datatype = "mean"
    ),
    format = "file"
    # system(command= "open Cleaningfiles/7_cleanfile_e120plantcluster_generate_mergedata.R")
    # system(command= "open Data_derived/data_e120plantcluster_meantraits.csv")
  ),
  # long format
  tar_target(
    meantraitdat_long,
    mergefunction(
      e120_species=e120_species,
      chem = e120chem,
      cadotte = aafe120,
      trydat = trydata_clean,
      datatype = "long"
    ),
    format = "file"
    # system(command= "open Cleaningfiles/7_cleanfile_e120plantcluster_generate_mergedata.R")
    # system(command= "open Data_derived/data_e120plantcluster_meantraits_long.csv")
  ),
  #####
  # e120 biomass cleaning
  ####
  # e120biomass
  tar_target(
    e120_biomass_raw,
    get_biomass_e120(1),
    format = "file"
    # system(command= "open Cleaningfiles/9.1_cleanfile_e120plantcluster_e120biomass_EDI_get.R")
    # system(command= "open Data/data_e120plantcluster_e120biomass_raw.csv")
    #Package ID: knb-lter-cdr.273.10 
  ),
  tar_target(
    e120_biomass,
    generate_e120_biomass(e120_biomass_raw),
    format = "file"
    # system(command= "open Cleaningfiles/9.2_cleanfile_e120plantcluster_e120biomass_EDI.R")
    # system(command= "open Data_derived/data_e120plantcluster_e120biomass.csv") 
  ),
  #####
  # Analyses
  ####
  #####
  # SuppTable3: Phylogenetic signal using picante
  ####
  tar_target(
    SuppTable3_file,
    generate_phylosignal_SuppTable3(
      trait_dat = meantraitdat,
      nametable_csv1 = nametable_csv,
      tree_dat = phylomaker_tree_dat,
      datatype = "csv",
      namefile = "phylomaker"
    ),
    format = "file"
    # system("open 1_e120plantcluster_generate_phylosignal_SuppTable3.R")
    # system("open Tables/table_e120plantcluster_SupplementalTable3_phylomaker.csv")
  ),
  tar_target(
    SuppTable3_word,
    generate_phylosignal_SuppTable3(
      trait_dat = meantraitdat,
      nametable_csv1 = nametable_csv,
      tree_dat = phylomaker_tree_dat,
      datatype = "word",
      namefile = "phylomaker"
    ),
    format = "file"
    # system("open 1_e120plantcluster_generate_phylosignal_SuppTable3.R")
    # system("open Tables/table_e120plantcluster_SupplementalTable3_phylomaker.docx")
    # manual formatting
    # system("open Tables/table_e120plantcluster_SupplementalTable3_phylomaker_clean.docx")
  ),
  #####
  # generate clusters
  tar_target(
    clusters_file,
    generate_clusters(
      meantraitdat = meantraitdat,
      treechoice = phylomaker_tree_file,
      name = "phylomaker"
    ),
    format = "file"
    # system("open 2_e120plantcluster_generate_clusters.R")
    # system("open Tables/table_e120plantcluster_Clusters_phylomaker.csv")
  ),
  #####
  #SupplementalFigure1
  ####
  # Cluster cut off for Supplemental Figures
  tar_target(
    cutoff_choice_0.80,
    0.80,
  ),
  #
  # make best top three clusters
  tar_target(
    top3way_SupplementalFigure1_data,
    generate_top3way_SupplementalFigure1(
      clusters_file1 = clusters_file,
      nametable1 = nametable_csv
    )
    # system("open 3.1_e120plantcluster_generate_SupplementalFigure1.R")
  ),
  # generate figure 2
  tar_target(
    top3way_SupplementalFigure1_file,
    plot_top3way_SupplementalFigure1(
      top3way_SupplementalFigure1_data1 = top3way_SupplementalFigure1_data
    ),
    format = "file"
    # system("open 3.2_e120plantcluster_plot_SupplementalFigure1.R")
    # system("open Figures/e120plantcluster_SupplementalFigure1.jpg")
  ),
  #####
  # Figure 1
  ####
  tar_target(
    plot_dendro_Figure1_file,
    plot_dendro_Figure1(
      meantraitdat1 = meantraitdat,
      tree_file = phylomaker_tree_file,
      choice = "phylomaker",
      matrix1 = c("Calcium", "Potassium", "Nitrogen"),#change here if you want other traits
      fname = "Figures/e120plantcluster_Figure1"),
    format = "file"
    # system("open 4_e120plantcluster_plot_dendro_Figure1.R")
    # system("open Figures/e120plantcluster_Figure1.eps")
  ),
  #####
  #Figure 2
  ####
  # frequency count of each trait for SuppFigure1
  tar_target(
    freq_Figure2_data,
    generate_freq_Figure2(
      clusters_file1 = clusters_file,
      nametable_csv = nametable_csv,
      clust_stat = "cor",
      cutoff = cutoff_choice_0.80
    ),
    # system("open 5.1_e120plantcluster_generate_freq_Figure2.R")
  ),
  # Make plots using cophentic correlation
  tar_target(
    plot_freq_Figure2_dat,
    plot_freq_Figure2(
      list1 = freq_Figure2_data,
     fname = "phylomaker",
      cutoff = cutoff_choice_0.80
    )
    # format = "file"
    # system("open 5.2_e120plantcluster_plot_freq_Figure2.R")
  ),
  #
  tar_target(
    fgmean_Figure2,
    generate_fgmean_Figure2(
      meantraitdat_long1 = meantraitdat_long,
      SuppTable3_file1 = SuppTable3_file,
      nametable_csv = nametable_csv
    )
    # format = "file"
    #system("open 5.3_e120plantcluster_generate_fgmean_Figure2.R")
    #system("open Figures/e120plantcluster_Figure2_clademean.pdf")
  ),
  #final combination plot for Figure 2
  tar_target(
    comboplot_freq_Figure2_file,
    comboplot_freq_Figure2(
      plot_freq_Figure2_dat = plot_freq_Figure2_dat,
      fgmean_Figure2 = fgmean_Figure2
    ),
    format = "file"
    # system("open 5.4_e120plantcluster_comboplot_freq_Figure2.R")
    # system("open Figures/e120plantcluster_Figure2_combo.eps")
  ),
  #####
  #Supplemental Figure 2 
  ####
  tar_target(
    rank_SuppFig2_data,
    generate_rank_SuppFig2(
      clusters_file1 = clusters_file,
      nametable_csv = nametable_csv
    )
    # system("open 6.1_e120plantcluster_generate_rank_SuppFig2.R")
  ),
  tar_target(
    rank_SuppFig2_file,
    plot_rank_SuppFig2(
      rank_SuppFig2_data = rank_SuppFig2_data,
      nametable_csv = nametable_csv,
      SuppTable3_file = SuppTable3_file
    ),
    format = "file"
    # system("open 6.2_e120plantcluster_plot_rank_SuppFig2.R")
    # sytem("open Figures/e120plantcluster_SupplementalFigure2.pdf")
  ),
  #####
  # Figure 3 productivity
  ####
  tar_target(
    e120biomass_plot,
    generate_e120biomass(
      e120_biomass = e120_biomass,
      hypervolume_plot = hypervolume_plot,
      choice = "plot"
    ),
    format = "file",
    # system("open 7_e120plantcluster_generate_ANPP_Figure3.R")
    # system("open Figures/e120plantcluster_Figure3_ANPP.eps")
  ),
  tar_target(
    e120biomass_table,
    generate_e120biomass(
      e120_biomass = e120_biomass,
      hypervolume_plot = hypervolume_plot,
      choice = "table"
    ),
    format = "file",
    # system("open 7_e120plantcluster_generate_ANPP_Figure3.R")
    # system("open Tables/table_e120plantcluster_SupplementalTable4.docx")
  ),
  #####
  # Figure 3 (hyper)volumes
  ####
  # generate volume using all bandwidth estimators
  tar_target(
    hypervolume_list,
    generate_hypervolume(
      chem_dat = e120chem,
      scale1 = FALSE,
      type1 = "gaussian",
      e120_species1=e120_species
    )
    # system("open 8.1_e120plantcluster_generate_volume.R")
  ),
  # create a hypervolume joined object
  tar_target(
    my_vols,
    hypervolume::hypervolume_join(
      hypervolume_list[[1]],
      hypervolume_list[[2]],
      hypervolume_list[[3]]
    )
  ),
  #grass vol
  tar_target(
    Poaceae_hv_silverman,
    saveRDS_return(
      object = hypervolume_list[[1]],
      fname = "Data_derived/e120plantcluster_Poaceae_hv_silverman.rds"
    ),
    format = "file"
  ),
  #legume vol
  tar_target(
    Fabaceae_hv_silverman,
    saveRDS_return(
      object = hypervolume_list[[2]],
      fname = "Data_derived/e120plantcluster_Fabaceae_hv_silverman.rds"
    ),
    format = "file"
  ),
  #forb 
  tar_target(
    Asterids_hv_silverman,
    saveRDS_return(
      object = hypervolume_list[[3]],
      fname = "Data_derived/e120plantcluster_Asterids_hv_silverman.rds"
    ),
    format = "file"
  ),
  # Function to plot the hypervolume using rgl
  tar_target(
    hypervolume_plot,
    plot_hypervolume(
      hv = my_vols,
      e120_species1=e120_species,
      e120chem1 = e120chem,
      meantraitdat1 = meantraitdat,
      fname="Figures/e120plantcluster_Figure3b_volume3d.png"
    ),
    format = "file"
    # system("open 8.2_e120plantcluster_plot_volume_Figure3b.R")
    # system("open Figures/e120plantcluster_Figure3b_volume3d.png")
  ),
  # save paths of resampled hypervolumes
  tar_target(
    volume_tests_paths,
    volume_tests(hypervolume_list1 = my_vols),
    # system("open 8.3_e120plantcluster_volume_tests.R")
  ),
  # returns test statistics compare three hypervolumes in my_vols
  tar_target(
    volume_tests_dat,
    volume_tests_fin(
      hypervolume_list1 = my_vols,
      stat = "test",
      paths1 = volume_tests_paths
    ),
    # system("open 8.4_e120plantcluster_volume_CI.R")
  ),
  # returns the 95 CI for hypervolume intersection
  tar_target(
    volume_tests_dat_CI,
    volume_tests_fin(
      hypervolume_list1 = my_vols,
      stat = "CI",
      paths1 = volume_tests_paths
    )
    # system("open 8.4_e120plantcluster_volume_CI.R")
  ),
  #####
  #Figure 4
  ####
  # pcaplots
  tar_target(
    PCA_Figure4,
    generate_PCA_Figure4(
      meantraitdat1 = meantraitdat,
      SuppTable3_file1 = SuppTable3_file,
      nametable_csv = nametable_csv
    ),
    format = "file"
    # system("open 9_e120plantcluster_generate_PCA_Figure4.R")
    # system("open Figures/e120plantcluster_Figure4_PCA.pdf")
  ),
  #Figure S3
  ####
  tar_target(
    c3c4_FigS3,
    generate_c3c4_FigS3(meantraitdat_long1 = meantraitdat_long),
    format = "file"
    #system("open 10_e120plantcluster_generate_Supplementalfigure3.R")
    #system("open Figures/e120plantcluster_FigS3.pdf")
  ),
  #####
  #generate Supplemental Information
  ####
  #SI 2
  tar_render(name = SI2_try_clean, path = "ELEFUREYSA2.Rmd"),
  #SI 3
  tar_render(name = SI3_hypervolume, path = "ELEFUREYSA3.Rmd")
)
