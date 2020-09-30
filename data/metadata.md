# Information about datasets used  

## data/co2_data.csv  
### CO2 influx. Each row represents a sample from a given pond at a given day.  
### Variable descriptions  
*influx:* CO2 influx (mmol/m2/day);  
*pond:* original pond code in the Dorset mesocosm;  
*treatment:* ambient (A) or warmed (H);  
*date:* sampling date (YYYY-MM-DD);  
*period:* before or after the addition of the tracer on the 16th July 2013.  

## data/control_data.csv  
### Stable isotope data from control ponds. Each row represents a sample from a given pond at a given day.  
### Variable descriptions  
*sample:* taxonomic group (phytoplankton, zooplankton);  
*pond:* original pond code in the Dorset mesocosm;  
*treatment:* control (C);  
*day:* day since of the experiment;  
*sample_vol_L:* sample volume;  
*biomass_mg_L:* sample biomass concentration;  
*C_ug:* sample total carbon mass;  
*N_ug:* sample total nitrogen mass;  
*t15N_ug:* sample total 15N mass;  
*N_atom_perc:* sample total nitrogen atom percent;  
*preN_atom_perc:* corresponding atom percent prior to the tracer addition;  
*add15N_perc:* added 15N percent, the main response variable.  

## data/data.csv  
### Stable isotope data from experimental ponds. Each row represents a sample from a given pond at a given day.  
### Variable descriptions  
*sample:* taxonomic group (phytoplankton, zooplankton);  
*pond:* mesocosm pond number (1--16);  
*treatment:* ambient (A) or warmed (H);  
*day:* day since of the experiment;  
*date:* sampling collection date;  
*sample_vol_L:* sample volume;  
*biomass_mg_L:* sample biomass concentration;  
*C_ug:* sample total carbon mass;  
*N_ug:* sample total nitrogen mass;  
*t15N_ug:* sample total 15N mass;  
*N_atom_perc:* sample total nitrogen atom percent;  
*preN_atom_perc:* corresponding atom percent prior to the tracer addition;  
*add15N_perc:* added 15N percent, the main response variable;  
*dorset_pond:* original pond code in the Dorset mesocosm.  

## data/er_gpp.csv  
### Metabolic balance. Each row represents a sample from a given pond at a given day.  
### Variable descriptions  
*pond:* original pond code in the Dorset mesocosm;  
*month:* month of sample collection;  
*treat:* ambient (A) or warmed (H);  
*ER.GPP:* ER to GPP ratio as explained in the Methods;  
*year:* month of sample collection;  
*id:* unique row id;  
*date:* sampling date (YYYY-MM-DD);  
*doy:* day of the year (1--365);  
*ratio_fits:* predicted ER to GPP ratio as explained in the Methods.

## data/nutrients.csv  
### DIN species. Each row represents a sample from a given pond at a given day.  
### Variable descriptions  
*pond:* original pond code in the Dorset mesocosm;  
*NO2_uM:* Nitrite (in umol / L);  
*NO3_NO2_uM:* Nitrate + Nitrite (in umol / L);  
*NH3_uM:* Ammonia (in umol / L);  
*sample_date:* sampling date (YYYY-MM-DD);  
*treatment:* ambient (A) or warmed (H);  
*NO3_uM:* Nitrate + Nitrite (in umol / L).  

## data/physchem_data.csv  
### Physico-chemical data. Each row represents a sample from a given pond at a given day.  
### Variable descriptions  
*Date:* sampling date (YYYY-MM-DD);  
*Temp:* Temperature (˚C);
*DOsat:* Dissolved oxygen saturation (%);  
*pH:* pH;  
*pond:* original pond code in the Dorset mesocosm;  
*hour:* hour of sampling;  
*day:* day of sampling;  
*month:* month of sampling;  
*code:* day-hour id of sample;  
*treatment:* ambient (A) or warmed (H);  
*pairs:* original pond block heating-element pairs in the Dorset mesocosm.  

## data/phyto_size_data_2012.csv  
### Pond-averages phytoplankton body mass from 2012.  
### Variable descriptions  
*pond:* original pond code in the Dorset mesocosm;  
*treatment:* ambient (A) or warmed (W);  
*av_C_ug:* mean body mass (ug carbon);  
*log_av_C_ug:* av_C_ug on log10 scale.  

## data/phyto_size_data_2016.csv  
### Pond-averaged phytoplankton body mass from 2016.  
### Variable descriptions  
*pond:* original pond code in the Dorset mesocosm;  
*treatment:* ambient (A) or warmed (W);  
*av_C_ug:* mean body mass (ug carbon);  
*log_av_C_ug:* av_C_ug on log10 scale.  

## data/rel_abun_phyto_2012.csv  
### Pond-averaged phytoplankton relative abundance from 2012.  
### Variable descriptions  
*pond:* original pond code in the Dorset mesocosm;  
*treatment:* ambient (A) or warmed (H);  
*remaining columns:* phytoplankton taxa.  

## data/rel_abun_phyto_2016.csv  
### Pond-averaged phytoplankton relative abundance from 2016.  
### Variable descriptions  
*pond:* original pond code in the Dorset mesocosm;  
*treatment:* ambient (A) or warmed (H);  
*remaining columns:* phytoplankton taxa (given as OTUs).  
