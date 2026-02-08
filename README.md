# GazaNowPop
> Population nowcasting for Gaza neighbourhoods using aggregated and anonymised telecommunications data

This repository contains source code developed to estimate populations inside Gaza in near real-time to support international humanitarian response through the Site Management Cluster (SMC) and the Assessment and Information Management (AIM) working group led by the United Nations Office for the Coordination of Humanitarian Affairs.  

## Features
- Data wrangling of various input data sources
- Indirect estimation of population sizes from counts of telecoms subscribers on each tower
- Summarising results and reporting

## Dependencies

### Software
- R and the following R packages: dplyr, terra, sf
- Quarto

### Data
- Current counts of active subscribers for each geolocated telecommunications tower (.csv)
- Administrative boundaries for Gaza governorates, municipalities, and neighbourhoods (.shp)
- Building footprints (.gpkg)
- Militarised border zone (.gpkg) 
- Evacuation orders by block (.csv; included in this repo)
- Evacuation blocks (.geojson; included in this repo)

# License
Copyright (c) 2026 Real Good Research Limited.  
This project is licensed under the [GNU GPL v3](LICENSE).  

# Acknowledgements
This work was funded through the UN humanitarian fund for Gaza via [Acted](https://www.acted.org/) and the Site Management Cluster.

# Author
Douglas R. Leasure, PhD.  
Director and Research Data Scientist  
Real Good Research Ltd.  
[https://realgoodresearch.com](https://realgoodresearch.com)  
