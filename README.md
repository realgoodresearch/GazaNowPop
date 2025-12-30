# GazaNowPop
> Population nowcasting for Gaza neighbourhoods using telecommunications data

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

# Sensitivity Classification
The data produced by this source code have been classified as "severe sensitivity" with a "strictly confidential" classification based on the [Information Sharing Protocol for the Occupied Palestinian Territory](https://www.unocha.org/publications/report/occupied-palestinian-territory/information-sharing-protocol-occupied-palestinian-territory-june-2024). Although this repository contains no data (i.e. only source code), we have applied the same classification to the repository out of an abundance of caution. 

# License
This source code cannot be shared publicly. It can only be shared bilateraly on a case-by-case basis with assurance of upholding the highest standards of data responsibility, including data protection. Please contact the Site Management Cluster and/or the Assessment and Information Management Working Group (AIMWG) for access approval. The [repository admin](mailto:info@realgoodresearch.com) can then provide access for individual approved GitHub users.

# Acknowledgements
This work was funded through the UN humanitarian fund for Gaza via [Acted](https://www.acted.org/) and [Oxford University Innovation](https://innovation.ox.ac.uk/).

# Author
Douglas R. Leasure, PhD.  
Director and Research Data Scientist  
Real Good Research Ltd.  
[https://realgoodresearch.com](https://realgoodresearch.com)  
