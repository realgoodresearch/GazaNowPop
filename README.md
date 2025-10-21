# GazaNowPop
> Population nowcasting for Gaza neighbourhoods using telecommunications data

This repository contains source code developed to estimate populations inside Gaza in near real-time to support international humanitarian response through the Site Management Cluster (SMC) and the Assessment and Information Management (AIM) working group led by the United Nations OFfice for the Coordination of Humanitarian Affairs.  

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
This project is licensed under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) (see `./COPYING`).

# Acknowledgements
This work was funded through the UN humanitarian fund for Gaza via [Acted](https://www.acted.org/) and [Oxford University Innovation](https://innovation.ox.ac.uk/).

# Author
Douglas R. Leasure, PhD.  
Director and Research Data Scientist  
Real Good Research Ltd.  
[https://realgoodresearch.com](https://realgoodresearch.com)  
