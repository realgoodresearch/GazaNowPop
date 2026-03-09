#!/bin/bash

# linux system dependencies
apt-get update && apt-get install cmake libgdal-dev libproj-dev libgeos-dev 

# others that may be required (if not installed with above)
apt-get update && apt-get install libudunits2-dev libssl-dev libabsl-dev