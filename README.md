# Niwot Saddle Plant Competition Forecasting (Spring, 2020)

This project is all about modeling and forecasting the relative abundances of Deschampsia cespitosa (DECE), Geum rossii (GEROT), and Kobresia myosuroides (KOMY). 

### Repo Organization and Workflow

Here is an (initial) file organization system:

`00_raw_data`: a folder which contains raw data. Data are stored in folders, unzipped from files in the Niwot EDI portal. Each folder also contains files with metadata, including protocol notes. This folder also contains a readme file explaining what is in each dataset.

This folder, right now, is intended solely to house data.

`01_process_data`: a folder containing scripts to perform any data processing, cleaning, etc. Right now there's a folder called `scripts` and a folder called `output` - the names here indicate what's in the folders.

`02_fit_species_models`: a folder with scripts for modeling species-level models, e.g., individual mixed-effects models predicting species abundances for JSDMs.

