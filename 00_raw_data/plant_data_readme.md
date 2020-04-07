# Data readme

This file will catalog all of the data in this folder. I've copied and modified it from the sandbox repo.

### vegetation_sampling

This contains vegetation data from the NWT saddle for various years between the 1990s and the 2010s, with the most frequent sampling in the 2010s. This has some grid-sampling, with plenty of records across time of Deschampsia, Geum, and Kobresia.

Downloaded by SN on 30 Jan, explored in the sandbox repo. Look at `data_sandbox/nwt_saddle_veg_EDA_sn.R`.

### n_p_expmt

Dataset from experiment conducted throughout most years in the 1990s looking at effects of nitrogen and/or phosphorus addition at the saddle. Conducted at a site near but distinct from the saddle grid. This looks at all species but in particular has a lot of records from our focal species (although inconsistencies in sampling near the end precludes observations of one species). This may be useful for looking at effect sizes of Nitrogen change. This was analyzed in a paper Bill and (Terry I think?) put out in Ecology around 2000, but that paper appears to be mostly community-level analysis. 

Downloaded by SN on 3 Feb, explored in the sandbox repo. Look at `data_sandbox/nwt_saddle_npexpmt_sn.R`.

### temp_old_greenland

Daily temperature data from one point on saddle, dating from 1981 to 2019. Includes minimum, maximum, and mean temperature each day. In the period 2010 - 2019 there are many NaNs. Measurements here were taken with the "Greenland 1987" method (?)

Downloaded by SN on 19 Mar, explored in March and April.

### temp_new_loggers

Daily temperature data, also from saddle, dating 2000 - 2019. Also has mean, min, max temperatures daily, as well as several other columns. These measurements were collected with data loggers. Statistical analysis should control for differences in methodology.

Downloaded by SN 6 Apr.
