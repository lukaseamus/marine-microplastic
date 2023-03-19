# Microplatic emission from fishing activity
Data and R code accompanying articles "Potential microplastic release from the maritime industry: abrasion of rope" in *Science of the Total Environment* (doi: 10.1016/j.scitotenv.2021.150155) and "Potential microplastic release from beached fishing gear in Great Britain’s region of highest fishing litter density" in *Marine Pollution Bulletin* (doi: 10.1016/j.marpolbul.2021.113115).

The repository is split into two folders: **STOTEN** and **MPB**. The former contains all files to perform the statitsical analysis required from the *Science of the Total Environment* article. The latter contains all files to perform the statitsical analysis required from the *Marine Pollution Bulletin* article. Below is a description of each file within those folders. Additionally, the published `Manuscript.pdf` and `Supplement.pdf` are provided.

**STOTEN**
1. `fragments.csv`|`fragments.R`: Microplatic emission from the abrasion of rope.
* *rope* = rope category for the main analysis
* *code* = rope ID
* *age* = rope age given in years
* *polymer* = rope material
* *diameter* = rope diameter given in millimetres
* *use* = previous use of rope
* *mass* = mass of microplastic released after 50 one-metre hauls of a 2.5-kilogram weight given in grams
* *density* = density of rope material given in grams per cubic centimetre
* *volume* = total volume of microplastic fragments released after 50 one-metre hauls of a 2.5-kilogram weight given in cubic centimetres (derived from *mass* and *density*)
* *fragment.volume* = mean volume of microplastic fragments released after 50 one-metre hauls of a 2.5-kilogram weight given in cubic centimetres
* *fragments* = number of microplatic fragments released after 50 one-metre hauls of a 2.5-kilogram weight (derived from *volume* and *fragment.volume*)

**MPB**
1. `seas.csv`|`coordiantes.csv`|`ALDFG.R`: Site map.
* *sea* = regional sea ID
* *lat* = latitude
* *long* = longitude
* *litter* = ALDFG items per person per metre per day (doi: 10.1016/j.scitotenv.2016.11.137)
* *site* = study beach ID

2. `count.csv`|`ALDFG.R`: Eulittoral abundance of beached ALDFG.
* *region* = north or south coast of the Southwest Peninsula
* *site* = study beach ID
* *type* = ALDFG category
* *count* = abundance per 100-m stretch of beach

3. `dimensions.csv`|`ALDFG.R`: ALDFG dimensions.
* *region* = north or south coast of the Southwest Peninsula
* *site* = study beach ID
* *type* = ALDFG category
* *length* = length in millimetres
* *width* = width in millimetres

4. `dimensions.csv`|`ALDFG.R`: ALDFG dimensions.
* *region* = north or south coast of the Southwest Peninsula
* *site* = study beach ID
* *type* = ALDFG category
* *filaments* = number of filaments

Luka Seamus Wright, 5 November 2021
