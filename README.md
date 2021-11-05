# Microplatic emission from fishing activity
Data and R code accompanying articles "Potential microplastic release from the maritime industry: abrasion of rope" in *Science of the Total Environment* and "Potential microplastic release from beached fishing gear in Great Britain’s region of highest fishing litter density" in *Marine Pollution Bulletin*.

The repository is split into two folders: **STOTEN** and **MPB**. The former contains all files to perform the statitsical analysis required from the *Science of the Total Environment* article. The latter contains all files to perform the statitsical analysis required from the *Marine Pollution Bulletin* article. Below is a description of each file within those folders.

**STOTEN**
`fragments.csv`|`fragments.R`: Microplatic emission from the abrasion of rope.
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

Luka Seamus Wright, 4 September 2021
