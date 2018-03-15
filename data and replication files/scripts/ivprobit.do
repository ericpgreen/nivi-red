# runs from impact analysis

clear
use "/Users/ericgreen/Box Sync/SENSITIVE Folder epg4/Repositories/bitbucket/nivi-red/output/datStata.dta"
set more off

eststo: ivprobit anyFP_locf textit_locf age postSec discontinued marriedUnion nulligravida childrenEverBorn (tried=trt), twostep first

esttab est1 using "/Users/ericgreen/Box Sync/SENSITIVE Folder epg4/Repositories/bitbucket/nivi-red/output/stataout.csv", cells("b se t p ci") wide replace plain
