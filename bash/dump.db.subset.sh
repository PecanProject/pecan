#!/bin/bash

#Note: this script requires read 
DB="ebi_analysis"
PFT={$1:-"ebifarm.pavi"}
mysql -e "select specie_id from pfts_species join pfts on pfts_species.pft_id = pfts.id where pfts.name = 'ebifarm.pavi'" ebi_analysis
SPID=`mysql --raw  --skip-column-names -e "select specie_id from pfts_species join pfts on pfts_species.pft_id = pfts.id where pfts.name = 'ebifarm.pavi'" ebi_analysis `

## if traits given in command, use, otherwise, use given list
TRAITS={$2:-"('mort2', 'growth_resp_factor', 'leaf_turnover_rate', 'leaf_width', 'nonlocal_dispersal', 'fineroot2leaf', 'root_turnover_rate', 'seedling_mortality', 'stomatal_slope', 'quantum_efficiency', 'r_fract', 'root_respiration_rate', 'Vm_low_temp', 'SLA', 'Vcmax')"}


IGNORE="--ignore-table=$DB.counties --ignore-table=$DB.county_boundaries --ignore-table=$DB.county_paths --ignore-table=$DB.drop_me --ignore-table=$DB.error_logs --ignore-table=$DB.formats --ignore-table=$DB.inputs --ignore-table=$DB.inputs_runs --ignore-table=$DB.inputs_variables    --ignore-table=$DB.likelihoods --ignore-table=$DB.location_yields --ignore-table=$DB.managements --ignore-table=$DB.managements_treatments --ignore-table=$DB.mimetypes --ignore-table=$DB.models --ignore-table=$DB.plants --ignore-table=$DB.posteriors --ignore-table=$DB.posteriors_runs --ignore-table=$DB.runs --ignore-table=$DB.schema_migrations --ignore-table=$DB.users --ignore-table=$DB.visitors" 


table="trait"
    CONDITION="specie_id in ($SPID) and variable_id in (select id from variables where name in $TRAITS)" 
    mysqldump --where="$CONDITION" --lock-all-tables $IGNORE $DB ${table}s > ${table}s.sql


table="yield"
    CONDITION="specie_id in ($SPID)" 
    mysqldump --where="$CONDITION" --lock-all-tables $IGNORE $DB ${table}s > ${table}s.sql

# tables linked directly to traits
for table in site specie citation cultivar treatment variable
do
    CONDITION="id in (select ${table}_id from traits where specie_id in ($SPID) and variable_id in (select id from variables where name in $TRAITS))" 
    mysqldump --where="$CONDITION" --lock-all-tables $IGNORE $DB ${table}s > ${table}s.sql
done 

# lookup and auxillary tables
table="covariate" 
CONDITION="trait_id in (select id from traits where specie_id in ($SPID) and variable_id in (select id from variables where name in $TRAITS))" 
mysqldump --where="$CONDITION" --lock-all-tables $IGNORE $DB ${table}s > ${table}s.sql

table="pfts_specie" 
CONDITION="specie_id in ($SPID)"
mysqldump --where="$CONDITION" --lock-all-tables $IGNORE $DB ${table}s > ${table}s.sql

table="pfts_prior" 
CONDITION="pft_id in (select pft_id from pfts_species where specie_id in ($SPID));" 
mysqldump --where="$CONDITION" --lock-all-tables $IGNORE $DB ${table}s > ${table}s.sql

table="prior" 
CONDITION="id in (select prior_id from pfts_priors where pft_id in (select id from pfts where name = 'ebifarm.pavi'));" 
mysqldump --where="$CONDITION" --lock-all-tables $IGNORE $DB ${table}s > ${table}s.sql

table="pft" 
CONDITION="name in ('$PFT');" 
mysqldump --where="$CONDITION" --lock-all-tables $IGNORE $DB ${table}s > ${table}s.sql

table="yield" 
CONDITION="specie_id in ($SPID)" 
mysqldump --where="$CONDITION" --lock-all-tables $IGNORE $DB ${table}s > ${table}s.sql

# Acknowledgements: 
# Rolando from LogicWorks: http://dba.stackexchange.com/q/4654/1580