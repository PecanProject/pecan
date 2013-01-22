set -x

# copy database and load locally
ssh kooper@ebi-forecast.igb.illinois.edu "mysqldump --lock-tables=false ZZZZ -u YYYY -pXXXX" > betydump.sql
mysql -u bety -pbety -e 'DROP DATABASE IF EXISTS betydump; CREATE DATABASE betydump'
grep -v "DEFINER" betydump.sql | mysql -f -u bety -pbety betydump

# remove all users
mysql -u bety -pbety betydump -e 'update users set login=CONCAT("user", id), name=CONCAT("user ", id), email=CONCAT("betydb+", id, "@gmail.com"), city="Urbana, IL", country="USA", field=NULL, created_at=NOW(), updated_at=NOW(), crypted_password="!", salt="!", remember_token=NULL, remember_token_expires_at=NULL, access_level=3, page_access_level=3, apikey=NULL, state_prov=NULL, postal_code=NULL;'
mysql -u bety -pbety betydump -e 'update users set access_level=1, page_access_level=1 where id=1;'

# mark data as secret (#1540)
mysql -u bety -pbety betydump -e "update traits set access_level = 1 where user_id = 15 and year(created_at) > "2012" or (year(created_at) = 2012 and month(created_at) > 5);" 
mysql -u bety -pbety betydump -e "update traits join citations on traits.citation_id = citations.id join variables on traits.variable_id = variables.id set access_level = 1 where author in ('McCreary','Crookshanks','Walters','Kelting','Rakonczay','Mata','Voigt','Steinbeck','Boyer','Carpenter','Szaniawski','McDowell','Fahey','Zogg','Sowell','Tripepi','Burton','Pregitzer','Bouma','Conlin','George','Cox','Lahde','Johnson-Flanagan','Lafond','Allen','Cropper','Marshall','BassiriRad','Ryan','Clinton','Janssens','Barnard','Drew','Edwards') and variables.name = 'root_respiration_rate';" 
mysql -u bety -pbety betydump -e 'update traits join species on traits.specie_id = species.id set access_level = 1 where genus = "Saccharum";'
mysql -u bety -pbety betydump -e 'update yields join species on yields.specie_id = species.id set access_level = 1 where genus = "Saccharum";'

# remove all non checked data
mysql -u bety -pbety betydump -e 'delete from traits where checked = -1;'
mysql -u bety -pbety betydump -e 'delete from yields where checked = -1;'

# remove all secret data
mysql -u bety -pbety betydump -e 'delete from traits where access_level < 3;'
mysql -u bety -pbety betydump -e 'delete from yields where access_level < 3;'

# fix ebifarm (#1541)
mysql -u bety -pbety betydump -e 'update inputs set site_id=76 where site_id=610;'

mysql -u bety -pbety betydump -e 'update inputs set name="ED_MET_DRIVER_HEADER", start_date="2004-01-01 00:00:00", end_date="2009-12-31 23:59:59" where id=7;'
mysql -u bety -pbety betydump -e 'update inputs set name="EBI Energy farm site" where id=5;'

# apply fix from 1521 until database on ebi-forecast is migrated
echo "delete from schema_migrations where version <> 1;

alter table species drop column FederalNoxiousStatus;
alter table species drop column FederalNoxiousCommonName;
alter table species drop column StateNoxiousStatus;
alter table species drop column StateNoxiousCommonName;
alter table species drop column Invasive;
alter table species drop column Federal_TE_Status;
alter table species drop column State_TE_Status;
alter table species drop column State_TE_Common_Name;
alter table species drop column FlowerColor;
alter table species drop column FlowerConspicuous;
alter table species drop column FoliageColor;
alter table species drop column FoliagePorositySummer;
alter table species drop column FoliagePorosityWinter;
alter table species drop column FruitColor;
alter table species drop column FruitConspicuous;
alter table species drop column Shape_and_Orientation;
alter table species drop column Toxicity;
alter table species drop column FruitSeedAbundance;
alter table species drop column FruitSeedPersistence;
alter table species drop column SmallGrain;
alter table species drop column VegetativeSpreadRate;
alter table species drop column Berry_Nut_Seed_Product;
alter table species drop column ChristmasTreeProduct;
alter table species drop column FodderProduct;
alter table species drop column FuelwoodProduct;
alter table species drop column LumberProduct;
alter table species drop column NavalStoreProduct;
alter table species drop column NurseryStockProduct;
alter table species drop column PalatableBrowseAnimal;
alter table species drop column PalatableGrazeAnimal;
alter table species drop column PalatableHuman;
alter table species drop column PostProduct;
alter table species drop column ProteinPotential;
alter table species drop column PulpwoodProduct;
alter table species drop column VeneerProduct;" | mysql -u bety -pbety betydump

# dump database and copy to isda
mysqldump -u bety -pbety betydump > betydump.sql
cp betydump.sql /mnt/isda/kooper/public_html/EBI/betydump.sql