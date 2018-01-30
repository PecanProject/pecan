## Site and model selection

This page is used to select the model to run and the site at which you would like to run that model.

**NOTE:** If this page does not load for you, it may be related to a known Google Maps API key issue. See [issue #1269][issue-1269] for a possible solution.


[issue-1269]: https://github.com/PecanProject/pecan/issues/1269

### Selecting a model

1. On the **Select Host** webpage use the Host pull-down menu to select the server you want to run on. Note that at the moment only the LOCAL server will run through the web-based workflow.

2. Next, select the model you want to run under the Model pull-down menu

3. If the model you want to run is not listed login to BETY and navigate to Runs > Models.

4. If there are already entries for the model you want to run then most likely the PEcAn modules for that model have been implemented but PEcAn is unaware that the model has been installed on your server. Information on adding a model executable to the PEcAn database can be found [here](../../developers_guide/Adding-an-Ecosystem-Model.md#model). After this is done, your model should appear on the PEcAn **Select Host** page after your refresh the page.

5. If there a no entries for the model you want to run then most likely the PEcAn modules need to be implemented. See the instructions at Adding an Ecosystem Model

6. If selecting your model causes your site to disappear from the Google Map then the site exists but there are no drivers for that site registered in the database. See the instruction [here](Choose-a-site.md#my-site-shows-up-when-i-dont-have-any-model-selected-but-disappears-once-i-select-the-model-i-want-to-run) for more info on diagnosing what drivers are missing.

### Selecting a site

#### Using existing sites

1. **Find the site on the map** The simplest way of determining if a site exists in PEcAn is through the Google Map interface of the web-based workflow. You'll want to make sure that the Host is set to All Sites and the Model is set to All Models. 

2. **Find the site in BETY** If the site is not on the map there's a chance that it still in PEcAn but just lacking geographic information. To begin you'll want to login to your local version of the BETY database. If you're on the PEcAn VM this will be at localhost:3280/bety or localhost:6480/bety depending on whether you downloaded the 32 or 64 bit version.  Within BETY navigate to Data > Sites and use the Search window on the page to try and locate your site. If you **DO** find your site you will want to click Edit and add geographic information so that the site will show up on the map. It is also worth noting that the site ID number shows up in the URL for the Show or Edit pages. This ID is frequently useful to know, for example if you have to set up a PEcAn settings file by hand. If you did not find you site you will want to follow the instructions below for adding a site

#### Adding a new site

(TODO: Move most of this out)

1. To add a new site to PEcAn you currently have to begin by logging in to BETY

2. Navigate to Data > Citations. Before you add a Site you will need to establish a Citation to associate with the site. If you went to Data > Sites and just clicked on **Add Site** it will take you to an error page reminding you of this.

3. Before adding a new citation, you should Search the system to make sure the Citation you are about to add doesn't already exist. If it does you should be able to just click on the check-box icon to select that citation and then skip the next step

4. To create a new citation click on the **New Citation** button, fill in the fields, and then click Create. The field URL should contain the web address that takes you to this publication on the publisher's website. The Pdf field should similarly be the web address for the pdf for this citation.  If there are no publications associated with the site you are interested in you can leave most of the fields blank, but you could add a descriptive title, such as "EBI Farm Field Data", and a relevant contact person as the Author so that future users can associate any site-specific data with this citation.

5. Once the Citation is created or selected this should automatically take you to the Sites page and list any Sites already associated with this citation. To create a new site click the **New Site** button.

6. When creating a new cite the most critical information is the **Site name** and the Lat/Lon. The Lat/Lon can be entered by hand or by clicking on the site location on the Google Map interface. It is also helpful to fill in the other location information (Cite, State, Country) and the Notes to make it easier for other users to search for the site. The topographic, climate, and soils information is optional.

7. When you are done click **Create**. At this point if you refresh the PEcAn site-level run webpage the site should automatically show up.

#### Troubleshooting

##### My site shows up when I don't have any model selected, but disappears once I select the model I want to run

Selecting a model will cause PEcAn to filter the available sites based on whether they possess the required Inputs for a given model (e.g. meteorology). To check what Inputs are missing for a site point your browser to the pecan/checksite.php webpage (e.g. localhost:3280/pecan/checksite.php). This page looks virtually identical to the site selection page, except that it has a *Check* button instead of *Prev* and *Next*. If you select a Machine, Model, and Site and then click *Check* the page should return a list of what Inputs are missing (listing both the name and the Format ID number). Don't forget that its possible for PEcAn to have required Inputs in its database, but just not have them for the Machine where you want to run.

To see more about what Inputs a given model can accept, and which of those are required, take a look at the MODEL_TYPE table entry in the database (e.g. go to localhost:3280/bety; Select Runs > Model Type; and then click on the model you want to run).

For information about loading missing Inputs into the database visit [How to insert new Input data](/../../How-to-insert-new-Input-data.md), and also read the rest of the pages under this section, which will provide important information about the specific classes of Inputs (e.g. meteorology, vegetation, etc).

Finally, we are in the process of developing workflows and standards for processing Input data in a model-agnostic way, starting with meteorology. So hopefully much of the work of building model driver files and uploading them into PEcAn will become automatic soon.
