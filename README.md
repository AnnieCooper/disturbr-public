# disturbr: methods for detecting and attributing forest disturbances in R

Disturbr uses raster time series to locate and attribute disturbances. Using cleaned raster time series (e.g., from Landsat), various variables are created for each pixel that describe time series characteristics (e.g., disturbance magnitude, duration, average slope of the raw time series). For a full list of variables, see the supplementary information included in the disturbr manuscript. Variables are also included that summarize the area of neighboring pixels (e.g., average magnitude of neighboring disturbed pixels, average slope of neighboring pixel time series). Random forest models are then fit using all variables for either detection or attribution. 

This algorithm was designed for detecting and attributing forest disturbances, but may be used for alternative purposes as it requires only a masking raster (e.g., forest cover), a data raster brick, and training data on disturbance locations.

A training algorithm is included in these methods that is based off of the TimeSync algorithm. However, using a ready-made training dataset would make the process of applying disturbr much easier.

The general steps included in the methods as described in the associated manuscript are below.

### Steps:
1a. Prepare and download data using Earth Engine. 

Summertime (June 1 - August 31) Landsat 5 and 7 NDVI data are filtered and summarized (note that Tasseled Cap Wetness data are also available for download). We use only clear data (no clouds, errors) and take the mean summertime TCW value. Data are exported to the Google Drive folder of the Earth Engine user. Current code can me found at https://code.earthengine.google.com/bb3a5354de72915b6ee26affb3b5dfaf.

1b. Download data from Google Drive, mosaic index raster stack.

Data are downloaded from the Google Drive where they were exported. The export often results in multiple files for the main index image (e.g., TCW, NDVI). These files need to be mosaiced together using GDAL once downloaded to a local or HPCC folder. See *gdalmerge* documentation for more information if necessary.

2. Create basic image data in R.

Here, we break the larger images into blocks if necessary, and create the basic csv containing the coordinates of each pixel. This will later be used to list variables (e.g., magnitude, duration) for each location within the image for input to the random forest models.

3. Train data for use in the detection model.

Using the 'step3_classify_training_pts.R' script, train randomly-selected points from specific blocks within each Landsat scene. This script runs an interactive function that shows users a time series of rasters showing the area around the randomly-selected pixel, then a recent Google Earth image of the area, and finally a time series of the pixel values. Users then enter the condition of the pixel. In this analysis, we set the pixel value to values between 0 and 9, as defined below.

+ 0 = remove (bad for training)
+ 1 = no apparent dist (or false positive for attribution)
+ 2 = biotic
+ 3 = fire
+ 4 = harvest
+ 5 = wind
+ 6 = ice
+ 7 = flooding
+ 8 = land use change (e.g., construction, mining)
+ 9 = landslide, avalanche

4. Create a dataframe of single-pixel and neighboring-pixel time series variables (detection).

Use the 'step4_add_data_to_img_pts.R' script to add time series variables for all pixels to a single dataframe. Variables are calculated both for single pixels and neighboring pixel groups. 64 pixels are included in the neighboring pixel calculations (240 x 240m window) for this analysis. 

5. Create and apply the Random Forest detection model.

Run the 'step5_train_model.R' script to create a Random Forest detection model and apply it to create a prediction dataframe as well as rasters of predicted disturbance locations and year.

6. Train data for use in the attribution model.

This process is the same as described in step 3. However, for this analysis we chose to not create an entirely new training dataset and rather to combine some new points with those already identified as disturbed in the original detection training dataset. As such, the code for this step is located in the 'step6_attribute_training_pts.R' script.

7. Create a dataframe of single-pixel and neighboring-pixel time series variables (attribution).

Use the 'step7_add_data_to_img_pts.R' script. Add variables to the dataframe created in step 4 that describe characteristics of disturbed pixels in the neighboring-pixel area. 

8. Create and apply the Random Forest attribution model.

Run the 'step8_train_att_model.R' script to create a Random Forest attribution model and apply it to create a prediction dataframe as well as rasters of predicted disturbance type and year.
