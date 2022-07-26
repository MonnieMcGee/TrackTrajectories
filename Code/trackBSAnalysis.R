# Code for bootstrap analysis for TAS track data paper
# March 9, 2022
# boostrap samples created from code in
# /Users/monniemcgee/Dropbox/2021Spring/Research/TrackData/Data

# Save bootstrap samples in a data frame
# Create table to display bootstrap intervals

# 200 m 
# bootstrap samples
boyBootSamps200m <- data.frame(diff1211=boySlope200,diff1110=boySlope200a,diff109=boySlope200b)
girlBootSamps200m <- data.frame(diff1211=girlSlope200,diff1110=girlSlope200a,diff109=girlSlope200b)
# Start here
# Boy
quantile(boySlope200,c(0.025,0.975))  # -0.4918656 -0.4609379 
quantile(boySlope200a,c(0.025,0.975)) # -0.5440148 -0.5182538 
quantile(boySlope200b,c(0.025,0.975)) # -0.9052172 -0.8689735 
# Girl
quantile(girlSlope200,c(0.025,0.975))  # -0.5495467 -0.4999354 
quantile(girlSlope200a,c(0.025,0.975)) # -0.5822523 -0.5411103 
quantile(girlSlope200b,c(0.025,0.975)) # -0.6012865 -0.5589128

#400m
# Boy
quantile(boySlope400,c(0.025,0.975))  # -1.224713 -1.131690  
quantile(boySlope400a,c(0.025,0.975)) # -1.389226 -1.297260 
quantile(boySlope400b,c(0.025,0.975)) # -2.303871 -2.197461
# Girl
quantile(girlSlope400,c(0.025,0.975))  # -1.543610 -1.372956 
quantile(girlSlope400a,c(0.025,0.975)) # -1.320272 -1.171745 
quantile(girlSlope400b,c(0.025,0.975)) # -1.297101 -1.153953

# 800m
# Boy
quantile(boySlope800,c(0.025,0.975))  # -2.621513 -2.384059  
quantile(boySlope800a,c(0.025,0.975)) # -3.866524 -3.622446  
quantile(boySlope800b,c(0.025,0.975)) # -5.714313 -5.404761

# Girl
quantile(girlSlope800,c(0.025,0.975))  # -3.878574 -3.456125 
quantile(girlSlope800a,c(0.025,0.975)) # -2.766901 -2.397414 
quantile(girlSlope800b,c(0.025,0.975)) # -3.096546 -2.692698

# 1600m
# Boy
quantile(boySlope1600,c(0.025,0.975))  # -7.017122 -6.578971 
quantile(boySlope1600a,c(0.025,0.975)) # -8.665006 -8.207267 
quantile(boySlope1600b,c(0.025,0.975)) # -12.60435 -12.08128  

# Girl
quantile(girlSlope1600,c(0.025,0.975))  # -9.561142 -8.801855 
quantile(girlSlope1600a,c(0.025,0.975)) # -6.389740 -5.693482 
quantile(girlSlope1600b,c(0.025,0.975)) # -5.379804 -4.666814

