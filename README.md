# NCViewer

## Goal 
## Motivation 
## Methods 

global.R   
- to see if the script is working properly    

ncviewer.R 
- a script for shiny apps    

cidp_edx.R   
- output file: for each demyelinating parameter of 2010 EFNS/PNS CIDP EDX criteria, count the number of nerves that fulfill the criteria   
- to identify whether there is any difference in the pattern of EDX criteria fulfillment between patients with CIDP and those who meet the EDX criteria for CIDP but have not CIDP           
- Note: there are some caveats in the output file. First, we did not exclude the median nerve in counting DML fulfillment. Second, we did not exclude the ulnar nerve across the elbow segment for CB. Third, we did not consider probable conduction block (if probable CB is considered, we should not count the posterior tibial nerve). Fourth, we did not exclude the wrist and Erb's point segment for definite partial conduction block (there should be no definite partial conduction block in this segment, only probable conduction block with 50% reduction criteria)  


input data should take a wide format    
- Hosp, Date, Name, ID, Sex, Age   
- 112 NCS parameters (values should be expressed as percentage values relative upper or lower limit of normal as appropriate)    


radial plot 
	parameter view (across different nerves)
	nerve view (across different parameters) 
	
spaghetti plot 




## Results 
## Discussion 
## References 


	
