# NCViewer

  
wide, serial as default 

radial plot 
	parameter view (across different nerves)
	nerve view (across different parameters) 
	
spaghetti plot 
	


input data format 
	either long or wide 
	either single or multiple (serial from one patient or single studies from different patients) studies

reference values (default or custom) 

if long 
	- convert raw value into percentage value (relative to upper or lower limit of normal) 
	r2p.R
	- make long to wide format 
	long2wide.R

if wide 
