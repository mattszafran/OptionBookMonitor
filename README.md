# OptionBookMonitor

Instructions: 

1. ) Download the folder, and place it on your desktop. This is the recommended way, since it will mean the relative paths to the positioning files will remain the same as in my build. If you want to keep the folder elsewhere, go into the ```exec.R``` file and modify the ```setwd()``` line to the correct path to the folder. Then, do the same in the ```globalOptions``` function found on line 18 of ```helperFunctions.R```. 
2. ) **VERY IMPORTANT, ONLY DO THIS THE FIRST TIME** launch Terminal. Type ```R``` and hit enter. Then, enter the following command.  
```install.packages("quantmod")```
3. ) Input your positions in the equity book and options book finals. Use the option identifier codes in the same format as provided. Note that the columns **ARE** case sensitive for any text input, so be wary of that. 
4. ) Open up Terminal (Mac) or cmd.exe (Win) and type ```Rscript``` then drag the exec.R file into the window, and hit enter. Calculation time will vary based on how strong your internet connection is (it pulls down a bunch of data on current prices). 


For a simpler version, a video of me running the file (with the default example book) is also included. 
