The following compiles monthly data from a variety of courses to estimate real-time economic conditions in Alberta. It closely follows, as much as the data allows, the Chicago Fed National Activity Index. https://www.chicagofed.org/publications/cfnai/index

There are three R files necessary to construct the Economic Conditions Index. First “core.R” loads packages, functions, themes, and other useful objects. Second, “ECI_data.R” downloads the processes the necessary data from various sources. Note that depending on the day this file is run, manual entry of data may be required (for example, wells drilled). Third, “ECI.R” estimates the first principle component from this data and generates the visualization.

![](https://raw.githubusercontent.com/trevortombe/alberta_eci/master/plot.png)
