Constructs Monthly Index of Economic Conditions in Alberta
==========================================================

The following compiles monthly data from a variety of courses to
estimate real-time economic conditions in Alberta. It closely follows,
as much as the data allows, the Chicago Fed National Activity Index.
<a href="https://www.chicagofed.org/publications/cfnai/index" class="uri">https://www.chicagofed.org/publications/cfnai/index</a>

There are three R files necessary to construct the Economic Conditions
Index. First “core.R” loads packages, functions, themes, and other
useful objects. Second, “ECI\_data.R” downloads the processes the
necessary data from various sources. Note that depending on the day this
file is run, manual entry of data may be required (for example, wells
drilled). Third, “ECI.R” estimates the first principle component from
this data and generates the visualization.

The end result is:

![Alberta ECI](plot.png)

The latest ECI for Jan 2021 is -1.4895048.
