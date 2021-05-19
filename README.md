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

The latest ECI for Feb 2021 is -1.15. The full time series since Jan
2002 is plotted below:

<iframe title="Alberta Economic Conditions Index to Feb 2021" aria-label="Interactive area chart" id="datawrapper-chart-fIIKi" src="https://datawrapper.dwcdn.net/fIIKi/5/" scrolling="no" frameborder="0" style="width: 0; min-width: 100% !important; border: none;" height="500"></iframe><script type="text/javascript">!function(){"use strict";window.addEventListener("message",(function(a){if(void 0!==a.data["datawrapper-height"])for(var e in a.data["datawrapper-height"]){var t=document.getElementById("datawrapper-chart-"+e)||document.querySelector("iframe[src*='"+e+"']");t&&(t.style.height=a.data["datawrapper-height"][e]+"px")}}))}();
</script>

![Alberta ECI](plot.png)

------------------------------------------------------------------------

The index was last updated on 2021-05-03 16:00:34 MDT.
