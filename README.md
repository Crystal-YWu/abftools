# abftools

abftools is a package for reading and analysing abf data in R. The tasks are
splitted into three major parts: data acquisition, data processing and plotting.

## Overview

Currently, a few R packages provide loading of abf files, notably abf2 and readABF.

* abf2 by Matthew Caldwell provides functions to load gap-free ABF2 files and
basic plotting functions.

* readABF by Stanislav Syekirin provides a better support of loading both ABF and 
ABF2 files.

What makes abftools different to aforementioned packages are:

* abftools not only provides functions to load ABF2 files, but also a set of 
analysis and plotting functions that help streamline data processing of ABF2 data.

* abftools is designed to be data-first and performance-focused.

  * data-first: An abf object is designed to be a 3d array with dimensions of
  `abf[time, episode, channel]`, regardless of op mode be it episodic, gapfree
  or event driven variable length. This guarantees consistent subsetting across
  all abf objects and compatibility of R base functions and 3rd party functions.
  Compared to nested list with meta available immediately to userspace, getter
  and setter functions are provided to access attributes that are still relevant.
  
  * performance-focused: abf objects are usually large, and dealing with large 
  objects are slow. Thus calculation critical functions are implemented with 
  performance in mind and profiled/optimised to save even microseconds of CPU time.

## Task: Data Acquisition

Abf files can be loaded by calling `abf2_load()` and `abf2_loadlist()` (to load
multiple files). To facilitate easier batch loading by `abf2_loadlist()`, two
simple helper functions `SelectSample()` and `ExcludeSample()` are provided to
select desired files from an index file which contain a **filename** column and
multiple conditional columns. By uing syntax like
`SelectSample(df, cond1 = value1, cond1 = value2, ...)`, one can achieve simply
data index management without loading extra packages.

abftools currently does not support writing/saving an abf objects to abf files.
We do not believe writing abf2 file structure our top priority since it's proprietary
and not even documented. Please resort to other serialisation methods at the moment.

### Base data structure

abftools is built around the data class abf. An abf object is essentially a 3d
array with some optional attributes attached to it.

### Dimensions definition of abf objects.

An abf object has dimensions of **`c(time, episode, channel)`** since the logic  
of accessing data by channel, and processing data by episodes (sweeps) along time 
and such  dim arrangement can help improve memory performance. Notice that the 
dimensions are different to abf files stored on disk (which is `c(channel, time, episode)`),
so if data is loaded by other means it may not be compatible with abftools before
permutation.

When loading non-episodic abf or single channel files, the 3d structures are still
preserved i.e. indices are not dropped, so function calls are consistent for any
op mode.

In addition to using `dim(abf)`, you can also call `nPts()`, `nEpi()` and `nChan()`
to acquire the dimensions of an abf object and improve code readability.

### Subsetting abf objects.

You can subset an abf object by `abf[time, episode, channel]`, just like any array.
Please notice that subsetting an abf object consequently removes all attributes
assigned to it, making the returned values not an abf object any more.

An alternative way of subsetting is using `abf[[channel]]` to extract channel data
of interest. The differences between `abf[,, channel]` and `abf[[channel]]` are:

* You can subset multiple channels using `abf[,, channel]`, however only one using
`abf[[channel]]`. A warning is printed and only first channel will be extracted 
if multiple channels are supplied to [[.

* `abf[,, channel]` returns all episodic data while `abf[[channel]]` excludes those
are marked "removed" using RemoveEpisode().

* `abf[,, channel]` returns "raw" slices of the subset data (some indices may be 
dropped), while `abf[[channel]]` does some extra steps to maintain returned shape, 
and attach  proper colnames to the returned matrix by calling `DefaultEpiLabel()` 
for a clearer presentation.

### Attributes of abf objects.

In most cases, you don't need to access attributes of an abf objects since they
are managed by specific functions automatically. Here is a list of attributes
currently assigned to an abf objects:

* class: should always be "abf".

* title: title of the abf object, defaults to the file name when loaded. You can
access title by `GetTitle()` and `SetTitle()`.

* mode: op mode of the abf object, defined by voltage clamp protocol. Accesed by
`GetMode()` and you should not change this attr.

* ChannelName, ChannelUnit and ChannelDesc: channel names, units and descriptions.
These attributes are parsed from Strings section of an abf file. Accessed by
`GetChannelName()`, `GetChannelUnit()` and `GetChannelDesc()`, and can be set by
`SetChannelName()`, `SetChannelUnit()` and `SetChannelDesc()`

* SamplingInterval: the sampling frequency defined by voltage clamp protocol.
Accessed by `GetSamplingIntv(abf)`.

* EpiAvail: a vector maintained to record whether an episode is marked as "removed",
accessed/set by `GetAvailEpisodes()`, `RemoveEpisode()`, `RestoreEpisodes()`.

* meta: a list of raw (not parsed) properties of the abf files. Most of the meta
data is not maintained, however, meta$SynchArray is evaluated and maintained by
some functions, espeically those related to var-len mode.

## Task: Data Processing

As 3d data structures, when processing/analysing abf data the final goals are 
mostly "flatten" the object to some 2d forms, be it plotting or presenting statistics
that can be printed on paper. Data processing in abftools follows the logic of
map-reduce and is facilitated by functional programming, most notably the wrapper
function `wrap()`. The following example demonstrates wrapping commonly used
`mean()` to calculate averages of abf objects.

```r
f <- wrap(mean)

#f() now calculates mean I-V of an abf object
f(abf, intv = c(7000, 7200))
```

|       |      I (nA)   |      V (mV)|
|-------|--------------:|-----------:|
|epi1   |25918.6251  |47.636025|
|epi2   |19352.5715  |29.722206|
|epi3   |13000.3191  |12.142201|
|epi4   | 6907.2745  |-4.560712|
|epi5   |  863.5107 |-20.718752|
|epi6   |-5051.8364 |-36.100685|
|epi7  |-11433.0079 |-52.206389|
|epi8  |-18052.0749 |-68.457909|
|epi9  |-24555.7584 |-84.004401|
|epi10 |-31109.3744 |-99.271597|
|epi11 |-12021.4602 |-53.548504|

In this case, `wrap(mean)` maps function `mean()` to time domain of an abf object,
calculating average values of desired time interval for very corresponding episodes
and channels. Then reduces those values by as.data.frame().

### Built-in statistical functions

Here is a breif overview of some most used statistical functions provided in
abftools. For a full function list, please refer to package help.

* The frequently used `mean()`, `sd_abf()` and `sem_abf()` are provided which can 
be used directly on an abf object.

* `IVSummary()` calculates mean and sem of mean voltage/current channels of a list 
of abf objects and can be handy for repeat/replicate experiments.

* `CmpWaveform()` and `FindSamplingInterval()` find a representative time interval 
of an abf object. `CmpWaveform()` returns a list of intervals which the current/voltage  
record matches the waveform settings. `FindSamplingInterval()` has a more specific  
usage: it finds an time interval that the voltage channel matches waveform while  
the current channel is most stable, which can be useful for TEVC analysis.
Combining fast plotting methods `PeekChannel()`, `QuickPlot()` for quick inspection,
one can avoid time consuming manual cursor settings, especially for multiple files.

* `SampleAbf()` reduces data points of an abf object, a sampling function can also
be used to provide more flexible sampling.

### Low-level functions

In most cases, data processing of an abf object can be generalised as:

* Procedures that maintains time-episode-channel dimensions. This is implemented 
by `samplend()`.

* Procedures that "collapse" a specific dimension and results in a 2d object. 
This is implemented by `mapnd()`.

* Procedures that coerce a 3d structure to 2d form. This is implemented by 
`MeltAbf()`.

### Examples:

I-V plots of TEVC data:

```r

#Step 1. Load data.
alist <- abf2_loadlist(filelist)
#Step 2. Find representative time intervals.
ilist <- FindAllSamplingIntervals(alist)
#Step 3. Calculate IV summaries.
ivs <- IVSummray(alist, ilist)
#Step 4. Plot I-V chart
QuickPlot(ivs, smooth = TRUE)

```

Custom plotting using ggplot:

```r

channel_labels <- c("Current", "Voltage")
f <- wrap(max, epi_id_func = DefaultEpiLabel, chan_id_func = channel_labels,
          #add an abf id column to identify each abf object
          abf_id_func = GetTitle)
data <- lapply(abf_list, f)
ggplot(data.table::rbindlist(data), aes(Voltage, Current)) + geom_line(aes(colour = id))

```

## Plotting

Plotting of an abf object can be easily achieved with the ggplot2 package. Results
from wrapped functions (to plot channel vs channel) and `MeltAbf()` (to plot
channel vs time) are compatible with ggplot2.

Predefined `PlotChannel()` and `PlotAllChannel()` plot channel time series of an
abf object. `MultiPlotChannel()` arranges multiple channel time series of a list
of abf objects into a single plot. `QuickPlot()` provides a unified interface to
plot various types depending on supplied objects. Please refer to package help
for a full list of plotting functions.
