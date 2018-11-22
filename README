# abftools

abftools is a package for reading and analysing abf data in R. The tasks are
splitted into three major parts: data acquisition, data processing and plotting.

## Base data structure

abftools is built around the data class abf. An abf object is essentially a 3-d
array with some optional properties attached to it.

### Dimensions of abf objects.

An abf object has dimensions of `c(time, episode, channel)`, though data recorded
in abf files actually has the dimensions of `c(channel, time, episode)`. The load
data is permutated since we usually access data channel by channel, and process
data by episodes, such dim arrangement can help improve memory performance.
When loading non-episodic abf file, the data is still represented in such 3-d
structures as if it is episodic but has only one episode to guarantee consistent
data subsetting convention. The same rule also applies to channel.

In addition to using `dim(abf)`, you can also call nPts, nEpi and nChan
(GetPointsPerEpisode/GetEpisodesPerChannel/GetNumOfChannel) to acquire the
dimensions of an abf objects. However, nPts/nEpi/nChan return values parsed
from the protocol of the abf objects. In some cases (mostly corrupted file), the
values from nPts/nEpi/nChan can be different to dim.

### Subsetting abf objects.

You can subset an abf object by `abf[time, episode, channel]`, just like any array.
Please notice that subsetting an abf object consequently removes all attributes
assigned to it, making the returned value not an abf object any more.

An alternative way of subsetting is using `abf[[channel]]` to extract channel data
of interest. The differences between `abf[,,channel]` and `abf[[channel]]` are:

* You can subset multiple channels using `abf[,,channel]`, however only one using
`abf[[channel]]`. A warning is printed and only first channel will be used if
multiple channels are supplied to [[.

* `abf[,,channel]` returns all episodic data while `abf[[channel]]` excludes those
marked "removed" using RemoveEpisode().

* `abf[,,channel]` returns "raw" slices of the subset data, while `abf[[channel]]`
does some extra steps to maintain returned shape, and attach proper colnames to the
returned matrix by calling DefaultEpiLabel() for a clearer presentation.

### Attributes of abf objects.

In most cases, you don't need to access attributes of an abf objects since they
are managed by specific functions automatically. Here is a list of attributes
currently assigned to an abf objects:

* class: should always be "abf".

* title: title of the abf object, defaults to the file name when loaded. You can
access title by `GetTitle(abf)` and `SetTitle(abf, "new title")`.

* mode: op mode of the abf object, defined by voltage clamp protocol. Accesed by
`GetMode(abf)` and you should not change this attr.

* ChannelName, ChannelUnit and ChannelDesc: channel names, units and descriptions.
These attributes are parsed from Strings section of an abf file. Accessed by
`GetChannelName(abf)`, `GetChannelUnit(abf)` and `GetChannelDesc(abf)`.

* SamplingInterval: the sampling frequency defined by voltage clamp protocol.
Accessed by `GetSamplingIntv(abf)`.

* EpiAvail: a vector maintained to record whether an episode is marked as "removed".
Accessed by `GetAvailEpisodes(abf)`, `RemoveEpisode(abf, episodes)`,
`RestoreEpisodes(abf, episodes)`.

* meta: a list of raw (not parsed) properties of the abf files. Accessed by
`GetProtocol(abf)`.

## Data Acquisition

Abf files can be simply loaded by calling abf2_load and abf2_loadlist (to load
multiple files).

Two simple helper functions SelectSample and ExcludeSample are also provided to
further simplify data management. You can maintain a data frame like structure
with file names and experiment conditions recorded in columns and use syntax like
`SelectSample(df, cond1 = val1, cond1 = val2, ...)` to batch select and load abf
files. `abf2_loadlist()` recognises filename/FileName column and can load abf files
recorded in such column.

abftools currently does not support writing/saving an abf objects to abf files.
We do not believe writing abf2 file structure our top priority since it's proprietary
and not even documented. Please resort to other serialisation methods such as
hdf etc. at the moment.

## Data Processing

### Low-level data accessment

Besides directly subsetting and assigning values of an abf object, a few functions
are provided so one can maintain the consistency of data structure and the attributes
of the abf object. These functions usually mimics a "by-ref" behaviour so that
you don't need an extra assignment. A corresponding "normal" function is always
available if such "by-ref" behaviour is not desired:

* MaskEpisodes (MskEpi for non by-ref): Replace a whole episode with given value.
You can't mask NA, since that doesn't make sense and you can use RemoveEpisode to
remove a whole episode.

* CropValue (non by-ref): crop values that are larger than max_value and smaller
than min_value from an abf object. This function can be handy when plotting.

* ReplaceChannel (RpclChan/[[<- for non by-ref): Replace a whole channel.

* AttachChannel (AtchChan for non by-ref): attach a new channel to an abf object.
attributes such as ChannelXXXX, meta$ADC are updated to maintain data consistency.
Please notice that RemoveChannel is not available since you can always exclude
a channel by not subsetting it.

* AverageAbf (non by-ref): calculate average of a list of abf objects. All attributes
are directly copied from the first element of the list.

* SampleAbf (SmplAbf for non by-ref): subsample an abf object by sampling_ratio
using sampling function. Attributes such as SamplingInterval, Protocol$lNumSamplesPerEpisode
etc. are updated to maintain data consistency.

### Statistical functions

The frequently used mean, sd_abf and sem_abf are provided which can be used directly
on an abf object and do corresponding function on each channel by episode. Other
functions can be easily implemented by using the powerful mapping function wrapper,
which will elaborate in later sections.

IVSummary which calculates mean and sem of mean voltage/current of a list of
abf objects can also be handy for repeat/replicate experiments.

Another commonly used feature is finding a representative interval of an abf object
CmpWaveform and FindSamplingInterval are provided for such purposes. CmpWaveform
returns a list of intervals which the current/voltage record matches the waveform
settings. FindSamplingInterval has a more specific usage. It finds an interval that
the voltage channel matches waveform and the current channel is most stable.
Combining fast plotting methods PeekXXXX for quick inspection, one can avoid time
consuming manual cursor settings, especially for multiple files.

This usually involves four steps:

```r

#Step 1. Load data.
alist <- abf2_loadlist(filelist)
#Step 2. Find those representative intervals.
ilist <- FindAllSamplingIntervals(alist)
#Step 3. Plot them for a quick inspection/sanity check.
print(MultiPeekChannel(alist, intv = ilist, channel = c(1, 2), num_label = TRUE))
#Step 4. Manual adjustment of improper intervals
SetIntv(ilist[[3]], 4500, 5000)
#Using noisy_data = TRUE could improve interval when the data is noisy.
ilist[[5]] <- FindSamplingIntervals(alist[[5]], noisy_data = TRUE)

#Following steps. Data processing, analysing, plotting over the intervals.

```

### Other data processing functions

Baseline calculation is available by calling BaselineEpoch or BaselineIntv, which
calculates baselines in an epoch or an interval respectively. Only asymmetric least
square smoothing (ALS) method (Paul H. C. Eilers, Hans F.M. Boelens, 2005) is
available since it's not the focus of abftools and it's only supposed to provided
rapid initial testing of data.

Likewise, peak detection (PeakDetectEpoch, PeakDetectIntv) and denoising
(DenoiseEpoch, DenoiseIntv) implemented in wmtsa package and wrapped with some
empirical parameters are also provided for quick processing. However, one should
always look for more sophisticated methods/packages for such purposes. Since
abf objects are essientially arrays, most available package should be compatible
and batch processing can also be achieved by combining existing algorithms with
mapping function wrappers.

### Mapping function wrapper

A mapping function is a function that takes input along specific dimension/axis
and is run along the whole object. One may already be familiar with lapply family
functions and notice that apply should work flawlessly with and abf object. However,
there are some drawbacks of using apply:

  * apply assumes FUN takes a single data variable as input.
  * `MARGIN = c(2,3)` may be less readable than `along = "time"`
  * Reusing apply may also reduce readability.

Some functions are provided in abftools in order to solve these problems:

* `PackArgs()` and `mapnd()`: PackArgs wrap a function that takes multiple input
argument to a function that takes a single vector as its input. `mapnd()` is
essentially `apply()` that is able to pack arguments using PackArgs. It also
accepts an along argument, which is just the opposite to MARGIN, and is usually
more readable than MARGIN when appling mapping functions along only one dimension.

* `WrapMappingFuncAlong()`/`wrap_along()`: Built on top PackArgs and mapnd, wrap_along
works specifically with abf objects. It wraps a mapping function to a function that
applies calculations on an abf object.

* `WrapMappingFunc()`/`warp()`: Built on top wrap_along(), wrap works specifically
along time axis of an abf object.

Assuming an arbitrary analytic function `three_chan()`, which does some calculations
using readings from three channels and two constant tao and k:

```r
three_chan <- function(ch1, ch2, ch3, tao, k) {

  ii <- 1 - exp(ch1/tao)
  jj <- 1 + exp((ch2 - ch3) / k)

  ii / jj
}
```

Now we want to apply this function to an list of abf objects alist, over different
intervals stored in ilist respectively. Each abf object in the list might even
have variable episodes removed.

```r

#wrap along channel ("c"), and pack arguments since three_chan accepts ch1, ch2,
#ch3 instead of a vector.
f <- wrap_along(three_chan, "c", pack_args = TRUE, tao = 2.0, k = 1.5)
#since lapply does not take two list
result <- purrr::map2(alist, ilist, f)

```

The following example calculates mean values of an abf object over a time interval.

```r

#wrap run along time axis by default, by setting epi_id and chan_id the returned
#value can set episode and channel names automatically.
f <- wrap(mean, epi_id_func = DefaultEpiLabel, chan_id_func = DefaultChanLabel)

m <- f(abf, c(5000, 7500))
print(m)

```

You may also notice that the returned values from a wrapped mapping function can
be easily row bound for plotting and consequent data processing.

```r

channel_labels <- c("Voltage", "Current")
f <- wrap(mean, epi_id_func = DefaultEpiLabel, chan_id_func = DefaultChanLabel,
          #add an abf id column to identify each abf object
          abf_id_func = GetTitle)
result <- lapply(abf_list, f)
ggplot(rbindlist(result), aes(Voltage, Current)) + geom_line(aes(colour = id))

```

## Plotting

Plotting of an abf object can be easily achieved with the ggplot2 package. Since
returned values from a wrapped mapping function is ready to plot by columns. In
addition, several helper functions are also provided for a smoother plotting
experience.

### Preparing data for plotting.

Besides using wrapped mapping functions, there are `melt()` (s3 function for abf
objects from reshape2 package) and `MeltAbfChannel()` for simple data preparing.

* `melt()`: This function melts a channel of an abf object to a data.frame, which
contains a time column as id var followed by a column of episodic data. By using
`melt()` you can plot an abf object in time series easily. You can also use
`sampling_ratio` argument to reduce data points and `sampling_func` to achieve
some basic data processing.

* `MeltAbfChannel()`: Unlike `melt()` treating data in time series perpective,
`MeltAbfChannel()` melts episodic data, by applying `map_func` on `intv`, so you
can plot an abf object's channel x vs channel y easily. In it's core, `MeltAbfChannel()`
is simply a wrapped `map_func` applied to `rbindlist`, with some additional
argument checkings.

### Ready to plot functions.

Multiple plotting functions are also provided so you can plot abf objects directly.
Those functions usually have PlotChannelXXXX, MultiPlotChannelXXX, QuickPlotXXXX
signatures. You can substitute Plot with Peek (except QuickPlot) for a sampled
fast plot since an abf object can have thousands or even millions of points.

All plotting functions accept similar arguments, and here are some most common ones:

* `intv`, `cursor`: intervals and cursors to add to a plot. An interval is simply
a vector of `c(start_pos, end_pos, length)` or can be simplified to `c(start_pos, end_pos)`.
A cursor is a vector of positions `c(pos1, pos2, pos3, pos4, ...)` that do not
indicate any time span but just the sampled moments.

* `colour`: a boolean value determines wheter to plot in coloured mode.

* `time_unit`: since data stored in an abf object is timed by "ticks" (array index
of dim 1), converting to an actual time unit can make a plot more sense. A time_unit
can be a character of tick, us, ms, s, min, hr.

* `auto_zoom`: when an interval or cursors are given, they defines an interval of
interest. Turning auto_zoom to TRUE will force the plot to zoom in to the interval
regardless of other values in tha abf objects. However, if `intv = NULL` and `cursor = NULL`,
auto_zoom won't work since it has no idea where to zoom in. In this case, you
can still use `CropValue()` to force remove maxima/minima for a better looking plot.
