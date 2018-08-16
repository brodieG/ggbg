# Developer Notes

## Next Steps

0. The ggplot hue palette made equidistant?
1. Improved version of JET or other that maximizes dynamic range with monotonic
   brightness / saturation?
2. Discussion of benefits of monotonic brightness / saturation?
3. Limitations of brightness based color maps, particularly their susceptibility
   to neighboring brightness issues (i.e. I find I cannot actually use the
   legends in any meaningful way with e.g. viridis in volcano)

## Brightness / Saturation

It seems clear that monotonic brightness, and possibly monotonic saturation
changes are fairly desirable, mostly because at least I do seem to have an
explicit association of value to those properties.

## Relevant docs

* ggplot2 docs `?Geom`, etc.
* ggplot2 vignette
* [B. Rudis in-progress Book](https://rud.is/books/creating-ggplot2-extensions/)
* [Matplotlib colormap](https://bids.github.io/colormap/)
* [CIECAM02](https://en.wikipedia.org/wiki/CIECAM02), the perceptual color
  space(?) function used to make things equal?
* [Source code for CIECAM02 transformations](http://scanline.ca/ciecam02/) and
  very interesting notes on the background effect.

## CIEDE vs CIECAM02

From [RIT FAQ](https://www.rit.edu/cos/colorscience/rc_faq_faq2.php#363):

> Would CIECAM02 or CIEDE2000 be better at predicting spot color differences?
> (363) Most of the research on predicting color differences has been aimed at
> optimizing equations in the CIELAB color space (such as the DE94 and DE2000
> equations). Similar work is being done within the CIECAM02 color space, but
> there is not yet a recommended technique for performing the computations.
> There is certainly reason to believe that one will be developed in the future.
> For now, and for the application you mention, the CIEDE2000 equations would be
> the recommended technique. For a simpler equation, that is likely to perform
> just about as well, you might want to consider the CIE DE94 equations. (Back
> to top)

## CIEDE questions

What exactly do the additional parameters do?  Could the improve the lightness
issues.

## Scale

### Gamma correction

Do we need to apply it the way swatches does (although at least on my system
whatever `gamma=TRUE` does is promptly undone by `as(, "LAB")`.

```
identical(
  as(colorspace::hex2RGB(rainbow(5), gamma=FALSE), "LAB"),
  as(colorspace::hex2RGB(rainbow(5), gamma=TRUE), "LAB")
)
```

### Literature

* Pals package has lots of useful tools and in the overview vignette details of
  goodness and badness of color scales?
* Colin Fay [viridis
  post](https://rtask.thinkr.fr/blog/ggplot2-welcome-viridis/)

### Re-use?

One question is whether this should be built on top of `scale_fill_gradientn` or
with its own dedicated `ggproto` object.  In favor of the latter is to easily
get access to the training data for the whole set.

Probably closest basis is `scale_*_gradientn`, which uses
`continuous_scale` with `gradient_n_pal` as the `palette` parameter.  It seems
the way the `palette` parameter is used is by requesting a specific number of
colors (or whatevers) as per `?continuous_scale`, but then not at all clear how
we can have non-linear mappings.

Actually the `palette` business is probably a documentation problem?  Clearly
`gradient_n_pal` does not meet the description, nor does `div_gradient_pal`.
Correct definition should be:

> A function that when called with a number between 0 and 1 returns a color
> code.

The problem might be that the docs in `?scale_fill_gradientn` and similar:

> A palette function that when called with a single integer argument (the number
> of levels in the scale) returns the values that they should take

Is really intended for `discrete_scale` where that might make more sense and
indeed where `hue_pal` is used to generate functions that meet that description.

ToDo: examine the @inheritDotArgs and - roxygen shortcuts.

So, given that our function will be given a number between 0 and 1, we now have
to figure out how to make sure our "anchors", etc., convey.

Looks like the mapping is governed by this:

```{r}
  map = function(self, x, limits = self$get_limits()) {
    ## x should have values in 0-1, as well as NAs
    x <- self$rescaler(self$oob(x, range = limits), from = limits)

    uniq <- unique(x)
    pal <- self$palette(uniq)     # a color for each unique value
    scaled <- pal[match(x, uniq)]

    ifelse(!is.na(scaled), scaled, self$na.value)
  },
```

So the scaling is linear.  I "think" this works properly so long as we generate
the `gradient_n_pal` function with the correct values scaled in the same way the
rescaler does here?

Are there problems with `self$get_limits`?  The underlying value is populated by
the training process and retrieved with `self$range$range`, but if the user
specifies explicit limits that will be overridden.

So if we want to use a different type of `range` that contains all of the
training data, and not just the min max, we have to override `self$map`
completely, and also `self$train`, and then we can record data in something
other than `range`.  We can also populate `range` the normal way to avoid
potential issues in case any other methods actually use `range`.

Older notes:

So one gut-feel issue is that normal course of business is to  generate our
'palette' function at spec time, but really we need to generate it at map time
once we have the totality of the data.  We can then, knowing the limits, do so
in `Scale$map`?  Well, one issue is that this only works if we have the entirety
of the data, so we'll have to modify `Scale$range` to be an object that records
all of the data, not just the extremes.  So we'll need a new `Range` object with
that field.

One issue is that those objects are not exported, but maybe we do different
objects, and calling them something other than `Range`?  We lose the semantics
of `self$range$train()`, but that's probably okay.

### Interface

What do we need:

* Input colors
* Anchor points? (these will need to be part of the training set)
* Should not have to provide values as those should be

### Color Palette Generation

Need to do some research on what the right way to generate a semi-reasonable
colorful scale is.  Some ideas were to maybe get the baseline hues, normalize
them to the same brightness and saturation.  And then modulate the brightness
and saturation up as we move away from the origin.  Not sure how well that will
work with saturation (or maybe chroma?) versus brightness.

