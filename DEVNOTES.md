# Developer Notes ## Relevant docs

* ggplot2 docs `?Geom`, etc.
* ggplot2 vignette
* [B. Rudis in-progress Book](https://rud.is/books/creating-ggplot2-extensions/)

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
the training process, but can be overridden if a user sets limits.

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

## Waterfall

### stacking

Need to figure out what the generic stacking mechanism is for various inputs:

* `y`
* `y` + `height`
* `ymin` + `ymax`
* `ymin` + `ymax` + `height`
* `y` + `ymin` + `ymax`
* `y` + `ymin` + `ymax` + `height`

Let's take the case with `y` + `height`.  What is the desired outcome?  We want
the graphical elements to align on top of each other.  So as soon as there is
height there is the potential for an element to float off of the x-axis.  But
then it becomes difficult to distinguish what it means for the waterfall to go
up or down.  If not floating then it is obvious based on the sign of y.  If it
is floating then it becomes a lot trickier as the semantics change completely.

Alternate: If the entire "value" of our element is dictated purely by the `y`
aesthetic this simplifies things a bit.  How do we handle conflicts with
pre-existing `ymin` and `ymax` values?  We're going to hijack them, so if they
already exist it is a bit of a problem.  And should we also force `height` to
conform?  Seemingly no, both stack and dodge appear to operate purely by
modifying `xmin/xmax/ymin/ymax`.

So basically, we require a `y` value.  If `ymax/min` values exist we over-write
them with the adjustments, so height values will probably stop working?

Actually, more important than anything is not hijacking existing xmin/xmax
values, so we just need to apply the stacking purely based on `y` values, and
whatever happens happens.  The only sensible stacking is then on geoms that
don't explicitly specify `ymin`/`ymax` values, or if they do, where the `y`
is the same as `ymax` if it is positive, and the same as `ymin` if it is
negative.

### Dodging

This is actually a bit tricky for objects that come in with a width.  For
something like a bar where we don't explicitly define the width, it's
straightforward.  Same when the width are all the same.  It gets a lot harder
when we have different widths across different elements.  Probably need to use
the widest element as the target width, and then allocate relative to that.  If
we do "preserve" single, then use the same measure for all.

Some complications given how this is done currently where there is limited
flexibility for different widths, etc.  If everything is given widths to begin
with, we'll need to scale them all to fit within some width.  Annoying
scenarios:

* some widths are missing or ridiculous
* widths are not all the same, or all different
* need to communicate what the overall max width is, which means changing how
  setup params works right now since it runs before setup data
* reconciling dodging width with actual width

Width of elements at x value should be width of widest at that value?  But it is
possible for specific groups to have different widths at different x values?  Do
we just not support this?  Or only not support it in preserve mode?  Yes, I
think this is what we do.

Do we support missing `x`?  In theory we can do this with `x` + `width`, or
`xmin` + `xmax`.  I guess we should.  We need something that will dodge/adjust
any of `x`, `xmin`, `xmax` if they exist.

* Geom width is `xmax` - `xmin`.
* If there is no geom width, then use the dodge width.
* Preserve only works if all the widths are the same.
* If there is no dodge width, use the geom width.
* Do we just ignore the `width` aesthetic if present?  Yes, under the assumption
  that `xmin` and `xmax` will have been computed?  What if we don't have dodge
  width?  Do we take `width` aesthetic before default dodge?


## Ggplot Doc Issues

### ?Geom

Seems like a paragraph is out of place in ?Geom:

> Each of the ‘Geom’ objects is a ‘ggproto’ object, descended from the
> top-level ‘Geom’, and each implements various methods and fields. To create a
> new type of Geom object, you typically will want to implement one or more of
> the following:

And then there is another paragraph before the bullet point list.  There is
also conflict between "implement" and "override" in the first bullet point.

## Gotcha's

You need to generate your first set of docs before you use any of the `ggproto`
calls because those are evaluated at package load time, and will break
documentation (and NAMESPACE) generation.

## Random Ggplot Analysis Notes

Need to check where `draw_panel` is called. AFAICT this is through the
`draw_layer` method of `Geom`, which invokes it as:

```
    args <- c(list(quote(data), quote(panel_params), quote(coord)), params)
    plyr::dlply(data, "PANEL", function(data) {
      if (empty(data)) return(zeroGrob())

      panel_params <- layout$panel_params[[data$PANEL[1]]]
      do.call(self$draw_panel, args)
    }, .drop = FALSE)
```

Since the arguments are not named, it is quite remarkable this works.  Not sure
how it is possible for this to work with the different signatures.

Actually, by the looks of it `params` in the composition of `args` can include a
named `self`.  This from a traceback of `geom_rect`:

```
[17]]
do.call(self$draw_panel, args)

[[18]]
(function (...)
f(..., self = self))(data, panel_params, coord)

[[19]]
f(..., self = self)
```

Ah, and it looks that what's happening is that a `ggproto` method is not just
the function you define, but a processed version of it that looks like:

```
<ggproto method>
  <Wrapper function>
    function (...)
f(..., self = self)

  <Inner function (f)>
    function (self, data, panel_params, coord)
{
```

This should mean that `self` is provided if it isn't otherwise, but if you do
provide somehow in a direct call to `draw_panel` it then you must do so with a
`self=self` form as otherwise it gets captured by the dots.

Actually, for `geom_point` which doesn't have a `self` param, the call doesn't
include `self`:

```
 $ : language do.call(self$draw_panel, args)
 $ : language (function (...)  f(...))(data, panel_params, coord, na.rm = FALSE)
 $ : language f(...)
```

Alright, looks like there is automated parameter trimming going on, which is why
`GeomPoint` doesn't need `self`:

```
params <- params[intersect(names(params), self$parameters())]
```
