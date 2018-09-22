I ran some tests and the function appears to behave as expected, although there
are is one change in behavior I note below in 4.

I attach a patch to help illustrate the comments that follow:

1. Documentation should also be updated to reflect the correct chromatic
   adaptation.

2. Good catch on your #3 `$white` vs `$reference.white`; I missed that.

3. I believe the likely reason for the above bug is the inconsistency between
   `make.rgb` (@63) and `colorConverter` (@96).  Both create `colorConverter` S3
   objects, but the former does it with a `reference.white` element and the
   latter with a `white` element.  If `make.rgb` used `colorConverter` to
   instantiate its `colorConverter` object this inconsistency would have been
   avoided.  As it stands the inconsistency remains.

4. The bug fixed by #3 unfortunately caused incorrect conversions for color
   spaces with a non-"D65" white point.  Effectively, all RGB color spaces were
   implicitly assumed to be "D65".  Fortunately this only affects the "CIE RGB"
   color space, which I'm guessing is not broadly used.  It does mean that
   behavior of the function will change for "CIE RGB" and any other `make.rgb`
   user created color spaces with "non-D65" reference white points.
