*Friday* is an image processing library for Haskell. It has been designed to be
fast and generic.

Except for I/Os, *friday* is entirely written in Haskell.

![Header](header.png)

# Features

The library uses FFI calls to the *DevIL* image library (written in C) to read
images from a wide variety of formats, including BMP, JPG, PNG, GIF, ICO and
PSD.

The library currently support four color-spaces: RGB, RGBA, HSV and gray-scale
images. Images can be converted between these color-paces.

At this moment, the following features and algorithms have been implemented:

* various image transformations: resizing, cropping, vertical and horizontal
[flipping](http://en.wikipedia.org/wiki/Flopped_image) and
[flood fill](http://en.wikipedia.org/wiki/Flood_filling) ;

* filters:
[morphological transformations](http://en.wikipedia.org/wiki/Mathematical_morphology)
(dilation and erosion), blurring (mean
and [Gaussian](http://en.wikipedia.org/wiki/Gaussian_blur) blurs) and derivative
computation
([Sobel](http://en.wikipedia.org/wiki/Sobel_operator) and
[Scharr](http://en.wikipedia.org/wiki/Sobel_operator#Alternative_operators)
operators) ;

* support for mutable and masked images ;

* both adaptive and non-adaptive thresholding ;

* edge detection using
[Canny's algorithm](http://en.wikipedia.org/wiki/Canny_edge_detector) ;

* color histograms (including comparisons and image equalization).


# Quick tour

## Image representations

### Manifest and Delayed images

To benefit from Haskell's purity, non-mutable images are represented in two
ways:

* the `Manifest` representation stores images in Haskell `Vector`s ;

* the `Delayed` representation uses a function to produce image pixels. These
images can be efficiently chained to produce complex transformations. By some
inlining, Haskell compilers are able to produce fast algorithms by removing
intermediate structures.

As every function works with both representations, the 

See [this file](example/Delayed.hs) for an example a pipeline of delayed images.

# Examples

* [How to create a pipeline of delayed images](example/Delayed.hs) ;

* [How to detect edges using Canny's edge detector](example/Canny.hs) ;

* [How to apply a filter like a Gaussian blur](example/GaussianBlur.hs) ;

* [How to resize an image](example/ResizeImage.hs).

# Benchmarks


