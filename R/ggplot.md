## Background

- R is loose about args being positional or keyword. I'm going to use the
  keyword versions here because I think it is clearer
- R allows you to overload built-in operators (obvs not advised etc. etc.)
    ```r
    rm('+')
    '+' <- function(x,y) paste(x,y,sep="")
    ```
- ggplot2 overloads `+` for combining objects of types it recognises.
    - `+` functions as a sort of "merge" function
- Potential gotcha: If you give `ggplot` data that is not a `data.frame` it will
  coerce it to a `data.frame` via the `fortify()` function.

## ggplot()

- initializes a plot
- Returns an S3 object which has classes `gg`, `ggplot`
- Takes a data frame and an optional aesthetic mapping
    - aesthetic mappings can also be set on the individual layers
    - `ggplot` doesn't actually do anything with the data or aesthetics itself -
      it just makes them available for layers to (optionally) inherit them.

```r
my_plot <- ggplot(data = somedata, mapping = aes(...))
```

```yaml
# anatomy of a ggplot object
ggplot_S3_object:
    s3_classes: gg, ggplot

    data: # a reference to the data frame arg you passed in
    mapping: # a reference to the mapping arg you passed in

    # these attributes are filled in by functions that you merge into this
    # object with `+`
    layers:
        - geom: NULL
          stat: NULL
          data: NULL
          mapping: NULL
          position: NULL
          params: list()
          inherit.aes: TRUE
          check.aes: TRUE
          check.param: TRUE
          show.legend: NA
          key_glyph: NULL
          layer_class: Layer
        - geom: NULL
          stat: NULL
          data: NULL
          mapping: NULL
          position: NULL
          params: list()
          inherit.aes: TRUE
          check.aes: TRUE
          check.param: TRUE
          show.legend: NA
          key_glyph: NULL
          layer_class: Layer
    scales:
    coordinates:
    facet:
    plot_env:
    labels:
    theme:
```

## aes()

- Build an aesthetic map (map in the sense of dictionary alike object)
    - the map is an S3 class
- Example: `aes(key1=value1, key2=value2, ...)`
- Just builds a list (dictionary) object mapping the keys to the values
- the keys and values are treated as snippets of R code and quoted but not
  evaluated by aes()
- the values are evaluated in the context of the data frame later on when
  building the graph
- Each layer must have an aesthetic map but can inherit it from the plot itself
- You can save the mapping either
    1. at the "root" of the graph by passing it to `ggplot()` - this makes this
       aesthetic mapping available to all subsequent layers unless overrideen by
       the particular layer
        - this is just a developer convenience to allow you to reuse an
          aesthetic mapping easily
    2. or you pass it to a specific layer i.e. `layer()` or one of its short-cut
       functions
- if you pass a mapping in both places they will be combined by default (but
  this can be controlled by an arg to the layer functions)

```r
# Create a plot with a data fram and an empty aesthetic mapping - this won't
# generate any output until you add a layer and that layer will have to provide
# its own aesthetic mapping ...
ggplot(data = my_data, mapping = aes())

# ... like this:
ggplot(data = rts_data, mapping = aes()) +
  geom_point(mapping = aes(x = Period, y = Data_value))


# Instead of passing the mapping to `ggplot` you could save it as a variable
my_mapping <- aes(x = Period, y = Data_value)

ggplot(data = rts_data, mapping = aes()) +
  geom_point(mapping = my_mapping)
```

## layer()

A layer is a combination of data, stat and geom with a potential position
adjustment. Usually layers are created using geom*\* or stat*\* calls but it can
also be created directly using this function.

```
layer(
  geom = NULL,
  stat = NULL,
  data = NULL,
  mapping = NULL,
  position = NULL,
  params = list(),
  inherit.aes = TRUE,
  check.aes = TRUE,
  check.param = TRUE,
  show.legend = NA,
  key_glyph = NULL,
  layer_class = Layer
)
```

```r
# geom calls are just a short cut for layer
ggplot(mpg, aes(displ, hwy)) + geom_point()

# shortcut for

ggplot() +
layer(
    data = mpg,
    mapping = aes(x = displ, y = hwy),
    geom = "point",
    stat = "identity",
    position = "identity",
    params = list(na.rm = FALSE)
)

# use a function as data to plot a subset of global data
ggplot(mpg, aes(displ, hwy)) +
  layer(geom = "point", stat = "identity", position = "identity",
    data = head, params = list(na.rm = FALSE)
  )
```

## geom*\*, stat*\*

- These functions are just shortcuts for the `layer()` function
- A collection of functions whose names being with `geom_`
- They create "layers" on your plot and fill in some values for you e.g. the
  "geom" property of layers
- each "geom" understands a different set of aesthetics (i.e. the values in the
  aesthetic mapping)

- they return an instance of an Environment (an S3 object which inherits from
  `LayerInstance Layer ggproto gg`
- The return value is merged into the plot object via the overloaded `+`
  operator

```
geom_point(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,  # extra args are passed on to layer()
        # these can be things like
        # color = "red"
        # size

  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_point() understands the following aesthetics (required aesthetics are in bold):
    x (required)
    y (required)
    alpha
    colour
    fill
    group
    shape
    size
    stroke
```

Q: so do aesthetics get passed to the geom or something else?

## themes (theme\_\* functions)

- ggplot2 has a few built-in themes
- the `theme_*` functions are mostly constructors which build S3 object
  instances of `S3 theme gg`)
    - the `+` operator saves the theme into the main plot object
    - some of the `theme_*` functions let you get an existing theme and tweak
      bits of it

```r
ggplot(data = rts_data, mapping = aes()) +
    geom_point(mapping = my_aes) +
    theme_bw()
```

## Anatomy of a full ggplot call

A plot needs

1. A base
1. 1+ layers. Each layer has
    1. Data
    1. An aesthetic mapping
    1. A geometry
    1. A statistical transformation
    1. A position adjustment
1. 1+ scales
1. A coordinate system
1. Faceting specification what gets faceted? layers? is faceting applied to the
   plot or to a particular layer?

1. a theme

```r
ggplot(
    data = rts_data,
    mapping = aes() # mapping inherited by all layers
) +
geom_point(
    mapping = my_aes,
    position = position_identity()
) +
theme_bw()
```
