
## Background

Remember that R is loose about args being positional or keyword. I'm going to use the keyword versions here because I think it is clearer
* R allows you to overload built-in operators (obvs not advised etc. etc.)
    ```r
    rm('+')
    '+' <- function(x,y) paste(x,y,sep="")
    ```
* ggplot2 overloads `+` for combining objects of types it recognises.
    * `+` functions as a sort of "merge" function

## ggplot(data = ?, mapping = ?)

* Returns an S3 object which has classes `gg`, `ggplot`
* Creates the base plot
* Takes a data frame and an optional aesthetic mapping
    * aesthetic mappings can also be set on the individual layers
    * `ggplot` doesn't actually understand any aesthetics itself - it just passes them down to the layers who do

```
ggplot S3 object:
    s3 classes: gg, ggplot

    data: a reference to the data frame you passed in
    mapping: a reference to the mapping you passed in

    layers:
    scales:
    theme:
    coordinates:
    facet:
    plot_env:
    labels:
```

## aes(key = value, ...)

* Build an aesthetic map (map in the sense of dictionary alike object)
    * the map is an S3 class
* Example: `aes(key1=value1, key2=value2, ...)`
* Just builds a list (dictionary) object mapping the keys to the values
* the keys and values are treated as snippets of R code and quoted but not evaluated by aes()
* the values are evaluated in the context of the data frame later on when building the graph
* Each layer must have an aesthetic map but can inherit it from the plot itself
* You can save the mapping either
    1. at the "root" of the graph by passing it to `ggplot()` - this makes this aesthetic mapping available to all subsequent layers unless overrideen by the particular layer
        * this is just a developer convenience to allow you to reuse an aesthetic mapping easily
    2. or you pass it to a specific layer by passing it to a `geom_*()` function
* if you pass a mapping in both places they will be combined by default (but this can be controlled by an arg to the layer functions)

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

## geom_*

* A collection of functions whose names being with `geom_`
* They create "layers" on your plot
* each layer function understands a different set of aesthetics (i.e. the values in the aesthetic mapping)

* they return an instance of an Environment (an S3 object which inherits from `LayerInstance Layer ggproto gg`
* The return value is merged into the plot object via the overloaded `+` operator

