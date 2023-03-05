# Speed index

-   https://docs.webpagetest.org/metrics/speedindex/
-   The average time at which visible parts of the page are displayed
-   a number which represents how quickly the page contents are visually populated
-   lower numbers are better
- carousels can cause problems for it because the content keeps changing
-   It depends on the size of the viewport
-   Measured in milliseconds
-   better than milestone timings e.g. `window.onload`
    -   milestones don't actually represent when the content was painted.
-   how it works
    1. Capture a video (low frame rate e.g. 10fps, one frame every 0.1 sec) of the page loading
    1. Assign each frame a percentage completeness
        - presumably 100% is determined by looking at the page at the end
    1. Plot how completeness percentage changes over time from 0% to 100%
        - The area under the curve would be a good measure except it is unbounded (it grows for every extra second you wait after 100%)
        - So we use the area above the curve because that is bounded

$$\textrm{Speed Index} = \sum_0^{end} 1 - \frac{VC}{100}$$
$$VC = \textrm{\% visually complete}$$

## How visual completion is calculated

> The technique we settled on was to take histograms of the colors in the image
> (one each for red, green and blue) and just look at the overall distribution of
> colors on the page. We calculate the difference between the starting histograms
> (for the first video frame) and the ending histogram (last video frame) and use
> that difference as the baseline. The difference of the histogram for each frame
> in the video versus the first histogram is compared to the baseline to determine
> how "complete" that video frame is. There are some cases where it will not
> really be accurate but the trade-off has proved to be very worthwhile for the
> vast majority of pages we have tested it against.
>
> ...
> More recently we have (successfully) experimented with using the Paint Events
> that are exposed by Webkit through the developer tools timeline (which are also
> available to extensions and through the remote debugging protocol). It is
> available on all recent webkit-based browsers including desktop and mobile and
> across all platforms. It is also very lightweight and does not require capturing
> video. It is somewhat sensitive to the renderer implementation in a given
> browser so it is not as useful for comparing performance across different
> browsers.
>
> In order to get useful data, it requires a fair bit of filtering and weighting.

## What is a good speed index nummber?

Average for Alexa top 100k is 5408

> you always want your speed index score to be as low as possible, anything below 1,000 is generally considered optimal
> NCC Group analyzed the homepages of the top 50 retailers in the UK and found that the average speed index score was between 3,000 - 8,500.
>
> https://www.keycdn.com/blog/speed-index

