
? ways of creating popups


```js
function popitup(url) {
    newwindow=window.open(url,'name','height=200,width=150');
    if (window.focus) {newwindow.focus()}
    return false;
}
```

```html
<a href="popupex.html" onclick="return popitup('popupex.html')"
    >Link to popup</a>
```


