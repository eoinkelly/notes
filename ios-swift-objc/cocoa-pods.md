# Cocoapods

- http://guides.cocoapods.org/

Good explanation of what it really does at end of
http://guides.cocoapods.org/using/using-cocoapods.html

To get going after `gem install cocoapods` I had to:

```
$ pod repo remove master
$ pod setup
$ pod install
```

Understanding cocoapods seems to be mostly understanding how XCode links
libraries, frameworks etc.

```
# significant dirs

~/Library/Caches/CocoaPods/
~/.cocoapods/repos/master/
```
