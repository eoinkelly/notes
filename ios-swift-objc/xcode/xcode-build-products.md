# Build products

An example of build products from an Xcode project

```sh
$ Build  pwd
/Users/eoinkelly/Library/Developer/Xcode/DerivedData/Katas-fawipqktdjkykjgsplacinhbcwwa/Build
$ Build  tree -a --charset utf-8 .
.
├── Intermediates
│   └── Katas.build
│       └── Debug-iphonesimulator
│           ├── Katas.build
│           │   ├── Katas-all-non-framework-target-headers.hmap
│           │   ├── Katas-all-target-headers.hmap
│           │   ├── Katas-generated-files.hmap
│           │   ├── Katas-own-target-headers.hmap
│           │   ├── Katas-project-headers.hmap
│           │   ├── Katas.hmap
│           │   ├── LaunchScreen-PartialInfo.plist
│           │   ├── Main-SBPartialInfo.plist
│           │   ├── Objects-normal
│           │   │   ├── i386
│           │   │   │   ├── AppDelegate.d
│           │   │   │   ├── AppDelegate.dia
│           │   │   │   ├── AppDelegate.o
│           │   │   │   ├── BlockParty.d
│           │   │   │   ├── BlockParty.dia
│           │   │   │   ├── BlockParty.o
│           │   │   │   ├── BlogPost.d
│           │   │   │   ├── BlogPost.dia
│           │   │   │   ├── BlogPost.o
│           │   │   │   ├── Katas.LinkFileList
│           │   │   │   ├── Katas_dependency_info.dat
│           │   │   │   ├── ViewController.d
│           │   │   │   ├── ViewController.dia
│           │   │   │   ├── ViewController.o
│           │   │   │   ├── main.d
│           │   │   │   ├── main.dia
│           │   │   │   └── main.o
│           │   │   └── x86_64
│           │   │       ├── AppDelegate.d
│           │   │       ├── AppDelegate.dia
│           │   │       ├── AppDelegate.o
│           │   │       ├── BlockParty.d
│           │   │       ├── BlockParty.dia
│           │   │       ├── BlockParty.o
│           │   │       ├── BlogPost.d
│           │   │       ├── BlogPost.dia
│           │   │       ├── BlogPost.o
│           │   │       ├── Kata1BlogPostTests.dia
│           │   │       ├── Kata2BlocksTests.dia
│           │   │       ├── Kata3EqualityTests.dia
│           │   │       ├── Katas.LinkFileList
│           │   │       ├── Katas_dependency_info.dat
│           │   │       ├── ViewController.d
│           │   │       ├── ViewController.dia
│           │   │       ├── ViewController.o
│           │   │       ├── main.d
│           │   │       ├── main.dia
│           │   │       └── main.o
│           │   ├── assetcatalog_dependencies.txt
│           │   ├── assetcatalog_generated_info.plist
│           │   └── dgph
│           └── KatasTests.build
│               ├── KatasTests-all-non-framework-target-headers.hmap
│               ├── KatasTests-all-target-headers.hmap
│               ├── KatasTests-generated-files.hmap
│               ├── KatasTests-own-target-headers.hmap
│               ├── KatasTests-project-headers.hmap
│               ├── KatasTests.hmap
│               ├── Objects-normal
│               │   ├── i386
│               │   │   ├── BlockParty.d
│               │   │   ├── BlockParty.dia
│               │   │   ├── BlockParty.o
│               │   │   ├── Kata1BlogPostTests.d
│               │   │   ├── Kata1BlogPostTests.dia
│               │   │   ├── Kata1BlogPostTests.o
│               │   │   ├── Kata2BlocksTests.d
│               │   │   ├── Kata2BlocksTests.dia
│               │   │   ├── Kata2BlocksTests.o
│               │   │   ├── Kata3EqualityTests.d
│               │   │   ├── Kata3EqualityTests.dia
│               │   │   ├── Kata3EqualityTests.o
│               │   │   ├── KatasTests.LinkFileList
│               │   │   └── KatasTests_dependency_info.dat
│               │   └── x86_64
│               │       ├── BlockParty.d
│               │       ├── BlockParty.dia
│               │       ├── BlockParty.o
│               │       ├── Kata1BlogPostTests.d
│               │       ├── Kata1BlogPostTests.dia
│               │       ├── Kata1BlogPostTests.o
│               │       ├── Kata2BlocksTests.d
│               │       ├── Kata2BlocksTests.dia
│               │       ├── Kata2BlocksTests.o
│               │       ├── Kata3EqualityTests.d
│               │       ├── Kata3EqualityTests.dia
│               │       ├── Kata3EqualityTests.o
│               │       ├── KatasTests.LinkFileList
│               │       ├── KatasTests.d
│               │       ├── KatasTests.dia
│               │       ├── KatasTests.o
│               │       └── KatasTests_dependency_info.dat
│               └── dgph
└── Products
    └── Debug-iphonesimulator
        ├── .DS_Store
        ├── Katas.app
        │   ├── Base.lproj
        │   │   ├── LaunchScreen.nib
        │   │   └── Main.storyboardc
        │   │       ├── Info.plist
        │   │       ├── UIViewController-vXZ-lx-hvc.nib
        │   │       └── vXZ-lx-hvc-view-kh9-bI-dsS.nib
        │   ├── Info.plist
        │   ├── Katas
        │   └── PkgInfo
        ├── Katas.app.dSYM
        │   └── Contents
        │       ├── Info.plist
        │       └── Resources
        │           └── DWARF
        │               └── Katas
        ├── KatasTests.xctest
        │   ├── Info.plist
        │   └── KatasTests
        └── KatasTests.xctest.dSYM
            └── Contents
                ├── Info.plist
                └── Resources
                    └── DWARF
                        └── KatasTests
```

The build products dir contains `Intermediates` and `Products`

File types:

- `.nib`
    - compiled `.xib`
- `.dat`
- `.hmap`
    - binary data
- `.linkFileList`

Every `.m` source file in the project generates:

- `.d`
- `.o`
- `.dia`

TODO: find out more about these files
