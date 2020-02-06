package main

// note that the folder name is different from the package name and that's fine
// - we just pointed go at the folder in the import and the package name is used
// as the name of the imported package (not the folder)

// however go fmt (or some linter) really wants to add a package alias to the
// import
import (
	"./examplething"
)

func main() {
	example.Thinger()
}
