package main

import (
	"io"
	"net/http"
	"os"
)

func RootServer(w http.ResponseWriter, req *http.Request) {
	io.WriteString(w, "hi there\n")
}

func main() {
	http.HandleFunc("/", RootServer)
	err := http.ListenAndServe(":3001", nil)
	print("Exiting ...")
	print(err)
	os.Exit(1)
}
