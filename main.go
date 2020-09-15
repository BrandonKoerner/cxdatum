package main

import (
	//"io/ioutil"
	"os"

	"github.com/ReewassSquared/cxdatum/syntax"
)

/* expects a single file. */
func main() {
	syntax.CompileFile(os.Args[1])
}
