package caret_test

import (
	"fmt"

	"github.com/shurcooL-legacy/Conception-go/caret"
	"github.com/shurcooL-legacy/Conception-go/pkg/multilinecontent"
	"github.com/shurcooL/go-goon"
)

const sample = `package main

import "fmt"

func main() {
	fmt.Println("Hello world.")
	fmt.Println("How are you?")

	if 1 == 1 {
		fmt.Println("Good!")
	}
}
`

func ExampleCaretPosition() {
	mc := multilinecontent.NewString(sample)
	cp := caret.NewCaretPosition(mc)
	cp.SetSelection(43, 45)

	fmt.Println(cp.AnySelection())
	goon.Dump(cp.GetSelectionContent())

	// Output:
	// true
	// (string)("fmt.Println(\"Hello world.\")\n\tfmt.Println(\"How")
}
