package main

import (
	"fmt"
	"testing"
)

func TestReverse(t *testing.T) {
	if got, want := reverse("Hello."), ".olleH"; got != want {
		t.Errorf("got:\n%v\nwant:\n%v\n", got, want)
	}
}

func Example_reverse() {
	fmt.Println(reverse("Hello."))
	fmt.Printf("%q\n", reverse(""))
	fmt.Printf("%q\n", reverse("1"))
	fmt.Printf("%q\n", reverse("12"))
	fmt.Printf("%q\n", reverse("123"))
	fmt.Printf("%q\n", reverse("Hello, 世界"))

	// Output:
	// .olleH
	// ""
	// "1"
	// "21"
	// "321"
	// "界世 ,olleH"
}

func Example_underline() {
	fmt.Print(underline("Underline Test") + "\nstuff that goes here")

	// Output:
	// Underline Test
	// --------------
	//
	// stuff that goes here
}
