package main

import (
	"http"
	"log"
)

func main() {
	// This works
	r, _, err := http.Get("https://www.google.com")
	if err != nil {
		log.Exit(err)
	}
	log.Println(r)

	// This doesn't
	r, _, err = http.Get("https://streaming.campfirenow.com")
	if err != nil {
		log.Exit(err)
	}
	log.Println(r)
}
