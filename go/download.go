package main

import (
	"fmt"
	"http"
	"path"
	"os"
	"log"
)

func save(from

func download(url string) {
	//filename := path.Base(url)
	//fullname := path.Join("~", "Downloads", filename)
	fullname := path.Join(os.Getenv("HOME"), "Downloads", "woot.tar.gz")
	fmt.Printf("Fullname: %s\n", fullname)
	encoded := fmt.Sprintf("%s", url)
	fmt.Println(encoded)
	r, _, err := http.Get(encoded)
	const NBUF = 512
	var buf [NBUF]byte
	fd, oerr := os.Open(fullname, os.O_WRONLY|os.O_CREAT, 0644)
	if oerr != nil {
		fmt.Printf("Opening file: %s failed with error: %s\n", fullname, oerr.String())
		os.Exit(1)
	}
	if err == nil {
		for {
			switch nr, ferr := r.Body.Read(buf[:]); true {
			case nr < 0:
				fmt.Fprintf(os.Stderr, "cat: error reading from Body: %s\n", ferr.String())
				os.Exit(1)
			case nr == 0:  // EOF
				break
			case nr > 0:
				if nw, ew := fd.Write(buf[0:nr]); nw != nr {
					fmt.Fprintf(os.Stderr, "cat: error writing %d bytes from Body: %s\n", nw, ew.String())
					os.Exit(1)
				}
			}

		}
		fmt.Printf("Finished reading/writing file\n")
		r.Body.Close()
	} else {
		fmt.Printf("Error reading from body: %s\n", err.String())
		log.Stderr(err)
		os.Exit(1)
	}
	fmt.Printf("Written. Closing...")
	fd.Close()
	fmt.Printf("Closed")
}

func main() {
	//download("http://thenine.ca/essential/1993/1993.10.30%20-%20Essential%20Mix%20-%20Pete%20Tong%20(Studio%20Session).mp3")
	download("http://protobuf-c.googlecode.com/files/protobuf-c-0.14.tar.gz")
}
