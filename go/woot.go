package main

import (
	"fmt"
	"strconv"
)

type Nothing struct {
	name string
	age int
}

func (thing *Nothing) String() string {
	return thing.name + " is " + strconv.Itoa(thing.age) + " years old"
}

func main() {
	first := &Nothing{"will",26}
	fmt.Printf("first: %s\n", first)
	//first.age++
	second := first
	second.age = 28
	fmt.Printf("now: %s\n", second)
}
