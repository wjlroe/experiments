package main

import (
	"fmt"
	"strconv"
	"time"
)

const NUMNODES = 5
const NUMMSGS = 5

type Message struct {
	message string
	counter int
	quit chan bool
}

func (m *Message) String() string {
	return "Message: " + m.message + " and counter is: " + strconv.Itoa(m.counter)
}

type Ringlink struct {
	nextlink *Ringlink
	tunnel chan *Message
}

func (r *Ringlink) String() string {
	return "Hello I am a ringlink, nice to meet you"
}

func (r *Ringlink) listen(quit chan bool) {
	for {
		msg := <-r.tunnel
		new_msg := msg
		new_msg.counter--
		if (new_msg.counter > 0) {
			r.nextlink.tunnel <- new_msg
		} else {
			quit <- true
		}
	}
}

func start(links []Ringlink, num_msgs int) {
	quit := make(chan bool)
	for i, _ := range links {
		go links[i].listen(quit)
	}
	fmt.Println("All links started")
	first_msg := &Message{message: "hello", counter: num_msgs}
	fmt.Println("Going to send", first_msg)
	links[0].tunnel <- first_msg
	<- quit
}

func run(num_nodes int, num_msgs int) {
	ringlinks := make([]Ringlink, num_nodes)
	l := len(ringlinks)
	fmt.Println("Created", strconv.Itoa(l), "Ringlinks")
	for i, _ := range ringlinks {
		j := (i+1) % l
		ringlinks[i].tunnel = make(chan *Message)
		ringlinks[i].nextlink = &ringlinks[j]
	}
	start(ringlinks, num_msgs)
}

func time_exec(num_nodes int, num_msgs int) {
	before := time.Nanoseconds()
	run(num_nodes, num_msgs)
	after := time.Nanoseconds()
	elapsed := after - before
	fmt.Println("Took:", elapsed)
}

func main() {
	time_exec(5,5)
	time_exec(50,50)
	time_exec(200,200)
	time_exec(1000,1000)
}
