package main

import "fmt"
import "container/list"

func everyNth(L *list.List, N int) *list.List {
	counter := 0
	l := list.New()
	for e := L.Front(); e != nil; e = e.Next() {
		counter += 1
		if counter % N == 0 {
			l.PushBack(e.Value)
		}
	}
	return l
}

func main() {
	L := list.New()
	for i := 0; i < 1000; i++ {
		L.PushBack(i)
	}
	l := everyNth(L, 109)
	for e := l.Front(); e != nil; e = e.Next() {
		fmt.Println(e.Value)
	}
}