package opening_test

import (
	"fmt"
	"testing"

	"github.com/0x5844/chess"
	"github.com/0x5844/chess/opening"
)

func ExampleBook_Find() {
	g := chess.NewGame()
	g.MoveStr("e4")
	g.MoveStr("e6")

	// print French Defense
	book, err := opening.NewBookECO()
	if err != nil {
		fmt.Println("Error creating book:", err)
		return
	}
	o := book.Find(g.Moves())
	fmt.Println(o.Title())
}

func ExampleBook_Possible() {
	g := chess.NewGame()
	g.MoveStr("e4")
	g.MoveStr("d5")

	// print all variations of the Scandinavian Defense
	book, err := opening.NewBookECO()
	if err != nil {
		fmt.Println("Error creating book:", err)
		return
	}
	for _, o := range book.Possible(g.Moves()) {
		fmt.Println(o.Title())
	}
}

func TestFind(t *testing.T) {
	g := chess.NewGame()
	if err := g.MoveStr("e4"); err != nil {
		t.Fatal(err)
	}
	if err := g.MoveStr("d5"); err != nil {
		t.Fatal(err)
	}
	book, err := opening.NewBookECO() // corrected variable name from 'error' to 'err'
	if err != nil {
		t.Fatal(err)
	}
	o := book.Find(g.Moves())
	expected := "Scandinavian Defense"
	if o == nil || o.Title() != expected {
		t.Fatalf("expected to find opening %s but got %s", expected, o.Title())
	}
}

func TestPossible(t *testing.T) {
	g := chess.NewGame()
	if err := g.MoveStr("g3"); err != nil {
		t.Fatal(err)
	}
	book, err := opening.NewBookECO() // corrected variable name from 'error' to 'err'
	if err != nil {
		t.Fatal(err)
	}
	openings := book.Possible(g.Moves())
	actual := len(openings)
	if actual != 22 {
		t.Fatalf("expected %d possible openings but got %d", 22, actual)
	}
}

func BenchmarkNewBookECO(b *testing.B) {
	for i := 0; i < b.N; i++ {
		opening.NewBookECO()
	}
}
