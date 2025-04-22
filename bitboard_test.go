package chess

import "testing"

type bitboardTestPair struct {
	initial  uint64
	reversed uint64
}

var (
	tests = []bitboardTestPair{
		{
			uint64(1),
			uint64(9223372036854775808),
		},
		{
			uint64(18446744073709551615),
			uint64(18446744073709551615),
		},
		{
			uint64(0),
			uint64(0),
		},
	}
)

func TestBitboardReverse(t *testing.T) {
	for _, p := range tests {
		r := uint64(Bitboard(p.initial).Reverse()) // Changed bitboard to Bitboard
		if r != p.reversed {
			t.Fatalf("bitboard reverse of %s expected %s but got %s", intStr(p.initial), intStr(p.reversed), intStr(r))
		}
	}
}

func TestBitboardOccupied(t *testing.T) {
	// Need to initialize the board using NewBoard or create a Bitboard directly
	// Let's create a Bitboard directly for simplicity in testing just Occupied
	bb := EmptyBB.Set(B3) // Create a bitboard with only B3 set

	if bb.Occupied(B3) != true {
		t.Fatalf("bitboard occupied of %s expected %t but got %t", bb, true, false)
	}

	if bb.Occupied(C4) != false {
		t.Fatalf("bitboard occupied of %s expected %t but got %t", bb, false, true)
	}
}

// Helper function to create a simple bitboard from a map for testing if needed elsewhere
// Note: This is simplified and doesn't match the board.go NewBoard logic
func newBitboardSimple(m map[Square]bool) Bitboard {
	var bb Bitboard
	for sq, present := range m {
		if present {
			bb = bb.Set(sq)
		}
	}
	return bb
}

func BenchmarkBitboardReverse(b *testing.B) {
	for i := 0; i < b.N; i++ {
		u := uint64(9223372036854775807)
		Bitboard(u).Reverse() // Changed bitboard to Bitboard
	}
}

func intStr(i uint64) string {
	return Bitboard(i).String() // Changed bitboard to Bitboard
}

// Example test for PopLSB (add more tests as needed)
func TestBitboardPopLSB(t *testing.T) {
	bb := SquareBB(A1) | SquareBB(H8) // 0x8000000000000001

	sq1, next1, ok1 := bb.PopLSB()
	if !ok1 || sq1 != A1 {
		t.Fatalf("PopLSB 1: expected (%s, true), got (%s, %t)", A1, sq1, ok1)
	}
	if next1 != SquareBB(H8) {
		t.Fatalf("PopLSB 1: expected remaining %s, got %s", SquareBB(H8), next1)
	}

	sq2, next2, ok2 := next1.PopLSB()
	if !ok2 || sq2 != H8 {
		t.Fatalf("PopLSB 2: expected (%s, true), got (%s, %t)", H8, sq2, ok2)
	}
	if next2 != EmptyBB {
		t.Fatalf("PopLSB 2: expected remaining %s, got %s", EmptyBB, next2)
	}

	_, _, ok3 := next2.PopLSB()
	if ok3 {
		t.Fatalf("PopLSB 3: expected (NoSquare, false), got ok=true")
	}
}

// Example test for Scan (Squares)
func TestBitboardScan(t *testing.T) {
	bb := SquareBB(A1) | SquareBB(B2) | SquareBB(H8)
	expected := []Square{A1, B2, H8}
	result := bb.Scan()

	if len(result) != len(expected) {
		t.Fatalf("Scan: expected %d squares, got %d", len(expected), len(result))
	}
	for i := range expected {
		if result[i] != expected[i] {
			t.Fatalf("Scan: expected %v, got %v", expected, result)
		}
	}

	emptyBB := EmptyBB
	if len(emptyBB.Scan()) != 0 {
		t.Fatalf("Scan (empty): expected 0 squares, got %d", len(emptyBB.Scan()))
	}
}

// Example test for benchmarking Scan
func BenchmarkBitboardScan(b *testing.B) {
	bb := SquareBB(A1) | SquareBB(B2) | SquareBB(H8)
	for i := 0; i < b.N; i++ {
		bb.Scan()
	}
}
