package chess

import (
	"bytes"
	"encoding/binary"
	"errors"
	"strconv"
	"strings"
	// Import necessary types from bitboard.go's package (implicitly same package)
	// Ensure bitboard.go is part of the same 'chess' package
)

// A Board represents a chess board and its relationship between squares and pieces using bitboards.
type Board struct {
	// Piece Bitboards
	bbWhiteKing   Bitboard
	bbWhiteQueen  Bitboard
	bbWhiteRook   Bitboard
	bbWhiteBishop Bitboard
	bbWhiteKnight Bitboard
	bbWhitePawn   Bitboard
	bbBlackKing   Bitboard
	bbBlackQueen  Bitboard
	bbBlackRook   Bitboard
	bbBlackBishop Bitboard
	bbBlackKnight Bitboard
	bbBlackPawn   Bitboard

	// Convenience Bitboards
	whiteSqs Bitboard // Combined white pieces
	blackSqs Bitboard // Combined black pieces
	emptySqs Bitboard // Unoccupied squares

	// King Locations (cached for efficiency)
	whiteKingSq Square
	blackKingSq Square
}

// NewBoard returns a board initialized from a square-to-piece mapping.
func NewBoard(m map[Square]Piece) *Board {
	b := &Board{} // Initializes all Bitboard fields to EmptyBB (0)
	for sq, p := range m {
		sqBB := SquareBB(sq) // Get bitboard for the square
		if sqBB == EmptyBB { // Skip invalid squares if any
			continue
		}
		switch p {
		case WhiteKing:
			b.bbWhiteKing |= sqBB
		case WhiteQueen:
			b.bbWhiteQueen |= sqBB
		case WhiteRook:
			b.bbWhiteRook |= sqBB
		case WhiteBishop:
			b.bbWhiteBishop |= sqBB
		case WhiteKnight:
			b.bbWhiteKnight |= sqBB
		case WhitePawn:
			b.bbWhitePawn |= sqBB
		case BlackKing:
			b.bbBlackKing |= sqBB
		case BlackQueen:
			b.bbBlackQueen |= sqBB
		case BlackRook:
			b.bbBlackRook |= sqBB
		case BlackBishop:
			b.bbBlackBishop |= sqBB
		case BlackKnight:
			b.bbBlackKnight |= sqBB
		case BlackPawn:
			b.bbBlackPawn |= sqBB
		}
	}
	b.calcConvienceBBs(nil) // Calculate combined BBs and find kings
	return b
}

// SquareMap returns a mapping of squares to pieces derived from the bitboard representation.
// Only occupied squares are included in the map.
func (b *Board) SquareMap() map[Square]Piece {
	m := map[Square]Piece{}
	combinedOccupancy := b.whiteSqs | b.blackSqs // Get all occupied squares

	// Iterate through set bits in the combined occupancy bitboard
	for tempBB := combinedOccupancy; tempBB != EmptyBB; {
		sq, next, ok := tempBB.PopLSB()
		if !ok {
			break // Should not happen if tempBB != 0
		}
		tempBB = next // Move to the next bit

		p := b.Piece(sq) // Find the piece on this square
		if p != NoPiece {
			m[sq] = p
		}
	}
	return m
}

// Piece returns the piece located on the given square by checking the individual bitboards.
// Returns NoPiece if the square is empty or invalid.
func (b *Board) Piece(sq Square) Piece {
	sqBB := SquareBB(sq)
	if sqBB == EmptyBB { // Handle invalid square index
		return NoPiece
	}

	// Check White Pieces
	if (b.bbWhiteKing & sqBB) != 0 {
		return WhiteKing
	}
	if (b.bbWhiteQueen & sqBB) != 0 {
		return WhiteQueen
	}
	if (b.bbWhiteRook & sqBB) != 0 {
		return WhiteRook
	}
	if (b.bbWhiteBishop & sqBB) != 0 {
		return WhiteBishop
	}
	if (b.bbWhiteKnight & sqBB) != 0 {
		return WhiteKnight
	}
	if (b.bbWhitePawn & sqBB) != 0 {
		return WhitePawn
	}

	// Check Black Pieces
	if (b.bbBlackKing & sqBB) != 0 {
		return BlackKing
	}
	if (b.bbBlackQueen & sqBB) != 0 {
		return BlackQueen
	}
	if (b.bbBlackRook & sqBB) != 0 {
		return BlackRook
	}
	if (b.bbBlackBishop & sqBB) != 0 {
		return BlackBishop
	}
	if (b.bbBlackKnight & sqBB) != 0 {
		return BlackKnight
	}
	if (b.bbBlackPawn & sqBB) != 0 {
		return BlackPawn
	}

	return NoPiece // Square is empty
}

// update applies a move to the board, modifying the bitboard representation.
// Assumes the move is valid.
func (b *Board) update(m *Move) {
	p1 := b.Piece(m.S1()) // Piece being moved
	s1 := m.S1()
	s2 := m.S2()

	// 1. Clear the origin square (s1) for the moving piece (p1)
	bbp1 := b.bbForPiece(p1)
	b.setBBForPiece(p1, bbp1.Clear(s1))

	// 2. Handle capture: Clear the destination square (s2) for any captured piece
	// Note: En passant capture is handled separately below.
	if m.HasTag(Capture) && !m.HasTag(EnPassant) {
		p2 := b.Piece(s2) // Piece on destination square
		if p2 != NoPiece {
			bbp2 := b.bbForPiece(p2)
			b.setBBForPiece(p2, bbp2.Clear(s2))
		}
	}

	// 3. Place the moving piece (p1) on the destination square (s2)
	// If promotion occurs, this is temporary; the pawn is removed and promo piece added below.
	bbp1 = b.bbForPiece(p1) // Re-fetch (might have been cleared)
	b.setBBForPiece(p1, bbp1.Set(s2))

	// 4. Handle Special Moves
	promoType := m.Promo()
	if promoType != NoPieceType {
		promoPiece := NewPiece(promoType, p1.Color())
		// Remove the pawn that just arrived at s2
		b.setBBForPiece(p1, b.bbForPiece(p1).Clear(s2))
		// Add the promoted piece at s2
		bbPromo := b.bbForPiece(promoPiece)
		b.setBBForPiece(promoPiece, bbPromo.Set(s2))
	} else if m.HasTag(EnPassant) {
		var capturedPawnSq Square
		if p1.Color() == White { // White captures Black pawn
			capturedPawnSq = s2 - 8 // Black pawn is one rank below EP square s2
			b.bbBlackPawn = b.bbBlackPawn.Clear(capturedPawnSq)
		} else { // Black captures White pawn
			capturedPawnSq = s2 + 8 // White pawn is one rank above EP square s2
			b.bbWhitePawn = b.bbWhitePawn.Clear(capturedPawnSq)
		}
	} else if m.HasTag(KingSideCastle) {
		if p1.Color() == White { // White O-O
			b.bbWhiteRook = b.bbWhiteRook.Clear(H1).Set(F1)
		} else { // Black O-O
			b.bbBlackRook = b.bbBlackRook.Clear(H8).Set(F8)
		}
	} else if m.HasTag(QueenSideCastle) {
		if p1.Color() == White { // White O-O-O
			b.bbWhiteRook = b.bbWhiteRook.Clear(A1).Set(D1)
		} else { // Black O-O-O
			b.bbBlackRook = b.bbBlackRook.Clear(A8).Set(D8)
		}
	}

	// 5. Recalculate convenience bitboards and update king locations
	b.calcConvienceBBs(m)
}

// calcConvienceBBs updates the combined white, black, and empty square bitboards,
// and caches the king's square location.
// If m is not nil, it assumes the king might have moved and updates kingSq accordingly.
func (b *Board) calcConvienceBBs(m *Move) {
	b.whiteSqs = b.bbWhiteKing | b.bbWhiteQueen | b.bbWhiteRook | b.bbWhiteBishop | b.bbWhiteKnight | b.bbWhitePawn
	b.blackSqs = b.bbBlackKing | b.bbBlackQueen | b.bbBlackRook | b.bbBlackBishop | b.bbBlackKnight | b.bbBlackPawn
	b.emptySqs = ^(b.whiteSqs | b.blackSqs)

	// Update king square caches
	if m == nil { // Initial calculation or state load
		// Find kings by checking their bitboards (safer than assuming LSB if BB could be empty/invalid)
		b.whiteKingSq = NoSquare
		wKingSq, wOk := b.bbWhiteKing.LSB()
		if wOk {
			b.whiteKingSq = wKingSq
		}

		b.blackKingSq = NoSquare
		bKingSq, bOk := b.bbBlackKing.LSB()
		if bOk {
			b.blackKingSq = bKingSq
		}
	} else { // Update based on move
		if m.S1() == b.whiteKingSq {
			b.whiteKingSq = m.S2()
		} else if m.S1() == b.blackKingSq {
			b.blackKingSq = m.S2()
		}
		// If a castle move, the king square is already updated based on S1->S2.
	}
}

// copy creates a deep copy of the board.
func (b *Board) copy() *Board {
	return &Board{
		// Copy Bitboards
		bbWhiteKing:   b.bbWhiteKing,
		bbWhiteQueen:  b.bbWhiteQueen,
		bbWhiteRook:   b.bbWhiteRook,
		bbWhiteBishop: b.bbWhiteBishop,
		bbWhiteKnight: b.bbWhiteKnight,
		bbWhitePawn:   b.bbWhitePawn,
		bbBlackKing:   b.bbBlackKing,
		bbBlackQueen:  b.bbBlackQueen,
		bbBlackRook:   b.bbBlackRook,
		bbBlackBishop: b.bbBlackBishop,
		bbBlackKnight: b.bbBlackKnight,
		bbBlackPawn:   b.bbBlackPawn,
		// Copy Convenience Bitboards
		whiteSqs: b.whiteSqs,
		blackSqs: b.blackSqs,
		emptySqs: b.emptySqs,
		// Copy King Locations
		whiteKingSq: b.whiteKingSq,
		blackKingSq: b.blackKingSq,
	}
}

// isOccupied checks if a given square is occupied by any piece.
func (b *Board) isOccupied(sq Square) bool {
	// Check if the square bit is NOT set in the empty squares bitboard
	// Or, check if it IS set in the combined occupancy bitboard
	// return !b.emptySqs.Occupied(sq) // Equivalent
	return ((b.whiteSqs | b.blackSqs) & SquareBB(sq)) != 0
}

// hasSufficientMaterial checks if there is enough material on the board for a checkmate to be possible.
// Used for automatic draw detection (Insufficient Material).
func (b *Board) hasSufficientMaterial() bool {
	// Rule out easy cases first:
	// Any pawn, rook, or queen guarantees sufficient material.
	if (b.bbWhitePawn | b.bbWhiteRook | b.bbWhiteQueen |
		b.bbBlackPawn | b.bbBlackRook | b.bbBlackQueen) != EmptyBB {
		return true
	}

	// If we reach here, only Kings, Knights, and Bishops remain.
	whiteKnights := b.bbWhiteKnight.PopCount()
	whiteBishops := b.bbWhiteBishop.PopCount()
	blackKnights := b.bbBlackKnight.PopCount()
	blackBishops := b.bbBlackBishop.PopCount()

	// K vs K is insufficient
	if whiteKnights == 0 && whiteBishops == 0 && blackKnights == 0 && blackBishops == 0 {
		return false
	}

	// K+N vs K is insufficient
	if whiteKnights == 1 && whiteBishops == 0 && blackKnights == 0 && blackBishops == 0 {
		return false
	}
	if blackKnights == 1 && blackBishops == 0 && whiteKnights == 0 && whiteBishops == 0 {
		return false
	}

	// K+B vs K is insufficient
	if whiteBishops == 1 && whiteKnights == 0 && blackKnights == 0 && blackBishops == 0 {
		return false
	}
	if blackBishops == 1 && blackKnights == 0 && whiteKnights == 0 && whiteBishops == 0 {
		return false
	}

	// K+B vs K+B (Bishops on same color) is insufficient
	if whiteKnights == 0 && blackKnights == 0 && whiteBishops == 1 && blackBishops == 1 {
		// Check if bishops are on the same color square
		wbSq, wOk := b.bbWhiteBishop.LSB()
		bbSq, bOk := b.bbBlackBishop.LSB()
		if wOk && bOk && SquareColor(wbSq) == SquareColor(bbSq) {
			return false
		}
	}

	// All other scenarios (e.g., K+N+N vs K, K+B+N vs K, K+B+B vs K etc.) are generally sufficient.
	// Note: K+N+N vs K is technically insufficient in FIDE rules, but complex to detect edge cases perfectly here.
	// Most engines treat it as sufficient or rely on game outcome/rules elsewhere.
	// The FIDE rule mainly applies if the stronger side cannot force mate.
	// For draw claim purposes (like in insufficient material rule), this simplified check is common.

	return true
}

// --- Helper methods for getting/setting specific piece bitboards ---

// bbForPiece returns the specific Bitboard for the given piece.
// Returns EmptyBB if the piece is NoPiece.
func (b *Board) bbForPiece(p Piece) Bitboard {
	switch p {
	case WhiteKing:
		return b.bbWhiteKing
	case WhiteQueen:
		return b.bbWhiteQueen
	case WhiteRook:
		return b.bbWhiteRook
	case WhiteBishop:
		return b.bbWhiteBishop
	case WhiteKnight:
		return b.bbWhiteKnight
	case WhitePawn:
		return b.bbWhitePawn
	case BlackKing:
		return b.bbBlackKing
	case BlackQueen:
		return b.bbBlackQueen
	case BlackRook:
		return b.bbBlackRook
	case BlackBishop:
		return b.bbBlackBishop
	case BlackKnight:
		return b.bbBlackKnight
	case BlackPawn:
		return b.bbBlackPawn
	default:
		return EmptyBB
	}
}

// setBBForPiece updates the specific Bitboard for the given piece.
// Panics if the piece is invalid (should not happen with internal use).
func (b *Board) setBBForPiece(p Piece, bb Bitboard) {
	switch p {
	case WhiteKing:
		b.bbWhiteKing = bb
	case WhiteQueen:
		b.bbWhiteQueen = bb
	case WhiteRook:
		b.bbWhiteRook = bb
	case WhiteBishop:
		b.bbWhiteBishop = bb
	case WhiteKnight:
		b.bbWhiteKnight = bb
	case WhitePawn:
		b.bbWhitePawn = bb
	case BlackKing:
		b.bbBlackKing = bb
	case BlackQueen:
		b.bbBlackQueen = bb
	case BlackRook:
		b.bbBlackRook = bb
	case BlackBishop:
		b.bbBlackBishop = bb
	case BlackKnight:
		b.bbBlackKnight = bb
	case BlackPawn:
		b.bbBlackPawn = bb
	default:
		// This should ideally not be reached if called with valid pieces
		panic("chess: setBBForPiece called with invalid piece")
	}
}

// --- FEN and Debugging ---

// Draw returns visual representation of the board useful for debugging.
func (b *Board) Draw() string {
	s := "\n  a b c d e f g h\n"
	for r := Rank8; r >= Rank1; r-- {
		s += Rank(r).String() + " " // Rank number (adjust if Rank String gives 1-8)
		// If Rank String returns 0-7, use: s += strconv.Itoa(int(r)+1) + " "
		for f := FileA; f <= FileH; f++ {
			p := b.Piece(NewSquare(f, r))
			if p == NoPiece {
				// Use dot or space based on square color for better readability
				if SquareColor(NewSquare(f, r)) == White { // Light square
					s += ". "
				} else { // Dark square
					s += "+ " // Or another char for dark empty
				}
			} else {
				s += p.String() + " " // Use Unicode piece character
			}
		}
		s += Rank(r).String() + "\n" // Rank number again
		// If Rank String returns 0-7, use: s += strconv.Itoa(int(r)+1) + "\n"
	}
	s += "  a b c d e f g h\n"
	return s
}

// String implements the fmt.Stringer interface and returns
// a string in the FEN board format: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR
func (b *Board) String() string {
	var fen strings.Builder
	for r := Rank8; r >= Rank1; r-- {
		emptyCount := 0
		for f := FileA; f <= FileH; f++ {
			sq := NewSquare(f, r)
			p := b.Piece(sq)
			if p == NoPiece {
				emptyCount++
			} else {
				if emptyCount > 0 {
					fen.WriteString(strconv.Itoa(emptyCount))
					emptyCount = 0
				}
				fen.WriteString(p.getFENChar()) // Use existing helper for FEN char
			}
		}
		if emptyCount > 0 {
			fen.WriteString(strconv.Itoa(emptyCount))
		}
		if r != Rank1 {
			fen.WriteString("/")
		}
	}
	return fen.String()
}

// --- Serialization ---

// MarshalBinary implements the encoding.BinaryMarshaler interface.
// Encodes the 12 piece bitboards in standard order.
func (b *Board) MarshalBinary() (data []byte, err error) {
	bbs := []Bitboard{
		b.bbWhiteKing, b.bbWhiteQueen, b.bbWhiteRook, b.bbWhiteBishop, b.bbWhiteKnight, b.bbWhitePawn,
		b.bbBlackKing, b.bbBlackQueen, b.bbBlackRook, b.bbBlackBishop, b.bbBlackKnight, b.bbBlackPawn,
	}
	buf := new(bytes.Buffer)
	// Ensure capacity for 12 * 8 bytes = 96 bytes
	buf.Grow(96)
	err = binary.Write(buf, binary.BigEndian, bbs)
	return buf.Bytes(), err
}

// UnmarshalBinary implements the encoding.BinaryUnmarshaler interface.
// Decodes the 12 piece bitboards in standard order.
func (b *Board) UnmarshalBinary(data []byte) error {
	if len(data) != 96 {
		return errors.New("chess: invalid number of bytes for board unmarshal binary (expected 96)")
	}

	b.bbWhiteKing = Bitboard(binary.BigEndian.Uint64(data[0:8]))
	b.bbWhiteQueen = Bitboard(binary.BigEndian.Uint64(data[8:16]))
	b.bbWhiteRook = Bitboard(binary.BigEndian.Uint64(data[16:24]))
	b.bbWhiteBishop = Bitboard(binary.BigEndian.Uint64(data[24:32]))
	b.bbWhiteKnight = Bitboard(binary.BigEndian.Uint64(data[32:40]))
	b.bbWhitePawn = Bitboard(binary.BigEndian.Uint64(data[40:48]))
	b.bbBlackKing = Bitboard(binary.BigEndian.Uint64(data[48:56]))
	b.bbBlackQueen = Bitboard(binary.BigEndian.Uint64(data[56:64]))
	b.bbBlackRook = Bitboard(binary.BigEndian.Uint64(data[64:72]))
	b.bbBlackBishop = Bitboard(binary.BigEndian.Uint64(data[72:80]))
	b.bbBlackKnight = Bitboard(binary.BigEndian.Uint64(data[80:88]))
	b.bbBlackPawn = Bitboard(binary.BigEndian.Uint64(data[88:96]))

	// Recalculate convenience bitboards and king locations after loading
	b.calcConvienceBBs(nil)
	return nil
}

// MarshalText implements the encoding.TextMarshaler interface. Returns FEN board string.
func (b *Board) MarshalText() (text []byte, err error) {
	return []byte(b.String()), nil
}

// UnmarshalText implements the encoding.TextUnmarshaler interface. Parses FEN board string.
func (b *Board) UnmarshalText(text []byte) error {
	// Use the existing fenBoard helper function (assuming it's adapted or available)
	// Need to handle potential circular dependency if fenBoard is in fen.go which imports board.go
	// A potential solution is to have a dedicated FEN parsing function accessible to both.
	// For now, let's assume fenBoard can be called or reimplement minimal FEN board parsing here.

	// Reimplementation sketch (if fenBoard is not accessible):
	parsedBoard, err := fenBoard(string(text)) // Assuming fenBoard exists and returns *Board
	if err != nil {
		return err
	}
	*b = *parsedBoard // Copy the state from the parsed board
	return nil
}

// Rotate rotates the board 90 degrees clockwise. (Leverages Flip and Transpose)
func (b *Board) Rotate() *Board {
	return b.Flip(UpDown).Transpose() // Or equivalent combination
}

// FlipDirection is the direction for the Board.Flip method
type FlipDirection int

const (
	UpDown    FlipDirection = iota // flips the board's rank values
	LeftRight                      // flips the board's file values
)

// Flip flips the board over the vertical or horizontal center line.
func (b *Board) Flip(fd FlipDirection) *Board {
	m := map[Square]Piece{}
	allOccupied := b.whiteSqs | b.blackSqs
	for tempBB := allOccupied; tempBB != EmptyBB; {
		sq, next, _ := tempBB.PopLSB()
		tempBB = next
		var flippedSq Square
		switch fd {
		case UpDown:
			flippedSq = FlipVertical(sq) // Use helper from bitboard.go if available
		case LeftRight:
			flippedSq = FlipHorizontal(sq) // Use helper from bitboard.go if available
		}
		if flippedSq != NoSquare {
			m[flippedSq] = b.Piece(sq)
		}
	}
	return NewBoard(m)
}

// Transpose flips the board over the A1-H8 diagonal.
func (b *Board) Transpose() *Board {
	m := map[Square]Piece{}
	allOccupied := b.whiteSqs | b.blackSqs
	for tempBB := allOccupied; tempBB != EmptyBB; {
		sq, next, _ := tempBB.PopLSB()
		tempBB = next
		transposedSq := FlipDiagonal(sq) // Use helper from bitboard.go if available
		if transposedSq != NoSquare {
			m[transposedSq] = b.Piece(sq)
		}
	}
	return NewBoard(m)
}
