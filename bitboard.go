package chess

import (
	"fmt"
	"math/bits"
	"strings"
)

// Bitboard represents a 64-bit integer used to represent the state of a chessboard.
type Bitboard uint64

// --- Constants ---

const (
	NumOfSquaresInBoard = 64 // Total squares on the board.
	NumOfFiles          = 8  // Number of files (columns).
	NumOfRanks          = 8  // Number of ranks (rows).
	NumOfPieces         = 6  // Number of piece types (P, N, B, R, Q, K).
)

// Internal color indices (consistent with chess.Color, assuming White=1, Black=2)
const (
	WhiteIdx = 0 // Index for White
	BlackIdx = 1 // Index for Black
)

// Internal piece type indices (consistent with chess.PieceType, assuming Pawn=1..King=6)
const (
	PawnIdx   = 0 // Index for Pawn
	KnightIdx = 1 // Index for Knight
	BishopIdx = 2 // Index for Bishop
	RookIdx   = 3 // Index for Rook
	QueenIdx  = 4 // Index for Queen
	KingIdx   = 5 // Index for King
)

// Directions for ray generation (N, NE, E, SE, S, SW, W, NW). Used for sliders.
const (
	North = iota
	NorthEast
	East
	SouthEast
	South
	SouthWest
	West
	NorthWest
	NumDirections // Total number of directions = 8
)

// --- Predefined Bitboard Constants ---

const (
	EmptyBB Bitboard = 0
	FullBB  Bitboard = ^EmptyBB // All squares set

	// Files (LSB = Rank 1)
	FileABB Bitboard = 0x0101010101010101
	FileBBB Bitboard = FileABB << 1
	FileCBB Bitboard = FileABB << 2
	FileDBB Bitboard = FileABB << 3
	FileEBB Bitboard = FileABB << 4
	FileFBB Bitboard = FileABB << 5
	FileGBB Bitboard = FileABB << 6
	FileHBB Bitboard = FileABB << 7

	// Ranks (LSB = File A)
	Rank1BB Bitboard = 0xFF
	Rank2BB Bitboard = Rank1BB << (8 * 1)
	Rank3BB Bitboard = Rank1BB << (8 * 2)
	Rank4BB Bitboard = Rank1BB << (8 * 3)
	Rank5BB Bitboard = Rank1BB << (8 * 4)
	Rank6BB Bitboard = Rank1BB << (8 * 5)
	Rank7BB Bitboard = Rank1BB << (8 * 6)
	Rank8BB Bitboard = Rank1BB << (8 * 7)

	// Colors
	LightSquaresBB Bitboard = 0x55AA55AA55AA55AA // A1 is dark (0), B1 is light (1)... H8 is dark (0)
	DarkSquaresBB  Bitboard = ^LightSquaresBB

	// Edge Masks
	NotAFile  Bitboard = ^FileABB
	NotHFile  Bitboard = ^FileHBB
	NotABFile Bitboard = ^(FileABB | FileBBB)
	NotGHFile Bitboard = ^(FileGBB | FileHBB)
	Border    Bitboard = FileABB | FileHBB | Rank1BB | Rank8BB // All squares on the edge

	// Useful Masks for Evaluation / Logic
	EdgeFilesMask      Bitboard = FileABB | FileHBB                                         // Mask for files A and H.
	EdgeRanksMask      Bitboard = Rank1BB | Rank8BB                                         // Mask for ranks 1 and 8.
	CenterFilesMask    Bitboard = FileCBB | FileDBB | FileEBB | FileFBB                     // Mask for files C, D, E, F.
	ExtendedCenterMask Bitboard = CenterFilesMask & (Rank3BB | Rank4BB | Rank5BB | Rank6BB) // c3-f6
	CenterFourMask     Bitboard = (FileDBB | FileEBB) & (Rank4BB | Rank5BB)                 // d4, e4, d5, e5
	CornerMask         Bitboard = (FileABB | FileHBB) & (Rank1BB | Rank8BB)                 // a1, h1, a8, h8
)

// --- Precomputed Attack, Geometry, and Evaluation Data ---
// These tables are initialized in the init() function below.
var (
	// --- Basic Board Geometry ---
	fileMasks          [NumOfFiles]Bitboard          // [file] Mask for each file.
	rankMasks          [NumOfRanks]Bitboard          // [rank] Mask for each rank.
	SquareColorMask    [NumOfSquaresInBoard]Bitboard // [square] Mask for all squares of the same color (Light or Dark).
	adjacentFilesMasks [NumOfSquaresInBoard]Bitboard // [square] Mask for adjacent files.

	// --- Direct Piece Attacks & Moves (Empty Board) ---
	// Indexed by [colorIdx][square] for pawns, [square] for others.
	pawnAttacks    [2][NumOfSquaresInBoard]Bitboard // Squares attacked *by* a pawn on sq (captures).
	pawnPushes     [2][NumOfSquaresInBoard]Bitboard // Single and double pawn pushes (ignores blockers).
	pawnAttackedBy [2][NumOfSquaresInBoard]Bitboard // Squares a pawn *must be on* to attack sq.
	knightAttacks  [NumOfSquaresInBoard]Bitboard    // Squares attacked by a knight on sq. (knightAttackedBy == knightAttacks)
	kingAttacks    [NumOfSquaresInBoard]Bitboard    // Squares attacked by a king on sq.

	// --- Slider Geometry & Ray Info (Empty Board) ---
	// These are crucial for generating slider attacks considering blockers.
	rays              [NumOfSquaresInBoard][NumDirections]Bitboard       // Ray in direction from sq (excluding sq).
	lineBB            [NumOfSquaresInBoard][NumOfSquaresInBoard]Bitboard // Line between two squares (inclusive). Useful for pins, skewers. Alias: LineIncludingSquares.
	betweenBB         [NumOfSquaresInBoard][NumOfSquaresInBoard]Bitboard // Squares strictly between two squares. Useful for checking pins, discoveries. Alias: InterveningSquares.
	diagonalMasks     [NumOfSquaresInBoard]Bitboard                      // [square] Diagonal mask (A1-H8 direction).
	antiDiagonalMasks [NumOfSquaresInBoard]Bitboard                      // [square] Anti-diagonal mask (A8-H1 direction).

	// --- Attack Potential (Empty Board) ---
	// Squares from which a piece *could* attack targetSq on an empty board. Useful for check/pin detection.
	rookAttackPotential   [NumOfSquaresInBoard]Bitboard // [targetSquare] Rank/File potential attackers (excluding targetSq).
	bishopAttackPotential [NumOfSquaresInBoard]Bitboard // [targetSquare] Diagonal potential attackers (excluding targetSq).
	queenAttackPotential  [NumOfSquaresInBoard]Bitboard // [targetSquare] Rook + Bishop potential attackers.

	// --- Pawn Structure & Evaluation Masks ---
	// Indexed by [colorIdx][square] or [file] unless otherwise noted.
	passedPawnMasks       [2][NumOfSquaresInBoard]Bitboard // [colorIdx][pawnSq] Squares in front (same & adjacent files) of a potential passed pawn. Enemy pawns in this mask prevent pawnSq from being passed.
	isolatedPawnMasks     [NumOfFiles]Bitboard             // [file] Mask for adjacent files (used to check if a pawn on 'file' is isolated).
	backwardPawnMasks     [2][NumOfSquaresInBoard]Bitboard // [colorIdx][pawnSq] Squares behind pawnSq on the same and adjacent files. Used to identify backward pawns.
	pawnFrontSpan         [2][NumOfSquaresInBoard]Bitboard // [colorIdx][pawnSq] Squares on the same file strictly ahead of pawnSq.
	pawnShieldMasks       [2][NumOfSquaresInBoard]Bitboard // [colorIdx][kingSq] Common pawn shield squares (e.g., rank 2/7 on kingFile +/- 1) relative to king's square.
	pawnShieldFrontMasks  [2][NumOfSquaresInBoard]Bitboard // [colorIdx][kingSq] Pawn shield squares directly in front of the king (e.g., rank 2/7).
	outpostSupportMasks   [2][NumOfSquaresInBoard]Bitboard // [colorIdx][outpostSq] Squares where friendly pawns could support an outpost on outpostSq.
	forwardRanksMasks     [2][NumOfSquaresInBoard]Bitboard // [colorIdx][sq] Mask for all ranks strictly ahead of the square.
	outpostCandidateMasks [2][NumOfSquaresInBoard]Bitboard // [colorIdx][sq] Mask containing just sq if it meets basic outpost criteria (rank/file). Dynamic checks needed for support/attack.

	// --- King Safety Masks ---
	kingZoneMasks     [NumOfSquaresInBoard]Bitboard // [kingSq] Expanded king influence zone (e.g., 5x5 area around king).
	kingSafeZoneMasks [NumOfSquaresInBoard]Bitboard // [kingSq] King's immediate safe zone (3x3 area around king).

	// --- Distance Metrics ---
	// Indexed by [square1][square2].
	chebyshevDistance [NumOfSquaresInBoard][NumOfSquaresInBoard]int // Max(abs(df), abs(dr)) - King distance.
	manhattanDistance [NumOfSquaresInBoard][NumOfSquaresInBoard]int // abs(df) + abs(dr) - Rook distance on empty board.

	// --- Other Specific Masks ---
	ringMasks [NumOfSquaresInBoard][4]Bitboard // [square][radius 1..3] Squares exactly 'radius' king moves away. Radius 0 is unused.
)

// PieceValues holds standard piece values (useful for SEE or basic material eval).
// Note: Actual engine values may differ and be tuned. Provided for reference.
// Use the PieceType constants defined in the chess package.
var PieceValues = map[PieceType]int{
	Pawn:   100,
	Knight: 300,   // Often tuned higher, e.g., 320
	Bishop: 320,   // Often tuned higher, e.g., 330
	Rook:   500,   // Often tuned higher, e.g., 500-560
	Queen:  900,   // Often tuned higher, e.g., 900-975
	King:   20000, // Effectively infinite
}

// --- Mappings ---

// IndexToColor converts internal index (0/1) back to chess.Color.
func IndexToColor(idx int) Color {
	if idx == BlackIdx {
		return Black
	}
	return White
}

// ColorToIndex maps chess.Color to internal index (White=0, Black=1).
// Assumes White=1, Black=2 in the chess package. Adjust if different.
func ColorToIndex(c Color) int {
	if c == Black {
		return BlackIdx
	}
	// Default to White for White or NoColor.
	return WhiteIdx
}

// PieceTypeToIndex maps chess.PieceType to internal index (Pawn=0..King=5). Returns -1 for invalid.
// Assumes Pawn=1..King=6 in the chess package. Adjust if different.
func PieceTypeToIndex(pt PieceType) int {
	switch pt {
	case Pawn:
		return PawnIdx
	case Knight:
		return KnightIdx
	case Bishop:
		return BishopIdx
	case Rook:
		return RookIdx
	case Queen:
		return QueenIdx
	case King:
		return KingIdx
	default:
		// Invalid piece type.
		return -1
	}
}

// IndexToPieceType maps internal piece type index (0..5) back to chess.PieceType.
// Assumes Pawn=1..King=6 in the chess package. Adjust if different.
func IndexToPieceType(idx int) PieceType {
	switch idx {
	case PawnIdx:
		return Pawn
	case KnightIdx:
		return Knight
	case BishopIdx:
		return Bishop
	case RookIdx:
		return Rook
	case QueenIdx:
		return Queen
	case KingIdx:
		return King
	default:
		// Invalid index.
		return NoPieceType
	}
}

// --- Initialization ---

// init calls all individual initialization functions for the precomputed tables.
func init() {
	initFileRankMasks()
	initAdjacentFilesMasks() // Needs fileMasks
	initSquareColorMask()
	initForwardRanksMasks()    // Needs rank masks
	initPawnAttacksAndPushes() // Initializes pawnAttacks, pawnPushes, pawnAttackedBy
	initKnightAttacks()        // Initializes knightAttacks
	initKingAttacks()
	initRays()                  // Initializes rays (depends on Border, geometry helpers)
	initLineBetween()           // Uses rays, geometry helpers
	initDiagonalMasks()         // Uses rays
	initSliderAttackPotential() // Uses rank/file/diagonal masks
	initPassedPawnMasks()       // Uses rank/file masks, adjacentFilesMasks, forwardRanksMasks
	initIsolatedPawnMasks()     // Uses fileMasks
	initOutpostCandidateMasks() // Uses rank/file masks
	initPawnFrontSpan()         // Uses rank/file masks, forwardRanksMasks
	initKingZoneMasks()         // Uses kingAttacks
	initKingSafeZoneMasks()     // Uses kingAttacks
	initDistanceTables()
	initRingMasks()           // Uses kingAttacks
	initOutpostSupportMasks() // Uses rank/file masks, pawnAttackedBy
	initBackwardPawnMasks()   // Uses rank/file masks, adjacentFilesMasks
	initPawnShieldMasks()     // Uses rank/file masks
}

// Initializes file and rank masks.
func initFileRankMasks() {
	for f := FileA; f <= FileH; f++ {
		fileMasks[f] = FileABB << f
	}
	for r := Rank1; r <= Rank8; r++ {
		rankMasks[r] = Rank1BB << (r * 8)
	}
}

// Initializes adjacent files masks.
func initAdjacentFilesMasks() {
	for sq := A1; sq <= H8; sq++ {
		f := sq.File()
		mask := EmptyBB
		if f > FileA {
			mask |= fileMasks[f-1]
		}
		if f < FileH {
			mask |= fileMasks[f+1]
		}
		adjacentFilesMasks[sq] = mask
	}
}

// Initializes Square Color Mask.
func initSquareColorMask() {
	for sq := A1; sq <= H8; sq++ {
		if (SquareBB(sq) & LightSquaresBB) != 0 {
			SquareColorMask[sq] = LightSquaresBB
		} else {
			SquareColorMask[sq] = DarkSquaresBB
		}
	}
}

// Initializes forward ranks masks (ranks strictly ahead).
func initForwardRanksMasks() {
	for sq := A1; sq <= H8; sq++ {
		rank := sq.Rank()
		// White
		whiteMask := EmptyBB
		for r := rank + 1; r <= Rank8; r++ {
			whiteMask |= rankMasks[r]
		}
		forwardRanksMasks[WhiteIdx][sq] = whiteMask
		// Black
		blackMask := EmptyBB
		for r := rank - 1; r >= Rank1; r-- {
			blackMask |= rankMasks[r]
		}
		forwardRanksMasks[BlackIdx][sq] = blackMask
	}
}

// Initializes pawn attack, reverse attack, and push tables.
func initPawnAttacksAndPushes() {
	for sq := A1; sq <= H8; sq++ {
		sqBB := SquareBB(sq)
		file := sq.File()
		rank := sq.Rank()

		// --- White Pawns ---
		whiteAttacks := EmptyBB
		if file > FileA { // NW capture
			if rank < Rank8 {
				whiteAttacks |= sqBB << 7
			}
		}
		if file < FileH { // NE capture
			if rank < Rank8 {
				whiteAttacks |= sqBB << 9
			}
		}
		pawnAttacks[WhiteIdx][sq] = whiteAttacks

		singlePushW := EmptyBB
		if rank < Rank8 {
			singlePushW = sqBB << 8
		}
		doublePushW := EmptyBB
		if rank == Rank2 {
			// Check if rank definitions align (Rank2 should be index 1)
			// Adjust if Rank constants are 1-8 instead of 0-7
			doublePushW = sqBB << 16
		}
		pawnPushes[WhiteIdx][sq] = singlePushW | doublePushW

		attackedByWhite := EmptyBB        // Squares a white pawn must be on to attack sq
		if file > FileA && rank > Rank1 { // SW source
			attackedByWhite |= sqBB >> 9
		}
		if file < FileH && rank > Rank1 { // SE source
			attackedByWhite |= sqBB >> 7
		}
		pawnAttackedBy[WhiteIdx][sq] = attackedByWhite

		// --- Black Pawns ---
		blackAttacks := EmptyBB
		if file > FileA { // SW capture
			if rank > Rank1 {
				blackAttacks |= sqBB >> 9
			}
		}
		if file < FileH { // SE capture
			if rank > Rank1 {
				blackAttacks |= sqBB >> 7
			}
		}
		pawnAttacks[BlackIdx][sq] = blackAttacks

		singlePushB := EmptyBB
		if rank > Rank1 {
			singlePushB = sqBB >> 8
		}
		doublePushB := EmptyBB
		if rank == Rank7 {
			// Check if rank definitions align (Rank7 should be index 6)
			doublePushB = sqBB >> 16
		}
		pawnPushes[BlackIdx][sq] = singlePushB | doublePushB

		attackedByBlack := EmptyBB        // Squares a black pawn must be on to attack sq
		if file > FileA && rank < Rank8 { // NW source
			attackedByBlack |= sqBB << 7
		}
		if file < FileH && rank < Rank8 { // NE source
			attackedByBlack |= sqBB << 9
		}
		pawnAttackedBy[BlackIdx][sq] = attackedByBlack
	}
}

// Initializes knight attack tables.
func initKnightAttacks() {
	for sq := A1; sq <= H8; sq++ {
		sqBB := SquareBB(sq)
		attacks := EmptyBB
		// Pattern: +/- 6, 10, 15, 17
		attacks |= (sqBB << 17) & NotAFile  // Up 2, Right 1
		attacks |= (sqBB << 15) & NotHFile  // Up 2, Left 1
		attacks |= (sqBB << 10) & NotABFile // Up 1, Right 2
		attacks |= (sqBB << 6) & NotGHFile  // Up 1, Left 2
		attacks |= (sqBB >> 6) & NotABFile  // Down 1, Right 2
		attacks |= (sqBB >> 10) & NotGHFile // Down 1, Left 2
		attacks |= (sqBB >> 15) & NotAFile  // Down 2, Right 1
		attacks |= (sqBB >> 17) & NotHFile  // Down 2, Left 1
		knightAttacks[sq] = attacks
	}
}

// Initializes king attack tables.
func initKingAttacks() {
	for sq := A1; sq <= H8; sq++ {
		sqBB := SquareBB(sq)
		attacks := EmptyBB
		// Pattern: +/- 1, 7, 8, 9
		attacks |= (sqBB << 9) & NotAFile // NorthEast
		attacks |= sqBB << 8              // North
		attacks |= (sqBB << 7) & NotHFile // NorthWest
		attacks |= (sqBB << 1) & NotAFile // East
		attacks |= (sqBB >> 1) & NotHFile // West
		attacks |= (sqBB >> 7) & NotAFile // SouthEast
		attacks |= sqBB >> 8              // South
		attacks |= (sqBB >> 9) & NotHFile // SouthWest
		kingAttacks[sq] = attacks
	}
}

// Initializes ray attack tables (used for sliders). Rays exclude the starting square.
func initRays() {
	// Steps: N=8, NE=9, E=1, SE=-7, S=-8, SW=-9, W=-1, NW=7
	steps := [NumDirections]int{8, 9, 1, -7, -8, -9, -1, 7}
	for sq := A1; sq <= H8; sq++ {
		for dir := 0; dir < NumDirections; dir++ {
			ray := EmptyBB
			currentSqIdx := int(sq)
			for {
				currentSqIdx += steps[dir]
				// Check bounds
				if currentSqIdx < 0 || currentSqIdx >= NumOfSquaresInBoard {
					break
				}

				nextSq := Square(currentSqIdx)
				// Need previous square to check for wrap-around
				prevSq := Square(currentSqIdx - steps[dir])

				// Check wrap-around using file/rank distance
				df := abs(int(nextSq.File()) - int(prevSq.File()))
				dr := abs(int(nextSq.Rank()) - int(prevSq.Rank()))
				if max(df, dr) > 1 { // Wrapped around edge if dist > 1 (King move)
					break
				}

				ray |= SquareBB(nextSq)
			}
			rays[sq][dir] = ray
		}
	}
}

// Initializes line and between bitboards using precomputed rays.
func initLineBetween() {
	for s1 := A1; s1 <= H8; s1++ {
		for s2 := A1; s2 <= H8; s2++ {
			if s1 == s2 {
				lineBB[s1][s2] = SquareBB(s1)
				betweenBB[s1][s2] = EmptyBB
				continue
			}

			dir := getDirection(s1, s2)
			if dir != -1 { // s1 and s2 are aligned (rank, file, or diagonal)
				// Ray from s1 towards s2
				ray_s1_to_s2 := Ray(s1, dir) // Use Ray func which gets precomputed table
				// Ray from s2 towards s1
				ray_s2_to_s1 := Ray(s2, getOppositeDirection(dir))

				// Squares strictly between s1 and s2 are the intersection of the rays
				between := ray_s1_to_s2 & ray_s2_to_s1
				betweenBB[s1][s2] = between

				// Line including endpoints
				lineBB[s1][s2] = between | SquareBB(s1) | SquareBB(s2)
			} else {
				// Not aligned, line only includes endpoints, between is empty
				lineBB[s1][s2] = SquareBB(s1) | SquareBB(s2)
				betweenBB[s1][s2] = EmptyBB
			}
		}
	}
}

// Initializes diagonal/anti-diagonal masks.
func initDiagonalMasks() {
	for sq := A1; sq <= H8; sq++ {
		sqBB := SquareBB(sq)
		// Diagonal (A1-H8 direction): NE + SW rays + square itself
		diag := Ray(sq, NorthEast) | Ray(sq, SouthWest) | sqBB
		diagonalMasks[sq] = diag

		// Anti-diagonal (A8-H1 direction): NW + SE rays + square itself
		antiDiag := Ray(sq, NorthWest) | Ray(sq, SouthEast) | sqBB
		antiDiagonalMasks[sq] = antiDiag
	}
}

// Initializes slider attack potential masks (squares from which a slider could attack sq).
func initSliderAttackPotential() {
	for sq := A1; sq <= H8; sq++ {
		sqBB := SquareBB(sq)
		// Rook potential: union of its rank/file (excluding the square itself)
		rookPot := (BBRank(sq.Rank()) | BBFile(sq.File())) &^ sqBB // Use Rank/File accessors
		rookAttackPotential[sq] = rookPot

		// Bishop potential: union of its diagonals (excluding the square itself)
		bishopPot := (diagonalMasks[sq] | antiDiagonalMasks[sq]) &^ sqBB
		bishopAttackPotential[sq] = bishopPot

		// Queen potential combines rook and bishop potential
		queenAttackPotential[sq] = rookPot | bishopPot
	}
}

// Initializes passed pawn masks (squares in front on same and adjacent files).
func initPassedPawnMasks() {
	for sq := A1; sq <= H8; sq++ {
		file := sq.File()
		sameAndAdjFiles := BBFile(file) | adjacentFilesMasks[sq] // Use File accessor

		// White: Ranks ahead
		whiteMask := forwardRanksMasks[WhiteIdx][sq]
		passedPawnMasks[WhiteIdx][sq] = whiteMask & sameAndAdjFiles

		// Black: Ranks ahead
		blackMask := forwardRanksMasks[BlackIdx][sq]
		passedPawnMasks[BlackIdx][sq] = blackMask & sameAndAdjFiles
	}
}

// Initializes isolated pawn masks (adjacent files).
func initIsolatedPawnMasks() {
	for f := FileA; f <= FileH; f++ {
		mask := EmptyBB
		if f > FileA {
			mask |= BBFile(f - 1) // Use File accessor
		}
		if f < FileH {
			mask |= BBFile(f + 1) // Use File accessor
		}
		isolatedPawnMasks[f] = mask
	}
}

// Initializes outpost candidate masks (potential squares - rank check + non-edge file).
func initOutpostCandidateMasks() {
	// Ranks 4, 5, 6 for White; Ranks 3, 4, 5 for Black
	whiteOutpostRanks := Rank4BB | Rank5BB | Rank6BB
	blackOutpostRanks := Rank3BB | Rank4BB | Rank5BB
	nonEdgeFiles := NotAFile & NotHFile

	for sq := A1; sq <= H8; sq++ {
		sqBB := SquareBB(sq)
		outpostCandidateMasks[WhiteIdx][sq] = EmptyBB // Initialize to empty
		outpostCandidateMasks[BlackIdx][sq] = EmptyBB // Initialize to empty

		if (sqBB & nonEdgeFiles) != 0 {
			if (sqBB & whiteOutpostRanks) != 0 {
				outpostCandidateMasks[WhiteIdx][sq] = sqBB
			}
			if (sqBB & blackOutpostRanks) != 0 {
				outpostCandidateMasks[BlackIdx][sq] = sqBB
			}
		}
	}
}

// Initializes pawn front span masks (squares strictly ahead on same file).
func initPawnFrontSpan() {
	for sq := A1; sq <= H8; sq++ {
		file := sq.File()
		// White: Ranks ahead
		pawnFrontSpan[WhiteIdx][sq] = forwardRanksMasks[WhiteIdx][sq] & BBFile(file) // Use File accessor
		// Black: Ranks ahead
		pawnFrontSpan[BlackIdx][sq] = forwardRanksMasks[BlackIdx][sq] & BBFile(file) // Use File accessor
	}
}

// Initializes king zone masks (expanded 5x5).
func initKingZoneMasks() {
	for sq := A1; sq <= H8; sq++ {
		// Start with 3x3 zone (king attacks + king square)
		zone3x3 := GetKingAttacks(sq) | SquareBB(sq) // Use GetKingAttacks accessor
		expandedZone := zone3x3
		// Expand outwards by one king step from each square in the 3x3 zone
		tempZone := zone3x3
		for tempZone != EmptyBB {
			s, next, ok := tempZone.PopLSB()
			if !ok {
				break
			}
			tempZone = next
			expandedZone |= GetKingAttacks(s) // Add attacks from the 3x3 squares
		}
		kingZoneMasks[sq] = expandedZone // Results in a 5x5 zone centered at sq
	}
}

// Initializes king safe zone masks (3x3 area around king).
func initKingSafeZoneMasks() {
	for sq := A1; sq <= H8; sq++ {
		kingSafeZoneMasks[sq] = GetKingAttacks(sq) | SquareBB(sq) // Use GetKingAttacks accessor
	}
}

// Initializes all distance tables (Chebyshev, Manhattan).
func initDistanceTables() {
	for s1 := A1; s1 <= H8; s1++ {
		for s2 := A1; s2 <= H8; s2++ {
			f1, r1 := s1.File(), s1.Rank()
			f2, r2 := s2.File(), s2.Rank()
			// Ensure File/Rank return 0-7 indices for calculation
			df := abs(int(f1) - int(f2))
			dr := abs(int(r1) - int(r2))
			chebyshevDistance[s1][s2] = max(df, dr) // King distance
			manhattanDistance[s1][s2] = df + dr     // Rook distance
		}
	}
}

// Initializes ring masks (squares at exact king distance).
func initRingMasks() {
	for sq := A1; sq <= H8; sq++ {
		sqBB := SquareBB(sq)
		// ringMasks[sq][0] is unused/empty
		currentRing := sqBB
		allLowerRings := sqBB
		for radius := 1; radius <= 3; radius++ {
			nextRingBoundary := EmptyBB
			tempCurrentRing := currentRing
			for tempCurrentRing != EmptyBB {
				s, next, ok := tempCurrentRing.PopLSB()
				if !ok {
					break
				}
				tempCurrentRing = next
				// Get squares adjacent (king moves) to current ring
				nextRingBoundary |= GetKingAttacks(s) // Use GetKingAttacks accessor
			}

			// The actual next ring excludes squares already covered
			currentRing = nextRingBoundary &^ allLowerRings
			ringMasks[sq][radius] = currentRing
			allLowerRings |= currentRing // Add this ring to the set of lower rings
		}
	}
}

// Initializes outpost support masks (squares where friendly pawns could support an outpost).
func initOutpostSupportMasks() {
	for sq := A1; sq <= H8; sq++ {
		// White support: Need white pawns diagonally behind on rank-1. Use pawnAttackedBy[WhiteIdx].
		outpostSupportMasks[WhiteIdx][sq] = GetPawnAttackedBy(White, sq) // Use accessor

		// Black support: Need black pawns diagonally behind on rank+1. Use pawnAttackedBy[BlackIdx].
		outpostSupportMasks[BlackIdx][sq] = GetPawnAttackedBy(Black, sq) // Use accessor
	}
}

// Initializes backward pawn masks (squares behind pawn on same & adjacent files).
func initBackwardPawnMasks() {
	for sq := A1; sq <= H8; sq++ {
		sameAndAdjFiles := BBFile(sq.File()) | adjacentFilesMasks[sq] // Use File accessor

		// White: Ranks behind (r-1 down to 1)
		whiteBehindMask := EmptyBB
		for r := sq.Rank() - 1; r >= Rank1; r-- {
			whiteBehindMask |= BBRank(r) // Use Rank accessor
		}
		backwardPawnMasks[WhiteIdx][sq] = whiteBehindMask & sameAndAdjFiles

		// Black: Ranks behind (r+1 up to 8)
		blackBehindMask := EmptyBB
		for r := sq.Rank() + 1; r <= Rank8; r++ {
			blackBehindMask |= BBRank(r) // Use Rank accessor
		}
		backwardPawnMasks[BlackIdx][sq] = blackBehindMask & sameAndAdjFiles
	}
}

// Initializes pawn shield masks (squares on ranks 2/7 on adjacent files relative to king file).
func initPawnShieldMasks() {
	for sq := A1; sq <= H8; sq++ {
		kingFile := sq.File()
		kingRank := sq.Rank() // Assuming Rank returns 0-7

		// Determine shield files (king file +/- 1, plus king file itself)
		shieldFiles := EmptyBB
		if kingFile > FileA {
			shieldFiles |= BBFile(kingFile - 1)
		}
		shieldFiles |= BBFile(kingFile)
		if kingFile < FileH {
			shieldFiles |= BBFile(kingFile + 1)
		}

		// --- White King Shield ---
		// Determine shield ranks relative to king's rank. Default is Ranks 2 & 3 (indices 1 & 2).
		whiteShieldRank2Num := Rank2 // Use Rank constants
		whiteShieldRank3Num := Rank3
		if kingRank == Rank2 { // King on Rank 2 -> shield on Rank 3 & 4
			whiteShieldRank2Num = Rank3
			whiteShieldRank3Num = Rank4
		} else if kingRank >= Rank3 { // King higher up -> shield relative ranks +1, +2
			whiteShieldRank2Num = kingRank + 1
			whiteShieldRank3Num = kingRank + 2
		} // Else: King on Rank 1 -> shield on Rank 2 & 3 (default)

		// Get Rank Bitboards, handling edge cases (king near rank 8)
		whiteShieldRank2BB := EmptyBB
		if whiteShieldRank2Num <= Rank8 {
			whiteShieldRank2BB = BBRank(whiteShieldRank2Num)
		}
		whiteShieldRank3BB := EmptyBB
		if whiteShieldRank3Num <= Rank8 {
			whiteShieldRank3BB = BBRank(whiteShieldRank3Num)
		}

		whiteShield := (whiteShieldRank2BB | whiteShieldRank3BB) & shieldFiles
		whiteFront := whiteShieldRank2BB & shieldFiles // Front is closer rank
		pawnShieldMasks[WhiteIdx][sq] = whiteShield
		pawnShieldFrontMasks[WhiteIdx][sq] = whiteFront

		// --- Black King Shield ---
		// Determine shield ranks relative to king's rank. Default is Ranks 7 & 6 (indices 6 & 5).
		blackShieldRank7Num := Rank7
		blackShieldRank6Num := Rank6
		if kingRank == Rank7 { // King on Rank 7 -> shield on Rank 6 & 5
			blackShieldRank7Num = Rank6
			blackShieldRank6Num = Rank5
		} else if kingRank <= Rank6 { // King lower down -> shield relative ranks -1, -2
			blackShieldRank7Num = kingRank - 1
			blackShieldRank6Num = kingRank - 2
		} // Else: King on Rank 8 -> shield on Rank 7 & 6 (default)

		// Get Rank Bitboards, handling edge cases (king near rank 1)
		blackShieldRank7BB := EmptyBB
		if blackShieldRank7Num >= Rank1 {
			blackShieldRank7BB = BBRank(blackShieldRank7Num)
		}
		blackShieldRank6BB := EmptyBB
		if blackShieldRank6Num >= Rank1 {
			blackShieldRank6BB = BBRank(blackShieldRank6Num)
		}

		blackShield := (blackShieldRank7BB | blackShieldRank6BB) & shieldFiles
		blackFront := blackShieldRank7BB & shieldFiles // Front is closer rank
		pawnShieldMasks[BlackIdx][sq] = blackShield
		pawnShieldFrontMasks[BlackIdx][sq] = blackFront
	}
}

// --- Geometric Helpers ---

// FlipVertical mirrors the square vertically (Rank 1 <-> Rank 8).
// sq ^ 56 == sq ^ (7 * 8)
func FlipVertical(sq Square) Square {
	// Ensure sq is valid before XORing
	if sq < A1 || sq > H8 {
		return NoSquare // Or some other indicator of invalid input
	}
	return sq ^ 56
}

// FlipHorizontal mirrors the square horizontally (File A <-> File H).
// sq ^ 7
func FlipHorizontal(sq Square) Square {
	if sq < A1 || sq > H8 {
		return NoSquare
	}
	return sq ^ 7
}

// FlipDiagonal mirrors the square diagonally along the A1-H8 diagonal.
// This flips file and rank: sq'(f,r) = sq(r,f).
// The bitwise operation sq ^ 63 doesn't achieve this directly.
// Use: return NewSquare(File(sq.Rank()), Rank(sq.File()))
// The provided sq ^ 63 seems incorrect for A1-H8 diagonal transpose.
// Let's implement the correct file/rank swap.
func FlipDiagonal(sq Square) Square {
	if sq < A1 || sq > H8 {
		return NoSquare
	}
	// Correct transpose: file index becomes rank index, rank index becomes file index
	return NewSquare(File(sq.Rank()), Rank(sq.File()))
}

// SquareColor returns the color of the square (White for light, Black for dark).
// Assumes A1 is dark. Uses precomputed LightSquaresBB.
func SquareColor(sq Square) Color {
	if sq < A1 || sq > H8 {
		return NoColor // Indicate invalid square
	}
	if (SquareBB(sq) & LightSquaresBB) != 0 {
		return White // Light Square
	}
	return Black // Dark Square
}

// IsPositiveRayDir checks if a direction index corresponds to a positive shift (N, NE, E, NW).
func IsPositiveRayDir(dir int) bool {
	// Positive directions generally increase the square index
	return dir == North || dir == NorthEast || dir == East || dir == NorthWest
}

// File returns a bitboard mask for the given file.
func BBFile(f File) Bitboard {
	if f >= FileA && f <= FileH {
		return fileMasks[f]
	}
	return EmptyBB
}

// Rank returns a bitboard mask for the given rank.
func BBRank(r Rank) Bitboard {
	if r >= Rank1 && r <= Rank8 {
		return rankMasks[r]
	}
	return EmptyBB
}

// --- Helper Functions ---

// abs returns the absolute value of an integer.
func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

// max returns the maximum of two integers.
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// min returns the minimum of two integers.
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// getDirection determines the direction index from square s1 to s2, or -1 if not aligned.
func getDirection(s1, s2 Square) int {
	if s1 == s2 || s1 < A1 || s1 > H8 || s2 < A1 || s2 > H8 {
		return -1 // No direction if same square or invalid
	}
	f1, r1 := s1.File(), s1.Rank()
	f2, r2 := s2.File(), s2.Rank()
	df, dr := int(f2)-int(f1), int(r2)-int(r1)

	// Check cardinal directions first
	if df == 0 { // Vertical
		if dr > 0 {
			return North
		}
		if dr < 0 {
			return South
		}
	}
	if dr == 0 { // Horizontal
		if df > 0 {
			return East
		}
		if df < 0 {
			return West
		}
	}
	// Check diagonals
	if abs(df) == abs(dr) {
		if dr > 0 { // North component
			if df > 0 {
				return NorthEast
			}
			// df < 0 implied
			return NorthWest
		}
		// dr < 0 implied (South component)
		if df > 0 {
			return SouthEast
		}
		// df < 0 implied
		return SouthWest
	}

	return -1 // Not aligned
}

// getOppositeDirection returns the opposite direction index.
func getOppositeDirection(dir int) int {
	if dir < 0 || dir >= NumDirections {
		return -1 // Invalid direction
	}
	// Directions are ordered such that opposite is +4 mod 8
	return (dir + 4) % NumDirections
}

// --- Bitboard Manipulation ---

// SquareBB returns a bitboard with only the given square set. Returns EmptyBB for invalid squares.
func SquareBB(sq Square) Bitboard {
	if sq >= A1 && sq <= H8 {
		return 1 << sq
	}
	return EmptyBB
}

// Set sets the bit corresponding to the square. Handles invalid squares.
func (b Bitboard) Set(sq Square) Bitboard { return b | SquareBB(sq) }

// Clear clears the bit corresponding to the square. Handles invalid squares.
func (b Bitboard) Clear(sq Square) Bitboard { return b &^ SquareBB(sq) }

// Toggle toggles the bit corresponding to the square. Handles invalid squares.
func (b Bitboard) Toggle(sq Square) Bitboard { return b ^ SquareBB(sq) }

// Occupied checks if the square is occupied (bit is set). Handles invalid squares.
func (b Bitboard) Occupied(sq Square) bool {
	return (b & SquareBB(sq)) != 0
}

// IsEmpty checks if the bitboard is empty.
func (b Bitboard) IsEmpty() bool { return b == 0 }

// PopCount counts the number of set bits.
func (b Bitboard) PopCount() int { return bits.OnesCount64(uint64(b)) }

// LSB finds the index of the least significant bit. Returns (square, true) or (NoSquare, false).
func (b Bitboard) LSB() (Square, bool) {
	if b == 0 {
		return NoSquare, false
	}
	// bits.TrailingZeros64 is undefined for input 0, hence the check above.
	sq := Square(bits.TrailingZeros64(uint64(b)))
	return sq, true
}

// MSB finds the index of the most significant bit. Returns (square, true) or (NoSquare, false).
func (b Bitboard) MSB() (Square, bool) {
	if b == 0 {
		return NoSquare, false
	}
	// bits.LeadingZeros64 is undefined for input 0.
	sq := Square(NumOfSquaresInBoard - 1 - bits.LeadingZeros64(uint64(b)))
	return sq, true
}

// PopLSB finds and removes the least significant bit. Returns (square, new bitboard, true) or (NoSquare, original bitboard, false).
func (b Bitboard) PopLSB() (Square, Bitboard, bool) {
	if b == 0 {
		return NoSquare, b, false
	}
	lsbIndex := bits.TrailingZeros64(uint64(b))
	sq := Square(lsbIndex)
	// b & (b-1) clears the LSB
	return sq, b & (b - 1), true
}

// PopMSB finds and removes the most significant bit. Returns (square, new bitboard, true) or (NoSquare, original bitboard, false).
func (b Bitboard) PopMSB() (Square, Bitboard, bool) {
	sq, ok := b.MSB()
	if !ok {
		return NoSquare, b, false
	}
	// Clear the MSB using XOR or AND NOT
	return sq, b ^ SquareBB(sq), true
	// Alt: return sq, b &^ SquareBB(sq), true
}

// Ray returns the precomputed ray from sq in direction dir (excluding sq).
func Ray(sq Square, dir int) Bitboard {
	if sq < A1 || sq > H8 || dir < 0 || dir >= NumDirections {
		return EmptyBB
	}
	return rays[sq][dir]
}

// Line returns the precomputed line between s1 and s2 (inclusive).
func Line(s1, s2 Square) Bitboard {
	if s1 < A1 || s1 > H8 || s2 < A1 || s2 > H8 {
		return EmptyBB
	}
	return lineBB[s1][s2]
}

// Between returns the precomputed squares strictly between s1 and s2.
func Between(s1, s2 Square) Bitboard {
	if s1 < A1 || s1 > H8 || s2 < A1 || s2 > H8 {
		return EmptyBB
	}
	return betweenBB[s1][s2]
}

// Scan returns a slice of all squares corresponding to set bits, ordered LSB to MSB.
func (b Bitboard) Scan() []Square {
	count := b.PopCount()
	if count == 0 {
		return []Square{} // Return nil or empty slice? Empty slice is safer.
	}
	squares := make([]Square, 0, count)
	tempBB := b
	for tempBB != 0 {
		sq, next, ok := tempBB.PopLSB()
		if !ok {
			break // Should not happen if tempBB != 0
		}
		squares = append(squares, sq)
		tempBB = next
	}
	return squares
}

// Squares is an alias for Scan().
func (b Bitboard) Squares() []Square { return b.Scan() }

// String returns the 64-bit binary string representation (MSB=H8, LSB=A1).
func (b Bitboard) String() string {
	// Format so index 63 (H8) is printed first, down to index 0 (A1).
	var sb strings.Builder
	for i := 63; i >= 0; i-- {
		if (uint64(b)>>i)&1 == 1 {
			sb.WriteByte('1')
		} else {
			sb.WriteByte('0')
		}
	}
	return sb.String()
}

// Draw returns a string visually representing the bitboard on a chessboard grid.
func (b Bitboard) Draw() string {
	var sb strings.Builder
	sb.WriteString("\n  a b c d e f g h\n")
	for r := Rank8; r >= Rank1; r-- { // Iterate ranks H->A (visual top to bottom)
		sb.WriteString(fmt.Sprintf("%d ", r+1)) // Rank number
		for f := FileA; f <= FileH; f++ {       // Iterate files A->H (visual left to right)
			sq := NewSquare(f, r)
			if b.Occupied(sq) {
				sb.WriteString("X ")
			} else {
				sb.WriteString(". ")
			}
		}
		sb.WriteString(fmt.Sprintf("%d\n", r+1)) // Rank number again
	}
	sb.WriteString("  a b c d e f g h\n")
	return sb.String()
}

// Reverse reverses the bits of the bitboard (A1 <-> H8).
func (b Bitboard) Reverse() Bitboard { return Bitboard(bits.Reverse64(uint64(b))) }

// And performs a bitwise AND operation.
func (b Bitboard) And(other Bitboard) Bitboard { return b & other }

// Or performs a bitwise OR operation.
func (b Bitboard) Or(other Bitboard) Bitboard { return b | other }

// Xor performs a bitwise XOR operation.
func (b Bitboard) Xor(other Bitboard) Bitboard { return b ^ other }

// Not performs a bitwise NOT operation.
func (b Bitboard) Not() Bitboard { return ^b }

// AndNot performs a bitwise AND NOT operation (b & ~other).
func (b Bitboard) AndNot(other Bitboard) Bitboard { return b &^ other }

// Shift shifts the bitboard left (positive) or right (negative).
func (b Bitboard) Shift(amount int) Bitboard {
	if amount >= 0 {
		return b << amount
	}
	// Ensure positive shift amount for right shift
	return b >> (-amount)
}

// --- Static Attack Generation Functions ---

// GetPawnAttacks returns the squares attacked by a pawn of 'color' on 'sq'.
func GetPawnAttacks(sq Square, color Color) Bitboard {
	if sq < A1 || sq > H8 {
		return EmptyBB
	}
	return pawnAttacks[ColorToIndex(color)][sq]
}

// GetPawnAttackedBy returns the squares where a pawn of 'color' would attack 'sq'.
func GetPawnAttackedBy(color Color, sq Square) Bitboard {
	if sq < A1 || sq > H8 {
		return EmptyBB
	}
	return pawnAttackedBy[ColorToIndex(color)][sq]
}

// GetKnightAttacks returns the squares attacked by a knight on 'sq'.
func GetKnightAttacks(sq Square) Bitboard {
	if sq < A1 || sq > H8 {
		return EmptyBB
	}
	return knightAttacks[sq]
}

// GetKingAttacks returns the squares attacked by a king on 'sq'.
func GetKingAttacks(sq Square) Bitboard {
	if sq < A1 || sq > H8 {
		return EmptyBB
	}
	return kingAttacks[sq]
}

// generateSliderAttacks generates Rook, Bishop, or Queen attacks from sq, considering blockers.
// dirs is a slice of direction indices (e.g., {North, East, South, West} for Rook).
// Uses precomputed rays. This is a standard approach ("classical").
// For higher performance, consider "Magic Bitboards" (requires complex setup).
func generateSliderAttacks(sq Square, blockers Bitboard, dirs []int) Bitboard {
	if sq < A1 || sq > H8 {
		return EmptyBB
	}
	attacks := EmptyBB
	for _, dir := range dirs {
		if dir < 0 || dir >= NumDirections {
			continue // Skip invalid directions
		}
		ray := Ray(sq, dir)          // Precomputed ray (excludes sq)
		blockedRay := ray & blockers // Find blockers along the ray

		if blockedRay != 0 {
			var blockerSq Square
			var ok bool

			// Find the first blocker along the ray
			if IsPositiveRayDir(dir) { // Positive directions (increasing square index)
				blockerSq, ok = blockedRay.LSB()
			} else { // Negative directions (decreasing square index)
				blockerSq, ok = blockedRay.MSB()
			}

			if ok {
				// Attacks include squares up to and including the blocker.
				// Ray(sq, dir) is the full ray *from* sq.
				// Ray(blockerSq, dir) is the full ray *from* the blocker square.
				// Attack = ray_from_sq AND NOT ray_from_blocker
				// This includes the blocker square itself.
				attacks |= ray &^ Ray(blockerSq, dir)
			}
			// If !ok, blockedRay was non-empty but LSB/MSB failed - should not happen.
		} else {
			// No blockers, the entire ray is attacked
			attacks |= ray
		}
	}
	return attacks
}

// GenerateRookAttacks calculates rook attacks from a square, considering blockers.
// NOTE: For optimal performance, consider implementing Magic Bitboards.
func GenerateRookAttacks(sq Square, blockers Bitboard) Bitboard {
	// TODO: Replace with Magic Bitboard lookup if implemented.
	return generateSliderAttacks(sq, blockers, []int{North, East, South, West})
}

// GenerateBishopAttacks calculates bishop attacks from a square, considering blockers.
// NOTE: For optimal performance, consider implementing Magic Bitboards.
func GenerateBishopAttacks(sq Square, blockers Bitboard) Bitboard {
	// TODO: Replace with Magic Bitboard lookup if implemented.
	return generateSliderAttacks(sq, blockers, []int{NorthEast, SouthEast, SouthWest, NorthWest})
}

// GenerateQueenAttacks calculates queen attacks from a square, considering blockers.
// NOTE: For optimal performance, consider implementing Magic Bitboards.
func GenerateQueenAttacks(sq Square, blockers Bitboard) Bitboard {
	// TODO: Replace with combined Rook/Bishop Magic Bitboard lookup if implemented.
	// Combine rook and bishop directions using the standard ray method for now.
	// return generateSliderAttacks(sq, blockers, []int{North, East, South, West, NorthEast, SouthEast, SouthWest, NorthWest})
	// Or simply:
	return GenerateRookAttacks(sq, blockers) | GenerateBishopAttacks(sq, blockers)
}

// GeneratePawnPushes generates single and double pawn pushes (does NOT check blockers).
func GeneratePawnPushes(pawns Bitboard, color Color) Bitboard {
	pushes := EmptyBB
	tempPawns := pawns
	cIdx := ColorToIndex(color)
	for tempPawns != EmptyBB {
		sq, next, ok := tempPawns.PopLSB()
		if !ok {
			break
		}
		tempPawns = next
		if sq >= A1 && sq <= H8 { // Check validity, though PopLSB should yield valid
			pushes |= pawnPushes[cIdx][sq]
		}
	}
	return pushes
}

// GeneratePawnAttacks generates attacks for all pawns of a given color.
func GeneratePawnAttacks(pawns Bitboard, color Color) Bitboard {
	attacks := EmptyBB
	tempPawns := pawns
	cIdx := ColorToIndex(color)
	for tempPawns != EmptyBB {
		sq, next, ok := tempPawns.PopLSB()
		if !ok {
			break
		}
		tempPawns = next
		if sq >= A1 && sq <= H8 {
			attacks |= pawnAttacks[cIdx][sq]
		}
	}
	return attacks
}

// GenerateAllAttacks generates a bitboard of all squares attacked by a given color's pieces.
// Takes bitboards for each piece type of the attacking color and the board's occupied state.
func GenerateAllAttacks(occupied Bitboard, color Color, pawns, knights, bishops, rooks, queens, king Bitboard) Bitboard {
	allAttacks := EmptyBB

	// Pawns
	allAttacks |= GeneratePawnAttacks(pawns, color)

	// Knights
	tempKnights := knights
	for tempKnights != EmptyBB {
		sq, next, ok := tempKnights.PopLSB()
		if !ok {
			break
		}
		tempKnights = next
		allAttacks |= GetKnightAttacks(sq) // Knight attacks don't depend on blockers
	}

	// Bishops & Queen-Bishops (Diagonal part of Queen)
	bishopsAndQueens := bishops | queens
	tempBishops := bishopsAndQueens
	for tempBishops != EmptyBB {
		sq, next, ok := tempBishops.PopLSB()
		if !ok {
			break
		}
		tempBishops = next
		allAttacks |= GenerateBishopAttacks(sq, occupied)
	}

	// Rooks & Queen-Rooks (Orthogonal part of Queen)
	rooksAndQueens := rooks | queens
	tempRooks := rooksAndQueens
	for tempRooks != EmptyBB {
		sq, next, ok := tempRooks.PopLSB()
		if !ok {
			break
		}
		tempRooks = next
		allAttacks |= GenerateRookAttacks(sq, occupied)
	}

	// King
	// Should only be one king, but loop handles potential future variants / debugging
	tempKing := king
	for tempKing != EmptyBB {
		sq, next, ok := tempKing.PopLSB()
		if !ok {
			break
		}
		tempKing = next
		allAttacks |= GetKingAttacks(sq) // King attacks don't depend on blockers
	}

	return allAttacks
}

// GetAttackersTo returns a bitboard of pieces of 'attackerColor' attacking 'targetSq', considering 'occupied' board state.
// Takes bitboards for each piece type of the attacking color.
// This is the primary function to find attackers to a square in a real position.
func GetAttackersTo(targetSq Square, attackerColor Color, occupied Bitboard,
	pawns, knights, bishops, rooks, queens, king Bitboard) Bitboard {

	if targetSq < A1 || targetSq > H8 {
		return EmptyBB
	}

	attackers := EmptyBB

	// Check pawn attackers (find pawns on squares from which they attack targetSq)
	attackers |= GetPawnAttackedBy(attackerColor, targetSq) & pawns

	// Check knight attackers (find knights on squares from which they attack targetSq)
	attackers |= GetKnightAttacks(targetSq) & knights // Knight attacks are symmetrical

	// Check bishop/queen diagonal attackers
	// Generate bishop attacks *from* targetSq, intersect with B/Q of attacker color
	bishopQueenAttackers := GenerateBishopAttacks(targetSq, occupied) & (bishops | queens)
	attackers |= bishopQueenAttackers

	// Check rook/queen rank/file attackers
	// Generate rook attacks *from* targetSq, intersect with R/Q of attacker color
	rookQueenAttackers := GenerateRookAttacks(targetSq, occupied) & (rooks | queens)
	attackers |= rookQueenAttackers

	// Check king attackers
	attackers |= GetKingAttacks(targetSq) & king

	return attackers
}

// --- Tactical/Evaluation Helpers ---
// These functions operate statically on bitboards provided as arguments.

// IsPassedPawn checks if a pawn on 'pawnSq' of 'color' is passed.
// Requires bitboards of friendly and opponent pawns.
func IsPassedPawn(pawnSq Square, color Color, friendlyPawns, opponentPawns Bitboard) bool {
	if pawnSq < A1 || pawnSq > H8 || !SquareBB(pawnSq).And(friendlyPawns).Occupied(pawnSq) {
		return false // Invalid square or no friendly pawn on square
	}
	cIdx := ColorToIndex(color)
	// Check if there are any opponent pawns in the passed pawn mask (front + adjacent files ahead)
	return (passedPawnMasks[cIdx][pawnSq] & opponentPawns) == EmptyBB
}

// PinnedPieces identifies pieces of 'color' pinned to their king.
// kingSqBB should be the Bitboard containing only the king of 'color'.
// potentialPins should be the bitboard of the pieces of 'color' that could potentially be pinned (usually all pieces except the king).
// oppRQ is opponent Rooks | Queens. oppBQ is opponent Bishops | Queens.
func PinnedPieces(color Color, occupied, kingSqBB, potentialPins, oppRQ, oppBQ Bitboard) Bitboard {
	kingSq, ok := kingSqBB.LSB()
	if !ok {
		return EmptyBB // No king, no pins
	}

	pinned := EmptyBB

	// Check for potential pins by opponent Rooks/Queens along rank/file
	// Potential pinners are opponent R/Q on the same rank/file as the king
	potentialPinnersRQ := rookAttackPotential[kingSq] & oppRQ
	tempPinnersRQ := potentialPinnersRQ
	for tempPinnersRQ != EmptyBB {
		pinnerSq, next, _ := tempPinnersRQ.PopLSB()
		tempPinnersRQ = next

		// Squares strictly between the king and the potential pinner
		between := Between(kingSq, pinnerSq) // Use Between precomputed table

		// Only consider if exactly one piece lies between them
		piecesBetween := between & occupied // Use the full occupancy board
		if piecesBetween.PopCount() == 1 {
			// If that one piece is one of the potential pins (of the correct color), it's pinned
			pinnedPiece := piecesBetween & potentialPins
			pinned |= pinnedPiece
		}
	}

	// Check for potential pins by opponent Bishops/Queens along diagonals
	potentialPinnersBQ := bishopAttackPotential[kingSq] & oppBQ
	tempPinnersBQ := potentialPinnersBQ
	for tempPinnersBQ != EmptyBB {
		pinnerSq, next, _ := tempPinnersBQ.PopLSB()
		tempPinnersBQ = next

		between := Between(kingSq, pinnerSq) // Use Between precomputed table
		piecesBetween := between & occupied
		if piecesBetween.PopCount() == 1 {
			pinnedPiece := piecesBetween & potentialPins
			pinned |= pinnedPiece
		}
	}

	return pinned
}

// Checkers identifies pieces giving check to the king of 'color'.
// This is equivalent to GetAttackersTo targeting the king square with opponentColor's pieces.
// kingSqBB is the Bitboard of the king being checked.
// oppP, oppN, oppB, oppR, oppQ are the bitboards for the opponent's pieces checking the king.
func Checkers(color Color, occupied, kingSqBB Bitboard, oppP, oppN, oppB, oppR, oppQ Bitboard) Bitboard {
	kingSq, ok := kingSqBB.LSB()
	if !ok {
		return EmptyBB // No king, no check
	}

	opponentColor := color.Other()
	// Find pieces of opponentColor attacking kingSq.
	// Pass EmptyBB for opponent King as it cannot deliver check directly.
	return GetAttackersTo(kingSq, opponentColor, occupied, oppP, oppN, oppB, oppR, oppQ, EmptyBB)
}

// --- Eval Mask Accessors ---
// Provide public functions to access precomputed evaluation masks if needed outside the package.

// AdjacentFilesMask returns the mask for files adjacent to the given square's file.
func AdjacentFilesMask(sq Square) Bitboard {
	if sq < A1 || sq > H8 {
		return EmptyBB
	}
	return adjacentFilesMasks[sq]
}

// PassedPawnMask returns the mask used to check if a pawn is passed.
func PassedPawnMask(sq Square, color Color) Bitboard {
	if sq < A1 || sq > H8 {
		return EmptyBB
	}
	return passedPawnMasks[ColorToIndex(color)][sq]
}

// KingPawnShieldMask returns the pawn shield mask for a king on sq.
func KingPawnShieldMask(sq Square, color Color) Bitboard {
	if sq < A1 || sq > H8 {
		return EmptyBB
	}
	return pawnShieldMasks[ColorToIndex(color)][sq]
}

// ManhattanDistance returns the Manhattan distance between two squares.
func ManhattanDistance(s1, s2 Square) int {
	if s1 < A1 || s1 > H8 || s2 < A1 || s2 > H8 {
		return 14 // Max possible
	}
	return manhattanDistance[s1][s2]
}

// ChebyshevDistance returns the Chebyshev (King) distance between two squares.
func ChebyshevDistance(s1, s2 Square) int {
	if s1 < A1 || s1 > H8 || s2 < A1 || s2 > H8 {
		return 7 // Max possible
	}
	return chebyshevDistance[s1][s2]
}

// CenterMask returns the mask for the central 4 squares (d4, e4, d5, e5).
func CenterMask() Bitboard {
	return CenterFourMask
}
