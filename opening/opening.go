// Package opening implements chess opening determination and exploration.
package opening

import (
	"bytes"
	"fmt"

	"github.com/0x5844/chess"
)

// A Opening represents a specific sequence of moves from the staring position.
type Opening struct {
	code  string
	title string
	pgn   string
	moves []*chess.Move
	game  *chess.Game
}

// Code returns the Encyclopaedia of Chess Openings (ECO) code.
func (o *Opening) Code() string {
	return o.code
}

// Title returns the Encyclopaedia of Chess Openings (ECO) title of the opening.
func (o *Opening) Title() string {
	return o.title
}

// PGN returns the opening in PGN format.
func (o *Opening) PGN() string {
	return o.pgn
}

// Moves returns the sequence of chess moves defining the opening.
func (o *Opening) Moves() []*chess.Move {
	// Return a copy to prevent external modification
	m := make([]*chess.Move, len(o.moves))
	copy(m, o.moves)
	return m
}

// Game returns the opening as a playable chess.Game instance.
func (o *Opening) Game() (*chess.Game, error) {
	if o.game == nil {
		// Use the stored PGN which should be valid SAN
		pgnReader := bytes.NewBufferString(o.pgn)
		pgn, err := chess.PGN(pgnReader)
		if err != nil {
			// This indicates an issue with the PGN data loaded initially
			return nil, fmt.Errorf("failed to parse PGN for opening %q (%s): %w", o.title, o.code, err)
		}
		o.game = chess.NewGame(pgn) // Assumes chess.NewGame handles PGN correctly
	}
	return o.game, nil
}

// Book is an opening book that returns openings for move sequences
type Book interface {
	// Find returns the most specific opening for the list of moves.  If no opening is found, Find returns nil.
	Find(moves []*chess.Move) *Opening
	// Possible returns the possible openings after the moves given.  If moves is empty or nil all openings are returned.
	Possible(moves []*chess.Move) []*Opening
}
