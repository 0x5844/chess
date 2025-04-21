package opening

import (
	"bytes"
	"encoding/csv"
	"errors"
	"fmt"

	_ "embed"

	"github.com/0x5844/chess"
)

//go:embed eco_lichess.tsv
var ecoData []byte

const (
	ecoColumnCode   = 0
	ecoColumnTitle  = 1
	ecoColumnPGN    = 2 // Standard Algebraic Notation (SAN)
	expectedColumns = 3 // Number of columns expected in the TSV data
)

// BookECO represents the Encyclopedia of Chess Openings https://en.wikipedia.org/wiki/Encyclopaedia_of_Chess_Openings
// BookECO is safe for concurrent use.
type BookECO struct {
	root             *node
	startingPosition *chess.Position
}

// node represents a position within the opening tree.
type node struct {
	parent   *node
	children map[string]*node // Keyed by move string (e.g., "e2e4")
	opening  *Opening         // Opening defined at this exact position (if any)
	pos      *chess.Position  // The board position after the move leading to this node
}

// NewBookECO creates and initializes a new BookECO from the embedded ECO data.
// It parses the TSV data and builds the opening tree.
func NewBookECO() (*BookECO, error) {
	// Initialize starting position
	startingPos := chess.StartingPosition()

	b := &BookECO{
		root: &node{
			children: make(map[string]*node),
			pos:      startingPos,
		},
		startingPosition: startingPos,
	}

	// Read and parse embedded TSV data
	reader := csv.NewReader(bytes.NewBuffer(ecoData))
	reader.Comma = '\t' // TSV
	reader.FieldsPerRecord = -1

	records, err := reader.ReadAll()
	if err != nil {
		return nil, fmt.Errorf("failed to read ECO data: %w", err)
	}

	for i, row := range records {
		if i == 0 {
			continue // Skip header row
		}
		if len(row) < expectedColumns {
			if len(row) <= ecoColumnPGN {
				fmt.Printf("warning: skipping ECO record %d due to insufficient columns (%d)\n", i+1, len(row))
				continue
			}
		}

		ecoCode := row[ecoColumnCode]
		title := row[ecoColumnTitle]
		pgnSAN := row[ecoColumnPGN]

		game, err := parseGameFromSAN(pgnSAN)
		if err != nil {
			fmt.Printf("warning: skipping ECO entry %q (%s) due to PGN parse error: %v\n", title, ecoCode, err)
			continue
		}

		moves := game.Moves()
		if len(moves) == 0 {
			continue
		}

		opening := &Opening{
			code:  ecoCode,
			title: title,
			pgn:   pgnSAN,
			moves: moves,
		}

		// Insert the opening into the tree
		if err := b.insert(opening, moves); err != nil {
			return nil, fmt.Errorf("failed to insert opening %q (%s): %w", title, ecoCode, err)
		}
	}

	if len(b.root.children) == 0 {
		return nil, errors.New("failed to load any valid openings from ECO data")
	}

	return b, nil
}

// parseGameFromSAN attempts to parse a PGN string in Standard Algebraic Notation (SAN)
func parseGameFromSAN(pgnSAN string) (*chess.Game, error) {
	pgnReader := bytes.NewBufferString(pgnSAN)
	pgn, err := chess.PGN(pgnReader)
	if err != nil {
		return nil, fmt.Errorf("chess.PGN parsing failed: %w", err)
	}
	game := chess.NewGame(pgn, chess.UseNotation(chess.AlgebraicNotation{}))
	return game, nil
}

// insert adds an opening to the tree structure.
// It takes the Opening object and the pre-parsed sequence of moves.
func (b *BookECO) insert(o *Opening, moves []*chess.Move) error {
	currentNode := b.root
	currentPos := b.startingPosition

	for i, move := range moves {
		// Ensure the move is valid from the current position
		isValid := false
		for _, validMove := range currentPos.ValidMoves() {
			if validMove.String() == move.String() {
				isValid = true
				break
			}
		}
		if !isValid {
			return fmt.Errorf("move %d (%s) is invalid for opening %q (%s) at position %s",
				i+1, move.String(), o.title, o.code, currentPos.String())
		}

		// Apply the move to get the next position
		nextPos := currentPos.Update(move)
		moveStr := move.String()

		childNode, exists := currentNode.children[moveStr]
		if !exists {
			childNode = &node{
				parent:   currentNode,
				children: make(map[string]*node),
				pos:      nextPos,
			}
			currentNode.children[moveStr] = childNode
		}

		// Move to the next node and position
		currentNode = childNode
		currentPos = nextPos
	}

	// Mark the final node with the opening data
	if currentNode.opening != nil {
		fmt.Printf("warning: overwriting opening %q with %q at the same position\n", currentNode.opening.Title(), o.Title())
	}
	currentNode.opening = o

	return nil
}

// Find implements the Book interface
func (b *BookECO) Find(moves []*chess.Move) *Opening {
	for n := b.followPath(b.root, moves); n != nil; n = n.parent {
		if n.opening != nil {
			return n.opening
		}
	}
	return nil
}

// Possible implements the Book interface
func (b *BookECO) Possible(moves []*chess.Move) []*Opening {
	n := b.followPath(b.root, moves)
	openings := []*Opening{}
	for _, n := range b.nodeList(n) {
		if n.opening != nil {
			openings = append(openings, n.opening)
		}
	}
	return openings
}

func (b *BookECO) followPath(n *node, moves []*chess.Move) *node {
	if len(moves) == 0 {
		return n
	}
	c, ok := n.children[moves[0].String()]
	if !ok {
		return n
	}
	return b.followPath(c, moves[1:])
}

func (b *BookECO) nodes(root *node, ch chan *node) {
	ch <- root
	for _, c := range root.children {
		b.nodes(c, ch)
	}
}

func (b *BookECO) nodeList(root *node) []*node {
	ch := make(chan *node)
	go func() {
		b.nodes(root, ch)
		close(ch)
	}()
	nodes := []*node{}
	for n := range ch {
		nodes = append(nodes, n)
	}
	return nodes
}
