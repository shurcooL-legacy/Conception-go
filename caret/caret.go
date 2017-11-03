// Package caret contains functionality related to text caret state and operations.
package caret

import (
	"sort"
	"strings"

	"github.com/go-gl/mathgl/mgl64"
	intmath "github.com/pkg/math"
	"github.com/shurcooL-legacy/Conception-go/pkg/gist7802150"
)

type ContentLine interface {
	Start() uint32
	End() uint32
	Length() uint32
}

type MultilineContentI interface {
	Content() string
	LenContent() int
	LongestLine() uint32 // Line length

	Line(lineIndex int) ContentLine
	LenLines() int

	gist7802150.ViewGroupI
}

// =====

func ExpandedLength(s string, currentAdvance uint32) (expandedLineLength uint32) {
	segments := strings.Split(s, "\t")
	var advance uint32 = currentAdvance
	for segmentIndex, segment := range segments {
		advance += uint32(len(segment))
		if segmentIndex != len(segments)-1 {
			advance += 4 - (advance % 4)
		}
	}
	return advance - currentAdvance
}

func ExpandedToLogical(s string, expanded uint32) uint32 {
	var logical uint32
	var advance uint32
	var smallestDifference int32 = int32(expanded) - 0
	for charIndex, char := range []byte(s) {
		if char == '\t' {
			advance += 4 - (advance % 4)
		} else {
			advance++
		}

		difference := int32(advance) - int32(expanded)
		if difference < 0 {
			difference *= -1
		}
		if difference < smallestDifference {
			smallestDifference = difference
			logical = uint32(charIndex + 1)
		}
	}

	return logical
}

// ---

// TODO: Rename.
type caretPositionInternal struct {
	w                  MultilineContentI
	lineIndex          int
	positionWithinLine uint32
	targetExpandedX    uint32

	gist7802150.DepNode2Manual
}

func (cp *caretPositionInternal) NotifyContentChanged() {
	if cp.lineIndex > cp.w.LenLines()-1 {
		cp.Move(+3)
	} else if cp.positionWithinLine > cp.w.Line(cp.lineIndex).Length() {
		cp.Move(+2)
	}
}

func (cp *caretPositionInternal) Logical() uint32 {
	return cp.w.Line(cp.lineIndex).Start() + cp.positionWithinLine
}

func (cp *caretPositionInternal) tryMoveH(direction direction, jumpWords bool) {
	switch direction {
	case Backward:
		if cp.Logical() >= 1 {
			if jumpWords {
				// Skip spaces to the left
				LookAt := cp.Logical()
				for LookAt > 0 && !isCoreCharacter(cp.w.Content()[LookAt-1]) {
					LookAt--
				}
				// Skip non-spaces to the left
				for LookAt > 0 && isCoreCharacter(cp.w.Content()[LookAt-1]) {
					LookAt--
				}

				cp.willMoveH(int32(LookAt) - int32(cp.Logical()))
			} else {
				cp.willMoveH(-1)
			}
		}
	case Forward:
		if cp.Logical() < uint32(cp.w.LenContent()) {
			if jumpWords {
				// Skip spaces to the right
				LookAt := cp.Logical()
				for LookAt < uint32(cp.w.LenContent()) && !isCoreCharacter(cp.w.Content()[LookAt]) {
					LookAt++
				}
				// Skip non-spaces to the right
				for LookAt < uint32(cp.w.LenContent()) && isCoreCharacter(cp.w.Content()[LookAt]) {
					LookAt++
				}

				cp.willMoveH(int32(LookAt) - int32(cp.Logical()))
			} else {
				cp.willMoveH(+1)
			}
		}
	}
}

type direction int8

const (
	Backward direction = -1
	Forward  direction = +1
)

// expandSelection goes in direction over core characters only.
func (cp *caretPositionInternal) expandSelection(direction direction) {
	switch direction {
	case Backward:
		if cp.Logical() > 0 {
			// Skip non-spaces to the left.
			lookAt := cp.Logical()
			for lookAt > 0 && isCoreCharacter(cp.w.Content()[lookAt-1]) {
				lookAt--
			}

			cp.willMoveH(int32(lookAt) - int32(cp.Logical()))
		}
	case Forward:
		if cp.Logical() < uint32(cp.w.LenContent()) {
			// Skip non-spaces to the right.
			lookAt := cp.Logical()
			for lookAt < uint32(cp.w.LenContent()) && isCoreCharacter(cp.w.Content()[lookAt]) {
				lookAt++
			}

			cp.willMoveH(int32(lookAt) - int32(cp.Logical()))
		}
	}
}

// Moves caret horizontally by amount. It doesn't do bounds checking, so it's
// the caller's responsibility to ensure it's a legal amount to move by.
//
// Pre-conditions:
//	- Moving caret by amount should result in a valid position.
func (cp *caretPositionInternal) willMoveH(amount int32) {
	switch {
	case amount < 0:
		absAmount := uint32(-amount)
		for absAmount != 0 {
			if cp.positionWithinLine >= absAmount {
				cp.positionWithinLine -= absAmount
				absAmount = 0
			} else { //if cp.lineIndex > 0
				absAmount -= 1 + cp.positionWithinLine
				cp.lineIndex--
				cp.positionWithinLine = cp.w.Line(cp.lineIndex).Length()
			}
		}
	case amount > 0:
		absAmount := uint32(amount)
		for absAmount != 0 {
			if cp.positionWithinLine+absAmount <= cp.w.Line(cp.lineIndex).Length() {
				cp.positionWithinLine += absAmount
				absAmount = 0
			} else { //if cp.lineIndex < some_max
				absAmount -= 1 + cp.w.Line(cp.lineIndex).Length() - cp.positionWithinLine
				cp.lineIndex++
				cp.positionWithinLine = 0
			}
		}
	default:
		// There's no change, so don't do anything else
		return
	}

	cp.targetExpandedX, _ = cp.expandedPosition() // TODO: More direct

	gist7802150.ExternallyUpdated(&cp.DepNode2Manual)
}

func (cp *caretPositionInternal) tryMoveV(direction direction, jumpWords bool) {
	switch direction {
	case Backward:
		if cp.lineIndex > 0 {
			if jumpWords {
				for cp.lineIndex > 0 {
					cp.lineIndex--
					line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start():cp.w.Line(cp.lineIndex).End()]
					if line == "" {
						break
					}
				}
				cp.positionWithinLine = 0
			} else {
				cp.lineIndex--
				line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start():cp.w.Line(cp.lineIndex).End()]
				cp.positionWithinLine = ExpandedToLogical(line, cp.targetExpandedX)
			}

			gist7802150.ExternallyUpdated(&cp.DepNode2Manual)
		} else {
			cp.Move(-2)
		}
	case Forward:
		if cp.lineIndex < cp.w.LenLines()-1 {
			if jumpWords {
				for cp.lineIndex < cp.w.LenLines()-1 {
					cp.lineIndex++
					line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start():cp.w.Line(cp.lineIndex).End()]
					if line == "" {
						break
					}
				}
				cp.positionWithinLine = 0
			} else {
				cp.lineIndex++
				line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start():cp.w.Line(cp.lineIndex).End()]
				cp.positionWithinLine = ExpandedToLogical(line, cp.targetExpandedX)
			}

			gist7802150.ExternallyUpdated(&cp.DepNode2Manual)
		} else {
			cp.Move(+2)
		}
	}
}

func (cp *caretPositionInternal) MoveTo(other *caretPositionInternal) {
	cp.lineIndex = other.lineIndex
	cp.positionWithinLine = other.positionWithinLine
	cp.targetExpandedX = other.targetExpandedX

	gist7802150.ExternallyUpdated(&cp.DepNode2Manual)
}

func (cp *caretPositionInternal) Compare(other *caretPositionInternal) int8 {
	if cp.lineIndex < other.lineIndex {
		return -1
	} else if cp.lineIndex > other.lineIndex {
		return +1
	} else {
		if cp.positionWithinLine < other.positionWithinLine {
			return -1
		} else if cp.positionWithinLine > other.positionWithinLine {
			return +1
		} else {
			return 0
		}
	}
}

// TODO: Change amount to a proper type with 4 values, etc. to avoid confusion with other funcs where amount can be an arbitrary number.
// TODO: Rename to JumpTo or something to indicate it's a method that can never fail.
// Move jumps the caret position to a new position. This operation never fails, but may not have any effect.
// If amount is ±1, move by 1 character within same line if possible.
// If amount is ±2, jump to start/end of line.
// If amount is ±3, jump to start/end of content.
func (cp *caretPositionInternal) Move(amount int8) {
	originalPosition := *cp

	switch amount {
	// Move by 1 character within same line, if possible.
	case -1:
		if cp.positionWithinLine > 0 {
			cp.positionWithinLine--
		}
	case +1:
		if cp.positionWithinLine < cp.w.Line(cp.lineIndex).Length() {
			cp.positionWithinLine++
		}
	case -2:
		cp.positionWithinLine = 0
	case +2:
		cp.positionWithinLine = cp.w.Line(cp.lineIndex).Length()
	case -3:
		cp.lineIndex = 0
		cp.positionWithinLine = 0
	case +3:
		cp.lineIndex = cp.w.LenLines() - 1
		cp.positionWithinLine = cp.w.Line(cp.lineIndex).Length()
	}

	if cp.Compare(&originalPosition) == 0 {
		// There's no change, so don't do anything else
		return
	}

	cp.targetExpandedX, _ = cp.expandedPosition() // TODO: More direct

	gist7802150.ExternallyUpdated(&cp.DepNode2Manual)
}

func (cp *caretPositionInternal) SetPositionFromLogical(pos mgl64.Vec2) {
	if pos[1] < 0 {
		cp.lineIndex = 0
	} else if pos[1] >= float64(cp.w.LenLines()) {
		cp.lineIndex = cp.w.LenLines() - 1
	} else {
		cp.lineIndex = int(pos[1])
	}

	if pos[0] < 0 {
		cp.targetExpandedX = 0
	} else {
		cp.targetExpandedX = uint32(pos[0] + 0.5)
	}

	line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start():cp.w.Line(cp.lineIndex).End()]
	cp.positionWithinLine = ExpandedToLogical(line, cp.targetExpandedX)

	gist7802150.ExternallyUpdated(&cp.DepNode2Manual)
}

// TrySetPositionAtLineIndex places caret at beginning of lineIndex line. It accepts out of range line indicies.
func (cp *caretPositionInternal) TrySetPositionAtLineIndex(lineIndex int) {
	if lineIndex < 0 {
		lineIndex = 0
	} else if lineIndex > cp.w.LenLines()-1 {
		lineIndex = cp.w.LenLines() - 1
	}

	cp.lineIndex = lineIndex
	cp.positionWithinLine = 0
	cp.targetExpandedX = 0

	gist7802150.ExternallyUpdated(&cp.DepNode2Manual)
}

func (cp *caretPositionInternal) TrySet(position uint32) {
	if position > uint32(cp.w.LenContent()) {
		position = uint32(cp.w.LenContent())
	}

	cp.lineIndex = sort.Search(cp.w.LenLines()-1, func(lineIndex int) bool {
		return cp.w.Line(lineIndex+1).Start() > position
	})
	cp.positionWithinLine = position - cp.w.Line(cp.lineIndex).Start()

	cp.targetExpandedX, _ = cp.expandedPosition() // TODO: More direct

	gist7802150.ExternallyUpdated(&cp.DepNode2Manual)
}

// expandedPosition returns logical character units.
// Multiply by (fontWidth, fontHeight) to get physical coords.
func (cp *caretPositionInternal) expandedPosition() (x uint32, y uint32) {
	expandedCaretPosition := ExpandedLength(cp.w.Content()[cp.w.Line(cp.lineIndex).Start():cp.w.Line(cp.lineIndex).Start()+cp.positionWithinLine], 0)

	return expandedCaretPosition, uint32(cp.lineIndex)
}

// HACK
func (cp *caretPositionInternal) SetHint(caretPosition uint32, beginLineIndex int) (x uint32, y uint32) {
	caretPosition -= cp.w.Line(beginLineIndex).Start()
	caretLine := beginLineIndex
	for caretPosition > cp.w.Line(caretLine).Length() {
		caretPosition -= cp.w.Line(caretLine).Length() + 1
		caretLine++
	}
	expandedCaretPosition := ExpandedLength(cp.w.Content()[cp.w.Line(caretLine).Start():cp.w.Line(caretLine).Start()+caretPosition], 0)

	cp.targetExpandedX, y = expandedCaretPosition, uint32(caretLine)

	return cp.targetExpandedX, y
}

func (cp *caretPositionInternal) SaveState() *caretPositionInternal {
	return &caretPositionInternal{
		lineIndex:          cp.lineIndex,
		positionWithinLine: cp.positionWithinLine,
		targetExpandedX:    cp.targetExpandedX,
	}
}

func isCoreCharacter(character byte) bool {
	return (('a' <= character && character <= 'z') ||
		('A' <= character && character <= 'Z') ||
		('0' <= character && character <= '9') ||
		'_' == character)
}

// ---

type CaretPosition struct {
	w                 MultilineContentI
	caretPosition     *caretPositionInternal
	selectionPosition *caretPositionInternal

	gist7802150.DepNode2
}

func NewCaretPosition(mc MultilineContentI) *CaretPosition {
	cp := &CaretPosition{
		w:                 mc,
		caretPosition:     &caretPositionInternal{w: mc},
		selectionPosition: &caretPositionInternal{w: mc},
	}
	cp.AddSources(cp.caretPosition, cp.selectionPosition)
	return cp
}

func (cp *CaretPosition) Update() {}

// TODO: Should DepNode2 be used instead of this custom func?
func (cp *CaretPosition) NotifyContentChanged() {
	cp.caretPosition.NotifyContentChanged()
	cp.selectionPosition.NotifyContentChanged()
}

func (cp *CaretPosition) Logical() uint32 {
	return cp.caretPosition.Logical()
}

func (cp *CaretPosition) SelectionRange() (start, end uint32) {
	min := intmath.MinUint32(cp.caretPosition.Logical(), cp.selectionPosition.Logical())
	max := intmath.MaxUint32(cp.caretPosition.Logical(), cp.selectionPosition.Logical())
	return min, max
}

func (cp *CaretPosition) SelectionRange2() (start, end *caretPositionInternal) {
	if cp.caretPosition.Compare(cp.selectionPosition) <= 0 {
		return cp.caretPosition, cp.selectionPosition
	} else {
		return cp.selectionPosition, cp.caretPosition
	}
}

func (cp *CaretPosition) AnySelection() bool {
	return cp.caretPosition.Compare(cp.selectionPosition) != 0
}

func (cp *CaretPosition) TryMoveH(direction direction, leaveSelection, jumpWords bool) {
	min, max := cp.SelectionRange2()

	switch direction {
	case Backward:
		if cp.AnySelection() && !leaveSelection {
			max.MoveTo(min)
		} else {
			if cp.caretPosition.Logical() >= 1 { // TODO: Where should this check happen
				cp.caretPosition.tryMoveH(direction, jumpWords)

				if !leaveSelection {
					cp.selectionPosition.MoveTo(cp.caretPosition)
				}
			}
		}
	case Forward:
		if cp.AnySelection() && !leaveSelection {
			min.MoveTo(max)
		} else {
			if cp.caretPosition.Logical() < uint32(cp.w.LenContent()) { // TODO: Where should this check happen
				cp.caretPosition.tryMoveH(direction, jumpWords)

				if !leaveSelection {
					cp.selectionPosition.MoveTo(cp.caretPosition)
				}
			}
		}
	}
}

// HACK: leaveSelection is currently an optional bool parameter
// TODO: Change amount to a proper type with 4 values, etc. to avoid confusion with other funcs where amount can be an arbitrary number.
// TODO: Rename to JumpTo or something to indicate it's a method that can never fail.
// Move jumps the caret position to a new position. This operation never fails, but may not have any effect.
// If amount is ±1, move by 1 character within same line if possible.
// If amount is ±2, jump to start/end of line.
// If amount is ±3, jump to start/end of content.
func (cp *CaretPosition) Move(amount int8, leaveSelectionOptional ...bool) {
	// HACK, TODO: Make leaveSelection a required parameter?
	leaveSelection := len(leaveSelectionOptional) != 0 && leaveSelectionOptional[0]

	cp.caretPosition.Move(amount)

	if !leaveSelection {
		cp.selectionPosition.MoveTo(cp.caretPosition)
	}
}

func (cp *CaretPosition) MoveTo(target *caretPositionInternal) {
	cp.caretPosition.MoveTo(target)
	cp.selectionPosition.MoveTo(target)
}

func (cp *CaretPosition) TryMoveV(direction direction, leaveSelection, jumpWords bool) {
	cp.caretPosition.tryMoveV(direction, jumpWords)

	if !leaveSelection {
		cp.selectionPosition.MoveTo(cp.caretPosition)
	}
}

func (cp *CaretPosition) SetPositionFromLogical(pos mgl64.Vec2, leaveSelectionOptional ...bool) {
	// HACK, TODO: Make leaveSelection a required parameter?
	leaveSelection := len(leaveSelectionOptional) != 0 && leaveSelectionOptional[0]

	cp.caretPosition.SetPositionFromLogical(pos)

	if !leaveSelection {
		cp.selectionPosition.MoveTo(cp.caretPosition)
	}
}

func (cp *CaretPosition) Backspace() {
	cp.CreateSelectionIfNone(-1)
	cp.ReplaceSelectionWith("")
}

func (cp *CaretPosition) SelectAll() {
	// TODO: Not move the view
	cp.Move(-3)
	cp.Move(+3, true)
}

func (cp *CaretPosition) CreateSelectionIfNone(amount int32) {
	if !cp.AnySelection() {
		if (amount < 0 && cp.caretPosition.Logical() >= uint32(-amount)) ||
			(amount > 0 && cp.caretPosition.Logical()+uint32(amount) <= uint32(cp.w.LenContent())) {

			cp.caretPosition.willMoveH(amount)
		}
	}
}

func (cp *CaretPosition) CreateSelectionLineIfNone() {
	if !cp.AnySelection() {
		cp.selectionPosition.Move(-2)
		cp.caretPosition.Move(+2)
		cp.caretPosition.tryMoveH(+1, false)
	}
}

// Replaces selection with string s and moves caret to end of s.
func (cp *CaretPosition) ReplaceSelectionWith(s string) {
	selStart, selEnd := cp.SelectionRange2()
	gist7802150.SetViewGroup(cp.w, cp.w.Content()[:selStart.Logical()]+s+cp.w.Content()[selEnd.Logical():])
	selStart.willMoveH(int32(len(s)))
	selEnd.MoveTo(selStart)
}

// Gets the selection content.
func (cp *CaretPosition) GetSelectionContent() (selectionContent string) {
	selStart, selEnd := cp.SelectionRange2()
	return cp.w.Content()[selStart.Logical():selEnd.Logical()]
}

// TrySetPositionAtLineIndex places caret at beginning of lineIndex line. It accepts out of range line indicies.
func (cp *CaretPosition) TrySetPositionAtLineIndex(lineIndex int) {
	cp.caretPosition.TrySetPositionAtLineIndex(lineIndex)
	cp.selectionPosition.MoveTo(cp.caretPosition)
}

// TODO: Change api to ask for an caretPositionInternal instance?
func (cp *CaretPosition) TrySet(position uint32, leaveSelectionOptional ...bool) {
	// HACK, TODO: Make leaveSelection a required parameter?
	leaveSelection := len(leaveSelectionOptional) != 0 && leaveSelectionOptional[0]

	cp.caretPosition.TrySet(position)

	if !leaveSelection {
		cp.selectionPosition.MoveTo(cp.caretPosition)
	}
}

// SetSelection sets a selection starting at start of length. Indices must be valid.
// TODO: Optimise by not doing the "try" logic.
func (cp *CaretPosition) SetSelection(start, length uint32) {
	cp.TrySet(start)
	cp.caretPosition.willMoveH(int32(length))
}

func (cp *CaretPosition) ExpandSelectionToWord() {
	switch 1 {
	case 0:
		if cp.AnySelection() {
			return
		}
	case 1:
		// Try it out so that it always works, even if there's already a selection...
		cp.selectionPosition, cp.caretPosition = cp.SelectionRange2()
	}

	cp.selectionPosition.expandSelection(-1)
	cp.caretPosition.expandSelection(+1)
}

// ExpandedPosition returns logical character units of primary cursor position.
// Multiply by (fontWidth, fontHeight) to get physical coords.
func (cp *CaretPosition) ExpandedPosition() (x, y uint32) {
	return cp.caretPosition.expandedPosition()
}

// LineNumber returns line number of primary caret. Line number is line index + 1, so it starts at 1.
func (cp *CaretPosition) LineNumber() int {
	return cp.caretPosition.lineIndex + 1
}

// lineIndexSpan returns line indicies that are spanned by the selection.
func (cp *CaretPosition) lineIndexSpan() (first, last int) {
	min := intmath.MinInt(cp.caretPosition.lineIndex, cp.selectionPosition.lineIndex)
	max := intmath.MaxInt(cp.caretPosition.lineIndex, cp.selectionPosition.lineIndex)
	return min, max
}

// DecreaseIndent decreases indent of lines that fall within selection by 1 tab (for lines where it's possible).
func (cp *CaretPosition) DecreaseIndent() {
	selStart, selEnd := cp.SelectionRange2()
	first, last := cp.lineIndexSpan()

	newContent := cp.w.Content()[:cp.w.Line(first).Start()]
	for lineIndex := first; lineIndex <= last; lineIndex++ {
		contentLine := cp.w.Line(lineIndex)

		if lineIndex != first {
			newContent += "\n"
		}
		line := cp.w.Content()[contentLine.Start():contentLine.End()]
		if strings.HasPrefix(line, "\t") {
			line = line[1:] // Trim leading tab.

			if lineIndex == first {
				selStart.Move(-1)
			}
			if lineIndex == last {
				selEnd.Move(-1)
			}
		}
		newContent += line
	}
	newContent += cp.w.Content()[cp.w.Line(last).End():]

	gist7802150.SetViewGroup(cp.w, newContent)
}

// IncreaseIndent increases indent of all lines that fall within selection by 1 tab.
func (cp *CaretPosition) IncreaseIndent() {
	selStart, selEnd := cp.SelectionRange2()
	first, last := cp.lineIndexSpan()

	newContent := cp.w.Content()[:cp.w.Line(first).Start()]
	for lineIndex := first; lineIndex <= last; lineIndex++ {
		contentLine := cp.w.Line(lineIndex)

		if lineIndex != first {
			newContent += "\n"
		}
		newContent += "\t" + cp.w.Content()[contentLine.Start():contentLine.End()]
	}
	newContent += cp.w.Content()[cp.w.Line(last).End():]

	gist7802150.SetViewGroup(cp.w, newContent)

	// Safe to use willMoveH after SetViewGroup, since we know each line will always get a character longer.
	selStart.willMoveH(1)
	selEnd.willMoveH(1)
}

// LeadingTabCount returns number of leading tabs on current line.
func (cp *CaretPosition) LeadingTabCount() int {
	tabCount := 0

	lineToCaret := cp.w.Content()[cp.w.Line(cp.caretPosition.lineIndex).Start():cp.Logical()]

	for _, c := range lineToCaret {
		if c == '\t' {
			tabCount++
		} else {
			break
		}
	}

	return tabCount
}

func (cp *CaretPosition) SaveState() CaretPosition {
	return CaretPosition{
		caretPosition:     cp.caretPosition.SaveState(),
		selectionPosition: cp.selectionPosition.SaveState(),
	}
}

func (cp *CaretPosition) RestoreState(state CaretPosition) {
	// TODO: Add support for gracefully handling content changing since the time state was saved.
	cp.caretPosition.MoveTo(state.caretPosition)
	cp.selectionPosition.MoveTo(state.selectionPosition)
}
