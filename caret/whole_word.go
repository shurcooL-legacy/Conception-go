package caret

// TODO: Implement IsWholeWord more efficiently without relying on virtual CaretPosition, just use linear content access.
//       But reuse tryMoveH jump condition logic for word boundaries.

func IsWholeWord(content MultilineContentI, caretPosition *CaretPosition) bool {
	if !caretPosition.AnySelection() {
		return false
	}

	start, end := caretPosition.SelectionRange2()

	// Figure out if the selection is a whole word.
	x := &caretPositionInternal{w: content}
	y := &caretPositionInternal{w: content}
	x.MoveTo(start)
	y.MoveTo(end)
	x.tryMoveH(+1, true)
	y.tryMoveH(-1, true)

	return (y.Compare(start) == 0 && x.Compare(end) == 0)
}

func IsWholeWord2(content MultilineContentI, start, length uint32) bool {
	cp := NewCaretPosition(content)
	cp.SetSelection(start, length)

	return IsWholeWord(content, cp)
}
