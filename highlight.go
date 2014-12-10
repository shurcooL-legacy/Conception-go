package main

import (
	"go/scanner"
	"go/token"
	"sort"

	"github.com/go-gl/glow/gl/2.1/gl"
	"github.com/go-gl/mathgl/mgl64"
	"github.com/mb0/diff"
	"github.com/sergi/go-diff/diffmatchpatch"
	"github.com/shurcooL/go/gists/gist7802150"

	"github.com/shurcooL/Conception-go/caret"
)

type TextStyle struct {
	FontOptions     *FontOptions
	TextColor       *mgl64.Vec3
	BorderColor     **mgl64.Vec3
	BackgroundColor **mgl64.Vec3
	ShowInvisibles  *bool
}

func (textStyle *TextStyle) Apply(glt *OpenGlStream) {
	if textStyle == nil {
		return
	}

	if textStyle.FontOptions != nil {
		glt.FontOptions = *textStyle.FontOptions
	}
	if textStyle.TextColor != nil {
		textColor := *textStyle.TextColor
		gl.Color3dv((*float64)(&textColor[0]))
	}
	if textStyle.BorderColor != nil {
		borderColor := *textStyle.BorderColor
		glt.BorderColor = borderColor
	}
	if textStyle.BackgroundColor != nil {
		backgroundColor := *textStyle.BackgroundColor
		glt.BackgroundColor = backgroundColor
	}
	if textStyle.ShowInvisibles != nil {
		glt.ShowInvisibles = *textStyle.ShowInvisibles
	}
}

// ---

type Highlighter interface {
	NewIterator(offset uint32) HighlighterIterator

	gist7802150.DepNode2I
}

type HighlighterIterator interface {
	Next() uint32
	Current() *TextStyle
	Advance(uint32) *TextStyle
}

// ---

type segmentHighlighter interface {
	Segment(index uint32) highlightSegment
	LenSegments() int

	SegmentToTextStyle(index uint32) *TextStyle
}

type highlighterIterator struct {
	hl     segmentHighlighter
	offset uint32
	index  uint32
}

func NewHighlighterIterator(hl segmentHighlighter, offset uint32) *highlighterIterator {
	return &highlighterIterator{
		hl:     hl,
		offset: offset,
		// Binary search for the first entry that ends past the beginning of visible text
		index: uint32(sort.Search(hl.LenSegments()-1, func(i int) bool {
			return hl.Segment(uint32(i)+1).offset > offset
		})),
	}
}

func (this *highlighterIterator) Next() uint32 {
	return this.hl.Segment(this.index+1).offset - this.offset
}

func (this *highlighterIterator) Current() *TextStyle {
	return this.hl.SegmentToTextStyle(this.index)
}

func (this *highlighterIterator) Advance(span uint32) *TextStyle {
	if span < this.Next() {
		this.offset += span
		return nil
	} else {
		this.offset += span
		this.index++
		return this.Current()
	}
}

// ---

type selectionHighlighterIterator struct {
	offset         uint32
	min, max       uint32
	hasTypingFocus bool
}

func NewSelectionHighlighterIterator(offset, min, max uint32, hasTypingFocus bool) *selectionHighlighterIterator {
	return &selectionHighlighterIterator{
		offset:         offset,
		min:            min,
		max:            max,
		hasTypingFocus: hasTypingFocus,
	}
}

func (this *selectionHighlighterIterator) Next() uint32 {
	if this.min == this.max { // HACK
		return 500000000 // TODO, HACK
	} else if this.offset < this.min {
		return this.min - this.offset
	} else if this.offset < this.max {
		return this.max - this.offset
	} else {
		return 500000000 // TODO, HACK
	}
}

func (this *selectionHighlighterIterator) Current() *TextStyle {
	if this.min == this.max { // HACK: Return nil if no selection at all
		return nil
	}

	borderColor := &selectedTextDarkColor
	color := &selectedTextColor
	showInvisibles := true
	if !this.hasTypingFocus {
		borderColor = &selectedTextColor
		color = &selectedTextInactiveColor
	}
	if this.offset < this.min || this.offset >= this.max {
		borderColor = nil
		color = nil
		showInvisibles = false
	}
	return &TextStyle{
		BorderColor:     &borderColor,
		BackgroundColor: &color,
		ShowInvisibles:  &showInvisibles,
	}
}

func (this *selectionHighlighterIterator) Advance(span uint32) *TextStyle {
	if span < this.Next() {
		this.offset += span
		return nil
	} else {
		this.offset += span
		return this.Current()
	}
}

// ---

type highlightSegment struct {
	offset uint32
	color  mgl64.Vec3
	bold   bool
}

type highlightedGoContent struct {
	segments []highlightSegment

	gist7802150.DepNode2
}

func (this *highlightedGoContent) NewIterator(offset uint32) HighlighterIterator {
	return NewHighlighterIterator(this, offset)
}

func (this *highlightedGoContent) Update() {
	content := this.GetSources()[0].(caret.MultilineContentI)

	this.segments = nil

	src := []byte(content.Content())
	//src := []byte(w.Content.Content()[w.Content.Line(beginLineIndex).Start:w.Content.Line(endLineIndex).Start])

	var s scanner.Scanner
	fset := token.NewFileSet()
	file := fset.AddFile("", fset.Base(), len(src))
	s.Init(file, src, nil, scanner.ScanComments)

	// Repeated calls to Scan yield the token sequence found in the input.
	for {
		pos, tok, lit := s.Scan()
		if tok == token.EOF {
			break
		}

		offset := uint32(fset.Position(pos).Offset)

		switch {
		case tok.IsKeyword() || (tok.IsOperator() && tok < token.LPAREN):
			//return syntaxhighlight.KEYWORD
			this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0.004, 0, 0.694}, bold: true})

		// Literals.
		case tok == token.INT || tok == token.FLOAT || tok == token.IMAG:
			//return syntaxhighlight.DECIMAL
			this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0.804, 0, 0}})
		case tok == token.STRING || tok == token.CHAR:
			//return syntaxhighlight.STRING
			this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0.804, 0, 0}})
		case lit == "true" || lit == "false" || lit == "iota":
			//return syntaxhighlight.LITERAL
			this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0.008, 0.024, 1}})

		case tok == token.COMMENT:
			//return syntaxhighlight.COMMENT
			this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0, 0.506, 0.094}})
		default:
			//return syntaxhighlight.PLAINTEXT
			this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0, 0, 0}})
		}
	}

	// HACK: Fake last element.
	this.segments = append(this.segments, highlightSegment{offset: uint32(content.LenContent())})
}

func (this *highlightedGoContent) Segment(index uint32) highlightSegment {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return highlightSegment{offset: 0}
	} else if index >= uint32(len(this.segments)) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return highlightSegment{offset: uint32(this.segments[len(this.segments)-1].offset)}
	} else {
		return this.segments[index]
	}
}
func (this *highlightedGoContent) LenSegments() int {
	return len(this.segments)
}
func (this *highlightedGoContent) SegmentToTextStyle(index uint32) *TextStyle {
	color := this.Segment(index).color
	var fontOptions FontOptions
	if this.Segment(index).bold {
		fontOptions = Bold
	}
	return &TextStyle{
		FontOptions: &fontOptions,
		TextColor:   &color,
	}
}

// ---

func highlightedDiffFunc(leftContent, rightContent string, segments *[2][]highlightSegment, offsets [2]uint32) {
	dmp := diffmatchpatch.New()
	diffs := dmp.DiffMain(leftContent, rightContent, true)

	for side := range *segments {
		offset := offsets[side]

		for _, diff := range diffs {
			if side == 0 && diff.Type == -1 {
				(*segments)[side] = append((*segments)[side], highlightSegment{offset: offset, color: darkRedColor})
				offset += uint32(len(diff.Text))
			}
			if side == 1 && diff.Type == +1 {
				(*segments)[side] = append((*segments)[side], highlightSegment{offset: offset, color: darkGreenColor})
				offset += uint32(len(diff.Text))
			}
			if diff.Type == 0 {
				(*segments)[side] = append((*segments)[side], highlightSegment{offset: offset})
				offset += uint32(len(diff.Text))
			}
		}
	}
}

type highlightedDiffSide struct {
	side int
	*highlightedDiff
}

func (this *highlightedDiffSide) NewIterator(offset uint32) HighlighterIterator {
	return NewHighlighterIterator(this, offset)
}
func (this *highlightedDiffSide) Segment(index uint32) highlightSegment {
	return this.highlightedDiff.segment(index, this.side)
}
func (this *highlightedDiffSide) LenSegments() int {
	return this.highlightedDiff.lenSegments(this.side)
}
func (this *highlightedDiffSide) SegmentToTextStyle(index uint32) *TextStyle {
	return this.highlightedDiff.segmentToTextStyle(index, this.side)
}

type highlightedDiff struct {
	segments [2][]highlightSegment

	gist7802150.DepNode2
}

func (this *highlightedDiff) Update() {
	left := this.GetSources()[0].(caret.MultilineContentI)
	right := this.GetSources()[1].(caret.MultilineContentI)

	dmp := diffmatchpatch.New()
	diffs := dmp.DiffMain(left.Content(), right.Content(), true)

	for side := range this.segments {
		this.segments[side] = nil

		offset := uint32(0)

		for _, diff := range diffs {
			if side == 0 && diff.Type == -1 {
				this.segments[side] = append(this.segments[side], highlightSegment{offset: offset, color: mediumRedColor})
				offset += uint32(len(diff.Text))
			}
			if side == 1 && diff.Type == +1 {
				this.segments[side] = append(this.segments[side], highlightSegment{offset: offset, color: mediumGreenColor})
				offset += uint32(len(diff.Text))
			}
			if diff.Type == 0 {
				this.segments[side] = append(this.segments[side], highlightSegment{offset: offset})
				offset += uint32(len(diff.Text))
			}
		}

		// HACK: Fake last element.
		if side == 0 {
			this.segments[side] = append(this.segments[side], highlightSegment{offset: uint32(left.LenContent())})
		} else {
			this.segments[side] = append(this.segments[side], highlightSegment{offset: uint32(right.LenContent())})
		}
	}
}

func (this *highlightedDiff) segment(index uint32, side int) highlightSegment {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return highlightSegment{offset: 0}
	} else if index >= uint32(len(this.segments[side])) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return highlightSegment{offset: uint32(this.segments[side][len(this.segments[side])-1].offset)}
	} else {
		return this.segments[side][index]
	}
}
func (this *highlightedDiff) lenSegments(side int) int {
	return len(this.segments[side])
}
func (this *highlightedDiff) segmentToTextStyle(index uint32, side int) *TextStyle {
	color := this.segment(index, side).color
	colorPtr := &color
	if color.ApproxEqual(mgl64.Vec3{}) {
		colorPtr = nil
	}
	return &TextStyle{
		BackgroundColor: &colorPtr,
	}
}

// ---

type tokLit struct {
	offset uint32
	tok    token.Token
	lit    string
}

type tokenizedGoContent struct {
	segments []tokLit

	gist7802150.DepNode2
}

func (this *tokenizedGoContent) Update() {
	content := this.GetSources()[0].(caret.MultilineContentI)

	this.segments = nil

	src := []byte(content.Content())
	//src := []byte(w.Content.Content()[w.Content.Line(beginLineIndex).Start:w.Content.Line(endLineIndex).Start])

	var s scanner.Scanner
	fset := token.NewFileSet()
	file := fset.AddFile("", fset.Base(), len(src))
	s.Init(file, src, nil, scanner.ScanComments)

	// Repeated calls to Scan yield the token sequence found in the input.
	// TODO: Perhaps include whitespace in between tokens?
	for {
		pos, tok, lit := s.Scan()
		if tok == token.EOF {
			break
		}

		offset := uint32(fset.Position(pos).Offset)

		this.segments = append(this.segments, tokLit{offset: offset, tok: tok, lit: lit})
	}

	// HACK: Fake last element.
	this.segments = append(this.segments, tokLit{offset: uint32(content.LenContent())})
}

func (this *tokenizedGoContent) Segment(index uint32) tokLit {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return tokLit{offset: 0}
	} else if index >= uint32(len(this.segments)) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return tokLit{offset: uint32(this.segments[len(this.segments)-1].offset)}
	} else {
		return this.segments[index]
	}
}
func (this *tokenizedGoContent) LenSegments() int {
	return len(this.segments)
}

// ---

type tokenizedDiffHelper struct {
	left  *tokenizedGoContent
	right *tokenizedGoContent
}

func (this *tokenizedDiffHelper) Equal(i, j int) bool {
	return this.left.Segment(uint32(i)).tok == this.right.Segment(uint32(j)).tok &&
		this.left.Segment(uint32(i)).lit == this.right.Segment(uint32(j)).lit
}

type tokenizedDiff struct {
	leftSide bool
	segments []highlightSegment

	gist7802150.DepNode2
}

func (this *tokenizedDiff) NewIterator(offset uint32) HighlighterIterator {
	return NewHighlighterIterator(this, offset)
}

func (this *tokenizedDiff) Update() {
	left := this.GetSources()[0].(*tokenizedGoContent)
	right := this.GetSources()[1].(*tokenizedGoContent)

	this.segments = nil

	dmp := tokenizedDiffHelper{left: left, right: right}
	diffs := diff.Diff(left.LenSegments(), right.LenSegments(), &dmp)

	// HACK: Fake first element.
	this.segments = append(this.segments, highlightSegment{offset: 0})

	for _, diff := range diffs {
		if !this.leftSide && diff.Ins > 0 {
			beginOffset := right.Segment(uint32(diff.B)).offset
			endOffset := right.Segment(uint32(diff.B + diff.Ins)).offset
			this.segments = append(this.segments, highlightSegment{offset: beginOffset, color: darkGreenColor})
			this.segments = append(this.segments, highlightSegment{offset: endOffset})
		} else if this.leftSide && diff.Del > 0 {
			beginOffset := left.Segment(uint32(diff.A)).offset
			endOffset := left.Segment(uint32(diff.A + diff.Del)).offset
			this.segments = append(this.segments, highlightSegment{offset: beginOffset, color: darkRedColor})
			this.segments = append(this.segments, highlightSegment{offset: endOffset})
		}
	}

	// HACK: Fake last element.
	this.segments = append(this.segments, highlightSegment{offset: uint32(500000000)}) // TODO, HACK
}

func (this *tokenizedDiff) Segment(index uint32) highlightSegment {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return highlightSegment{offset: 0}
	} else if index >= uint32(len(this.segments)) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return highlightSegment{offset: uint32(this.segments[len(this.segments)-1].offset)}
	} else {
		return this.segments[index]
	}
}
func (this *tokenizedDiff) LenSegments() int {
	return len(this.segments)
}
func (this *tokenizedDiff) SegmentToTextStyle(index uint32) *TextStyle {
	color := this.Segment(index).color
	colorPtr := &color
	if color.ApproxEqual(mgl64.Vec3{}) {
		colorPtr = nil
	}
	return &TextStyle{
		BackgroundColor: &colorPtr,
	}
}

// ---

type lineDiffHelper struct {
	left  caret.MultilineContentI
	right caret.MultilineContentI
}

func (this *lineDiffHelper) Equal(i, j int) bool {
	line1 := this.left.Content()[this.left.Line(i).Start():this.left.Line(i).End()]
	line2 := this.right.Content()[this.right.Line(j).Start():this.right.Line(j).End()]
	return line1 == line2
}

// Line-based differ.
type lineDiffSide struct {
	side int
	*lineDiff
}

func (this *lineDiffSide) NewIterator(offset uint32) HighlighterIterator {
	return NewHighlighterIterator(this, offset)
}
func (this *lineDiffSide) Segment(index uint32) highlightSegment {
	return this.lineDiff.segment(index, this.side)
}
func (this *lineDiffSide) LenSegments() int {
	return this.lineDiff.lenSegments(this.side)
}
func (this *lineDiffSide) SegmentToTextStyle(index uint32) *TextStyle {
	return this.lineDiff.segmentToTextStyle(index, this.side)
}

func (this *lineDiffSide) LineBackgroundColor(lineIndex int) (BackgroundColor *mgl64.Vec3) {
	if this.lineDiff.lines[this.side][lineIndex] {
		switch this.side {
		case 0:
			return &lightRedColor
		case 1:
			return &lightGreenColor
		}
	}
	return nil
}

type lineDiff struct {
	segments [2][]highlightSegment
	lines    [2][]bool // For LineBackgroundColor, true indicates the line with that index was modified (deleted or inserted).

	gist7802150.DepNode2
}

func (this *lineDiff) Update() {
	left := this.GetSources()[0].(caret.MultilineContentI)
	right := this.GetSources()[1].(caret.MultilineContentI)

	dmp := lineDiffHelper{left: left, right: right}
	diffs := diff.Diff(left.LenLines(), right.LenLines(), &dmp)

	for side := range this.segments {
		this.segments[side] = nil

		// HACK: Fake first element.
		this.segments[side] = append(this.segments[side], highlightSegment{offset: 0})
	}
	this.lines[0] = make([]bool, left.LenLines())
	this.lines[1] = make([]bool, right.LenLines())

	for _, diff := range diffs {
		if diff.Del > 0 || diff.Ins > 0 {
			beginOffsetLeft := left.Line(diff.A).Start()
			endOffsetLeft := left.Line(diff.A + diff.Del).Start()
			beginOffsetRight := right.Line(diff.B).Start()
			endOffsetRight := right.Line(diff.B + diff.Ins).Start()

			for line := diff.A; line < diff.A+diff.Del; line++ {
				this.lines[0][line] = true
			}
			for line := diff.B; line < diff.B+diff.Ins; line++ {
				this.lines[1][line] = true
			}

			leftContent := left.Content()[beginOffsetLeft:endOffsetLeft]
			rightContent := right.Content()[beginOffsetRight:endOffsetRight]

			highlightedDiffFunc(leftContent, rightContent, &this.segments, [2]uint32{beginOffsetLeft, beginOffsetRight})

			this.segments[0] = append(this.segments[0], highlightSegment{offset: endOffsetLeft})
			this.segments[1] = append(this.segments[1], highlightSegment{offset: endOffsetRight})
		} /* else {
			for side := range this.segments {
				if side == 0 && diff.Del > 0 {
					beginOffset := left.Line(diff.A).Start()
					endOffset := left.Line(diff.A + diff.Del).Start()

					this.segments[side] = append(this.segments[side], highlightSegment{offset: beginOffset, color: darkRedColor})
					this.segments[side] = append(this.segments[side], highlightSegment{offset: endOffset})
				}
				if side == 1 && diff.Ins > 0 {
					beginOffset := right.Line(diff.B).Start()
					endOffset := right.Line(diff.B + diff.Ins).Start()

					this.segments[side] = append(this.segments[side], highlightSegment{offset: beginOffset, color: darkGreenColor})
					this.segments[side] = append(this.segments[side], highlightSegment{offset: endOffset})
				}
			}
		}*/
	}

	for side := range this.segments {
		// HACK: Fake last element.
		this.segments[side] = append(this.segments[side], highlightSegment{offset: uint32(500000000)}) // TODO, HACK
	}
}

func (this *lineDiff) segment(index uint32, side int) highlightSegment {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return highlightSegment{offset: 0}
	} else if index >= uint32(len(this.segments[side])) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return highlightSegment{offset: uint32(this.segments[side][len(this.segments[side])-1].offset)}
	} else {
		return this.segments[side][index]
	}
}
func (this *lineDiff) lenSegments(side int) int {
	return len(this.segments[side])
}
func (this *lineDiff) segmentToTextStyle(index uint32, side int) *TextStyle {
	color := this.segment(index, side).color
	colorPtr := &color
	if color.ApproxEqual(mgl64.Vec3{}) {
		colorPtr = nil
	}
	return &TextStyle{
		BackgroundColor: &colorPtr,
	}
}

// ---

type LineHighlighter interface {
	LineBackgroundColor(lineIndex int) (BackgroundColor *mgl64.Vec3)

	gist7802150.DepNode2I
}

type diffHighlighter struct {
	segments []highlightSegment

	gist7802150.DepNode2
}

func (this *diffHighlighter) Update() {
	content := this.GetSources()[0].(caret.MultilineContentI)

	this.segments = nil

	// HACK: Fake first element.
	this.segments = append(this.segments, highlightSegment{offset: 0})

	lastDel, lastIns := -1, -1
	for lineIndex := 0; lineIndex < content.LenLines(); lineIndex++ {
		var lineFirstChar byte
		if content.Line(lineIndex).Length() > 0 {
			lineFirstChar = content.Content()[content.Line(lineIndex).Start()]
		}
		switch lineFirstChar {
		case '@':
			this.segments = append(this.segments, highlightSegment{bold: true, color: mgl64.Vec3{0.5, 0, 0.5}, offset: content.Line(lineIndex).Start()})
			this.segments = append(this.segments, highlightSegment{bold: true, offset: content.Line(lineIndex).End()})
		case '+':
			if lastIns == -1 {
				lastIns = lineIndex
			}
		case '-':
			if lastDel == -1 {
				lastDel = lineIndex
			}
		default:
			if lastDel != -1 || lastIns != -1 {
				if lastDel == -1 {
					lastDel = lastIns
				} else if lastIns == -1 {
					lastIns = lineIndex
				}

				beginOffsetLeft := content.Line(lastDel).Start()
				endOffsetLeft := content.Line(lastIns).Start()
				beginOffsetRight := content.Line(lastIns).Start()
				endOffsetRight := content.Line(lineIndex).Start()

				// This is needed to filter out the "-" and "+" at the beginning of each line from being highlighted.
				// TODO: Still not completely filtered out.
				leftContent := ""
				for line := lastDel; line < lastIns; line++ {
					leftContent += "\x00" + content.Content()[content.Line(line).Start()+1:content.Line(line).End()] + "\n"
				}
				rightContent := ""
				for line := lastIns; line < lineIndex; line++ {
					rightContent += "\x00" + content.Content()[content.Line(line).Start()+1:content.Line(line).End()] + "\n"
				}

				var sectionSegments [2][]highlightSegment
				highlightedDiffFunc(leftContent, rightContent, &sectionSegments, [2]uint32{beginOffsetLeft, beginOffsetRight})

				sectionSegments[0] = append(sectionSegments[0], highlightSegment{offset: endOffsetLeft})
				sectionSegments[1] = append(sectionSegments[1], highlightSegment{offset: endOffsetRight})

				this.segments = append(this.segments, sectionSegments[0]...)
				this.segments = append(this.segments, sectionSegments[1]...)
			}
			lastDel, lastIns = -1, -1
		}
	}

	// HACK: Fake last element.
	this.segments = append(this.segments, highlightSegment{offset: uint32(500000000)}) // TODO, HACK
}

func (this *diffHighlighter) NewIterator(offset uint32) HighlighterIterator {
	return NewHighlighterIterator(this, offset)
}
func (this *diffHighlighter) Segment(index uint32) highlightSegment {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return highlightSegment{offset: 0}
	} else if index >= uint32(len(this.segments)) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return highlightSegment{offset: uint32(this.segments[len(this.segments)-1].offset)}
	} else {
		return this.segments[index]
	}
}
func (this *diffHighlighter) LenSegments() int {
	return len(this.segments)
}
func (this *diffHighlighter) SegmentToTextStyle(index uint32) *TextStyle {
	color := this.Segment(index).color
	colorPtr := &color
	if color.ApproxEqual(mgl64.Vec3{}) {
		colorPtr = nil
	}
	switch {
	case this.Segment(index).bold && colorPtr != nil: // Beginning of '@' line.
		fontOptions := Bold
		return &TextStyle{
			FontOptions: &fontOptions,
			TextColor:   &color,
		}
	case this.Segment(index).bold && colorPtr == nil: // End of '@' line.
		fontOptions := Regular
		return &TextStyle{
			FontOptions: &fontOptions,
			TextColor:   &color,
		}
	default: // A normal diff segment.
		return &TextStyle{
			BackgroundColor: &colorPtr,
		}
	}
}

func (this *diffHighlighter) LineBackgroundColor(lineIndex int) (BackgroundColor *mgl64.Vec3) {
	content := this.GetSources()[0].(caret.MultilineContentI)

	if content.Line(lineIndex).Length() > 0 {
		switch lineFirstChar := content.Content()[content.Line(lineIndex).Start()]; lineFirstChar {
		case '+':
			return &lightGreenColor
		case '-':
			return &lightRedColor
		}
	}
	return nil
}
