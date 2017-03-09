package multilinecontent

import (
	"strings"

	"github.com/shurcooL/Conception-go/caret"
	"github.com/shurcooL/Conception-go/pkg/gist7802150"
)

// New creates an empty memory-backed MultilineContent.
func New() *MultilineContent {
	mc := &MultilineContent{}
	mc.InitViewGroup(mc, "memory://???")
	mc.updateLines()
	return mc
}

// New creates a memory-backed MultilineContent,
// setting its initial value to content.
func NewString(content string) *MultilineContent {
	mc := &MultilineContent{}
	mc.InitViewGroup(mc, "memory://???")
	gist7802150.SetViewGroup(mc, content)
	return mc
}

type MultilineContent struct {
	content     string
	lines       []contentLine // TODO: Can be replaced by line starts only, calculate length in Line()
	longestLine uint32        // Line length

	gist7802150.ViewGroup
}

func (c *MultilineContent) Content() string     { return c.content }
func (c *MultilineContent) LongestLine() uint32 { return c.longestLine }

func (c *MultilineContent) LenContent() int { return len(c.content) }

func (c *MultilineContent) Line(lineIndex int) caret.ContentLine {
	if lineIndex < 0 {
		return contentLine{0, 0}
	} else if lineIndex >= len(c.lines) {
		return contentLine{uint32(len(c.content)), 0}
	} else {
		return c.lines[lineIndex]
	}
}
func (c *MultilineContent) LenLines() int {
	return len(c.lines)
}

func (mc *MultilineContent) SetSelf(content string) {
	mc.content = content
	mc.updateLines()
}

func (w *MultilineContent) updateLines() {
	lines := strings.Split(w.content, "\n")
	w.lines = make([]contentLine, len(lines))
	w.longestLine = 0
	for lineIndex, line := range lines {
		expandedLineLength := caret.ExpandedLength(line, 0)
		if expandedLineLength > w.longestLine {
			w.longestLine = expandedLineLength
		}
		if lineIndex >= 1 {
			w.lines[lineIndex].start = w.lines[lineIndex-1].End() + 1
		}
		w.lines[lineIndex].length = uint32(len(line))
	}
}

// contentLine represents a content line.
type contentLine struct {
	start  uint32
	length uint32
}

func (this contentLine) Start() uint32 {
	return this.start
}

func (this contentLine) End() uint32 {
	return this.start + this.length
}

func (this contentLine) Length() uint32 {
	return this.length
}
