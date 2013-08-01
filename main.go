package main

import (
	"fmt"
	. "gist.github.com/5286084.git"
	"log"
	"runtime"
	"strings"
	"time"

	_ "github.com/ftrvxmtrx/tga"
	"image"
	_ "image/png"
	"os"

	//"github.com/go-gl/gl"
	gl "github.com/chsc/gogl/gl21"
	glfw "github.com/go-gl/glfw3"

	"github.com/Jragonmiris/mathgl"

	"github.com/shurcooL/go-goon"

	. "gist.github.com/6003701.git"

	"io"
	//. "gist.github.com/6096872.git"
	. "gist.github.com/5258650.git"
	"os/exec"
)

var _ = UnderscoreSepToCamelCase
var _ = goon.Dump

var offX, offY gl.Float
var oFontBase gl.Uint

var redraw bool = true
var widgets []Widgeter
var mousePointer *Pointer
var keyboardPointer *Pointer

func CheckGLError() {
	errorCode := gl.GetError()
	if 0 != errorCode {
		log.Panic("GL Error: ", errorCode)
	}
}

func PrintLine(x, y float32, s string) {
	segments := strings.Split(s, "\t")
	var advance uint32
	for _, segment := range segments {
		Print(x+float32(8*advance), y, segment)
		advance += uint32(len(segment))
		advance += 4 - (advance % 4)
	}
}

func Print(x, y float32, s string) {
	if 0 == len(s) {
		return
	}

	gl.Enable(gl.BLEND)
	gl.Enable(gl.TEXTURE_2D)

	gl.PushMatrix()
	gl.Translatef(gl.Float(x), gl.Float(y), 0)
	gl.Translatef(-4+0.25, -1, 0)
	gl.ListBase(oFontBase)
	gl.CallLists(gl.Sizei(len(s)), gl.UNSIGNED_BYTE, gl.Pointer(&[]byte(s)[0]))
	gl.PopMatrix()

	CheckGLError()

	gl.Disable(gl.BLEND)
	gl.Disable(gl.TEXTURE_2D)
}

func InitFont() {
	const fontWidth = 8

	LoadTexture("./data/Font2048.tga")

	oFontBase = gl.GenLists(256)

	for iLoop1 := 0; iLoop1 < 256; iLoop1++ {
		fCharX := gl.Float(iLoop1%16) / 16.0
		fCharY := gl.Float(iLoop1/16) / 16.0

		gl.NewList(oFontBase+gl.Uint(iLoop1), gl.COMPILE)
		const offset = gl.Float(0.004)
		//#if DECISION_RENDER_TEXT_VCENTERED_MID
		VerticalOffset := gl.Float(0.00125)
		if ('a' <= iLoop1 && iLoop1 <= 'z') || '_' == iLoop1 {
			VerticalOffset = gl.Float(-0.00225)
		}
		/*#else
					VerticalOffset := gl.Float(0.0)
		//#endif*/
		gl.Begin(gl.QUADS)
		gl.TexCoord2f(fCharX+offset, 1-(1-fCharY-0.0625+offset+VerticalOffset))
		gl.Vertex2i(0, 16)
		gl.TexCoord2f(fCharX+0.0625-offset, 1-(1-fCharY-0.0625+offset+VerticalOffset))
		gl.Vertex2i(16, 16)
		gl.TexCoord2f(fCharX+0.0625-offset, 1-(1-fCharY-offset+VerticalOffset))
		gl.Vertex2i(16, 0)
		gl.TexCoord2f(fCharX+offset, 1-(1-fCharY-offset+VerticalOffset))
		gl.Vertex2i(0, 0)
		gl.End()
		gl.Translated(fontWidth, 0.0, 0.0)
		gl.EndList()
	}

	CheckGLError()
}

func DeinitFont() {
	gl.DeleteLists(oFontBase, 256)
}

func LoadTexture(path string) {
	fmt.Printf("Trying to load texture %q: ", path)

	// Open the file
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Decode the image
	img, _, err := image.Decode(file)
	if err != nil {
		log.Fatal(err)
	}

	bounds := img.Bounds()
	fmt.Printf("loaded %vx%v texture.\n", bounds.Dx(), bounds.Dy())

	var pixPointer *uint8
	switch img := img.(type) {
	case *image.RGBA:
		pixPointer = &img.Pix[0]
	case *image.NRGBA:
		pixPointer = &img.Pix[0]
	default:
		panic("Unsupported type.")
	}

	var texture gl.Uint
	gl.GenTextures(1, &texture)
	gl.BindTexture(gl.TEXTURE_2D, texture)
	gl.TexParameteri(gl.TEXTURE_2D, gl.GENERATE_MIPMAP, gl.TRUE)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_LOD_BIAS, -0.65)
	gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.Sizei(bounds.Dx()), gl.Sizei(bounds.Dy()), 0, gl.RGBA, gl.UNSIGNED_BYTE, gl.Pointer(pixPointer))
	CheckGLError()
}

type Widgeter interface {
	Render()
	Hit(mathgl.Vec2f) []Widgeter
	ProcessEvent(InputEvent) // TODO: Upgrade to MatchEventQueue() or so
	HoverPointers() map[*Pointer]bool
}

type Widget struct {
	x, y          gl.Float
	dx, dy        gl.Float
	hoverPointers map[*Pointer]bool
}

func NewWidget(x, y, dx, dy gl.Float) Widget {
	return Widget{x, y, dx, dy, map[*Pointer]bool{}}
}

func (*Widget) Render() {}
func (w *Widget) Hit(ParentPosition mathgl.Vec2f) []Widgeter {
	Hit := (ParentPosition[0] >= float32(w.x) &&
		ParentPosition[1] >= float32(w.y) &&
		ParentPosition[0] < float32(w.x+w.dx) &&
		ParentPosition[1] < float32(w.y+w.dy))

	if Hit {
		return []Widgeter{w}
	} else {
		return nil
	}
}
func (w *Widget) ProcessEvent(inputEvent InputEvent) {}
func (w *Widget) HoverPointers() map[*Pointer]bool {
	return w.hoverPointers
}

type BoxWidget struct {
	Widget
	Name string
}

func (w *BoxWidget) Render() {
	// HACK: Brute-force check the mouse pointer if it contains this widget
	isOriginHit := false
	for _, hit := range mousePointer.OriginMapping {
		if w == hit {
			isOriginHit = true
			break
		}
	}
	isHit := len(w.HoverPointers()) > 0

	// HACK: Assumes mousePointer rather than considering all connected pointing pointers
	if isOriginHit && mousePointer.State.IsActive() && isHit {
		DrawGBox(w.x, w.y, w.dx, w.dy)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.x, w.y, w.dx, w.dy)
	} else {
		DrawBox(w.x, w.y, w.dx, w.dy)
	}
}
func (w *BoxWidget) Hit(ParentPosition mathgl.Vec2f) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}

// TODO: Make this a method of []Widgeter?
func containsWidget(widgets []Widgeter, targetWidget Widgeter) bool {
	for _, widget := range mousePointer.Mapping {
		if widget == targetWidget {
			return true
		}
	}
	return false
}

var globalWindow *glfw.Window

func (w *BoxWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		containsWidget(inputEvent.Pointer.Mapping, w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		containsWidget(inputEvent.Pointer.OriginMapping, w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		fmt.Printf("%q BoxWidget pressed!\n", w.Name)
		x, y := globalWindow.GetPosition()
		globalWindow.SetPosition(x-16, y)
	}
}

func DrawBox(x, y, dx, dy gl.Float) {
	gl.Color3f(0.3, 0.3, 0.3)
	gl.Rectf(x-1, y-1, x+dx+1, y+dy+1)
	gl.Color3f(1, 1, 1)
	gl.Rectf(x, y, x+dx, y+dy)
}
func DrawYBox(x, y, dx, dy gl.Float) {
	gl.Color3f(0.898, 0.765, 0.396)
	gl.Rectf(x-1, y-1, x+dx+1, y+dy+1)
	gl.Color3f(1, 1, 1)
	gl.Rectf(x, y, x+dx, y+dy)
}
func DrawGBox(x, y, dx, dy gl.Float) {
	gl.Color3f(0.898, 0.765, 0.396)
	gl.Rectf(x-1, y-1, x+dx+1, y+dy+1)
	gl.Color3f(0.75, 0.75, 0.75)
	gl.Rectf(x, y, x+dx, y+dy)
}

type CompositeWidget struct {
	Widget
	Widgets []Widgeter
}

func (w *CompositeWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translatef(w.x, w.y, 0)

	for _, widget := range w.Widgets {
		widget.Render()
	}
}
func (w *CompositeWidget) Hit(ParentPosition mathgl.Vec2f) []Widgeter {
	LocalPosition := ParentPosition.Sub(mathgl.Vec2f{float32(w.x), float32(w.y)})

	hits := []Widgeter{}
	for _, widget := range w.Widgets {
		hits = append(hits, widget.Hit(LocalPosition)...)
	}

	return hits
}

type SomethingWidget struct {
	Widget
	Updated bool
}

func (w *SomethingWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translatef(w.x, w.y, 0)
	gl.Color3f(0.3, 0.3, 0.3)
	gl.Rectf(0-1, 0-1, 8*27+1, 16+1)
	if !w.Updated {
		gl.Color3f(1, 1, 1)
	} else {
		gl.Color3f(0.9, 1, 0.9)
	}
	gl.Rectf(0, 0, 8*27, 16)

	gl.Color3f(0, 0, 0)
	Print(0, 0, "Hello Conception 2 (Go)! :D Woot")
}

type Something2Widget struct {
	Widget
}

func (w *Something2Widget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translatef(w.x, w.y, 0)
	gl.Color3f(0, 0, 0)

	gl.Enable(gl.TEXTURE_2D)
	gl.Enable(gl.BLEND)
	gl.Begin(gl.TRIANGLE_FAN)
	{
		const size = 256
		gl.TexCoord2i(0, 0)
		gl.Vertex2i(0, 0)
		gl.TexCoord2i(1, 0)
		gl.Vertex2i(size, 0)
		gl.TexCoord2i(1, 1)
		gl.Vertex2i(size, size)
		gl.TexCoord2i(0, 1)
		gl.Vertex2i(0, size)
	}
	gl.End()
	gl.Disable(gl.TEXTURE_2D)
	gl.Disable(gl.BLEND)
}

type UnderscoreSepToCamelCaseWidget struct {
	Widget
	window *glfw.Window
}

func (w *UnderscoreSepToCamelCaseWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translatef(w.x, w.y, 0)

	//s := w.window.GetClipboardString()
	s := "get_clipboard_string"
	// E.g., get_clipboard_string -> GetClipboardString
	s += " -> " + UnderscoreSepToCamelCase(s)
	w.dx = gl.Float(8 * len(s))
	w.dy = 16

	gl.Color3f(0.3, 0.3, 0.3)
	gl.Rectf(0-1, 0-1, w.dx+1, w.dy+1)
	gl.Color3f(1, 1, 1)
	gl.Rectf(0, 0, w.dx, w.dy)

	gl.Color3f(0, 0, 0)
	Print(0, 0, s)
}

type ChannelExpeWidget struct {
	Widget
	Content string
	ch      <-chan []byte
	errCh   <-chan error
}

// TODO: Fix the duplication of gist.github.com/6096872.git
func ByteReaderW(r io.Reader) (<-chan []byte, <-chan error) {
	ch := make(chan []byte)
	errCh := make(chan error)

	go func() {
		for {
			buf := make([]byte, 2048)
			s := 0
		inner:
			for {
				n, err := r.Read(buf[s:])
				if n > 0 {
					redraw = true
					ch <- buf[s : s+n]
					s += n
				}
				if err != nil {
					errCh <- err
					return
				}
				if s >= len(buf) {
					break inner
				}
			}
		}
	}()

	return ch, errCh
}

func NewChannelExpeWidget(x, y gl.Float) *ChannelExpeWidget {
	cmd := exec.Command("ping", "google.com")
	stdout, err := cmd.StdoutPipe()
	CheckError(err)
	err = cmd.Start()
	CheckError(err)

	w := ChannelExpeWidget{Widget: NewWidget(x, y, 0, 0)}
	w.ch, w.errCh = ByteReaderW(stdout)

	return &w
}

func (w *ChannelExpeWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translatef(w.x, w.y, 0)

	// TODO: This needs to go into "ProcessTimePassed" type of thing that gets called even when redraw is false
	select {
	case b := <-w.ch:
		w.Content += string(b)
	default:
	}

	LongestLine := uint32(0)
	lines := GetLines(w.Content)
	for _, line := range lines {
		lineLength := ExpandedLength(line)
		if lineLength > LongestLine {
			LongestLine = lineLength
		}
	}

	w.dx = gl.Float(8 * LongestLine)
	w.dy = gl.Float(16 * len(lines))

	gl.Color3f(0.3, 0.3, 0.3)
	gl.Rectf(0-1, 0-1, w.dx+1, w.dy+1)
	gl.Color3f(1, 1, 1)
	gl.Rectf(0, 0, w.dx, w.dy)

	gl.Color3f(0, 0, 0)
	for lineNumber, line := range lines {
		Print(0, float32(16*lineNumber), line)
	}
}

type SpinnerWidget struct {
	Widget
	Spinner uint32
}

func (w *SpinnerWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Color3f(0, 0, 0)
	gl.Translatef(w.x, w.y, 0)
	//gl.Rotatef(float32(spinner), 0, 0, 1)
	gl.Rotatef(gl.Float(w.Spinner), 0, 0, 1)
	gl.Begin(gl.LINES)
	gl.Vertex2i(0, -10)
	gl.Vertex2i(0, 10)
	gl.End()
}

// ---

func ExpandedLength(s string) uint32 {
	segments := strings.Split(s, "\t")
	var advance uint32
	for segmentIndex, segment := range segments {
		advance += uint32(len(segment))
		if segmentIndex != len(segments)-1 {
			advance += 4 - (advance % 4)
		}
	}
	return advance
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

type CaretPosition struct {
	w               *TextBoxWidget
	caretPosition   uint32
	targetExpandedX uint32
}

func (cp *CaretPosition) Logical() uint32 {
	return cp.caretPosition
}

func (cp *CaretPosition) ExpandedPosition() (x uint32, y uint32) {
	caretPosition := cp.caretPosition
	caretLine := uint32(0)
	for caretPosition > cp.w.lines[caretLine].Length {
		caretPosition -= cp.w.lines[caretLine].Length + 1
		caretLine++
	}
	expandedCaretPosition := ExpandedLength(cp.w.content[cp.w.lines[caretLine].Start : cp.w.lines[caretLine].Start+caretPosition])

	return expandedCaretPosition, caretLine
}

func (cp *CaretPosition) Move(amount int8) {
	switch {
	case amount == +1:
		cp.caretPosition++
	case amount == -1:
		cp.caretPosition--
	}

	x, _ := cp.ExpandedPosition()
	cp.targetExpandedX = x
}

func (cp *CaretPosition) TryMoveV(amount int8) {
	_, y := cp.ExpandedPosition()

	switch {
	case amount == -1:
		if y > 0 {
			y--
			line := cp.w.content[cp.w.lines[y].Start : cp.w.lines[y].Start+cp.w.lines[y].Length]
			cp.caretPosition = cp.w.lines[y].Start + ExpandedToLogical(line, cp.targetExpandedX)
		}
	case amount == +1:
		if y < uint32(len(cp.w.lines))-1 {
			y++
			line := cp.w.content[cp.w.lines[y].Start : cp.w.lines[y].Start+cp.w.lines[y].Length]
			cp.caretPosition = cp.w.lines[y].Start + ExpandedToLogical(line, cp.targetExpandedX)
		}
	}
}

// ---

type TextBoxWidget struct {
	Widget
	content       string
	caretPosition CaretPosition

	lines       []contentLine
	longestLine uint32 // Line length
}

type contentLine struct {
	Start  uint32
	Length uint32
}

func NewTextBoxWidget(x, y gl.Float) *TextBoxWidget {
	w := &TextBoxWidget{Widget: NewWidget(x, y, 0, 0)}
	w.caretPosition.w = w
	return w
}

func (w *TextBoxWidget) SetContent(content string) {
	w.content = content
	w.UpdateLines()
	//goon.Dump(w.Content, w.Lines)
}

func (w *TextBoxWidget) UpdateLines() {
	lines := GetLines(w.content)
	w.lines = make([]contentLine, len(lines))
	w.longestLine = 0
	for lineNumber, line := range lines {
		lineLength := ExpandedLength(line)
		if lineLength > w.longestLine {
			w.longestLine = lineLength
		}
		if lineNumber >= 1 {
			w.lines[lineNumber].Start = w.lines[lineNumber-1].Start + w.lines[lineNumber-1].Length + 1
		}
		w.lines[lineNumber].Length = uint32(len(line))
	}
}

func (w *TextBoxWidget) Render() {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	// HACK: Setting the widget size in Render() is a bad, because all the input calculations will fail before it's rendered
	if w.longestLine < 3 {
		w.dx = gl.Float(8 * 3)
	} else {
		w.dx = gl.Float(8 * w.longestLine)
	}
	w.dy = gl.Float(16 * len(w.lines))

	// HACK: Brute-force check the mouse pointer if it contains this widget
	isOriginHit := false
	for _, hit := range mousePointer.OriginMapping {
		if w == hit {
			isOriginHit = true
			break
		}
	}
	isHit := len(w.HoverPointers()) > 0

	// HACK: Assumes mousePointer rather than considering all connected pointing pointers
	if isOriginHit && mousePointer.State.IsActive() && isHit {
		DrawYBox(w.x, w.y, w.dx, w.dy)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.x, w.y, w.dx, w.dy)
	} else if hasTypingFocus {
		DrawYBox(w.x, w.y, w.dx, w.dy)
	} else {
		DrawBox(w.x, w.y, w.dx, w.dy)
	}

	gl.Color3f(0, 0, 0)
	for lineNumber, contentLine := range w.lines {
		PrintLine(float32(w.x), float32(w.y)+float32(16*lineNumber), w.content[contentLine.Start:contentLine.Start+contentLine.Length])
	}

	if hasTypingFocus {
		expandedCaretPosition, caretLine := w.caretPosition.ExpandedPosition()

		// Draw caret
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translatef(w.x, w.y, 0)
		gl.Color3f(0, 0, 0)
		gl.Recti(gl.Int(expandedCaretPosition*8-1), gl.Int(caretLine*16), gl.Int(expandedCaretPosition*8+1), gl.Int(caretLine*16)+16)
	}
}
func (w *TextBoxWidget) Hit(ParentPosition mathgl.Vec2f) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}
func (w *TextBoxWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		containsWidget(inputEvent.Pointer.Mapping, w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		containsWidget(inputEvent.Pointer.OriginMapping, w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		keyboardPointer.OriginMapping = []Widgeter{w}
	}

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	/*hasTypingFocus := len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.Pointer.State.Buttons[0] == true || inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0) {
		if inputEvent.Pointer.State.Axes[0]-float64(w.x) < 0 {
			w.caretPosition = 0
		} else if inputEvent.Pointer.State.Axes[0]-float64(w.x) > float64(len(w.content)*8) {
			w.caretPosition = uint32(len(w.content))
		} else {
			w.caretPosition = uint32((inputEvent.Pointer.State.Axes[0] - float64(w.x) + 4) / 8)
		}
	}*/

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
		switch glfw.Key(inputEvent.InputId) {
		case glfw.KeyBackspace:
			if w.caretPosition.Logical() >= 1 {
				w.caretPosition.Move(-1)
				w.SetContent(w.content[:w.caretPosition.Logical()] + w.content[w.caretPosition.Logical()+1:])
			}
		case glfw.KeyEnter:
			w.SetContent(w.content[:w.caretPosition.Logical()] + "\n" + w.content[w.caretPosition.Logical():])
			w.caretPosition.Move(+1)
		case glfw.KeyTab:
			w.SetContent(w.content[:w.caretPosition.Logical()] + "\t" + w.content[w.caretPosition.Logical():])
			w.caretPosition.Move(+1)
		case glfw.KeyLeft:
			if inputEvent.ModifierKey == glfw.ModSuper {
				// TODO: Go to start of line-ish
				//w.caretPosition = 0
			} else if inputEvent.ModifierKey == 0 {
				if w.caretPosition.Logical() >= 1 {
					w.caretPosition.Move(-1)
				}
			}
		case glfw.KeyRight:
			if inputEvent.ModifierKey == glfw.ModSuper {
				// TODO: Go to end of line
				//w.caretPosition = uint32(len(w.content))
			} else if inputEvent.ModifierKey == 0 {
				if w.caretPosition.Logical() < uint32(len(w.content)) {
					w.caretPosition.Move(+1)
				}
			}
		case glfw.KeyUp:
			if inputEvent.ModifierKey == 0 {
				w.caretPosition.TryMoveV(-1)
			}
		case glfw.KeyDown:
			if inputEvent.ModifierKey == 0 {
				w.caretPosition.TryMoveV(+1)
			}
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[CHARACTER_EVENT] && inputEvent.InputId < 128 {
		w.SetContent(w.content[:w.caretPosition.Logical()] + string(byte(inputEvent.InputId)) + w.content[w.caretPosition.Logical():])
		w.caretPosition.Move(+1)
	}
}

type TextFieldWidget struct {
	Widget
	Content       string
	CaretPosition uint32
}

func NewTextFieldWidget(x, y gl.Float) *TextFieldWidget {
	return &TextFieldWidget{NewWidget(x, y, 0, 0), "", 0}
}

func (w *TextFieldWidget) Render() {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	// HACK: Setting the widget size in Render() is a bad, because all the input calculations will fail before it's rendered
	if len(w.Content) < 3 {
		w.dx = gl.Float(8 * 3)
	} else {
		w.dx = gl.Float(8 * len(w.Content))
	}
	w.dy = 16

	// HACK: Brute-force check the mouse pointer if it contains this widget
	isOriginHit := false
	for _, hit := range mousePointer.OriginMapping {
		if w == hit {
			isOriginHit = true
			break
		}
	}
	isHit := len(w.HoverPointers()) > 0

	// HACK: Assumes mousePointer rather than considering all connected pointing pointers
	if isOriginHit && mousePointer.State.IsActive() && isHit {
		DrawYBox(w.x, w.y, w.dx, w.dy)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.x, w.y, w.dx, w.dy)
	} else if hasTypingFocus {
		DrawYBox(w.x, w.y, w.dx, w.dy)
	} else {
		DrawBox(w.x, w.y, w.dx, w.dy)
	}

	gl.Color3f(0, 0, 0)
	Print(float32(w.x), float32(w.y), w.Content)

	if hasTypingFocus {
		// Draw caret
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translatef(w.x, w.y, 0)
		gl.Color3f(0, 0, 0)
		gl.Recti(gl.Int(w.CaretPosition*8-1), 0, gl.Int(w.CaretPosition*8+1), 16)
	}
}
func (w *TextFieldWidget) Hit(ParentPosition mathgl.Vec2f) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}
func (w *TextFieldWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		containsWidget(inputEvent.Pointer.Mapping, w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		containsWidget(inputEvent.Pointer.OriginMapping, w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		keyboardPointer.OriginMapping = []Widgeter{w}
	}

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.Pointer.State.Buttons[0] == true || inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0) {
		if inputEvent.Pointer.State.Axes[0]-float64(w.x) < 0 {
			w.CaretPosition = 0
		} else if inputEvent.Pointer.State.Axes[0]-float64(w.x) > float64(len(w.Content)*8) {
			w.CaretPosition = uint32(len(w.Content))
		} else {
			w.CaretPosition = uint32((inputEvent.Pointer.State.Axes[0] - float64(w.x) + 4) / 8)
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
		switch glfw.Key(inputEvent.InputId) {
		case glfw.KeyBackspace:
			if w.CaretPosition >= 1 {
				w.CaretPosition--
				w.Content = w.Content[:w.CaretPosition] + w.Content[w.CaretPosition+1:]
			}
		case glfw.KeyLeft:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.CaretPosition = 0
			} else if inputEvent.ModifierKey == 0 {
				if w.CaretPosition >= 1 {
					w.CaretPosition--
				}
			}
		case glfw.KeyRight:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.CaretPosition = uint32(len(w.Content))
			} else if inputEvent.ModifierKey == 0 {
				if w.CaretPosition < uint32(len(w.Content)) {
					w.CaretPosition++
				}
			}
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[CHARACTER_EVENT] && inputEvent.InputId < 128 {
		w.Content = w.Content[:w.CaretPosition] + string(byte(inputEvent.InputId)) + w.Content[w.CaretPosition:]
		w.CaretPosition++
	}
}

type MetaCharacter struct {
	Character byte
	Timestamp int64
}

func NewMetaCharacter(ch byte) MetaCharacter {
	return MetaCharacter{ch, time.Now().UnixNano()}
}

type MetaTextFieldWidget struct {
	Widget
	Content       []MetaCharacter
	CaretPosition uint32
}

func NewMetaTextFieldWidget(x, y gl.Float) *MetaTextFieldWidget {
	return &MetaTextFieldWidget{NewWidget(x, y, 0, 0), nil, 0}
}

func (w *MetaTextFieldWidget) Render() {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	// HACK: Setting the widget size in Render() is a bad, because all the input calculations will fail before it's rendered
	if len(w.Content) < 3 {
		w.dx = gl.Float(8 * 3)
	} else {
		w.dx = gl.Float(8 * len(w.Content))
	}
	w.dy = 16

	// HACK: Brute-force check the mouse pointer if it contains this widget
	isOriginHit := false
	for _, hit := range mousePointer.OriginMapping {
		if w == hit {
			isOriginHit = true
			break
		}
	}
	isHit := len(w.HoverPointers()) > 0

	// HACK: Assumes mousePointer rather than considering all connected pointing pointers
	if isOriginHit && mousePointer.State.IsActive() && isHit {
		DrawYBox(w.x, w.y, w.dx, w.dy)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.x, w.y, w.dx, w.dy)
	} else if hasTypingFocus {
		DrawYBox(w.x, w.y, w.dx, w.dy)
	} else {
		DrawBox(w.x, w.y, w.dx, w.dy)
	}

	now := time.Now().UnixNano()
	for i, mc := range w.Content {
		age := now - mc.Timestamp
		highlight := gl.Float(age) / 10000000000

		gl.Color3f(1, 1, highlight)
		gl.Rectf(w.x+gl.Float(i*8), w.y, w.x+gl.Float((i+1)*8), w.y+16)

		gl.Color3f(0, 0, 0)
		Print(float32(w.x)+float32(8*i), float32(w.y), string(mc.Character))
	}

	if hasTypingFocus {
		// Draw caret
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translatef(w.x, w.y, 0)
		gl.Color3f(0, 0, 0)
		gl.Recti(gl.Int(w.CaretPosition*8-1), 0, gl.Int(w.CaretPosition*8+1), 16)
	}
}
func (w *MetaTextFieldWidget) Hit(ParentPosition mathgl.Vec2f) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}
func (w *MetaTextFieldWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		containsWidget(inputEvent.Pointer.Mapping, w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		containsWidget(inputEvent.Pointer.OriginMapping, w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		keyboardPointer.OriginMapping = []Widgeter{w}
	}

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.Pointer.State.Buttons[0] == true || inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0) {
		if inputEvent.Pointer.State.Axes[0]-float64(w.x) < 0 {
			w.CaretPosition = 0
		} else if inputEvent.Pointer.State.Axes[0]-float64(w.x) > float64(len(w.Content)*8) {
			w.CaretPosition = uint32(len(w.Content))
		} else {
			w.CaretPosition = uint32((inputEvent.Pointer.State.Axes[0] - float64(w.x) + 4) / 8)
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
		switch glfw.Key(inputEvent.InputId) {
		case glfw.KeyBackspace:
			if w.CaretPosition >= 1 {
				w.CaretPosition--
				w.Content = append(w.Content[:w.CaretPosition], w.Content[w.CaretPosition+1:]...)
			}
		case glfw.KeyLeft:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.CaretPosition = 0
			} else if inputEvent.ModifierKey == 0 {
				if w.CaretPosition >= 1 {
					w.CaretPosition--
				}
			}
		case glfw.KeyRight:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.CaretPosition = uint32(len(w.Content))
			} else if inputEvent.ModifierKey == 0 {
				if w.CaretPosition < uint32(len(w.Content)) {
					w.CaretPosition++
				}
			}
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[CHARACTER_EVENT] && inputEvent.InputId < 128 {
		//w.Content = append(append(w.Content[:w.CaretPosition], NewMetaCharacter(byte(inputEvent.InputId))), w.Content[w.CaretPosition:]...)
		w.Content = append(w.Content, MetaCharacter{})
		copy(w.Content[w.CaretPosition+1:], w.Content[w.CaretPosition:])
		w.Content[w.CaretPosition] = NewMetaCharacter(byte(inputEvent.InputId))
		w.CaretPosition++
	}
}

type VirtualCategory uint8

const (
	TYPING VirtualCategory = iota
	POINTING
)

type Pointer struct {
	VirtualCategory VirtualCategory
	Mapping         []Widgeter
	OriginMapping   []Widgeter
	State           PointerState
}

type PointerState struct {
	Buttons []bool // True means pressed down
	Axes    []float64

	Timestamp int64
}

// A pointer is defined to be active if any of its buttons are pressed down
func (ps *PointerState) IsActive() bool {
	//IsAnyButtonsPressed()
	for _, button := range ps.Buttons {
		if button {
			return true
		}
	}

	return false
}

type EventType uint8

const (
	BUTTON_EVENT EventType = iota
	CHARACTER_EVENT
	SLIDER_EVENT
	AXIS_EVENT
	POINTER_ACTIVATION
	POINTER_DEACTIVATION
)

type InputEvent struct {
	Pointer    *Pointer
	EventTypes map[EventType]bool
	//DeviceId   uint8
	InputId uint16

	Buttons []bool
	// TODO: Characters? Split into distinct event types, bundle up in an event frame based on time?
	Sliders     []float64
	Axes        []float64
	ModifierKey glfw.ModifierKey // HACK
}

func ProcessInputEventQueue(inputEventQueue []InputEvent) []InputEvent {
	for len(inputEventQueue) > 0 {
		inputEvent := inputEventQueue[0]

		if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.InputId == 0 && inputEvent.EventTypes[AXIS_EVENT] {
			Position := mathgl.Vec2f{float32(inputEvent.Pointer.State.Axes[0]), float32(inputEvent.Pointer.State.Axes[1])}

			// Clear previously hit widgets
			for _, widget := range inputEvent.Pointer.Mapping {
				delete(widget.HoverPointers(), inputEvent.Pointer)
			}
			inputEvent.Pointer.Mapping = []Widgeter{}

			// Recalculate currently hit widgets
			for _, widget := range widgets {
				inputEvent.Pointer.Mapping = append(inputEvent.Pointer.Mapping, widget.Hit(Position)...)
			}
			for _, widget := range inputEvent.Pointer.Mapping {
				widget.HoverPointers()[inputEvent.Pointer] = true
			}
		}

		// Populate PointerMappings (but only when pointer is moved while not active, and this isn't a deactivation since that's handled below)
		if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.InputId == 0 && inputEvent.EventTypes[AXIS_EVENT] &&
			!inputEvent.EventTypes[POINTER_DEACTIVATION] && !inputEvent.Pointer.State.IsActive() {
			inputEvent.Pointer.OriginMapping = make([]Widgeter, len(inputEvent.Pointer.Mapping))
			copy(inputEvent.Pointer.OriginMapping, inputEvent.Pointer.Mapping)
		}

		if inputEvent.Pointer == mousePointer && inputEvent.InputId == 0 && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] {
			//fmt.Println("Left down!")
		} else if inputEvent.Pointer == mousePointer && inputEvent.InputId == 1 && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] {
			//fmt.Println("Right down!")
		}

		for _, widget := range inputEvent.Pointer.OriginMapping {
			widget.ProcessEvent(inputEvent)
		}

		// Populate PointerMappings (but only upon pointer deactivation event)
		if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[POINTER_DEACTIVATION] {
			inputEvent.Pointer.OriginMapping = make([]Widgeter, len(inputEvent.Pointer.Mapping))
			copy(inputEvent.Pointer.OriginMapping, inputEvent.Pointer.Mapping)
		}

		inputEventQueue = inputEventQueue[1:]
	}

	inputEventQueue = []InputEvent{}

	return inputEventQueue
}

func EnqueueInputEvent(inputEvent InputEvent, inputEventQueue []InputEvent) []InputEvent {
	//fmt.Printf("%#v\n", inputEvent)

	preStateActive := inputEvent.Pointer.State.IsActive()

	{
		if inputEvent.EventTypes[BUTTON_EVENT] {
			// Extend slice if needed
			neededSize := int(inputEvent.InputId) + len(inputEvent.Buttons)
			if neededSize > len(inputEvent.Pointer.State.Buttons) {
				inputEvent.Pointer.State.Buttons = append(inputEvent.Pointer.State.Buttons, make([]bool, neededSize-len(inputEvent.Pointer.State.Buttons))...)
			}

			copy(inputEvent.Pointer.State.Buttons[inputEvent.InputId:], inputEvent.Buttons)
		}

		if inputEvent.EventTypes[AXIS_EVENT] {
			// Extend slice if needed
			neededSize := int(inputEvent.InputId) + len(inputEvent.Axes)
			if neededSize > len(inputEvent.Pointer.State.Axes) {
				inputEvent.Pointer.State.Axes = append(inputEvent.Pointer.State.Axes, make([]float64, neededSize-len(inputEvent.Pointer.State.Axes))...)
			}

			copy(inputEvent.Pointer.State.Axes[inputEvent.InputId:], inputEvent.Axes)
		}

		inputEvent.Pointer.State.Timestamp = time.Now().UnixNano()
	}

	postStateActive := inputEvent.Pointer.State.IsActive()

	if !preStateActive && postStateActive {
		inputEvent.EventTypes[POINTER_ACTIVATION] = true
	} else if preStateActive && !postStateActive {
		inputEvent.EventTypes[POINTER_DEACTIVATION] = true
	}

	return append(inputEventQueue, inputEvent)
}

func main() {
	runtime.LockOSThread()

	glfw.SetErrorCallback(func(err glfw.ErrorCode, desc string) {
		panic(fmt.Sprintf("glfw.ErrorCallback: %v: %v\n", err, desc))
	})

	if !glfw.Init() {
		panic("glfw.Init()")
	}
	defer glfw.Terminate()

	//glfw.WindowHint(glfw.Samples, 32)
	//glfw.WindowHint(glfw.Decorated, glfw.False)
	window, err := glfw.CreateWindow(400, 400, "", nil, nil)
	globalWindow = window
	CheckError(err)
	window.MakeContextCurrent()

	err = gl.Init()
	if nil != err {
		log.Print(err)
	}
	fmt.Println(gl.GoStringUb(gl.GetString(gl.VENDOR)), gl.GoStringUb(gl.GetString(gl.RENDERER)), gl.GoStringUb(gl.GetString(gl.VERSION)))

	//window.SetPosition(1600, 600)
	window.SetPosition(1275, 300)
	glfw.SwapInterval(1)

	InitFont()
	defer DeinitFont()

	size := func(w *glfw.Window, width, height int) {
		windowWidth, windowHeight := w.GetSize()
		//fmt.Println("Framebuffer Size:", width, height, "Window Size:", windowWidth, windowHeight)
		gl.Viewport(0, 0, gl.Sizei(width), gl.Sizei(height))

		// Update the projection matrix
		gl.MatrixMode(gl.PROJECTION)
		gl.LoadIdentity()
		gl.Ortho(0, gl.Double(windowWidth), gl.Double(windowHeight), 0, -1, 1)
		gl.MatrixMode(gl.MODELVIEW)

		redraw = true
	}
	{
		width, height := window.GetFramebufferSize()
		size(window, width, height)
	}
	window.SetFramebufferSizeCallback(size)

	box := &BoxWidget{NewWidget(50, 150, 16, 16), "The Original Box"}
	widgets = append(widgets, box)
	spinner := SpinnerWidget{NewWidget(20, 20, 0, 0), 0}
	widgets = append(widgets, &spinner)
	something := SomethingWidget{NewWidget(50, 100, 0, 0), false}
	widgets = append(widgets, &something)
	//widgets = append(widgets, &Something2Widget{NewWidget(50, 220, 0, 0)})
	widgets = append(widgets, &CompositeWidget{NewWidget(150, 150, 0, 0),
		[]Widgeter{
			&BoxWidget{NewWidget(0, 0, 16, 16), "Left of Duo"},
			&BoxWidget{NewWidget(16+2, 0, 16, 16), "Right of Duo"},
		},
	})
	widgets = append(widgets, &UnderscoreSepToCamelCaseWidget{NewWidget(50, 180, 0, 0), window})
	widgets = append(widgets, NewTextFieldWidget(50, 50))
	widgets = append(widgets, NewMetaTextFieldWidget(50, 70))
	widgets = append(widgets, NewChannelExpeWidget(-100, 210))
	widgets = append(widgets, NewTextBoxWidget(100, 20))

	mousePointer = &Pointer{VirtualCategory: POINTING}
	keyboardPointer = &Pointer{VirtualCategory: TYPING}
	inputEventQueue := []InputEvent{}

	MousePos := func(w *glfw.Window, x, y float64) {
		redraw = true
		//fmt.Println("MousePos:", x, y)

		//(widgets[len(widgets)-1]).(*CompositeWidget).x = gl.Float(x)
		//(widgets[len(widgets)-1]).(*CompositeWidget).y = gl.Float(y)

		inputEvent := InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[EventType]bool{AXIS_EVENT: true},
			//DeviceId:   0,
			InputId: 0,
			Buttons: nil,
			Sliders: nil,
			Axes:    []float64{x, y},
		}
		inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
	}
	{
		x, y := window.GetCursorPosition()
		MousePos(window, x, y)
	}
	window.SetCursorPositionCallback(MousePos)

	window.SetScrollCallback(func(w *glfw.Window, xoff float64, yoff float64) {
		offX += gl.Float(xoff * 10)
		offY += gl.Float(yoff * 10)
		redraw = true

		inputEvent := InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[EventType]bool{SLIDER_EVENT: true},
			//DeviceId:   0,
			InputId: 2,
			Buttons: nil,
			Sliders: []float64{yoff, xoff},
			Axes:    nil,
		}
		inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
	})

	window.SetMouseButtonCallback(func(w *glfw.Window, button glfw.MouseButton, action glfw.Action, mod glfw.ModifierKey) {
		// TODO: Move redraw = true elsewhere? Like somewhere within events processing? Or keep it in all event handlers?
		redraw = true
		inputEvent := InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[EventType]bool{BUTTON_EVENT: true},
			//DeviceId:   0,
			InputId: uint16(button),
			Buttons: []bool{action != glfw.Release},
			Sliders: nil,
			Axes:    nil,
		}
		inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
	})

	window.SetKeyCallback(func(w *glfw.Window, key glfw.Key, scancode int, action glfw.Action, mods glfw.ModifierKey) {
		/*if key == glfw.KeyEnter && action == glfw.Press {
			x, y := window.GetPosition()
			window.SetPosition(x-16, y)
		}*/

		inputEvent := InputEvent{
			Pointer:    keyboardPointer,
			EventTypes: map[EventType]bool{BUTTON_EVENT: true},
			//DeviceId:   130,
			InputId:     uint16(key),
			Buttons:     []bool{action != glfw.Release},
			Sliders:     nil,
			Axes:        nil,
			ModifierKey: mods,
		}
		//fmt.Println(key, action, mods)
		inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
		redraw = true // HACK
	})

	window.SetCharacterCallback(func(w *glfw.Window, char uint) {
		inputEvent := InputEvent{
			Pointer:    keyboardPointer,
			EventTypes: map[EventType]bool{CHARACTER_EVENT: true},
			//DeviceId:   130,
			InputId: uint16(char),
			Buttons: nil,
			Sliders: nil,
			Axes:    nil,
		}
		inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
		redraw = true // HACK
	})

	go func() {
		<-time.After(3 * time.Second)
		something.Updated = true
		redraw = true
	}()

	gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA)
	//gl.ClearColor(0.8, 0.3, 0.01, 1)
	gl.ClearColor(0.85, 0.85, 0.85, 1)

	keyboardPointer.OriginMapping = []Widgeter{widgets[len(widgets)-1]}
	widgets[len(widgets)-1].(*TextBoxWidget).SetContent("blah\nnew line\n\thello tab\n.\ttab\n..\ttab\n...\ttab\n....\ttab! step by step\n\t\ttwo tabs.")

	//last := window.GetClipboardString()

	for !window.ShouldClose() && glfw.Press != window.GetKey(glfw.KeyEscape) {
		//glfw.WaitEvents()
		glfw.PollEvents()

		/*now := window.GetClipboardString()
		if now != last {
			last = now
			redraw = true
			fmt.Println("GetClipboardString changed!")
		}*/

		// Input
		inputEventQueue = ProcessInputEventQueue(inputEventQueue)

		if redraw {
			redraw = false

			gl.Clear(gl.COLOR_BUFFER_BIT)
			gl.LoadIdentity()
			gl.Translatef(offX, offY, 0)

			for _, widget := range widgets {
				widget.Render()
			}

			window.SwapBuffers()
			spinner.Spinner++
			//log.Println("swapped buffers")
		} else {
			time.Sleep(time.Millisecond)
		}

		//runtime.Gosched()
	}
}
