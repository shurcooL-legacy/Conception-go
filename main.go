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

	. "gist.github.com/5258650.git"
	. "gist.github.com/6096872.git"
	"os/exec"

	. "gist.github.com/5571468.git"

	//"go/parser"
	//"go/token"

	"math"
)

var _ = UnderscoreSepToCamelCase
var _ = goon.Dump

const katOnly = false

var offX, offY gl.Double
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

func PrintLine(pos mathgl.Vec2d, s string) {
	segments := strings.Split(s, "\t")
	var advance uint32
	for _, segment := range segments {
		Print(mathgl.Vec2d{pos[0] + float64(8*advance), pos[1]}, segment)
		advance += uint32(len(segment))
		advance += 4 - (advance % 4)
	}
}

func Print(pos mathgl.Vec2d, s string) {
	if 0 == len(s) {
		return
	}

	gl.Enable(gl.BLEND)
	gl.Enable(gl.TEXTURE_2D)

	gl.PushMatrix()
	gl.Translated(gl.Double(pos[0]), gl.Double(pos[1]), 0)
	gl.Translated(-4+0.25, -1, 0)
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
		fCharX := gl.Double(iLoop1%16) / 16.0
		fCharY := gl.Double(iLoop1/16) / 16.0

		gl.NewList(oFontBase+gl.Uint(iLoop1), gl.COMPILE)
		const offset = gl.Double(0.004)
		//#if DECISION_RENDER_TEXT_VCENTERED_MID
		VerticalOffset := gl.Double(0.00125)
		if ('a' <= iLoop1 && iLoop1 <= 'z') || '_' == iLoop1 {
			VerticalOffset = gl.Double(-0.00225)
		}
		/*#else
					VerticalOffset := gl.Double(0.0)
		//#endif*/
		gl.Begin(gl.QUADS)
		gl.TexCoord2d(fCharX+offset, 1-(1-fCharY-0.0625+offset+VerticalOffset))
		gl.Vertex2i(0, 16)
		gl.TexCoord2d(fCharX+0.0625-offset, 1-(1-fCharY-0.0625+offset+VerticalOffset))
		gl.Vertex2i(16, 16)
		gl.TexCoord2d(fCharX+0.0625-offset, 1-(1-fCharY-offset+VerticalOffset))
		gl.Vertex2i(16, 0)
		gl.TexCoord2d(fCharX+offset, 1-(1-fCharY-offset+VerticalOffset))
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
	Hit(mathgl.Vec2d) []Widgeter
	ProcessEvent(InputEvent) // TODO: Upgrade to MatchEventQueue() or so
	ProcessTimePassed(timePassed float64)
	HoverPointers() map[*Pointer]bool
}

type Widget struct {
	pos           mathgl.Vec2d
	size          mathgl.Vec2d
	hoverPointers map[*Pointer]bool
}

func NewWidget(pos, size mathgl.Vec2d) Widget {
	return Widget{pos, size, map[*Pointer]bool{}}
}

func (*Widget) Render() {}
func (w *Widget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
	Hit := (ParentPosition[0] >= float64(w.pos[0]) &&
		ParentPosition[1] >= float64(w.pos[1]) &&
		ParentPosition[0] < float64(w.pos.Add(w.size)[0]) &&
		ParentPosition[1] < float64(w.pos.Add(w.size)[1]))

	if Hit {
		return []Widgeter{w}
	} else {
		return nil
	}
}
func (w *Widget) ProcessEvent(inputEvent InputEvent)   {}
func (w *Widget) ProcessTimePassed(timePassed float64) {}
func (w *Widget) HoverPointers() map[*Pointer]bool {
	return w.hoverPointers
}

// ---

type Test1Widget struct {
	Widget
}

func NewTest1Widget(pos mathgl.Vec2d) *Test1Widget {
	return &Test1Widget{Widget: NewWidget(pos, mathgl.Vec2d{300, 300})}
}

func (w *Test1Widget) Render() {
	DrawBox(w.pos, w.size)
}

// ---

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
		DrawGBox(w.pos, w.size)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.pos, w.size)
	} else {
		DrawBox(w.pos, w.size)
	}
}
func (w *BoxWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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

func DrawBox(pos, size mathgl.Vec2d) {
	gl.Color3d(0.3, 0.3, 0.3)
	gl.Rectd(gl.Double(pos[0]-1), gl.Double(pos[1]-1), gl.Double(pos.Add(size)[0]+1), gl.Double(pos.Add(size)[1]+1))
	gl.Color3d(1, 1, 1)
	gl.Rectd(gl.Double(pos[0]), gl.Double(pos[1]), gl.Double(pos.Add(size)[0]), gl.Double(pos.Add(size)[1]))
}
func DrawYBox(pos, size mathgl.Vec2d) {
	gl.Color3d(0.898, 0.765, 0.396)
	gl.Rectd(gl.Double(pos[0]-1), gl.Double(pos[1]-1), gl.Double(pos.Add(size)[0]+1), gl.Double(pos.Add(size)[1]+1))
	gl.Color3d(1, 1, 1)
	gl.Rectd(gl.Double(pos[0]), gl.Double(pos[1]), gl.Double(pos.Add(size)[0]), gl.Double(pos.Add(size)[1]))
}
func DrawGBox(pos, size mathgl.Vec2d) {
	gl.Color3d(0.898, 0.765, 0.396)
	gl.Rectd(gl.Double(pos[0]-1), gl.Double(pos[1]-1), gl.Double(pos.Add(size)[0]+1), gl.Double(pos.Add(size)[1]+1))
	gl.Color3d(0.75, 0.75, 0.75)
	gl.Rectd(gl.Double(pos[0]), gl.Double(pos[1]), gl.Double(pos.Add(size)[0]), gl.Double(pos.Add(size)[1]))
}

func DrawCircle(Position mathgl.Vec2d, Size mathgl.Vec2d, BackgroundColor, BorderColor mathgl.Vec3d) {
	const TwoPi = math.Pi * 2

	const x = 64

	gl.Color3dv((*gl.Double)(&BorderColor[0]))
	gl.Begin(gl.TRIANGLE_FAN)
	gl.Vertex2d(gl.Double(Position[0]), gl.Double(Position[1]))
	for i := 0; i <= x; i++ {
		gl.Vertex2d(gl.Double(Position[0]+math.Sin(TwoPi*float64(i)/x)*Size[0]/2), gl.Double(Position[1]+math.Cos(TwoPi*float64(i)/x)*Size[1]/2))
	}
	gl.End()

	gl.Color3dv((*gl.Double)(&BackgroundColor[0]))
	gl.Begin(gl.TRIANGLE_FAN)
	gl.Vertex2d(gl.Double(Position[0]), gl.Double(Position[1]))
	for i := 0; i <= x; i++ {
		gl.Vertex2d(gl.Double(Position[0]+math.Sin(TwoPi*float64(i)/x)*(Size[0]/2-1)), gl.Double(Position[1]+math.Cos(TwoPi*float64(i)/x)*(Size[1]/2-1)))
	}
	gl.End()
}

func DrawCircleBorder(Position mathgl.Vec2d, Size mathgl.Vec2d, BorderColor mathgl.Vec3d) {
	const TwoPi = math.Pi * 2

	const x = 64

	gl.Color3dv((*gl.Double)(&BorderColor[0]))
	gl.Begin(gl.TRIANGLE_STRIP)
	for i := 0; i <= x; i++ {
		gl.Vertex2d(gl.Double(Position[0]+math.Sin(TwoPi*float64(i)/x)*Size[0]/2), gl.Double(Position[1]+math.Cos(TwoPi*float64(i)/x)*Size[1]/2))
		gl.Vertex2d(gl.Double(Position[0]+math.Sin(TwoPi*float64(i)/x)*(Size[0]/2-1)), gl.Double(Position[1]+math.Cos(TwoPi*float64(i)/x)*(Size[1]/2-1)))
	}
	gl.End()
}

// ---

type KatWidget struct {
	Widget
	target      mathgl.Vec2d
	mode        KatMode
	skillActive bool
}

const ShunpoRadius = 120

type KatMode uint8

const (
	AutoAttack KatMode = iota
	Shunpo
)

func NewKatWidget(pos mathgl.Vec2d) *KatWidget {
	return &KatWidget{Widget: NewWidget(pos, mathgl.Vec2d{16, 16}), target: pos}
}

func (w *KatWidget) Render() {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	isHit := len(w.HoverPointers()) > 0

	if !hasTypingFocus && !isHit {
		DrawCircle(w.pos, w.size, mathgl.Vec3d{1, 1, 1}, mathgl.Vec3d{0.3, 0.3, 0.3})
	} else {
		DrawCircle(w.pos, w.size, mathgl.Vec3d{1, 1, 1}, mathgl.Vec3d{0.898, 0.765, 0.396})
	}

	if w.mode == Shunpo {
		DrawCircleBorder(w.pos, mathgl.Vec2d{ShunpoRadius * 2, ShunpoRadius * 2}, mathgl.Vec3d{0.7, 0.7, 0.7})
	}
}

func (w *KatWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
	if w.pos.Sub(ParentPosition).Len() <= w.size[0]/2 {
		return []Widgeter{w}
	} else {
		return nil
	}
}

func (w *KatWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		containsWidget(inputEvent.Pointer.Mapping, w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		containsWidget(inputEvent.Pointer.OriginMapping, w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		keyboardPointer.OriginMapping = []Widgeter{w}
	}

	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == true &&
		w.mode == Shunpo {
		w.target = mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
		w.skillActive = true
	}

	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.Pointer.State.Button(1) {
		pointerPos := mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
		if pointerPos.Sub(w.pos).Len() > w.size[0]*2/3 || w.target.Sub(w.pos).Len() > w.size[0]*2/3 {
			w.target = pointerPos
		}
		w.mode = AutoAttack
		w.skillActive = false
	} else if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 'E' && inputEvent.Buttons[0] == true {
		w.mode = Shunpo
	}
}

func (w *KatWidget) ProcessTimePassed(timePassed float64) {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	const speed = float64(100.0)

	if hasTypingFocus {
		if keyboardPointer.State.Button('A') && !keyboardPointer.State.Button('D') {
			w.pos[0] -= timePassed * speed
			redraw = true
		} else if keyboardPointer.State.Button('D') && !keyboardPointer.State.Button('A') {
			w.pos[0] += timePassed * speed
			redraw = true
		}
		if keyboardPointer.State.Button('W') && !keyboardPointer.State.Button('S') {
			w.pos[1] -= timePassed * speed
			redraw = true
		} else if keyboardPointer.State.Button('S') && !keyboardPointer.State.Button('W') {
			w.pos[1] += timePassed * speed
			redraw = true
		}
	}

	if w.target.Sub(w.pos).Len() <= speed*timePassed {
		w.pos = w.target
	} else {
		moveBy := w.target.Sub(w.pos)
		moveBy = moveBy.Normalize().Mul(speed * timePassed)
		w.pos = w.pos.Add(moveBy)
		redraw = true
	}

	if w.skillActive && w.target.Sub(w.pos).Len() <= ShunpoRadius {
		w.pos = w.target
		w.mode = AutoAttack
		w.skillActive = false
	}
}

// ---

type CompositeWidget struct {
	Widget
	Widgets []Widgeter
}

func (w *CompositeWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)

	for _, widget := range w.Widgets {
		widget.Render()
	}
}
func (w *CompositeWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
	LocalPosition := ParentPosition.Sub(w.pos)

	hits := []Widgeter{}
	for _, widget := range w.Widgets {
		hits = append(hits, widget.Hit(LocalPosition)...)
	}

	return hits
}
func (w *CompositeWidget) ProcessTimePassed(timePassed float64) {
	for _, widget := range w.Widgets {
		widget.ProcessTimePassed(timePassed)
	}
}

type UnderscoreSepToCamelCaseWidget struct {
	Widget
	window *glfw.Window
}

func (w *UnderscoreSepToCamelCaseWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)

	//s := w.window.GetClipboardString()
	s := "get_clipboard_string"
	// E.g., get_clipboard_string -> GetClipboardString
	s += " -> " + UnderscoreSepToCamelCase(s)
	w.size[0] = float64(8 * len(s))
	w.size[1] = 16

	gl.Color3d(0.3, 0.3, 0.3)
	gl.Rectd(0-1, 0-1, gl.Double(w.size[0]+1), gl.Double(w.size[1]+1))
	gl.Color3d(1, 1, 1)
	gl.Rectd(0, 0, gl.Double(w.size[0]), gl.Double(w.size[1]))

	gl.Color3d(0, 0, 0)
	Print(mathgl.Vec2d{0, 0}, s)
}

type ChannelExpeWidget struct {
	Widget
	Content string
	ch      <-chan []byte
	errCh   <-chan error
}

func NewChannelExpeWidget(pos mathgl.Vec2d) *ChannelExpeWidget {
	cmd := exec.Command("ping", "google.com")
	stdout, err := cmd.StdoutPipe()
	CheckError(err)
	err = cmd.Start()
	CheckError(err)

	w := ChannelExpeWidget{Widget: NewWidget(pos, mathgl.Vec2d{0, 0})}
	w.ch, w.errCh = ByteReader(stdout)

	return &w
}

func (w *ChannelExpeWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)

	LongestLine := uint32(0)
	lines := GetLines(w.Content)
	for _, line := range lines {
		lineLength := ExpandedLength(line)
		if lineLength > LongestLine {
			LongestLine = lineLength
		}
	}

	w.size = mathgl.Vec2d{float64(8 * LongestLine), float64(16 * len(lines))}

	gl.Color3d(0.3, 0.3, 0.3)
	gl.Rectd(0-1, 0-1, gl.Double(w.size[0]+1), gl.Double(w.size[1]+1))
	gl.Color3d(1, 1, 1)
	gl.Rectd(0, 0, gl.Double(w.size[0]), gl.Double(w.size[1]))

	gl.Color3d(0, 0, 0)
	for lineNumber, line := range lines {
		Print(mathgl.Vec2d{0, float64(16 * lineNumber)}, line)
	}
}

func (w *ChannelExpeWidget) ProcessTimePassed(timePassed float64) {
	select {
	case b := <-w.ch:
		w.Content += string(b)
		redraw = true
	default:
	}
}

// ---

type SpinnerWidget struct {
	Widget
	Spinner uint32
}

func (w *SpinnerWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Color3d(0, 0, 0)
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
	//gl.Rotated(float64(spinner), 0, 0, 1)
	gl.Rotated(gl.Double(w.Spinner), 0, 0, 1)
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
	case amount == -1:
		cp.caretPosition--
	case amount == +1:
		cp.caretPosition++
	case amount == -2:
		_, y := cp.ExpandedPosition()
		cp.caretPosition = cp.w.lines[y].Start
	case amount == +2:
		_, y := cp.ExpandedPosition()
		cp.caretPosition = cp.w.lines[y].Start + cp.w.lines[y].Length
	case amount == -3:
		cp.caretPosition = 0
	case amount == +3:
		y := len(cp.w.lines) - 1
		cp.caretPosition = cp.w.lines[y].Start + cp.w.lines[y].Length
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
		} else {
			cp.Move(-2)
		}
	case amount == +1:
		if y < uint32(len(cp.w.lines))-1 {
			y++
			line := cp.w.content[cp.w.lines[y].Start : cp.w.lines[y].Start+cp.w.lines[y].Length]
			cp.caretPosition = cp.w.lines[y].Start + ExpandedToLogical(line, cp.targetExpandedX)
		} else {
			cp.Move(+2)
		}
	}
}

func (cp *CaretPosition) SetPositionFromPhysical(xf, yf float64) {
	var y uint32
	if yf < 0 {
		y = 0
	} else if yf >= float64(len(cp.w.lines)*16) {
		y = uint32(len(cp.w.lines) - 1)
	} else {
		y = uint32(yf / 16)
	}

	if xf < 0 {
		cp.targetExpandedX = 0
	} else {
		cp.targetExpandedX = uint32((xf + 4) / 8)
	}

	line := cp.w.content[cp.w.lines[y].Start : cp.w.lines[y].Start+cp.w.lines[y].Length]
	cp.caretPosition = cp.w.lines[y].Start + ExpandedToLogical(line, cp.targetExpandedX)
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

func NewTextBoxWidget(pos mathgl.Vec2d) *TextBoxWidget {
	w := &TextBoxWidget{Widget: NewWidget(pos, mathgl.Vec2d{0, 0})}
	w.caretPosition.w = w
	w.UpdateLines()
	return w
}

func (w *TextBoxWidget) SetContent(content string) {
	w.content = content
	w.UpdateLines()

	if w.caretPosition.caretPosition > uint32(len(w.content)) {
		w.caretPosition.Move(+3)
	}
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
		w.size[0] = float64(8 * 3)
	} else {
		w.size[0] = float64(8 * w.longestLine)
	}
	w.size[1] = float64(16 * len(w.lines))

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
		DrawYBox(w.pos, w.size)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.pos, w.size)
	} else if hasTypingFocus {
		DrawYBox(w.pos, w.size)
	} else {
		DrawBox(w.pos, w.size)
	}

	gl.Color3d(0, 0, 0)
	for lineNumber, contentLine := range w.lines {
		PrintLine(mathgl.Vec2d{w.pos[0], w.pos[1] + float64(16*lineNumber)}, w.content[contentLine.Start:contentLine.Start+contentLine.Length])
	}

	if hasTypingFocus {
		expandedCaretPosition, caretLine := w.caretPosition.ExpandedPosition()

		// Draw caret
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
		gl.Color3d(0, 0, 0)
		gl.Recti(gl.Int(expandedCaretPosition*8-1), gl.Int(caretLine*16), gl.Int(expandedCaretPosition*8+1), gl.Int(caretLine*16)+16)
	}
}
func (w *TextBoxWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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
	hasTypingFocus := len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.Pointer.State.Button(0) == true || inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0) {
		w.caretPosition.SetPositionFromPhysical(inputEvent.Pointer.State.Axes[0]-w.pos[0], inputEvent.Pointer.State.Axes[1]-w.pos[1])
	}

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
				// TODO: Go to start of line-ish (toggle between real start and non-whitespace start)
				// TODO: Rename to TryMove since no check is being made
				w.caretPosition.Move(-2)
			} else if inputEvent.ModifierKey == 0 {
				if w.caretPosition.Logical() >= 1 {
					w.caretPosition.Move(-1)
				}
			}
		case glfw.KeyRight:
			if inputEvent.ModifierKey == glfw.ModSuper {
				// TODO: Rename to TryMove since no check is being made
				w.caretPosition.Move(+2)
			} else if inputEvent.ModifierKey == 0 {
				if w.caretPosition.Logical() < uint32(len(w.content)) {
					w.caretPosition.Move(+1)
				}
			}
		case glfw.KeyUp:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.caretPosition.Move(-3)
			} else if inputEvent.ModifierKey == 0 {
				w.caretPosition.TryMoveV(-1)
			}
		case glfw.KeyDown:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.caretPosition.Move(+3)
			} else if inputEvent.ModifierKey == 0 {
				w.caretPosition.TryMoveV(+1)
			}
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[CHARACTER_EVENT] && inputEvent.InputId < 128 {
		w.SetContent(w.content[:w.caretPosition.Logical()] + string(byte(inputEvent.InputId)) + w.content[w.caretPosition.Logical():])
		w.caretPosition.Move(+1)
	}
}

// ---

type TextFileWidget struct {
	*TextBoxWidget
	path string
}

func NewTextFileWidget(pos mathgl.Vec2d, path string) *TextFileWidget {
	return &TextFileWidget{TextBoxWidget: NewTextBoxWidget(pos), path: path}
}

func (w *TextFileWidget) ProcessTimePassed(timePassed float64) {
	// Check if the file has been changed externally, and if so, override this widget
	NewContent := TryReadFile(w.path)
	if NewContent != w.content {
		w.TextBoxWidget.SetContent(NewContent)
		redraw = true
	}

	w.TextBoxWidget.ProcessTimePassed(timePassed)
}

// ---

type TextFieldWidget struct {
	Widget
	Content       string
	CaretPosition uint32
}

func NewTextFieldWidget(pos mathgl.Vec2d) *TextFieldWidget {
	return &TextFieldWidget{NewWidget(pos, mathgl.Vec2d{0, 0}), "", 0}
}

func (w *TextFieldWidget) Render() {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	// HACK: Setting the widget size in Render() is a bad, because all the input calculations will fail before it's rendered
	if len(w.Content) < 3 {
		w.size[0] = float64(8 * 3)
	} else {
		w.size[0] = float64(8 * len(w.Content))
	}
	w.size[1] = 16

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
		DrawYBox(w.pos, w.size)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.pos, w.size)
	} else if hasTypingFocus {
		DrawYBox(w.pos, w.size)
	} else {
		DrawBox(w.pos, w.size)
	}

	gl.Color3d(0, 0, 0)
	Print(w.pos, w.Content)

	if hasTypingFocus {
		// Draw caret
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
		gl.Color3d(0, 0, 0)
		gl.Recti(gl.Int(w.CaretPosition*8-1), 0, gl.Int(w.CaretPosition*8+1), 16)
	}
}
func (w *TextFieldWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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

	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.Pointer.State.Button(0) == true || inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0) {
		if inputEvent.Pointer.State.Axes[0]-w.pos[0] < 0 {
			w.CaretPosition = 0
		} else if inputEvent.Pointer.State.Axes[0]-w.pos[0] > float64(len(w.Content)*8) {
			w.CaretPosition = uint32(len(w.Content))
		} else {
			w.CaretPosition = uint32((inputEvent.Pointer.State.Axes[0] - w.pos[0] + 4) / 8)
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

func NewMetaTextFieldWidget(pos mathgl.Vec2d) *MetaTextFieldWidget {
	return &MetaTextFieldWidget{NewWidget(pos, mathgl.Vec2d{0, 0}), nil, 0}
}

func (w *MetaTextFieldWidget) Render() {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	// HACK: Setting the widget size in Render() is a bad, because all the input calculations will fail before it's rendered
	if len(w.Content) < 3 {
		w.size[0] = float64(8 * 3)
	} else {
		w.size[0] = float64(8 * len(w.Content))
	}
	w.size[1] = 16

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
		DrawYBox(w.pos, w.size)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.pos, w.size)
	} else if hasTypingFocus {
		DrawYBox(w.pos, w.size)
	} else {
		DrawBox(w.pos, w.size)
	}

	now := time.Now().UnixNano()
	for i, mc := range w.Content {
		age := now - mc.Timestamp
		highlight := gl.Double(age) / 10000000000

		gl.Color3d(1, 1, highlight)
		gl.Rectd(gl.Double(w.pos[0]+float64(i*8)), gl.Double(w.pos[1]), gl.Double(w.pos[0]+float64(i+1)*8), gl.Double(w.pos[1]+16))

		gl.Color3d(0, 0, 0)
		Print(mathgl.Vec2d{w.pos[0] + float64(8*i), w.pos[1]}, string(mc.Character))
	}

	if hasTypingFocus {
		// Draw caret
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
		gl.Color3d(0, 0, 0)
		gl.Recti(gl.Int(w.CaretPosition*8-1), 0, gl.Int(w.CaretPosition*8+1), 16)
	}
}
func (w *MetaTextFieldWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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

	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.Pointer.State.Button(0) == true || inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0) {
		if inputEvent.Pointer.State.Axes[0]-w.pos[0] < 0 {
			w.CaretPosition = 0
		} else if inputEvent.Pointer.State.Axes[0]-w.pos[0] > float64(len(w.Content)*8) {
			w.CaretPosition = uint32(len(w.Content))
		} else {
			w.CaretPosition = uint32((inputEvent.Pointer.State.Axes[0] - w.pos[0] + 4) / 8)
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

func (ps *PointerState) Button(button int) bool {
	if button < len(ps.Buttons) {
		return ps.Buttons[button]
	} else {
		return false
	}
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

		if !katOnly {
			if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.InputId == 0 && inputEvent.EventTypes[AXIS_EVENT] {
				Position := mathgl.Vec2d{float64(inputEvent.Pointer.State.Axes[0]), float64(inputEvent.Pointer.State.Axes[1])}

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
		} else {
			keyboardPointer.OriginMapping[0].ProcessEvent(inputEvent)
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
	glfw.SwapInterval(1) // Vsync

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

	spinner := SpinnerWidget{NewWidget(mathgl.Vec2d{20, 20}, mathgl.Vec2d{0, 0}), 0}
	widgets = append(widgets, &spinner)
	if true {
		widgets = append(widgets, &BoxWidget{NewWidget(mathgl.Vec2d{50, 150}, mathgl.Vec2d{16, 16}), "The Original Box"})
		widgets = append(widgets, &CompositeWidget{NewWidget(mathgl.Vec2d{150, 150}, mathgl.Vec2d{0, 0}),
			[]Widgeter{
				&BoxWidget{NewWidget(mathgl.Vec2d{0, 0}, mathgl.Vec2d{16, 16}), "Left of Duo"},
				&BoxWidget{NewWidget(mathgl.Vec2d{16 + 2, 0}, mathgl.Vec2d{16, 16}), "Right of Duo"},
			},
		})
		widgets = append(widgets, &UnderscoreSepToCamelCaseWidget{NewWidget(mathgl.Vec2d{50, 180}, mathgl.Vec2d{0, 0}), window})
		widgets = append(widgets, NewTextFieldWidget(mathgl.Vec2d{50, 50}))
		widgets = append(widgets, NewMetaTextFieldWidget(mathgl.Vec2d{50, 70}))
		//widgets = append(widgets, NewChannelExpeWidget(mathgl.Vec2d{-100, 210}))
		widgets = append(widgets, NewTextBoxWidget(mathgl.Vec2d{100, 5}))
		widgets = append(widgets, NewTextFileWidget(mathgl.Vec2d{100, 25}, "/Users/Dmitri/Dropbox/Needs Processing/sample.txt"))
		widgets = append(widgets, NewKatWidget(mathgl.Vec2d{370, 20}))
	} else {
		widgets = append(widgets, NewTest1Widget(mathgl.Vec2d{10, 50}))
	}

	mousePointer = &Pointer{VirtualCategory: POINTING}
	keyboardPointer = &Pointer{VirtualCategory: TYPING}
	inputEventQueue := []InputEvent{}

	MousePos := func(w *glfw.Window, x, y float64) {
		redraw = true
		//fmt.Println("MousePos:", x, y)

		//(widgets[len(widgets)-1]).(*CompositeWidget).x = gl.Double(x)
		//(widgets[len(widgets)-1]).(*CompositeWidget).y = gl.Double(y)

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
		offX += gl.Double(xoff * 10)
		offY += gl.Double(yoff * 10)
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

	gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA)
	//gl.ClearColor(0.8, 0.3, 0.01, 1)
	gl.ClearColor(0.85, 0.85, 0.85, 1)

	keyboardPointer.OriginMapping = []Widgeter{widgets[len(widgets)-1]}
	//widgets[len(widgets)-1].(*TextBoxWidget).SetContent("blah\nnew line\n\thello tab\n.\ttab\n..\ttab\n...\ttab\n....\ttab! step by step\n\t\ttwo tabs.")

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

		for _, widget := range widgets {
			widget.ProcessTimePassed(1.0 / 60) // TODO: Use proper value?
		}

		if redraw {
			redraw = false

			gl.Clear(gl.COLOR_BUFFER_BIT)
			gl.LoadIdentity()
			gl.Translated(offX, offY, 0)

			for _, widget := range widgets {
				widget.Render()
			}

			window.SwapBuffers()
			spinner.Spinner++
			//log.Println("swapped buffers")
		} else {
			time.Sleep(5 * time.Millisecond)
		}

		//runtime.Gosched()
	}
}
