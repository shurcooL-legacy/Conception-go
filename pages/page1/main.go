package main

import (
	"math"
	"math/rand"
	"runtime"
	"time"

	"github.com/go-gl/glow/gl/2.1/gl"
	"github.com/go-gl/mathgl/mgl64"
	glfw "github.com/shurcooL/goglfw"
)

// TODO: Remove these
var globalWindow *glfw.Window

var mousePointer *Pointer

func init() {
	runtime.LockOSThread()
}

func main() {
	if err := glfw.Init(); err != nil {
		panic(err)
	}
	defer glfw.Terminate()

	window, err := glfw.CreateWindow(1536, 960, "", nil, nil)
	if err != nil {
		panic(err)
	}
	globalWindow = window
	window.MakeContextCurrent()

	window.SetInputMode(glfw.Cursor, glfw.CursorHidden)

	if err := gl.Init(); nil != err {
		panic(err)
	}

	glfw.SwapInterval(1) // Vsync

	framebufferSizeCallback := func(w *glfw.Window, framebufferSize0, framebufferSize1 int) {
		gl.Viewport(0, 0, int32(framebufferSize0), int32(framebufferSize1))

		var windowSize [2]int
		windowSize[0], windowSize[1], _ = w.GetSize()

		// Update the projection matrix
		gl.MatrixMode(gl.PROJECTION)
		gl.LoadIdentity()
		gl.Ortho(0, float64(windowSize[0]), float64(windowSize[1]), 0, -1, 1)
		gl.MatrixMode(gl.MODELVIEW)

		/*inputEvent := InputEvent{
			Pointer:    windowPointer,
			EventTypes: map[EventType]bool{AXIS_EVENT: true},
			InputId:    0,
			Buttons:    nil,
			Sliders:    nil,
			Axes:       []float64{float64(windowSize[0]), float64(windowSize[1])},
		}
		inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)*/
	}
	{
		var framebufferSize [2]int
		framebufferSize[0], framebufferSize[1], _ = window.GetFramebufferSize()
		framebufferSizeCallback(window, framebufferSize[0], framebufferSize[1])
	}
	window.SetFramebufferSizeCallback(framebufferSizeCallback)

	var inputEventQueue []InputEvent
	mousePointer = &Pointer{VirtualCategory: POINTING}

	var lastMousePos mgl64.Vec2
	lastMousePos[0], lastMousePos[1], _ = window.GetCursorPosition()
	MousePos := func(w *glfw.Window, x, y float64) {
		//fmt.Println("MousePos:", x, y)

		inputEvent := InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[EventType]bool{SLIDER_EVENT: true, AXIS_EVENT: true},
			InputId:    0,
			Buttons:    nil,
			Sliders:    []float64{x - lastMousePos[0], y - lastMousePos[1]}, // TODO: Do this in a pointer general way?
			Axes:       []float64{x, y},
		}
		lastMousePos[0] = x
		lastMousePos[1] = y
		inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
	}
	window.SetCursorPositionCallback(MousePos)
	MousePos(window, lastMousePos[0], lastMousePos[1])

	gl.ClearColor(0.85, 0.85, 0.85, 1)

	rand.Seed(4)
	var widget = newMultitouchTestBoxWidget(mgl64.Vec2{600, 300}, rand.Intn(6))
	var widget2 = newMultitouchTestBoxWidget(mgl64.Vec2{600 + 210, 300 + 210}, rand.Intn(6))
	var widget3 = newMultitouchTestBoxWidget(mgl64.Vec2{600 + 210, 300}, rand.Intn(6))
	var widget4 = newMultitouchTestBoxWidget(mgl64.Vec2{600, 300 + 210}, rand.Intn(6))

	for !mustBool(window.ShouldClose()) {
		glfw.PollEvents()

		// Process Input.
		inputEventQueue = ProcessInputEventQueue(inputEventQueue)

		gl.Clear(gl.COLOR_BUFFER_BIT)

		widget.Render()
		widget2.Render()
		widget3.Render()
		widget4.Render()

		mousePointer.Render()

		window.SwapBuffers()
		runtime.Gosched()
	}
}

type MultitouchTestBoxWidget struct {
	pos   mgl64.Vec2
	color int

	buffer uint32
}

func newMultitouchTestBoxWidget(pos mgl64.Vec2, color int) MultitouchTestBoxWidget {
	var buffer uint32
	gl.GenBuffers(1, &buffer)
	gl.BindBuffer(gl.ARRAY_BUFFER, buffer)
	vertices := []float32{
		0, 0,
		0, 200,
		200, 200,
		200, 0,
	}
	gl.BufferData(gl.ARRAY_BUFFER, len(vertices)*4, gl.Ptr(vertices), gl.STATIC_DRAW)

	return MultitouchTestBoxWidget{pos: pos, color: color, buffer: buffer}
}

func (w *MultitouchTestBoxWidget) Render() {
	colors := [...]mgl64.Vec3{
		{0 / 255.0, 140 / 255.0, 0 / 255.0},
		{0 / 255.0, 98 / 255.0, 140 / 255.0},
		{194 / 255.0, 74 / 255.0, 0 / 255.0},
		{89 / 255.0, 0 / 255.0, 140 / 255.0},
		{191 / 255.0, 150 / 255.0, 0 / 255.0},
		{140 / 255.0, 0 / 255.0, 0 / 255.0},
	}

	backgroundColor := colors[w.color]

	//borderColor := backgroundColor

	switch 1 {
	case 0:
		DrawBorderlessBox(w.pos, mgl64.Vec2{200, 200}, backgroundColor)
	case 1:
		gl.PushMatrix()
		gl.Translated(w.pos[0], w.pos[1], 0)
		gl.Color3dv(&backgroundColor[0])

		gl.EnableClientState(gl.VERTEX_ARRAY)
		gl.BindBuffer(gl.ARRAY_BUFFER, w.buffer)
		gl.VertexPointer(2, gl.FLOAT, 0, nil)

		gl.DrawArrays(gl.TRIANGLE_FAN, 0, 4)

		gl.PopMatrix()
	}
}

// ---

func DrawBorderlessBox(pos, size mgl64.Vec2, backgroundColor mgl64.Vec3) {
	gl.Color3dv(&backgroundColor[0])
	gl.Rectd(float64(pos[0]), float64(pos[1]), float64(pos.Add(size)[0]), float64(pos.Add(size)[1]))
}

func DrawBox(pos, size mgl64.Vec2, borderColor, backgroundColor mgl64.Vec3) {
	DrawBorderlessBox(pos.Add(mgl64.Vec2{-1, -1}), size.Add(mgl64.Vec2{2, 2}), borderColor)
	DrawBorderlessBox(pos, size, backgroundColor)
}

// ---

func mustBool(b bool, err error) bool {
	if err != nil {
		panic(err)
	}
	return b
}

// =====

type VirtualCategory uint8

const (
	TYPING VirtualCategory = iota
	POINTING
	WINDOWING
)

type Pointer struct {
	VirtualCategory VirtualCategory
	//Mapping         Widgeters // Always reflects current pointer state.
	//OriginMapping   Widgeters // Updated only when pointer is moved while not active (e.g., where mouse button was first pressed down).
	State PointerState
}

func (this *Pointer) Render() {
	switch {
	case this.VirtualCategory == POINTING && len(this.State.Axes) >= 2:
		// Prevent pointer from being drawn when the OS mouse pointer is visible.
		{
			// HACK
			var windowSize [2]int
			if globalWindow != nil {
				windowSize[0], windowSize[1], _ = globalWindow.GetSize()
			}

			// HACK: OS X specific.
			const border = 3
			if this.State.Axes[1] < 0 || this.State.Axes[0] < border || this.State.Axes[0] >= float64(windowSize[0])-border || this.State.Axes[1] >= float64(windowSize[1])-border {
				break
			}
		}

		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translated(float64(NearInt64(this.State.Axes[0]))+0.5, float64(NearInt64(this.State.Axes[1]))+0.5, 0)

		const size float64 = 12 * 40
		gl.Color3d(1, 1, 1)
		gl.Begin(gl.TRIANGLE_FAN)
		gl.Vertex2d(0, 0)
		gl.Vertex2d(0, size)
		gl.Vertex2d(size*0.85*math.Sin(math.Pi/8), size*0.85*math.Cos(math.Pi/8))
		gl.Vertex2d(size/math.Sqrt2, size/math.Sqrt2)
		gl.End()

		gl.Begin(gl.LINE_LOOP)
		gl.Color3d(0, 0, 0)
		gl.Vertex2d(0, 0)
		gl.Vertex2d(0, size)
		gl.Color3d(0.75, 0.75, 0.75)
		gl.Vertex2d(size*0.85*math.Sin(math.Pi/8), size*0.85*math.Cos(math.Pi/8))
		gl.Color3d(0, 0, 0)
		gl.Vertex2d(size/math.Sqrt2, size/math.Sqrt2)
		gl.End()
	}
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
	InputId    uint16
	// TODO: Add pointers to BeforeState and AfterState?

	Buttons []bool
	// TODO: Characters? Split into distinct event types, bundle up in an event frame based on time?
	Sliders     []float64
	Axes        []float64
	ModifierKey glfw.ModifierKey // HACK
}

func ProcessInputEventQueue(inputEventQueue []InputEvent) []InputEvent {
	for len(inputEventQueue) > 0 {
		/*inputEvent := inputEventQueue[0]

		// TODO: Calculate whether a pointing pointer moved relative to canvas in a better way... what if canvas is moved via keyboard, etc.
		pointingPointerMovedRelativeToCanvas := inputEvent.Pointer.VirtualCategory == POINTING &&
			(inputEvent.EventTypes[AXIS_EVENT] && inputEvent.InputId == 0 || inputEvent.EventTypes[SLIDER_EVENT] && inputEvent.InputId == 2)

		if pointingPointerMovedRelativeToCanvas {
			LocalPosition := mgl64.Vec2{float64(inputEvent.Pointer.State.Axes[0]), float64(inputEvent.Pointer.State.Axes[1])}

			// Clear previously hit widgets
			for _, widget := range inputEvent.Pointer.Mapping {
				delete(widget.HoverPointers(), inputEvent.Pointer)
			}
			inputEvent.Pointer.Mapping = []Widgeter{}

			// Recalculate currently hit widgets
			inputEvent.Pointer.Mapping = append(inputEvent.Pointer.Mapping, widget.Hit(LocalPosition)...)
			for _, widget := range inputEvent.Pointer.Mapping {
				widget.HoverPointers()[inputEvent.Pointer] = true
			}
		}

		// Populate OriginMapping (but only when pointer is moved while not active, and this isn't a deactivation since that's handled below)
		if pointingPointerMovedRelativeToCanvas &&
			!inputEvent.EventTypes[POINTER_DEACTIVATION] && !inputEvent.Pointer.State.IsActive() {

			inputEvent.Pointer.OriginMapping = make([]Widgeter, len(inputEvent.Pointer.Mapping))
			copy(inputEvent.Pointer.OriginMapping, inputEvent.Pointer.Mapping)
		}

		for _, widget := range inputEvent.Pointer.OriginMapping {
			widget.ProcessEvent(inputEvent)
		}

		// Populate OriginMapping (but only upon pointer deactivation event)
		if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[POINTER_DEACTIVATION] {

			inputEvent.Pointer.OriginMapping = make([]Widgeter, len(inputEvent.Pointer.Mapping))
			copy(inputEvent.Pointer.OriginMapping, inputEvent.Pointer.Mapping)
		}*/

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

	switch {
	case !preStateActive && postStateActive:
		inputEvent.EventTypes[POINTER_ACTIVATION] = true
	case preStateActive && !postStateActive:
		inputEvent.EventTypes[POINTER_DEACTIVATION] = true
	}

	return append(inputEventQueue, inputEvent)
}

// =====

func NearInt64(value float64) int64 {
	if value >= 0 {
		return int64(value + 0.5)
	} else {
		return int64(value - 0.5)
	}
}
