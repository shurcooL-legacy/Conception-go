package main

import (
	"fmt"
	"log"
	"math"
	"runtime"
	"time"

	"github.com/go-gl/gl/v2.1/gl"
	"github.com/go-gl/mathgl/mgl64"
	"github.com/goxjs/glfw"
	"github.com/shurcooL/Conception-go/events"
)

var boxUpdated bool

func drawBox() {
	gl.LoadIdentity()
	/*gl.Translatef(50, 100, 0)
	gl.Color3d(201.0/255, 201.0/255, 201.0/255)
	gl.Recti(0, 0, 106, 18)
	if !boxUpdated {
		gl.Color3d(1, 1, 1)
	} else {
		gl.Color3d(0, 1, 0)
	}
	gl.Recti(0+1, 0+1, 106-1, 18-1)*/

	c := mgl64.Vec3{1, 1, 1}
	if boxUpdated {
		c = mgl64.Vec3{42.0 / 255, 154.0 / 255, 254.0 / 255}
	}

	drawInnerRoundedBox(mgl64.Vec2{50, 100},
		mgl64.Vec2{106, 18},
		c.Mul(201.0/255),
		c)
}

func drawInnerRoundedBox(pos, size mgl64.Vec2, borderColor, backgroundColor mgl64.Vec3) {
	if size[0] == 0 || size[1] == 0 {
		return
	}

	const OuterDistance = 2.5
	gl.Begin(gl.POLYGON)
	gl.Color3dv((*float64)(&borderColor[0]))
	gl.Vertex2d(float64(pos[0]+OuterDistance), float64(pos[1]))
	gl.Vertex2d(float64(pos[0]), float64(pos[1]+OuterDistance))
	gl.Vertex2d(float64(pos[0]), float64(pos[1]-OuterDistance+size[1]))
	gl.Vertex2d(float64(pos[0]+OuterDistance), float64(pos[1]+size[1]))
	gl.Vertex2d(float64(pos[0]-OuterDistance+size[0]), float64(pos[1]+size[1]))
	gl.Vertex2d(float64(pos[0]+size[0]), float64(pos[1]-OuterDistance+size[1]))
	gl.Vertex2d(float64(pos[0]+size[0]), float64(pos[1]+OuterDistance))
	gl.Vertex2d(float64(pos[0]-OuterDistance+size[0]), float64(pos[1]))
	gl.End()

	const InnerDistance = OuterDistance + (math.Sqrt2 - 1)
	gl.Begin(gl.POLYGON)
	gl.Color3dv((*float64)(&backgroundColor[0]))
	gl.Vertex2d(float64(pos[0]+InnerDistance), float64(pos[1]+1))
	gl.Vertex2d(float64(pos[0]+1), float64(pos[1]+InnerDistance))
	gl.Vertex2d(float64(pos[0]+1), float64(pos[1]-InnerDistance+size[1]))
	gl.Vertex2d(float64(pos[0]+InnerDistance), float64(pos[1]-1+size[1]))
	gl.Vertex2d(float64(pos[0]-InnerDistance+size[0]), float64(pos[1]-1+size[1]))
	gl.Vertex2d(float64(pos[0]-1+size[0]), float64(pos[1]-InnerDistance+size[1]))
	gl.Vertex2d(float64(pos[0]-1+size[0]), float64(pos[1]+InnerDistance))
	gl.Vertex2d(float64(pos[0]-InnerDistance+size[0]), float64(pos[1]+1))
	gl.End()
}

func drawSpinner(spinner int) {
	gl.LoadIdentity()
	gl.Translatef(30.5, 30.5, 0)
	gl.Rotatef(float32(spinner), 0, 0, 1)
	gl.Color3d(0, 0, 0)
	gl.Rectd(-0.5, -10.5, 0.5, 10.5)
}

func init() {
	runtime.LockOSThread()
}

type nopContextWatcher struct{}

func (nopContextWatcher) OnMakeCurrent(context interface{}) {}
func (nopContextWatcher) OnDetach()                         {}

func main() {
	if err := glfw.Init(nopContextWatcher{}); err != nil {
		panic(err)
	}
	defer glfw.Terminate()

	glfw.WindowHint(glfw.Samples, 8) // Anti-aliasing.

	window, err := glfw.CreateWindow(400, 400, "", nil, nil)
	if err != nil {
		panic(err)
	}
	window.MakeContextCurrent()

	if err := gl.Init(); err != nil {
		panic(err)
	}

	glfw.SwapInterval(1) // Vsync.
	//window.SetPos(50, 600)
	//window.SetPos(1600, 600)
	//window.SetPos(1275, 300)
	//window.SetPos(1200, 300)

	InitFont()
	defer DeinitFont()

	framebufferSizeCallback := func(w *glfw.Window, framebufferSize0, framebufferSize1 int) {
		gl.Viewport(0, 0, int32(framebufferSize0), int32(framebufferSize1))

		var windowSize [2]int
		windowSize[0], windowSize[1] = w.GetSize()

		// Update the projection matrix.
		gl.MatrixMode(gl.PROJECTION)
		gl.LoadIdentity()
		gl.Ortho(0, float64(windowSize[0]), float64(windowSize[1]), 0, -1, 1)
		gl.MatrixMode(gl.MODELVIEW)
	}
	{
		var framebufferSize [2]int
		framebufferSize[0], framebufferSize[1] = window.GetFramebufferSize()
		framebufferSizeCallback(window, framebufferSize[0], framebufferSize[1])
	}
	window.SetFramebufferSizeCallback(framebufferSizeCallback)

	var inputEventQueue []events.InputEvent
	mousePointer = &events.Pointer{VirtualCategory: events.POINTING}

	window.SetMouseMovementCallback(func(w *glfw.Window, xpos, ypos, xdelta, ydelta float64) {
		inputEvent := events.InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[events.EventType]struct{}{events.SLIDER_EVENT: {}},
			InputId:    0,
			Buttons:    nil,
			Sliders:    []float64{xdelta, ydelta},
		}
		if w.GetInputMode(glfw.CursorMode) != glfw.CursorDisabled {
			inputEvent.EventTypes[events.AXIS_EVENT] = struct{}{}
			inputEvent.Axes = []float64{xpos, ypos}
		}
		inputEventQueue = events.EnqueueInputEvent(inputEventQueue, inputEvent)
	})

	window.SetMouseButtonCallback(func(w *glfw.Window, button glfw.MouseButton, action glfw.Action, mods glfw.ModifierKey) {
		inputEvent := events.InputEvent{
			Pointer:     mousePointer,
			EventTypes:  map[events.EventType]struct{}{events.BUTTON_EVENT: {}},
			InputId:     uint16(button),
			Buttons:     []bool{action != glfw.Release},
			Sliders:     nil,
			Axes:        nil,
			ModifierKey: uint8(mods),
		}
		inputEventQueue = events.EnqueueInputEvent(inputEventQueue, inputEvent)
	})

	go func() {
		<-time.After(5 * time.Second)
		log.Println("trigger!")
		boxUpdated = true

		glfw.PostEmptyEvent()
	}()

	gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA) // For font.

	//gl.ClearColor(0.8, 0.3, 0.01, 1)
	gl.ClearColor(247.0/255, 247.0/255, 247.0/255, 1)

	var spinner int

	var widgets []events.Widgeter

	widgets = append(widgets, NewButtonWidget(mgl64.Vec2{50, 200}, func() { fmt.Println("button triggered") }))

	for !window.ShouldClose() && glfw.Press != window.GetKey(glfw.KeyEscape) {
		glfw.WaitEvents()
		//glfw.PollEvents()

		// Process Input.
		inputEventQueue = events.ProcessInputEventQueue(inputEventQueue, widgets[0])

		gl.Clear(gl.COLOR_BUFFER_BIT)

		for _, widget := range widgets {
			widget.Render()
		}

		drawSpinner(spinner)
		spinner++

		drawBox()

		gl.LoadIdentity()
		gl.Color3d(1, 0, 0)
		NewOpenGlStream(mgl64.Vec2{50, 300}).PrintText(` !"#$%&'()*+,-./
0123456789:;<=>?
@ABCDEFGHIJKLMNO
PQRSTUVWXYZ[\]^_
` + "`" + `abcdefghijklmno
pqrstuvwxyz{|}~`)

		window.SwapBuffers()
		log.Println("swapped buffers")

		//runtime.Gosched()
	}
}

// =====

var mousePointer *events.Pointer

// ---

type Widget struct {
	pos           mgl64.Vec2
	size          mgl64.Vec2
	hoverPointers map[*events.Pointer]bool
	parent        events.Widgeter

	//DepNode
}

func NewWidget(pos, size mgl64.Vec2) Widget {
	return Widget{pos: pos, size: size, hoverPointers: map[*events.Pointer]bool{}}
}

func (_ *Widget) PollLogic()   {}
func (_ *Widget) Close() error { return nil }
func (w *Widget) Layout() {
	if w.parent != nil {
		w.parent.Layout()
	}
}
func (_ *Widget) LayoutNeeded() {}
func (_ *Widget) Render()       {}
func (w *Widget) Hit(ParentPosition mgl64.Vec2) []events.Widgeter {
	LocalPosition := w.ParentToLocal(ParentPosition)

	Hit := (LocalPosition[0] >= 0 &&
		LocalPosition[1] >= 0 &&
		LocalPosition[0] <= w.size[0] &&
		LocalPosition[1] <= w.size[1])

	if Hit {
		return []events.Widgeter{w}
	} else {
		return nil
	}
}
func (w *Widget) ProcessEvent(inputEvent events.InputEvent) {}
func (_ *Widget) ContainsWidget(widget, target events.Widgeter) bool {
	return widget == target
}

func (w *Widget) Pos() *mgl64.Vec2  { return &w.pos }
func (w *Widget) Size() *mgl64.Vec2 { return &w.size }

func (w *Widget) HoverPointers() map[*events.Pointer]bool {
	return w.hoverPointers
}

func (w *Widget) Parent() events.Widgeter     { return w.parent }
func (w *Widget) SetParent(p events.Widgeter) { w.parent = p }

func (w *Widget) ParentToLocal(ParentPosition mgl64.Vec2) (LocalPosition mgl64.Vec2) {
	return ParentPosition.Sub(w.pos)
}

type WidgeterS struct{ events.Widgeter }

func (w WidgeterS) GlobalToParent(GlobalPosition mgl64.Vec2) (ParentPosition mgl64.Vec2) {
	switch w.Parent() {
	case nil:
		ParentPosition = GlobalPosition
	default:
		ParentPosition = WidgeterS{w.Parent()}.GlobalToLocal(GlobalPosition)
	}
	return ParentPosition
}
func (w WidgeterS) GlobalToLocal(GlobalPosition mgl64.Vec2) (LocalPosition mgl64.Vec2) {
	return w.ParentToLocal(WidgeterS{w}.GlobalToParent(GlobalPosition))
}

// ---

type ButtonWidget struct {
	Widget
	action func()
}

func NewButtonWidget(pos mgl64.Vec2, action func()) *ButtonWidget {
	w := &ButtonWidget{Widget: NewWidget(pos, mgl64.Vec2{106, 18})}
	//w := &ButtonWidget{Widget: NewWidget(pos, mgl64.Vec2{122, 18})}
	w.setAction(action)

	return w
}

func (w *ButtonWidget) setAction(action func()) {
	w.action = action
}

var (
	nearlyWhiteColor = mgl64.Vec3{0.975, 0.975, 0.975}
	grayColor        = mgl64.Vec3{0.75, 0.75, 0.75}
	highlightColor   = mgl64.Vec3{0.898, 0.765, 0.396} // Yellowish on-hover border color.
)

func (w *ButtonWidget) Render() {
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
		//DrawGBox(w.pos, w.size)
		//drawInnerRoundedBox(w.pos, w.size, highlightColor, grayColor)
		c := mgl64.Vec3{42.0 / 255, 154.0 / 255, 254.0 / 255}
		drawInnerRoundedBox(w.pos, w.size, c.Mul(201.0/255), c)
		gl.Color3d(1, 1, 1)
		//} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		//	//DrawYBox(w.pos, w.size)
		//	drawInnerRoundedBox(w.pos, w.size, highlightColor, nearlyWhiteColor)
	} else {
		//DrawNBox(w.pos, w.size)
		//drawInnerRoundedBox(w.pos, w.size, mgl64.Vec3{0.3, 0.3, 0.3}, nearlyWhiteColor)
		c := mgl64.Vec3{1, 1, 1}
		drawInnerRoundedBox(w.pos, w.size, c.Mul(201.0/255), c)
		gl.Color3d(0, 0, 0)
	}

	NewOpenGlStream(w.pos.Add(mgl64.Vec2{8, 3})).PrintText("Software Update...")
}
func (w *ButtonWidget) Hit(ParentPosition mgl64.Vec2) []events.Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []events.Widgeter{w}
	} else {
		return nil
	}
}

func (w *ButtonWidget) ProcessEvent(inputEvent events.InputEvent) {
	//if _, buttonEvent := inputEvent.EventTypes[events.BUTTON_EVENT]; inputEvent.Pointer.VirtualCategory == events.POINTING && buttonEvent && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
	if inputEvent.Pointer.VirtualCategory == events.POINTING && inputEvent.EventTypes.Has(events.BUTTON_EVENT) && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		if w.action != nil {
			w.action()
			//println(GetSourceAsString(w.action))

			w.Layout()
			//w.NotifyAllListeners()
		}
	}
}
