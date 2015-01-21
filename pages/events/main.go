package main

import (
	"runtime"

	"github.com/go-gl/glow/gl/2.1/gl"
	"github.com/go-gl/mathgl/mgl64"
	"github.com/shurcooL/Conception-go/events"
	glfw "github.com/shurcooL/glfw3"
)

var mousePointer *events.Pointer
var keyboardPointer *events.Pointer

func init() {
	runtime.LockOSThread()
}

func main() {
	if err := glfw.Init(); err != nil {
		panic(err)
	}
	defer glfw.Terminate()

	window, err := glfw.CreateWindow(400, 400, "", nil, nil)
	if err != nil {
		panic(err)
	}
	window.MakeContextCurrent()

	//window.SetInputMode(glfw.Cursor, glfw.CursorHidden)

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

	var inputEventQueue []events.InputEvent
	mousePointer = &events.Pointer{VirtualCategory: events.POINTING}
	keyboardPointer = &events.Pointer{VirtualCategory: events.TYPING}

	var lastMousePos mgl64.Vec2
	lastMousePos[0], lastMousePos[1], _ = window.GetCursorPosition()
	MousePos := func(w *glfw.Window, x, y float64) {
		inputEvent := events.InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[events.EventType]bool{events.SLIDER_EVENT: true, events.AXIS_EVENT: true},
			InputId:    0,
			Buttons:    nil,
			Sliders:    []float64{x - lastMousePos[0], y - lastMousePos[1]}, // TODO: Do this in a pointer general way?
			Axes:       []float64{x, y},
		}
		lastMousePos[0] = x
		lastMousePos[1] = y
		inputEventQueue = events.EnqueueInputEvent(inputEvent, inputEventQueue)
	}
	window.SetCursorPositionCallback(MousePos)
	//MousePos(window, lastMousePos[0], lastMousePos[1])

	window.SetScrollCallback(func(w *glfw.Window, xoff float64, yoff float64) {
		inputEvent := events.InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[events.EventType]bool{events.SLIDER_EVENT: true},
			InputId:    2,
			Buttons:    nil,
			Sliders:    []float64{yoff, xoff},
			Axes:       nil,
		}
		inputEventQueue = events.EnqueueInputEvent(inputEvent, inputEventQueue)
	})

	window.SetMouseButtonCallback(func(w *glfw.Window, button glfw.MouseButton, action glfw.Action, mods glfw.ModifierKey) {
		inputEvent := events.InputEvent{
			Pointer:     mousePointer,
			EventTypes:  map[events.EventType]bool{events.BUTTON_EVENT: true},
			InputId:     uint16(button),
			Buttons:     []bool{action != glfw.Release},
			Sliders:     nil,
			Axes:        nil,
			ModifierKey: mods,
		}
		inputEventQueue = events.EnqueueInputEvent(inputEvent, inputEventQueue)
	})

	window.SetKeyCallback(func(w *glfw.Window, key glfw.Key, scancode int, action glfw.Action, mods glfw.ModifierKey) {
		inputEvent := events.InputEvent{
			Pointer:     keyboardPointer,
			EventTypes:  map[events.EventType]bool{events.BUTTON_EVENT: true},
			InputId:     uint16(key),
			Buttons:     []bool{action != glfw.Release},
			Sliders:     nil,
			Axes:        nil,
			ModifierKey: mods,
		}
		inputEventQueue = events.EnqueueInputEvent(inputEvent, inputEventQueue)
	})

	window.SetCharCallback(func(w *glfw.Window, char rune) {
		inputEvent := events.InputEvent{
			Pointer:    keyboardPointer,
			EventTypes: map[events.EventType]bool{events.CHARACTER_EVENT: true},
			InputId:    uint16(char),
			Buttons:    nil,
			Sliders:    nil,
			Axes:       nil,
		}
		inputEventQueue = events.EnqueueInputEvent(inputEvent, inputEventQueue)
	})

	gl.ClearColor(0.85, 0.85, 0.85, 1)

	for !mustBool(window.ShouldClose()) {
		glfw.PollEvents()

		// Process Input.
		inputEventQueue = events.ProcessInputEventQueue(inputEventQueue)

		gl.Clear(gl.COLOR_BUFFER_BIT)

		mousePointer.Render()

		window.SwapBuffers()
		runtime.Gosched()
	}
}

// ---

func mustBool(b bool, err error) bool {
	if err != nil {
		panic(err)
	}
	return b
}
