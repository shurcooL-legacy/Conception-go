package main

import (
	"runtime"

	"github.com/go-gl/mathgl/mgl64"
	"github.com/shurcooL/Conception-go/events"
	glfw "github.com/shurcooL/goglfw"
	"github.com/shurcooL/webgl"
)

var mousePointer *events.Pointer
var keyboardPointer *events.Pointer

func init() {
	runtime.LockOSThread()
}

var gl *webgl.Context

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

	gl = window.Context

	glfw.SwapInterval(1) // Vsync.

	framebufferSizeCallback := func(w *glfw.Window, framebufferSize0, framebufferSize1 int) {
		gl.Viewport(0, 0, framebufferSize0, framebufferSize1)

		var windowSize [2]int
		windowSize[0], windowSize[1] = w.GetSize()
		_, _ = windowSize[0], windowSize[1]

		/*inputEvent := InputEvent{
			Pointer:    windowPointer,
			EventTypes: map[EventType]struct{}{AXIS_EVENT: struct{}{}},
			InputId:    0,
			Buttons:    nil,
			Sliders:    nil,
			Axes:       []float64{float64(windowSize[0]), float64(windowSize[1])},
		}
		inputEventQueue = events.EnqueueInputEvent(inputEventQueue, inputEvent)*/
	}
	{
		var framebufferSize [2]int
		framebufferSize[0], framebufferSize[1] = window.GetFramebufferSize()
		framebufferSizeCallback(window, framebufferSize[0], framebufferSize[1])
	}
	window.SetFramebufferSizeCallback(framebufferSizeCallback)

	var inputEventQueue []events.InputEvent
	mousePointer = &events.Pointer{VirtualCategory: events.POINTING}
	keyboardPointer = &events.Pointer{VirtualCategory: events.TYPING}

	var lastMousePos mgl64.Vec2
	lastMousePos[0], lastMousePos[1] = window.GetCursorPos()
	MousePos := func(w *glfw.Window, x, y float64) {
		inputEvent := events.InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[events.EventType]struct{}{events.SLIDER_EVENT: {}},
			InputId:    0,
			Buttons:    nil,
			Sliders:    []float64{x - lastMousePos[0], y - lastMousePos[1]}, // TODO: Do this in a pointer general way?
		}
		if w.GetInputMode(glfw.CursorMode) != glfw.CursorDisabled {
			inputEvent.EventTypes[events.AXIS_EVENT] = struct{}{}
			inputEvent.Axes = []float64{x, y}
		}
		lastMousePos[0] = x
		lastMousePos[1] = y
		inputEventQueue = events.EnqueueInputEvent(inputEventQueue, inputEvent)
	}
	window.SetCursorPosCallback(MousePos)
	//MousePos(window, lastMousePos[0], lastMousePos[1])

	window.SetScrollCallback(func(w *glfw.Window, xoff float64, yoff float64) {
		inputEvent := events.InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[events.EventType]struct{}{events.SLIDER_EVENT: {}},
			InputId:    2,
			Buttons:    nil,
			Sliders:    []float64{yoff, xoff},
			Axes:       nil,
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

	window.SetKeyCallback(func(w *glfw.Window, key glfw.Key, scancode int, action glfw.Action, mods glfw.ModifierKey) {
		inputEvent := events.InputEvent{
			Pointer:     keyboardPointer,
			EventTypes:  map[events.EventType]struct{}{events.BUTTON_EVENT: {}},
			InputId:     uint16(key),
			Buttons:     []bool{action != glfw.Release},
			Sliders:     nil,
			Axes:        nil,
			ModifierKey: uint8(mods),
		}
		inputEventQueue = events.EnqueueInputEvent(inputEventQueue, inputEvent)

		// HACK.
		switch {
		case key == glfw.Key1 && action == glfw.Press:
			window.SetInputMode(glfw.CursorMode, glfw.CursorNormal)
		case key == glfw.Key2 && action == glfw.Press:
			window.SetInputMode(glfw.CursorMode, glfw.CursorHidden)
		case key == glfw.Key3 && action == glfw.Press:
			window.SetInputMode(glfw.CursorMode, glfw.CursorDisabled)
		}
	})

	window.SetCharCallback(func(w *glfw.Window, char rune) {
		inputEvent := events.InputEvent{
			Pointer:    keyboardPointer,
			EventTypes: map[events.EventType]struct{}{events.CHARACTER_EVENT: {}},
			InputId:    uint16(char),
			Buttons:    nil,
			Sliders:    nil,
			Axes:       nil,
		}
		inputEventQueue = events.EnqueueInputEvent(inputEventQueue, inputEvent)
	})

	gl.ClearColor(0.85, 0.85, 0.85, 1)

	for !window.ShouldClose() {
		glfw.PollEvents()

		// Process Input.
		inputEventQueue = events.ProcessInputEventQueue(inputEventQueue)

		gl.Clear(gl.COLOR_BUFFER_BIT)

		mousePointer.Render()

		window.SwapBuffers()
		runtime.Gosched()
	}
}
