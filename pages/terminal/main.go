// terminal is a simple terminal app.
package main

import (
	"go/build"
	"log"
	"os"
	"runtime"

	"github.com/go-gl/gl/v2.1/gl"
	"github.com/go-gl/mathgl/mgl64"
	"github.com/goxjs/glfw"
	"github.com/shurcooL/Conception-go/events"
)

const (
	borderX, borderY = 5, 4
)

func init() {
	runtime.LockOSThread()
}

func main() {
	if err := glfw.Init(nopContextWatcher{}); err != nil {
		log.Fatalln(err)
	}
	defer glfw.Terminate()

	glfw.WindowHint(glfw.Resizable, gl.FALSE)
	window, err := glfw.CreateWindow(80*fontWidth+borderX*2, 25*fontHeight+borderY*2, "Terminal — 80×25", nil, nil)
	if err != nil {
		log.Fatalln(err)
	}
	window.MakeContextCurrent()

	if err := gl.Init(); err != nil {
		log.Fatalln(err)
	}

	// Clear screen right away.
	gl.ClearColor(1, 1, 1, 1)
	gl.Clear(gl.COLOR_BUFFER_BIT)
	window.SwapBuffers()

	glfw.SwapInterval(1) // Vsync.

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

		/*inputEvent := InputEvent{
			Pointer:    windowPointer,
			EventTypes: map[EventType]struct{}{AxisEvent: struct{}{}},
			InputID:    0,
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

	var mousePointer = &events.Pointer{VirtualCategory: events.Pointing}
	var keyboardPointer = &events.Pointer{VirtualCategory: events.Typing}
	var inputEventQueue []events.InputEvent

	window.SetMouseMovementCallback(func(w *glfw.Window, xpos, ypos, xdelta, ydelta float64) {
		inputEvent := events.InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[events.EventType]struct{}{events.SliderEvent: {}},
			InputID:    0,
			Buttons:    nil,
			Sliders:    []float64{xdelta, ydelta},
		}
		if w.GetInputMode(glfw.CursorMode) != glfw.CursorDisabled {
			inputEvent.EventTypes[events.AxisEvent] = struct{}{}
			inputEvent.Axes = []float64{xpos, ypos}
		}
		inputEventQueue = events.EnqueueInputEvent(inputEventQueue, inputEvent)
	})

	window.SetScrollCallback(func(w *glfw.Window, xoff float64, yoff float64) {
		inputEvent := events.InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[events.EventType]struct{}{events.SliderEvent: {}},
			InputID:    2,
			Buttons:    nil,
			Sliders:    []float64{yoff, xoff},
			Axes:       nil,
		}
		inputEventQueue = events.EnqueueInputEvent(inputEventQueue, inputEvent)
	})

	window.SetMouseButtonCallback(func(w *glfw.Window, button glfw.MouseButton, action glfw.Action, mods glfw.ModifierKey) {
		inputEvent := events.InputEvent{
			Pointer:     mousePointer,
			EventTypes:  map[events.EventType]struct{}{events.ButtonEvent: {}},
			InputID:     uint16(button),
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
			EventTypes:  map[events.EventType]struct{}{events.ButtonEvent: {}},
			InputID:     uint16(key),
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
			EventTypes: map[events.EventType]struct{}{events.CharacterEvent: {}},
			InputID:    uint16(char),
			Buttons:    nil,
			Sliders:    nil,
			Axes:       nil,
		}
		inputEventQueue = events.EnqueueInputEvent(inputEventQueue, inputEvent)
	})

	var framebufferSize [2]int
	framebufferSize[0], framebufferSize[1] = window.GetFramebufferSize()

	app := &app{window: window}

	for !window.ShouldClose() {
		glfw.PollEvents()

		// Process Input.
		inputEventQueue = app.processInputEventQueue(inputEventQueue)

		gl.Clear(gl.COLOR_BUFFER_BIT)

		/*gl.Color3d(0.5, 0.5, 0.5)
		gl.Begin(gl.TRIANGLE_FAN)
		gl.Vertex2i(0, 0)
		gl.Vertex2i(int32(framebufferSize[0]), 0)
		gl.Vertex2i(int32(framebufferSize[0]), 1)
		gl.Vertex2i(0, 1)
		gl.End()*/

		gl.Color3d(0, 0, 0)
		NewOpenGlStream(mgl64.Vec2{borderX, borderY}).PrintText(` !"#$%&'()*+,-./
0123456789:;<=>?
@ABCDEFGHIJKLMNO
PQRSTUVWXYZ[\]^_
` + "`" + `abcdefghijklmno
pqrstuvwxyz{|}~`)

		mousePointer.Render()

		window.SwapBuffers()
		runtime.Gosched()
	}
}

type app struct {
	window *glfw.Window
}

func (a *app) processInputEventQueue(inputEventQueue []events.InputEvent) []events.InputEvent {
	for len(inputEventQueue) > 0 {
		inputEvent := inputEventQueue[0]

		//fmt.Println(inputEvent)
		_ = inputEvent
		//spew.Dump(inputEvent)

		if inputEvent.Pointer.VirtualCategory == events.Typing && inputEvent.EventTypes.Has(events.ButtonEvent) && glfw.Key(inputEvent.InputID) == glfw.KeyW && inputEvent.Buttons[0] && glfw.ModifierKey(inputEvent.ModifierKey) == glfw.ModSuper {
			a.window.SetShouldClose(true)
		}

		inputEventQueue = inputEventQueue[1:]
	}

	return inputEventQueue
}

type nopContextWatcher struct{}

func (nopContextWatcher) OnMakeCurrent(context interface{}) {}
func (nopContextWatcher) OnDetach()                         {}

// Set the working directory to the root of Conception-go package, so that its assets can be accessed.
func init() {
	// importPathToDir resolves the absolute path from importPath.
	// There doesn't need to be a valid Go package inside that import path,
	// but the directory must exist.
	importPathToDir := func(importPath string) (string, error) {
		p, err := build.Import(importPath, "", build.FindOnly)
		if err != nil {
			return "", err
		}
		return p.Dir, nil
	}

	dir, err := importPathToDir("github.com/shurcooL/Conception-go")
	if err != nil {
		log.Fatalln("Unable to find github.com/shurcooL/Conception-go package in your GOPATH, it's needed to load assets:", err)
	}
	err = os.Chdir(dir)
	if err != nil {
		log.Panicln("os.Chdir:", err)
	}
}
