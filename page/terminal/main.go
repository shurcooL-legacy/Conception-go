// terminal is a simple terminal app.
package main

import (
	"go/build"
	"log"
	"os"
	"runtime"
	"strings"

	"github.com/go-gl/gl/v2.1/gl"
	"github.com/go-gl/mathgl/mgl64"
	"github.com/goxjs/glfw"
	"github.com/shurcooL-legacy/Conception-go/event"
)

const (
	borderX, borderY = 5, 4
)

func init() {
	runtime.LockOSThread()
}

const prompt = "~ $ "

var text = prompt

func run() error {
	if err := glfw.Init(nopContextWatcher{}); err != nil {
		return err
	}
	defer glfw.Terminate()

	glfw.WindowHint(glfw.Resizable, gl.FALSE)
	window, err := glfw.CreateWindow(80*fontWidth+borderX*2, 25*fontHeight+borderY*2, "Terminal — 80×25", nil, nil)
	if err != nil {
		return err
	}
	window.MakeContextCurrent()

	if err := gl.Init(); err != nil {
		return err
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
		inputEventQueue = event.EnqueueInputEvent(inputEventQueue, inputEvent)*/
	}
	{
		var framebufferSize [2]int
		framebufferSize[0], framebufferSize[1] = window.GetFramebufferSize()
		framebufferSizeCallback(window, framebufferSize[0], framebufferSize[1])
	}
	window.SetFramebufferSizeCallback(framebufferSizeCallback)

	var mousePointer = &event.Pointer{VirtualCategory: event.Pointing}
	var keyboardPointer = &event.Pointer{VirtualCategory: event.Typing}
	var inputEventQueue []event.InputEvent

	window.SetMouseMovementCallback(func(w *glfw.Window, xpos, ypos, xdelta, ydelta float64) {
		inputEvent := event.InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[event.EventType]struct{}{event.SliderEvent: {}},
			InputID:    0,
			Buttons:    nil,
			Sliders:    []float64{xdelta, ydelta},
		}
		if w.GetInputMode(glfw.CursorMode) != glfw.CursorDisabled {
			inputEvent.EventTypes[event.AxisEvent] = struct{}{}
			inputEvent.Axes = []float64{xpos, ypos}
		}
		inputEventQueue = event.EnqueueInputEvent(inputEventQueue, inputEvent)
	})

	window.SetScrollCallback(func(w *glfw.Window, xoff float64, yoff float64) {
		inputEvent := event.InputEvent{
			Pointer:    mousePointer,
			EventTypes: map[event.EventType]struct{}{event.SliderEvent: {}},
			InputID:    2,
			Buttons:    nil,
			Sliders:    []float64{yoff, xoff},
			Axes:       nil,
		}
		inputEventQueue = event.EnqueueInputEvent(inputEventQueue, inputEvent)
	})

	window.SetMouseButtonCallback(func(w *glfw.Window, button glfw.MouseButton, action glfw.Action, mods glfw.ModifierKey) {
		inputEvent := event.InputEvent{
			Pointer:     mousePointer,
			EventTypes:  map[event.EventType]struct{}{event.ButtonEvent: {}},
			InputID:     uint16(button),
			Buttons:     []bool{action != glfw.Release},
			Sliders:     nil,
			Axes:        nil,
			ModifierKey: uint8(mods),
		}
		inputEventQueue = event.EnqueueInputEvent(inputEventQueue, inputEvent)
	})

	window.SetKeyCallback(func(w *glfw.Window, key glfw.Key, scancode int, action glfw.Action, mods glfw.ModifierKey) {
		inputEvent := event.InputEvent{
			Pointer:     keyboardPointer,
			EventTypes:  map[event.EventType]struct{}{event.ButtonEvent: {}},
			InputID:     uint16(key),
			Buttons:     []bool{action != glfw.Release},
			Sliders:     nil,
			Axes:        nil,
			ModifierKey: uint8(mods),
		}
		inputEventQueue = event.EnqueueInputEvent(inputEventQueue, inputEvent)
	})

	window.SetCharCallback(func(w *glfw.Window, char rune) {
		inputEvent := event.InputEvent{
			Pointer:    keyboardPointer,
			EventTypes: map[event.EventType]struct{}{event.CharacterEvent: {}},
			InputID:    uint16(char),
			Buttons:    nil,
			Sliders:    nil,
			Axes:       nil,
		}
		inputEventQueue = event.EnqueueInputEvent(inputEventQueue, inputEvent)
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
		NewOpenGLStream(mgl64.Vec2{borderX, borderY}).PrintText(text)

		// Draw caret.
		lines := strings.Split(text, "\n")
		lastLine := lines[len(lines)-1]
		expandedCaretPosition, caretLine := len(lastLine), len(lines)-1
		gl.PushMatrix()
		gl.Translated(borderX, borderY, 0)
		gl.Color3d(0, 0, 0)
		gl.Recti(int32(expandedCaretPosition*fontWidth-1), int32(caretLine*fontHeight), int32(expandedCaretPosition*fontWidth+1), int32(caretLine*fontHeight)+fontHeight)
		gl.PopMatrix()

		mousePointer.Render()

		window.SwapBuffers()
		runtime.Gosched()
	}

	return nil
}

func main() {
	err := run()
	if err != nil {
		log.Fatalln(err)
	}
}

type app struct {
	window *glfw.Window
}

func (a *app) processInputEventQueue(inputEventQueue []event.InputEvent) []event.InputEvent {
	for len(inputEventQueue) > 0 {
		inputEvent := inputEventQueue[0]

		//fmt.Println(inputEvent)
		_ = inputEvent
		//spew.Dump(inputEvent)

		if inputEvent.Pointer.VirtualCategory == event.Typing && inputEvent.EventTypes.Has(event.ButtonEvent) && glfw.Key(inputEvent.InputID) == glfw.KeyW && inputEvent.Buttons[0] && glfw.ModifierKey(inputEvent.ModifierKey) == glfw.ModSuper {
			a.window.SetShouldClose(true)
		}

		if inputEvent.Pointer.VirtualCategory == event.Typing && inputEvent.EventTypes.Has(event.ButtonEvent) && inputEvent.Buttons[0] && glfw.ModifierKey(inputEvent.ModifierKey) & ^glfw.ModShift == 0 {
			switch glfw.Key(inputEvent.InputID) {
			case glfw.KeyBackspace:
				if len(text) > 0 {
					text = text[:len(text)-1]
				}
			case glfw.KeyEnter:
				text += "\n-bash: command not found"
				text += "\n" + prompt
			}
		}

		if inputEvent.Pointer.VirtualCategory == event.Typing && inputEvent.EventTypes.Has(event.ButtonEvent) && glfw.Key(inputEvent.InputID) == glfw.KeyL && inputEvent.Buttons[0] && glfw.ModifierKey(inputEvent.ModifierKey) == glfw.ModControl {
			text = prompt
		}
		if inputEvent.Pointer.VirtualCategory == event.Typing && inputEvent.EventTypes.Has(event.ButtonEvent) && glfw.Key(inputEvent.InputID) == glfw.KeyC && inputEvent.Buttons[0] && glfw.ModifierKey(inputEvent.ModifierKey) == glfw.ModControl {
			text += "^C\n" + prompt
		}

		if inputEvent.Pointer.VirtualCategory == event.Typing && inputEvent.EventTypes.Has(event.CharacterEvent) {
			text += string(inputEvent.InputID)
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

	dir, err := importPathToDir("github.com/shurcooL-legacy/Conception-go")
	if err != nil {
		log.Fatalln("Unable to find github.com/shurcooL-legacy/Conception-go package in your GOPATH, it's needed to load assets:", err)
	}
	err = os.Chdir(dir)
	if err != nil {
		log.Fatalln("os.Chdir:", err)
	}
}
