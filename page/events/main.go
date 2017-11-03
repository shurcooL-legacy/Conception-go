// events is a test program for printing basic events.
package main

import (
	"fmt"
	"runtime"

	"github.com/goxjs/gl"
	"github.com/goxjs/glfw"
	"github.com/shurcooL-legacy/Conception-go/event"
)

var mousePointer = &event.Pointer{VirtualCategory: event.Pointing}
var keyboardPointer = &event.Pointer{VirtualCategory: event.Typing}

func init() {
	runtime.LockOSThread()
}

func main() {
	if err := glfw.Init(gl.ContextWatcher); err != nil {
		panic(err)
	}
	defer glfw.Terminate()

	window, err := glfw.CreateWindow(400, 400, "", nil, nil)
	if err != nil {
		panic(err)
	}
	window.MakeContextCurrent()

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
		inputEventQueue = event.EnqueueInputEvent(inputEventQueue, inputEvent)*/
	}
	{
		var framebufferSize [2]int
		framebufferSize[0], framebufferSize[1] = window.GetFramebufferSize()
		framebufferSizeCallback(window, framebufferSize[0], framebufferSize[1])
	}
	window.SetFramebufferSizeCallback(framebufferSizeCallback)

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

	gl.ClearColor(0.85, 0.85, 0.85, 1)

	for !window.ShouldClose() {
		glfw.PollEvents()

		// Process Input.
		inputEventQueue = processInputEventQueue(inputEventQueue)

		gl.Clear(gl.COLOR_BUFFER_BIT)

		mousePointer.Render()

		window.SwapBuffers()
		runtime.Gosched()
	}
}

func processInputEventQueue(inputEventQueue []event.InputEvent) []event.InputEvent {
	for len(inputEventQueue) > 0 {
		inputEvent := inputEventQueue[0]

		fmt.Println(inputEvent)
		//spew.Dump(inputEvent)

		inputEventQueue = inputEventQueue[1:]
	}

	return inputEventQueue
}
