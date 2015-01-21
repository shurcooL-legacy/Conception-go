// Package events is a WIP package for common event handling. It is not yet used by Conception-go.
package events

import (
	"fmt"
	"time"

	glfw "github.com/shurcooL/glfw3"
)

func AttachTo(window *glfw.Window) {
}

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
	/*switch {
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

		const size float64 = 12
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
	}*/
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

//go:generate stringer -type=EventType

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

/*func (e InputEvent) String() string {
	return spew.Sdump(e)
}*/

func ProcessInputEventQueue(inputEventQueue []InputEvent) []InputEvent {
	for len(inputEventQueue) > 0 {
		inputEvent := inputEventQueue[0]

		fmt.Println(inputEvent)
		//spew.Dump(inputEvent)

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
			// Extend slice if needed.
			neededSize := int(inputEvent.InputId) + len(inputEvent.Buttons)
			if neededSize > len(inputEvent.Pointer.State.Buttons) {
				inputEvent.Pointer.State.Buttons = append(inputEvent.Pointer.State.Buttons, make([]bool, neededSize-len(inputEvent.Pointer.State.Buttons))...)
			}

			copy(inputEvent.Pointer.State.Buttons[inputEvent.InputId:], inputEvent.Buttons)
		}

		if inputEvent.EventTypes[AXIS_EVENT] {
			// Extend slice if needed.
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

// ---

func NearInt64(value float64) int64 {
	if value >= 0 {
		return int64(value + 0.5)
	} else {
		return int64(value - 0.5)
	}
}
