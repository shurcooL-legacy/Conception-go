// Package events is a WIP package for common event handling. It is not yet used by Conception-go.
package events

import (
	"io"
	"time"

	"github.com/go-gl/mathgl/mgl64"
)

type VirtualCategory uint8

const (
	TYPING VirtualCategory = iota
	POINTING
	WINDOWING
)

//go:generate stringer -type=VirtualCategory

type Pointer struct {
	VirtualCategory VirtualCategory
	Mapping         Widgeters // Always reflects current pointer state.
	OriginMapping   Widgeters // Updated only when pointer is moved while not active (e.g., where mouse button was first pressed down).
	State           PointerState
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
		gl.Translated(float64(nearInt64(this.State.Axes[0]))+0.5, float64(nearInt64(this.State.Axes[1]))+0.5, 0)

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
	Buttons []bool // True means pressed down.
	Axes    []float64

	Time time.Time
}

// A pointer is defined to be active if any of its buttons are pressed down.
func (ps *PointerState) IsActive() bool {
	for _, button := range ps.Buttons {
		if button {
			return true
		}
	}
	return false
}

func (ps *PointerState) Button(button int) bool {
	if button >= len(ps.Buttons) {
		return false
	}
	return ps.Buttons[button]
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

type eventTypeSet map[EventType]struct{}

func (s eventTypeSet) Has(et EventType) bool {
	_, ok := s[et]
	return ok
}

type InputEvent struct {
	Pointer    *Pointer
	EventTypes eventTypeSet
	InputId    uint16
	// TODO: Add pointers to BeforeState and AfterState?

	Buttons []bool
	// TODO: Characters? Split into distinct event types, bundle up in an event frame based on time?
	Sliders     []float64
	Axes        []float64
	ModifierKey uint8 // TODO: Think about the best solution here.
}

/*func (e InputEvent) String() string {
	return spew.Sdump(e)
}*/

func ProcessInputEventQueue(inputEventQueue []InputEvent, widget Widgeter) []InputEvent {
	for len(inputEventQueue) > 0 {
		/*inputEvent := inputEventQueue[0]

		//fmt.Println(inputEvent)
		//spew.Dump(inputEvent)*/

		inputEvent := inputEventQueue[0]

		// TODO: Calculate whether a pointing pointer moved relative to canvas in a better way... what if canvas is moved via keyboard, etc.
		pointingPointerMovedRelativeToCanvas := inputEvent.Pointer.VirtualCategory == POINTING &&
			(inputEvent.EventTypes.Has(AXIS_EVENT) && inputEvent.InputId == 0 || inputEvent.EventTypes.Has(SLIDER_EVENT) && inputEvent.InputId == 2)

		if pointingPointerMovedRelativeToCanvas {
			LocalPosition := mgl64.Vec2{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}

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
			!inputEvent.EventTypes.Has(POINTER_DEACTIVATION) && !inputEvent.Pointer.State.IsActive() {

			inputEvent.Pointer.OriginMapping = make([]Widgeter, len(inputEvent.Pointer.Mapping))
			copy(inputEvent.Pointer.OriginMapping, inputEvent.Pointer.Mapping)
		}

		for _, widget := range inputEvent.Pointer.OriginMapping {
			widget.ProcessEvent(inputEvent)
		}

		// Populate OriginMapping (but only upon pointer deactivation event)
		if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes.Has(POINTER_DEACTIVATION) {

			inputEvent.Pointer.OriginMapping = make([]Widgeter, len(inputEvent.Pointer.Mapping))
			copy(inputEvent.Pointer.OriginMapping, inputEvent.Pointer.Mapping)
		}

		inputEventQueue = inputEventQueue[1:]
	}

	return inputEventQueue
}

func EnqueueInputEvent(inputEventQueue []InputEvent, inputEvent InputEvent) []InputEvent {
	preStateActive := inputEvent.Pointer.State.IsActive()

	{
		if inputEvent.EventTypes.Has(BUTTON_EVENT) {
			// Extend slice if needed.
			neededSize := int(inputEvent.InputId) + len(inputEvent.Buttons)
			if neededSize > len(inputEvent.Pointer.State.Buttons) {
				inputEvent.Pointer.State.Buttons = append(inputEvent.Pointer.State.Buttons, make([]bool, neededSize-len(inputEvent.Pointer.State.Buttons))...)
			}

			copy(inputEvent.Pointer.State.Buttons[inputEvent.InputId:], inputEvent.Buttons)
		}

		if inputEvent.EventTypes.Has(AXIS_EVENT) {
			// Extend slice if needed.
			neededSize := int(inputEvent.InputId) + len(inputEvent.Axes)
			if neededSize > len(inputEvent.Pointer.State.Axes) {
				inputEvent.Pointer.State.Axes = append(inputEvent.Pointer.State.Axes, make([]float64, neededSize-len(inputEvent.Pointer.State.Axes))...)
			}

			copy(inputEvent.Pointer.State.Axes[inputEvent.InputId:], inputEvent.Axes)
		}

		inputEvent.Pointer.State.Time = time.Now()
	}

	postStateActive := inputEvent.Pointer.State.IsActive()

	switch {
	case !preStateActive && postStateActive:
		inputEvent.EventTypes[POINTER_ACTIVATION] = struct{}{}
	case preStateActive && !postStateActive:
		inputEvent.EventTypes[POINTER_DEACTIVATION] = struct{}{}
	}

	return append(inputEventQueue, inputEvent)
}

// ---

func nearInt64(value float64) int64 {
	if value >= 0 {
		return int64(value + 0.5)
	} else {
		return int64(value - 0.5)
	}
}

// =====

// ---

type ChangeListener interface {
	NotifyChange()
}

type ChangeListenerFunc func()

func (f ChangeListenerFunc) NotifyChange() {
	f()
}

// ---

type DepNodeI interface {
	AddChangeListener(l ChangeListener)
}

type DepNode struct {
	changeListeners []ChangeListener
}

func (this *DepNode) AddChangeListener(l ChangeListener) {
	this.changeListeners = append(this.changeListeners, l)

	l.NotifyChange() // TODO: In future, don't literally NotifyChange() right away, as this can lead to duplicate work; instead mark as "need to update" for next run
}

// Pre-condition: l is a change listener that exists
func (this *DepNode) RemoveChangeListener(l ChangeListener) {
	for i := range this.changeListeners {
		if this.changeListeners[i] == l {
			// Delete
			copy(this.changeListeners[i:], this.changeListeners[i+1:])
			this.changeListeners[len(this.changeListeners)-1] = nil
			this.changeListeners = this.changeListeners[:len(this.changeListeners)-1]
			//println("removed ith element of originally this many", i, len(this.changeListeners)+1)
			return
		}
	}
	panic("RemoveChangeListener: ChangeListener to be deleted wasn't found.")
}

func (this *DepNode) NotifyAllListeners() {
	// TODO: In future, don't literally NotifyChange() right away, as this can lead to duplicate work; instead mark as "need to update" for next run
	for _, changeListener := range this.changeListeners {
		changeListener.NotifyChange()
	}
}

// ---

type Widgeter interface {
	PollLogic()
	io.Closer
	Layout()
	LayoutNeeded()
	Render()
	Hit(mgl64.Vec2) []Widgeter
	ProcessEvent(InputEvent)                     // TODO: Upgrade to MatchEventQueue() or so
	ContainsWidget(widget, target Widgeter) bool // Returns true if target is widget or within it.

	Pos() *mgl64.Vec2
	Size() *mgl64.Vec2
	HoverPointers() map[*Pointer]bool
	Parent() Widgeter
	SetParent(Widgeter)

	ParentToLocal(mgl64.Vec2) mgl64.Vec2

	//DepNodeI
}

type Widgeters []Widgeter

func (widgets Widgeters) ContainsWidget(target Widgeter) bool {
	for _, widget := range widgets {
		if widget.ContainsWidget(widget, target) {
			return true
		}
	}
	return false
}
