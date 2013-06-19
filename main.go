package main

import (
	"fmt"
	"log"
	. "gist.github.com/5286084.git"
	"time"
	"runtime"

	"github.com/go-gl/gl"
	//gl "github.com/chsc/gogl/gl21"
	//"github.com/go-gl/glfw"
	glfw "github.com/tapir/glfw3-go"
	//glfw "github.com/shurcooL/glfw3-go"
)

var updated bool

func DrawSomething() {
	gl.LoadIdentity()
	gl.Translatef(50, 100, 0)
	gl.Color3f(0, 0, 0)
	gl.Rectf(0, 0, 300, 100)
	if !updated {
		gl.Color3f(1, 1, 1)
	} else {
		gl.Color3f(0, 1, 0)
	}
	gl.Rectf(0 + 1, 0 + 1, 300 - 1, 100 - 1)
}

func DrawSpinner(spinner int) {
	gl.LoadIdentity()
	gl.Color3f(0, 0, 0)
	gl.Translatef(30, 30, 0)
	gl.Rotatef(float32(spinner), 0, 0, 1)
	//gl.Rotatef(gl.Float(spinner), 0, 0, 1)
	gl.Begin(gl.LINES)
	gl.Vertex2i(0, 0)
	gl.Vertex2i(0, 20)
	gl.End()
}

func main() {
	runtime.LockOSThread()

	glfw.SetErrorCallback(func(err glfw.ErrorCode, desc string) {
		panic(fmt.Sprintf("%v: %v\n", err, desc))
	})

	if !glfw.Init() {
		panic("glfw.Init()")
	}
	defer glfw.Terminate()

	//glfw.OpenWindowHint(glfw.FsaaSamples, 32)
	window, err := glfw.CreateWindow(400, 400, "", nil, nil)
	CheckError(err)
	window.MakeContextCurrent()

	window.SetPosition(1600, 600)
	//glfw.SetWindowPos(1200, 300)
	glfw.SwapInterval(1)

	redraw := true

	size := func(w *glfw.Window, width, height int) {
		fmt.Println("Framebuffer Size:", width, height)
		gl.Viewport(0, 0, width, height)

		// Update the projection matrix
		gl.MatrixMode(gl.PROJECTION)
		gl.LoadIdentity()
		gl.Ortho(0, float64(width), float64(height), 0, -1, 1)
		gl.MatrixMode(gl.MODELVIEW)

		redraw = true
	}
	window.SetFramebufferSizeCallback(size)
	width, height := window.GetFramebufferSize()
	size(window, width, height)

	MousePos := func(w *glfw.Window, x, y float64) {
		redraw = true
		//fmt.Println("MousePos:", x, y)
	}
	window.SetCursorPositionCallback(MousePos)

	go func() {
		<-time.After(10 * time.Second)
		log.Println("trigger!")
		updated = true
		redraw = true
	}()

	gl.ClearColor(0.8, 0.3, 0.01, 1)

	var spinner int

	/*in := make(chan int)
	out := make(chan int)
	go func(in, out chan int) {
		for {
			<-in
			println("in goroutine")
			out <- 0
		}
	}(in, out)*/

	for !window.ShouldClose() && glfw.Press != window.GetKey(glfw.KeyEscape) {
		//in <- 0
		//glfw.WaitEvents()
		glfw.PollEvents()
		//println("glfw.WaitEvents()")
		//<-out

		if redraw {
			redraw = false

			gl.Clear(gl.COLOR_BUFFER_BIT)

			DrawSpinner(spinner)
			spinner++

			DrawSomething()

			window.SwapBuffers()
			log.Println("swapped buffers")
		} else {
			time.Sleep(time.Millisecond)
		}

		//runtime.Gosched()
	}
}