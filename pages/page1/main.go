package main

import (
	"runtime"

	"github.com/go-gl/glow/gl/2.1/gl"
	glfw "github.com/shurcooL/glfw3"
)

func main() {
	runtime.LockOSThread()

	if err := glfw.Init(); err != nil {
		panic(err)
	}
	defer glfw.Terminate()

	window, err := glfw.CreateWindow(400, 400, "", nil, nil)
	if err != nil {
		panic(err)
	}
	window.MakeContextCurrent()

	if err := gl.Init(); nil != err {
		panic(err)
	}

	glfw.SwapInterval(1) // Vsync

	gl.ClearColor(0.85, 0.85, 0.85, 1)

	for !mustBool(window.ShouldClose()) {
		glfw.PollEvents()

		gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

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
