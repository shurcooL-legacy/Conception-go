package main

import (
	"fmt"
	. "gist.github.com/5286084.git"

	"github.com/go-gl/gl"
	//gl "github.com/chsc/gogl/gl21"
	"github.com/go-gl/glfw"
)

func DrawSomething() {
	gl.LoadIdentity()
	gl.Translatef(50, 100, 0)
	gl.Color3f(0, 0, 0)
	gl.Rectf(0, 0, 300, 100)
	gl.Color3f(1, 1, 1)
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
	err := glfw.Init()
	CheckError(err)
	defer glfw.Terminate()

	//glfw.OpenWindowHint(glfw.FsaaSamples, 32)
	err = glfw.OpenWindow(400, 400, 0, 0, 0, 0, 0, 0, glfw.Windowed)
	CheckError(err)

	glfw.SetWindowPos(1600, 600)
	//glfw.SetWindowPos(1200, 300)
	glfw.SetSwapInterval(1)
	glfw.Disable(glfw.AutoPollEvents)

	size := func(width, height int) {
		fmt.Println("screen size:", width, height)
		gl.Viewport(0, 0, width, height)

		// Update the projection matrix
		gl.MatrixMode(gl.PROJECTION)
		gl.LoadIdentity()
		gl.Ortho(0, float64(width), float64(height), 0, -1, 1)
		gl.MatrixMode(gl.MODELVIEW)
	}
	glfw.SetWindowSizeCallback(size)

	MousePos := func(x, y int) {
		fmt.Println(x, y)
	}
	glfw.SetMousePosCallback(MousePos)

	gl.ClearColor(0.8, 0.3, 0.01, 1)
	
	var spinner int

	for gl.TRUE == glfw.WindowParam(glfw.Opened) && glfw.KeyPress != glfw.Key(glfw.KeyEsc) {
		gl.Clear(gl.COLOR_BUFFER_BIT)

		glfw.WaitEvents()

		DrawSpinner(spinner)
		spinner++

		DrawSomething()

		glfw.SwapBuffers()
	}
}