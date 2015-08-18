package main

import (
	"log"
	"math"
	"runtime"
	"time"

	"github.com/go-gl/gl/v2.1/gl"
	"github.com/go-gl/glfw/v3.1/glfw"
	"github.com/go-gl/mathgl/mgl64"
)

var boxUpdated bool

func drawBox() {
	gl.LoadIdentity()
	/*gl.Translatef(50, 100, 0)
	gl.Color3d(201.0/255, 201.0/255, 201.0/255)
	gl.Recti(0, 0, 106, 18)
	if !boxUpdated {
		gl.Color3d(1, 1, 1)
	} else {
		gl.Color3d(0, 1, 0)
	}
	gl.Recti(0+1, 0+1, 106-1, 18-1)*/

	c := mgl64.Vec3{1, 1, 1}
	if boxUpdated {
		c = mgl64.Vec3{42.0 / 255, 154.0 / 255, 254.0 / 255}
	}

	drawInnerRoundedBox(mgl64.Vec2{50, 100},
		mgl64.Vec2{106, 18},
		c.Mul(201.0/255),
		c)
}

func drawInnerRoundedBox(pos, size mgl64.Vec2, borderColor, backgroundColor mgl64.Vec3) {
	if size[0] == 0 || size[1] == 0 {
		return
	}

	const OuterDistance = 2.5
	gl.Begin(gl.POLYGON)
	gl.Color3dv((*float64)(&borderColor[0]))
	gl.Vertex2d(float64(pos[0]+OuterDistance), float64(pos[1]))
	gl.Vertex2d(float64(pos[0]), float64(pos[1]+OuterDistance))
	gl.Vertex2d(float64(pos[0]), float64(pos[1]-OuterDistance+size[1]))
	gl.Vertex2d(float64(pos[0]+OuterDistance), float64(pos[1]+size[1]))
	gl.Vertex2d(float64(pos[0]-OuterDistance+size[0]), float64(pos[1]+size[1]))
	gl.Vertex2d(float64(pos[0]+size[0]), float64(pos[1]-OuterDistance+size[1]))
	gl.Vertex2d(float64(pos[0]+size[0]), float64(pos[1]+OuterDistance))
	gl.Vertex2d(float64(pos[0]-OuterDistance+size[0]), float64(pos[1]))
	gl.End()

	const InnerDistance = OuterDistance + (math.Sqrt2 - 1)
	gl.Begin(gl.POLYGON)
	gl.Color3dv((*float64)(&backgroundColor[0]))
	gl.Vertex2d(float64(pos[0]+InnerDistance), float64(pos[1]+1))
	gl.Vertex2d(float64(pos[0]+1), float64(pos[1]+InnerDistance))
	gl.Vertex2d(float64(pos[0]+1), float64(pos[1]-InnerDistance+size[1]))
	gl.Vertex2d(float64(pos[0]+InnerDistance), float64(pos[1]-1+size[1]))
	gl.Vertex2d(float64(pos[0]-InnerDistance+size[0]), float64(pos[1]-1+size[1]))
	gl.Vertex2d(float64(pos[0]-1+size[0]), float64(pos[1]-InnerDistance+size[1]))
	gl.Vertex2d(float64(pos[0]-1+size[0]), float64(pos[1]+InnerDistance))
	gl.Vertex2d(float64(pos[0]-InnerDistance+size[0]), float64(pos[1]+1))
	gl.End()
}

func drawSpinner(spinner int) {
	gl.LoadIdentity()
	gl.Translatef(30.5, 30.5, 0)
	gl.Rotatef(float32(spinner), 0, 0, 1)
	gl.Color3d(0, 0, 0)
	gl.Rectd(-0.5, -10.5, 0.5, 10.5)
}

func init() {
	runtime.LockOSThread()
}

func main() {
	if err := glfw.Init(); err != nil {
		panic(err)
	}
	defer glfw.Terminate()

	glfw.WindowHint(glfw.Samples, 8) // Anti-aliasing.

	window, err := glfw.CreateWindow(400, 400, "", nil, nil)
	if err != nil {
		panic(err)
	}
	window.MakeContextCurrent()

	if err := gl.Init(); err != nil {
		panic(err)
	}

	glfw.SwapInterval(1)
	//window.SetPos(50, 600)
	//window.SetPos(1600, 600)
	//window.SetPos(1275, 300)
	//window.SetPos(1200, 300)

	framebufferSizeCallback := func(w *glfw.Window, framebufferSize0, framebufferSize1 int) {
		gl.Viewport(0, 0, int32(framebufferSize0), int32(framebufferSize1))

		var windowSize [2]int
		windowSize[0], windowSize[1] = w.GetSize()

		// Update the projection matrix.
		gl.MatrixMode(gl.PROJECTION)
		gl.LoadIdentity()
		gl.Ortho(0, float64(windowSize[0]), float64(windowSize[1]), 0, -1, 1)
		gl.MatrixMode(gl.MODELVIEW)
	}
	{
		var framebufferSize [2]int
		framebufferSize[0], framebufferSize[1] = window.GetFramebufferSize()
		framebufferSizeCallback(window, framebufferSize[0], framebufferSize[1])
	}
	window.SetFramebufferSizeCallback(framebufferSizeCallback)

	go func() {
		<-time.After(5 * time.Second)
		log.Println("trigger!")
		boxUpdated = true

		glfw.PostEmptyEvent()
	}()

	//gl.ClearColor(0.8, 0.3, 0.01, 1)
	gl.ClearColor(247.0/255, 247.0/255, 247.0/255, 1)

	var spinner int

	for !window.ShouldClose() && glfw.Press != window.GetKey(glfw.KeyEscape) {
		glfw.WaitEvents()
		//glfw.PollEvents()

		gl.Clear(gl.COLOR_BUFFER_BIT)

		drawSpinner(spinner)
		spinner++

		drawBox()

		window.SwapBuffers()
		log.Println("swapped buffers")

		//runtime.Gosched()
	}
}
