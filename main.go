package main

import (
	"fmt"
	"log"
	. "gist.github.com/5286084.git"
	"time"
	"runtime"

	"image"
	"os"
	_ "github.com/ftrvxmtrx/tga"
	_ "image/png"

	//"github.com/go-gl/gl"
	gl "github.com/chsc/gogl/gl21"
	glfw "github.com/go-gl/glfw3"

	"github.com/shurcooL/go-goon"
)

var _ = goon.Dump

var updated bool

func CheckGLError() {
	errorCode := gl.GetError()
	if 0 != errorCode {
		log.Panic("GL Error: ", errorCode)
	}
}

func LoadTexture(path string) {
	fmt.Printf("Trying to load texture %q.\n", path)

	// Open the file
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Decode the image
	img, _, err := image.Decode(file)
	if err != nil {
		log.Fatal(err)
	}
	bounds := img.Bounds()
	goon.Dump(len(img.(*image.RGBA).Pix))
	//goon.Dump(img.(*image.RGBA).Pix)
	goon.Dump(gl.Sizei(bounds.Dx()), gl.Sizei(bounds.Dy()))

	var texture gl.Uint
	gl.GenTextures(1, &texture)
	gl.BindTexture(gl.TEXTURE_2D, texture)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.Sizei(bounds.Dx()), gl.Sizei(bounds.Dy()), 0, gl.RGBA, gl.UNSIGNED_BYTE, gl.Pointer(&(img.(*image.RGBA).Pix)))
	CheckGLError()
}

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

func DrawSomething2() {
	gl.LoadIdentity()
	gl.Translatef(50, 250, 0)
	gl.Color3f(1, 1, 1)

	gl.Enable(gl.TEXTURE_2D)
	gl.Begin(gl.TRIANGLE_FAN)
	{
		gl.TexCoord2i(0, 0)
		gl.Vertex2i(0, 0)
		gl.TexCoord2i(1, 0)
		gl.Vertex2i(32, 0)
		gl.TexCoord2i(1, 1)
		gl.Vertex2i(32, 32)
		gl.TexCoord2i(0, 1)
		gl.Vertex2i(0, 32)
	}
	gl.End()
	gl.Disable(gl.TEXTURE_2D)
}

func DrawSpinner(spinner int) {
	gl.LoadIdentity()
	gl.Color3f(0, 0, 0)
	gl.Translatef(30, 30, 0)
	//gl.Rotatef(float32(spinner), 0, 0, 1)
	gl.Rotatef(gl.Float(spinner), 0, 0, 1)
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

	err = gl.Init()
	if nil != err {
		log.Print(err)
	}
	fmt.Println(gl.GoStringUb(gl.GetString(gl.VENDOR)), gl.GoStringUb(gl.GetString(gl.RENDERER)), gl.GoStringUb(gl.GetString(gl.VERSION)))

	//window.SetPosition(1600, 600)
	window.SetPosition(1200, 300)
	glfw.SwapInterval(1)

	//LoadTexture("./data/bg test.tga")
	LoadTexture("./GoLand/src/gist.github.com/5694308.git/Simple.png")

	redraw := true

	size := func(w *glfw.Window, width, height int) {
		fmt.Println("Framebuffer Size:", width, height)
		windowWidth, windowHeight := w.GetSize()
		fmt.Println("Window Size:", windowWidth, windowHeight)
		gl.Viewport(0, 0, gl.Sizei(width), gl.Sizei(height))

		// Update the projection matrix
		gl.MatrixMode(gl.PROJECTION)
		gl.LoadIdentity()
		gl.Ortho(0, gl.Double(windowWidth), gl.Double(windowHeight), 0, -1, 1)
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
		<-time.After(3 * time.Second)
		log.Println("trigger!")
		updated = true
		redraw = true
	}()

	gl.ClearColor(0.8, 0.3, 0.01, 1)

	var spinner int

	for !window.ShouldClose() && glfw.Press != window.GetKey(glfw.KeyEscape) {
		//glfw.WaitEvents()
		glfw.PollEvents()

		if redraw {
			redraw = false

			gl.Clear(gl.COLOR_BUFFER_BIT)

			DrawSpinner(spinner)
			spinner++

			DrawSomething()

			DrawSomething2()

			window.SwapBuffers()
			log.Println("swapped buffers")
		} else {
			time.Sleep(time.Millisecond)
		}

		//runtime.Gosched()
	}
}