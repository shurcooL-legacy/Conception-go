package main

import (
	"fmt"
	. "gist.github.com/5286084.git"
	"log"
	"runtime"
	"time"

	_ "github.com/ftrvxmtrx/tga"
	"image"
	_ "image/png"
	"os"

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

var oFontBase gl.Uint

func Print(x, y float32, s string) {
	if 0 == len(s) {
		return
	}

	gl.Enable(gl.BLEND)
	gl.Enable(gl.TEXTURE_2D)

	gl.PushMatrix()
	gl.Translatef(gl.Float(x), gl.Float(y), 0)
	gl.Translated(-4 + 0.25, -1, 0)
	gl.ListBase(oFontBase)
	gl.CallLists(gl.Sizei(len(s)), gl.UNSIGNED_BYTE, gl.Pointer(&[]byte(s)[0]));
	gl.PopMatrix()
	
	CheckGLError()

	gl.Disable(gl.BLEND)
	gl.Disable(gl.TEXTURE_2D)
}

func InitFont() {
	const fontWidth = 8

	LoadTexture("./data/Font2048.tga")

	oFontBase = gl.GenLists(256)
	
	for iLoop1 := 0; iLoop1 < 256; iLoop1++ {
		fCharX := gl.Float(iLoop1 % 16) / 16.0;
		fCharY := gl.Float(iLoop1 / 16) / 16.0;

		gl.NewList(oFontBase + gl.Uint(iLoop1), gl.COMPILE)
			const offset = gl.Float(0.004)
//#if DECISION_RENDER_TEXT_VCENTERED_MID
			VerticalOffset := gl.Float(0.00125)
			if (('a' <= iLoop1 && iLoop1 <= 'z') || '_' == iLoop1) {
				VerticalOffset = gl.Float(-0.00225)
			}
/*#else
			VerticalOffset := gl.Float(0.0)
//#endif*/
			gl.Begin(gl.QUADS)
				gl.TexCoord2f(fCharX+offset, 1 - (1 - fCharY - 0.0625+offset + VerticalOffset))
				gl.Vertex2i(0, 16);
				gl.TexCoord2f(fCharX + 0.0625-offset, 1 - (1 - fCharY - 0.0625+offset + VerticalOffset))
				gl.Vertex2i(16, 16);
				gl.TexCoord2f(fCharX + 0.0625-offset, 1 - (1 - fCharY-offset + VerticalOffset))
				gl.Vertex2i(16, 0);
				gl.TexCoord2f(fCharX+offset, 1 - (1 - fCharY-offset + VerticalOffset))
				gl.Vertex2i(0, 0);
			gl.End()
			gl.Translated(fontWidth, 0.0, 0.0);
		gl.EndList()
	}
	
	CheckGLError()
}

func DeinitFont() {
	gl.DeleteLists(oFontBase, 256)
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
	fmt.Printf("Loaded %vx%v texture.\n", bounds.Dx(), bounds.Dy())

	var pixPointer *uint8
	switch img := img.(type) {
	case *image.RGBA:
		pixPointer = &img.Pix[0]
	case *image.NRGBA:
		pixPointer = &img.Pix[0]
	default:
		panic("Unsupported type.")
	}

	var texture gl.Uint
	gl.GenTextures(1, &texture)
	gl.BindTexture(gl.TEXTURE_2D, texture)
	gl.TexParameteri(gl.TEXTURE_2D, gl.GENERATE_MIPMAP, gl.TRUE)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.Sizei(bounds.Dx()), gl.Sizei(bounds.Dy()), 0, gl.RGBA, gl.UNSIGNED_BYTE, gl.Pointer(pixPointer))
	CheckGLError()
}

func DrawSomething() {
	gl.LoadIdentity()
	gl.Translatef(50, 100, 0)
	gl.Color3f(0, 0, 0)
	gl.Rectf(0-1, 0-1, 8*22+1, 16+1)
	if !updated {
		gl.Color3f(1, 1, 1)
	} else {
		gl.Color3f(0, 1, 0)
	}
	gl.Rectf(0, 0, 8*22, 16)
	gl.Color3f(0, 0, 0)
	Print(0, 0, "Hello Conception 2! :D")
}

func DrawSomething2() {
	gl.LoadIdentity()
	gl.Translatef(50, 220, 0)
	gl.Color3f(0, 0, 0)

	gl.Enable(gl.TEXTURE_2D)
	gl.Enable(gl.BLEND)
	gl.Begin(gl.TRIANGLE_FAN)
	{
		const size = 256
		gl.TexCoord2i(0, 0)
		gl.Vertex2i(0, 0)
		gl.TexCoord2i(1, 0)
		gl.Vertex2i(size, 0)
		gl.TexCoord2i(1, 1)
		gl.Vertex2i(size, size)
		gl.TexCoord2i(0, 1)
		gl.Vertex2i(0, size)
	}
	gl.End()
	gl.Disable(gl.TEXTURE_2D)
	gl.Disable(gl.BLEND)
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

	InitFont()
	defer DeinitFont()

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

	gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA)
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