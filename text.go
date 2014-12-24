package main

import (
	"fmt"
	"image"
	_ "image/png"
	"log"
	"os"
	"strings"

	"github.com/go-gl/glow/gl/2.1/gl"
	"github.com/go-gl/mathgl/mgl64"

	"github.com/shurcooL/Conception-go/caret"
)

// FontOptions specifies the properties of the font.
type FontOptions uint8

const (
	Regular FontOptions = iota
	Bold
	Italic
	BoldItalic
)

// IsBold returns true if the font has the bold property set.
func (fo FontOptions) IsBold() bool { return fo == Bold || fo == BoldItalic }

// IsItalic returns true if the font has the italic property set.
func (fo FontOptions) IsItalic() bool { return fo == Italic || fo == BoldItalic }

type OpenGlStream struct {
	pos        mgl64.Vec2
	lineStartX float64
	advance    uint32

	FontOptions     FontOptions
	BorderColor     *mgl64.Vec3 // nil means no border color.
	BackgroundColor *mgl64.Vec3 // nil means no background color.
	ShowInvisibles  bool
}

func NewOpenGlStream(pos mgl64.Vec2) *OpenGlStream {
	return &OpenGlStream{pos: pos, lineStartX: pos[0]}
}

func (o *OpenGlStream) SetPos(pos mgl64.Vec2) {
	o.pos = pos
	o.lineStartX = pos[0]
	o.advance = 0
}

func (o *OpenGlStream) SetPosWithExpandedPosition(pos mgl64.Vec2, x, y uint32) {
	o.pos = pos.Add(mgl64.Vec2{float64(x * fontWidth), float64(y * fontHeight)})
	o.lineStartX = pos[0]
	o.advance = x
}

func (o *OpenGlStream) PrintText(s string) {
	for {
		end := strings.Index(s, "\n")

		length := len(s)
		if end != -1 {
			length = end
		}
		o.PrintLine(s[:length])

		if end == -1 {
			break
		} else {
			//o.NewLine()
			o.PrintSegment(" ") // Newline
			o.pos[1] += fontHeight
			o.advanceReset()
			s = s[end+1:]
		}
	}
}

// Input shouldn't have newlines
func (o *OpenGlStream) PrintLine(s string) {
	if o.BorderColor != nil {
		gl.PushAttrib(gl.CURRENT_BIT)

		expandedLineLength := caret.ExpandedLength(s, o.advance)

		backgroundColor := nearlyWhiteColor
		if o.BackgroundColor != nil {
			backgroundColor = *o.BackgroundColor
		}

		DrawInnerRoundedBox(o.pos, mgl64.Vec2{fontWidth * float64(expandedLineLength), fontHeight}, *o.BorderColor, backgroundColor)

		gl.PopAttrib()
	}

	segments := strings.Split(s, "\t")
	for index, segment := range segments {
		o.PrintSegment(segment)
		o.advanceBy(uint32(len(segment)))
		if index+1 < len(segments) {
			tabSpaces := 4 - (o.advance % 4)
			o.PrintSegment(strings.Repeat(" ", int(tabSpaces))) // Tab.
			if o.ShowInvisibles {
				gl.PushAttrib(gl.CURRENT_BIT)
				DrawBorderlessBox(o.pos.Add(mgl64.Vec2{1, fontHeight/2 - 1}), mgl64.Vec2{fontWidth*float64(tabSpaces) - 2, 2}, selectedTextDarkColor)
				gl.PopAttrib()
			}
			o.advanceBy(tabSpaces)
		}
	}
}

func (o *OpenGlStream) advanceBy(amount uint32) {
	o.advance += amount
	o.afterAdvance()
}
func (o *OpenGlStream) advanceReset() {
	o.advance = 0
	o.afterAdvance()
}
func (o *OpenGlStream) afterAdvance() {
	o.pos[0] = o.lineStartX + float64(fontWidth*o.advance)
}

// Shouldn't have tabs nor newlines
func (o *OpenGlStream) PrintSegment(s string) {
	if s == "" {
		return
	}

	if o.BackgroundColor != nil && o.BorderColor == nil {
		gl.PushAttrib(gl.CURRENT_BIT)
		gl.Color3dv((*float64)(&o.BackgroundColor[0]))
		gl.PushMatrix()
		gl.Translated(float64(o.pos[0]), float64(o.pos[1]), 0)
		for _ = range s {
			gl.CallList(oFontBackground)
		}
		gl.PopMatrix()
		gl.PopAttrib()
	}

	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_LOD_BIAS, float32(lodBias*0.01))

	gl.Enable(gl.BLEND)
	defer gl.Disable(gl.BLEND)
	gl.Enable(gl.TEXTURE_2D)
	defer gl.Disable(gl.TEXTURE_2D)

	gl.PushMatrix()
	gl.Translated(float64(o.pos[0]), float64(o.pos[1]), 0)
	gl.ListBase(oFontBase + uint32(o.FontOptions)*96)
	gl.CallLists(int32(len(s)), gl.UNSIGNED_BYTE, gl.Ptr(&[]byte(s)[0]))
	gl.PopMatrix()

	//CheckGLError()
}

// ---

const fontWidth, fontHeight = 6, 12

func InitFont() {
	LoadTexture("./data/Font.png")

	oFontBase = gl.GenLists(32 + 96*4)
	for i := 0; i < 96*4; i++ {
		const shiftX, shiftY = float64(1.0 / 16), float64(1.0 / 6 / 4)

		indexX, indexY := i%16, i/16

		gl.NewList(oFontBase+uint32(i+32), gl.COMPILE)
		gl.Begin(gl.QUADS)
		gl.TexCoord2d(float64(indexX)*shiftX, float64(indexY)*shiftY)
		gl.Vertex2i(0, 0)
		gl.TexCoord2d(float64(indexX+1)*shiftX, float64(indexY)*shiftY)
		gl.Vertex2i(fontWidth, 0)
		gl.TexCoord2d(float64(indexX+1)*shiftX, float64(indexY+1)*shiftY)
		gl.Vertex2i(fontWidth, fontHeight)
		gl.TexCoord2d(float64(indexX)*shiftX, float64(indexY+1)*shiftY)
		gl.Vertex2i(0, fontHeight)
		gl.End()
		gl.Translated(fontWidth, 0.0, 0.0)
		gl.EndList()
	}

	oFontBackground = gl.GenLists(1)
	gl.NewList(oFontBackground, gl.COMPILE)
	gl.Begin(gl.QUADS)
	gl.Vertex2i(0, 0)
	gl.Vertex2i(0, fontHeight)
	gl.Vertex2i(fontWidth, fontHeight)
	gl.Vertex2i(fontWidth, 0)
	gl.End()
	gl.Translated(fontWidth, 0.0, 0.0)
	gl.EndList()

	CheckGLError()
}

func DeinitFont() {
	gl.DeleteLists(oFontBase, 32+96*4)
	gl.DeleteLists(oFontBackground, 1)
}

func LoadTexture(path string) {
	//fmt.Printf("Trying to load texture %q: ", path)

	// Open the file
	file, err := os.Open(path)
	if err != nil {
		fmt.Println(os.Getwd())
		log.Fatal(err)
	}
	defer file.Close()

	// Decode the image
	img, _, err := image.Decode(file)
	if err != nil {
		log.Fatal(err)
	}

	bounds := img.Bounds()
	//fmt.Printf("Loaded %vx%v texture.\n", bounds.Dx(), bounds.Dy())

	var pixPointer *uint8
	switch img := img.(type) {
	case *image.RGBA:
		pixPointer = &img.Pix[0]
	case *image.NRGBA:
		pixPointer = &img.Pix[0]
	default:
		panic("Unsupported type.")
	}

	var texture uint32
	gl.GenTextures(1, &texture)
	gl.BindTexture(gl.TEXTURE_2D, texture)
	gl.TexParameteri(gl.TEXTURE_2D, gl.GENERATE_MIPMAP, gl.TRUE)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_LOD_BIAS, -0.5)
	gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, int32(bounds.Dx()), int32(bounds.Dy()), 0, gl.RGBA, gl.UNSIGNED_BYTE, gl.Ptr(pixPointer))
	CheckGLError()
}
