package main

import (
	"fmt"
	"image"
	_ "image/png"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/go-gl/gl/v2.1/gl"
	"github.com/go-gl/mathgl/mgl64"

	"github.com/shurcooL/Conception-go/caret"
)

var oFontBase, oFontBackground uint32
var lodBias float64 = -66.67

var selectedTextColor = mgl64.Vec3{195 / 255.0, 212 / 255.0, 242 / 255.0}
var selectedTextDarkColor = selectedTextColor.Mul(0.75)

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
	o.pos = pos.Add(mgl64.Vec2{float64(x) * fontWidth, float64(y) * fontHeight})
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

		drawInnerSlicedBox(o.pos, mgl64.Vec2{fontWidth * float64(expandedLineLength), fontHeight}, *o.BorderColor, backgroundColor)

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
				drawBorderlessBox(o.pos.Add(mgl64.Vec2{1, fontHeight/2 - 1}), mgl64.Vec2{fontWidth*float64(tabSpaces) - 2, 2}, selectedTextDarkColor)
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
	o.pos[0] = o.lineStartX + fontWidth*float64(o.advance)
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
		for range s {
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

//const fontHeight = 150
const fontHeight = 13.1
const fontWidth = fontHeight * (1312.0 / 900) * 6.0 / 16

var shiftXs = [6][17]float64{{0, 0.025914634146341462, 0.051067073170731704, 0.09070121951219512, 0.14329268292682926, 0.19588414634146342, 0.29039634146341464, 0.34984756097560976, 0.37652439024390244, 0.4009146341463415, 0.4253048780487805, 0.45884146341463417, 0.5152439024390244, 0.541920731707317, 0.5785060975609756, 0.604420731707317, 0.6364329268292683}, {0, 0.052591463414634144, 0.10518292682926829, 0.15777439024390244, 0.21036585365853658, 0.2629573170731707, 0.3155487804878049, 0.36814024390243905, 0.42073170731707316, 0.4725609756097561, 0.5251524390243902, 0.551829268292683, 0.5777439024390244, 0.6349085365853658, 0.6913109756097561, 0.7484756097560976, 0.8010670731707317}, {0, 0.07545731707317073, 0.13719512195121952, 0.20121951219512196, 0.2698170731707317, 0.3361280487804878, 0.3940548780487805, 0.4481707317073171, 0.5198170731707317, 0.5884146341463414, 0.6128048780487805, 0.6615853658536586, 0.7248475609756098, 0.7774390243902439, 0.8597560975609756, 0.9283536585365854, 1}, {0, 0.06097560975609756, 0.13338414634146342, 0.19817073170731708, 0.25914634146341464, 0.3132621951219512, 0.38185975609756095, 0.43902439024390244, 0.5266768292682927, 0.5846036585365854, 0.645579268292683, 0.7035060975609756, 0.7278963414634146, 0.7591463414634146, 0.7842987804878049, 0.8407012195121951, 0.8879573170731707}, {0, 0.021341463414634148, 0.07164634146341463, 0.12804878048780488, 0.17835365853658536, 0.2347560975609756, 0.2850609756097561, 0.3132621951219512, 0.3673780487804878, 0.41996951219512196, 0.4413109756097561, 0.46189024390243905, 0.5114329268292683, 0.5320121951219512, 0.6128048780487805, 0.6653963414634146, 0.7195121951219512}, {0, 0.056402439024390245, 0.11204268292682927, 0.14329268292682926, 0.19054878048780488, 0.22027439024390244, 0.2728658536585366, 0.3201219512195122, 0.39176829268292684, 0.4413109756097561, 0.4885670731707317, 0.5335365853658537, 0.5647865853658537, 0.5861280487804879, 0.6173780487804879, 0.6745426829268293, 0.7004573170731707}}

func InitFont() {
	LoadTexture(filepath.Join("data", "fonts", "Helvetica Neue.png"))

	oFontBase = gl.GenLists(32 + 96)
	for i := 0; i < 96; i++ {
		const shiftY = float64(1.0 / 6)

		indexX, indexY := i%16, i/16

		charWidth := shiftXs[indexY][indexX+1] - shiftXs[indexY][indexX]

		gl.NewList(oFontBase+uint32(i+32), gl.COMPILE)
		gl.Begin(gl.QUADS)
		gl.TexCoord2d(shiftXs[indexY][indexX], float64(indexY)*shiftY)
		gl.Vertex2d(0, 0)
		gl.TexCoord2d(shiftXs[indexY][indexX+1], float64(indexY)*shiftY)
		gl.Vertex2d(fontWidth*charWidth/float64(1.0/16), 0)
		gl.TexCoord2d(shiftXs[indexY][indexX+1], float64(indexY+1)*shiftY)
		gl.Vertex2d(fontWidth*charWidth/float64(1.0/16), fontHeight)
		gl.TexCoord2d(shiftXs[indexY][indexX], float64(indexY+1)*shiftY)
		gl.Vertex2d(0, fontHeight)
		gl.End()
		gl.Translated(fontWidth*charWidth/float64(1.0/16), 0.0, 0.0)
		gl.EndList()
	}

	oFontBackground = gl.GenLists(1)
	gl.NewList(oFontBackground, gl.COMPILE)
	gl.Begin(gl.QUADS)
	gl.Vertex2d(0, 0)
	gl.Vertex2d(0, fontHeight)
	gl.Vertex2d(fontWidth, fontHeight)
	gl.Vertex2d(fontWidth, 0)
	gl.End()
	gl.Translated(fontWidth, 0.0, 0.0)
	gl.EndList()

	CheckGLError()
}

func DeinitFont() {
	gl.DeleteLists(oFontBase, 32+96)
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

	var format int
	var pixPointer *uint8
	switch img := img.(type) {
	case *image.RGBA:
		format = gl.RGBA
		pixPointer = &img.Pix[0]
	case *image.NRGBA:
		format = gl.RGBA
		pixPointer = &img.Pix[0]
	case *image.Gray:
		format = gl.ALPHA
		pixPointer = &img.Pix[0]
	default:
		log.Fatalf("LoadTexture: Unsupported type %T.\n", img)
	}

	var texture uint32
	gl.GenTextures(1, &texture)
	gl.BindTexture(gl.TEXTURE_2D, texture)
	gl.TexParameteri(gl.TEXTURE_2D, gl.GENERATE_MIPMAP, gl.TRUE)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_LOD_BIAS, -0.5)
	gl.TexImage2D(gl.TEXTURE_2D, 0, int32(format), int32(bounds.Dx()), int32(bounds.Dy()), 0, uint32(format), gl.UNSIGNED_BYTE, gl.Ptr(pixPointer))
	CheckGLError()
}

// =====

func drawBorderlessBox(pos, size mgl64.Vec2, backgroundColor mgl64.Vec3) {
	gl.Color3dv((*float64)(&backgroundColor[0]))
	gl.Rectd(float64(pos[0]), float64(pos[1]), float64(pos.Add(size)[0]), float64(pos.Add(size)[1]))
}

func CheckGLError() {
	errorCode := gl.GetError()
	if errorCode != 0 {
		log.Panicln("GL Error:", errorCode)
	}
}
