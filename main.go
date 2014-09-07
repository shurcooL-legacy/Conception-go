// A work in progress implementation of Conception in Go.
package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/scanner"
	"go/token"
	"image"
	"io"
	"io/ioutil"
	"log"
	"math"
	"net/http"
	_ "net/http/pprof"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"time"

	"code.google.com/p/go.net/websocket"
	"code.google.com/p/go.tools/go/types"
	goimports "code.google.com/p/go.tools/imports"
	igo_ast "github.com/DAddYE/igo/ast"
	"github.com/DAddYE/igo/from_go"
	igo_parser "github.com/DAddYE/igo/parser"
	"github.com/DAddYE/igo/to_go"
	igo_token "github.com/DAddYE/igo/token"
	"github.com/bradfitz/iter"
	"github.com/davecheney/profile"
	_ "github.com/ftrvxmtrx/tga"
	"github.com/go-gl/glow/gl/2.1/gl"
	"github.com/go-gl/mathgl/mgl64"
	"github.com/mb0/diff"
	intmath "github.com/pkg/math"
	"github.com/sergi/go-diff/diffmatchpatch"
	glfw "github.com/shurcooL/glfw3" // Effectively, a fork of github.com/go-gl/glfw3 but with 3.1 PR merged.
	"github.com/shurcooL/go-goon"
	"github.com/shurcooL/go/exp/11"
	"github.com/shurcooL/go/exp/12"
	"github.com/shurcooL/go/exp/13"
	"github.com/shurcooL/go/exp/14"
	. "github.com/shurcooL/go/gists/gist4727543"
	. "github.com/shurcooL/go/gists/gist5258650"
	. "github.com/shurcooL/go/gists/gist5259939"
	. "github.com/shurcooL/go/gists/gist5286084"
	. "github.com/shurcooL/go/gists/gist5423254"
	. "github.com/shurcooL/go/gists/gist5504644"
	. "github.com/shurcooL/go/gists/gist5571468"
	. "github.com/shurcooL/go/gists/gist5639599"
	. "github.com/shurcooL/go/gists/gist5645828"
	. "github.com/shurcooL/go/gists/gist5892738"
	. "github.com/shurcooL/go/gists/gist5953185"
	. "github.com/shurcooL/go/gists/gist6003701"
	. "github.com/shurcooL/go/gists/gist6096872"
	. "github.com/shurcooL/go/gists/gist6418290"
	. "github.com/shurcooL/go/gists/gist6418462"
	. "github.com/shurcooL/go/gists/gist6445065"
	. "github.com/shurcooL/go/gists/gist6545684"
	. "github.com/shurcooL/go/gists/gist6724654"
	. "github.com/shurcooL/go/gists/gist7390843"
	. "github.com/shurcooL/go/gists/gist7480523"
	. "github.com/shurcooL/go/gists/gist7519227"
	. "github.com/shurcooL/go/gists/gist7576154"
	. "github.com/shurcooL/go/gists/gist7576804"
	. "github.com/shurcooL/go/gists/gist7651991"
	. "github.com/shurcooL/go/gists/gist7728088"
	. "github.com/shurcooL/go/gists/gist7802150"
	"github.com/shurcooL/go/gists/gist8065433"
	"github.com/shurcooL/go/github_flavored_markdown"
	"github.com/shurcooL/go/pipe_util"
	"github.com/shurcooL/go/u/u1"
	"github.com/shurcooL/go/u/u5"
	"github.com/shurcooL/go/u/u6"
	"github.com/shurcooL/go/vcs"
	"github.com/shurcooL/gostatus/status"
	"github.com/shurcooL/markdownfmt/markdown"
	"gopkg.in/pipe.v2"
	"honnef.co/go/importer"
)

var _ = UnderscoreSepToCamelCase
var _ = goon.Dump
var _ = GetDocPackageAll
var _ = GetThisGoSourceDir
var _ = SprintAstBare
var _ = errors.New
var _ = GetExprAsString
var _ = UnsafeReflectValue
var _ = profile.Start
var _ = http.ListenAndServe
var _ = Underline
var _ = PrintPackageFullSummary

var headlessFlag = flag.Bool("headless", false, "Headless mode.")

var keepRunning = true
var oFontBase, oFontBackground uint32
var redraw bool = true
var mousePointer *Pointer
var keyboardPointer *Pointer
var websocketPointer *Pointer // TEST

var goCompileErrorsManagerTest GoCompileErrorsManagerTest
var goCompileErrorsEnabledTest *TriButtonExternalStateWidget

var booVcs *exp12.Directory

// Colors
var (
	nearlyWhiteColor = mgl64.Vec3{0.975, 0.975, 0.975}
	veryLightColor   = mgl64.Vec3{0.95, 0.95, 0.95}
	lightColor       = mgl64.Vec3{0.85, 0.85, 0.85}
	grayColor        = mgl64.Vec3{0.75, 0.75, 0.75}
	darkColor        = mgl64.Vec3{0.35, 0.35, 0.35}
	nearlyBlackColor = mgl64.Vec3{0.025, 0.025, 0.025}

	highlightColor = mgl64.Vec3{0.898, 0.765, 0.396}

	selectedTextColor         = mgl64.Vec3{195 / 255.0, 212 / 255.0, 242 / 255.0}
	selectedTextDarkColor     = selectedTextColor.Mul(0.75)
	selectedTextInactiveColor = mgl64.Vec3{225 / 255.0, 235 / 255.0, 250 / 255.0}

	lightRedColor    = mgl64.Vec3{1, 0.867, 0.867}
	lightGreenColor  = mgl64.Vec3{0.867, 1, 0.867}
	mediumRedColor   = mgl64.Vec3{1, 0.767, 0.767}
	mediumGreenColor = mgl64.Vec3{0.767, 1, 0.767}
	darkRedColor     = mgl64.Vec3{1, 0.667, 0.667}
	darkGreenColor   = mgl64.Vec3{0.667, 1, 0.667}
)

var np = mgl64.Vec2{} // np stands for "No Position" and it's basically the (0, 0) position, used when it doesn't matter

// TODO: Remove these
var globalWindow *glfw.Window
var keepUpdatedTEST = []DepNode2I{}
var globalTypeCheckedPackage *typeCheckedPackage
var globalGoSymbols *goSymbolsB
var con2RunBinPath = filepath.Join(os.TempDir(), "Conception-go", "Con2RunBin")

func CheckGLError() {
	errorCode := gl.GetError()
	if errorCode != 0 {
		log.Panicln("GL Error:", errorCode)
	}
}

// ---

func PrintText(pos mgl64.Vec2, s string) {
	lines := GetLines(s)
	for lineIndex, line := range lines {
		PrintLine(pos.Add(mgl64.Vec2{0, float64(fontHeight * lineIndex)}), line)
	}
}

// Input shouldn't have newlines
func PrintLine(pos mgl64.Vec2, s string) {
	segments := strings.Split(s, "\t")
	var advance uint32
	for _, segment := range segments {
		PrintSegment(mgl64.Vec2{pos[0] + float64(fontWidth*advance), pos[1]}, segment)
		advance += uint32(len(segment))
		advance += 4 - (advance % 4)
	}
}

// Shouldn't have tabs nor newlines
func PrintSegment(pos mgl64.Vec2, s string) {
	if s == "" {
		return
	}

	gl.Enable(gl.BLEND)
	defer gl.Disable(gl.BLEND)
	gl.Enable(gl.TEXTURE_2D)
	defer gl.Disable(gl.TEXTURE_2D)

	gl.PushMatrix()
	gl.Translated(float64(pos[0])-3.75*fontWidth/8.0, float64(pos[1])-1*fontHeight/16.0, 0)
	gl.ListBase(oFontBase)
	gl.CallLists(int32(len(s)), gl.UNSIGNED_BYTE, gl.Ptr(&[]byte(s)[0]))
	gl.PopMatrix()

	//CheckGLError()
}

// ---

type OpenGlStream struct {
	pos        mgl64.Vec2
	lineStartX float64
	advance    uint32

	BorderColor     *mgl64.Vec3 // nil means no border color.
	BackgroundColor *mgl64.Vec3 // nil means no background color.
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

		expandedLineLength := ExpandedLength(s, o.advance)

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
			o.PrintSegment(strings.Repeat(" ", 4-int(o.advance%4))) // Tab
			o.advanceBy(4 - (o.advance % 4))
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

	gl.Enable(gl.BLEND)
	defer gl.Disable(gl.BLEND)
	gl.Enable(gl.TEXTURE_2D)
	defer gl.Disable(gl.TEXTURE_2D)

	gl.PushMatrix()
	gl.Translated(float64(o.pos[0])-3.75*fontWidth/8.0, float64(o.pos[1])-1*fontHeight/16.0, 0)
	gl.ListBase(oFontBase)
	gl.CallLists(int32(len(s)), gl.UNSIGNED_BYTE, gl.Ptr(&[]byte(s)[0]))
	gl.PopMatrix()

	//CheckGLError()
}

// ---

const fontWidth, fontHeight = 7, 14

func InitFont() {
	LoadTexture("./Font.tga")

	oFontBase = gl.GenLists(256)
	oFontBackground = gl.GenLists(1)

	for iLoop1 := 0; iLoop1 < 256; iLoop1++ {
		fCharX := float64(iLoop1%16) / 16.0
		fCharY := float64(iLoop1/16) / 16.0

		gl.NewList(oFontBase+uint32(iLoop1), gl.COMPILE)
		const offset = float64(0.04 / 16)
		const shiftX, shiftY = float64(0.0 / 16), float64(0.0 / 16)
		//#if DECISION_RENDER_TEXT_VCENTERED_MID
		/*VerticalOffset := gl.Double(0.02 / 16)
		if ('a' <= iLoop1 && iLoop1 <= 'z') || '_' == iLoop1 {
			VerticalOffset = gl.Double(-0.036 / 16)
		}*/
		VerticalOffset := float64(0.0)
		gl.Begin(gl.QUADS)
		gl.TexCoord2d(fCharX-shiftX+offset, 1-(1-fCharY-0.0625+shiftY+offset+VerticalOffset))
		gl.Vertex2i(0, fontHeight)
		gl.TexCoord2d(fCharX+0.0625-shiftX-offset, 1-(1-fCharY-0.0625+shiftY+offset+VerticalOffset))
		gl.Vertex2i(fontWidth*2, fontHeight)
		gl.TexCoord2d(fCharX+0.0625-shiftX-offset, 1-(1-fCharY+shiftY-offset+VerticalOffset))
		gl.Vertex2i(fontWidth*2, 0)
		gl.TexCoord2d(fCharX-shiftX+offset, 1-(1-fCharY+shiftY-offset+VerticalOffset))
		gl.Vertex2i(0, 0)
		gl.End()
		gl.Translated(fontWidth, 0.0, 0.0)
		gl.EndList()
	}

	gl.NewList(oFontBackground, gl.COMPILE)
	gl.Begin(gl.QUADS)
	gl.Vertex2i(0, fontHeight)
	gl.Vertex2i(fontWidth, fontHeight)
	gl.Vertex2i(fontWidth, 0)
	gl.Vertex2i(0, 0)
	gl.End()
	gl.Translated(fontWidth, 0.0, 0.0)
	gl.EndList()

	CheckGLError()
}

func DeinitFont() {
	gl.DeleteLists(oFontBase, 256)
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
	//fmt.Printf("loaded %vx%v texture.\n", bounds.Dx(), bounds.Dy())

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
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_LOD_BIAS, -0.75)
	gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, int32(bounds.Dx()), int32(bounds.Dy()), 0, gl.RGBA, gl.UNSIGNED_BYTE, gl.Ptr(pixPointer))
	CheckGLError()
}

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
	Close() error
	Layout()
	LayoutNeeded()
	Render()
	Hit(mgl64.Vec2) []Widgeter
	ProcessEvent(InputEvent) // TODO: Upgrade to MatchEventQueue() or so

	Pos() *mgl64.Vec2
	Size() *mgl64.Vec2
	HoverPointers() map[*Pointer]bool
	Parent() Widgeter
	SetParent(Widgeter)

	ParentToLocal(mgl64.Vec2) mgl64.Vec2

	DepNodeI
}

type Widgeters []Widgeter

type Widget struct {
	pos           mgl64.Vec2
	size          mgl64.Vec2
	hoverPointers map[*Pointer]bool
	parent        Widgeter

	DepNode
}

func NewWidget(pos, size mgl64.Vec2) Widget {
	return Widget{pos: pos, size: size, hoverPointers: map[*Pointer]bool{}}
}

func (_ *Widget) PollLogic()   {}
func (_ *Widget) Close() error { return nil }
func (w *Widget) Layout() {
	if w.parent != nil {
		w.parent.Layout()
	}
}
func (_ *Widget) LayoutNeeded() {}
func (_ *Widget) Render()       {}
func (w *Widget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	LocalPosition := w.ParentToLocal(ParentPosition)

	Hit := (LocalPosition[0] >= 0 &&
		LocalPosition[1] >= 0 &&
		LocalPosition[0] <= w.size[0] &&
		LocalPosition[1] <= w.size[1])

	if Hit {
		return []Widgeter{w}
	} else {
		return nil
	}
}
func (w *Widget) ProcessEvent(inputEvent InputEvent) {}

func (w *Widget) Pos() *mgl64.Vec2  { return &w.pos }
func (w *Widget) Size() *mgl64.Vec2 { return &w.size }

func (w *Widget) HoverPointers() map[*Pointer]bool {
	return w.hoverPointers
}

func (w *Widget) Parent() Widgeter     { return w.parent }
func (w *Widget) SetParent(p Widgeter) { w.parent = p }

func (w *Widget) ParentToLocal(ParentPosition mgl64.Vec2) (LocalPosition mgl64.Vec2) {
	return ParentPosition.Sub(w.pos)
}

type WidgeterS struct{ Widgeter }

func (w WidgeterS) GlobalToParent(GlobalPosition mgl64.Vec2) (ParentPosition mgl64.Vec2) {
	switch w.Parent() {
	case nil:
		ParentPosition = GlobalPosition
	default:
		ParentPosition = WidgeterS{w.Parent()}.GlobalToLocal(GlobalPosition)
	}
	return ParentPosition
}
func (w WidgeterS) GlobalToLocal(GlobalPosition mgl64.Vec2) (LocalPosition mgl64.Vec2) {
	return w.ParentToLocal(WidgeterS{w}.GlobalToParent(GlobalPosition))
}

// ---

type CustomWidget struct {
	Widget
	PollLogicFunc    func(this *CustomWidget)
	RenderFunc       func()
	ProcessEventFunc func(inputEvent InputEvent)
}

func (this *CustomWidget) PollLogic() {
	if this.PollLogicFunc != nil {
		this.PollLogicFunc(this)
	} else {
		this.Widget.PollLogic()
	}
}

func (this *CustomWidget) Render() {
	if this.RenderFunc != nil {
		this.RenderFunc()
	} else {
		this.Widget.Render()
	}
}

func (this *CustomWidget) ProcessEvent(inputEvent InputEvent) {
	if this.ProcessEventFunc != nil {
		this.ProcessEventFunc(inputEvent)
	} else {
		this.Widget.ProcessEvent(inputEvent)
	}
}

// ---

type Test1Widget struct {
	Widget
}

func NewTest1Widget(pos mgl64.Vec2) *Test1Widget {
	return &Test1Widget{Widget: NewWidget(pos, mgl64.Vec2{300, 300})}
}

func (w *Test1Widget) Render() {
	DrawNBox(w.pos, w.size)
	gl.Color3d(0, 0, 0)
	//PrintText(w.pos, goon.Sdump(inputEventQueue))

	//x := GetDocPackageAll("gist.github.com/5694308.git")
	//PrintText(w.pos, strings.Join(x.Imports, "\n"))

	/*files, _ := ioutil.ReadDir("/Users/Dmitri/Dropbox/Work/2013/GoLand/src/")
	for lineIndex, file := range files {
		if file.IsDir() {
			PrintText(w.pos.Add(mathgl.Vec2d{0, float64(16 * lineIndex)}), ">>>> " + file.Name() + "/ (FOLDER)")
		} else {
			PrintText(w.pos.Add(mathgl.Vec2d{0, float64(16 * lineIndex)}), file.Name())
		}
	}*/

	//PrintText(w.pos, TryReadFile("/Users/Dmitri/Dropbox/Work/2013/GoLand/src/PrintPackageSummary.go"))

	//pkg := GetThisGoPackage()
	//PrintText(w.pos, pkg.ImportPath+" - "+pkg.Name)

	//PrintText(w.pos, string(debug.Stack()))

	//PrintText(w.pos, GetThisGoSourceFilepath())
	//PrintText(w.pos.Add(mathgl.Vec2d{0, 16}), GetThisGoSourceDir())
	//PrintText(w.pos.Add(mathgl.Vec2d{0, 2 * 16}), GetThisGoPackage().ImportPath)

	/*x := GetDocPackageAll(BuildPackageFromSrcDir(GetThisGoSourceDir()))
	for lineIndex, y := range x.Vars {
		PrintText(w.pos.Add(mathgl.Vec2d{0, float64(16 * lineIndex)}), SprintAstBare(y.Decl))
	}*/

	/*kat := widgets[len(widgets)-2].(*KatWidget)
	PrintText(w.pos, fmt.Sprintf("%d %s", kat.mode, kat.mode.String()))*/
}

// ---

type Test2Widget struct {
	*TextBoxWidget
	field *float64
}

func NewTest2Widget(pos mgl64.Vec2, field *float64) *Test2Widget {
	return &Test2Widget{TextBoxWidget: NewTextBoxWidgetExternalContent(pos, NewMultilineContentFuncInstant(func() string { return TrimLastNewline(goon.Sdump(*field)) }), nil), field: field}
}

func (w *Test2Widget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}

func (w *Test2Widget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.Pointer.State.Button(0) && (inputEvent.EventTypes[SLIDER_EVENT] && inputEvent.InputId == 0) {
		*w.field += inputEvent.Sliders[0]
	}
}

// ---

type parsedIgoFile struct {
	fset    *igo_token.FileSet
	fileAst *igo_ast.File

	DepNode2
}

func (t *parsedIgoFile) Update() {
	source := t.GetSources()[0].(MultilineContentI)

	t.fset = nil
	t.fileAst = nil

	defer func() {
		_ = recover()
	}()

	fset := igo_token.NewFileSet()
	fileAst, _ := igo_parser.ParseFile(fset, "", source.Content(), igo_parser.ParseComments|igo_parser.AllErrors)

	t.fset = fset
	t.fileAst = fileAst
}

// ---

type parsedFile struct {
	fset    *token.FileSet
	fileAst *ast.File

	DepNode2
}

func (t *parsedFile) Update() {
	source := t.GetSources()[0].(MultilineContentI)
	fset := token.NewFileSet()
	fileAst, err := parser.ParseFile(fset, "", source.Content(), parser.ParseComments|parser.AllErrors)

	{
		//fileAst.Decls[0].(*ast.GenDecl).Specs = append(fileAst.Decls[0].(*ast.GenDecl).Specs, &ast.ImportSpec{Path: &ast.BasicLit{Kind: token.STRING, Value: `"yay/new/import"`}})
		//astutil.AddImport(fset, fileAst, "yay/new/import")
	}

	t.fset = fset
	t.fileAst = fileAst
	_ = err
}

func NewTest3Widget(pos mgl64.Vec2, source *TextBoxWidget) (*LiveGoroutineExpeWidget, *parsedFile) {
	parsedFile := &parsedFile{}
	parsedFile.AddSources(source.Content)

	params := func() interface{} {
		return []interface{}{
			source.caretPosition.Logical(),
			parsedFile.fset,
			parsedFile.fileAst,
		}
	}

	action := func(params interface{}) string {
		index := params.([]interface{})[0].(uint32)
		fset := params.([]interface{})[1].(*token.FileSet)
		fileAst := params.([]interface{})[2].(*ast.File)

		query := func(i interface{}) bool {
			if f, ok := i.(ast.Node); ok && (uint32(f.Pos())-1 <= index && index <= uint32(f.End())-1) {
				return true
			}
			return false
		}
		found := FindAll(fileAst, query)

		if len(found) == 0 {
			return ""
		}
		smallest := uint64(math.MaxUint64)
		var smallestV interface{}
		for v := range found {
			size := uint64(v.(ast.Node).End() - v.(ast.Node).Pos())
			if size < smallest {
				smallestV = v
				smallest = size
			}
		}
		out := fmt.Sprintf("%d-%d, ", smallestV.(ast.Node).Pos()-1, smallestV.(ast.Node).End()-1)
		out += fmt.Sprintf("%p, %T\n", smallestV, smallestV)
		out += SprintAst(fset, smallestV) + "\n\n"

		// This is can be huge if ran on root AST node of large Go files, so don't
		if _, huge := smallestV.(*ast.File); !huge {
			buf := new(bytes.Buffer)
			_ = ast.Fprint(buf, fset, smallestV, nil)
			out += buf.String()

			out += goon.Sdump(smallestV)
		}
		return out
	}

	w := NewLiveGoroutineExpeWidget(pos, true, []DepNode2I{parsedFile, source.caretPosition}, params, action)
	return w, parsedFile
}

// ---

type typeCheckedPackage struct {
	fset  *token.FileSet
	files []*ast.File

	tpkg *types.Package
	info *types.Info

	DepNode2
}

func (t *typeCheckedPackage) Update() {
	goPackageSelecter := t.GetSources()[0].(GoPackageSelecter)

	if goPackageSelecter.GetSelectedGoPackage() == nil {
		t.fset = nil
		t.files = nil
		t.tpkg = nil
		t.info = nil
		return
	}

	bpkg := goPackageSelecter.GetSelectedGoPackage().Bpkg

	fset := token.NewFileSet()
	files, err := ParseFiles(fset, bpkg.Dir, append(bpkg.GoFiles, bpkg.CgoFiles...)...)
	if err != nil {
		t.fset = nil
		t.files = nil
		t.tpkg = nil
		t.info = nil
		return
	}

	t.fset = fset
	t.files = files

	imp := importer.New()
	imp.Config.UseGcFallback = true
	cfg := &types.Config{Import: imp.Import}
	info := &types.Info{
		Types:      make(map[ast.Expr]types.TypeAndValue),
		Defs:       make(map[*ast.Ident]types.Object),
		Uses:       make(map[*ast.Ident]types.Object),
		Implicits:  make(map[ast.Node]types.Object),
		Selections: make(map[*ast.SelectorExpr]*types.Selection),
		Scopes:     make(map[ast.Node]*types.Scope),
	}
	tpkg, err := cfg.Check(bpkg.ImportPath, fset, files, info)
	if err == nil {
		t.tpkg = tpkg
		t.info = info
	} else {
		t.tpkg = nil
		t.info = nil
	}
}

// HACK
var Test4WidgetIdent *ast.Ident

func FindFileAst(fset *token.FileSet, file *token.File, fileAsts []*ast.File) *ast.File {
	for _, fileAst := range fileAsts {
		if fset.File(fileAst.Package) == file {
			return fileAst
		}
	}
	return nil
}

func NewTest4Widget(pos mgl64.Vec2, goPackageSelecter GoPackageSelecter, source *TextBoxWidget) (*LiveGoroutineExpeWidget, *typeCheckedPackage) {
	typeCheckedPackage := &typeCheckedPackage{}
	typeCheckedPackage.AddSources(goPackageSelecter, source.Content)

	params := func() interface{} {
		fileUri, _ := source.Content.GetUriForProtocol("file://")
		return []interface{}{
			source.caretPosition.Logical(),
			fileUri,
			typeCheckedPackage.fset,
			typeCheckedPackage.files,
			typeCheckedPackage.info,
		}
	}

	action := func(params interface{}) string {
		index := params.([]interface{})[0].(uint32)
		fileUri := params.([]interface{})[1].(FileUri)
		fset := params.([]interface{})[2].(*token.FileSet)
		files := params.([]interface{})[3].([]*ast.File)
		//tpkg := typeCheckedPackage.tpkg
		info := params.([]interface{})[4].(*types.Info)

		if fset == nil {
			return "fset == nil"
		}

		// Figure out the file index and token.Pos of caret in that file
		var fileAst *ast.File
		var caretPos token.Pos
		fset.Iterate(func(file *token.File) bool {
			if fileUri == FileUri("file://"+file.Name()) {
				fileAst = FindFileAst(fset, file, files)
				caretPos = file.Pos(int(index))
				return false
			}
			return true
		})

		if fileAst == nil {
			return "fileAst == null, caretPos == token.NoPos"
		}

		var found2 []ast.Node
		ast.Inspect(fileAst, func(n ast.Node) bool {
			if n != nil && n.Pos() <= caretPos && caretPos <= n.End() {
				found2 = append(found2, n)
				return true
			}
			return false
		})

		out := ""
		// TODO: Remove after some time, to ensure ast.Inspect() returns same results...
		{
			query := func(i interface{}) bool {
				if f, ok := i.(ast.Node); ok && (f.Pos() <= caretPos && caretPos <= f.End()) {
					return true
				}
				return false
			}
			found := FindAll(fileAst, query)

			foundDiff := false
			if len(found) != len(found2) {
				fmt.Printf("%+v\n%+v\n", found, found2)
				foundDiff = true
				//panic("found vs. found2 diff")
			}
			for _, v := range found2 {
				if !found[v] {
					fmt.Printf("%+v\n%+v\n", found, found2)
					foundDiff = true
					//panic("found vs. found2 diff")
				}
			}
			if foundDiff {
				smallest := uint64(math.MaxUint64)
				var smallestV interface{}
				for v := range found {
					size := uint64(v.(ast.Node).End() - v.(ast.Node).Pos())
					if size < smallest {
						smallestV = v
						smallest = size
					}

					out += fmt.Sprintf("%T %d-%d [%d]\n", v, v.(ast.Node).Pos()-1, v.(ast.Node).End()-1, size)
				}
				out += "\n"
				out += fmt.Sprintf("%d-%d, ", smallestV.(ast.Node).Pos()-1, smallestV.(ast.Node).End()-1)
				out += fmt.Sprintf("%p, %T\n", smallestV, smallestV)
				out += SprintAst(fset, smallestV) + "\n\n"
			}
		}

		if len(found2) == 0 {
			return ""
		}
		//out := ""
		smallestV := found2[len(found2)-1]
		for _, v := range found2 {
			size := v.End() - v.Pos()
			out += fmt.Sprintf("%T %d-%d [%d]\n", v, v.Pos()-1, v.End()-1, size)
		}
		out += "\n"

		out += fmt.Sprintf("%d-%d, %p, %T\n\n", smallestV.Pos()-1, smallestV.End()-1, smallestV, smallestV)

		out += SprintAst(fset, smallestV) + "\n\n"

		if ident, ok := smallestV.(*ast.Ident); ok {
			Test4WidgetIdent = ident // HACK

			if obj := findTypesObject(info, ident); obj != nil {
				out += ">>> " + TypeChainString(obj.Type())
				if constObj, ok := obj.(*types.Const); ok {
					out += fmt.Sprintf(" = %v", constObj.Val())
				}
				out += "\n\n"
			} else {
				out += "nil obj\n\n"
			}
		}

		// This is can be huge if ran on root AST node of large Go files, so don't
		if _, huge := smallestV.(*ast.File); !huge {
			buf := new(bytes.Buffer)
			_ = ast.Fprint(buf, fset, smallestV, nil)
			out += buf.String()

			out += goon.Sdump(smallestV)

			out += goon.SdumpExpr(fset)
		}
		return out
	}

	w := NewLiveGoroutineExpeWidget(pos, true, []DepNode2I{typeCheckedPackage, source.caretPosition}, params, action)
	return w, typeCheckedPackage
}

func NewTypeUnderCaretWidget(pos mgl64.Vec2, goPackageSelecter GoPackageSelecter, source *TextBoxWidget, typeCheckedPackage *typeCheckedPackage) *LiveGoroutineExpeWidget {
	params := func() interface{} {
		fileUri, _ := source.Content.GetUriForProtocol("file://")
		return []interface{}{
			source.caretPosition.Logical(),
			fileUri,
			typeCheckedPackage.fset,
			typeCheckedPackage.files,
			typeCheckedPackage.info,
		}
	}

	action := func(params interface{}) string {
		index := params.([]interface{})[0].(uint32)
		fileUri := params.([]interface{})[1].(FileUri)
		fset := params.([]interface{})[2].(*token.FileSet)
		files := params.([]interface{})[3].([]*ast.File)
		//tpkg := typeCheckedPackage.tpkg
		info := params.([]interface{})[4].(*types.Info)

		if fset == nil {
			return "fset == nil"
		}

		// Figure out the file index and token.Pos of caret in that file
		var fileAst *ast.File
		var caretPos token.Pos
		fset.Iterate(func(file *token.File) bool {
			if fileUri == FileUri("file://"+file.Name()) {
				fileAst = FindFileAst(fset, file, files)
				caretPos = file.Pos(int(index))
				return false
			}
			return true
		})

		if fileAst == nil {
			return "fileAst == null, caretPos == token.NoPos"
		}

		var found2 []ast.Node
		ast.Inspect(fileAst, func(n ast.Node) bool {
			if n != nil && n.Pos() <= caretPos && caretPos <= n.End() {
				found2 = append(found2, n)
				return true
			}
			return false
		})

		if len(found2) == 0 {
			return ""
		}

		out := ""

		smallestV := found2[len(found2)-1]

		if ident, ok := smallestV.(*ast.Ident); ok {
			Test4WidgetIdent = ident // HACK

			if obj := findTypesObject(info, ident); obj != nil {
				out += TypeChainString(obj.Type())
				if constObj, ok := obj.(*types.Const); ok {
					out += fmt.Sprintf(" = %v", constObj.Val())
				}
			} else {
				out += "nil Object"
			}
		}

		return out
	}

	w := NewLiveGoroutineExpeWidget(pos, true, []DepNode2I{typeCheckedPackage, source.caretPosition}, params, action)
	return w
}

func findTypesObject(info *types.Info, ident *ast.Ident) (obj types.Object) {
	if info != nil {
		switch {
		case info.Uses[ident] != nil:
			obj = info.Uses[ident]
		case info.Defs[ident] != nil:
			obj = info.Defs[ident]
		}
	}
	return obj
}

// ---

type SliceStringerS struct {
	entries []fmt.Stringer
	DepNode2Manual
}

func NewSliceStringerS(entries ...string) *SliceStringerS {
	s := &SliceStringerS{}
	for _, entry := range entries {
		s.entries = append(s.entries, json.Number(entry))
	}
	return s
}

func (this *SliceStringerS) Get(index uint64) fmt.Stringer {
	return this.entries[index]
}

func (this *SliceStringerS) Len() uint64 {
	return uint64(len(this.entries))
}

var oracleModes = NewSliceStringerS("callees", "callers", "callgraph", "callstack", "describe", "freevars", "implements", "peers", "referrers")

func NewTest6OracleWidget(pos mgl64.Vec2, goPackageSelecter GoPackageSelecter, source *TextBoxWidget) Widgeter {
	mode := NewFilterableSelecterWidget(np, oracleModes, NewMultilineContent())

	params := func() interface{} {
		fileUri, _ := source.Content.GetUriForProtocol("file://")
		return []interface{}{
			source.caretPosition.Logical(),
			fileUri,
			goPackageSelecter,
			mode.GetSelected().String(),
		}
	}

	action := func(params interface{}) string {
		caretPosition := params.([]interface{})[0].(uint32)
		fileUri := params.([]interface{})[1].(FileUri)
		goPackageSelecter := params.([]interface{})[2].(GoPackageSelecter)
		mode := params.([]interface{})[3].(string)

		if pkg := goPackageSelecter.GetSelectedGoPackage(); pkg != nil && fileUri != "" && mode != "" {
			cmd := exec.Command("oracle", fmt.Sprintf("--pos=%s:#%d", fileUri[len("file://"):], caretPosition), mode, pkg.Bpkg.ImportPath)
			out, err := cmd.CombinedOutput()
			if err != nil {
				return strings.Join(cmd.Args, " ") + "\nError:\n" + err.Error() + "\nOutput:\n" + string(out)
			}
			return strings.Join(cmd.Args, " ") + "\n" + string(out)
		} else {
			return "<no file or mode selected>"
		}
	}

	w := NewLiveGoroutineExpeWidget(pos, false, []DepNode2I{source.caretPosition, goPackageSelecter, mode}, params, action)

	return NewFlowLayoutWidget(pos, Widgeters{mode, w}, nil)
}

// ---

// NOTE: I'm probably not going to use doc.Package because it duplicates AST stuff from other packages, and doesn't point back to real code...
// Instead, I mimic doc.Package functionality, but stack it on top of types.Package rather than a duplicated ast.Package.
/*type docPackage struct {
	dpkg *doc.Package

	DepNode2
}

func (this *docPackage) Update() {
	goPackage := this.GetSources()[0].(ImportPathFoundSelecter)

	importPath := ""
	if goPackage.GetSelected() != nil {
		importPath = goPackage.GetSelected().ImportPath()
	}

	// TODO: Factor out bpkg into buildPackage DepNode2
	bpkg, err := BuildPackageFromImportPath(importPath)
	if err != nil {
		this.dpkg = nil
		return
	}

	dpkg, err := GetDocPackageAll(bpkg, nil)
	if err != nil {
		this.dpkg = nil
		return
	}

	this.dpkg = dpkg
}*/

// ---

// TEST
type NodeStringer struct {
	ast.Node
	str string
}

func NewNodeStringer(node ast.Node) NodeStringer {
	return NodeStringer{Node: node, str: SprintAstBare(node)}
}

func (this *NodeStringer) String() string { return this.str }

// ---

type goSymbols struct {
	entries []NodeStringer

	DepNode2
}

func (this *goSymbols) Get(index uint64) fmt.Stringer {
	return &this.entries[index]
}

func (this *goSymbols) Len() uint64 {
	return uint64(len(this.entries))
}

/*func (this *goSymbols) Update() {
	dpkg := this.GetSources()[0].(*docPackage).dpkg

	if dpkg == nil {
		this.entries = nil
		return
	}

	this.entries = nil
	for _, f := range dpkg.Funcs {
		this.entries = append(this.entries, NewNodeStringer(f.Decl))
	}
	for _, t := range dpkg.Types {
		for _, f := range t.Funcs {
			this.entries = append(this.entries, NewNodeStringer(f.Decl))
		}
		for _, m := range t.Methods {
			this.entries = append(this.entries, NewNodeStringer(m.Decl))
		}
	}
}

func NewTest5Widget(pos mathgl.Vec2d, goPackage *GoPackageListingPureWidget, source *TextBoxWidget) *ListWidget {
	docPackage := &docPackage{}
	docPackage.AddSources(goPackage, source.Content)

	goSymbols := &goSymbols{}
	goSymbols.AddSources(docPackage)

	w := NewListWidget(np, goSymbols)
	return w
}*/

type goSymbolsB struct {
	goSymbols
}

func (this *goSymbolsB) Update() {
	files := this.GetSources()[0].(*typeCheckedPackage).files

	if files == nil {
		this.entries = nil
		return
	}

	// Mimic doc.Package functionality, but stack it on top of types.Package rather than a duplicated ast.Package
	// https://code.google.com/p/go/source/browse/src/pkg/go/doc/reader.go?name=release#456
	this.entries = nil
	for _, fileAst := range files {
		for _, decl := range fileAst.Decls {
			switch d := decl.(type) {
			case *ast.FuncDecl:
				funcDeclSignature := &ast.FuncDecl{Recv: d.Recv, Name: d.Name, Type: d.Type}
				nodeStringer := NodeStringer{Node: d, str: SprintAstBare(funcDeclSignature)}
				this.entries = append(this.entries, nodeStringer)
			}
		}
	}
}

func NewTest5BWidget(pos mgl64.Vec2, typeCheckedPackage *typeCheckedPackage) (*SearchableListWidget, *goSymbolsB) {
	goSymbols := &goSymbolsB{}
	goSymbols.AddSources(typeCheckedPackage)

	w := NewSearchableListWidget(np, mgl64.Vec2{200, 200}, goSymbols)
	return w, goSymbols
}

// =====

type goPackagesSliceStringer struct {
	*exp14.GoPackages
}

func (this *goPackagesSliceStringer) Get(index uint64) fmt.Stringer {
	return this.Entries[index]
}

func (this *goPackagesSliceStringer) Len() uint64 {
	return uint64(len(this.Entries))
}

// ---

type GoPackageSelecter interface {
	GetSelectedGoPackage() *GoPackage

	DepNode2I
}

type GoPackageSelecterAdapter struct {
	Selecter
}

func (this *GoPackageSelecterAdapter) GetSelectedGoPackage() *GoPackage {
	if selected := this.GetSelected(); selected != nil {
		return selected.(*GoPackage)
	} else {
		return nil
	}
}

// TODO: Move to the right place.
//var goPackages = &exp14.GoPackages{SkipGoroot: true}
var goPackages = &exp14.GoPackages{SkipGoroot: false}

func NewGoPackageListingWidget(pos, size mgl64.Vec2) *SearchableListWidget {
	//goPackagesSliceStringer := &goPackagesSliceStringer{&exp14.GoPackages{SkipGoroot: false}}
	goPackagesSliceStringer := &goPackagesSliceStringer{goPackages}

	w := NewSearchableListWidget(pos, size, goPackagesSliceStringer)
	return w
}

// =====

type GoCompileErrorsManagerTest struct {
	//Sources []*GoCompileErrorsTest // TODO: Migrate to using DepNode2
	DepNode2

	All map[FileUri]map[int][]string // FileUri -> LineIndex -> []Message.
}

func (this *GoCompileErrorsManagerTest) Update() {
	this.All = make(map[FileUri]map[int][]string)

	for _, source := range this.GetSources() {
		for _, goCompilerError := range source.(*GoCompileErrorsTest).Out {
			if _, ok := this.All[goCompilerError.FileUri]; !ok {
				this.All[goCompilerError.FileUri] = make(map[int][]string)
			}
			this.All[goCompilerError.FileUri][goCompilerError.ErrorMessage.LineIndex] = append(this.All[goCompilerError.FileUri][goCompilerError.ErrorMessage.LineIndex], goCompilerError.ErrorMessage.Message)
		}
	}

	redraw = true // TODO: Think about this and deprecate if possible
}

// ---

type GoCompileErrorsTest struct {
	DepNode2

	Out []GoCompilerError
}

type GoErrorMessage struct {
	LineIndex int
	Message   string
}

type GoCompilerError struct {
	FileUri      FileUri
	ErrorMessage GoErrorMessage
}

func (this *GoCompileErrorsTest) Update() {
	reduceFunc := func(in string) interface{} {
		x := strings.Index(in, ":") // Find first colon
		if x == -1 {
			return nil
		}
		fileUri, err := filepath.Abs(in[:x])
		if err != nil {
			return nil
		}
		// TODO: Check if file exists? Maybe?

		in = in[x+1:]
		x = strings.Index(in, ":") // Find second colon
		if x == -1 {
			return nil
		}
		lineNumber, err := strconv.Atoi(in[:x])
		if err != nil {
			return nil
		}
		lineIndex := lineNumber - 1 // Convert line number (e.g. 1) to line index (e.g. 0)

		in = in[x+1:]
		message := TrimFirstSpace(in)

		return GoCompilerError{FileUri: FileUri("file://" + fileUri), ErrorMessage: GoErrorMessage{LineIndex: lineIndex, Message: message}}
	}

	source := this.DepNode2.GetSources()[0].(MultilineContentI)
	outChan := GoReduceLinesFromReader(NewContentReader(source), 4, reduceFunc) // TODO: Preserve order (for error messages on same line) by using an order preserving version of GoReduce...
	//outChan := GoReduceLinesFromReader(NewContentReader(this.DepNode2.Sources[0].(MultilineContentI)), 4, reduceFunc)

	this.Out = nil
	for out := range outChan {
		this.Out = append(this.Out, out.(GoCompilerError))
	}
}

// ---

type GpcFileWidget struct {
	Widget
	p Polygon
}

func NewGpcFileWidget(pos mgl64.Vec2, path string) *GpcFileWidget {
	return &GpcFileWidget{Widget: NewWidget(pos, mgl64.Vec2{0, 0}), p: ReadGpcFile(path)}
}

func (w *GpcFileWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)

	gl.Color3d(0, 0, 0)
	for _, contour := range w.p.Contours {
		gl.Begin(gl.LINE_LOOP)
		for _, vertex := range contour.Vertices {
			gl.Vertex2dv((*float64)(&vertex[0]))
		}
		gl.End()
	}
}

// ---

type ButtonTriggerWidget struct {
	*ButtonWidget
	DepNode2Manual
}

func NewButtonTriggerWidget(pos mgl64.Vec2) *ButtonTriggerWidget {
	w := &ButtonTriggerWidget{}
	w.ButtonWidget = NewButtonWidget(pos, func() { ExternallyUpdated(&w.DepNode2Manual) })

	return w
}

// ---

type ButtonLabelWidget struct {
	*ButtonWidget
	label string
}

func NewButtonLabelWidget(pos mgl64.Vec2, label string, action func()) *ButtonLabelWidget {
	w := &ButtonLabelWidget{ButtonWidget: NewButtonWidget(pos, action), label: label}

	w.ButtonWidget.Widget.Size()[0] = float64(fontWidth*len(label) + 8)
	w.ButtonWidget.Widget.Size()[1] = fontHeight

	return w
}

func (w *ButtonLabelWidget) Render() {
	w.ButtonWidget.Render()

	gl.Color3d(0, 0, 0)
	PrintLine(w.Pos().Add(mgl64.Vec2{4, 0}), w.label)
}

// ---

type ButtonWidget struct {
	Widget
	action  func()
	tooltip Widgeter
}

func NewButtonWidget(pos mgl64.Vec2, action func()) *ButtonWidget {
	w := &ButtonWidget{Widget: NewWidget(pos, mgl64.Vec2{fontHeight, fontHeight})}
	w.setAction(action)

	return w
}

func (w *ButtonWidget) setAction(action func()) {
	w.action = action

	if action != nil {
		// HACK: This isn't thread-safe in any way
		//go func() { w.tooltip = NewTextLabelWidgetString(np, GetSourceAsString(action)) }()
	}
}

func (w *ButtonWidget) Render() {
	// HACK: Brute-force check the mouse pointer if it contains this widget
	isOriginHit := false
	for _, hit := range mousePointer.OriginMapping {
		if w == hit {
			isOriginHit = true
			break
		}
	}
	isHit := len(w.HoverPointers()) > 0

	// HACK: Assumes mousePointer rather than considering all connected pointing pointers
	if isOriginHit && mousePointer.State.IsActive() && isHit {
		//DrawGBox(w.pos, w.size)
		DrawInnerRoundedBox(w.pos, w.size, highlightColor, grayColor)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		//DrawYBox(w.pos, w.size)
		DrawInnerRoundedBox(w.pos, w.size, highlightColor, nearlyWhiteColor)
	} else {
		//DrawNBox(w.pos, w.size)
		DrawInnerRoundedBox(w.pos, w.size, mgl64.Vec3{0.3, 0.3, 0.3}, nearlyWhiteColor)
	}

	// Tooltip
	if w.tooltip != nil && isHit {
		mousePointerPositionLocal := WidgeterS{w}.GlobalToLocal(mgl64.Vec2{mousePointer.State.Axes[0], mousePointer.State.Axes[1]})
		tooltipOffset := mgl64.Vec2{0, 16}
		*w.tooltip.Pos() = w.pos.Add(mousePointerPositionLocal).Add(tooltipOffset)
		w.tooltip.Render()
	}
}
func (w *ButtonWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}

func (w *ButtonWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		if w.action != nil {
			w.action()
			//println(GetSourceAsString(w.action))

			w.Layout()
			//w.NotifyAllListeners()
		}
	}
}

// ---

type TriButtonWidget struct {
	*ButtonWidget
	state bool
}

func NewTriButtonWidget(pos mgl64.Vec2, action func()) *TriButtonWidget {
	w := &TriButtonWidget{ButtonWidget: NewButtonWidget(pos, nil)}
	w.setAction(action)

	return w
}

// Pre-conditions: Currently, nil action is not supported.
func (w *TriButtonWidget) setAction(action func()) {
	w.action = func() {
		w.state = !w.state
		action()
	}
}

func (w *TriButtonWidget) Render() {
	gl.Color3dv((*float64)(&darkColor[0]))
	if !w.state {
		gl.Begin(gl.TRIANGLES)
		gl.Vertex2d(float64(w.pos[0]+w.size[0]*0.25), float64(w.pos[1]+w.size[0]*0.15))
		gl.Vertex2d(float64(w.pos[0]+w.size[0]*0.95), float64(w.pos[1]+w.size[1]*0.5))
		gl.Vertex2d(float64(w.pos[0]+w.size[0]*0.25), float64(w.pos[1]+w.size[1]*0.85))
		gl.End()
	} else {
		gl.Begin(gl.TRIANGLES)
		gl.Vertex2d(float64(w.pos[0]+w.size[0]*0.15), float64(w.pos[1]+w.size[0]*0.25))
		gl.Vertex2d(float64(w.pos[0]+w.size[0]*0.85), float64(w.pos[1]+w.size[1]*0.25))
		gl.Vertex2d(float64(w.pos[0]+w.size[0]*0.5), float64(w.pos[1]+w.size[1]*0.95))
		gl.End()
	}
}

func (w *TriButtonWidget) State() bool {
	return w.state
}

// ---

type TriButtonExternalStateWidget struct {
	*ButtonWidget
	state func() bool
}

func NewTriButtonExternalStateWidget(pos mgl64.Vec2, state func() bool, action func()) *TriButtonExternalStateWidget {
	w := &TriButtonExternalStateWidget{ButtonWidget: NewButtonWidget(pos, action), state: state}

	return w
}

func (w *TriButtonExternalStateWidget) Render() {
	// HACK: Brute-force check the mouse pointer if it contains this widget
	isOriginHit := false
	for _, hit := range mousePointer.OriginMapping {
		if w == hit {
			isOriginHit = true
			break
		}
	}
	isHit := len(w.HoverPointers()) > 0

	// HACK: Assumes mousePointer rather than considering all connected pointing pointers
	if isOriginHit && mousePointer.State.IsActive() && isHit {
		DrawGBox(w.pos, w.size)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.pos, w.size)
	} else {
		DrawNBox(w.pos, w.size)
	}

	if w.state() {
		DrawBorderlessBox(w.pos.Add(w.size.Mul(2.0/14)), w.size.Mul(10.0/14), mgl64.Vec3{0.9, 0.3, 0.01})
	} else {
		DrawBorderlessBox(w.pos.Add(w.size.Mul(2.0/14)), w.size.Mul(10.0/14), mgl64.Vec3{0.9, 0.9, 0.9})
	}
}

// ---

type BoxWidget struct {
	Widget
	Name string
}

var boxWidgetTooltip = NewTextLabelWidgetString(np, GetSourceAsString((*BoxWidget).ProcessEvent))

func (w *BoxWidget) Render() {
	// HACK: Brute-force check the mouse pointer if it contains this widget
	isOriginHit := false
	for _, hit := range mousePointer.OriginMapping {
		if w == hit {
			isOriginHit = true
			break
		}
	}
	isHit := len(w.HoverPointers()) > 0

	// HACK: Assumes mousePointer rather than considering all connected pointing pointers
	if isOriginHit && mousePointer.State.IsActive() && isHit {
		DrawGBox(w.pos, w.size)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.pos, w.size)
	} else {
		DrawNBox(w.pos, w.size)
	}

	// Tooltip
	if isHit {
		mousePointerPositionLocal := WidgeterS{w}.GlobalToLocal(mgl64.Vec2{mousePointer.State.Axes[0], mousePointer.State.Axes[1]})
		tooltipOffset := mgl64.Vec2{0, -4 - boxWidgetTooltip.Size()[1]}
		*boxWidgetTooltip.Pos() = w.pos.Add(mousePointerPositionLocal).Add(tooltipOffset)
		boxWidgetTooltip.Render()
	}
}
func (w *BoxWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}

func (w *BoxWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		fmt.Printf("%q BoxWidget pressed!\n", w.Name)
		x, y := globalWindow.GetPosition()
		globalWindow.SetPosition(x-16, y)
	}
}

// ---

type WindowWidget struct {
	Widget
	chrome *CompositeWidget
	Name   string
	child  Widgeter
}

func NewWindowWidget(pos, size mgl64.Vec2, child Widgeter) *WindowWidget {
	w := &WindowWidget{Widget: NewWidget(pos, size), child: child}
	closeButton := NewButtonWidget(np, func() {
		w.Parent().(RemoveWidgeter).RemoveWidget(w)
		w.SetParent(nil) // TODO: Not sure if needed
	})
	w.chrome = NewCompositeWidget(np, []Widgeter{closeButton})
	w.chrome.SetParent(w)
	w.child.SetParent(w)
	w.Layout() // TODO: Should this be automatic from above SetParent()?
	return w
}

func (w *WindowWidget) PollLogic() {
	w.chrome.PollLogic()
	w.child.PollLogic()

	for i := range w.pos {
		w.pos[i] = math.Floor(w.pos[i] + 0.5)
	}

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.PollLogic()
}

func (w *WindowWidget) Close() error {
	// TODO: Errors.
	_ = w.chrome.Close()
	_ = w.child.Close()
	return nil
}

func (w *WindowWidget) Layout() {
	w.size = *w.child.Size()

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *WindowWidget) LayoutNeeded() {
	w.chrome.LayoutNeeded()
	w.child.LayoutNeeded()
}

func (w *WindowWidget) Render() {
	DrawGradientBox(w.pos, mgl64.Vec2{w.size[0], fontHeight}, mgl64.Vec3{0.3, 0.3, 0.3}, nearlyWhiteColor, lightColor)

	// Title
	gl.Color3dv((*float64)(&nearlyBlackColor[0]))
	PrintSegment(w.pos.Add(mgl64.Vec2{60}), w.Name)

	gl.PushMatrix()
	gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)
	w.chrome.Render()
	gl.Translated(float64(0), float64(fontHeight+1), 0)
	w.child.Render()
	gl.PopMatrix()
}
func (w *WindowWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	LocalPosition := w.Widget.ParentToLocal(ParentPosition) // HACK: Should use actual ParentToLocal properly.

	Hit := (LocalPosition[0] >= 0 &&
		LocalPosition[1] >= 0 &&
		LocalPosition[0] <= w.size[0] &&
		LocalPosition[1] <= fontHeight)

	if Hit {
		hits := []Widgeter{w}
		hits = append(hits, w.chrome.Hit(LocalPosition)...)
		return hits
	} else {
		return w.child.Hit(LocalPosition.Sub(mgl64.Vec2{0, fontHeight + 1}))
	}
}

func (w *WindowWidget) ParentToLocal(ParentPosition mgl64.Vec2) (LocalPosition mgl64.Vec2) {
	return w.Widget.ParentToLocal(ParentPosition).Sub(mgl64.Vec2{0, fontHeight + 1})
}

func (w *WindowWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.Pointer.State.Button(0) && (inputEvent.EventTypes[SLIDER_EVENT] && inputEvent.InputId == 0) {
		w.pos[0] += inputEvent.Sliders[0]
		w.pos[1] += inputEvent.Sliders[1]
	}
}

// ---

func (widgets Widgeters) ContainsWidget(targetWidget Widgeter) bool {
	for _, widget := range widgets {
		if widget == targetWidget {
			return true
		}
	}
	return false
}

// ---

func DrawBorderlessBox(pos, size mgl64.Vec2, backgroundColor mgl64.Vec3) {
	gl.Color3dv((*float64)(&backgroundColor[0]))
	gl.Rectd(float64(pos[0]), float64(pos[1]), float64(pos.Add(size)[0]), float64(pos.Add(size)[1]))
}

func DrawBorderlessGradientBox(pos, size mgl64.Vec2, topColor, bottomColor mgl64.Vec3) {
	gl.Begin(gl.TRIANGLE_STRIP)
	gl.Color3dv((*float64)(&topColor[0]))
	gl.Vertex2d(float64(pos[0]), float64(pos[1]))
	gl.Vertex2d(float64(pos.Add(size)[0]), float64(pos[1]))
	gl.Color3dv((*float64)(&bottomColor[0]))
	gl.Vertex2d(float64(pos[0]), float64(pos.Add(size)[1]))
	gl.Vertex2d(float64(pos.Add(size)[0]), float64(pos.Add(size)[1]))
	gl.End()
}

func DrawBox(pos, size mgl64.Vec2, borderColor, backgroundColor mgl64.Vec3) {
	DrawBorderlessBox(pos.Add(mgl64.Vec2{-1, -1}), size.Add(mgl64.Vec2{2, 2}), borderColor)
	DrawBorderlessBox(pos, size, backgroundColor)
}
func DrawNBox(pos, size mgl64.Vec2) {
	DrawBox(pos, size, mgl64.Vec3{0.3, 0.3, 0.3}, nearlyWhiteColor)
}
func DrawYBox(pos, size mgl64.Vec2) {
	DrawBox(pos, size, highlightColor, nearlyWhiteColor)
}
func DrawGBox(pos, size mgl64.Vec2) {
	DrawBox(pos, size, highlightColor, grayColor)
}
func DrawLGBox(pos, size mgl64.Vec2) {
	DrawBox(pos, size, mgl64.Vec3{0.6, 0.6, 0.6}, lightColor)
}

func DrawGradientBox(pos, size mgl64.Vec2, borderColor, topColor, bottomColor mgl64.Vec3) {
	gl.Color3dv((*float64)(&borderColor[0]))
	gl.Rectd(float64(pos[0]-1), float64(pos[1]-1), float64(pos.Add(size)[0]+1), float64(pos.Add(size)[1]+1))
	DrawBorderlessGradientBox(pos, size, topColor, bottomColor)
}

func DrawInnerRoundedBox(pos, size mgl64.Vec2, borderColor, backgroundColor mgl64.Vec3) {
	if size[0] == 0 || size[1] == 0 {
		return
	}

	const OuterDistance = 1.5
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

	const InnerDistance = math.Sqrt2 + 0.5
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

const Tau = 2 * math.Pi

func DrawCircle(pos mgl64.Vec2, size mgl64.Vec2, borderColor, backgroundColor mgl64.Vec3) {
	const x = 64

	gl.Color3dv((*float64)(&borderColor[0]))
	gl.Begin(gl.TRIANGLE_FAN)
	gl.Vertex2d(float64(pos[0]), float64(pos[1]))
	for i := 0; i <= x; i++ {
		gl.Vertex2d(float64(pos[0]+math.Sin(Tau*float64(i)/x)*size[0]/2), float64(pos[1]+math.Cos(Tau*float64(i)/x)*size[1]/2))
	}
	gl.End()

	gl.Color3dv((*float64)(&backgroundColor[0]))
	gl.Begin(gl.TRIANGLE_FAN)
	gl.Vertex2d(float64(pos[0]), float64(pos[1]))
	for i := 0; i <= x; i++ {
		gl.Vertex2d(float64(pos[0]+math.Sin(Tau*float64(i)/x)*(size[0]/2-1)), float64(pos[1]+math.Cos(Tau*float64(i)/x)*(size[1]/2-1)))
	}
	gl.End()
}

func DrawCircleBorder(pos mgl64.Vec2, size mgl64.Vec2, borderColor mgl64.Vec3) {
	DrawCircleBorderCustom(pos, size, borderColor, 1, 64, 0, 64)
}

func DrawCircleBorderCustom(pos mgl64.Vec2, size mgl64.Vec2, borderColor mgl64.Vec3, borderWidth float64, totalSlices, startSlice, endSlice int32) {
	var x = float64(totalSlices)

	gl.Color3dv((*float64)(&borderColor[0]))
	gl.Begin(gl.TRIANGLE_STRIP)
	for i := startSlice; i <= endSlice; i++ {
		gl.Vertex2d(float64(pos[0]+math.Sin(Tau*float64(i)/x)*size[0]/2), float64(pos[1]-math.Cos(Tau*float64(i)/x)*size[1]/2))
		gl.Vertex2d(float64(pos[0]+math.Sin(Tau*float64(i)/x)*(size[0]/2-borderWidth)), float64(pos[1]-math.Cos(Tau*float64(i)/x)*(size[1]/2-borderWidth)))
	}
	gl.End()
}

// ---

type KatWidget struct {
	Widget
	target      mgl64.Vec2
	rotation    float64
	mode        KatMode
	skillActive bool
}

const ShunpoRadius = 120

type KatMode uint8

const (
	/*AutoAttack KatMode = iota
	Shunpo*/

	AutoAttack KatMode = 17 * iota
	_
	Shunpo
)

func (mode KatMode) String() string {
	//fmt.Printf("%T %T\n", AutoAttack, Shunpo)
	x, _ := GetDocPackageAll(BuildPackageFromSrcDir(GetThisGoSourceDir()))
	for _, y := range x.Types {
		if y.Name == "KatMode" {
			for _, c := range y.Consts {
				goon.DumpExpr(c.Names, mode)
				return c.Names[mode]
			}
		}
	}
	panic(0)
}

func NewKatWidget(pos mgl64.Vec2) *KatWidget {
	w := &KatWidget{Widget: NewWidget(pos, mgl64.Vec2{16, 16}), target: pos}
	UniversalClock.AddChangeListener(w)
	return w
}

func (w *KatWidget) Render() {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	/*hasTypingFocus := keyboardPointer != nil && keyboardPointer.OriginMapping.ContainsWidget(w)

	isHit := len(w.HoverPointers()) > 0

	if !hasTypingFocus && !isHit {
		DrawCircle(w.pos, w.size, mathgl.Vec3d{0.3, 0.3, 0.3}, mathgl.Vec3d{1, 1, 1})
	} else {
		DrawCircle(w.pos, w.size, highlightColor, mathgl.Vec3d{1, 1, 1})
	}*/

	// Shadow
	{
		gl.PushMatrix()
		gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)

		gl.Enable(gl.BLEND)
		gl.Begin(gl.TRIANGLE_FAN)
		{
			gl.Color4d(0, 0, 0, 0.3)
			gl.Vertex2d(0, 0)
			gl.Color4d(0, 0, 0, 0)
			nSlices := 16
			PLAYER_HALF_WIDTH := 7.74597
			dShadowRadius := PLAYER_HALF_WIDTH * 1.75
			for nSlice := 0; nSlice <= nSlices; nSlice++ {
				gl.Vertex2d(float64(math.Cos(Tau*float64(nSlice)/float64(nSlices))*dShadowRadius), float64(math.Sin(Tau*float64(nSlice)/float64(nSlices))*dShadowRadius))
			}
		}
		gl.End()
		gl.Disable(gl.BLEND)

		gl.PopMatrix()
	}

	// eX0 Player
	{
		gl.PushMatrix()
		gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)
		gl.Rotated(float64(w.rotation), 0, 0, 1)

		DrawCircleBorderCustom(np, mgl64.Vec2{16, 16}, mgl64.Vec3{1, 0, 0}, 2, 12, 1, 11)

		// Draw the gun
		{
			gl.Begin(gl.QUADS)
			gl.Vertex2d(float64(-1), -float64(3+10))
			gl.Vertex2d(float64(-1), -float64(3-1))
			gl.Vertex2d(float64(1), -float64(3-1))
			gl.Vertex2d(float64(1), -float64(3+10))
			gl.End()
		}

		gl.PopMatrix()
	}

	if w.mode == Shunpo && !w.skillActive {
		DrawCircleBorder(w.pos, mgl64.Vec2{ShunpoRadius * 2, ShunpoRadius * 2}, mgl64.Vec3{0.7, 0.7, 0.7})
	}
}

func (w *KatWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	if w.pos.Sub(ParentPosition).Len() <= w.size[0]/2 {
		return []Widgeter{w}
	} else {
		return nil
	}
}

func (w *KatWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		keyboardPointer.OriginMapping = []Widgeter{w}
	}

	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == true &&
		w.mode == Shunpo {
		w.target = WidgeterS{w}.GlobalToParent(mgl64.Vec2{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]})
		w.skillActive = true
	}

	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.Pointer.State.Button(1) {
		pointerPos := WidgeterS{w}.GlobalToParent(mgl64.Vec2{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]})
		if pointerPos.Sub(w.pos).Len() > w.size[0]*2/3 || w.target.Sub(w.pos).Len() > w.size[0]*2/3 {
			w.target = pointerPos
		}
		w.mode = AutoAttack
		w.skillActive = false
	} else if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 'E' && inputEvent.Buttons[0] == true {
		w.mode = Shunpo
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && glfw.Key(inputEvent.InputId) == glfw.KeyEscape && inputEvent.Buttons[0] == true {
		if w.mode == Shunpo {
			// TODO: Make this consume the event, so the window doesn't get closed...
			w.mode = AutoAttack
		}
	}
}

func (w *KatWidget) NotifyChange() {
	var timePassed float64 = UniversalClock.TimePassed

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && keyboardPointer.OriginMapping.ContainsWidget(w)

	var speed = float64(100.0)

	if hasTypingFocus {
		if keyboardPointer.State.Button(int(glfw.KeyLeftShift)) || keyboardPointer.State.Button(int(glfw.KeyRightShift)) {
			speed *= 0.4
		} else if keyboardPointer.State.Button(int(glfw.KeySpace)) {
			speed *= 10
		}

		if keyboardPointer.State.Button(int(glfw.KeyLeft)) && !keyboardPointer.State.Button(int(glfw.KeyRight)) {
			w.rotation -= 180 * timePassed
			redraw = true
		} else if keyboardPointer.State.Button(int(glfw.KeyRight)) && !keyboardPointer.State.Button(int(glfw.KeyLeft)) {
			w.rotation += 180 * timePassed
			redraw = true
		}

		var direction mgl64.Vec2
		if keyboardPointer.State.Button('A') && !keyboardPointer.State.Button('D') {
			direction[0] = -1
			redraw = true
		} else if keyboardPointer.State.Button('D') && !keyboardPointer.State.Button('A') {
			direction[0] = +1
			redraw = true
		}
		if keyboardPointer.State.Button('W') && !keyboardPointer.State.Button('S') {
			direction[1] = -1
			redraw = true
		} else if keyboardPointer.State.Button('S') && !keyboardPointer.State.Button('W') {
			direction[1] = +1
			redraw = true
		}
		if direction.Len() != 0 {
			rotM := mgl64.Rotate2D(mgl64.DegToRad(w.rotation))
			direction = rotM.Mul2x1(direction)

			w.target = w.pos.Add(direction.Normalize().Mul(speed * timePassed))
			w.pos = w.target
		}
	}

	if w.target.Sub(w.pos).Len() <= speed*timePassed {
		w.pos = w.target
	} else {
		moveBy := w.target.Sub(w.pos)
		moveBy = moveBy.Normalize().Mul(speed * timePassed)
		w.pos = w.pos.Add(moveBy)
		redraw = true
	}

	if w.skillActive && w.target.Sub(w.pos).Len() <= ShunpoRadius {
		w.pos = w.target
		w.mode = AutoAttack
		w.skillActive = false
	}
}

// ---

type AddWidgeter interface {
	AddWidget(Widgeter)
}

type RemoveWidgeter interface {
	RemoveWidget(Widgeter)
}

// ---

type CompositeWidget struct {
	Widget
	Widgets Widgeters
}

func NewCompositeWidget(pos mgl64.Vec2, Widgets Widgeters) *CompositeWidget {
	w := &CompositeWidget{Widget: NewWidget(pos, np), Widgets: Widgets}
	for _, widget := range w.Widgets {
		widget.SetParent(w)
	}
	w.Layout() // TODO: Should this be automatic from above SetParent()?
	return w
}

func (w *CompositeWidget) AddWidget(widget Widgeter) {
	w.Widgets = append(w.Widgets, widget)
	widget.SetParent(w)
	w.Layout()
}

func (w *CompositeWidget) RemoveWidget(targetWidget Widgeter) {
	for index, widget := range w.Widgets {
		if widget == targetWidget {
			copy(w.Widgets[index:], w.Widgets[index+1:])
			w.Widgets[len(w.Widgets)-1] = nil
			w.Widgets = w.Widgets[:len(w.Widgets)-1]
			return
		}
	}
}

func (w *CompositeWidget) PollLogic() {
	for _, widget := range w.Widgets {
		widget.PollLogic()
	}
}

func (w *CompositeWidget) Close() error {
	// TODO: Errors.
	for _, widget := range w.Widgets {
		_ = widget.Close()
	}
	return nil
}

func (w *CompositeWidget) Layout() {
	w.size = np
	for _, widget := range w.Widgets {
		bottomRight := widget.Pos().Add(*widget.Size())
		for d := 0; d < len(w.size); d++ {
			if bottomRight[d] > w.size[d] {
				w.size[d] = bottomRight[d]
			}
		}
	}

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}
func (w *CompositeWidget) LayoutNeeded() {
	for _, widget := range w.Widgets {
		widget.LayoutNeeded()
	}
}
func (w *CompositeWidget) Render() {
	gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)
	defer gl.Translated(float64(-w.pos[0]), float64(-w.pos[1]), 0)

	for _, widget := range w.Widgets {
		widget.Render()
	}
}
func (w *CompositeWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	LocalPosition := w.ParentToLocal(ParentPosition)

	hits := []Widgeter{}
	for _, widget := range w.Widgets {
		hits = append(hits, widget.Hit(LocalPosition)...)
	}

	return hits
}

// ---

type FlowLayoutType uint8

const (
	HorizontalLayout FlowLayoutType = iota
	VerticalLayout
)

type FlowLayoutWidget struct {
	CompositeWidget // THINK: Should I use a pointer or value?
	options         FlowLayoutWidgetOptions
}

type FlowLayoutWidgetOptions struct {
	FlowLayoutType
}

func NewFlowLayoutWidget(pos mgl64.Vec2, Widgets []Widgeter, options *FlowLayoutWidgetOptions) *FlowLayoutWidget {
	if options == nil {
		options = &FlowLayoutWidgetOptions{}
	}
	w := &FlowLayoutWidget{CompositeWidget: CompositeWidget{Widget: NewWidget(pos, mgl64.Vec2{}), Widgets: Widgets}, options: *options}
	// TODO: This is a hack, I'm manually overriding parents of each widget that were set in NewCompositeWidget()
	for _, widget := range w.Widgets {
		widget.SetParent(w)
	}
	w.Layout() // TODO: Should this be automatic from above SetParent()?
	return w
}

// TEST
func (w *FlowLayoutWidget) SetWidgets(widgets []Widgeter) {
	w.Widgets = widgets
	// TODO: This is a hack, I'm manually overriding parents of each widget that were set in NewCompositeWidget()
	for _, widget := range w.Widgets {
		widget.SetParent(w)
	}
	w.Layout() // TODO: Should this be automatic from above SetParent()?
}

func (w *FlowLayoutWidget) AddWidget(widget Widgeter) {
	w.Widgets = append(w.Widgets, widget)
	widget.SetParent(w)
	w.Layout()
}

func (w *FlowLayoutWidget) Layout() {
	w.size = np
	var combinedOffset float64
	for _, widget := range w.CompositeWidget.Widgets {
		pos := np
		pos[w.options.FlowLayoutType] = combinedOffset
		*widget.Pos() = pos
		combinedOffset += widget.Size()[w.options.FlowLayoutType] + 2

		bottomRight := widget.Pos().Add(*widget.Size())
		for d := 0; d < len(w.size); d++ {
			if bottomRight[d] > w.size[d] {
				w.size[d] = bottomRight[d]
			}
		}
	}

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

// ---

type CanvasWidget struct {
	CompositeWidget
	offset  mgl64.Vec2
	options CanvasWidgetOptions

	PopupTest Widgeter
}

type CanvasWidgetOptions struct {
	Scrollable bool
}

func NewCanvasWidget(pos mgl64.Vec2, Widgets []Widgeter, options *CanvasWidgetOptions) *CanvasWidget {
	if options == nil {
		options = &CanvasWidgetOptions{}
	}
	w := &CanvasWidget{CompositeWidget: CompositeWidget{Widget: NewWidget(pos, mgl64.Vec2{}), Widgets: Widgets}, options: *options}
	for _, widget := range w.Widgets {
		widget.SetParent(w)
	}
	return w
}

func (w *CanvasWidget) AddWidget(widget Widgeter) {
	w.Widgets = append(w.Widgets, widget)
	widget.SetParent(w)
	w.Layout()
}

// HACK: Offset all widgets by (1, 1) so their border is visible. Need to generalize this.
func (w *CanvasWidget) offsetBy1Px() {
	for _, widget := range w.Widgets {
		*widget.Pos() = widget.Pos().Add(mgl64.Vec2{1, 1})
	}
}

func (w *CanvasWidget) PollLogic() {
	var timePassed float64 = UniversalClock.TimePassed

	if w.options.Scrollable {
		// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
		// HACK: Instead of checking for exclusive focus, should have a waterfall model with low priority checking.
		hasExclusiveTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) == 1 && w == keyboardPointer.OriginMapping[0]

		if hasExclusiveTypingFocus {
			var speed = float64(1000.0)
			if keyboardPointer.State.Button(int(glfw.KeyLeftShift)) || keyboardPointer.State.Button(int(glfw.KeyRightShift)) {
				speed *= 0.4
			} else if keyboardPointer.State.Button(int(glfw.KeySpace)) {
				speed *= 10
			}

			var direction mgl64.Vec2
			if keyboardPointer.State.Button(int(glfw.KeyLeft)) && !keyboardPointer.State.Button(int(glfw.KeyRight)) {
				direction[0] = +1
				redraw = true
			} else if keyboardPointer.State.Button(int(glfw.KeyRight)) && !keyboardPointer.State.Button(int(glfw.KeyLeft)) {
				direction[0] = -1
				redraw = true
			}
			if keyboardPointer.State.Button(int(glfw.KeyUp)) && !keyboardPointer.State.Button(int(glfw.KeyDown)) {
				direction[1] = +1
				redraw = true
			} else if keyboardPointer.State.Button(int(glfw.KeyDown)) && !keyboardPointer.State.Button(int(glfw.KeyUp)) {
				direction[1] = -1
				redraw = true
			}

			if direction.Len() != 0 {
				w.offset = w.offset.Add(direction.Normalize().Mul(speed * timePassed))
			} else {
				for i := range w.offset {
					w.offset[i] = math.Floor(w.offset[i] + 0.5)
				}
			}
		}
	}

	if w.PopupTest != nil {
		w.PopupTest.PollLogic()
	}

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.CompositeWidget.PollLogic()
}

func (w *CanvasWidget) Layout() {
	// HACK
	var windowSize0, windowSize1 int
	if globalWindow != nil {
		windowSize0, windowSize1 = globalWindow.GetSize()
	}
	windowSize := mgl64.Vec2{float64(windowSize0), float64(windowSize1)} // HACK: This is not updated as window resizes, etc.
	w.size = windowSize

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *CanvasWidget) LayoutNeeded() {
	if w.PopupTest != nil {
		w.PopupTest.LayoutNeeded()
	}

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.CompositeWidget.LayoutNeeded()
}

func (w *CanvasWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(w.pos[0], w.pos[1], 0)

	// Background.
	{
		backgroundTopColor := mgl64.Vec3{226 / 255.0, 144 / 255.0, 153 / 255.0}
		backgroundBottomColor := mgl64.Vec3{96 / 255.0, 93 / 255.0, 160 / 255.0}

		DrawBorderlessGradientBox(w.pos, w.size, backgroundTopColor, backgroundBottomColor)
	}

	gl.Translated(w.offset[0], w.offset[1], 0)

	for _, widget := range w.Widgets {
		widget.Render()
	}

	if w.PopupTest != nil {
		gl.PushMatrix()
		gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)
		w.PopupTest.Render()
		gl.PopMatrix()
	}
}

func (w *CanvasWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == true &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// HACK: Give exclusive keyboard capture if Canvas is the only thing clicked, and assume others will steal/add focus if they want.
		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		keyboardPointer.OriginMapping = []Widgeter{w}
	}

	if w.options.Scrollable {
		if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[SLIDER_EVENT] && inputEvent.InputId == 2 {
			w.offset[1] += inputEvent.Sliders[0] * 10
			w.offset[0] += inputEvent.Sliders[1] * 10
		}
	}

	// TODO: Make this happen as a PostProcessEvent if it hasn't been processed by an earlier widget, etc.
	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
		switch glfw.Key(inputEvent.InputId) {
		// TEST, DEBUG: Cmd+O shortcut to open a new window, for testing purposes
		case glfw.KeyO:
			if inputEvent.ModifierKey&glfw.ModSuper != 0 {
				w2 := NewWindowWidget(mgl64.Vec2{600, 400}, mgl64.Vec2{300, 60}, NewTextBoxWidget(np))
				w2.Name = "New Window"

				// Add new widget to canvas
				w.AddWidget(w2)

				// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
				keyboardPointer.OriginMapping = []Widgeter{w2}
			}

		case glfw.KeyP:
			if inputEvent.ModifierKey == glfw.ModSuper {
				textBox := NewTextBoxWidget(np)
				w.PopupTest = NewSpacerWidget(mgl64.Vec2{200, 0}, textBox)

				originalMapping := keyboardPointer.OriginMapping // HACK
				closeOnEscape := &CustomWidget{
					Widget: NewWidget(np, np),
					PollLogicFunc: func(this *CustomWidget) {
						if !keyboardPointer.OriginMapping.ContainsWidget(this.Parent()) {
							w.PopupTest = nil

							// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
							keyboardPointer.OriginMapping = originalMapping
						}
					},
					ProcessEventFunc: func(inputEvent InputEvent) {
						if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
							switch glfw.Key(inputEvent.InputId) {
							case glfw.KeyEnter:
								content := textBox.Content.Content()
								{
									globalPosition := mgl64.Vec2{mousePointer.State.Axes[0], mousePointer.State.Axes[1]}
									localPosition := WidgeterS{w}.GlobalToLocal(globalPosition)

									body := NewTextFileWidget(np, content)

									w2 := NewWindowWidget(localPosition, mgl64.Vec2{}, body)
									w2.Name = content

									// Add new widget to canvas
									w.AddWidget(w2)

									// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
									keyboardPointer.OriginMapping = []Widgeter{w2}
								}

								w.PopupTest = nil

								// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
								keyboardPointer.OriginMapping = originalMapping
							case glfw.KeyEscape:
								w.PopupTest = nil

								// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
								keyboardPointer.OriginMapping = originalMapping
							}
						}
					},
				}

				// TODO: textBox.AddExtension(closeOnEscape).
				{
					closeOnEscape.SetParent(textBox)
					textBox.ExtensionsTest = append(textBox.ExtensionsTest, closeOnEscape)
				}

				// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
				keyboardPointer.OriginMapping = []Widgeter{textBox}
			}

		case glfw.KeyEscape:
			keepRunning = false
		}
	}
}

func (w *CanvasWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	LocalPosition := w.ParentToLocal(ParentPosition)

	if len(w.Widget.Hit(ParentPosition)) > 0 {
		hits := []Widgeter{w}
		for _, widget := range w.Widgets {
			hits = append(hits, widget.Hit(LocalPosition)...)
		}
		return hits
	} else {
		return nil
	}
}

func (w *CanvasWidget) ParentToLocal(ParentPosition mgl64.Vec2) (LocalPosition mgl64.Vec2) {
	return w.Widget.ParentToLocal(ParentPosition).Sub(w.offset)
}

// ---

type CollapsibleWidget struct {
	Widget
	state *TriButtonWidget //*TriButtonExternalStateWidget
	label Widgeter         // HACK: Manually adding widgets, this doesn't scale, need to use CompositeWidget or make Widgeter naturally extendable, etc.
	child Widgeter
}

func NewCollapsibleWidget(pos mgl64.Vec2, child Widgeter, title string) *CollapsibleWidget {
	w := &CollapsibleWidget{Widget: NewWidget(pos, np), child: child}
	w.child.SetParent(w)

	//state := true
	//w.state = NewTriButtonExternalStateWidget(np, func() bool { return state }, func() { state = !state })
	w.state = NewTriButtonWidget(np, func() {})
	//w.state.state = true // HACK
	w.state.SetParent(w) // For its Layout() to work...

	w.label = NewTextLabelWidgetString(mgl64.Vec2{w.state.Size()[0] + 2, 0}, title) // HACK, should use FlowLayout or something
	w.label.SetParent(w)                                                            // For its Layout() to work...

	// HACK: Set position of child
	w.child.Pos()[0] = w.state.Size()[0] + 2
	w.child.Pos()[1] = w.state.Size()[1] + 2

	// TODO: This needs to be automated, easy to forget, moved into Layout2(), etc.
	w.Layout() // TODO: Should this be automatic from above SetParent()?

	return w
}

func (w *CollapsibleWidget) PollLogic() {
	w.state.PollLogic()
	w.label.PollLogic()

	if w.state.State() {
		w.child.PollLogic()
	}
}

func (w *CollapsibleWidget) Close() error {
	// TODO: Errors.
	_ = w.state.Close()
	_ = w.label.Close()
	_ = w.child.Close()
	return nil
}

func (w *CollapsibleWidget) Layout() {
	// HACK
	Widgets := []Widgeter{w.state, w.label}
	if w.state.State() {
		Widgets = append(Widgets, w.child)
	}

	w.size = np
	for _, widget := range Widgets {
		bottomRight := widget.Pos().Add(*widget.Size())
		for d := 0; d < len(w.size); d++ {
			if bottomRight[d] > w.size[d] {
				w.size[d] = bottomRight[d]
			}
		}
	}

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *CollapsibleWidget) LayoutNeeded() {
	w.state.LayoutNeeded()
	w.label.LayoutNeeded()

	if w.state.State() {
		w.child.LayoutNeeded()
	}
}

func (w *CollapsibleWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)

	w.state.Render()
	w.label.Render()

	if w.state.State() {
		w.child.Render()
	}
}

func (w *CollapsibleWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	LocalPosition := w.ParentToLocal(ParentPosition)

	hits := w.state.Hit(LocalPosition)
	if w.state.State() {
		hits = append(hits, w.child.Hit(LocalPosition)...)
	}

	return hits
}

// ---

type OptionalHighlighter struct {
	highlighter Highlighter
	stateFunc   func() bool
}

func NewOptionalHighlighter(highlighter Highlighter, stateFunc func() bool) *OptionalHighlighter {
	return &OptionalHighlighter{
		highlighter: highlighter,
		stateFunc:   stateFunc,
	}
}

func (this *OptionalHighlighter) Highlighter() Highlighter {
	if !this.stateFunc() {
		return nil
	}
	return this.highlighter
}

// ---

type ScrollPaneWidget struct {
	Widget
	child Widgeter
}

func NewScrollPaneWidget(pos, size mgl64.Vec2, child Widgeter) *ScrollPaneWidget {
	w := &ScrollPaneWidget{Widget: NewWidget(pos, size), child: child}
	w.child.SetParent(w)
	return w
}

func (w *ScrollPaneWidget) PollLogic() {
	w.child.PollLogic()
}

func (w *ScrollPaneWidget) Close() error {
	// TODO: Errors.
	_ = w.child.Close()
	return nil
}

func (w *ScrollPaneWidget) Layout() {
	// Keep the child widget within the scroll pane
	for i := 0; i < 2; i++ {
		if w.child.Pos()[i]+w.child.Size()[i] < w.size[i] {
			w.child.Pos()[i] = w.size[i] - w.child.Size()[i]
		}

		if w.child.Pos()[i] > 0 {
			w.child.Pos()[i] = 0
		}
	}

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

/*func (w *ScrollPaneWidget) scrollToPoint(target mathgl.Vec2d) {
	for i := 0; i < 2; i++ {
		if w.child.Pos()[i]+target[i] < 0 {
			w.child.Pos()[i] = -target[i]
		} else if w.child.Pos()[i]+target[i] > w.size[i] {
			w.child.Pos()[i] = w.size[i] - target[i]
		}
	}
}*/

func (w *ScrollPaneWidget) ScrollToArea(target, size mgl64.Vec2) {
	for i := 0; i < 2; i++ {
		// Move the minimum amount that increases overlap
		w.child.Pos()[i] += math.Min(math.Dim(-w.child.Pos()[i], target[i]), math.Dim(-w.child.Pos()[i]+w.size[i], target[i]+size[i]))
		w.child.Pos()[i] -= math.Min(math.Dim(target[i], -w.child.Pos()[i]), math.Dim(target[i]+size[i], -w.child.Pos()[i]+w.size[i]))
	}

	// TODO: Needed?
	//w.Layout()
}

func (w *ScrollPaneWidget) CenterOnArea(target, size mgl64.Vec2) {
	for i := 0; i < 2; i++ {
		w.child.Pos()[i] = -(target[i] + size[i]/2) + w.size[i]/2
	}

	// This is needed to normalize the view area, as the above potentially places the viewport outside of the widget
	w.Layout()
}

func NearInt64(value float64) int64 {
	if value >= 0 {
		return int64(value + 0.5)
	} else {
		return int64(value - 0.5)
	}
}

func SetScissor(pos, size mgl64.Vec2) {
	var ModelMatrix [16]float64
	var ProjectionMatrix [16]float64
	var Viewport [4]int32

	gl.GetDoublev(gl.MODELVIEW_MATRIX, &ModelMatrix[0])
	gl.GetDoublev(gl.PROJECTION_MATRIX, &ProjectionMatrix[0])
	gl.GetIntegerv(gl.VIEWPORT, &Viewport[0])

	p0 := mgl64.Project(mgl64.Vec3{pos[0], pos[1] + size[1] /* Inverted y coordinate. */, 0},
		mgl64.Mat4{float64(ModelMatrix[0]), float64(ModelMatrix[1]), float64(ModelMatrix[2]), float64(ModelMatrix[3]), float64(ModelMatrix[4]), float64(ModelMatrix[5]), float64(ModelMatrix[6]), float64(ModelMatrix[7]), float64(ModelMatrix[8]), float64(ModelMatrix[9]), float64(ModelMatrix[10]), float64(ModelMatrix[11]), float64(ModelMatrix[12]), float64(ModelMatrix[13]), float64(ModelMatrix[14]), float64(ModelMatrix[15])},
		mgl64.Mat4{float64(ProjectionMatrix[0]), float64(ProjectionMatrix[1]), float64(ProjectionMatrix[2]), float64(ProjectionMatrix[3]), float64(ProjectionMatrix[4]), float64(ProjectionMatrix[5]), float64(ProjectionMatrix[6]), float64(ProjectionMatrix[7]), float64(ProjectionMatrix[8]), float64(ProjectionMatrix[9]), float64(ProjectionMatrix[10]), float64(ProjectionMatrix[11]), float64(ProjectionMatrix[12]), float64(ProjectionMatrix[13]), float64(ProjectionMatrix[14]), float64(ProjectionMatrix[15])},
		int(Viewport[0]), int(Viewport[1]), int(Viewport[2]), int(Viewport[3]))
	p1 := mgl64.Project(mgl64.Vec3{size[0], size[1], 0},
		mgl64.Mat4{float64(ModelMatrix[0]), float64(ModelMatrix[1]), float64(ModelMatrix[2]), float64(ModelMatrix[3]), float64(ModelMatrix[4]), float64(ModelMatrix[5]), float64(ModelMatrix[6]), float64(ModelMatrix[7]), float64(ModelMatrix[8]), float64(ModelMatrix[9]), float64(ModelMatrix[10]), float64(ModelMatrix[11]), float64(ModelMatrix[12]), float64(ModelMatrix[13]), float64(ModelMatrix[14]), float64(ModelMatrix[15])},
		mgl64.Mat4{float64(ProjectionMatrix[0]), float64(ProjectionMatrix[1]), float64(ProjectionMatrix[2]), float64(ProjectionMatrix[3]), float64(ProjectionMatrix[4]), float64(ProjectionMatrix[5]), float64(ProjectionMatrix[6]), float64(ProjectionMatrix[7]), float64(ProjectionMatrix[8]), float64(ProjectionMatrix[9]), float64(ProjectionMatrix[10]), float64(ProjectionMatrix[11]), float64(ProjectionMatrix[12]), float64(ProjectionMatrix[13]), float64(ProjectionMatrix[14]), float64(ProjectionMatrix[15])},
		int(Viewport[0]), int(Viewport[1]), int(Viewport[2]), int(Viewport[3]))
	p2 := mgl64.Project(mgl64.Vec3{0, 0, 0},
		mgl64.Mat4{float64(ModelMatrix[0]), float64(ModelMatrix[1]), float64(ModelMatrix[2]), float64(ModelMatrix[3]), float64(ModelMatrix[4]), float64(ModelMatrix[5]), float64(ModelMatrix[6]), float64(ModelMatrix[7]), float64(ModelMatrix[8]), float64(ModelMatrix[9]), float64(ModelMatrix[10]), float64(ModelMatrix[11]), float64(ModelMatrix[12]), float64(ModelMatrix[13]), float64(ModelMatrix[14]), float64(ModelMatrix[15])},
		mgl64.Mat4{float64(ProjectionMatrix[0]), float64(ProjectionMatrix[1]), float64(ProjectionMatrix[2]), float64(ProjectionMatrix[3]), float64(ProjectionMatrix[4]), float64(ProjectionMatrix[5]), float64(ProjectionMatrix[6]), float64(ProjectionMatrix[7]), float64(ProjectionMatrix[8]), float64(ProjectionMatrix[9]), float64(ProjectionMatrix[10]), float64(ProjectionMatrix[11]), float64(ProjectionMatrix[12]), float64(ProjectionMatrix[13]), float64(ProjectionMatrix[14]), float64(ProjectionMatrix[15])},
		int(Viewport[0]), int(Viewport[1]), int(Viewport[2]), int(Viewport[3]))

	pos0 := NearInt64(float64(p0[0]))
	pos1 := NearInt64(float64(p0[1]))
	size0 := NearInt64(float64(p1[0] - p2[0]))
	size1 := NearInt64(float64(p2[1] - p1[1]))

	// Crop the scissor box by the parent scissor box
	// TODO

	gl.Scissor(int32(pos0), int32(pos1), int32(size0), int32(size1))
}

func (w *ScrollPaneWidget) LayoutNeeded() {
	w.child.LayoutNeeded()
}

func (w *ScrollPaneWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)

	SetScissor(mgl64.Vec2{-1, -1}, w.size.Add(mgl64.Vec2{2, 2}))
	gl.Enable(gl.SCISSOR_TEST)

	w.child.Render()

	gl.Disable(gl.SCISSOR_TEST)

	// Draw scrollbars, if needed
	{
		const scrollbarWidth = 2

		// Vertical
		if w.child.Size()[1] > w.size[1] {
			DrawBorderlessBox(mgl64.Vec2{w.size[0] - scrollbarWidth + 1, -w.child.Pos()[1]/w.child.Size()[1]*(w.size[1]+2) - 1},
				mgl64.Vec2{scrollbarWidth, w.size[1] / w.child.Size()[1] * (w.size[1] + 2)},
				darkColor)
		}

		// Horizontal
		if w.child.Size()[0] > w.size[0] {
			DrawBorderlessBox(mgl64.Vec2{-w.child.Pos()[0]/w.child.Size()[0]*(w.size[0]+2) - 1, w.size[1] - scrollbarWidth + 1},
				mgl64.Vec2{w.size[0] / w.child.Size()[0] * (w.size[0] + 2), scrollbarWidth},
				darkColor)
		}
	}
}

func (w *ScrollPaneWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	LocalPosition := w.ParentToLocal(ParentPosition)

	if len(w.Widget.Hit(ParentPosition)) > 0 {
		hits := []Widgeter{w}
		hits = append(hits, w.child.Hit(LocalPosition)...)
		return hits
	} else {
		return nil
	}
}

func (w *ScrollPaneWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[SLIDER_EVENT] && inputEvent.InputId == 2 {
		w.child.Pos()[0] += inputEvent.Sliders[1] * 10
		w.child.Pos()[1] += inputEvent.Sliders[0] * 10

		// HACK: Snap to nearest point. This prevents smaller scroll increments from being possible.
		w.child.Pos()[0] = float64(NearInt64(w.child.Pos()[0]))
		w.child.Pos()[1] = float64(NearInt64(w.child.Pos()[1]))

		w.Layout()
	}
}

// ---

type SpacerWidget struct {
	Widget
	child  Widgeter
	border int
}

func NewSpacerWidget(pos mgl64.Vec2, child Widgeter) *SpacerWidget {
	w := &SpacerWidget{Widget: NewWidget(pos, np), child: child, border: 2}
	w.child.SetParent(w)
	w.Layout() // TODO: Should this be automatic from above SetParent()?
	return w
}

func (w *SpacerWidget) PollLogic() {
	w.child.PollLogic()

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.PollLogic()
}

func (w *SpacerWidget) Layout() {
	w.Widget.size = w.child.Size().Add(mgl64.Vec2{float64(w.border * 2), float64(w.border * 2)})

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *SpacerWidget) LayoutNeeded() {
	w.child.LayoutNeeded()

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.LayoutNeeded()
}

func (w *SpacerWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(float64(w.pos[0]+float64(w.border)), float64(w.pos[1]+float64(w.border)), 0)

	w.child.Render()
}

func (w *SpacerWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	LocalPosition := w.ParentToLocal(ParentPosition)

	hits := w.child.Hit(LocalPosition)

	return hits
}

func (w *SpacerWidget) ParentToLocal(ParentPosition mgl64.Vec2) (LocalPosition mgl64.Vec2) {
	return w.Widget.ParentToLocal(ParentPosition).Sub(mgl64.Vec2{float64(w.border), float64(w.border)})
}

// ---

type UnderscoreSepToCamelCaseWidget struct {
	Widget
	window *glfw.Window
}

func (w *UnderscoreSepToCamelCaseWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)

	//s := w.window.GetClipboardString()
	s := "get_clipboard_string"
	// E.g., get_clipboard_string -> GetClipboardString
	s += " -> " + UnderscoreSepToCamelCase(s)
	w.size[0] = float64(8 * len(s))
	w.size[1] = 16

	gl.Color3d(0.3, 0.3, 0.3)
	gl.Rectd(0-1, 0-1, float64(w.size[0]+1), float64(w.size[1]+1))
	gl.Color3d(1, 1, 1)
	gl.Rectd(0, 0, float64(w.size[0]), float64(w.size[1]))

	gl.Color3d(0, 0, 0)
	PrintText(mgl64.Vec2{0, 0}, s)
}

// ---

type ChannelExpeWidget struct {
	*FlowLayoutWidget
	cmd *exec.Cmd
	ch  ChanWriter
}

func NewChannelExpeWidget(pos mgl64.Vec2) *ChannelExpeWidget {
	w := &ChannelExpeWidget{ch: make(ChanWriter)}
	action := func() {
		// Comments are currently not preserved in the tooltip

		if w.cmd == nil {
			w.cmd = exec.Command("ping", "google.com")
			w.cmd.Stdout = w.ch
			w.cmd.Stderr = w.ch
			err := w.cmd.Start()
			CheckError(err)
			go w.cmd.Wait() // It looks like I need to wait for the process, else it doesn't terminate properly
		} else {
			//w.cmd.Process.Kill()
			w.cmd.Process.Signal(os.Interrupt)
			w.cmd = nil
		}
	}
	w.FlowLayoutWidget = NewFlowLayoutWidget(pos,
		[]Widgeter{
			NewTriButtonExternalStateWidget(np, func() bool { return w.cmd != nil }, action),
			NewTextBoxWidget(np),
		}, nil)

	UniversalClock.AddChangeListener(w)

	return w
}

func (w *ChannelExpeWidget) NotifyChange() {
	select {
	case b, ok := <-w.ch:
		if ok {
			box := w.FlowLayoutWidget.Widgets[1].(*TextBoxWidget)
			SetViewGroup(box.Content, box.Content.Content()+string(b))
			redraw = true
		}
	default:
	}
}

// ---

type commandNode struct {
	w        *LiveCmdExpeWidget
	template CmdFactory
	DepNode2
}

func (this *commandNode) Update() {
	if this.w.cmd != nil && this.w.cmd.ProcessState == nil {
		//w.cmd.Process.Kill()
		this.w.cmd.Process.Signal(os.Interrupt)
		//w.cmd.Process.Signal(syscall.SIGTERM)
		//fmt.Println("sigint'ed process", this.w.cmd.Process.Pid)
		this.w.cmd = nil
	}

	SetViewGroup(this.w.Content, "")

	MakeUpdatedLock.Unlock() // HACK: Needed because (*CmdTemplateDynamic2) NewCommand() calls MakeUpdated().
	this.w.cmd = this.template.NewCommand()
	MakeUpdatedLock.Lock() // HACK
	this.w.stdoutChan = make(ChanWriter)
	this.w.stderrChan = make(ChanWriter)
	this.w.cmd.Stdout = this.w.stdoutChan
	this.w.cmd.Stderr = this.w.stderrChan

	err := this.w.cmd.Start()
	if err != nil {
		this.w.cmd = nil
		return
	}
	//fmt.Printf("started new process %v %+v\n", this.w.cmd.Process.Pid, this.w.cmd.Args)

	go func(cmd *exec.Cmd) {
		_ = cmd.Wait()
		//fmt.Println("waited til end of", cmd.Process.Pid)
		this.w.finishedChan <- cmd.ProcessState
	}(this.w.cmd)
}

type LiveCmdExpeWidget struct {
	*TextBoxWidget
	commandNode    *commandNode
	cmd            *exec.Cmd
	stdoutChan     ChanWriter
	stderrChan     ChanWriter
	finishedChan   chan *os.ProcessState
	DepNode2Manual // FinishedDepNode2
}

func NewLiveCmdExpeWidget(pos mgl64.Vec2, dependees []DepNode2I, template CmdFactory) *LiveCmdExpeWidget {
	w := &LiveCmdExpeWidget{TextBoxWidget: NewTextBoxWidget(pos), finishedChan: make(chan *os.ProcessState)}

	// THINK: The only reason to have a separate command node is because current NotifyChange() does not tell the originator of change, so I can't tell UniversalClock's changes from dependee changes (and I need to do different actions for each)
	w.commandNode = &commandNode{w: w, template: template}
	w.commandNode.AddSources(dependees...)

	return w
}

func (w *LiveCmdExpeWidget) PollLogic() {
	MakeUpdated(w.commandNode) // THINK: Is this a hack or is this the way to go?
	w.layout2Test()

	w.TextBoxWidget.PollLogic()
}

func (w *LiveCmdExpeWidget) Close() error {
	// TODO: Kill live cmd...

	// TODO: Errors.
	_ = w.TextBoxWidget.Close()
	return nil
}

func (w *LiveCmdExpeWidget) layout2Test() {
	select {
	case b, ok := <-w.stdoutChan:
		if ok {
			SetViewGroup(w.Content, w.Content.Content()+string(b))
			redraw = true
		}
	default:
	}

	select {
	case b, ok := <-w.stderrChan:
		if ok {
			SetViewGroup(w.Content, w.Content.Content()+string(b))
			redraw = true
		}
	default:
	}

	select {
	/*case processState := <-w.finishedChan:
	if processState.Success() {
		// TODO: Is ChangeListener stuff a good fit for these not-really-change events?
		w.SuccessDepNode.NotifyAllListeners()
	}*/
	case <-w.finishedChan:
		ExternallyUpdated(w)
	default:
	}
}

func (w *LiveCmdExpeWidget) Render() {
	w.TextBoxWidget.Render()
}

// ---

type WriterNotifier struct {
	io.Writer
	n func()
}

func (wn *WriterNotifier) Write(p []byte) (n int, err error) {
	wn.n()
	n, err = wn.Writer.Write(p)
	wn.n()
	return
}

type pipeNode struct {
	w *LivePipeExpeWidget
	p PipeFactory
	s *pipe.State
	DepNode2
}

func (this *pipeNode) Update() {
	if this.s != nil {
		this.s.Kill()
		this.s = nil
	}

	SetViewGroup(this.w.Content, "")

	notify := func() {
		//println("yo, notify, yo?")
		//redraw = true
		//glfw.PostEmptyEvent()
	}
	this.w.stdoutChan = &WriterNotifier{make(ChanWriter), notify}
	this.w.stderrChan = &WriterNotifier{make(ChanWriter), notify}
	var p pipe.Pipe
	MakeUpdatedLock.Unlock() // HACK: Needed because NewPipe() calls MakeUpdated().
	this.s, p = this.p.NewPipe(this.w.stdoutChan, this.w.stderrChan)
	MakeUpdatedLock.Lock() // HACK

	go func(s *pipe.State, p pipe.Pipe) {
		err := p(s)
		if err == nil {
			err = this.s.RunTasks()
		}
		//close(this.w.stdoutChan) // This is causing panics because pipe tries to write to closed channel.
		//close(this.w.stderrChan)
		this.w.finishedChan <- err
	}(this.s, p)
}

type LivePipeExpeWidget struct {
	*TextBoxWidget
	pipeNode       *pipeNode
	stdoutChan     *WriterNotifier
	stderrChan     *WriterNotifier
	finishedChan   chan error
	DepNode2Manual // FinishedDepNode2
}

func NewLivePipeExpeWidget(pos mgl64.Vec2, dependees []DepNode2I, p PipeFactory) *LivePipeExpeWidget {
	w := &LivePipeExpeWidget{TextBoxWidget: NewTextBoxWidget(pos), finishedChan: make(chan error)}

	// THINK: The only reason to have a separate pipe node is because current NotifyChange() does not tell the originator of change, so I can't tell UniversalClock's changes from dependee changes (and I need to do different actions for each)
	w.pipeNode = &pipeNode{w: w, p: p}
	w.pipeNode.AddSources(dependees...)

	return w
}

func (w *LivePipeExpeWidget) PollLogic() {
	MakeUpdated(w.pipeNode) // THINK: Is this a hack or is this the way to go?
	w.layout2Test()

	w.TextBoxWidget.PollLogic()
}

func (w *LivePipeExpeWidget) Close() error {
	if w.pipeNode.s != nil {
		w.pipeNode.s.Kill()
		w.pipeNode.s = nil
	}

	// TODO: Errors.
	_ = w.TextBoxWidget.Close()
	return nil
}

func (w *LivePipeExpeWidget) layout2Test() {
	select {
	case b, ok := <-w.stdoutChan.Writer.(ChanWriter):
		if ok {
			SetViewGroup(w.Content, w.Content.Content()+string(b))
			redraw = true
		}
	default:
	}

	select {
	case b, ok := <-w.stderrChan.Writer.(ChanWriter):
		if ok {
			SetViewGroup(w.Content, w.Content.Content()+string(b))
			redraw = true
		}
	default:
	}

	select {
	/*case processState := <-w.finishedChan:
	if processState.Success() {
		// TODO: Is ChangeListener stuff a good fit for these not-really-change events?
		w.SuccessDepNode.NotifyAllListeners()
	}*/
	case <-w.finishedChan:
		ExternallyUpdated(w)
	default:
	}
}

func (w *LivePipeExpeWidget) Render() {
	w.TextBoxWidget.Render()
}

// ---

type actionNode struct {
	owner  *LiveGoroutineExpeWidget
	params func() interface{}
	action func(interface{}) string
	DepNode2
}

func (this *actionNode) Update() {
	// TODO: See if it's possible to have a _general_ solution to EOL goroutines whose work has become obsolete
	// In order not to overload the system, only start a new goroutine if all others have finished
	if this.owner.lastStartedT == this.owner.lastFinishedT {
		this.owner.lastStartedT++
		ti := this.owner.lastStartedT

		//this.owner.Content.Set(this.action()); _ = ti
		go func(params interface{}) {
			//defer close(outChan)
			//started := time.Now()
			ts := timestampString{this.action(params), ti}
			//fmt.Println(time.Since(started).Seconds())
			this.owner.outChan <- ts
		}(this.params())
	} else {
		// TODO: Make the update happen later, once the current goroutine is done?
	}
}

type timestampString struct {
	s string
	t uint32
}

type LiveGoroutineExpeWidget struct {
	*FlowLayoutWidget
	actionNode                  *actionNode
	outChan                     chan timestampString
	lastStartedT, lastFinishedT uint32
	live                        bool
}

func NewLiveGoroutineExpeWidget(pos mgl64.Vec2, live bool, dependees []DepNode2I, params func() interface{}, action func(interface{}) string) *LiveGoroutineExpeWidget {
	w := &LiveGoroutineExpeWidget{outChan: make(chan timestampString), live: live}

	refreshButton := NewButtonWidget(np, func() { MakeUpdated(w.actionNode) })
	liveToggle := NewTriButtonExternalStateWidget(np, func() bool { return w.live }, func() { w.live = !w.live })
	textBoxWidget := NewTextBoxWidget(pos)

	w.FlowLayoutWidget = NewFlowLayoutWidget(pos, []Widgeter{refreshButton, liveToggle, textBoxWidget}, nil)

	// THINK: The only reason to have a separate action node is because current NotifyChange() does not tell the originator of change, so I can't tell UniversalClock's changes from dependee changes (and I need to do different actions for each)
	w.actionNode = &actionNode{owner: w, params: params, action: action}
	w.actionNode.AddSources(dependees...)

	return w
}

func (w *LiveGoroutineExpeWidget) PollLogic() {
	if w.live {
		MakeUpdated(w.actionNode) // THINK: Is this a hack or is this the way to go?
	}
	w.layout2Test()

	w.FlowLayoutWidget.PollLogic()
}

func (w *LiveGoroutineExpeWidget) Close() error {
	// TODO: Kill live goroutine?

	// TODO: Errors.
	_ = w.FlowLayoutWidget.Close()
	return nil
}

func (w *LiveGoroutineExpeWidget) layout2Test() {
	select {
	case s, ok := <-w.outChan:
		if ok {
			if s.t > w.lastFinishedT {
				w.lastFinishedT = s.t

				SetViewGroup(w.Widgets[2].(*TextBoxWidget).Content, s.s) // HACK: Should get Content in a better way
				redraw = true
			}
		}
	default:
	}
}

func (w *LiveGoroutineExpeWidget) Render() {
	w.FlowLayoutWidget.Render()
}

// ---

type ConnectionWidget struct {
	Widget
}

func NewConnectionWidget(pos mgl64.Vec2) *ConnectionWidget {
	w := &ConnectionWidget{Widget: NewWidget(pos, mgl64.Vec2{fontHeight, fontHeight})}
	return w
}

func (w *ConnectionWidget) Render() {
	// HACK: Brute-force check the mouse pointer if it contains this widget
	isOriginHit := false
	for _, hit := range mousePointer.OriginMapping {
		if w == hit {
			isOriginHit = true
			break
		}
	}
	isHit := len(w.HoverPointers()) > 0

	// HACK: Assumes mousePointer rather than considering all connected pointing pointers
	if isOriginHit && mousePointer.State.IsActive() && isHit {
		DrawCircle(w.pos, w.size, highlightColor, nearlyWhiteColor)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawCircle(w.pos, w.size, highlightColor, nearlyWhiteColor)
	} else {
		DrawCircle(w.pos, w.size, mgl64.Vec3{0.3, 0.3, 0.3}, nearlyWhiteColor)
	}

	DrawCircle(w.pos, w.size.Mul(0.5625), mgl64.Vec3{0.3, 0.3, 0.3}, mgl64.Vec3{0.3, 0.3, 0.3})

	if isOriginHit && mousePointer.State.IsActive() {
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)

		globalPosition := mgl64.Vec2{mousePointer.State.Axes[0], mousePointer.State.Axes[1]}
		localPosition := WidgeterS{w}.GlobalToLocal(globalPosition)

		gl.Color3d(0, 0, 0)
		gl.Begin(gl.LINES)
		gl.Vertex2d(float64(0), float64(0))
		gl.Vertex2d(float64(localPosition[0]), float64(localPosition[1]))
		gl.End()
	}
}

func (w *ConnectionWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	if w.pos.Sub(ParentPosition).Len() <= w.size[0]/2 {
		return []Widgeter{w}
	} else {
		return nil
	}
}

// ---

type HttpServerTestWidget struct {
	*FlowLayoutWidget
	started        bool
	stopServerChan chan struct{}
}

func NewHttpServerTestWidget(pos mgl64.Vec2) *HttpServerTestWidget {
	const httpServerAddr = ":8060"

	w := &HttpServerTestWidget{stopServerChan: make(chan struct{})}
	action := func() {
		if !w.started {
			go func() {
				err := ListenAndServeStoppable(httpServerAddr, nil, w.stopServerChan)
				CheckError(err)
			}()
		} else {
			w.stopServerChan <- struct{}{}
		}
		w.started = !w.started // TODO: Factor this out to toggle-button?
	}
	action()
	w.FlowLayoutWidget = NewFlowLayoutWidget(pos,
		[]Widgeter{
			NewTriButtonExternalStateWidget(np, func() bool { return w.started }, action),
		}, nil)
	if publicIps, err := gist8065433.GetPublicIps(); err == nil && len(publicIps) >= 1 {
		w.FlowLayoutWidget.AddWidget(NewTextLabelWidgetString(np, "http://"+publicIps[0]+httpServerAddr))
	}

	return w
}

// ---

type VcsLocalInvalidator struct {
	DepNode2
	init bool

	Repo *exp13.VcsState
}

func (this *VcsLocalInvalidator) Update() {
	// Don't do anything for initialization, only for further changes.
	if !this.init {
		this.init = true
		return
	}

	//fileView := this.GetSources()[0].(*FileView)
	//println("changed:", fileView.path, "of:", this.Repo.Vcs.RootPath()[32:])

	// Invalidate the Repo.VcsLocal.
	ExternallyUpdated(this.Repo.VcsLocal.GetSources()[1].(DepNode2ManualI))
}

func (this *VcsLocalInvalidator) SetTarget() {
}

// ---

type FileOpener struct {
	editor     ViewGroupI
	openedFile *FileView

	DepNode2
}

func NewFileOpener(editor ViewGroupI) *FileOpener {
	this := &FileOpener{editor: editor}
	return this
}

func (this *FileOpener) Update() {
	if this.openedFile != nil {
		this.editor.RemoveView(this.openedFile)
		this.openedFile.Close()
		this.openedFile = nil

		SetViewGroup(this.editor, "")
	}

	if path := this.GetSources()[0].(*FolderListingWidget).GetSelectedPath(); strings.HasSuffix(path, ".go") {
		this.openedFile = NewFileView(path)

		this.openedFile.AddAndSetViewGroup(this.editor, TryReadFile(path))

		if goPackage := this.GetSources()[1].(GoPackageSelecter).GetSelectedGoPackage(); goPackage != nil {
			MakeUpdatedLock.Unlock() // HACK: Needed because UpdateVcs() calls MakeUpdated().
			goPackage.UpdateVcs()
			MakeUpdatedLock.Lock() // HACK
			if goPackage.Dir.Repo != nil {
				vcsLocalInvalidator := &VcsLocalInvalidator{Repo: goPackage.Dir.Repo}
				vcsLocalInvalidator.AddSources(this.openedFile)
				keepUpdatedTEST = append(keepUpdatedTEST, vcsLocalInvalidator) // TODO: Clear old ones...
			}
		}
	}
}

// ---

type DepDumper struct {
	DepNode2
}

func (this *DepDumper) Update() {
	goon.Dump(this.GetSources()[0].(*FolderListingWidget).GetSelectedPath())
}

// ---

type SpinnerWidget struct {
	Widget
	Spinner uint32
	DepNode2
}

func (w *SpinnerWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Color3d(0, 0, 0)
	gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)
	//gl.Rotated(float64(spinner), 0, 0, 1)
	gl.Rotated(float64(w.Spinner), 0, 0, 1)
	gl.Begin(gl.LINES)
	gl.Vertex2i(0, -10)
	gl.Vertex2i(0, 10)
	gl.End()
}

func (w *SpinnerWidget) Update() {
	//w.Spinner++
	w.Spinner += 45
}

// ---

type fpsSample struct{ Render, Total float64 }

type FpsWidget struct {
	Widget
	samples []fpsSample
}

func NewFpsWidget(pos mgl64.Vec2) *FpsWidget {
	return &FpsWidget{Widget: NewWidget(pos, np)}
}

func (w *FpsWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)
	gl.Begin(gl.LINES)
	gl.Color3d(1, 0, 0)
	gl.Vertex2d(float64(0), float64(-1000.0/60))
	gl.Vertex2d(float64(30), float64(-1000.0/60))
	gl.End()
	for index, sample := range w.samples {
		var color mgl64.Vec3
		if sample.Render <= 1000.0/60*1.25 {
			color = mgl64.Vec3{0, 0, 0}
		} else {
			color = mgl64.Vec3{1, 0, 0}
		}
		DrawBorderlessBox(mgl64.Vec2{float64(30 - len(w.samples) + index), -sample.Render}, mgl64.Vec2{1, sample.Render}, color)
		DrawBorderlessBox(mgl64.Vec2{float64(30 - len(w.samples) + index), -sample.Total}, mgl64.Vec2{1, sample.Total - sample.Render}, mgl64.Vec3{0.65, 0.65, 0.65})
	}
}

func (w *FpsWidget) PushTimeToRender(sample float64) {
	w.samples = append(w.samples, fpsSample{Render: sample})
	if len(w.samples) > 30 {
		w.samples = w.samples[len(w.samples)-30:]
	}
}
func (w *FpsWidget) PushTimeTotal(sample float64) {
	w.samples[len(w.samples)-1].Total = sample
}

// ---

func WidgeterIndex(widgeters []Widgeter, w Widgeter) int {
	for index := range widgeters {
		if w == widgeters[index] {
			return index
		}
	}

	return -1
}

// ---

type SearchableListWidget struct {
	*CompositeWidget

	// Internal access shortcuts (also inside CompositeWidget).
	searchField Widgeter
	listWidget  *FilterableSelecterWidget

	ExtensionsTest []Widgeter
}

func NewSearchableListWidget(pos, size mgl64.Vec2, entries SliceStringer) *SearchableListWidget {
	searchField := NewTextBoxWidgetOptions(np, TextBoxWidgetOptions{SingleLine: true})
	//listWidget := NewListWidget(mathgl.Vec2d{0, fontHeight + 2}, entries)
	listWidget := NewFilterableSelecterWidget(np, entries, searchField.Content)
	listWidgetScrollPane := NewScrollPaneWidget(mgl64.Vec2{0, fontHeight + 2}, size, listWidget)

	w := &SearchableListWidget{CompositeWidget: NewCompositeWidget(pos, []Widgeter{searchField, listWidgetScrollPane}), searchField: searchField, listWidget: listWidget}

	return w
}

type entriesPlusOne struct {
	SliceStringer
	extra *TextBoxWidget
}

func (this *entriesPlusOne) Get(index uint64) fmt.Stringer {
	if index < this.SliceStringer.Len() {
		return this.SliceStringer.Get(index)
	} else {
		return json.Number("Create new \"" + this.extra.Content.Content() + "\"...") // HACK: Should use a better suited fmt.Stringer type.
	}
}

func (this *entriesPlusOne) Len() uint64 {
	return this.SliceStringer.Len() + 1
}

func NewSearchableListWidgetTest(pos, size mgl64.Vec2, entries SliceStringer) *SearchableListWidget {
	searchField := NewTextBoxWidgetOptions(np, TextBoxWidgetOptions{SingleLine: true})
	listWidget := NewFilterableSelecterWidget(np, &entriesPlusOne{entries, searchField}, searchField.Content)
	listWidgetScrollPane := NewScrollPaneWidget(mgl64.Vec2{0, fontHeight + 2}, size, listWidget)

	w := &SearchableListWidget{CompositeWidget: NewCompositeWidget(pos, []Widgeter{searchField, listWidgetScrollPane}), searchField: searchField, listWidget: listWidget}

	return w
}

func (w *SearchableListWidget) OnSelectionChanged() Selecter {
	return w.listWidget
}

// HACK/TEST: Mostly for external callers...
func (w *SearchableListWidget) SetKeyboardFocus() {
	// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
	// HACK: Temporarily set both widgets as mapping here
	keyboardPointer.OriginMapping = append([]Widgeter{w}, w.searchField, w.listWidget)
}

func (w *SearchableListWidget) ProcessEvent(inputEvent InputEvent) {
	for _, extension := range w.ExtensionsTest {
		extension.ProcessEvent(inputEvent)
	}

	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && // TODO: GetHoverer() // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { // TODO: GetHoverer() // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		w.SetKeyboardFocus()
	}
}

func (w *SearchableListWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		hits := []Widgeter{w}
		hits = append(hits, w.CompositeWidget.Hit(ParentPosition)...)
		return hits
	} else {
		return nil
	}
}

// ---

type FilterableSliceStringer struct {
	filteredEntries []fmt.Stringer

	DepNode2
}

func NewFilterableSliceStringer(source SliceStringer, filter MultilineContentI) *FilterableSliceStringer {
	this := &FilterableSliceStringer{}
	this.AddSources(source, filter)
	return this
}

func (this *FilterableSliceStringer) Get(index uint64) fmt.Stringer {
	return this.filteredEntries[index]
}

func (this *FilterableSliceStringer) Len() uint64 {
	return uint64(len(this.filteredEntries))
}

func (this *FilterableSliceStringer) Update() {
	source := this.GetSources()[0].(SliceStringer)
	filter := this.GetSources()[1].(MultilineContentI)

	this.filteredEntries = nil
	for index := uint64(0); index < source.Len(); index++ {
		entry := source.Get(index).String()

		if filter.Content() != "" {
			if !strings.Contains(strings.ToLower(entry), strings.ToLower(filter.Content())) { // TODO: Do case folding correctly
				continue
			}
		}

		this.filteredEntries = append(this.filteredEntries, source.Get(index))
	}
}

// ---

type SliceStringer interface {
	Get(uint64) fmt.Stringer
	Len() uint64

	DepNode2I
}

type Selecter interface {
	GetSelected() fmt.Stringer

	DepNode2I
}

// TODO: Refactor other list-like widgets to use this?
type FilterableSelecterWidget struct {
	Widget
	longestEntryLength int
	selected           uint64 // index of selected entry [0, len)
	manuallyPicked     fmt.Stringer
	DepNode2Manual     // SelectionChanged
	layoutDepNode2     DepNode2Func

	entries SliceStringer
}

func NewFilterableSelecterWidget(pos mgl64.Vec2, entries SliceStringer, filter MultilineContentI) *FilterableSelecterWidget {
	entries = NewFilterableSliceStringer(entries, filter)

	w := &FilterableSelecterWidget{Widget: NewWidget(pos, np), entries: entries}

	w.layoutDepNode2.UpdateFunc = func(DepNode2I) { w.NotifyChange() }
	w.layoutDepNode2.AddSources(entries) // TODO: What about removing w when it's "deleted"?

	return w
}

func (this *FilterableSelecterWidget) GetSelected() fmt.Stringer {
	if this.entries.Len() == 0 {
		return nil
	}
	return this.entries.Get(this.selected)
}

func (w *FilterableSelecterWidget) NotifyChange() {
	selectionPreserved := false

	w.longestEntryLength = 0
	for index := uint64(0); index < w.entries.Len(); index++ {
		entry := w.entries.Get(index).String()
		entryLength := len(entry)
		if entryLength > w.longestEntryLength {
			w.longestEntryLength = entryLength
		}

		// Preserve selection
		if w.entries.Get(index) == w.manuallyPicked {
			w.selected = index
			selectionPreserved = true
		}
	}

	if !selectionPreserved {
		w.selected = 0
		if w.entries.Len() > 0 {
			// HACK, TODO: This should happen not when the selection is unpreserved, but when it is unchanged
			// (i.e. it may be unpreserved, but still equal, then no need to report a change)
			w.selectionChangedTest() // TEST, HACK: This sets manuallyPicked because it's meant for user-driven actions, etc. Need to do this in a better way.
		}
		w.manuallyPicked = nil
	}

	w.Layout()

	w.NotifyAllListeners()
}

// Pre-condition: w.entries.Len() > 0
func (w *FilterableSelecterWidget) selectionChangedTest() {
	// TEST
	if scrollPane, ok := w.Parent().(*ScrollPaneWidget); ok && scrollPane.child == w {
		scrollPane.ScrollToArea(mgl64.Vec2{0, float64(w.selected * fontHeight)}, mgl64.Vec2{float64(w.longestEntryLength * fontWidth), fontHeight})
	}

	w.manuallyPicked = w.entries.Get(w.selected)

	ExternallyUpdated(w)
}

func (w *FilterableSelecterWidget) Layout() {
	if w.longestEntryLength < 3 {
		w.size[0] = float64(fontWidth * 3)
	} else {
		w.size[0] = float64(fontWidth * w.longestEntryLength)
	}
	if w.entries.Len() == 0 {
		w.size[1] = float64(fontHeight * 1)
	} else {
		w.size[1] = float64(fontHeight * w.entries.Len())
	}

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *FilterableSelecterWidget) LayoutNeeded() {
	MakeUpdated(&w.layoutDepNode2)
}

func (w *FilterableSelecterWidget) Render() {
	DrawNBox(w.pos, w.size)

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer
	hasTypingFocus := keyboardPointer != nil && keyboardPointer.OriginMapping.ContainsWidget(w)

	// Render only visible lines.
	// TODO: Generalize this.
	const debugSmallerViewport = fontHeight
	beginLineIndex, endLineIndex := 0, int(w.entries.Len())
	if beginVisibleLineIndex := int(WidgeterS{w}.GlobalToLocal(mgl64.Vec2{0, debugSmallerViewport})[1] / fontHeight); beginVisibleLineIndex > beginLineIndex {
		beginLineIndex = intmath.MinInt(beginVisibleLineIndex, endLineIndex)
	}
	_, height := globalWindow.GetSize() // HACK: Should be some viewport
	height -= debugSmallerViewport
	if endVisibleLineIndex := int(WidgeterS{w}.GlobalToLocal(mgl64.Vec2{0, float64(height)})[1]/fontHeight + 1); endVisibleLineIndex < endLineIndex {
		endLineIndex = intmath.MaxInt(endVisibleLineIndex, beginLineIndex)
	}

	for ; beginLineIndex < endLineIndex; beginLineIndex++ {
		entry := w.entries.Get(uint64(beginLineIndex)).String()
		if w.selected == uint64(beginLineIndex) {
			if hasTypingFocus {
				DrawBorderlessBox(w.pos.Add(mgl64.Vec2{0, float64(beginLineIndex * fontHeight)}), mgl64.Vec2{w.size[0], fontHeight}, mgl64.Vec3{0.21, 0.45, 0.84})
				gl.Color3d(1, 1, 1)
			} else {
				DrawBorderlessBox(w.pos.Add(mgl64.Vec2{0, float64(beginLineIndex * fontHeight)}), mgl64.Vec2{w.size[0], fontHeight}, mgl64.Vec3{0.83, 0.83, 0.83})
				gl.Color3d(0, 0, 0)
			}
		} else {
			gl.Color3d(0, 0, 0)
		}

		PrintText(w.pos.Add(mgl64.Vec2{0, float64(beginLineIndex * fontHeight)}), entry)
	}
}

func (w *FilterableSelecterWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}
func (w *FilterableSelecterWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		if !keyboardPointer.OriginMapping.ContainsWidget(w) {
			keyboardPointer.OriginMapping = []Widgeter{w}
		}
	}

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer
	hasTypingFocus := keyboardPointer != nil && keyboardPointer.OriginMapping.ContainsWidget(w)

	// Check if button 0 was released (can't do pressed atm because first on-focus event gets ignored, and it promotes on-move switching, etc.)
	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false) {
		globalPosition := mgl64.Vec2{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
		localPosition := WidgeterS{w}.GlobalToLocal(globalPosition)
		if w.entries.Len() > 0 {
			if localPosition[1] < 0 {
				w.selected = 0
			} else if uint64(localPosition[1]/fontHeight) > uint64(w.entries.Len()-1) {
				w.selected = uint64(w.entries.Len() - 1)
			} else {
				w.selected = uint64(localPosition[1] / fontHeight)
			}
			w.selectionChangedTest()
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
		switch glfw.Key(inputEvent.InputId) {
		case glfw.KeyUp:
			if inputEvent.ModifierKey == glfw.ModSuper {
				if w.entries.Len() > 0 && w.selected > 0 {
					w.selected = 0
					w.selectionChangedTest()
				}
			} else if inputEvent.ModifierKey == 0 {
				if w.entries.Len() > 0 && w.selected > 0 {
					w.selected--
					w.selectionChangedTest()
				}
			}
		case glfw.KeyDown:
			if inputEvent.ModifierKey == glfw.ModSuper {
				if w.entries.Len() > 0 && w.selected < uint64(w.entries.Len()-1) {
					w.selected = uint64(w.entries.Len() - 1)
					w.selectionChangedTest()
				}
			} else if inputEvent.ModifierKey == 0 {
				if w.entries.Len() > 0 && w.selected < uint64(w.entries.Len()-1) {
					w.selected++
					w.selectionChangedTest()
				}
			}
		}
	}
}

// ---

// TODO: Remove this, it's been superseded by more general NewGoPackageListingWidget().
// TODO: Take its "background streaming" functionality before removing.
/*func NewGoPackageListingPureWidget() *GoPackageListingPureWidget {
	w := &GoPackageListingPureWidget{Widget: NewWidget(np, np), goPackages: make(chan ImportPathFound, 64)}

	go gist8018045.GetGoPackages(w.goPackages)
	UniversalClock.AddChangeListener(w)

	return w
}

func (w *GoPackageListingPureWidget) NotifyChange() {
	// TODO: Support for preserving selection

	for {
		select {
		case entry, ok := <-w.goPackages:
			if ok {
				w.entries = append(w.entries, entry)

				entryLength := len(entry.ImportPath())
				if entryLength > w.longestEntryLength {
					w.longestEntryLength = entryLength
				}

				redraw = true

				w.Layout()
			} else {
				return
			}
		default:
			return
		}
	}
}*/

// ---

type FolderListingWidget struct {
	*CompositeWidget
	flow *FlowLayoutWidget // HACK: Shortcut to CompositeWidget.Widgets[0]

	DepNode2Manual // SelectionChanged
}

func NewFolderListingWidget(pos mgl64.Vec2, path string) *FolderListingWidget {
	w := &FolderListingWidget{CompositeWidget: NewCompositeWidget(pos, []Widgeter{NewFlowLayoutWidget(np, []Widgeter{newFolderListingPureWidget(path)}, nil)})}
	w.flow = w.Widgets[0].(*FlowLayoutWidget)
	w.flow.SetParent(w) // HACK?
	return w
}

// TODO: Use a custom path type instead of a string?
func (w *FolderListingWidget) GetSelectedPath() (path string) {
	for _, widget := range w.flow.Widgets {
		if pure := widget.(*FolderListingPureWidget); pure.selected != 0 {
			path = filepath.Join(pure.path, pure.entries[pure.selected-1].Name())
			if pure.entries[pure.selected-1].IsDir() {
				path += string(filepath.Separator)
			}
		}
	}
	return path
}

func (w *FolderListingWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
		switch glfw.Key(inputEvent.InputId) {
		case glfw.KeyLeft:
			c := keyboardPointer.OriginMapping[0] // HACK
			index := WidgeterIndex(w.flow.Widgets, c)

			if index > 0 {
				// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
				// HACK: Temporarily set both this and parent as mapping here
				c = w.flow.Widgets[index-1]
				keyboardPointer.OriginMapping = []Widgeter{c, w}
				if cp, ok := c.(*FolderListingPureWidget); ok {
					cp.selectionChangedTest()
				}
			}
		case glfw.KeyRight:
			c := keyboardPointer.OriginMapping[0] // HACK
			index := WidgeterIndex(w.flow.Widgets, c)

			if index != -1 && index+1 < len(w.flow.Widgets) {
				// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
				// HACK: Temporarily set both this and parent as mapping here
				c = w.flow.Widgets[index+1]
				keyboardPointer.OriginMapping = []Widgeter{c, w}
				if cp, ok := c.(*FolderListingPureWidget); ok && cp.selected == 0 && len(cp.entries) > 0 {
					cp.selected = 1
					cp.selectionChangedTest()
				}
			}
		}
	}
}

// ---

type FolderListingPureWidget struct {
	Widget
	path               string
	entries            []os.FileInfo
	longestEntryLength int
	selected           uint64 // 0 is unselected, else index+1 is selected
}

func newFolderListingPureWidget(path string) *FolderListingPureWidget {
	w := &FolderListingPureWidget{Widget: NewWidget(np, np), path: path}
	w.NotifyChange() // TODO: Give it a proper source
	return w
}

func newFolderListingPureWidgetWithSelection(path string) *FolderListingPureWidget {
	w := newFolderListingPureWidget(path)
	if len(w.entries) >= 1 {
		w.selected = 1
	}
	return w
}

func (w *FolderListingPureWidget) NotifyChange() {
	// TODO: Support for preserving selection

	entries, err := ioutil.ReadDir(w.path)
	if err == nil {
		w.entries = make([]os.FileInfo, 0, len(entries))
		w.longestEntryLength = 0
		for _, v := range entries {
			//if !strings.HasPrefix(v.Name(), ".") {
			// HACK: Temporarily override this to list .go files only
			if !strings.HasPrefix(v.Name(), ".") && strings.HasSuffix(v.Name(), ".go") && !v.IsDir() {
				w.entries = w.entries[:len(w.entries)+1]
				w.entries[len(w.entries)-1] = v

				entryLength := len(v.Name())
				if v.IsDir() {
					entryLength++
				}
				if entryLength > w.longestEntryLength {
					w.longestEntryLength = entryLength
				}
			}
		}
	}

	w.Layout()

	w.NotifyAllListeners()
}

func (w *FolderListingPureWidget) selectionChangedTest() {
	if w.selected != 0 && w.entries[w.selected-1].IsDir() {
		path := filepath.Join(w.path, w.entries[w.selected-1].Name())
		var newFolder Widgeter

		/*if bpkg, err := BuildPackageFromSrcDir(path); err == nil {
			dpkg := GetDocPackage(bpkg, err)

			out := Underline(`import "`+dpkg.ImportPath+`"`) + "\n"
			for _, v := range dpkg.Vars {
				out += SprintAstBare(v.Decl) + "\n"
			}
			out += "\n"
			for _, f := range dpkg.Funcs {
				out += SprintAstBare(f.Decl) + "\n"
			}
			out += "\n"
			for _, c := range dpkg.Consts {
				out += SprintAstBare(c.Decl) + "\n"
			}
			out += "\n"
			for _, t := range dpkg.Types {
				out += fmt.Sprint(t.Name) + "\n"
				//PrintlnAstBare(t.Decl)
			}

			newFolder = NewTextLabelWidgetString(np, out)
		} else if isGitRepo, status := IsFolderGitRepo(path); isGitRepo {
			newFolder = NewTextLabelWidgetString(np, status)
		} else */{
			newFolder = newFolderListingPureWidget(path)
		}

		p := w.Parent().(*FlowLayoutWidget)
		index := WidgeterIndex(p.Widgets, w)
		p.SetWidgets(append(p.Widgets[:index+1], newFolder))
	} else {
		p := w.Parent().(*FlowLayoutWidget)
		index := WidgeterIndex(p.Widgets, w)
		p.SetWidgets(p.Widgets[:index+1])
	}

	ExternallyUpdated(w.Parent().Parent().(DepNode2ManualI))
}

func (w *FolderListingPureWidget) Layout() {
	if w.longestEntryLength < 3 {
		w.size[0] = float64(fontWidth * 3)
	} else {
		w.size[0] = float64(fontWidth * w.longestEntryLength)
	}
	if len(w.entries) == 0 {
		w.size[1] = float64(fontHeight * 1)
	} else {
		w.size[1] = float64(fontHeight * len(w.entries))
	}

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

const PathSeparator = "/"

func (w *FolderListingPureWidget) Render() {
	DrawNBox(w.pos, w.size)

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && keyboardPointer.OriginMapping.ContainsWidget(w)

	for i, v := range w.entries {
		if w.selected == uint64(i+1) {
			if hasTypingFocus {
				DrawBorderlessBox(w.pos.Add(mgl64.Vec2{0, float64(i * fontHeight)}), mgl64.Vec2{w.size[0], fontHeight}, mgl64.Vec3{0.21, 0.45, 0.84})
				gl.Color3d(1, 1, 1)
			} else {
				DrawBorderlessBox(w.pos.Add(mgl64.Vec2{0, float64(i * fontHeight)}), mgl64.Vec2{w.size[0], fontHeight}, mgl64.Vec3{0.83, 0.83, 0.83})
				gl.Color3d(0, 0, 0)
			}
		} else {
			gl.Color3d(0, 0, 0)
		}

		if v.IsDir() {
			PrintText(w.pos.Add(mgl64.Vec2{0, float64(i * fontHeight)}), v.Name()+PathSeparator)
		} else {
			PrintText(w.pos.Add(mgl64.Vec2{0, float64(i * fontHeight)}), v.Name())
		}
	}
}

func (w *FolderListingPureWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}
func (w *FolderListingPureWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		// HACK: Temporarily set both this and parent as mapping here
		p := w.Parent().Parent().(*FolderListingWidget)
		keyboardPointer.OriginMapping = []Widgeter{w, p}
	}

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && keyboardPointer.OriginMapping.ContainsWidget(w)

	// Check if button 0 was released (can't do pressed atm because first on-focus event gets ignored, and it promotes on-move switching, etc.)
	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false) {
		globalPosition := mgl64.Vec2{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
		localPosition := WidgeterS{w}.GlobalToLocal(globalPosition)
		if len(w.entries) > 0 {
			if localPosition[1] < 0 {
				w.selected = 1
			} else if uint64((localPosition[1]/fontHeight)+1) > uint64(len(w.entries)) {
				w.selected = uint64(len(w.entries))
			} else {
				w.selected = uint64((localPosition[1] / fontHeight) + 1)
			}
			w.selectionChangedTest()
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
		switch glfw.Key(inputEvent.InputId) {
		case glfw.KeyUp:
			if inputEvent.ModifierKey == glfw.ModSuper {
				if len(w.entries) > 0 {
					w.selected = 1
					w.selectionChangedTest()
				}
			} else if inputEvent.ModifierKey == 0 {
				if w.selected > 1 {
					w.selected--
					w.selectionChangedTest()
				}
			}
		case glfw.KeyDown:
			if inputEvent.ModifierKey == glfw.ModSuper {
				if len(w.entries) > 0 {
					w.selected = uint64(len(w.entries))
					w.selectionChangedTest()
				}
			} else if inputEvent.ModifierKey == 0 {
				if w.selected < uint64(len(w.entries)) {
					w.selected++
					w.selectionChangedTest()
				}
			}
		}
	}
}

// ---

type GoonWidget struct {
	*CompositeWidget
	title    string
	a        reflect.Value
	expanded *TriButtonWidget
}

func NewGoonWidget(pos mgl64.Vec2, a interface{}) Widgeter {
	title := GetParentArgExprAsString(1)
	if !strings.HasPrefix(title, "&") {
		panic("NewGoonWidget: Need to pass address of value.")
	}
	//goonWidget := newGoonWidget(mathgl.Vec2d{fontHeight + 2}, title[1:], reflect.ValueOf(a))
	goonWidget := setupInternals3(mgl64.Vec2{fontHeight + 2}, title[1:], UnsafeReflectValue(reflect.ValueOf(a)).Elem())
	return NewCompositeWidget(pos, []Widgeter{goonWidget})
}

func newGoonWidget(pos mgl64.Vec2, title string, a reflect.Value) *GoonWidget {
	/*expanded := &Bool{}

	action := func() {
		expanded.Toggle()
	}
	b := NewButtonWidget(mathgl.Vec2d{}, action)

	contentFunc := func() string {
		if expanded.Get() {
			return goon.Sdump(a)
		} else {
			return fmt.Sprintf("(%T)(%v)", a, a)
		}
	}
	dependees := []DepNodeI{expanded}
	mc := NewMultilineContentFunc(contentFunc, dependees)
	mc.NotifyChange()
	t := NewTextLabelWidgetExternalContent(mathgl.Vec2d{16 + 2}, mc)

	return &GoonWidget{CompositeWidget: NewCompositeWidget(pos, []Widgeter{b, t}), a: a}*/

	a = UnsafeReflectValue(a)

	w := &GoonWidget{CompositeWidget: NewCompositeWidget(pos, nil), title: title, a: a}
	w.setupInternals()
	return w
}

func (w *GoonWidget) flip() {
	w.setupInternals()
}
func (w *GoonWidget) setupInternals() {
	expandable := w.checkInternals()
	oldParent := w.parent
	if expandable {
		if w.expanded == nil {
			w.expanded = NewTriButtonWidget(mgl64.Vec2{-fontHeight - 2}, func() { w.flip() })
		}

		w.CompositeWidget = NewCompositeWidget(w.pos, []Widgeter{w.expanded, &Widget{}})
	} else {
		w.CompositeWidget = NewCompositeWidget(w.pos, []Widgeter{&Widget{}})
	}
	w.SetParent(oldParent)

	var f *FlowLayoutWidget
	if w.expanded == nil || !w.expanded.State() {
		title := NewTextLabelWidgetString(np, w.title+": ")

		var mc MultilineContentI
		if !expandable {
			// TODO: Strings need %#v, numbers need %+v
			mc = NewMultilineContentFuncInstant(func() string { return fmt.Sprintf("(%s)(%+v)", getTypeString(w.a.Elem()), w.a.Elem().Interface()) })
		} else {
			mc = NewMultilineContentString(fmt.Sprintf("%s{...}", getTypeString(w.a.Elem())))
		}
		t := NewTextLabelWidgetExternalContent(np, mc)
		f = NewFlowLayoutWidget(np, []Widgeter{title, t}, nil)
	} else {
		f = w.setupInternals2(w.a)
	}
	f.SetParent(w) // HACK: This needs to be automated, it's too easy to forget to do
	w.Widgets[len(w.Widgets)-1] = f
	f.Layout()
}

func (w *GoonWidget) checkInternals() (depth bool) {
	v := w.a.Elem()

	switch v.Kind() {
	case reflect.Struct, reflect.Map, reflect.Array, reflect.Slice, reflect.Interface:
		return true
	case reflect.Ptr: //reflect.Chan, reflect.Func, reflect.Interface, reflect.Map, reflect.Slice
		return !v.IsNil()
	default:
		return false
	}
}

func getTypeString(a reflect.Value) string {
	// TODO: Do this properly
	//return fmt.Sprintf("%T/%s/%s", a.Interface(), a.Type().Name(), a.Type().String())
	return a.Type().String() // Let's see how this works out...
}

func (w *GoonWidget) setupInternals2(a reflect.Value) (f *FlowLayoutWidget) {
	v := a.Elem()

	title := NewTextLabelWidgetString(np, w.title+": ")
	t := NewTextLabelWidgetString(np, fmt.Sprintf("%s{", getTypeString(v)))
	header := NewFlowLayoutWidget(np, []Widgeter{title, t}, nil)
	tab := mgl64.Vec2{fontHeight + 2}

	widgets := []Widgeter{header}

	switch v.Kind() {
	case reflect.Chan, reflect.Func, reflect.Interface, reflect.Map, reflect.Ptr, reflect.Slice:
		// TODO: Instead of skipping nil values, maybe pass the info as a bool parameter to query?
		if v.IsNil() {
			widgets = append(widgets, NewTextLabelWidgetString(np, "}"))

			return NewFlowLayoutWidget(np, widgets, &FlowLayoutWidgetOptions{FlowLayoutType: VerticalLayout})
		}
	}

	// Follow pointer
	switch v.Kind() {
	case reflect.Ptr, reflect.Interface:
		v = v.Elem()
	}

	switch v.Kind() {
	case reflect.Struct:
		vt := v.Type()
		for i := 0; i < v.NumField(); i++ {
			widgets = append(widgets, setupInternals3(tab, vt.Field(i).Name, v.Field(i)))
		}
	case reflect.Map:
		for _, key := range v.MapKeys() {
			addrValue := UnsafeReflectValue(v.MapIndex(key))
			widgets = append(widgets, setupInternals3(tab, key.String(), addrValue))
		}
	case reflect.Array, reflect.Slice:
		for i := 0; i < v.Len(); i++ {
			widgets = append(widgets, setupInternals3(tab, strconv.Itoa(i), v.Index(i)))
		}
	case reflect.Ptr, reflect.Interface:
		widgets = append(widgets, setupInternals3(tab, "*", v.Elem()))
	}

	widgets = append(widgets, NewTextLabelWidgetString(np, "}"))

	return NewFlowLayoutWidget(np, widgets, &FlowLayoutWidgetOptions{FlowLayoutType: VerticalLayout})
}

func setupInternals3(pos mgl64.Vec2, titleString string, a reflect.Value) Widgeter {
	/*if !a.CanInterface() {
		a = UnsafeReflectValue(a)
	}*/

	var w Widgeter
	if a.Kind() == reflect.Float64 && a.Addr().CanInterface() {
		title := NewTextLabelWidgetString(np, titleString+": ")
		t := NewTest2Widget(np, a.Addr().Interface().(*float64))
		w = NewFlowLayoutWidget(pos, []Widgeter{title, t}, nil)
	} else if a.Kind() == reflect.String && a.Addr().CanInterface() {
		title := NewTextLabelWidgetString(np, titleString+": ")
		t := NewTextBoxWidgetExternalContent(np, NewMultilineContentPointer(a.Addr().Interface().(*string)), nil)
		w = NewFlowLayoutWidget(pos, []Widgeter{title, t}, nil)
	} else if a.Kind() == reflect.Bool && a.Addr().CanInterface() {
		title := NewTextLabelWidgetString(np, titleString+": ")
		t := NewTriButtonExternalStateWidget(np, func() bool { return *a.Addr().Interface().(*bool) }, func() { *a.Addr().Interface().(*bool) = !*a.Addr().Interface().(*bool) })
		t2 := NewTextLabelWidgetExternalContent(np, NewMultilineContentFuncInstant(func() string { return fmt.Sprint(*a.Addr().Interface().(*bool)) }))
		w = NewFlowLayoutWidget(pos, []Widgeter{title, t, t2}, nil)
	} else if vv := a; vv.CanAddr() {
		w = newGoonWidget(pos, titleString, vv.Addr())
	} else if vv := a; (vv.Kind() == reflect.Interface || vv.Kind() == reflect.Ptr) && vv.Elem().CanAddr() { // HACK
		w = newGoonWidget(pos, titleString, vv.Elem().Addr())
	} else {
		//w = NewTextLabelWidgetString(pos, goon.Sdump(vv))
		w = NewTextLabelWidgetString(pos, fmt.Sprintf("(%s)(can't addr... %s)", vv.Kind().String(), vv.String()))
	}

	spacer := NewCompositeWidget(np, []Widgeter{w})
	return spacer
}

// ---

func ExpandedLength(s string, currentAdvance uint32) (expandedLineLength uint32) {
	segments := strings.Split(s, "\t")
	var advance uint32 = currentAdvance
	for segmentIndex, segment := range segments {
		advance += uint32(len(segment))
		if segmentIndex != len(segments)-1 {
			advance += 4 - (advance % 4)
		}
	}
	return advance - currentAdvance
}

func ExpandedToLogical(s string, expanded uint32) uint32 {
	var logical uint32
	var advance uint32
	var smallestDifference int32 = int32(expanded) - 0
	for charIndex, char := range []byte(s) {
		if char == '\t' {
			advance += 4 - (advance % 4)
		} else {
			advance++
		}

		difference := int32(advance) - int32(expanded)
		if difference < 0 {
			difference *= -1
		}
		if difference < smallestDifference {
			smallestDifference = difference
			logical = uint32(charIndex + 1)
		}
	}

	return logical
}

// ---

// DOING: Turn current struct into higher-level CaretWithSelection; move each of caret and selection positions into sub-struct CaretPosition
// TODO: Rename
type caretPositionInternal struct {
	w                  MultilineContentI
	lineIndex          int
	positionWithinLine uint32
	targetExpandedX    uint32

	DepNode2Manual
}

func (cp *caretPositionInternal) NotifyContentChanged() {
	if cp.lineIndex > cp.w.LenLines()-1 {
		cp.Move(+3)
	} else if cp.positionWithinLine > cp.w.Line(cp.lineIndex).Length {
		cp.Move(+2)
	}
}

func (cp *caretPositionInternal) Logical() uint32 {
	return cp.w.Line(cp.lineIndex).Start + cp.positionWithinLine
}

// TODO: Change amount to a proper type with 2 values, etc. to avoid confusion with other funcs where amount can be an arbitrary number.
func (cp *caretPositionInternal) TryMoveH(amount int8, jumpWords bool) {
	switch amount {
	case -1:
		if cp.Logical() >= 1 {
			if jumpWords {
				// Skip spaces to the left
				LookAt := cp.Logical()
				for LookAt > 0 && !isCoreCharacter(cp.w.Content()[LookAt-1]) {
					LookAt--
				}
				// Skip non-spaces to the left
				for LookAt > 0 && isCoreCharacter(cp.w.Content()[LookAt-1]) {
					LookAt--
				}

				cp.willMoveH(int32(LookAt) - int32(cp.Logical()))
			} else {
				cp.willMoveH(-1)
			}
		}
	case +1:
		if cp.Logical() < uint32(cp.w.LenContent()) {
			if jumpWords {
				// Skip spaces to the right
				LookAt := cp.Logical()
				for LookAt < uint32(cp.w.LenContent()) && !isCoreCharacter(cp.w.Content()[LookAt]) {
					LookAt++
				}
				// Skip non-spaces to the right
				for LookAt < uint32(cp.w.LenContent()) && isCoreCharacter(cp.w.Content()[LookAt]) {
					LookAt++
				}

				cp.willMoveH(int32(LookAt) - int32(cp.Logical()))
			} else {
				cp.willMoveH(+1)
			}
		}
	}
}

// Moves caret horizontally by amount. It doesn't do bounds checking, so it's
// the caller's responsibility to ensure it's a legal amount to move by.
//
// Pre-conditions:
//	- Moving caret by amount should result in a valid position.
func (cp *caretPositionInternal) willMoveH(amount int32) {
	switch {
	case amount < 0:
		absAmount := uint32(-amount)
		for absAmount != 0 {
			if cp.positionWithinLine >= absAmount {
				cp.positionWithinLine -= absAmount
				absAmount = 0
			} else { //if cp.lineIndex > 0
				absAmount -= 1 + cp.positionWithinLine
				cp.lineIndex--
				cp.positionWithinLine = cp.w.Line(cp.lineIndex).Length
			}
		}
	case amount > 0:
		absAmount := uint32(amount)
		for absAmount != 0 {
			if cp.positionWithinLine+absAmount <= cp.w.Line(cp.lineIndex).Length {
				cp.positionWithinLine += absAmount
				absAmount = 0
			} else { //if cp.lineIndex < some_max
				absAmount -= 1 + cp.w.Line(cp.lineIndex).Length - cp.positionWithinLine
				cp.lineIndex++
				cp.positionWithinLine = 0
			}
		}
	default:
		// There's no change, so don't do anything else
		return
	}

	cp.targetExpandedX, _ = cp.ExpandedPosition() // TODO: More direct

	ExternallyUpdated(&cp.DepNode2Manual)
}

// TODO: Change amount to a proper type with 2 values, etc. to avoid confusion with other funcs where amount can be an arbitrary number.
func (cp *caretPositionInternal) TryMoveV(amount int8, jumpWords bool) {
	switch amount {
	case -1:
		if cp.lineIndex > 0 {
			if jumpWords {
				for cp.lineIndex > 0 {
					cp.lineIndex--
					line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start:cp.w.Line(cp.lineIndex).End()]
					if line == "" {
						break
					}
				}
				cp.positionWithinLine = 0
			} else {
				cp.lineIndex--
				line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start:cp.w.Line(cp.lineIndex).End()]
				cp.positionWithinLine = ExpandedToLogical(line, cp.targetExpandedX)
			}

			ExternallyUpdated(&cp.DepNode2Manual)
		} else {
			cp.Move(-2)
		}
	case +1:
		if cp.lineIndex < cp.w.LenLines()-1 {
			if jumpWords {
				for cp.lineIndex < cp.w.LenLines()-1 {
					cp.lineIndex++
					line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start:cp.w.Line(cp.lineIndex).End()]
					if line == "" {
						break
					}
				}
				cp.positionWithinLine = 0
			} else {
				cp.lineIndex++
				line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start:cp.w.Line(cp.lineIndex).End()]
				cp.positionWithinLine = ExpandedToLogical(line, cp.targetExpandedX)
			}

			ExternallyUpdated(&cp.DepNode2Manual)
		} else {
			cp.Move(+2)
		}
	}
}

func (cp *caretPositionInternal) MoveTo(other *caretPositionInternal) {
	cp.lineIndex = other.lineIndex
	cp.positionWithinLine = other.positionWithinLine
	cp.targetExpandedX = other.targetExpandedX

	ExternallyUpdated(&cp.DepNode2Manual)
}

func (cp *caretPositionInternal) Compare(other *caretPositionInternal) int8 {
	if cp.lineIndex < other.lineIndex {
		return -1
	} else if cp.lineIndex > other.lineIndex {
		return +1
	} else {
		if cp.positionWithinLine < other.positionWithinLine {
			return -1
		} else if cp.positionWithinLine > other.positionWithinLine {
			return +1
		} else {
			return 0
		}
	}
}

// TODO: Change amount to a proper type with 4 values, etc. to avoid confusion with other funcs where amount can be an arbitrary number.
// TOOD: Rename to JumpTo or something to indicate it's a method that can never fail.
func (cp *caretPositionInternal) Move(amount int8) {
	originalPosition := *cp

	switch amount {
	case -1, +1:
		panic("Move(+-1) deprecated, should use TryMoveH")
	case -2:
		cp.positionWithinLine = 0
	case +2:
		cp.positionWithinLine = cp.w.Line(cp.lineIndex).Length
	case -3:
		cp.lineIndex = 0
		cp.positionWithinLine = 0
	case +3:
		cp.lineIndex = cp.w.LenLines() - 1
		cp.positionWithinLine = cp.w.Line(cp.lineIndex).Length
	}

	if cp.Compare(&originalPosition) == 0 {
		// There's no change, so don't do anything else
		return
	}

	cp.targetExpandedX, _ = cp.ExpandedPosition() // TODO: More direct

	ExternallyUpdated(&cp.DepNode2Manual)
}

func (cp *caretPositionInternal) SetPositionFromPhysical(pos mgl64.Vec2) {
	if pos[1] < 0 {
		cp.lineIndex = 0
	} else if pos[1] >= float64(cp.w.LenLines()*fontHeight) {
		cp.lineIndex = cp.w.LenLines() - 1
	} else {
		cp.lineIndex = int(pos[1] / fontHeight)
	}

	if pos[0] < 0 {
		cp.targetExpandedX = 0
	} else {
		cp.targetExpandedX = uint32((pos[0] + fontWidth/2) / fontWidth)
	}

	line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start:cp.w.Line(cp.lineIndex).End()]
	cp.positionWithinLine = ExpandedToLogical(line, cp.targetExpandedX)

	ExternallyUpdated(&cp.DepNode2Manual)
}

func (cp *caretPositionInternal) TrySet(position uint32) {
	if position > uint32(cp.w.LenContent()) {
		position = uint32(cp.w.LenContent())
	}

	cp.lineIndex = sort.Search(cp.w.LenLines()-1, func(lineIndex int) bool {
		return cp.w.Line(lineIndex+1).Start > position
	})
	cp.positionWithinLine = position - cp.w.Line(cp.lineIndex).Start

	cp.targetExpandedX, _ = cp.ExpandedPosition() // TODO: More direct

	ExternallyUpdated(&cp.DepNode2Manual)
}

// ExpandedPosition returns logical character units.
// Multiply by (fontWidth, fontHeight) to get physical coords.
func (cp *caretPositionInternal) ExpandedPosition() (x uint32, y uint32) {
	expandedCaretPosition := ExpandedLength(cp.w.Content()[cp.w.Line(cp.lineIndex).Start:cp.w.Line(cp.lineIndex).Start+cp.positionWithinLine], 0)

	return expandedCaretPosition, uint32(cp.lineIndex)
}

// HACK
func (cp *caretPositionInternal) SetHint(caretPosition uint32, beginLineIndex int) (x uint32, y uint32) {
	caretPosition -= cp.w.Line(beginLineIndex).Start
	caretLine := beginLineIndex
	for caretPosition > cp.w.Line(caretLine).Length {
		caretPosition -= cp.w.Line(caretLine).Length + 1
		caretLine++
	}
	expandedCaretPosition := ExpandedLength(cp.w.Content()[cp.w.Line(caretLine).Start:cp.w.Line(caretLine).Start+caretPosition], 0)

	cp.targetExpandedX, y = expandedCaretPosition, uint32(caretLine)

	return cp.targetExpandedX, y
}

func (cp *caretPositionInternal) SaveState() *caretPositionInternal {
	return &caretPositionInternal{
		lineIndex:          cp.lineIndex,
		positionWithinLine: cp.positionWithinLine,
		targetExpandedX:    cp.targetExpandedX,
	}
}

func isCoreCharacter(character byte) bool {
	return (('a' <= character && character <= 'z') ||
		('A' <= character && character <= 'Z') ||
		('0' <= character && character <= '9') ||
		'_' == character)
}

// ---

type CaretPosition struct {
	w                 MultilineContentI
	caretPosition     *caretPositionInternal
	selectionPosition *caretPositionInternal

	DepNode2
}

func NewCaretPosition(mc MultilineContentI) *CaretPosition {
	cp := &CaretPosition{
		w:                 mc,
		caretPosition:     &caretPositionInternal{w: mc},
		selectionPosition: &caretPositionInternal{w: mc},
	}
	cp.AddSources(cp.caretPosition, cp.selectionPosition)
	return cp
}

func (cp *CaretPosition) Update() {}

// TODO: Should DepNode2 be used instead of this custom func?
func (cp *CaretPosition) NotifyContentChanged() {
	cp.caretPosition.NotifyContentChanged()
	cp.selectionPosition.NotifyContentChanged()
}

func (cp *CaretPosition) Logical() uint32 {
	return cp.caretPosition.Logical()
}

func (cp *CaretPosition) SelectionRange() (start uint32, end uint32) {
	min := intmath.MinUint32(cp.caretPosition.Logical(), cp.selectionPosition.Logical())
	max := intmath.MaxUint32(cp.caretPosition.Logical(), cp.selectionPosition.Logical())
	return min, max
}

func (cp *CaretPosition) SelectionRange2() (start, end *caretPositionInternal) {
	if cp.caretPosition.Compare(cp.selectionPosition) <= 0 {
		return cp.caretPosition, cp.selectionPosition
	} else {
		return cp.selectionPosition, cp.caretPosition
	}
}

func (cp *CaretPosition) anySelection() bool {
	return cp.caretPosition.Compare(cp.selectionPosition) != 0
}

// TODO: Change amount to a proper type with 2 values, etc. to avoid confusion with other funcs where amount can be an arbitrary number.
func (cp *CaretPosition) TryMoveH(amount int8, leaveSelection, jumpWords bool) {
	min, max := cp.SelectionRange2()

	switch amount {
	case -1:
		if cp.anySelection() && !leaveSelection {
			max.MoveTo(min)
		} else {
			if cp.caretPosition.Logical() >= 1 { // TODO: Where should this check happen
				cp.caretPosition.TryMoveH(amount, jumpWords)

				if !leaveSelection {
					cp.selectionPosition.MoveTo(cp.caretPosition)
				}
			}
		}
	case +1:
		if cp.anySelection() && !leaveSelection {
			min.MoveTo(max)
		} else {
			if cp.caretPosition.Logical() < uint32(cp.w.LenContent()) { // TODO: Where should this check happen
				cp.caretPosition.TryMoveH(amount, jumpWords)

				if !leaveSelection {
					cp.selectionPosition.MoveTo(cp.caretPosition)
				}
			}
		}
	}
}

// HACK: leaveSelection is currently an optional bool parameter
// TODO: Change amount to a proper type with 4 values, etc. to avoid confusion with other funcs where amount can be an arbitrary number.
// TOOD: Rename to JumpTo or something to indicate it's a method that can never fail.
func (cp *CaretPosition) Move(amount int8, leaveSelectionOptional ...bool) {
	// HACK, TODO: Make leaveSelection a required parameter?
	leaveSelection := len(leaveSelectionOptional) != 0 && leaveSelectionOptional[0]

	cp.caretPosition.Move(amount)

	if !leaveSelection {
		cp.selectionPosition.MoveTo(cp.caretPosition)
	}
}

func (cp *CaretPosition) MoveTo(target *caretPositionInternal) {
	cp.caretPosition.MoveTo(target)
	cp.selectionPosition.MoveTo(target)
}

// TODO: Change amount to a proper type with 2 values, etc. to avoid confusion with other funcs where amount can be an arbitrary number.
func (cp *CaretPosition) TryMoveV(amount int8, leaveSelection, jumpWords bool) {
	cp.caretPosition.TryMoveV(amount, jumpWords)

	if !leaveSelection {
		cp.selectionPosition.MoveTo(cp.caretPosition)
	}
}

func (cp *CaretPosition) SetPositionFromPhysical(pos mgl64.Vec2, leaveSelectionOptional ...bool) {
	// HACK, TODO: Make leaveSelection a required parameter?
	leaveSelection := len(leaveSelectionOptional) != 0 && leaveSelectionOptional[0]

	cp.caretPosition.SetPositionFromPhysical(pos)

	if !leaveSelection {
		cp.selectionPosition.MoveTo(cp.caretPosition)
	}
}

func (cp *CaretPosition) Backspace() {
	cp.CreateSelectionIfNone(-1)
	cp.ReplaceSelectionWith("")
}

func (cp *CaretPosition) SelectAll() {
	// TODO: Not move the view
	cp.Move(-3)
	cp.Move(+3, true)
}

func (cp *CaretPosition) CreateSelectionIfNone(amount int32) {
	if !cp.anySelection() {
		if (amount < 0 && cp.caretPosition.Logical() >= uint32(-amount)) ||
			(amount > 0 && cp.caretPosition.Logical()+uint32(amount) <= uint32(cp.w.LenContent())) {

			cp.caretPosition.willMoveH(amount)
		}
	}
}

func (cp *CaretPosition) CreateSelectionLineIfNone() {
	if !cp.anySelection() {
		cp.selectionPosition.Move(-2)
		cp.caretPosition.Move(+2)
		cp.caretPosition.TryMoveH(+1, false)
	}
}

// Replaces selection with string s and moves caret to end of s.
func (cp *CaretPosition) ReplaceSelectionWith(s string) {
	selStart, selEnd := cp.SelectionRange2()
	SetViewGroup(cp.w, cp.w.Content()[:selStart.Logical()]+s+cp.w.Content()[selEnd.Logical():])
	selStart.willMoveH(int32(len(s)))
	selEnd.MoveTo(selStart)
}

// Gets the selection content.
func (cp *CaretPosition) GetSelectionContent() (selectionContent string) {
	selStart, selEnd := cp.SelectionRange2()
	return cp.w.Content()[selStart.Logical():selEnd.Logical()]
}

// TODO: Change api to ask for an caretPositionInternal instance?
func (cp *CaretPosition) TrySet(position uint32, leaveSelectionOptional ...bool) {
	// HACK, TODO: Make leaveSelection a required parameter?
	leaveSelection := len(leaveSelectionOptional) != 0 && leaveSelectionOptional[0]

	cp.caretPosition.TrySet(position)

	if !leaveSelection {
		cp.selectionPosition.MoveTo(cp.caretPosition)
	}
}

func (cp *CaretPosition) SaveState() CaretPosition {
	return CaretPosition{
		caretPosition:     cp.caretPosition.SaveState(),
		selectionPosition: cp.selectionPosition.SaveState(),
	}
}

func (cp *CaretPosition) RestoreState(state CaretPosition) {
	// TODO: Add support for gracefully handling content changing since the time state was saved.
	cp.caretPosition.MoveTo(state.caretPosition)
	cp.selectionPosition.MoveTo(state.selectionPosition)
}

// ---

type MultilineContentI interface {
	Content() string
	LenContent() int
	LongestLine() uint32 // Line length

	Line(lineIndex int) contentLine
	LenLines() int

	ViewGroupI
}

func NewContentReader(c MultilineContentI) io.Reader { return strings.NewReader(c.Content()) }

// ---

type contentLine struct {
	Start  uint32
	Length uint32
}

func (this contentLine) End() uint32 {
	return this.Start + this.Length
}

type MultilineContent struct {
	content     string
	lines       []contentLine // TODO: Can be replaced by line starts only, calculate length in Line()
	longestLine uint32        // Line length

	ViewGroup
}

func NewMultilineContent() *MultilineContent {
	mc := &MultilineContent{}
	mc.InitViewGroup(mc, "memory://???")
	mc.updateLines()
	return mc
}
func NewMultilineContentString(content string) *MultilineContent {
	mc := &MultilineContent{}
	mc.InitViewGroup(mc, "memory://???")
	SetViewGroup(mc, content)
	return mc
}

func (c *MultilineContent) Content() string     { return c.content }
func (c *MultilineContent) LongestLine() uint32 { return c.longestLine }

func (c *MultilineContent) LenContent() int { return len(c.content) }

func (c *MultilineContent) Line(lineIndex int) contentLine {
	if lineIndex < 0 {
		return contentLine{0, 0}
	} else if lineIndex >= len(c.lines) {
		return contentLine{uint32(len(c.content)), 0}
	} else {
		return c.lines[lineIndex]
	}
}
func (c *MultilineContent) LenLines() int {
	return len(c.lines)
}

func (mc *MultilineContent) SetSelf(content string) {
	mc.content = content
	mc.updateLines()
}

func (w *MultilineContent) updateLines() {
	lines := GetLines(w.content)
	w.lines = make([]contentLine, len(lines))
	w.longestLine = 0
	for lineIndex, line := range lines {
		expandedLineLength := ExpandedLength(line, 0)
		if expandedLineLength > w.longestLine {
			w.longestLine = expandedLineLength
		}
		if lineIndex >= 1 {
			w.lines[lineIndex].Start = w.lines[lineIndex-1].Start + w.lines[lineIndex-1].Length + 1
		}
		w.lines[lineIndex].Length = uint32(len(line))
	}
}

// ---

type MultilineContentFile struct {
	*MultilineContent // TODO: Explore this being a pointer vs value
	path              string
	ViewGroup
}

func NewMultilineContentFile(path string) *MultilineContentFile {
	this := &MultilineContentFile{MultilineContent: NewMultilineContent(), path: path}
	absPath, err := filepath.Abs(path)
	CheckError(err)
	this.InitViewGroup(this, FileUri("file://"+absPath))
	this.AddAndSetViewGroup(this.MultilineContent, TryReadFile(this.path))
	UniversalClock.AddChangeListener(this)
	return this
}

func (this *MultilineContentFile) SetSelf(content string) {
	err := ioutil.WriteFile(this.path, []byte(content), 0666)
	CheckError(err)
}

func (this *MultilineContentFile) NotifyChange() {
	// Check if the file has been changed externally, and if so, override this widget
	NewContent := TryReadFile(this.path)
	if NewContent != this.Content() {
		SetViewGroupOther(this, NewContent)
	}
}

func (this *MultilineContentFile) Path() string {
	return this.path
}

// ---

type FileView struct {
	path string
	ViewGroup

	DepNode2Manual // File content changed.

	lastContentQUICKHACK string // HACK: Need this here for `git diff` testing
}

// TODO: Opening same path should result in same FileView, etc.
func NewFileView(path string) *FileView {
	this := &FileView{path: path}

	absPath, err := filepath.Abs(path)
	CheckError(err)

	this.lastContentQUICKHACK = TryReadFile(this.path)

	this.InitViewGroup(this, FileUri("file://"+absPath))
	UniversalClock.AddChangeListener(this) // TODO: Closing, etc.

	return this
}

// TODO, THINK: Should I be closing, or "stop keeping updating"? Or is it the same thing...
func (this *FileView) Close() error {
	UniversalClock.RemoveChangeListener(this)
	return nil
}

func (this *FileView) SetSelf(content string) {
	err := ioutil.WriteFile(this.path, []byte(content), 0666)
	CheckError(err)
	this.lastContentQUICKHACK = content
	ExternallyUpdated(&this.DepNode2Manual) // File content changed.
}

// TODO: Change detection, closing, etc.
func (this *FileView) NotifyChange() {
	// Check if the file has been changed externally, and if so, override this widget
	NewContent := TryReadFile(this.path)
	if NewContent != this.lastContentQUICKHACK {
		this.lastContentQUICKHACK = NewContent
		SetViewGroupOther(this, NewContent)
		ExternallyUpdated(&this.DepNode2Manual) // File content changed.
	}
}

// ---

type MultilineContentPointer struct {
	*MultilineContent
	p *string
	ViewGroup
}

func NewMultilineContentPointer(p *string) *MultilineContentPointer {
	this := &MultilineContentPointer{MultilineContent: NewMultilineContent(), p: p}
	this.InitViewGroup(this, "memory://???")
	this.AddAndSetViewGroup(this.MultilineContent, *p)
	UniversalClock.AddChangeListener(this) // TODO: Perhaps switch to a push notifications type setup, instead of constantly polling for change...
	return this
}

func (this *MultilineContentPointer) SetSelf(content string) {
	*this.p = content
}

func (this *MultilineContentPointer) NotifyChange() {
	// Check if the pointer value has been changed externally, and if so, override this widget
	NewContent := *this.p
	if NewContent != this.Content() {
		SetViewGroupOther(this, NewContent)
	}
}

// ---

type WebSocketView struct {
	c          *websocket.Conn
	WsReadChan chan string
	ViewGroup
}

func NewWebSocketView(c *websocket.Conn) *WebSocketView {
	this := &WebSocketView{c: c, WsReadChan: make(chan string)}
	this.InitViewGroup(this, FileUri(c.LocalAddr().String()))
	UniversalClock.AddChangeListener(this)
	return this
}

func (this *WebSocketView) SetSelf(content string) {
	// TODO: In case of multipart sends, follow with a null separator, have JS parse it out and reassemble parts
	io.WriteString(this.c, content)
}

func (this *WebSocketView) NotifyChange() {
	select {
	case NewContent, ok := <-this.WsReadChan:
		if ok {
			//if NewContent != this.Content() {
			SetViewGroupOther(this, NewContent)
			//}
		}
	default:
	}
}

// ---

// TEST: An unfinished experiment.
type ReverseMultilineContent struct {
	*MultilineContent
}

func NewReverseMultilineContent() *ReverseMultilineContent {
	rmc := &ReverseMultilineContent{&MultilineContent{}}
	rmc.InitViewGroup(rmc, "memory://???(reverse)")
	rmc.updateLines()
	return rmc
}

func (c *ReverseMultilineContent) Content() string { return Reverse(c.content) }

/*func (c *ReverseMultilineContent) SetSelf(content string) {
	c.content = Reverse(content)
	c.updateLines()
}*/

// ---

type lifeFormWidget struct {
	Widget

	Color uint8

	/*struct State
	{
		sint32				Health		= 100;		// [0, 200]
		sint32				Pain		= 0;		// [0, 100]
		sint32				Energy		= 1000;		// [0, 2000]
	};

	struct Output
	{
		sint32				Action		= -1;		// [-1, 8], move direction, where -1 means stop
	};

	Vector2d				m_PositionD;
	Vector2d				m_VelocityD;

	State					m_CurrentState;

	Output GenerateOutput();*/
}

func NewLifeFormWidget(pos mgl64.Vec2) *lifeFormWidget {
	w := &lifeFormWidget{Widget: NewWidget(pos, mgl64.Vec2{100, 100})}
	UniversalClock.AddChangeListener(w)
	return w
}

func (w *lifeFormWidget) Render() {
	Colors := []mgl64.Vec3{
		{0 / 255.0, 140 / 255.0, 0 / 255.0},
		{0 / 255.0, 98 / 255.0, 140 / 255.0},
		{194 / 255.0, 74 / 255.0, 0 / 255.0},
		{89 / 255.0, 0 / 255.0, 140 / 255.0},
		{191 / 255.0, 150 / 255.0, 0 / 255.0},
		{140 / 255.0, 0 / 255.0, 0 / 255.0},
	}

	DrawCircle(w.pos, w.size, mgl64.Vec3{0.3, 0.3, 0.3}, Colors[w.Color])

	// Render properties
	/*OpenGLStream OpenGLStream(GetPosition() + Vector2n(GetDimensions().X() / 2 + 5, GetDimensions().X() / -2));
	std::stringstream ss;
	ss << "Health: " << m_CurrentState.Health << '\n';
	ss << "Pain: " << m_CurrentState.Pain << '\n';
	ss << "Energy: " << m_CurrentState.Energy;
	OpenGLStream << ss.str();*/
}

func (w *lifeFormWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	if w.pos.Sub(ParentPosition).Len() <= w.size[0]/2 {
		return []Widgeter{w}
	} else {
		return nil
	}
}

func (w *lifeFormWidget) NotifyChange() {
	var timePassed float64 = UniversalClock.TimePassed

	m_PositionD := w.pos
	var m_VelocityD mgl64.Vec2

	// Instinct model
	{
		m_VelocityD = mgl64.Vec2{8, 5}

		const speedMultiplier = float64(250)

		ParentPosition := WidgeterS{w}.GlobalToParent(mgl64.Vec2{mousePointer.State.Axes[0], mousePointer.State.Axes[1]})
		LocalPosition := w.ParentToLocal(ParentPosition)

		if true && //PointerState.GetButtonState(0)
			len(w.Hit(ParentPosition)) > 0 {

			EscapeVector := LocalPosition
			EscapeAngle := math.Atan2(EscapeVector[0], EscapeVector[1])
			EscapeDirection := mgl64.Vec2{math.Sin(EscapeAngle), math.Cos(EscapeAngle)}.Mul(-1)

			m_VelocityD = EscapeDirection.Mul(speedMultiplier)
		}
	}

	// Thinking model
	/*{
		const auto QuarterPi = std::atan(1);

		auto Output = GenerateOutput();

		m_VelocityD = Vector2d(0, 0);

		if (   Output.Action >= 0
			&& Output.Action <  8)
		{
			m_VelocityD = Vector2d(std::cos(QuarterPi * Output.Action), std::sin(QuarterPi * Output.Action)) * 10;
		}
	}*/

	// Simulation
	{
		m_PositionD = m_PositionD.Add(m_VelocityD.Mul(timePassed))

		w.pos = m_PositionD
		redraw = true
	}
}

// ---

type MultilineContentFunc struct {
	*MultilineContent // TODO: Explore this being a pointer vs value
	contentFunc       func() string
	ViewGroup
}

// THINK: Merge the func and dependees into one struct? Maybe can't if funcs can have different signatures...
func NewMultilineContentFunc(contentFunc func() string, dependees []DepNodeI) *MultilineContentFunc {
	this := &MultilineContentFunc{MultilineContent: NewMultilineContent(), contentFunc: contentFunc}
	this.InitViewGroup(this, "func://???")
	this.AddAndSetViewGroup(this.MultilineContent, contentFunc())
	for _, dependee := range dependees {
		dependee.AddChangeListener(this)
	}
	return this
}

func (*MultilineContentFunc) SetSelf(string) {
	// TODO: Figure out if it's okay to effectively ignore this... or should I prevent it from being possible to call Set()?
	// Do nothing because the content of MultilineContentFunc can't be set as a string
}

func (this *MultilineContentFunc) NotifyChange() {
	NewContent := this.contentFunc()
	if NewContent != this.Content() {
		SetViewGroupOther(this, NewContent)
	}
}

// ---

type MultilineContentFuncInstant struct {
	*MultilineContentFunc
}

func NewMultilineContentFuncInstant(contentFunc func() string) *MultilineContentFuncInstant {
	return &MultilineContentFuncInstant{MultilineContentFunc: NewMultilineContentFunc(contentFunc, nil)}
}

// HACK: Because a func that calls Content(), Lines(), LongestLine() in some arbitrary order will get potentitally inconsistent results
func (this *MultilineContentFuncInstant) Content() string {
	this.MultilineContentFunc.NotifyChange()
	return this.MultilineContentFunc.Content()
}
func (this *MultilineContentFuncInstant) LongestLine() uint32 {
	this.MultilineContentFunc.NotifyChange()
	return this.MultilineContentFunc.LongestLine()
}
func (this *MultilineContentFuncInstant) LenContent() int {
	this.MultilineContentFunc.NotifyChange()
	return this.MultilineContentFunc.LenContent()
}
func (this *MultilineContentFuncInstant) Line(lineIndex int) contentLine {
	this.MultilineContentFunc.NotifyChange()
	return this.MultilineContentFunc.Line(lineIndex)
}

// ---

type TextLabelWidget struct {
	Widget
	Content        MultilineContentI
	tooltip        Widgeter
	layoutDepNode2 DepNode2Func
}

func NewTextLabelWidgetExternalContent(pos mgl64.Vec2, mc MultilineContentI) *TextLabelWidget {
	w := &TextLabelWidget{
		Widget:  NewWidget(pos, mgl64.Vec2{0, 0}),
		Content: mc,
	}
	w.layoutDepNode2.UpdateFunc = func(DepNode2I) { w.NotifyChange() }
	w.layoutDepNode2.AddSources(mc) // TODO: What about removing w when it's "deleted"?
	keepUpdatedTEST = append(keepUpdatedTEST, &w.layoutDepNode2)
	return w
}

func NewTextLabelWidgetString(pos mgl64.Vec2, s string) *TextLabelWidget {
	mc := NewMultilineContentString(s)
	w := NewTextLabelWidgetExternalContent(pos, mc)
	return w
}

func NewTextLabelWidgetGoon(pos mgl64.Vec2, any interface{}) *TextLabelWidget {
	mc := NewMultilineContentFuncInstant(func() string { return TrimLastNewline(goon.Sdump(any)) })
	return NewTextLabelWidgetExternalContent(pos, mc)
}

func NewTextLabelWidgetStringTooltip(pos mgl64.Vec2, s, tooltip string) *TextLabelWidget {
	mc := NewMultilineContentString(s)
	w := NewTextLabelWidgetExternalContent(pos, mc)
	w.tooltip = NewTextLabelWidgetString(np, tooltip)
	return w
}

func (w *TextLabelWidget) NotifyChange() {
	w.Layout()

	w.NotifyAllListeners()

	// TODO: Figure out if this should be here... is it a big deal if it gets called here AND elsewhere?
	redraw = true
}

func (w *TextLabelWidget) Layout() {
	if w.Content.LongestLine() < 3 {
		w.size[0] = float64(fontWidth * 3)
	} else {
		w.size[0] = float64(fontWidth * w.Content.LongestLine())
	}
	w.size[1] = float64(fontHeight * w.Content.LenLines())

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *TextLabelWidget) Render() {
	DrawLGBox(w.pos, w.size)

	gl.Color3d(0, 0, 0)
	for lineIndex := 0; lineIndex < w.Content.LenLines(); lineIndex++ {
		contentLine := w.Content.Line(lineIndex)
		PrintLine(mgl64.Vec2{w.pos[0], w.pos[1] + float64(fontHeight*lineIndex)}, w.Content.Content()[contentLine.Start:contentLine.Start+contentLine.Length])
	}

	isHit := len(w.HoverPointers()) > 0
	// Tooltip
	if w.tooltip != nil && isHit {
		mousePointerPositionLocal := WidgeterS{w}.GlobalToLocal(mgl64.Vec2{mousePointer.State.Axes[0], mousePointer.State.Axes[1]})
		w.tooltip.Layout()
		tooltipOffset := mgl64.Vec2{0, -fontHeight - w.tooltip.Size()[1]}
		*w.tooltip.Pos() = w.pos.Add(mousePointerPositionLocal).Add(tooltipOffset)
		w.tooltip.Render()
	}
}

// ---

type DepStringerI interface {
	fmt.Stringer
	DepNode2I
}

type DepStringerFunc struct {
	content string
	DepNode2Func
}

func (this *DepStringerFunc) String() string {
	return this.content
}

// ---

type StringerWidget struct {
	Widget
	content        DepStringerI
	layoutDepNode2 DepNode2Func
}

func NewStringerWidget(pos mgl64.Vec2, content DepStringerI) *StringerWidget {
	w := &StringerWidget{
		Widget:  NewWidget(pos, mgl64.Vec2{0, 0}),
		content: content,
	}
	w.layoutDepNode2.UpdateFunc = func(DepNode2I) { w.Layout() }
	w.layoutDepNode2.AddSources(content) // TODO: What about removing w when it's "deleted"?
	return w
}

func (w *StringerWidget) Layout() {
	// TODO: Multiline support?
	longestLine := len(w.content.String())
	if longestLine < 3 {
		w.size[0] = float64(fontWidth * 3)
	} else {
		w.size[0] = float64(fontWidth * longestLine)
	}
	w.size[1] = float64(fontHeight * 1)

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *StringerWidget) LayoutNeeded() {
	MakeUpdated(&w.layoutDepNode2)
}

func (w *StringerWidget) Render() {
	DrawLGBox(w.pos, w.size)

	gl.Color3d(0, 0, 0)
	// TODO: Multiline support?
	PrintLine(w.pos, w.content.String())
}

// ---

type TextStyle struct {
	TextColor       *mgl64.Vec3
	BorderColor     **mgl64.Vec3
	BackgroundColor **mgl64.Vec3
}

func (textStyle *TextStyle) Apply(glt *OpenGlStream) {
	if textStyle == nil {
		return
	}

	if textStyle.TextColor != nil {
		textColor := *textStyle.TextColor
		gl.Color3dv((*float64)(&textColor[0]))
	}
	if textStyle.BorderColor != nil {
		borderColor := *textStyle.BorderColor
		glt.BorderColor = borderColor
	}
	if textStyle.BackgroundColor != nil {
		backgroundColor := *textStyle.BackgroundColor
		glt.BackgroundColor = backgroundColor
	}
}

// ---

type Highlighter interface {
	NewIterator(offset uint32) HighlighterIterator

	DepNode2I
}

type HighlighterIterator interface {
	Next() uint32
	Current() *TextStyle
	Advance(uint32) *TextStyle
}

// ---

type segmentHighlighter interface {
	Segment(index uint32) highlightSegment
	LenSegments() int

	SegmentToTextStyle(index uint32) *TextStyle
}

type highlighterIterator struct {
	hl     segmentHighlighter
	offset uint32
	index  uint32
}

func NewHighlighterIterator(hl segmentHighlighter, offset uint32) *highlighterIterator {
	return &highlighterIterator{
		hl:     hl,
		offset: offset,
		// Binary search for the first entry that ends past the beginning of visible text
		index: uint32(sort.Search(hl.LenSegments()-1, func(i int) bool {
			return hl.Segment(uint32(i)+1).offset > offset
		})),
	}
}

func (this *highlighterIterator) Next() uint32 {
	return this.hl.Segment(this.index+1).offset - this.offset
}

func (this *highlighterIterator) Current() *TextStyle {
	return this.hl.SegmentToTextStyle(this.index)
}

func (this *highlighterIterator) Advance(span uint32) *TextStyle {
	if span < this.Next() {
		this.offset += span
		return nil
	} else {
		this.offset += span
		this.index++
		return this.Current()
	}
}

// ---

type selectionHighlighterIterator struct {
	offset         uint32
	min, max       uint32
	hasTypingFocus bool
}

func NewSelectionHighlighterIterator(offset, min, max uint32, hasTypingFocus bool) *selectionHighlighterIterator {
	return &selectionHighlighterIterator{
		offset:         offset,
		min:            min,
		max:            max,
		hasTypingFocus: hasTypingFocus,
	}
}

func (this *selectionHighlighterIterator) Next() uint32 {
	if this.min == this.max { // HACK
		return 500000000 // TODO, HACK
	} else if this.offset < this.min {
		return this.min - this.offset
	} else if this.offset < this.max {
		return this.max - this.offset
	} else {
		return 500000000 // TODO, HACK
	}
}

func (this *selectionHighlighterIterator) Current() *TextStyle {
	if this.min == this.max { // HACK: Return nil if no selection at all
		return nil
	}

	borderColor := &selectedTextDarkColor
	color := &selectedTextColor
	if !this.hasTypingFocus {
		borderColor = &selectedTextColor
		color = &selectedTextInactiveColor
	}
	if this.offset < this.min || this.offset >= this.max {
		borderColor = nil
		color = nil
	}
	return &TextStyle{
		BorderColor:     &borderColor,
		BackgroundColor: &color,
	}
}

func (this *selectionHighlighterIterator) Advance(span uint32) *TextStyle {
	if span < this.Next() {
		this.offset += span
		return nil
	} else {
		this.offset += span
		return this.Current()
	}
}

// ---

type highlightSegment struct {
	offset uint32
	color  mgl64.Vec3
}

type highlightedGoContent struct {
	segments []highlightSegment

	DepNode2
}

func (this *highlightedGoContent) NewIterator(offset uint32) HighlighterIterator {
	return NewHighlighterIterator(this, offset)
}

func (this *highlightedGoContent) Update() {
	content := this.GetSources()[0].(MultilineContentI)

	this.segments = nil

	src := []byte(content.Content())
	//src := []byte(w.Content.Content()[w.Content.Line(beginLineIndex).Start:w.Content.Line(endLineIndex).Start])

	var s scanner.Scanner
	fset := token.NewFileSet()
	file := fset.AddFile("", fset.Base(), len(src))
	s.Init(file, src, nil, scanner.ScanComments)

	// Repeated calls to Scan yield the token sequence found in the input.
	for {
		pos, tok, lit := s.Scan()
		if tok == token.EOF {
			break
		}

		offset := uint32(fset.Position(pos).Offset)

		switch {
		case tok.IsKeyword() || tok.IsOperator() && tok < token.LPAREN:
			this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0.004, 0, 0.714}})
		case tok.IsLiteral() && tok != token.IDENT:
			this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0.804, 0, 0}})
		case lit == "false" || lit == "true":
			this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0.008, 0.024, 1}})
		case tok == token.COMMENT:
			this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0, 0.506, 0.094}})
		default:
			this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0, 0, 0}})
		}
	}

	// HACK: Fake last element.
	this.segments = append(this.segments, highlightSegment{offset: uint32(content.LenContent())})
}

func (this *highlightedGoContent) Segment(index uint32) highlightSegment {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return highlightSegment{offset: 0}
	} else if index >= uint32(len(this.segments)) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return highlightSegment{offset: uint32(this.segments[len(this.segments)-1].offset)}
	} else {
		return this.segments[index]
	}
}
func (this *highlightedGoContent) LenSegments() int {
	return len(this.segments)
}
func (this *highlightedGoContent) SegmentToTextStyle(index uint32) *TextStyle {
	color := this.Segment(index).color
	return &TextStyle{
		TextColor: &color,
	}
}

// ---

func highlightedDiffFunc(leftContent, rightContent string, segments *[2][]highlightSegment, offsets [2]uint32) {
	dmp := diffmatchpatch.New()
	diffs := dmp.DiffMain(leftContent, rightContent, true)

	for side := range *segments {
		offset := offsets[side]

		for _, diff := range diffs {
			if side == 0 && diff.Type == -1 {
				(*segments)[side] = append((*segments)[side], highlightSegment{offset: offset, color: darkRedColor})
				offset += uint32(len(diff.Text))
			}
			if side == 1 && diff.Type == +1 {
				(*segments)[side] = append((*segments)[side], highlightSegment{offset: offset, color: darkGreenColor})
				offset += uint32(len(diff.Text))
			}
			if diff.Type == 0 {
				(*segments)[side] = append((*segments)[side], highlightSegment{offset: offset})
				offset += uint32(len(diff.Text))
			}
		}
	}
}

type highlightedDiffSide struct {
	side int
	*highlightedDiff
}

func (this *highlightedDiffSide) NewIterator(offset uint32) HighlighterIterator {
	return NewHighlighterIterator(this, offset)
}
func (this *highlightedDiffSide) Segment(index uint32) highlightSegment {
	return this.highlightedDiff.segment(index, this.side)
}
func (this *highlightedDiffSide) LenSegments() int {
	return this.highlightedDiff.lenSegments(this.side)
}
func (this *highlightedDiffSide) SegmentToTextStyle(index uint32) *TextStyle {
	return this.highlightedDiff.segmentToTextStyle(index, this.side)
}

type highlightedDiff struct {
	segments [2][]highlightSegment

	DepNode2
}

func (this *highlightedDiff) Update() {
	left := this.GetSources()[0].(MultilineContentI)
	right := this.GetSources()[1].(MultilineContentI)

	dmp := diffmatchpatch.New()
	diffs := dmp.DiffMain(left.Content(), right.Content(), true)

	for side := range this.segments {
		this.segments[side] = nil

		offset := uint32(0)

		for _, diff := range diffs {
			if side == 0 && diff.Type == -1 {
				this.segments[side] = append(this.segments[side], highlightSegment{offset: offset, color: mediumRedColor})
				offset += uint32(len(diff.Text))
			}
			if side == 1 && diff.Type == +1 {
				this.segments[side] = append(this.segments[side], highlightSegment{offset: offset, color: mediumGreenColor})
				offset += uint32(len(diff.Text))
			}
			if diff.Type == 0 {
				this.segments[side] = append(this.segments[side], highlightSegment{offset: offset})
				offset += uint32(len(diff.Text))
			}
		}

		// HACK: Fake last element.
		if side == 0 {
			this.segments[side] = append(this.segments[side], highlightSegment{offset: uint32(left.LenContent())})
		} else {
			this.segments[side] = append(this.segments[side], highlightSegment{offset: uint32(right.LenContent())})
		}
	}
}

func (this *highlightedDiff) segment(index uint32, side int) highlightSegment {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return highlightSegment{offset: 0}
	} else if index >= uint32(len(this.segments[side])) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return highlightSegment{offset: uint32(this.segments[side][len(this.segments[side])-1].offset)}
	} else {
		return this.segments[side][index]
	}
}
func (this *highlightedDiff) lenSegments(side int) int {
	return len(this.segments[side])
}
func (this *highlightedDiff) segmentToTextStyle(index uint32, side int) *TextStyle {
	color := this.segment(index, side).color
	colorPtr := &color
	if color.ApproxEqual(mgl64.Vec3{}) {
		colorPtr = nil
	}
	return &TextStyle{
		BackgroundColor: &colorPtr,
	}
}

// ---

type tokLit struct {
	offset uint32
	tok    token.Token
	lit    string
}

type tokenizedGoContent struct {
	segments []tokLit

	DepNode2
}

func (this *tokenizedGoContent) Update() {
	content := this.GetSources()[0].(MultilineContentI)

	this.segments = nil

	src := []byte(content.Content())
	//src := []byte(w.Content.Content()[w.Content.Line(beginLineIndex).Start:w.Content.Line(endLineIndex).Start])

	var s scanner.Scanner
	fset := token.NewFileSet()
	file := fset.AddFile("", fset.Base(), len(src))
	s.Init(file, src, nil, scanner.ScanComments)

	// Repeated calls to Scan yield the token sequence found in the input.
	// TODO: Perhaps include whitespace in between tokens?
	for {
		pos, tok, lit := s.Scan()
		if tok == token.EOF {
			break
		}

		offset := uint32(fset.Position(pos).Offset)

		this.segments = append(this.segments, tokLit{offset: offset, tok: tok, lit: lit})
	}

	// HACK: Fake last element.
	this.segments = append(this.segments, tokLit{offset: uint32(content.LenContent())})
}

func (this *tokenizedGoContent) Segment(index uint32) tokLit {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return tokLit{offset: 0}
	} else if index >= uint32(len(this.segments)) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return tokLit{offset: uint32(this.segments[len(this.segments)-1].offset)}
	} else {
		return this.segments[index]
	}
}
func (this *tokenizedGoContent) LenSegments() int {
	return len(this.segments)
}

// ---

type tokenizedDiffHelper struct {
	left  *tokenizedGoContent
	right *tokenizedGoContent
}

func (this *tokenizedDiffHelper) Equal(i, j int) bool {
	return this.left.Segment(uint32(i)).tok == this.right.Segment(uint32(j)).tok &&
		this.left.Segment(uint32(i)).lit == this.right.Segment(uint32(j)).lit
}

type tokenizedDiff struct {
	leftSide bool
	segments []highlightSegment

	DepNode2
}

func (this *tokenizedDiff) NewIterator(offset uint32) HighlighterIterator {
	return NewHighlighterIterator(this, offset)
}

func (this *tokenizedDiff) Update() {
	left := this.GetSources()[0].(*tokenizedGoContent)
	right := this.GetSources()[1].(*tokenizedGoContent)

	this.segments = nil

	dmp := tokenizedDiffHelper{left: left, right: right}
	diffs := diff.Diff(left.LenSegments(), right.LenSegments(), &dmp)

	// HACK: Fake first element.
	this.segments = append(this.segments, highlightSegment{offset: 0})

	for _, diff := range diffs {
		if !this.leftSide && diff.Ins > 0 {
			beginOffset := right.Segment(uint32(diff.B)).offset
			endOffset := right.Segment(uint32(diff.B + diff.Ins)).offset
			this.segments = append(this.segments, highlightSegment{offset: beginOffset, color: darkGreenColor})
			this.segments = append(this.segments, highlightSegment{offset: endOffset})
		} else if this.leftSide && diff.Del > 0 {
			beginOffset := left.Segment(uint32(diff.A)).offset
			endOffset := left.Segment(uint32(diff.A + diff.Del)).offset
			this.segments = append(this.segments, highlightSegment{offset: beginOffset, color: darkRedColor})
			this.segments = append(this.segments, highlightSegment{offset: endOffset})
		}
	}

	// HACK: Fake last element.
	this.segments = append(this.segments, highlightSegment{offset: uint32(500000000)}) // TODO, HACK
}

func (this *tokenizedDiff) Segment(index uint32) highlightSegment {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return highlightSegment{offset: 0}
	} else if index >= uint32(len(this.segments)) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return highlightSegment{offset: uint32(this.segments[len(this.segments)-1].offset)}
	} else {
		return this.segments[index]
	}
}
func (this *tokenizedDiff) LenSegments() int {
	return len(this.segments)
}
func (this *tokenizedDiff) SegmentToTextStyle(index uint32) *TextStyle {
	color := this.Segment(index).color
	colorPtr := &color
	if color.ApproxEqual(mgl64.Vec3{}) {
		colorPtr = nil
	}
	return &TextStyle{
		BackgroundColor: &colorPtr,
	}
}

// ---

type lineDiffHelper struct {
	left  MultilineContentI
	right MultilineContentI
}

func (this *lineDiffHelper) Equal(i, j int) bool {
	line1 := this.left.Content()[this.left.Line(i).Start:this.left.Line(i).End()]
	line2 := this.right.Content()[this.right.Line(j).Start:this.right.Line(j).End()]
	return line1 == line2
}

// Line-based differ.
type lineDiffSide struct {
	side int
	*lineDiff
}

func (this *lineDiffSide) NewIterator(offset uint32) HighlighterIterator {
	return NewHighlighterIterator(this, offset)
}
func (this *lineDiffSide) Segment(index uint32) highlightSegment {
	return this.lineDiff.segment(index, this.side)
}
func (this *lineDiffSide) LenSegments() int {
	return this.lineDiff.lenSegments(this.side)
}
func (this *lineDiffSide) SegmentToTextStyle(index uint32) *TextStyle {
	return this.lineDiff.segmentToTextStyle(index, this.side)
}

func (this *lineDiffSide) LineBackgroundColor(lineIndex int) (BackgroundColor *mgl64.Vec3) {
	if this.lineDiff.lines[this.side][lineIndex] {
		switch this.side {
		case 0:
			return &lightRedColor
		case 1:
			return &lightGreenColor
		}
	}
	return nil
}

type lineDiff struct {
	segments [2][]highlightSegment
	lines    [2][]bool // For LineBackgroundColor, true indicates the line with that index was modified (deleted or inserted).

	DepNode2
}

func (this *lineDiff) Update() {
	left := this.GetSources()[0].(MultilineContentI)
	right := this.GetSources()[1].(MultilineContentI)

	dmp := lineDiffHelper{left: left, right: right}
	diffs := diff.Diff(left.LenLines(), right.LenLines(), &dmp)

	for side := range this.segments {
		this.segments[side] = nil

		// HACK: Fake first element.
		this.segments[side] = append(this.segments[side], highlightSegment{offset: 0})
	}
	this.lines[0] = make([]bool, left.LenLines())
	this.lines[1] = make([]bool, right.LenLines())

	for _, diff := range diffs {
		if diff.Del > 0 || diff.Ins > 0 {
			beginOffsetLeft := left.Line(diff.A).Start
			endOffsetLeft := left.Line(diff.A + diff.Del).Start
			beginOffsetRight := right.Line(diff.B).Start
			endOffsetRight := right.Line(diff.B + diff.Ins).Start

			for line := diff.A; line < diff.A+diff.Del; line++ {
				this.lines[0][line] = true
			}
			for line := diff.B; line < diff.B+diff.Ins; line++ {
				this.lines[1][line] = true
			}

			leftContent := left.Content()[beginOffsetLeft:endOffsetLeft]
			rightContent := right.Content()[beginOffsetRight:endOffsetRight]

			highlightedDiffFunc(leftContent, rightContent, &this.segments, [2]uint32{beginOffsetLeft, beginOffsetRight})

			this.segments[0] = append(this.segments[0], highlightSegment{offset: endOffsetLeft})
			this.segments[1] = append(this.segments[1], highlightSegment{offset: endOffsetRight})
		} /* else {
			for side := range this.segments {
				if side == 0 && diff.Del > 0 {
					beginOffset := left.Line(diff.A).Start
					endOffset := left.Line(diff.A + diff.Del).Start

					this.segments[side] = append(this.segments[side], highlightSegment{offset: beginOffset, color: darkRedColor})
					this.segments[side] = append(this.segments[side], highlightSegment{offset: endOffset})
				}
				if side == 1 && diff.Ins > 0 {
					beginOffset := right.Line(diff.B).Start
					endOffset := right.Line(diff.B + diff.Ins).Start

					this.segments[side] = append(this.segments[side], highlightSegment{offset: beginOffset, color: darkGreenColor})
					this.segments[side] = append(this.segments[side], highlightSegment{offset: endOffset})
				}
			}
		}*/
	}

	for side := range this.segments {
		// HACK: Fake last element.
		this.segments[side] = append(this.segments[side], highlightSegment{offset: uint32(500000000)}) // TODO, HACK
	}
}

func (this *lineDiff) segment(index uint32, side int) highlightSegment {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return highlightSegment{offset: 0}
	} else if index >= uint32(len(this.segments[side])) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return highlightSegment{offset: uint32(this.segments[side][len(this.segments[side])-1].offset)}
	} else {
		return this.segments[side][index]
	}
}
func (this *lineDiff) lenSegments(side int) int {
	return len(this.segments[side])
}
func (this *lineDiff) segmentToTextStyle(index uint32, side int) *TextStyle {
	color := this.segment(index, side).color
	colorPtr := &color
	if color.ApproxEqual(mgl64.Vec3{}) {
		colorPtr = nil
	}
	return &TextStyle{
		BackgroundColor: &colorPtr,
	}
}

// ---

type LineHighlighter interface {
	LineBackgroundColor(lineIndex int) (BackgroundColor *mgl64.Vec3)

	DepNode2I
}

type diffHighlighter struct {
	segments []highlightSegment

	DepNode2
}

func (this *diffHighlighter) Update() {
	content := this.GetSources()[0].(MultilineContentI)

	this.segments = nil

	// HACK: Fake first element.
	this.segments = append(this.segments, highlightSegment{offset: 0})

	lastDel, lastIns := -1, -1
	for lineIndex := 0; lineIndex < content.LenLines(); lineIndex++ {
		var lineFirstChar byte
		if content.Line(lineIndex).Length > 0 {
			lineFirstChar = content.Content()[content.Line(lineIndex).Start]
		}
		switch lineFirstChar {
		case '+':
			if lastIns == -1 {
				lastIns = lineIndex
			}
		case '-':
			if lastDel == -1 {
				lastDel = lineIndex
			}
		default:
			if lastDel != -1 || lastIns != -1 {
				if lastDel == -1 {
					lastDel = lastIns
				} else if lastIns == -1 {
					lastIns = lineIndex
				}

				beginOffsetLeft := content.Line(lastDel).Start
				endOffsetLeft := content.Line(lastIns).Start
				beginOffsetRight := content.Line(lastIns).Start
				endOffsetRight := content.Line(lineIndex).Start

				// This is needed to filter out the "-" and "+" at the beginning of each line from being highlighted.
				// TODO: Still not completely filtered out.
				leftContent := ""
				for line := lastDel; line < lastIns; line++ {
					leftContent += "\x00" + content.Content()[content.Line(line).Start+1:content.Line(line).End()] + "\n"
				}
				rightContent := ""
				for line := lastIns; line < lineIndex; line++ {
					rightContent += "\x00" + content.Content()[content.Line(line).Start+1:content.Line(line).End()] + "\n"
				}

				var sectionSegments [2][]highlightSegment
				highlightedDiffFunc(leftContent, rightContent, &sectionSegments, [2]uint32{beginOffsetLeft, beginOffsetRight})

				sectionSegments[0] = append(sectionSegments[0], highlightSegment{offset: endOffsetLeft})
				sectionSegments[1] = append(sectionSegments[1], highlightSegment{offset: endOffsetRight})

				this.segments = append(this.segments, sectionSegments[0]...)
				this.segments = append(this.segments, sectionSegments[1]...)
			}
			lastDel, lastIns = -1, -1
		}
	}

	// HACK: Fake last element.
	this.segments = append(this.segments, highlightSegment{offset: uint32(500000000)}) // TODO, HACK
}

func (this *diffHighlighter) NewIterator(offset uint32) HighlighterIterator {
	return NewHighlighterIterator(this, offset)
}
func (this *diffHighlighter) Segment(index uint32) highlightSegment {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return highlightSegment{offset: 0}
	} else if index >= uint32(len(this.segments)) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return highlightSegment{offset: uint32(this.segments[len(this.segments)-1].offset)}
	} else {
		return this.segments[index]
	}
}
func (this *diffHighlighter) LenSegments() int {
	return len(this.segments)
}
func (this *diffHighlighter) SegmentToTextStyle(index uint32) *TextStyle {
	color := this.Segment(index).color
	colorPtr := &color
	if color.ApproxEqual(mgl64.Vec3{}) {
		colorPtr = nil
	}
	return &TextStyle{
		BackgroundColor: &colorPtr,
	}
}

func (this *diffHighlighter) LineBackgroundColor(lineIndex int) (BackgroundColor *mgl64.Vec3) {
	content := this.GetSources()[0].(MultilineContentI)

	if content.Line(lineIndex).Length > 0 {
		switch lineFirstChar := content.Content()[content.Line(lineIndex).Start]; lineFirstChar {
		case '+':
			return &lightGreenColor
		case '-':
			return &lightRedColor
		}
	}
	return nil
}

// ---

type FindPanel struct {
	FindBox      *TextBoxWidget
	OriginalView TextBoxScrollPaneView // View as it was when FindPanel was opened.

	Widgeter
}

func NewFindPanel(pos mgl64.Vec2, findResults *FindResults, caretPosition *CaretPosition) *FindPanel {
	findBox := NewTextBoxWidgetOptions(np, TextBoxWidgetOptions{SingleLine: true})

	numResultsStringer := &DepStringerFunc{}
	numResultsStringer.UpdateFunc = func(this DepNode2I) {
		findResults := this.GetSources()[0].(*FindResults)
		caretPosition := this.GetSources()[1].(*CaretPosition)

		numResultsStringer.content = ""
		if index, match := findResults.MatchesResult(caretPosition.SelectionRange()); match {
			numResultsStringer.content += fmt.Sprintf("%d of ", index+1)
		}
		numResultsStringer.content += fmt.Sprintf("%d matches", findResults.NumResults())
	}
	numResultsStringer.AddSources(findResults, caretPosition)

	findPanelWidget := NewFlowLayoutWidget(pos, []Widgeter{
		NewSpacerWidget(np, NewTextLabelWidgetString(np, "Find:")),
		NewSpacerWidget(np, findBox),
		NewSpacerWidget(np, NewStringerWidget(np, numResultsStringer)),
		NewSpacerWidget(np, NewButtonLabelWidget(np, "Next", nil)),
		NewSpacerWidget(np, NewButtonLabelWidget(np, "Previous", nil)),
	}, nil)

	return &FindPanel{
		FindBox:  findBox,
		Widgeter: findPanelWidget,
	}
}

func (this *FindPanel) SetKeyboardFocus() {
	// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
	keyboardPointer.OriginMapping = []Widgeter{this.FindBox}
}

// ---

type FindResults struct {
	Owner *TextBoxWidget

	segments []highlightSegment

	DepNode2
}

func (this *FindResults) Update() {
	content := this.GetSources()[0].(MultilineContentI).Content()
	var findTarget string
	if findBox, ok := this.GetSources()[1].(MultilineContentI); ok {
		findTarget = findBox.Content()
	} else if _, ok := this.GetSources()[1].(*WholeWordHighlighter); ok {
		// If the find panel is not visible, but a whole word is selected, use it as the find target instead.
		findTarget = this.Owner.caretPosition.GetSelectionContent()
	}

	this.segments = nil

	// HACK: Fake first element.
	this.segments = append(this.segments, highlightSegment{offset: 0})

	if findTarget != "" {
		var offset uint32
		nonresults := strings.Split(content, findTarget)
		if len(nonresults) > 1 {
			for _, nonresult := range nonresults[:len(nonresults)-1] {
				offset += uint32(len(nonresult))
				this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{1, 1, 1}})
				offset += uint32(len(findTarget))
				this.segments = append(this.segments, highlightSegment{offset: offset, color: mgl64.Vec3{0, 0, 0}})
			}
		}
	}

	// HACK: Fake last element.
	this.segments = append(this.segments, highlightSegment{offset: uint32(len(content))})

	// TODO: Is this the best place to do this? Shouldn't Update() not have event side-effects?
	// If find panel is visible, update the selection.
	if this.Owner.isFindPanelVisible() {
		this.Owner.caretPosition.RestoreState(this.Owner.findPanel.OriginalView.caretPosition) // Move cursor but not view, so it doesn't jump.
		selStart, selEnd := this.Owner.caretPosition.SelectionRange()
		_ = selEnd // TODO: Verify if this the right way to not skip first selection...
		if start, end, ok := this.GetResult(selStart, selStart, +1); ok {
			this.Owner.caretPosition.TrySet(start)
			this.Owner.caretPosition.caretPosition.willMoveH(int32(end - start))
			this.Owner.CenterOnCaretPositionIfOffscreen()
		} else {
			this.Owner.RestoreView(this.Owner.findPanel.OriginalView)
		}
	}
}

// NumResults returns current number of find results.
func (this *FindResults) NumResults() int {
	if len(this.segments) <= 2 {
		return 0
	}
	return (len(this.segments) - 2) / 2
}

// MatchesResult returns true and index of result matched to the given start and end content index.
func (this *FindResults) MatchesResult(start uint32, end uint32) (index int, match bool) {
	// TODO: Should use binary search to optimize.
	for ; index < this.NumResults(); index++ {
		if start == this.segments[1+2*index].offset && end == this.segments[1+2*index+1].offset {
			return index, true
		}
	}
	return index, false
}

// TODO: Use a named const for direction or something to indicate it can only be +1 or -1.
func (this *FindResults) GetResult(start uint32, end uint32, direction int) (resultStart uint32, resultEnd uint32, ok bool) {
	if this.NumResults() == 0 {
		return 0, 0, false
	}

	switch direction {
	case +1:
		resultStart, resultEnd = this.nextResult(end)
		return resultStart, resultEnd, true
	case -1:
		resultStart, resultEnd = this.prevResult(start)
		return resultStart, resultEnd, true
	default:
		panic(0)
	}
}

// TODO: Dedup with prevResult.
// Pre-condition: this.NumResults() >= 1.
func (this *FindResults) nextResult(end uint32) (resultStart uint32, resultEnd uint32) {
	// TODO: Should use binary search to optimize.
	for index := 0; index < this.NumResults(); index++ {
		if end <= this.segments[1+2*index].offset {
			return this.segments[1+2*index].offset, this.segments[1+2*index+1].offset
		}
	}
	index := 0
	return this.segments[1+2*index].offset, this.segments[1+2*index+1].offset
}
func (this *FindResults) prevResult(start uint32) (resultStart uint32, resultEnd uint32) {
	// TODO: Should use binary search to optimize.
	for index := this.NumResults() - 1; index >= 0; index-- {
		if this.segments[1+2*index+1].offset <= start {
			return this.segments[1+2*index].offset, this.segments[1+2*index+1].offset
		}
	}
	index := this.NumResults() - 1
	return this.segments[1+2*index].offset, this.segments[1+2*index+1].offset
}

func (this *FindResults) NewIterator(offset uint32) HighlighterIterator {
	return NewHighlighterIterator(this, offset)
}

func (this *FindResults) Segment(index uint32) highlightSegment {
	if index < 0 {
		//fmt.Println("warning: Segment < 0")
		return highlightSegment{offset: 0}
	} else if index >= uint32(len(this.segments)) {
		//fmt.Println("warning: Segment index >= max") // TODO: Fix this.
		return highlightSegment{offset: uint32(this.segments[len(this.segments)-1].offset)}
	} else {
		return this.segments[index]
	}
}
func (this *FindResults) LenSegments() int {
	return len(this.segments)
}
func (this *FindResults) SegmentToTextStyle(index uint32) *TextStyle {
	highlight := this.Segment(index).color[0] != 0

	borderColor := &darkColor
	if !highlight {
		borderColor = nil
	}
	return &TextStyle{
		BorderColor: &borderColor,
	}
}

// ---

type WholeWordHighlighter struct {
	wholeWord bool

	DepNode2
}

func (this *WholeWordHighlighter) Update() {
	content := this.GetSources()[0].(MultilineContentI)
	caretPosition := this.GetSources()[1].(*CaretPosition)

	this.wholeWord = false

	if !caretPosition.anySelection() {
		return
	}

	start, end := caretPosition.SelectionRange2()

	// Figure out if the selection is a whole word.
	x := &caretPositionInternal{w: content}
	y := &caretPositionInternal{w: content}
	x.MoveTo(start)
	y.MoveTo(end)
	x.TryMoveH(+1, true)
	y.TryMoveH(-1, true)

	this.wholeWord = y.Compare(start) == 0 && x.Compare(end) == 0
}

func (this *WholeWordHighlighter) IsWholeWord() bool {
	return this.wholeWord
}

// ---

type TextBoxWidget struct {
	Widget
	Content        MultilineContentI
	caretPosition  *CaretPosition
	layoutDepNode2 DepNode2Func
	scrollToCaret  DepNode2Func // TODO: DepNode2Event?

	findPanel            *FindPanel
	findResults          *FindResults
	wholeWordHighlighter *WholeWordHighlighter
	wholeWordResults     *FindResults

	options TextBoxWidgetOptions

	// TESTS
	ValidChange TextBoxWidgetValidChange // TODO: This should probably be properly moved into DepNode2 or similar.

	ExtensionsTest      []Widgeter
	HighlightersTest    []Highlighter
	DynamicHighlighters []interface {
		Highlighter() Highlighter
	}
	LineHighlighter        LineHighlighter
	DynamicLineHighlighter func() LineHighlighter
	PopupsTest             []Widgeter
	DepsTest               []DepNode2I // Temporary solution until there's a better MultilineContentFunc.
}

type TextBoxWidgetValidChange struct {
	*TextBoxWidget
	DepNode2Manual
}

type TextBoxWidgetOptions struct {
	SingleLine bool
	Private    bool
	PopupTest  bool
	ValidFunc  func(MultilineContentI) bool
	FindPanel  bool
}

func NewTextBoxWidget(pos mgl64.Vec2) *TextBoxWidget {
	mc := NewMultilineContent()
	return NewTextBoxWidgetExternalContent(pos, mc, nil)
}

func NewTextBoxWidgetOptions(pos mgl64.Vec2, options TextBoxWidgetOptions) *TextBoxWidget {
	mc := NewMultilineContent()
	return NewTextBoxWidgetExternalContent(pos, mc, &options)
}

func NewTextBoxWidgetExternalContent(pos mgl64.Vec2, mc MultilineContentI, options *TextBoxWidgetOptions) *TextBoxWidget {
	if options == nil {
		options = &TextBoxWidgetOptions{}
	}

	w := &TextBoxWidget{
		Widget:        NewWidget(pos, mgl64.Vec2{0, 0}),
		Content:       mc,
		caretPosition: NewCaretPosition(mc),
		options:       *options,
	}
	w.layoutDepNode2.UpdateFunc = func(DepNode2I) { w.NotifyChange() }
	w.layoutDepNode2.AddSources(mc) // TODO: What about removing w when it's "deleted"?
	w.ValidChange.TextBoxWidget = w

	// TEST
	w.scrollToCaret.UpdateFunc = func(DepNode2I) {
		if scrollPane, ok := w.Parent().(*ScrollPaneWidget); ok && scrollPane.child == w {
			expandedCaretPosition, caretLine := w.caretPosition.caretPosition.ExpandedPosition()

			scrollPane.ScrollToArea(mgl64.Vec2{float64(expandedCaretPosition * fontWidth), float64(caretLine * fontHeight)}, mgl64.Vec2{0, fontHeight})
		}
	}
	w.scrollToCaret.AddSources(w.caretPosition)

	// DEBUG, TEMPORARY: Turn on Go highlighting for everything
	//if uri, ok := w.Content.GetUriForProtocol("file://"); ok && strings.HasSuffix(string(uri), ".go") {
	//if false {
	if _, instant := w.Content.(*MultilineContentFuncInstant); !instant { // The MultilineContentFuncInstant hack is unsafe to use with highlighting
		highlightedGoContent := &highlightedGoContent{}
		highlightedGoContent.AddSources(mc)

		w.HighlightersTest = append(w.HighlightersTest, highlightedGoContent)
	}

	if w.options.FindPanel {
		w.findResults = &FindResults{Owner: w}
		w.findPanel = NewFindPanel(mgl64.Vec2{0, 0}, w.findResults, w.caretPosition) // TODO: Make it appear on bottom instead of (0, 0) top left corner.
		w.findResults.AddSources(w.Content, w.findPanel.FindBox.Content)

		w.wholeWordHighlighter = &WholeWordHighlighter{}
		w.wholeWordHighlighter.AddSources(w.Content, w.caretPosition, &w.layoutDepNode2) // layoutDepNode2 is needed to ensure caret position is kept within bounds as a prerequisite.

		w.wholeWordResults = &FindResults{Owner: w}
		w.wholeWordResults.AddSources(w.Content, w.wholeWordHighlighter)
	}

	return w
}

// TOOD: Improve horizontal centering.
func (w *TextBoxWidget) CenterOnCaretPosition() {
	// HACK: This kinda conflicts/overlaps with MakeUpdated(&w.scrollToCaret), which also tries to scroll the scroll pane... Find a better way.
	if scrollPane, ok := w.Parent().(*ScrollPaneWidget); ok && scrollPane.child == w {
		expandedCaretPosition, caretLine := w.caretPosition.caretPosition.ExpandedPosition()

		scrollPane.CenterOnArea(mgl64.Vec2{float64(expandedCaretPosition * fontWidth), float64(caretLine * fontHeight)}, mgl64.Vec2{0, fontHeight})
	}
}

// TOOD: Improve horizontal centering.
func (w *TextBoxWidget) CenterOnCaretPositionIfOffscreen() {
	// HACK: This kinda conflicts/overlaps with MakeUpdated(&w.scrollToCaret), which also tries to scroll the scroll pane... Find a better way.
	if scrollPane, ok := w.Parent().(*ScrollPaneWidget); ok && scrollPane.child == w {
		expandedCaretPosition, caretLine := w.caretPosition.caretPosition.ExpandedPosition()

		// Check for visible lines.
		// TODO: Generalize this.
		const debugSmallerViewport = fontHeight
		beginLineIndex, endLineIndex := 0, w.Content.LenLines()
		if beginVisibleLineIndex := int(WidgeterS{w}.GlobalToLocal(mgl64.Vec2{0, debugSmallerViewport})[1] / fontHeight); beginVisibleLineIndex > beginLineIndex {
			beginLineIndex = intmath.MinInt(beginVisibleLineIndex, endLineIndex)
		}
		_, height := globalWindow.GetSize() // HACK: Should be some viewport
		height -= debugSmallerViewport
		if endVisibleLineIndex := int(WidgeterS{w}.GlobalToLocal(mgl64.Vec2{0, float64(height)})[1]/fontHeight + 1); endVisibleLineIndex < endLineIndex {
			endLineIndex = intmath.MaxInt(endVisibleLineIndex, beginLineIndex)
		}

		if int(caretLine) < beginLineIndex || int(caretLine) >= endLineIndex {
			scrollPane.CenterOnArea(mgl64.Vec2{float64(expandedCaretPosition * fontWidth), float64(caretLine * fontHeight)}, mgl64.Vec2{0, fontHeight})
		}
	}
}

type TextBoxScrollPaneView struct {
	caretPosition      CaretPosition
	scrollPaneChildPos mgl64.Vec2
}

func (w *TextBoxWidget) SaveView() (view TextBoxScrollPaneView) {
	view.caretPosition = w.caretPosition.SaveState()

	if scrollPane, ok := w.Parent().(*ScrollPaneWidget); ok && scrollPane.child == w {
		view.scrollPaneChildPos = *scrollPane.child.Pos()
	}

	return view
}

func (w *TextBoxWidget) RestoreView(view TextBoxScrollPaneView) {
	w.caretPosition.RestoreState(view.caretPosition)

	if scrollPane, ok := w.Parent().(*ScrollPaneWidget); ok && scrollPane.child == w {
		*scrollPane.child.Pos() = view.scrollPaneChildPos
	}
}

func (w *TextBoxWidget) isFindPanelVisible() bool {
	for _, widget := range w.PopupsTest {
		if findPanel, ok := widget.(*FindPanel); ok && findPanel == w.findPanel {
			return true
		}
	}
	return false
}

func (w *TextBoxWidget) NotifyChange() {
	w.caretPosition.NotifyContentChanged()

	w.Layout()

	w.NotifyAllListeners()

	// TODO: This should probably be properly moved into DepNode2 or similar.
	if w.IsValidTEST() {
		ExternallyUpdated(&w.ValidChange)
	}

	// TODO: Figure out if this should be here... is it a big deal if it gets called here AND elsewhere?
	redraw = true
}

// TODO: Remove after done testing...
func (w *TextBoxWidget) IsValidTEST() bool {
	return w.options.ValidFunc == nil || w.options.ValidFunc(w.Content)
}

func (w *TextBoxWidget) PollLogic() {
	for _, extension := range w.ExtensionsTest {
		extension.PollLogic()
	}

	for _, dep := range w.DepsTest {
		MakeUpdated(dep)
	}

	if w.findResults != nil && w.isFindPanelVisible() {
		MakeUpdated(w.findResults)
	} else if w.wholeWordHighlighter != nil && !w.isFindPanelVisible() {
		MakeUpdated(w.wholeWordHighlighter)
		// TODO: Improve this... Need to add logic support for DepNode2 system?
		if w.wholeWordHighlighter.IsWholeWord() {
			MakeUpdated(w.wholeWordResults)
		}
	}

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.PollLogic()
}

func (w *TextBoxWidget) Layout() {
	if w.Content.LongestLine() < 3 {
		w.size[0] = float64(fontWidth * 3)
	} else {
		w.size[0] = float64(fontWidth * w.Content.LongestLine())
	}
	w.size[1] = float64(fontHeight * w.Content.LenLines())

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *TextBoxWidget) LayoutNeeded() {
	// HACK: Set parent of findPanel. Can't do this in constructor because insideScrollPane may not be true yet, etc. But is this really the place to do this?
	if w.findPanel != nil && w.findPanel.Parent() == nil {
		// HACK: Not general at all
		if scrollPane, insideScrollPane := w.Parent().(*ScrollPaneWidget); insideScrollPane && scrollPane.child == w {
			w.findPanel.SetParent(scrollPane)
		} else {
			w.findPanel.SetParent(w)
		}
	}

	MakeUpdated(&w.layoutDepNode2)
	MakeUpdated(&w.scrollToCaret)

	for _, widget := range w.PopupsTest {
		widget.LayoutNeeded()
	}
}

func (w *TextBoxWidget) Render() {
	// HACK: This is an update operation, so it should happen in PollLogic(). However, it seems putting it there causes crashes. Need to improve this.
	for _, highlighter := range w.HighlightersTest {
		MakeUpdated(highlighter)
	}
	for _, dynamicHighlighter := range w.DynamicHighlighters {
		if highlighter := dynamicHighlighter.Highlighter(); highlighter != nil {
			MakeUpdated(highlighter)
		}
	}
	// HACK: DynamicLineHighlighter currently simply overrides existing LineHighlighter.
	if w.DynamicLineHighlighter != nil {
		w.LineHighlighter = w.DynamicLineHighlighter()
	}
	if w.LineHighlighter != nil {
		MakeUpdated(w.LineHighlighter)
	}

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && keyboardPointer.OriginMapping.ContainsWidget(w)

	// HACK: Brute-force check the mouse pointer if it contains this widget
	isOriginHit := false
	for _, hit := range mousePointer.OriginMapping {
		if w == hit {
			isOriginHit = true
			break
		}
	}
	isHit := len(w.HoverPointers()) > 0

	if w.options.ValidFunc == nil {
		// HACK: Assumes mousePointer rather than considering all connected pointing pointers
		if isOriginHit && mousePointer.State.IsActive() && isHit {
			DrawYBox(w.pos, w.size)
		} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
			DrawYBox(w.pos, w.size)
		} else if hasTypingFocus {
			DrawYBox(w.pos, w.size)
		} else {
			DrawNBox(w.pos, w.size)
		}
	} else {
		var background mgl64.Vec3
		if w.options.ValidFunc(w.Content) {
			background = lightGreenColor
		} else {
			background = lightRedColor
		}

		// HACK: Assumes mousePointer rather than considering all connected pointing pointers
		if isOriginHit && mousePointer.State.IsActive() && isHit {
			DrawBox(w.pos, w.size, highlightColor, background)
		} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
			DrawBox(w.pos, w.size, highlightColor, background)
		} else if hasTypingFocus {
			DrawBox(w.pos, w.size, highlightColor, background)
		} else {
			DrawBox(w.pos, w.size, mgl64.Vec3{0.3, 0.3, 0.3}, background)
		}
	}

	// Render only visible lines.
	// TODO: Generalize this.
	const debugSmallerViewport = fontHeight
	beginLineIndex, endLineIndex := 0, w.Content.LenLines()
	if beginVisibleLineIndex := int(WidgeterS{w}.GlobalToLocal(mgl64.Vec2{0, debugSmallerViewport})[1] / fontHeight); beginVisibleLineIndex > beginLineIndex {
		beginLineIndex = intmath.MinInt(beginVisibleLineIndex, endLineIndex)
	}
	_, height := globalWindow.GetSize() // HACK: Should be some viewport
	height -= debugSmallerViewport
	if endVisibleLineIndex := int(WidgeterS{w}.GlobalToLocal(mgl64.Vec2{0, float64(height)})[1]/fontHeight + 1); endVisibleLineIndex < endLineIndex {
		endLineIndex = intmath.MaxInt(endVisibleLineIndex, beginLineIndex)
	}

	// Line Highlighter
	if w.LineHighlighter != nil {
		for lineIndex := beginLineIndex; lineIndex < endLineIndex; lineIndex++ {
			if backgroundColor := w.LineHighlighter.LineBackgroundColor(lineIndex); backgroundColor != nil {
				DrawBorderlessBox(mgl64.Vec2{w.pos[0], w.pos[1] + float64(fontHeight*lineIndex)}, mgl64.Vec2{w.size[0], fontHeight}, *backgroundColor)
			}
		}
	}

	// DEBUG, HACK: Temporarily use cursor to highlight entire line when inactive, etc.
	//if !hasTypingFocus {
	if w.options.ValidFunc == nil && w.LineHighlighter == nil && !w.caretPosition.anySelection() { // TEST: Try to always highlight in subtly darker white (unless there's validation).
		_, caretLine := w.caretPosition.caretPosition.ExpandedPosition()

		// Highlight line
		gl.PushMatrix()
		gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)
		gl.Color3dv((*float64)(&veryLightColor[0]))
		gl.Recti(int32(0), int32(caretLine*fontHeight), int32(w.size[0]), int32(caretLine*fontHeight)+fontHeight)
		gl.PopMatrix()
	}

	{
		gl.Color3d(0, 0, 0)
		if !w.options.Private {
			hlIters := []HighlighterIterator{}
			for _, highlighter := range w.HighlightersTest {
				hlIters = append(hlIters, highlighter.NewIterator(w.Content.Line(beginLineIndex).Start))
			}
			for _, dynamicHighlighter := range w.DynamicHighlighters {
				if highlighter := dynamicHighlighter.Highlighter(); highlighter != nil {
					hlIters = append(hlIters, highlighter.NewIterator(w.Content.Line(beginLineIndex).Start))
				}
			}

			// Highlight search results or whole words.
			if w.findResults != nil && w.isFindPanelVisible() {
				hlIters = append(hlIters, w.findResults.NewIterator(w.Content.Line(beginLineIndex).Start))
			} else if w.wholeWordHighlighter != nil && !w.isFindPanelVisible() && w.wholeWordHighlighter.IsWholeWord() {
				hlIters = append(hlIters, w.wholeWordResults.NewIterator(w.Content.Line(beginLineIndex).Start))
			}

			// HACK, TODO: Manually add NewSelectionHighlighter for now, need to make this better
			{
				min, max := w.caretPosition.SelectionRange()
				hlIters = append(hlIters, NewSelectionHighlighterIterator(w.Content.Line(beginLineIndex).Start, min, max, hasTypingFocus))
			}

			glt := NewOpenGlStream(w.pos.Add(mgl64.Vec2{0, float64(fontHeight * beginLineIndex)}))

			for _, hlIter := range hlIters {
				textStyle := hlIter.Current()
				textStyle.Apply(glt)
			}

			for contentOffset, contentSpan := w.Content.Line(beginLineIndex).Start, uint32(0); contentOffset < w.Content.Line(endLineIndex).Start; contentOffset += contentSpan {
				contentSpan = w.Content.Line(endLineIndex).Start - contentOffset
				for _, hlIter := range hlIters {
					contentSpan = intmath.MinUint32(contentSpan, hlIter.Next())
				}

				glt.PrintText(w.Content.Content()[contentOffset : contentOffset+contentSpan])

				for _, hlIter := range hlIters {
					textStyle := hlIter.Advance(contentSpan)
					textStyle.Apply(glt)
				}
			}
		} else {
			for lineIndex := 0; lineIndex < w.Content.LenLines(); lineIndex++ {
				contentLine := w.Content.Line(lineIndex)
				PrintLine(mgl64.Vec2{w.pos[0], w.pos[1] + float64(fontHeight*lineIndex)}, strings.Repeat("*", int(contentLine.Length)))
			}
		}
	}

	// Display Go Errors via line highlighting.
	for _, uri := range w.Content.GetAllUris() {
		if _, ok := goCompileErrorsManagerTest.All[uri]; ok {
			glt := NewOpenGlStream(np)
			for lineIndex, messages := range goCompileErrorsManagerTest.All[uri] {
				if lineIndex < beginLineIndex || lineIndex >= endLineIndex {
					continue // Skip lines that are offscreen.
				}

				expandedLineLength := ExpandedLength(w.Content.Content()[w.Content.Line(lineIndex).Start:w.Content.Line(lineIndex).End()], 0)
				for sameLineIndex, message := range messages {
					pos := w.pos.Add(mgl64.Vec2{fontWidth * float64(expandedLineLength+1), fontHeight * float64(lineIndex+sameLineIndex)})
					DrawInnerRoundedBox(pos, mgl64.Vec2{fontWidth * float64(ExpandedLength(message, 0)), fontHeight}, darkColor, darkRedColor)
					gl.Color3d(0, 0, 0)
					glt.SetPos(pos)
					glt.PrintLine(message)
				}
			}
		}
	}

	if hasTypingFocus {
		expandedCaretPosition, caretLine := w.caretPosition.caretPosition.ExpandedPosition()

		// Draw caret
		gl.PushMatrix()
		gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)
		gl.Color3d(0, 0, 0)
		gl.Recti(int32(expandedCaretPosition*fontWidth-1), int32(caretLine*fontHeight), int32(expandedCaretPosition*fontWidth+1), int32(caretLine*fontHeight)+fontHeight)
		gl.PopMatrix()
	}

	for _, widget := range w.PopupsTest {
		gl.PushMatrix()
		// HACK: Not general at all
		if _, insideScrollPane := w.Parent().(*ScrollPaneWidget); !insideScrollPane {
			gl.Translated(float64(w.pos[0]), float64(w.pos[1]), 0)
		}
		widget.Render()
		gl.PopMatrix()
	}
}
func (w *TextBoxWidget) Hit(ParentPosition mgl64.Vec2) []Widgeter {
	/*if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}*/
	LocalPosition := w.ParentToLocal(ParentPosition)

	if len(w.Widget.Hit(ParentPosition)) > 0 {
		hits := []Widgeter{w}
		for _, widget := range w.PopupsTest {
			// HACK: Not general at all
			if scrollPane, insideScrollPane := w.Parent().(*ScrollPaneWidget); insideScrollPane && scrollPane.child == w {
				popupHits := widget.Hit(ParentPosition)
				if len(popupHits) > 0 {
					return popupHits
				}
			} else {
				popupHits := widget.Hit(LocalPosition)
				if len(popupHits) > 0 {
					return popupHits
				}
			}
		}
		return hits
	} else {
		return nil
	}
}
func (w *TextBoxWidget) ProcessEvent(inputEvent InputEvent) {
	for _, extension := range w.ExtensionsTest {
		extension.ProcessEvent(inputEvent)
	}

	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == true &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		if !keyboardPointer.OriginMapping.ContainsWidget(w) {
			keyboardPointer.OriginMapping = []Widgeter{w}
		}
	}

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	//hasTypingFocus := keyboardPointer != nil && keyboardPointer.OriginMapping.ContainsWidget(w)

	if inputEvent.Pointer.VirtualCategory == POINTING {
		if inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == true { // On mouse button 0 down
			// Leave selection if shift is held down
			leaveSelection := inputEvent.ModifierKey&glfw.ModShift != 0

			globalPosition := mgl64.Vec2{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
			localPosition := WidgeterS{w}.GlobalToLocal(globalPosition)
			w.caretPosition.SetPositionFromPhysical(localPosition, leaveSelection)
		} else if inputEvent.EventTypes[AXIS_EVENT] && inputEvent.InputId == 0 && inputEvent.Pointer.State.Button(0) { // On mouse move while button 0 down
			leaveSelection := true

			globalPosition := mgl64.Vec2{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
			localPosition := WidgeterS{w}.GlobalToLocal(globalPosition)
			w.caretPosition.SetPositionFromPhysical(localPosition, leaveSelection)
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
		switch glfw.Key(inputEvent.InputId) {
		case glfw.KeyBackspace:
			w.caretPosition.Backspace()
		case glfw.KeyDelete:
			w.caretPosition.CreateSelectionIfNone(+1)
			w.caretPosition.ReplaceSelectionWith("")
		case glfw.KeyEnter:
			if w.options.SingleLine {
				break
			}

			w.caretPosition.ReplaceSelectionWith("")

			//func GetLeadingTabCount() uint32
			tabCount := 0
			{
				lineToCaret := w.Content.Content()[w.Content.Line(w.caretPosition.caretPosition.lineIndex).Start:w.caretPosition.Logical()]

				for _, c := range lineToCaret {
					if c == '\t' {
						tabCount++
					} else {
						break
					}
				}
			}

			w.caretPosition.ReplaceSelectionWith("\n" + strings.Repeat("\t", tabCount))

		case glfw.KeyTab:
			w.caretPosition.ReplaceSelectionWith("\t")
		case glfw.KeyLeft:
			if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModSuper {
				// Go to start of line.
				// TODO: Go to start of line-ish (toggle between real start and non-whitespace start); leave Move(-2) alone because it's used elsewhere for existing purpose
				w.caretPosition.Move(-2, inputEvent.ModifierKey&glfw.ModShift != 0)
			} else if inputEvent.ModifierKey & ^(glfw.ModShift|glfw.ModAlt) == 0 {
				w.caretPosition.TryMoveH(-1, inputEvent.ModifierKey&glfw.ModShift != 0, inputEvent.ModifierKey&glfw.ModAlt != 0)
			}
		case glfw.KeyRight:
			if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModSuper {
				// Go to end of line.
				w.caretPosition.Move(+2, inputEvent.ModifierKey&glfw.ModShift != 0)
			} else if inputEvent.ModifierKey & ^(glfw.ModShift|glfw.ModAlt) == 0 {
				w.caretPosition.TryMoveH(+1, inputEvent.ModifierKey&glfw.ModShift != 0, inputEvent.ModifierKey&glfw.ModAlt != 0)
			}
		case glfw.KeyUp:
			if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModSuper {
				w.caretPosition.Move(-3, inputEvent.ModifierKey&glfw.ModShift != 0)
			} else if inputEvent.ModifierKey & ^(glfw.ModShift|glfw.ModAlt) == 0 {
				w.caretPosition.TryMoveV(-1, inputEvent.ModifierKey&glfw.ModShift != 0, inputEvent.ModifierKey&glfw.ModAlt != 0)
			}
		case glfw.KeyDown:
			if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModSuper {
				w.caretPosition.Move(+3, inputEvent.ModifierKey&glfw.ModShift != 0)
			} else if inputEvent.ModifierKey & ^(glfw.ModShift|glfw.ModAlt) == 0 {
				w.caretPosition.TryMoveV(+1, inputEvent.ModifierKey&glfw.ModShift != 0, inputEvent.ModifierKey&glfw.ModAlt != 0)
			}
		case glfw.KeyA:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.caretPosition.SelectAll()
			} else if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModControl {
				// Go to start of line.
				// TODO: Go to start of line-ish (toggle between real start and non-whitespace start); leave Move(-2) alone because it's used elsewhere for existing purpose
				w.caretPosition.Move(-2, inputEvent.ModifierKey&glfw.ModShift != 0)
			}
		case glfw.KeyX:
			if !w.options.Private &&
				inputEvent.ModifierKey == glfw.ModSuper {

				w.caretPosition.CreateSelectionLineIfNone()
				if selectionContent := w.caretPosition.GetSelectionContent(); selectionContent != "" {
					globalWindow.SetClipboardString(selectionContent) // TODO: Don't use globalWindow
					w.caretPosition.ReplaceSelectionWith("")
				}
			}
		case glfw.KeyC:
			if !w.options.Private &&
				inputEvent.ModifierKey == glfw.ModSuper {

				w.caretPosition.CreateSelectionLineIfNone()
				if selectionContent := w.caretPosition.GetSelectionContent(); selectionContent != "" {
					globalWindow.SetClipboardString(selectionContent) // TODO: Don't use globalWindow
				}
			}
		case glfw.KeyV:
			if inputEvent.ModifierKey == glfw.ModSuper {
				if clipboard, err := globalWindow.GetClipboardString(); err == nil && clipboard != "" { // TODO: Don't use globalWindow
					w.caretPosition.ReplaceSelectionWith(clipboard)
				}
			}
		case glfw.KeyE:
			if inputEvent.ModifierKey == glfw.ModSuper {
				if fileUri, ok := w.Content.GetUriForProtocol("file://"); ok {
					// Open in external editor
					func(filePath string) {

						// HACK: OS X specific
						cmd := exec.Command("open", filePath)
						err := cmd.Start()
						CheckError(err)
						go cmd.Wait() // It looks like I need to wait for the process, else it doesn't terminate properly

					}(string(fileUri[len("file://"):]))
				}
			} else if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModControl {
				// Go to end of line.
				w.caretPosition.Move(+2, inputEvent.ModifierKey&glfw.ModShift != 0)
			}
		/*case glfw.KeyR:
		if inputEvent.ModifierKey == glfw.ModSuper {
			ExternallyUpdated(w.Content) // TODO: Need to make this apply only for event-based things; no point in forcibly updating pure data structures
		}*/
		case glfw.KeyS:
			if inputEvent.ModifierKey == glfw.ModSuper {
				// TODO: Move this compoment out of TextBoxWidget; make it dynamically attachable or something.
				if uri, ok := w.Content.GetUriForProtocol("file://"); ok {
					switch {
					case strings.HasSuffix(string(uri), ".go"):
						// Run `goimports` on the source code
						if out, err := goimports.Process("", []byte(w.Content.Content()), nil); err == nil {
							SetViewGroup(w.Content, string(out))
						}
					case strings.HasSuffix(string(uri), ".md") || strings.HasSuffix(string(uri), ".markdown"):
						// Run `markdownfmt` on Markdown.
						if out, err := markdown.Process("", []byte(w.Content.Content()), nil); err == nil {
							SetViewGroup(w.Content, string(out))
						}
					}
				}
			}
		case glfw.KeyR:
			if inputEvent.ModifierKey == glfw.ModSuper {
				// TODO: Move this compoment out of TextBoxWidget; make it dynamically attachable or something.
				if !w.options.PopupTest {
					break
				}

				popupTest := NewSearchableListWidgetTest(mgl64.Vec2{200, 0}, mgl64.Vec2{600, 600}, globalGoSymbols)
				// HACK: Not general at all
				if scrollPane, insideScrollPane := w.Parent().(*ScrollPaneWidget); insideScrollPane {
					popupTest.SetParent(scrollPane)
				} else {
					popupTest.SetParent(w)
				}

				// HACK: Duplicated code, need to refactor
				{
					scrollToSymbolB := DepNode2Func{}
					scrollToSymbolB.UpdateFunc = func(this DepNode2I) {
						if entry := this.GetSources()[0].(Selecter).GetSelected(); entry != nil {
							// TODO: Replace with entry.(Something).CaretPositionStart, End -> editor.ScrollToCaret(Start, End)
							//println("selected:", entry.String())

							if declNode, ok := entry.(*NodeStringer); ok {

								if file := globalTypeCheckedPackage.fset.File(declNode.Pos()); file != nil {
									// TODO: Change folderListing selection if it's in a different file

									// TODO: There's a race condition here, to do the thing below I need to have finished changing the file, but
									//       that currently reloads ASTs, etc. causing crashes sometimes depending on order of file ASTs being processed.

									w.caretPosition.TrySet(uint32(file.Offset(declNode.Pos())))
									w.CenterOnCaretPosition()
								}
							}
						}
					}
					scrollToSymbolB.AddSources(popupTest.OnSelectionChanged())
					keepUpdatedTEST = append(keepUpdatedTEST, &scrollToSymbolB)
				}

				originalMapping := keyboardPointer.OriginMapping // HACK
				originalView := w.SaveView()
				closeOnEscape := &CustomWidget{
					Widget: NewWidget(np, np),
					ProcessEventFunc: func(inputEvent InputEvent) {
						if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
							switch glfw.Key(inputEvent.InputId) {
							case glfw.KeyEnter:
								// Remove popupTest from w.PopupsTest.
								for i, widget := range w.PopupsTest {
									if widget == popupTest {
										w.PopupsTest = append(w.PopupsTest[:i], w.PopupsTest[i+1:]...)
										break
									}
								}

								// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
								keyboardPointer.OriginMapping = originalMapping
							case glfw.KeyEscape:
								// Remove popupTest from w.PopupsTest.
								for i, widget := range w.PopupsTest {
									if widget == popupTest {
										w.PopupsTest = append(w.PopupsTest[:i], w.PopupsTest[i+1:]...)
										break
									}
								}

								w.RestoreView(originalView)

								// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
								keyboardPointer.OriginMapping = originalMapping
							}
						}
					},
				}
				popupTest.ExtensionsTest = append(popupTest.ExtensionsTest, closeOnEscape)

				w.PopupsTest = append(w.PopupsTest, popupTest)

				popupTest.SetKeyboardFocus()
			}
		// Find panel.
		case glfw.KeyF:
			if inputEvent.ModifierKey == glfw.ModSuper {
				// TODO: Move this compoment out of TextBoxWidget; make it dynamically attachable or something.
				if !(w.options.PopupTest && w.options.FindPanel) {
					break
				}

				popupTest := w.findPanel

				if !w.isFindPanelVisible() {
					originalMapping := keyboardPointer.OriginMapping // HACK
					w.findPanel.OriginalView = w.SaveView()
					closeOnEscape := &CustomWidget{
						Widget: NewWidget(np, np),
						ProcessEventFunc: func(inputEvent InputEvent) {
							if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
								switch glfw.Key(inputEvent.InputId) {
								case glfw.KeyEnter:
									// HACK: I should map it so Enter triggers the same action as Cmd+G handler, instead of faking it here...
									inputEvent.InputId = uint16(glfw.KeyG)
									inputEvent.ModifierKey |= glfw.ModSuper
									w.ProcessEvent(inputEvent)
								case glfw.KeyEscape:
									// Remove popupTest from w.PopupsTest.
									for i, widget := range w.PopupsTest {
										if widget == popupTest {
											w.PopupsTest = append(w.PopupsTest[:i], w.PopupsTest[i+1:]...)
											break
										}
									}

									//w.RestoreView(w.findPanel.OriginalView)

									// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
									keyboardPointer.OriginMapping = originalMapping

								// HACK: I should make it so Cmd+F triggers the parent's Cmd+F handler, instead of faking it here...
								case glfw.KeyF:
									if inputEvent.ModifierKey == glfw.ModSuper {
										w.findPanel.FindBox.caretPosition.SelectAll()
									}
								// HACK: I should make it so Cmd+G triggers the parent's Cmd+G handler, instead of faking it here...
								case glfw.KeyG:
									w.ProcessEvent(inputEvent)
								}
							}
						},
					}
					w.findPanel.FindBox.ExtensionsTest = []Widgeter{closeOnEscape} // HACK: Since I'm reusing an existing object, I need to update its extension rather than add new one. But doing it this way effectively prevents any other extensions to be added elsewhere, etc. Number of times I was bitten by this hack so far: 1.

					w.PopupsTest = append(w.PopupsTest, popupTest)
				}

				popupTest.SetKeyboardFocus()

				if w.caretPosition.anySelection() {
					SetViewGroup(w.findPanel.FindBox.Content, w.caretPosition.GetSelectionContent())
				}
				w.findPanel.FindBox.caretPosition.SelectAll()
			}
		case glfw.KeyMinus:
			if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModControl {
				// TODO: Move this compoment out of TextBoxWidget; make it dynamically attachable or something.
				if !(w.options.PopupTest && w.options.FindPanel) {
					break
				}

				// HACK: Need to properly implement Cmd+-, instead of this temporary most-common-use hack.
				w.RestoreView(w.findPanel.OriginalView)
			}
		case glfw.KeyG:
			if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModSuper {
				// TODO: Move this compoment out of TextBoxWidget; make it dynamically attachable or something.
				if !(w.options.PopupTest && w.options.FindPanel) {
					break
				}

				direction := +1
				if shift := inputEvent.ModifierKey&glfw.ModShift != 0; shift {
					direction = -1
				}

				selStart, selEnd := w.caretPosition.SelectionRange()
				if start, end, ok := w.findResults.GetResult(selStart, selEnd, direction); ok {
					w.caretPosition.TrySet(start)
					w.caretPosition.caretPosition.willMoveH(int32(end - start))
					w.CenterOnCaretPositionIfOffscreen()
				}
			}
		case glfw.KeyEscape:
			// Close the last popup, if any.
			if len(w.PopupsTest) > 0 {
				w.PopupsTest = w.PopupsTest[:len(w.PopupsTest)-1]
			}
		// TEST: Closing this widget...
		case glfw.KeyW:
			if inputEvent.ModifierKey == glfw.ModControl {
				/*for i, widget := range widgets {
					if widget == w {
						widgets = append(widgets[:i], widgets[i+1:]...)
						break
					}
				}*/
			}
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[CHARACTER_EVENT] && inputEvent.InputId < 128 {
		w.caretPosition.ReplaceSelectionWith(string(inputEvent.InputId))
	}
}

// ---

type TextFileWidget struct {
	*TextBoxWidget
}

func NewTextFileWidget(pos mgl64.Vec2, path string) *TextFileWidget {
	// TODO: Opening the same file shouldn't result in a new MultilineContentFile
	ec := NewMultilineContentFile(path)
	w := &TextFileWidget{TextBoxWidget: NewTextBoxWidgetExternalContent(pos, ec, nil)}
	return w
}

func (w *TextFileWidget) Path() string {
	return w.TextBoxWidget.Content.(*MultilineContentFile).Path()
}

// ---

func NewTextBoxValidationWidget(pos mgl64.Vec2, validFunc func(MultilineContentI) bool) *TextBoxWidget {
	w := NewTextBoxWidgetOptions(pos, TextBoxWidgetOptions{ValidFunc: validFunc})
	return w
}

// ---

type Clock struct {
	DepNode

	TimePassed float64
}

var UniversalClock Clock

// ---

type VirtualCategory uint8

const (
	TYPING VirtualCategory = iota
	POINTING
)

type Pointer struct {
	VirtualCategory VirtualCategory
	Mapping         Widgeters // Always reflects current pointer state.
	OriginMapping   Widgeters // Updated only when pointer is moved while not active (e.g., where mouse button was first pressed down).
	State           PointerState
}

func (this *Pointer) Render() {
	switch {
	case this.VirtualCategory == POINTING && len(this.State.Axes) >= 2:
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translated(float64(float64(NearInt64(this.State.Axes[0]))+0.5), float64(float64(NearInt64(this.State.Axes[1]))+0.5), 0)

		const size float64 = fontHeight
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
	}
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

const katOnly = false

func ProcessInputEventQueue(widget Widgeter, inputEventQueue []InputEvent) []InputEvent {
	for len(inputEventQueue) > 0 {
		inputEvent := inputEventQueue[0]

		if !katOnly {
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
			}
		} else {
			keyboardPointer.OriginMapping[0].ProcessEvent(inputEvent)
		}

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
			// Extend slice if needed
			neededSize := int(inputEvent.InputId) + len(inputEvent.Buttons)
			if neededSize > len(inputEvent.Pointer.State.Buttons) {
				inputEvent.Pointer.State.Buttons = append(inputEvent.Pointer.State.Buttons, make([]bool, neededSize-len(inputEvent.Pointer.State.Buttons))...)
			}

			copy(inputEvent.Pointer.State.Buttons[inputEvent.InputId:], inputEvent.Buttons)
		}

		if inputEvent.EventTypes[AXIS_EVENT] {
			// Extend slice if needed
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

type MarkdownHandlerFunc func(req *http.Request) (markdown []byte, err error)

func (this MarkdownHandlerFunc) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	markdown, err := this(req)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	if _, plain := req.URL.Query()["plain"]; plain {
		w.Header().Set("Content-Type", "text/plain")
		w.Write(markdown)
	} else if _, github := req.URL.Query()["github"]; github {
		w.Header().Set("Content-Type", "text/html")
		started := time.Now()
		u1.WriteGitHubFlavoredMarkdownViaGitHub(w, markdown)
		fmt.Println("rendered GFM via GitHub, took", time.Since(started))
	} else {
		w.Header().Set("Content-Type", "text/html")
		started := time.Now()
		u1.WriteGitHubFlavoredMarkdownViaLocal(w, markdown)
		fmt.Println("rendered GFM locally, took", time.Since(started))
	}
}

func initHttpHandlers() {
	http.HandleFunc("/close", func(w http.ResponseWriter, req *http.Request) {
		fmt.Fprintln(w, "Closing.")
		keepRunning = false
	})
	/*http.HandleFunc("/widgets", func(w http.ResponseWriter, req *http.Request) {
		w.Write([]byte(goon.SdumpExpr(len(widgets))))
		fmt.Fprintln(w)
		fmt.Fprintf(w, "%#v\n", widgets)
	})*/
	http.Handle("/favicon.ico", http.NotFoundHandler())
	http.Handle("/status/", http.StripPrefix("/status", MarkdownHandlerFunc(func(req *http.Request) ([]byte, error) {

		// HACK: Handle .go files specially, just assume they're in "./GoLand"
		/*if strings.HasSuffix(req.URL.Path, ".go") {
			w.Header().Set("Content-Type", "text/plain")
			w.Write(MustReadFileB(filepath.Join("../../../", req.URL.Path)))
			return
		}*/

		var b string

		// TODO: Try to lookup the GoPackage rather than creating a new one.
		importPath := req.URL.Path[1:]
		if goPackage := GoPackageFromImportPath(importPath); goPackage != nil {
			// TODO: Cache this via DepNode2I
			dpkg, err := GetDocPackageAll(goPackage.Bpkg, nil)
			if err == nil {
				b += Underline(`import "`+dpkg.ImportPath+`"`) + "\n```Go\n"
				var buf bytes.Buffer
				FprintPackageFullSummary(&buf, dpkg)
				b += buf.String()
				b += "\n```\n"
			} else {
				b += Underline(`import "`+importPath+`"`) + "\n```\n"
				b += err.Error()
				b += "\n```\n"
			}

			b += "\n---\n\n"

			goPackage.UpdateVcs()
			if goPackage.Dir.Repo != nil {
				MakeUpdated(goPackage.Dir.Repo.VcsLocal)
				MakeUpdated(goPackage.Dir.Repo.VcsRemote)
			}

			b += "```\n" + status.PorcelainPresenter(goPackage) + "\n```\n"

			b += "\n---\n\n"

			if goPackage.Dir.Repo != nil {
				b += "```\n"
				if goPackage.Dir.Repo.VcsLocal.Status == "" {
					b += "nothing to commit, working directory clean\n\n"
				} else {
					b += goPackage.Dir.Repo.VcsLocal.Status + "\n"
				}
				b += "Branch: " + goPackage.Dir.Repo.VcsLocal.LocalBranch + "\n"
				b += "Local:  " + goPackage.Dir.Repo.VcsLocal.LocalRev + "\n"
				b += "Remote: " + goPackage.Dir.Repo.VcsRemote.RemoteRev + "\n"
				b += "```\n"

				// git diff
				if workingDiff := u6.GoPackageWorkingDiff(goPackage); workingDiff != "" {
					b += "\n" /*+ `<a id="git-diff"></a>`*/ + Underline("git diff")
					cmd := exec.Command("git", "diff", "--stat")
					cmd.Dir = goPackage.Dir.Repo.Vcs.RootPath()
					if stat, err := cmd.CombinedOutput(); err == nil {
						b += "\n```\n" + TrimLastNewline(string(stat)) + "\n```\n"
					}
					b += "\n```diff\n" + workingDiff + "\n```\n"
				}
			}

			b += "\n---\n\n"

			b += "```\n" + goPackage.Bpkg.Dir + "\n```\n"
			x := newFolderListingPureWidget(goPackage.Bpkg.Dir)
			for _, v := range x.entries {
				b += fmt.Sprintf("[%s](%s)  \n", v.Name(), path.Join("/status/", importPath, v.Name()))
			}
		} else {
			b += fmt.Sprintf("Package %q not found in %q (are you sure it's a valid Go package; maybe its subdir).\n", importPath, os.Getenv("GOPATH"))
		}

		return []byte(b), nil
	})))
	http.Handle("/status", MarkdownHandlerFunc(func(req *http.Request) ([]byte, error) {
		started := time.Now()

		_, short := req.URL.Query()["short"]

		// rootPath -> []*GoPackage
		var goPackagesInRepo = make(map[string][]*GoPackage)

		MakeUpdated(goPackages)
		// TODO: Factor this out somewhere. A cached GoPackagesInRepo type similar to exp14.GoPackages? Or something?
		{
			inChan := make(chan interface{})
			go func() { // This needs to happen in the background because sending input will be blocked on reading output.
				for _, goPackage := range goPackages.Entries {
					inChan <- goPackage
				}
				close(inChan)
			}()
			reduceFunc := func(in interface{}) interface{} {
				goPackage := in.(*GoPackage)
				if rootPath := getRootPath(goPackage); rootPath != "" {
					return NewGoPackageRepo(rootPath, []*GoPackage{goPackage})
				}
				return nil
			}
			outChan := GoReduce(inChan, 64, reduceFunc)
			for out := range outChan {
				repo := out.(GoPackageRepo)
				goPackagesInRepo[repo.RootPath()] = append(goPackagesInRepo[repo.RootPath()], repo.GoPackages()[0])
			}
		}

		goon.DumpExpr(len(goPackagesInRepo))

		reduceFunc := func(in interface{}) interface{} {
			repo := in.(GoPackageRepo)

			goPackage := repo.GoPackages()[0]
			if goPackage.Dir.Repo != nil {
				// HACK: Invalidate cache, always.
				ExternallyUpdated(goPackage.Dir.Repo.VcsLocal.GetSources()[1].(DepNode2ManualI))

				MakeUpdated(goPackage.Dir.Repo.VcsLocal)
			}

			return repo
		}

		inChan := make(chan interface{})
		go func() { // This needs to happen in the background because sending input will be blocked on reading output.
			for rootPath, goPackages := range goPackagesInRepo {
				inChan <- NewGoPackageRepo(rootPath, goPackages)
			}
			close(inChan)
		}()
		outChan := GoReduce(inChan, 8, reduceFunc)

		var summary = bytes.NewBufferString("# GOPATH diff Summary\n\n")
		var buf = new(bytes.Buffer)

		for out := range outChan {
			repo := out.(GoPackageRepo)

			goPackage := repo.GoPackages()[0]

			if goPackage.Dir.Repo.VcsLocal.Status != "" {
				repoImportPathPattern := GetRepoImportPathPattern(repo.RootPath(), goPackage.Bpkg.SrcRoot)
				fmt.Fprintf(summary, "- [%s](%s)\n", repoImportPathPattern, github_flavored_markdown.HeaderLink(repoImportPathPattern))
				fmt.Fprint(buf, "### "+repoImportPathPattern+"\n\n")
				fmt.Fprint(buf, "```\n"+goPackage.Dir.Repo.VcsLocal.Status+"```\n\n")
				if !short {
					fmt.Fprint(buf, "```diff\n"+u6.GoPackageWorkingDiff(goPackage)+"```\n\n")
				}
			}
		}
		fmt.Fprint(summary, "\n")
		if buf.Len() == 0 {
			fmt.Fprint(buf, "### working directory clean (across GOPATH workspaces)")
		}

		fmt.Printf("diffHandler: %v ms.\n", time.Since(started).Seconds()*1000)

		return append(summary.Bytes(), buf.Bytes()...), nil
	}))
	http.Handle("/inline/", http.StripPrefix("/inline", MarkdownHandlerFunc(func(req *http.Request) ([]byte, error) {
		importPath := req.URL.Path[1:]

		// TODO: Cache this via DepNode2I
		buf := new(bytes.Buffer)
		buf.WriteString("```go\n")
		exp11.InlineDotImports(buf, importPath)
		buf.WriteString("\n```")

		return buf.Bytes(), nil
	})))
}

func getRootPath(goPackage *GoPackage) (rootPath string) {
	if goPackage.Standard {
		return ""
	}

	goPackage.UpdateVcs()
	if goPackage.Dir.Repo == nil {
		return ""
	} else {
		return goPackage.Dir.Repo.Vcs.RootPath()
	}
}

// ---

// HACK
type goPackageHardcoded struct {
	DepNode2Manual
}

func (*goPackageHardcoded) GetSelectedGoPackage() *GoPackage {
	x := NewImportPathFound("github.com/gists/gist7176504", "/Users/Dmitri/Dropbox/Work/2013/GoLand/")
	return GoPackageFromImportPathFound(x)
}

// ---

var startedProcess = time.Now()

func main() {
	//defer profile.Start(profile.CPUProfile).Stop()
	//defer profile.Start(profile.MemProfile).Stop()

	fmt.Printf("go version %s %s/%s.\n", runtime.Version(), runtime.GOOS, runtime.GOARCH)
	runtime.GOMAXPROCS(runtime.NumCPU())
	flag.Parse()

	var inputEventQueue []InputEvent
	var inputEventQueue2 = make(chan InputEvent, 32)
	var window *glfw.Window

	if !*headlessFlag {
		runtime.LockOSThread()

		// Verify the GLFW library and header versions match
		{
			major, minor, revision := glfw.GetVersion()
			match := (major == glfw.VersionMajor && minor == glfw.VersionMinor && revision == glfw.VersionRevision)
			if !match {
				panic("Error: GLFW library and header versions do not match.")
			}
		}
		if err := glfw.Init(); err != nil {
			log.Panicln("glfw.Init():", err)
		}
		defer glfw.Terminate()

		glfw.WindowHint(glfw.Samples, 8) // Anti-aliasing
		//glfw.WindowHint(glfw.Decorated, glfw.False)
		var err error
		window, err = glfw.CreateWindow(1536, 960, "", nil, nil)
		CheckError(err)
		globalWindow = window

		window.SetInputMode(glfw.Cursor, glfw.CursorHidden)
		window.MakeContextCurrent()

		if err := gl.Init(); nil != err {
			log.Print(err)
		}
		{
			var samples int32
			gl.GetIntegerv(gl.SAMPLES, &samples)
			fmt.Printf("glfw %d.%d.%d; %s %s %s; %s; %v samples.\n", glfw.VersionMajor, glfw.VersionMinor, glfw.VersionRevision,
				gl.GoStr(gl.GetString(gl.VENDOR)), gl.GoStr(gl.GetString(gl.RENDERER)), gl.GoStr(gl.GetString(gl.VERSION)),
				gl.GoStr(gl.GetString(gl.SHADING_LANGUAGE_VERSION)), samples)
		}

		{
			m, err := glfw.GetPrimaryMonitor()
			CheckError(err)
			vm, err := m.GetVideoMode()
			CheckError(err)

			width, height := window.GetSize()
			window.SetPosition((vm.Width-width)/2, (vm.Height-height)/2)
		}
		glfw.SwapInterval(1) // Vsync

		InitFont()
		defer DeinitFont()

		window.SetCloseCallback(func(w *glfw.Window) {
			keepRunning = false
		})

		framebufferSizeCallback := func(w *glfw.Window, framebufferSize0, framebufferSize1 int) {
			gl.Viewport(0, 0, int32(framebufferSize0), int32(framebufferSize1))

			var windowSize [2]int
			windowSize[0], windowSize[1] = w.GetSize()

			// Update the projection matrix
			gl.MatrixMode(gl.PROJECTION)
			gl.LoadIdentity()
			gl.Ortho(0, float64(windowSize[0]), float64(windowSize[1]), 0, -1, 1)
			gl.MatrixMode(gl.MODELVIEW)

			redraw = true
		}
		{
			var framebufferSize [2]int
			framebufferSize[0], framebufferSize[1] = window.GetFramebufferSize()
			framebufferSizeCallback(window, framebufferSize[0], framebufferSize[1])
		}
		window.SetFramebufferSizeCallback(framebufferSizeCallback)

		mousePointer = &Pointer{VirtualCategory: POINTING}
		keyboardPointer = &Pointer{VirtualCategory: TYPING}

		var lastMousePos mgl64.Vec2
		lastMousePos[0], lastMousePos[1] = window.GetCursorPosition()
		MousePos := func(w *glfw.Window, x, y float64) {
			//fmt.Println("MousePos:", x, y)

			inputEvent := InputEvent{
				Pointer:    mousePointer,
				EventTypes: map[EventType]bool{SLIDER_EVENT: true, AXIS_EVENT: true},
				InputId:    0,
				Buttons:    nil,
				Sliders:    []float64{x - lastMousePos[0], y - lastMousePos[1]}, // TODO: Do this in a pointer general way?
				Axes:       []float64{x, y},
			}
			lastMousePos[0] = x
			lastMousePos[1] = y
			inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
			redraw = true // TODO: Move redraw = true elsewhere? Like somewhere within events processing? Or keep it in all event handlers?
		}
		window.SetCursorPositionCallback(MousePos)
		MousePos(window, lastMousePos[0], lastMousePos[1])

		window.SetScrollCallback(func(w *glfw.Window, xoff float64, yoff float64) {
			inputEvent := InputEvent{
				Pointer:    mousePointer,
				EventTypes: map[EventType]bool{SLIDER_EVENT: true},
				InputId:    2,
				Buttons:    nil,
				Sliders:    []float64{yoff, xoff},
				Axes:       nil,
			}
			inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
			redraw = true // TODO: Move redraw = true elsewhere? Like somewhere within events processing? Or keep it in all event handlers?
		})

		window.SetMouseButtonCallback(func(w *glfw.Window, button glfw.MouseButton, action glfw.Action, mods glfw.ModifierKey) {
			inputEvent := InputEvent{
				Pointer:     mousePointer,
				EventTypes:  map[EventType]bool{BUTTON_EVENT: true},
				InputId:     uint16(button),
				Buttons:     []bool{action != glfw.Release},
				Sliders:     nil,
				Axes:        nil,
				ModifierKey: mods,
			}
			inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
			redraw = true // TODO: Move redraw = true elsewhere? Like somewhere within events processing? Or keep it in all event handlers?
		})

		window.SetKeyCallback(func(w *glfw.Window, key glfw.Key, scancode int, action glfw.Action, mods glfw.ModifierKey) {
			/*if key == glfw.KeyEnter && action == glfw.Press {
				x, y := window.GetPosition()
				window.SetPosition(x-16, y)
			}*/

			inputEvent := InputEvent{
				Pointer:     keyboardPointer,
				EventTypes:  map[EventType]bool{BUTTON_EVENT: true},
				InputId:     uint16(key),
				Buttons:     []bool{action != glfw.Release},
				Sliders:     nil,
				Axes:        nil,
				ModifierKey: mods,
			}
			//fmt.Println(key, action, mods)
			inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
			redraw = true // HACK
		})

		window.SetCharacterCallback(func(w *glfw.Window, char rune) {
			inputEvent := InputEvent{
				Pointer:    keyboardPointer,
				EventTypes: map[EventType]bool{CHARACTER_EVENT: true},
				InputId:    uint16(char),
				Buttons:    nil,
				Sliders:    nil,
				Axes:       nil,
			}
			inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
			redraw = true // HACK
		})

		window.SetDropCallback(func(w *glfw.Window, names []string) {
			goon.DumpExpr(names)
		})

		gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA)
		//gl.ClearColor(0.8, 0.3, 0.01, 1)
		//gl.ClearColor(0.85, 0.85, 0.85, 1)
		//gl.ClearColor(float32(backgroundColor[0]), float32(backgroundColor[1]), float32(backgroundColor[2]), 1)
	}

	// ---

	var widget Widgeter
	var widgets []Widgeter

	spinner := SpinnerWidget{Widget: NewWidget(mgl64.Vec2{20, 20}, mgl64.Vec2{0, 0}), Spinner: 0}

	const sublimeMode = true

	if !sublimeMode && false {

		// iGo Live Editor experiment

		//source := NewTextFileWidget(mathgl.Vec2d{50, 160}, "/Users/Dmitri/Dropbox/Work/2013/GoLand/src/gist.github.com/7176504.git/main.go")
		source := NewTextBoxWidget(mgl64.Vec2{50, 160})
		widgets = append(widgets, source)

		parsedFile := &parsedFile{}
		parsedFile.AddSources(source.Content)

		params := func() interface{} {
			return []interface{}{
				parsedFile.fset,
				parsedFile.fileAst,
			}
		}

		action := func(params interface{}) string {
			fset := params.([]interface{})[0].(*token.FileSet)
			fileAst := params.([]interface{})[1].(*ast.File)

			if fset == nil || fileAst == nil {
				return "<Go parsing error>"
			}

			/*defer func() {
				_ = recover()
			}()*/

			var buf bytes.Buffer
			from_go.Fprint(&buf, fset, fileAst)

			return buf.String()
		}

		w := NewLiveGoroutineExpeWidget(mgl64.Vec2{500, 160}, true, []DepNode2I{parsedFile}, params, action)
		widgets = append(widgets, w)

		parsedIgoFile := &parsedIgoFile{}
		parsedIgoFile.AddSources(w.Widgets[2].(*TextBoxWidget).Content) // HACK: Should get Content in a better way

		params2 := func() interface{} {
			return []interface{}{
				parsedIgoFile.fset,
				parsedIgoFile.fileAst,
			}
		}

		action2 := func(params interface{}) string {
			fset := params.([]interface{})[0].(*igo_token.FileSet)
			fileAst := params.([]interface{})[1].(*igo_ast.File)

			if fset == nil || fileAst == nil {
				return "<iGo parsing error>"
			}

			var buf bytes.Buffer
			to_go.Fprint(&buf, fset, fileAst)

			return buf.String()
		}

		w2 := NewLiveGoroutineExpeWidget(mgl64.Vec2{950, 160}, true, []DepNode2I{parsedIgoFile}, params2, action2)
		widgets = append(widgets, w2)

		{
			mc1 := NewMultilineContent()
			b1 := NewTextBoxWidgetExternalContent(mgl64.Vec2{400, 800}, mc1, nil)

			mc2 := NewReverseMultilineContent()
			mc2.AddAndSetViewGroup(mc1, "")
			b2 := NewTextBoxWidgetExternalContent(mgl64.Vec2{800, 800}, mc2, nil)

			widgets = append(widgets, b1, b2)
		}

		// TEST: Bunch of buttons with spacing.
		{
			widgets = append(widgets, NewFlowLayoutWidget(mgl64.Vec2{500, 900}, []Widgeter{
				NewSpacerWidget(np, NewButtonWidget(np, nil)),
				NewSpacerWidget(np, NewButtonWidget(np, nil)),
				NewSpacerWidget(np, NewButtonWidget(np, nil)),
				NewSpacerWidget(np, NewTriButtonExternalStateWidget(np, func() bool { return true }, nil)),
				NewSpacerWidget(np, NewTextLabelWidgetString(np, "Label!")),
				NewSpacerWidget(np, NewTextBoxWidget(np)),
				NewSpacerWidget(np, NewButtonWidget(np, nil)),
			}, nil))
		}

		// TEST: GoonWidget improvements...
		{
			type Inner struct {
				Field1 string
				Field2 int
			}
			type Lang struct {
				Name  string
				Year  int
				URLs  [2]string
				Inner Inner
				Bool  bool
			}
			x := Lang{
				Name: "Go",
				Year: 2009,
				URLs: [2]string{"http", "https"},
				Inner: Inner{
					Field1: "Secret!",
					Field2: 123367,
				},
				Bool: true,
			}

			someString := "Can you edit me?"
			widgets = append(widgets, NewGoonWidget(mgl64.Vec2{600, 296}, &someString))
			widgets = append(widgets, NewGoonWidget(mgl64.Vec2{600 - 100, 296 + 20}, &x))

			// *ast.File goon
			src := `package main

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/shurcooL/markdownfmt/markdown"
)

func main() {
	input, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}

	output, err := markdown.Process("", input, nil)
	if err != nil {
		panic(err)
	}

	fmt.Print(string(output))
}
`
			fset := token.NewFileSet()
			fileAst, err := parser.ParseFile(fset, "", src, 0 /*parser.ParseComments|parser.AllErrors*/)
			CheckError(err)

			widgets = append(widgets, NewGoonWidget(mgl64.Vec2{600, 296 + 40}, &fileAst))
			widgets = append(widgets, NewGoonWidget(mgl64.Vec2{600, 296 + 60}, &parsedFile))

			output := NewTextBoxWidget(mgl64.Vec2{1000, 300})
			widgets = append(widgets, output)

			dumpButton := NewButtonWidget(mgl64.Vec2{600 - 20, 296}, func() {
				goon.DumpExpr(x)
				goon.DumpExpr(someString)
				SetViewGroup(output.Content, SprintAstBare(fileAst))
			})
			widgets = append(widgets, dumpButton)
		}

	} else if !sublimeMode {

		editor := NewTextBoxWidgetExternalContent(mgl64.Vec2{0, 200}, NewMultilineContentString(`package main

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/shurcooL/markdownfmt/markdown"
)

func main() {
	input, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}

	output, err := markdown.Process("", input, nil)
	if err != nil {
		panic(err)
	}

	fmt.Print(string(output))
}
`), &TextBoxWidgetOptions{PopupTest: true, FindPanel: true})

		widgets = append(widgets, editor)

		widgets = append(widgets, NewButtonLabelWidget(mgl64.Vec2{640, 400}, "Previous", nil))

	} else if sublimeMode {

		var windowSize0, windowSize1 int
		if window != nil {
			windowSize0, windowSize1 = window.GetSize()
		}
		windowSize := mgl64.Vec2{float64(windowSize0), float64(windowSize1)} // HACK: This is not updated as window resizes, etc.
		_ = windowSize

		goPackageListing := NewGoPackageListingWidget(mgl64.Vec2{0, 200}, mgl64.Vec2{200, 500 - fontHeight - 2})
		widgets = append(widgets, goPackageListing)

		folderListing := NewFolderListingWidget(np, "../../../") // Hopefully the "$GOPATH/src/" folder
		widgets = append(widgets, NewScrollPaneWidget(mgl64.Vec2{0, 200 + 500 + 2}, mgl64.Vec2{200, float64(windowSize1 - 702 - 2)}, folderListing))

		// TEST, HACK: Open the folder listing to the folder of the Go package
		folderListingDirChanger := DepNode2Func{}
		folderListingDirChanger.UpdateFunc = func(this DepNode2I) {
			if goPackage := this.GetSources()[0].(GoPackageSelecter).GetSelectedGoPackage(); goPackage != nil {
				path := goPackage.Bpkg.Dir
				folderListing.flow.SetWidgets([]Widgeter{newFolderListingPureWidgetWithSelection(path)})
				ExternallyUpdated(folderListing)
			}
		}
		folderListingDirChanger.AddSources(&GoPackageSelecterAdapter{goPackageListing.OnSelectionChanged()})
		keepUpdatedTEST = append(keepUpdatedTEST, &folderListingDirChanger)

		// Main editor
		editorContent := NewMultilineContent()
		editorFileOpener := NewFileOpener(editorContent)
		editorFileOpener.AddSources(folderListing, &GoPackageSelecterAdapter{goPackageListing.OnSelectionChanged()})
		keepUpdatedTEST = append(keepUpdatedTEST, editorFileOpener)
		editor := NewTextBoxWidgetExternalContent(np, editorContent, &TextBoxWidgetOptions{PopupTest: true, FindPanel: true})
		widgets = append(widgets, NewScrollPaneWidget(mgl64.Vec2{200 + 2, 0}, mgl64.Vec2{750, float64(windowSize1 - 2)}, editor))

		// View sidebar
		{
			// git diff
			template := NewPipeTemplateDynamic()
			template.UpdateFunc = func(this DepNode2I) {
				template.Template = NewPipeTemplate(pipe.Exec("echo", "-n", "No git diff available."))

				if path := this.GetSources()[0].(*FolderListingWidget).GetSelectedPath(); path != "" && strings.HasSuffix(path, ".go") {
					dir, file := filepath.Split(path)
					if isGitRepo, _ := vcs.IsFolderGitRepo(dir); isGitRepo { // TODO: Centralize this somewhere (GoPackage with DepNode2I?)
						template.Template = NewPipeTemplate(pipe.Line(
							// TODO: Reuse u6.GoPackageWorkingDiff.
							pipe.Exec("git", "diff", "--no-ext-diff", "--", file),
							pipe.TaskFunc(func(s *pipe.State) error {
								r := bufio.NewReader(s.Stdin)
								for _ = range iter.N(4) {
									r.ReadBytes('\n')
								}
								var b bytes.Buffer
								b.ReadFrom(r)
								if b.Len() == 0 {
									return nil
								}
								b.Truncate(b.Len() - 1)
								b.WriteTo(s.Stdout)
								return nil
							}),
						))
						template.Template.Dir = dir
					}
				}
			}
			template.AddSources(folderListing)

			gitDiff := NewLivePipeExpeWidget(np, []DepNode2I{editorContent}, template) // TODO: Are both editorContent and folderListing deps needed? Or is editorContent enough, since it probably depends on folderListing, etc.
			diffHighlighter := &diffHighlighter{}
			diffHighlighter.AddSources(gitDiff.Content)
			gitDiff.HighlightersTest = append(gitDiff.HighlightersTest, diffHighlighter)
			gitDiff.LineHighlighter = diffHighlighter

			gitDiffCollapsible := NewCollapsibleWidget(np, gitDiff, "git diff")

			// ---

			// git show HEAD.
			var nextTool10Collapsible *CollapsibleWidget
			{
				template := NewPipeTemplateDynamic()
				template.UpdateFunc = func(this DepNode2I) {
					template.Template = NewPipeTemplate(pipe.Exec("echo", "-n", "No luck."))

					if path := this.GetSources()[0].(*FolderListingWidget).GetSelectedPath(); path != "" && strings.HasSuffix(path, ".go") {
						dir, file := filepath.Split(path)
						if isGitRepo, _ := vcs.IsFolderGitRepo(dir); isGitRepo { // TODO: Centralize this somewhere (GoPackage with DepNode2I?)
							template.Template = NewPipeTemplate(pipe.Line(
								pipe.Exec("git", "show", "HEAD:./"+file),
							))
							template.Template.Dir = dir
						}
					}
				}
				template.AddSources(folderListing)

				gitHead := NewLivePipeExpeWidget(np, []DepNode2I{folderListing}, template)
				nextTool10Collapsible = NewCollapsibleWidget(np, gitHead, "git show HEAD")

				{
					box1 := gitHead
					box2 := editor
					stateFunc := func() bool { return nextTool10Collapsible.state.State() } // This should only be visible when "git show HEAD" is expanded.

					if false {
						highlightedDiff := &highlightedDiff{}
						highlightedDiff.AddSources(box1.Content, box2.Content)

						box1.DynamicHighlighters = append(box1.DynamicHighlighters, NewOptionalHighlighter(&highlightedDiffSide{highlightedDiff: highlightedDiff, side: 0}, stateFunc))
						box2.DynamicHighlighters = append(box2.DynamicHighlighters, NewOptionalHighlighter(&highlightedDiffSide{highlightedDiff: highlightedDiff, side: 1}, stateFunc))
					} else {
						lineDiff := &lineDiff{}
						lineDiff.AddSources(box1.Content, box2.Content)

						lineDiffSide0 := &lineDiffSide{lineDiff: lineDiff, side: 0}
						lineDiffSide1 := &lineDiffSide{lineDiff: lineDiff, side: 1}

						box1.DynamicHighlighters = append(box1.DynamicHighlighters, NewOptionalHighlighter(lineDiffSide0, stateFunc))
						box2.DynamicHighlighters = append(box2.DynamicHighlighters, NewOptionalHighlighter(lineDiffSide1, stateFunc))

						box1.DynamicLineHighlighter = func() LineHighlighter {
							if !stateFunc() {
								return nil
							}
							return lineDiffSide0
						}
						box2.DynamicLineHighlighter = func() LineHighlighter {
							if !stateFunc() {
								return nil
							}
							return lineDiffSide1
						}
					}
				}
			}

			// ---

			// Local Branch of selected GoPackage
			localBranch := &DepStringerFunc{}
			localBranch.UpdateFunc = func(this DepNode2I) {
				localBranch.content = ""
				if goPackage := this.GetSources()[0].(GoPackageSelecter).GetSelectedGoPackage(); goPackage != nil {
					MakeUpdatedLock.Unlock() // HACK: Needed because UpdateVcs() calls MakeUpdated().
					goPackage.UpdateVcs()
					MakeUpdatedLock.Lock() // HACK
					if goPackage.Dir.Repo != nil {
						MakeUpdatedLock.Unlock() // HACK: Needed because we're calling MakeUpdated() directly.
						MakeUpdated(goPackage.Dir.Repo.VcsLocal)
						MakeUpdatedLock.Lock() // HACK
						localBranch.content = goPackage.Dir.Repo.VcsLocal.LocalBranch
					}
				}
			}
			localBranch.AddSources(&GoPackageSelecterAdapter{goPackageListing.OnSelectionChanged()})

			nextTool := NewStringerWidget(np, localBranch)
			nextToolCollapsible := NewCollapsibleWidget(np, nextTool, "Local Branch")

			// ---

			// Build Output.
			buildOutput := NewMultilineContent()
			nextTool9Collapsible := NewCollapsibleWidget(np, NewTextBoxWidgetExternalContent(np, buildOutput, nil), "Build Output")

			// Go Compile Errors hardcoded TEST
			{
				goCompileErrorsTest := GoCompileErrorsTest{}
				goCompileErrorsTest.AddSources(buildOutput)
				goCompileErrorsManagerTest.AddSources(&goCompileErrorsTest)
			}

			// ---

			// go build.
			template2 := NewPipeTemplateDynamic()
			template2.UpdateFunc = func(this DepNode2I) {
				template2.Template = NewPipeTemplate(pipe.Exec("echo", "-n", "Nothing to go build."))

				if goPackage := this.GetSources()[0].(GoPackageSelecter).GetSelectedGoPackage(); goPackage != nil {
					template2.Template = NewPipeTemplate(pipe.Line(
						pipe_util.ExecCombinedOutput("go", "build", "-o", "/dev/null", goPackage.Bpkg.ImportPath),
						pipe.TaskFunc(func(s *pipe.State) error {
							var b bytes.Buffer
							b.ReadFrom(s.Stdin)
							SetViewGroup(buildOutput, b.String())
							b.WriteTo(s.Stdout)
							return nil
						}),
					))
					//template2.Template.Dir = ""
				}
			}
			template2.AddSources(&GoPackageSelecterAdapter{goPackageListing.OnSelectionChanged()})

			build := NewLivePipeExpeWidget(np, []DepNode2I{editorContent}, template2)
			nextTool2Collapsible := NewCollapsibleWidget(np, build, "go build")

			// ---

			// go run.
			template3 := NewPipeTemplateDynamic()
			template3.UpdateFunc = func(this DepNode2I) {
				template3.Template = NewPipeTemplate(pipe.Exec("echo", "-n", "Nothing to go run."))

				if goPackage := this.GetSources()[0].(GoPackageSelecter).GetSelectedGoPackage(); goPackage != nil {
					template3.Template = NewPipeTemplate(pipe.Script(
						pipe.Println(fmt.Sprintf("Building %q.", goPackage.Bpkg.ImportPath)),
						pipe.Line(
							pipe_util.ExecCombinedOutput("go", "build", "-o", con2RunBinPath, goPackage.Bpkg.ImportPath),
							pipe.TaskFunc(func(s *pipe.State) error {
								var b bytes.Buffer
								b.ReadFrom(s.Stdin)
								SetViewGroup(buildOutput, b.String())
								b.WriteTo(s.Stdout)
								return nil
							}),
						),
						pipe.Println("Running."),
						pipe.Exec(con2RunBinPath),
						pipe.Println("Done."),
					))
					template3.Template.Dir = goPackage.Bpkg.Dir
				}
			}
			template3.AddSources(&GoPackageSelecterAdapter{goPackageListing.OnSelectionChanged()})

			run := NewLivePipeExpeWidget(np, []DepNode2I{editorContent}, template3)
			nextTool2bCollapsible := NewCollapsibleWidget(np, run, "go run")

			// ---

			// go test.
			template4 := NewPipeTemplateDynamic()
			template4.UpdateFunc = func(this DepNode2I) {
				template4.Template = NewPipeTemplate(pipe.Exec("echo", "-n", "Nothing to go test."))
				if goPackage := this.GetSources()[0].(GoPackageSelecter).GetSelectedGoPackage(); goPackage != nil {
					template4.Template = NewPipeTemplate(pipe.Script(
						pipe_util.ExecCombinedOutput("go", "test", goPackage.Bpkg.ImportPath),
					))
				}
			}
			template4.AddSources(&GoPackageSelecterAdapter{goPackageListing.OnSelectionChanged()})

			goTest := NewLivePipeExpeWidget(np, []DepNode2I{editorContent}, template4)
			nextTool2cCollapsible := NewCollapsibleWidget(np, goTest, "go test")

			// ---

			nextTool3, typeCheckedPackage := NewTest4Widget(np, &GoPackageSelecterAdapter{goPackageListing.OnSelectionChanged()}, editor)
			globalTypeCheckedPackage = typeCheckedPackage // HACK
			nextTool3Collapsible := NewCollapsibleWidget(np, nextTool3, "typeCheckedPackage verbose")

			typeUnderCaretWidget := NewTypeUnderCaretWidget(np, &GoPackageSelecterAdapter{goPackageListing.OnSelectionChanged()}, editor, typeCheckedPackage)
			nextTool3bCollapsible := NewCollapsibleWidget(np, typeUnderCaretWidget, "Type")

			// ---

			// DEBUG: Goon widget of typeCheckedPackage
			nextTool4 := NewGoonWidget(np, &typeCheckedPackage)
			nextTool4Collapsible := NewCollapsibleWidget(np, nextTool4, "goon of typeCheckedPackage")

			// TEST: Add goto declaration extension to editor
			{
				gotoDecl := &CustomWidget{
					Widget: NewWidget(np, np),
					ProcessEventFunc: func(inputEvent InputEvent) {
						if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
							switch glfw.Key(inputEvent.InputId) {
							case glfw.KeyDown:
								if inputEvent.ModifierKey == glfw.ModSuper|glfw.ModAlt {
									if Test4WidgetIdent != nil && typeCheckedPackage.info != nil {
										if obj := typeCheckedPackage.info.Uses[Test4WidgetIdent]; obj != nil {
											if file := typeCheckedPackage.fset.File(obj.Pos()); file != nil {
												// TODO: Change folderListing selection if it's in a different file

												editor.caretPosition.TrySet(uint32(file.Offset(obj.Pos())))
												editor.CenterOnCaretPosition()
											}
										}
									}
								}
							}
						}
					},
				}
				editor.ExtensionsTest = append(editor.ExtensionsTest, gotoDecl)
			}

			// ---

			var nextTool5B *SearchableListWidget
			nextTool5B, globalGoSymbols = NewTest5BWidget(np, typeCheckedPackage)
			nextTool5BCollapsible := NewCollapsibleWidget(np, nextTool5B, "Go To Symbol")

			scrollToSymbolB := DepNode2Func{}
			scrollToSymbolB.UpdateFunc = func(this DepNode2I) {
				if entry := this.GetSources()[0].(Selecter).GetSelected(); entry != nil {
					// TODO: Replace with entry.(Something).CaretPositionStart, End -> editor.ScrollToCaret(Start, End)
					//println("selected:", entry.String())

					if declNode := entry.(*NodeStringer); true {

						if file := typeCheckedPackage.fset.File(declNode.Pos()); file != nil {
							// TODO: Change folderListing selection if it's in a different file
							// TEST, HACK: Externally change folderListing selection
							goFileListing := folderListing.flow.Widgets[0].(*FolderListingPureWidget)
							for index, entry := range goFileListing.entries {
								if strings.HasSuffix(file.Name(), entry.Name()) {
									if goFileListing.selected != uint64(index)+1 {
										goFileListing.selected = uint64(index) + 1
										goFileListing.selectionChangedTest()
									}
									break
								}
							}

							// TODO: There's a race condition here, to do the thing below I need to have finished changing the file, but
							//       that currently reloads ASTs, etc. causing crashes sometimes depending on order of file ASTs being processed.

							editor.caretPosition.TrySet(uint32(file.Offset(declNode.Pos())))
							editor.CenterOnCaretPosition()
						}
					}
				}
			}
			scrollToSymbolB.AddSources(nextTool5B.OnSelectionChanged())
			keepUpdatedTEST = append(keepUpdatedTEST, &scrollToSymbolB)

			// ---

			nextTool6 := NewTest6OracleWidget(np, &GoPackageSelecterAdapter{goPackageListing.OnSelectionChanged()}, editor)
			nextTool6Collapsible := NewCollapsibleWidget(np, nextTool6, "Oracle Tool")

			// ---

			godocOrgImporters := &DepStringerFunc{}
			godocOrgImporters.UpdateFunc = func(this DepNode2I) {
				//fmt.Print("\x07")
				godocOrgImporters.content = ""
				if goPackage := this.GetSources()[0].(GoPackageSelecter).GetSelectedGoPackage(); goPackage != nil {
					godocOrgImporters.content = goon.Sdump(u5.GetGodocOrgImporters(goPackage))
				}
			}
			godocOrgImporters.AddSources(&GoPackageSelecterAdapter{goPackageListing.OnSelectionChanged()})

			// TODO: This needs to be refactored to use more modern DepNode2I, which will make this simpler and reduce duplication.
			nextTool7 := NewTextBoxWidgetExternalContent(np, NewMultilineContentFunc(func() string { return godocOrgImporters.String() }, []DepNodeI{&UniversalClock}), nil)
			nextTool7.DepsTest = append(nextTool7.DepsTest, godocOrgImporters)
			nextTool7Collapsible := NewCollapsibleWidget(np, nextTool7, "godoc.org Importers")

			// ---

			caretPositionStringer := &DepStringerFunc{}
			caretPositionStringer.UpdateFunc = func(this DepNode2I) {
				caretPosition := this.GetSources()[0].(*CaretPosition)
				caretPositionStringer.content = fmt.Sprintf("Line: %v, Caret Position: %v", caretPosition.caretPosition.lineIndex, caretPosition.Logical())
				if caretPosition.anySelection() {
					start, end := caretPosition.SelectionRange()
					caretPositionStringer.content += fmt.Sprintf(", %d characters selected", end-start)
				}
			}
			caretPositionStringer.AddSources(editor.caretPosition)

			nextTool8 := NewStringerWidget(np, caretPositionStringer)

			// =====

			tools := NewFlowLayoutWidget(np, []Widgeter{nextTool8, nextTool9Collapsible, nextTool2Collapsible, nextTool2bCollapsible, nextTool2cCollapsible, nextToolCollapsible, gitDiffCollapsible, nextTool10Collapsible, nextTool3bCollapsible, nextTool3Collapsible, nextTool4Collapsible, nextTool5BCollapsible, nextTool6Collapsible, nextTool7Collapsible}, &FlowLayoutWidgetOptions{FlowLayoutType: VerticalLayout})
			widgets = append(widgets, NewScrollPaneWidget(mgl64.Vec2{950 + 4, 0}, mgl64.Vec2{580, float64(windowSize1 - 2)}, tools))
		}

	} else if false { // Deleted test widget instances
		{
			src := NewTextFileWidget(np, "/Users/Dmitri/Dropbox/Work/2013/GoLand/src/gist.github.com/7176504.git/main.go")
			//src := NewTextFileWidget(mathgl.Vec2d{}, "./GoLand/src/simple.go")
			//src := NewTextFileWidget(mathgl.Vec2d{}, "/Users/Dmitri/Dropbox/Work/2013/GoLand/src/gist.github.com/5694308.git/main.go")
			//src := NewTextFileWidget(mathgl.Vec2d{0, 0}, "/Users/Dmitri/Dropbox/Work/2013/GoLand/src/gist.github.com/5068062.git/gistfile1.go")
			//src := NewTextBoxWidget(mathgl.Vec2d{50, 200})

			build := NewLiveCmdExpeWidget(np, []DepNode2I{src.Content}, NewCmdTemplate("go", "build", "-o", "./Con2RunBin", "gist.github.com/7176504.git" /*src.Path()*/)) // TODO: Do this right
			spinner.AddSources(build)

			// Go Compile Errors hardcoded TEST
			{
				goCompileErrorsTest := GoCompileErrorsTest{}
				//goCompileErrorsTest.Source = build.Content
				//build.AddChangeListener(&goCompileErrorsTest)
				goCompileErrorsTest.AddSources(build)
				//goCompileErrorsManagerTest.Sources = append(goCompileErrorsManagerTest.Sources, &goCompileErrorsTest) // TODO: This should call the next line, etc.
				//goCompileErrorsTest.AddChangeListener(&goCompileErrorsManagerTest)
				goCompileErrorsManagerTest.AddSources(&goCompileErrorsTest)
			}

			run := NewLiveCmdExpeWidget(np, []DepNode2I{build}, NewCmdTemplate("./Con2RunBin")) // TODO: Proper path

			widgets = append(widgets, NewFlowLayoutWidget(mgl64.Vec2{50, 200}, []Widgeter{src, build, run}, nil))
		}

		// DEBUG: Testing out new DepNode2 system
		{
			// TODO: Use DepNode2 so that if this is false, then goCompileErrorsManagerTest.All (and goCompileErrorsTest also) shouldn't get updated
			goCompileErrorsEnabled := true
			goCompileErrorsEnabledTest = NewTriButtonExternalStateWidget(mgl64.Vec2{500, 700}, func() bool { return goCompileErrorsEnabled }, func() { goCompileErrorsEnabled = !goCompileErrorsEnabled })
			widgets = append(widgets, goCompileErrorsEnabledTest)

			//widgets = append(widgets, NewTextLabelWidgetGoon(mathgl.Vec2d{500, 716 + 2}, &goCompileErrorsManagerTest.DepNode2.NeedToUpdate))
			widgets = append(widgets, NewTextLabelWidgetGoon(mgl64.Vec2{500, 732 + 4}, &goCompileErrorsManagerTest.All))
		}

		// TEST: Render the Go code tokens with color highlighting
		{
			source := widgets[2].(*FlowLayoutWidget).Widgets[0].(*TextFileWidget).TextBoxWidget

			w := &CustomWidget{
				Widget: NewWidget(np, mgl64.Vec2{fontHeight, fontHeight}),
			}
			w.RenderFunc = func() {
				// HACK: Layout in Render()
				w.size[0] = float64(fontWidth * source.Content.LongestLine())
				w.size[1] = float64(fontHeight * source.Content.LenLines())

				DrawNBox(w.pos, w.size)

				glt := NewOpenGlStream(w.pos)

				// TODO: Cache results of scanning
				src := []byte(source.Content.Content())

				var s scanner.Scanner
				fset := token.NewFileSet()
				file := fset.AddFile("", fset.Base(), len(src))
				s.Init(file, src, nil /* no error handler */, scanner.ScanComments)

				// Repeated calls to Scan yield the token sequence found in the input.
				for {
					pos, tok, lit := s.Scan()
					if tok == token.EOF {
						break
					}

					tempCaretPosition := &caretPositionInternal{w: source.Content}
					x, y := tempCaretPosition.SetHint(uint32(fset.Position(pos).Offset), 0)
					glt.SetPosWithExpandedPosition(w.pos, x, y)

					switch {
					case tok.IsKeyword() || tok.IsOperator() && tok < token.LPAREN:
						gl.Color3d(0.004, 0, 0.714)
					case tok.IsLiteral() && tok != token.IDENT:
						gl.Color3d(0.804, 0, 0)
					case lit == "false" || lit == "true":
						gl.Color3d(0.008, 0.024, 1)
					case tok == token.COMMENT:
						gl.Color3d(0, 0.506, 0.094)
					default:
						gl.Color3d(0, 0, 0)
					}

					if lit != "" {
						glt.PrintText(lit)
					} else {
						glt.PrintText(tok.String())
					}
				}
			}

			widgets[2].(*FlowLayoutWidget).Widgets = append(widgets[2].(*FlowLayoutWidget).Widgets, w)
			w.SetParent(widgets[2]) // Needed for pointer coordinates to be accurate
			widgets[2].(*FlowLayoutWidget).Layout()
		}

		// Shows the AST node underneath caret (asynchonously via LiveGoroutineExpeWidget)
		{
			//w, _ := NewTest3Widget(np, widgets[2].(*FlowLayoutWidget).Widgets[0].(*TextFileWidget).TextBoxWidget)
			w, _ := NewTest4Widget(np, &goPackageHardcoded{}, widgets[2].(*FlowLayoutWidget).Widgets[0].(*TextFileWidget).TextBoxWidget)
			widgets[2].(*FlowLayoutWidget).Widgets = append(widgets[2].(*FlowLayoutWidget).Widgets, w)
			w.SetParent(widgets[2]) // Needed for pointer coordinates to be accurate
			widgets[2].(*FlowLayoutWidget).Layout()
		}

		widgets = append(widgets, &BoxWidget{NewWidget(mgl64.Vec2{50, 150}, mgl64.Vec2{16, 16}), "The Original Box"})
		widgets = append(widgets, NewCompositeWidget(mgl64.Vec2{150, 150},
			[]Widgeter{
				&BoxWidget{NewWidget(mgl64.Vec2{0, 0}, mgl64.Vec2{16, 16}), "Left of Duo"},
				&BoxWidget{NewWidget(mgl64.Vec2{16 + 2, 0}, mgl64.Vec2{16, 16}), "Right of Duo"},
			}))
		widgets = append(widgets, &UnderscoreSepToCamelCaseWidget{NewWidget(mgl64.Vec2{50, 180}, mgl64.Vec2{0, 0}), window})
		widgets = append(widgets, NewChannelExpeWidget(mgl64.Vec2{10, 220}))
		widgets = append(widgets, NewTextBoxWidget(mgl64.Vec2{50, 5}))
		widgets = append(widgets, NewTextFileWidget(mgl64.Vec2{90, 25}, "/Users/Dmitri/Dropbox/Needs Processing/Sample.txt"))
		widgets = append(widgets, NewTextBoxWidgetExternalContent(mgl64.Vec2{90, 60}, widgets[len(widgets)-1].(*TextFileWidget).TextBoxWidget.Content, nil)) // HACK: Manual test
		widgets = append(widgets, NewTextLabelWidgetExternalContent(mgl64.Vec2{90, 95}, widgets[len(widgets)-2].(*TextFileWidget).TextBoxWidget.Content))    // HACK: Manual test

		widgets = append(widgets, NewTest2Widget(mgl64.Vec2{240, 5}, &widgets[7].(*TextBoxWidget).pos[0]))

		type Inner struct {
			Field1 string
			Field2 int
		}
		type Lang struct {
			Name  string
			Year  int
			URLs  [2]string
			Inner Inner
		}
		x := Lang{
			Name: "Go",
			Year: 2009,
			URLs: [2]string{"http", "https"},
			Inner: Inner{
				Field1: "Secret!",
				Field2: 123367,
			},
		}

		/*Lang{
			Name: "Go",
			Year: 2009,
			URL:  "http",
			Inner: Inner{...},
		}*/

		//widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{260, 130}, FlowLayoutWidget{}))
		//widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{260, 130}, InputEvent{}))
		widgets = append(widgets, NewGoonWidget(mgl64.Vec2{380, 10}, &x))
		y := NewWidget(mgl64.Vec2{1, 2}, mgl64.Vec2{3})
		widgets = append(widgets, NewGoonWidget(mgl64.Vec2{600, 10}, &y))
	} else if true {
		widgets = append(widgets, &spinner)

		katWidget := NewKatWidget(mgl64.Vec2{370, 15})
		widgets = append(widgets, katWidget)
		if katOnly {
			// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
			keyboardPointer.OriginMapping = []Widgeter{katWidget}
		}

		// GoForcedUseWidget
		{
			src := NewTextBoxWidget(np)
			label := NewTextLabelWidgetString(np, "go Forced Use")

			params := func() interface{} { return src.Content.Content() }
			action := func(params interface{}) string {
				if strings.TrimSpace(params.(string)) != "" {
					//started := time.Now(); defer func() { fmt.Println(time.Since(started).Seconds()) }()
					return GetForcedUseFromImport(strings.TrimSpace(src.Content.Content()))
				} else {
					return ""
				}
			}
			dst := NewLiveGoroutineExpeWidget(np, true, []DepNode2I{src.Content}, params, action)

			w := NewFlowLayoutWidget(mgl64.Vec2{80, 130}, []Widgeter{src, label, dst}, nil)
			widgets = append(widgets, w)
		}
		// GoForcedUseWidget2
		{
			src := NewTextBoxWidget(np)
			label := NewTextLabelWidgetString(np, "go Forced Use")

			params := func() interface{} { return src.Content.Content() }
			action := func(params interface{}) string {
				if strings.TrimSpace(params.(string)) != "" {
					//started := time.Now(); defer func() { fmt.Println(time.Since(started).Seconds()) }()
					cmd := exec.Command("goe", "--quiet", "fmt", "github.com/shurcooL/go/gists/gist4727543", "github.com/shurcooL/go/gists/gist5498057", "Print(GetForcedUseFromImport(ReadAllStdin()))")
					//cmd := exec.Command("cat")
					cmd.Stdin = strings.NewReader(strings.TrimSpace(src.Content.Content()))
					out, err := cmd.CombinedOutput()
					CheckError(err)
					return string(out)
				} else {
					return ""
				}
			}
			dst := NewLiveGoroutineExpeWidget(np, true, []DepNode2I{src.Content}, params, action)

			w := NewFlowLayoutWidget(mgl64.Vec2{80, 150}, []Widgeter{src, label, dst}, nil)
			widgets = append(widgets, w)
		}

		widgets = append(widgets, NewFolderListingWidget(mgl64.Vec2{350, 30}, "../../../")) // Hopefully the "$GOPATH/src/" folder

		//widgets = append(widgets, NewCompositeWidget(mathgl.Vec2d{160, 30}, []Widgeter{NewGoPackageListingPureWidget()}))

		contentWs := NewMultilineContent()
		widgets = append(widgets, NewTextBoxWidgetExternalContent(mgl64.Vec2{800 - 50, 30}, contentWs, nil))
		http.HandleFunc("/websocket", func(w http.ResponseWriter, req *http.Request) {
			io.WriteString(w, `<html>
	<body>
		<script type="text/javascript">
			var prev_value = "";
			var input_field = null;
			function liveUpdateTest() {
				try {
					if (input_field.value != prev_value) {
						sock.send(input_field.value + "\0");
						prev_value = input_field.value;
					}
				} catch (exc) {
					document.getElementById("myLiveOut").textContent = "sock.send Error: " + exc;
				}
			}

			var sock = new WebSocket("ws://" + window.location.host + "/websocket.ws");
			//sock.addEventListener('open', function(e2) { sock.send( ... ) });
			sock.onopen = function(evt) {
				document.getElementById("myLiveOut").textContent = "Connected.";
				input_field = document.getElementById("inputField");
				input_field.addEventListener('input', liveUpdateTest);
				//input_field.select();
				//liveUpdateTest();
				console.log("Open: ", evt);
			};
			sock.onclose = function(evt) {
				document.getElementById("myLiveOut").textContent = "Disconnected.";
				console.log("Close: ", evt);
			};
			sock.onmessage = function(evt) {
				prev_value = evt.data;
				document.getElementById("inputField").value = evt.data;
				//console.log("Message: ", evt.data);
			};
			sock.onerror = function(evt) {
				document.getElementById("myLiveOut").textContent += " Error: " + evt;
				console.log("Error: ", evt);
			};
		</script>

		<textarea id="inputField" placeholder="type something..." autofocus></textarea>
		<br><br>
		<div id="myLiveOut">Connecting...</div>
	</body>
</html>`)
		})
		http.Handle("/websocket.ws", websocket.Handler(func(c *websocket.Conn) {
			wsView := NewWebSocketView(c)

			contentWs.AddAndSetViewGroup(wsView, contentWs.Content()) // TODO: Fix race condition

			br := bufio.NewReader(c)
			for {
				line, err := br.ReadString('\x00')
				if err == nil {
					wsView.WsReadChan <- line[:len(line)-1] // Trim delimiter
				} else {
					//wsView.WsReadChan <- line
					//close(wsView.WsReadChan)
					break
				}
			}

			contentWs.RemoveView(wsView) // TODO: Fix race condition
		}))

		// WebSocket Touch Pointer.
		http.HandleFunc("/websocket2", func(w http.ResponseWriter, req *http.Request) {
			io.WriteString(w, `<html>
	<head>
		<meta name="viewport" content="width=device-width, maximum-scale=1, minimum-scale=1, target-densitydpi=device-dpi, user-scalable=no">
		<style>
			body {
				margin: 0px;
			}
			canvas {
				background-color: #111133;
				display: block;
				position: absolute;
			}
			.container {
				width: auto;
				text-align: center;
				background-color: #ff0000;
			}
		</style>
	</head>
	<body>
		<script>
			var canvas, c, container;

			var mouseX, mouseY;
			// is this running in a touch capable environment?
			var touchable = "ontouchstart" in window;
			var touches = []; // array of touch vectors

			function orientationchange() {
				// Hack to work around lack of orientationchange/resize event
				if (   canvas.width == window.innerWidth
					|| canvas.height == window.innerHeight) {
					alert("orientationchange called but nothing changed??!");
				}

				canvas.width = window.innerWidth;
				canvas.height = window.innerHeight;
			}

			var lastUpdate = new Date, fps = "fps: ", framesDrawn = 0;

			function loop() {
				window.webkitRequestAnimationFrame(loop);

				try {
					//sock.send(JSON.stringify(touches.length) + "\n");		// HACK: Should make sure that sock.onopen has happened before calling send...
					if (touches.length >= 1) {
						sock.send(touches[0].clientX + " " +touches[0].clientY + "\0")
					}
				} catch (exc) {
					//alert("Error: " + exc);
				}

				c.clearRect(0, 0, canvas.width, canvas.height);
				c.strokeStyle = "cyan";
				c.lineWidth = "4";
				// Need to handle canvas offsetLeft/offsetTop once we have SD panel
				for (var i = 0, l = touches.length; i < l; i++) {
					c.beginPath();
					c.arc(touches[i].clientX, touches[i].clientY, 50, 0, Math.PI * 2, true);
					c.stroke();

					c.beginPath();
					c.fillStyle = "white";
					c.fillText("touch id : "+touches[i].identifier+" x:"+touches[i].clientX+" y:"+touches[i].clientY, touches[i].clientX+40, touches[i].clientY-40);
				}
				if (touches.length > 1) {
					c.beginPath();
					c.lineWidth = "2";
					c.moveTo(touches[0].clientX, touches[0].clientY);
					for(var i = 1, l = touches.length; i < l; i++) {
						c.lineTo(touches[i].clientX, touches[i].clientY);
					}
					c.stroke();
				}

				framesDrawn++;
				now = new Date;
				if ((now - lastUpdate) >= 100) {
					fps = "fps: " + (framesDrawn * 1000 / (now - lastUpdate)).toFixed(2);
					lastUpdate = now;
					framesDrawn = 0;
				}
				c.fillStyle = "white";
				c.fillText(fps, 0, 10);
			}

			function touchHandler(e) {
				e.preventDefault();
				touches = e.touches;
				//loop();
				//window.webkitRequestAnimationFrame(loop);
			}

			function init() {
				canvas = document.createElement("canvas");
				c = canvas.getContext("2d");
				container = document.createElement("div");
				container.className = "container";
				canvas.width = window.innerWidth;
				canvas.height = window.innerHeight;
				container.appendChild(canvas);
				document.body.appendChild(container);
				c.strokeStyle = "#ffffff";
				c.lineWidth = 2;

				if (touchable) {
					canvas.addEventListener("touchstart", touchHandler);
					canvas.addEventListener("touchmove", touchHandler);
					canvas.addEventListener("touchend", touchHandler);
					window.addEventListener("orientationchange", orientationchange);
					//setInterval(loop, 1000 / 35);
					loop();
				} else {
					document.write("Not touchable.");
				}
			}

			window.addEventListener("load", function() {
				// Hack to prevent firing the init script before the window object's values are populated
				setTimeout(init, 100);
			});

			var sock = new WebSocket("ws://" + window.location.host + "/websocket2.ws");
			sock.onopen = function(evt) { console.log("Open: ", evt); };
			sock.onclose = function(evt) { console.log("Close: ", evt); };
			sock.onerror = function(evt) { console.log("Error: ", evt); };
		</script>
	</body>
</html>`)
		})
		http.Handle("/websocket2.ws", websocket.Handler(func(c *websocket.Conn) {
			websocketPointer = &Pointer{VirtualCategory: POINTING} // TODO: Fix race condition

			br := bufio.NewReader(c)
			for {
				line, err := br.ReadString('\x00')
				if err == nil {
					touchPositionString := line[:len(line)-1] // Trim delimiter
					var x, y float64
					fmt.Sscan(touchPositionString, &x, &y)

					inputEvent := InputEvent{
						Pointer:    websocketPointer,
						EventTypes: map[EventType]bool{AXIS_EVENT: true},
						InputId:    0,
						Buttons:    nil,
						Sliders:    nil,
						Axes:       []float64{x, y},
					}
					inputEventQueue2 <- inputEvent
				} else {
					//wsView.WsReadChan <- line
					//close(wsView.WsReadChan)
					break
				}
			}

			websocketPointer = nil // TODO: Fix race condition
		}))

		// Shuryear Clock
		{
			contentFunc := func() string {
				shuryearNow := 1970 + float64(time.Now().UnixNano())/(3600*24*3652422*100000)
				return fmt.Sprintf("%.8f", shuryearNow)
			}
			mc := NewMultilineContentFunc(contentFunc, []DepNodeI{&UniversalClock})
			widgets = append(widgets, NewTextLabelWidgetExternalContent(mgl64.Vec2{1443, 0}, mc)) // TODO: Stick to top right corner?
		}

		{
			buttonTrigger := NewButtonTriggerWidget(mgl64.Vec2{50, 30})
			spinner.AddSources(buttonTrigger)
			widgets = append(widgets, buttonTrigger)
		}

		// `gofmt -r rule` experiment
		if false {
			in := NewTextBoxWidget(np)
			SetViewGroup(in.Content, `package main

import "bytes"

func main() {
	a := []byte("something")
	b := []byte("or other")

	if bytes.Compare(a, b) != 0 {
		println("Mismatch.")
	}
}
`)

			validFunc := func(c MultilineContentI) bool {
				_, err := parser.ParseExpr(c.Content())
				return err == nil
			}
			from := NewTextBoxValidationWidget(np, validFunc)
			to := NewTextBoxValidationWidget(np, validFunc)

			SetViewGroup(from.Content, "bytes.Compare(a, b) != 0")
			SetViewGroup(to.Content, "!bytes.Equal(a, b)")

			/* debug
			template := new(CmdTemplate)
			*template = NewCmdTemplate("gofmt", "-r", "")
			template.Stdin = func() io.Reader { return NewContentReader(in.Content) }

			templateString := func() string {
				template.NameArgs[2] = fmt.Sprintf("%s -> %s", from.Content.Content(), to.Content.Content()) // HACK: I'm doing modification in a place that's meant to be a pure function for display, not side-effects...
				return strings.Join(template.NameArgs, " ")
			}
			cmd := NewTextLabelWidgetContentFunc(np, templateString, []DepNodeI{&from.DepNode, &to.DepNode})

			out := NewLiveCmdExpeWidget(np, []DepNodeI{in, cmd}, template)
			*/

			/*nameArgs := StringsFunc(func() []string {
				out := []string{"gofmt"}
				if from.IsValidTEST() && to.IsValidTEST() {
					out = append(out, "-r", fmt.Sprintf("%s -> %s", from.Content.Content(), to.Content.Content()))
				}
				return out
			})
			template := NewCmdTemplateDynamic(nameArgs)*/

			template := NewCmdTemplateDynamic2()
			template.UpdateFunc = func(this DepNode2I) {
				inContent := this.GetSources()[0].(MultilineContentI)
				from := this.GetSources()[1].(*TextBoxWidgetValidChange)
				to := this.GetSources()[2].(*TextBoxWidgetValidChange)

				params := []string{}
				if from.IsValidTEST() && to.IsValidTEST() {
					params = append(params, "-r", fmt.Sprintf("%s -> %s", from.Content.Content(), to.Content.Content()))
				}

				template.Template = NewCmdTemplate("gofmt", params...)
				template.Template.Stdin = func() io.Reader { return NewContentReader(inContent) } // This is not a race condition only because template.NewCommand() gets called from same thread that updates in.Content.
			}
			template.AddSources(in.Content, &from.ValidChange, &to.ValidChange)

			/*debugOutput := func() string {
				cmd := template.NewCommand()
				return fmt.Sprintf("%#v", cmd.Args)
			}
			out := NewTextLabelWidgetContentFunc(np, debugOutput, []DepNodeI{&UniversalClock})*/
			out := NewLiveCmdExpeWidget(np, []DepNode2I{template}, template)

			widgets = append(widgets, NewFlowLayoutWidget(mgl64.Vec2{800, 10}, []Widgeter{in, NewTextLabelWidgetString(np, "gofmt -r "), from, NewTextLabelWidgetString(np, " -> "), to, out}, nil))

			box1 := in
			box2 := out

			/*highlightedDiff1 := &highlightedDiff{leftSide: true}
			highlightedDiff1.AddSources(box1.Content, box2.Content)
			box1.HighlightersTest = append(box1.HighlightersTest, highlightedDiff1)

			// TODO: Avoid having two objects that do similar work, merge into one with two iterators
			highlightedDiff2 := &highlightedDiff{}
			highlightedDiff2.AddSources(box1.Content, box2.Content)
			box2.HighlightersTest = append(box2.HighlightersTest, highlightedDiff2)*/

			tokenizedGoContent1 := &tokenizedGoContent{}
			tokenizedGoContent1.AddSources(box1.Content)

			tokenizedGoContent2 := &tokenizedGoContent{}
			tokenizedGoContent2.AddSources(box2.Content)

			tokenizedDiff1 := &tokenizedDiff{leftSide: true}
			tokenizedDiff1.AddSources(tokenizedGoContent1, tokenizedGoContent2)

			// TODO: Avoid having two objects that do similar work, merge into one with two iterators
			tokenizedDiff2 := &tokenizedDiff{}
			tokenizedDiff2.AddSources(tokenizedGoContent1, tokenizedGoContent2)

			box1.HighlightersTest = append(box1.HighlightersTest, tokenizedDiff1)
			box2.HighlightersTest = append(box2.HighlightersTest, tokenizedDiff2)
		}

		// +Gist Button
		{
			username := NewTextBoxWidgetOptions(np, TextBoxWidgetOptions{SingleLine: true})
			password := NewTextBoxWidgetOptions(np, TextBoxWidgetOptions{SingleLine: true, Private: true})

			gistButtonTrigger := NewButtonTriggerWidget(np)

			params := func() interface{} { return []string{username.Content.Content(), password.Content.Content()} }
			action := func(params interface{}) string {
				username := params.([]string)[0]
				password := params.([]string)[1]

				// TODO: This should be checked at higher level
				if username == "" || password == "" {
					return ""
				}

				// Create a gist
				cmd := exec.Command("curl", "-d", "{\"public\":true,\"files\":{\"main.go\":{\"content\":\"package gist\\n\\nimport ()\\n\"}}}", "https://api.github.com/gists", "--config", "-")
				cmd.Stdin = strings.NewReader("-u \"" + username + ":" + password + "\"")
				out, err := cmd.Output() // We want only the output, ignore progress meter (hence don't use CombinedOutput())
				if err != nil {
					return goon.SdumpExpr("Error creating gist.", err, string(out))
				}
				GistId, err := ParseGistId(out)
				if err != nil {
					return goon.SdumpExpr("Error parsing GistId.", err, string(out))
				}

				// Clone the gist repo
				// HACK: Need to generalize this; perhaps use `go get` after go1.2?
				Command := "cd ../../../../" // Hopefully the "$GOPATH/" folder
				Command += "\nmkdir -p \"./src/gist.github.com\""
				Command += "\ncd \"./src/gist.github.com\""
				Command += "\ngit clone https://gist.github.com/" + GistId + ".git \"./" + GistId + ".git\""
				//Command += "\ncurl -d 'path=gist.github.com/" + GistId + ".git' http://godoc.org/-/refresh";
				cmd = exec.Command("bash", "-c", Command)
				out, err = cmd.CombinedOutput()
				if err != nil {
					return goon.SdumpExpr("Error cloning the gist repo.", err, string(out))
				}

				// Open it in a new LiveProgramFileWidget
				//const auto FullPath = "./GoLand/src/gist.github.com/" + GistId + ".git/main.go";
				//AddWidgetForPath(FullPath, *MainCanvas, *m_TypingModule, m_CurrentProject);

				// Return import statement as the output
				return ". \"gist.github.com/" + GistId + ".git\""
			}
			output := NewLiveGoroutineExpeWidget(np, true, []DepNode2I{gistButtonTrigger}, params, action)

			widgets = append(widgets, NewFlowLayoutWidget(mgl64.Vec2{500, 10}, []Widgeter{username, password, NewTextLabelWidgetString(np, "+Gist"), gistButtonTrigger, output}, nil))
		}

		// Sample Window widget
		{
			newWindowButton := NewButtonWidget(np, func() {
				w := NewWindowWidget(mgl64.Vec2{500, 500}, mgl64.Vec2{200, 140}, NewTextBoxWidget(np))
				w.Name = "New Window"

				// Add new widget to canvas
				widget.(AddWidgeter).AddWidget(w)
			})
			widgets = append(widgets, newWindowButton)

			w := NewWindowWidget(mgl64.Vec2{1000, 40}, mgl64.Vec2{200, 140}, NewTextBoxWidget(np))
			w.Name = "Sample Window"
			widgets = append(widgets, w)
		}

		// TODO: Highlighting Go code + selection at a time
		if false {
			widgets = append(widgets, NewTextBoxWidget(mgl64.Vec2{20, 240}))
			widgets = append(widgets, NewTextFileWidget(mgl64.Vec2{100, 240}, "/Users/Dmitri/Dropbox/Work/2013/GoLand/src/gist.github.com/7176504.git/main.go"))
		}

		// TEST: Live vcs status of a single repo
		if false {
			booVcs = exp12.LookupDirectory("/Users/Dmitri/Dropbox/Work/2013/GoLand/src/github.com/shurcooL/Go-Package-Store/")
			MakeUpdated(booVcs)

			params := func() interface{} {
				return []interface{}{
					booVcs.Repo.Vcs.RootPath(),
					booVcs.Repo.VcsLocal.Status,
				}
			}

			action := func(params interface{}) string {
				rootPath := params.([]interface{})[0].(string)
				vcsStatus := params.([]interface{})[1].(string)

				return rootPath + "\n" + vcsStatus
			}

			widgets = append(widgets, NewLiveGoroutineExpeWidget(mgl64.Vec2{260, 180}, false, []DepNode2I{booVcs.Repo.VcsLocal}, params, action))
		}

		// Diff experiment
		{
			box1 := NewTextBoxWidgetExternalContent(np, NewMultilineContentString(`const Tau = 2 * math.Pi

func DrawCircle(pos mathgl.Vec2d, size mathgl.Vec2d) {
	const x = 64

	gl.Color3dv((*gl.Double)(&borderColor[0]))
	gl.Begin(gl.TRIANGLE_FAN)
	gl.Vertex2d(gl.Double(pos[0]), gl.Double(pos[1]))
	for i := 0; i <= x; i++ {
		gl.Vertex2d(gl.Double(pos[0]+math.Sin(Tau*float64(i)/x)*size[0]/2), ...)
	}
	gl.End()`), nil)
			box2 := NewTextBoxWidgetExternalContent(np, NewMultilineContentString(`
func DrawCircle(pos mathgl.Vec2d, size mathgl.Vec2d) {
	const TwoPi = math.Pi * 2

	const x = 64

	gl.Color3dv((*gl.Double)(&borderColor[0]))
	gl.Begin(gl.TRIANGLE_FAN)
	gl.Vertex2d(gl.Double(pos[0]), gl.Double(pos[1]))
	for i := 0; i <= x; i++ {
		// Completely new line
		gl.Vertex2d(gl.Double(pos[0]+math.Sin(TwoPi*float64(i)/x)*size[0]/2), ...)
	}
	gl.End()`), nil)
			widgets = append(widgets, NewFlowLayoutWidget(mgl64.Vec2{200, 250}, []Widgeter{box1, box2}, &FlowLayoutWidgetOptions{FlowLayoutType: HorizontalLayout}))

			if false {
				highlightedDiff := &highlightedDiff{}
				highlightedDiff.AddSources(box1.Content, box2.Content)

				box1.HighlightersTest = append(box1.HighlightersTest, &highlightedDiffSide{highlightedDiff: highlightedDiff, side: 0})
				box2.HighlightersTest = append(box2.HighlightersTest, &highlightedDiffSide{highlightedDiff: highlightedDiff, side: 1})
			} else {
				lineDiff := &lineDiff{}
				lineDiff.AddSources(box1.Content, box2.Content)

				lineDiffSide0 := &lineDiffSide{lineDiff: lineDiff, side: 0}
				lineDiffSide1 := &lineDiffSide{lineDiff: lineDiff, side: 1}

				box1.HighlightersTest = append(box1.HighlightersTest, lineDiffSide0)
				box2.HighlightersTest = append(box2.HighlightersTest, lineDiffSide1)

				box1.LineHighlighter = lineDiffSide0
				box2.LineHighlighter = lineDiffSide1
			}
		}

		if false {
			w := NewConnectionWidget(mgl64.Vec2{700, 500})
			widgets = append(widgets, w)
		}

		// Test having multiple .go files stacked vertically with a FlowLayoutWidget.
		if false {
			box1 := NewTextFileWidget(np, "/Users/Dmitri/Dropbox/Work/2013/GoLand/src/github.com/shurcooL/go/u/u8/main.go")
			box2 := NewTextFileWidget(np, "/Users/Dmitri/Dropbox/Work/2013/GoLand/src/github.com/shurcooL/go/u/u8/main_test.go")
			boxes := NewFlowLayoutWidget(mgl64.Vec2{1130, 220}, Widgeters{box1, box2}, &FlowLayoutWidgetOptions{FlowLayoutType: VerticalLayout})
			widgets = append(widgets, boxes)
		}

	} else if false {
		widgets = append(widgets, NewGpcFileWidget(mgl64.Vec2{1100, 500}, "/Users/Dmitri/Dropbox/Work/2013/eX0 Project/eX0 Client/levels/test3.wwl"))
		widgets = append(widgets, NewTest1Widget(mgl64.Vec2{10, 50}))
		widgets = append(widgets, NewKatWidget(mgl64.Vec2{370, 20}))
	}

	fpsWidget := NewFpsWidget(mgl64.Vec2{10, 120})
	widgets = append(widgets, fpsWidget)
	// NumGoroutines
	{
		contentFunc := func() string { return fmt.Sprint(runtime.NumGoroutine()) }
		mc := NewMultilineContentFunc(contentFunc, []DepNodeI{&UniversalClock})
		widgets = append(widgets, NewTextLabelWidgetExternalContent(mgl64.Vec2{10, 40}, mc))
	}

	// Http Server
	initHttpHandlers()
	widgets = append(widgets, NewHttpServerTestWidget(mgl64.Vec2{10, 130}))

	// lifeFormWidget test.
	//widgets = append(widgets, NewLifeFormWidget(mathgl.Vec2d{400, 400}))

	// Debug Panel
	if !*headlessFlag {
		var w Widgeters
		{
			contentFunc := func() (out string) {
				for _, widget := range mousePointer.Mapping {
					out += fmt.Sprintf("%T\n", widget)
				}
				return TrimLastNewline(out)
			}
			w = append(w, NewCollapsibleWidget(np, NewTextLabelWidgetExternalContent(np, NewMultilineContentFuncInstant(contentFunc)), "Mouse Mapping"))
		}
		{
			contentFunc := func() (out string) {
				for _, widget := range keyboardPointer.OriginMapping {
					out += fmt.Sprintf("%T\n", widget)
				}
				return TrimLastNewline(out)
			}
			w = append(w, NewCollapsibleWidget(np, NewTextLabelWidgetExternalContent(np, NewMultilineContentFuncInstant(contentFunc)), "Keyboard Origin Mapping"))
		}
		w = append(w, NewGoonWidget(np, &mousePointer))
		w = append(w, NewGoonWidget(np, &keyboardPointer))
		w = append(w, NewGoonWidget(np, &widgets))
		widgets = append(widgets, NewCollapsibleWidget(np, NewFlowLayoutWidget(np, w, &FlowLayoutWidgetOptions{FlowLayoutType: VerticalLayout}), "Debug"))
	}

	if sublimeMode {
		widget = NewCanvasWidget(mgl64.Vec2{0, 0}, widgets, &CanvasWidgetOptions{Scrollable: false})
	} else {
		widget = NewCanvasWidget(mgl64.Vec2{0, 0}, widgets, &CanvasWidgetOptions{Scrollable: true})
	}
	widget.(*CanvasWidget).offsetBy1Px()
	//widget := NewFlowLayoutWidget(mathgl.Vec2d{1, 1}, widgets, nil)
	//widget = NewCompositeWidget(mathgl.Vec2d{1, 1}, widgets)

	if keyboardPointer != nil {
		// Give the canvas initial keyboard focus
		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		keyboardPointer.OriginMapping = []Widgeter{widget}
	}

	fmt.Printf("Loaded in %v ms.\n", time.Since(startedProcess).Seconds()*1000)

	// ---

	for keepRunning {
		frameStartTime := time.Now()

		if !*headlessFlag {
			//glfw.WaitEvents()
			glfw.PollEvents()
		}

		// Move all input events from inputEventQueue2 into inputEventQueue.
	inputEventQueue2Loop:
		for {
			select {
			case inputEvent := <-inputEventQueue2:
				inputEventQueue = EnqueueInputEvent(inputEvent, inputEventQueue)
				redraw = true // TODO: Move redraw = true elsewhere? Like somewhere within events processing? Or keep it in all event handlers?
			default:
				break inputEventQueue2Loop
			}
		}

		// Process Input.
		inputEventQueue = ProcessInputEventQueue(widget, inputEventQueue)

		UniversalClock.TimePassed = 1.0 / 60 // TODO: Use proper value?
		UniversalClock.NotifyAllListeners()

		// TEMPORARY, HACK
		if booVcs != nil {
			ExternallyUpdated(booVcs.Repo.VcsLocal.GetSources()[1].(DepNode2ManualI)) // TODO: Updating this every frame is very slow (and should be done in background, not block main thread)
		}

		// DepNode2 dependency resolution
		// TODO: General solution
		/*if goCompileErrorsEnabledTest != nil && goCompileErrorsEnabledTest.state() {
			MakeUpdated(&goCompileErrorsManagerTest)
		}*/
		MakeUpdated(&goCompileErrorsManagerTest) // TODO: Only when build is on
		MakeUpdated(&spinner)
		for _, keepUpdatedEntry := range keepUpdatedTEST {
			MakeUpdated(keepUpdatedEntry)
		}

		widget.PollLogic()

		if redraw && !*headlessFlag {
			//gl.Clear(gl.COLOR_BUFFER_BIT)
			gl.LoadIdentity()

			widget.LayoutNeeded()
			widget.Render()

			mousePointer.Render()
			keyboardPointer.Render()
			if websocketPointer != nil {
				websocketPointer.Render()
			}

			fpsWidget.PushTimeToRender(time.Since(frameStartTime).Seconds() * 1000)
			window.SwapBuffers()
			runtime.Gosched()

			fpsWidget.PushTimeTotal(time.Since(frameStartTime).Seconds() * 1000)
			redraw = false
		} else {
			time.Sleep(5 * time.Millisecond)
			runtime.Gosched()
		}
	}

	_ = widget.Close()
	os.Remove(con2RunBinPath) // TODO: Generalize this

	fmt.Println("Graceful exit.")
}
