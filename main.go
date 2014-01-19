// A work in progress implementation of Conception in Go.
package main

import (
	"fmt"
	"log"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"time"
	. "gist.github.com/5286084.git"

	"image"
	_ "github.com/ftrvxmtrx/tga"
	//_ "image/png"
	"os"

	//"github.com/go-gl/gl"
	gl "github.com/chsc/gogl/gl21"
	glfw "github.com/go-gl/glfw3"
	//"github.com/go-gl/glu"
	glu "github.com/shurcooL/goglu/glu21"

	//"github.com/shurcooL/go/exp/11"
	"github.com/shurcooL/go/vcs"
	"github.com/shurcooL/gostatus/status"

	"github.com/Jragonmiris/mathgl"

	"github.com/shurcooL/go-goon"

	. "gist.github.com/6003701.git"

	"os/exec"
	. "gist.github.com/5258650.git"
	. "gist.github.com/6096872.git"

	. "gist.github.com/5571468.git"

	//"go/parser"
	//"go/token"

	"math"
	intmath "github.com/pkg/math"

	. "gist.github.com/5504644.git"
	. "gist.github.com/5639599.git"
	//"io/ioutil"
	//"runtime/debug"
	. "gist.github.com/5259939.git"
	. "gist.github.com/6418462.git"

	"errors"

	"io"
	"io/ioutil"

	. "gist.github.com/5892738.git"
	. "gist.github.com/6418290.git"
	. "gist.github.com/6545684.git"

	. "gist.github.com/4727543.git"

	"go/ast"
	"go/doc"
	"go/parser"
	"go/scanner"
	"go/token"
	. "gist.github.com/6445065.git"

	"reflect"

	. "gist.github.com/6724654.git"

	"code.google.com/p/go.tools/go/exact"
	"code.google.com/p/go.tools/go/types"
	. "gist.github.com/7576804.git"
	"honnef.co/go/importer"

	"github.com/davecheney/profile"

	"path"
	"path/filepath"
	. "gist.github.com/5953185.git"

	"flag"
	"net/http"
	_ "net/http/pprof"
	. "gist.github.com/7390843.git"
	//"github.com/russross/blackfriday"

	. "gist.github.com/7480523.git"
	//. "gist.github.com/7519227.git"

	. "gist.github.com/7576154.git"

	"bytes"
	. "gist.github.com/5645828.git"

	"bufio"
	"code.google.com/p/go.net/websocket"

	"github.com/sergi/go-diff/diffmatchpatch"

	"code.google.com/p/go.tools/astutil"

	. "gist.github.com/7728088.git"

	. "gist.github.com/7729255.git"

	. "gist.github.com/7651991.git"

	. "gist.github.com/7802150.git"

	. "gist.github.com/7519227.git"
	"gist.github.com/8018045.git"
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

const katOnly = false

var headlessFlag = flag.Bool("headless", false, "Headless mode.")

var keepRunning = true
var oFontBase, oFontBackground gl.Uint
var redraw bool = true
var mousePointer *Pointer
var keyboardPointer *Pointer

var goCompileErrorsManagerTest GoCompileErrorsManagerTest
var goCompileErrorsEnabledTest *TriButtonExternalStateWidget

// Colors
var (
	veryLightColor = mathgl.Vec3d{0.95, 0.95, 0.95}
	lightColor     = mathgl.Vec3d{0.85, 0.85, 0.85}
	darkColor      = mathgl.Vec3d{0.35, 0.35, 0.35}

	blackColor     = mathgl.Vec3d{0, 0, 0}
	highlightColor = mathgl.Vec3d{0.898, 0.765, 0.396}

	selectedTextColor         = mathgl.Vec3d{195 / 255.0, 212 / 255.0, 242 / 255.0}
	selectedTextInactiveColor = mathgl.Vec3d{240 / 255.0, 240 / 255.0, 240 / 255.0}
)

// TODO: Remove these
var globalWindow *glfw.Window
var np = mathgl.Vec2d{} // np stands for "No Position" and it's basically the (0, 0) position, used when it doesn't matter
var keepUpdatedTEST = []DepNode2I{}

func CheckGLError() {
	errorCode := gl.GetError()
	if errorCode != 0 {
		log.Panicln("GL Error:", errorCode)
	}
}

// ---

func PrintText(pos mathgl.Vec2d, s string) {
	lines := GetLines(s)
	for lineIndex, line := range lines {
		PrintLine(pos.Add(mathgl.Vec2d{0, float64(fontHeight * lineIndex)}), line)
	}
}

// Input shouldn't have newlines
func PrintLine(pos mathgl.Vec2d, s string) {
	segments := strings.Split(s, "\t")
	var advance uint32
	for _, segment := range segments {
		PrintSegment(mathgl.Vec2d{pos[0] + float64(fontWidth*advance), pos[1]}, segment)
		advance += uint32(len(segment))
		advance += 4 - (advance % 4)
	}
}

// Shouldn't have tabs nor newlines
func PrintSegment(pos mathgl.Vec2d, s string) {
	if s == "" {
		return
	}

	gl.Enable(gl.BLEND)
	gl.Enable(gl.TEXTURE_2D)
	defer gl.Disable(gl.BLEND)
	defer gl.Disable(gl.TEXTURE_2D)

	gl.PushMatrix()
	gl.Translated(gl.Double(pos[0])-3.75*fontWidth/8.0, gl.Double(pos[1])-1*fontHeight/16.0, 0)
	gl.ListBase(oFontBase)
	gl.CallLists(gl.Sizei(len(s)), gl.UNSIGNED_BYTE, gl.Pointer(&[]byte(s)[0]))
	gl.PopMatrix()

	//CheckGLError()
}

// ---

type OpenGlStream struct {
	pos        mathgl.Vec2d
	lineStartX float64
	advance    uint32

	BackgroundColor *mathgl.Vec3d // nil means no background color
}

func NewOpenGlStream(pos mathgl.Vec2d) *OpenGlStream {
	return &OpenGlStream{pos: pos, lineStartX: pos[0]}
}

func (o *OpenGlStream) SetPos(pos mathgl.Vec2d) {
	o.pos = pos
	o.lineStartX = pos[0]
	o.advance = 0
}

func (o *OpenGlStream) SetPosWithExpandedPosition(pos mathgl.Vec2d, x, y uint32) {
	o.pos = pos.Add(mathgl.Vec2d{float64(x * fontWidth), float64(y * fontHeight)})
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
			o.advance = 0
			s = s[end+1:]
		}
	}
}

// Input shouldn't have newlines
func (o *OpenGlStream) PrintLine(s string) {
	segments := strings.Split(s, "\t")
	for index, segment := range segments {
		o.PrintSegment(segment)
		o.advance += uint32(len(segment))
		if index+1 < len(segments) {
			o.PrintSegment(strings.Repeat(" ", 4-int(o.advance%4))) // Tab
			o.advance += 4 - (o.advance % 4)
		}
	}
}

// Shouldn't have tabs nor newlines
func (o *OpenGlStream) PrintSegment(s string) {
	if s == "" {
		return
	}

	o.pos[0] = o.lineStartX + float64(fontWidth*o.advance)

	if o.BackgroundColor != nil {
		gl.PushAttrib(gl.CURRENT_BIT)
		gl.Color3dv((*gl.Double)(&o.BackgroundColor[0]))
		gl.PushMatrix()
		gl.Translated(gl.Double(o.pos[0]), gl.Double(o.pos[1]), 0)
		for _ = range s {
			gl.CallList(oFontBackground)
		}
		gl.PopMatrix()
		gl.PopAttrib()
	}

	gl.Enable(gl.BLEND)
	gl.Enable(gl.TEXTURE_2D)
	defer gl.Disable(gl.BLEND)
	defer gl.Disable(gl.TEXTURE_2D)

	gl.PushMatrix()
	gl.Translated(gl.Double(o.pos[0])-3.75*fontWidth/8.0, gl.Double(o.pos[1])-1*fontHeight/16.0, 0)
	gl.ListBase(oFontBase)
	gl.CallLists(gl.Sizei(len(s)), gl.UNSIGNED_BYTE, gl.Pointer(&[]byte(s)[0]))
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
		fCharX := gl.Double(iLoop1%16) / 16.0
		fCharY := gl.Double(iLoop1/16) / 16.0

		gl.NewList(oFontBase+gl.Uint(iLoop1), gl.COMPILE)
		const offset = gl.Double(0.04 / 16)
		const shiftX, shiftY = gl.Double(0.0 / 16), gl.Double(0.0 / 16)
		//#if DECISION_RENDER_TEXT_VCENTERED_MID
		/*VerticalOffset := gl.Double(0.02 / 16)
		if ('a' <= iLoop1 && iLoop1 <= 'z') || '_' == iLoop1 {
			VerticalOffset = gl.Double(-0.036 / 16)
		}*/
		VerticalOffset := gl.Double(0.0)
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

	var texture gl.Uint
	gl.GenTextures(1, &texture)
	gl.BindTexture(gl.TEXTURE_2D, texture)
	gl.TexParameteri(gl.TEXTURE_2D, gl.GENERATE_MIPMAP, gl.TRUE)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_LOD_BIAS, -0.75)
	gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.Sizei(bounds.Dx()), gl.Sizei(bounds.Dy()), 0, gl.RGBA, gl.UNSIGNED_BYTE, gl.Pointer(pixPointer))
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
	Layout()
	Render()
	Hit(mathgl.Vec2d) []Widgeter
	ProcessEvent(InputEvent) // TODO: Upgrade to MatchEventQueue() or so

	Pos() *mathgl.Vec2d
	Size() *mathgl.Vec2d
	HoverPointers() map[*Pointer]bool
	Parent() Widgeter
	SetParent(Widgeter)

	ParentToLocal(mathgl.Vec2d) mathgl.Vec2d

	DepNodeI
}

type Widgeters []Widgeter

type Widget struct {
	pos           mathgl.Vec2d
	size          mathgl.Vec2d
	hoverPointers map[*Pointer]bool
	parent        Widgeter

	DepNode
}

func NewWidget(pos, size mathgl.Vec2d) Widget {
	return Widget{pos: pos, size: size, hoverPointers: map[*Pointer]bool{}}
}

func (w *Widget) Layout() {
	if w.parent != nil {
		w.parent.Layout()
	}
}
func (*Widget) Render() {}
func (w *Widget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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

func (w *Widget) Pos() *mathgl.Vec2d  { return &w.pos }
func (w *Widget) Size() *mathgl.Vec2d { return &w.size }

func (w *Widget) HoverPointers() map[*Pointer]bool {
	return w.hoverPointers
}

func (w *Widget) Parent() Widgeter     { return w.parent }
func (w *Widget) SetParent(p Widgeter) { w.parent = p }

func (w *Widget) ParentToLocal(ParentPosition mathgl.Vec2d) (LocalPosition mathgl.Vec2d) {
	return ParentPosition.Sub(w.pos)
}

type WidgeterS struct{ Widgeter }

func (w WidgeterS) GlobalToParent(GlobalPosition mathgl.Vec2d) (ParentPosition mathgl.Vec2d) {
	switch w.Parent() {
	case nil:
		ParentPosition = GlobalPosition
	default:
		ParentPosition = WidgeterS{w.Parent()}.GlobalToLocal(GlobalPosition)
	}
	return ParentPosition
}
func (w WidgeterS) GlobalToLocal(GlobalPosition mathgl.Vec2d) (LocalPosition mathgl.Vec2d) {
	return w.ParentToLocal(WidgeterS{w}.GlobalToParent(GlobalPosition))
}

// ---

type CustomWidget struct {
	Widget
	RenderFunc       func()
	ProcessEventFunc func(inputEvent InputEvent)
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

func NewTest1Widget(pos mathgl.Vec2d) *Test1Widget {
	return &Test1Widget{Widget: NewWidget(pos, mathgl.Vec2d{300, 300})}
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

func NewTest2Widget(pos mathgl.Vec2d, field *float64) *Test2Widget {
	return &Test2Widget{TextBoxWidget: NewTextBoxWidgetExternalContent(pos, NewMultilineContentFuncInstant(func() string { return TrimLastNewline(goon.Sdump(*field)) })), field: field}
}

func (w *Test2Widget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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

type parsedFile struct {
	fset    *token.FileSet
	fileAst *ast.File

	DepNode2
}

func (t *parsedFile) Update() {
	source := t.GetSources()[0].(MultilineContentI)
	fset := token.NewFileSet()
	fileAst, err := parser.ParseFile(fset, "", source.Content(), 1*parser.ParseComments)

	{
		//fileAst.Decls[0].(*ast.GenDecl).Specs = append(fileAst.Decls[0].(*ast.GenDecl).Specs, &ast.ImportSpec{Path: &ast.BasicLit{Kind: token.STRING, Value: `"yay/new/import"`}})
		astutil.AddImport(fset, fileAst, "yay/new/import")
	}

	if err == nil {
		t.fset = fset
		t.fileAst = fileAst
	} else {
		t.fset = nil
		t.fileAst = nil
	}
}

func NewTest3Widget(pos mathgl.Vec2d, source *TextBoxWidget) (*LiveGoroutineExpeWidget, *parsedFile) {
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

	w := NewLiveGoroutineExpeWidget(pos, []DepNode2I{parsedFile, source.caretPosition}, params, action)
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
	goPackage := t.GetSources()[0].(ImportPathFoundSelector)

	importPath := ""
	if goPackage.GetSelected() != nil {
		importPath = goPackage.GetSelected().ImportPath()
	}

	bpkg, err := BuildPackageFromImportPath(importPath)
	if err != nil {
		t.fset = nil
		t.files = nil
		t.tpkg = nil
		t.info = nil
		return
	}

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
		Types:      make(map[ast.Expr]types.Type),
		Values:     make(map[ast.Expr]exact.Value),
		Objects:    make(map[*ast.Ident]types.Object),
		Implicits:  make(map[ast.Node]types.Object),
		Selections: make(map[*ast.SelectorExpr]*types.Selection),
		Scopes:     make(map[ast.Node]*types.Scope),
	}
	tpkg, err := cfg.Check(importPath, fset, files, info)
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

func NewTest4Widget(pos mathgl.Vec2d, goPackage ImportPathFoundSelector, source *TextBoxWidget) (*LiveGoroutineExpeWidget, *typeCheckedPackage) {
	typeCheckedPackage := &typeCheckedPackage{}
	typeCheckedPackage.AddSources(goPackage, source.Content)

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

			if info != nil && info.Objects[ident] != nil {
				obj := info.Objects[ident]
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
		}
		return out
	}

	w := NewLiveGoroutineExpeWidget(pos, []DepNode2I{typeCheckedPackage, source.caretPosition}, params, action)
	return w, typeCheckedPackage
}

// ---

type docPackage struct {
	dpkg *doc.Package

	DepNode2
}

func (this *docPackage) Update() {
	goPackage := this.GetSources()[0].(*GoPackageListingPureWidget)

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
}

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

func (this *goSymbols) Update() {
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

/*func NewTest5Widget(pos mathgl.Vec2d, goPackage *GoPackageListingPureWidget, source *TextBoxWidget) *ListWidget {
	docPackage := &docPackage{}
	docPackage.AddSources(goPackage, source.Content)

	goSymbols := &goSymbols{}
	goSymbols.AddSources(docPackage)

	w := NewListWidget(np, goSymbols)
	return w
}*/

// ---

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

func NewTest5BWidget(pos mathgl.Vec2d, typeCheckedPackage *typeCheckedPackage) *SearchableListWidget {
	goSymbols := &goSymbolsB{}
	goSymbols.AddSources(typeCheckedPackage)

	w := NewSearchableListWidget(np, goSymbols)
	return w
}

// ---

type GoCompileErrorsManagerTest struct {
	//Sources []*GoCompileErrorsTest // TODO: Migrate to using DepNode2
	DepNode2

	All map[FileUri][]GoErrorMessage
}

func (this *GoCompileErrorsManagerTest) Update() {
	this.All = make(map[FileUri][]GoErrorMessage)

	for _, source := range this.GetSources() {
		for _, goCompilerError := range source.(*GoCompileErrorsTest).Out {
			this.All[goCompilerError.FileUri] = append(this.All[goCompilerError.FileUri], goCompilerError.ErrorMessage)
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

	source := this.DepNode2.GetSources()[0].(*LiveCmdExpeWidget).Content
	outChan := GoReduceLinesFromReader(NewContentReader(source), 4, reduceFunc)
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

func NewGpcFileWidget(pos mathgl.Vec2d, path string) *GpcFileWidget {
	return &GpcFileWidget{Widget: NewWidget(pos, mathgl.Vec2d{0, 0}), p: ReadGpcFile(path)}
}

func (w *GpcFileWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)

	gl.Color3d(0, 0, 0)
	for _, contour := range w.p.Contours {
		gl.Begin(gl.LINE_LOOP)
		for _, vertex := range contour.Vertices {
			gl.Vertex2dv((*gl.Double)(&vertex[0]))
		}
		gl.End()
	}
}

// ---

type ButtonTriggerWidget struct {
	*ButtonWidget
	DepNode2Manual
}

func NewButtonTriggerWidget(pos mathgl.Vec2d) *ButtonTriggerWidget {
	w := &ButtonTriggerWidget{}
	w.ButtonWidget = NewButtonWidget(pos, func() { ExternallyUpdated(&w.DepNode2Manual) })

	return w
}

// ---

type ButtonWidget struct {
	Widget
	action  func()
	tooltip Widgeter
}

func NewButtonWidget(pos mathgl.Vec2d, action func()) *ButtonWidget {
	w := &ButtonWidget{Widget: NewWidget(pos, mathgl.Vec2d{fontHeight, fontHeight})}
	w.setAction(action)

	return w
}

func (w *ButtonWidget) setAction(action func()) {
	w.action = action

	if action != nil {
		// HACK: This isn't thread-safe in any way
		go func() { w.tooltip = NewTextLabelWidgetString(np, GetSourceAsString(action)) }()
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
		DrawGBox(w.pos, w.size)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.pos, w.size)
	} else {
		DrawNBox(w.pos, w.size)
	}

	// Tooltip
	if w.tooltip != nil && isHit {
		mousePointerPositionLocal := WidgeterS{w}.GlobalToLocal(mathgl.Vec2d{mousePointer.State.Axes[0], mousePointer.State.Axes[1]})
		tooltipOffset := mathgl.Vec2d{0, 16}
		*w.tooltip.Pos() = w.pos.Add(mousePointerPositionLocal).Add(tooltipOffset)
		w.tooltip.Render()
	}
}
func (w *ButtonWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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

func NewTriButtonWidget(pos mathgl.Vec2d, action func()) *TriButtonWidget {
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
	gl.Color3dv((*gl.Double)(&darkColor[0]))
	if !w.state {
		gl.Begin(gl.TRIANGLES)
		gl.Vertex2d(gl.Double(w.pos[0]+w.size[0]*0.25), gl.Double(w.pos[1]+w.size[0]*0.15))
		gl.Vertex2d(gl.Double(w.pos[0]+w.size[0]*0.95), gl.Double(w.pos[1]+w.size[1]*0.5))
		gl.Vertex2d(gl.Double(w.pos[0]+w.size[0]*0.25), gl.Double(w.pos[1]+w.size[1]*0.85))
		gl.End()
	} else {
		gl.Begin(gl.TRIANGLES)
		gl.Vertex2d(gl.Double(w.pos[0]+w.size[0]*0.15), gl.Double(w.pos[1]+w.size[0]*0.25))
		gl.Vertex2d(gl.Double(w.pos[0]+w.size[0]*0.85), gl.Double(w.pos[1]+w.size[1]*0.25))
		gl.Vertex2d(gl.Double(w.pos[0]+w.size[0]*0.5), gl.Double(w.pos[1]+w.size[1]*0.95))
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

func NewTriButtonExternalStateWidget(pos mathgl.Vec2d, state func() bool, action func()) *TriButtonExternalStateWidget {
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
		DrawBorderlessBox(w.pos.Add(w.size.Mul(0.125)), w.size.Mul(0.75), mathgl.Vec3d{0.9, 0.3, 0.01})
	} else {
		DrawBorderlessBox(w.pos.Add(w.size.Mul(0.125)), w.size.Mul(0.75), mathgl.Vec3d{0.9, 0.9, 0.9})
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
		mousePointerPositionLocal := WidgeterS{w}.GlobalToLocal(mathgl.Vec2d{mousePointer.State.Axes[0], mousePointer.State.Axes[1]})
		tooltipOffset := mathgl.Vec2d{0, -4 - boxWidgetTooltip.Size()[1]}
		*boxWidgetTooltip.Pos() = w.pos.Add(mousePointerPositionLocal).Add(tooltipOffset)
		boxWidgetTooltip.Render()
	}
}
func (w *BoxWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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
}

func NewWindowWidget(pos, size mathgl.Vec2d) *WindowWidget {
	w := &WindowWidget{Widget: NewWidget(pos, size)}
	closeButton := NewButtonWidget(np, func() {
		// HACK: Assumes *CompositeWidget
		(&w.Parent().(*CompositeWidget).Widgets).RemoveWidget(w)
		w.SetParent(nil) // TODO: Not sure if needed
	})
	w.chrome = NewCompositeWidget(np, []Widgeter{closeButton})
	w.chrome.SetParent(w)
	return w
}

func (w *WindowWidget) Render() {
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
	DrawNBox(w.pos, w.size)
	if isOriginHit && mousePointer.State.IsActive() && isHit {
		DrawGradientBox(w.pos, mathgl.Vec2d{w.size[0], fontHeight}, highlightColor, veryLightColor, lightColor)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawGradientBox(w.pos, mathgl.Vec2d{w.size[0], fontHeight}, highlightColor, veryLightColor, lightColor)
	} else {
		DrawGradientBox(w.pos, mathgl.Vec2d{w.size[0], fontHeight}, mathgl.Vec3d{0.3, 0.3, 0.3}, veryLightColor, lightColor)
	}

	// Title
	gl.Color3dv((*gl.Double)(&blackColor[0]))
	PrintSegment(w.pos.Add(mathgl.Vec2d{60}), w.Name)

	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
	w.chrome.Render()
}
func (w *WindowWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
	LocalPosition := w.ParentToLocal(ParentPosition)

	Hit := (LocalPosition[0] >= 0 &&
		LocalPosition[1] >= 0 &&
		LocalPosition[0] <= w.size[0] &&
		LocalPosition[1] <= fontHeight)

	if Hit {
		hits := []Widgeter{w}
		hits = append(hits, w.chrome.Hit(LocalPosition)...)
		return hits
	} else {
		return nil
	}
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

// TEST
func (widgets *Widgeters) RemoveWidget(targetWidget Widgeter) {
	for index, widget := range *widgets {
		if widget == targetWidget {
			copy((*widgets)[index:], (*widgets)[index+1:])
			(*widgets)[len(*widgets)-1] = nil
			*widgets = (*widgets)[:len(*widgets)-1]
			return
		}
	}
}

// ---

func DrawBorderlessBox(pos, size mathgl.Vec2d, backgroundColor mathgl.Vec3d) {
	gl.Color3dv((*gl.Double)(&backgroundColor[0]))
	gl.Rectd(gl.Double(pos[0]), gl.Double(pos[1]), gl.Double(pos.Add(size)[0]), gl.Double(pos.Add(size)[1]))
}

func DrawBorderlessGradientBox(pos, size mathgl.Vec2d, topColor, bottomColor mathgl.Vec3d) {
	gl.Begin(gl.TRIANGLE_STRIP)
	gl.Color3dv((*gl.Double)(&topColor[0]))
	gl.Vertex2d(gl.Double(pos[0]), gl.Double(pos[1]))
	gl.Vertex2d(gl.Double(pos.Add(size)[0]), gl.Double(pos[1]))
	gl.Color3dv((*gl.Double)(&bottomColor[0]))
	gl.Vertex2d(gl.Double(pos[0]), gl.Double(pos.Add(size)[1]))
	gl.Vertex2d(gl.Double(pos.Add(size)[0]), gl.Double(pos.Add(size)[1]))
	gl.End()
}

func DrawBox(pos, size mathgl.Vec2d, borderColor, backgroundColor mathgl.Vec3d) {
	gl.Color3dv((*gl.Double)(&borderColor[0]))
	gl.Rectd(gl.Double(pos[0]-1), gl.Double(pos[1]-1), gl.Double(pos.Add(size)[0]+1), gl.Double(pos.Add(size)[1]+1))
	DrawBorderlessBox(pos, size, backgroundColor)
}
func DrawNBox(pos, size mathgl.Vec2d) {
	DrawBox(pos, size, mathgl.Vec3d{0.3, 0.3, 0.3}, mathgl.Vec3d{1, 1, 1})
}
func DrawYBox(pos, size mathgl.Vec2d) {
	DrawBox(pos, size, highlightColor, mathgl.Vec3d{1, 1, 1})
}
func DrawGBox(pos, size mathgl.Vec2d) {
	DrawBox(pos, size, highlightColor, mathgl.Vec3d{0.75, 0.75, 0.75})
}
func DrawLGBox(pos, size mathgl.Vec2d) {
	DrawBox(pos, size, mathgl.Vec3d{0.6, 0.6, 0.6}, mathgl.Vec3d{0.95, 0.95, 0.95})
}

func DrawGradientBox(pos, size mathgl.Vec2d, borderColor, topColor, bottomColor mathgl.Vec3d) {
	gl.Color3dv((*gl.Double)(&borderColor[0]))
	gl.Rectd(gl.Double(pos[0]-1), gl.Double(pos[1]-1), gl.Double(pos.Add(size)[0]+1), gl.Double(pos.Add(size)[1]+1))
	DrawBorderlessGradientBox(pos, size, topColor, bottomColor)
}

func DrawCircle(pos mathgl.Vec2d, size mathgl.Vec2d, borderColor, backgroundColor mathgl.Vec3d) {
	const TwoPi = math.Pi * 2

	const x = 64

	gl.Color3dv((*gl.Double)(&borderColor[0]))
	gl.Begin(gl.TRIANGLE_FAN)
	gl.Vertex2d(gl.Double(pos[0]), gl.Double(pos[1]))
	for i := 0; i <= x; i++ {
		gl.Vertex2d(gl.Double(pos[0]+math.Sin(TwoPi*float64(i)/x)*size[0]/2), gl.Double(pos[1]+math.Cos(TwoPi*float64(i)/x)*size[1]/2))
	}
	gl.End()

	gl.Color3dv((*gl.Double)(&backgroundColor[0]))
	gl.Begin(gl.TRIANGLE_FAN)
	gl.Vertex2d(gl.Double(pos[0]), gl.Double(pos[1]))
	for i := 0; i <= x; i++ {
		gl.Vertex2d(gl.Double(pos[0]+math.Sin(TwoPi*float64(i)/x)*(size[0]/2-1)), gl.Double(pos[1]+math.Cos(TwoPi*float64(i)/x)*(size[1]/2-1)))
	}
	gl.End()
}

func DrawCircleBorder(pos mathgl.Vec2d, size mathgl.Vec2d, borderColor mathgl.Vec3d) {
	DrawCircleBorderCustom(pos, size, borderColor, 1, 64, 0, 64)
}

func DrawCircleBorderCustom(pos mathgl.Vec2d, size mathgl.Vec2d, borderColor mathgl.Vec3d, borderWidth float64, totalSlices, startSlice, endSlice int32) {
	const TwoPi = math.Pi * 2

	var x = float64(totalSlices)

	gl.Color3dv((*gl.Double)(&borderColor[0]))
	gl.Begin(gl.TRIANGLE_STRIP)
	for i := startSlice; i <= endSlice; i++ {
		gl.Vertex2d(gl.Double(pos[0]+math.Sin(TwoPi*float64(i)/x)*size[0]/2), gl.Double(pos[1]-math.Cos(TwoPi*float64(i)/x)*size[1]/2))
		gl.Vertex2d(gl.Double(pos[0]+math.Sin(TwoPi*float64(i)/x)*(size[0]/2-borderWidth)), gl.Double(pos[1]-math.Cos(TwoPi*float64(i)/x)*(size[1]/2-borderWidth)))
	}
	gl.End()
}

// ---

type KatWidget struct {
	Widget
	target      mathgl.Vec2d
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
	panic(nil)
}

func NewKatWidget(pos mathgl.Vec2d) *KatWidget {
	w := &KatWidget{Widget: NewWidget(pos, mathgl.Vec2d{16, 16}), target: pos}
	UniversalClock.AddChangeListener(w)
	return w
}

func (w *KatWidget) Render() {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	/*hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	isHit := len(w.HoverPointers()) > 0

	if !hasTypingFocus && !isHit {
		DrawCircle(w.pos, w.size, mathgl.Vec3d{0.3, 0.3, 0.3}, mathgl.Vec3d{1, 1, 1})
	} else {
		DrawCircle(w.pos, w.size, highlightColor, mathgl.Vec3d{1, 1, 1})
	}*/

	// Shadow
	{
		gl.PushMatrix()
		gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)

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
				gl.Vertex2d(gl.Double(math.Cos(2*math.Pi*float64(nSlice)/float64(nSlices))*dShadowRadius), gl.Double(math.Sin(2*math.Pi*float64(nSlice)/float64(nSlices))*dShadowRadius))
			}
		}
		gl.End()
		gl.Disable(gl.BLEND)

		gl.PopMatrix()
	}

	// eX0 Player
	{
		gl.PushMatrix()
		gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
		gl.Rotated(gl.Double(w.rotation), 0, 0, 1)

		DrawCircleBorderCustom(np, mathgl.Vec2d{16, 16}, mathgl.Vec3d{1, 0, 0}, 2, 12, 1, 11)

		// Draw the gun
		{
			gl.Begin(gl.QUADS)
			gl.Vertex2d(gl.Double(-1), -gl.Double(3+10))
			gl.Vertex2d(gl.Double(-1), -gl.Double(3-1))
			gl.Vertex2d(gl.Double(1), -gl.Double(3-1))
			gl.Vertex2d(gl.Double(1), -gl.Double(3+10))
			gl.End()
		}

		gl.PopMatrix()
	}

	if w.mode == Shunpo && !w.skillActive {
		DrawCircleBorder(w.pos, mathgl.Vec2d{ShunpoRadius * 2, ShunpoRadius * 2}, mathgl.Vec3d{0.7, 0.7, 0.7})
	}
}

func (w *KatWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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
		w.target = WidgeterS{w}.GlobalToParent(mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]})
		w.skillActive = true
	}

	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.Pointer.State.Button(1) {
		pointerPos := WidgeterS{w}.GlobalToParent(mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]})
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
	hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

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

		var direction mathgl.Vec2d
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
			rotM := mathgl.Rotate2Dd(w.rotation)
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

// ---

type CompositeWidget struct {
	Widget
	Widgets Widgeters
}

func NewCompositeWidget(pos mathgl.Vec2d, Widgets Widgeters) *CompositeWidget {
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
func (w *CompositeWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)

	for _, widget := range w.Widgets {
		widget.Render()
	}
}
func (w *CompositeWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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

func NewFlowLayoutWidget(pos mathgl.Vec2d, Widgets []Widgeter, options *FlowLayoutWidgetOptions) *FlowLayoutWidget {
	if options == nil {
		options = &FlowLayoutWidgetOptions{}
	}
	w := &FlowLayoutWidget{CompositeWidget: CompositeWidget{Widget: NewWidget(pos, mathgl.Vec2d{}), Widgets: Widgets}, options: *options}
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
	offset  mathgl.Vec2d
	options CanvasWidgetOptions
}

type CanvasWidgetOptions struct {
}

func NewCanvasWidget(pos mathgl.Vec2d, Widgets []Widgeter, options *CanvasWidgetOptions) *CanvasWidget {
	if options == nil {
		options = &CanvasWidgetOptions{}
	}
	w := &CanvasWidget{CompositeWidget: CompositeWidget{Widget: NewWidget(pos, mathgl.Vec2d{}), Widgets: Widgets}, options: *options}
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

func (w *CanvasWidget) Layout() {
	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *CanvasWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]+w.offset[0]), gl.Double(w.pos[1]+w.offset[1]), 0)

	for _, widget := range w.Widgets {
		widget.Render()
	}
}

func (w *CanvasWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[SLIDER_EVENT] && inputEvent.InputId == 2 {
		w.offset[1] += inputEvent.Sliders[0] * 10
		w.offset[0] += inputEvent.Sliders[1] * 10
	}
}

func (w *CanvasWidget) ParentToLocal(ParentPosition mathgl.Vec2d) (LocalPosition mathgl.Vec2d) {
	return w.Widget.ParentToLocal(ParentPosition).Sub(w.offset)
}

// ---

type CollapsibleWidget struct {
	Widget
	state *TriButtonWidget //*TriButtonExternalStateWidget
	child Widgeter
}

func NewCollapsibleWidget(pos mathgl.Vec2d, child Widgeter) *CollapsibleWidget {
	w := &CollapsibleWidget{Widget: NewWidget(pos, np), child: child}
	w.child.SetParent(w)

	//state := true
	//w.state = NewTriButtonExternalStateWidget(np, func() bool { return state }, func() { state = !state })
	w.state = NewTriButtonWidget(np, func() {})
	//w.state.state = true // HACK
	w.state.SetParent(w) // For its Layout() to work...

	// HACK
	w.child.Pos()[0] = w.state.Size()[0] + 2

	// TODO: This needs to be automated, easy to forget, moved into Layout2(), etc.
	w.Layout() // TODO: Should this be automatic from above SetParent()?

	return w
}

func (w *CollapsibleWidget) Layout() {
	// HACK
	Widgets := []Widgeter{w.state}
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

func (w *CollapsibleWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)

	w.state.Render()

	if w.state.State() {
		w.child.Render()
	}
}

func (w *CollapsibleWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
	LocalPosition := w.ParentToLocal(ParentPosition)

	hits := w.state.Hit(LocalPosition)
	if w.state.State() {
		hits = append(hits, w.child.Hit(LocalPosition)...)
	}

	return hits
}

// ---

type ScrollPaneWidget struct {
	Widget
	child Widgeter
}

func NewScrollPaneWidget(pos, size mathgl.Vec2d, child Widgeter) *ScrollPaneWidget {
	w := &ScrollPaneWidget{Widget: NewWidget(pos, size), child: child}
	w.child.SetParent(w)
	return w
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

func (w *ScrollPaneWidget) ScrollToArea(target, size mathgl.Vec2d) {
	for i := 0; i < 2; i++ {
		// Move the minimum amount that increases overlap
		w.child.Pos()[i] += math.Min(math.Dim(-w.child.Pos()[i], target[i]), math.Dim(-w.child.Pos()[i]+w.size[i], target[i]+size[i]))
		w.child.Pos()[i] -= math.Min(math.Dim(target[i], -w.child.Pos()[i]), math.Dim(target[i]+size[i], -w.child.Pos()[i]+w.size[i]))
	}

	// TODO: Needed?
	//w.Layout()
}

func (w *ScrollPaneWidget) CenterOnArea(target, size mathgl.Vec2d) {
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

func SetScissor(pos, size mathgl.Vec2d) {
	var ModelMatrix [16]gl.Double
	var ProjectionMatrix [16]gl.Double
	var Viewport [4]gl.Int
	var x0, y0, z0, x1, y1, z1, x2, y2, z2 gl.Double

	gl.GetDoublev(gl.MODELVIEW_MATRIX, &ModelMatrix[0])
	gl.GetDoublev(gl.PROJECTION_MATRIX, &ProjectionMatrix[0])
	gl.GetIntegerv(gl.VIEWPORT, &Viewport[0])

	glu.Project(gl.Double(pos[0]), gl.Double(pos[1]+size[1]), 0, &ModelMatrix[0], &ProjectionMatrix[0], &Viewport[0], &x0, &y0, &z0)
	glu.Project(gl.Double(size[0]), gl.Double(size[1]), 0, &ModelMatrix[0], &ProjectionMatrix[0], &Viewport[0], &x1, &y1, &z1)
	glu.Project(0, 0, 0, &ModelMatrix[0], &ProjectionMatrix[0], &Viewport[0], &x2, &y2, &z2)

	pos0 := NearInt64(float64(x0))
	pos1 := NearInt64(float64(y0))
	size0 := NearInt64(float64(x1 - x2))
	size1 := NearInt64(float64(y2 - y1))

	// Crop the scissor box by the parent scissor box
	// TODO

	gl.Scissor(gl.Int(pos0), gl.Int(pos1), gl.Sizei(size0), gl.Sizei(size1))
}

func (w *ScrollPaneWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)

	SetScissor(mathgl.Vec2d{-1, -1}, w.size.Add(mathgl.Vec2d{2, 2}))
	gl.Enable(gl.SCISSOR_TEST)

	w.child.Render()

	gl.Disable(gl.SCISSOR_TEST)

	// Draw scrollbars, if needed
	{
		const scrollbarWidth = 2

		// Vertical
		if w.child.Size()[1] > w.size[1] {
			DrawBorderlessBox(mathgl.Vec2d{w.size[0] - scrollbarWidth + 1, -w.child.Pos()[1]/w.child.Size()[1]*(w.size[1]+2) - 1},
				mathgl.Vec2d{scrollbarWidth, w.size[1] / w.child.Size()[1] * (w.size[1] + 2)},
				darkColor)
		}

		// Horizontal
		if w.child.Size()[0] > w.size[0] {
			DrawBorderlessBox(mathgl.Vec2d{-w.child.Pos()[0]/w.child.Size()[0]*(w.size[0]+2) - 1, w.size[1] - scrollbarWidth + 1},
				mathgl.Vec2d{w.size[0] / w.child.Size()[0] * (w.size[0] + 2), scrollbarWidth},
				darkColor)
		}
	}
}

func (w *ScrollPaneWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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

type UnderscoreSepToCamelCaseWidget struct {
	Widget
	window *glfw.Window
}

func (w *UnderscoreSepToCamelCaseWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)

	//s := w.window.GetClipboardString()
	s := "get_clipboard_string"
	// E.g., get_clipboard_string -> GetClipboardString
	s += " -> " + UnderscoreSepToCamelCase(s)
	w.size[0] = float64(8 * len(s))
	w.size[1] = 16

	gl.Color3d(0.3, 0.3, 0.3)
	gl.Rectd(0-1, 0-1, gl.Double(w.size[0]+1), gl.Double(w.size[1]+1))
	gl.Color3d(1, 1, 1)
	gl.Rectd(0, 0, gl.Double(w.size[0]), gl.Double(w.size[1]))

	gl.Color3d(0, 0, 0)
	PrintText(mathgl.Vec2d{0, 0}, s)
}

// ---

type ChannelExpeWidget struct {
	*CompositeWidget
	cmd *exec.Cmd
	ch  ChanWriter
}

func NewChannelExpeWidget(pos mathgl.Vec2d) *ChannelExpeWidget {
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
	w.CompositeWidget = NewCompositeWidget(pos,
		[]Widgeter{
			NewTextBoxWidget(mathgl.Vec2d{0, 0}),
			NewTriButtonExternalStateWidget(mathgl.Vec2d{0, -fontHeight - 2}, func() bool { return w.cmd != nil }, action),
		})

	UniversalClock.AddChangeListener(w)

	return w
}

/*func (w *ChannelExpeWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)

	LongestLine := uint32(0)
	lines := GetLines(w.Content)
	for _, line := range lines {
		lineLength := ExpandedLength(line)
		if lineLength > LongestLine {
			LongestLine = lineLength
		}
	}

	w.size = mathgl.Vec2d{float64(8 * LongestLine), float64(16 * len(lines))}

	gl.Color3d(0.3, 0.3, 0.3)
	gl.Rectd(0-1, 0-1, gl.Double(w.size[0]+1), gl.Double(w.size[1]+1))
	gl.Color3d(1, 1, 1)
	gl.Rectd(0, 0, gl.Double(w.size[0]), gl.Double(w.size[1]))

	gl.Color3d(0, 0, 0)
	PrintText(mathgl.Vec2d{}, w.Content)
}*/

func (w *ChannelExpeWidget) NotifyChange() {
	select {
	case b, ok := <-w.ch:
		if ok {
			box := w.CompositeWidget.Widgets[0].(*TextBoxWidget)
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
	dst      *TextBoxWidget
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

	this.w.cmd = this.template.NewCommand()
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

func NewLiveCmdExpeWidget(pos mathgl.Vec2d, dependees []DepNode2I, template CmdFactory) *LiveCmdExpeWidget {
	w := &LiveCmdExpeWidget{TextBoxWidget: NewTextBoxWidget(pos), finishedChan: make(chan *os.ProcessState)}

	// THINK: The only reason to have a separate command node is because current NotifyChange() does not tell the originator of change, so I can't tell UniversalClock's changes from dependee changes (and I need to do different actions for each)
	w.commandNode = &commandNode{w: w, template: template}
	w.commandNode.AddSources(dependees...)

	UniversalClock.AddChangeListener(w)

	return w
}

// HACK: I'm overriding NotifyChange() of TextBoxWidget here; it works because TextBoxWidget uses its own, but this isn't good
func (w *LiveCmdExpeWidget) NotifyChange() {
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
	// TODO: I think this should be moved to a new proper Layout2() method or something. Render() is for putting it on the screen, not making changes.
	MakeUpdated(w.commandNode) // THINK: Is this a hack or is this the way to go?

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
	*TextBoxWidget
	actionNode                  *actionNode
	outChan                     chan timestampString
	lastStartedT, lastFinishedT uint32
}

func NewLiveGoroutineExpeWidget(pos mathgl.Vec2d, dependees []DepNode2I, params func() interface{}, action func(interface{}) string) *LiveGoroutineExpeWidget {
	/*dst := NewTextBoxWidget(mathgl.Vec2d{0, 0})
	src.AfterChange = append(src.AfterChange, func() {
		// TODO: Async?
		dst.Content.Set(GetForcedUseFromImport(src.Content.Content()))
	})*/
	/*dst := NewTextBoxWidgetContentFunc(mathgl.Vec2d{0, 0}, func() string {
		// TODO: Async?
		if strings.TrimSpace(src.Content.Content()) != "" {
			time.Sleep(time.Second)
			return GetForcedUseFromImport(strings.TrimSpace(src.Content.Content()))
		} else {
			return ""
		}
	}, []DepNodeI{src})*/

	w := &LiveGoroutineExpeWidget{TextBoxWidget: NewTextBoxWidget(pos), outChan: make(chan timestampString)}

	// THINK: The only reason to have a separate action node is because current NotifyChange() does not tell the originator of change, so I can't tell UniversalClock's changes from dependee changes (and I need to do different actions for each)
	w.actionNode = &actionNode{owner: w, params: params, action: action}
	w.actionNode.AddSources(dependees...)

	UniversalClock.AddChangeListener(w)

	return w
}

// HACK: I'm overriding NotifyChange() of TextBoxWidget here; it works because TextBoxWidget uses its own, but this isn't good
func (w *LiveGoroutineExpeWidget) NotifyChange() {
	select {
	case s, ok := <-w.outChan:
		if ok {
			if s.t > w.lastFinishedT {
				w.lastFinishedT = s.t

				SetViewGroup(w.Content, s.s)
				redraw = true
			}
		}
	default:
	}
}

func (w *LiveGoroutineExpeWidget) Render() {
	// TODO: I think this should be moved to a new proper Layout2() method or something. Render() is for putting it on the screen, not making changes.
	MakeUpdated(w.actionNode) // THINK: Is this a hack or is this the way to go?

	w.TextBoxWidget.Render()
}

// ---

type HttpServerTestWidget struct {
	*FlowLayoutWidget
	started        bool
	stopServerChan chan bool
}

func NewHttpServerTestWidget(pos mathgl.Vec2d) *HttpServerTestWidget {
	w := &HttpServerTestWidget{stopServerChan: make(chan bool)}
	action := func() {
		if !w.started {
			go func() {
				err := ListenAndServeStoppable("localhost:8080", nil, w.stopServerChan)
				CheckError(err)
			}()
		} else {
			w.stopServerChan <- true
		}
		w.started = !w.started // TODO: Factor this out to toggle-button?
	}
	action()
	w.FlowLayoutWidget = NewFlowLayoutWidget(pos,
		[]Widgeter{
			NewTriButtonExternalStateWidget(np, func() bool { return w.started }, action),
			NewTextLabelWidgetString(np, "http"),
		}, nil)

	return w
}

// ---

type FileOpener struct {
	editor     ViewGroupI
	openedFile ViewGroupI
	DepNode2
}

func NewFileOpener(editor ViewGroupI) *FileOpener {
	this := &FileOpener{editor: editor}
	return this
}

func (this *FileOpener) Update() {
	if this.openedFile != nil {
		this.editor.RemoveView(this.openedFile)
		this.openedFile.(*FileView).Close()
		this.openedFile = nil

		SetViewGroup(this.editor, "")
	}

	if path := this.GetSources()[0].(*FolderListingWidget).GetSelectedPath(); strings.HasSuffix(path, ".go") {
		this.openedFile = NewFileView(path)

		this.openedFile.AddAndSetViewGroup(this.editor, TryReadFile(path))
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

type MultilineContentDepDumper struct {
	*MultilineContent
	DepNode2
}

func NewMultilineContentDepDumper(sources ...DepNode2I) MultilineContentI {
	this := &MultilineContentDepDumper{MultilineContent: NewMultilineContent()}
	this.AddSources(sources...)
	return this
}

func (this *MultilineContentDepDumper) Update() {
	content := TrimLastNewline(goon.Sdump(this.GetSources()[0].(*GoPackageListingPureWidget).GetSelected()))
	//content := this.Content() + "+"
	SetViewGroup(this.MultilineContent, content)
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
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
	//gl.Rotated(float64(spinner), 0, 0, 1)
	gl.Rotated(gl.Double(w.Spinner), 0, 0, 1)
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

func NewFpsWidget(pos mathgl.Vec2d) *FpsWidget {
	return &FpsWidget{Widget: NewWidget(pos, np)}
}

func (w *FpsWidget) Render() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
	gl.Begin(gl.LINES)
	gl.Color3d(1, 0, 0)
	gl.Vertex2d(gl.Double(0), gl.Double(-1000.0/60))
	gl.Vertex2d(gl.Double(30), gl.Double(-1000.0/60))
	gl.End()
	for index, sample := range w.samples {
		var color mathgl.Vec3d
		if sample.Render <= 1000.0/60*1.25 {
			color = mathgl.Vec3d{0, 0, 0}
		} else {
			color = mathgl.Vec3d{1, 0, 0}
		}
		DrawBorderlessBox(mathgl.Vec2d{float64(30 - len(w.samples) + index), -sample.Render}, mathgl.Vec2d{1, sample.Render}, color)
		DrawBorderlessBox(mathgl.Vec2d{float64(30 - len(w.samples) + index), -sample.Total}, mathgl.Vec2d{1, sample.Total - sample.Render}, mathgl.Vec3d{0.65, 0.65, 0.65})
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
}

func NewSearchableListWidget(pos mathgl.Vec2d, entries Strings2) *SearchableListWidget {
	searchField := NewTextFieldWidget(np)
	//listWidget := NewListWidget(mathgl.Vec2d{0, fontHeight + 2}, entries)
	listWidget := NewFilterableSelecterWidget(mathgl.Vec2d{0, fontHeight + 2}, entries, searchField)

	w := &SearchableListWidget{NewCompositeWidget(pos, []Widgeter{searchField, listWidget})}

	return w
}

func (w *SearchableListWidget) OnSelectionChanged() DepNode2ManualI {
	return w.CompositeWidget.Widgets[1].(DepNode2ManualI)
}

func (w *SearchableListWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && // TODO: GetHoverer() // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { // TODO: GetHoverer() // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		// HACK: Temporarily set both widgets as mapping here
		keyboardPointer.OriginMapping = w.CompositeWidget.Widgets
	}
}

func (w *SearchableListWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		hits := []Widgeter{w}
		hits = append(hits, w.CompositeWidget.Hit(ParentPosition)...)
		return hits
	} else {
		return nil
	}
}

// ---

type FilterableStrings2 struct {
	filteredEntries []fmt.Stringer

	DepNode2
}

// TODO: Change *TextFieldWidget for String interface (that includes DepNode2I)
func NewFilterableStrings2(source Strings2, filter *TextFieldWidget) *FilterableStrings2 {
	this := &FilterableStrings2{}
	this.AddSources(source, filter)
	return this
}

func (this *FilterableStrings2) Get(index uint64) fmt.Stringer {
	return this.filteredEntries[index]
}

func (this *FilterableStrings2) Len() uint64 {
	return uint64(len(this.filteredEntries))
}

func (this *FilterableStrings2) Update() {
	source := this.GetSources()[0].(Strings2)
	filter := this.GetSources()[1].(*TextFieldWidget)

	this.filteredEntries = nil
	for index := uint64(0); index < source.Len(); index++ {
		entry := source.Get(index).String()

		if filter.Content != "" {
			if !strings.Contains(strings.ToLower(entry), strings.ToLower(filter.Content)) { // TODO: Do case folding correctly
				continue
			}
		}

		this.filteredEntries = append(this.filteredEntries, source.Get(index))
	}
}

// ---

type Strings2 interface {
	Get(uint64) fmt.Stringer
	Len() uint64

	DepNode2I
}

type Selecter interface {
	GetSelected() fmt.Stringer
}

// TODO: Refactor other list-like widgets to use this?
type FilterableSelecterWidget struct {
	Widget
	longestEntryLength int
	selected           uint64 // index of selected entry [0, len)
	manuallyPicked     fmt.Stringer
	DepNode2Manual     // SelectionChanged
	layoutDepNode2     DepNode2Func

	entries Strings2

	filter *TextFieldWidget
}

// TODO: Change *TextFieldWidget for String interface (that includes DepNode2I)
func NewFilterableSelecterWidget(pos mathgl.Vec2d, entries Strings2, filter *TextFieldWidget) *FilterableSelecterWidget {
	entries = NewFilterableStrings2(entries, filter)

	w := &FilterableSelecterWidget{Widget: NewWidget(pos, np), entries: entries, filter: filter}

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
		w.manuallyPicked = nil
	}

	w.Layout()

	w.NotifyAllListeners()
}

// Pre-condition: w.entries.Len() > 0
func (w *FilterableSelecterWidget) selectionChangedTest() {
	// TEST
	if scrollPane, ok := w.Parent().(*ScrollPaneWidget); ok {
		scrollPane.ScrollToArea(mathgl.Vec2d{0, float64(w.selected * fontHeight)}, mathgl.Vec2d{float64(w.longestEntryLength * fontWidth), fontHeight})
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

func (w *FilterableSelecterWidget) Render() {
	// TODO: Move to Layout2()
	MakeUpdated(&w.layoutDepNode2)

	DrawNBox(w.pos, w.size)

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer
	hasTypingFocus := keyboardPointer != nil && keyboardPointer.OriginMapping.ContainsWidget(w)

	// Render only visible lines.
	// TODO: Generalize this.
	index, lastIndex := 0, int(w.entries.Len()-1)
	if topIndex := int(WidgeterS{w}.GlobalToLocal(mathgl.Vec2d{})[1] / fontHeight); topIndex > index {
		index = topIndex
	}
	_, height := globalWindow.GetSize() // HACK
	if bottomIndex := int(WidgeterS{w}.GlobalToLocal(mathgl.Vec2d{0, float64(height)})[1] / fontHeight); bottomIndex < lastIndex {
		lastIndex = bottomIndex
	}
	for ; index <= lastIndex; index++ {
		entry := w.entries.Get(uint64(index)).String()
		if w.selected == uint64(index) {
			if hasTypingFocus {
				DrawBorderlessBox(w.pos.Add(mathgl.Vec2d{0, float64(index * fontHeight)}), mathgl.Vec2d{w.size[0], fontHeight}, mathgl.Vec3d{0.21, 0.45, 0.84})
				gl.Color3d(1, 1, 1)
			} else {
				DrawBorderlessBox(w.pos.Add(mathgl.Vec2d{0, float64(index * fontHeight)}), mathgl.Vec2d{w.size[0], fontHeight}, mathgl.Vec3d{0.83, 0.83, 0.83})
				gl.Color3d(0, 0, 0)
			}
		} else {
			gl.Color3d(0, 0, 0)
		}

		PrintText(w.pos.Add(mathgl.Vec2d{0, float64(index * fontHeight)}), entry)
	}
}

func (w *FilterableSelecterWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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
		globalPosition := mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
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

type ImportPathFoundSelector interface {
	GetSelected() *ImportPathFound
	DepNode2I
}

type GoPackageListingPureWidget struct {
	Widget
	longestEntryLength int
	selected           uint64 // 0 is unselected, else index+1 is selected
	DepNode2Manual            // SelectionChanged

	goPackages chan ImportPathFound // TODO: Generalize this, this should be refactored out into an external DepNode2I Strings2 with goPackages.

	entries []ImportPathFound
}

func NewGoPackageListingPureWidget() *GoPackageListingPureWidget {
	w := &GoPackageListingPureWidget{Widget: NewWidget(np, np), goPackages: make(chan ImportPathFound, 64)}

	go gist8018045.GetGoPackages(w.goPackages)
	UniversalClock.AddChangeListener(w)

	return w
}

func (this *GoPackageListingPureWidget) GetSelected() *ImportPathFound {
	if this.selected == 0 {
		return nil
	}
	return &this.entries[this.selected-1]
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
}

func (w *GoPackageListingPureWidget) selectionChangedTest() {
	if w.selected != 0 {
		// TEST
		if scrollPane, ok := w.Parent().(*ScrollPaneWidget); ok {
			scrollPane.ScrollToArea(mathgl.Vec2d{0, float64((w.selected - 1) * fontHeight)}, mathgl.Vec2d{float64(w.longestEntryLength * fontWidth), fontHeight})
		}
	}

	ExternallyUpdated(w)
}

func (w *GoPackageListingPureWidget) Layout() {
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

func (w *GoPackageListingPureWidget) Render() {
	DrawNBox(w.pos, w.size)

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	for i, entry := range w.entries {
		if w.selected == uint64(i+1) {
			if hasTypingFocus {
				DrawBorderlessBox(w.pos.Add(mathgl.Vec2d{0, float64(i * fontHeight)}), mathgl.Vec2d{w.size[0], fontHeight}, mathgl.Vec3d{0.21, 0.45, 0.84})
				gl.Color3d(1, 1, 1)
			} else {
				DrawBorderlessBox(w.pos.Add(mathgl.Vec2d{0, float64(i * fontHeight)}), mathgl.Vec2d{w.size[0], fontHeight}, mathgl.Vec3d{0.83, 0.83, 0.83})
				gl.Color3d(0, 0, 0)
			}
		} else {
			gl.Color3d(0, 0, 0)
		}

		PrintText(w.pos.Add(mathgl.Vec2d{0, float64(i * fontHeight)}), entry.ImportPath())
	}
}

func (w *GoPackageListingPureWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}
func (w *GoPackageListingPureWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		keyboardPointer.OriginMapping = []Widgeter{w}
	}

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	// Check if button 0 was released (can't do pressed atm because first on-focus event gets ignored, and it promotes on-move switching, etc.)
	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false) {
		globalPosition := mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
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

type FolderListingWidget struct {
	*CompositeWidget
	flow *FlowLayoutWidget // HACK: Shortcut to CompositeWidget.Widgets[0]

	DepNode2Manual // SelectionChanged
}

func NewFolderListingWidget(pos mathgl.Vec2d, path string) *FolderListingWidget {
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
	hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	for i, v := range w.entries {
		if w.selected == uint64(i+1) {
			if hasTypingFocus {
				DrawBorderlessBox(w.pos.Add(mathgl.Vec2d{0, float64(i * fontHeight)}), mathgl.Vec2d{w.size[0], fontHeight}, mathgl.Vec3d{0.21, 0.45, 0.84})
				gl.Color3d(1, 1, 1)
			} else {
				DrawBorderlessBox(w.pos.Add(mathgl.Vec2d{0, float64(i * fontHeight)}), mathgl.Vec2d{w.size[0], fontHeight}, mathgl.Vec3d{0.83, 0.83, 0.83})
				gl.Color3d(0, 0, 0)
			}
		} else {
			gl.Color3d(0, 0, 0)
		}

		if v.IsDir() {
			PrintText(w.pos.Add(mathgl.Vec2d{0, float64(i * fontHeight)}), v.Name()+PathSeparator)
		} else {
			PrintText(w.pos.Add(mathgl.Vec2d{0, float64(i * fontHeight)}), v.Name())
		}
	}
}

func (w *FolderListingPureWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
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
	hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	// Check if button 0 was released (can't do pressed atm because first on-focus event gets ignored, and it promotes on-move switching, etc.)
	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false) {
		globalPosition := mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
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

type Bool struct {
	bool
	DepNode2Manual
}

func (t *Bool) Get() bool {
	return t.bool
}
func (t *Bool) Set(value bool) {
	if t.bool != value {
		t.bool = value
		ExternallyUpdated(&t.DepNode2Manual)
	}
}
func (t *Bool) Toggle() {
	t.bool = !t.bool
	ExternallyUpdated(&t.DepNode2Manual)
}

type GoonWidget struct {
	*CompositeWidget
	title    string
	a        reflect.Value
	expanded *TriButtonWidget
}

func NewGoonWidget(pos mathgl.Vec2d, a interface{}) *GoonWidget {
	return newGoonWidget(pos, GetParentArgExprAsString(1)[1:], reflect.ValueOf(a))
}

func newGoonWidget(pos mathgl.Vec2d, title string, a reflect.Value) *GoonWidget {
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
			w.expanded = NewTriButtonWidget(mathgl.Vec2d{-fontHeight - 2}, func() { w.flip() })
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
			widgets = append(widgets, setupInternals3(vt.Field(i).Name, v.Field(i)))
		}
	case reflect.Map:
		for _, key := range v.MapKeys() {
			addrValue := UnsafeReflectValue(v.MapIndex(key))
			widgets = append(widgets, setupInternals3(key.String(), addrValue))
		}
	case reflect.Array, reflect.Slice:
		for i := 0; i < v.Len(); i++ {
			widgets = append(widgets, setupInternals3(strconv.Itoa(i), v.Index(i)))
		}
	case reflect.Ptr, reflect.Interface:
		widgets = append(widgets, setupInternals3("*", v.Elem()))
	}

	widgets = append(widgets, NewTextLabelWidgetString(np, "}"))

	return NewFlowLayoutWidget(np, widgets, &FlowLayoutWidgetOptions{FlowLayoutType: VerticalLayout})
}

func setupInternals3(titleString string, a reflect.Value) Widgeter {
	tab := mathgl.Vec2d{fontWidth * 4}

	/*if !a.CanInterface() {
		a = UnsafeReflectValue(a)
	}*/

	var w Widgeter
	if a.Kind() == reflect.Float64 && a.Addr().CanInterface() {
		title := NewTextLabelWidgetString(np, titleString+": ")
		t := NewTest2Widget(np, a.Addr().Interface().(*float64))
		w = NewFlowLayoutWidget(tab, []Widgeter{title, t}, nil)
	} else if a.Kind() == reflect.String && a.Addr().CanInterface() {
		title := NewTextLabelWidgetString(np, titleString+": ")
		t := NewTextBoxWidgetExternalContent(np, NewMultilineContentPointer(a.Addr().Interface().(*string)))
		w = NewFlowLayoutWidget(tab, []Widgeter{title, t}, nil)
	} else if vv := a; vv.CanAddr() {
		w = newGoonWidget(tab, titleString, vv.Addr())
	} else if vv := a; (vv.Kind() == reflect.Interface || vv.Kind() == reflect.Ptr) && vv.Elem().CanAddr() { // HACK
		w = newGoonWidget(tab, titleString, vv.Elem().Addr())
	} else {
		//w = NewTextLabelWidgetString(tab, goon.Sdump(vv))
		w = NewTextLabelWidgetString(tab, fmt.Sprintf("(%s)(can't addr... %s)", vv.Kind().String(), vv.String()))
	}

	spacer := NewCompositeWidget(np, []Widgeter{w})

	return spacer
}

// ---

func ExpandedLength(s string) uint32 {
	segments := strings.Split(s, "\t")
	var advance uint32
	for segmentIndex, segment := range segments {
		advance += uint32(len(segment))
		if segmentIndex != len(segments)-1 {
			advance += 4 - (advance % 4)
		}
	}
	return advance
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
	if cp.Logical() > uint32(len(cp.w.Content())) {
		cp.Move(+3)
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
				cp.willMove(-1)
			}
		}
	case +1:
		if cp.Logical() < uint32(len(cp.w.Content())) {
			if jumpWords {
				// Skip spaces to the right
				LookAt := cp.Logical()
				for LookAt < uint32(len(cp.w.Content())) && !isCoreCharacter(cp.w.Content()[LookAt]) {
					LookAt++
				}
				// Skip non-spaces to the right
				for LookAt < uint32(len(cp.w.Content())) && isCoreCharacter(cp.w.Content()[LookAt]) {
					LookAt++
				}

				cp.willMoveH(int32(LookAt) - int32(cp.Logical()))
			} else {
				cp.willMove(+1)
			}
		}
	}
}

func (cp *caretPositionInternal) willMove(amount int8) {
	switch amount {
	case -1:
		if cp.positionWithinLine > 0 {
			cp.positionWithinLine--
		} else { //if cp.lineIndex > 0
			cp.lineIndex--
			cp.positionWithinLine = cp.w.Line(cp.lineIndex).Length
		}
	case +1:
		if cp.positionWithinLine < cp.w.Line(cp.lineIndex).Length {
			cp.positionWithinLine++
		} else { //if cp.lineIndex < some_max
			cp.lineIndex++
			cp.positionWithinLine = 0
		}
	}

	cp.targetExpandedX, _ = cp.ExpandedPosition() // TODO: More direct
}

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
	}

	cp.targetExpandedX, _ = cp.ExpandedPosition() // TODO: More direct
}

// TODO: Change amount to a proper type with 2 values, etc. to avoid confusion with other funcs where amount can be an arbitrary number.
func (cp *caretPositionInternal) TryMoveV(amount int8) {
	switch amount {
	case -1:
		if cp.lineIndex > 0 {
			cp.lineIndex--
			line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start : cp.w.Line(cp.lineIndex).Start+cp.w.Line(cp.lineIndex).Length]
			cp.positionWithinLine = ExpandedToLogical(line, cp.targetExpandedX)

			ExternallyUpdated(&cp.DepNode2Manual)
		} else {
			cp.Move(-2)
		}
	case +1:
		if cp.lineIndex < cp.w.LenLines()-1 {
			cp.lineIndex++
			line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start : cp.w.Line(cp.lineIndex).Start+cp.w.Line(cp.lineIndex).Length]
			cp.positionWithinLine = ExpandedToLogical(line, cp.targetExpandedX)

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
func (cp *caretPositionInternal) Move(amount int8) {
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

	cp.targetExpandedX, _ = cp.ExpandedPosition() // TODO: More direct

	ExternallyUpdated(&cp.DepNode2Manual)
}

func (cp *caretPositionInternal) SetPositionFromPhysical(pos mathgl.Vec2d) {
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

	line := cp.w.Content()[cp.w.Line(cp.lineIndex).Start : cp.w.Line(cp.lineIndex).Start+cp.w.Line(cp.lineIndex).Length]
	cp.positionWithinLine = ExpandedToLogical(line, cp.targetExpandedX)

	ExternallyUpdated(&cp.DepNode2Manual)
}

func (cp *caretPositionInternal) TrySet(position uint32) {
	if position > uint32(cp.w.LenContent()) {
		position = uint32(cp.w.LenContent())
	}

	cp.lineIndex = sort.Search(cp.w.LenLines(), func(lineIndex int) bool {
		return cp.w.Line(lineIndex).Start >= position
	})
	cp.positionWithinLine = position - cp.w.Line(cp.lineIndex).Start

	cp.targetExpandedX, _ = cp.ExpandedPosition() // TODO: More direct

	ExternallyUpdated(&cp.DepNode2Manual)
}

// ExpandedPosition returns logical character units.
// Multiply by (fontWidth, fontHeight) to get physical coords.
func (cp *caretPositionInternal) ExpandedPosition() (x uint32, y uint32) {
	expandedCaretPosition := ExpandedLength(cp.w.Content()[cp.w.Line(cp.lineIndex).Start : cp.w.Line(cp.lineIndex).Start+cp.positionWithinLine])

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
	expandedCaretPosition := ExpandedLength(cp.w.Content()[cp.w.Line(caretLine).Start : cp.w.Line(caretLine).Start+caretPosition])

	cp.targetExpandedX, y = expandedCaretPosition, uint32(caretLine)

	return cp.targetExpandedX, y
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
			if cp.caretPosition.Logical() < uint32(len(cp.w.Content())) { // TODO: Where should this check happen
				cp.caretPosition.TryMoveH(amount, jumpWords)

				if !leaveSelection {
					cp.selectionPosition.MoveTo(cp.caretPosition)
				}
			}
		}
	}
}

// HACK: leaveSelection is currently an optional bool parameter
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
func (cp *CaretPosition) TryMoveV(amount int8, leaveSelectionOptional ...bool) {
	// HACK, TODO: Make leaveSelection a required parameter?
	leaveSelection := len(leaveSelectionOptional) != 0 && leaveSelectionOptional[0]

	cp.caretPosition.TryMoveV(amount)

	if !leaveSelection {
		cp.selectionPosition.MoveTo(cp.caretPosition)
	}
}

func (cp *CaretPosition) SetPositionFromPhysical(pos mathgl.Vec2d, leaveSelectionOptional ...bool) {
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

func (cp *CaretPosition) CreateSelectionIfNone(amount int32) {
	if !cp.anySelection() {
		if (amount < 0 && cp.caretPosition.Logical() >= uint32(-amount)) ||
			(amount > 0 && cp.caretPosition.Logical()+uint32(amount) <= uint32(cp.w.LenContent())) {

			cp.caretPosition.willMoveH(amount)
		}
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

// ---

type MultilineContentI interface {
	Content() string
	LenContent() int
	Lines() []contentLine // DEPRECATED
	LongestLine() uint32  // Line length

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

func (c *MultilineContent) Content() string      { return c.content }
func (c *MultilineContent) Lines() []contentLine { return c.lines }
func (c *MultilineContent) LongestLine() uint32  { return c.longestLine }

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
		lineLength := ExpandedLength(line)
		if lineLength > w.longestLine {
			w.longestLine = lineLength
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

	lastContentQUICKHACK string // HACK: Need this here for `git diff` testing
}

// TODO: Opening same path should result in same FileView, etc.
func NewFileView(path string) *FileView {
	this := &FileView{path: path}
	absPath, err := filepath.Abs(path)
	CheckError(err)
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
}

// TODO: Change detection, closing, etc.
func (this *FileView) NotifyChange() {
	// Check if the file has been changed externally, and if so, override this widget
	NewContent := TryReadFile(this.path)
	if NewContent != this.lastContentQUICKHACK {
		this.lastContentQUICKHACK = NewContent
		SetViewGroupOther(this, NewContent)
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
func (this *MultilineContentFuncInstant) Lines() []contentLine {
	this.MultilineContentFunc.NotifyChange()
	return this.MultilineContentFunc.Lines()
}
func (this *MultilineContentFuncInstant) LongestLine() uint32 {
	this.MultilineContentFunc.NotifyChange()
	return this.MultilineContentFunc.LongestLine()
}

// ---

type TextLabelWidget struct {
	Widget
	Content        MultilineContentI
	tooltip        Widgeter
	layoutDepNode2 DepNode2Func
}

func NewTextLabelWidgetExternalContent(pos mathgl.Vec2d, mc MultilineContentI) *TextLabelWidget {
	w := &TextLabelWidget{
		Widget:  NewWidget(pos, mathgl.Vec2d{0, 0}),
		Content: mc,
	}
	w.layoutDepNode2.UpdateFunc = func(DepNode2I) { w.NotifyChange() }
	w.layoutDepNode2.AddSources(mc) // TODO: What about removing w when it's "deleted"?
	keepUpdatedTEST = append(keepUpdatedTEST, &w.layoutDepNode2)
	return w
}

func NewTextLabelWidgetString(pos mathgl.Vec2d, s string) *TextLabelWidget {
	mc := NewMultilineContentString(s)
	w := NewTextLabelWidgetExternalContent(pos, mc)
	return w
}

func NewTextLabelWidgetGoon(pos mathgl.Vec2d, any interface{}) *TextLabelWidget {
	mc := NewMultilineContentFuncInstant(func() string { return TrimLastNewline(goon.Sdump(any)) })
	return NewTextLabelWidgetExternalContent(pos, mc)
}

func NewTextLabelWidgetStringTooltip(pos mathgl.Vec2d, s, tooltip string) *TextLabelWidget {
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
		PrintLine(mathgl.Vec2d{w.pos[0], w.pos[1] + float64(fontHeight*lineIndex)}, w.Content.Content()[contentLine.Start:contentLine.Start+contentLine.Length])
	}

	isHit := len(w.HoverPointers()) > 0
	// Tooltip
	if w.tooltip != nil && isHit {
		mousePointerPositionLocal := WidgeterS{w}.GlobalToLocal(mathgl.Vec2d{mousePointer.State.Axes[0], mousePointer.State.Axes[1]})
		w.tooltip.Layout()
		tooltipOffset := mathgl.Vec2d{0, -fontHeight - w.tooltip.Size()[1]}
		*w.tooltip.Pos() = w.pos.Add(mousePointerPositionLocal).Add(tooltipOffset)
		w.tooltip.Render()
	}
}

// ---

type TextBoxWidget struct {
	Widget
	Content        MultilineContentI
	caretPosition  *CaretPosition
	Private        Bool
	layoutDepNode2 DepNode2Func
	scrollToCaret  DepNode2Func // TODO: DepNode2Event?

	// TESTS
	DiffsTest []diffmatchpatch.Diff
	Side      int8

	ExtensionsTest []Widgeter
}

func NewTextBoxWidget(pos mathgl.Vec2d) *TextBoxWidget {
	mc := NewMultilineContent()
	return NewTextBoxWidgetExternalContent(pos, mc)
}

func NewTextBoxWidgetExternalContent(pos mathgl.Vec2d, mc MultilineContentI) *TextBoxWidget {
	w := &TextBoxWidget{
		Widget:        NewWidget(pos, mathgl.Vec2d{0, 0}),
		Content:       mc,
		caretPosition: NewCaretPosition(mc),
	}
	w.layoutDepNode2.UpdateFunc = func(DepNode2I) { w.NotifyChange() }
	w.layoutDepNode2.AddSources(mc) // TODO: What about removing w when it's "deleted"?

	// TEST
	w.scrollToCaret.UpdateFunc = func(DepNode2I) {
		if scrollPane, ok := w.Parent().(*ScrollPaneWidget); ok {
			expandedCaretPosition, caretLine := w.caretPosition.caretPosition.ExpandedPosition()

			scrollPane.ScrollToArea(mathgl.Vec2d{float64(expandedCaretPosition * fontWidth), float64(caretLine * fontHeight)}, mathgl.Vec2d{0, fontHeight})
		}
	}
	w.scrollToCaret.AddSources(w.caretPosition)

	return w
}

func (w *TextBoxWidget) CenterOnCaretPosition(caretPosition uint32) {
	w.caretPosition.TrySet(caretPosition)

	// HACK: This kinda conflicts/overlaps with MakeUpdated(&w.scrollToCaret), which also tries to scroll the scroll pane... Find a better way.
	if scrollPane, ok := w.Parent().(*ScrollPaneWidget); ok {
		expandedCaretPosition, caretLine := w.caretPosition.caretPosition.ExpandedPosition()

		scrollPane.CenterOnArea(mathgl.Vec2d{float64(expandedCaretPosition * fontWidth), float64(caretLine * fontHeight)}, mathgl.Vec2d{0, fontHeight})
	}
}

func (w *TextBoxWidget) NotifyChange() {
	w.caretPosition.NotifyContentChanged()

	w.Layout()

	w.NotifyAllListeners()

	// TODO: Figure out if this should be here... is it a big deal if it gets called here AND elsewhere?
	redraw = true
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

func (w *TextBoxWidget) Render() {
	// TODO: Move to Layout2()
	MakeUpdated(&w.layoutDepNode2)
	MakeUpdated(&w.scrollToCaret)

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

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
		DrawYBox(w.pos, w.size)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.pos, w.size)
	} else if hasTypingFocus {
		DrawYBox(w.pos, w.size)
	} else {
		DrawNBox(w.pos, w.size)
	}

	// DEBUG, HACK: Temporarily use cursor to highlight entire line when inactive, etc.
	if !hasTypingFocus {
		_, caretLine := w.caretPosition.caretPosition.ExpandedPosition()

		// Highlight line
		gl.PushMatrix()
		gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
		gl.Color3d(0.75, 0.75, 0.75)
		gl.Recti(gl.Int(0), gl.Int(caretLine*fontHeight), gl.Int(w.size[0]), gl.Int(caretLine*fontHeight)+fontHeight)
		gl.PopMatrix()
	}

	if w.DiffsTest == nil {
		gl.Color3d(0, 0, 0)
		if !w.Private.Get() {
			// Render only visible lines.
			// TODO: Generalize this.
			beginLineIndex, endLineIndex := 0, w.Content.LenLines()
			if beginVisibleLineIndex := int(WidgeterS{w}.GlobalToLocal(mathgl.Vec2d{})[1] / fontHeight); beginVisibleLineIndex > beginLineIndex {
				beginLineIndex = intmath.MinInt(beginVisibleLineIndex, endLineIndex)
			}
			_, height := globalWindow.GetSize() // HACK: Should be some viewport
			if endVisibleLineIndex := int(WidgeterS{w}.GlobalToLocal(mathgl.Vec2d{0, float64(height)})[1]/fontHeight + 1); endVisibleLineIndex < endLineIndex {
				endLineIndex = intmath.MaxInt(endVisibleLineIndex, beginLineIndex)
			}

			// TEST: Apply highlighting for ".go" files
			if uri, ok := w.Content.GetUriForProtocol("file://"); ok && strings.HasSuffix(string(uri), ".go") {

				glt := NewOpenGlStream(w.pos.Add(mathgl.Vec2d{0, float64(fontHeight * beginLineIndex)}))

				// TODO: Cache results of scanning
				//src := []byte(w.Content.Content())
				src := []byte(w.Content.Content()[w.Content.Line(beginLineIndex).Start:w.Content.Line(endLineIndex).Start])

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

					offset := uint32(fset.Position(pos).Offset) + w.Content.Line(beginLineIndex).Start

					if offset < w.Content.Line(beginLineIndex).Start {
						continue
					} else if offset >= w.Content.Line(endLineIndex).Start {
						break
					}

					// HACK
					tempCaretPosition := &caretPositionInternal{w: w.Content}
					x, y := tempCaretPosition.SetHint(offset, beginLineIndex)
					glt.SetPosWithExpandedPosition(w.pos, x, y)

					switch {
					case tok.IsKeyword() || tok.IsOperator() && tok < token.LPAREN:
						gl.Color3d(0.004, 0, 0.714)
					case tok.IsLiteral() && tok != token.IDENT:
						gl.Color3d(0.804, 0, 0)
					case lit == "false" || lit == "true":
						gl.Color3d(0.008, 0.024, 1)
					case tok == token.COMMENT:
						gl.Color3d(0, 0.706, 0.094)
					default:
						gl.Color3d(0, 0, 0)
					}

					if lit != "" {
						glt.PrintText(lit)
					} else {
						glt.PrintText(tok.String())
					}
				}

				//glt.PrintText(w.Content.Content()[w.Content.Line(beginLineIndex).Start:w.Content.Line(endLineIndex).Start])

			} else {
				// Selection
				// TODO: Refactor (make this more concise and easier to understand)
				min, max := w.caretPosition.SelectionRange()
				min = intmath.MaxUint32(min, w.Content.Line(beginLineIndex).Start)
				min = intmath.MinUint32(min, w.Content.Line(endLineIndex).Start)
				max = intmath.MaxUint32(max, w.Content.Line(beginLineIndex).Start)
				max = intmath.MinUint32(max, w.Content.Line(endLineIndex).Start)

				glt := NewOpenGlStream(w.pos.Add(mathgl.Vec2d{0, float64(fontHeight * beginLineIndex)}))
				glt.PrintText(w.Content.Content()[w.Content.Line(beginLineIndex).Start:min])
				if hasTypingFocus {
					glt.BackgroundColor = &selectedTextColor
				} else {
					glt.BackgroundColor = &selectedTextInactiveColor
				}
				glt.PrintText(w.Content.Content()[min:max])
				glt.BackgroundColor = nil
				glt.PrintText(w.Content.Content()[max:w.Content.Line(endLineIndex).Start])
			}
		} else {
			for lineIndex := 0; lineIndex < w.Content.LenLines(); lineIndex++ {
				contentLine := w.Content.Line(lineIndex)
				PrintLine(mathgl.Vec2d{w.pos[0], w.pos[1] + float64(fontHeight*lineIndex)}, strings.Repeat("*", int(contentLine.Length)))
			}
		}
	} else {
		gl.Color3d(0, 0, 0)
		glt := NewOpenGlStream(w.pos)
		//glt.PrintText(w.Content.Content())
		for _, diff := range w.DiffsTest {
			if diff.Type == w.Side && diff.Type == -1 {
				glt.BackgroundColor = &mathgl.Vec3d{1, 0.8, 0.8}
			} else if diff.Type == w.Side && diff.Type == +1 {
				glt.BackgroundColor = &mathgl.Vec3d{0.8, 1, 0.8}
			} else if diff.Type == 0 {
				glt.BackgroundColor = nil
			} else {
				continue
			}
			glt.PrintText(diff.Text)
		}
	}

	// Go Errors Test
	/*if goCompileErrorsEnabledTest != nil && goCompileErrorsEnabledTest.state() {
		if contentFile, ok := w.Content.(*MultilineContentFile); ok && strings.HasSuffix(contentFile.Path(), ".go") {
			gl.Color3d(0, 0, 0)
			glt := NewOpenGlStream(np)
			glt.BackgroundColor = &mathgl.Vec3d{1, 0.5, 0.5}
			for _, goErrorMessage := range goCompileErrorsManagerTest.All[contentFile.Path()] { // TODO: Path() isn't guaranteed to be absolute, so either change that, or use something else here
				expandedLineLength := ExpandedLength(w.Content.Content()[w.Content.Lines()[goErrorMessage.LineIndex].Start : w.Content.Lines()[goErrorMessage.LineIndex].Start+w.Content.Lines()[goErrorMessage.LineIndex].Length])
				glt.SetPos(w.pos.Add(mathgl.Vec2d{fontWidth * float64(expandedLineLength+1), fontHeight * float64(goErrorMessage.LineIndex)}))
				glt.PrintLine(goErrorMessage.Message)
			}
		}
	}*/
	for _, uri := range w.Content.GetAllUris() {
		if _, ok := goCompileErrorsManagerTest.All[uri]; ok {
			gl.Color3d(0, 0, 0)
			glt := NewOpenGlStream(np)
			glt.BackgroundColor = &mathgl.Vec3d{1, 0.5, 0.5}
			for _, goErrorMessage := range goCompileErrorsManagerTest.All[uri] {
				if goErrorMessage.LineIndex < w.Content.LenLines() {
					expandedLineLength := ExpandedLength(w.Content.Content()[w.Content.Lines()[goErrorMessage.LineIndex].Start : w.Content.Lines()[goErrorMessage.LineIndex].Start+w.Content.Lines()[goErrorMessage.LineIndex].Length])
					glt.SetPos(w.pos.Add(mathgl.Vec2d{fontWidth * float64(expandedLineLength+1), fontHeight * float64(goErrorMessage.LineIndex)}))
					glt.PrintLine(goErrorMessage.Message)
				}
			}
		}
	}

	if hasTypingFocus {
		expandedCaretPosition, caretLine := w.caretPosition.caretPosition.ExpandedPosition()

		// Draw caret
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
		gl.Color3d(0, 0, 0)
		gl.Recti(gl.Int(expandedCaretPosition*fontWidth-1), gl.Int(caretLine*fontHeight), gl.Int(expandedCaretPosition*fontWidth+1), gl.Int(caretLine*fontHeight)+fontHeight)
	}
}
func (w *TextBoxWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
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
		keyboardPointer.OriginMapping = []Widgeter{w}
	}

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	//hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	if inputEvent.Pointer.VirtualCategory == POINTING {
		if inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 {
			// Leave selection on mouse release, or if shift is held down
			leaveSelection := inputEvent.Buttons[0] == false || (inputEvent.ModifierKey&glfw.ModShift != 0)

			globalPosition := mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
			localPosition := WidgeterS{w}.GlobalToLocal(globalPosition)
			w.caretPosition.SetPositionFromPhysical(localPosition, leaveSelection)
		} else if inputEvent.EventTypes[AXIS_EVENT] && inputEvent.InputId == 0 && inputEvent.Pointer.State.Button(0) {
			leaveSelection := true

			globalPosition := mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
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
			w.caretPosition.ReplaceSelectionWith("\n")
		case glfw.KeyTab:
			w.caretPosition.ReplaceSelectionWith("\t")
		case glfw.KeyLeft:
			if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModSuper {
				// TODO: Go to start of line-ish (toggle between real start and non-whitespace start); leave Move(-2) alone because it's used elsewhere for existing purpose
				w.caretPosition.Move(-2, inputEvent.ModifierKey&glfw.ModShift != 0)
			} else if inputEvent.ModifierKey & ^(glfw.ModShift|glfw.ModAlt) == 0 {
				w.caretPosition.TryMoveH(-1, inputEvent.ModifierKey&glfw.ModShift != 0, inputEvent.ModifierKey&glfw.ModAlt != 0)
			}
		case glfw.KeyRight:
			if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModSuper {
				w.caretPosition.Move(+2, inputEvent.ModifierKey&glfw.ModShift != 0)
			} else if inputEvent.ModifierKey & ^(glfw.ModShift|glfw.ModAlt) == 0 {
				w.caretPosition.TryMoveH(+1, inputEvent.ModifierKey&glfw.ModShift != 0, inputEvent.ModifierKey&glfw.ModAlt != 0)
			}
		case glfw.KeyUp:
			if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModSuper {
				w.caretPosition.Move(-3, inputEvent.ModifierKey&glfw.ModShift != 0)
			} else if inputEvent.ModifierKey & ^glfw.ModShift == 0 {
				w.caretPosition.TryMoveV(-1, inputEvent.ModifierKey&glfw.ModShift != 0)
			}
		case glfw.KeyDown:
			if inputEvent.ModifierKey & ^glfw.ModShift == glfw.ModSuper {
				w.caretPosition.Move(+3, inputEvent.ModifierKey&glfw.ModShift != 0)
			} else if inputEvent.ModifierKey & ^glfw.ModShift == 0 {
				w.caretPosition.TryMoveV(+1, inputEvent.ModifierKey&glfw.ModShift != 0)
			}
		case glfw.KeyA:
			if inputEvent.ModifierKey == glfw.ModSuper {
				// Select all
				w.caretPosition.Move(-3)
				w.caretPosition.Move(+3, true)
			}
		case glfw.KeyX:
			if !w.Private.Get() &&
				inputEvent.ModifierKey == glfw.ModSuper {

				if w.caretPosition.anySelection() {
					globalWindow.SetClipboardString(w.caretPosition.GetSelectionContent()) // TODO: Don't use globalWindow
					w.caretPosition.ReplaceSelectionWith("")
				}
			}
		case glfw.KeyC:
			if !w.Private.Get() &&
				inputEvent.ModifierKey == glfw.ModSuper {

				if w.caretPosition.anySelection() {
					globalWindow.SetClipboardString(w.caretPosition.GetSelectionContent()) // TODO: Don't use globalWindow
				}
			}
		case glfw.KeyV:
			if inputEvent.ModifierKey == glfw.ModSuper {
				if clipboard, err := globalWindow.GetClipboardString(); err == nil && clipboard != "" { // TODO: Don't use globalWindow
					w.caretPosition.ReplaceSelectionWith(clipboard)
				}
			}
		case glfw.KeyR:
			if inputEvent.ModifierKey == glfw.ModSuper {
				ExternallyUpdated(w.Content) // TODO: Need to make this apply only for event-based things; no point in forcibly updating pure data structures
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

func NewTextFileWidget(pos mathgl.Vec2d, path string) *TextFileWidget {
	// TODO: Opening the same file shouldn't result in a new MultilineContentFile
	ec := NewMultilineContentFile(path)
	w := &TextFileWidget{TextBoxWidget: NewTextBoxWidgetExternalContent(pos, ec)}
	return w
}

func (w *TextFileWidget) Path() string {
	return w.TextBoxWidget.Content.(*MultilineContentFile).Path()
}

// ---

func NewTextBoxWidgetContentFunc(pos mathgl.Vec2d, contentFunc func() string, dependees []DepNodeI) *TextBoxWidget {
	mc := NewMultilineContentFunc(contentFunc, dependees)
	w := NewTextBoxWidgetExternalContent(pos, mc)
	return w
}

func NewTextLabelWidgetContentFunc(pos mathgl.Vec2d, contentFunc func() string, dependees []DepNodeI) *TextLabelWidget {
	mc := NewMultilineContentFunc(contentFunc, dependees)
	w := NewTextLabelWidgetExternalContent(pos, mc)
	return w
}

// ---

type TextBoxValidationWidget struct {
	*TextBoxWidget
	validFunc func(MultilineContentI) bool
	DepNode   // Forward NotifyChanges from TextBoxWidget to us
}

func NewTextBoxValidationWidget(pos mathgl.Vec2d, validFunc func(MultilineContentI) bool) *TextBoxValidationWidget {
	w := &TextBoxValidationWidget{TextBoxWidget: NewTextBoxWidget(pos), validFunc: validFunc}
	w.TextBoxWidget.AddChangeListener(w) // Forward NotifyChanges from TextBoxWidget to us
	return w
}

// TODO: Remove after done testing...
func (w *TextBoxValidationWidget) IsValidTEST() bool {
	return w.validFunc(w.Content)
}

func (w *TextBoxValidationWidget) Render() {
	// TODO: Move to Layout2()
	MakeUpdated(&w.layoutDepNode2)
	MakeUpdated(&w.scrollToCaret)

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w.TextBoxWidget == keyboardPointer.OriginMapping[0]

	// HACK: Brute-force check the mouse pointer if it contains this widget
	isOriginHit := false
	for _, hit := range mousePointer.OriginMapping {
		if w == hit {
			isOriginHit = true
			break
		}
	}
	isHit := len(w.HoverPointers()) > 0

	var background mathgl.Vec3d
	if w.validFunc(w.Content) {
		background = mathgl.Vec3d{0.9, 1, 0.9}
	} else {
		background = mathgl.Vec3d{1, 0.9, 0.9}
	}

	// HACK: Assumes mousePointer rather than considering all connected pointing pointers
	if isOriginHit && mousePointer.State.IsActive() && isHit {
		DrawBox(w.pos, w.size, highlightColor, background)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawBox(w.pos, w.size, highlightColor, background)
	} else if hasTypingFocus {
		DrawBox(w.pos, w.size, highlightColor, background)
	} else {
		DrawBox(w.pos, w.size, mathgl.Vec3d{0.3, 0.3, 0.3}, background)
	}

	gl.Color3d(0, 0, 0)
	for lineIndex := 0; lineIndex < w.Content.LenLines(); lineIndex++ {
		contentLine := w.Content.Line(lineIndex)
		PrintLine(mathgl.Vec2d{w.pos[0], w.pos[1] + float64(fontHeight*lineIndex)}, w.Content.Content()[contentLine.Start:contentLine.Start+contentLine.Length])
	}

	if hasTypingFocus {
		expandedCaretPosition, caretLine := w.caretPosition.caretPosition.ExpandedPosition()

		// Draw caret
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
		gl.Color3d(0, 0, 0)
		gl.Recti(gl.Int(expandedCaretPosition*fontWidth-1), gl.Int(caretLine*fontHeight), gl.Int(expandedCaretPosition*fontWidth+1), gl.Int(caretLine*fontHeight)+fontHeight)
	}
}

func (w *TextBoxValidationWidget) NotifyChange() {
	if w.validFunc(w.Content) {
		w.NotifyAllListeners()
	}
}

// ---

type TextFieldWidget struct {
	Widget
	Content        string
	DepNode2Manual // ContentChanged
	CaretPosition  uint32
}

func NewTextFieldWidget(pos mathgl.Vec2d) *TextFieldWidget {
	return &TextFieldWidget{Widget: NewWidget(pos, mathgl.Vec2d{0, 0})}
}

func (w *TextFieldWidget) Layout() {
	if len(w.Content) < 3 {
		w.size[0] = float64(fontWidth * 3)
	} else {
		w.size[0] = float64(fontWidth * len(w.Content))
	}
	w.size[1] = fontHeight

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *TextFieldWidget) Render() {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer
	hasTypingFocus := keyboardPointer != nil && keyboardPointer.OriginMapping.ContainsWidget(w)

	// HACK: Setting the widget size in Render() is bad, because all the input calculations will fail before it's rendered
	w.Layout()

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
		DrawYBox(w.pos, w.size)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.pos, w.size)
	} else if hasTypingFocus {
		DrawYBox(w.pos, w.size)
	} else {
		DrawNBox(w.pos, w.size)
	}

	gl.Color3d(0, 0, 0)
	PrintSegment(w.pos, w.Content)

	if hasTypingFocus {
		// Draw caret
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
		gl.Color3d(0, 0, 0)
		gl.Recti(gl.Int(w.CaretPosition*fontWidth-1), 0, gl.Int(w.CaretPosition*fontWidth+1), fontHeight)
	}
}
func (w *TextFieldWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}
func (w *TextFieldWidget) ProcessEvent(inputEvent InputEvent) {
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

	// Need to check if either button 0 is currently down, or was released. This is so that caret gets set at cursor pos when widget gains focus.
	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.Pointer.State.Button(0) || (inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0)) {
		globalPosition := mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
		localPosition := WidgeterS{w}.GlobalToLocal(globalPosition)
		if localPosition[0] < 0 {
			w.CaretPosition = 0
		} else if localPosition[0] > float64(len(w.Content)*fontWidth) {
			w.CaretPosition = uint32(len(w.Content))
		} else {
			w.CaretPosition = uint32((localPosition[0] + fontWidth*0.5) / fontWidth)
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
		switch glfw.Key(inputEvent.InputId) {
		case glfw.KeyBackspace:
			if w.CaretPosition >= 1 {
				w.CaretPosition--
				w.Content = w.Content[:w.CaretPosition] + w.Content[w.CaretPosition+1:]
				ExternallyUpdated(&w.DepNode2Manual)
			}
		case glfw.KeyDelete:
			if w.CaretPosition+1 <= uint32(len(w.Content)) {
				w.Content = w.Content[:w.CaretPosition] + w.Content[w.CaretPosition+1:]
				ExternallyUpdated(&w.DepNode2Manual)
			}
		case glfw.KeyLeft:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.CaretPosition = 0
			} else if inputEvent.ModifierKey == 0 {
				if w.CaretPosition >= 1 {
					w.CaretPosition--
				}
			}
		case glfw.KeyRight:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.CaretPosition = uint32(len(w.Content))
			} else if inputEvent.ModifierKey == 0 {
				if w.CaretPosition < uint32(len(w.Content)) {
					w.CaretPosition++
				}
			}
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[CHARACTER_EVENT] && inputEvent.InputId < 128 {
		w.Content = w.Content[:w.CaretPosition] + string(byte(inputEvent.InputId)) + w.Content[w.CaretPosition:]
		ExternallyUpdated(&w.DepNode2Manual)
		w.CaretPosition++
	}
}

// ---

type MetaCharacter struct {
	Character byte
	Timestamp int64
}

func NewMetaCharacter(ch byte) MetaCharacter {
	return MetaCharacter{ch, time.Now().UnixNano()}
}

type MetaTextFieldWidget struct {
	Widget
	Content       []MetaCharacter
	CaretPosition uint32
}

func NewMetaTextFieldWidget(pos mathgl.Vec2d) *MetaTextFieldWidget {
	return &MetaTextFieldWidget{NewWidget(pos, mathgl.Vec2d{0, 0}), nil, 0}
}

func (w *MetaTextFieldWidget) Render() {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	// HACK: Setting the widget size in Render() is bad, because all the input calculations will fail before it's rendered
	if len(w.Content) < 3 {
		w.size[0] = float64(fontWidth * 3)
	} else {
		w.size[0] = float64(fontWidth * len(w.Content))
	}
	w.size[1] = fontHeight

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
		DrawYBox(w.pos, w.size)
	} else if (isHit && !mousePointer.State.IsActive()) || isOriginHit {
		DrawYBox(w.pos, w.size)
	} else if hasTypingFocus {
		DrawYBox(w.pos, w.size)
	} else {
		DrawNBox(w.pos, w.size)
	}

	now := time.Now().UnixNano()
	for i, mc := range w.Content {
		age := now - mc.Timestamp
		highlight := gl.Double(age) / 10000000000

		gl.Color3d(1, 1, highlight)
		gl.Rectd(gl.Double(w.pos[0]+float64(i*8)), gl.Double(w.pos[1]), gl.Double(w.pos[0]+float64(i+1)*8), gl.Double(w.pos[1]+16))

		gl.Color3d(0, 0, 0)
		PrintSegment(mathgl.Vec2d{w.pos[0] + float64(8*i), w.pos[1]}, string(mc.Character))
	}

	if hasTypingFocus {
		// Draw caret
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
		gl.Color3d(0, 0, 0)
		gl.Recti(gl.Int(w.CaretPosition*8-1), 0, gl.Int(w.CaretPosition*8+1), 16)
	}
}
func (w *MetaTextFieldWidget) Hit(ParentPosition mathgl.Vec2d) []Widgeter {
	if len(w.Widget.Hit(ParentPosition)) > 0 {
		return []Widgeter{w}
	} else {
		return nil
	}
}
func (w *MetaTextFieldWidget) ProcessEvent(inputEvent InputEvent) {
	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false &&
		inputEvent.Pointer.Mapping.ContainsWidget(w) && /* TODO: GetHoverer() */ // IsHit(this button) should be true
		inputEvent.Pointer.OriginMapping.ContainsWidget(w) { /* TODO: GetHoverer() */ // Make sure we're releasing pointer over same button that it originally went active on, and nothing is in the way (i.e. button is hoverer)

		// TODO: Request pointer mapping in a kinder way (rather than forcing it - what if it's active and shouldn't be changed)
		keyboardPointer.OriginMapping = []Widgeter{w}
	}

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	// Need to check if either button 0 is currently down, or was released. This is so that caret gets set at cursor pos when widget gains focus.
	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.Pointer.State.Button(0) || (inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0)) {
		if inputEvent.Pointer.State.Axes[0]-w.pos[0] < 0 {
			w.CaretPosition = 0
		} else if inputEvent.Pointer.State.Axes[0]-w.pos[0] > float64(len(w.Content)*8) {
			w.CaretPosition = uint32(len(w.Content))
		} else {
			w.CaretPosition = uint32((inputEvent.Pointer.State.Axes[0] - w.pos[0] + 4) / 8)
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
		switch glfw.Key(inputEvent.InputId) {
		case glfw.KeyBackspace:
			if w.CaretPosition >= 1 {
				w.CaretPosition--
				w.Content = append(w.Content[:w.CaretPosition], w.Content[w.CaretPosition+1:]...)
			}
		case glfw.KeyLeft:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.CaretPosition = 0
			} else if inputEvent.ModifierKey == 0 {
				if w.CaretPosition >= 1 {
					w.CaretPosition--
				}
			}
		case glfw.KeyRight:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.CaretPosition = uint32(len(w.Content))
			} else if inputEvent.ModifierKey == 0 {
				if w.CaretPosition < uint32(len(w.Content)) {
					w.CaretPosition++
				}
			}
		}
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[CHARACTER_EVENT] && inputEvent.InputId < 128 {
		//w.Content = append(append(w.Content[:w.CaretPosition], NewMetaCharacter(byte(inputEvent.InputId))), w.Content[w.CaretPosition:]...)
		w.Content = append(w.Content, MetaCharacter{})
		copy(w.Content[w.CaretPosition+1:], w.Content[w.CaretPosition:])
		w.Content[w.CaretPosition] = NewMetaCharacter(byte(inputEvent.InputId))
		w.CaretPosition++
	}
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
	Mapping         Widgeters
	OriginMapping   Widgeters
	State           PointerState
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

func ProcessInputEventQueue(widget Widgeter, inputEventQueue []InputEvent) []InputEvent {
	for len(inputEventQueue) > 0 {
		inputEvent := inputEventQueue[0]

		widget.ProcessEvent(inputEvent)

		if !katOnly {
			// TODO: Calculate whether a pointing pointer moved relative to canvas in a better way... what if canvas is moved via keyboard, etc.
			pointingPointerMovedRelativeToCanvas := inputEvent.Pointer.VirtualCategory == POINTING &&
				(inputEvent.EventTypes[AXIS_EVENT] && inputEvent.InputId == 0 || inputEvent.EventTypes[SLIDER_EVENT] && inputEvent.InputId == 2)

			if pointingPointerMovedRelativeToCanvas {
				LocalPosition := WidgeterS{widget}.GlobalToLocal(mathgl.Vec2d{float64(inputEvent.Pointer.State.Axes[0]), float64(inputEvent.Pointer.State.Axes[1])})

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

			// Populate PointerMappings (but only when pointer is moved while not active, and this isn't a deactivation since that's handled below)
			if pointingPointerMovedRelativeToCanvas &&
				!inputEvent.EventTypes[POINTER_DEACTIVATION] && !inputEvent.Pointer.State.IsActive() {

				inputEvent.Pointer.OriginMapping = make([]Widgeter, len(inputEvent.Pointer.Mapping))
				copy(inputEvent.Pointer.OriginMapping, inputEvent.Pointer.Mapping)
			}

			if inputEvent.Pointer == mousePointer && inputEvent.InputId == 0 && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] {
				//fmt.Println("Left down!")
			} else if inputEvent.Pointer == mousePointer && inputEvent.InputId == 1 && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] {
				//fmt.Println("Right down!")
			}

			for _, widget := range inputEvent.Pointer.OriginMapping {
				widget.ProcessEvent(inputEvent)
			}

			// Populate PointerMappings (but only upon pointer deactivation event)
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

func initHttpHandlers() {
	http.HandleFunc("/close", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintln(w, "Closing.")
		keepRunning = false
	})
	/*http.HandleFunc("/widgets", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(goon.SdumpExpr(len(widgets))))
		fmt.Fprintln(w)
		fmt.Fprintf(w, "%#v\n", widgets)
	})*/
	http.Handle("/favicon.ico", http.NotFoundHandler())
	http.Handle("/status/", http.StripPrefix("/status", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {

		// HACK: Handle .go files specially, just assume they're in "./GoLand"
		if strings.HasSuffix(r.URL.Path, ".go") {
			w.Header().Set("Content-Type", "text/plain")
			w.Write(MustReadFileB(filepath.Join("../../../", r.URL.Path)))
			return
		}

		_, plain := r.URL.Query()["plain"]
		switch plain {
		case true:
			w.Header().Set("Content-Type", "text/plain")
		case false:
			w.Header().Set("Content-Type", "text/html")
		}

		var b string

		importPath := r.URL.Path[1:]
		if goPackage := GoPackageFromImportPath(importPath); goPackage != nil {
			// TODO: Cache this via DepNode2I
			goPackage.UpdateVcs()
			goPackage.UpdateVcsFields()

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

			b += "```\n" + status.PorcelainPresenter(goPackage) + "\n```\n"

			b += "\n---\n\n"

			if goPackage.Vcs != nil {
				b += "```\n"
				if goPackage.Status == "" {
					b += "nothing to commit, working directory clean\n\n"
				} else {
					b += goPackage.Status + "\n"
				}
				b += "Branch: " + goPackage.LocalBranch + "\n"
				b += "Local:  " + goPackage.Local + "\n"
				b += "Remote: " + goPackage.Remote + "\n"
				b += "```\n"

				// git diff
				if goPackage.Status != "" {
					if goPackage.Vcs.Type() == vcs.Git {
						cmd := exec.Command("git", "diff", "--no-ext-diff")
						cmd.Dir = goPackage.Bpkg.Dir
						if outputBytes, err := cmd.CombinedOutput(); err == nil {
							b += "\n" /*+ `<a id="git-diff"></a>`*/ + Underline("git diff")
							b += "\n```diff\n" + string(outputBytes) + "\n```\n"
						}
					}
				}
			}

			b += "\n---\n\n"

			b += "```\n" + goPackage.Bpkg.Dir + "\n```\n"
			x := newFolderListingPureWidget(goPackage.Bpkg.Dir)
			for _, v := range x.entries {
				b += fmt.Sprintf("[%s](%s)  \n", v.Name(), path.Join("/status/", importPath, v.Name()))
			}
		} else {
			fmt.Fprintf(w, "Package %q not found in %q (are you sure it's a valid Go package; maybe its subdir).\n", importPath, os.Getenv("GOPATH"))
		}

		if plain {
			w.Write([]byte(b))
		} else {
			//w.Write(blackfriday.MarkdownCommon([]byte(b)))

			WriteGitHubFlavoredMarkdown(w, strings.NewReader(b))
		}
	})))
	http.Handle("/inline/", http.StripPrefix("/inline", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		io.WriteString(w, "importer api changed, exp11 needs to be updated")
		/*importPath := r.URL.Path[1:]

		// TODO: Cache this via DepNode2I
		buf := new(bytes.Buffer)
		buf.WriteString("```go\n")
		exp11.InlineDotImports(buf, importPath)
		buf.WriteString("\n```")

		WriteGitHubFlavoredMarkdown(w, buf)*/
	})))
}

func WriteGitHubFlavoredMarkdown(w io.Writer, body io.Reader) {
	// TODO: Don't hotlink the css file from github.com, serve it locally (it's needed for the GFM html to appear properly)
	io.WriteString(w, `<html><head><link href="https://github.com/assets/github.css" media="all" rel="stylesheet" type="text/css" /></head><body><article class="markdown-body entry-content" style="padding: 30px;">`)

	// TODO: Do this locally via a native Go library... That's not too much to ask for, is it?
	// Convert GitHub-Flavored-Markdown to HTML (includes syntax highlighting for diff, Go, etc.)
	resp, err := http.Post("https://api.github.com/markdown/raw", "text/x-markdown", body)
	CheckError(err)
	defer resp.Body.Close()
	_, err = io.Copy(w, resp.Body)
	CheckError(err)

	io.WriteString(w, `</article></body></html>`)
}

// ---

// HACK
type goPackageHardcoded struct {
	DepNode2Manual
}

func (*goPackageHardcoded) GetSelected() *ImportPathFound {
	x := NewImportPathFound("gist.github.com/7176504.git", "/Users/Dmitri/Dropbox/Work/2013/GoLand/")
	return &x
}

// ---

func main() {
	//defer profile.Start(profile.CPUProfile).Stop()
	//defer profile.Start(profile.MemProfile).Stop()

	startedProcess := time.Now()
	fmt.Printf("go version %s %s/%s\n", runtime.Version(), runtime.GOOS, runtime.GOARCH)
	runtime.GOMAXPROCS(runtime.NumCPU())
	flag.Parse()

	inputEventQueue := []InputEvent{}
	var window *glfw.Window

	if !*headlessFlag {
		runtime.LockOSThread()

		glfw.SetErrorCallback(func(err glfw.ErrorCode, desc string) {
			panic(fmt.Sprintf("glfw.ErrorCallback: %v: %v\n", err, desc))
		})

		// Verify the GLFW library and header versions match
		{
			major, minor, revision := glfw.GetVersion()
			match := (major == glfw.VersionMajor && minor == glfw.VersionMinor && revision == glfw.VersionRevision)
			if !match {
				panic("Error: GLFW library and header versions do not match.")
			}
		}
		if !glfw.Init() {
			panic("glfw.Init()")
		}
		defer glfw.Terminate()

		//glfw.WindowHint(glfw.Samples, 32) // Anti-aliasing
		//glfw.WindowHint(glfw.Decorated, glfw.False)
		var err error
		window, err = glfw.CreateWindow(1536, 960, "", nil, nil)
		globalWindow = window
		CheckError(err)
		window.MakeContextCurrent()

		err = gl.Init()
		if nil != err {
			log.Print(err)
		}
		fmt.Printf("glfw %d.%d.%d; %s %s %s\n", glfw.VersionMajor, glfw.VersionMinor, glfw.VersionRevision,
			gl.GoStringUb(gl.GetString(gl.VENDOR)), gl.GoStringUb(gl.GetString(gl.RENDERER)), gl.GoStringUb(gl.GetString(gl.VERSION)))

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
			gl.Viewport(0, 0, gl.Sizei(framebufferSize0), gl.Sizei(framebufferSize1))

			var windowSize [2]int
			windowSize[0], windowSize[1] = w.GetSize()

			// Update the projection matrix
			gl.MatrixMode(gl.PROJECTION)
			gl.LoadIdentity()
			gl.Ortho(0, gl.Double(windowSize[0]), gl.Double(windowSize[1]), 0, -1, 1)
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

		var lastMousePos mathgl.Vec2d
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

		window.SetCharacterCallback(func(w *glfw.Window, char uint) {
			// HACK: Ignore characters when Super key is held down
			if window.GetKey(glfw.KeyLeftSuper) != glfw.Release || window.GetKey(glfw.KeyRightSuper) != glfw.Release {
				return
			}

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

		gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA)
		//gl.ClearColor(0.8, 0.3, 0.01, 1)
		gl.ClearColor(0.85, 0.85, 0.85, 1)
	}

	// ---

	var widget Widgeter
	var widgets []Widgeter

	spinner := SpinnerWidget{Widget: NewWidget(mathgl.Vec2d{20, 20}, mathgl.Vec2d{0, 0}), Spinner: 0}

	if false {

		windowSize0, windowSize1 := window.GetSize()
		windowSize := mathgl.Vec2d{float64(windowSize0), float64(windowSize1)} // HACK: This is not updated as window resizes, etc.
		_ = windowSize

		goPackageListing := NewGoPackageListingPureWidget()
		widgets = append(widgets, NewScrollPaneWidget(np, mathgl.Vec2d{200, 600}, goPackageListing))

		folderListing := NewFolderListingWidget(np, "../../../") // Hopefully the "$GOPATH/src/" folder
		widgets = append(widgets, NewScrollPaneWidget(mathgl.Vec2d{0, 600 + 2}, mathgl.Vec2d{200, float64(windowSize1 - 602 - 2)}, folderListing))

		// TEST, HACK: Open the folder listing to the folder of the Go package
		folderListingDirChanger := DepNode2Func{}
		folderListingDirChanger.UpdateFunc = func(this DepNode2I) {
			if importPathFound := this.GetSources()[0].(*GoPackageListingPureWidget).GetSelected(); importPathFound != nil {
				path := importPathFound.FullPath()
				folderListing.flow.SetWidgets([]Widgeter{newFolderListingPureWidgetWithSelection(path)})
				ExternallyUpdated(folderListing)
			}
		}
		folderListingDirChanger.AddSources(goPackageListing)
		keepUpdatedTEST = append(keepUpdatedTEST, &folderListingDirChanger)

		// Main editor
		editorContent := NewMultilineContent()
		editorFileOpener := NewFileOpener(editorContent)
		editorFileOpener.AddSources(folderListing)
		keepUpdatedTEST = append(keepUpdatedTEST, editorFileOpener)
		editor := NewTextBoxWidgetExternalContent(np, editorContent)
		widgets = append(widgets, NewScrollPaneWidget(mathgl.Vec2d{200 + 2, 0}, mathgl.Vec2d{1000, float64(windowSize1 - 2)}, editor))

		// View sidebar
		{
			// git diff
			template := NewCmdTemplateDynamic2()
			template.UpdateFunc = func(this DepNode2I) {
				template.Template = NewCmdTemplate("echo", "-n", "No git diff available.")

				if path := this.GetSources()[0].(*FolderListingWidget).GetSelectedPath(); path != "" && strings.HasSuffix(path, ".go") {
					dir, file := filepath.Split(path)
					if isGitRepo, _ := vcs.IsFolderGitRepo(dir); isGitRepo { // TODO: Centralize this somewhere (GoPackage with DepNode2I?)
						template.Template = NewCmdTemplate("git", "diff", "--no-ext-diff", "--", file)
						template.Template.Dir = dir
					}
				}
			}
			template.AddSources(folderListing)

			gitDiff := NewLiveCmdExpeWidget(np, []DepNode2I{editorContent}, template) // TODO: Are both editorContent and folderListing deps needed? Or is editorContent enough, since it probably depends on folderListing, etc.
			gitDiffCollapsible := NewCollapsibleWidget(np, gitDiff)

			// ---

			/*nextTool := NewTextBoxWidgetExternalContent(np, NewMultilineContentDepDumper(goPackageListing))
			nextToolCollapsible := NewCollapsibleWidget(np, nextTool)*/

			// ---

			// go build
			template2 := NewCmdTemplateDynamic2()
			template2.UpdateFunc = func(this DepNode2I) {
				template2.Template = NewCmdTemplate("echo", "-n", "Nothing to go build.")

				if importPathFound := this.GetSources()[0].(*GoPackageListingPureWidget).GetSelected(); importPathFound != nil {
					template2.Template = NewCmdTemplate("go", "build", "-o", "./Con2RunBin", importPathFound.ImportPath())
					//template2.Template.Dir = ""
				}
			}
			template2.AddSources(goPackageListing)

			build := NewLiveCmdExpeWidget(np, []DepNode2I{editorContent}, template2)
			nextTool2Collapsible := NewCollapsibleWidget(np, build)

			// Go Compile Errors hardcoded TEST
			{
				goCompileErrorsTest := GoCompileErrorsTest{}
				goCompileErrorsTest.AddSources(build)
				goCompileErrorsManagerTest.AddSources(&goCompileErrorsTest)
			}

			// ---

			nextTool3, typeCheckedPackage := NewTest4Widget(np, goPackageListing, editor)
			nextTool3Collapsible := NewCollapsibleWidget(np, nextTool3)

			// ---

			// DEBUG: Goon widget of typeCheckedPackage
			nextTool4 := NewCompositeWidget(np, []Widgeter{NewGoonWidget(mathgl.Vec2d{20, 0}, typeCheckedPackage)})
			nextTool4Collapsible := NewCollapsibleWidget(np, nextTool4)

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
										if obj := typeCheckedPackage.info.Objects[Test4WidgetIdent]; obj != nil {
											if file := typeCheckedPackage.fset.File(obj.Pos()); file != nil {
												// TODO: Change folderListing selection if it's in a different file

												editor.CenterOnCaretPosition(uint32(file.Offset(obj.Pos())))
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

			nextTool5B := NewTest5BWidget(np, typeCheckedPackage)
			nextTool5BCollapsible := NewCollapsibleWidget(np, nextTool5B)

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

							editor.CenterOnCaretPosition(uint32(file.Offset(declNode.Pos())))
						}
					}
				}
			}
			scrollToSymbolB.AddSources(nextTool5B.OnSelectionChanged())
			keepUpdatedTEST = append(keepUpdatedTEST, &scrollToSymbolB)

			// =====

			tools := NewFlowLayoutWidget(np, []Widgeter{nextTool2Collapsible, gitDiffCollapsible, nextTool3Collapsible, nextTool4Collapsible, nextTool5BCollapsible}, &FlowLayoutWidgetOptions{FlowLayoutType: VerticalLayout})
			widgets = append(widgets, NewScrollPaneWidget(mathgl.Vec2d{1200 + 4, 0}, mathgl.Vec2d{330, float64(windowSize1 - 2)}, tools))
			//widgets = append(widgets, NewLiveCmdExpeWidget(mathgl.Vec2d{1200 + 4, 0}, []DepNode2I{folderListing}, template))
		}

	} else if false { // Deleted test widget instances
		widgets = append(widgets, &BoxWidget{NewWidget(mathgl.Vec2d{50, 150}, mathgl.Vec2d{16, 16}), "The Original Box"})
		widgets = append(widgets, NewCompositeWidget(mathgl.Vec2d{150, 150},
			[]Widgeter{
				&BoxWidget{NewWidget(mathgl.Vec2d{0, 0}, mathgl.Vec2d{16, 16}), "Left of Duo"},
				&BoxWidget{NewWidget(mathgl.Vec2d{16 + 2, 0}, mathgl.Vec2d{16, 16}), "Right of Duo"},
			}))
		widgets = append(widgets, &UnderscoreSepToCamelCaseWidget{NewWidget(mathgl.Vec2d{50, 180}, mathgl.Vec2d{0, 0}), window})
		widgets = append(widgets, NewTextFieldWidget(mathgl.Vec2d{50, 50}))
		widgets = append(widgets, NewMetaTextFieldWidget(mathgl.Vec2d{50, 70}))
		widgets = append(widgets, NewChannelExpeWidget(mathgl.Vec2d{10, 220}))
		widgets = append(widgets, NewTextBoxWidget(mathgl.Vec2d{50, 5}))
		widgets = append(widgets, NewTextFileWidget(mathgl.Vec2d{90, 25}, "/Users/Dmitri/Dropbox/Needs Processing/Sample.txt"))
		widgets = append(widgets, NewTextBoxWidgetExternalContent(mathgl.Vec2d{90, 60}, widgets[len(widgets)-1].(*TextFileWidget).TextBoxWidget.Content))   // HACK: Manual test
		widgets = append(widgets, NewTextLabelWidgetExternalContent(mathgl.Vec2d{90, 95}, widgets[len(widgets)-2].(*TextFileWidget).TextBoxWidget.Content)) // HACK: Manual test

		if false {
			contentFunc := func() string { return TrimLastNewline(goon.Sdump(widgets[7])) }
			widgets = append(widgets, NewTextBoxWidgetContentFunc(mathgl.Vec2d{390, -1525}, contentFunc, []DepNodeI{&UniversalClock}))
		}
		widgets = append(widgets, NewTest2Widget(mathgl.Vec2d{240, 5}, &widgets[7].(*TextBoxWidget).pos[0]))

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
		widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{380, 10}, &x))
		y := NewWidget(mathgl.Vec2d{1, 2}, mathgl.Vec2d{3})
		widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{600, 10}, &y))
	} else if true {
		widgets = append(widgets, &spinner)

		widgets = append(widgets, NewKatWidget(mathgl.Vec2d{370, 15}))

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

			//run := NewLiveCmdExpeWidget(np, []DepNodeI{&build.FinishedDepNode}, NewCmdTemplate("./Con2RunBin")) // TODO: Proper path

			widgets = append(widgets, NewFlowLayoutWidget(mathgl.Vec2d{50, 200}, []Widgeter{src, build /*, run*/}, nil))
		}

		// DEBUG: Testing out new DepNode2 system
		{
			// TODO: Use DepNode2 so that if this is false, then goCompileErrorsManagerTest.All (and goCompileErrorsTest also) shouldn't get updated
			goCompileErrorsEnabled := true
			goCompileErrorsEnabledTest = NewTriButtonExternalStateWidget(mathgl.Vec2d{500, 700}, func() bool { return goCompileErrorsEnabled }, func() { goCompileErrorsEnabled = !goCompileErrorsEnabled })
			widgets = append(widgets, goCompileErrorsEnabledTest)

			//widgets = append(widgets, NewTextLabelWidgetGoon(mathgl.Vec2d{500, 716 + 2}, &goCompileErrorsManagerTest.DepNode2.NeedToUpdate))
			widgets = append(widgets, NewTextLabelWidgetGoon(mathgl.Vec2d{500, 732 + 4}, &goCompileErrorsManagerTest.All))
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
			dst := NewLiveGoroutineExpeWidget(np, []DepNode2I{src.Content}, params, action)

			w := NewFlowLayoutWidget(mathgl.Vec2d{80, 130}, []Widgeter{src, label, dst}, nil)
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
					cmd := exec.Command("goe", "--quiet", "fmt", "gist.github.com/4727543.git", "gist.github.com/5498057.git", "Print(GetForcedUseFromImport(ReadAllStdin()))")
					//cmd := exec.Command("cat")
					cmd.Stdin = strings.NewReader(strings.TrimSpace(src.Content.Content()))
					out, err := cmd.CombinedOutput()
					CheckError(err)
					return string(out)
				} else {
					return ""
				}
			}
			dst := NewLiveGoroutineExpeWidget(np, []DepNode2I{src.Content}, params, action)

			w := NewFlowLayoutWidget(mathgl.Vec2d{80, 150}, []Widgeter{src, label, dst}, nil)
			widgets = append(widgets, w)
		}

		// git diff
		if false {
			source := widgets[2].(*FlowLayoutWidget).Widgets[0].(*TextFileWidget)
			dir, file := filepath.Split(source.Path())
			if isGitRepo, _ := vcs.IsFolderGitRepo(dir); isGitRepo { // TODO: Centralize this somewhere (GoPackage with DepNode2I?)
				template := NewCmdTemplate("git", "diff", "--no-ext-diff", "--", file)
				template.Dir = dir
				w := NewLiveCmdExpeWidget(np, []DepNode2I{source.Content}, template)

				widgets[2].(*FlowLayoutWidget).Widgets = append(widgets[2].(*FlowLayoutWidget).Widgets, w)
				w.SetParent(widgets[2]) // Needed for pointer coordinates to be accurate
				widgets[2].(*FlowLayoutWidget).Layout()
			}
		}

		// TEST: Render the Go code tokens with color highlighting
		{
			source := widgets[2].(*FlowLayoutWidget).Widgets[0].(*TextFileWidget).TextBoxWidget

			w := &CustomWidget{
				Widget: NewWidget(np, mathgl.Vec2d{fontHeight, fontHeight}),
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
						gl.Color3d(0, 0.706, 0.094)
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

		widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{510, 70}, &widgets))
		widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{510, 100}, &keyboardPointer))
		widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{510, 130}, &mousePointer))

		widgets = append(widgets, NewFolderListingWidget(mathgl.Vec2d{350, 30}, "../../../")) // Hopefully the "$GOPATH/src/" folder

		//widgets = append(widgets, NewCompositeWidget(mathgl.Vec2d{160, 30}, []Widgeter{NewGoPackageListingPureWidget()}))

		contentWs := NewMultilineContent()
		widgets = append(widgets, NewTextBoxWidgetExternalContent(mathgl.Vec2d{800 - 50, 30}, contentWs))
		http.HandleFunc("/websocket", func(w http.ResponseWriter, r *http.Request) {
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

		// Shuryear Clock
		{
			contentFunc := func() string {
				shuryearNow := 1970 + float64(time.Now().UnixNano())/(3600*24*3652422*100000)
				return fmt.Sprintf("%.8f", shuryearNow)
			}
			mc := NewMultilineContentFunc(contentFunc, []DepNodeI{&UniversalClock})
			widgets = append(widgets, NewTextLabelWidgetExternalContent(mathgl.Vec2d{1443, 0}, mc)) // TODO: Stick to top right corner?
		}

		{
			buttonTrigger := NewButtonTriggerWidget(mathgl.Vec2d{50, 30})
			spinner.AddSources(buttonTrigger)
			widgets = append(widgets, buttonTrigger)
		}

		// `gofmt -r rule` experiment
		{
			in := NewTextBoxWidget(np)

			validFunc := func(c MultilineContentI) bool {
				_, err := parser.ParseExpr(c.Content())
				return err == nil
			}
			from := NewTextBoxValidationWidget(np, validFunc)
			to := NewTextBoxValidationWidget(np, validFunc)

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

			nameArgs := StringsFunc(func() []string {
				out := []string{"gofmt"}
				if from.IsValidTEST() && to.IsValidTEST() {
					out = append(out, "-r", fmt.Sprintf("%s -> %s", from.Content.Content(), to.Content.Content()))
				}
				return out
			})
			template := NewCmdTemplateDynamic(nameArgs)
			template.Stdin = func() io.Reader { return NewContentReader(in.Content) } // This is not a race condition only because template.NewCommand() gets called from same thread that updates in.Content.

			debugOutput := func() string {
				cmd := template.NewCommand()
				return fmt.Sprintf("%#v", cmd.Args)
			}
			out := NewTextLabelWidgetContentFunc(np, debugOutput, []DepNodeI{&UniversalClock})

			//out.AddChangeListener(ChangeListenerFunc(func() {
			/*refresh := NewButtonWidget(np, func() {
				dmp := diffmatchpatch.New()
				diffs := dmp.DiffMain(in.Content.Content(), out.Content.Content(), true)
				in.DiffsTest = diffs
				in.Side = -1
				out.DiffsTest = diffs
				out.Side = +1
				//contentWs.Set(goon.Sdump(dmp.DiffMain(in.Content.Content(), out.Content.Content(), true)))
				out.Content.Set(out.Content.Content() + goon.SdumpExpr(template.NewCommand().Args))
			})*/

			widgets = append(widgets, NewFlowLayoutWidget(mathgl.Vec2d{800, 10}, []Widgeter{in, NewTextLabelWidgetString(np, "gofmt -r "), from, NewTextLabelWidgetString(np, " -> "), to, out}, nil))
		}

		// +Gist Button
		{
			username := NewTextBoxWidget(np)
			password := NewTextBoxWidget(np)
			password.Private.Set(true)

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
			output := NewLiveGoroutineExpeWidget(np, []DepNode2I{gistButtonTrigger}, params, action)

			widgets = append(widgets, NewFlowLayoutWidget(mathgl.Vec2d{500, 10}, []Widgeter{username, password, NewTextLabelWidgetString(np, "+Gist"), gistButtonTrigger, output}, nil))
		}

		// Sample Window widget
		{
			newWindowButton := NewButtonWidget(np, func() {
				w := NewWindowWidget(mathgl.Vec2d{500, 500}, mathgl.Vec2d{200, 140})
				w.Name = "New Window"

				// Add new widget to canvas
				widget.(AddWidgeter).AddWidget(w)
			})
			widgets = append(widgets, newWindowButton)

			w := NewWindowWidget(mathgl.Vec2d{1000, 40}, mathgl.Vec2d{200, 140})
			w.Name = "Sample Window"
			widgets = append(widgets, w)
		}

	} else if false {
		widgets = append(widgets, NewGpcFileWidget(mathgl.Vec2d{1100, 500}, "/Users/Dmitri/Dropbox/Work/2013/eX0 Project/eX0 Client/levels/test3.wwl"))
		widgets = append(widgets, NewTest1Widget(mathgl.Vec2d{10, 50}))
		widgets = append(widgets, NewKatWidget(mathgl.Vec2d{370, 20}))
	}

	fpsWidget := NewFpsWidget(mathgl.Vec2d{10, 120})
	widgets = append(widgets, fpsWidget)
	// NumGoroutines
	{
		contentFunc := func() string { return fmt.Sprint(runtime.NumGoroutine()) }
		mc := NewMultilineContentFunc(contentFunc, []DepNodeI{&UniversalClock})
		widgets = append(widgets, NewTextLabelWidgetExternalContent(mathgl.Vec2d{10, 40}, mc))
	}

	// Http Server
	initHttpHandlers()
	widgets = append(widgets, NewHttpServerTestWidget(mathgl.Vec2d{10, 130}))

	//widget = NewCanvasWidget(mathgl.Vec2d{1, 1}, widgets, nil)
	//widget := NewFlowLayoutWidget(mathgl.Vec2d{1, 1}, widgets, nil)
	widget = NewCompositeWidget(mathgl.Vec2d{1, 1}, widgets)

	fmt.Printf("Loaded in %v ms.\n", time.Since(startedProcess).Seconds()*1000)

	// ---

	for keepRunning {
		frameStartTime := time.Now()

		if !*headlessFlag {
			//glfw.WaitEvents()
			glfw.PollEvents()

			// HACK: Close window check
			if glfw.Release != window.GetKey(glfw.KeyEscape) {
				keepRunning = false
				break
			}
		}

		// Input
		inputEventQueue = ProcessInputEventQueue(widget, inputEventQueue)

		UniversalClock.TimePassed = 1.0 / 60 // TODO: Use proper value?
		UniversalClock.NotifyAllListeners()

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

		if redraw && !*headlessFlag {
			gl.Clear(gl.COLOR_BUFFER_BIT)
			gl.LoadIdentity()

			widget.Render()

			//spinner.NotifyChange()
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

	goon.DumpExpr(os.Remove("./Con2RunBin")) // TODO: Generalize this
}
