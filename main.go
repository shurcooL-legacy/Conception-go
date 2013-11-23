package main

import (
	"fmt"
	. "gist.github.com/5286084.git"
	"log"
	"runtime"
	"strconv"
	"strings"
	"time"

	_ "github.com/ftrvxmtrx/tga"
	"image"
	//_ "image/png"
	"os"

	//"github.com/go-gl/gl"
	gl "github.com/chsc/gogl/gl21"
	glfw "github.com/go-gl/glfw3"
	//"github.com/go-gl/glu"

	"github.com/Jragonmiris/mathgl"

	"github.com/shurcooL/go-goon"

	. "gist.github.com/6003701.git"

	. "gist.github.com/5258650.git"
	. "gist.github.com/6096872.git"
	"os/exec"

	. "gist.github.com/5571468.git"

	//"go/parser"
	//"go/token"

	"math"

	. "gist.github.com/5504644.git"
	. "gist.github.com/5639599.git"
	//"io/ioutil"
	//"runtime/debug"
	. "gist.github.com/5259939.git"
	. "gist.github.com/6418462.git"

	"errors"

	"io/ioutil"

	. "gist.github.com/5892738.git"
	. "gist.github.com/6418290.git"
	. "gist.github.com/6545684.git"

	. "gist.github.com/4727543.git"

	. "gist.github.com/6445065.git"
	"go/ast"
	"go/parser"
	"go/token"

	"reflect"

	. "gist.github.com/6724654.git"

	"code.google.com/p/go.tools/go/exact"
	"code.google.com/p/go.tools/go/types"
	"code.google.com/p/go.tools/importer"
	. "gist.github.com/7576804.git"
	importer2 "honnef.co/go/importer"

	"github.com/davecheney/profile"

	. "gist.github.com/5953185.git"
	"path/filepath"

	"flag"
	. "gist.github.com/7390843.git"
	"github.com/russross/blackfriday"
	"net/http"
	_ "net/http/pprof"

	. "gist.github.com/7480523.git"
	//. "gist.github.com/7519227.git"

	. "gist.github.com/7576154.git"
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
var oFontBase gl.Uint
var redraw bool = true
var widgets []Widgeter
var mousePointer *Pointer
var keyboardPointer *Pointer

// TODO: Remove these
var globalWindow *glfw.Window

func CheckGLError() {
	errorCode := gl.GetError()
	if 0 != errorCode {
		log.Panic("GL Error: ", errorCode)
	}
}

func PrintText(pos mathgl.Vec2d, s string) {
	lines := GetLines(s)
	for lineNumber, line := range lines {
		PrintLine(pos.Add(mathgl.Vec2d{0, float64(16 * lineNumber)}), line)
	}
}

// Input shouldn't have newlines
func PrintLine(pos mathgl.Vec2d, s string) {
	segments := strings.Split(s, "\t")
	var advance uint32
	for _, segment := range segments {
		PrintSegment(mathgl.Vec2d{pos[0] + float64(8*advance), pos[1]}, segment)
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
	gl.Translated(gl.Double(pos[0]), gl.Double(pos[1]), 0)
	gl.Translated(-4+0.25, -1, 0)
	gl.ListBase(oFontBase)
	gl.CallLists(gl.Sizei(len(s)), gl.UNSIGNED_BYTE, gl.Pointer(&[]byte(s)[0]))
	gl.PopMatrix()

	CheckGLError()
}

func InitFont() {
	const fontWidth = 8

	LoadTexture("./data/Font2048.tga")

	oFontBase = gl.GenLists(256)

	for iLoop1 := 0; iLoop1 < 256; iLoop1++ {
		fCharX := gl.Double(iLoop1%16) / 16.0
		fCharY := gl.Double(iLoop1/16) / 16.0

		gl.NewList(oFontBase+gl.Uint(iLoop1), gl.COMPILE)
		const offset = gl.Double(0.004)
		//#if DECISION_RENDER_TEXT_VCENTERED_MID
		VerticalOffset := gl.Double(0.00125)
		if ('a' <= iLoop1 && iLoop1 <= 'z') || '_' == iLoop1 {
			VerticalOffset = gl.Double(-0.00225)
		}
		/*#else
		VerticalOffset := gl.Double(0.0)
		//#endif*/
		gl.Begin(gl.QUADS)
		gl.TexCoord2d(fCharX+offset, 1-(1-fCharY-0.0625+offset+VerticalOffset))
		gl.Vertex2i(0, 16)
		gl.TexCoord2d(fCharX+0.0625-offset, 1-(1-fCharY-0.0625+offset+VerticalOffset))
		gl.Vertex2i(16, 16)
		gl.TexCoord2d(fCharX+0.0625-offset, 1-(1-fCharY-offset+VerticalOffset))
		gl.Vertex2i(16, 0)
		gl.TexCoord2d(fCharX+offset, 1-(1-fCharY-offset+VerticalOffset))
		gl.Vertex2i(0, 0)
		gl.End()
		gl.Translated(fontWidth, 0.0, 0.0)
		gl.EndList()
	}

	CheckGLError()
}

func DeinitFont() {
	gl.DeleteLists(oFontBase, 256)
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
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_LOD_BIAS, -0.65)
	gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.Sizei(bounds.Dx()), gl.Sizei(bounds.Dy()), 0, gl.RGBA, gl.UNSIGNED_BYTE, gl.Pointer(pixPointer))
	CheckGLError()
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

	Pos() mathgl.Vec2d
	SetPos(mathgl.Vec2d)
	Size() mathgl.Vec2d
	SetSize(mathgl.Vec2d)
	HoverPointers() map[*Pointer]bool
	Parent() Widgeter
	SetParent(Widgeter)

	ParentToLocal(mathgl.Vec2d) mathgl.Vec2d

	AddChangeListener(l ChangeListener)
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

func (w *Widget) Pos() mathgl.Vec2d         { return w.pos }
func (w *Widget) SetPos(pos mathgl.Vec2d)   { w.pos = pos }
func (w *Widget) Size() mathgl.Vec2d        { return w.size }
func (w *Widget) SetSize(size mathgl.Vec2d) { w.size = size }

func (w *Widget) HoverPointers() map[*Pointer]bool {
	return w.hoverPointers
}

func (w *Widget) Parent() Widgeter     { return w.parent }
func (w *Widget) SetParent(p Widgeter) { w.parent = p }

func (w *Widget) ParentToLocal(ParentPosition mathgl.Vec2d) (LocalPosition mathgl.Vec2d) {
	return ParentPosition.Sub(w.pos)
}

func GlobalToParent(w Widgeter, GlobalPosition mathgl.Vec2d) (ParentPosition mathgl.Vec2d) {
	switch w.Parent() {
	case nil:
		ParentPosition = GlobalPosition
	default:
		ParentPosition = GlobalToLocal(w.Parent(), GlobalPosition)
	}
	return ParentPosition
}
func GlobalToLocal(w Widgeter, GlobalPosition mathgl.Vec2d) (LocalPosition mathgl.Vec2d) {
	return w.ParentToLocal(GlobalToParent(w, GlobalPosition))
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
	for lineNumber, file := range files {
		if file.IsDir() {
			PrintText(w.pos.Add(mathgl.Vec2d{0, float64(16 * lineNumber)}), ">>>> " + file.Name() + "/ (FOLDER)")
		} else {
			PrintText(w.pos.Add(mathgl.Vec2d{0, float64(16 * lineNumber)}), file.Name())
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
	for lineNumber, y := range x.Vars {
		PrintText(w.pos.Add(mathgl.Vec2d{0, float64(16 * lineNumber)}), SprintAstBare(y.Decl))
	}*/

	kat := widgets[len(widgets)-2].(*KatWidget)
	PrintText(w.pos, fmt.Sprintf("%d %s", kat.mode, kat.mode.String()))
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
	source MultilineContentI // THINK: I only need this because NotifyChange doesn't tell where the change comes from, maybe it should?

	fs      *token.FileSet
	fileAst *ast.File

	DepNode
}

func (t *parsedFile) NotifyChange() {
	fs := token.NewFileSet()
	fileAst, err := parser.ParseFile(fs, "", t.source.Content(), 1*parser.ParseComments)

	{
		fileAst.Decls[0].(*ast.GenDecl).Specs = append(fileAst.Decls[0].(*ast.GenDecl).Specs, &ast.ImportSpec{Path: &ast.BasicLit{Kind: token.STRING, Value: `"yay/new/import"`}})
	}

	if err == nil {
		t.fs = fs
		t.fileAst = fileAst
	} else {
		t.fs = nil
		t.fileAst = nil
	}

	t.NotifyAllListeners()
}

func NewTest3Widget(pos mathgl.Vec2d, source *TextBoxWidget) *LiveGoroutineExpeWidget {
	parsedFile := &parsedFile{source: source.Content}
	source.Content.AddChangeListener(parsedFile)

	params := func() interface{} {
		return []interface{}{
			source.caretPosition.Logical(),
			parsedFile.fs,
			parsedFile.fileAst,
		}
	}

	action := func(params interface{}) string {
		index := params.([]interface{})[0].(uint32)
		fs := params.([]interface{})[1].(*token.FileSet)
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
		out += SprintAst(fs, smallestV) + "\n\n"

		// This is can be huge if ran on root AST node of large Go files, so don't
		//if _, huge := smallestV.(*ast.File); !huge {
		out += goon.Sdump(smallestV)
		//}
		return out
	}

	w := NewLiveGoroutineExpeWidget(pos, []DepNodeI{parsedFile, &source.caretPosition}, params, action)
	return w
}

// ---

type typeCheckedPackage struct {
	source *MultilineContentFile

	fset  *token.FileSet
	files []*ast.File

	tpkg *types.Package
	info *types.Info

	DepNode
}

func (t *typeCheckedPackage) NotifyChange() {
	ImportPath := "gist.github.com/7176504.git"
	//ImportPath := "gist.github.com/5694308.git"

	bpkg, err := BuildPackageFromImportPath(ImportPath)
	if err != nil {
		t.fset = nil
		t.files = nil
		t.tpkg = nil
		t.info = nil
		return
	}

	fset := token.NewFileSet()
	files, err := importer.ParseFiles(fset, bpkg.Dir, append(bpkg.GoFiles, bpkg.CgoFiles...)...)
	if err != nil {
		t.fset = nil
		t.files = nil
		t.tpkg = nil
		t.info = nil
		return
	}

	t.fset = fset
	t.files = files

	imp := importer2.New()
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
	tpkg, err := cfg.Check(ImportPath, fset, files, info)
	if err == nil {
		t.tpkg = tpkg
		t.info = info
	} else {
		t.tpkg = nil
		t.info = nil
	}

	t.NotifyAllListeners()
}

func NewTest4Widget(pos mathgl.Vec2d, source *TextFileWidget) *LiveGoroutineExpeWidget {
	typeCheckedPackage := &typeCheckedPackage{source: source.Content.(*MultilineContentFile)}
	source.Content.AddChangeListener(typeCheckedPackage)

	params := func() interface{} {
		return []interface{}{
			source.caretPosition.Logical(),
			typeCheckedPackage.fset,
			typeCheckedPackage.files,
			typeCheckedPackage.info,
		}
	}

	action := func(params interface{}) string {
		index := params.([]interface{})[0].(uint32)
		fs := params.([]interface{})[1].(*token.FileSet)
		files := params.([]interface{})[2].([]*ast.File)
		//tpkg := typeCheckedPackage.tpkg
		info := params.([]interface{})[3].(*types.Info)

		if len(files) == 0 {
			return ""
		}
		fileAst := files[0] // HACK: Use first file...

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
		out := ""
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
		out += SprintAst(fs, smallestV) + "\n\n"

		if ident, ok := smallestV.(*ast.Ident); ok {
			if info != nil && info.Objects[ident] != nil {
				obj := info.Objects[ident]
				out += TypeChainString(obj.Type())
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
			out += goon.Sdump(smallestV)
		}
		return out
	}

	w := NewLiveGoroutineExpeWidget(pos, []DepNodeI{typeCheckedPackage, &source.caretPosition}, params, action)
	return w
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

type ButtonWidget struct {
	Widget
	action  func()
	tooltip Widgeter
}

func NewButtonWidget(pos mathgl.Vec2d, action func()) *ButtonWidget {
	w := &ButtonWidget{Widget: NewWidget(pos, mathgl.Vec2d{16, 16})}
	w.setAction(action)

	return w
}

func NewButtonTriggerWidget(pos mathgl.Vec2d) *ButtonWidget {
	w := &ButtonWidget{Widget: NewWidget(pos, mathgl.Vec2d{16, 16})}
	w.setAction(func() { w.NotifyAllListeners() })

	return w
}

func (w *ButtonWidget) setAction(action func()) {
	w.action = action

	if action != nil {
		go func() { w.tooltip = NewTextLabelWidgetString(mathgl.Vec2d{}, GetSourceAsString(action)) }()
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
		mousePointerPositionLocal := GlobalToLocal(w, mathgl.Vec2d{mousePointer.State.Axes[0], mousePointer.State.Axes[1]})
		tooltipOffset := mathgl.Vec2d{0, 16}
		w.tooltip.SetPos(w.pos.Add(mousePointerPositionLocal).Add(tooltipOffset))
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
	gl.Color3d(0, 0, 0)
	if !w.state {
		gl.Begin(gl.TRIANGLES)
		gl.Vertex2d(gl.Double(w.pos[0]), gl.Double(w.pos[1]))
		gl.Vertex2d(gl.Double(w.pos[0]+w.size[0]), gl.Double(w.pos[1]+w.size[1]/2))
		gl.Vertex2d(gl.Double(w.pos[0]), gl.Double(w.pos[1]+w.size[1]))
		gl.End()
	} else {
		gl.Begin(gl.TRIANGLES)
		gl.Vertex2d(gl.Double(w.pos[0]), gl.Double(w.pos[1]))
		gl.Vertex2d(gl.Double(w.pos[0]+w.size[0]), gl.Double(w.pos[1]))
		gl.Vertex2d(gl.Double(w.pos[0]+w.size[0]/2), gl.Double(w.pos[1]+w.size[1]))
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

var boxWidgetTooltip = NewTextLabelWidgetString(mathgl.Vec2d{}, GetSourceAsString((*BoxWidget).ProcessEvent))

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
		mousePointerPositionLocal := GlobalToLocal(w, mathgl.Vec2d{mousePointer.State.Axes[0], mousePointer.State.Axes[1]})
		tooltipOffset := mathgl.Vec2d{0, -4 - boxWidgetTooltip.Size()[1]}
		boxWidgetTooltip.SetPos(w.pos.Add(mousePointerPositionLocal).Add(tooltipOffset))
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

func (widgets *Widgeters) ContainsWidget(targetWidget Widgeter) bool {
	for _, widget := range mousePointer.Mapping {
		if widget == targetWidget {
			return true
		}
	}
	return false
}

// ---

func DrawBorderlessBox(pos, size mathgl.Vec2d, backgroundColor mathgl.Vec3d) {
	gl.Color3dv((*gl.Double)(&backgroundColor[0]))
	gl.Rectd(gl.Double(pos[0]), gl.Double(pos[1]), gl.Double(pos.Add(size)[0]), gl.Double(pos.Add(size)[1]))
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
	DrawBox(pos, size, mathgl.Vec3d{0.898, 0.765, 0.396}, mathgl.Vec3d{1, 1, 1})
}
func DrawGBox(pos, size mathgl.Vec2d) {
	DrawBox(pos, size, mathgl.Vec3d{0.898, 0.765, 0.396}, mathgl.Vec3d{0.75, 0.75, 0.75})
}
func DrawLGBox(pos, size mathgl.Vec2d) {
	DrawBox(pos, size, mathgl.Vec3d{0.6, 0.6, 0.6}, mathgl.Vec3d{0.95, 0.95, 0.95})
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
	x := GetDocPackageAll(BuildPackageFromSrcDir(GetThisGoSourceDir()))
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
		DrawCircle(w.pos, w.size, mathgl.Vec3d{0.898, 0.765, 0.396}, mathgl.Vec3d{1, 1, 1})
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

		DrawCircleBorderCustom(mathgl.Vec2d{}, mathgl.Vec2d{16, 16}, mathgl.Vec3d{1, 0, 0}, 2, 12, 1, 11)

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
		w.target = GlobalToParent(w, mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]})
		w.skillActive = true
	}

	if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.Pointer.State.Button(1) {
		pointerPos := GlobalToParent(w, mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]})
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

type CompositeWidget struct {
	Widget
	Widgets []Widgeter
}

func NewCompositeWidget(pos, size mathgl.Vec2d, Widgets []Widgeter) *CompositeWidget {
	w := &CompositeWidget{Widget: NewWidget(pos, size), Widgets: Widgets}
	for _, widget := range w.Widgets {
		widget.SetParent(w)
	}
	w.Layout() // TODO: Should this be automatic from above SetParent()?
	return w
}

func (w *CompositeWidget) Layout() {
	w.size = mathgl.Vec2d{}
	for _, widget := range w.Widgets {
		bottomRight := widget.Pos().Add(widget.Size())
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
	w := &FlowLayoutWidget{CompositeWidget: *NewCompositeWidget(pos, mathgl.Vec2d{0, 0}, Widgets), options: *options}
	// TODO: Unsure this is a good way of registering ourselves as a listener... can it be more automated? What if someone manually adds another widget later, this'd get bypassed...
	/*for _, widget := range Widgets {
		widget.AddChangeListener(w)
	}*/
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

func (w *FlowLayoutWidget) Layout() {
	w.size = mathgl.Vec2d{}
	var combinedOffset float64
	for _, widget := range w.CompositeWidget.Widgets {
		pos := mathgl.Vec2d{}
		pos[w.options.FlowLayoutType] = combinedOffset
		widget.SetPos(pos)
		combinedOffset += widget.Size()[w.options.FlowLayoutType] + 2

		bottomRight := widget.Pos().Add(widget.Size())
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
	w := &CanvasWidget{CompositeWidget: *NewCompositeWidget(pos, mathgl.Vec2d{0, 0}, Widgets), options: *options}
	// TODO: This is a hack, I'm manually overriding parents of each widget that were set in NewCompositeWidget()
	for _, widget := range w.Widgets {
		widget.SetParent(w)
	}
	return w
}

func (w *CanvasWidget) Layout() {}

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
	w.CompositeWidget = NewCompositeWidget(pos, mathgl.Vec2d{},
		[]Widgeter{
			NewTextBoxWidget(mathgl.Vec2d{0, 0}),
			NewTriButtonExternalStateWidget(mathgl.Vec2d{0, -16 - 2}, func() bool { return w.cmd != nil }, action),
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
			box.Content.Set(box.Content.Content() + string(b))
			redraw = true
		}
	default:
	}
}

// ---

type commandNode struct {
	w        *LiveCmdExpeWidget
	template CmdTemplate
	dst      *TextBoxWidget
}

func (this *commandNode) NotifyChange() {
	if this.w.cmd != nil && this.w.cmd.ProcessState == nil {
		//w.cmd.Process.Kill()
		this.w.cmd.Process.Signal(os.Interrupt)
		//w.cmd.Process.Signal(syscall.SIGTERM)
		//fmt.Println("sigint'ed process", this.w.cmd.Process.Pid)
		this.w.cmd = nil
	}

	this.w.Content.Set("")

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
	//fmt.Println("started new process", this.w.cmd.Process.Pid)

	go func(cmd *exec.Cmd) {
		_ = cmd.Wait()
		//fmt.Println("waited til end of", cmd.Process.Pid)
		this.w.finishedChan <- cmd.ProcessState
	}(this.w.cmd)
}

type LiveCmdExpeWidget struct {
	*TextBoxWidget
	cmd             *exec.Cmd
	stdoutChan      ChanWriter
	stderrChan      ChanWriter
	finishedChan    chan *os.ProcessState
	FinishedDepNode DepNode
}

func NewLiveCmdExpeWidget(pos mathgl.Vec2d, dependees []DepNodeI, template CmdTemplate) *LiveCmdExpeWidget {
	w := &LiveCmdExpeWidget{TextBoxWidget: NewTextBoxWidget(pos), finishedChan: make(chan *os.ProcessState)}

	UniversalClock.AddChangeListener(w)

	// THINK: The only reason to have a separate command node is because current NotifyChange() does not tell the originator of change, so I can't tell UniversalClock's changes from dependee changes (and I need to do different actions for each)
	commandNode := &commandNode{w: w, template: template}
	for _, dependee := range dependees {
		dependee.AddChangeListener(commandNode)
	}

	return w
}

func (w *LiveCmdExpeWidget) NotifyChange() {
	select {
	case b, ok := <-w.stdoutChan:
		if ok {
			w.Content.Set(w.Content.Content() + string(b))
			redraw = true
		}
	default:
	}

	select {
	case b, ok := <-w.stderrChan:
		if ok {
			w.Content.Set(w.Content.Content() + string(b))
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
		w.FinishedDepNode.NotifyAllListeners()
	default:
	}
}

// ---

type actionNode struct {
	owner  *LiveGoroutineExpeWidget
	params func() interface{}
	action func(interface{}) string
}

func (this *actionNode) NotifyChange() {
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
}

type timestampString struct {
	s string
	t uint32
}

type LiveGoroutineExpeWidget struct {
	*TextBoxWidget
	outChan                     chan timestampString
	lastStartedT, lastFinishedT uint32
}

func NewLiveGoroutineExpeWidget(pos mathgl.Vec2d, dependees []DepNodeI, params func() interface{}, action func(interface{}) string) *LiveGoroutineExpeWidget {
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

	UniversalClock.AddChangeListener(w)

	// THINK: The only reason to have a separate action node is because current NotifyChange() does not tell the originator of change, so I can't tell UniversalClock's changes from dependee changes (and I need to do different actions for each)
	actionNode := &actionNode{owner: w, params: params, action: action}
	for _, dependee := range dependees {
		dependee.AddChangeListener(actionNode)
	}

	return w
}

func (w *LiveGoroutineExpeWidget) NotifyChange() {
	select {
	case s, ok := <-w.outChan:
		if ok {
			if s.t > w.lastFinishedT {
				w.lastFinishedT = s.t

				w.Content.Set(s.s)
				redraw = true
			}
		}
	default:
	}
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
			NewTriButtonExternalStateWidget(mathgl.Vec2d{}, func() bool { return w.started }, action),
			NewTextLabelWidgetString(mathgl.Vec2d{}, "pprof"),
		}, nil)

	return w
}

// ---

type SpinnerWidget struct {
	Widget
	Spinner uint32
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

func (w *SpinnerWidget) NotifyChange() {
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
	return &FpsWidget{Widget: NewWidget(pos, mathgl.Vec2d{})}
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

type FolderListingWidget struct {
	*CompositeWidget
	flow *FlowLayoutWidget // HACK: Shortcut to CompositeWidget.Widgets[0]
}

func NewFolderListingWidget(pos mathgl.Vec2d, path string) *FolderListingWidget {
	w := &FolderListingWidget{CompositeWidget: NewCompositeWidget(pos, mathgl.Vec2d{}, []Widgeter{NewFlowLayoutWidget(mathgl.Vec2d{}, []Widgeter{newFolderListingPureWidget(path)}, nil)})}
	w.flow = w.Widgets[0].(*FlowLayoutWidget)
	w.flow.SetParent(w) // HACK?
	return w
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
	w := &FolderListingPureWidget{Widget: NewWidget(mathgl.Vec2d{}, mathgl.Vec2d{}), path: path}
	w.NotifyChange() // TODO: Give it a proper source
	return w
}

func (w *FolderListingPureWidget) NotifyChange() {
	// TODO: Support for preserving selection

	entries, err := ioutil.ReadDir(w.path)
	if err == nil {
		w.entries = make([]os.FileInfo, 0, len(entries))
		w.longestEntryLength = 0
		for _, v := range entries {
			if !strings.HasPrefix(v.Name(), ".") {
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

		if bpkg, err := BuildPackageFromSrcDir(path); err == nil {
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

			newFolder = NewTextLabelWidgetString(mathgl.Vec2d{}, out)
		} else if isGitRepo, status := IsFolderGitRepo(path); isGitRepo {
			newFolder = NewTextLabelWidgetString(mathgl.Vec2d{}, status)
		} else {
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
}

func (w *FolderListingPureWidget) Layout() {
	if w.longestEntryLength < 3 {
		w.size[0] = float64(8 * 3)
	} else {
		w.size[0] = float64(8 * w.longestEntryLength)
	}
	if len(w.entries) == 0 {
		w.size[1] = float64(16 * 1)
	} else {
		w.size[1] = float64(16 * len(w.entries))
	}

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *FolderListingPureWidget) Render() {
	DrawNBox(w.pos, w.size)

	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	for i, v := range w.entries {
		if w.selected == uint64(i+1) {
			if hasTypingFocus {
				DrawBorderlessBox(w.pos.Add(mathgl.Vec2d{0, float64(i * 16)}), mathgl.Vec2d{w.size[0], 16}, mathgl.Vec3d{0.21, 0.45, 0.84})
				gl.Color3d(1, 1, 1)
			} else {
				DrawBorderlessBox(w.pos.Add(mathgl.Vec2d{0, float64(i * 16)}), mathgl.Vec2d{w.size[0], 16}, mathgl.Vec3d{0.83, 0.83, 0.83})
				gl.Color3d(0, 0, 0)
			}
		} else {
			gl.Color3d(0, 0, 0)
		}

		if v.IsDir() {
			PrintText(w.pos.Add(mathgl.Vec2d{0, float64(i * 16)}), v.Name()+"/")
		} else {
			PrintText(w.pos.Add(mathgl.Vec2d{0, float64(i * 16)}), v.Name())
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

	// Check if button 0 was released.
	if hasTypingFocus && inputEvent.Pointer.VirtualCategory == POINTING && (inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.InputId == 0 && inputEvent.Buttons[0] == false) {
		globalPosition := mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
		localPosition := GlobalToLocal(w, globalPosition)
		if len(w.entries) > 0 {
			if localPosition[1] < 0 {
				w.selected = 1
			} else if uint64((localPosition[1]/16)+1) > uint64(len(w.entries)) {
				w.selected = uint64(len(w.entries))
			} else {
				w.selected = uint64((localPosition[1] / 16) + 1)
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

/*func IsGitRepoWidgeter(w *Something) Widgeter {
	if w.IsGitRepo {
		return NewTextLabelWidgetString(mathgl.Vec2d{}, "@")
	} else {
		return NewTextLabelWidgetString(mathgl.Vec2d{}, " ")
	}
}

func StatusWidgeter(w *Something) Widgeter {
	if !w.IsGitRepo {
		return NewTextLabelWidgetString(mathgl.Vec2d{}, " ")
	}
	if w.Status != "" {
		return NewTextLabelWidgetStringTooltip(mathgl.Vec2d{}, "*", w.Status)
	} else {
		return NewTextLabelWidgetString(mathgl.Vec2d{}, " ")
	}
}

func RemoteWidgeter(w *Something) Widgeter {
	if !w.IsGitRepo {
		return NewTextLabelWidgetString(mathgl.Vec2d{}, " ")
	}
	if w.Remote != w.Local {
		return NewTextLabelWidgetStringTooltip(mathgl.Vec2d{}, "+", w.Remote)
	} else {
		return NewTextLabelWidgetStringTooltip(mathgl.Vec2d{}, " ", w.Remote)
	}
}

func LocalWidgeter(w *Something) Widgeter {
	if !w.IsGitRepo {
		return NewTextLabelWidgetString(mathgl.Vec2d{}, " ")
	}
	if w.Remote != w.Local {
		return NewTextLabelWidgetStringTooltip(mathgl.Vec2d{}, "+", w.Local)
	} else {
		return NewTextLabelWidgetStringTooltip(mathgl.Vec2d{}, " ", w.Local)
	}
}

func IsCommandWidgeter(w *Something) Widgeter {
	if w.IsGitRepo {
		return NewTextLabelWidgetString(mathgl.Vec2d{}, "/")
	} else {
		return NewTextLabelWidgetString(mathgl.Vec2d{}, " ")
	}
}

func rec(pkgs *[]Something, gopathEntry, importPath string) {
	pkg := SomethingFromPath(gopathEntry, importPath)
	if pkg != nil {
		//*pkgs = append(*pkgs, NewGoPackageWidget(mathgl.Vec2d{}, pkg))
		*pkgs = append(*pkgs, *pkg)
		(*pkgs)[len(*pkgs)-1].Update()
	}

	path := filepath.Join(gopathEntry, "src", importPath)
	entries, err := ioutil.ReadDir(path)
	if err == nil {
		for _, v := range entries {
			if v.IsDir() && !strings.HasPrefix(v.Name(), ".") {
				rec(pkgs, gopathEntry, filepath.Join(importPath, v.Name()))
			}
		}
	}
}

// In main()
{
	//pkgs := []Widgeter(nil)
	pkgs := []Something(nil)

	//pkgs = append(pkgs, NewGoPackageWidget(mathgl.Vec2d{}, "/Users/Dmitri/Dropbox/Work/2013/GoLand", "honnef.co/go/importer"))
	//pkgs = append(pkgs, NewGoPackageWidget(mathgl.Vec2d{}, SomethingFromPath("/Users/Dmitri/Dropbox/Work/2013/GoLand", "honnef.co/go/importer")))
	var skipGopath = map[string]bool{"/Users/Dmitri/Local/GoTrLand": true, "/Users/Dmitri/Dropbox/Work/2013/GoLanding": true}
	gopathEntries := filepath.SplitList(os.Getenv("GOPATH"))
	for _, gopathEntry := range gopathEntries {
		if skipGopath[gopathEntry] {
			continue
		}
		rec(&pkgs, gopathEntry, ".")
	}

	//widgets = append(widgets, NewFlowLayoutWidget(mathgl.Vec2d{}, pkgs, &FlowLayoutWidgetOptions{FlowLayoutType: VerticalLayout}))
	widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{}, &pkgs))
}

type GoPackageWidget struct {
	*FlowLayoutWidget
	//pkg *Something
}

func NewGoPackageWidget(pos mathgl.Vec2d, pkg *Something) *GoPackageWidget {
	contentFunc := func() Widgeter {
		pkg.Update()

		isGitRepo := IsGitRepoWidgeter(pkg)
		status := StatusWidgeter(pkg)
		remote := RemoteWidgeter(pkg)
		local := LocalWidgeter(pkg)
		isCommand := IsCommandWidgeter(pkg)
		bpkg := NewTextLabelWidgetString(mathgl.Vec2d{}, pkg.Bpkg.ImportPath)

		inner := NewFlowLayoutWidget(mathgl.Vec2d{}, []Widgeter{isGitRepo, status, remote, local, isCommand, bpkg}, nil)

		return inner
		//return pkg.String() //+ "\n" + goon.SdumpExpr(pkg.Local) + goon.SdumpExpr(pkg.Remote)
	}
	//mc := NewMultilineContentFunc(contentFunc, []DepNodeI{})
	// TODO: mc := NewMultilineContentStruct(&GoPackageWidget{}, []DepNodeI{})
	//mc.NotifyChange()
	//l := NewTextLabelWidgetExternalContent(mathgl.Vec2d{}, mc)

	l := contentFunc()

	b := NewButtonWidget(mathgl.Vec2d{}, nil)

	w := &GoPackageWidget{FlowLayoutWidget: NewFlowLayoutWidget(pos, []Widgeter{b, l}, nil)}

	w.Widgets[0].(*ButtonWidget).SetAction(func() { x := contentFunc(); x.SetParent(w); w.Widgets[1] = x; w.Layout() })

	return w
}*/

// ---

type Bool struct {
	bool
	DepNode
}

func (t *Bool) Get() bool {
	return t.bool
}
func (t *Bool) Toggle() {
	t.bool = !t.bool
	t.NotifyAllListeners()
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

	return &GoonWidget{CompositeWidget: NewCompositeWidget(pos, mathgl.Vec2d{}, []Widgeter{b, t}), a: a}*/

	a = UnsafeReflectValue(a)

	w := &GoonWidget{CompositeWidget: NewCompositeWidget(pos, mathgl.Vec2d{}, nil), title: title, a: a}
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
			w.expanded = NewTriButtonWidget(mathgl.Vec2d{-16 - 2}, func() { w.flip() })
		}

		w.CompositeWidget = NewCompositeWidget(w.pos, mathgl.Vec2d{}, []Widgeter{w.expanded, &Widget{}})
	} else {
		w.CompositeWidget = NewCompositeWidget(w.pos, mathgl.Vec2d{}, []Widgeter{&Widget{}})
	}
	w.SetParent(oldParent)

	var f *FlowLayoutWidget
	if w.expanded == nil || !w.expanded.State() {
		title := NewTextLabelWidgetString(mathgl.Vec2d{}, w.title+": ")

		var mc MultilineContentI
		if !expandable {
			// TODO: Strings need %#v, numbers need %+v
			mc = NewMultilineContentFuncInstant(func() string { return fmt.Sprintf("(%s)(%+v)", getTypeString(w.a.Elem()), w.a.Elem().Interface()) })
		} else {
			mc = NewMultilineContentString(fmt.Sprintf("%s{...}", getTypeString(w.a.Elem())))
		}
		t := NewTextLabelWidgetExternalContent(mathgl.Vec2d{}, mc)
		f = NewFlowLayoutWidget(mathgl.Vec2d{}, []Widgeter{title, t}, nil)
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
	return fmt.Sprintf("%T/%s", a.Interface(), a.Type().Name())
}

func (w *GoonWidget) setupInternals2(a reflect.Value) (f *FlowLayoutWidget) {
	v := a.Elem()

	title := NewTextLabelWidgetString(mathgl.Vec2d{}, w.title+": ")
	t := NewTextLabelWidgetString(mathgl.Vec2d{}, fmt.Sprintf("%s{", getTypeString(v)))
	header := NewFlowLayoutWidget(mathgl.Vec2d{}, []Widgeter{title, t}, nil)

	widgets := []Widgeter{header}

	switch v.Kind() {
	case reflect.Chan, reflect.Func, reflect.Interface, reflect.Map, reflect.Ptr, reflect.Slice:
		// TODO: Instead of skipping nil values, maybe pass the info as a bool parameter to query?
		if v.IsNil() {
			widgets = append(widgets, NewTextLabelWidgetString(mathgl.Vec2d{}, "}"))

			return NewFlowLayoutWidget(mathgl.Vec2d{}, widgets, &FlowLayoutWidgetOptions{FlowLayoutType: VerticalLayout})
		}
	}

	switch v.Kind() {
	case reflect.Struct:
		vt := v.Type()
		for i := 0; i < v.NumField(); i++ {
			widgets = append(widgets, setupInternals3(vt.Field(i).Name, v.Field(i)))
		}
	case reflect.Map:
		for _, key := range v.MapKeys() {
			widgets = append(widgets, setupInternals3(key.String(), v.MapIndex(key)))
		}
	case reflect.Array, reflect.Slice:
		for i := 0; i < v.Len(); i++ {
			widgets = append(widgets, setupInternals3(strconv.Itoa(i), v.Index(i)))
		}
	case reflect.Ptr, reflect.Interface:
		widgets = append(widgets, setupInternals3("*", v.Elem()))
	}

	widgets = append(widgets, NewTextLabelWidgetString(mathgl.Vec2d{}, "}"))

	return NewFlowLayoutWidget(mathgl.Vec2d{}, widgets, &FlowLayoutWidgetOptions{FlowLayoutType: VerticalLayout})
}

func setupInternals3(titleString string, a reflect.Value) Widgeter {
	tab := mathgl.Vec2d{8 * 4}

	/*if !a.CanInterface() {
		a = UnsafeReflectValue(a)
	}*/

	var w Widgeter
	if a.Kind() == reflect.Float64 && a.Addr().CanInterface() {
		title := NewTextLabelWidgetString(mathgl.Vec2d{}, titleString+": ")
		t := NewTest2Widget(mathgl.Vec2d{}, a.Addr().Interface().(*float64))
		w = NewFlowLayoutWidget(tab, []Widgeter{title, t}, nil)
	} else if a.Kind() == reflect.String {
		title := NewTextLabelWidgetString(mathgl.Vec2d{}, titleString+": ")
		t := NewTextBoxWidgetExternalContent(mathgl.Vec2d{}, NewMultilineContentPointer(a.Addr().Interface().(*string)))
		w = NewFlowLayoutWidget(tab, []Widgeter{title, t}, nil)
	} else if vv := a; vv.CanAddr() {
		w = newGoonWidget(tab, titleString, vv.Addr())
	} else if vv := a; (vv.Kind() == reflect.Interface || vv.Kind() == reflect.Ptr) && vv.Elem().CanAddr() { // HACK
		w = newGoonWidget(tab, titleString, vv.Elem().Addr())
	} else {
		//w = NewTextLabelWidgetString(tab, goon.Sdump(vv))
		w = NewTextLabelWidgetString(tab, fmt.Sprintf("(%s)(can't addr... %s)", vv.Kind().String(), vv.String()))
	}

	spacer := NewCompositeWidget(mathgl.Vec2d{}, mathgl.Vec2d{}, []Widgeter{w})

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

type CaretPosition struct {
	w               MultilineContentI
	caretPosition   uint32
	targetExpandedX uint32

	DepNode
}

func (cp *CaretPosition) Logical() uint32 {
	return cp.caretPosition
}

func (cp *CaretPosition) ExpandedPosition() (x uint32, y uint32) {
	caretPosition := cp.caretPosition
	caretLine := uint32(0)
	for caretPosition > cp.w.Lines()[caretLine].Length {
		caretPosition -= cp.w.Lines()[caretLine].Length + 1
		caretLine++
	}
	expandedCaretPosition := ExpandedLength(cp.w.Content()[cp.w.Lines()[caretLine].Start : cp.w.Lines()[caretLine].Start+caretPosition])

	return expandedCaretPosition, caretLine
}

func (cp *CaretPosition) Move(amount int8) {
	switch amount {
	case -1:
		cp.caretPosition--
	case +1:
		cp.caretPosition++
	case -2:
		_, y := cp.ExpandedPosition()
		cp.caretPosition = cp.w.Lines()[y].Start
	case +2:
		_, y := cp.ExpandedPosition()
		cp.caretPosition = cp.w.Lines()[y].Start + cp.w.Lines()[y].Length
	case -3:
		cp.caretPosition = 0
	case +3:
		y := len(cp.w.Lines()) - 1
		cp.caretPosition = cp.w.Lines()[y].Start + cp.w.Lines()[y].Length
	}

	x, _ := cp.ExpandedPosition()
	cp.targetExpandedX = x

	cp.NotifyAllListeners()
}

func (cp *CaretPosition) TryMoveV(amount int8) {
	_, y := cp.ExpandedPosition()

	switch amount {
	case -1:
		if y > 0 {
			y--
			line := cp.w.Content()[cp.w.Lines()[y].Start : cp.w.Lines()[y].Start+cp.w.Lines()[y].Length]
			cp.caretPosition = cp.w.Lines()[y].Start + ExpandedToLogical(line, cp.targetExpandedX)

			cp.NotifyAllListeners()
		} else {
			cp.Move(-2)
		}
	case +1:
		if y < uint32(len(cp.w.Lines()))-1 {
			y++
			line := cp.w.Content()[cp.w.Lines()[y].Start : cp.w.Lines()[y].Start+cp.w.Lines()[y].Length]
			cp.caretPosition = cp.w.Lines()[y].Start + ExpandedToLogical(line, cp.targetExpandedX)

			cp.NotifyAllListeners()
		} else {
			cp.Move(+2)
		}
	}
}

func (cp *CaretPosition) SetPositionFromPhysical(pos mathgl.Vec2d) {
	var y uint32
	if pos[1] < 0 {
		y = 0
	} else if pos[1] >= float64(len(cp.w.Lines())*16) {
		y = uint32(len(cp.w.Lines()) - 1)
	} else {
		y = uint32(pos[1] / 16)
	}

	if pos[0] < 0 {
		cp.targetExpandedX = 0
	} else {
		cp.targetExpandedX = uint32((pos[0] + 4) / 8)
	}

	line := cp.w.Content()[cp.w.Lines()[y].Start : cp.w.Lines()[y].Start+cp.w.Lines()[y].Length]
	cp.caretPosition = cp.w.Lines()[y].Start + ExpandedToLogical(line, cp.targetExpandedX)

	cp.NotifyAllListeners()
}

// ---

type ChangeListener interface {
	NotifyChange()
}

// ---

type MultilineContentI interface {
	Content() string
	Lines() []contentLine
	LongestLine() uint32

	Set(content string)

	AddChangeListener(l ChangeListener)
}

// ---

type contentLine struct {
	Start  uint32
	Length uint32
}

type MultilineContent struct {
	content     string
	lines       []contentLine
	longestLine uint32 // Line length

	DepNode
}

func NewMultilineContent() *MultilineContent {
	mc := new(MultilineContent)
	mc.updateLines()
	return mc
}
func NewMultilineContentString(content string) *MultilineContent {
	mc := new(MultilineContent)
	mc.Set(content)
	return mc
}

func (c *MultilineContent) Content() string      { return c.content }
func (c *MultilineContent) Lines() []contentLine { return c.lines }
func (c *MultilineContent) LongestLine() uint32  { return c.longestLine }

func (mc *MultilineContent) Set(content string) {
	mc.content = content
	mc.updateLines()

	mc.NotifyAllListeners()
}

func (w *MultilineContent) updateLines() {
	lines := GetLines(w.content)
	w.lines = make([]contentLine, len(lines))
	w.longestLine = 0
	for lineNumber, line := range lines {
		lineLength := ExpandedLength(line)
		if lineLength > w.longestLine {
			w.longestLine = lineLength
		}
		if lineNumber >= 1 {
			w.lines[lineNumber].Start = w.lines[lineNumber-1].Start + w.lines[lineNumber-1].Length + 1
		}
		w.lines[lineNumber].Length = uint32(len(line))
	}
}

// ---

type MultilineContentFile struct {
	*MultilineContent // TODO: Explore this being a pointer vs value
	path              string
}

func NewMultilineContentFile(path string) *MultilineContentFile {
	this := &MultilineContentFile{MultilineContent: NewMultilineContent(), path: path}
	this.Set(TryReadFile(this.path))
	UniversalClock.AddChangeListener(this)
	return this
}

func (this *MultilineContentFile) Set(content string) {
	this.content = content
	this.updateLines()

	// TODO: This shouldn't be triggered from TryReadFile() update below... Nor this.Set() in NewMultilineContentFile().
	{
		err := ioutil.WriteFile(this.path, []byte(this.Content()), 0666)
		CheckError(err)
	}

	this.NotifyAllListeners()
}

func (this *MultilineContentFile) NotifyChange() {
	// Check if the file has been changed externally, and if so, override this widget
	NewContent := TryReadFile(this.path)
	if NewContent != this.Content() {
		// TODO: Have this not trigger WriteFile() somehow...
		this.Set(NewContent)
	}
}

func (this *MultilineContentFile) Path() string {
	return this.path
}

// ---

type MultilineContentPointer struct {
	*MultilineContent
	p *string
}

func NewMultilineContentPointer(p *string) *MultilineContentPointer {
	this := &MultilineContentPointer{MultilineContent: NewMultilineContent(), p: p}
	this.Set(*p)
	UniversalClock.AddChangeListener(this)
	return this
}

func (this *MultilineContentPointer) Set(content string) {
	this.content = content
	this.updateLines()

	// TODO: This shouldn't be triggered from TryReadFile() update below... Nor this.Set() in NewMultilineContentPointer().
	*this.p = this.Content()

	this.NotifyAllListeners()
}

func (this *MultilineContentPointer) NotifyChange() {
	// Check if the file has been changed externally, and if so, override this widget
	NewContent := *this.p
	if NewContent != this.Content() {
		// TODO: Have this not trigger WriteFile() somehow...
		this.Set(NewContent)
	}
}

// ---

type MultilineContentFunc struct {
	*MultilineContent // TODO: Explore this being a pointer vs value
	contentFunc       func() string
}

// THINK: Merge the func and dependees into one struct? Maybe can't if funcs can have different signatures...
func NewMultilineContentFunc(contentFunc func() string, dependees []DepNodeI) *MultilineContentFunc {
	this := &MultilineContentFunc{MultilineContent: NewMultilineContent(), contentFunc: contentFunc}
	for _, dependee := range dependees {
		dependee.AddChangeListener(this)
	}
	return this
}

func (*MultilineContentFunc) Set(string) {
	// TODO: Figure out if it's okay to effectively ignore this... or should I prevent it from being possible to call Set()?
	// Do nothing because the content of MultilineContentFunc can't be set as a string
}

func (this *MultilineContentFunc) NotifyChange() {
	NewContent := this.contentFunc()
	if NewContent != this.MultilineContent.Content() {
		this.MultilineContent.Set(NewContent)
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
	Content MultilineContentI
	tooltip Widgeter
}

func NewTextLabelWidgetExternalContent(pos mathgl.Vec2d, mc MultilineContentI) *TextLabelWidget {
	w := &TextLabelWidget{
		Widget:  NewWidget(pos, mathgl.Vec2d{0, 0}),
		Content: mc,
	}
	mc.AddChangeListener(w) // TODO: What about removing w when it's "deleted"?
	return w
}

func NewTextLabelWidgetString(pos mathgl.Vec2d, s string) *TextLabelWidget {
	mc := NewMultilineContentString(s)
	w := NewTextLabelWidgetExternalContent(pos, mc)
	return w
}

func NewTextLabelWidgetStringTooltip(pos mathgl.Vec2d, s, tooltip string) *TextLabelWidget {
	mc := NewMultilineContentString(s)
	w := NewTextLabelWidgetExternalContent(pos, mc)
	w.tooltip = NewTextLabelWidgetString(mathgl.Vec2d{}, tooltip)
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
		w.size[0] = float64(8 * 3)
	} else {
		w.size[0] = float64(8 * w.Content.LongestLine())
	}
	w.size[1] = float64(16 * len(w.Content.Lines()))

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *TextLabelWidget) Render() {
	DrawLGBox(w.pos, w.size)

	gl.Color3d(0, 0, 0)
	for lineNumber, contentLine := range w.Content.Lines() {
		PrintLine(mathgl.Vec2d{w.pos[0], w.pos[1] + float64(16*lineNumber)}, w.Content.Content()[contentLine.Start:contentLine.Start+contentLine.Length])
	}

	isHit := len(w.HoverPointers()) > 0
	// Tooltip
	if w.tooltip != nil && isHit {
		mousePointerPositionLocal := GlobalToLocal(w, mathgl.Vec2d{mousePointer.State.Axes[0], mousePointer.State.Axes[1]})
		w.tooltip.Layout()
		tooltipOffset := mathgl.Vec2d{0, -16 - w.tooltip.Size()[1]}
		w.tooltip.SetPos(w.pos.Add(mousePointerPositionLocal).Add(tooltipOffset))
		w.tooltip.Render()
	}
}

// ---

type TextBoxWidget struct {
	Widget
	Content       MultilineContentI
	caretPosition CaretPosition
}

func NewTextBoxWidget(pos mathgl.Vec2d) *TextBoxWidget {
	mc := NewMultilineContent()
	return NewTextBoxWidgetExternalContent(pos, mc)
}

func NewTextBoxWidgetExternalContent(pos mathgl.Vec2d, mc MultilineContentI) *TextBoxWidget {
	w := &TextBoxWidget{
		Widget:        NewWidget(pos, mathgl.Vec2d{0, 0}),
		Content:       mc,
		caretPosition: CaretPosition{w: mc},
	}
	mc.AddChangeListener(w) // TODO: What about removing w when it's "deleted"?
	return w
}

func (w *TextBoxWidget) NotifyChange() {
	if w.caretPosition.caretPosition > uint32(len(w.Content.Content())) {
		w.caretPosition.Move(+3)
	}

	w.Layout()

	w.NotifyAllListeners()

	// TODO: Figure out if this should be here... is it a big deal if it gets called here AND elsewhere?
	redraw = true
}

func (w *TextBoxWidget) Layout() {
	if w.Content.LongestLine() < 3 {
		w.size[0] = float64(8 * 3)
	} else {
		w.size[0] = float64(8 * w.Content.LongestLine())
	}
	w.size[1] = float64(16 * len(w.Content.Lines()))

	// TODO: Standardize this mess... have graph-level func that don't get overriden, and class-specific funcs to be overridden
	w.Widget.Layout()
}

func (w *TextBoxWidget) Render() {
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

	gl.Color3d(0, 0, 0)
	for lineNumber, contentLine := range w.Content.Lines() {
		PrintLine(mathgl.Vec2d{w.pos[0], w.pos[1] + float64(16*lineNumber)}, w.Content.Content()[contentLine.Start:contentLine.Start+contentLine.Length])
	}

	if hasTypingFocus {
		expandedCaretPosition, caretLine := w.caretPosition.ExpandedPosition()

		// Draw caret
		gl.PushMatrix()
		defer gl.PopMatrix()
		gl.Translated(gl.Double(w.pos[0]), gl.Double(w.pos[1]), 0)
		gl.Color3d(0, 0, 0)
		gl.Recti(gl.Int(expandedCaretPosition*8-1), gl.Int(caretLine*16), gl.Int(expandedCaretPosition*8+1), gl.Int(caretLine*16)+16)
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
		globalPosition := mathgl.Vec2d{inputEvent.Pointer.State.Axes[0], inputEvent.Pointer.State.Axes[1]}
		localPosition := GlobalToLocal(w, globalPosition)
		w.caretPosition.SetPositionFromPhysical(localPosition)
	}

	if inputEvent.Pointer.VirtualCategory == TYPING && inputEvent.EventTypes[BUTTON_EVENT] && inputEvent.Buttons[0] == true {
		switch glfw.Key(inputEvent.InputId) {
		case glfw.KeyBackspace:
			if w.caretPosition.Logical() >= 1 {
				w.caretPosition.Move(-1)
				w.Content.Set(w.Content.Content()[:w.caretPosition.Logical()] + w.Content.Content()[w.caretPosition.Logical()+1:])
			}
		case glfw.KeyDelete:
			if w.caretPosition.Logical()+1 <= uint32(len(w.Content.Content())) {
				w.Content.Set(w.Content.Content()[:w.caretPosition.Logical()] + w.Content.Content()[w.caretPosition.Logical()+1:])
			}
		case glfw.KeyEnter:
			w.Content.Set(w.Content.Content()[:w.caretPosition.Logical()] + "\n" + w.Content.Content()[w.caretPosition.Logical():])
			w.caretPosition.Move(+1)
		case glfw.KeyTab:
			w.Content.Set(w.Content.Content()[:w.caretPosition.Logical()] + "\t" + w.Content.Content()[w.caretPosition.Logical():])
			w.caretPosition.Move(+1)
		case glfw.KeyLeft:
			if inputEvent.ModifierKey == glfw.ModSuper {
				// TODO: Go to start of line-ish (toggle between real start and non-whitespace start); leave Move(-2) alone because it's used elsewhere for existing purpose
				w.caretPosition.Move(-2)
			} else if inputEvent.ModifierKey == 0 {
				if w.caretPosition.Logical() >= 1 {
					w.caretPosition.Move(-1)
				}
			}
		case glfw.KeyRight:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.caretPosition.Move(+2)
			} else if inputEvent.ModifierKey == 0 {
				if w.caretPosition.Logical() < uint32(len(w.Content.Content())) {
					w.caretPosition.Move(+1)
				}
			}
		case glfw.KeyUp:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.caretPosition.Move(-3)
			} else if inputEvent.ModifierKey == 0 {
				w.caretPosition.TryMoveV(-1)
			}
		case glfw.KeyDown:
			if inputEvent.ModifierKey == glfw.ModSuper {
				w.caretPosition.Move(+3)
			} else if inputEvent.ModifierKey == 0 {
				w.caretPosition.TryMoveV(+1)
			}
		case glfw.KeyX:
			if inputEvent.ModifierKey == glfw.ModSuper {
				globalWindow.SetClipboardString(w.Content.Content()) // TODO: Don't use globalWindow
				w.Content.Set("")
			}
		case glfw.KeyC:
			if inputEvent.ModifierKey == glfw.ModSuper {
				globalWindow.SetClipboardString(w.Content.Content()) // TODO: Don't use globalWindow
			}
		case glfw.KeyV:
			if inputEvent.ModifierKey == glfw.ModSuper {
				// TODO: Don't use globalWindow
				if clipboard, err := globalWindow.GetClipboardString(); err == nil && clipboard != "" {
					//EraseSelectionIfAny();
					w.Content.Set(w.Content.Content()[:w.caretPosition.Logical()] + clipboard + w.Content.Content()[w.caretPosition.Logical():])
					for _ = range []byte(clipboard) { // TODO
						w.caretPosition.Move(+1)
					}
				}
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
		w.Content.Set(w.Content.Content()[:w.caretPosition.Logical()] + string(byte(inputEvent.InputId)) + w.Content.Content()[w.caretPosition.Logical():])
		w.caretPosition.Move(+1)
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
	// TODO: So now both TextBoxWidget and TextFileWidget are set to receive change notifications from ec... Is that cool? Or should I remove TextBoxWidget here? Or go from TextBoxWidget to TextFileWidget rather than from ec directly?
	ec.AddChangeListener(w)
	return w
}

func (w *TextFileWidget) Path() string {
	return w.TextBoxWidget.Content.(*MultilineContentFile).Path()
}

func (w *TextFileWidget) NotifyChange() {
	redraw = true
}

// ---

func NewTextBoxWidgetContentFunc(pos mathgl.Vec2d, contentFunc func() string, dependees []DepNodeI) *TextBoxWidget {
	mc := NewMultilineContentFunc(contentFunc, dependees)
	w := NewTextBoxWidgetExternalContent(pos, mc)
	return w
}

// ---

type TextFieldWidget struct {
	Widget
	Content       string
	CaretPosition uint32
}

func NewTextFieldWidget(pos mathgl.Vec2d) *TextFieldWidget {
	return &TextFieldWidget{NewWidget(pos, mathgl.Vec2d{0, 0}), "", 0}
}

func (w *TextFieldWidget) Render() {
	// HACK: Should iterate over all typing pointers, not just assume keyboard pointer and its first mapping
	hasTypingFocus := keyboardPointer != nil && len(keyboardPointer.OriginMapping) > 0 && w == keyboardPointer.OriginMapping[0]

	// HACK: Setting the widget size in Render() is a bad, because all the input calculations will fail before it's rendered
	if len(w.Content) < 3 {
		w.size[0] = float64(8 * 3)
	} else {
		w.size[0] = float64(8 * len(w.Content))
	}
	w.size[1] = 16

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
		gl.Recti(gl.Int(w.CaretPosition*8-1), 0, gl.Int(w.CaretPosition*8+1), 16)
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
				w.Content = w.Content[:w.CaretPosition] + w.Content[w.CaretPosition+1:]
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
		w.CaretPosition++
	}
}

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

	// HACK: Setting the widget size in Render() is a bad, because all the input calculations will fail before it's rendered
	if len(w.Content) < 3 {
		w.size[0] = float64(8 * 3)
	} else {
		w.size[0] = float64(8 * len(w.Content))
	}
	w.size[1] = 16

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
			if inputEvent.Pointer.VirtualCategory == POINTING &&
				(inputEvent.EventTypes[AXIS_EVENT] && inputEvent.InputId == 0 || inputEvent.EventTypes[SLIDER_EVENT] && inputEvent.InputId == 2) {

				LocalPosition := GlobalToLocal(widget, mathgl.Vec2d{float64(inputEvent.Pointer.State.Axes[0]), float64(inputEvent.Pointer.State.Axes[1])})

				// Clear previously hit widgets
				for _, widget := range inputEvent.Pointer.Mapping {
					delete(widget.HoverPointers(), inputEvent.Pointer)
				}
				inputEvent.Pointer.Mapping = []Widgeter{}

				// Recalculate currently hit widgets
				for _, widget := range widgets {
					inputEvent.Pointer.Mapping = append(inputEvent.Pointer.Mapping, widget.Hit(LocalPosition)...)
				}
				for _, widget := range inputEvent.Pointer.Mapping {
					widget.HoverPointers()[inputEvent.Pointer] = true
				}
			}

			// Populate PointerMappings (but only when pointer is moved while not active, and this isn't a deactivation since that's handled below)
			if inputEvent.Pointer.VirtualCategory == POINTING && inputEvent.EventTypes[AXIS_EVENT] && inputEvent.InputId == 0 &&
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

func main() {
	//defer profile.Start(profile.MemProfile).Stop()

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

		if !glfw.Init() {
			panic("glfw.Init()")
		}
		fmt.Printf("glfw %d.%d.%d; ", glfw.VersionMajor, glfw.VersionMinor, glfw.VersionRevision)
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
		fmt.Println(gl.GoStringUb(gl.GetString(gl.VENDOR)), gl.GoStringUb(gl.GetString(gl.RENDERER)), gl.GoStringUb(gl.GetString(gl.VERSION)))

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

		window.SetMouseButtonCallback(func(w *glfw.Window, button glfw.MouseButton, action glfw.Action, mod glfw.ModifierKey) {
			inputEvent := InputEvent{
				Pointer:    mousePointer,
				EventTypes: map[EventType]bool{BUTTON_EVENT: true},
				InputId:    uint16(button),
				Buttons:    []bool{action != glfw.Release},
				Sliders:    nil,
				Axes:       nil,
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

	spinner := SpinnerWidget{NewWidget(mathgl.Vec2d{20, 20}, mathgl.Vec2d{0, 0}), 0}
	widgets = append(widgets, &spinner)
	if true {
		widgets = append(widgets, &BoxWidget{NewWidget(mathgl.Vec2d{50, 150}, mathgl.Vec2d{16, 16}), "The Original Box"})
		widgets = append(widgets, NewCompositeWidget(mathgl.Vec2d{150, 150}, mathgl.Vec2d{0, 0},
			[]Widgeter{
				&BoxWidget{NewWidget(mathgl.Vec2d{0, 0}, mathgl.Vec2d{16, 16}), "Left of Duo"},
				&BoxWidget{NewWidget(mathgl.Vec2d{16 + 2, 0}, mathgl.Vec2d{16, 16}), "Right of Duo"},
			}))
		widgets = append(widgets, &UnderscoreSepToCamelCaseWidget{NewWidget(mathgl.Vec2d{50, 180}, mathgl.Vec2d{0, 0}), window})
		widgets = append(widgets, NewTextFieldWidget(mathgl.Vec2d{50, 50}))
		widgets = append(widgets, NewMetaTextFieldWidget(mathgl.Vec2d{50, 70}))
		widgets = append(widgets, NewChannelExpeWidget(mathgl.Vec2d{10, 220}))
		widgets = append(widgets, NewTextBoxWidget(mathgl.Vec2d{50, 5}))
		widgets = append(widgets, NewTextFileWidget(mathgl.Vec2d{100, 25}, "/Users/Dmitri/Dropbox/Needs Processing/Sample.txt"))
		widgets = append(widgets, NewTextBoxWidgetExternalContent(mathgl.Vec2d{100, 60}, widgets[len(widgets)-1].(*TextFileWidget).TextBoxWidget.Content))   // HACK: Manual test
		widgets = append(widgets, NewTextLabelWidgetExternalContent(mathgl.Vec2d{100, 95}, widgets[len(widgets)-2].(*TextFileWidget).TextBoxWidget.Content)) // HACK: Manual test
		widgets = append(widgets, NewKatWidget(mathgl.Vec2d{370, 20}))
		{
			src := NewTextFileWidget(mathgl.Vec2d{}, "/Users/Dmitri/Dropbox/Work/2013/GoLand/src/gist.github.com/7176504.git/main.go")
			//src := NewTextFileWidget(mathgl.Vec2d{}, "./GoLand/src/simple.go")
			//src := NewTextFileWidget(mathgl.Vec2d{}, "/Users/Dmitri/Dropbox/Work/2013/GoLand/src/gist.github.com/5694308.git/main.go")
			//src := NewTextFileWidget(mathgl.Vec2d{0, 0}, "/Users/Dmitri/Dropbox/Work/2013/GoLand/src/gist.github.com/5068062.git/gistfile1.go")
			//src := NewTextBoxWidget(mathgl.Vec2d{50, 200})

			build := NewLiveCmdExpeWidget(mathgl.Vec2d{}, []DepNodeI{src}, NewCmdTemplate("go", "build", "-o", "./Con2RunBin", src.Path()))
			build.AddChangeListener(&spinner)

			run := NewLiveCmdExpeWidget(mathgl.Vec2d{}, []DepNodeI{&build.FinishedDepNode}, NewCmdTemplate("./Con2RunBin")) // TODO: Proper path

			widgets = append(widgets, NewFlowLayoutWidget(mathgl.Vec2d{50, 200}, []Widgeter{src, build, run}, nil))
		}
		if false {
			contentFunc := func() string { return TrimLastNewline(goon.Sdump(widgets[7])) }
			widgets = append(widgets, NewTextBoxWidgetContentFunc(mathgl.Vec2d{390, -1525}, contentFunc, []DepNodeI{&UniversalClock}))
		}
		widgets = append(widgets, NewTest2Widget(mathgl.Vec2d{250, 5}, &widgets[7].(*TextBoxWidget).pos[0]))

		// GoForcedUseWidget
		{
			src := NewTextBoxWidget(mathgl.Vec2d{})
			label := NewTextLabelWidgetString(mathgl.Vec2d{}, "go Forced Use")

			params := func() interface{} { return src.Content.Content() }
			action := func(param interface{}) string {
				if strings.TrimSpace(param.(string)) != "" {
					//started := time.Now(); defer func() { fmt.Println(time.Since(started).Seconds()) }()
					return GetForcedUseFromImport(strings.TrimSpace(src.Content.Content()))
				} else {
					return ""
				}
			}
			dst := NewLiveGoroutineExpeWidget(mathgl.Vec2d{}, []DepNodeI{src}, params, action)

			w := NewFlowLayoutWidget(mathgl.Vec2d{80, 130}, []Widgeter{src, label, dst}, nil)
			widgets = append(widgets, w)
		}
		// GoForcedUseWidget2
		{
			src := NewTextBoxWidget(mathgl.Vec2d{})
			label := NewTextLabelWidgetString(mathgl.Vec2d{}, "go Forced Use")

			params := func() interface{} { return src.Content.Content() }
			action := func(param interface{}) string {
				if strings.TrimSpace(param.(string)) != "" {
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
			dst := NewLiveGoroutineExpeWidget(mathgl.Vec2d{}, []DepNodeI{src}, params, action)

			w := NewFlowLayoutWidget(mathgl.Vec2d{80, 150}, []Widgeter{src, label, dst}, nil)
			widgets = append(widgets, w)
		}

		// git diff
		{
			source := widgets[12].(*FlowLayoutWidget).Widgets[0].(*TextFileWidget)
			dir, file := filepath.Split(source.Path())
			if isGitRepo, _ := IsFolderGitRepo(dir); isGitRepo {
				template := NewCmdTemplate("git", "diff", "--no-ext-diff", "--", file)
				template.Dir = dir
				w := NewLiveCmdExpeWidget(mathgl.Vec2d{}, []DepNodeI{source}, template) // TODO: Figure out []DepNodeI{source} vs. []DepNodeI{source.Content}?

				widgets[12].(*FlowLayoutWidget).Widgets = append(widgets[12].(*FlowLayoutWidget).Widgets, w)
				w.SetParent(widgets[12]) // Needed for pointer coordinates to be accurate
				widgets[12].(*FlowLayoutWidget).Layout()
			}
		}

		// Shows the AST node underneath caret (asynchonously via LiveGoroutineExpeWidget)
		{
			w := NewTest3Widget(mathgl.Vec2d{0, 0}, widgets[12].(*FlowLayoutWidget).Widgets[0].(*TextFileWidget).TextBoxWidget)
			//w := NewTest4Widget(mathgl.Vec2d{0, 0}, widgets[12].(*FlowLayoutWidget).Widgets[0].(*TextFileWidget))
			widgets[12].(*FlowLayoutWidget).Widgets = append(widgets[12].(*FlowLayoutWidget).Widgets, w)
			w.SetParent(widgets[12]) // Needed for pointer coordinates to be accurate
			widgets[12].(*FlowLayoutWidget).Layout()
		}

		// NumGoroutines
		{
			contentFunc := func() string { return fmt.Sprint(runtime.NumGoroutine()) }
			mc := NewMultilineContentFunc(contentFunc, []DepNodeI{&UniversalClock})
			widgets = append(widgets, NewTextLabelWidgetExternalContent(mathgl.Vec2d{10, 40}, mc))
		}

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
		widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{410, 10}, &x))
		y := NewWidget(mathgl.Vec2d{1, 2}, mathgl.Vec2d{3})
		widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{410, 40}, &y))
		widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{510, 70}, &widgets))
		widgets = append(widgets, NewGoonWidget(mathgl.Vec2d{510, 100}, &keyboardPointer))

		widgets = append(widgets, NewFolderListingWidget(mathgl.Vec2d{360, 70}, "./GoLand/src/"))

		http.HandleFunc("/close", func(w http.ResponseWriter, r *http.Request) {
			fmt.Fprintln(w, "Closing.")
			keepRunning = false
		})
		http.HandleFunc("/widgets", func(w http.ResponseWriter, r *http.Request) {
			w.Write([]byte(goon.SdumpExpr(len(widgets))))
			fmt.Fprintln(w)
			fmt.Fprintf(w, "%#v\n", widgets)
		})
		http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {

			// HACK: Handle .go files specially, just assume they're in "./GoLand"
			if strings.HasSuffix(r.URL.Path, ".go") {
				w.Header().Set("Content-Type", "text/plain")
				w.Write(MustReadFileB(filepath.Join("./GoLand/src/", r.URL.Path)))
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
			if something := SomethingFromImportPath(importPath); something != nil {
				something.Update()

				dpkg := GetDocPackageAll(something.Bpkg, nil)

				b += Underline(`import "`+dpkg.ImportPath+`"`) + "\n```Go\n"
				for _, v := range dpkg.Vars {
					b += SprintAstBare(v.Decl) + "\n"
				}
				for _, t := range dpkg.Types {
					for _, v := range t.Vars {
						b += SprintAstBare(v.Decl) + "\n"
					}
				}
				b += "\n"
				for _, f := range dpkg.Funcs {
					b += SprintAstBare(f.Decl) + "\n"
				}
				for _, t := range dpkg.Types {
					for _, f := range t.Funcs {
						b += SprintAstBare(f.Decl) + "\n"
					}
					for _, m := range t.Methods {
						b += SprintAstBare(m.Decl) + "\n"
					}
				}
				b += "\n"
				for _, c := range dpkg.Consts {
					b += strings.Join(c.Names, "\n") + "\n"
				}
				for _, t := range dpkg.Types {
					for _, c := range t.Consts {
						b += strings.Join(c.Names, "\n") + "\n"
					}
				}
				b += "\n"
				for _, t := range dpkg.Types {
					b += t.Name + "\n"
					//PrintlnAstBare(t.Decl)
				}
				b += "\n```\n"

				b += "\n---\n\n"

				b += "```\n" + something.String() + "\n```\n"

				b += "\n---\n\n"

				if something.IsGitRepo {
					if something.Status == "" {
						b += "nothing to commit, working directory clean  \n"
					} else {
						b += something.Status + "  \n"
					}
					b += something.Local + "  \n"
					b += something.Remote + "  \n"

					// git diff
					if something.Status != "" {
						cmd := exec.Command("git", "diff", "--no-ext-diff")
						cmd.Dir = something.Path
						if outputBytes, err := cmd.CombinedOutput(); err == nil {
							b += "\n```diff\n" + string(outputBytes) + "\n```\n"
						}
					}
				}

				b += "\n---\n\n"

				x := newFolderListingPureWidget(something.Path)
				for _, v := range x.entries {
					b += fmt.Sprintf("[%s](%s)  \n", v.Name(), filepath.Join(r.URL.Path, v.Name()))
				}
			} else {
				fmt.Fprintf(w, "Package %q not found in %q (are you sure it's a valid Go package; maybe its subdir).\n", importPath, os.Getenv("GOPATH"))
			}

			if plain {
				w.Write([]byte(b))
			} else {
				w.Write(blackfriday.MarkdownCommon([]byte(b)))
			}
		})
		widgets = append(widgets, NewHttpServerTestWidget(mathgl.Vec2d{10, 130}))

		// Shuryear Clock
		{
			contentFunc := func() string {
				shuryearNow := 1970 + float64(time.Now().UnixNano())/(3600*24*3652422*100000)
				return fmt.Sprintf("%.8f", shuryearNow)
			}
			mc := NewMultilineContentFunc(contentFunc, []DepNodeI{&UniversalClock})
			widgets = append(widgets, NewTextLabelWidgetExternalContent(mathgl.Vec2d{535, 1}, mc)) // TODO: Stick to top right corner?
		}

		{
			buttonTrigger := NewButtonTriggerWidget(mathgl.Vec2d{50, 30})
			buttonTrigger.AddChangeListener(&spinner)
			widgets = append(widgets, buttonTrigger)
		}
	} else if false {
		widgets = append(widgets, NewGpcFileWidget(mathgl.Vec2d{1100, 500}, "/Users/Dmitri/Dropbox/Work/2013/eX0 Project/eX0 Client/levels/test3.wwl"))
		widgets = append(widgets, NewTest1Widget(mathgl.Vec2d{10, 50}))
		widgets = append(widgets, NewKatWidget(mathgl.Vec2d{370, 20}))
	} else if true {
		//widgets = append(widgets, NewLiveCmdExpeWidget(mathgl.Vec2d{50, 0}))

		// NumGoroutines
		{
			contentFunc := func() string { return fmt.Sprint(runtime.NumGoroutine()) }
			mc := NewMultilineContentFunc(contentFunc, []DepNodeI{&UniversalClock})
			widgets = append(widgets, NewTextLabelWidgetExternalContent(mathgl.Vec2d{10, 40}, mc))
		}
	}
	fpsWidget := NewFpsWidget(mathgl.Vec2d{10, 120})
	widgets = append(widgets, fpsWidget)

	widget := NewCanvasWidget(mathgl.Vec2d{}, widgets, nil)

	// ---

	for keepRunning {
		frameStartTime := time.Now()

		if !*headlessFlag {
			//glfw.WaitEvents()
			glfw.PollEvents()

			// HACK: Close window check
			if glfw.Release != window.GetKey(glfw.KeyEscape) {
				keepRunning = false
			}
		}

		// Input
		inputEventQueue = ProcessInputEventQueue(widget, inputEventQueue)

		UniversalClock.TimePassed = 1.0 / 60 // TODO: Use proper value?
		UniversalClock.NotifyAllListeners()

		if redraw && !*headlessFlag {
			gl.Clear(gl.COLOR_BUFFER_BIT)
			gl.LoadIdentity()

			widget.Render()

			//spinner.NotifyChange()
			fpsWidget.PushTimeToRender(time.Since(frameStartTime).Seconds() * 1000)
			window.SwapBuffers()
		} else {
			time.Sleep(5 * time.Millisecond)
		}

		runtime.Gosched()

		if redraw && !*headlessFlag {
			fpsWidget.PushTimeTotal(time.Since(frameStartTime).Seconds() * 1000)
			redraw = false
		}
	}
}
