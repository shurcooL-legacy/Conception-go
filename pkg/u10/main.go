// Package u10 provides an http.Handler with options that serves rendered Markdown.
package u10

import (
	"fmt"
	"io"
	"net/http"
	"time"

	"github.com/shurcooL/github_flavored_markdown"
	"github.com/shurcooL/go/gfmutil"

	// An experiment in making the frontend resources available.
	// This registers on default mux paths "/table-of-contents.js" and "/table-of-contents.css".
	// TODO: This is not quite done and requires figuring out a good way to solve the challenge...
	//       Relative paths do not work at all when it's a library rather than package main.
	// TODO: Perhaps the strings `<script type="text/javascript" src="/table-of-contents.js"></script>` and
	//       `<link href="/table-of-contents.css" media="all" rel="stylesheet" type="text/css" />` should be coming
	//       from the TOC handler package?
	// TODO: Perhaps I could use "/go/import/path" notation to ensure no path collisions?
	_ "github.com/shurcooL/frontend/table-of-contents/handler"
)

type Options struct {
	TableOfContents bool
}

// MarkdownOptionsHandlerFunc is an http.Handler that serves rendered Markdown.
type MarkdownOptionsHandlerFunc func(req *http.Request) (markdown []byte, opt *Options, err error)

func (f MarkdownOptionsHandlerFunc) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	markdown, opt, err := f(req)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	if opt == nil {
		opt = &Options{}
	}

	if _, plain := req.URL.Query()["plain"]; plain {
		w.Header().Set("Content-Type", "text/plain")
		w.Write(markdown)
	} else if _, github := req.URL.Query()["github"]; github {
		w.Header().Set("Content-Type", "text/html")
		started := time.Now()
		switch opt.TableOfContents {
		case false:
			gfmutil.WriteGitHubFlavoredMarkdownViaGitHub(w, markdown)
		case true:
			http.Error(w, "not implemented", http.StatusInternalServerError)
			panic("not implemented")
		}
		fmt.Println("rendered GFM via GitHub, took", time.Since(started))
	} else {
		w.Header().Set("Content-Type", "text/html")
		started := time.Now()
		switch opt.TableOfContents {
		case false:
			gfmutil.WriteGitHubFlavoredMarkdownViaLocal(w, markdown)
		case true:
			writeGitHubFlavoredMarkdownViaLocalWithToc(w, markdown)
		}
		fmt.Println("rendered GFM locally, took", time.Since(started))
	}
}

// writeGitHubFlavoredMarkdownViaLocalWithToc renders GFM as a full HTML page with table of contents and writes to w.
// It assumes that GFM CSS is available at /assets/gfm/gfm.css.
func writeGitHubFlavoredMarkdownViaLocalWithToc(w io.Writer, markdown []byte) {
	io.WriteString(w, `<html><head><meta charset="utf-8"><link href="/assets/gfm/gfm.css" media="all" rel="stylesheet" type="text/css" /><link href="/assets/octicons/octicons.css" media="all" rel="stylesheet" type="text/css" /><link href="/table-of-contents.css" media="all" rel="stylesheet" type="text/css" /></head><body><article class="markdown-body entry-content" style="padding: 30px;">`)
	w.Write(github_flavored_markdown.Markdown(markdown))
	io.WriteString(w, `</article><script type="text/javascript" src="/table-of-contents.js"></script></body></html>`)
}
