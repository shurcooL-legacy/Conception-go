package multilinecontent

func NewReverse() *ReverseMultilineContent {
	rmc := &ReverseMultilineContent{&MultilineContent{}}
	rmc.InitViewGroup(rmc, "memory://???(reverse)")
	rmc.updateLines()
	return rmc
}

// TEST: An unfinished experiment.
type ReverseMultilineContent struct {
	*MultilineContent
}

func (c *ReverseMultilineContent) Content() string { return reverse(c.content) }

/*func (c *ReverseMultilineContent) SetSelf(content string) {
	c.content = reverse(content)
	c.updateLines()
}*/

// reverse returns a reversed s.
func reverse(s string) string {
	r := []rune(s)
	for i, j := 0, len(r)-1; i < j; i, j = i+1, j-1 {
		r[i], r[j] = r[j], r[i]
	}
	return string(r)
}
