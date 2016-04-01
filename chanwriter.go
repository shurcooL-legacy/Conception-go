package main

type ChanWriter chan []byte

func (cw ChanWriter) Write(p []byte) (n int, err error) {
	// Make a copy of p in order to avoid retaining it.
	b := make([]byte, len(p))
	copy(b, p)
	cw <- b
	return len(b), nil
}
