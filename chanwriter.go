package main

type ChanWriter chan []byte

func (cw ChanWriter) Write(p []byte) (n int, err error) {
	// TODO: Copy the slice contents rather than sending the original, as it may get modified after we return?
	cw <- p
	return len(p), nil
}
