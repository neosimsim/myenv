package main

// Usage: Afmt [flags] [formatter [formatter args...]]
//
// Formats the current Acme window using <formatter>.
//
// Flags
//   -v	print verbose messages
//
// Based on "9fans.net/go/acme/acmego"

import (
	"bytes"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"strconv"
	"strings"

	"9fans.net/go/acme"
)

var formatters = map[string][]string{
	".go":  []string{"goimports"},
	".nix": []string{"nixpkgs-fmt"},
	".rs":  []string{"rustfmt"},
	".hs":  []string{"ormolu"},
	".ex":  []string{"mix", "format", "-"},
	".exs": []string{"mix", "format", "-"},
}

func main() {
	verbose := flag.Bool("v", false, "print verbose messages")
	flag.Parse()

	winid, err := strconv.Atoi(os.Getenv("winid"))
	if err != nil {
		log.Fatalf("Could not read $winid: %v", err)
	}

	formatter := flag.Args()
	if len(flag.Args()) == 0 {

		windows, err := acme.Windows()
		if err != nil {
			log.Fatalf("Could not acme windows: %v", err)
		}

		var filePath string
		for _, window := range windows {
			if window.ID == winid {
				filePath = window.Name
				break
			}
		}

		formatter, err = formatterForFile(filePath)
		if err != nil {
			log.Fatalf("%v", err)
		}
	}

	if *verbose {
		log.Printf("using formatter '%s'\n", strings.Join(formatter, ""))
	}

	reformat(winid, formatter)
}

func formatterForFile(filePath string) ([]string, error) {
	for suffix, formatter := range formatters {
		if strings.HasSuffix(filePath, suffix) {
			return formatter, nil
		}
	}
	return nil, fmt.Errorf("no formatter registered for file '%s'", filePath)
}

func reformat(id int, formatter []string) {
	w, err := acme.Open(id, nil)
	if err != nil {
		log.Printf("Could not open Acme window %d: %v", id, err)
		return
	}
	defer w.CloseFiles()

	old, err := w.ReadAll("body")
	if err != nil {
		log.Printf("Could not read window body: %v", err)
		return
	}

	exe, err := exec.LookPath(formatter[0])
	if err != nil {
		log.Printf("Formatter not installed: %v", err)
		return
	}

	cmd := exec.Command(exe, formatter[1:]...)
	cmd.Stdin = bytes.NewReader(old)
	new, err := cmd.CombinedOutput()
	if err != nil {
		log.Print(string(new))
		return
	}

	if bytes.Equal(old, new) {
		return
	}

	f, err := ioutil.TempFile("", "acmego")
	if err != nil {
		log.Print(err)
		return
	}
	if _, err := f.Write(old); err != nil {
		log.Print(err)
		return
	}
	tmpOld := f.Name()
	f.Close()
	defer os.Remove(tmpOld)

	f, err = ioutil.TempFile("", "acmego")
	if err != nil {
		log.Print(err)
		return
	}
	if _, err := f.Write(new); err != nil {
		log.Print(err)
		return
	}
	tmpNew := f.Name()
	f.Close()
	defer os.Remove(tmpNew)

	diffCmd := exec.Command("9", "diff", tmpOld, tmpNew)
	diffCmd.Stdin = bytes.NewReader(old)
	diff, _ := diffCmd.CombinedOutput()

	latest, err := w.ReadAll("body")
	if err != nil {
		log.Print(err)
		return
	}
	if !bytes.Equal(old, latest) {
		log.Printf("skipped update: window modified since Put\n")
		return
	}

	w.Write("ctl", []byte("mark"))
	w.Write("ctl", []byte("nomark"))
	diffLines := strings.Split(string(diff), "\n")
	for i := len(diffLines) - 1; i >= 0; i-- {
		line := diffLines[i]
		if line == "" {
			continue
		}
		if line[0] == '<' || line[0] == '-' || line[0] == '>' {
			continue
		}
		j := 0
		for j < len(line) && line[j] != 'a' && line[j] != 'c' && line[j] != 'd' {
			j++
		}
		if j >= len(line) {
			log.Printf("cannot parse diff line: %q", line)
			break
		}
		oldStart, oldEnd := parseSpan(line[:j])
		newStart, newEnd := parseSpan(line[j+1:])
		if newStart == 0 || (oldStart == 0 && line[j] != 'a') {
			continue
		}
		switch line[j] {
		case 'a':
			err := w.Addr("%d+#0", oldStart)
			if err != nil {
				log.Print(err)
				break
			}
			w.Write("data", findLines(new, newStart, newEnd))
		case 'c':
			err := w.Addr("%d,%d", oldStart, oldEnd)
			if err != nil {
				log.Print(err)
				break
			}
			w.Write("data", findLines(new, newStart, newEnd))
		case 'd':
			err := w.Addr("%d,%d", oldStart, oldEnd)
			if err != nil {
				log.Print(err)
				break
			}
			w.Write("data", nil)
		}
	}
	if !bytes.HasSuffix(old, nlBytes) && bytes.HasSuffix(new, nlBytes) {
		// plan9port diff doesn't report a difference if there's a mismatch in the
		// final newline, so add one if needed.
		if err := w.Addr("$"); err != nil {
			log.Print(err)
			return
		}
		w.Write("data", nlBytes)
	}
}

var nlBytes = []byte("\n")

func parseSpan(text string) (start, end int) {
	i := strings.Index(text, ",")
	if i < 0 {
		n, err := strconv.Atoi(text)
		if err != nil {
			log.Printf("cannot parse span %q", text)
			return 0, 0
		}
		return n, n
	}
	start, err1 := strconv.Atoi(text[:i])
	end, err2 := strconv.Atoi(text[i+1:])
	if err1 != nil || err2 != nil {
		log.Printf("cannot parse span %q", text)
		return 0, 0
	}
	return start, end
}

func findLines(text []byte, start, end int) []byte {
	i := 0

	start--
	for ; i < len(text) && start > 0; i++ {
		if text[i] == '\n' {
			start--
			end--
		}
	}
	startByte := i
	for ; i < len(text) && end > 0; i++ {
		if text[i] == '\n' {
			end--
		}
	}
	endByte := i
	return text[startByte:endByte]
}
