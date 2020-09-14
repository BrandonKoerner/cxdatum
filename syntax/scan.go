package syntax

import (
	"fmt"
	"io/ioutil"
)

const (
	TOKBEGIN = iota
	IDENT
	LPAREN
	RPAREN
	LBRACK
	RBRACK
	LBRACE
	RBRACE
	DBSELECT
	DTSELECT
	SELECT
	COLON
	SCOLON
	NUMLIT
	STRLIT
	EXACT
	DATUM
	DATABASE
	INT
	ARRAY
	BYTE
	HASH
	WALLET
	STRING
	OF
	TO
	AND
	INC
	DEC
	UPDATE
	SET
	FOR
	IN
	EXPECT
	RETURN
	MATCH
	INGRESS
	EGRESS
	APPEND
	NEW

	EOF
)

type Scanner struct {
	lit  string
	tok  int
	ch   rune
	buf  []byte
	idx  int
	bgn  int
	eof  bool
	l, c int
}

func (s *Scanner) Init(fn string) {
	var err error
	s.buf, err = ioutil.ReadFile(fn)
	if err != nil {
		panic(err.Error())
	}
	fmt.Println(len(s.buf))
	s.l = 1
	s.nextch()
}

func (s *Scanner) nextch() {
	if s.idx == len(s.buf) {
		s.eof = true
		return
	}
	s.ch = rune(s.buf[s.idx])
	s.c++
	if s.ch == '\n' {
		s.l++
		s.c = 0
	}
	s.idx++
}

func (s *Scanner) start() {
	s.bgn = s.idx - 1
}

func (s *Scanner) segment() {
	if s.bgn < 0 {
		panic("scanner.segment error: bgn not set!")
	}
	s.lit = string(s.buf[s.bgn : s.idx-1])
}

/**
 * next() - tries to read a token, ident, or literal
 * into s.lit or s.tok.
 */
func (s *Scanner) Next() {
redo:
	if !s.eof {
		switch s.ch {
		case ' ', '\t', '\n', '\r':
			s.nextch()
			goto redo
		case '/':
			s.nextch()
			if s.ch == '*' {
				s.comment()
				goto redo
			}
		case '{':
			s.nextch()
			s.tok = LBRACE
		case '}':
			s.nextch()
			s.tok = RBRACE
		case '[':
			s.nextch()
			s.tok = LBRACK
		case ']':
			s.nextch()
			s.tok = RBRACK
		case '(':
			s.nextch()
			s.tok = LPAREN
		case ')':
			s.nextch()
			s.tok = RPAREN
		case '@':
			s.nextch()
			s.tok = DBSELECT
		case '$':
			s.nextch()
			s.tok = DTSELECT
		case '.':
			s.nextch()
			s.tok = SELECT
		case ':':
			s.nextch()
			s.tok = COLON
		case ';':
			s.nextch()
			s.tok = SCOLON
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			s.start()
			s.number()
			s.tok = NUMLIT
		case '"':
			s.nextch()
			s.start()
			s.str()
			s.tok = STRLIT
		case 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
			'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a',
			'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
			'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z':
			s.start()
			s.ident()
			/* could be identifier or keyword. so no tok. */
		case '!':
			s.nextch()
			s.tok = EXACT
		default:
			panic(fmt.Sprintf("unexpected character: %v", s.ch))
		}
	} else {
		s.tok = EOF
	}
}

func (s *Scanner) comment() {
comredo:
	for s.ch != '*' && !s.eof {
		s.nextch()
	}
	s.nextch()
	if s.ch != '/' {
		goto comredo
	}
	s.nextch()
}

func (s *Scanner) number() {
	for s.ch == '0' || s.ch == '1' || s.ch == '2' || s.ch == '3' || s.ch == '4' || s.ch == '5' || s.ch == '6' || s.ch == '7' || s.ch == '8' || s.ch == '9' {
		s.nextch()
	}
	s.segment()
}

func (s *Scanner) str() {
	for s.ch != '"' {
		s.nextch()
	}
	s.segment()
	s.nextch()
}

func (s *Scanner) ident() {
	var brk__ bool
	for !s.eof && !brk__ {
		switch s.ch {
		case 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
			'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a',
			'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
			'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z':
			s.nextch()
		default:
			brk__ = true
		}
	}
	s.segment()
	s.tok = IDENT
	s.key()
	s.ensureNotReserved()
}

var keywords map[string]int = map[string]int{
	"datum":    DATUM,
	"database": DATABASE,
	"int":      INT,
	"array":    ARRAY,
	"byte":     BYTE,
	"hash":     HASH,
	"wallet":   WALLET,
	"string":   STRING,
	"of":       OF,
	"to":       TO,
	"and":      AND,
	"inc":      INC,
	"dec":      DEC,
	"update":   UPDATE,
	"set":      SET,
	"for":      FOR,
	"in":       IN,
	"expect":   EXPECT,
	"return":   RETURN,
	"match":    MATCH,
	"ingress":  INGRESS,
	"egress":   EGRESS,
	"append":   APPEND,
	"new":      NEW,
}

func (s *Scanner) key() {
	if tok_, ok := keywords[s.lit]; ok {
		s.lit = ""
		s.tok = tok_
	}
}

func (s *Scanner) ensureNotReserved() {
	switch s.lit {
	case "i", "j", "out", "idx":
		panic("reserved identifier detected!")
	}
}

func tok2str(tok int) string {
	switch tok {
	case TOKBEGIN:
		return "TOKBEGIN"
	case IDENT:
		return "IDENT"
	case LPAREN:
		return "LPAREN"
	case RPAREN:
		return "RPAREN"
	case LBRACK:
		return "LBRACK"
	case RBRACK:
		return "RBRACK"
	case LBRACE:
		return "LBRACE"
	case RBRACE:
		return "RBRACE"
	case DBSELECT:
		return "DBSELECT"
	case DTSELECT:
		return "DTSELECT"
	case SELECT:
		return "SELECT"
	case COLON:
		return "COLON"
	case SCOLON:
		return "SCOLON"
	case NUMLIT:
		return "NUMLIT"
	case STRLIT:
		return "STRLIT"
	case EXACT:
		return "EXACT"
	case DATUM:
		return "DATUM"
	case DATABASE:
		return "DATABASE"
	case INT:
		return "INT"
	case ARRAY:
		return "ARRAY"
	case BYTE:
		return "BYTE"
	case HASH:
		return "HASH"
	case WALLET:
		return "WALLET"
	case STRING:
		return "STRING"
	case OF:
		return "OF"
	case TO:
		return "TO"
	case AND:
		return "AND"
	case INC:
		return "INC"
	case DEC:
		return "DEC"
	case UPDATE:
		return "UPDATE"
	case SET:
		return "SET"
	case FOR:
		return "FOR"
	case IN:
		return "IN"
	case EXPECT:
		return "EXPECT"
	case RETURN:
		return "RETURN"
	case MATCH:
		return "MATCH"
	case INGRESS:
		return "INGRESS"
	case EGRESS:
		return "EGRESS"
	case APPEND:
		return "APPEND"
	case NEW:
		return "NEW"
	default:
		return fmt.Sprintf("INV%d", tok)
	}
}

func Tokenize(fn string) {
	sc := Scanner{}
	sc.Init(fn)
	for !sc.eof {
		sc.Next()
		if sc.tok == NUMLIT || sc.tok == STRLIT || sc.tok == IDENT {
			fmt.Printf("%9s %s\n", tok2str(sc.tok), sc.lit)
		} else {
			fmt.Printf("%9s\n", tok2str(sc.tok))
		}
	}
}
