package syntax

import (
	"fmt"
	"strconv"
)

/**
 * CXDatum AST node types:
 * (Global clauses)
 * Datum clause: has fields, is basically struct.
 * Database clause: has fields, is a hash map.
 * Ingress clause: function with inputs, no outputs.
 * Egress clause: function with outputs, no inputs.
 * (Ingress/Egress clauses)
 * For clause: iterates over array, does action.
 * Expect clause: specifies input/output and order.
 * Match clause: primitive pattern matching.
 * (Statements)
 * Update:
 */

const (
	TKINDUNK = iota
	TKINDINT
	TKINDSTRING
	TKINDHASH
	TKINDBYTE
	TKINDWALLET
	TKINDDATUM
	TKINDDATABASE
)

const (
	GLKINDUNK = iota
	GLKINDDATUM
	GLKINDDATABASE
	GLKINDINGRESS
	GLKINDEGRESS
)

const (
	CKINDUNK = iota
	CKINDFOR
	CKINDMATCH
	CKINDMATCHS
	CKINDEXACT
	CKINDDECL
	CKINDUPDATE
	CKINDCALL
	CKINDEXPR //stuff like X set Y
	CKINDSTORE
	CKINDDEBUG
	CKINDEXIT
)

const (
	EKINDUNK = iota
	EKINDIN
	EKINDTO
	EKINDINC
	EKINDDEC
	EKINDAPPEND
	EKINDSET
	EKINDSELECT
	EKINDLOOKUP
	EKINDSLICE
	EKINDPRIME
	EKINDNEW
)

const (
	UNK_TO_UNK = iota
	ABYTE_TO_BYTE
	BYTE_TO_ABYTE
	STRING_TO_HASH
	STRING_TO_WALLET
	HASH_TO_STRING
	WALLET_TO_STRING
	HASH_TO_ABYTE
	WALLET_TO_ABYTE
	ABYTE_TO_HASH
	ABYTE_TO_WALLET
	HASH_TO_BYTE
	WALLET_TO_BYTE
	BYTE_TO_HASH
	BYTE_TO_WALLET
	STRING_TO_BYTE
	STRING_TO_ABYTE
	BYTE_TO_STRING
	ABYTE_TO_STRING
	BYTE_TO_DATUM
	DATUM_TO_BYTE
)

type DType struct {
	array bool
	asize int
	tkind int
	datum *DGlause //points to datum if datum
	dbase *DGlause //points to database if database
	islit bool     //used by the compiler
}

type DField struct {
	name string
	typ  *DType
}

type DGlause struct {
	name    string
	fields  []*DField
	glkind  int
	clauses []*DClause

	//used if ingress/egress to store all possible $vars.
	syms map[string]*DField
	expt []*DField

	//used for database
	key *DType
	val *DType
	ret []*DField
}

type DClause struct {
	ckind int
	expr  *DExpr
	scls  []*DClause
	exprs []*DExpr
	igrs  *DGlause
	nvar  *DField
}

type DExpr struct {
	ekind int
	mut   bool
	lhs   *DExpr
	rhs   *DExpr
	fld   *DField
	typ   *DType

	/* used for literals, slicing. */
	lit   string
	left  int
	right int

	/* used exclusively by the compiler */
	kname string
}

type Parser struct {
	Scanner

	err error
	//l, c int

	data map[string]*DGlause
	dbs  map[string]*DGlause
	igrs map[string]*DGlause
	egrs map[string]*DGlause

	main *DGlause
}

func ParseFile(fn string) {
	p := Parser{}
	p.l = 1

	p.Init(fn)
	p.parse()
}

func (p *Parser) got(t int) bool {
	if p.tok == t {
		p.Next()
		return true
	}
	return false
}

func (p *Parser) want(t int) {
	if !p.got(t) {
		p.errorf("Expected " + tok2str(t) + "; got " + tok2str(p.tok))
	}
}

func (p *Parser) parse() {
	p.Next()
	p.data = make(map[string]*DGlause)
	p.dbs = make(map[string]*DGlause)
	p.igrs = make(map[string]*DGlause)
	p.egrs = make(map[string]*DGlause)
	for p.tok != EOF {
		switch p.tok {
		case DATUM:
			p.Next()
			datum := p.parseDatum()
			p.data[datum.name] = datum
		case DATABASE:
			p.Next()
			dbase := p.parseDatabase()
			p.dbs[dbase.name] = dbase
		case INGRESS:
			igrs := p.parseGress()
			p.igrs[igrs.name] = igrs
		case EGRESS:
			egrs := p.parseGress()
			p.egrs[egrs.name] = egrs
		default:
			p.errorf("unexpected " + tok2str(p.tok))
		}
	}
}

func (p *Parser) errorf(msg string) {
	panic(fmt.Sprintf("[%d:%d] ", p.l, p.c) + msg)
}

func (p *Parser) parseDatum() *DGlause {
	datum := new(DGlause)

	datum.glkind = GLKINDDATUM

	datum.name = p.name()

	p.want(LBRACE)

	for !p.got(RBRACE) {
		datum.fields = append(datum.fields, p.parseField())
	}
	datum.syms = make(map[string]*DField)
	for _, v := range datum.fields {
		datum.syms[v.name] = v
	}

	return datum
}

func (p *Parser) parseDatabase() *DGlause {
	dbase := new(DGlause)
	dbase.glkind = GLKINDDATABASE

	dbase.name = p.name()

	p.want(LBRACE)

	for !p.got(RBRACE) {
		dbase.key = p.parseType()
		p.want(COLON)
		dbase.val = p.parseType()
		p.want(SCOLON)
		//dbase.fields = append(dbase.fields, p.parseField())
	}

	return dbase
}

func (p *Parser) parseGress() *DGlause {
	gress := new(DGlause)
	gress.syms = make(map[string]*DField)
	switch p.tok {
	case INGRESS:
		gress.glkind = GLKINDINGRESS
	case EGRESS:
		gress.glkind = GLKINDEGRESS
	default:
		p.errorf("Expected INGRESS or EGRESS; got " + tok2str(p.tok))
	}
	p.Next()
	gress.name = p.name()
	if gress.glkind == GLKINDEGRESS {
		/* egress uses "out" as return value name */
		rvfld := &DField{name: "out", typ: &DType{array: true, tkind: TKINDBYTE}}
		gress.syms["out"] = rvfld
	}
	if gress.name == "main" {
		if gress.glkind == GLKINDEGRESS {
			panic("main cannot be EGRESS")
		}
		p.main = gress
	}
	p.want(LBRACE)
	for !p.got(RBRACE) {
		clause := p.parseClause(gress)
		if clause != nil {
			gress.clauses = append(gress.clauses, clause)
		}
	}
	if gress.glkind == GLKINDEGRESS && gress.ret == nil {
		panic("egress " + gress.name + " has no return specified!")
	}
	return gress
}

func (p *Parser) parseClause(g *DGlause) *DClause {
	switch p.tok {
	case EXPECT:
		if g.expt != nil {
			p.errorf("ingress/egress " + g.name + " already has expect")
		}
		p.Next()
		expect := p.parseExpect()
		for _, v := range expect {
			g.syms[v.name] = v
		}
		g.expt = expect
		return nil
	case RETURN:
		if g.ret != nil {
			p.errorf("egress " + g.name + " already has return")
		}
		if g.glkind == GLKINDINGRESS {
			p.errorf("ingress cannot specify return!")
		}
		p.Next()
		g.ret = p.parseExpect()
		for _, v := range g.ret {
			g.syms[v.name] = v
		}
		return nil
	case EXACT:
		/* indicates data is to be sent as new blockchain object */
		clause := new(DClause)
		clause.ckind = CKINDEXACT
		p.Next()
		clause.expr = p.parseExpr(g, false)
		if dtypeToString(clause.expr.typ) != "array of byte" {
			panic("cannot use exact statement on expression of type " + dtypeToString(clause.expr.typ))
		}
		p.want(SCOLON)
		return clause
	case FOR:
		clause := new(DClause)
		clause.ckind = CKINDFOR
		p.Next()
		clause.expr = p.parseExprIn(g)
		p.want(LBRACE)
		for !p.got(RBRACE) {
			clause.scls = append(clause.scls, p.parseClause(g))
		}
		return clause
	case MATCH:
		clause := new(DClause)
		clause.ckind = CKINDMATCH
		p.Next()
		clause.expr = p.parseExpr(g, false)
		if dtypeToString(clause.expr.typ) != "string" && dtypeToString(clause.expr.typ) != "int" {
			panic("error: match clauses only work currently with string and int ; not " + dtypeToString(clause.expr.typ))
		}
		p.want(LBRACE)
		for !p.got(RBRACE) {
			clause.scls = append(clause.scls, p.parseMatch(g, clause.expr.typ))
		}
		return clause
	case UPDATE:
		clause := new(DClause)
		clause.ckind = CKINDUPDATE
		p.Next()
		clause.expr = p.parseExpr(g, true)
		p.want(SCOLON)
		return clause
	case DEBUG:
		clause := new(DClause)
		clause.ckind = CKINDDEBUG
		p.Next()
		clause.expr = p.parseExpr(g, false)
		p.want(SCOLON)
		if dtypeToString(clause.expr.typ) != "string" {
			p.errorf("debug expected expression of type string; got" + dtypeToString(clause.expr.typ))
		}
		return clause
	case STORE:
		clause := new(DClause)
		clause.ckind = CKINDSTORE
		p.Next()
		clause.exprs = append(clause.exprs, p.parseExpr(g, false))
		dbtyp := clause.exprs[0].typ
		if dbtyp.tkind != TKINDDATABASE {
			p.errorf("store expected database")
		}
		p.want(KEY)
		clause.exprs = append(clause.exprs, p.parseExpr(g, false))
		p.mustCheckTypeEqual(clause.exprs[1].typ, dbtyp.dbase.key)
		p.want(VALUE)
		clause.exprs = append(clause.exprs, p.parseExpr(g, false))
		p.mustCheckTypeEqual(clause.exprs[2].typ, dbtyp.dbase.val)
		p.want(SCOLON)
		return clause
	case EXIT:
		p.Next()
		p.want(SCOLON)
		return &DClause{ckind: CKINDEXIT}
	case IDENT:
		clause := new(DClause)
		nm := p.name()
		if p.got(COLON) {
			clause.expr = p.parseExpr(g, false)
			clause.ckind = CKINDDECL
			clause.nvar = &DField{name: nm, typ: clause.expr.typ}
			if _, ok := g.syms[nm]; ok {
				p.errorf(nm + " redeclared!")
			}
			g.syms[nm] = clause.nvar
			p.want(SCOLON)
			return clause
		} else {
			p.want(LPAREN)
			clause.igrs = p.igrs[nm]
			if clause.igrs == nil {
				p.errorf("no ingress with name: " + nm)
			}
			clause.ckind = CKINDCALL
			var i int = 0
			for !p.got(RPAREN) {
				/* each argument is a thing. */
				if i >= len(clause.igrs.expt) {
					p.errorf(fmt.Sprintf("ingress call received at least %d arguments, but expected %d", i+1, len(clause.igrs.expt)))
				}
				clause.exprs = append(clause.exprs, p.parseExpr(g, false))
				p.mustCheckTypeEqual(clause.exprs[i].typ, clause.igrs.expt[i].typ)
				i++
				if p.tok != RPAREN {
					p.want(AND)
				}
			}
			p.want(SCOLON)
		}
		return clause
	default:
		clause := new(DClause)
		clause.ckind = CKINDEXPR
		clause.expr = p.parseExpr(g, true)
		if !clause.expr.mut {
			p.errorf("expression does nothing!")
		}
		p.want(SCOLON)
		return clause
	}
}

func (p *Parser) mustCheckTypeEqual(t1 *DType, t2 *DType) {
	if t1.array != t2.array || t1.asize != t2.asize || t1.datum != t2.datum || t1.dbase != t2.dbase || t1.tkind != t2.tkind {
		p.errorf(fmt.Sprintf("invalid type conversion: (%s) to (%s)", dtypeToString(t1), dtypeToString(t2)))
	}
}

func (p *Parser) parseMatch(g *DGlause, typ *DType) *DClause {
	clause := new(DClause)
	clause.ckind = CKINDMATCHS
	clause.expr = p.parseExpr(g, false)
	if clause.expr.typ.tkind != typ.tkind || clause.expr.typ.datum != typ.datum || clause.expr.typ.dbase != typ.dbase || clause.expr.typ.array != typ.array || clause.expr.typ.asize != typ.asize {
		p.errorf(fmt.Sprintf("cannot perform matching on types %s and %s", dtypeToString(clause.expr.typ), dtypeToString(typ)))
	}
	p.want(LBRACE)
	for !p.got(RBRACE) {
		clause.scls = append(clause.scls, p.parseClause(g))
	}
	return clause
}

func (p *Parser) parseExprIn(g *DGlause) *DExpr {
	elt := new(DField)
	expr := new(DExpr)
	elt.name = p.name()
	p.want(IN)
	rhs := p.parseExpr(g, false)
	if rhs.typ == nil || !rhs.typ.array {
		p.errorf("RHS of IN is not typed or not array type!")
	}
	elt.typ = &DType{
		tkind: rhs.typ.tkind,
		datum: rhs.typ.datum,
		dbase: rhs.typ.dbase,
	}
	expr.rhs = rhs
	expr.fld = elt
	if _, ok := g.syms[elt.name]; ok {
		p.errorf("identifier already in use: " + elt.name)
	}
	g.syms[elt.name] = elt
	expr.ekind = EKINDIN
	expr.mut = false
	expr.typ = elt.typ //only for debug purposes

	return expr
}

func (p *Parser) parseExpr(g *DGlause, mut bool) *DExpr {
	/* expression cannot contain inc, dec, set, or append. */
	/* these are the top level expressions. */
	/* OOP: AND, */
	expr := new(DExpr)
	switch p.tok {
	case DTSELECT, DBSELECT:
		p.Next()
		/* must be IDENT */
		expr.ekind = EKINDPRIME
		nm := p.name()
		expr.fld = g.syms[nm]
		if expr.fld == nil {
			/* it could still be a database */
			dbq := p.dbs[nm]
			if dbq == nil {
				p.errorf("no variable found with identifier $" + nm)
			}
			expr.fld = &DField{name: nm, typ: &DType{tkind: TKINDDATABASE, dbase: dbq}}
		}
		expr.typ = expr.fld.typ
		return p.parseExprPost(g, expr, mut)
	case NEW:
		/* expects IDENT (name of thing) */
		p.Next()
		nm := p.name()
		if v, ok := p.data[nm]; ok {
			expr.typ = &DType{tkind: TKINDDATUM, datum: v}
			expr.ekind = EKINDNEW
		}
		return expr
	case NUMLIT:
		num := p.parseNum()
		expr.typ = &DType{tkind: TKINDINT, islit: true}
		expr.left = num
		expr.right = -1
		expr.ekind = EKINDPRIME
		return p.parseExprPost(g, expr, mut)
	case STRLIT:
		expr.typ = &DType{tkind: TKINDSTRING, islit: true}
		expr.lit = p.lit
		expr.ekind = EKINDPRIME
		p.Next()
		return p.parseExprPost(g, expr, mut)
	default:
		p.errorf("unexpected " + tok2str(p.tok))
	}
	panic("unreachable")
}

func (p *Parser) parseExprPost(g *DGlause, lhs *DExpr, mut bool) *DExpr {
	expr := new(DExpr)
	expr.lhs = lhs
	switch p.tok {
	case LBRACK:
		/* next tok is either an immutable expression or slice params */
		if lhs.typ.tkind == TKINDDATABASE {
			/* expect expression */
			p.Next()
			expr.rhs = p.parseExpr(g, false)
			expr.ekind = EKINDLOOKUP
			expr.typ = lhs.typ.dbase.val
			expr.mut = false
		} else {
			p.Next()
			expr.left = p.parseNum()
			p.want(COLON)
			expr.right = p.parseNum()
			expr.ekind = EKINDSLICE
			expr.mut = false
			/* check type and bounds */
			if expr.right <= expr.left || expr.left < 0 || expr.right < 1 {
				p.errorf(fmt.Sprintf("slice expression bounds are invalid: %d : %d", expr.left, expr.right))
			}
			if !lhs.typ.array {
				p.errorf("Slice expression on non-array type!")
			}
			if lhs.typ.asize > 0 && lhs.typ.asize < expr.right {
				p.errorf(fmt.Sprintf("Slice expression on fixed array goes out of bounds! Array size: %d; slice bounds: %d : %d", lhs.typ.asize, expr.left, expr.right))
			}
			expr.typ = &DType{
				tkind: lhs.typ.tkind,
				datum: lhs.typ.datum,
				dbase: lhs.typ.dbase,
				array: true,
			}
		}
		p.want(RBRACK)
		return p.parseExprPost(g, expr, mut)
	case TO:
		/* Type conversions can be very annoying. */
		/* lhs has restrictions based on type. */
		p.Next()
		expr.typ = p.parseType()
		p.mustCheckTypePair(lhs.typ, expr.typ)
		expr.left = getTypeToCode(lhs.typ, expr.typ)
		expr.ekind = EKINDTO
		expr.mut = false
		return p.parseExprPost(g, expr, mut)
	case SELECT:
		if lhs.typ.tkind != TKINDDATUM {
			p.errorf("Tried to select a non-datum type!")
		}
		p.Next()
		nm := p.name()
		if v, ok := lhs.typ.datum.syms[nm]; ok {
			expr.fld = v
			expr.typ = v.typ
			expr.mut = false
			expr.ekind = EKINDSELECT
		} else {
			p.errorf("Datum " + lhs.typ.datum.name + " has no field " + nm)
		}
		return p.parseExprPost(g, expr, mut)
	case APPEND:
		if !mut {
			p.errorf("cannot perform append here!")
		}
		/* operates on arrays of non-fixed size only */
		p.Next()
		expr.rhs = p.parseExpr(g, false)
		expr.ekind = EKINDAPPEND
		expr.mut = true
		if expr.rhs.typ.tkind != lhs.typ.tkind || expr.rhs.typ.array || !lhs.typ.array || lhs.typ.asize > 0 {
			p.errorf("invalid append!")
		}
		return expr
	case INC, DEC:
		if !mut {
			p.errorf("cannot perform inc/dec here!")
		}
		if lhs.typ.tkind != TKINDINT || lhs.typ.array {
			p.errorf("cannot perform inc/dec on this type!")
		}
		expr.mut = true
		if p.tok == INC {
			expr.ekind = EKINDINC
		} else {
			expr.ekind = EKINDDEC
		}
		p.Next()
		return expr
	case SET:
		if !mut {
			p.errorf("cannot perform set here!")
		}
		expr.mut = true
		expr.ekind = EKINDSET
		p.Next()
		expr.rhs = p.parseExpr(g, false)
		return expr
	}

	return lhs
}

func getTypeToCode(from *DType, to *DType) int {
	switch {
	case dtypeToString(to) == "array of byte" && from.array && from.asize > 0 && from.tkind == TKINDBYTE:
		return ABYTE_TO_BYTE
	case dtypeToString(from) == "array of byte" && to.array && to.asize > 0 && to.tkind == TKINDBYTE:
		return BYTE_TO_ABYTE
	case dtypeToString(from) == "string" && dtypeToString(to) == "hash":
		return STRING_TO_HASH
	case dtypeToString(from) == "string" && dtypeToString(to) == "wallet":
		return STRING_TO_WALLET
	case dtypeToString(to) == "string" && dtypeToString(from) == "hash":
		return HASH_TO_STRING
	case dtypeToString(to) == "string" && dtypeToString(from) == "wallet":
		return WALLET_TO_STRING
	case dtypeToString(from) == "hash" && to.array && to.asize > 0 && to.tkind == TKINDBYTE:
		return HASH_TO_ABYTE
	case dtypeToString(from) == "wallet" && to.array && to.asize > 0 && to.tkind == TKINDBYTE:
		return WALLET_TO_ABYTE
	case dtypeToString(to) == "hash" && from.array && from.asize > 0 && from.tkind == TKINDBYTE:
		return ABYTE_TO_HASH
	case dtypeToString(to) == "wallet" && from.array && from.asize > 0 && from.tkind == TKINDBYTE:
		return ABYTE_TO_WALLET
	case dtypeToString(from) == "hash" && to.array && to.asize == 0 && to.tkind == TKINDBYTE:
		return HASH_TO_BYTE
	case dtypeToString(from) == "wallet" && to.array && to.asize == 0 && to.tkind == TKINDBYTE:
		return WALLET_TO_BYTE
	case dtypeToString(to) == "hash" && from.array && from.asize == 0 && from.tkind == TKINDBYTE:
		return BYTE_TO_HASH
	case dtypeToString(to) == "wallet" && from.array && from.asize == 0 && from.tkind == TKINDBYTE:
		return BYTE_TO_WALLET
	case dtypeToString(from) == "string" && to.array && to.asize == 0 && to.tkind == TKINDBYTE:
		return STRING_TO_BYTE
	case dtypeToString(from) == "string" && to.array && to.asize > 0 && to.tkind == TKINDBYTE:
		return STRING_TO_ABYTE
	case dtypeToString(to) == "string" && from.array && from.asize == 0 && from.tkind == TKINDBYTE:
		return BYTE_TO_STRING
	case dtypeToString(to) == "string" && from.array && from.asize > 0 && from.tkind == TKINDBYTE:
		return ABYTE_TO_STRING
	case to.tkind == TKINDDATUM && dtypeToString(from) == "array of byte":
		return BYTE_TO_DATUM
	case from.tkind == TKINDDATUM && dtypeToString(to) == "array of byte":
		return DATUM_TO_BYTE
	}

	return UNK_TO_UNK
}

func dtypeToString(t *DType) string {
	var rv string
	switch t.tkind {
	case TKINDBYTE:
		rv = "byte"
	case TKINDDATABASE:
		rv = "@" + t.dbase.name
	case TKINDDATUM:
		rv = "$" + t.datum.name
	case TKINDHASH:
		rv = "hash"
	case TKINDINT:
		rv = "int"
	case TKINDSTRING:
		rv = "string"
	case TKINDWALLET:
		rv = "wallet"
	default:
		rv = "<UNK>"
	}
	if t.array {
		if t.asize > 0 {
			rv = fmt.Sprintf("array [%d] of %s", t.asize, rv)
		} else {
			rv = "array of " + rv
		}
	}

	return rv
}

func (p *Parser) mustCheckTypePair(from *DType, to *DType) {
	if !checkTypePair(from, to) {
		p.errorf(fmt.Sprintf("invalid type conversion: (%s) to (%s)", dtypeToString(from), dtypeToString(to)))
	}
}

func checkTypePair(from *DType, to *DType) bool {
	switch from.tkind {
	case TKINDBYTE:
		if !from.array {
			return false
		}
		if from.asize == 0 {
			switch to.tkind {
			case TKINDSTRING, TKINDDATUM, TKINDHASH, TKINDWALLET:
				return true
			case TKINDBYTE:
				if to.array && to.asize > 0 {
					return true
				}
				return false
			default:
				return false
			}
		} else if from.asize == 32 && to.tkind == TKINDHASH && !to.array {
			return true
		} else if from.asize == 25 && to.tkind == TKINDWALLET && !to.array {
			return true
		} else if to.tkind == TKINDBYTE && to.array && to.asize == 0 {
			return true
		} else if to.tkind == TKINDSTRING && !to.array {
			return true
		}
		return false
	case TKINDWALLET, TKINDHASH, TKINDSTRING:
		if from.array || from.tkind == to.tkind {
			return false
		} else if !to.array && to.tkind == TKINDSTRING {
			return true
		} else if to.array && to.tkind == TKINDBYTE {
			return true
		} else if from.tkind == TKINDSTRING && !to.array && (to.tkind == TKINDHASH || to.tkind == TKINDWALLET) {
			return true
		}
		return false
	case TKINDDATUM:
		return to.array && to.asize == 0 && to.tkind == TKINDBYTE
	}
	return false
}

func (p *Parser) parseExpect() []*DField {
	expt := []*DField{}
	p.want(LBRACE)
	for !p.got(RBRACE) {
		expt = append(expt, p.parseField())
	}
	return expt
}

func (p *Parser) parseField() *DField {
	fld := new(DField)
	fld.name = p.name()
	p.want(COLON)
	fld.typ = p.parseType()
	p.want(SCOLON)

	return fld
}

func (p *Parser) parseType() *DType {
	switch p.tok {
	case INT:
		p.Next()
		return &DType{tkind: TKINDINT}
	case STRING:
		p.Next()
		return &DType{tkind: TKINDSTRING}
	case WALLET:
		p.Next()
		return &DType{tkind: TKINDWALLET}
	case HASH:
		p.Next()
		return &DType{tkind: TKINDHASH}
	case BYTE:
		p.Next()
		return &DType{tkind: TKINDBYTE}
	case IDENT:
		/* MUST be a datum */
		if datum, ok := p.data[p.lit]; ok {
			dtyp := new(DType)
			dtyp.datum = datum
			dtyp.tkind = TKINDDATUM
			p.Next()
			return dtyp
		}
		p.errorf("Datum not found! : " + p.lit)
	case ARRAY:
		dtyp := new(DType)
		dtyp.array = true
		p.Next()
		if p.tok == LBRACK {
			p.Next()
			dtyp.asize = p.parseNum()
			p.want(RBRACK)
		}
		p.want(OF)
		typk := p.parseType()
		if typk.array {
			p.errorf("Cannot have array of array")
		}
		dtyp.tkind = typk.tkind
		return dtyp
	default:
		p.errorf("unknown type beginning with " + tok2str(p.tok))
	}
	panic("unreachable")
}

func (p *Parser) parseNum() int {
	if p.tok != NUMLIT {
		p.errorf("expected NUMLIT; got " + tok2str(p.tok))
	}
	aint, err := strconv.ParseInt(p.lit, 10, 32)
	if err != nil {
		p.errorf(err.Error())
	}
	p.Next() //], :
	return int(aint)
}

func (p *Parser) name() string {
	if p.tok == IDENT {
		rv := p.lit
		p.Next()
		return rv
	}

	p.errorf("Expected IDENT; got " + tok2str(p.tok))
	panic("unreachable")
}
