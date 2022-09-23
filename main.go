package main

import (
	"math/rand"
	"time"
	"fmt"
	"os"
)
/**
	* literals are not enunciated here we will fuzz them in an unstructed way

	* psuedo code:
		input: binary string of digits
		output: random flux program
			process:
				- if a binary number is 1/0 choose either branch (production rule) in the grammar, recurse.
				- if the seed is fully consumed choose the closest terminator
	* we consume a binary number for each element in the grammar, should we run out of numbers
		we then choose the closes terminator or string literal (might need some work discovering this)
	* should there be an automated way to convert a grammar into a normal form that allows for binary disovery?
	
**/

//var keywords := []string{"and","import","not","return","option","test","empty","in","or","package"}

type FluxFile struct {
	pkg PackageClause_t
	imps ImportList_t
	stmts StatementList_t
}

type PackageClause_t struct {
	op string
	expr Identifier_t
}
type StatementList_t struct {
	op []Statement_t
}
type Statement_t struct {
	ops OptionAssignment_t
	builts BuiltinStatement_t
	vars VariableAssignment_t
	ret ReturnStatement_t
	expr Expression_t
}

type ReturnStatement_t struct {
	//ReturnStatement = "return" Expression .
	op Expression_t
}
type VariableAssignment_t struct {
	op Identifier_t
	expr Expression_t
}
type OptionAssignment_t struct {
	//OptionAssignment = "option" [ identifier "." ] identifier "=" Expression
	op Identifier_t
	expr Expression_t 
}

type BuiltinStatement_t struct {
	op Identifier_t
	expr TypeExpression_t
}

type TypeExpression_t struct {
	op MonoType_t //remember "where" string
	expr ConstraintList_t
}

type ConstraintList_t struct {
	op []Constraint_t
}

type Constraint_t struct {
	op Tvar_t
	expr Kinds_t
}

type Kinds_t struct {
	op []Identifier_t
}

type MonoType_t struct {
	tvar Tvar_t
	basic BasicType_t
	array ArrayType_t
	stream StreamType_t
	vec VectorType_t
	rec RecordType_t
	fun FunctionType_t
}

type Tvar_t struct {
	op string
}

type BasicType_t struct {
	op string //"int" | "uint" | "float" | "string" | "bool" | "time" | "duration" | "bytes" | "regexp"
}

type ArrayType_t struct {
	op *MonoType_t //"[" MonoType "]" .
}

type StreamType_t struct {
	op *MonoType_t //"stream" "[" MonoType "]" .
}

type VectorType_t struct {
	op *MonoType_t //"vector" "[" MonoType "]" .
}

type RecordType_t struct {
	op LHSRecordType_t
	expr RHSRecordType_t
}

type LHSRecordType_t struct {
	op RecordTypeProperties_t  //"{" [RecordTypeProperties] "}" 
}

type RecordTypeProperties_t struct {
	op []RecordTypeProperty_t
}


type RHSRecordType_t struct {
	op Tvar_t//( "{" Tvar "with" RecordTypeProperties "}" ) .
	expr RecordTypePropertyList_t
}
type RecordTypePropertyList_t struct {
	op []RecordTypeProperty_t
}

type RecordTypeProperty_t struct {
	//RecordTypeProperty   = Label ":" MonoType .
	op Label_t
	expr *MonoType_t
}

type Label_t struct {
	op Identifier_t
	expr string //string literal
}

type RetunStatement_t struct {
	op string //"return" string
	expr Expression_t
}

type VariableStatement_t struct {
	op Identifier_t
	expr Expression_t
}
type ImportList_t struct {
	op ImportDeclaration_t
}

type ImportDeclaration_t struct {

	op string //string literal that follows the identifier
	expr Identifier_t
}
type FunctionType_t struct {
	//FunctionType = "(" [FunctionTypeParameters] ")" "=>" MonoType .
	op FunctionTypeParameterList_t
	expr *MonoType_t
}

type FunctionTypeParameterList_t struct {
	op []FunctionTypeParameter_t
}

type FunctionTypeParameter_t struct{
	//FunctionTypeParameter_t = [ "<-" | "?" ] identifier ":" MonoType .
	op Identifier_t
	expr *MonoType_t
}

type Production_t struct {
	op string
	expr Expression_t
}

type Expression_t struct {
	op *Alternative_t
	expr *ConditionalExpression_t
}

type ConditionalExpression_t struct {
	op LogicalExpression_t
	expr ConditionalExpressionTruple_t
}

type ConditionalExpressionTruple_t struct {

	op Expression_t //if 
	expr SubConditionalExpressionTruple_t //then
}

type SubConditionalExpressionTruple_t struct {
	op Expression_t //then
	expr Expression_t //else
}

type LogicalExpression_t struct {
	op UnaryLogicalExpression_t
	expr BinaryLogicalExpression_t
}

type BinaryLogicalExpression_t struct {
	lhs *LogicalExpression_t
	op LogicalOperator_t
	rhs *UnaryLogicalExpression_t
}

type UnaryLogicalExpression_t struct {
	op ComparisonExpression_t
	expr *UnaryLogicalTruple_t
}

type UnaryLogicalTruple_t struct {
	op UnaryLogicalOperator_t
	expr *UnaryLogicalExpression_t
}

type ComparisonExpression_t struct {
	op *MultiplicativeExpression_t
	expr ComparisonTruple_t
}

type ComparisonTruple_t struct {
	lhs *ComparisonExpression_t
	op ComparisonOperator_t
	rhs *AdditiveExpression_t
}

type AdditiveExpression_t struct {
	op *MultiplicativeExpression_t
	expr AdditiveExpressionTruple_t
}

type AdditiveExpressionTruple_t struct {
	lhs *AdditiveExpression_t
	op AdditiveOperator_t
	rhs *MultiplicativeExpression_t
}

type MultiplicativeExression_t struct {
	op *ExponentExpression_t
	expr *MultiplicativeExpressionTruple_t
}

type MultiplicativeExpressionTruple_t struct {
	lhs *ExponentExpression_t
	op *MultiplicativeOperator_t
	rhs *MultiplicativeExpression_t
}

type MultiplicativeExpression_t struct {
	op ExponentExpression_t
	expr MultiplicativeExpressionTruple_t
}

type ExponentExpression_t struct {
	op PipeExpression_t
	expr ExponentExpressionTruple_t
}
type ExponentExpressionTruple_t struct {
	lhs *ExponentExpression_t
	op ExponentOperator_t
	rhs *PipeExpression_t
}

type PipeExpression_t struct {
	op PostfixExpression_t
	expr *PipeExpressionTruple_t
}

type PipeExpressionTruple_t struct {
	lhs *PipeExpression_t
	op PipeOperator_t
	rhs *UnaryExpression_t
}

type UnaryExpression_t struct {
	op PostfixExpression_t
	expr UnaryExpressionTruple_t
}

type UnaryExpressionTruple_t struct {
	op PrefixOperator_t
	expr *UnaryExpression_t
}

type PrefixOperator_t struct {
	op string
}

type PostfixExpression_t struct {
	op PrimaryExpression_t
	expr PostfixExpressionTruple_t
}

type PrimaryExpression_t struct {
	op Identifier_t //should terminate in a literal soon
	expr PrimaryExpressionTruple_t
}

type PrimaryExpressionTruple_t struct {
	op Identifier_t
	expr Expression_t
}

type PostfixExpressionTruple_t struct {
	op *PostfixExpression_t
	expr PostfixOperator_t
}

type PostfixOperator_t struct {
	op MemberExpression_t
	expr PostfixOperatorTruple_t
}

type PostfixOperatorTruple_t struct {
	op CallExpression_t
	expr IndexExpression_t
}

type MemberExpression_t struct {
	op DotExpression_t
	expr MemberBracketExpression_t
}
type DotExpression_t struct { //prefix with a "."
	expr Identifier_t
}
type MemberBracketExpression_t struct {
	op string
}

type CallExpression_t struct {
	op PropertyListExpression_t
}

type PropertyListExpression_t struct {
	op []Property_t //needs to be an array of properties
	length int
}

type Property_t struct {
	op LHSProperty_t
	expr RHSProperty_t
}

type LHSProperty_t struct {
	op Identifier_t
	expr Expression_t
}

type RHSProperty_t struct {
	op string
	expr Expression_t
}

type IndexExpression_t struct {
	op Expression_t //remember brackets
}

//type ComparisonOperator_t []string
type ComparisonOperator_t struct {
	op string
}

//type PipeOperator_t []string //{"|>"}
type PipeOperator_t struct {
	op string
}


//type LogicalOperator_t []string //{"and","or"}
type LogicalOperator_t struct {
	op string
}

type UnaryLogicalOperator_t struct {
	op string
}

type AdditiveOperator_t struct {
	op string
}

type MultiplicativeOperator_t struct {
	op string
}

type ExponentOperator_t struct {
	op string
}

type Alternative_t struct {
	term Term_t
}

type Term_t struct {
	op string
	expr TermTruple_t
}

type TermTruple_t struct {
	op Token_t
	expr SubTermTruple_t
}

type SubTermTruple_t struct {
	op Group_t
	expr SubSubTermTruple_t
}
type Group_t struct {
	op Expression_t
}

type SubSubTermTruple_t struct {
	op Option_t
	expr Repetition_t
}
type Option_t struct {
	op Expression_t
}

type Repetition_t struct {
	op Expression_t
}

type Token_t struct {
	op Identifier_t
	expr SubTokenTruple_t
}

type SubTokenTruple_t struct {
	op Keyword_t
	expr SubSubTokenTruple_t
}

type SubSubTokenTruple_t struct {
	op Operator_t
	expr Literal_t
}

type Identifier_t struct {
	op string //letter
	expr string
}

type Keyword_t struct {
	op string
}
type Operator_t struct {
	op string
}
type Literal_t struct {
	int_lit int
	float_lit float64
	string_lit string
	regex_lit RegexLiteral_t
	duration_lit DurationLiteral_t
	piperec_lit PipeRecieveLiteral_t
	rec_lit RecordLiteral_t
	array_lit ArrayLiteral_t
	dict_lit DictLiteral_t
	func_lit FunctionLiteral_t
}
type FunctionLiteral_t struct {
	//FunctionLiteral    = FunctionParameters "=>" FunctionBody .
	op FunctionParameters_t
	expr FunctionBody_t

}
type FunctionBody_t struct {
	op Expression_t
	expr Block_t
}
type Block_t struct {
	op []Statement_t
}
type FunctionParameters_t struct {
	op []Parameter_t
}
type Parameter_t struct {
	op Identifier_t
	expr Expression_t
}
type DictLiteral_t struct {
	op EmptyDict_t
	expr AssociativeList_t
}
type EmptyDict_t struct {
	op string
}
type AssociativeList_t struct {
	op Association_t
}
type Association_t struct {
	op []ExpressionList_t
}
type ArrayLiteral_t struct {
	op ExpressionList_t
}

type ExpressionList_t struct {
	op []Expression_t
}
type RegexLiteral_t struct {
	//regexp_lit         = "/" regexp_char { regexp_char } "/" .
	regex_char RegexChar_t
}

type RegexChar_t struct {
	uni UnicodeChar_t
	bv rune
}

type DurationLiteral_t struct {
	op DurationMagnitude_t
	expr DurationUnit_t
}
type DurationUnit_t struct {
	op rune
}

type PipeRecieveLiteral_t struct {
	op string //<-
}
type RecordLiteral_t struct {
	//RecordLiteral  = "{" RecordBody "}" .
	op RecordBody_t
}
type RecordBody_t struct {
	op WithProperties_t
	expr PropertyList_t
}
type WithProperties_t struct {
	//WithProperties = identifier "with"  PropertyList .
	op Identifier_t
	expr PropertyList_t
}

type PropertyList_t struct {
	op LHSPropertyList_t
	expr RHSPropertyList_t
}

type LHSPropertyList_t struct {
	//identifier [ ":" Expression ]
	op Identifier_t
	expr Expression_t
}

type RHSPropertyList_t struct {
	//string_lit ":" Expression .
	op string
	expr Expression_t
}

type UnicodeChar_t struct {
	op rune
}

type DurationMagnitude_t struct {
	op []int //
	expr string //"y" | "mo" | "w" | "d" | "h" | "m" | "s" | "ms" | "us" | "µs" | "ns"
}

func init_random() {
	rand.Seed(time.Now().UnixNano())
}
func generate_string() string {
	return "string\n" //TODO: need to replace this with a string generator
}

func (lit Literal_t) generate(seed string, index int) string {
	return "literal\n" //TODO: fix once we get everything working
}

func (memb MemberBracketExpression_t) generate(seed string, index int) string{
	return "[" + generate_string() + "]"
}

func (i Identifier_t) generate(seed string, index int) string{
	return "identifier\n" //replace with random string when this works
}

func (dot DotExpression_t) generate(seed string, index int) string{
	i := Identifier_t {}
	return "." + i.generate(seed,index)
}

func (mem MemberExpression_t) generate(seed string, index int) string {
	if index < len(seed){
		switch {
			case '0' == seed[index]:
				d := DotExpression_t {}
				return d.generate(seed,index+1)
			case '1' == seed[index]:
				m := MemberBracketExpression_t {}
				return m.generate(seed,index+1)
		}
	}
	return "member_expression\n" //should never return
}

func (post PostfixOperatorTruple_t) generate(seed string, index int) string{
	if index < len(seed){
		switch {
			case '0' == seed[index]:
				c := CallExpression_t {}
				return c.generate(seed,index+1)
			case '1' == seed[index]:
				i := IndexExpression_t {}
				return i.generate(seed,index+1)
		}
	}
	return "postfix_op_truple\n" //should never return
}
func (call CallExpression_t) generate(seed string, index int) string {
	p := PropertyListExpression_t {}
	return "(" + p.generate(seed,index) + ")"
}

func (prop PropertyListExpression_t) generate(seed string, index int) string {
	var out string = "";
	for i := 0; i < 10; i++ { //need to randomize the propertylist
		p := Property_t{}
		out += p.generate(seed,index) + ","
	}
	return out
}
func (prop Property_t) generate(seed string, index int) string {
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				l := LHSProperty_t {}
				return l.generate(seed,index+1)
			case '1' == seed[index]:
				r := RHSProperty_t {}
				return r.generate(seed,index+1)
		}
	}
	return "[ prop,prop ]\n" //need to handle base case
}
func (rhs RHSProperty_t) generate(seed string, index int) string{
	i := Identifier_t {}
	e := Expression_t {}
	return i.generate(seed,index) + ":" + e.generate(seed,index)
}

func (lhs LHSProperty_t) generate(seed string, index int) string {
	e := Expression_t {}
	return "prop " + generate_string() + ":" + e.generate(seed,index)
}

func (iex IndexExpression_t) generate(seed string, index int) string{
	e := Expression_t {}
	return "[" + e.generate(seed,index) + "]\n"
}


func (comp ComparisonOperator_t) generate(seed string, index int) string {
	if index < len(seed) {
		if seed[index] == '0' {
			return "<"
		}
		return ">" //need to represent the other comparison operators
	}
	return "comp\n"
}
func (comp ComparisonTruple_t) generate(seed string, index int) string {
	lhs := ComparisonExpression_t {}
	op := ComparisonOperator_t {}
	rhs := AdditiveExpression_t {}
	return lhs.generate(seed,index) + op.generate(seed,index) + rhs.generate(seed,index)
}

func (add AdditiveExpression_t) generate(seed string, index int) string {
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				m := MultiplicativeExpression_t {}
				return m.generate(seed,index+1)
			case '1' == seed[index]:
				a := AdditiveExpressionTruple_t {}
				return a.generate(seed,index+1)
		}
	}
	return "addtive_expression\n"  //need to fix base case
}

func (add_tup AdditiveExpressionTruple_t) generate(seed string, index int) string{
	lhs := AdditiveExpression_t {}
	op := AdditiveOperator_t {}
	rhs := MultiplicativeExpression_t {}	
	return lhs.generate(seed,index) + op.generate(seed,index) + rhs.generate(seed,index)
}

func (unary UnaryLogicalTruple_t) generate(seed string, index int) string {
	op := UnaryLogicalOperator_t {}
	lhs := UnaryLogicalExpression_t {}
	return op.generate(seed,index+1) + lhs.generate(seed,index)
}

func (unary UnaryLogicalOperator_t) generate(seed string, index int) string {
	if index < len(seed){
			switch {
				case '0' == seed[index]:
					return "not "
				case '1' == seed[index]:
					return "exists "
			}
	}
	return "unarylogical_op\n" //fix base case
}

func (pipe_op PipeOperator_t) generate(seed string, index int) string {
	return "\n|>"
}

func (pre PrefixOperator_t) generate(seed string, index int) string {
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				return "+"
			case '1' == seed[index]:
				return "-"
		}
	}
	return "-" //need to fix base case
}
func (post_op PostfixOperator_t) generate(seed string, index int) string {
	if index < len(seed){
		switch {
			case '0' == seed[index]:
				m := MemberExpression_t {}
				return m.generate(seed,index+1)
			case '1' == seed[index]:
				p := PostfixOperatorTruple_t {}
				return p.generate(seed,index+1)
		}
	}
	return "postfix_op " //should fix base case
}
func (log LogicalOperator_t) generate(seed string, index int) string {
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				return "and "
			case '1' == seed[index]:
				return "or "
		}
	}
	return "logical_op" //should fix base case
}
func (unary AdditiveOperator_t) generate(seed string, index int) string {
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				return "+"
			case '1' == seed[index]:
				return "-"
		}
	}
	return "additive_op " //fix base case, find nearest legal terminator
}

func (mul MultiplicativeOperator_t) generate(seed string, index int) string {
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				return "*"
			case '1' == seed[index]:
				return "/"
		} //we don't have a '%' operator
	}
	return "multiplicative_op " //fix base case
}

func (ex ExponentOperator_t) generate(seed string, index int) string {
	return "^"
}

func (sub SubConditionalExpressionTruple_t) generate(seed string, index int) string {
		e := Expression_t {}
		d := Expression_t {}
		f := Expression_t {}

		return "if " + string(e.generate(seed,index)) + "then " + string(d.generate(seed,index)) + "else " + string(f.generate(seed,index))

}

func (term Term_t) generate(seed string, index int) string {
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				return generate_string()
			case '1' == seed[index]:
				s := SubTermTruple_t {}
				return s.generate(seed,index)
		}
	}
	return "term " //fix base case
}
func (alt Alternative_t) generate(seed string, index int) string {
	t := Term_t {}
	return t.generate(seed,index)
}



func (sub SubTermTruple_t) generate(seed string,index int) string{
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				g := Group_t {}
				return g.generate(seed,index+1)
			case '1' == seed[index]:
				s := SubSubTermTruple_t {}
				return s.generate(seed,index+1)
		}
	}
	return "subtermtruple " //fix base case
}
func (subsub SubSubTermTruple_t) generate(seed string, index int) string{
	if index < len(seed) {
		switch {
				case '0' == seed[index]:
					o := Option_t {}
					return o.generate(seed,index+1)
				case '1' == seed[index]:
					r := Repetition_t {}
					return r.generate(seed,index+1)
		}
	}
	return "subsubtermtruple "
}

func (opt Option_t) generate(seed string, index int) string{
	e := Expression_t {}
	return  "[" + e.generate(seed, index) + "]"
}

func (rep Repetition_t) generate(seed string, index int) string{
	e := Expression_t {}
	return "{" + e.generate(seed,index) + "}"
}

func (grp Group_t) generate(seed string, index int) string{
	if index < len(seed){
		e := Expression_t {}
		return "(" + e.generate(seed,index) + ")"
	}
	return "()"
}



func (cond ConditionalExpression_t) generate(seed string, index int) string{
	if index < len(seed){
		switch {
			case '0' == seed[index]:
				l := LogicalExpression_t {}
				return l.generate(seed,index+1)
			case '1' == seed[index]:
				s := SubConditionalExpressionTruple_t {}
				return s.generate(seed,index+1)
		}
	}
	return "conditional_expression " //fix base case
}
func (expr Expression_t) generate(seed string, index int) string{
	if index < len(seed){
		switch {
			case '0' == seed[index]:
				a := Alternative_t {}
				return a.generate(seed,index+1)
			case '1' == seed[index]:
				c := ConditionalExpression_t {}
				return c.generate(seed,index+1)
		}
	}
	return "expression " //we've consumed the entire bit string
}
//conditionalexpression
func (prod Production_t) generate(seed string,index int) string{
	e := Expression_t {}
	return e.generate(seed, index)
}
func (primary PrimaryExpressionTruple_t) generate(seed string, index int) string {
	if index < len(seed){
		switch {
			case '0' == seed[index]:
				return "literal " //Literal_t.generate(seed,index)
			case '1' == seed[index]:
				e := Expression_t{}
				return "(" + e.generate(seed,index) + ")"
		}
	}
	return "primaryexpression_truple " //fix base case
}
func (primary PrimaryExpression_t) generate(seed string, index int) string {
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				i := Identifier_t {}
				return i.generate(seed,index+1)
			case '1' == seed[index]:
				p := PrimaryExpressionTruple_t {}
				return p.generate(seed,index+1)
		}
	}
	return "primary_expression "
}
func (post PostfixExpression_t) generate(seed string, index int) string {
	if index < len(seed){
		switch {
			case '0' == seed[index]:
					p := PrimaryExpression_t {}
					return p.generate(seed,index+1)
			case '1' == seed[index]:
					p := PostfixExpressionTruple_t {}
					return p.generate(seed,index+1)
		}
	}
	return "postfix_expression "
}
func (post_tup PostfixExpressionTruple_t) generate(seed string, index int) string {
	op := PostfixOperator_t {}
	p := PostfixExpression_t {}
	return p.generate(seed,index) + op.generate(seed,index)
}
func (pipe PipeExpression_t) generate(seed string, index int) string {
	if index < len(seed){
		switch {
			case '0' == seed[index]:
				post := PostfixExpression_t {}
				return post.generate(seed,index+1)
			case '1' == seed[index]:
				pipe :=  PipeExpression_t {}
				return pipe.generate(seed,index+1)
		}

	}
	return "pipe_expression " //fix base case
}

func (pipe_tup PipeExpressionTruple_t) generate(seed string, index int) string {
	pipe := PipeExpression_t {}
	un := UnaryExpression_t {}
	return pipe.generate(seed,index) + "|>" + un.generate(seed,index)
}

func (un UnaryExpression_t) generate(seed string, index int) string {
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				p := PostfixExpression_t {}
				return p.generate(seed,index)
			case '1' == seed[index]:
				u := UnaryExpressionTruple_t{}
				return u.generate(seed,index+1)
		}
	}
	return "unary_expression "
}
func (un UnaryExpressionTruple_t) generate(seed string, index int) string {
	pre := PrefixOperator_t{}
	u := UnaryExpression_t{}
	return pre.generate(seed,index) + u.generate(seed,index)
}
func (exp ExponentExpression_t) generate(seed string, index int) string {
	if index < len(seed){
		switch {
			case '0' == seed[index]:
				p := PipeExpression_t {}
				return p.generate(seed,index+1)
			case '1' == seed[index]:
				e := ExponentExpressionTruple_t {}
				return e.generate(seed,index+1)
		}
	}
	return "exponent_expression " //fix base case
}

func (exp_tup ExponentExpressionTruple_t) generate(seed string, index int) string {
	exp := ExponentExpression_t {}
	ex_op := ExponentOperator_t {}
	pipe := PipeExpression_t {}
	return exp.generate(seed,index) + ex_op.generate(seed,index) + pipe.generate(seed,index)
}


func (mul MultiplicativeExpression_t) generate(seed string, index int) string {
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				e := ExponentExpression_t {}
				return e.generate(seed,index+1)
			case '1' == seed[index]:
				m := MultiplicativeExpressionTruple_t {}
				return m.generate(seed,index+1)
		}
	}
	return "multi_expression " //fix base case
}

func (m MultiplicativeExpressionTruple_t) generate(seed string, index int) string {
	exp  := ExponentExpression_t {}
	mulop := MultiplicativeOperator_t {}
	mul := MultiplicativeExpression_t {}

	return exp.generate(seed,index) +
				 mulop.generate(seed,index) +
						mul.generate(seed,index)
}

func (comp ComparisonExpression_t) generate(seed string, index int) string {
	if index < len(seed) {
		switch {
			case '0' == seed[index]:
				mul := MultiplicativeExpression_t {}
				return mul.generate(seed,index+1)
			case '1' == seed[index]:
				comp := ComparisonTruple_t{}
				return comp.generate(seed,index+1)
		}
	}
	return "comparison_expression "
}
func (unar UnaryLogicalExpression_t) generate(seed string, index int) string {
	if index < len(seed){
		switch {
			case '0' == seed[index]:
				comp := ComparisonExpression_t {}
				return comp.generate(seed,index+1)
			case '1' == seed[index]:
				un := UnaryLogicalTruple_t {}
				return un.generate(seed,index+1)
		}
	}
	return "unarylogical_expression "
}
func (bin BinaryLogicalExpression_t) generate(seed string, index int) string {
	if index > len(seed) {
		lhs := BinaryLogicalExpression_t {}
		op := LogicalExpression_t {}
		rhs := UnaryLogicalExpression_t {}
		return lhs.generate(seed,index+1) +
					op.generate(seed,index) +
						rhs.generate(seed,index+1)
	}
	return "binarylogicalexpression "

}
func (log LogicalExpression_t) generate(seed string, index int) string {
	if index < len(seed){
		switch {
			case '0' == seed[index]:
				un := UnaryLogicalExpression_t {}
				return un.generate(seed,index+1)
			case '1' == seed[index]:
				bin := BinaryLogicalExpression_t {}
				return bin.generate(seed,index+1)
		}
	}
	return "logical_expression " //fix base case
}


func main(){
	prod := Production_t {}
	fmt.Println(prod.generate(os.Args[1],0))
}

