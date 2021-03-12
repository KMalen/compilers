grammar leq;

prog : ((nonVoidFunc | voidFunc) NEWLINE+)* ;

bodyContent:
     (declareValue
     | declareArray
     | initValue
     | initArray
     | variableAssignment
     | variableCast
     | ifCondition
     | forCycle
     | whileCycle)
     NEWLINE+
    ;

//type value
varType: INT | FLOAT | BOOL ;
arrayType: RIGHT_SQUARE_BRACKET INT | FLOAT | BOOL LEFT_SQUARE_BRACKET ;

//name value
varName: CHAR+ (CHAR | DIGIT | UNDERSCORE | ZERO)* ;

//type_value value
value: (intValue | unsignedIntValue | floatValue) ;
array: (intArray | floatArray | boolArray) ;

intValue: ZERO | MINUS? DIGIT+ (ZERO | DIGIT)* ;
unsignedIntValue: ZERO | DIGIT+ (ZERO | DIGIT)* ;
floatValue: MINUS? (ZERO | DIGIT (ZERO | DIGIT)*) POINT (ZERO | DIGIT)+ ;
boolValue: TRUE | FALSE ;

intArray: LEFT_CURVY_BRACKET intValue* (COMMA intValue)* RIGHT_CURVY_BRACKET ;
floatArray: LEFT_CURVY_BRACKET floatValue* (COMMA floatValue)* RIGHT_CURVY_BRACKET ;
boolArray: LEFT_CURVY_BRACKET boolValue* (COMMA boolValue)* RIGHT_CURVY_BRACKET ;

getElement:  varName LEFT_SQUARE_BRACKET unsignedIntValue RIGHT_SQUARE_BRACKET ;

//declaration
declareValue: varName ;
declareArray: varName LEFT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET ;

//init
initValue: varName ASSIGN (intValue | floatValue | boolValue) ;
initArray: varName LEFT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET ASSIGN (intArray | floatArray | boolArray) ;

//assignment
variableAssignment: varName ASSIGN (value | varName | getElement | ariphmeticExpression) ;
arrayElementAssignment: getElement ASSIGN (value | varName | getElement | ariphmeticExpression) ;

//cast
castExpr: LEFT_ROUND_BRACKET (INT | FLOAT | BOOL) RIGHT_ROUND_BRACKET ;

variableCast: varName ASSIGN castExpr (ariphmeticExpression | varName | value | getElement) ;
arrayElementToVariableCast: getElement ASSIGN castExpr (varName | value | getElement) ;

//func
signature: varName LEFT_ROUND_BRACKET attribute* RIGHT_ROUND_BRACKET;
attribute: ((varType declareValue | varType declareArray) COMMA)* (varType declareValue | varType declareArray);
return: 'return' (varName | value | array) ;

nonVoidFunc:
    'func' signature '->' varType LEFT_CURVY_BRACKET NEWLINE
    bodyContent*
    return NEWLINE
    RIGHT_CURVY_BRACKET
    ;

voidFunc:
    'func' signature '->' 'void' LEFT_CURVY_BRACKET NEWLINE
    bodyContent* NEWLINE
    RIGHT_CURVY_BRACKET
    ;

// if else
comparisonOperator: EQUAL | GREATER | GREATER_OR_EQUAL | LESS | LESS_OR_EQUAL | NOT_EQUAL ;
logicalComparisonOperator: LOGICAL_AND | LOGICAL_OR ;
comparisonAtom: varName | value | signature | TRUE | FALSE | ariphmeticExpression ;
comparisonStatement: comparisonAtom (comparisonOperator comparisonAtom)*;
comparisonLogicalStatement: LEFT_ROUND_BRACKET comparisonStatement (logicalComparisonOperator comparisonStatement)* RIGHT_ROUND_BRACKET;

ifCondition: IF comparisonLogicalStatement LEFT_CURVY_BRACKET
    NEWLINE
    bodyContent*
    RIGHT_CURVY_BRACKET
    (NEWLINE ELSE ifCondition)*
    (NEWLINE ELSE LEFT_CURVY_BRACKET
    NEWLINE
    bodyContent*
    RIGHT_CURVY_BRACKET)?
    ;

//ariphemitcs
ariphmeticOperator: PLUS | MINUS | MULTIPLY | DIVISION | MOD | POW;
ariphmeticAtom: (varName | value | getElement | ariphmeticAtomWithBrackets) (ariphmeticOperator (varName | value | getElement | ariphmeticAtomWithBrackets))+;
ariphmeticAtomWithBrackets: LEFT_ROUND_BRACKET ariphmeticAtom+  RIGHT_ROUND_BRACKET;
ariphmeticExpression: (ariphmeticAtomWithBrackets | ariphmeticAtom) ( ariphmeticOperator (ariphmeticAtom | ariphmeticAtomWithBrackets))*;

//CYCLES
cycleStart: initValue | variableAssignment | variableCast ;
cycleEnd: variableAssignment | variableCast ;
cycleStep: value | varName;
cycleHead: LEFT_ROUND_BRACKET cycleStart cycleEnd cycleStep RIGHT_ROUND_BRACKET ;

whileCycle: WHILE comparisonLogicalStatement LEFT_CURVY_BRACKET
    NEWLINE
    bodyContent*
    RIGHT_CURVY_BRACKET
    ;

forCycle: FOR cycleHead LEFT_CURVY_BRACKET
    NEWLINE
    bodyContent*
    RIGHT_CURVY_BRACKET
    ;

// LEXEMAS
WHILE: 'while' ;
FOR: 'for' ;
INT: 'int' ;
FLOAT: 'float' ;
BOOL: 'bool' ;
ASSIGN: '=' ;
PLUS: '+' ;
MOD: '%' ;
POW: '^' ;
MULTIPLY: '*' ;
DIVISION: '/' ;
MINUS : '-' ;
POINT : '.' ;
COMMA : ',' ;
LEFT_SQUARE_BRACKET: '[' ;
RIGHT_SQUARE_BRACKET: ']' ;
LEFT_ROUND_BRACKET: '(' ;
RIGHT_ROUND_BRACKET: ')' ;
LEFT_CURVY_BRACKET: '{' ;
RIGHT_CURVY_BRACKET: '}' ;
ZERO : '0' ;
TRUE: 'true' ;
FALSE: 'false' ;
EQUAL:  '==' ;
GREATER: '>' ;
LESS: '<' ;
GREATER_OR_EQUAL: '>=' ;
LESS_OR_EQUAL: '<=' ;
NOT_EQUAL: '!=' ;
LOGICAL_AND: '&&' ;
LOGICAL_OR: '||' ;
IF : 'if' ;
ELSE: 'else' ;
CHAR : [a-zA-Z] ;
UNDERSCORE: '_' ;
DIGIT : [1-9] ;
NEWLINE : [\n\r]+ ;
WS : [ \r] -> skip ;