batch model a b = model ! [ a, b ]
batch_withComments model a b = model {- A -} ! {- B -} [ a, b ]
batch_notALiteralList model cmds = model ! cmds

none model = model ! []
none_withComments model = model {- A -} ! {- B -} [ {- C -} ]

inBinaryExpression model x a b = x + model ! [ a ] * [ b ]
multipleBangs model x = x + model ! [] * model ! []
nestedBangs model a b = model ! [ a ] ! [ b ] ! []
withHighPrecedenceOperators_leftAssoc f g x = f >> g ! [] >> x
withHighPrecedenceOperators_leftAssoc_complex a b c d e f g h i j = a >> b + c >> d >> e ! f >> g >> h + i >> j
withHighPrecedenceOperators_rightAssoc f g x = f << g ! [] << x
withFunctionApplication f g x = f x ! g x
