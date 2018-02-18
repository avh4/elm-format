batch model a b = model ! [ a, b ]
batch_withComments model a b = model {- A -} ! {- B -} [ a, b ]

none model = model ! []
none_withComments model = model {- A -} ! {- B -} [ {- C -} ]
