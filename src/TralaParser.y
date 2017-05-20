{
module TralaParser where
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    dummy    { DummyToken }

%%

dummyRule : dummy { 1 } |
            dummyRule dummy { $1 + 1 }


{

data Token = DummyToken

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
