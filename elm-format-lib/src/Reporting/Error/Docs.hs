module Reporting.Error.Docs where


data Error
    = NoDocs
    | OnlyInDocs String [String]
    | OnlyInExports [String]
    | NoComment String
    | NoType String
