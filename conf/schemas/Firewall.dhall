{- The firewalld rules -}
{ Type =
    { immediate : Text
    , permanent : Text
    , port : Optional Text
    , rich_rule : Optional Text
    , state : Text
    }
, default =
  { immediate = "yes"
  , permanent = "yes"
  , port = None Text
  , rich_rule = None Text
  , state = "enabled"
  }
}
