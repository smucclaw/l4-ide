module Backend.Jl4 where
import Control.Monad.Trans.Except
import Backend.Api
import Base.Text

createL4Function ::
  Monad m =>
  FunctionDeclaration ->
  Text ->
  ExceptT EvaluatorError m RunFunction
createL4Function fnDecl fnImpl =
  undefined
