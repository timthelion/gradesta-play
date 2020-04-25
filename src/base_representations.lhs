> import Gradesta.Pure

> cell_runtime_template :: CellRuntime
> ...

> main :: IO ()
> main = do
>  serve_gradesta numbers update cell_runtime_template

> type State :: ()

> update :: State -> State
> update = id

> numbers :: State -> CellView -> CellRuntime
> numbers _ cell_view =
>   let number = (get_parameter_int cell_view "number" 0)
>       base   = (get_parameter_int cell_view "base" 0)
>       with_same_base = add_parameter "base" base empty_parameters
>       with_same_number = add_parameter "number" number empty_parameters
>   in
>    mk_cell
>      (text_cell $ render_number number base) $
>      [
>        (Up,
>         add_parameter "number" (number - 1)
>         with_same_base),
>        (Down,
>         add_parameter "number" (number + 1) $
>         with_same_base)
>      ] + case (base, number `mod` 2) of
>       (2, 0) ->
>          (Left,
>            add_parameter_int "base" 16 $
>            add_parameter_int "number" (number / 2) $
>            empty_parameters) :
>          (Right,
>            add_parameter "base" 16 $
>            with_same_number) : []
>       (10, _) ->
>          (Left,
>            add_parameter_int "base" 2 $
>            empty_parameters) :
>          (Right,
>            add_parameter "base" 16 $
>            with_same_number) : [],
>       (16, _) ->
>          (Left,
>            add_parameter "base" 10 $
>            with_same_number) :
>          (Right,
>            add_parameter_int "base" 2 $
>            add_parameter_int "number" (number * 2) $
>            empty_parameters) : []
