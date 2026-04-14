defmodule Dagger.Mod.Object do
  @moduledoc """
  Declare a module as an object type.

  ## Declare an object

  Add `use Dagger.Mod.Object` to the Elixir module that want to be a
  Dagger module and give a name through `:name` configuration:

      defmodule Potato do
        use Dagger.Mod.Object, name: "Potato"

        # ...
      end

  The module also support documentation by using Elixir standard documentation,
  `@moduledoc`.

  ## Declare a function

  The module provides a `defn`, a macro for declare a function.
  Let's declare a new function named `echo` that accepts a `name` as a string
  and return a container that echo a name in the module `Potato` from the previous
  section:

      defmodule Potato do
        use Dagger.Mod.Object, name: "Potato"

        defn echo(name: String.t()) :: Dagger.Container.t() do
          dag()
          |> Dagger.Client.container()
          |> Dagger.Container.from("alpine")
          |> Dagger.Container.with_exec(["echo", name])
        end
      end

  From the example above, the `defn` allows you to annotate a type to function
  arguments and return type by using Elixir Typespec. The type will convert to
  a Dagger type when registering a module.

  The supported primitive types for now are:

  1. `integer()` for a boolean type.
  2. `boolean()` for a boolean type.
  3. `String.t()` or `binary()` for a string type.
  4. `list(type)` or `[type]` for a list type.
  5. `type | nil` for optional type.
  6. Any type that generated under `Dagger` namespace (`Dagger.Container.t()`,
     `Dagger.Directory.t()`, etc.).

  The function also support documentation by using Elixir standard documentation,
  `@doc`.
  """

  @type function_name() :: atom()
  @type function_def() :: {function_name(), keyword()}

  alias Dagger.Mod.Object.Defn
  alias Dagger.Mod.Object.Meta

  @doc """
  Get module deprecation reason if deprecated from docs annotation metadata

  Return `{:deprecated, reason}` or `nil` if the module did not specify `@moduledoc deprecated: "reason"`
  """
  def get_module_deprecated(module) do
    with {_, metadatas, _} <- fetch_docs(module),
         %{deprecated: reason} <- metadatas do
      {:deprecated, reason}
    else
      _ -> nil
    end
  end

  @doc """
  Get function deprecation reason if deprecated from docs or attribute

  Return `{:deprecated, reason}` or `nil` if the function did not specify `@deprecated reason` attributes or `@doc deprecated: "reason" docstring`
  """
  def get_function_deprecated(module, func_name) do
    fun = fn
      {{:function, ^func_name, _}, _, _, _, _} -> true
      _ -> false
    end

    with {_, _, func_docs} <- fetch_docs(module),
         {{:function, ^func_name, _}, _, _, _, metadatas} <- Enum.find(func_docs, fun),
         %{deprecated: reason} <- metadatas do
      {:deprecated, reason}
    else
      _ ->
        nil
    end
  end

  @doc """
  Get module documentation.

  Returns module doc string or `nil` if the given module didn't have a documentation.
  """
  @spec get_module_doc(module()) :: String.t() | nil
  def get_module_doc(module) do
    with {module_doc, _, _} <- fetch_docs(module),
         %{"en" => doc} <- module_doc do
      String.trim(doc)
    else
      :none -> nil
      :hidden -> nil
      {:error, :module_not_found} -> nil
    end
  end

  @doc """
  Get function documentation.

  Return doc string or `nil` if that function didn't have a documentation.
  """
  @spec get_function_doc(module(), function_name()) :: String.t() | nil
  def get_function_doc(module, name) do
    fun = fn
      {{:function, ^name, _}, _, _, _, _} -> true
      _ -> false
    end

    with {_, _, function_docs} <- fetch_docs(module),
         {{:function, ^name, _}, _, _, doc_content, _} <- Enum.find(function_docs, fun),
         %{"en" => doc} <- doc_content do
      String.trim(doc)
    else
      nil -> nil
      :none -> nil
      :hidden -> nil
    end
  end

  defp fetch_docs(module) do
    {:docs_v1, _, :elixir, _, module_doc, metadatas, function_docs} = Code.fetch_docs(module)
    {module_doc, metadatas, function_docs}
  end

  @doc """
  Get function cache policy.

  Return `Dagger.FunctionCachePolicy` or `nil` if the function did not specify `@cache` attributes
  """
  @spec get_function_cache_policy(struct()) ::
          Dagger.FunctionCachePolicy.t()
          | {Dagger.FunctionCachePolicy.t(), {:ttl, String.t()}}
          | nil

  def get_function_cache_policy(fun_def) do
    policy = fun_def |> Map.get(:cache_policy)

    case policy do
      :never ->
        Dagger.FunctionCachePolicy.never()

      :per_session ->
        Dagger.FunctionCachePolicy.per_session()

      [ttl: ttl] ->
        {Dagger.FunctionCachePolicy.default(), [time_to_live: ttl]}

      nil ->
        nil
    end
  end

  defmacro __before_compile__(env) do
    module = env.module

    decoder_impl =
      if Module.get_attribute(module, :struct_declared) do
        required_fields = Module.get_attribute(module, :required_fields) || []
        optional_fields = Module.get_attribute(module, :optional_fields) || []
        fields = required_fields ++ optional_fields
        fields = Macro.escape(fields)

        quote do
          defimpl Nestru.Decoder do
            def decode_fields_hint(_empty_struct, _context, _value) do
              {:ok, Dagger.Mod.Object.decoder_hint(unquote(fields))}
            end
          end
        end
      else
        quote do
        end
      end

    # Build compile-time registration data from accumulated attributes.
    object_name = Module.get_attribute(module, :object_name)

    # @function_compile_data accumulates in reverse; reverse to get definition order.
    compile_data =
      (Module.get_attribute(module, :function_compile_data) || [])
      |> Enum.reverse()

    # @field accumulates in reverse; reverse for definition order.
    fields =
      (Module.get_attribute(module, :field) || [])
      |> List.flatten()
      |> Enum.reverse()

    # Compute direct dependencies at compile time.
    deps = compute_deps(compile_data)

    # Generate the __register__/1 body as quoted AST.
    register_body = build_register_body(object_name, compile_data, fields, module)

    dag_var = Macro.var(:dag, nil)

    quote do
      unquote(decoder_impl)

      @doc false
      def __register__(unquote(dag_var)) do
        unquote(register_body)
      end

      @doc false
      def __dependencies__() do
        unquote(Macro.escape(deps))
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Compile-time helpers for __register__/1 and __dependencies__/0 generation
  # ---------------------------------------------------------------------------

  # Returns list of direct dependency modules (enums + object return types)
  # computed from the accumulated @function_compile_data entries.
  defp compute_deps(compile_data) do
    compile_data
    |> Enum.flat_map(fn {_name, _self, arg_defs, return_def, _cache, _doc, _deprecated} ->
      arg_types = Enum.map(arg_defs, fn {_name, kw} -> Keyword.fetch!(kw, :type) end)
      [return_def | arg_types]
    end)
    |> Enum.flat_map(&collect_dep_modules/1)
    |> Enum.uniq()
  end

  defp collect_dep_modules({:list, inner}), do: collect_dep_modules(inner)
  defp collect_dep_modules({:optional, inner}), do: collect_dep_modules(inner)
  defp collect_dep_modules(primitive) when primitive in [:integer, :float, :boolean, :string],
    do: []

  defp collect_dep_modules(module) when is_atom(module) do
    if Code.ensure_loaded?(module) and function_exported?(module, :__kind__, 0) do
      case module.__kind__() do
        :object ->
          # Only user-defined object modules (those using Dagger.Mod.Object) are
          # tracked as dependencies; Dagger built-ins like Dagger.Container are not.
          if function_exported?(module, :__object__, 1), do: [module], else: []

        :enum ->
          [module]

        _ ->
          []
      end
    else
      []
    end
  end

  # Build the AST for the __register__/1 function body.
  defp build_register_body(object_name, compile_data, fields, module) do
    dag = Macro.var(:dag, nil)
    camelized = Dagger.Mod.Helper.camelize(object_name)

    # Separate constructor (:init) from regular functions.
    {constructors, regular_fns} =
      Enum.split_with(compile_data, fn {name, _, _, _, _, _, _} -> name == :init end)

    # Base TypeDef expression.
    base =
      quote do
        unquote(dag)
        |> Dagger.Client.type_def()
        |> Dagger.TypeDef.with_object(
          unquote(camelized),
          case Dagger.Mod.Object.get_module_deprecated(unquote(module)) do
            nil -> []
            {key, val} -> [{key, val}]
          end
        )
      end

    # Pipe in each field.
    with_fields =
      Enum.reduce(fields, base, fn {fname, field_def}, acc ->
        type = field_def.type
        field_opts = build_field_opts(field_def)

        quote do
          unquote(acc)
          |> Dagger.TypeDef.with_field(
            unquote(fname),
            Dagger.Mod.Object.TypeDef.define(unquote(dag), unquote(Macro.escape(type))),
            unquote(field_opts)
          )
        end
      end)

    # Pipe in each regular function.
    with_fns =
      Enum.reduce(regular_fns, with_fields, fn fn_entry, acc ->
        fn_ast = build_function_ast(dag, fn_entry, module)

        quote do
          unquote(acc)
          |> Dagger.TypeDef.with_function(unquote(fn_ast))
        end
      end)

    # Pipe in constructor if present.
    case constructors do
      [] ->
        with_fns

      [fn_entry] ->
        fn_ast = build_function_ast(dag, fn_entry, module)

        quote do
          unquote(with_fns)
          |> Dagger.TypeDef.with_constructor(unquote(fn_ast))
        end
    end
  end

  defp build_field_opts(field_def) do
    opts = []
    opts = if field_def.deprecated, do: Keyword.put(opts, :deprecated, field_def.deprecated), else: opts
    opts = if field_def.doc, do: Keyword.put(opts, :description, field_def.doc), else: opts
    Macro.escape(opts)
  end

  # Build the AST for a single Dagger.Function.
  defp build_function_ast(dag, {fn_name, _has_self, arg_defs, return_def, cache, doc_raw, deprecated_raw}, module) do
    camelized = Dagger.Mod.Helper.camelize(fn_name)
    {doc, doc_deprecated} = normalize_doc_attr(doc_raw)
    deprecated = normalize_deprecated_attr(deprecated_raw) || doc_deprecated

    # Build the return TypeDef expression.
    return_type_ast =
      quote do
        Dagger.Mod.Object.TypeDef.define(unquote(dag), unquote(Macro.escape(return_def)))
      end

    base_fn =
      quote do
        unquote(dag)
        |> Dagger.Client.function(unquote(camelized), unquote(return_type_ast))
      end

    with_doc =
      if doc do
        quote do
          unquote(base_fn) |> Dagger.Function.with_description(unquote(doc))
        end
      else
        base_fn
      end

    with_deprecated =
      if deprecated do
        quote do
          unquote(with_doc) |> Dagger.Function.with_deprecated(reason: unquote(deprecated))
        end
      else
        with_doc
      end

    with_cache = apply_cache_ast(with_deprecated, cache)

    # Pipe in each argument.
    Enum.reduce(arg_defs, with_cache, fn {arg_name, arg_kw}, acc ->
      type = Keyword.fetch!(arg_kw, :type)
      arg_opts = build_arg_opts(arg_kw)

      quote do
        unquote(acc)
        |> Dagger.Function.with_arg(
          unquote(arg_name),
          Dagger.Mod.Object.TypeDef.define(unquote(dag), unquote(Macro.escape(type))),
          unquote(arg_opts)
        )
      end
    end)
  end

  defp build_arg_opts(arg_kw) do
    opts =
      arg_kw
      |> Keyword.take([:doc, :default, :default_path, :ignore, :deprecated])
      |> Enum.reject(fn {_, v} -> is_nil(v) end)
      |> Enum.map(fn
        {:doc, v} -> {:description, v}
        {:default, v} -> {:default_value, Jason.encode!(v)}
        pair -> pair
      end)

    Macro.escape(opts)
  end

  defp apply_cache_ast(ast, nil), do: ast

  defp apply_cache_ast(ast, :never) do
    quote do
      unquote(ast)
      |> Dagger.Function.with_cache_policy(Dagger.FunctionCachePolicy.never())
    end
  end

  defp apply_cache_ast(ast, :per_session) do
    quote do
      unquote(ast)
      |> Dagger.Function.with_cache_policy(Dagger.FunctionCachePolicy.per_session())
    end
  end

  defp apply_cache_ast(ast, ttl: ttl) do
    quote do
      unquote(ast)
      |> Dagger.Function.with_cache_policy(Dagger.FunctionCachePolicy.default(),
        time_to_live: unquote(ttl)
      )
    end
  end

  defp apply_cache_ast(ast, _), do: ast

  # Normalize raw @doc attribute value.
  # Returns {doc_string_or_nil, deprecated_reason_or_nil}.
  defp normalize_doc_attr(nil), do: {nil, nil}
  defp normalize_doc_attr(false), do: {nil, nil}
  defp normalize_doc_attr({_line, doc}) when is_binary(doc), do: {doc, nil}
  defp normalize_doc_attr({_line, opts}) when is_list(opts),
    do: {opts[:doc], opts[:deprecated]}
  defp normalize_doc_attr(doc) when is_binary(doc), do: {doc, nil}
  defp normalize_doc_attr(opts) when is_list(opts), do: {opts[:doc], opts[:deprecated]}
  defp normalize_doc_attr(_), do: {nil, nil}

  # Normalize raw @deprecated attribute value.
  defp normalize_deprecated_attr(nil), do: nil
  defp normalize_deprecated_attr(reason) when is_binary(reason), do: reason
  defp normalize_deprecated_attr(_), do: nil

  defmacro __using__(opts) do
    name = opts[:name]

    quote do
      use Dagger.Core.Base, kind: :object, name: unquote(name)

      import Dagger.Mod.Object, only: [defn: 2, field: 2, field: 3, object: 1]
      import Dagger.Global, only: [dag: 0]

      Module.register_attribute(__MODULE__, :function, accumulate: true, persist: true)
      Module.register_attribute(__MODULE__, :field, accumulate: true, persist: true)
      Module.register_attribute(__MODULE__, :cache, accumulate: false, persist: true)
      # Stores {fn_name, has_self?, arg_defs, return_def, cache, doc, deprecated}
      # captured at defn expansion time so @doc/@deprecated are available before
      # the inner `def` clears them.
      Module.register_attribute(__MODULE__, :function_compile_data, accumulate: true)
      # Store the Dagger object name for use in @before_compile.
      Module.register_attribute(__MODULE__, :object_name, accumulate: false)
      @object_name unquote(name)

      @before_compile Dagger.Mod.Object

      # Get an object name
      def __object__(:name), do: unquote(name)

      # List available function definitions.
      def __object__(:functions) do
        __MODULE__.__info__(:attributes)
        |> Keyword.get_values(:function)
        |> Enum.flat_map(& &1)
      end

      # Get a function definition.
      def __object__(:function, name) do
        __object__(:functions)
        |> Keyword.fetch!(name)
      end

      # List available field definitions.
      def __object__(:fields) do
        __MODULE__.__info__(:attributes)
        |> Keyword.get_values(:field)
        |> Enum.flat_map(& &1)
      end
    end
  end

  @doc """
  Declare a function.
  """
  defmacro defn(call, do: block) do
    {name, args, return} = extract_call(call)
    has_self? = is_tuple(args)
    arg_defs = compile_args(args)
    return_def = compile_typespec!(return)

    quote do
      # Capture @doc, @deprecated, @cache BEFORE the inner `def` (inside
      # Defn.define) clears them.  These are evaluated at module-compile time,
      # in the order they appear in the module body, so @doc is still set here.
      cache_attr = Module.get_attribute(__MODULE__, :cache)
      doc_attr = Module.get_attribute(__MODULE__, :doc)
      deprecated_attr = Module.get_attribute(__MODULE__, :deprecated)

      @function_compile_data {unquote(name), unquote(has_self?),
                               unquote(Macro.escape(arg_defs)),
                               unquote(Macro.escape(return_def)), cache_attr,
                               doc_attr, deprecated_attr}

      @function {unquote(name),
                 %Dagger.Mod.Object.FunctionDef{
                   self: unquote(has_self?),
                   cache_policy: cache_attr,
                   args: unquote(arg_defs),
                   return: unquote(return_def)
                 }}
      unquote(Defn.define(name, args, return, block))
    end
  end

  @doc """
  Declare an object struct.
  """
  defmacro object(do: block) do
    quote do
      Module.register_attribute(__MODULE__, :required_fields, accumulate: true)
      Module.register_attribute(__MODULE__, :optional_fields, accumulate: true)

      unquote(block)

      required_fields = @required_fields || []
      optional_fields = @optional_fields || []
      fields = @required_fields ++ @optional_fields

      # TODO: convert fields into typespec.
      @type t() :: %__MODULE__{}

      @derive Jason.Encoder
      @enforce_keys Keyword.keys(required_fields)
      defstruct fields |> Keyword.keys() |> Enum.sort()

      @struct_declared true
    end
  end

  def decoder_hint(fields) do
    fields
    |> Enum.filter(&only_module/1)
    |> Enum.into(%{}, fn {name, field_def} ->
      type =
        case field_def.type do
          {:list, type} -> type
          {:optional, type} -> type
          type -> type
        end

      {name, type}
    end)
  end

  defp only_module({_, field_def}) do
    case field_def.type do
      {:list, type} -> module?(type)
      {:optional, type} -> module?(type)
      type -> module?(type)
    end
  end

  defp module?(type) do
    {:module, ^type} = Code.ensure_loaded(type)
    function_exported?(type, :__struct__, 0)
  end

  @doc """
  Declare a field.
  """
  defmacro field(name, type, opts \\ []) do
    type = compile_typespec!(type)
    optional? = match?({:optional, _}, type)
    doc = opts[:doc]
    deprecated = opts[:deprecated]

    field =
      Macro.escape(
        {name, %Dagger.Mod.Object.FieldDef{type: type, doc: doc, deprecated: deprecated}}
      )

    quote do
      @field unquote(field)
      if unquote(optional?) do
        Module.put_attribute(__MODULE__, :optional_fields, unquote(field))
      else
        Module.put_attribute(__MODULE__, :required_fields, unquote(field))
      end
    end
  end

  defguardp is_self(self) when is_atom(elem(self, 0)) and is_nil(elem(self, 2))
  defguardp is_args(args) when is_list(args)

  defp extract_call({:"::", _, [call_def, return]}) do
    {name, args} = extract_call_def(call_def)
    {name, args, return}
  end

  defp extract_call_def({name, _, []}) do
    {name, []}
  end

  defp extract_call_def({name, _, [self]}) when is_self(self) do
    {name, {self, []}}
  end

  defp extract_call_def({name, _, [args]}) when is_args(args) do
    {name, args}
  end

  defp extract_call_def({name, _, [self, args]}) when is_self(self) and is_args(args) do
    {name, {self, args}}
  end

  defp compile_args({_, args}) do
    compile_args(args)
  end

  defp compile_args(args) do
    for {name, spec} <- args do
      type = compile_typespec!(spec)
      meta = spec |> extract_options() |> Keyword.put(:type, type)
      {name, Meta.validate!(meta)}
    end
  end

  defp compile_typespec!({:integer, _, []}), do: :integer
  defp compile_typespec!({:float, _, []}), do: :float
  defp compile_typespec!({:boolean, _, []}), do: :boolean

  ## List

  defp compile_typespec!({:list, _, [type]}) do
    {:list, compile_typespec!(type)}
  end

  defp compile_typespec!([type]) do
    {:list, compile_typespec!(type)}
  end

  ## Optional

  defp compile_typespec!(
         {{{:., _,
            [
              {:__aliases__, _, [_type]},
              :t
            ]}, _, []} = type, [default: _default_value]}
       ) do
    {:optional, compile_typespec!(type)}
  end

  defp compile_typespec!({:|, _, [type, nil]}) do
    {:optional, compile_typespec!(type)}
  end

  ## Type with options

  defp compile_typespec!({type, _}) do
    compile_typespec!(type)
  end

  ## String

  defp compile_typespec!({:binary, _, []}), do: :string

  defp compile_typespec!(
         {{:., _,
           [
             {:__aliases__, _, [:String]},
             :t
           ]}, _, []}
       ) do
    :string
  end

  defp compile_typespec!({{:., _, [{:__aliases__, _, module}, :t]}, _, []}) do
    Module.concat(module)
  end

  defp compile_typespec!(unsupported_type) do
    raise ArgumentError, "type `#{Macro.to_string(unsupported_type)}` is not supported"
  end

  defp extract_options({_, options}), do: options
  defp extract_options(_), do: []
end
