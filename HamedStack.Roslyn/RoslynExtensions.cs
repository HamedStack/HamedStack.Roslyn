// ReSharper disable UnusedMember.Global
// ReSharper disable RedundantUsingDirective
// ReSharper disable UnusedType.Global

using System.Collections.Immutable;
using System.Runtime.InteropServices;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using RoslynQuoter;

namespace HamedStack.Roslyn;

/// <summary>
/// Provides extension methods for manipulating and formatting C# source code using Roslyn.
/// </summary>
public static class RoslynExtensions
{
    /// <summary>
    /// Formats the provided C# source code string using Roslyn's formatting services.
    /// </summary>
    /// <param name="sourceCode">The C# source code string to format.</param>
    /// <returns>A formatted C# source code string.</returns>
    public static string FormatCsharpSourceCode(this string sourceCode)
    {
        var tree = CSharpSyntaxTree.ParseText(sourceCode);
        var root = (CompilationUnitSyntax)tree.GetRoot();
        var workspace = new AdhocWorkspace();
        var syntax = Formatter.Format(root, workspace);
        return syntax.GetText().ToString();
    }
    /// <summary>
    /// Formats the provided C# syntax node using Roslyn's formatting services.
    /// </summary>
    /// <param name="syntaxNode">The C# syntax node to format.</param>
    /// <returns>A formatted C# syntax node.</returns>
    public static SyntaxNode FormatCsharpSourceCode(this SyntaxNode syntaxNode)
    {
        var workspace = new AdhocWorkspace();
        return Formatter.Format(syntaxNode, workspace);
    }
    /// <summary>
    /// Converts a C# source code string to its equivalent Roslyn API code for generating that source code.
    /// </summary>
    /// <param name="source">The C# source code string to convert.</param>
    /// <param name="useDefaultFormatting">Indicates whether to use default formatting.</param>
    /// <param name="openParenthesisOnNewLine">Indicates whether to place open parenthesis on a new line.</param>
    /// <param name="closingParenthesisOnNewLine">Indicates whether to place closing parenthesis on a new line.</param>
    /// <param name="removeRedundantModifyingCalls">Indicates whether to remove redundant modifying calls from the generated Roslyn API code.</param>
    /// <param name="shortenCodeWithUsingStatic">Indicates whether to shorten the generated Roslyn API code using static imports.</param>
    /// <returns>A string containing the Roslyn API code for generating the provided source code.</returns>
    public static string ToRoslynApi(this string source, bool useDefaultFormatting = true, bool openParenthesisOnNewLine = false
        , bool closingParenthesisOnNewLine = false, bool removeRedundantModifyingCalls = false, bool shortenCodeWithUsingStatic = false)
    {
        var quoter = new Quoter()
        {
            UseDefaultFormatting = useDefaultFormatting,
            ShortenCodeWithUsingStatic = shortenCodeWithUsingStatic,
            RemoveRedundantModifyingCalls = removeRedundantModifyingCalls,
            OpenParenthesisOnNewLine = openParenthesisOnNewLine,
            ClosingParenthesisOnNewLine = closingParenthesisOnNewLine
        };
        return quoter.Quote(source).ToString() ?? string.Empty;
    }

    /// <summary>
    /// Generates a string representation of the provided method symbol, optionally including a given method body.
    /// </summary>
    /// <param name="methodSymbol">The method symbol to generate a representation for.</param>
    /// <param name="methodBody">Optional body of the method to be included in the generated string. If not provided, a default or placeholder body might be used.</param>
    /// <returns>A string representation of the method symbol, incorporating the provided method body if given.</returns>
    /// <remarks>
    /// This method can be leveraged for code generation tasks or when you need a textual representation of a method 
    /// including a specific method body. If no method body is provided, the behavior might default to generating a placeholder or 
    /// standard body depending on the method's characteristics.
    /// </remarks>
    public static string GenerateMethod(this IMethodSymbol methodSymbol, string? methodBody = null)
    {
        var sb = new StringBuilder();

        // XML documentation comments
        if (methodSymbol.GetDocumentationCommentXml() != null)
        {
            sb.AppendLine($"/// {methodSymbol.GetDocumentationCommentXml()?.Replace("\n", "\n/// ")}");
        }

        // Attributes
        foreach (var attributeData in methodSymbol.GetAttributes())
        {
            var positionalArgs = string.Join(", ", attributeData.ConstructorArguments.Select(arg =>
                arg.Value is string ? $"\"{arg.Value}\"" : arg.Value?.ToString()));

            var namedArgs = string.Join(", ",
                attributeData.NamedArguments.Select(arg => $"{arg.Key} = {arg.Value.Value}"));
            var allArgs = string.Join(", ", new[] { positionalArgs, namedArgs }.Where(a => !string.IsNullOrEmpty(a)));
            sb.AppendLine(
                $"[{attributeData.AttributeClass?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}({allArgs})]");
        }

        // Modifiers
        if (methodSymbol.IsStatic && methodSymbol.MethodKind != MethodKind.LocalFunction)
        {
            sb.Append("static ");
        }

        if (methodSymbol.IsExtern)
        {
            sb.Append("extern ");
        }

        if (methodSymbol.IsAbstract && methodSymbol.ContainingType.TypeKind != TypeKind.Interface)
        {
            sb.Append("abstract ");
        }
        else if (methodSymbol.IsVirtual)
        {
            sb.Append("virtual ");
        }
        else if (methodSymbol.IsOverride)
        {
            sb.Append("override ");
        }
        else if (methodSymbol.IsSealed)
        {
            sb.Append("sealed ");
        }

        if (methodSymbol.IsReadOnly)
        {
            sb.Append("readonly ");
        }

        if (methodSymbol.IsAsync)
        {
            sb.Append("async ");
        }

        if (methodSymbol.MethodKind != MethodKind.LocalFunction)
        {
            sb.Append(methodSymbol.DeclaredAccessibility.ToString().ToLower());
            sb.Append(" ");
        }

        if (methodSymbol.IsPartialDefinition)
        {
            sb.Append("partial ");
        }

        // Ref return types
        if (methodSymbol.RefKind == RefKind.Ref)
        {
            sb.Append("ref ");
        }
        else if (methodSymbol.RefKind == RefKind.RefReadOnly)
        {
            sb.Append("ref readonly ");
        }

        // Return type
        sb.Append(methodSymbol.ReturnsVoid
            ? "void"
            : methodSymbol.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        sb.Append(" ");

        // Operator overloading and conversion operators
        if (methodSymbol.MethodKind == MethodKind.UserDefinedOperator ||
            methodSymbol.MethodKind == MethodKind.Conversion)
        {
            sb.Append("operator ");
            sb.Append(methodSymbol.MetadataName.Replace("op_", ""));
        }
        else
        {
            // Explicit interface implementation
            foreach (var interfaces in methodSymbol.ExplicitInterfaceImplementations)
            {
                sb.Append($"{interfaces.ContainingType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}.");
            }

            // Method name
            sb.Append(methodSymbol.Name);
        }

        // Type parameters
        if (methodSymbol.TypeParameters.Length > 0)
        {
            sb.Append("<");
            sb.Append(string.Join(", ",
                methodSymbol.TypeParameters.Select(tp =>
                    tp.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat))));
            sb.Append(">");
        }

        sb.Append("(");

        // Parameters
        for (int i = 0; i < methodSymbol.Parameters.Length; i++)
        {
            var param = methodSymbol.Parameters[i];

            if (param.IsThis)
            {
                sb.Append("this ");
            }

            // Parameter attributes
            foreach (var paramAttribute in param.GetAttributes())
            {
                sb.Append(
                    $"[{paramAttribute.AttributeClass?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}] ");
            }

            if (param.RefKind == RefKind.In)
            {
                sb.Append("in ");
            }
            else if (param.RefKind == RefKind.Out)
            {
                sb.Append("out ");
            }
            else if (param.RefKind == RefKind.Ref)
            {
                sb.Append("ref ");
            }

            if (param.IsParams)
            {
                sb.Append("params ");
            }

            sb.Append($"{param.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)} {param.Name}");

            if (param.HasExplicitDefaultValue)
            {
                if (param.ExplicitDefaultValue is string strVal)
                {
                    sb.Append($" = \"{strVal}\"");
                }
                else
                {
                    sb.Append($" = {param.ExplicitDefaultValue}");
                }
            }

            if (i < methodSymbol.Parameters.Length - 1)
                sb.Append(", ");
        }

        sb.Append(")");

        // Constraints on type parameters
        foreach (var typeParam in methodSymbol.TypeParameters)
        {
            var constraints = typeParam.ConstraintTypes;
            if (constraints.Length > 0 || typeParam.HasConstructorConstraint || typeParam.HasValueTypeConstraint ||
                typeParam.HasReferenceTypeConstraint)
            {
                sb.AppendLine();
                var constraintList = new List<string>();
                if (typeParam.HasReferenceTypeConstraint)
                {
                    constraintList.Add("class");
                }

                if (typeParam.HasValueTypeConstraint)
                {
                    constraintList.Add("struct");
                }

                constraintList.AddRange(constraints.Select(c =>
                    c.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)));
                if (typeParam.HasConstructorConstraint)
                {
                    constraintList.Add("new()");
                }

                sb.Append($"where {typeParam.Name} : {string.Join(", ", constraintList)}");
            }
        }

        // Beginning of method body
        if (!methodSymbol.IsAbstract && methodSymbol is { IsExtern: false, IsPartialDefinition: false })
        {
            sb.AppendLine();
            sb.AppendLine("{");
            if (methodBody != null)
            {
                sb.AppendLine($"\t{methodBody}");
            }
            sb.AppendLine("}");
        }
        else
        {
            sb.AppendLine(";");
        }

        return sb.ToString();
    }

    /// <summary>
    /// Determines whether the given symbol has public accessibility.
    /// </summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>true if the symbol is public; otherwise, false.</returns>
    public static bool IsPublic(this ISymbol symbol)
    {
        return symbol.DeclaredAccessibility == Accessibility.Public;
    }

    /// <summary>
    /// Determines whether the given symbol has private accessibility.
    /// </summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>true if the symbol is private; otherwise, false.</returns>
    public static bool IsPrivate(this ISymbol symbol)
    {
        return symbol.DeclaredAccessibility == Accessibility.Private;
    }

    /// <summary>
    /// Determines whether the given symbol has internal accessibility.
    /// </summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>true if the symbol is internal; otherwise, false.</returns>
    public static bool IsInternal(this ISymbol symbol)
    {
        return symbol.DeclaredAccessibility == Accessibility.Internal;
    }

    /// <summary>
    /// Determines whether the given symbol has protected accessibility.
    /// </summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>true if the symbol is protected; otherwise, false.</returns>
    public static bool IsProtected(this ISymbol symbol)
    {
        return symbol.DeclaredAccessibility == Accessibility.Protected;
    }

    /// <summary>
    /// Determines whether the given symbol is virtual.
    /// </summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>true if the symbol is virtual; otherwise, false.</returns>
    public static bool IsVirtual(this ISymbol symbol)
    {
        return (symbol as IMethodSymbol)?.IsVirtual ?? false;
    }

    /// <summary>
    /// Determines whether the given symbol is an override.
    /// </summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>true if the symbol is an override; otherwise, false.</returns>
    public static bool IsOverride(this ISymbol symbol)
    {
        return (symbol as IMethodSymbol)?.IsOverride ?? false;
    }

    /// <summary>
    /// Determines whether the given symbol has the specified attribute.
    /// </summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <param name="attributeType">The attribute type to check for.</param>
    /// <returns>true if the symbol has the attribute; otherwise, false.</returns>
    public static bool HasAttribute(this ISymbol? symbol, INamedTypeSymbol attributeType)
    {
        return symbol?.GetAttributes()
            .Any(attr => SymbolEqualityComparer.Default.Equals(attr.AttributeClass, attributeType)) ?? false;
    }

    /// <summary>
    /// Determines whether the given type is a class.
    /// </summary>
    /// <param name="type">The type to check.</param>
    /// <returns>true if the type is a class; otherwise, false.</returns>
    public static bool IsClassType(this INamedTypeSymbol type)
    {
        return type.TypeKind == TypeKind.Class;
    }

    /// <summary>
    /// Determines whether the given type is an interface.
    /// </summary>
    /// <param name="type">The type to check.</param>
    /// <returns>true if the type is an interface; otherwise, false.</returns>
    public static bool IsInterfaceType(this INamedTypeSymbol type)
    {
        return type.TypeKind == TypeKind.Interface;
    }

    /// <summary>
    /// Determines whether the given type is an enum.
    /// </summary>
    /// <param name="type">The type to check.</param>
    /// <returns>true if the type is an enum; otherwise, false.</returns>
    public static bool IsEnumType(this INamedTypeSymbol type)
    {
        return type.TypeKind == TypeKind.Enum;
    }

    /// <summary>
    /// Retrieves the base types of the given type.
    /// </summary>
    /// <param name="type">The type to check.</param>
    /// <returns>An IEnumerable of base types for the given type.</returns>
    public static IEnumerable<INamedTypeSymbol> GetBaseTypes(this INamedTypeSymbol type)
    {
        return type.BaseType != null
            ? GetBaseTypes(type.BaseType).Concat(new[] { type.BaseType })
            : Enumerable.Empty<INamedTypeSymbol>();
    }

    /// <summary>
    /// Retrieves the members declared in the given type.
    /// </summary>
    /// <param name="type">The type to check.</param>
    /// <returns>An IEnumerable of members declared in the given type.</returns>
    public static IEnumerable<ISymbol> GetDeclaredMembers(this INamedTypeSymbol type)
    {
        return type.GetMembers().Where(m => m.ContainingType.Equals(type, SymbolEqualityComparer.Default));
    }

    /// <summary>
    /// Retrieves the fields of the given type.
    /// </summary>
    /// <param name="type">The type to check.</param>
    /// <returns>An IEnumerable of fields for the given type.</returns>
    public static IEnumerable<IFieldSymbol> GetFields(this INamedTypeSymbol type)
    {
        return type.GetMembers().OfType<IFieldSymbol>();
    }

    /// <summary>
    /// Retrieves the properties of the given type.
    /// </summary>
    /// <param name="type">The type to check.</param>
    /// <returns>An IEnumerable of properties for the given type.</returns>
    public static IEnumerable<IPropertySymbol> GetProperties(this INamedTypeSymbol type)
    {
        return type.GetMembers().OfType<IPropertySymbol>();
    }

    /// <summary>
    /// Retrieves the methods of the given type.
    /// </summary>
    /// <param name="type">The type to check.</param>
    /// <returns>An IEnumerable of methods for the given type.</returns>
    public static IEnumerable<IMethodSymbol> GetMethods(this INamedTypeSymbol type)
    {
        return type.GetMembers().OfType<IMethodSymbol>();
    }

    /// <summary>
    /// Determines whether the given method has parameters.
    /// </summary>
    /// <param name="method">The method to check.</param>
    /// <returns>true if the method has parameters; otherwise, false.</returns>
    public static bool HasParameters(this IMethodSymbol method)
    {
        return method.Parameters.Length > 0;
    }

    /// <summary>
    /// Reports a diagnostic for the given symbol if it meets the specified validation criteria.
    /// </summary>
    /// <typeparam name="TSymbol">The type of the symbol being checked.</typeparam>
    /// <param name="symbol">The symbol being checked.</param>
    /// <param name="context">The generator execution context used to report diagnostics.</param>
    /// <param name="validationCriteria">The criteria that determine if a diagnostic should be reported for the symbol.</param>
    /// <param name="diagnosticRule">The diagnostic rule to be reported if the symbol meets the validation criteria.</param>
    /// <remarks>
    /// This method provides a generic way to report diagnostics for any symbol based on custom validation logic and diagnostic rules.
    /// </remarks>
    public static void ReportDiagnosticIf<TSymbol>(
        this TSymbol symbol,
        GeneratorExecutionContext context,
        Predicate<TSymbol> validationCriteria,
        DiagnosticDescriptor diagnosticRule)
        where TSymbol : ISymbol
    {
        if (validationCriteria(symbol)) return;
        var diagnostic = Diagnostic.Create(diagnosticRule, symbol.Locations[0], symbol.Name);
        context.ReportDiagnostic(diagnostic);
    }

    /// <summary>
    /// Runs the specified source generator on the input code and returns the generated syntax trees.
    /// </summary>
    /// <typeparam name="TGenerator">The type of source generator to run.</typeparam>
    /// <param name="inputCode">The C# source code to process.</param>
    /// <param name="diagnostics">Returns any diagnostics emitted during the generation process.</param>
    /// <param name="additionalReferences">Optional additional metadata references to be used in the compilation.</param>
    /// <returns>The collection of syntax trees after running the generator, including the input and generated ones.</returns>
    public static IEnumerable<SyntaxTree> RunGenerator<TGenerator>(
        this string inputCode,
        out ImmutableArray<Diagnostic> diagnostics,
        IEnumerable<MetadataReference>? additionalReferences = null)
        where TGenerator : ISourceGenerator, new()
    {
        var parseOptions = new CSharpParseOptions(LanguageVersion.Latest
            , kind: SourceCodeKind.Regular
            , documentationMode: DocumentationMode.Parse
            , preprocessorSymbols: new[] { "NULLABLE_ENABLE" }
            );
        var syntaxTree = CSharpSyntaxTree.ParseText(inputCode, parseOptions);

        var references = new List<MetadataReference>
        {
            MetadataReference.CreateFromFile(typeof(object).Assembly.Location)
        };

        if (additionalReferences != null)
        {
            references.AddRange(additionalReferences);
        }

        var compilation = CSharpCompilation.Create(
            "TestAssembly",
            new[] { syntaxTree },
            references,
            new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var generator = new TGenerator();
        var driver = CSharpGeneratorDriver.Create(generator);

        driver.RunGeneratorsAndUpdateCompilation(compilation, out var outputCompilation, out diagnostics);

        return outputCompilation.SyntaxTrees;
    }

    /// <summary>
    /// Creates a metadata reference for an assembly located in the current runtime directory.
    /// </summary>
    /// <param name="assemblyName">The name of the assembly. If the ".dll" extension is omitted, it will be appended.</param>
    /// <returns>A metadata reference for the specified assembly.</returns>
    public static MetadataReference CreateFromRuntime(string assemblyName)
    {
        if (string.IsNullOrWhiteSpace(assemblyName))
            throw new ArgumentException("Value cannot be null or whitespace.", nameof(assemblyName));

        if (!assemblyName.EndsWith(".dll"))
        {
            assemblyName += ".dll";
        }

        var assemblyPath = Path.Combine(RuntimeEnvironment.GetRuntimeDirectory(), assemblyName);
        return MetadataReference.CreateFromFile(assemblyPath);
    }
}