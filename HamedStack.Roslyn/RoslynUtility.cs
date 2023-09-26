// ReSharper disable UnusedType.Global
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable UnusedMember.Global

using System.Collections.Immutable;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace HamedStack.Roslyn;

/// <summary>
/// Provides utility methods for working with Roslyn and .NET assemblies.
/// </summary>
public class RoslynUtility
{
    /// <summary>
    /// Gets the runtime directory where the .NET assemblies are located.
    /// </summary>
    /// <returns>
    /// A <see cref="System.String"/> representing the runtime directory path.
    /// </returns>
    public static string GetRuntimeDirectory()
    {
        return RuntimeEnvironment.GetRuntimeDirectory();
    }

    /// <summary>
    /// Gets the full path to a specific assembly in the runtime directory.
    /// </summary>
    /// <param name="assemblyName">The name of the assembly (e.g., "mscorlib.dll").</param>
    /// <returns>
    /// A <see cref="System.String"/> representing the full path to the assembly.
    /// </returns>
    public static string GetRuntimeAssemblyPath(string assemblyName)
    {
        if (string.IsNullOrWhiteSpace(assemblyName))
            throw new ArgumentException("Value cannot be null or whitespace.", nameof(assemblyName));
        return Path.Combine(RuntimeEnvironment.GetRuntimeDirectory(), assemblyName);
    }

    /// <summary>
    /// Retrieves the runtime directory of the current .NET installation
    /// and enumerates all the .dll files within it and its subdirectories.
    /// </summary>
    /// <param name="assemblies">
    /// When this method returns, contains an <see cref="IEnumerable{T}"/> of <see cref="string"/> objects
    /// representing the full paths to all the .dll files found in the .NET runtime directory and its subdirectories.
    /// </param>
    /// <returns>
    /// A <see cref="string"/> representing the full path to the runtime directory.
    /// </returns>
    public static string GetRuntimeDirectory(out IEnumerable<string> assemblies)
    {
        var path = RuntimeEnvironment.GetRuntimeDirectory();
        assemblies = Directory.EnumerateFiles(path, "*.dll", SearchOption.AllDirectories);
        return path;
    }

    /// <summary>
    /// Gets a portable executable reference to a specific assembly in the runtime directory.
    /// </summary>
    /// <param name="assemblyName">The name of the assembly (e.g., "mscorlib.dll").</param>
    /// <returns>
    /// A <see cref="PortableExecutableReference"/> representing the reference to the assembly.
    /// </returns>
    public static PortableExecutableReference GetPortableExecutableReference(string assemblyName)
    {
        if (string.IsNullOrWhiteSpace(assemblyName))
            throw new ArgumentException("Value cannot be null or whitespace.", nameof(assemblyName));
        
        return MetadataReference.CreateFromFile(GetRuntimeAssemblyPath(assemblyName));
    }

    /// <summary>
    /// Retrieves a collection of portable executable references for runtime assemblies.
    /// </summary>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="PortableExecutableReference"/> containing the references to runtime assemblies.
    /// </returns>
    public static IEnumerable<PortableExecutableReference> GetPortableExecutableReferences()
    {
        _ = GetRuntimeDirectory(out var assemblies);
        foreach (var assembly in assemblies)
            yield return MetadataReference.CreateFromFile(assembly);
    }

    /// <summary>
    /// Retrieves a collection of portable executable references for runtime assemblies based on a specified filter.
    /// </summary>
    /// <param name="assemblyPathFilter">
    /// A function that determines whether an assembly should be included in the collection based on its path.
    /// </param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="PortableExecutableReference"/> containing the references to runtime assemblies
    /// that match the filter criteria.
    /// </returns>
    public static IEnumerable<PortableExecutableReference> GetPortableExecutableReferences(
        Func<string, bool> assemblyPathFilter)
    {
        _ = GetRuntimeDirectory(out var assemblies);
        foreach (var assembly in assemblies)
        {
            var condition = assemblyPathFilter(assembly);
            if (condition)
                yield return MetadataReference.CreateFromFile(assembly);
        }
    }

    /// <summary>
    /// Compiles C# code and returns the resulting assembly.
    /// </summary>
    /// <param name="sourceCode">The C# code to compile.</param>
    /// <param name="metadataReferences">Optional: An array of additional <see cref="MetadataReference"/> objects to include in the compilation.</param>
    /// <param name="includeAllRuntimeMetadataReferences">Indicates whether to include all .NET runtime metadata references.</param>
    /// <returns>
    /// An <see cref="Assembly"/> representing the compiled code.
    /// </returns>
    /// <exception cref="InvalidOperationException">
    /// Thrown if the compilation fails. The exception message contains compilation error details.
    /// </exception>
    public static Assembly ConvertToAssembly(string sourceCode, MetadataReference[]? metadataReferences = null,
        bool includeAllRuntimeMetadataReferences = true)
    {
        var syntaxTree = CSharpSyntaxTree.ParseText(sourceCode);
        var assemblyName = Path.GetRandomFileName();
        var references = new List<MetadataReference>();

        if (includeAllRuntimeMetadataReferences)
        {
            references.AddRange(GetPortableExecutableReferences());
        }

        if (metadataReferences != null)
        {
            references.AddRange(metadataReferences);
        }

        var compilation = CSharpCompilation.Create(
            assemblyName,
            syntaxTrees: new[] {syntaxTree},
            references: references.Distinct(),
            options: new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var assemblyStream = new MemoryStream();
        var emitResult = compilation.Emit(assemblyStream);

        if (!emitResult.Success)
        {
            var errorMessages = new StringBuilder();
            foreach (var diagnostic in emitResult.Diagnostics)
            {
                errorMessages.AppendLine(diagnostic.ToString());
            }

            throw new InvalidOperationException($"Compilation failed:{Environment.NewLine}{errorMessages}");
        }

        assemblyStream.Seek(0, SeekOrigin.Begin);
        var assembly = Assembly.Load(assemblyStream.ToArray());
        return assembly;
    }

    /// <summary>
    /// Compiles C# code and returns the resulting assembly.
    /// </summary>
    /// <param name="sourceCode">The C# code to compile.</param>
    /// <param name="assemblyFilePaths">Optional: An array of file paths to additional assemblies for reference.</param>
    /// <param name="includeAllRuntimeMetadataReferences">Indicates whether to include all .NET runtime metadata references.</param>
    /// <returns>
    /// An <see cref="Assembly"/> representing the compiled code.
    /// </returns>
    /// <exception cref="InvalidOperationException">
    /// Thrown if the compilation fails. The exception message contains compilation error details.
    /// </exception>
    public static Assembly ConvertToAssembly(string sourceCode, string[]? assemblyFilePaths = null,
        bool includeAllRuntimeMetadataReferences = true)
    {
        var syntaxTree = CSharpSyntaxTree.ParseText(sourceCode);
        var assemblyName = Path.GetRandomFileName();
        var references = new List<MetadataReference>();

        if (includeAllRuntimeMetadataReferences)
        {
            references.AddRange(GetPortableExecutableReferences());
        }

        if (assemblyFilePaths != null)
        {
            foreach (var assemblyPath in assemblyFilePaths)
            {
                if (File.Exists(assemblyPath))
                {
                    references.Add(MetadataReference.CreateFromFile(assemblyPath));
                }
            }
        }

        var compilation = CSharpCompilation.Create(
            assemblyName,
            syntaxTrees: new[] {syntaxTree},
            references: references.Distinct(),
            options: new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var assemblyStream = new MemoryStream();
        var emitResult = compilation.Emit(assemblyStream);

        if (!emitResult.Success)
        {
            var errorMessages = new StringBuilder();
            foreach (var diagnostic in emitResult.Diagnostics)
            {
                errorMessages.AppendLine(diagnostic.ToString());
            }

            throw new InvalidOperationException($"Compilation failed:{Environment.NewLine}{errorMessages}");
        }

        assemblyStream.Seek(0, SeekOrigin.Begin);
        var assembly = Assembly.Load(assemblyStream.ToArray());
        return assembly;
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
        string inputCode,
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
    /// Creates a metadata reference for an assembly located in the current .NET runtime directory.
    /// </summary>
    /// <param name="assemblyName">The name of the assembly.</param>
    /// <returns>A metadata reference for the specified assembly.</returns>
    public static MetadataReference CreateMetadataReferenceFromRuntime(string assemblyName)
    {
        if (string.IsNullOrWhiteSpace(assemblyName))
            throw new ArgumentException("Value cannot be null or whitespace.", nameof(assemblyName));
        
        var assemblyPath = Path.Combine(RuntimeEnvironment.GetRuntimeDirectory(), assemblyName);
        if (!File.Exists(assemblyPath))
        {
            throw new FileNotFoundException($"The file '{assemblyPath}' does not exist.");
        }
        return MetadataReference.CreateFromFile(assemblyPath);
    }
}