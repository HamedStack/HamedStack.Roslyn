// ReSharper disable UnusedMember.Global
// ReSharper disable UnusedType.Global
// ReSharper disable MemberCanBePrivate.Global

using System.Collections.Immutable;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;

namespace HamedStack.Roslyn;

/// <summary>
/// A script engine that allows evaluation and execution of C# code at runtime.
/// </summary>
public class CSharpScriptEngine
{
    private ScriptOptions _options = ScriptOptions.Default;
    private ScriptState<object>? _scriptState;

    /// <summary>
    /// Adds a reference to the script options.
    /// </summary>
    /// <param name="reference">The reference to add.</param>
    /// <returns>The current instance of <see cref="CSharpScriptEngine"/>.</returns>
    public CSharpScriptEngine AddReference(string reference)
    {
        _options = _options.AddReferences(reference);
        return this;
    }

    /// <summary>
    /// Adds a namespace import to the script options.
    /// </summary>
    /// <param name="namespace">The namespace to import.</param>
    /// <returns>The current instance of <see cref="CSharpScriptEngine"/>.</returns>
    public CSharpScriptEngine AddUsing(string @namespace)
    {
        _options = _options.AddImports(@namespace);
        return this;
    }

    /// <summary>
    /// Asynchronously evaluates a given expression.
    /// </summary>
    /// <typeparam name="T">Type of the expected result.</typeparam>
    /// <param name="expression">Expression to evaluate.</param>
    /// <param name="globals">Optional parameter for globals used during script evaluation.</param>
    /// <returns>The evaluated result of type T.</returns>
    public async Task<T> EvaluateAsync<T>(string expression, object? globals = null)
    {
        return (T)await CSharpScript.EvaluateAsync(expression, _options, globals, typeof(T));
    }

    /// <summary>
    /// Executes a given script and returns its result.
    /// </summary>
    /// <typeparam name="T">Type of the expected result.</typeparam>
    /// <param name="script">Script to execute.</param>
    /// <param name="assemblies">Optional assemblies to reference during script execution.</param>
    /// <returns>The result of the script execution.</returns>
    public T? Execute<T>(string script, params Assembly[]? assemblies)
    {
        if (assemblies is { Length: > 0 })
        {
            var option = ScriptOptions.Default.WithReferences(assemblies);
            _scriptState = _scriptState == null
                ? CSharpScript.RunAsync(script, option).Result
                : _scriptState.ContinueWithAsync(script, option).Result;
        }
        else
            _scriptState = _scriptState == null
                ? CSharpScript.RunAsync(script).Result
                : _scriptState.ContinueWithAsync(script).Result;

        return (T?)_scriptState.ReturnValue;
    }

    /// <summary>
    /// Asynchronously executes a given code with optional timeout and globals.
    /// </summary>
    /// <typeparam name="T">Type of the expected result.</typeparam>
    /// <param name="code">Code to execute.</param>
    /// <param name="globals">Optional parameter for globals used during script execution.</param>
    /// <param name="timeout">Optional parameter to set a timeout for the script execution.</param>
    /// <returns>The result of the script execution.</returns>
    public async Task<T> ExecuteAsync<T>(string code, object? globals = null, TimeSpan? timeout = null)
    {
        using var memoryStream = new MemoryStream();
        await using var writer = new StreamWriter(memoryStream);
        using var reader = new StreamReader(memoryStream);
        Console.SetOut(writer);

        Task<ScriptState<object>> task = _scriptState == null ? CSharpScript.RunAsync(code, _options, globals) : _scriptState.ContinueWithAsync(code);

        if (timeout.HasValue)
        {
            var completedTask = await Task.WhenAny(task, Task.Delay(timeout.Value));
            if (completedTask == task)
            {
                _scriptState = await task;
            }
            else
            {
                throw new TimeoutException("Script execution timed out.");
            }
        }
        else
        {
            _scriptState = await task;
        }

        if (!task.IsCompleted) throw new TimeoutException("Script execution timed out.");

        memoryStream.Position = 0;
        var output = await reader.ReadToEndAsync();
        if (!string.IsNullOrEmpty(output))
        {
            Console.WriteLine("Script Output: " + output);
        }

        return (T)_scriptState.ReturnValue;
    }

    /// <summary>
    /// Asynchronously executes a script from a file.
    /// </summary>
    /// <typeparam name="T">Type of the expected result.</typeparam>
    /// <param name="filePath">Path to the file containing the script.</param>
    /// <param name="globals">Optional parameter for globals used during script execution.</param>
    /// <returns>The result of the script execution.</returns>
    public async Task<T> ExecuteFromFileAsync<T>(string filePath, object? globals = null)
    {
        if (!File.Exists(filePath))
        {
            throw new ArgumentException("File does not exist.", nameof(filePath));
        }

        var code = await File.ReadAllTextAsync(filePath);
        return await ExecuteAsync<T>(code, globals);
    }

    /// <summary>
    /// Gets compilation diagnostics for a given code.
    /// </summary>
    /// <param name="code">Code to check for diagnostics.</param>
    /// <returns>An array of diagnostics.</returns>
    public ImmutableArray<Diagnostic> GetDiagnostics(string code)
    {
        var script = CSharpScript.Create(code, _options);
        return script.Compile();
    }

    /// <summary>
    /// Resets the state of the engine.
    /// </summary>
    public void ResetState()
    {
        _scriptState = null;
    }

    /// <summary>
    /// Sets the optimization level for the script compilation.
    /// </summary>
    /// <param name="level">Optimization level to set.</param>
    /// <returns>Current instance with the updated optimization level.</returns>
    public CSharpScriptEngine SetOptimizationLevel(OptimizationLevel level)
    {
        _options.WithOptimizationLevel(level);
        return this;
    }
}