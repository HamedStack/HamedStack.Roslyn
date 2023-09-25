// ReSharper disable UnusedType.Global
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable UnusedMember.Global

using Microsoft.CodeAnalysis;

namespace HamedStack.Roslyn;

/// <summary>
/// Provides utility methods for working with Roslyn and .NET assemblies.
/// </summary>
public class RoslynUtility
{
    /// <summary>
    /// Gets the runtime directory where the .NET Framework assemblies are located.
    /// </summary>
    /// <returns>
    /// A <see cref="System.String"/> representing the runtime directory path.
    /// </returns>
    public static string GetRuntimeDirectory()
    {
        return System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory();
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
        return Path.Combine(System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory(), assemblyName);
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
        return MetadataReference.CreateFromFile(GetRuntimeAssemblyPath(assemblyName));
    }
}