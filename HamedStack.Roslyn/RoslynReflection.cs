// ReSharper disable IdentifierTypo
// ReSharper disable UnusedMember.Global

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace HamedStack.Roslyn;

/// <summary>
/// Provides reflection-like capabilities for analyzing C# code using the Roslyn API.
/// </summary>
public class RoslynReflection
{
    private readonly SemanticModel _semanticModel;
    private readonly SyntaxTree _syntaxTree;

    /// <summary>
    /// Initializes a new instance of the <see cref="RoslynReflection"/> class.
    /// </summary>
    /// <param name="code">The C# code to analyze.</param>
    protected RoslynReflection(string code)
    {
        _syntaxTree = CSharpSyntaxTree.ParseText(code);
        var compilation = CSharpCompilation.Create("MyCompilation" + DateTime.UtcNow.Ticks,
            new[] { _syntaxTree },
            new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) });
        _semanticModel = compilation.GetSemanticModel(_syntaxTree);
    }

    /// <summary>
    /// Gets the named class from the analyzed code.
    /// </summary>
    /// <param name="className">The name of the class to retrieve.</param>
    /// <returns>The <see cref="INamedTypeSymbol"/> representing the class, or null if not found.</returns>
    public INamedTypeSymbol? GetClass(string className)
    {
        return _syntaxTree.GetRoot().DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Where(c => c.Identifier.Text == className)
            .Select(c => _semanticModel.GetDeclaredSymbol(c))
            .FirstOrDefault();
    }

    /// <summary>
    /// Gets all classes defined in the analyzed code.
    /// </summary>
    /// <returns>An IEnumerable of <see cref="INamedTypeSymbol"/> representing all classes.</returns>
    public IEnumerable<INamedTypeSymbol?> GetClasses()
    {
        return _syntaxTree.GetRoot().DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Select(c => _semanticModel.GetDeclaredSymbol(c));
    }

    /// <summary>
    /// Gets a property by name from a class.
    /// </summary>
    /// <param name="className">The name of the class containing the property.</param>
    /// <param name="propertyName">The name of the property to retrieve.</param>
    /// <returns>The <see cref="IPropertySymbol"/> representing the property, or null if not found.</returns>
    public IPropertySymbol? GetProperty(string className, string propertyName)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IPropertySymbol>().FirstOrDefault(p => p.Name == propertyName);
    }

    /// <summary>
    /// Gets all properties from a class.
    /// </summary>
    /// <param name="className">The name of the class to retrieve properties from.</param>
    /// <returns>An IEnumerable of <see cref="IPropertySymbol"/> representing all properties in the class.</returns>
    public IEnumerable<IPropertySymbol>? GetProperties(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IPropertySymbol>();
    }

    /// <summary>
    /// Gets a field by name from a class.
    /// </summary>
    /// <param name="className">The name of the class containing the field.</param>
    /// <param name="fieldName">The name of the field to retrieve.</param>
    /// <returns>The <see cref="IFieldSymbol"/> representing the field, or null if not found.</returns>
    public IFieldSymbol? GetField(string className, string fieldName)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IFieldSymbol>().FirstOrDefault(f => f.Name == fieldName);
    }

    /// <summary>
    /// Gets all fields from a class.
    /// </summary>
    /// <param name="className">The name of the class to retrieve fields from.</param>
    /// <returns>An IEnumerable of <see cref="IFieldSymbol"/> representing all fields in the class.</returns>
    public IEnumerable<IFieldSymbol>? GetFields(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IFieldSymbol>();
    }

    /// <summary>
    /// Gets a member by name from a class.
    /// </summary>
    /// <param name="className">The name of the class containing the member.</param>
    /// <param name="memberName">The name of the member to retrieve.</param>
    /// <returns>The <see cref="ISymbol"/> representing the member, or null if not found.</returns>
    public ISymbol? GetMember(string className, string memberName)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().FirstOrDefault(m => m.Name == memberName);
    }

    /// <summary>
    /// Gets all members from a class.
    /// </summary>
    /// <param name="className">The name of the class to retrieve members from.</param>
    /// <returns>An IEnumerable of <see cref="ISymbol"/> representing all members in the class.</returns>
    public IEnumerable<ISymbol>? GetMembers(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers();
    }

    /// <summary>
    /// Gets all attributes applied to a member within a class.
    /// </summary>
    /// <param name="className">The name of the class containing the member.</param>
    /// <param name="memberName">The name of the member to retrieve attributes from.</param>
    /// <returns>An IEnumerable of <see cref="AttributeData"/> representing the attributes.</returns>
    public IEnumerable<AttributeData>? GetAttributes(string className, string memberName)
    {
        var memberSymbol = GetMember(className, memberName);
        return memberSymbol?.GetAttributes();
    }

    /// <summary>
    /// Gets a specific attribute applied to a member within a class.
    /// </summary>
    /// <param name="className">The name of the class containing the member.</param>
    /// <param name="memberName">The name of the member to retrieve the attribute from.</param>
    /// <param name="attributeName">The name of the attribute to retrieve.</param>
    /// <returns>The <see cref="AttributeData"/> representing the attribute, or null if not found.</returns>
    public AttributeData? GetAttribute(string className, string memberName, string attributeName)
    {
        return GetAttributes(className, memberName)?.FirstOrDefault(a => a.AttributeClass?.Name == attributeName);
    }

    /// <summary>
    /// Retrieves a method symbol by class name and method name.
    /// </summary>
    /// <param name="className">The name of the class containing the method.</param>
    /// <param name="methodName">The name of the method to retrieve.</param>
    /// <returns>
    /// The <see cref="IMethodSymbol"/> representing the method if found; otherwise, <c>null</c>.
    /// </returns>
    public IMethodSymbol? GetMethod(string className, string methodName)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IMethodSymbol>().FirstOrDefault(m => m.Name == methodName);
    }

    /// <summary>
    /// Retrieves all method symbols within a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class containing the methods.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="IMethodSymbol"/> representing all methods in the class if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<IMethodSymbol>? GetMethods(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IMethodSymbol>();
    }

    /// <summary>
    /// Retrieves a specific event symbol within a class by its name.
    /// </summary>
    /// <param name="className">The name of the class containing the event.</param>
    /// <param name="eventName">The name of the event to retrieve.</param>
    /// <returns>
    /// The <see cref="IEventSymbol"/> representing the event if found; otherwise, <c>null</c>.
    /// </returns>
    public IEventSymbol? GetEvent(string className, string eventName)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IEventSymbol>().FirstOrDefault(e => e.Name == eventName);
    }

    /// <summary>
    /// Retrieves all event symbols within a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class containing the events.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="IEventSymbol"/> representing all events in the class if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<IEventSymbol>? GetEvents(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IEventSymbol>();
    }

    /// <summary>
    /// Retrieves the constructor symbol of a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class for which to retrieve the constructor.</param>
    /// <returns>
    /// The <see cref="IMethodSymbol"/> representing the constructor if found; otherwise, <c>null</c>.
    /// </returns>
    public IMethodSymbol? GetConstructor(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.Constructors.FirstOrDefault();
    }

    /// <summary>
    /// Retrieves all constructor symbols within a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class containing the constructors.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="IMethodSymbol"/> representing all constructors in the class if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<IMethodSymbol>? GetConstructors(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.Constructors;
    }

    /// <summary>
    /// Retrieves a specific interface symbol implemented by a class by the class name and interface name.
    /// </summary>
    /// <param name="className">The name of the class implementing the interface.</param>
    /// <param name="interfaceName">The name of the interface to retrieve.</param>
    /// <returns>
    /// The <see cref="INamedTypeSymbol"/> representing the interface if found; otherwise, <c>null</c>.
    /// </returns>
    public INamedTypeSymbol? GetInterface(string className, string interfaceName)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.AllInterfaces.FirstOrDefault(i => i.Name == interfaceName);
    }

    /// <summary>
    /// Retrieves all interface symbols implemented by a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class implementing the interfaces.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="INamedTypeSymbol"/> representing all interfaces implemented by the class if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<INamedTypeSymbol>? GetInterfaces(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.AllInterfaces;
    }

    /// <summary>
    /// Retrieves a specific delegate symbol within a class by its name.
    /// </summary>
    /// <param name="className">The name of the class containing the delegate.</param>
    /// <param name="delegateName">The name of the delegate to retrieve.</param>
    /// <returns>
    /// The <see cref="INamedTypeSymbol"/> representing the delegate if found; otherwise, <c>null</c>.
    /// </returns>
    public INamedTypeSymbol? GetDelegate(string className, string delegateName)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetTypeMembers().FirstOrDefault(t => t.TypeKind == TypeKind.Delegate && t.Name == delegateName);
    }

    /// <summary>
    /// Retrieves all delegate symbols within a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class containing the delegates.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="INamedTypeSymbol"/> representing all delegates in the class if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<INamedTypeSymbol>? GetDelegates(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetTypeMembers().Where(t => t.TypeKind == TypeKind.Delegate);
    }

    /// <summary>
    /// Retrieves a specific nested type symbol within a class by its name.
    /// </summary>
    /// <param name="className">The name of the class containing the nested type.</param>
    /// <param name="nestedTypeName">The name of the nested type to retrieve.</param>
    /// <returns>
    /// The <see cref="INamedTypeSymbol"/> representing the nested type if found; otherwise, <c>null</c>.
    /// </returns>
    public INamedTypeSymbol? GetNestedType(string className, string nestedTypeName)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetTypeMembers(nestedTypeName).FirstOrDefault();
    }

    /// <summary>
    /// Retrieves all nested type symbols within a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class containing the nested types.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="INamedTypeSymbol"/> representing all nested types in the class if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<INamedTypeSymbol>? GetNestedTypes(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetTypeMembers();
    }

    /// <summary>
    /// Retrieves the return type of a specific method within a class by the class name and method name.
    /// </summary>
    /// <param name="className">The name of the class containing the method.</param>
    /// <param name="methodName">The name of the method to retrieve the return type for.</param>
    /// <returns>
    /// The <see cref="ITypeSymbol"/> representing the return type of the method if found; otherwise, <c>null</c>.
    /// </returns>
    public ITypeSymbol? GetMethodReturnType(string className, string methodName)
    {
        var methodSymbol = GetMethod(className, methodName);
        return methodSymbol?.ReturnType;
    }

    /// <summary>
    /// Retrieves the parameters of a specific method within a class by the class name and method name.
    /// </summary>
    /// <param name="className">The name of the class containing the method.</param>
    /// <param name="methodName">The name of the method to retrieve parameters for.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="IParameterSymbol"/> representing the parameters of the method if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<IParameterSymbol>? GetMethodParameters(string className, string methodName)
    {
        var methodSymbol = GetMethod(className, methodName);
        return methodSymbol?.Parameters;
    }

    /// <summary>
    /// Retrieves the members of an enum by the class name.
    /// </summary>
    /// <param name="className">The name of the class representing an enum.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="ISymbol"/> representing the enum members if the class is an enum; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<ISymbol>? GetEnumMembers(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.TypeKind == TypeKind.Enum ? classSymbol.GetMembers().Where(m => m.Kind == SymbolKind.Field) : null;
    }

    /// <summary>
    /// Retrieves all methods declared in a specific interface implemented by a class by the class name and interface name.
    /// </summary>
    /// <param name="className">The name of the class implementing the interface.</param>
    /// <param name="interfaceName">The name of the interface containing the methods.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="IMethodSymbol"/> representing all methods in the interface if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<IMethodSymbol>? GetInterfaceMethods(string className, string interfaceName)
    {
        var interfaceSymbol = GetInterface(className, interfaceName);
        return interfaceSymbol?.GetMembers().OfType<IMethodSymbol>();
    }

    /// <summary>
    /// Retrieves the generic type parameters of a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class containing the generic type parameters.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="ITypeParameterSymbol"/> representing the generic type parameters of the class if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<ITypeParameterSymbol>? GetGenericTypeParameters(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.TypeParameters;
    }

    /// <summary>
    /// Retrieves the base class of a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class for which to retrieve the base class.</param>
    /// <returns>
    /// The <see cref="INamedTypeSymbol"/> representing the base class if found; otherwise, <c>null</c>.
    /// </returns>
    public INamedTypeSymbol? GetBaseClass(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.BaseType;
    }

    /// <summary>
    /// Retrieves the type of a specific property within a class by the class name and property name.
    /// </summary>
    /// <param name="className">The name of the class containing the property.</param>
    /// <param name="propertyName">The name of the property to retrieve the type for.</param>
    /// <returns>
    /// The <see cref="ITypeSymbol"/> representing the type of the property if found; otherwise, <c>null</c>.
    /// </returns>
    public ITypeSymbol? GetPropertyType(string className, string propertyName)
    {
        var propertySymbol = GetProperty(className, propertyName);
        return propertySymbol?.Type;
    }

    /// <summary>
    /// Retrieves the type of a specific field within a class by the class name and field name.
    /// </summary>
    /// <param name="className">The name of the class containing the field.</param>
    /// <param name="fieldName">The name of the field to retrieve the type for.</param>
    /// <returns>
    /// The <see cref="ITypeSymbol"/> representing the type of the field if found; otherwise, <c>null</c>.
    /// </returns>
    public ITypeSymbol? GetFieldType(string className, string fieldName)
    {
        var fieldSymbol = GetField(className, fieldName);
        return fieldSymbol?.Type;
    }

    /// <summary>
    /// Retrieves all operator overloads declared in a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class containing the operator overloads.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="IMethodSymbol"/> representing all operator overloads in the class if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<IMethodSymbol>? GetOperatorOverloads(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IMethodSymbol>().Where(m => m.MethodKind == MethodKind.UserDefinedOperator);
    }

    /// <summary>
    /// Retrieves all methods explicitly implemented by a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class containing explicit interface implementations.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="IMethodSymbol"/> representing all methods explicitly implemented in the class if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<IMethodSymbol>? GetExplicitInterfaceImplementations(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IMethodSymbol>().Where(m => m.ExplicitInterfaceImplementations.Any());
    }

    /// <summary>
    /// Retrieves all methods implicitly implemented by a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class containing implicit interface implementations.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="IMethodSymbol"/> representing all methods implicitly implemented in the class if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<IMethodSymbol>? GetImplicitInterfaceImplementations(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IMethodSymbol>().Where(m => !m.ExplicitInterfaceImplementations.Any() && m.ContainingType.AllInterfaces.Any(i => i.GetMembers().Contains(m)));
    }

    /// <summary>
    /// Retrieves all attributes applied to a class by the class name.
    /// </summary>
    /// <param name="className">The name of the class for which to retrieve attributes.</param>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="AttributeData"/> representing all attributes applied to the class if found; otherwise, <c>null</c>.
    /// </returns>
    public IEnumerable<AttributeData>? GetClassAttributes(string className)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetAttributes();
    }

    /// <summary>
    /// Retrieves a specific attribute applied to a class by the class name and attribute name.
    /// </summary>
    /// <param name="className">The name of the class for which to retrieve the attribute.</param>
    /// <param name="attributeName">The name of the attribute to retrieve.</param>
    /// <returns>
    /// The <see cref="AttributeData"/> representing the attribute if found; otherwise, <c>null</c>.
    /// </returns>
    public AttributeData? GetClassAttribute(string className, string attributeName)
    {
        return GetClassAttributes(className)?.FirstOrDefault(a => a.AttributeClass?.Name == attributeName);
    }

    /// <summary>
    /// Retrieves a specific struct symbol by its name.
    /// </summary>
    /// <param name="structName">The name of the struct to retrieve.</param>
    /// <returns>
    /// The <see cref="INamedTypeSymbol"/> representing the struct if found; otherwise, <c>null</c>.
    /// </returns>
    public INamedTypeSymbol? GetStruct(string structName)
    {
        return _syntaxTree.GetRoot().DescendantNodes()
            .OfType<StructDeclarationSyntax>()
            .Where(s => s.Identifier.Text == structName)
            .Select(s => _semanticModel.GetDeclaredSymbol(s))
            .FirstOrDefault();
    }

    /// <summary>
    /// Retrieves all struct symbols in the syntax tree.
    /// </summary>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="INamedTypeSymbol"/> representing all structs in the syntax tree.
    /// </returns>
    public IEnumerable<INamedTypeSymbol?> GetStructs()
    {
        return _syntaxTree.GetRoot().DescendantNodes()
            .OfType<StructDeclarationSyntax>()
            .Select(s => _semanticModel.GetDeclaredSymbol(s));
    }

    /// <summary>
    /// Retrieves a specific interface symbol declaration by its name.
    /// </summary>
    /// <param name="interfaceName">The name of the interface to retrieve.</param>
    /// <returns>
    /// The <see cref="INamedTypeSymbol"/> representing the interface declaration if found; otherwise, <c>null</c>.
    /// </returns>
    public INamedTypeSymbol? GetInterfaceDeclaration(string interfaceName)
    {
        return _syntaxTree.GetRoot().DescendantNodes()
            .OfType<InterfaceDeclarationSyntax>()
            .Where(i => i.Identifier.Text == interfaceName)
            .Select(i => _semanticModel.GetDeclaredSymbol(i))
            .FirstOrDefault();
    }

    /// <summary>
    /// Retrieves all interface symbol declarations in the syntax tree.
    /// </summary>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of <see cref="INamedTypeSymbol"/> representing all interface declarations in the syntax tree.
    /// </returns>
    public IEnumerable<INamedTypeSymbol?> GetInterfaceDeclarations()
    {
        return _syntaxTree.GetRoot().DescendantNodes()
            .OfType<InterfaceDeclarationSyntax>()
            .Select(i => _semanticModel.GetDeclaredSymbol(i));
    }

    /// <summary>
    /// Gets the accessibility level of a member within a class.
    /// </summary>
    /// <param name="className">The name of the class containing the member.</param>
    /// <param name="memberName">The name of the member to retrieve.</param>
    /// <returns>The <see cref="Accessibility"/> level of the member, or null if not found.</returns>
    public Accessibility? GetMemberAccessibility(string className, string memberName)
    {
        var memberSymbol = GetMember(className, memberName);
        return memberSymbol?.DeclaredAccessibility;
    }

    /// <summary>
    /// Gets the methods within a class that have a specified return type.
    /// </summary>
    /// <param name="className">The name of the class to search in.</param>
    /// <param name="returnType">The name of the return type to filter methods by.</param>
    /// <returns>An IEnumerable of <see cref="IMethodSymbol"/> representing matching methods.</returns>
    public IEnumerable<IMethodSymbol>? GetMethodsByReturnType(string className, string returnType)
    {
        var classSymbol = GetClass(className);
        return classSymbol?.GetMembers().OfType<IMethodSymbol>().Where(m => m.ReturnType.Name == returnType);
    }
}