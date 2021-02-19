//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     ANTLR Version: 4.9.1
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

// Generated from CSharp.g4 by ANTLR 4.9.1

// Unreachable code detected
#pragma warning disable 0162
// The variable '...' is assigned but its value is never used
#pragma warning disable 0219
// Missing XML comment for publicly visible type or member '...'
#pragma warning disable 1591
// Ambiguous reference in cref attribute
#pragma warning disable 419

using Antlr4.Runtime.Misc;
using IParseTreeListener = Antlr4.Runtime.Tree.IParseTreeListener;
using IToken = Antlr4.Runtime.IToken;

/// <summary>
/// This interface defines a complete listener for a parse tree produced by
/// <see cref="CSharpParser"/>.
/// </summary>
[System.CodeDom.Compiler.GeneratedCode("ANTLR", "4.9.1")]
[System.CLSCompliant(false)]
public interface ICSharpListener : IParseTreeListener {
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.source_file"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterSource_file([NotNull] CSharpParser.Source_fileContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.source_file"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitSource_file([NotNull] CSharpParser.Source_fileContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.root_expr"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterRoot_expr([NotNull] CSharpParser.Root_exprContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.root_expr"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitRoot_expr([NotNull] CSharpParser.Root_exprContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.using_statement"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterUsing_statement([NotNull] CSharpParser.Using_statementContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.using_statement"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitUsing_statement([NotNull] CSharpParser.Using_statementContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.namespace_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterNamespace_declaration([NotNull] CSharpParser.Namespace_declarationContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.namespace_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitNamespace_declaration([NotNull] CSharpParser.Namespace_declarationContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.qualified_identifier"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterQualified_identifier([NotNull] CSharpParser.Qualified_identifierContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.qualified_identifier"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitQualified_identifier([NotNull] CSharpParser.Qualified_identifierContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.namespace_body"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterNamespace_body([NotNull] CSharpParser.Namespace_bodyContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.namespace_body"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitNamespace_body([NotNull] CSharpParser.Namespace_bodyContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.namespace_member_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterNamespace_member_declaration([NotNull] CSharpParser.Namespace_member_declarationContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.namespace_member_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitNamespace_member_declaration([NotNull] CSharpParser.Namespace_member_declarationContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.type_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterType_declaration([NotNull] CSharpParser.Type_declarationContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.type_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitType_declaration([NotNull] CSharpParser.Type_declarationContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.class_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterClass_declaration([NotNull] CSharpParser.Class_declarationContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.class_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitClass_declaration([NotNull] CSharpParser.Class_declarationContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.class_base"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterClass_base([NotNull] CSharpParser.Class_baseContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.class_base"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitClass_base([NotNull] CSharpParser.Class_baseContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.class_body"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterClass_body([NotNull] CSharpParser.Class_bodyContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.class_body"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitClass_body([NotNull] CSharpParser.Class_bodyContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.class_member_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterClass_member_declaration([NotNull] CSharpParser.Class_member_declarationContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.class_member_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitClass_member_declaration([NotNull] CSharpParser.Class_member_declarationContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.method_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterMethod_declaration([NotNull] CSharpParser.Method_declarationContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.method_declaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitMethod_declaration([NotNull] CSharpParser.Method_declarationContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.method_body"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterMethod_body([NotNull] CSharpParser.Method_bodyContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.method_body"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitMethod_body([NotNull] CSharpParser.Method_bodyContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.block"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterBlock([NotNull] CSharpParser.BlockContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.block"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitBlock([NotNull] CSharpParser.BlockContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.statement_list"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterStatement_list([NotNull] CSharpParser.Statement_listContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.statement_list"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitStatement_list([NotNull] CSharpParser.Statement_listContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.statement"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterStatement([NotNull] CSharpParser.StatementContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.statement"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitStatement([NotNull] CSharpParser.StatementContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.method_header"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterMethod_header([NotNull] CSharpParser.Method_headerContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.method_header"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitMethod_header([NotNull] CSharpParser.Method_headerContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.formal_parameter_list"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterFormal_parameter_list([NotNull] CSharpParser.Formal_parameter_listContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.formal_parameter_list"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitFormal_parameter_list([NotNull] CSharpParser.Formal_parameter_listContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.fixed_parameters"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterFixed_parameters([NotNull] CSharpParser.Fixed_parametersContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.fixed_parameters"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitFixed_parameters([NotNull] CSharpParser.Fixed_parametersContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.fixed_parameter"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterFixed_parameter([NotNull] CSharpParser.Fixed_parameterContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.fixed_parameter"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitFixed_parameter([NotNull] CSharpParser.Fixed_parameterContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.member_name"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterMember_name([NotNull] CSharpParser.Member_nameContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.member_name"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitMember_name([NotNull] CSharpParser.Member_nameContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.return_type"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterReturn_type([NotNull] CSharpParser.Return_typeContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.return_type"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitReturn_type([NotNull] CSharpParser.Return_typeContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.type"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterType([NotNull] CSharpParser.TypeContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.type"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitType([NotNull] CSharpParser.TypeContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.method_modifier"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterMethod_modifier([NotNull] CSharpParser.Method_modifierContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.method_modifier"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitMethod_modifier([NotNull] CSharpParser.Method_modifierContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="CSharpParser.class_modifier"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterClass_modifier([NotNull] CSharpParser.Class_modifierContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="CSharpParser.class_modifier"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitClass_modifier([NotNull] CSharpParser.Class_modifierContext context);
}
