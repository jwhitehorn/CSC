using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using Antlr4.Runtime.Misc;

namespace CSC.Compiler {

    public class CSharpListenerImpl : CSharpBaseListener {

        private AssemblyBuilder _asmBuilder;
        private ModuleBuilder _modBuilder;
        private TypeBuilder _typeBuilder;
        private MethodBuilder _methodBuilder;
        private MethodDefinitionHandle _entryPoint;

        private List<string> _usingStatements;
        private List<Type> _definedTypes;

        public CSharpListenerImpl() {
            _asmBuilder = AssemblyBuilder.DefineDynamicAssembly(
                    new AssemblyName(Guid.NewGuid().ToString()),
                    AssemblyBuilderAccess.Run
                );

            _modBuilder = _asmBuilder.DefineDynamicModule("MainModule");
            _definedTypes = new List<Type>();
        }

        public override void EnterSource_file(CSharpParser.Source_fileContext ctx) {
            _usingStatements = new List<string>();
        }

        public override void ExitUsing_statement(CSharpParser.Using_statementContext ctx) {
            string namesp = ctx.qualified_identifier().GetText();
            _usingStatements.Add(namesp);
        }

        public override void EnterNamespace_declaration(CSharpParser.Namespace_declarationContext ctx) {
            string namesp = ctx.qualified_identifier().GetText();
            _usingStatements.Add(namesp);
        }

        public override void ExitNamespace_declaration(CSharpParser.Namespace_declarationContext ctx) {
            string namesp = ctx.qualified_identifier().GetText();
            _usingStatements.Remove(namesp);
        }

        public override void EnterClass_declaration(CSharpParser.Class_declarationContext ctx) {
            string typeName = ctx.IDENTIFIER().GetText();
            TypeAttributes flags = TypeAttributes.Class;
            Type parentType = typeof(Object); // TODO use class_base

            var modifiers = ctx.class_modifier();
            foreach(var modifier in modifiers) {
                var modifierText = modifier.GetText();
                switch (modifierText) {
                    case "public":
                        flags |= TypeAttributes.Public;
                        break;
                    case "static":
                        flags |= TypeAttributes.Sealed | TypeAttributes.Abstract;
                        break;
                    case "abstract":
                        flags |= TypeAttributes.Abstract;
                        break;
                    case "sealed":
                        flags |= TypeAttributes.Sealed;
                        break;

                }
            }

            _typeBuilder = _modBuilder.DefineType(typeName, flags, parentType);
        }

        public override void ExitClass_declaration(CSharpParser.Class_declarationContext ctx) {
            Type t = _typeBuilder.CreateType();
            _definedTypes.Add(t);
        }

        public override void ExitMethod_header(CSharpParser.Method_headerContext ctx) {
            string methodName = ctx.member_name().GetText();
            MethodAttributes flags = MethodAttributes.Private;
            // TODO: parameters & return type

            bool isOverride = false;
            foreach(var modifier in ctx.method_modifier()) {
                switch (modifier.GetText()) {
                    case "public":
                        flags |= MethodAttributes.Public;
                        break;
                    case "static":
                        flags |= MethodAttributes.Static;
                        break;
                    case "virtual":
                        flags |= MethodAttributes.Virtual;
                        break;
                    case "override":
                        isOverride = true;
                        break;
                    case "abstract":
                        flags |= MethodAttributes.Abstract;
                        break;
                }
            }
            _methodBuilder = _typeBuilder.DefineMethod(methodName, flags);
            if (isOverride) {
                throw new NotImplementedException("Override is not yet implemented");
            }
        }

        public override void ExitStatement(CSharpParser.StatementContext ctx) {
            ILGenerator il = _methodBuilder.GetILGenerator();

            //il.Emit(OpCodes.Ldtoken, Label);

            var str = ctx.STRING().GetText();
            str = str.Substring(1, str.Length - 2);
            il.Emit(OpCodes.Ldstr, str);

            var funcName = ctx.qualified_identifier().GetText();
            // TODO: don't hard code

            var methodInfo = typeof(Console).GetMethod("WriteLine", new Type[] { typeof(string) });

            il.EmitCall(OpCodes.Call, methodInfo, new Type[] { typeof(string) });
        }

        public override void ExitMethod_declaration(CSharpParser.Method_declarationContext ctx) {
            ILGenerator il = _methodBuilder.GetILGenerator();


            if (ctx.method_header().member_name().GetText() == "Main") {
                var token = _methodBuilder.MetadataToken;
                _entryPoint = (MethodDefinitionHandle)MetadataTokens.EntityHandle(token);
            }

            il.Emit(OpCodes.Ret);
        }

        public void Finish(string filename) {
            var generator = new Lokad.ILPack.AssemblyGenerator();

            generator.GenerateAssembly(_asmBuilder, filename);//, _entryPoint);
        }

        public Assembly Finish() {
            return _modBuilder.Assembly;
        }
    }
}
