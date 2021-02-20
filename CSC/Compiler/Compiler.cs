using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;

namespace CSC.Compiler {
    public class Compiler : IAntlrErrorListener<int>, IAntlrErrorListener<IToken> {

        private CSharpListenerImpl _listener;
        private List<string> _errors;

        public Compiler() {
            _listener = new CSharpListenerImpl();
            _errors = new List<string>();
        }


        public void Compile(string src) {
            var inputStream = new AntlrInputStream(src);
            CSharpLexer lexer = new CSharpLexer(inputStream);
            lexer.RemoveErrorListeners();
            lexer.AddErrorListener(this);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            CSharpParser parser = new CSharpParser(tokens);
            parser.RemoveErrorListeners();
            parser.AddErrorListener(this);

            IParseTree tree = parser.source_file();
            ParseTreeWalker walker = new ParseTreeWalker();

            walker.Walk(_listener, tree);
        }

        public Assembly Finish() {
            return _listener.Finish();
        }

        public object Run(string[] args) {
            return Run("Program", "Main", args);
        }

        public object Run(string klass, string method, string[] args) {
            var program = this.Finish();
            var entryClassType = program.GetType(klass);
            return entryClassType.InvokeMember(method,
                                                BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.InvokeMethod,
                                                null,
                                                null,
                                                args);
        }

        public void Finish(string filename) {
            _listener.Finish(filename + ".dll");

            // https://natemcmaster.com/blog/2017/12/21/netcore-primitives/
            var config = 
            @"{
              ""runtimeOptions"": {
                ""framework"": {
                  ""name"": ""Microsoft.NETCore.App"",
                  ""version"": ""3.1.0""
                }
                    }
                }";
            File.WriteAllText(filename + ".runtimeconfig.json", config);
        }


        public static object Eval(string src, string[] args = null) {
            var compiler = new Compiler();
            compiler.Compile(src);
            return compiler.Run(args);
        }

        public bool HasErrors {
            get {
                return _errors.Count > 0;
            }
        }

        void IAntlrErrorListener<int>.SyntaxError(TextWriter output, IRecognizer recognizer, int offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e) {
            _errors.Add(output.ToString());
        }

        void IAntlrErrorListener<IToken>.SyntaxError(TextWriter output, IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e) {
            _errors.Add(output.ToString());
        }
    }
}
