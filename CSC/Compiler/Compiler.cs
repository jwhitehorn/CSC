using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;

namespace CSC.Compiler {
    public class Compiler {

        private CSharpListenerImpl _listener;

        public Compiler() {
            _listener = new CSharpListenerImpl();
        }


        public void Compile(string src) {
            var inputStream = new AntlrInputStream(src);
            CSharpLexer lexer = new CSharpLexer(inputStream);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            CSharpParser parser = new CSharpParser(tokens);

            IParseTree tree = parser.source_file();
            ParseTreeWalker walker = new ParseTreeWalker();

            walker.Walk(_listener, tree);
        }

        public Assembly Finish() {
            return _listener.Finish();
        }

        public object Run(string[] args) {
            var program = this.Finish();
            var entryClassType = program.GetType("Program");
            return entryClassType.InvokeMember("Main",
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

    }
}
