using System;
using System.IO;

namespace CSC {
    class Program {
        static void Main(string[] args) {
            if (args.Length < 1) {
                Console.WriteLine("Please specify the name of a source file");
                return;
            }
            string inputFile = args[0];
            string[] lines;
            try {
                lines = File.ReadAllLines(inputFile);
            } catch (FileNotFoundException) {
                Console.WriteLine($"ERROR: Unable to open file {inputFile} for reading");
                return;
            }
            var src = String.Join('\n', lines);

            var compiler = new Compiler.Compiler();
            compiler.Compile(src);
            //compiler.Finish("test");
            compiler.Run(new string[] { });
        }
    }
}
