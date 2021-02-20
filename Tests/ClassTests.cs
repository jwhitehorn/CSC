using System;
using Xunit;
using CSC.Compiler;

namespace Tests {
    public class ClassTests {

        [Fact]
        public void CompileEmptyClass() {
            var source = @"
                using System;

                namespace CSC {
                    class Program {
                    }
                }
            ";

            var compiler = new Compiler();

            compiler.Compile(source);
            Assert.False(compiler.HasErrors);

            var assembly = compiler.Finish();

            Assert.NotNull(assembly);

            var programClassType = assembly.GetType("Program");

            Assert.NotNull(programClassType);
        }

        [Fact]
        public void CompileEmptyStaticClass() {
            var source = @"
                using System;

                namespace CSC {
                    static class Program {
                    }
                }
            ";

            var compiler = new Compiler();

            compiler.Compile(source);
            Assert.False(compiler.HasErrors);

            var assembly = compiler.Finish();

            Assert.NotNull(assembly);

            var programClassType = assembly.GetType("Program");

            Assert.NotNull(programClassType);
            Assert.True(programClassType.IsSealed);
            Assert.True(programClassType.IsAbstract);
        }

        [Fact]
        public void CompileEmptyAbstractClass() {
            var source = @"
                using System;

                namespace CSC {
                    static class Program {
                    }
                }
            ";

            var compiler = new Compiler();

            compiler.Compile(source);
            Assert.False(compiler.HasErrors);

            var assembly = compiler.Finish();

            Assert.NotNull(assembly);

            var programClassType = assembly.GetType("Program");

            Assert.NotNull(programClassType);
            Assert.True(programClassType.IsAbstract);
        }


    }
}
