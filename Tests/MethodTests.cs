using System;
using Xunit;
using CSC.Compiler;

namespace Tests {
    public class MethodTests {

        [Fact]
        public void CompileEmptyMethod() {
            var source = @"
                using System;

                namespace CSC {
                    class Program {
                        public void TestFunc(){}
                    }
                }
            ";

            var compiler = new Compiler();

            compiler.Compile(source);
            var assembly = compiler.Finish();

            Assert.NotNull(assembly);

            var programClassType = assembly.GetType("Program");
            Assert.NotNull(programClassType);

            var funcInfo = programClassType.GetMember("TestFunc");
            Assert.NotNull(funcInfo);
        }

        [Fact]
        public void CompileNullObjectMethod() {
            var source = @"
                using System;

                namespace CSC {
                    class Program {
                        public object TestFunc(){
                            return null;
                        }
                    }
                }
            ";

            var compiler = new Compiler();

            compiler.Compile(source);
            var assembly = compiler.Finish();

            Assert.NotNull(assembly);

            var programClassType = assembly.GetType("Program");
            Assert.NotNull(programClassType);

            var funcInfo = programClassType.GetMember("TestFunc");
            Assert.NotNull(funcInfo);

            var pClass = Activator.CreateInstance(programClassType);
            //pClass.
            
        }

    }
}
