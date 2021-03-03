using System;
using Xunit;
using CSC.Compiler;
using System.Reflection;

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
                        object TestFunc(){
                            return null;
                        }
                    }
                }
            ";

            var compiler = new Compiler();

            compiler.Compile(source);
            var assembly = compiler.Finish();

            Assert.False(compiler.HasErrors);

            Assert.NotNull(assembly);

            var programClassType = assembly.GetType("Program");
            Assert.NotNull(programClassType);

            var funcInfo = programClassType.GetMember("TestFunc", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.InvokeMethod);
            Assert.NotNull(funcInfo);

            var result = compiler.Run("Program", "TestFunc", null);

            //var obj = Activator.CreateInstance(programClassType);
            //var func = programClassType.GetMethod("TestFunc");
            //var result = func.Invoke(obj, null);
            //var flags =  BindingFlags.Public | BindingFlags.InvokeMethod | BindingFlags.Instance;
            //var result = obj.GetType().InvokeMember("TestFunc", flags, null, obj, null);
            Assert.Null(result);
            
        }

    }
}
