using System;
using System.IO;
using System.Windows.Forms;

using GKCommon;

namespace GKSandbox
{
    public static class SerializeTests
    {
        public static void TestSerializes()
        {
            var person = new Person() { Name = "test", BirthDay = "bd" };

            /*var json = JsonHelper.SerializeObject(person);
            using (var writer = new StreamWriter("test.json")) {
                writer.WriteLine(json);
            }*/

            var yaml = YamlHelper.SerializeObject(person);
            using (var writer = new StreamWriter("test.yaml")) {
                writer.WriteLine(yaml);
            }

            using (var reader = new StreamReader("test2.yaml")) {
                var content = reader.ReadToEnd();
                var obj = YamlHelper.DeserializeObject(content);

                var obj2 = YamlHelper.DeserializeObject(content, typeof(Person));
                obj2 = obj2;
            }
        }

        public class Person
        {
            public string Name;
            public string BirthDay;
        }
    }
}
