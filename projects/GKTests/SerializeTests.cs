using System;
using System.Collections.Generic;
using System.IO;
using GKCommon;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class SerializeTests
    {
        [Test]
        public void TestSerializes()
        {
            var dtx = new DateTime(2009, 01, 08);
            var person = new Person() { Name = "test test test", BirthDay = dtx };

            string yaml = YamlHelper.Serialize(person);
            using (var writer = new StreamWriter("test.yaml")) {
                writer.WriteLine(yaml);
            }

            using (var reader = new StreamReader("test.yaml")) {
                string content = reader.ReadToEnd();
                var obj = YamlHelper.Deserialize(content) as Dictionary<object, object>;
                Assert.IsNotNull(obj);
                Assert.AreEqual("test test test", obj["Name"]);
                Assert.AreEqual(dtx, obj["BirthDay"]);

                var obj2 = YamlHelper.Deserialize(content, typeof(Person));
                Assert.IsNotNull(obj2);
                var p2 = obj2[0] as Person;
                Assert.IsNotNull(p2);
                Assert.AreEqual("test test test", p2.Name);
                Assert.AreEqual(dtx, p2.BirthDay);
            }
        }

        public class Person
        {
            public string Name;
            public DateTime BirthDay;
        }
    }
}
