// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;

namespace GKCore.Utilities
{
    [TestFixture]
    public class YamlHelperTests
    {
        [Test]
        public void Test_Common()
        {
            var dtx = new DateTime(2009, 01, 08);
            var person = new Person() { Name = "test test test", BirthDay = dtx };

            string yaml = YamlHelper.Serialize(person);
            using (var writer = new StreamWriter("test.yaml")) {
                writer.WriteLine(yaml);
            }

            using (var reader = new StreamReader("test.yaml")) {
                string content = reader.ReadToEnd();
                var obj = YamlHelper.Deserialize<Dictionary<object, object>>(content);
                Assert.IsNotNull(obj);
                Assert.AreEqual("test test test", obj["Name"]);
                Assert.AreEqual("2009-01-08T00:00:00.0000000", obj["BirthDay"]);

                var p2 = YamlHelper.Deserialize<Person>(content);
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
