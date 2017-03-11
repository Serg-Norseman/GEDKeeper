/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
