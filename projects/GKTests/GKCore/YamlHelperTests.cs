/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using GKCore.Utilities;
using NUnit.Framework;

namespace GKCore
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
