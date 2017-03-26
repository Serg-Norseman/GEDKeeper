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
using System.IO;
using GKCommon;
using GKCore;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class IniFileTests
    {
        [Test]
        public void Test_Common()
        {
            #if !__MonoCS__
            string fileName = GKUtils.GetTempDir() + "test.ini";
            #else
            string fileName = GKUtils.GetHomePath() + "test.ini";
            #endif

            if (File.Exists(fileName)) File.Delete(fileName); // for local tests!

            using (IniFile iniFile = new IniFile(fileName)) {
                iniFile.WriteInteger("test", "int", 15);
                Assert.AreEqual(15, iniFile.ReadInteger("test", "int", 0));
                iniFile.WriteString("test", "int", "0x9F");
                Assert.AreEqual(159, iniFile.ReadInteger("test", "int", 0));

                iniFile.WriteBool("test", "bool", true);
                Assert.AreEqual(true, iniFile.ReadBool("test", "bool", false));

                iniFile.WriteFloat("test", "float", 0.6666d);
                Assert.AreEqual(0.6666d, iniFile.ReadFloat("test", "float", 0.3333d));

                iniFile.WriteString("test", "str", "alpha");
                Assert.AreEqual("alpha", iniFile.ReadString("test", "str", "beta"));

                DateTime dtx = new DateTime(2016, 08, 11);
                iniFile.WriteDateTime("test", "dtx", dtx);
                Assert.AreEqual(dtx, iniFile.ReadDateTime("test", "dtx", new DateTime())); // writed value

                dtx = new DateTime();
                Assert.AreEqual(dtx, iniFile.ReadDateTime("test", "dtx2", dtx)); // default value

                iniFile.DeleteKey("test", "str");
                Assert.AreEqual("beta", iniFile.ReadString("test", "str", "beta"));

                //iniFile.DeleteSection("test"); // don't work!!!
                iniFile.DeleteKey("test", "int");
                Assert.AreEqual(0, iniFile.ReadInteger("test", "int", 0));
                iniFile.DeleteKey("test", "bool");
                Assert.AreEqual(false, iniFile.ReadBool("test", "bool", false));
                iniFile.DeleteKey("test", "float");
                Assert.AreEqual(0.3333d, iniFile.ReadFloat("test", "float", 0.3333d));
            }
        }
    }
}
