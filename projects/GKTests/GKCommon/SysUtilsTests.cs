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
using System.Reflection;
using System.Text;

using BSLib;
using GKCommon;
using GKTests.Mocks;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class SysUtilsTests
    {
        /*public void MyTestFunc1(
            [Values(1, 2, 5)]int x,
            [Values("hello", "buy")]string s)
        {
            Assert.IsTrue(x < 10);
        }

        [Test]
        public void MyTestFunc2(
            [Range(1, 100, 2)]int x,
            [Values("hello", "buy")]string s)
        {
            Assert.IsTrue(x < 50);
        }

        public void MyTestFunc3(
            [Random(100)]int x,
            [Values("hello", "buy")]string s)
        {
        }*/

        [Test]
        public void SysUtils_Tests()
        {
            #if __MonoCS__
            Assert.IsTrue(SysUtils.IsUnix());
            Assert.AreEqual(PlatformID.Unix, SysUtils.GetPlatformID());
            Assert.IsFalse(string.IsNullOrEmpty(SysUtils.GetMonoVersion()));
            Assert.AreNotEqual(DesktopType.Windows, SysUtils.GetDesktopType());
            #else
            Assert.IsFalse(SysUtils.IsUnix());
            Assert.AreEqual(PlatformID.Win32NT, SysUtils.GetPlatformID());
            Assert.IsTrue(string.IsNullOrEmpty(SysUtils.GetMonoVersion()));
            Assert.AreEqual(DesktopType.Windows, SysUtils.GetDesktopType());
            #endif

            //

            Assert.IsTrue(SysUtils.IsUnicodeEncoding(Encoding.UTF8));
            Assert.IsFalse(SysUtils.IsUnicodeEncoding(Encoding.ASCII));

            // other
            string st = "ivan";
            st = SysUtils.NormalizeName(st);
            Assert.AreEqual("Ivan", st);

            st = SysUtils.NormalizeName(null);
            Assert.AreEqual("", st);

            //
            Assert.AreEqual("", SysUtils.GetFileExtension("testfile"));
            Assert.AreEqual(".ext", SysUtils.GetFileExtension("testfile.eXt"));

            Assert.IsFalse(SysUtils.IsRemovableDrive(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)));

            Assembly asm = this.GetType().Assembly;
            var attr1 = SysUtils.GetAssemblyAttribute<AssemblyTitleAttribute>(asm);
            Assert.IsNotNull(attr1);
            Assert.AreEqual("GKTests", attr1.Title);

            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.GetAssemblyAttribute<AssemblyTitleAttribute>(null); });
        }

        [Test]
        public void GfxHelper_Tests()
        {
            Assert.AreEqual(2.0, GfxHelper.ZoomToFit(50, 20, 100, 50));
            Assert.AreEqual(3.0, GfxHelper.ZoomToFit(15, 40, 45, 120));

            Assert.AreEqual(1.0, GfxHelper.ZoomToFit(0, 40, 45, 120));
            Assert.AreEqual(1.0, GfxHelper.ZoomToFit(15, 0, 45, 120));
        }

        [Test]
        public void Test_Matches()
        {
            bool res = SysUtils.MatchesMask("abrakadabra", "*kad*");
            Assert.IsTrue(res);

            res = SysUtils.MatchesMask("abrakadabra", "*test*");
            Assert.IsFalse(res);
        }

        [Test]
        public void Test_GetRectUID()
        {
            Assert.AreEqual("0F000F00D700D700CCDC", SysUtils.GetRectUID(15, 15, 215, 215));
        }
    }
}
