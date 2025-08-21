/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Reflection;
using System.Text;
using GKCore.Locales;
using NUnit.Framework;

namespace GKCore.Utilities
{
    [TestFixture]
    public class SysUtilsTests
    {
        [Test]
        public void Test_SysUtils()
        {
#if OS_LINUX || OS_MACOS
            Assert.IsTrue(SysUtils.IsUnix());
            Assert.AreEqual(PlatformID.Unix, SysUtils.GetPlatformID());
            Assert.AreNotEqual(DesktopType.Windows, SysUtils.GetDesktopType());
#else
            Assert.IsFalse(SysUtils.IsUnix());
            Assert.AreEqual(PlatformID.Win32NT, SysUtils.GetPlatformID());
            Assert.IsTrue(string.IsNullOrEmpty(SysUtils.GetMonoVersion()));
            Assert.AreEqual(DesktopType.Windows, SysUtils.GetDesktopType());
#endif
        }

        [Test]
        public void Test_SysUtils_IsUnicodeEncoding()
        {
            Assert.IsTrue(SysUtils.IsUnicodeEncoding(Encoding.UTF8));
            Assert.IsFalse(SysUtils.IsUnicodeEncoding(Encoding.ASCII));
        }

        [Test]
        public void Test_SysUtils_GetAssemblyAttribute()
        {
            Assembly asm = this.GetType().Assembly;
            var attr1 = SysUtils.GetAssemblyAttribute<AssemblyTitleAttribute>(asm);
            Assert.IsNotNull(attr1);
            Assert.AreEqual("GKTests", attr1.Title);

            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.GetAssemblyAttribute<AssemblyTitleAttribute>(null); });
        }

        [Test]
        public void Test_SysUtils_ImplementsInterface()
        {
            Assert.IsTrue(SysUtils.ImplementsInterface(typeof(LangManager), typeof(ILangMan)));

            Assert.IsFalse(SysUtils.ImplementsInterface(typeof(LangManager), typeof(ILocalizable)));
        }

        [Test]
        public void Test_IsDamagedUtf8Sequence()
        {
            var bytes = new byte[] { 0xA4, 0xD0, 0xB0, 0xD0, 0xBC, 0x20, 0x20, 0x20, 0xD0 };

            Assert.IsTrue(SysUtils.IsDamagedUtf8Sequence(bytes[0], false));
            Assert.IsTrue(SysUtils.IsDamagedUtf8Sequence(bytes[bytes.Length - 1], true));

            bytes = new byte[] { 0xD0, 0xB0, 0xD0, 0xBC };

            Assert.IsFalse(SysUtils.IsDamagedUtf8Sequence(bytes[0], false));
            Assert.IsFalse(SysUtils.IsDamagedUtf8Sequence(bytes[bytes.Length - 1], true));
        }

        [Test]
        public void Test_HasRangeIntersection()
        {
            Assert.IsTrue(SysUtils.HasRangeIntersection(1, 10, 5, 15));
            Assert.IsTrue(SysUtils.HasRangeIntersection(1, 5, 1, 5));
            Assert.IsFalse(SysUtils.HasRangeIntersection(1, 5, 6, 10));
            Assert.IsTrue(SysUtils.HasRangeIntersection(1, 5, 5, 10));
        }

        [Test]
        [TestCase(true, "Silkin*", "Silkinova")]
        [TestCase(true, "Si*kin", "Silkin")]
        [TestCase(true, "Si?kin", "Silkin")]
        [TestCase(false, "Si*kin", "Sunkin")]
        [TestCase(true, "Iv*nov", "Ivanov")]
        [TestCase(true, "Iv*nov|Sid*v", "Sidorov")]
        [TestCase(true, "Sid*v|Pe*ov|Iv*nov", "Ivanov")]
        [TestCase(true, "Sid*v|Iv*nov", "Sidorov")]
        [Parallelizable(ParallelScope.All)]
        public void Test_PatternMatcher(bool value, string expression, string name)
        {
            Assert.AreEqual(value, SysUtils.MatchPattern(expression, name));
        }
    }
}
