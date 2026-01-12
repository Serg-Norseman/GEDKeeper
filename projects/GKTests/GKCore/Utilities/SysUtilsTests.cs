/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using BSLib;
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
            if (!RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) {
                Assert.IsTrue(SysUtils.IsUnix());
                Assert.AreEqual(PlatformID.Unix, SysUtils.GetPlatformID());
                Assert.AreNotEqual(DesktopType.Windows, SysUtils.GetDesktopType());
            } else {
                Assert.IsFalse(SysUtils.IsUnix());
                Assert.AreEqual(PlatformID.Win32NT, SysUtils.GetPlatformID());
                Assert.IsTrue(string.IsNullOrEmpty(SysUtils.GetMonoVersion()));
                Assert.AreEqual(DesktopType.Windows, SysUtils.GetDesktopType());
            }
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
            Assert.IsTrue(MathHelper.HasRangeIntersection(1, 10, 5, 15));
            Assert.IsTrue(MathHelper.HasRangeIntersection(1, 5, 1, 5));
            Assert.IsFalse(MathHelper.HasRangeIntersection(1, 5, 6, 10));
            Assert.IsTrue(MathHelper.HasRangeIntersection(1, 5, 5, 10));
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
