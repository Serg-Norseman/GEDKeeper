/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.Text;
using NUnit.Framework;

namespace GDModel.Providers.GEDCOM
{
    [TestFixture]
    public class AnselEncodingTests
    {
        [Test]
        public void Test_Common()
        {
            var ansel = new AnselEncoding();
            Assert.IsNotNull(ansel);

            Assert.AreEqual("ansel", ansel.HeaderName);
            Assert.AreEqual("ansel", ansel.WebName);
            Assert.AreEqual("ansel", ansel.BodyName);
            Assert.AreEqual("ANSEL", ansel.EncodingName);

            Assert.AreEqual(false, ansel.IsMailNewsDisplay);
            Assert.AreEqual(false, ansel.IsMailNewsSave);
            Assert.AreEqual(true, ansel.IsSingleByte);
        }

        [Test]
        public void Test_GetMaxCharCount()
        {
            var ansel = new AnselEncoding();

            Assert.AreEqual(10, ansel.GetMaxCharCount(10));
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { ansel.GetMaxCharCount(-1); });
        }

        [Test]
        public void Test_GetMaxByteCount()
        {
            var ansel = new AnselEncoding();

            Assert.AreEqual(20, ansel.GetMaxByteCount(10));
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { ansel.GetMaxByteCount(-1); });
        }

        [Test]
        public void Test_GetByteCount()
        {
            var ansel = new AnselEncoding();

            char[] chars = null;
            Assert.Throws(typeof(ArgumentNullException), () => { ansel.GetByteCount(chars, 0, 0); });
            chars = new char[] { ' ' };
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { ansel.GetByteCount(chars, -1, 0); });
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { ansel.GetByteCount(chars, 0, -1); });

            string s = null;
            Assert.Throws(typeof(ArgumentNullException), () => { ansel.GetByteCount(s); });
        }

        [Test]
        public void Test_GetBytes()
        {
            var ansel = new AnselEncoding();

            char[] chars = null;
            Assert.Throws(typeof(ArgumentNullException), () => { ansel.GetBytes(chars, 0, 0, null, 0); });
            chars = new char[] { ' ' };
            byte[] bytes = null;
            Assert.Throws(typeof(ArgumentNullException), () => { ansel.GetBytes(chars, 0, 0, bytes, 0); });
            bytes = new byte[0];
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { ansel.GetBytes(chars, -1, 0, bytes, 0); });

            string s = null;
            Assert.Throws(typeof(ArgumentNullException), () => { ansel.GetBytes(s, 0, 0, null, 0); });
        }

        [Test]
        public void Test_GetCharCount()
        {
            var ansel = new AnselEncoding();

            Assert.Throws(typeof(ArgumentNullException), () => { ansel.GetCharCount(null, 0, 0); });
            byte[] bytes = new byte[0];
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { ansel.GetCharCount(bytes, -1, 0); });
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { ansel.GetCharCount(bytes, 0, -1); });
        }

        [Test]
        public void Test_GetChars()
        {
            var ansel = new AnselEncoding();

            byte[] bytes = null;
            Assert.Throws(typeof(ArgumentNullException), () => { ansel.GetChars(null, 0, 0, null, 0); });
            bytes = new byte[0];
            Assert.Throws(typeof(ArgumentNullException), () => { ansel.GetChars(bytes, 0, 0, null, 0); });
        }

        [Test]
        public void Test_GetString()
        {
            var ansel = new AnselEncoding();

            char[] chars = null;
            Assert.Throws(typeof(ArgumentNullException), () => { ansel.GetByteCount(chars, 0, 0); });

            string s = null;
            Assert.Throws(typeof(ArgumentNullException), () => { ansel.GetByteCount(s); });

            byte[] bytes = null;

            Assert.Throws(typeof(ArgumentNullException), () => { ansel.GetString(null, 0, 0); });
            bytes = Encoding.GetEncoding(437).GetBytes("test");
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { ansel.GetString(bytes, -1, 0); });
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { ansel.GetString(bytes, 0, -1); });
            var res1 = ansel.GetString(bytes, 0, 0);
            Assert.AreEqual(string.Empty, res1);
            res1 = ansel.GetString(bytes, 0, bytes.Length);
            Assert.AreEqual("test", res1);
        }

        [Test]
        public void Test_Complex()
        {
            var ansel = new AnselEncoding();
            Assert.IsNotNull(ansel);

            byte[] data;
            string res, sampleUnic, sampleAnsel;

            // code: E0 (Unicode: hook above, 0309)/low rising tone mark/
            sampleAnsel = "αAαBαCαDαEαFαGαHαIαJαKαLαMαNαOαPαQαRαSαTαUαVαWαXαYαZ";
            sampleUnic = "ẢB̉C̉D̉ẺF̉G̉H̉ỈJ̉K̉L̉M̉N̉ỎP̉Q̉R̉S̉T̉ỦV̉W̉X̉ỶZ̉";

            //Assert.AreEqual(52, ansel.GetByteCount(sampleAnsel));

            //chars = sampleUnic.ToCharArray();
            //Assert.AreEqual(52, ansel.GetByteCount(chars, 0, chars.Length));
            //Assert.AreEqual(52, ansel.GetByteCount(sampleUnic));

            data = Encoding.GetEncoding(437).GetBytes(sampleAnsel);
            Assert.AreEqual(52, data.Length);

            res = ansel.GetString(data);
            Assert.AreEqual(sampleUnic, res);
            Assert.AreEqual(52, res.Length);

            res = ansel.GetString(data, 0, data.Length);
            Assert.AreEqual(sampleUnic, res);
            Assert.AreEqual(52, res.Length);

            data = ansel.GetBytes(res);
            res = Encoding.GetEncoding(437).GetString(data);
            Assert.AreEqual(sampleAnsel, res.Trim('\0')); //FIXME: ALERT!
        }
    }
}
