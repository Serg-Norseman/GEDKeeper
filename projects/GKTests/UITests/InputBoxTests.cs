/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

#if !MONO

using System;
using System.Globalization;
using System.Threading;
using System.Windows.Forms;
using GKTests;
using NUnit.Framework;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class InputBoxTests : CustomWindowTest
    {
        public override void Setup()
        {
        }

        [Test]
        public void Test_StrInput()
        {
            string strValue = "test";

            ModalFormHandler = InputBox_btnAccept_StrHandler;
            GKInputBox.QueryText(null, "caption", "prompt", ref strValue);

            Assert.AreEqual("input", strValue);
        }

        [Test]
        public void Test_PwInput()
        {
            string strValue = "";

            ModalFormHandler = InputBox_btnAccept_StrHandler;
            GKInputBox.QueryPassword("caption", "prompt", ref strValue);

            Assert.AreEqual("input", strValue);
        }

        [Test]
        public void Test_IntInput()
        {
            int intValue;

            ModalFormHandler = InputBox_btnAccept_IntHandler;
            GKInputBox.QueryInt("caption", "prompt", out intValue);

            Assert.AreEqual(123, intValue);
        }

        [Test]
        public void Test_DblInput()
        {
            double val;

            ModalFormHandler = InputBox_btnAccept_DblHandler;
            GKInputBox.QueryDouble("caption", "prompt", out val);

            Assert.AreEqual(15.59, val);
        }

        private void InputBox_btnAccept_StrHandler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtValue", form, "input");

            ClickButton("btnAccept", form);
        }

        private void InputBox_btnAccept_IntHandler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtValue", form, "123");

            ClickButton("btnAccept", form);
        }

        private void InputBox_btnAccept_DblHandler(string name, IntPtr ptr, Form form)
        {
            NumberFormatInfo nfi = (NumberFormatInfo)Thread.CurrentThread.CurrentCulture.NumberFormat.Clone();
            EnterText("txtValue", form, "15"+nfi.NumberDecimalSeparator+"59");

            ClickButton("btnAccept", form);
        }
    }
}

#endif
